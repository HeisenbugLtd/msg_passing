------------------------------------------------------------------------
--  Copyright (C) 2010-2020 by Heisenbug Ltd. (github@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under
--  the terms of the Do What The Fuck You Want To Public License,
--  Version 2, as published by Sam Hocevar. See the LICENSE file for
--  more details.
------------------------------------------------------------------------
pragma License (Unrestricted);

------------------------------------------------------------------------
--  Mailbox_Sharing
--
--  Supports import and export of generic instantiations of a mailbox
--  from the Local_Message_Passing package.
--
--  Careful: For the actual import and export function, addresses are
--           used defeating any type checking!
--
------------------------------------------------------------------------

with Ada.Strings.Fixed;
with System.Storage_Elements;

use type System.Address;
use type System.Storage_Elements.Storage_Count;

package body Mailbox_Sharing is

   type Mailbox_Name is new String (1 .. MAX_NAME_LENGTH);

   type Mailbox_Info is
      record
         Name     : Mailbox_Name;      --  Name of the mailbox.
         Addr     : System.Address;    --  Address of mailbox object.
         Msg_Size : SSE.Storage_Count; --  Type size for sanity checks.
      end record;

   --  The protected object uses two queues to serve the tasks waiting
   --  for a mailbox to appear. See explanation below.
   --  A modular type makes it easier to switch the queues with a simple
   --  increment operation (0 + 1 => 1 & 1 + 1 => 0), which - at the
   --  binary level - is the intended exclusive or.
   type Queue_Index is mod 2;

   --  Type for mailbox list.
   type    Mailbox_List is array (Positive range <>) of Mailbox_Info;
   subtype Mailbox_Idx  is Positive range 1 .. MAX_MAILBOXES;

   Null_Name : constant Mailbox_Name := Mailbox_Name'(others => ' ');

   --  Type for "pure" boolean entry barriers.
   type Entry_Barrier is array (Queue_Index) of Boolean;

   ---------------------------------------------------------------------
   --  Test_For_Mailbox
   --
   --  Internal helper which checks if a specific mailbox is present.
   ---------------------------------------------------------------------
   procedure Test_For_Mailbox (Mailboxes : in     Mailbox_List;
                               Name      : in     Mailbox_Name;
                               Msg_Size  : in     SSE.Storage_Count;
                               Slot      :    out Mailbox_Idx;
                               Found     :    out Boolean);

   ---------------------------------------------------------------------
   --  To_Mailbox_Name
   --
   --  Converts a string to a mailbox name with length MAX_NAME_LENGTH.
   ---------------------------------------------------------------------
   function To_Mailbox_Name (Name : in String) return Mailbox_Name;

   ---------------------------------------------------------------------
   --  Mailbox_DB
   --
   --  Concurrent version of mailbox ex- and import.
   --
   --  It works as follows:
   --
   --  (1)  If a connection request comes in at the "Wait_For_Mailbox"
   --       entry, it is immediately checked if the requested mailbox
   --       is present.
   --  (1a) Upon success all is done here and the waiter leaves.
   --  (1b) If the mailbox is not yet present, the waiter will be
   --       requeued to the entry "Check_For_Mailbox" for the
   --       "Current_Queue" and will suspend there.
   --  (2)  Once "Insert_Mailbox" successfully added a named mailbox to
   --       the internal list, the entry barrier for the current queue
   --       is raised.
   --  (3)  This wakes up all waiters that were previously requeued in
   --       step (1b). All those waiters check again if their mailbox
   --       is present now.
   --  (3a) If the requested mailbox is present, the wait is over and
   --       the waiter leaves the queue.
   --  (3b) Unsuccessful waiters will then be requeued again, but to the
   --       alternative entry queue.
   --  (4)  After all waiters have been served, the entry barrier for
   --       the "Current_Queue" is lowered again and the entry queues
   --       switch their places and the whole process can start again.
   ---------------------------------------------------------------------
   protected Mailbox_DB is

      ------------------------------------------------------------------
      --  Mailbox_DB.Insert_Mailbox
      --
      --  Adds a mailbox to the list and notifies the waiters that the
      --  list has changed and they can check for their named box again.
      ------------------------------------------------------------------
      procedure Insert_Mailbox (Mbx_Address : in System.Address;
                                Name        : in Mailbox_Name;
                                Msg_Size    : in SSE.Storage_Count);

      ------------------------------------------------------------------
      --  Mailbox_DB.Wait_For_Mailbox
      --
      --  Waits for the appearance of a named mailbox.
      --  Immediately checks if the requested mailbox is present and if
      --  not, it requeues the waiter to the internal Check_For_Mailbox
      --  entry.
      ------------------------------------------------------------------
      entry Wait_For_Mailbox (Name        : in     Mailbox_Name;
                              Msg_Size    : in     SSE.Storage_Count;
                              Mbx_Address :    out System.Address);

   private

      ------------------------------------------------------------------
      --  Mailbox_DB.Check_For_Mailbox
      --
      --  Once the current list of exported mailboxes change, the entry
      --  with the current queue index opens and all queued waiters
      --  check for the mailbox they wish to connect to. If they are not
      --  successful, they get requeued to the alternative entry.
      ------------------------------------------------------------------
      entry Check_For_Mailbox (Queue_Index)
        (Name        : in     Mailbox_Name;
         Msg_Size    : in     SSE.Storage_Count;
         Mbx_Address :    out System.Address);

      --  The list of mailboxes.
      Mailboxes : Mailbox_List (Mailbox_Idx'Range) :=
                    Mailbox_List'(Mailbox_Idx'Range =>
                                    (Name     => Null_Name,
                                     Addr     => System.Null_Address,
                                     Msg_Size => 0));

      Barrier_Open  : Entry_Barrier := Entry_Barrier'(others => False);
      Current_Queue : Queue_Index   := Queue_Index'First;
   end Mailbox_DB;

   protected body Mailbox_DB is

      ------------------------------------------------------------------
      --  Mailbox_DB.Insert_Mailbox
      ------------------------------------------------------------------
      procedure Insert_Mailbox (Mbx_Address : in System.Address;
                                Name        : in Mailbox_Name;
                                Msg_Size    : in SSE.Storage_Count)
      is
         Slot  : Mailbox_Idx;
         Found : Boolean;
      begin
         --  Find a free slot in the mailbox list.
         Slot := Mailboxes'First;

         Find_Slot :
         loop
            declare
               Current_Mailbox : Mailbox_Info renames Mailboxes (Slot);
            begin
               --  Empty slot found?
               Found := Current_Mailbox.Name = Null_Name;

               --  Sanity check.
               --  Allow exporting the *same* mailbox more than once.
               if not Found and then Current_Mailbox.Name = Name then
                  Found :=
                    Current_Mailbox.Addr     = Mbx_Address and
                    Current_Mailbox.Msg_Size = Msg_Size;

                  --  If it was not the same mailbox, raise exception.
                  if not Found then
                     raise Already_Exported;
                  end if;
               end if;
            end;

            exit Find_Slot when Found or Slot = Mailboxes'Last;

            Slot := Mailbox_Idx'Succ (Slot);
         end loop Find_Slot;

         if not Found then
            raise Too_Many_Mailboxes;
         end if;

         Mailboxes (Slot).Name     := Name;
         Mailboxes (Slot).Addr     := Mbx_Address;
         Mailboxes (Slot).Msg_Size := Msg_Size;

         --  This triggers a new mailbox check even if the exported
         --  mailbox has already been exported. But well...
         Barrier_Open (Current_Queue) := True; --  Open barrier.
         Current_Queue := Current_Queue + 1;   --  Modular type.
      end Insert_Mailbox;

      ------------------------------------------------------------------
      --  Mailbox_DB.Wait_For_Mailbox
      ------------------------------------------------------------------
      entry Wait_For_Mailbox (Name        : in     Mailbox_Name;
                              Msg_Size    : in     SSE.Storage_Count;
                              Mbx_Address :    out System.Address)
        when True
      is
         Slot  : Mailbox_Idx;
         Found : Boolean;
      begin
         Test_For_Mailbox (Mailboxes, Name, Msg_Size, Slot, Found);

         if Found then
            Mbx_Address := Mailboxes (Slot).Addr;
         else
            requeue Check_For_Mailbox (Current_Queue) with abort;
         end if;
      end Wait_For_Mailbox;

      ------------------------------------------------------------------
      --  Mailbox_DB.Check_For_Mailbox
      --
      --  Checks if a named mailbox to be connected to is present.
      ------------------------------------------------------------------
      entry Check_For_Mailbox (for Queue in Queue_Index)
        (Name        : in     Mailbox_Name;
         Msg_Size    : in     SSE.Storage_Count;
         Mbx_Address :    out System.Address)
        when Barrier_Open (Queue)
      is
         Slot  : Mailbox_Idx;
         Found : Boolean;
      begin
         --  Keep changed flag up until all waiters have checked in.
         if Check_For_Mailbox (Queue)'Count = 0 then
            --  All current waiters served. Close current entry.
            Barrier_Open (Queue) := False;
         end if;

         Test_For_Mailbox (Mailboxes, Name, Msg_Size, Slot, Found);

         if Found then
            Mbx_Address := Mailboxes (Slot).Addr;
         else
            --  Mailbox still not present.
            --  Requeue waiter to the alternative entry.
            requeue Check_For_Mailbox (Queue + 1) with abort;
         end if;
      end Check_For_Mailbox;

   end Mailbox_DB;

   ---------------------------------------------------------------------
   --  Add_Mailbox
   ---------------------------------------------------------------------
   procedure Add_Mailbox (Mbx_Address : in System.Address;
                          Name        : in String;
                          Msg_Size    : in SSE.Storage_Count) is
   begin
      Mailbox_DB.Insert_Mailbox
        (Mbx_Address, To_Mailbox_Name (Name), Msg_Size);
   end Add_Mailbox;

   ---------------------------------------------------------------------
   --  Find_Mailbox
   ---------------------------------------------------------------------
   function Find_Mailbox
     (Name     : in String;
      Msg_Size : in SSE.Storage_Count;
      Latest   : in Ada.Real_Time.Time) return System.Address
   is
      Result : System.Address;
   begin
      loop
         select
            Mailbox_DB.Wait_For_Mailbox
              (To_Mailbox_Name (Name), Msg_Size, Result);

            return Result;
         or
            delay until Latest;

            raise No_Such_Mailbox;
         end select;
      end loop;
   end Find_Mailbox;

   ---------------------------------------------------------------------
   --  Mailbox_DB.Test_For_Mailbox
   ---------------------------------------------------------------------
   procedure Test_For_Mailbox (Mailboxes : in     Mailbox_List;
                               Name      : in     Mailbox_Name;
                               Msg_Size  : in     SSE.Storage_Count;
                               Slot      :    out Mailbox_Idx;
                               Found     :    out Boolean) is
   begin
      Slot := Mailboxes'First;

      Find_Name :
      loop
         Found := (Mailboxes (Slot).Name = Name and
                     Mailboxes (Slot).Msg_Size = Msg_Size);

         exit Find_Name when Found or Slot = Mailboxes'Last;

         Slot := Mailbox_Idx'Succ (Slot);
      end loop Find_Name;
   end Test_For_Mailbox;

   ---------------------------------------------------------------------
   --  To_Mailbox_Name
   ---------------------------------------------------------------------
   function To_Mailbox_Name (Name : in String) return Mailbox_Name is
   begin
      return
        Mailbox_Name
          (Ada.Strings.Fixed.Overwrite (Source   => String (Null_Name),
                                        Position => Null_Name'First,
                                        New_Item => Name));
   end To_Mailbox_Name;

end Mailbox_Sharing;
