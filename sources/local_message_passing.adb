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
--  Local_Message_Passing
--
--  Generic package providing mailbox communication channels.
--
--  All mailboxes instantiated from this package can be shared by name
--  between different tasks without needing the specific package where
--  the mailbox is located.
--
------------------------------------------------------------------------

with Ada.Real_Time;
with System.Address_To_Access_Conversions;
with System.Storage_Elements;

use type Ada.Real_Time.Time;
use type Ada.Real_Time.Time_Span;
use type System.Storage_Elements.Storage_Count;

package body Local_Message_Passing is

   package SSE renames System.Storage_Elements;

   --  Due to alignment rules, the size of the type and that of an
   --  object of that type may be different, so use the size of a
   --  (dummy) object instead of Message'Size for consistency checks.
   --  TODO: Use 'Object_Size instead.
   Dummy                 : Message;
   MESSAGE_OBJECT_LENGTH : constant SSE.Storage_Count :=
     (Dummy'Size + System.Storage_Unit - 1) / System.Storage_Unit;

   ---------------------------------------------------------------------
   --  Conv
   --
   --  To convert between a Mailbox and its Handle we simply convert the
   --  address into an access type.
   --
   --  This avoids 'Unchecked_Access at the danger of creating dangling
   --  pointers if the scope of the mailbox is smaller than that of its
   --  pointer to it.
   ---------------------------------------------------------------------
   package Conv is
     new System.Address_To_Access_Conversions (Object => Mailbox);

   ---------------------------------------------------------------------
   --  Export_Mailbox
   --
   --  Exports a mailbox by name for use by other processes.
   --
   --  The name given to the mailbox is only significant up to the first
   --  31 characters, and shall be unique in those.
   ---------------------------------------------------------------------
   procedure Export_Mailbox (Mbx  : in out Mailbox;
                             Name : in     String);

   ---------------------------------------------------------------------
   --  Conditional_Receive
   ---------------------------------------------------------------------
   procedure Conditional_Receive (Mbx     : in     Handle;
                                  Msg     :    out Message;
                                  Success :    out Boolean) is
   begin
      Mbx.all.Try_Get (Msg, Success);
   end Conditional_Receive;

   ---------------------------------------------------------------------
   --  Conditional_Send
   ---------------------------------------------------------------------
   procedure Conditional_Send (Mbx     : in     Handle;
                               Msg     : in     Message;
                               Success :    out Boolean) is
   begin
      Mbx.all.Try_Put (Msg, Success);
   end Conditional_Send;

   ---------------------------------------------------------------------
   --  Export_Mailbox
   ---------------------------------------------------------------------
   procedure Export_Mailbox (Mbx  : in out Mailbox;
                             Name : in     String) is
   begin
      Mailbox_Sharing.Add_Mailbox
        (Mbx'Address, Name, MESSAGE_OBJECT_LENGTH);
   end Export_Mailbox;

   ---------------------------------------------------------------------
   --  Import_Mailbox
   ---------------------------------------------------------------------
   procedure Import_Mailbox (Name     : in     String;
                             Hnd      :    out Handle;
                             Max_Wait : in     Ada.Real_Time.Time_Span)
   is
      --  Convert the relative delay to an absolute one.
      Latest : constant Ada.Real_Time.Time :=
        Ada.Real_Time.Clock + Max_Wait;
   begin
      Hnd :=
        Handle
          (Conv.To_Pointer
             (Mailbox_Sharing.Find_Mailbox
                (Name, MESSAGE_OBJECT_LENGTH, Latest)));
   end Import_Mailbox;

   ---------------------------------------------------------------------
   --  Open_Mailbox
   ---------------------------------------------------------------------
   procedure Open_Mailbox (Mbx         : in out Mailbox;
                           Hnd         :    out Handle;
                           Export_Name : in     String := "") is
   begin
      Mbx.Clear;
      Hnd := Handle (Conv.To_Pointer (Mbx'Address));

      if Export_Name /= "" then
         Export_Mailbox (Mbx, Export_Name);
      end if;
   end Open_Mailbox;

   ---------------------------------------------------------------------
   --  Receive
   ---------------------------------------------------------------------
   procedure Receive (Mbx : in     Handle;
                      Msg :    out Message) is
   begin
      Mbx.all.Get (Msg);
   end Receive;

   ---------------------------------------------------------------------
   --  Send
   ---------------------------------------------------------------------
   procedure Send (Mbx : in Handle;
                   Msg : in Message) is
   begin
      Mbx.all.Put (Msg);
   end Send;

   ---------------------------------------------------------------------
   --  Timed_Receive
   ---------------------------------------------------------------------
   procedure Timed_Receive (Mbx     : in     Handle;
                            Msg     :    out Message;
                            Latest  : in     Ada.Real_Time.Time;
                            Success :    out Boolean) is
   begin
      select
         Mbx.all.Get (Msg);
         Success := True;
      or
         delay until Latest;

         Success := False;
      end select;
   end Timed_Receive;

   ---------------------------------------------------------------------
   --  Timed_Receive
   ---------------------------------------------------------------------
   procedure Timed_Receive (Mbx      : in     Handle;
                            Msg      :    out Message;
                            Max_Wait : in     Ada.Real_Time.Time_Span;
                            Success  :    out Boolean)
   is
   begin
      Timed_Receive (Mbx, Msg, Ada.Real_Time.Clock + Max_Wait, Success);
   end Timed_Receive;

   ---------------------------------------------------------------------
   --  Timed_Send
   ---------------------------------------------------------------------
   procedure Timed_Send (Mbx     : in     Handle;
                         Msg     : in     Message;
                         Latest  : in     Ada.Real_Time.Time;
                         Success :    out Boolean) is
   begin
      select
         Mbx.all.Put (Msg);

         Success := True;
      or
         delay until Latest;

         Success := False;
      end select;
   end Timed_Send;

   ---------------------------------------------------------------------
   --  Timed_Send
   ---------------------------------------------------------------------
   procedure Timed_Send (Mbx      : in     Handle;
                         Msg      : in     Message;
                         Max_Wait : in     Ada.Real_Time.Time_Span;
                         Success  :    out Boolean)
   is
   begin
      Timed_Send (Mbx, Msg, Ada.Real_Time.Clock + Max_Wait, Success);
   end Timed_Send;

   ---------------------------------------------------------------------
   --  Mailbox
   ---------------------------------------------------------------------
   protected body Mailbox is

      ------------------------------------------------------------------
      --  Mailbox.Clear
      ------------------------------------------------------------------
      procedure Clear is
      begin
         --  Reset number of messages and indices.
         Num_Messages := 0;
         Oldest       := Msg_Queue'First;
         Latest       := Msg_Queue'First;
      end Clear;

      ------------------------------------------------------------------
      --  Mailbox.Get
      ------------------------------------------------------------------
      entry Get (Msg : out Message) when Num_Messages > 0 is
      begin
         --  Get oldest message from queue and advance index.
         Msg := Msg_Queue (Oldest);
         Oldest := (Oldest mod Size) + 1;

         --  Message has been retrieved now.
         Num_Messages := Num_Messages - 1;
      end Get;

      ------------------------------------------------------------------
      --  Mailbox.Put
      ------------------------------------------------------------------
      entry Put (Msg : in Message) when Num_Messages < Size is
      begin
         --  Put message into latest slot and advance index.
         Msg_Queue (Latest) := Msg;
         Latest := (Latest mod Size) + 1;

         --  One more message deposited.
         Num_Messages := Num_Messages + 1;
      end Put;

      ------------------------------------------------------------------
      --  Mailbox.Try_Get
      ------------------------------------------------------------------
      procedure Try_Get (Msg     : out Message;
                         Success : out Boolean)
      is
      begin
         Success := Num_Messages > 0;

         if Success then
            --  Get message from oldest slot and advance index.
            Msg := Msg_Queue (Oldest);
            Oldest := (Oldest mod Size) + 1;

            --  One message retrieved.
            Num_Messages := Num_Messages - 1;
         end if;
      end Try_Get;

      ------------------------------------------------------------------
      --  Mailbox.Try_Put
      ------------------------------------------------------------------
      procedure Try_Put (Msg     : in     Message;
                         Success :    out Boolean)
      is
      begin
         Success := Num_Messages < Size;

         if Success then
            --  Put message into latest slot and advance index.
            Msg_Queue (Latest) := Msg;
            Latest := (Latest mod Size) + 1;

            --  One more message deposited.
            Num_Messages := Num_Messages + 1;
         end if;
      end Try_Put;

   end Mailbox;

end Local_Message_Passing;
