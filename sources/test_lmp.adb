------------------------------------------------------------------------
--  Copyright (C) 2010-2020 by Heisenbug Ltd. (github@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under
--  the terms of the Do What The Fuck You Want To Public License,
--  Version 2, as published by Sam Hocevar. See the LICENSE file for
--  more details.
------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Real_Time;
with Ada.Task_Attributes;
with Ada.Task_Identification;
with Ada.Text_IO;

with Local_Message_Passing;

use type Ada.Real_Time.Time;

procedure Test_LMP is

   --  Message type declarations.
   type Int_Msg is new Integer;
   type Flt_Msg is new Float;

   ---------------------------------------------------------------------
   --  Use task attributes to control the receiver and sender task
   --  timing.
   ---------------------------------------------------------------------
   type Cycle_Info is
      record
         Start_At : Ada.Real_Time.Time;
         Interval : Ada.Real_Time.Time_Span;
      end record;

   package Task_Scheduling is new
     Ada.Task_Attributes
       (Attribute     => Cycle_Info,
        Initial_Value =>
        Cycle_Info'(Start_At => Ada.Real_Time.Time_Last,
                    Interval => Ada.Real_Time.Time_Span_Last));

   ---------------------------------------------------------------------
   --  Suspension_Object
   --
   --  Ada.Synchronous_Task_Control.Suspension_Object can not handle
   --  more than one task suspending on it, so declare a protected
   --  object which does the same, but does have an entry queue.
   ---------------------------------------------------------------------
   protected type Suspension_Object is

      procedure Set_True;

      entry Suspend_Until_True;

   private

      Guard : Boolean := False;

   end Suspension_Object;

   protected body Suspension_Object is

      ------------------------------------------------------------------
      --  Suspension_Object.Set_True
      ------------------------------------------------------------------
      procedure Set_True is
      begin
         Guard := True;
      end Set_True;

      ------------------------------------------------------------------
      --  Suspension_Object.Suspend_Until_True
      ------------------------------------------------------------------
      entry Suspend_Until_True when Guard is
      begin
         null;
      end Suspend_Until_True;

   end Suspension_Object;
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   --  package Generic_Sender
   ---------------------------------------------------------------------
   generic
      type Msg_Type is private;

      Initial_Value : in     Msg_Type;
      Mbx_Name      : in     String;
      Suspend_On    : in out Suspension_Object;

      with procedure Increment (Value : in out Msg_Type);

   package Generic_Sender is

      task type Sender;

   end Generic_Sender;

   package body Generic_Sender is

      package LMP is new Local_Message_Passing (Message => Msg_Type);

      task body Sender is
         M : LMP.Handle;
         X : Msg_Type;
      begin
         Suspend_On.Suspend_Until_True;
         X := Initial_Value;

         Ada.Text_IO.Put_Line
           (Ada.Task_Identification.Image
              (Ada.Task_Identification.Current_Task) &
            ": Trying to connect to """ & Mbx_Name & """ ...");

         declare
            Start_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
         begin
            LMP.Import_Mailbox
              (Name     => Mbx_Name,
               Hnd      => M,
               Max_Wait => Ada.Real_Time.Milliseconds (10000));

            Ada.Text_IO.Put_Line
              (Ada.Task_Identification.Image
                 (Ada.Task_Identification.Current_Task) &
               ": Connected to mailbox """ & Mbx_Name & """ after" &
               Duration'Image
                 (Ada.Real_Time.To_Duration
                    (Ada.Real_Time.Clock - Start_Time)) &
               " seconds.");
         exception
            when LMP.No_Such_Mailbox =>
               Ada.Text_IO.Put_Line
                 (Ada.Task_Identification.Image
                    (Ada.Task_Identification.Current_Task) &
                  ": Gave up after" &
                  Duration'Image
                    (Ada.Real_Time.To_Duration
                       (Ada.Real_Time.Clock - Start_Time)) &
                  " seconds.");

               raise;
         end;

         declare
            Sched_Info : constant Cycle_Info := Task_Scheduling.Value;
            Next       : Ada.Real_Time.Time  := Sched_Info.Start_At;
         begin
            loop
               delay until Next;
               Next := Next + Sched_Info.Interval;

               LMP.Send (M, X);
               Increment (X);
            end loop;
         end;
      exception
         when others =>
            Ada.Text_IO.Put_Line
              (Ada.Task_Identification.Image
                 (Ada.Task_Identification.Current_Task) &
               " terminated.");
      end Sender;

   end Generic_Sender;
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   --  package Generic_Receiver
   ---------------------------------------------------------------------
   generic
      type Msg_Type is private;

      Mbx_Name   : in     String;
      Suspend_On : in out Suspension_Object;

      with function Image (Value : in Msg_Type) return String;

   package Generic_Receiver is

      task Receiver;

   end Generic_Receiver;

   package body Generic_Receiver is

      package LMP is new Local_Message_Passing (Message => Msg_Type);

      procedure Print_Message (Msg : in Msg_Type);
      procedure Print_Message (Msg : in Msg_Type) is
      begin
         Ada.Text_IO.Put_Line
           (Ada.Task_Identification.Image
              (Ada.Task_Identification.Current_Task) &
            ":" &
            Image (Msg));
      end Print_Message;

      task body Receiver is
         M : LMP.Mailbox (Size => 16);
         H : LMP.Handle;
         X : Msg_Type;
      begin
         Suspend_On.Suspend_Until_True;
         delay until Task_Scheduling.Value.Start_At;

         LMP.Open_Mailbox (M, H, Mbx_Name);

         loop
            LMP.Receive (H, X);
            Print_Message (X);
         end loop;
      exception
         when others =>
            Ada.Text_IO.Put_Line
              (Ada.Task_Identification.Image
                 (Ada.Task_Identification.Current_Task) &
               " terminated.");
      end Receiver;

   end Generic_Receiver;
   ---------------------------------------------------------------------

   --
   --  The global suspension object to serve as task start trigger.
   --
   Global_Unsuspend : Suspension_Object;

   ---------------------------------------------------------------------
   --  package Senders
   ---------------------------------------------------------------------
   package Senders is

      --  Subtype for indices into sending tasks array.
      subtype Sender_Index is Positive range 1 .. 10;

      ------------------------------------------------------------------
      --  Instantiation of Int_Sender package.
      ------------------------------------------------------------------
      procedure Increment (Value : in out Int_Msg);

      package Int_Sender is
        new Generic_Sender (Msg_Type      => Int_Msg,
                            Initial_Value => 0,
                            Suspend_On    => Global_Unsuspend,
                            Mbx_Name      => "INTEGER",
                            Increment     => Increment);

      Int_Senders : array (Sender_Index) of Int_Sender.Sender;

      ------------------------------------------------------------------
      --  Instantiation of Flt_Sender package.
      ------------------------------------------------------------------
      procedure Increment (Value : in out Flt_Msg);

      package Flt_Sender is
        new Generic_Sender (Msg_Type      => Flt_Msg,
                            Initial_Value => 0.0,
                            Suspend_On    => Global_Unsuspend,
                            Mbx_Name      => "FLOAT",
                            Increment     => Increment);

      Flt_Senders : array (Sender_Index) of Flt_Sender.Sender;

   end Senders;

   package body Senders is

      ------------------------------------------------------------------
      --  Increment for Int_Sender package.
      ------------------------------------------------------------------
      procedure Increment (Value : in out Int_Msg) is
      begin
         Value := Int_Msg'Succ (Value);
      end Increment;

      ------------------------------------------------------------------
      --  Increment for Flt_Sender package.
      ------------------------------------------------------------------
      procedure Increment (Value : in out Flt_Msg) is
      begin
         Value := Flt_Msg'Succ (Value);
      end Increment;

   end Senders;
   ---------------------------------------------------------------------

   ---------------------------------------------------------------------
   --  package Receivers
   ---------------------------------------------------------------------
   package Receivers is

      ------------------------------------------------------------------
      --  Instantiation of Int_Receiver package.
      ------------------------------------------------------------------
      function Image (Value : in Int_Msg) return String;

      pragma Warnings (Off, "cannot call ""Image"" before body seen");
      --  While technically true, the task will never call it before
      --  the body has been seen.
      package Int_Receiver is
        new Generic_Receiver (Msg_Type   => Int_Msg,
                              Mbx_Name   => "INTEGER",
                              Suspend_On => Global_Unsuspend,
                              Image      => Image);
      pragma Warnings (On, "cannot call ""Image"" before body seen");

      ------------------------------------------------------------------
      --  Instantiation of Flt_Receiver package.
      ------------------------------------------------------------------
      function Image (Value : in Flt_Msg) return String;

      pragma Warnings (Off, "cannot call ""Image"" before body seen");
      --  While technically true, the task will never call it before
      --  the body has been seen.
      package Flt_Receiver is
        new Generic_Receiver (Msg_Type   => Flt_Msg,
                              Mbx_Name   => "FLOAT",
                              Suspend_On => Global_Unsuspend,
                              Image      => Image);
      pragma Warnings (On, "cannot call ""Image"" before body seen");

   end Receivers;

   package body Receivers is

      ------------------------------------------------------------------
      --  Image for Int_Receiver package.
      ------------------------------------------------------------------
      function Image (Value : in Int_Msg) return String is
      begin
         return Int_Msg'Image (Value);
      end Image;

      ------------------------------------------------------------------
      --  Image for Int_Receiver package.
      ------------------------------------------------------------------
      function Image (Value : in Flt_Msg) return String is
      begin
         return Flt_Msg'Image (Value);
      end Image;

   end Receivers;
   ---------------------------------------------------------------------

   Start : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
begin

   for T in Senders.Int_Senders'Range loop
      declare
         Offset : constant Integer := Senders.Sender_Index'Pos (T) + 1;
      begin
         Task_Scheduling.Set_Value
           (Val => Cycle_Info'(Start_At =>
                                 Start +
                                   Ada.Real_Time.Milliseconds (1000 * Offset),
                               Interval =>
                                 Ada.Real_Time.Milliseconds (250 * Offset)),
            T   => Senders.Int_Senders (T)'Identity);
      end;
   end loop;

   for T in Senders.Flt_Senders'Range loop
      declare
         Offset : constant Integer := Senders.Sender_Index'Pos (T) + 1;
      begin
         Task_Scheduling.Set_Value
           (Val => Cycle_Info'(Start_At =>
                                 Start +
                                   Ada.Real_Time.Milliseconds (2000 * Offset),
                               Interval =>
                                 Ada.Real_Time.Milliseconds (250 * Offset)),
            T   => Senders.Flt_Senders (T)'Identity);
      end;
   end loop;

   Task_Scheduling.Set_Value
     (Val => Cycle_Info'(Start_At => Start + Ada.Real_Time.Milliseconds (3000),
                         Interval => Ada.Real_Time.Time_Span_Zero),
      T   => Receivers.Int_Receiver.Receiver'Identity);

   Task_Scheduling.Set_Value
     (Val => Cycle_Info'(Start_At => Start + Ada.Real_Time.Milliseconds (4000),
                         Interval => Ada.Real_Time.Time_Span_Zero),
      T   => Receivers.Flt_Receiver.Receiver'Identity);

   Global_Unsuspend.Set_True;

end Test_LMP;
