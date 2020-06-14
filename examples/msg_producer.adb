------------------------------------------------------------------------
--  Copyright (C) 2010-2020 by Heisenbug Ltd. (github@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under
--  the terms of the Do What The Fuck You Want To Public License,
--  Version 2, as published by Sam Hocevar. See the LICENSE file for
--  more details.
------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Synchronous_Task_Control;
with Ada.Numerics.Discrete_Random;
with Ada.Real_Time;
with Ada.Text_IO;

package body Msg_Producer is

   subtype Random_Int is Message range 10 .. 99;
   package Msg_Random is new Ada.Numerics.Discrete_Random (Random_Int);

   ---------------------------------------------------------------------
   --  Messenger
   --
   --  No entry, the whole activation is done via suspension object.
   ---------------------------------------------------------------------
   task type Messenger;

   --  The cycle time for the intended message rate.
   Cycle_Time : Ada.Real_Time.Time_Span;

   --  Administration data. Message boards and appropriate suspension
   --  objects.
   type Board_Ref is access all Int_Messaging.Object'Class;

   Msg_Board : Board_Ref;
   Init_Done : Ada.Synchronous_Task_Control.Suspension_Object;

   ---------------------------------------------------------------------
   --  Messenger
   ---------------------------------------------------------------------
   task body Messenger is
      use type Ada.Real_Time.Time;

      Gen             : Msg_Random.Generator;
      Next_Activation : Ada.Real_Time.Time;
   begin
      ------------------------------------------------------------------
      --  "entry" Messenger.Init
      ------------------------------------------------------------------
      Ada.Synchronous_Task_Control.Suspend_Until_True (Init_Done);

      Next_Activation := Ada.Real_Time.Clock;

      Msg_Random.Reset (Gen);

      loop
         Int_Messaging.Write (Msg_Board.all, Msg_Random.Random (Gen));

         Next_Activation := Next_Activation + Cycle_Time;
         delay until Next_Activation;
      end loop;
   end Messenger;

   ---------------------------------------------------------------------
   --  the messenger task
   ---------------------------------------------------------------------
   My_Task : Messenger;

   ---------------------------------------------------------------------
   --  Start
   ---------------------------------------------------------------------
   procedure Start (Msg_Rate : in     Integer;
                    MB       : in out Int_Messaging.Object'Class) is
   begin
      Msg_Board  := MB'Unchecked_Access;
      Cycle_Time :=
        Ada.Real_Time.Nanoseconds (1_000_000_000 / Msg_Rate);

      --  Initialization of package state is done, release task.
      Ada.Synchronous_Task_Control.Set_True (Init_Done);
   end Start;

   package Test is
      function Constant_Result return Boolean;
   end Test;
   pragma Unreferenced (Test);

   package body Test is
      Start_Time : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;

      function Constant_Result return Boolean is
         use Ada.Real_Time;

         Result : constant Boolean :=
           (Clock - Start_Time) < Milliseconds (5000);
      begin
         Ada.Text_IO.Put_Line (Boolean'Image (Result));
         return Result;
      end Constant_Result;

      task Test_Task is
         entry Init;
         pragma Unreferenced (Init);
      end Test_Task;

      task body Test_Task is
      begin
         loop
            select
               when Constant_Result =>
                  delay 0.0;
            or
               accept Init;
            end select;
         end loop;
      end Test_Task;

   end Test;

end Msg_Producer;
