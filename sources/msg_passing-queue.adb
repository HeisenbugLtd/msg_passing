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
--  Msg_Passing.Queue
--
--  Implementation of queue based message passing.
--
------------------------------------------------------------------------

package body Msg_Passing.Queue is

   ---------------------------------------------------------------------
   --  Object.Read
   ---------------------------------------------------------------------
   procedure Read (Instance : in out Object;
                   Message  :    out Letter) is
   begin
      Instance.Locked.Get (Message);
   end Read;

   ---------------------------------------------------------------------
   --  Object.Write
   ---------------------------------------------------------------------
   procedure Write (Instance : in out Object;
                    Message  : in     Letter) is
   begin
      Instance.Locked.Put (Message);
   end Write;

   ---------------------------------------------------------------------
   --  protected body Mutex
   ---------------------------------------------------------------------
   protected body Mutex is

      ------------------------------------------------------------------
      --  Queue.Get
      ------------------------------------------------------------------
      entry Get (Message : out Letter) when Num_Messages > 0 is
      begin
         Message := The_Queue (Num_Messages);

         Num_Messages := Num_Messages - 1;
      end Get;

      ------------------------------------------------------------------
      --  Queue.Put
      ------------------------------------------------------------------
      entry Put (Message : in Letter) when Num_Messages < Capacity is
      begin
         Num_Messages := Num_Messages + 1;

         The_Queue (Num_Messages) := Message;
      end Put;

   end Mutex;

end Msg_Passing.Queue;
