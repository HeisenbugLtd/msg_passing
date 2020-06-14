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
--  This package provides message passing based on a queue.
--
--  This means, there can be multiple senders, receivers, and messages
--  at a time. Senders put messages into a queue, receivers read them.
--
------------------------------------------------------------------------
generic

   Capacity : Positive := 1;

package Msg_Passing.Queue is

   pragma Pure;

   ---------------------------------------------------------------------
   --  The queue based messaging object
   ---------------------------------------------------------------------
   type Object is new Msg_Passing.Object with private;

   ---------------------------------------------------------------------
   --  Object.Read
   ---------------------------------------------------------------------
   procedure Read  (Instance : in out Object;
                    Message  :    out Letter);

   ---------------------------------------------------------------------
   --  Object.Write
   ---------------------------------------------------------------------
   procedure Write (Instance : in out Object;
                    Message  : in     Letter);

private

   ---------------------------------------------------------------------
   --  Queue implemented as simple array
   ---------------------------------------------------------------------
   subtype Message_Counter is Natural range 0 .. Capacity;

   type Queue_Type is array (1 .. Message_Counter'Last) of Letter;

   ---------------------------------------------------------------------
   --  Mutex
   ---------------------------------------------------------------------
   protected type Mutex is

      ------------------------------------------------------------------
      --  Mutex.Get
      --
      --  Reads a message from the queue. If there is no message, the
      --  calling task is blocked until a new message is put into the
      --  queue.
      --
      ------------------------------------------------------------------
      entry Get (Message : out Letter);

      ------------------------------------------------------------------
      --  Mutex.Put
      --
      --  Writes a message into the queue. If the queue is full, the
      --  calling task is blocked until some message is removed from the
      --  queue again.
      --
      ------------------------------------------------------------------
      entry Put (Message : in Letter);

   private

      The_Queue    : Queue_Type;
      Num_Messages : Message_Counter := 0;

   end Mutex;

   ---------------------------------------------------------------------
   --  Object type completion
   ---------------------------------------------------------------------
   type Object is new Msg_Passing.Object with
      record
         Locked : Mutex;
      end record;

end Msg_Passing.Queue;
