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
--  Msg_Passing.Blackboard
--
--  This package provides message passing based on blackboard semantics.
--
--  This means, there is only one sender, one receiver, and one message
--  at a time. Upon receiving the message, it is erased immediately.
--
------------------------------------------------------------------------

generic

package Msg_Passing.Blackboard is

   pragma Pure;

   ---------------------------------------------------------------------
   --  The messaging object.
   ---------------------------------------------------------------------
   type Object is new Msg_Passing.Object with private;

   ---------------------------------------------------------------------
   --  Object.Read
   ---------------------------------------------------------------------
   procedure Read  (Instance : in out Object;
                    Message  :    out Msg_Passing.Letter);

   ---------------------------------------------------------------------
   --  Object.Write
   ---------------------------------------------------------------------
   procedure Write (Instance : in out Object;
                    Message  : in     Msg_Passing.Letter);

private

   ---------------------------------------------------------------------
   --  protected spec Mutex
   ---------------------------------------------------------------------
   protected type Mutex is

      ------------------------------------------------------------------
      --  Blackboard.Get
      --
      --  Reads the message on the blackboard and erases it. If there is
      --  no message, the calling task is blocked until a new message is
      --  written on the blackboard.
      --
      ------------------------------------------------------------------
      entry Get (Message : out Letter);

      ------------------------------------------------------------------
      --  Blackboard.Put
      --
      --  Writes a message to the blackboard. If the last message hasn't
      --  been read yet, the calling task is blocked until the previous
      --  message has been read and erased.
      --
      ------------------------------------------------------------------
      entry Put (Message : in Letter);

   private

      Msg_Available : Boolean := False;
      The_Message   : Letter;

   end Mutex;

   ---------------------------------------------------------------------
   --  Object type completion.
   ---------------------------------------------------------------------
   type Object is new Msg_Passing.Object with
      record
         Locked : Mutex;
      end record;

end Msg_Passing.Blackboard;
