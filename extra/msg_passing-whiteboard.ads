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
--  Msg_Passing.Whiteboard
--
--  This package provides message passing based on whiteboard semantics.
--
--  This means, there is only one sender, and one message at a time, but
--  there can be multiple receivers (including none!) of the same
--  message. The message must be erased explicitely. In other words,
--  it's like posting a message and anyone passing by may read it until
--  it disappears again.
--
------------------------------------------------------------------------

generic

package Msg_Passing.Whiteboard is

   pragma Pure;

   ---------------------------------------------------------------------
   --  Whiteboard messaging object
   ---------------------------------------------------------------------
   type Object is new Msg_Passing.Object with private;

   ---------------------------------------------------------------------
   --  Object.Read
   ---------------------------------------------------------------------
   procedure Read (Instance : in out Object;
                   Message  :    out Letter);

   ---------------------------------------------------------------------
   --  Object.Write
   ---------------------------------------------------------------------
   procedure Write (Instance : in out Object;
                    Message  : in     Letter);

private

   ---------------------------------------------------------------------
   --  protected spec Mutex
   ---------------------------------------------------------------------
   protected type Mutex is

      ------------------------------------------------------------------
      --  Whiteboard.Put
      --
      --  Writes a message to the whiteboard. If the last message hasn't
      --  been erased yet, the calling task is blocked until the
      --  previous message has been erased.
      --
      ------------------------------------------------------------------
      entry Put (Message : in Letter);

      ------------------------------------------------------------------
      --  Whiteboard.Get
      --
      --  Reads the message on the whiteboard. If there is no message,
      --  the calling task is blocked until a new message is written on
      --  the whiteboard.
      --
      ------------------------------------------------------------------
      entry Get (Message : out Letter);

      ------------------------------------------------------------------
      --  Whiteboard.Erase
      --
      --  Erases the message on the whiteboard.
      --
      ------------------------------------------------------------------
      procedure Erase;

   private

      Msg_Available : Boolean := False;
      The_Message   : Letter;

   end Mutex;

   ---------------------------------------------------------------------
   --  Object type completion
   ---------------------------------------------------------------------
   type Object is new Msg_Passing.Object with
      record
         Locked : Mutex;
      end record;

end Msg_Passing.Whiteboard;
