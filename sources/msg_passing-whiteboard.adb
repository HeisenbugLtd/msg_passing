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
--  Implementation of message passing via whiteboards.
--
------------------------------------------------------------------------

package body Msg_Passing.Whiteboard is

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
      --  Whiteboard.Put
      ------------------------------------------------------------------
      entry Put (Message : in Letter) when not Msg_Available is
      begin
         The_Message   := Message;
         Msg_Available := True;
      end Put;

      ------------------------------------------------------------------
      --  Whiteboard.Get
      ------------------------------------------------------------------
      entry Get (Message : out Letter) when Msg_Available is
      begin
         Message := The_Message;
      end Get;

      ------------------------------------------------------------------
      --  Whiteboard.Erase
      ------------------------------------------------------------------
      procedure Erase is
      begin
         Msg_Available := False;
      end Erase;

   end Mutex;

end Msg_Passing.Whiteboard;
