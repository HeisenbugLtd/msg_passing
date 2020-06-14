------------------------------------------------------------------------
--  Copyright (C) 2010-2020 by Heisenbug Ltd. (github@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under
--  the terms of the Do What The Fuck You Want To Public License,
--  Version 2, as published by Sam Hocevar. See the LICENSE file for
--  more details.
------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Text_IO;
with Msg_Producer;

procedure Msg_Consumer is
   MESSAGE_RATE : constant := 1;

   Msg       : Msg_Producer.Message;
   Msg_Board : Msg_Producer.BB_Mailbox.Object;

   --  Rename base class (root type) for easier view conversion.
   subtype Object is Msg_Producer.Int_Messaging.Object;
begin
   Ada.Text_IO.Put_Line ("Start producing messages...");
   Msg_Producer.Start (Msg_Rate => MESSAGE_RATE,
                       MB       => Msg_Board);

   Ada.Text_IO.Put_Line ("Start consuming messages...");

   loop
      Msg_Producer.Int_Messaging.Read (Object'Class (Msg_Board), Msg);

      Ada.Text_IO.Put (Msg_Producer.Message'Image (Msg));

      --  If Msg_Board is an instance of Whiteboard, Erase should be
      --  called from time to time or else we're just looping around
      --  eating all CPU.

      --  delay 1.0;
   end loop;

end Msg_Consumer;
