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
with Ada.Text_IO; --  Using Ada.Text_IO in tasking context is not
                  --  safe, but use it for the sake of a simple example.
with Local_Message_Passing;
with Message_Types;

------------------------------------------------------------------------
--  The sender task package.
------------------------------------------------------------------------
package body Sender is

   --  Instantiate the Local_Message_Passing package, so we get the
   --  Handle type to access a mailbox.
   package LMP is new
     Local_Message_Passing (Message => Message_Types.The_Message);

   Our_Message : constant Message_Types.The_Message :=
     "Can you hear me?              ";

   task Send_Messages;
   task body Send_Messages is
      MB : LMP.Handle;
   begin
      Ada.Text_IO.Put_Line
        ("Sender: Waiting for mailbox to get exported...");

      --  Import the mailbox. This initializes our Handle.
      --  In this example, the receiver task delays for a second before
      --  exporting the mailbox, so you should see a small delay.
      LMP.Import_Mailbox (Name     => "MY_MAILBOX",
                          Hnd      => MB,
                          Max_Wait => Ada.Real_Time.Milliseconds (2000));
      Ada.Text_IO.Put_Line
        ("Sender: Mailbox found, I start sending messages now.");

      loop
         --  Blocking send. Waits until there is space in the message
         --  queue.
         LMP.Send (Mbx => MB,
                   Msg => Our_Message);
         delay 1.0; --  Wait a second... (literally)
      end loop;
   exception
      when LMP.No_Such_Mailbox =>
         Ada.Text_IO.Put_Line ("Sender died: Could not find mailbox!");
   end Send_Messages;

end Sender;
