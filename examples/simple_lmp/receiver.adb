------------------------------------------------------------------------
--  Copyright (C) 2010-2020 by Heisenbug Ltd. (github@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under
--  the terms of the Do What The Fuck You Want To Public License,
--  Version 2, as published by Sam Hocevar. See the LICENSE file for
--  more details.
------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.IO_Exceptions;
with Ada.Text_IO; --  Using Ada.Text_IO in tasking context is not task
                  --  safe, but use it for the sake of a simple example.

with Local_Message_Passing;
with Message_Types;

------------------------------------------------------------------------
--  The receiver task package.
------------------------------------------------------------------------
package body Receiver is

   --  Instantiate the Local_Message_Passing package, so we get the
   --  Mailbox type and Handle type to access the mailbox.
   package LMP is new
     Local_Message_Passing (Message => Message_Types.The_Message);

   --  A mailbox should be declared at library level.
   --
   --  It is perfectly possible to declare it locally in the task, but
   --  if the task dies, any other task accessing the mailbox via an
   --  acquired handle to it will then access invalid memory.

   The_Mailbox : LMP.Mailbox (Size => 8); --  Declares our mailbox.

   task Receive_Messages;
   task body Receive_Messages is
      MB          : LMP.Handle;
      Our_Message : Message_Types.The_Message;
      use type Message_Types.The_Message;
   begin
      Ada.Text_IO.Put_Line
        ("Receiver: Waiting a second before exporting mailbox.");
      delay 1.0;

      --  Export the mailbox. This also initializes our Handle.
      LMP.Open_Mailbox (Mbx         => The_Mailbox,
                        Hnd         => MB,
                        Export_Name => "MY_MAILBOX");
      Ada.Text_IO.Put_Line
        ("Receiver: Mailbox exported, sender task should see it now.");

      loop
         --  Blocking receive. Waits until a message becomes available.
         LMP.Receive (Mbx => MB,
                      Msg => Our_Message);

         if Our_Message (1 .. 16) = "Can you hear me?" then
            Ada.Text_IO.Put_Line ("Loud and clear.");
         else
            raise Ada.IO_Exceptions.Data_Error;
         end if;
      end loop;
   exception
      when Ada.IO_Exceptions.Data_Error =>
         Ada.Text_IO.Put_Line ("Receiver died: Unexpected message!");
   end Receive_Messages;

end Receiver;
