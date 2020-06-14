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
--  A very simple program to show how the Local_Message_Passing works.
--
--  The actual implementation is in the Sender and Receiver packages.
------------------------------------------------------------------------
with Sender;
with Receiver;
--  Include the sender and receiver tasks.

procedure Simple_LMP is
begin
   --  Sender and Receiver have their own tasks, we don't need to do
   --  anything here.
   --  In fact, this program never ends.

   --  When running this program, you should see the text output
   --  "Loud and clear." at regular intervals.  This is the confirmation
   --  that the receiver task actually received the expected message
   --  "Do you hear me?" from the sender task.
   null;
end Simple_LMP;
