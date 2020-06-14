------------------------------------------------------------------------
--  Copyright (C) 2010-2020 by Heisenbug Ltd. (github@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under
--  the terms of the Do What The Fuck You Want To Public License,
--  Version 2, as published by Sam Hocevar. See the LICENSE file for
--  more details.
------------------------------------------------------------------------
pragma License (Unrestricted);

with Msg_Passing;
with Msg_Passing.Blackboard;
with Msg_Passing.Queue;
with Msg_Passing.Whiteboard;

package Msg_Producer is

   --  Type for exchanged messages.
   type Message is new Integer;

   --  Instantiate generic parent (root package) with message type.
   package Int_Messaging is new Msg_Passing (Letter => Message);

   --  Instantiate appropriate message board packages.
   package BB_Mailbox is new Int_Messaging.Blackboard;
   package WB_Mailbox is new Int_Messaging.Whiteboard;
   package Qu_Mailbox is new Int_Messaging.Queue (Capacity => 32);

   ---------------------------------------------------------------------
   --  Start
   --
   --  Starts the message producing task.
   --
   --  The scope of the Msg_Board.Object in parameter MB must be global.
   ---------------------------------------------------------------------
   procedure Start (Msg_Rate : in     Integer;
                    MB       : in out Int_Messaging.Object'Class);

end Msg_Producer;
