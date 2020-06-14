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
--  Local_Message_Passing
--
--  Generic package providing mailbox communication channels.
--
--  All mailboxes instantiated from this package can be shared by name
--  between different tasks without needing the specific package where
--  the mailbox is located.
--
--  The order of exporting and importing calls is not important, the
--  underlying mailbox sharing mechanism takes care of suspending a
--  caller until its requested mailbox becomes available.
--
------------------------------------------------------------------------

with Ada.Real_Time;

with Mailbox_Sharing;

generic

   type Message is private;

package Local_Message_Passing is

   --  A mailbox with this name has already been exported.
   Already_Exported : exception renames
     Mailbox_Sharing.Already_Exported;

   --  If a mailbox can not be connected to within the given duration,
   --  this exception will be raised.
   No_Such_Mailbox : exception renames
     Mailbox_Sharing.No_Such_Mailbox;

   --  If more than the supported number of mailboxes (see package
   --  Mailbox_Sharing) are exported, this exception will be raised.
   Too_Many_Mailboxes : exception renames
     Mailbox_Sharing.Too_Many_Mailboxes;

   ---------------------------------------------------------------------
   --  Mailbox
   --
   --  A package can own a mailbox by declaring an object of type
   --  Mailbox with an appropriate queue length (Size).
   --
   --  Please note that a size of 0 is supported in the technical
   --  sense, but then the queue will always be empty and full at the
   --  same time, any calling task will block on both send and receive.
   ---------------------------------------------------------------------
   type Mailbox (Size : Natural) is limited private;

   ---------------------------------------------------------------------
   --  Handle
   --
   --  To actually use the mailbox, "Open_Mailbox" must be called to
   --  create a handle to it. The mailbox itself has no operations at
   --  all, all operations on the mailbox are done through the handle.
   --
   --  To make the mailbox available to other processes, it must be
   --  exported with an unique name. Any process wishing to connect to
   --  such a published mailbox may call "Import_Mailbox" then, which -
   --  upon success - makes a handle to that mailbox available to the
   --  calling process.
   --
   --  Because the handle type is also private, uses like
   --  "Export_Mailbox (Handle.all)" are not allowed (outside of this
   --  package or children, that is), thus only the owning package is
   --  able to create such a handle directly from a mailbox declaration
   --  (unless its mailbox declaration would be made public).
   --
   --  Also make it limited to disallow free copying of this handle.
   --  It is technically safe to pass around this handle once it has
   --  been properly obtained (after all, it's just a reference), but
   --  there is also no technical reason to do so.
   ---------------------------------------------------------------------
   type Handle is limited private;

   ---------------------------------------------------------------------
   --  Open_Mailbox
   --
   --  "Constructor" function.
   --  Re-Initialises the mailbox for use and also "returns" its handle.
   --  If Export_Name is given, the mailbox is exported via the (local)
   --  sharing mechanism.
   ---------------------------------------------------------------------
   procedure Open_Mailbox (Mbx         : in out Mailbox;
                           Hnd         :    out Handle;
                           Export_Name : in     String := "");

   ---------------------------------------------------------------------
   --  Import_Mailbox
   --
   --  Imports a named mailbox for use by calling process.
   --
   --  If the name for the mailbox does not exist and does not become
   --  available during the given Max_Wait timespan, No_Such_Mailbox
   --  will be raised.
   ---------------------------------------------------------------------
   procedure Import_Mailbox (Name     : in     String;
                             Hnd      :    out Handle;
                             Max_Wait : in     Ada.Real_Time.Time_Span);

   --
   --  Actual messaging functions.
   --

   ---------------------------------------------------------------------
   --  Conditional_Send
   --
   --  Puts a message into the mailbox, if it is not full yet.
   --  Returns True for the Success parameter if the message has been
   --  deposited.
   ---------------------------------------------------------------------
   procedure Conditional_Send (Mbx     : in     Handle;
                               Msg     : in     Message;
                               Success :    out Boolean);

   ---------------------------------------------------------------------
   --  Send
   --
   --  Puts a message into the mailbox. If the mailbox is full, the
   --  caller gets blocked until a slot becomes available.
   ---------------------------------------------------------------------
   procedure Send (Mbx : in Handle;
                   Msg : in Message);

   ---------------------------------------------------------------------
   --  Timed_Send (Send_Until)
   --
   --  Puts a message into the mailbox. Just like Send it blocks, but at
   --  most until the time specified.
   ---------------------------------------------------------------------
   procedure Timed_Send (Mbx     : in     Handle;
                         Msg     : in     Message;
                         Latest  : in     Ada.Real_Time.Time;
                         Success :    out Boolean);

   ---------------------------------------------------------------------
   --  Timed_Send (Send_For)
   --
   --  Puts a message into the mailbox. Just like Send it blocks, but at
   --  most for the time span specified.
   ---------------------------------------------------------------------
   procedure Timed_Send (Mbx      : in     Handle;
                         Msg      : in     Message;
                         Max_Wait : in     Ada.Real_Time.Time_Span;
                         Success  :    out Boolean);

   ---------------------------------------------------------------------
   --  Conditional_Receive
   --
   --  Retrieves a message from the mailbox. If the mailbox is empty,
   --  the Success parameter will be False. If it is True the message
   --  parameter will contain the message received.
   ---------------------------------------------------------------------
   procedure Conditional_Receive (Mbx     : in     Handle;
                                  Msg     :    out Message;
                                  Success :    out Boolean);

   ---------------------------------------------------------------------
   --  Receive
   --
   --  Retrieves a message from the mailbox. If the mailbox is empty,
   --  the caller gets blocked until a message becomes available.
   ---------------------------------------------------------------------
   procedure Receive (Mbx : in     Handle;
                      Msg :    out Message);

   ---------------------------------------------------------------------
   --  Timed_Receive (Receive_Until)
   --
   --  Retrieves a message from the mailbox. Just like Receive it blocks
   --  until a message becomes available, but at most until the time
   --  specified.
   ---------------------------------------------------------------------
   procedure Timed_Receive (Mbx     : in     Handle;
                            Msg     :    out Message;
                            Latest  : in     Ada.Real_Time.Time;
                            Success :    out Boolean);

   ---------------------------------------------------------------------
   --  Timed_Receive (Receive_For)
   --
   --  Retrieves a message from the mailbox. Just like Receive it blocks
   --  until a message becomes available, but at most for the time span
   --  specified.
   ---------------------------------------------------------------------
   procedure Timed_Receive (Mbx      : in     Handle;
                            Msg      :    out Message;
                            Max_Wait : in     Ada.Real_Time.Time_Span;
                            Success  :    out Boolean);

private

   ---------------------------------------------------------------------
   --  Message_Queue
   --
   --  All messages of a mailbox are stored in a fixed size queue.
   ---------------------------------------------------------------------
   type Message_Queue is array (Positive range <>) of Message;

   ---------------------------------------------------------------------
   --  Handle
   --
   --  The actual mailbox handle.
   ---------------------------------------------------------------------
   type Handle is access all Mailbox;

   ---------------------------------------------------------------------
   --  Mailbox
   --
   --  A mailbox is implemented as a protected type.
   ---------------------------------------------------------------------
   protected type Mailbox (Size : Natural) is

      ------------------------------------------------------------------
      --  Mailbox.Clear
      --
      --  Initialisation. Clears the internal queue.
      ------------------------------------------------------------------
      procedure Clear;

      ------------------------------------------------------------------
      --  Mailbox.Try_Put
      --
      --  Unblocking send.
      ------------------------------------------------------------------
      procedure Try_Put (Msg     : in     Message;
                         Success :    out Boolean);

      ------------------------------------------------------------------
      --  Mailbox.Put
      --
      --  Blocking send.
      ------------------------------------------------------------------
      entry Put (Msg : in Message);

      ------------------------------------------------------------------
      --  Mailbox.Try_Get
      --
      --  Unblocking read.
      ------------------------------------------------------------------
      procedure Try_Get (Msg     : out Message;
                         Success : out Boolean);

      ------------------------------------------------------------------
      --  Mailbox.Get
      --
      --  Blocking read.
      ------------------------------------------------------------------
      entry Get (Msg : out Message);

      --  Reading virtual channels shall only happen for the mailbox
      --  owner. This means, we can bind the Reader VC onto the mailbox
      --  type instead of the handle.

   private
      --  The message queue if the mailbox is used locally (same CPU).
      Msg_Queue : Message_Queue (1 .. Size);

      --  The queue is implemented like a ring buffer so that always the
      --  oldest message is retrieved if there is more than one. Thus we
      --  need to keep track of the beginning and end of the buffer.
      --
      --  The default values here are only given for safety purposes.
      --  It is expected that before using a mailbox "Init_Mailbox" is
      --  called which re-initialises the mailbox.
      Num_Messages : Natural := 0;
      Latest       : Natural := 1;
      Oldest       : Natural := 1;
   end Mailbox;

end Local_Message_Passing;
