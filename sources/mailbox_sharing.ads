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
--  Mailbox_Sharing
--
--  Supports import and export of mailboxes from any instantiation of
--  the generic Local_Message_Passing package.
--
--  This package should be viewed and used like a private child of
--  Local_Message_Passing, meaning it should not be used by user code at
--  all!
--
------------------------------------------------------------------------

with Ada.Real_Time;
with System.Storage_Elements;

package Mailbox_Sharing is

   package SSE renames System.Storage_Elements;

   --
   --  Package configuration. May be adapted for different purposes.
   --

   --  Configured maximum number of mailboxes to handle.
   MAX_MAILBOXES   : constant Positive := 100;

   --  Number of significant characters in mailbox names.
   MAX_NAME_LENGTH : constant Positive :=  31;

   --
   --  Several exception objects to indicate erroneous situations.
   --

   --  A mailbox with this name has already been exported.
   Already_Exported   : exception;

   --  There are no more slots in the static list to export a mailbox.
   --  MAX_MAILBOXES should be increased.
   Too_Many_Mailboxes : exception;

   --  Raised when the maximum timeout for a connection attempt is gone
   --  by and the requested mailbox has not been exported yet.
   No_Such_Mailbox    : exception;

   ---------------------------------------------------------------------
   --  Add_Mailbox
   --
   --  Adds a mailbox to the global pool. Each mailbox name can be
   --  exported once only.
   ---------------------------------------------------------------------
   procedure Add_Mailbox (Mbx_Address : in System.Address;
                          Name        : in String;
                          Msg_Size    : in SSE.Storage_Count);

   ---------------------------------------------------------------------
   --  Find_Mailbox
   --
   --  Finds a mailbox in the global pool up until the time specified.
   --  For a successful connection both name and message size of the
   --  mailbox must match.
   ---------------------------------------------------------------------
   function Find_Mailbox
     (Name     : in String;
      Msg_Size : in SSE.Storage_Count;
      Latest   : in Ada.Real_Time.Time) return System.Address;

end Mailbox_Sharing;
