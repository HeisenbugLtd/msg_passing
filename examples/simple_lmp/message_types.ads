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
--  The only thing Sender and Receiver share directly is the type of
--  message being exchanged.
------------------------------------------------------------------------
package Message_Types with
  Pure         => True,
  Remote_Types => True
is

   --  We only support definite types, so the message is a string of 30
   --  characters.
   type The_Message is new String (1 .. 30);

end Message_Types;
