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
--  Msg_Passing
--
--  Root package for the several message passing algorithms, like
--  blackboards, whiteboards, and queues.
--
------------------------------------------------------------------------
generic

   --  Type of message being exchanged.
   --  May be any constrained type.
   type Letter is private;

package Msg_Passing is

   pragma Pure;

   ---------------------------------------------------------------------
   --  The messenger object
   ---------------------------------------------------------------------
   type Object is abstract tagged limited private;

   ---------------------------------------------------------------------
   --  Object.Read
   ---------------------------------------------------------------------
   procedure Read  (Instance : in out Object;
                    Message  :    out Letter) is abstract;

   ---------------------------------------------------------------------
   --  Object.Write
   ---------------------------------------------------------------------
   procedure Write (Instance : in out Object;
                    Message  : in     Letter) is abstract;

private

   type Object is abstract tagged limited null record;

end Msg_Passing;
