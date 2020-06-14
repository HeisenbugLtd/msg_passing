# Message Passing
Message passing with mailboxes with no dynamic memory allocation.

This is an old proof-of-concept project which I dusted off from the attic.

## Purpose

The core of the library is what I dubbed *Local Message Passing*, i.e.
Ada task communication via generic shared mailboxes, aka. message queues.

Everything here is bounded (it was initially targeted for a restricted embedded
system), so mailboxes are allocated statically with a discriminant denoting
their size. In most instances where you would want to use that, this is not a
restriction, but rather a feature.

## The Trick

So far, so simple. The trick used here is that mailboxes are never referred to
them directly, you only refer to them by a unique name and the user of the
mailbox (either sender or receiver) is not even aware of in which package the
other communicating party is declared. That allows for easy reshuffling of
computing nodes and guaranteed separation of communicating tasks. The core of
this implementation can be seen in `mailbox_sharing.adb`, which uses some
rather dirty tricks under the hood to circumvent some of Ada's type safety to
actually implement that possibility.

## Usage

As said, mailboxes are referred to by name, so there are no (visible) pointers
or such involved.

To create a communication between two tasks that don't even know that the other
party exists, one needs to export the mailbox (assigning a name to it), while
the other one can import it using the same name.

The nice thing is that the order of exports and imports does not matter,
because importing a mailbox implies waiting for it until it becomes available
(you can specify a timeout, of course).

## Example

```ada
package Message_Types is

   type The_Message is ...;

end Message_Types;
```

```ada
package Sender with Elaborate_Body is

  --  task and mailbox are both declared in the body, so nobody is aware that
  --  such even exist.

end;
```

Same for a receiver, so I am omitting that package spec.


Now the `Sender`'s body:

```ada
with Local_Message_Passing;
with Message_Types;

package body Sender is

   --  Instantiate the Local_Message_Passing package, so we can declare
   --  mailboxes.
   package Mailboxes is
     new Local_Message_Passing (Message => Message_Types.The_Message);
   --  The only thing receiver and sender should agree on is the message
   --  type being exchanged.

   --  Instantiate a mailbox with space for 16 messages.
   My_Mailbox : Mailboxes.Mailbox (Size => 16);
   Handle     : Mailboxes.Handle;

   --  other needed stuff ...

   --  ... in some task
   
      --  Initialize and export the mailbox.
      --  After that you get a handle to the mailbox which you can use
      --  to send and receive messages and the mailbox becomes available
      --  to other clients.
      Mailboxes.Open_Mailbox (Mbx         => My_Mailbox,
                              Hnd         => Handle,
                              Export_Name => "MY_MAILBOX");

      loop
        -- Send a message.
        Mailboxes.Send (Handle, Message);
        
        ...
      end loop;
   
end Sender;
```

Now, the receiver:

```ada
with Local_Message_Passing;
with Message_Types;

package body Receiver is

   --  Instantiate the Local_Message_Passing package, so we can declare
   --  mailbox handles.
   package Mailboxes is
     new Local_Message_Passing (Message => Message_Types.The_Message);

   --  The mailbox resides in the Sender task, so we don't declare a
   --  mailbox, but a handle to one.
   Handle : Mailboxes.Handle;

   --  other needed stuff ...

   --  ... in some task
   
      --  Try importing the mailbox for at most 10 seconds.
      --
      --  Here you will either get a handle to the mailbox which you can use
      --  to (send and) receive messages, or a No_Such_Mailbox exception will
      --  be raised.
      Mailboxes.Import_Mailbox
        (Name     => "MY_MAILBOX",
         Hnd      => M,
         Max_Wait => Ada.Real_Time.Milliseconds (10000));

      loop
        -- Receive message from other tasks.
        Mailboxes.Receive (Handle, Message);

        ...
      end loop;
   
end Receiver;
```

Note that the `Mailboxes` instances are two different ones (which is not
strictly necessary, but convenient), and the tasks don't know about each
other. Talk about decoupling.

Mailboxes are bi-directional, the can be exported/imported by any task, no
matter if they are receiving or sending and they can be used by multiple tasks
each (e.g. you can have 2 tasks sending to the same mailbox, and 5 receivers).

Of course, there should only be one mailbox object declared, so only one task
can "own" the mailbox. You just should agree on which one.
The recommended pattern is that the server (i.e. Reader task) which processes the messages owns the mailbox.

If you need two-way communication between tasks, you need two mailboxes, of course, otherwise a task would end up sending messages to itself.
Don't forget that in that case you may run into deadlocks, having cycles in the mailbox communication should be avoided and at the very least very carefully reviewed.

## Compatibility

Unless I made a mistake when dusting off this stuff, this is about 100%
portable Ada95 code. Back when I wrote that we did not have an Ada2005 compiler
for our target. Hence, no container library, so the queue types are done by
hand.

