# cereal

*An LFE NIF for serial port communications*

<img src="resources/images/johnny-automatic-cereal-box-and-milk-small.png" />

The LFE code in this app was converted from the Erlang code from which this
repo was forked. It has since been updated with additional functions, so the
APIs differ.

The C used in this app was inspired by and in some cases
copy/pasted from https://github.com/tonyg/erlang-serial


# Usage

To use ``cereal``, the first thing you need to do is start it:

```cl
> (cereal:start)
ok
```

Now you can open a connection to the serial deivce of your choice:

```cl
> (cereal:open "/dev/tty.usbserial-DA01L2I5")
<0.32.0>
```

You may also pass options when starting a connection:
```cl
> (cereal:start "/dev/tty.usbserial-DA01L2I5" `(#(speed 9600)))
<0.32.0>
```

Valid options are:

* ``#(speed <int> <int>)`` (in-speed, out-speed)
* ``#(speed <int>)`` (both ways the same)
* ``#(parity odd)``
* ``#(parity even)``

Now you can use the library:

```cl
> (cereal:send (binary #x7e #x00 #x04 #x08 #x52 #x4e #x4a #x0d))
#B(126 0 5 136 82 78 74 2 139)
```

Responses are streamed without packet length information, so you will need
to implement a protocol that allows you to define the end of a packet.

```

When you're finished, be sure to close the cereal connection:

```cl
> (cereal:close)
#(ok stopped)
```
