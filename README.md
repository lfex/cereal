# cereal

*An LFE NIF and get_server for serial communications*

<img src="resources/images/johnny-automatic-cereal-box-and-milk-small.png" />

The LFE code in this app was converted from the Erlang code from which this
repo was forked. It has since been updated with additional functions, so the
APIs differ.

The C used in this app was inspired by and in some cases
copy/pasted from https://github.com/tonyg/erlang-serial


# Usage

To use ``cereal``, you start a connection to the serial device by passing
the full path to the device name:

```cl
> (set tty (cereal:start "/dev/tty.usbserial-DA01L2I5"))
<0.32.0>
```

You may also pass options when starting a connection:
```cl
> (set tty (cereal:start "/dev/tty.usbserial-DA01L2I5" `(#(speed 9600))))
<0.32.0>
```

Valid options are:

* ``#(speed <int> <int>)`` (in-speed, out-speed)
* ``#(speed <int>)`` (both ways the same)
* ``#(parity odd)``
* ``#(parity even)``

Now you can use the library:

```cl
> (cereal:send tty (binary #x7e #x00 #x04 #x08 #x52 #x4e #x4a #x0d))
#(send #B(126 0 4 8 82 78 74 13))
```

Then you can check for a response:

```cl
> (c:flush)
Shell got {data,<<136,82,78,74,2,139>>}
ok
```

When you're finished, be sure to stop cereal (in order to prevent memory leaks):

```cl
> (cereal:stop tty)
#(stop)
```
