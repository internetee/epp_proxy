epp_proxy
=====

An application that translates EPP over TCP connection into set of predefined HTTP calls.

Serves as direct replacement for mod_epp (https://github.com/mod-epp/mod-epp), and tries to
emulate it's behaviour to the biggest possible extent.

Aside from the standard library of Erlang/OTP, it uses hackney for making HTTP requests
and lager for logging.

Build
-----
You need Erlang/OTP release 21 and Rebar3 to build it. No other versions than 21 were tested.

    $ rebar3 compile

epp_proxy should be deployed as a self-contained Erlang application (release). You can create one
with one of the following commands:

```
$ rebar3 release # Creates a release locally.
$ rebar3 tar # Creates an archive that can be shipped to another machine
```

Configuration
-----
Configuration for the application tries to emulate the mod_epp configuration as close as possible
to make migration easier.


Deployment
-----


TODO
----

1. DONE -- Add syslog logger
2. DONE -- Add default error responses for errors while contacting registry and for invalid XML.
3. This readme.
3. Migration guide for mod epp.
