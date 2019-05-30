epp_proxy
=====

An application that translates EPP over TCP connection into set of predefined HTTP calls.

Serves as direct replacement for mod_epp (https://github.com/mod-epp/mod-epp), and tries to
emulate it's behaviour to the biggest possible extent.

Aside from the standard library of Erlang/OTP, it uses hackney for making HTTP requests
and lager for logging.

Design
----

Translation of EPP calls
-----
The application listens on predefined TCP port for formatted EPP frames and translates them into
HTTP requests according to the following matrix. Application performs regex search for clTRID and an
XPATH search for command name. It does not check against any XSD schema.

| EPP Command | HTTP request                     | Parameters          | Headers                                                             | Payload Type   | Cookies |
|-------------|----------------------------------|---------------------|---------------------------------------------------------------------|----------------|---------|
| hello       | `GET /epp_session_url/hello`     |                     |  SSL_CLIENT_CERT, SSL_CLIENT_S_DN_CN, User-Agent, X-Forwarded-for   |                | session |
| login       | `POST /epp_session_url/login`    | `raw_frame, clTRID` |  SSL_CLIENT_CERT, SSL_CLIENT_S_DN_CN, User-Agent, X-Forwarded-for   | form/multipart | session |
| logout      | `POST /epp_session_url/logout`   | `raw_frame, clTRID` |  SSL_CLIENT_CERT, SSL_CLIENT_S_DN_CN, User-Agent, X-Forwarded-for   | form/multipart | session |
| poll        | `POST /epp_command_url/poll`     | `raw_frame, clTRID` |  SSL_CLIENT_CERT, SSL_CLIENT_S_DN_CN, User-Agent, X-Forwarded-for   | form/multipart | session |
| check       | `POST /epp_command_url/check`    | `raw_frame, clTRID` |  SSL_CLIENT_CERT, SSL_CLIENT_S_DN_CN, User-Agent, X-Forwarded-for   | form/multipart | session |
| info        | `POST /epp_command_url/info`     | `raw_frame, clTRID` |  SSL_CLIENT_CERT, SSL_CLIENT_S_DN_CN, User-Agent, X-Forwarded-for   | form/multipart | session |
| create      | `POST /epp_command_url/create`   | `raw_frame, clTRID` |  SSL_CLIENT_CERT, SSL_CLIENT_S_DN_CN, User-Agent, X-Forwarded-for   | form/multipart | session |
| delete      | `POST /epp_command_url/delete`   | `raw_frame, clTRID` |  SSL_CLIENT_CERT, SSL_CLIENT_S_DN_CN, User-Agent, X-Forwarded-for   | form/multipart | session |
| renew       | `POST /epp_command_url/renew`    | `raw_frame, clTRID` |  SSL_CLIENT_CERT, SSL_CLIENT_S_DN_CN, User-Agent, X-Forwarded-for   | form/multipart | session |
| update      | `POST /epp_command_url/update`   | `raw_frame, clTRID` |  SSL_CLIENT_CERT, SSL_CLIENT_S_DN_CN, User-Agent, X-Forwarded-for   | form/multipart | session |
| ANY (error) | `GET /epp_error_url`             | `msg, code, clTRID` |  SSL_CLIENT_CERT, SSL_CLIENT_S_DN_CN, User-Agent, X-Forwarded-for   | query params   | session |

Error route is used in case EPP frame is malformed.

The application performs TLS handshake, checks certficate against provided revocation list and acts as
TLS termination proxy, passing on the certificate information downstream as HTTP headers. It also passes
on the client IP address.

Supervision Tree
-----
The application leverages OTP to isolate EPP clients from one another, each connection is isolated from others. There are two processes that
accept the connections and spin off workers, those are supervised in "one for one" strategy.

Build
-----
You need Erlang/OTP release 21 and Rebar3 to build it. No other versions than 21 were tested.

    $ rebar3 compile

epp_proxy should be deployed as a self-contained Erlang application (release). You can create one
with one of the following commands:

```bash
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
