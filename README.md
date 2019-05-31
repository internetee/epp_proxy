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
The application leverages OTP to isolate EPP clients from one another, each connection is owned by its own process. There are two processes that
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
to make migration easier. The configuration is placed in `config/sys.config` file, it takes a format
of Erlang property list.

*Configuration variables*

| Variable name        | Expected values                    | Apache equivalent     | Definition
-----------------------|------------------------------------|-----------------------|--------------------------------------------
| `dev_mode`           | `true`, `false`                    | None                  | Enables TCP access without TLS.
| `tls_port`           | `700`                              | Listen                | At which port should we open a TLS socket. Default is 700.
| `tcp_port`           | `70000`                            | Listen                | At which port should we open a TCP socket. Only in `dev_mode`.
| `epp_session_url`    | `https://example.com/epp/session`  | EppSessionRoot        | HTTP address of the session endpoints including schema and port.
| `epp_command_url`    | `https://example.com/epp/command`  | EppCommandRoot        | HTTP address of the command endpoints including schema and port.
| `epp_error_url`      | `https://example.com/epp/error`    | EppErrorRoot          | HTTP address of the error endpoints including schema and port.
| `cacertfile_path`    | `/opt/ca/ca.crt.pem`               | SSLCACertificateFile  | Where is the client root CA located.
| `certfile_path`      | `/opt/ca/server.crt.pem`           | SSLCertificateFile    | Where is the server certificate located.
| `keyfile_path`       | `/opt/ca/server.key.pem`           | SSLCertificateKeyFile | Where is the server key located.
| `crlfile_path`       | `/opt/ca/crl.pem`                  | SSLCARevocationFile   | Where is the CRL file located.


Migrating from mod_epp
----

Checklist of steps to perform if you want to migrate from mod_epp, but still use Apache to be a reverse proxy.

1. Remove SSL-CLIENT-S-DN-CN and SSL-CLIENT-CERT headers from Apache. Epp_proxy takes care of those.
2. Install this project to desired location.
3. Ensure that the user who will run epp_proxy has access to all certificate files.
4. Set up syslog in `config/sys.config`.
5. If you do not feel comfortable using Erlang configuration file, you can use command line arguments and flags in format of `/epp_proxy/rel/bin/epp_proxy -epp_proxy tls_port 444`, where `-epp_proxy` is name of application,
   followed by configuration parameter name and value.

TODO
----

1. DONE -- Add syslog logger
2. DONE -- Add default error responses for errors while contacting registry and for invalid XML.
3. DONE -- This readme.
4. DONE -- Migration guide for mod epp.
5. Ensure CRL gets updated periodically without a need for restart of the service
