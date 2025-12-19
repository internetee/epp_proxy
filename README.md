epp_proxy
=====

[![Build Status](https://travis-ci.org/internetee/epp_proxy.svg?branch=master)](https://travis-ci.org/internetee/epp_proxy)

An application that translates EPP over TCP connection into set of predefined HTTP calls.

Serves as direct replacement for mod_epp (https://github.com/mod-epp/mod-epp), and tries to
emulate it's behaviour to the biggest possible extent.

Aside from the standard library of Erlang/OTP, it uses hackney for making HTTP requests
and lager for logging.

Code style
----
We enforce the style from Erlang's own configuration. You can use the rebar3_fmt plugin to do it for you:

    $ rebar3 fmt


Design
----

Translation of EPP calls
-----
The application listens on predefined TCP port for formatted EPP frames and translates them into
HTTP requests according to the following matrix. Application performs regex search for clTRID and an
XPATH search for command name. It does not check against any XSD schema.

| EPP Command | HTTP request                     | Parameters                 | Headers                                                             | Payload Type   | Cookies |
|-------------|----------------------------------|----------------------------|---------------------------------------------------------------------|----------------|---------|
| hello       | `GET /epp_session_url/hello`     |                            |  SSL-CLIENT-CERT, SSL-CLIENT-S-DN-CN, User-Agent, X-Forwarded-for   |                | session |
| login       | `POST /epp_session_url/login`    | `raw_frame, frame, clTRID` |  SSL-CLIENT-CERT, SSL-CLIENT-S-DN-CN, User-Agent, X-Forwarded-for   | form/multipart | session |
| logout      | `POST /epp_session_url/logout`   | `raw_frame, frame, clTRID` |  SSL-CLIENT-CERT, SSL-CLIENT-S-DN-CN, User-Agent, X-Forwarded-for   | form/multipart | session |
| poll        | `POST /epp_command_url/poll`     | `raw_frame, frame, clTRID` |  SSL-CLIENT-CERT, SSL-CLIENT-S-DN-CN, User-Agent, X-Forwarded-for   | form/multipart | session |
| check       | `POST /epp_command_url/check`    | `raw_frame, frame, clTRID` |  SSL-CLIENT-CERT, SSL-CLIENT-S-DN-CN, User-Agent, X-Forwarded-for   | form/multipart | session |
| info        | `POST /epp_command_url/info`     | `raw_frame, frame, clTRID` |  SSL-CLIENT-CERT, SSL-CLIENT-S-DN-CN, User-Agent, X-Forwarded-for   | form/multipart | session |
| create      | `POST /epp_command_url/create`   | `raw_frame, frame, clTRID` |  SSL-CLIENT-CERT, SSL-CLIENT-S-DN-CN, User-Agent, X-Forwarded-for   | form/multipart | session |
| delete      | `POST /epp_command_url/delete`   | `raw_frame, frame, clTRID` |  SSL-CLIENT-CERT, SSL-CLIENT-S-DN-CN, User-Agent, X-Forwarded-for   | form/multipart | session |
| renew       | `POST /epp_command_url/renew`    | `raw_frame, frame, clTRID` |  SSL-CLIENT-CERT, SSL-CLIENT-S-DN-CN, User-Agent, X-Forwarded-for   | form/multipart | session |
| transfer | `POST /epp_command_url/transfer` | `raw_frame, frame, clTRID` |  SSL-CLIENT-CERT, SSL-CLIENT-S-DN-CN, User-Agent, X-Forwarded-for   | form/multipart | session |
| update      | `POST /epp_command_url/update`   | `raw_frame, frame, clTRID` |  SSL-CLIENT-CERT, SSL-CLIENT-S-DN-CN, User-Agent, X-Forwarded-for   | form/multipart | session |
| ANY (error) | `GET /epp_error_url`             | `msg, code, clTRID`        |  SSL-CLIENT-CERT, SSL-CLIENT-S-DN-CN, User-Agent, X-Forwarded-for   | query params   | session |

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
$ rebar3 as prod release tar # Combines the steps above into single one, uses production profile.
```

### Creating and installing Releases

The application is configured to automatically create releases based on git tags. After you develop
a new functionality, you can create a release tag on Github.

You can then use appup to generate an upgrade path from currently running release to the new one.
Assuming that a release currently running is v0.1.0, and you want to upgrade to v0.1.1, this is how
the process looks like:

```sh
$ git checkout tags/v0.1.0 # Check out the older version.
$ rebar3 as prod release # Create the currently in use release, as it could have been deleted or corrupted in any way.
$ git checkout tags/v0.1.1
$ rebar3 as prod do release, appup generate --previous_version 0.1.0, relup, tar # Generate upgrade path, and then package it.
```

#### Installating

You can copy the archive with 0.1.1 to your target machine(s), and then execute:

```sh
$ ./bin/epp_proxy install 0.1.1 # Install the new version and marks it as permanent.
```

From there, you can either upgrade in place or restart into the new release.

```sh
$ ./bin/epp_proxy upgrade 0.1.1 # Upgrade in place.
$ ./bin/epp_proxy reboot # Reboot the VM completely into the new permanent release.
```

Configuration
-----
Configuration for the application tries to emulate the mod_epp configuration as close as possible
to make migration easier. The configuration is placed in `config/sys.config` file, it takes a format
of Erlang property list.

There are three example configuration files in `config/`:

* `sys.config` – default configuration used for real deployments. Values such as `tls_port`,
  `epp_session_url` and certificate paths are typically provided via environment variables (eg.
  `${TLS_PORT}`, `${EPP_SESSION_URL}`), so the same file can be reused across environments.
* `docker.config` – configuration tuned for running inside Docker. It uses hardcoded ports,
  certificate paths under `/opt/ca/...` and EPP endpoints pointing to the `epp` container
  (eg. `http://epp:3000/epp/…`).
* `test.config` – local development/test configuration. It enables `dev_mode`, uses local ports
  and points EPP endpoints to `http://localhost:9292/...`, with test CA material under
  `test_ca/`.

*Configuration variables*

| Variable name          | Expected values                    | Apache equivalent     | Definition
-------------------------|------------------------------------|-----------------------|--------------------------------------------
| `dev_mode`             | `true`, `false`                    | None                  | Enables TCP access without TLS.
| `tls_port`             | `700`                              | Listen                | At which port should we open a TLS socket. Default is 700.
| `tcp_port`             | `70000`                            | Listen                | At which port should we open a TCP socket. Only in `dev_mode`.
| `epp_session_url`      | `https://example.com/epp/session`  | EppSessionRoot        | HTTP address of the session endpoints including schema and port.
| `epp_command_url`      | `https://example.com/epp/command`  | EppCommandRoot        | HTTP address of the command endpoints including schema and port.
| `epp_error_url`        | `https://example.com/epp/error`    | EppErrorRoot          | HTTP address of the error endpoints including schema and port.
| `require_client_certs` | `true`, `false`                    | None                  | Allows client to connect to epp_proxy without client certificate using TLS.
| `cacertfile_path`      | `/opt/ca/ca.crt.pem`               | SSLCACertificateFile  | Where is the client root CA located. Can be inside apps/epp_proxy/priv or absolute path.
| `certfile_path`        | `/opt/ca/server.crt.pem`           | SSLCertificateFile    | Where is the server certificate located. Can be inside apps/epp_proxy/priv or absolute path.
| `keyfile_path`         | `/opt/ca/server.key.pem`           | SSLCertificateKeyFile | Where is the server key located. Can be inside apps/epp_proxy/priv or absolute path.
| `crlfile_path`         | `/opt/ca/crl.pem`                  | SSLCARevocationFile   | Where is the CRL file located. Can be inside apps/epp_proxy/priv or absolute path. When not set, not CRL check is performed.


Migrating from mod_epp
----

Checklist of steps to perform if you want to migrate from mod_epp, but still use Apache to be a reverse proxy.

1. Remove SSL-CLIENT-S-DN-CN and SSL-CLIENT-CERT headers from Apache. Epp_proxy takes care of those.
2. Install this project to desired location.
3. Ensure that the user who will run epp_proxy has access to all certificate files.
4. Set up syslog in `config/sys.config`.
5. If you do not feel comfortable using Erlang configuration file, you can use command line arguments and flags in format of `/epp_proxy/rel/bin/epp_proxy -epp_proxy tls_port 444`, where `-epp_proxy` is name of application,
   followed by configuration parameter name and value.

Testing
----
The application comes with test suite written with common_test. For integration
tests, there is a small Roda application located in `apps/epp_proxy/priv/test_backend_app`.
It has been written with Ruby 3.2.2.

There is also a number of generated ssl certificates that are used only for testing. Those are
valid until 2029 and they are located in `apps/epp_proxy/priv/test_ca`. The password for test CA
is `password`.

You need to start the backend application before running the test suite. To start it as a deamon,
from the root folder of the project, execute:

```bash
$ /bin/bash -l -c "cd apps/epp_proxy/priv/test_backend_app && bundle install"
$ /bin/bash -l -c "cd apps/epp_proxy/priv/test_backend_app && bundle exec rackup --pid pidfile -D"
```

The easiest way to run tests is using Docker:

```bash
# Run all tests
docker compose run --rm epp_proxy bash -c "cd /opt/erlang/epp_proxy && rebar3 ct"

# Run a specific test suite
docker compose run --rm epp_proxy bash -c "cd /opt/erlang/epp_proxy && rebar3 ct --suite apps/epp_proxy/test/epp_http_client_SUITE"

# Start a shell for debugging
docker compose run --rm epp_proxy bash -c "cd /opt/erlang/epp_proxy && rebar3 shell"

# Then in the Erlang shell:
application:get_all_env(epp_proxy).
# To exit the shell:
halt().
```

After you finish testing, you can stop the process by reading the stored pid:

    $ kill `cat apps/epp_proxy/priv/test_backend_app/pidfile`

TODO
----

1. Ensure CRL gets updated periodically without a need for restart of the service
