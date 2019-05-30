from erlang:21

RUN curl https://s3.amazonaws.com/rebar3/rebar3 -o /rebar3
RUN mv rebar3 /usr/local/bin
RUN chmod +x /usr/local/bin/rebar3

RUN mkdir -p /opt/erlang/epp_proxy
WORKDIR /opt/erlang/epp_proxy
