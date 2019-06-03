FROM debian:wheezy

SHELL ["/bin/bash", "-o", "pipefail", "-c"]
COPY ./docker/apt/sources.list /etc/apt/

RUN apt-get update && apt-get -t wheezy install -y -qq wget \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

COPY ./docker/apt/frexian.list /etc/apt/sources.list.d/

RUN wget http://deb.freexian.com/extended-lts/pool/main/f/freexian-archive-keyring/freexian-archive-keyring_2018.05.29_all.deb && dpkg -i freexian-archive-keyring_2018.05.29_all.deb

RUN apt-get update && apt-get -t wheezy-backports install -y -qq git \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y \
  build-essential=* \
  libncurses5-dev=* \
  automake=* \
  autoconf=* \
  curl=* \
  ca-certificates=* \
  libssl-dev=* \
  libreadline-dev=* \
  libdpkg-perl=* \
  perl-modules=* \
  liberror-perl=* \
  libc6=* \
  libc-dev \
  perl=* \
  procps=* \
  libssl1.0.0=* \
  perl-base=* \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

# RUN (groupadd -g 999 asdf || true)
# RUN (adduser --shell /bin/bash --home /asdf --disabled-password -gid 999 -u 999 asdf || true)
# ENV PATH="${PATH}:/asdf/.asdf/shims:/asdf/.asdf/bin"
# USER asdf
# WORKDIR /asdf

RUN git clone https://github.com/asdf-vm/asdf.git "$HOME"/.asdf && \
    echo '. $HOME/.asdf/asdf.sh' >> "$HOME"/.bashrc && \
        echo '. $HOME/.asdf/asdf.sh' >> "$HOME"/.profile

ENV PATH="${PATH}:/root/.asdf/shims:/root/.asdf/bin"

RUN mkdir -p /opt/erlang/epp_proxy
WORKDIR /opt/erlang/epp_proxy

COPY .tool-versions ./
RUN asdf plugin-add erlang
RUN asdf install

RUN curl https://s3.amazonaws.com/rebar3/rebar3 -o /rebar3
RUN mv /rebar3 /usr/local/bin
RUN chmod +x /usr/local/bin/rebar3
