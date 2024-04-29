FROM debian:buster-slim

SHELL ["/bin/bash", "-o", "pipefail", "-c"]
COPY ./docker/apt/sources.list /etc/apt/

RUN apt-get update && apt-get install -y -qq wget \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*


RUN apt-get update && apt-get install -y -qq git \
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
  liberror-perl=* \
  libc6=* \
  libc-dev \
  perl=* \
  procps=* \
  inotify-tools=* \
  libssl1.1=* \
  perl-base=* \
  zlib1g-dev \
  net-tools \
  iproute2 \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

# RUN (groupadd -g 999 asdf || true)
# RUN (adduser --shell /bin/bash --home /asdf --disabled-password -gid 999 -u 999 asdf || true)
# ENV PATH="${PATH}:/asdf/.asdf/shims:/asdf/.asdf/bin"
# USER asdf
# WORKDIR /asdf

RUN git clone https://github.com/asdf-vm/asdf.git --branch v0.6.3 "$HOME"/.asdf && \
    echo '. $HOME/.asdf/asdf.sh' >> "$HOME"/.bashrc && \
        echo '. $HOME/.asdf/asdf.sh' >> "$HOME"/.profile

ENV PATH="${PATH}:/root/.asdf/shims:/root/.asdf/bin"

RUN mkdir -p /opt/erlang/epp_proxy
WORKDIR /opt/erlang/epp_proxy

COPY .tool-versions ./
RUN asdf plugin-add erlang
RUN asdf install
RUN asdf global erlang $(grep erlang .tool-versions | cut -d' ' -f2)
RUN asdf plugin-add ruby
RUN asdf plugin-add rebar
RUN asdf install
