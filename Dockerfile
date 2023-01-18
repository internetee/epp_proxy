FROM debian:buster-slim

SHELL ["/bin/bash", "-o", "pipefail", "-c"]
COPY ./docker/apt/sources.list /etc/apt/

RUN apt-get update && apt-get -t buster install -y -qq wget \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*


RUN apt-get update && apt-get install -y -qq git \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

RUN apt-get update && apt-get install -y --allow-downgrades \
  dpkg-dev=* \
  bzip2=* \
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
  libc6=2.28-10+deb10u1 \
  libc-dev \
  perl=* \
  procps=* \
  inotify-tools=* \
  libssl1.1=* \
  libbz2-1.0=1.0.6-9.2~deb10u1 \
  perl-base=* \
  zlib1g=1:1.2.11.dfsg-1+deb10u1 \
  zlib1g-dev=* \
  libtinfo6=6.1+20181013-2+deb10u2 \
  libncurses-dev=6.1+20181013-2+deb10u2 \
  libncursesw6=6.1+20181013-2+deb10u2 \
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
