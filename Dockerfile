FROM debian:bullseye-slim

SHELL ["/bin/bash", "-o", "pipefail", "-c"]
COPY ./docker/apt/sources.list /etc/apt/

# Install all dependencies in a single layer to reduce image size
RUN apt-get update && apt-get install -y -qq \
  wget \
  git \
  build-essential \
  libncurses5-dev \
  automake \
  autoconf \
  curl \
  ca-certificates \
  libssl-dev \
  libreadline-dev \
  libdpkg-perl \
  liberror-perl \
  libc6 \
  libc-dev \
  perl \
  procps \
  inotify-tools \
  libssl1.1 \
  perl-base \
  zlib1g-dev \
  # Additional dependencies for Erlang build
  libncurses-dev \
  libsctp-dev \
  # Documentation tools to prevent build failures
  xsltproc \
  libxml2-utils \
  # Dependencies for Ruby 3.2.2
  libffi-dev \
  libyaml-dev \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*

# Set environment variables for Erlang build
ENV KERL_CONFIGURE_OPTIONS="--disable-debug --without-javac --without-wx --without-odbc --disable-hipe --without-jinterface --without-docs"
ENV KERL_BUILD_DOCS="no"
ENV KERL_DOC_TARGETS=""
ENV KERL_INSTALL_HTMLDOCS="no"
ENV KERL_INSTALL_MANPAGES="no"

# Install asdf and add it to PATH
RUN git clone https://github.com/asdf-vm/asdf.git --branch v0.6.3 /root/.asdf && \
    echo '. /root/.asdf/asdf.sh' >> /root/.bashrc && \
    echo '. /root/.asdf/asdf.sh' >> /root/.profile

ENV PATH="/root/.asdf/shims:/root/.asdf/bin:${PATH}"

RUN mkdir -p /opt/erlang/epp_proxy
WORKDIR /opt/erlang/epp_proxy

COPY .tool-versions ./

# Install plugins and tools with explicit sourcing of asdf.sh
RUN asdf plugin-add erlang
RUN source /root/.asdf/asdf.sh && asdf install
RUN asdf global erlang $(grep erlang .tool-versions | cut -d' ' -f2)
RUN asdf plugin-add ruby
RUN asdf plugin-add rebar
RUN asdf install
