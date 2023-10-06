# This is the Dockerfile used to build the image hosted at:
# ghcr.io/weaversa/cryptol-course:$CRYPTOL_COURSE_VERSION
# To use this Dockerfile directly, uncomment the appropriate line in
# devcontainer.json

ARG CRYPTOL_COURSE_TAG=nightly

ARG UBUNTU_VERSION=22.04

ARG CRYPTOL_TAG=nightly
ARG SAW_TAG=nightly
ARG WHAT4_SOLVERS_SNAPSHOT=snapshot-20230711
ARG WHAT4_SOLVERS_ARCHIVE=ubuntu-$UBUNTU_VERSION-X64-bin.zip

ARG CRYPTOL_SERVER_SCHEME=http
ARG CRYPTOL_SERVER_HOST=0.0.0.0
ARG CRYPTOL_SERVER_PORT=36681

ARG SAW_SERVER_SCHEME=http
ARG SAW_SERVER_HOST=0.0.0.0
ARG SAW_SERVER_PORT=36691

FROM ghcr.io/galoisinc/cryptol:$CRYPTOL_TAG as galois_cryptol

FROM ghcr.io/galoisinc/cryptol-remote-api:$CRYPTOL_TAG as galois_cryptol_remote_api

FROM ghcr.io/galoisinc/saw:$SAW_TAG as galois_saw

FROM ghcr.io/galoisinc/saw-remote-api:$SAW_TAG as galois_saw_remote_api

FROM ubuntu:$UBUNTU_VERSION

USER root

RUN ln -snf /usr/share/zoneinfo/$CONTAINER_TIMEZONE /etc/localtime && echo $CONTAINER_TIMEZONE > /etc/timezone

# Install the Docker apt repository
RUN apt-get -qq update && \
    DEBIAN_FRONTEND="noninteractive" apt-get -qq install --yes ca-certificates curl gnupg lsb-release
RUN mkdir -p /etc/apt/keyrings && \
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | gpg --dearmor -o /etc/apt/keyrings/docker.gpg && \
    chmod a+r /etc/apt/keyrings/docker.gpg
RUN echo \
    "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
    $(lsb_release -cs) stable" | tee /etc/apt/sources.list.d/docker.list > /dev/null

# Install all tools
# We use an old containerd.io because it contains a version of runc that works
# with sysbox correctly.
RUN apt-get -qq update \
    && DEBIAN_FRONTEND="noninteractive" apt-get -qq install --yes \
      bash \
      build-essential \
      containerd.io \
      docker-ce \
      docker-ce-cli \
      docker-compose-plugin \
      htop \
      locales \
      man \
      software-properties-common \
      sudo \
      systemd \
      systemd-sysv \
      unzip \
      vim \
      openssh-client \
      make \
      cmake \
      libgmp-dev \
      gperf \
      autoconf \
      vim \
      dos2unix \
      bash-completion \
      htop \
      man \
      python3 \
      python3-pip \
      python3-venv \
      zip \
      locales \
      graphviz \
      clang-12 \
      clang-tools-12 \
      clang-format-12 \
      libffi-dev \
      libffi8 \
      libgmp10 \
      libncurses-dev \
      libncurses6 \
      libtinfo6 \
      libssl-dev \
      libnuma-dev \
      zlib1g-dev \
      wget \
    && add-apt-repository ppa:kelleyk/emacs -y \
    && DEBIAN_FRONTNED="noninteractive" apt-get -qq install -y emacs28-nox \
    && apt-get -qq clean && rm -rf /var/lib/apt/lists/* \
    # Install latest Git using their official PPA
    && add-apt-repository ppa:git-core/ppa \
    && DEBIAN_FRONTEND="noninteractive" apt-get -qq install --yes git

# Link clang-12 utils
RUN find /usr/bin/ -name "*-12" -exec basename {} \; | sed "s/\-12//" | xargs -I{} ln -s /usr/bin/'{}'-12 /usr/bin/'{}'

# Enables Docker starting with systemd
RUN systemctl enable docker

# Add docker-compose
RUN curl -L "https://github.com/docker/compose/releases/download/v2.5.0/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
RUN chmod +x /usr/local/bin/docker-compose

# Install SAW
COPY --from=galois_saw /usr/local/bin /usr/local/bin
COPY --from=galois_saw_remote_api /usr/local/bin/saw-remote-api /usr/local/bin/saw-remote-api
ARG SAW_SERVER_SCHEME
ARG SAW_SERVER_HOST
ARG SAW_SERVER_PORT
ENV SAW_SERVER_URL "$SAW_SERVER_SCHEME://$SAW_SERVER_HOST:$SAW_SERVER_PORT"
RUN echo 'saw-remote-api $SAW_SERVER_SCHEME --host $SAW_SERVER_HOST --port $SAW_SERVER_PORT --read-only / &' >> /usr/local/bin/start-saw-remote-api-read-only
RUN echo 'saw-remote-api $SAW_SERVER_SCHEME --host $SAW_SERVER_HOST --port $SAW_SERVER_PORT / &' >> /usr/local/bin/start-saw-remote-api
ENV LC_ALL=C.UTF-8

# Install Cryptol
COPY --from=galois_cryptol /usr/local/bin /usr/local/bin
COPY --from=galois_cryptol_remote_api /usr/local/bin/cryptol-remote-api /usr/local/bin/cryptol-remote-api
ARG CRYPTOL_SERVER_SCHEME
ARG CRYPTOL_SERVER_HOST
ARG CRYPTOL_SERVER_PORT
ENV CRYPTOL_SERVER_URL "$CRYPTOL_SERVER_SCHEME://$CRYPTOL_SERVER_HOST:$CRYPTOL_SERVER_PORT"
RUN echo 'cryptol-remote-api $CRYPTOL_SERVER_SCHEME --host $CRYPTOL_SERVER_HOST --port $CRYPTOL_SERVER_PORT / --read-only &' >> /usr/local/bin/start-cryptol-remote-api-read-only
RUN echo 'cryptol-remote-api $CRYPTOL_SERVER_SCHEME --host $CRYPTOL_SERVER_HOST --port $CRYPTOL_SERVER_PORT / &' >> /usr/local/bin/start-cryptol-remote-api

# Get fresh Python clients for Cryptol and SAW
RUN wget --no-verbose https://github.com/GaloisInc/cryptol/archive/refs/heads/master.zip && unzip -qq master.zip
RUN mv cryptol-master /usr/local/share/cryptol && rm -rf master.zip
RUN wget --no-verbose https://github.com/GaloisInc/saw-script/archive/refs/heads/master.zip && unzip -qq master.zip
RUN mv saw-script-master /usr/local/share/saw-script && rm -rf master.zip

# Link to Python clients
ENV PYTHONPATH "${PYTHONPATH}:/usr/local/share/cryptol/cryptol-remote-api/python:/usr/local/share/saw-script/saw-remote-api/python"

# Install Python client dependencies
RUN pip3 -q install typing_extensions argo_client BitVector

# Get `what4-solvers` snapshot compiled for Ubuntu
ARG WHAT4_SOLVERS_SNAPSHOT
ARG WHAT4_SOLVERS_ARCHIVE
RUN wget --no-verbose https://github.com/GaloisInc/what4-solvers/releases/download/$WHAT4_SOLVERS_SNAPSHOT/$WHAT4_SOLVERS_ARCHIVE
RUN unzip -qq -o $WHAT4_SOLVERS_ARCHIVE -d /usr/local/bin && rm -rf $WHAT4_SOLVERS_ARCHIVE

RUN chmod a+x /usr/local/bin/*

RUN adduser --gecos '' --disabled-password cryptol \
    && echo "cryptol ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers.d/nopasswd \
    && chown -R cryptol:cryptol /home/cryptol
USER cryptol

ENV LANG C.UTF-8
RUN sudo chsh -s /bin/bash $(whoami)
ENV SHELL=/bin/bash
RUN echo 'export PS1="[\u \W]\$ "' >> /home/cryptol/.bashrc

# ENTRYPOINT ["/bin/bash"]
