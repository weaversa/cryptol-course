# This is the Dockerfile used to build the image hosted at:
# ghcr.io/weaversa/cryptol-course:2.13
# To use this Dockerfile directly, uncomment the appropriate line in
# devcontainer.json

FROM ubuntu:22.04

USER root

RUN ln -snf /usr/share/zoneinfo/$CONTAINER_TIMEZONE /etc/localtime && echo $CONTAINER_TIMEZONE > /etc/timezone

# Install the Docker apt repository
RUN apt-get update && \
    DEBIAN_FRONTEND="noninteractive" apt-get install --yes ca-certificates curl gnupg lsb-release
RUN mkdir -p /etc/apt/keyrings && \
    curl -fsSL https://download.docker.com/linux/ubuntu/gpg | gpg --dearmor -o /etc/apt/keyrings/docker.gpg && \
    chmod a+r /etc/apt/keyrings/docker.gpg
RUN echo \
    "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/ubuntu \
    $(lsb_release -cs) stable" | tee /etc/apt/sources.list.d/docker.list > /dev/null

# Install all tools
RUN apt-get update \
    && DEBIAN_FRONTEND="noninteractive" apt-get install --yes \
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
      unzip \
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
    && DEBIAN_FRONTNED="noninteractive" apt-get install -y emacs28-nox \
    && apt-get clean && rm -rf /var/lib/apt/lists/* \
    # Install latest Git using their official PPA
    && add-apt-repository ppa:git-core/ppa \
    && DEBIAN_FRONTEND="noninteractive" apt-get install --yes git

# Link clang-12 utils
RUN find /usr/bin/ -name "*-12" -exec basename {} \; | sed "s/\-12//" | xargs -I{} ln -s /usr/bin/'{}'-12 /usr/bin/'{}'

# Enables Docker starting with systemd
RUN systemctl enable docker

# Add docker-compose
RUN curl -L "https://github.com/docker/compose/releases/download/v2.5.0/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
RUN chmod +x /usr/local/bin/docker-compose

# Install SAW
COPY --from=ghcr.io/galoisinc/saw:nightly /usr/local/bin /usr/local/bin
COPY --from=ghcr.io/galoisinc/saw-remote-api:nightly /usr/local/bin/saw-remote-api /usr/local/bin/saw-remote-api
ENV SAW_SERVER_URL=http://0.0.0.0:36691
RUN echo 'saw-remote-api --read-only http --host 0.0.0.0 --port 36691 / &' >> /usr/local/bin/start-saw-remote-api-read-only
RUN echo 'saw-remote-api http --host 0.0.0.0 --port 36691 / &' >> /usr/local/bin/start-saw-remote-api
ENV LC_ALL=C.UTF-8

# Install Cryptol
COPY --from=ghcr.io/galoisinc/cryptol:nightly /usr/local/bin /usr/local/bin
COPY --from=ghcr.io/galoisinc/cryptol-remote-api:nightly /usr/local/bin/cryptol-remote-api /usr/local/bin/cryptol-remote-api
ENV CRYPTOL_SERVER_URL=http://0.0.0.0:36681
RUN echo 'cryptol-remote-api --read-only http --host 0.0.0.0 --port 36681 / &' >> /usr/local/bin/start-cryptol-remote-api-read-only
RUN echo 'cryptol-remote-api http --host 0.0.0.0 --port 36681 / &' >> /usr/local/bin/start-cryptol-remote-api

# Get fresh Python clients for Cryptol and SAW
RUN wget https://github.com/GaloisInc/cryptol/archive/refs/heads/master.zip && unzip master.zip
RUN mv cryptol-master /usr/local/share/cryptol && rm -rf master.zip
RUN wget https://github.com/GaloisInc/saw-script/archive/refs/heads/master.zip && unzip master.zip
RUN mv saw-script-master /usr/local/share/saw-script && rm -rf master.zip

# Link to nightly python clients
ENV PYTHONPATH="${PYTHONPATH}:/usr/local/share/cryptol/cryptol-remote-api/python:/usr/local/share/saw-script/saw-remote-api/python"

# Install Python client dependencies
RUN pip3 install typing_extensions argo_client BitVector

# Get latest what4-solvers compiled for ubuntu
RUN wget https://github.com/GaloisInc/what4-solvers/releases/download/snapshot-20220812/ubuntu-latest-bin.zip
RUN unzip -o ubuntu-latest-bin.zip -d /usr/local/bin && rm -rf ubuntu-latest-bin.zip

RUN chmod a+x /usr/local/bin/*

# create local non-root user, but mimic host ids to avoid file permission problems
ARG USER_NAME="cryptol"
ARG USER_GNAME="cryptol"
# Copy the .env file with USER_UID and USER_GID set in devcontainer.json
COPY .env /tmp/.env
RUN . /tmp/.env && rm /tmp/.env \
    && groupadd -g ${USER_GID} -o ${USER_GNAME} \
    && useradd -m -u ${USER_UID} -g ${USER_GID} -o -s /bin/bash ${USER_NAME} \
    && echo "${USER_NAME} ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers.d/nopasswd
USER ${USER_NAME}

ENV LANG=C.UTF-8
RUN sudo chsh -s /bin/bash ${USER_NAME}
ENV SHELL=/bin/bash
RUN echo 'export PS1="[\u \W]\$ "' >> /home/${USER_NAME}/.bashrc

# ENTRYPOINT ["/bin/bash"]
