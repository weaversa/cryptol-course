# This is the Dockerfile used to build the image hosted at:
# https://hub.docker.com/repository/docker/cryptolcourse/dev
# To use this Dockerfile directly, uncomment the appropriate line in
# devcontainer.json
FROM debian:buster
RUN apt-get update \
    && apt-get install -y curl make libgmp-dev gperf autoconf clang wget unzip vim emacs-nox git curl dos2unix \
    && apt-get clean && rm -rf /var/lib/apt/lists/*
COPY --from=ghcr.io/galoisinc/saw:0.9 /usr/local/bin /usr/local/bin
COPY --from=ghcr.io/galoisinc/cryptol:2.12.0 /usr/local/bin /usr/local/bin
RUN useradd -m -p '' cryptol && chown -R cryptol:cryptol /home/cryptol
USER cryptol
ENV LANG C.UTF-8
RUN echo 'export PS1="\$(pwd)> "' >> /home/cryptol/.bashrc

ENTRYPOINT ["/bin/bash"]
