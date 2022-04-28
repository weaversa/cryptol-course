# This is the Dockerfile used to build the image hosted at:
# ghcr.io/weaversa/cryptol-course:2.12
# To use this Dockerfile directly, uncomment the appropriate line in
# devcontainer.json

FROM ubuntu:latest
RUN ln -snf /usr/share/zoneinfo/$CONTAINER_TIMEZONE /etc/localtime && echo $CONTAINER_TIMEZONE > /etc/timezone
RUN apt-get update \
    && apt-get install -y curl make cmake libgmp-dev gperf autoconf gcc clang-12 wget unzip vim emacs-nox git curl dos2unix bash-completion \
    && apt-get clean && rm -rf /var/lib/apt/lists/*
COPY --from=ghcr.io/galoisinc/saw:nightly /usr/local/bin /usr/local/bin
COPY --from=ghcr.io/galoisinc/cryptol:nightly /usr/local/bin /usr/local/bin

RUN ln -s /usr/bin/clang-12 /usr/bin/clang
RUN chmod a+x /usr/local/bin/*

RUN useradd -m -p '' cryptol && chown -R cryptol:cryptol /home/cryptol
USER cryptol
ENV LANG C.UTF-8
RUN chsh -s /bin/bash
ENV SHELL=/bin/bash
RUN echo 'export PS1="\$(pwd)> "' >> /home/cryptol/.bashrc

# ENTRYPOINT ["/bin/bash"]
