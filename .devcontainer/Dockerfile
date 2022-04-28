# This is the Dockerfile used to build the image hosted at:
# ghcr.io/weaversa/cryptol-course:2.12
# To use this Dockerfile directly, uncomment the appropriate line in
# devcontainer.json

FROM ubuntu:latest
RUN ln -snf /usr/share/zoneinfo/$CONTAINER_TIMEZONE /etc/localtime && echo $CONTAINER_TIMEZONE > /etc/timezone
RUN apt-get update \
    && apt-get install -y curl make cmake libgmp-dev gperf autoconf build-essential clang-12 wget unzip vim emacs-nox git curl dos2unix bash-completion \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# Link clang
RUN ln -s /usr/bin/clang-12 /usr/bin/clang
RUN ln -s /usr/bin/clang++-12 /usr/bin/clang++

COPY --from=ghcr.io/galoisinc/saw:nightly /usr/local/bin /usr/local/bin
COPY --from=ghcr.io/galoisinc/cryptol:nightly /usr/local/bin /usr/local/bin

# Get latest what4-solvers compiled for ubuntu
RUN wget https://github.com/GaloisInc/what4-solvers/releases/download/snapshot-20220131/ubuntu-latest-bin.zip
RUN unzip -o ubuntu-latest-bin.zip -d /usr/local/bin && rm -rf ubuntu-latest-bin.zip

# Install recent abc
RUN git clone https://github.com/berkeley-abc/abc.git
RUN cd abc \
    && OPTFLAGS="-O2" ABC_USE_NO_READLINE=1 make -j 4 abc \
    && cp abc /usr/local/bin
RUN rm -rf abc

# Install slightly older Boolector
RUN wget https://github.com/Boolector/boolector/archive/refs/tags/3.2.1.tar.gz -O boolector-3.2.1.tar.gz
RUN tar -xvzf boolector-3.2.1.tar.gz && rm -rf boolector-3.2.1.tar.gz
RUN cd boolector-3.2.1 \
    && ./contrib/setup-btor2tools.sh \
    && ./contrib/setup-lingeling.sh \
    && ./configure.sh \
    && cd build \
    && make -j 4 boolector boolector-bin \
    && cp bin/boolector /usr/local/bin/boolector
RUN rm -rf boolector-3.2.1

RUN chmod a+x /usr/local/bin/*

RUN useradd -m -p '' cryptol && chown -R cryptol:cryptol /home/cryptol
USER cryptol
ENV LANG C.UTF-8
RUN chsh -s /bin/bash
ENV SHELL=/bin/bash
RUN echo 'export PS1="\$(pwd)> "' >> /home/cryptol/.bashrc

# ENTRYPOINT ["/bin/bash"]
