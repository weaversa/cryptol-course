FROM debian:buster-slim

COPY --from=galoisinc/saw:0.5 /usr/local/bin /usr/local/bin
COPY --from=galoisinc/cryptol:2.9.0 /usr/local/bin /usr/local/bin

RUN apt-get update \
    && apt-get install -qq -y --fix-missing --no-install-recommends \
        # ABC
        libreadline7 \
        # Z3
        libgomp1 \
        # SAW
        libtinfo5 \
        # MathSAT script
        apt-transport-https ca-certificates curl \
        # Yices script
        autoconf build-essential gperf libgmp-dev \
        # Editors
        emacs-nox vim \
        # Extra Libraries
        # libffi6 libncurses5 
        # Utilities
        # git wget unzip
        # Why?!
        # sudo \
    # MathSAT
    && curl -kL https://mathsat.fbk.eu/download.php?file=mathsat-5.6.3-linux-x86_64.tar.gz | tar xz \
    && cp mathsat-5.6.3-linux-x86_64/bin/mathsat /usr/local/bin \
    && rm -rf mathsat-5.6.3-linux-x86_64 \
    # Yices (because running container in Windows causes [an issue](https://github.com/SRI-CSL/yices2/issues/120))
    && curl -L https://yices.csl.sri.com/releases/2.6.2/yices-2.6.2-src.tar.gz | tar xz \
    && cd yices* && autoconf && ./configure && make \
    && cp build/*/bin/* /usr/local/bin \
    # && cp build/*/bin/yices_smt2 /usr/local/bin/yices-smt2 \
    && cd .. && rm -rf yices* \
    && apt-get remove --purge --auto-remove -y apt-transport-https autoconf build-essential ca-certificates curl gperf libgmp-dev \
    && apt-get clean && rm -rf /var/lib/apt/lists/*

# RUN adduser cryptol sudo
# RUN echo "Defaults lecture = never" >> /etc/sudoers \
#     && echo "cryptol ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

RUN useradd -m -p '' cryptol && chown -R cryptol:cryptol /home/cryptol
USER cryptol
ENV LANG C.UTF-8
RUN echo 'export PS1="\$(pwd)> "' >> /home/cryptol/.bashrc

ENTRYPOINT ["/bin/bash"]