FROM debian:buster
RUN apt-get update \
    && apt-get install -y libgmp10 libgomp1 libffi6 wget libncurses5 unzip libreadline7 vim emacs-nox sudo git curl \
    make libgmp-dev gperf gcc autoconf \
    && apt-get clean && rm -rf /var/lib/apt/lists/*
COPY --from=galoisinc/cryptol:2.9.0 /usr/local/bin /usr/local/bin
COPY --from=galoisinc/saw:0.5 /usr/local/bin /usr/local/bin
RUN curl -L https://mathsat.fbk.eu/download.php?file=mathsat-5.6.3-linux-x86_64.tar.gz | tar xz \
    && cp mathsat*/bin/mathsat /usr/local/bin \
    && rm -rf mathsat*
RUN curl -L https://yices.csl.sri.com/releases/2.6.2/yices-2.6.2-src.tar.gz | tar xz \
    && cd yices* && autoconf && ./configure && make \
    && cp build/*/bin/* /usr/local/bin \
    && cp build/*/bin/yices_smt2 /usr/local/bin/yices-smt2 \
    && cd .. && rm -rf yices*
RUN useradd -m -p '' cryptol && chown -R cryptol:cryptol /home/cryptol
RUN adduser cryptol sudo
RUN echo "Defaults lecture = never" >> /etc/sudoers \
    && echo "cryptol ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers
USER cryptol
ENV LANG C.UTF-8
RUN echo 'export PS1="\$(pwd)> "' >> /home/cryptol/.bashrc

ENTRYPOINT ["/bin/bash"]
