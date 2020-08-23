FROM debian:buster
COPY --from=galoisinc/cryptol:2.9.0 /usr/local/bin /usr/local/bin
COPY --from=galoisinc/saw:0.5 /usr/local/bin /usr/local/bin
RUN apt-get update \
    && apt-get install -y libgmp10 libgomp1 libffi6 wget libncurses5 unzip libreadline7 vim emacs-nox sudo git curl \
    && apt-get clean && rm -rf /var/lib/apt/lists/*
RUN curl -L https://mathsat.fbk.eu/download.php?file=mathsat-5.6.3-linux-x86_64.tar.gz | tar xz \
    && cp mathsat-5.6.3-linux-x86_64/bin/mathsat /usr/local/bin \
    && rm -rf mathsat-5.6.3-linux-x86_64
RUN useradd -m -p '' cryptol && chown -R cryptol:cryptol /home/cryptol
RUN adduser cryptol sudo
RUN echo "Defaults        lecture = never" >> /etc/sudoers \
    && echo "cryptol ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers
USER cryptol
ENV LANG C.UTF-8
RUN echo 'export PS1="\$(pwd)> "' >> /home/cryptol/.bashrc

ENTRYPOINT ["/bin/bash"]