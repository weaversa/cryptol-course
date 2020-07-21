FROM debian:stretch
RUN apt-get update \
    && apt-get install -y libgmp10 libgomp1 libffi6 wget libncurses5 unzip libreadline7 vim emacs-nox \
    && apt-get clean && rm -rf /var/lib/apt/lists/*
COPY --from=cryptolcourse/cryptol /usr/local/bin /usr/local/bin
COPY --from=cryptolcourse/saw /usr/local/bin /usr/local/bin
RUN useradd -m cryptol && chown -R cryptol:cryptol /home/cryptol
USER cryptol
ENV LANG C.UTF-8
RUN echo 'export PS1="$(pwd)> "' >> /home/cryptol/.bashrc

ENTRYPOINT ["/bin/bash"]
