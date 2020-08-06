FROM haskell:8.8

RUN apt-get update && apt-get install -y libncurses-dev
RUN useradd -m cryptol
RUN git clone --depth 1 --branch 2.9.0 -- https://github.com/GaloisInc/cryptol.git /cryptol
RUN chown -R cryptol:cryptol /cryptol
USER cryptol
WORKDIR /cryptol
ENV PATH=/cryptol/rootfs/usr/local/bin:$PATH
ARG CRYPTOLPATH="/cryptol/.cryptol"
ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8
RUN cp cabal.GHC-8.8.3.config cabal.project.freeze
RUN mkdir -p rootfs/usr/local/bin
RUN cabal v2-update
RUN cabal v2-install --install-method=copy --installdir=rootfs/usr/local/bin exe:cryptol
RUN cabal v2-install --install-method=copy --installdir=bin test-lib
#RUN ./bin/test-runner --ext=.icry --exe=./rootfs/usr/local/bin/cryptol -F -b tests
ENV PATH=/usr/local/bin:/cryptol/rootfs/usr/local/bin:$PATH
RUN mkdir -p rootfs/"${CRYPTOLPATH}" \
    && cp -r lib/* rootfs/"${CRYPTOLPATH}"
USER root
RUN chown -R root:root /cryptol/rootfs

RUN apt-get update \
    && apt-get install -y libgmp10 libgomp1 libffi6 wget libncurses5 unzip libreadline7 vim emacs-nox sudo git curl dos2unix \
    && apt-get clean && rm -rf /var/lib/apt/lists/*
COPY --from=galoisinc/cryptol:2.9.0 /usr/local/bin /usr/local/bin
COPY --from=galoisinc/saw:0.5 /usr/local/bin /usr/local/bin
RUN curl -L https://mathsat.fbk.eu/download.php?file=mathsat-5.6.3-linux-x86_64.tar.gz | tar xz \
    && cp mathsat-5.6.3-linux-x86_64/bin/mathsat /usr/local/bin \
    && rm -rf mathsat-5.6.3-linux-x86_64
RUN chown -R cryptol:cryptol /home/cryptol
RUN passwd -d cryptol
RUN adduser cryptol sudo
RUN echo "Defaults        lecture = never" >> /etc/sudoers \
    && echo "cryptol ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers
USER cryptol
ENV LANG C.UTF-8
RUN echo 'export PS1="\$(pwd)> "' >> /home/cryptol/.bashrc

ENTRYPOINT ["/bin/bash"]
