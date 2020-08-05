FROM haskell:8.8

RUN apt-get update && apt-get install -y libncurses-dev
COPY --from=solvers /solvers/rootfs /
RUN useradd -m cryptol
COPY --chown=cryptol:cryptol . /cryptol
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
RUN ./bin/test-runner --ext=.icry --exe=./rootfs/usr/local/bin/cryptol -F -b tests
ENV PATH=/usr/local/bin:/cryptol/rootfs/usr/local/bin:$PATH
RUN    ! $(cryptol -c ":s prover=abc" | tail -n +2 | grep -q .) \
#    && ! $(cryptol -c ":s prover=mathsat" | tail -n +2 | grep -q .) \
    && ! $(cryptol -c ":s prover=cvc4" | tail -n +2 | grep -q .) \
    && ! $(cryptol -c ":s prover=yices" | tail -n +2 | grep -q .) \
    && ! $(cryptol -c ":s prover=boolector" | tail -n +2 | grep -q .) \
    && ! $(cryptol -c ":s prover=z3" | tail -n +2 | grep -q .)
RUN mkdir -p rootfs/"${CRYPTOLPATH}" \
    && cp -r lib/* rootfs/"${CRYPTOLPATH}"
USER root
RUN chown -R root:root /cryptol/rootfs

RUN apt-get update \
    && apt-get install -y libgmp10 libgomp1 libffi6 wget libncurses5 unzip libreadline7 vim emacs-nox sudo git \
    && apt-get clean && rm -rf /var/lib/apt/lists/*
COPY --from=cryptolcourse/cryptol /usr/local/bin /usr/local/bin
COPY --from=cryptolcourse/saw /usr/local/bin /usr/local/bin
RUN useradd -m -p '' cryptol && chown -R cryptol:cryptol /home/cryptol
RUN adduser cryptol sudo
RUN echo "Defaults        lecture = never" >> /etc/sudoers
USER cryptol
ENV LANG C.UTF-8
RUN echo 'export PS1="\$(pwd)> "' >> /home/cryptol/.bashrc

ENTRYPOINT ["/bin/bash"]
