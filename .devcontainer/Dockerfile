FROM cryptolcourse/cryptol-dev

# This Dockerfile adds a non-root user with sudo access. Use the "remoteUser"
# property in devcontainer.json to use it. On Linux, the container user's GID/UIDs
# will be updated to match your local UID/GID (when using the dockerFile property).
# See https://aka.ms/vscode-remote/containers/non-root-user for details.
ARG USERNAME=cryptol
ARG USER_UID=1000
ARG USER_GID=$USER_UID
RUN echo 'export PS1="Cryptol> "' >> /home/cryptol/.bashrc


