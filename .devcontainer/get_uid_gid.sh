#!/bin/sh

# determine the caller's uid:gid to use in container
USER_UID=$(id -u)
USER_GID=$(id -g)

# check if running as root, and instead use 1000:1000
if [ "${USER_UID}" = "0" ]
then
    USER_UID=1000
fi
if [ "${USER_GID}" = "0" ]
then
    USER_GID=1000
fi

echo "# created by .devcontainer/get_uid_gid.sh"
echo "USER_UID=${USER_UID}"
echo "USER_GID=${USER_GID}"

exit 0