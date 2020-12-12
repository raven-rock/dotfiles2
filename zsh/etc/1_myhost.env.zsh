# -*- sh -*-

# This file sets $MYHOST. It is so we can have more sane looking hostnames
# displayed later wherever we want, such as in the $PROMPT. This is especially
# nice when on a cloud-provided host where the name is a bunch of random
# characters.

# zsh sets $HOST automatically; bash does not. But let's be bash-portatble by
# setting it if needed.
#: ${HOST:=$(hostname 2> /dev/null || cat /proc/sys/kernel/hostname)}
#: ${MYHOST:=$HOST}
#
## Map exceptions
#case $HOST in
#    dev-dsk-lgillett-1a-58e846ad.us-east-1.amazon.com)
#        MYHOST=lgillett-2.aka.corp.amazon.com
#        ;;
#    dev-dsk-lgillett-2a-a86de484.us-west-2.amazon.com)
#        MYHOST=lgillett-3.corp.amazon.com
#        ;;
#esac
#
#export MYHOST
#export HOST
