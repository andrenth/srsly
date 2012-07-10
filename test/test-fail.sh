#!/bin/sh

. `dirname $0`/config.sh

ENVFROM="spf-test@openspf.net"
HELOFQDN="`host -t mx openspf.net | awk '{ print $NF}'`"

runtest "`milter-test-server                      \
            --connection-spec $CONNSPEC           \
            --connect-address inet:8888@$HELOFQDN \
            --helo-fqdn       $HELOFQDN           \
            --envelope-from   $ENVFROM`"          \
        '^status: reject'

exit $?
