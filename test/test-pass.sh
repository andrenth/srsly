#!/bin/sh

. `dirname $0`/config.sh

ENVFROM=spf-test@digirati.com.br
HELOADDR="`host mxz.f1.k8.com.br | head -n1 | awk '{ print $NF}'`"
HELOFQDN="`host $HELOADDR | head -n1 | awk '{ print $NF}'`"

runtest "`milter-test-server                      \
            --connection-spec $CONNSPEC           \
            --connect-address inet:8888@$HELOADDR \
            --helo-fqdn       $HELOFQDN           \
            --envelope-from   $ENVFROM`"          \
        '^status: pass'

exit $?
