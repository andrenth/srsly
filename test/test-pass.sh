#!/bin/sh

. `dirname $0`/config.sh

ENVFROM=andrenth@gmail.com
ENVRCPT1=andrenth@gmail.com
ENVRCPT2=andrenth@digirati.com.br
HELOADDR="`host mail-we0-f179.google.com | head -n1 | awk '{ print $NF}'`"
HELOFQDN="`host $HELOADDR | head -n1 | awk '{ print $NF}'`"

runtest "`milter-test-server                         \
            --connection-spec    $CONNSPEC           \
            --connect-address    inet:9999@$HELOADDR \
            --connect-host       $HELOFQDN           \
            --helo-fqdn          $HELOFQDN           \
            --envelope-recipient $ENVRCPT1           \
            --envelope-recipient $ENVRCPT2           \
            --threads=0                              \
            --envelope-from   $ENVFROM`"             \
        '^status: pass'

exit $?
