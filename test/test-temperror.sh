#!/bin/sh

. `dirname $0`/config.sh

ENVFROM='spf-test@bradescoseguros.com.br'
HELOFQDN='gwmail.bradescoseguros.com.br'

runtest "`milter-test-server                         \
            --connection-spec    $CONNSPEC           \
            --connect-address    inet:8888@$HELOFQDN \
            --helo-fqdn          $HELOFQDN           \
            --envelope-from      $ENVFROM            \
            --reading-timeout    30                  \
            --writing-timeout    30`"                \
        '^status: temporary-failure'

exit $?
