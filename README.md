# srsly

## Introduction

srsly is a milter-based daemon that does [SPF](http://www.openspf.org/)
verification and supports [SRS](http://www.openspf.org/SRS) address rewriting.

## Installation

To build srsly you need to install its dependencies:

* [lwt](http://ocsigen.org/lwt/)
* [extunix](http://extunix.forge.ocamlcore.org/)
* [ocaml-syslog](http://homepage.mac.com/letaris)
* [ocaml-uint](https://github.com/andrenth/ocaml-uint)
* [release](https://github.com/andrenth/release)
* [ocaml-milter](https://github.com/andrenth/ocaml-milter)
* [ocaml-spf](https://github.com/andrenth/ocaml-spf)
* [ocaml-srs](https://github.com/andrenth/ocaml-srs)

Having those installed, simply run the following commands.

    $ ocaml setup.ml -configure
    $ ocaml setup.ml -build
    # ocaml setup.ml -install

## Documentation

srsly runs in a 3-process configuration: a master process (`srslyd`) and two
unprivileged slave processes (`srsly_in`) and (`srsly_out`). There is also a
control command, `srsly`.

The master process is responsible for starting the slaves and handling requests
from the control command. The slave processes do all the real work:

* `srsly_in` does SPF verification on inbound messages and reverses SRS-signed
  bounces.
* `srsly_out` simply SRS-signs every outgoing message.

You need to split your MTA configuration in two MTAs, one for inbound and one
for outbound mail. This way, you can point each one to the correct srsly
process. In postfix, this is done by specifying the appropriate
`-o smtpd_milters=...` option to each MTA in `master.cf`.

To learn how to configure and run srsly, please see the `srslyd(8)`,
`srslyd.conf(5)` and `srsly(1)` manual pages in the `man` directory.
