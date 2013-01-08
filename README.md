# srsly

## Introduction

srsly is a milter-based daemon for [Postfix](http://www.postfix.org/) that does
[SPF](http://www.openspf.org/) verification and supports
[SRS](http://www.openspf.org/SRS) address rewriting in redirects.

## Installation

To build srsly you need to install its dependencies:

* [lwt](http://ocsigen.org/lwt/)
* [ocaml-uint](https://github.com/andrenth/ocaml-uint)
* [release](https://github.com/andrenth/release) (As of srsly 1.1.1, please
  install `release` from its `master` branch.)
* [ocaml-milter](https://github.com/andrenth/ocaml-milter)
* [ocaml-spf](https://github.com/andrenth/ocaml-spf)
* [ocaml-srs](https://github.com/andrenth/ocaml-srs)

Having those installed, simply run the following commands.

    $ make
    # make install

It is also possible to build a Debian package with the command

    $ debuild -uc -us

## Architecture

srsly runs in a 2-process configuration: a master process (`srslyd`) and an
unprivileged chrooted slave process (`srsly-milter`). There is also a control
command, the `srsly` executable.

The master process is responsible for starting `srsly-milter` and handling
requests from the control command. It also works as a proxy for the slave
process, performing actions that can't be done from inside a chroot jail and
sending their results to the slave.

## Running srsly

Before running srsly, you need to prepare its chroot area. The chroot
directory is the home directory of the unprivileged user `srsly-milter` runs as.
By default this is the `srsly` user.

On Linux, `srsly-milter` requires the following files under the chroot
directory, here referred to as `$CHROOT`. For other operating systems, those
requirements will likely be different.

* `$CHROOT/dev/log`
* `$CHROOT/etc/hosts`
* `$CHROOT/etc/resolv.conf`
* `$CHROOT/lib/libgcc_s.so.1`
* `$CHROOT/lib/libnss_dns*.so`
* `$CHROOT/lib/libresolv*.so`

You must create the directory structure and copy the above files into their
correct location, or use bind-mounts to mirror the system files into the
chroot area.

Once the chroot environment is ready, you must configure srsly. Please refer to
the `srslyd(8)`, `srslyd.conf(5)` and `srsly(1)` manual pages. Afterwards,
simply run

    # srsly start [/path/to/configuration/file]

to start srsly. If you don't pass a configuration file on the command line,
`/etc/srsly/srslyd.conf` will be assumed.

The Debian package comes with an [upstart](http://upstart.ubuntu.com/) script
which automatically sets up the chroot environment on startup and cleans it up
on service shutdown, so the above steps are unneeded. However, for that to
work you need to start srsly using the `start` command from upstart:

    # start srsly

To enable srsly in Postfix, add srsly's listen address to the `smtpd_milters`
directive in `main.cf` or in a specific `smtpd` instance in `master.cf`. By
default, srsly listens on an IPv4 socket on localhost, port 8387:

    smtpd_milters = inet:localhost:8387

## Communication with Postfix

In order to decide if a MAIL FROM address needs to be rewritten, srsly needs
to determine if the message is a redirect. There is a simple criteria that can
be used in that decision: a redirect message is one in which both the envelope
sender and envelope recipient are not local to the SMTP server.

While this is a simple decision in principle, things are not that easy in
practice. One of the difficulties of testing whether a given address is local
or remote from the point of view of the SMTP server is that Postfix supports a
large number of different databases that can be queried in order to perform
address translations, including text files, regular expression tables, SQL and
LDAP queries, among others. Therefore, in order to provide a solution that
works on every possible Postfix configuration, one would be forced to implement
different connectors for each of the databases supported by Postfix.

The approach taken by srsly is more pragmatic, if not officially supported by
Postfix. In order to allow its multiple processes to communicate, Postfix
creates a number of UNIX sockets in the `private` directory located in its own
chroot area. One of these sockets, `proxymap`, can be used for querying any
of the multiple database types supported by Postfix, using a protocol that is
well documented in the Postfix source code.

Therefore, in order to test if an email address is local, srsly will
recursively query Postfix via the `proxymap` socket until a final destination
is reached. If that final destination matches a regular expression configured
in srsly's configuration file, the address will be considered to be local.

Ideally, a "libpostmap" library would allow an application to perform the same
queries done by the `postmap` command from Postfix without having to provide
specific support for multiple databases. Until such a library is available,
srsly will use the approach described above.

While the `proxymap` query protocol is well documented, srsly provides a number
of directives in the `proxymap` section of its configuration file
(`query_format`, `query_flags`, `query_socket`, `result_format` and
`result_value_separator`) that allow one to change the way queries are made
and results are parsed. This provides some flexibility against possible
changes in the Postfix query protocol without the need of recompiling srsly.
Please see `srslyd.conf(5)` for more details on the above configuration
directives.

## SRS edge cases

The use of SRS results in a number of edge cases which arise in the case of
a message with multiple recipients. Those edge cases are illustrated in a
generic example below. Similar situations that occur in different setups
can be considered variations of this example.

In this example, the `local*.com` domains are local to the SMTP server, while
any other domain is remote.

Consider an email sent from `a@example.com` to `x@local1.com` and
`y@local2.com`. Assume that the following redirect table is configured in
Postfix:

* `x@local1.com` translates to `x@external1.com` and to a local mail box;
* `y@local2.com` translates to `y@external1.com` and to `y@external2.com`.

The final result of the translations performed by Postfix will result in the
following deliveries (without considering SRS for now):

1. A message from `a@example.com` to `x@local1.com` (local delivery);
2. A message from `a@example.com` to `x@external1.com` and `y@external1.com`;
3. A message from `a@example.com` to `y@external2.com`.

With regards to SRS, two issues can be identified from the situation described
above. Case 1 doesn't need SRS because it's not a redirect, but there's no
way a milter application can force delivery to be split in order to avoid
performing address rewrites in some of them. Case 2 results in a single
message sent in one SMTP session, but this message is the result of
redirections performed by two different domains. Which of the domains should
be used by SRS to rewrite the sender address, `a@example.com`?

One possibility is to dumb down Postfix, forcing it to perform delivery to at
most one destination at a time. There are a number of issues with this approach,
the most important of them being the creation of a delivery bottleneck. Thus
this approach cannot be recommended.

The issue of choosing the correct forward domain for SRS is usually solved in
other implementations in a trivial way: a single forward domain is configured
and used for rewriting on every redirection. While this works, it makes a
server that hosts multiple domains vulnerable: if one of those domains uses the
service to send spam, the SRS forward domain could end up in a blacklist that
would affect every other domain performing redirections in this server.

It becomes clear then that there's no perfect solution for these SRS edge
cases. Here's the approach taken by srsly.

With regards to case 1 above, srsly will apply SRS even to the local delivery.
While this is not necessary, it should cause no issues either. The more
complicated scenario is the one of choosing a forward domain. In the example
above, we can observe the following:

* `x@local1.com` translates to one remote address and to one local address;
* `y@local2.com` translates to two remote addresses.

In order to decide which forward domain to use, srsly will take a probabilistic
decision, choosing either `local1.com` or `local2.com` randomly. This decision
is weighted by the number of unique remote destinations that result from the
address translations performed on the original recipients of the message.
As noted above, `x@local1.com` translates to one remote address while
``y@local2.com` translates to two remote addresses. This means that `local1.com`
will be the forward domain with probability 1/3, while `local2.com` will be
chosen with probability 2/3.

While the probabilistic decision can result in "weird" rewrites (for example,
the message from `a@example.com` to `y@external2.com` could have its MAIL FROM
address rewritten by SRS using the `local1.com` domain, which was never
involved in sending any messages to `external2.com` in the first place),
these rewrites are correct from the point of view of SPF, which will see in
the return-path a domain that has lists the MTA as an allowed server. They are,
in fact, no different in this regard than the usual scheme of using a single
forward domain for every redirect, with the added benefit of making it less
likely that a domain will end up in a blacklist due to misuse by malicious
forwarders, because it will not be chosen for rewriting on every redirection.
