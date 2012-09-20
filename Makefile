all:
	ocaml setup.ml -configure
	ocaml setup.ml -build

install:
	groupadd --system srsly
	useradd                     \
		--system                  \
		--gid srsly               \
		--home /var/lib/srsly     \
		--comment "srsly daemon"  \
		--shell /bin/false        \
		srsly
	install -d -m0755 /etc/srsly
	install -d -m0700 /etc/srsly/srs_secrets.d
	install -d -m0755 /usr/lib/srsly
	install -d -m0755 /usr/sbin
	install -d -m0755 /var/lib/srsly
	install -d -m0755 /var/lib/srsly/dev
	install -d -m0755 /var/lib/srsly/etc
	install -d -m0755 /var/lib/srsly/lib
	install -m0600 etc/srslyd.conf /etc/srsly/srslyd.conf
	install -m0700 _build/src/srsly.native /usr/sbin/srsly
	install -m0700 _build/src/srsly_milter.native /usr/lib/srsly/srsly-milter
	install -m0700 _build/src/srslyd.native /usr/lib/srsly/srslyd
	touch /etc/srsly/srs_secret
	chown root:root /etc/srsly/srs_secret
	chmod 0600 /etc/srsly/srs_secret
	srsly new-secret > /etc/srsly/srs_secret

uninstall:
	rm -r /etc/srsly
	rm -r /usr/lib/srsly
	rm -r /var/lib/srsly
	rm /usr/sbin/srsly
	userdel srsly

clean:
	ocaml setup.ml -clean

distclean:
	ocaml setup.ml -distclean
