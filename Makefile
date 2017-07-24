# To be used by system package managers to bootstrap opam. topkg
# cannot be used as it needs opam-installer which is provided by opam
# itself.

# Typical usage:
#
# make all
# make install DESTDIR=/usr/local/lib/cmdliner
# make install-doc DOCDIR=/usr/local/doc/cmdliner


DESTDIR=/dev/null # Adjust on the cli invocation
DOCDIR=/dev/null  # Adjust on the cli invocation

CP=cp
OCAMLBUILD=ocamlbuild -use-ocamlfind

B=_build/src
BASE=$(B)/cmdliner


all: build-byte build-native build-native-dynlink

install: install-common install-byte install-native install-native-dynlink

install-doc:
	$(CP) CHANGES.md LICENSE.md README.md $(DOCDIR)

clean:
	$(OCAMLBUILD) -clean


build-byte:
	$(OCAMLBUILD) src/cmdliner.cma

build-native:
	$(OCAMLBUILD) src/cmdliner.cmxa

build-native-dynlink:
	$(OCAMLBUILD) src/cmdliner.cmxs

install-common:
	$(CP) pkg/META opam $(BASE).mli $(BASE).cmi $(BASE).cmti $(DESTDIR)

install-byte:
	$(CP) $(BASE).cma $(DESTDIR)

install-native:
	$(CP) $(BASE).cmxa $(BASE).a $(wildcard $(B)/cmdliner*.cmx) $(DESTDIR)

install-native-dynlink:
	$(CP) $(BASE).cmxs $(DESTDIR)

.PHONY: all install install-doc clean build-byte build-native \
	build-native-dynlink install-common install-byte install-native \
	install-dynlink
