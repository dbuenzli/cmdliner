# To be used by system package managers to bootstrap opam. topkg
# cannot be used as it needs opam-installer which is provided by opam
# itself.

# Typical usage:
#
# make all
# make install PREFIX=/usr/local
# make install-doc PREFIX=/usr/local

# Adjust the following on the cli invocation for configuring

-include $(shell ocamlc -where)/Makefile.config

PREFIX=/usr
BINDIR=$(DESTDIR)$(PREFIX)/bin
LIBDIR=$(DESTDIR)$(PREFIX)/lib/ocaml/cmdliner
DOCDIR=$(DESTDIR)$(PREFIX)/share/doc/cmdliner
NATIVE=$(shell ocamlopt -version > /dev/null 2>&1 && echo true)
# EXT_LIB     by default value of OCaml's Makefile.config
# NATDYNLINK  by default value of OCaml's Makefile.config

INSTALL=install
B=_build
BASE=$(B)/src/cmdliner

ifeq ($(NATIVE),true)
	BUILD-TARGETS=build-byte build-native
	INSTALL-TARGETS=install-common install-byte install-native
	ifeq ($(NATDYNLINK),true)
	  BUILD-TARGETS += build-native-dynlink
	  INSTALL-TARGETS += install-native-dynlink
	endif
else
	BUILD-TARGETS=build-byte
	INSTALL-TARGETS=install-common install-byte
endif

all: $(BUILD-TARGETS)

install: $(INSTALL-TARGETS)

install-doc:
	$(INSTALL) -d "$(DOCDIR)/odoc-pages"
	$(INSTALL) CHANGES.md LICENSE.md README.md "$(DOCDIR)"
	$(INSTALL) doc/index.mld doc/cli.mld doc/examples.mld doc/tutorial.mld \
	           doc/cookbook.mld doc/tool_man.mld "$(DOCDIR)/odoc-pages"

clean:
	ocaml build.ml clean

build-byte:
	ocaml build.ml cma

build-native:
	ocaml build.ml cmxa
	ocaml build.ml exe

build-native-dynlink:
	ocaml build.ml cmxs

prepare-prefix:
	$(INSTALL) -d "$(BINDIR)" "$(LIBDIR)"

install-common: prepare-prefix
	$(INSTALL) pkg/META $(BASE).mli $(BASE).cmi $(BASE).cmti "$(LIBDIR)"
	$(INSTALL) cmdliner.opam "$(LIBDIR)/opam"

install-byte: prepare-prefix
	$(INSTALL) $(BASE).cma "$(LIBDIR)"

install-native: prepare-prefix
	$(INSTALL) $(BASE).cmxa $(BASE)$(EXT_LIB) $(wildcard $(B)/cmdliner*.cmx) \
  "$(LIBDIR)"
	$(INSTALL) -m 755 $(B)/bin/cmdliner.exe "$(BINDIR)/cmdliner"

install-native-dynlink: prepare-prefix
	$(INSTALL) $(BASE).cmxs "$(LIBDIR)"

.PHONY: all install install-doc clean build-byte build-native \
	build-native-dynlink prepare-prefix install-common install-byte \
  install-native install-dynlink
