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
SHAREDIR=$(DESTDIR)$(PREFIX)/share
DOCDIR=$(SHAREDIR/doc/cmdliner
BASHCOMPDIR=$(SHAREDIR)/bash-completion/completions
ZSHCOMPDIR=$(SHAREDIR)/zsh/site-functions
NATIVE=$(shell ocamlopt -version > /dev/null 2>&1 && echo true)
# EXT_LIB     by default value of OCaml's Makefile.config
# NATDYNLINK  by default value of OCaml's Makefile.config

INSTALL=install
B=_build
BASE=$(B)/src/cmdliner
TOOLBDIR=$(B)/src/tool
TOOL=$(TOOLBDIR)/cmdliner

ifeq ($(NATIVE),true)
	BUILD-EXE=build-native-exe
	BUILD-TARGETS=build-byte build-native build-native-exe build-completions
	INSTALL-TARGETS=install-common install-srcs install-byte install-native \
	                install-exe install-completions
	ifeq ($(NATDYNLINK),true)
	  BUILD-TARGETS += build-native-dynlink
	  INSTALL-TARGETS += install-native-dynlink
	endif
else
	BUILD-EXE=build-byte-exe
	BUILD-TARGETS=build-byte build-byte-exe build-completions
	INSTALL-TARGETS=install-common install-srcs install-byte install-exe \
	                install-completions
endif

all: $(BUILD-TARGETS)

install: $(INSTALL-TARGETS)

clean:
	ocaml build.ml clean

build-byte:
	ocaml build.ml cma

build-native:
	ocaml build.ml cmxa

build-native-dynlink:
	ocaml build.ml cmxs

build-byte-exe: build-byte
	ocaml build.ml bytexe

build-native-exe: build-native
	ocaml build.ml natexe

build-completions: $(BUILD-EXE)
	$(TOOL) generic-completion bash > $(TOOLBDIR)/bash-completion.sh
	$(TOOL) tool-completion bash cmdliner > $(TOOLBDIR)/bash-cmdliner.sh
	$(TOOL) generic-completion zsh > $(TOOLBDIR)/zsh-completion.sh
	$(TOOL) tool-completion zsh cmdliner > $(TOOLBDIR)/zsh-cmdliner.sh

prepare-prefix:
	$(INSTALL) -d "$(BINDIR)" "$(LIBDIR)"

install-common: prepare-prefix
	$(INSTALL) -m 644 pkg/META $(BASE).cmi $(BASE).cmti "$(LIBDIR)"
	$(INSTALL) -m 644 cmdliner.opam "$(LIBDIR)/opam"

install-srcs: prepare-prefix
	$(INSTALL) -m 644 $(wildcard $(BASE)*.mli) $(wildcard $(BASE)*.ml) \
	  "$(LIBDIR)"

install-byte: prepare-prefix
	$(INSTALL) -m 644 $(BASE).cma "$(LIBDIR)"

install-native: prepare-prefix
	$(INSTALL) -m 644 $(BASE).cmxa $(BASE)$(EXT_LIB) $(wildcard $(BASE)*.cmx) \
	  "$(LIBDIR)"

install-native-dynlink: prepare-prefix
	$(INSTALL) -m 644 $(BASE).cmxs "$(LIBDIR)"

install-exe:
	$(INSTALL) -m 755 "$(B)/src/tool/cmdliner" "$(BINDIR)/cmdliner"

install-doc:
	$(INSTALL) -d "$(DOCDIR)/odoc-pages"
	$(INSTALL) -m 644 CHANGES.md LICENSE.md README.md "$(DOCDIR)"
	$(INSTALL) -m 644 doc/index.mld doc/cli.mld doc/examples.mld \
	   doc/tutorial.mld doc/cookbook.mld doc/tool_man.mld "$(DOCDIR)/odoc-pages"

install-completions:
	$(INSTALL) -d "$(BASHCOMPDIR)"
	$(INSTALL) -m 644 $(TOOLBDIR)/bash-completion.sh \
	  "$(BASHCOMPDIR)/_cmdliner_generic"
	$(INSTALL) -m 644 $(TOOLBDIR)/bash-cmdliner.sh "$(BASHCOMPDIR)/cmdliner"
	$(INSTALL) -d "$(ZSHCOMPDIR)"
	$(INSTALL) -m 644 $(TOOLBDIR)/zsh-completion.sh \
	  "$(ZSHCOMPDIR)/_cmdliner_generic"
	$(INSTALL) -m 644 $(TOOLBDIR)/zsh-cmdliner.sh "$(ZSHCOMPDIR)/_cmdliner"

.PHONY: all install install-doc clean build-byte build-native \
	build-native-dynlink build-byte-exe build-native-exe build-completions \
	prepare-prefix install-common install-byte install-native install-dynlink \
	install-exe install-completions
