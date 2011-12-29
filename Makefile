# ----------------------------------------------------------------------------
# Makefile.
# ----------------------------------------------------------------------------

ERL=erl
ERLC=erlc
DIALYZER=dialyzer
VERNAME=doyer-0.01

INSTALL_LIBDIR=`$(ERL) -noinput -eval 'io:format("~s",[code:lib_dir()])' -run init stop`
INSTALL_OWNER=#--owner root --group root

autoparam= \
	VERNAME="$(VERNAME)" \
	ERL=$(ERL) \
	$(NULL)

all: build run-example

build:
	mkdir -p _build/$(VERNAME)/ebin
	mkdir -p _build/$(VERNAME)/include
	mkdir -p _build/examples/ebin
	mkdir -p _build/t/ebin
	cp -f src/doyer.hrl _build/$(VERNAME)/include
	$(MAKE) $(autoparam) -C src
	$(MAKE) $(autoparam) -C t

run-example:
	@#TRACE=1 $(MAKE) $(autoparam) -C examples 2>&1 | tee log.txt; false
	$(MAKE) $(autoparam) -C examples
	$(ERL) -pz _build/$(VERNAME)/ebin -pz _build/examples/ebin -noinput -run example main
	$(ERL) -pz _build/$(VERNAME)/ebin -pz _build/t/ebin -noinput -run test main

clean:
	rm -rf *.beam _build

check: all
	test -e _build/check.plt || time dialyzer --build_plt --apps erts kernel stdlib compiler --output_plt _build/check.plt
	$(DIALYZER) \
	  --plt _build/check.plt \
	  -I _build/$(VERNAME)/include/ \
	  -pa _build/$(VERNAME)/ebin \
	  src/*.erl t/*.erl

dist: distdir
	tar zcvf $(VERNAME).tar.gz $(VERNAME)
	rm -rf $(VERNAME)

distdir:
	rm -rf $(VERNAME)
	mkdir $(VERNAME)
	{ cat MANIFEST || exit $$?; } \
	  | while read f; \
	    do \
	      d=`dirname $$f` || exit $$?; \
	      test -d $(VERNAME)/$$d || mkdir $(VERNAME)/$$d || exit $$?; \
	      cp -f $$f $(VERNAME)/$$d/ || exit $$?; \
	    done

install: build
	$(MAKE) INSTALL_LIBDIR=$(INSTALL_LIBDIR) install-2

install-2:
	install $(INSTALL_OWNER) -d "$(DESTDIR)$(INSTALL_LIBDIR)/$(VERNAME)/ebin"
	install $(INSTALL_OWNER) -d "$(DESTDIR)$(INSTALL_LIBDIR)/$(VERNAME)/include"
	for f in "_build/$(VERNAME)/ebin"/*.beam; \
	do \
	  b=`basename $$f` || exit $$?; \
	  install $(INSTALL_OWNER) --mode 444 "$$f" "$(DESTDIR)$(INSTALL_LIBDIR)/$(VERNAME)/ebin/$$b" || exit $$?; \
	done
	install $(INSTALL_OWNER) --mode 444 src/doyer.hrl "$(DESTDIR)$(INSTALL_LIBDIR)/$(VERNAME)/include"/doyer.hrl

# ----------------------------------------------------------------------------
# End of File.
# ----------------------------------------------------------------------------
