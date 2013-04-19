PREFIX ?= /opt/local
SHARE   = $(PREFIX)/share/check-riak
LIB     = $(SHARE)/lib
PROG    = check-riak

all:
	@echo 'nothing to do. use `install` or `uninstall`'
install:
	mkdir -p $(LIB)
	cp -f $(PROG) $(PREFIX)/bin/$(PROG)
	cp -r ./lib/* $(LIB)
uninstall:
	rm -f $(PREFIX)/bin/$(PROG)

.PHONY: all install uninstall
