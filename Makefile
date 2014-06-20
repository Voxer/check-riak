PREFIX ?= /opt/local
SHARE   = $(PREFIX)/share/check-riak
PROG    = check-riak

all:
	@echo 'nothing to do. use `install` or `uninstall`'
install:
	cp -f $(PROG) $(PREFIX)/bin/$(PROG)

uninstall:
	rm -f $(PREFIX)/bin/$(PROG)

.PHONY: all install uninstall
