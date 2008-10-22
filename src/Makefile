
include ../../../support/include.mk

include ../vsn.mk
VSN=$(FD_SERVER_VSN)

ifeq ($(TYPE),debug)
DEBUG_FLAGS = -Ddebug
else
DEBUG_FLAGS =
endif

MODULES= fdsrv

EBIN = ../ebin
EBIN_FILES = $(MODULES:%=$(EBIN)/%.beam) $(EBIN)/fd_server.app


all: $(EBIN_FILES)

debug:
	$(MAKE) DEBUG=-DDEBUG
clean:
	rm -rf $(EBIN_FILES) ../doc/*.html ../doc/edoc-info \
	  ../doc/stylesheet.css ../doc/erlang.png

docs: ../doc/index.html

../doc/index.html: $(MODULES:%=%.erl)
	erl -noshell -run edoc_run application "fd_server" '".."' '[]'
