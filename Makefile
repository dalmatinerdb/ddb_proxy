APP=dpx
include fifo.mk

all:
	rebar3 compile

clean:
	$(REBAR) clean
	make -C rel/deb clean

rel: FORCE
	$(REBAR) as prod release

deb-clean:
	make -C rel/deb clean

deb-prepare:
	$(REBAR) as deb compile
	$(REBAR) as deb release
	make -C rel/deb prepare

deb-package: deb-prepare
	make -C rel/deb package
	
package: rel
	make -C rel/pkg package

FORCE:
