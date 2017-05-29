APP=dpx
include fifo.mk

all:
	$(REBAR) compile

clean:
	$(REBAR) clean
	$(MAKE) -C rel/deb clean

rel: FORCE
	$(REBAR) as prod release

deb-clean:
	$(MAKE) -C rel/deb clean

deb-prepare:
	$(REBAR) as deb compile
	$(REBAR) as deb release
	$(MAKE) -C rel/deb prepare

deb-package: deb-prepare
	$(MAKE) -C rel/deb package
	
package: rel
	$(MAKE) -C rel/pkg package

FORCE:
