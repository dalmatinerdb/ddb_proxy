include fifo.mk

all:
	rebar3 compile

clean:
	$(REBAR) clean
	make -C rel/deb clean

rel: update
	$(REBAR) as prod release

deb-clean:
	make -C rel/deb clean

deb-prepare:
	$(REBAR) as deb compile
	$(REBAR) as deb release
	make -C rel/deb prepare
