VERSION = $(BUILD_NUMBER) # THis does not seem to work at the moment
VERSION ?= 0
COMPONENT = ddb

.PHONY: prepare package

prepare:
	mkdir -p stage/etc
	mkdir -p stage/etc/systemd/system
	mkdir -p stage/usr/bin
	mkdir -p stage/usr/lib/ddb_proxy/lib
	mkdir -p stage/usr/share/ddb_proxy
	mkdir -p stage/var/lib/ddb_proxy
	mkdir -p stage/var/log/ddb_proxy
	cp -r ../../_build/deb/rel/ddb_proxy/bin stage/usr/lib/ddb_proxy/
	cp -r ../../_build/deb/rel/ddb_proxy/etc stage/etc/ddb_proxy
	cp -r ../../_build/deb/rel/ddb_proxy/erts-* stage/usr/lib/ddb_proxy/
	tar -cC ../../_build/deb/rel/ddb_proxy/lib --exclude c_src --exclude src . | tar -xC stage/usr/lib/ddb_proxy/lib
	cp -r ../../_build/deb/rel/ddb_proxy/releases stage/usr/lib/ddb_proxy/
	cp -r ../../_build/deb/rel/ddb_proxy/share stage/usr/lib/ddb_proxy/
	cp -r systemd/ddb_proxy.service stage/etc/systemd/system/ddb_proxy.service
	cp -r ../../example/*conf stage/etc/ddb_proxy/

clean:
	rm -rf ./stage
	rm -f *.deb
