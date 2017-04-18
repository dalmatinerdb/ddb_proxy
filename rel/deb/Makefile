VERSION = $(BUILD_NUMBER) # THis does not seem to work at the moment
VERSION ?= 0
COMPONENT = dpx

.PHONY: prepare package

prepare:
	mkdir -p stage/data/dalmatinerpx/example
	mkdir -p stage/etc/systemd/system
	mkdir -p stage/usr/bin
	mkdir -p stage/usr/lib/dpx/lib
	mkdir -p stage/usr/share/dpx
	mkdir -p stage/var/lib/dpx
	mkdir -p stage/var/log/dpx
	mkdir -p stage/DEBIAN
	cp -r ../../_build/deb/rel/dpx/bin stage/usr/lib/dpx/
	cp -r ../../_build/deb/rel/dpx/etc stage/data/dalmatinerpx/etc/
	cp -r ../../_build/deb/rel/dpx/erts-* stage/usr/lib/dpx/
	tar -cC ../../_build/deb/rel/dpx/lib --exclude c_src --exclude src . | tar -xC stage/usr/lib/dpx/lib
	cp -r ../../_build/deb/rel/dpx/releases stage/usr/lib/dpx/
	cp -r ../../_build/deb/rel/dpx/share stage/usr/lib/dpx/
	cp -r ../../example/*conf stage/data/dalmatinerpx/example/
	cp -r systemd/dalmatinerpx.service stage/etc/systemd/system/dalmatinerpx.service
	cp control stage/DEBIAN/	
	cp preinst stage/DEBIAN/
	cp postinst stage/DEBIAN/
	chmod 555 stage/DEBIAN/preinst
	chmod 555 stage/DEBIAN/postinst

package:
	dpkg-deb --build stage

clean:
	rm -rf ./stage
	rm -f *.deb
