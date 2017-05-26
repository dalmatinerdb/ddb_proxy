VERSION = $(BUILD_NUMBER) # THis does not seem to work at the moment
VERSION ?= 0
COMPONENT = dpx

.PHONY: prepare package

prepare:
	mkdir -p stage/data/dalmatinerpx/example
	mkdir -p stage/etc/systemd/system
	mkdir -p stage/usr/bin
	mkdir -p stage/usr/lib/dalmatinerpx/lib
	mkdir -p stage/usr/share/dalmatinerpx
	mkdir -p stage/var/lib/dalmatinerpx
	mkdir -p stage/var/log/dalmatinerpx
	mkdir -p stage/DEBIAN
	cp -r ../../_build/deb/rel/dalmatinerpx/bin stage/usr/lib/dalmatinerpx/
	cp -r ../../_build/deb/rel/dalmatinerpx/etc stage/data/dalmatinerpx/etc/
	cp -r ../../_build/deb/rel/dalmatinerpx/erts-* stage/usr/lib/dalmatinerpx/
	tar -cC ../../_build/deb/rel/dalmatinerpx/lib --exclude c_src --exclude src . | tar -xC stage/usr/lib/dalmatinerpx/lib
	cp -r ../../_build/deb/rel/dalmatinerpx/releases stage/usr/lib/dalmatinerpx/
	cp -r ../../_build/deb/rel/dalmatinerpx/share stage/usr/lib/dalmatinerpx/
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
