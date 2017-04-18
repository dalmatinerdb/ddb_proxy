VERSION=0.3.2
COMPONENT_INTERNAL=dpx
COMPONENT=dpx
DEPS="erlang" "coreutils" "sudo"

include ../../_build/default/lib/fifo_utils/priv/pkg.mk

.PHONY: prepare

prepare:
	-rm -r $(STAGE_DIR)/$(COMPONENT)
	cp -r ../../_build/prod/rel/$(COMPONENT_INTERNAL) $(STAGE_DIR)/$(COMPONENT)

package: prepare $(FILE).tgz

clean: clean-pkg
	-rm *.tgz

