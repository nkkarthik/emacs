# Private fleet build entry point.
#
# Emacs's ordinary public build remains the generated/default Makefile.  Fleet
# policy lives in git-crypt protected files below a/ and z/; this open shim
# deliberately contains no machine, product, signing, or rollout inventory.

.DEFAULT_GOAL := all

E_REPO_ROOT := $(patsubst %/,%,$(dir $(abspath $(lastword $(MAKEFILE_LIST)))))
export E_REPO_ROOT

PRIVATE_MAKEFILES := $(E_REPO_ROOT)/a/e.mk $(E_REPO_ROOT)/z/fleet.mk
PRIVATE_MAKEFILES_READY := $(shell \
	for file in $(PRIVATE_MAKEFILES); do \
		test -r "$$file" && \
		test "$$(LC_ALL=C head -c 1 "$$file")" = "$$(printf '\043')" \
			|| exit 1; \
	done; \
	printf ready)

ifeq ($(PRIVATE_MAKEFILES_READY),ready)
include $(PRIVATE_MAKEFILES)
else
$(error Fleet build rules are unavailable; unlock the private checkout)
endif
