# ``The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved via the world wide web at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# The Initial Developer of the Original Code is Ericsson Utvecklings AB.
# Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
# AB. All Rights Reserved.''
# 
#     $Id$
#

ifndef EXPECTED_AUTOCONF_VERSION
EXPECTED_AUTOCONF_VERSION=2.59
endif
SAVE_ARGS=$(ERL_TOP)/make/save_args
CONFIG_STATUS=$(CONFIGURE_DIR)/$(TARGET)/config.status
SAVED_CONFIG_FLAGS_FILE=$(CONFIGURE_DIR)/$(TARGET)/lazy.config.flags
SAVED_CONFIG_LOG=$(CONFIGURE_DIR)/$(TARGET)/config.log
CONFIG_CACHE_FILE=$(CONFIGURE_DIR)/$(TARGET)/lazy.config.cache
ALL_CONFIG_FLAGS=$(CONFIGURE_FLAGS) --no-create --no-recursion --cache-file=$(CONFIG_CACHE_FILE)

lazy_configure: save_config_flags $(CONFIG_STATUS)
	rm -f $(CONFIGURE_DIR)/config.log
	cd $(CONFIGURE_DIR) && $(CONFIG_STATUS)
	cat $(CONFIGURE_DIR)/config.log >> $(SAVED_CONFIG_LOG)
	rm -f $(CONFIGURE_DIR)/config.log

save_config_flags:
	$(SAVE_ARGS) $(SAVED_CONFIG_FLAGS_FILE) --- $(ALL_CONFIG_FLAGS)

$(SAVED_CONFIG_FLAGS_FILE): save_config_flags

$(CONFIGURE_DIR)/configure: $(CONFIGURE_DIR)/configure.in $(EXTRA_CONFIGURE_DEPENDENCIES)
	rm -f $(CONFIG_CACHE_FILE)
	@ exp_ac_vsn=$(EXPECTED_AUTOCONF_VERSION) ;                                \
	ac_vsn_blob=`autoconf --version` ;                                         \
	ac_vsn=`echo x$$ac_vsn_blob | sed "s|[^0-9]*\([0-9][^ \t\n]*\).*|\1|"` ;   \
	case "$$ac_vsn" in                                                         \
	    $$exp_ac_vsn)                                                          \
		;;                                                                 \
	    *)                                                                     \
		echo "***************************************************"  1>&2 ; \
		echo "***************************************************"  1>&2 ; \
		echo "*** WARNING: System might fail to configure or"       1>&2 ; \
		echo "***          might be erroneously configured"         1>&2 ; \
		echo "***          since autoconf version $$ac_vsn is used" 1>&2 ; \
		echo "***          instead of version $$exp_ac_vsn!"        1>&2 ; \
		echo "***************************************************"  1>&2 ; \
		echo "***************************************************"  1>&2 ; \
		;;                                                                 \
	esac
	cd $(CONFIGURE_DIR) && autoconf -f

$(CONFIGURE_DIR)/config.h.in: $(CONFIGURE_DIR)/configure.in $(CONFIGURE_DIR)/aclocal.m4
	cd $(CONFIGURE_DIR) && autoheader ./configure.in > ./config.h.in

$(CONFIG_STATUS): $(SAVED_CONFIG_FLAGS_FILE) $(CONFIGURE_DIR)/configure $(EXTRA_CONFIG_STATUS_DEPENDENCIES)
	rm -f $(CONFIGURE_DIR)/config.log
	cd $(CONFIGURE_DIR) && CONFIG_STATUS=$(CONFIG_STATUS) ./configure $(ALL_CONFIG_FLAGS)
	rm -f $(SAVED_CONFIG_LOG)
	mv $(CONFIGURE_DIR)/config.log $(SAVED_CONFIG_LOG)

lazy_configure_target_clean:
	rm -f $(CONFIG_STATUS)
	rm -f $(CONFIG_CACHE_FILE)
	rm -f $(SAVED_CONFIG_FLAGS_FILE)
	rm -f $(SAVED_CONFIG_LOG)

lazy_configure_clean: lazy_configure_target_clean
	rm -f $(CONFIGURE_DIR)/configure
	test ! -f $(CONFIGURE_DIR)/acconfig.h || rm -f $(CONFIGURE_DIR)/config.h.in

.PHONY: lazy_configure save_config_flags lazy_configure_clean

