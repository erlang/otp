# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1997-2011. All Rights Reserved.
# 
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
# 
# %CopyrightEnd%
#
# Make include file for otp

.PHONY: debug opt release docs release_docs tests release_tests \
	clean depend valgrind static_lib

#
# Targets that don't affect documentation directories
#
opt debug release docs release_docs tests release_tests clean depend valgrind static_lib:
	@set -e ;							\
	app_pwd=`pwd` ;							\
	if test -f vsn.mk; then						\
	    echo "=== Entering application" `basename $$app_pwd` ;	\
	fi ;								\
	for d in $(SUB_DIRECTORIES); do					\
	    if test -f $$d/SKIP ; then					\
		echo "=== Skipping subdir $$d, reason:" ;		\
		cat $$d/SKIP ;						\
		echo "===" ;						\
	    else							\
		if test ! -d $$d ; then					\
		    echo "=== Skipping subdir $$d, it is missing" ;	\
		else							\
		    (cd $$d && $(MAKE) $@) || exit $$? ;		\
		fi ;							\
	    fi ;							\
	done ;								\
	if test -f vsn.mk; then						\
	    if test release = $@ && test ! -f SKIP; then		\
		app=`basename $$app_pwd` ;				\
		app_vsn=`echo $$app | sed "y|abcdefghijklmnopqrstuvwxyz|ABCDEFGHIJKLMNOPQRSTUVWXYZ|"` ; \
		app_vsn=$${app_vsn}_VSN ;				\
		( $(MAKE) -f "$(ERL_TOP)/make/otp_released_app.mk"	\
			APP_PWD="$$app_pwd" APP_VSN=$$app_vsn APP=$$app	\
			TESTROOT="$(TESTROOT)" update)			\
		|| exit $$?  ;						\
	    fi	;							\
	    echo "=== Leaving application" `basename $$app_pwd` ;	\
	fi
