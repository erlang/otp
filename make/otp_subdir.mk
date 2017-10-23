# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 1997-2011. All Rights Reserved.
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
# 
# %CopyrightEnd%
#
# Make include file for otp

.PHONY: debug opt lcnt release docs release_docs tests release_tests \
	clean depend valgrind static_lib

#
# Targets that don't affect documentation directories
#
opt debug lcnt release docs release_docs tests release_tests clean depend valgrind static_lib xmllint:
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
