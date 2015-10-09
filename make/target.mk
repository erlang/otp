#
# %CopyrightBegin%
#
# Copyright Ericsson AB 1998-2011. All Rights Reserved.
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

# Ensure that the make variable TARGET is set
#

ifeq ($(OVERRIDE_TARGET),)

ifeq ($(TARGET),)

TARGET := $(shell $(ERL_TOP)/erts/autoconf/config.guess)

else

endif

else

ifneq ($(TARGET),)

ifneq ($(TARGET), $(OVERRIDE_TARGET))
$(warning overriding $$(TARGET) = \
	"$(TARGET)" \
	with \
	$$(OVERRIDE_TARGET) = \
	"$(OVERRIDE_TARGET)")
else
endif

override TARGET := $(OVERRIDE_TARGET)

else

TARGET := $(OVERRIDE_TARGET)

endif

endif

ifneq ($(TARGET),)
ifneq ($(TARGET),win32)
ifneq ($(findstring vxworks,$(TARGET)),vxworks)
override TARGET := $(shell $(ERL_TOP)/erts/autoconf/config.sub $(TARGET))
else
endif
else
endif
else
endif

ifeq ($(TARGET),)
$(error Neither TARGET nor OVERRIDE_TARGET can be determined!)
else
endif
