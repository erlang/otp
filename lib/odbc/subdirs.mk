#-*-makefile-*-   ; force emacs to enter makefile-mode

ifeq ($(ODBC_CAN_BUILD_DRIVER), true)
   SUB_DIRECTORIES = src c_src doc
else
  SUB_DIRECTORIES = src doc
endif

# %CopyrightBegin%
#
# SPDX-License-Identifier: Apache-2.0
#
# Copyright Ericsson AB 1996-2026. All Rights Reserved.
#
# %CopyrightEnd%
