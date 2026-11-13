// This file is part of AsmJit project <https://asmjit.com>
//
// See <asmjit/core.h> or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include <asmjit/core/api_build_p.h>

#include <asmjit/core/error_handler.h>
#include <asmjit/core/globals.h>

ASMJIT_BEGIN_NAMESPACE

ErrorHandler::ErrorHandler() noexcept {}
ErrorHandler::~ErrorHandler() noexcept {}

void ErrorHandler::handle_error(Error err, const char* message, BaseEmitter* origin) {
  axl::maybe_unused(err, message, origin);
}

ASMJIT_END_NAMESPACE
