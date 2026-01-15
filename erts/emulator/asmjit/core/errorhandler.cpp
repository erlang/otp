// This file is part of AsmJit project <https://asmjit.com>
//
// See asmjit.h or LICENSE.md for license and copyright information
// SPDX-License-Identifier: Zlib

#include "../core/api-build_p.h"
#include "../core/errorhandler.h"

ASMJIT_BEGIN_NAMESPACE

ErrorHandler::ErrorHandler() noexcept {}
ErrorHandler::~ErrorHandler() noexcept {}

void ErrorHandler::handleError(Error err, const char* message, BaseEmitter* origin) {
  DebugUtils::unused(err, message, origin);
}

ASMJIT_END_NAMESPACE
