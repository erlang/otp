/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1996-2016. All Rights Reserved.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * 
 * %CopyrightEnd%
 */
/* -----------------------------------------------------------------
 * erl_log:
 *
 * Provides a simple debug log for the Erlang emulator.
 * It simples echoes its standard intput to the console.
 *
 * Author:  Bjorn Gustavsson
 * Created: 1996-12-06
 * -----------------------------------------------------------------
 */

#include <windows.h>
#include <stdio.h>

static void print_last_error(char* message);

main()
{
  HANDLE in;
  HANDLE out;
  char sbuf[256];
  DWORD written;
  DWORD numChars;

  in = GetStdHandle(STD_INPUT_HANDLE);
  out = CreateFile("CONOUT$", GENERIC_WRITE, 0, NULL, OPEN_EXISTING,
		  FILE_ATTRIBUTE_NORMAL, NULL);
  if (out == INVALID_HANDLE_VALUE) {
    print_last_error("CreateFile");
    exit(1);
  }

  while (ReadFile(in, sbuf, sizeof(sbuf), &numChars, NULL) && numChars) {
    WriteFile(out, sbuf, numChars, &written, NULL);
  }
  return 0;
}

static void print_last_error(char* message)
{
    LPTSTR* lpBufPtr;

    FormatMessage(
		  FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
		  NULL,
		  GetLastError(),
		  MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		  (LPTSTR) &lpBufPtr,
		  0,
		  NULL);
    if (message == NULL)
      printf("%s", lpBufPtr);
    else
      printf("%s: %s\n", message, lpBufPtr);
}
