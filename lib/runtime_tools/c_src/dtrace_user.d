/*
 * %CopyrightBegin%
 *
 * Copyright Scott Lystig Fritchie 2011-2016.
 * All Rights Reserved.
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

provider erlang {
    /*
     * The set of probes for use by Erlang code ... moved from here to
     * erts/emulator/beam/erlang_dtrace.d until a more portable solution is
     * found; see erlang_dtrace.d for details.
     */
};

#pragma D attributes Evolving/Evolving/Common provider erlang provider
#pragma D attributes Private/Private/Common provider erlang module
#pragma D attributes Private/Private/Common provider erlang function
#pragma D attributes Evolving/Evolving/Common provider erlang name
#pragma D attributes Evolving/Evolving/Common provider erlang args
