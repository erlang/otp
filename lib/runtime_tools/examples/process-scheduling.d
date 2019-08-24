/* example usage: dtrace -q -s /path/to/process-scheduling.d */
/*
 * %CopyrightBegin%
 *
 * Copyright Scott Lystig Fritchie 2011-2016. All Rights Reserved.
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

erlang*:::process-scheduled
{
    printf("  Schedule pid %s mfa %s\n", copyinstr(arg0), copyinstr(arg1));
}

erlang*:::process-unscheduled
{
    printf("Unschedule pid %s\n", copyinstr(arg0));
}

erlang*:::process-hibernate
{
    printf("  Hibernate pid %s resume mfa %s\n",
           copyinstr(arg0), copyinstr(arg1));
}
