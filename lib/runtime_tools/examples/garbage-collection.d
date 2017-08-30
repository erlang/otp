/* example usage: dtrace -q -s /path/to/garbage-collection.d */
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

erlang*:::gc_major-start
{
    printf("GC major start pid %s need %d words\n", copyinstr(arg0), arg1);
}

erlang*:::gc_minor-start
{
    printf("GC minor start pid %s need %d words\n", copyinstr(arg0), arg1);
}

erlang*:::gc_major-end
{
    printf("GC major end pid %s reclaimed %d words\n", copyinstr(arg0), arg1);
}

erlang*:::gc_minor-start
{
    printf("GC minor end pid %s reclaimed %d words\n", copyinstr(arg0), arg1);
}
