# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2006-2016. All Rights Reserved.
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

$SIG{HUP} = 'IGNORE';
if (fork() == 0) {
    print "child\n";
    my $i = 0;
    for (;;) {
	sleep(5);
	print $i++, "\n";
    }
} else {
    print "hejsan\n";
    exit(1);
}
