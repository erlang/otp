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

my $run_erl = shift;
my $defuncter = shift;
my $pipe = shift;
my $log_dir = shift;
my $cmd = "run_erl $pipe $log_dir \"$^X $defuncter\"";
my $pid;

if (($pid = fork()) == 0) {
    #print join(" ", $run_erl, $pipe, $log_dir, "$^X $defuncter");
    exec($run_erl, $pipe, $log_dir, "$^X $defuncter");
    die "ERROR: exec failed: $!\n";
} elsif ($pid > 0) {
    sleep(1);
    my $res = waitpid($pid, 0);
    if ($res == $pid) {
	print "OK\n";
	exit(0);
    }
    die "ERROR: waitpid($pid, 0) returned $res\n";
} else {
    die "ERROR: fork() failed: $!\n";
}
