# 
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2006-2009. All Rights Reserved.
# 
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
# 
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
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
