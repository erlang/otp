#! /usr/bin/env perl
#
# %CopyrightBegin%
# 
# Copyright Ericsson AB 2009-2016. All Rights Reserved.
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
use strict;

# Generate ADT (Abstract Data Type) access and generation functions
# for internal records.
# 
# The following defines which ADT function sets that will be generated
# and which record fields that will be exponated.
#
# (FunctionBaseName => [RecordName, FieldName ...], ...)
my %Names = ('msg' => ['dns_rec', 'header', 'qdlist',
		       'anlist', 'nslist', 'arlist'],
	     'dns_rr' => ['dns_rr', 'domain', 'type', 'class', 'ttl', 'data'],
	     'dns_rr_opt' => ['dns_rr_opt', 'domain', 'type',
			      'udp_payload_size', 'ext_rcode', 'version',
			      'z', 'data'],
	     'dns_query' => ['dns_query', 'domain', 'type', 'class'],
	     'header' => ['dns_header', 'id', 'qr', 'opcode', 'aa', 'tc',
			  'rd', 'ra', 'pr', 'rcode']);
# The functions are defined in the __DATA__ section at the end.

# Read in __DATA__ and merge lines.
my $line = '';
my @DATA;
my @INDEX;
while(<DATA>) {
    chomp;
    $line .= $_;
    unless ($line =~ s/\\$//) {
	if ($line =~ s/^[+]//) {
	    push(@INDEX, $line);
	} else {
	    push(@DATA, $line);
	}
	$line = '';
    }
}

$" = ',';
$\ = "\n";
foreach my $Name (sort keys %Names) {
    my $r = $Names{$Name};
    # Create substitutions for this Name
    my ($Record, @Fields) = @{ $r };
    my @FieldMatchValues;
    my @FieldValueTuples;
    my @Values;
    my $n = $#{ $r };
    for my $i ( 1 .. $n ) {
	push(@FieldMatchValues, "$Fields[$i-1]=V$i");
	push(@FieldValueTuples, "{$Fields[$i-1],V$i}");
	push(@Values, "V$i");
    }
    # "@FieldMatchValues" = "field1=V1,field2=V2"...",fieldN=VN"
    # "@FieldMatchTuples" = "{field1,V1},{field2,V2}"...",{fieldN,VN}"
    # "@Values" = "V1,V2"...",VN"
    my @D = @DATA;
    foreach my $line (@D) {
	# Ignore !name lines
	if ($line =~ s/^\!(\S+)\s+//) {
	    next if $1 eq $Name;
	}
	my $m = 1;
	# For leading * iterate $n times, otherwise once
	$line =~ s/^\s*[*]// and $m = $n;
	for my $i ( 1 .. $m ) {
	    # For this iteration - substitute and print
	    my $Value = "V$i";
	    my $SemicolonDot = ";";
	    $SemicolonDot = "." if $i == $m;
	    my @ValuesIgnoreValue = @Values;
	    $ValuesIgnoreValue[$i-1] = '_';
	    # "$Value" = "V1" or "V2" or ... "VN"
	    # "@ValuesIgnoreValue" = "_,V2"...",VN"
	    #                     or "V1,_"...",VN"
	    #                     or ... "V1,V2"...",_"
	    $_ = $line;
	    s/FieldMatchValues\b/@FieldMatchValues/g;
	    s/FieldValueTuples\b/@FieldValueTuples/g;
	    s/Field\b/$Fields[$i-1]/g;
	    s/Name\b/$Name/g;
	    s/Record\b/$Record/g;
	    s/ValuesIgnoreValue\b/@ValuesIgnoreValue/g;
	    s/Values\b/@Values/g;
	    s/Value\b/$Value/g;
	    s/[;][.]/$SemicolonDot/g;
	    s/->\s*/->\n    /;
	    print;
	}
    }
}
for my $i ( 0 .. $#INDEX ) {
    my $line = $INDEX[$i];
    if ($line =~ s/^[*]//) {
	foreach my $Name (sort keys %Names) {
	    my $r = $Names{$Name};
	    my ($Record) = @{ $r };
	    $_ = $line;
	    s/Name\b/$Name/g;
	    s/Record\b/$Record/g;
	    s/->\s*/->\n    /;
	    print;
	}
    } else {
	print $line;
    }
}

# Trailing \ will merge line with the following.
# Leading * will iterate the (merged) line over all field names.
# Sub-words in the loop above are substituted.
__DATA__

%%
%% Abstract Data Type functions for #Record{}
%%
%% -export([Name/1, Name/2,
%%          make_Name/0, make_Name/1, make_Name/2, make_Name/3]).

%% Split #Record{} into property list
%%
Name(#Record{FieldMatchValues}) -> \
    [FieldValueTuples].

%% Get one field value from #Record{}
%%
*Name(#Record{Field=Value}, Field) -> \
    Value;
%% Map field name list to value list from #Record{}
%%
Name(#Record{}, []) -> \
    [];
*Name(#Record{Field=Value}=R, [Field|L]) -> \
    [Value|Name(R, L)];.

%% Generate default #Record{}
%%
make_Name() -> \
    #Record{}.

%% Generate #Record{} from property list
%%
make_Name(L) when is_list(L) -> \
    make_Name(#Record{}, L).

!dns_rr_opt %% Generate #Record{} with one updated field
!dns_rr_opt %%
!dns_rr_opt *make_Name(Field, Value) -> \
    #Record{Field=Value};
%%
%% Update #Record{} from property list
%%
make_Name(#Record{FieldMatchValues}, L) when is_list(L) -> \
    do_make_Name(L, Values).
do_make_Name([], Values) -> \
    #Record{FieldMatchValues};
*do_make_Name([{Field,Value}|L], ValuesIgnoreValue) -> \
    do_make_Name(L, Values);.

%% Update one field of #Record{}
%%
*make_Name(#Record{}=R, Field, Value) -> \
    R#Record{Field=Value};.

+%% Record type index
+%%
+*record_adts(#Record{}) -> Name;
+record_adts(_) -> undefined.
