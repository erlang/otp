# snmp-v2tov1.awk
# nawk script - pass 1 of translation from SNMP v2 SMI to v1 SMI
# mbj@erlang.ericsson.se 971114
#

# Translate v2 IMPORTs to their v1 equivalents
$[ = 1;			# set array base to 1
$, = ' ';		# set output field separator
$\ = "\n";		# set output record separator

line: while (<>) {
    chomp;	# strip record separator
    @Fld = split(' ', $_, 9999);
    if (/IMPORT/) {
	$import = 1;
	$isave = 0;
	print $_;
	next line;
    }
    if (($import == 1) && ($Fld[1] eq ';')) {
	$import = 0;
    }
    if (($import == 1) && ($Fld[1] ne 'FROM')) {
	for ($i = 1; $i <= $#Fld; $i++) {
	    $s = ',', $Fld[$i] =~ s/$s//;
	    $imp{$i + $isave} = $Fld[$i];
	}
	$isave = $isave + ($i - 1);
	next line;
    }
    if (($import == 1) && ($Fld[1] eq 'FROM')) {
	&print_imp($Fld[2], *imp, $isave);
	$isave = 0;
	next line;
    }

    # str is 1 if we're inside a string, and 0 otherwise.
    if (/\"/) {
	$str = 1;
    }
    if ($Fld[$#Fld] =~ /\"$/) {
	$str = 0;
    }

    # Just reprint all comments
    if (/^--/) {
	print $_;
	next line;
    }

    # Place comments around MODULE-IDENTITY
    if (/MODULE-IDENTITY/ && ($str == 0)) {
	$moduleid = 1;
	print '--', $_;
	next line;
    }
    if (($moduleid == 1) && ($Fld[1] eq '::=')) {
	$moduleid = 0;
	print '--', $_;
	next line;
    }
    if ($moduleid == 1) {
	print '--', $_;
	next line;
    }

    # Translate TEXTUAL-CONVENTION into an ordinary type assignement.
    # Place comments around body.
    if (/TEXTUAL-CONVENTION/ && ($str == 0)) {
	$textual = 1;

	print $Fld[1], $Fld[2];
	print '--TEXTUAL-CONVENTION';
	next line;
    }
    if (($textual == 1) && ($Fld[1] eq 'SYNTAX')) {
	$textual = 0;
	print "--SYNTAX\n";
	for ($i = 2; $i <= $#Fld; $i++) {
	    print $Fld[$i];
	}
	next line;
    }
    if ($textual == 1) {
	$s = '--', s/$s/-- --/;
	print '--', $_;
	next line;
    }

    # Translate OBJECT-IDENTITY into an OBJECT IDENTIFIER.
    # Place comments around body.
    if (/OBJECT-IDENTITY/ && ($str == 0)) {
	$objid = 1;

	print $Fld[1], 'OBJECT IDENTIFIER';
	print '--OBJECT-IDENTITY';
	next line;
    }
    if (($objid == 1) && ($Fld[1] eq '::=')) {
	$objid = 0;
	print $_;
	next line;
    }
    if ($objid == 1) {
	$s = '--', s/$s/-- --/;
	print '--', $_;
	next line;
    }

    # Place comments around MODULE-COMPLIANCE
    if (/MODULE-COMPLIANCE/ && ($str == 0)) {
	$modcomp = 1;
	print '--', $_;
	next line;
    }
    if (($modcomp == 1) && ($Fld[1] eq '::=')) {
	$modcomp = 0;
	print '--', $_;
	next line;
    }
    if ($modcomp == 1) {
	$s = '--', s/$s/-- --/;
	print '--', $_;
	next line;
    }

    # Place comments around OBJECT-GROUP
    if (/OBJECT-GROUP/ && ($str == 0)) {
	$objgr = 1;
	print '--', $_;
	next line;
    }
    if (($objgr == 1) && ($Fld[1] eq '::=')) {
	$objgr = 0;
	print '--', $_;
	next line;
    }
    if ($objgr == 1) {
	$s = '--', s/$s/-- --/;
	print '--', $_;
	next line;
    }

    if (/OBJECT-GROUP/) {
	print 'tjolaopp';
    }

    # Place comments around NOTIFICATION-GROUP
    if (/NOTIFICATION-GROUP/ && ($str == 0)) {
	$notgr = 1;
	print '--', $_;
	next line;
    }
    if (($notgr == 1) && ($Fld[1] eq '::=')) {
	$notgr = 0;
	print '--', $_;
	next line;
    }
    if ($notgr == 1) {
	$s = '--', s/$s/-- --/;
	print '--', $_;
	next line;
    }

    # Translate NOTIFICATION-TYPE into a TRAP-TYPE.
    if (/NOTIFICATION-TYPE/ && ($str == 0)) {
	$trap = 1;
	print $Fld[1], ' TRAP-TYPE';
	printf '    ENTERPRISE ';
	$tri = 1;
	next line;
    }
    if (($trap == 1) && ($Fld[1] eq 'OBJECTS')) {
	$tra{$tri++} = $_;
	next line;
    }
    if (($trap == 1) && ($Fld[1] eq 'STATUS')) {
	next line;
    }
    if (($trap == 1) && ($Fld[1] eq '::=')) {
	print $Fld[3];
	&pr_trap(*tra, $tri);
	printf '    ::= ';
	print $Fld[4];
	$tri = 1;
	$trap = 0;
	next line;
    }
    if ($trap == 1) {
	$tra{$tri++} = $_;
	next line;
    }

    if (/UNITS/) {
	$s = '--', s/$s/-- --/;
	print '--', $_;
	next line;
    }

    print $_;

    # Print v1 IMPORT statements

    # Print a trap
}

sub print_imp {
    local($mib, *imp, $isave) = @_;
    for ($i = 1; $i <= $isave; $i++) {
	if ($imp{$i} eq 'Counter32') {
	    print '   ', $imp{$i};
	    print '        FROM RFC1155-SMI';
	}
	elsif ($imp{$i} eq 'Gauge32') {
	    print '   ', $imp{$i};
	    print '        FROM RFC1155-SMI';
	}
	elsif ($imp{$i} eq 'TimeTicks') {
	    print '   ', $imp{$i};
	    print '        FROM RFC1155-SMI';
	}
	elsif ($imp{$i} eq 'Opaque') {
	    print '   ', $imp{$i};
	    print '        FROM RFC1155-SMI';
	}
	elsif ($imp{$i} eq 'IpAddress') {
	    print '   ', $imp{$i};
	    print '        FROM RFC1155-SMI';
	}
	elsif ($imp{$i} eq 'NetworkAddress') {
	    print '   ', $imp{$i};
	    print '        FROM RFC1155-SMI';
	}
	elsif ($imp{$i} eq 'enterprises') {
	    print '   ', $imp{$i};
	    print '        FROM RFC1155-SMI';
	}
	elsif ($imp{$i} eq 'private') {
	    print '   ', $imp{$i};
	    print '        FROM RFC1155-SMI';
	}
	elsif ($imp{$i} eq 'experimental') {
	    print '   ', $imp{$i};
	    print '        FROM RFC1155-SMI';
	}
	elsif ($imp{$i} eq 'mgmt') {
	    print '   ', $imp{$i};
	    print '        FROM RFC1155-SMI';
	}
	elsif ($imp{$i} eq 'internet') {
	    print '   ', $imp{$i};
	    print '        FROM RFC1155-SMI';
	}
	elsif ($imp{$i} eq 'directory') {
	    print '   ', $imp{$i};
	    print '        FROM RFC1155-SMI';
	}
	elsif ($imp{$i} eq 'DisplayString') {
	    print '   ', $imp{$i};
	    print '        FROM RFC1213-MIB';
	}
	elsif ($imp{$i} eq 'mib-2') {
	    print '   ', $imp{$i};
	    print '        FROM RFC1213-MIB';
	}
	elsif ($imp{$i} eq 'OBJECT-TYPE') {
	    print '   ', $imp{$i};
	    print '        FROM RFC-1212';
	}
	elsif ($imp{$i} eq 'Integer32') {
	    ;
	}
	elsif ($imp{$i} eq 'MODULE-IDENTITY') {
	    ;
	}
	elsif ($imp{$i} eq 'TEXTUAL-CONVENTION') {
	    ;
	}
	elsif ($imp{$i} eq 'OBJECT-IDENTITY') {
	    ;
	}
	elsif ($imp{$i} eq 'OBJECT-GROUP') {
	    ;
	}
	elsif ($imp{$i} eq 'MODULE-COMPLIANCE') {
	    ;
	}
	elsif ($imp{$i} eq 'NOTIFICATION-GROUP') {
	    ;
	}
	elsif ($imp{$i} eq 'NOTIFICATION-TYPE') {
	    print '    TRAP-TYPE';
	    print '        FROM RFC-1215';
	}
	elsif ($imp{$i} eq 'DateAndTime') {
	    print '   ', $imp{$i};
	    print '        FROM STANDARD-MIB';
	}
	elsif ($imp{$i} eq 'TruthValue') {
	    print '   ', $imp{$i};
	    print '        FROM STANDARD-MIB';
	}
	elsif ($imp{$i} eq 'RowStatus') {
	    print '   ', $imp{$i};
	    print '        FROM STANDARD-MIB';
	}
	else {
	    print '   ', $imp{$i};
	    print '        FROM', $mib;
	}
    }
}

sub pr_trap {
    local(*tra, $tri) = @_;
    for ($i = 1; $i < $tri; $i++) {
	print $tra{$i};
    }
}
