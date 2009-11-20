# ------------------------------------------------------------
# Some tcl scripts used from erlang
# By Ola Samuelsson in May 1995
# © Ericsson Software Technology / Erlang System
# ------------------------------------------------------------

package require Tk 8.3

# Scrolled object.
# This is a script that creates a
# scrolled object of specified type
# and connects it with scrollbars.
#
# Give a name for the frame and you'll find
#   name.z, 
#   name.sy,
#   name.pad.sx
#   name.pad.it
#

proc so_create {type w} {
    set parent [frame $w] 
    eval {$type $parent.z -highlightt 0}
    $parent.z config -yscrollcommand [list $parent.sy set] \
	    -xscrollcommand [list $parent.pad.sx set]
    scrollbar $parent.sy -orient vertical  -takefocus 0 \
	    -command [list $parent.z yview]
    # create extra frame to hold pad
    frame $parent.pad
    scrollbar $parent.pad.sx -orient horizontal -takefocus 0 \
	    -command [list $parent.z xview]
    #create padding based on the scrollbars width
    set pad [expr [$parent.sy cget -width] + 2 * \
	    ([$parent.sy cget -bd] + \
	    [$parent.sy cget -highlightthickness])]
    frame $parent.pad.it -width $pad -height $pad
    # Arrange everything
    so_plain $parent
    return $parent
}

proc so_top_right {w} {
    so_unpack $w
    pack $w.pad -side top -fill x
    pack $w.pad.it -side right
    pack $w.pad.sx -side top -fill x
    pack $w.sy -side right -fill y
    pack $w.z -side left -fill both -expand true    
}

proc so_top_left {w} {
    so_unpack $w
    pack $w.pad -side top -fill x
    pack $w.pad.it -side left
    pack $w.pad.sx -side top -fill x
    pack $w.sy -side left -fill y
    pack $w.z -side right -fill both -expand true    
}

proc so_bottom_right {w} {
    so_unpack $w
    pack $w.pad -side bottom -fill x
    pack $w.pad.it -side right
    pack $w.pad.sx -side bottom -fill x
    pack $w.sy -side right -fill y
    pack $w.z -side left -fill both -expand true    
}

proc so_bottom_left {w} {
    so_unpack $w
    pack $w.pad -side bottom -fill x
    pack $w.pad.it -side left
    pack $w.pad.sx -side bottom -fill x
    pack $w.sy -side left -fill y
    pack $w.z -side right -fill both -expand true    
}

proc so_bottom {w} {
    so_unpack $w
    pack $w.pad -side bottom -fill x
    pack $w.pad.sx -side bottom -fill x
    pack $w.z -side left -fill both -expand true    
}

proc so_top {w} {
    so_unpack $w
    pack $w.pad -side top -fill x
    pack $w.pad.sx -side top -fill x
    pack $w.z -side left -fill both -expand true    
}

proc so_right {w} {
    so_unpack $w
    pack $w.sy -side right -fill y
    pack $w.z -side left -fill both -expand true    
}

proc so_left {w} {
    so_unpack $w
    pack $w.sy -side left -fill y
    pack $w.z -side right -fill both -expand true    
}

proc so_plain {w} {
    so_unpack $w
    pack $w.z -side left -fill both -expand true
}

proc so_unpack {w} {
    pack forget $w.pad.it
    pack forget $w.pad.sx
    pack forget $w.pad
    pack forget $w.sy
    pack forget $w.z
}

# ------------------------------------------------------------
# editor stuff

proc ed_load {w file} {
    if [catch {open $file r} fileId] {
	report "$fileId"
	return -code error
    } else {
	$w delete 1.0 end
	while {![eof $fileId]} {
	    $w insert end [read $fileId 1000]
	}
	close $fileId
    }
}

proc ed_save {w file} {
    if [catch {open $file w} fileId] {
	report "$fileId"
	return -code error
    } else {
	puts -nonewline $fileId [$w get 1.0 end]
	close $fileId
    }
}

# returns rect1 text1 rect2 text2 ...
proc mkgrid {canvas colws startrow endrow height font fg bg} {
    set res [list]
    set ncols [llength $colws]
    set y [expr ($startrow-1)*$height+$startrow]
    for {set row $startrow} {$row <= $endrow} {incr row} {
	set x 1
	for {set col 0} {$col < $ncols} {incr col} {
	    set colw [lindex $colws $col]
# each object is tagged q<columnNo>
# is this (ugly) way we can find to what column an object belongs
# even later...
	    set r [$canvas create re $x $y [expr $x+$colw] [expr $y+$height]\
		    -f $bg -outline $fg -tag "q$col"]
	    set t [$canvas create te [expr $x+2] [expr $y+2]\
		    -anch nw -fo $font -fi $fg -tag "q$col"]
	    $canvas raise $t
	    lappend res $r $t
	    set x [expr $x+$colw+1]
	}
	set y [expr $y+$height+1]
    }
    return $res
}

# new x values
proc calc_new_xs {colws} {
    set ncols [llength $colws]
    set res [list]
    set x 1
    for {set col 0} {$col < $ncols} {incr col} {
	lappend res $x
	set x [expr $x+1+[lindex $colws $col]]
    }
    lappend res $x
    return $res
}

proc resize_grid_cols {canvas colws} {
    set item 1
    set xs [calc_new_xs $colws]
    while {[set nbr_of_coords\
	    [llength [set coords [$canvas coords $item]]]] > 0} {
	set tags [$canvas itemcget $item -tag]
	set first [string first "q" $tags]
# find the column of the current object by
# searching for the q tag.
	set col [string range $tags [expr 1 + $first]\
		[expr [string wordend $tags $first] -1]]
	switch $nbr_of_coords {
	    2 {  # a text object
		set y [lindex $coords 1]
		set newx [expr [lindex $xs $col] + 2]
		$canvas coords $item $newx $y
	    }
	    4 { # a rectangle object
		set y1 [lindex $coords 1]
		set y2 [lindex $coords 3]
		set newx1 [lindex $xs $col]
		set newx2 [expr [lindex $xs [expr $col + 1]]-1]
		$canvas coords $item $newx1 $y1 $newx2 $y2
	    }
	}
        set item [expr $item+1]
    }
}

# ------------------------------------------------------------
# A wish init script to make it possible for
# Tcl/Tk and erlang to communicate.
# Written by Ola Samuelsson in August 1995
# © Ericsson Software Technology AB / Erlang Systems
# ------------------------------------------------------------

# Protocol:
# \1 =   it's an event
# \2 =   it's a reply for a call
# \3 =   it's a error reply for call
# \4 =   it's an error
# \5 =   stopbyte (end of message)

proc erlsend {args} {
    global outstream
    set msg [join $args]
    puts -nonewline $outstream [binary format Ica* \
	    [expr 1 + [string length $msg]] 1 $msg]
    flush $outstream
}

proc erlcall {w} {
    global outstream
    set errcode [catch $w result]
    if {$errcode == 0} {
	puts -nonewline $outstream [binary format Ica* \
		[expr 1 + [string length $result]] 2 $result]
	flush $outstream
    } else {
	puts -nonewline $outstream [binary format Ica* \
		[expr 1 + [string length $result]] 3 $result]
	flush $outstream
    }
}

proc erlexec {w} {
    set errcode [catch $w result]
    if {$errcode != 0} {
	global outstream
	puts -nonewline $outstream [binary format Ica* \
		[expr 1 + [string length $result]] 4 $result]
	flush $outstream
    }
}

proc erlerror {w} {
    global outstream
    puts -nonewline $outstream [binary format Ica* \
	    [expr 1 + [string length $w]] 4 $w]
    flush $outstream
}

proc report {s} {
    catch {console show}
    puts -nonewline stderr "$s\r\n"
}

wm withdraw .

# The stdin stream is by default in line buffering mode.
# We set non blocking so that if the line is large we are
# not blocked until we get all data. The gets command
# will never give us a line until we have read it all
# so we do nothing if we get the return code -1.
# Note that -1 also means eof so we check that. 

# FIXME: What is the default encoding on Unix?
# Do we need to set "-encoding iso8859-1" ?

# FIXME: If pipe we should do "catch {close $pipe}"
# but we don't do that on stdin do we? If not how
# do we unregister from 'fileevent'?

# The ending "vwait forever" will block
# until all streams are closed.
# FIXME: How are we terminated? No check for eof?

# If we got something on the command line after --
# we have a port number and we are to use sockets
# for the communication.

set privdir [lindex $argv 0]
set portno  [lindex $argv 1]
#report $argv
#report $privdir
#report $portno
set resfile [file join $privdir gs-xdefaults]

# FIXME we may use 'startupFile' as priority level to enable the user
# to use .Xdefaults to override things but I think this require that
# we change gs-xdefaults to use Tk*Option format?

if [catch {option readfile $resfile} err] {
    report "Error reading $resfile: $err"
}

if {$portno == ""} {

    global use_socket
    set use_socket 0
    set instream  stdin
    set outstream stdout

    # We are only allowed to set non blocking output
    # for pipes because sockets are bidirectional
    # and the fconfigure sets both input and output.

    fconfigure stdout -buffering none -blocking false \
	    -translation binary -encoding binary
} else {

    global use_socket
    set use_socket 1
    set sock [socket 127.0.0.1 $portno]
    set instream  $sock
    set outstream $sock

}

fconfigure $instream -buffering none -blocking true \
	-translation binary -encoding binary

fileevent $instream readable do_read

proc do_read {} {
    global instream

    binary scan [read $instream 4] I len

    if {[eof $instream]} {
	catch {close $instream}
	exit
    }

#    report {"LEN $len"}

    # FIXME need to read again if less then $len ?????
    set command [read $instream $len]

    if {[eof $instream]} {
	catch {close $instream}
	exit
    }

#    report {"INMSG $command EOMSG"}

    if [catch {uplevel #0 $command} msg] {
	report {$msg}
    } else {
	if {[string length $msg] != 0} {
#	    report {"OUTMSG $msg EOMSG"}
	    puts -nonewline $instream [binary format Ia* \
		    [string length $msg] $msg]
	}
    }
}
