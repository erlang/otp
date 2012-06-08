#!/usr/bin/perl

# bargraph.pl: a bar graph builder that supports stacking and clustering.
# Modifies gnuplot's output to fill in bars and add a legend.
#
# Copyright (C) 2004-2010 Derek Bruening <iye@alum.mit.edu>
# http://www.burningcutlery.com/derek/bargraph/
# http://code.google.com/p/bargraphgen/
#
# Contributions:
# * sorting by data contributed by Tom Golubev
# * legendfill= code inspired by Kacper Wysocki's code
# * =barsinbg option contributed by Manolis Lourakis
# * gnuplot 4.3 fixes contributed by Dima Kogan
# * ylabelshift contributed by Ricardo Nabinger Sanchez.
# * Error bar code contributed by Mohammad Ansari.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or (at
# your option) any later version.
# 
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307,
# USA.

###########################################################################
###########################################################################

$usage = "
Usage: $0 [-gnuplot] [-fig] [-pdf] [-png [-non-transparent]] [-eps]
  [-gnuplot-path <path>] [-fig2dev-path <path>] <graphfile>

File format:
<graph parameters>
<data>

Graph parameter types:
<value_param>=<value>
=<bool_param>
";

# Main features:
# * Stacked bars of 9+ datasets
# * Clustered bars of 8+ datasets
# * Clusters of stacked bars
# * Lets you keep your data in table format, or separated but listed in
#   the same file, rather than requiring each dataset to be in a separate file
# * Custom gnuplot command pass-through for fine-grained customization
#    without having a separate tool chain step outside the script
# * Color control
# * Font face control and limited font size control
# * Automatic arithmetic or harmonic mean calculation
# * Automatic legend creation
# * Automatic sorting, including sorting into SPEC CPU 2000 integer and 
#   floating point benchmark groups and sorting by data 
#
# Multiple data sets can either be separated by =multi,
#   or in a table with =table.  Does support incomplete datasets,
#   but issues warning.
# For clusters of stacked bars, separate your stacked data for each
#   cluster with =multi or place in a table, and separate (and optionally
#   name) each cluster with multimulti=
# For complete documentation see
#   http://www.burningcutlery.com/derek/bargraph/

# This is pre-release version 4.7.
# Changes in version 4.7, not yet released:
#    * switched to boxerror to avoid the data marker for yerrorbars
#    * added custfont= feature
#    * fixed bugs in centering in-graph legend box
#    * added fudging for capital letters to work around gnuplot weirdness
#      (issue #15)
# Changes in version 4.6, released January 31, 2010:
#    * added automatic legend placement, including automatically
#      finding an empty spot inside the graph, via the 'inside',
#      'right', 'top', and 'center' keywords in legendx= and legendy=
#    * added logscaley= to support logarithmic y values
#    * added leading_space_mul=, intra_space_mul=, and barwidth=
#      parameters to control spacing and bar size.  as part of this change,
#      bars are no longer placed in an integer-based fashion.
#    * fixed gnuplot 4.0 regression
# Changes in version 4.5, released January 17, 2010:
#    * changed legends to have a white background and border outline
#      by default, with legendfill= option (inspired by Kacper
#      Wysocki's code) to control the background fill color (and
#      whether there is a fill) and =nolegoutline to turn off the
#      outline
#    * the legend bounding box is now much more accurately calculated
#    * eliminated =patterns color with recent gnuplots
#    * added legendfontsz= option
#    * added =legendinbg option (legend in fg is new default)
#    * added =reverseorder option (from Tom Golubev)
#    * added =sortdata_ascend option (from Tom Golubev)
#    * added =sortdata_descend option (from Tom Golubev)
#    * added =barsinbg option (from Manolis Lourakis)
#    * added horizline= option (issue #2)
#    * added grouprotateby= option (issue #1)
# Changes in version 4.4, released August 10, 2009:
#    * added rotateby= option
#    * added xticshift= option
#    * added support for gnuplot 4.3 (from Dima Kogan)
#    * added ylabelshift= option (from Ricardo Nabinger Sanchez)
#    * added =stackabs option
# Changes in version 4.3, released June 1, 2008:
#    * added errorbar support (from Mohammad Ansari)
#    * added support for multiple colors in a single dataset
#    * added -non-transparent option to disable png transparency
#    * added option to disable the legend
#    * added datascale and datasub options
# Changes in version 4.2, released May 25, 2007:
#    * handle gnuplot 4.2 fig terminal output
# Changes in version 4.1, released April 1, 2007:
#    * fixed bug in handling scientific notation
#    * fixed negative offset font handling bug
# Changes in version 4.0, released October 16, 2006:
#    * added support for clusters of stacked bars
#    * added support for font face and size changes
#    * added support for negative maximum values
# Changes in version 3.0, released July 15, 2006:
#    * added support for spaces and quotes in x-axis labels
#    * added support for missing values in table format
#    * added support for custom table delimiter
#    * added an option to suppress adding of commas
# Changes in version 2.0, released January 21, 2006:
#    * added pattern fill support
#    * fixed errors in large numbers of datasets:
#      - support > 8 clustered bars
#      - fix > 9 dataset color bug
#      - support > 25 stacked bars

# we need special support for bidirectional pipe
use IPC::Open2;

###########################################################################
###########################################################################

# The full set of Postscript fonts supported by FIG
%fig_font = (
    'Default'                            => -1,      
    'Times Roman'                        =>  0,      
    # alias
    'Times'                              =>  0,      
    'Times Italic'                       =>  1,      
    'Times Bold'                         =>  2,      
    'Times Bold Italic'                  =>  3,      
    'AvantGarde Book'                    =>  4,      
    'AvantGarde Book Oblique'            =>  5,      
    'AvantGarde Demi'                    =>  6,      
    'AvantGarde Demi Oblique'            =>  7,      
    'Bookman Light'                      =>  8,      
    'Bookman Light Italic'               =>  9,      
    'Bookman Demi'                       => 10,      
    'Bookman Demi Italic'                => 11,      
    'Courier'                            => 12,      
    'Courier Oblique'                    => 13,      
    'Courier Bold'                       => 14,      
    'Courier Bold Oblique'               => 15,      
    'Helvetica'                          => 16,      
    'Helvetica Oblique'                  => 17,      
    'Helvetica Bold'                     => 18,      
    'Helvetica Bold Oblique'             => 19,      
    'Helvetica Narrow'                   => 20,      
    'Helvetica Narrow Oblique'           => 21,      
    'Helvetica Narrow Bold'              => 22,      
    'Helvetica Narrow Bold Oblique'      => 23,      
    'New Century Schoolbook Roman'       => 24,      
    'New Century Schoolbook Italic'      => 25,      
    'New Century Schoolbook Bold'        => 26,      
    'New Century Schoolbook Bold Italic' => 27,      
    'Palatino Roman'                     => 28,      
    'Palatino Italic'                    => 29,      
    'Palatino Bold'                      => 30,      
    'Palatino Bold Italic'               => 31,      
    'Symbol'                             => 32,      
    'Zapf Chancery Medium Italic'        => 33,      
    'Zapf Dingbats'                      => 34,      
);

###########################################################################
###########################################################################

# default is to output eps
$output = "eps";
$gnuplot_path = "gnuplot";
$fig2dev_path = "fig2dev";
$debug_seefig_unmod = 0;
$png_transparent = 1;
$verbose = 0;

# FIXME i#13: switch to GetOptions
while ($#ARGV >= 0) {
    if ($ARGV[0] eq '-fig') {
        $output = "fig";
    } elsif ($ARGV[0] eq '-rawfig') {
        $output = "fig";
        $debug_seefig_unmod = 1;
    } elsif ($ARGV[0] eq '-gnuplot') {
        $output = "gnuplot";
    } elsif ($ARGV[0] eq '-pdf') {
        $output = "pdf";
    } elsif ($ARGV[0] eq '-png') {
        $output = "png";
    } elsif ($ARGV[0] eq '-non-transparent') {
        $png_transparent = 0;
    } elsif ($ARGV[0] eq '-eps') {
        $output = "eps";
    } elsif ($ARGV[0] eq '-gnuplot-path') {
        die $usage if ($#ARGV <= 0);
        shift;
        $gnuplot_path = $ARGV[0];
    } elsif ($ARGV[0] eq '-fig2dev-path') {
        die $usage if ($#ARGV <= 0);
        shift;
        $fig2dev_path = $ARGV[0];
    } elsif ($ARGV[0] eq '-v') {
        $verbose = 1;
    } else {
        $graph = $ARGV[0];
        shift;
        last;
    }
    shift;
}
die $usage if ($#ARGV >= 0 || $graph eq "");
open(IN, "< $graph") || die "Couldn't open $graph";

# gnuplot syntax varies by version
$gnuplot_version = `$gnuplot_path --version`;
$gnuplot_version =~ /gnuplot ([\d\.]+)/;
$gnuplot_version = $1;
$gnuplot_uses_offset = 1;
$gnuplot_uses_offset = 0 if ($gnuplot_version <= 4.0);

# support for clusters and stacked
$stacked = 0;
$stacked_absolute = 0;
$stackcount = 1;
$clustercount = 1;
$plotcount = 1; # multi datasets to cycle colors through
$dataset = 0;
$table = 0;
# leave $column undefined by default

# support for clusters of stacked
$stackcluster = 0;
$groupcount = 1;
$grouplabels = 0;
$groupset = 0;
$grouplabel_rotateby = 0;

$title = "";
$xlabel = "";
$ylabel = "";
$usexlabels = 1;
# xlabel rotation seems to not be supported by gnuplot

# default is to rotate x tic labels by 90 degrees
# when tic labels are rotated, need to shift axis label down. -1 is reasonable:
$xlabelshift = "0,-1";
$xticsopts = "rotate";
$xticshift = "0,0";
$ylabelshift = "0,0";

$sort = 0;
# sort into SPEC CPU 2000 and JVM98 groups: first, SPECFP, then SPECINT, then JVM
$sortbmarks = 0;
$sortdata_ascend = 0;   # sort by data, from low to high
$sortdata_descend = 0;  # sort by data, from high to low
$reverseorder = 0;      # if not sorting, reverse order
$bmarks_fp = "ammp applu apsi art equake facerec fma3d galgel lucas mesa mgrid sixtrack swim wupwise";
$bmarks_int = "bzip2 crafty eon gap gcc gzip mcf parser perlbmk twolf vortex vpr";
$bmarks_jvm = "check compress jess raytrace db javac mpegaudio mtrt jack checkit";

$ymax = "";
$ymin = 0;
$calc_min = 1;

$lineat = "";
$gridx = "noxtics";
$gridy = "ytics";
$noupperright = 0;

# space on both ends of graph
$leading_space_mul = 0; # set below
# space between clusters
$intra_space_mul = 0; # set below
# width of bars
$barwidth = 0; # set below

$invert = 0;

$use_mean = 0;
$arithmean = 0; # else, harmonic
# leave $mean_label undefined by default

$datascale = 1;
$datasub = 0;
$percent = 0;
$base1 = 0;
$yformat = "%.0f";

$logscaley = 0;

$extra_gnuplot_cmds = "";

# if still 0 later will be initialized to default
$use_legend = 1;
$legendx = 'inside';
$legendy = 'top';
$legend_fill = 'white';
$legend_outline = 1;
$legend_font_size = 0; # if left at 0 will be $font_size-1

# use patterns instead of solid fills?
$patterns = 0;
# there are only 22 patterns that fig supports
$max_patterns = 22;

$custom_colors = 0;
$color_per_datum = 0;

# fig depth: leave enough room for many datasets
# (for stacked bars we subtract 2 for each)
# but max gnuplot terminal depth for fig is 99!
# fig depth might change later via =barsinbg
$legend_depth = 0; # 100 for =legendinbg
$plot_depth = 98;

$add_commas = 1;

$font_face = $fig_font{'Default'};
$font_size = 10.0;
# let user have some control over font bounding box heuristic
$bbfudge = 1.0;

# yerrorbar support
$yerrorbars = 0;

# are bars in the foreground (default) or background of plot?
$barsinbg = 0;

# sentinel value
$sentinel = 999999;

while (<IN>) {
    next if (/^\#/ || /^\s*$/);
    # line w/ = is a control line (except =>)
    # FIXME i#13: switch to GetOptions
    if (/=[^>]/) {
        if (/^=cluster(.)/) {
            $splitby = $1;
            s/=cluster$splitby//;
            chop;
            @legend = split($splitby, $_);
            $clustercount = $#legend + 1;
            $plotcount = $clustercount;
        } elsif (/^=stacked(.)/) {
            $splitby = $1;
            s/=stacked$splitby//;
            chop;
            @legend = split($splitby, $_);
            $stackcount = $#legend + 1;
            $plotcount = $stackcount;
            $stacked = 1;
            # reverse order of datasets
            $dataset = $#legend;
        } elsif (/^=stackcluster(.)/) {
            $splitby = $1;
            s/=stackcluster$splitby//;
            chop;
            @legend = split($splitby, $_);
            $stackcount = $#legend + 1;
            $plotcount = $stackcount;
            $stackcluster = 1;
            # reverse order of datasets
            $dataset = $#legend;
            # FIXME: two types of means: for stacked (mean bar per cluster)
            # or for cluster (cluster of stacked bars)
            $use_mean = 0;
        } elsif (/^multimulti=(.*)/) {
            if (!($groupset == 0 && $dataset == $stackcount-1)) {
                $groupset++;
                $dataset = $stackcount-1;
            }
            $groupname[$groupset] = $1;
            $grouplabels = 1 if ($groupname[$groupset] ne "");
        } elsif (/^=multi/) {
            die "Neither cluster nor stacked specified for multiple dataset"
                if ($plotcount == 1);
            if ($stacked || $stackcluster) {
                # reverse order of datasets
                $dataset--;
            } else {
                $dataset++;
            }
        } elsif (/^=patterns/) {
            $patterns = 1;
        } elsif (/^=color_per_datum/) {
            $color_per_datum = 1;
        } elsif (/^colors=(.*)/) {
            $custom_colors = 1;
            @custom_color = split(',', $1);
        } elsif (/^=table/) {
            $table = 1;
            if (/^=table(.)/) {
                $table_splitby = $1;
            } else {
                $table_splitby = ' ';
            }
        } elsif (/^column=(\S+)/) {
            $column = $1;
        } elsif (/^=base1/) {
            $base1 = 1;
        } elsif (/^=invert/) {
            $invert = 1;
        } elsif (/^datascale=(.*)/) {
            $datascale = $1;
        } elsif (/^datasub=(.*)/) {
            $datasub = $1;
        } elsif (/^=percent/) {
            $percent = 1;
        } elsif (/^=sortdata_ascend/) {
            $sort = 1;
            $sortdata_ascend = 1;
        } elsif (/^=sortdata_descend/) {
            $sort = 1;
            $sortdata_descend = 1;
        } elsif (/^=sortbmarks/) {
            $sort = 1;
            $sortbmarks = 1;
        } elsif (/^=sort/) { # don't prevent match of =sort*
            $sort = 1;
        } elsif (/^=reverseorder/) {
            $reverseorder = 1;
        } elsif (/^=arithmean/) {
            die "Stacked-clustered does not suport mean" if ($stackcluster);
            $use_mean = 1;
            $arithmean = 1;
        } elsif (/^=harmean/) {
            die "Stacked-clustered does not suport mean" if ($stackcluster);
            $use_mean = 1;
        } elsif (/^meanlabel=(.*)$/) {
            $mean_label = $1;
        } elsif (/^min=([-\d\.]+)/) {
            $ymin = $1;
            $calc_min = 0;
        } elsif (/^max=([-\d\.]+)/) {
            $ymax = $1;
        } elsif (/^=norotate/) {
            $xticsopts = "";
            # actually looks better at -1 when not rotated, too
            $xlabelshift = "0,-1";
        } elsif (/^xlabelshift=(.+)/) {
            $xlabelshift = $1;
        } elsif (/^ylabelshift=(.+)/) {
            $ylabelshift = $1;
        } elsif (/^xticshift=(.+)/) {
            $xticsopts .= " offset $1";
        } elsif (/^rotateby=(.+)/) {
            $xticsopts = "rotate by $1";
        } elsif (/^grouprotateby=(.+)/) {
            $grouplabel_rotateby = $1;
        } elsif (/^title=(.*)$/) {
            $title = $1;
        } elsif (/^=noxlabels/) {
            $usexlabels = 0;
        } elsif (/^xlabel=(.*)$/) {
            $xlabel = $1;
        } elsif (/^ylabel=(.*)$/) {
            $ylabel = $1;
        } elsif (/^yformat=(.*)$/) {
            $yformat = $1;
        } elsif (/^=noupperright/) {
            $noupperright = 1;
        } elsif (/^=gridx/) {
            $gridx = "xtics";
        } elsif (/^=nogridy/) {
            $gridy = "noytics";
        } elsif (/^=nolegend/) {
            $use_legend = 0;
        } elsif (/^legendx=(\S+)/) {
            $legendx = $1;
        } elsif (/^legendy=(\S+)/) {
            $legendy = $1;
        } elsif (/^legendfill=(.*)/) {
            $legend_fill = $1;
        } elsif (/^=nolegoutline/) {
            $legend_outline = 0;
        } elsif (/^legendfontsz=(.+)/) {
            $legend_font_size = $1;
        } elsif (/^extraops=(.*)/) {
            $extra_gnuplot_cmds .= "$1\n";
        } elsif (/^=nocommas/) {
            $add_commas = 0;
        } elsif (/^font=(.+)/) {
            if (defined($fig_font{$1})) {
                $font_face = $fig_font{$1};
            } else {
                @known_fonts = keys(%fig_font);
                die "Unknown font \"$1\": known fonts are @known_fonts";
            }
        } elsif (/^custfont=([^=]+)=(.+)/) {
            if (defined($fig_font{$1})) {
                $custfont{$2} = $fig_font{$1};
            } else {
                @known_fonts = keys(%fig_font);
                die "Unknown font \"$1\": known fonts are @known_fonts";
            }
        } elsif (/^fontsz=(.+)/) {
            $font_size = $1;
        } elsif (/^bbfudge=(.+)/) {
            $bbfudge = $1;
        } elsif (/^=yerrorbars/) {
            $table = 0;
            $yerrorbars = 1;
            if (/^=yerrorbars(.)/) {
                $yerrorbars_splitby = $1;
            } else {
                $yerrorbars_splitby = ' ';
            }
        } elsif (/^=stackabs/) {
            $stacked_absolute = 1;
        } elsif (/^horizline=(.+)/) {
            $lineat .= "f(x)=$1,f(x) notitle lt -1,"; # put black line at $1
        } elsif (/^=barsinbg/) {
            $barsinbg = 1;
        } elsif (/^=legendinbg/) {
            $legend_depth = 100;
        } elsif (/^leading_space_mul=(.+)/) {
            $leading_space_mul = $1;
        } elsif (/^intra_space_mul=(.+)/) {
            $intra_space_mul = $1;
        } elsif (/^barwidth=(.+)/) {
            $barwidth = $1;
        } elsif (/^logscaley=(.+)/) {
            $logscaley = $1;
        } else {
            die "Unknown command $_\n";
        }
        next;
    }

    # compatibility checks
    die "Graphs of type stacked or stackcluster do not suport yerrorbars"
        if ($yerrorbars  && ($stacked || $stackcluster));

    # this line must have data on it!
    
    if ($table) {
        # table has to look like this, separated by $table_splitby (default ' '):
        # <bmark1> <dataset1> <dataset2> <dataset3> ...
        # <bmark2> <dataset1> <dataset2> <dataset3> ...
        # ...

        # perl split has a special case for literal ' ' to collapse adjacent
        # spaces
        if ($table_splitby eq ' ') {
            @table_entry = split(' ', $_);
        } else {
            @table_entry = split($table_splitby, $_);
        }
        if ($#table_entry != $plotcount) { # not +1 since bmark
            print STDERR "WARNING: table format error on line $_: found $#table_entry entries, expecting $plotcount entries\n";
        }
        # remove leading and trailing spaces, and escape quotes
        $table_entry[0] =~ s/^\s*//;
        $table_entry[0] =~ s/\s*$//;
        $table_entry[0] =~ s/\"/\\\"/g;
        $bmark = $table_entry[0];
        for ($i=1; $i<=$#table_entry; $i++) {
            $table_entry[$i] =~ s/^\s*//;
            $table_entry[$i] =~ s/\s*$//;
            if ($stacked || $stackcluster) {
                # reverse order of datasets
                $dataset = $stackcount-1 - ($i-1);
            } else {
                $dataset = $i-1;
            }
            $val = get_val($table_entry[$i], $dataset);
            if (($stacked || $stackcluster) && $dataset < $stackcount-1 &&
                !$stacked_absolute) {
                # need to add prev bar to stick above
                $entry{$groupset,$bmark,$dataset+1} =~ /([-\d\.eE]+)/;
                $val += $1;
            }
            if ($val ne '') {
                $entry{$groupset,$bmark,$dataset} = "$val";
            } # else, leave undefined
        }
        goto nextiter;
    }

    if ($yerrorbars) {
        # yerrorbars has to look like this, separated by $yerrorbars_splitby (default ' '):
        # <bmark1> <dataset1> <dataset2> <dataset3> ...
        # <bmark2> <dataset1> <dataset2> <dataset3> ...
        # ...

        # perl split has a special case for literal ' ' to collapse adjacent
        # spaces
        if ($yerrorbars_splitby eq ' ') {
            @yerrorbars_entry = split(' ', $_);
        } else {
            @yerrorbars_entry = split($yerrorbars_splitby, $_);
        }
        if ($#yerrorbars_entry != $plotcount) { # not +1 since bmark
            print STDERR "WARNING: yerrorbars format error on line $_: found $#yerrorbars_entry entries, expecting $plotcount entries\n";
        }
        # remove leading and trailing spaces, and escape quotes
        $yerrorbars_entry[0] =~ s/^\s*//;
        $yerrorbars_entry[0] =~ s/\s*$//;
        $yerrorbars_entry[0] =~ s/\"/\\\"/g;
        $bmark = $yerrorbars_entry[0];
        for ($i=1; $i<=$#yerrorbars_entry; $i++) {
            $yerrorbars_entry[$i] =~ s/^\s*//;
            $yerrorbars_entry[$i] =~ s/\s*$//;
            if ($stacked || $stackcluster) {
                # reverse order of datasets
                $dataset = $stackcount-1 - ($i-1);
            } else {
                $dataset = $i-1;
            }
            $val = get_val($yerrorbars_entry[$i], $dataset);
            if (($stacked || $stackcluster) && $dataset < $stackcount-1 &&
                !$stacked_absolute) {
                # need to add prev bar to stick above
                $yerror_entry{$groupset,$bmark,$dataset+1} =~ /([-\d\.eE]+)/;
                $val += $1;
            }
            if ($val ne '') {
                $yerror_entry{$groupset,$bmark,$dataset} = "$val";
            } # else, leave undefined
        }
        goto nextiter;
    }

    # support the column= feature
    if (defined($column)) {
        # only support separation by spaces
        my @columns = split(' ', $_);
        $bmark = $columns[0];
        if ($column eq "last") {
            $val_string = $columns[$#columns];
        } else {
            die "Column $column out of bounds" if ($column > 1 + $#columns);
            $val_string = $columns[$column - 1];
        }
    } elsif (/^\s*(.+)\s+([-\d\.]+)\s*$/) {
        $bmark = $1;
        $val_string = $2;
        # remove leading spaces, and escape quotes
        $bmark =~ s/\s+$//;
        $bmark =~ s/\"/\\\"/g;
    } else {
        if (/\S+/) {
            print STDERR "WARNING: unexpected, unknown-format line $_";
        }
        next;
    }

    # strip out trailing %
    $val_string =~ s/%$//;
    if ($val_string !~ /^[-\d\.]+$/) {
        print STDERR "WARNING: non-numeric value \"$val_string\" for $bmark\n";
    }

    $val = get_val($val_string, $dataset);
    if (($stacked || $stackcluster) && $dataset < $stackcount-1 &&
        !$stacked_absolute) {
        # need to add prev bar to stick above
        # remember that we're walking backward
        $entry{$groupset,$bmark,$dataset+1} =~ /([-\d\.]+)/;
        $val += $1;
    }
    $entry{$groupset,$bmark,$dataset} = "$val";

  nextiter:
    if (!defined($names{$bmark})) {
        $names{$bmark} = $bmark;
        $order{$bmark} = $bmarks_seen++;
    }
}
close(IN);

###########################################################################
###########################################################################

$groupcount = $groupset + 1;

$clustercount = $bmarks_seen if ($stackcluster);

if ($barwidth > 0) {
    $boxwidth = $barwidth;
} else {
    # default
    $boxwidth = 0.75/$clustercount;
}

if ($sort) {
    if ($sortbmarks) {
        @sorted = sort sort_bmarks (keys %names);
    } elsif ($sortdata_ascend) {
        @sorted = sort { $entry{0,$a,0} <=> $entry{0,$b,0}} (keys %names);
    } elsif ($sortdata_descend) {
        @sorted = sort { $entry{0,$b,0} <=> $entry{0,$a,0}} (keys %names);
    } else {
        @sorted = sort (keys %names);
    }
} else {
    # put into order seen in file, or reverse
    if ($reverseorder) {
        @sorted = sort {$order{$b} <=> $order{$a}} (keys %names);
    } else {
        @sorted = sort {$order{$a} <=> $order{$b}} (keys %names);
    }
}

# default spacing: increase spacing if have many clusters+bmarks
# but keep lead spacing small if only one bmark
if ($leading_space_mul != 0) {
    # user-specified
    $outer_space = $boxwidth * $leading_space_mul;
} else {
    $outer_space = $boxwidth * (1.0 + ($clustercount-1)/4.);
}
if ($intra_space_mul != 0) {
    # user-specified
    $intra_space = $boxwidth * $intra_space_mul;
} else {
    $intra_space = $boxwidth * (1.0 + ($clustercount-1)/10.);
}

# clamp at 1/10 the full width, if not user-specified
$num_items = $#sorted + 1 + (($use_mean) ? 1 : 0);
$xmax = get_xval($groupcount-1, $clustercount-1, $num_items-1)
    + $boxwidth/2.;
$outer_space = $xmax/10. if ($outer_space > $xmax/10. && $leading_space_mul == 0);
$intra_space = $xmax/10. if ($intra_space > $xmax/10. && $intra_space_mul == 0);

# re-calculate now that we know $intra_space and $outer_space
$xmax = get_xval($groupcount-1, $clustercount-1, $num_items-1)
    + $boxwidth/2. + $outer_space;

if ($use_mean) {
    for ($i=0; $i<$plotcount; $i++) {
        if ($stacked || $stackcluster) {
            $category = $plotcount-$i;
        } else {
            $category = $i;
        }
        if ($arithmean) {
            die "Error calculating mean: category $category has denom 0"
                if ($harnum[$i] == 0);
            $harmean[$i] = $harsum[$i] / $harnum[$i];
        } else {
            die "Error calculating mean: category $category has denom 0"
                if ($harsum[$i] == 0);
            $harmean[$i] = $harnum[$i] / $harsum[$i];
        }
        if ($datascale != 1) {
            $harmean[$i] *= $datascale;
        }
        if ($percent) {
            $harmean[$i] = ($harmean[$i] - 1) * 100;
        } elsif ($base1) {
            $harmean[$i] = ($harmean[$i] - 1);
        }
    }
    if (($stacked || $stackcluster) && !$stacked_absolute) {
        for ($i=$plotcount-2; $i>=0; $i--) {
            # need to add prev bar to stick above
            # since reversed, prev is +1
            $harmean[$i] += $harmean[$i+1];
        }
    }
}

# x-axis labels
$xtics = "";
for ($g=0; $g<$groupcount; $g++) {
    $item = 0;
    foreach $b (@sorted) {
        if ($stackcluster) {
            $xval = get_xval($g, $item, $item);
        } else {
            $xval = get_xval($g, ($clustercount-1)/2., $item);
        }
        if ($usexlabels) {
            $label = $b;
        } else {
            if ($stackcluster && $grouplabels && $item==&ceil($bmarks_seen/2)-1) {
                $label = $groupname[$g];
            } else {
                $label = "";
            }
        }
        $xtics .= "\"$label\" $xval, ";
        $item++;
    }
    if ($stackcluster && $grouplabels && $usexlabels) {
        $label = sprintf("set label \"%s\" at %f,0 center rotate by %d",
                         $groupname[$g], get_xval($g, ($clustercount-1)/2.,
                                                  ($clustercount-1)/2.),
                         $grouplabel_rotateby);
        $extra_gnuplot_cmds .= "$label\n";
    }
}
# For stackcluster we need to find the y value for the group labels
# so we look where gnuplot put the x label.  If the user specifies none,
# we add our own.
$unique_xlabel = "UNIQUEVALUETOLOOKFOR";
if ($stackcluster && $xlabel eq "") {
    $xlabel = $unique_xlabel;
}
if ($use_mean) {
    if ($usexlabels) {
        if (!defined($mean_label)) {
            if ($arithmean) {
                $mean_label = "mean";
            } else {
                $mean_label = "har_mean";
            }
        }
    } else {
        $xtics .= "\"\" $item, ";
    }
    if ($stackcluster) {
        # FIXME: support mean and move this into $g loop
        $xval = get_xval(0, $item, $item);
    } else {
        $xval = get_xval(0, ($clustercount-1)/2., $item);
    }
    $xtics .= "\"$mean_label\" $xval, ";
    $item++;
}

# lose the last comma-space
chop $xtics;
chop $xtics;

# add space between y-axis label and y tic labels
if ($ylabel ne "") {
    $yformat = "  $yformat";
} else {
    # fix bounding box problem: cutting off tic labels on left if
    # no axis label -- is it gnuplot bug?  we're not mangling these
    $yformat = " $yformat";
}

if ($calc_min) {
    if ($logscaley > 0) {
        die "Error: logscaley does not support negative values\n" if ($min < 0);
        $ymin = 1;
    } elsif ($min < 0) {
        # round to next lower int
        if ($min < 0) {
            $min = int($min - 1);
        }
        $ymin = $min;
        $lineat .= "f(x)=0,f(x) notitle lt -1,"; # put black line at 0
    } # otherwise leave ymin at 0
} # otherwise leave ymin at user-specified value

###########################################################################
###########################################################################

# add dummy labels so we can extract the bounds of the legend text
# from gnuplot's text extent calculations.
# use a prefix so we can identify, process, and remove these.
# to be really thorough we should check that no user-specified string
# matches the prefix but too unlikely.
my $dummy_prefix = "BARGRAPH_TEMP_";
my $legend_old_fontsz = 0;
my $legend_text_widest = ""; # widest legend string
my $legend_text_width = 0; # width of widest legend string
my $legend_text_height = 0;
my $legend_prefix_width = 0;
# base to subtract prefix itself
# avoid x or y of 0 since illegal for logscale
$extra_gnuplot_cmds .= "set label \"$dummy_prefix\" at 1,1\n";
for ($i=0; $i<$plotcount; $i++) {
    # no need to reverse labels: order doesn't matter
    $label = sprintf("set label \"%s%s\" at %d,1",
                     $dummy_prefix, $legend[$i], $i + 1);
    $extra_gnuplot_cmds .= "$label\n";
}

###########################################################################
###########################################################################

$use_colors=1;

# some default fig colors
$colornm{'blue'}=1;
$colornm{'green'}=2;
$colornm{'white'}=7;
# custom colors are from 32 onward, we insert them into the fig file
# the order here is the order for 9+ datasets
$basefigcolor=32;
$numfigclrs=0;
$figcolor[$numfigclrs]="#000000"; $fig_black=$colornm{'black'}=$basefigcolor + $numfigclrs++;
$figcolor[$numfigclrs]="#aaaaff"; $fig_light_blue=$colornm{'light_blue'}=$basefigcolor + $numfigclrs++;
$figcolor[$numfigclrs]="#00aa00"; $fig_dark_green=$colornm{'dark_green'}=$basefigcolor + $numfigclrs++;
$figcolor[$numfigclrs]="#77ff00"; $fig_light_green=$colornm{'light_green'}=$basefigcolor + $numfigclrs++;
$figcolor[$numfigclrs]="#ffff00"; $fig_yellow=$colornm{'yellow'}=$basefigcolor + $numfigclrs++;
$figcolor[$numfigclrs]="#ff0000"; $fig_red=$colornm{'red'}=$basefigcolor + $numfigclrs++;
$figcolor[$numfigclrs]="#dd00ff"; $fig_magenta=$colornm{'magenta'}=$basefigcolor + $numfigclrs++;
$figcolor[$numfigclrs]="#0000ff"; $fig_dark_blue=$colornm{'dark_blue'}=$basefigcolor + $numfigclrs++;
$figcolor[$numfigclrs]="#00ffff"; $fig_cyan=$colornm{'cyan'}=$basefigcolor + $numfigclrs++;
$figcolor[$numfigclrs]="#dddddd"; $fig_grey=$colornm{'grey'}=$basefigcolor + $numfigclrs++;
$figcolor[$numfigclrs]="#6666ff"; $fig_med_blue=$colornm{'med_blue'}=$basefigcolor + $numfigclrs++;
$num_nongrayscale = $numfigclrs;
# for grayscale
$figcolor[$numfigclrs]="#222222"; $fig_grey=$colornm{'grey1'}=$basefigcolor + $numfigclrs++;
$figcolor[$numfigclrs]="#444444"; $fig_grey=$colornm{'grey2'}=$basefigcolor + $numfigclrs++;
$figcolor[$numfigclrs]="#666666"; $fig_grey=$colornm{'grey3'}=$basefigcolor + $numfigclrs++;
$figcolor[$numfigclrs]="#888888"; $fig_grey=$colornm{'grey4'}=$basefigcolor + $numfigclrs++;
$figcolor[$numfigclrs]="#aaaaaa"; $fig_grey=$colornm{'grey5'}=$basefigcolor + $numfigclrs++;
$figcolor[$numfigclrs]="#cccccc"; $fig_grey=$colornm{'grey6'}=$basefigcolor + $numfigclrs++;
$figcolor[$numfigclrs]="#eeeeee"; $fig_grey=$colornm{'grey7'}=$basefigcolor + $numfigclrs++;

$figcolorins = "";
for ($i=0; $i<=$#figcolor; $i++) {
    $figcolorins .= sprintf("0 %d %s\n", 32+$i, $figcolor[$i]);
}
chomp($figcolorins);

$colorcount = $plotcount; # re-set for color_per_datum below
if ($patterns) {
    $colorcount = $max_patterns if ($color_per_datum);
    for ($i=0; $i<$colorcount; $i++) {
        # cycle around at max
        $fillstyle[$i] = 41 + ($i % $max_patterns);
        # FIXME: could combine patterns and colors, we don't bother to support that
        $fillcolor[$i] = 7; # white
    }
} elsif ($use_colors) {
    $colorcount = $num_nongrayscale if ($color_per_datum);
    # colors: all solid fill
    for ($i=0; $i<$colorcount; $i++) {
        $fillstyle[$i]=20;
    }
    if ($custom_colors) {
        $colorcount = $#custom_color+1 if ($color_per_datum);
        for ($i=0; $i<$colorcount; $i++) {
            $fillcolor[$i]=$colornm{$custom_color[$i]};
        }
    } else {
        # color schemes that I tested as providing good contrast when
        # printed on a non-color printer.
        if ($yerrorbars && $colorcount >= 5) {
            # for yerrorbars we avoid using black since the errorbars are black.
            # a hack where we take the next-highest set and then remove black:
            $colorcount++;
        }
        if ($colorcount == 1) {
            $fillcolor[0]=$fig_light_blue;
        } elsif ($colorcount == 2) {
            $fillcolor[0]=$fig_med_blue;
            $fillcolor[1]=$fig_yellow;
        } elsif ($colorcount == 3) {
            $fillcolor[0]=$fig_med_blue;
            $fillcolor[1]=$fig_yellow;
            $fillcolor[2]=$fig_red;
        } elsif ($colorcount == 4) {
            $fillcolor[0]=$fig_med_blue;
            $fillcolor[1]=$fig_yellow;
            $fillcolor[2]=$fig_dark_green;
            $fillcolor[3]=$fig_red;
        } elsif ($colorcount == 5) {
            $fillcolor[0]=$fig_black;
            $fillcolor[1]=$fig_yellow;
            $fillcolor[2]=$fig_red;
            $fillcolor[3]=$fig_med_blue;
            $fillcolor[4]=$fig_grey;
        } elsif ($colorcount == 6) {
            $fillcolor[0]=$fig_black;
            $fillcolor[1]=$fig_dark_green;
            $fillcolor[2]=$fig_yellow;
            $fillcolor[3]=$fig_red;
            $fillcolor[4]=$fig_med_blue;
            $fillcolor[5]=$fig_grey;
        } elsif ($colorcount == 7) {
            $fillcolor[0]=$fig_black;
            $fillcolor[1]=$fig_dark_green;
            $fillcolor[2]=$fig_yellow;
            $fillcolor[3]=$fig_red;
            $fillcolor[4]=$fig_dark_blue;
            $fillcolor[5]=$fig_cyan;
            $fillcolor[6]=$fig_grey;
        } elsif ($colorcount == 8) {
            $fillcolor[0]=$fig_black;
            $fillcolor[1]=$fig_dark_green;
            $fillcolor[2]=$fig_yellow;
            $fillcolor[3]=$fig_red;
            $fillcolor[4]=$fig_magenta;
            $fillcolor[5]=$fig_dark_blue;
            $fillcolor[6]=$fig_cyan;
            $fillcolor[7]=$fig_grey;
        } elsif ($colorcount == 9) {
            $fillcolor[0]=$fig_black;
            $fillcolor[1]=$fig_dark_green;
            $fillcolor[2]=$fig_light_green;
            $fillcolor[3]=$fig_yellow;
            $fillcolor[4]=$fig_red;
            $fillcolor[5]=$fig_magenta;
            $fillcolor[6]=$fig_dark_blue;
            $fillcolor[7]=$fig_cyan;
            $fillcolor[8]=$fig_grey;
        } else {
            for ($i=0; $i<$colorcount; $i++) {
                # FIXME: set to programmatic spread of custom colors
                # for now we simply re-use our set of colors
                $fillcolor[$i]=$basefigcolor + ($i % $num_nongrayscale);
            }
        }
        if ($yerrorbars) {
            if ($colorcount >= 5) {
                # a hack where we take the next-highest set and remove black,
                # which we assume to be first
                die "Internal color assumption error"
                    if ($colorcount == 5 || $fillcolor[0] != $fig_black);
                $colorcount--;
                for ($i=0; $i<$colorcount; $i++) {
                    $fillcolor[$i] = $fillcolor[$i+1];
                }
            }
            # double-check we have no conflicts w/ the black error bars
            for ($i=0; $i<$colorcount; $i++) {
                die "Internal color assumption error"
                    if ($fillcolor[i] == $fig_black);
            }
        }
    }
    if ($stacked || $stackcluster) {
        # reverse order for stacked since we think of bottom as "first"
        for ($i=0; $i<$colorcount; $i++) {
            $tempcolor[$i]=$fillcolor[$i];
        }
        for ($i=0; $i<$colorcount; $i++) {
            $fillcolor[$i]=$tempcolor[$colorcount-$i-1];
        }
    }
} else {
    $colorcount = 10 if ($color_per_datum);
    # b&w fills
    $bwfill[0]=5;
    $bwfill[1]=10;
    $bwfill[2]=2;
    $bwfill[3]=14;
    $bwfill[4]=7;
    $bwfill[5]=13;
    $bwfill[6]=3;
    $bwfill[7]=9;
    $bwfill[8]=4;
    $bwfill[9]=11;
    $bwfill[10]=6;
    for ($i=0; $i<$colorcount; $i++) {
        if ($stacked || $stackcluster) {
            # reverse order for stacked since we think of bottom as "first"
            $fillstyle[$i]=$bwfill[$colorcount-$i-1];
        } else {
            $fillstyle[$i]=$bwfill[$i];
        }
        $fillcolor[$i]=-1;
    }
}

# "set terminal" set the default depth to $plot_depth
# we want bars in front of rest of plot
# though we will violate that rule to fit extra datasets (> 48)
$start_depth = ($plot_depth - 2 - 2*($plotcount-1)) < 0 ?
    2*$plotcount : $plot_depth;
for ($i=0; $i<$plotcount; $i++) {
    $depth[$i] = $start_depth - 2 - 2*$i;
}
if ($barsinbg) {
    $plot_depth = $start_depth - 2 - 2*$plotcount;
    $plot_depth = 0 if ($plot_depth < 0);
}

###########################################################################
###########################################################################

local (*FIG, *GNUPLOT);

# now process the resulting figure
if ($output eq "gnuplot") {
    $debug_seegnuplot = 1;
} else {
    $debug_seegnuplot = 0;
}

if ($debug_seegnuplot) {
    open(GNUPLOT, "| cat") || die "Couldn't open cat\n";
} else {
    # open a bidirectional pipe to gnuplot to avoid temp files
    # we can read its output back using FIG filehandle
    $pid = open2(\*FIG, \*GNUPLOT, "$gnuplot_path") || die "Couldn't open2 gnuplot\n";
}

printf GNUPLOT "
set title '%s'
# can also pass \"fontsize 12\" to fig terminal
set terminal fig color depth %d
", $title, $plot_depth;

printf GNUPLOT "
set xlabel '%s' %s%s
set ylabel '%s' %s%s
set xtics %s (%s)
set format y \"%s\"
", $xlabel, $gnuplot_uses_offset ? "offset " : "", $xlabelshift,
$ylabel, $gnuplot_uses_offset ? "offset " : "", $ylabelshift,
$xticsopts, $xtics, $yformat;

printf GNUPLOT "
set boxwidth %s
set xrange [0:%.2f]
set yrange[%s:%s]
set grid %s %s
", $boxwidth, $xmax, $ymin, $ymax, $gridx, $gridy;

if ($noupperright) {
    print GNUPLOT "
set xtics nomirror
set ytics nomirror
set border 3
";
}

if ($logscaley > 0) {
    print GNUPLOT "set logscale y $logscaley\n";
}
if ($extra_gnuplot_cmds ne "") {
    print GNUPLOT "\n$extra_gnuplot_cmds\n";
}

# plot data from stdin, separate style for each so can distinguish
# in resulting fig
printf GNUPLOT "plot %s ", $lineat;
for ($g=0; $g<$groupcount; $g++) {
    for ($i=0; $i<$plotcount; $i++) {
        if ($i != 0 || $g != 0) {
            printf GNUPLOT ", ";
        }
        if ($patterns) {
            # newer gnuplot uses colors by default so request black w/ "lt -1"
            # (xref issue 3)
            printf GNUPLOT "'-' notitle with boxes fs pattern %d lt -1",
                ($i % $max_patterns);
        } else {
            printf GNUPLOT "'-' notitle with boxes lt %d", $i+3;
        }
    }
}

if ($yerrorbars) {
    for ($g=0; $g<$groupcount; $g++) {
        for ($i=0; $i<$plotcount; $i++) {
            print GNUPLOT ", '-' notitle with boxerror lt 0";
        }
    }
}
print GNUPLOT "\n";
for ($g=0; $g<$groupcount; $g++) {
    for ($i=0; $i<$plotcount; $i++) {
        $line = 0;
        foreach $b (@sorted) {
            # support missing values in some datasets
            if (defined($entry{$g,$b,$i})) {
                $xval = get_xval($g, $i, $line);
                print GNUPLOT "$xval, $entry{$g,$b,$i}\n";
                $line++;
            } else {
                print STDERR "WARNING: missing value for $b in dataset $i\n";
                $line++;
            }
        }
        # skip over missing values to put harmean at end
        $line = $bmarks_seen;
        if ($use_mean) {
            $xval = get_xval($g, $i, $line);
            print GNUPLOT "$xval, $harmean[$i]\n";
        }
        # an e separates each dataset
        print GNUPLOT "e\n";
    }
}
if ($yerrorbars) {
    for ($g=0; $g<$groupcount; $g++) {
        for ($i=0; $i<$plotcount; $i++) {
            $line = 0; 
            foreach $b (@sorted) {
                # support missing values in some datasets
                if (defined($entry{$g,$b,$i})) {
                    $xval = get_xval($g, $i, $line);
                    print GNUPLOT "$xval, $entry{$g,$b,$i}, $yerror_entry{$g,$b,$i}\n";
                    $line++;
                } else {
                    print STDERR "WARNING: missing value for $b in dataset $i\n";
                    $line++;
                }
            }
            # skip over missing values to put harmean at end
            $line = $bmarks_seen;
            # an e separates each dataset
            print GNUPLOT "e\n";
        }
    }
}

close(GNUPLOT);

exit if ($debug_seegnuplot);

###########################################################################
###########################################################################

# now process the resulting figure
if ($output eq "fig") {
    $fig2dev = "cat";
} elsif ($output eq "eps") {
    $fig2dev = "$fig2dev_path -L eps -n \"$title\"";
} elsif ($output eq "pdf") {
    $fig2dev = "$fig2dev_path -L pdf -n \"$title\"";
} elsif ($output eq "png") {
    $fig2dev = "$fig2dev_path -L png -m 2";
    $fig2dev .= " | convert -transparent white - - " if ($png_transparent);
} else {
    die "Error: unknown output type $output\n";
}

$debug_seefig = 0;
if ($debug_seefig) {
    $fig2dev = "cat";
}

open(FIG2DEV, "| $fig2dev") || die "Couldn't open $fig2dev\n";

# fig format for polyline:
#   2   1    0    1     -1     -1     10     0     6    0.000    0     0    0    0    0     5
#          line  line  line   fill   depth        fill  dash   join   cap      frwrd back
#          style width color  color               style  gap   style style    arrws? arrws?
# fill style: 0-20: 0=darkest, 20=pure color
# arrows have another line of stats, if present

# fig format for text:
#   4   1    0    0   -1    0     10   1.5708     0    135    1830  1386 2588  Actual text\001
#     just     depth      font  fontsz rotation  flag boundy boundx   x    y
#                                     angle(rads)             
# justification: 0=center, 1=left, 2=right
# flag (or-ed together): 1=rigid, 2=special, 4=PS fonts, 8=hidden
# boundy and boundx: should be calculated from X::TextExtents but
#   users won't have X11::Lib installed so we use heuristics:
#  boundy: 10-pt default Times font: 75 + 30 above + 30 below
#          Helvetica is 90 base
#          FIXME: what about Courier?
#   => 135 if both above and below line chars present, 105 if only above, etc.
#  boundx: 10-pt default latex font: M=150, m=120, i=45, ave lowercase=72, ave uppercase=104
#   that's ave over alphabet, a capitalized word seems to be closer to 69 ave
#   if have bounds wrong then fig2dev will get eps bounding box wrong
# font size: y increases by 15 per 2-point font increase

if ($stackcluster && $grouplabels && $usexlabels) {
    # For stackcluster we need to find the y value for the group labels
    # FIXME: we assume an ordering: xlabel followed by each group label, in
    # that order, else we'll mess up and need multiple passes here!
    $grouplabel_y = 0;
    $groupset = 0;
}

# compute bounding boxes
$graph_min_x = $sentinel;
$graph_proper_min_x = $sentinel; # ignoring text
$graph_max_x = 0;
$graph_min_y = $sentinel;
$graph_max_y = 0;
$graph_box_width = 0;
$graph_box_max_y = 0;

my $is_polyline = 0;

$set = -1;
$set_raw = "";
while (<FIG>) {
    if ($debug_seefig_unmod) {
        print FIG2DEV $_;
        next;
    }

    # Insert our custom fig colors
    s|^1200 2$|1200 2
$figcolorins|;

    # Convert rectangles with line style N to filled rectangles.
    # We put them at depth $plot_depth.
    # Look for '^2 1 ... 5' to indicate a full box w/ 5 points.
    if (/^2 1 \S+ \S+ (\S+) \1 $plot_depth 0 -1(\s+\S+){6}\s+5/) {
        # Rather than hardcode the styles that gnuplot uses for fig (which has
        # changed), we assume the plots are in sequential order.
        # We assume that the coordinates are all on the subsequent line,
        # so that we can use the entire first line as our key (else we should pull
        # out at least line style, line color, and dash gap).
        $cur_raw = $_;
        # We need to not convert the plot outline, so we assume that
        # the first plot box never has a fill of 0 or -1.
        $cur_fill = $1;
        if ($set == -1 && ($cur_fill == 0 || $cur_fill == -1)) {
            # ignore: it's the plot outline
        } else {
            if ($cur_raw ne $set_raw || $set == -1) {
                $set++;
                $set_raw = $cur_raw;
                if ($set < $plotcount) {
                    # For repeats, match the entire line
                    $xlate{$_} = $set;
                }
            }
            # There are some polylines past the last plot
            if ($set < $plotcount) {
                $color_idx = $color_per_datum ? ($itemcount++ % ($#fillcolor+1)) :
                    $set;
                s|^2 1 \S+ \S+ (\S+) \1 $plot_depth 0 -1 +([0-9]+).000|2 1 0 1 -1 $fillcolor[$color_idx] $depth[$set] 0 $fillstyle[$color_idx]     0.000|;
            } elsif (defined($xlate{$_})) {
                $repeat = $xlate{$_};
                $color_idx = $color_per_datum ? ($itemcount++ % ($#fillcolor+1)) :
                    $repeat;
                # Handle later repeats, like for cluster of stacked
                s|^2 1 \S+ \S+ (\S+) \1 $plot_depth 0 -1 +([0-9]+).000|2 1 0 1 -1 $fillcolor[$color_idx] $depth[$repeat] 0 $fillstyle[$color_idx]     0.000|;
            }
        }
    }

    if ($yerrorbars) {
        # increase thickness of dotted line errorbars
        s|^2 1 (\S+) 1 0 0 $plot_depth 0 -1     4.000 0 (\S+) 0 0 0 2|2 1 $1 1 0 0 10 0 -1     0.000 0 $2 0 0 0 2|;
    }

    # Process and remove dummy strings to determine legend text bounds
    if (/^4(\s+[-\d\.]+){5}\s+([\d\.]+)(\s+[-\d\.]+){2}\s+([\d\.]+)\s+([\d\.]+)\s+\d+\s+\d+\s+$dummy_prefix(.*)\\001$/) {
        $legend_old_fontsz = $2;
        my $boundy = $4;
        my $boundx = $5;
        my $text = $6;
        if ($text eq "") {
            $legend_prefix_width = $boundx;
        } else {
            $legend_text_height = $boundy if ($boundy > $legend_text_height);        
            if ($boundx > $legend_text_width) {
                $legend_text_width = $boundx;                
                $legend_text_widest = $text;
            }
        }
        s/^.*$//;
    }

    if ($stackcluster && $grouplabels && $usexlabels) {
        if (/^4\s+.*\s+(\d+)\s+$xlabel\\001/) {
            $grouplabel_y = $1;
            if ($xlabel eq $unique_xlabel) {
                s/^.*$//; # remove
            } else {
                # HACK to push below
                $newy = $grouplabel_y + 160 + &font_bb_diff_y($font_size-1, $font_size);
                s/(\s+)\d+(\s+$xlabel\\001)/\1$newy\2/;
            }
        }
        if (/^4\s+.*$groupname[$groupset]\\001/) {
            s/(\s+)\d+(\s+$groupname[$groupset]\\001)/\1$grouplabel_y\2/;
            $groupset++;
        }
    }

    # Custom fonts
    if (/^(4\s+\d+\s+[-\d]+\s+\d+\s+[-\d]+)\s+[-\d]+\s+([\d\.]+)\s+([-\d\.]+)\s+(\d+)\s+([\d\.]+)\s+([\d\.]+)(\s+[-\d\.]+\s+[-\d\.]+) (.*)\\001/) {
        my $prefix = $1;
        my $oldsz = $2;
        my $orient = $3;
        my $flags = $4;
        my $szy = $5;
        my $szx = $6;
        my $text = $8; # $7 is position
        my $textlen = length($text);
        my $newy = $szy + &font_bb_diff_y($oldsz, $font_size);
        my $newx = $szx + $textlen * &font_bb_diff_x($oldsz, $font_size, $text);
        my $newfont = defined($custfont{$text}) ? $custfont{$text} : $font_face;
        s|^$prefix\s+[-\d]+\s+$oldsz\s+$orient\s+$flags\s+$szy\s+$szx|$prefix $newfont $font_size $orient $flags $newy $newx|;
    } elsif (/^4/) {
        print STDERR "WARNING: unknown font element $_";
    }

    if ($add_commas) {
        # Add commas between 3 digits for text in thousands or millions
        s|^4 (.*\d)(\d{3}\S*)\\001$|4 $1,$2\\001|; 
        s|^4 (.*\d)(\d{3}),(\d{3}\S*)\\001$|4 $1,$2,$3\\001|; 
    }

    # With gnuplot 4.2, I get a red x axis in some plots w/ negative values (but
    # not all: FIXME: why?): I'm turning it to black
    s|^2 1 0 1 4 4 $plot_depth|2 1 0 1 0 0 $plot_depth|;

    # Bounds: we assume for polyline on 2nd line w/ leading space
    # We process after above changes so we don't see temp text, etc.
    if (/^(\d+)(\s+\S+){3}\s+(\S+)\s+(\S+)(\s+\S+){2}\s+(\S+)/) {
        $is_polyline = ($1 == 2);
        # to rule out rectangle around entire graph: can't use just
        # fill style ($6) since old gnuplot doesn't fill bars so we
        # check for any of line color, fill color, or fill style
        $is_bar = ($is_polyline && ($3 > 0 || $4 > 0 || $6 > -1));
    }
    if ($is_polyline && /^\s+\d+/) {
        my @coords = split(' ', $_);
        for ($i = 0; $i <= $#coords; $i++) {
            if ($i % 2 == 0) {
                $graph_min_x = $coords[$i] if ($coords[$i] < $graph_min_x);
                $graph_proper_min_x = $coords[$i] if ($coords[$i] < $graph_proper_min_x);
                $graph_max_x = $coords[$i] if ($coords[$i] > $graph_max_x);
            } else {
                $graph_min_y = $coords[$i] if ($coords[$i] < $graph_min_y);
                $graph_max_y = $coords[$i] if ($coords[$i] > $graph_max_y);
            }
        }
        if ($is_bar && $#coords == 9) { # verify rectangle: 5 points
            my $x1 = $sentinel;
            my $y1 = $sentinel;
            my $x2 = 0;
            my $y2 = 0;
            for ($i = 0; $i <= $#coords; $i += 2) {
                $x1 = $coords[$i] if ($coords[$i] < $x1);
                $x2 = $coords[$i] if ($coords[$i] > $x2);
            }
            for ($i = 1; $i <= $#coords; $i += 2) {
                $y1 = $coords[$i] if ($coords[$i] < $y1);
                $y2 = $coords[$i] if ($coords[$i] > $y2);
            }
            print STDERR "bar $x1,$y1 $x2,$y2 <= $_" if ($verbose);
            # use x1 as the key.  combine data for stacked bars.
            $bardata{$x1}{"x2"} = $x2;
            if (defined($bardata{$x1}{"y1"})) {
                $bardata{$x1}{"y1"} = $y1 if ($y1 < $bardata{$x1}{"y1"});
            } else {
                $bardata{$x1}{"y1"} = $y1;
            }
            $graph_box_max_y = $y2 if ($y2 > $graph_box_max_y);
            my $width = $x2 - $x1;
            if ($graph_box_width == 0) {
                $graph_box_width = $width;
            } else {
                die "Boxes should not be different widths ($graph_box_width vs $width)".
                    ": report this!\n"
                    # I've seen them be different by 1
                    unless (abs($width - $graph_box_width) < 5);
            }
        }
    }
    if (/^4(\s+\S+){8}\s+([-\d\.]+)\s+([-\d\.]+)\s+([-\d\.]+)\s+([-\d\.]+)/) {
        # boundy,boundx x,y
        # FIXME: take into account rotation!  no matter the orientation,
        # the text bounds are given as though the text is horizontal.
        my $maxx = $3 + $4;
        my $maxy = $2 + $5;
        $graph_min_x = $4 if ($4 < $graph_min_x);
        # ignoring fonts for max x: bounds seem to be over-estimates, and even
        # if x labels stick off end, fine for legend to align w/ graph itself
        $graph_min_y = $5 if ($5 < $graph_min_y);
        $graph_max_y = $maxy if ($maxy > $graph_max_y);
    }

    print FIG2DEV $_;
}

print STDERR "bounds are $graph_min_x,$graph_min_y $graph_max_x,$graph_max_y\n"
    if ($verbose);

# add the legend
if ($use_legend && $plotcount > 1) {
    # first, compute bounding box of legend
    $legend_text_width -= $legend_prefix_width;
    # default is one smaller than main font so legend not so big
    $legend_font_size = $font_size - 1 if ($legend_font_size == 0);
    my $maxlen = 0;
    for ($i=0; $i<$plotcount; $i++) {
        $leglen = length $legend[$i];
        $maxlen = $leglen if ($leglen > $maxlen);
    }
    my $border = 50;
    my $key_box_width = 121;
    my $key_box_height = 116;
    my $key_text_pre_space = 104;
    # this should really be derived from $legend_text_height
    my $key_text_line_space = 157;
    my $legend_width = $border*2 + $key_box_width + $key_text_pre_space +
        $legend_text_width +
        $maxlen*&font_bb_diff_x($legend_old_fontsz, $legend_font_size,
                                $legend_text_widest);
    my $legend_height = $border*2 + $plotcount*$key_text_line_space
        # subtract off the extra spacing after bottom box
        - ($key_text_line_space - $key_box_height);
    # to get text centered where box is, shift from bottom of box
    my $key_text_yshift = -5;

    my $ly = $sentinel;
    my $lx = $sentinel;

    # try to fit inside the graph
    if ($legendx eq 'inside') {
        my $xstart = $sentinel;
        my $lastx2 = $sentinel;
        my $ytall = $sentinel;
        foreach $x (sort (keys %bardata)) {
            # we use $border*2 as a fudge factor to move below the top
            # line and top x tics
            die "X value $x >= sentinel!\n" if ($x >= $sentinel);
            die "Y value >= sentinel!\n" if ($bardata{$x}{"y1"} >= $sentinel);
            $shift = $noupperright ? 0 : $border*3;
            printf STDERR "bar @ x1=$x,y=%d\n", $bardata{$x}{"y1"} if ($verbose);
            if ($graph_min_y + $shift + $legend_height + $border*2 < $bardata{$x}{"y1"}) {
                if ($xstart == $sentinel) {
                    # include space between bars: use the last bad x2, or if none,
                    # use the y axis (which is what $graph_proper_min_x should be)
                    $xstart = ($lastx2 == $sentinel) ? $graph_proper_min_x : $lastx2;
                }
                $ytall = $bardata{$x}{"y1"} if ($bardata{$x}{"y1"} < $ytall);
            } else {
                if ($xstart != $sentinel &&
                    $x - $xstart > $legend_width + $border*2) {
                    printf STDERR "legend fits inside $xstart,$x\n" if ($verbose);
                    # center in the space
                    $lx = ($xstart + $x - $legend_width)/2;
                    $ly = (($graph_min_y + $shift) +
                           ($ytall - $legend_height - $border*2)) / 2;
                    # keep going: prefer right-most spot
                }
                $xstart = $sentinel;
            }
            $lastx2 = $bardata{$x}{"x2"};
        }
        if ($xstart != $sentinel &&
            # $graph_max_x should be right-hand y axis
            $graph_max_x - $xstart > $legend_width + $border*2) {
            printf STDERR "legend fits inside $xstart,$graph_max_x\n" if ($verbose);
            # center in the space
            $lx = ($xstart + $graph_max_x - $legend_width)/2;
            $ly = (($graph_min_y + $shift) +
                   ($ytall - $legend_height - $border*2)) / 2;
        }
    }

    if ($lx == $sentinel) { # if legendx=inside matches, it sets $lx
        if ($legendx eq 'inside') {
            # if inside fails, use top
            $legendx = 'center';
            $legendy = 'top';
        }
        if ($legendx eq 'right') {
            $lx = $graph_max_x + $border*2;
        } elsif ($legendx eq 'center') {
            $lx = ($graph_max_x - $graph_proper_min_x - $legend_width) / 2 +
                $graph_proper_min_x;
        } else {
            die "Invalid legendx value $legendx\n" unless ($legendx =~ /^\d+$/);
            $lx = $legendx;
        }
    }
    if ($ly == $sentinel) { # if legendx=inside matches, it sets $ly
        if ($legendy eq 'top') {
            $ly = $graph_min_y - $legend_height - $border*2;
        } elsif ($legendy eq 'center') {
            # center vertically considering only the graph area, not the labels beneath
            $ly = ($graph_box_max_y - $graph_min_y - $legend_height) / 2 + $graph_min_y;
        } else {
            die "Invalid legendy value $legendy\n" unless ($legendy =~ /\d+/);
            $ly = $legendy;
        }
    }

    print STDERR "legend at $lx,$ly ($outer_space)\n" if ($verbose);
      
    # draw boxes w/ appropriate colors
    for ($i=0; $i<$plotcount; $i++) {
        $dy = $i * $key_text_line_space;
        printf FIG2DEV
"2 1 0 1 -1 $fillcolor[$i] $legend_depth 0 $fillstyle[$i] 0.000 0 0 0 0 0 5
\t %d %d %d %d %d %d %d %d %d %d  
",  $lx+$border, $ly+$border+$key_box_height+$dy, 
    $lx+$border, $ly+$border+$dy, 
    $lx+$border+$key_box_width, $ly+$border+$dy,
    $lx+$border+$key_box_width, $ly+$border+$key_box_height+$dy,
    $lx+$border, $ly+$border+$key_box_height+$dy;
    }
    # legend text
    for ($i=0; $i<$plotcount; $i++) {
        # legend was never reversed, reverse it here
        if ($stacked || $stackcluster) {
            $legidx = $plotcount - 1 - $i;
        } else {
            $legidx = $i;
        }
        # bounds are important if legend on right to get bounding box
        # for simplicity we give each line the bounds of longest line
        $leglen = length $legend[$legidx];
        $maxlen = $leglen if ($leglen > $maxlen);
        printf FIG2DEV
"4 0 0 %d 0 %d %d 0.0000 4 %d %d %d %d %s\\001
", $legend_depth, $font_face, $legend_font_size,
$legend_text_height + &font_bb_diff_y($legend_old_fontsz, $legend_font_size),
$legend_text_width + $leglen*&font_bb_diff_x($legend_old_fontsz, $legend_font_size,
  $legend[$legidx]),
$lx+$border+$key_box_width+$key_text_pre_space,
$ly+$border+$key_box_height+$key_text_yshift+$key_text_line_space*$i,
$legend[$legidx];
    }
    if ($legend_fill ne '' || $legend_outline) {
        # background fill for legend box
        my $fill_color;
        if ($legend_fill eq '') {
            $fill_color = $colornm{'white'};
        } else {
            if (defined($colornm{$legend_fill})) {
                $fill_color = $colornm{$legend_fill};
            } else {
                print STDERR "WARNING: unknown color $legend_fill\n";
                $fill_color = $colornm{'white'};
            }
        }
        my $fill_style = ($legend_fill eq '') ? -1 : 20;
        my $x1 = $lx;
        my $x2 = $x1 + $legend_width;
        my $y1 = $ly;
        my $y2 = $y1 + $legend_height;
        printf FIG2DEV
            "2 2 0 $legend_outline 0 $fill_color %d 0 $fill_style 0.000 0 0 0 0 0 5
\t %d %d %d %d %d %d %d %d %d %d
",      $legend_depth + 1, # UNDER legend
        $x1,$y1, $x2,$y1, $x2,$y2, $x1,$y2, $x1,$y1;
    }
}

close(FIG);
close(FIG2DEV);

waitpid($pid, 0);

###########################################################################
###########################################################################

# supporting subroutines

sub get_val($, $)
{
    my ($val, $idx) = @_;
    if ($invert) {
        $val = 1/$val;
    }
    if ($use_mean) {
        if ($arithmean) {
            $harsum[$idx] += $val;
        } else {
            die "Harmonic mean cannot be computed with a value of 0!" if ($val == 0);
            $harsum[$idx] += 1/$val;
        }
        $harnum[$idx]++;
    }
    if ($datasub != 0) {
        $val -= $datasub;
    }
    if ($datascale != 1) {
        $val *= $datascale;
    }
    if ($percent) {
        $val = ($val - 1) * 100;
    } elsif ($base1) {
        $val = ($val - 1);
    }
    if (!defined($min)) {
        $min = $val;
    } elsif ($val < $min) {
        $min = $val;
    }
    return $val;
}

sub get_xval($, $, $)
{
    # item ranges from 0..plotcount-1
    my ($gset, $dset, $item) = @_;
    my $xvalue;
    if ($stacked || $clustercount == 1) {
        $xvalue = &cluster_xval($item, 0);
    } elsif ($stackcluster) {
        $xvalue = &cluster_xval($gset, $item);
    } else {
        $xvalue = &cluster_xval($item, $dset);
    }
    return $xvalue;
}

sub cluster_xval($, $)
{
    my ($base, $dset) = @_;
    return $outer_space + $boxwidth/2. +
        $base*($clustercount*$boxwidth + $intra_space) +
        $dset*$boxwidth;
}

sub sort_bmarks()
{
    return ((&bmark_group($a) <=> &bmark_group($b)) or ($a cmp $b));
}

sub bmark_group($)
{
    my ($bmark) = @_;
    return 1 if ($bmarks_fp =~ $bmark);
    return 2 if ($bmarks_int =~ $bmark);
    return 3 if ($bmarks_jvm =~ $bmark);
    return 4; # put unknowns at end
}

sub font_bb_diff_y($,$)
{
    my ($oldsz, $newsz) = @_;
    # This is an inadequate hack: font bounding boxes vary
    # by 15 per 2-point font size change for smaller chars, but up
    # to 30 per 2-point font size for larger chars.  We try to use a
    # single value here for all chars.  Overestimating is better than under.
    # And of course any error accumulates over larger sizes.
    # The real way is to call XTextExtents.
    $diff = ($newsz - $oldsz)*15*$bbfudge;
    if ($font_face >= $fig_font{'Helvetica'} &&
        $font_face <= $fig_font{'Helvetica Narrow Bold Oblique'}) {
        $diff += 15*$bbfudge; # extra height for Helvetica
    }
    return &ceil($diff);
}

sub font_bb_diff_x($,$,$)
{
    my ($oldsz, $newsz, $text) = @_;
    # This is an inadequate hack: font bounding boxes vary
    # by 15 per 2-point font size change for smaller chars, but up
    # to 30 per 2-point font size for larger chars.  We try to use a
    # single value here for all chars.  Overestimating is better than under.
    # And of course any error accumulates over larger sizes.
    # The real way is to call XTextExtents.
    my $scale = ($newsz < 8) ? 9 : 10;
    # FIXME issue #15: even using gnuplot to add the text and then
    # taking its bounding box is inaccurate for capital letters: is
    # gnuplot calling XTextExtents incorrectly?  For now we have a
    # hack to compensate.
    my $numcaps = () = ($text =~ /[A-Z][A-Z]/g); # only consecutive caps
    my $numwidecaps = () = ($text =~ /[MWL][A-Z]/g); # L b/c l is so narrow
    # really hacky: but long strings of caps seem to not need as much
    $numcaps = $numcaps/2 if ($numcaps > 4);
    $numwidecaps = $numwidecaps/2 if ($numwidecaps > 4);
    return &ceil(($newsz - $oldsz)*$scale*$bbfudge + $numcaps*5 + $numwidecaps*3);
}

sub ceil {
    my ($n) = @_;
    return int($n + ($n <=> 0));
}
