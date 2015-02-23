#!/usr/bin/env bash

## All arguments are made globals:
ACTION=      # The run function to be executed (run_all, run_class)
OTP=         # The OTP to be used for erlc and erl
COMP=        # The name of the compiler (beam, hipe, erllvm)
CLASS=       # The class of benchmarks to be executed
BENCH=       # The name of the benchmark to be executed
ITERATIONS=2 # Number of executions of benchmarks to collect statistics
DEBUG=0      # Debug mode (0=Off, 1=On)

run_all ()
{
    echo "Running all benchmark classes..."

    ## Look for all available Classes to run
    for c in `find ebin/ -maxdepth 1 -mindepth 1 -type d`; do
        CLASS=`basename $c`
        run_class
    done
}

run_class ()
{
    echo "   [Class] $CLASS"

    ## Get failing
    if [ -r failing ]; then
        skipped=`cat failing`
    else
        skipped=
    fi

    ## Get boilerplate
    BOILERPLATE=src/$CLASS/boilerplate
    if [ -r $BOILERPLATE ]; then
        skipped="$skipped `cat $BOILERPLATE`"
    fi

    for f in `ls ebin/$CLASS/*.beam`; do
        BENCH=`basename $f .beam`
        ## Skip file if in failing or boileprlate
        SKIP="no"
        for s in $skipped; do
            if [ "$BENCH" = "$s" ]; then
                SKIP="yes"
                break
            fi
        done
        if [ "$SKIP" = "yes" ]; then
            continue
        fi
        ## Else run benchmark
        run_benchmark
    done
}

run_benchmark ()
{
    echo "   --- $BENCH"

    EBIN_DIRS=`find ebin/ -maxdepth 1 -mindepth 1 -type d`

    if [ -x src/$CLASS/$BENCH.input ]; then
	cd src/$CLASS
	PARAMS=`./"$BENCH.input"`
	cd ../..
        for l in `seq 1 $ITERATIONS`; do
            exec 3>&1 4>&2
            time=$( { time `$OTP/bin/erl -pa ebin/shootout \
               -smp enable -noshell $PARAMS \
               -run $BENCH main 0 < src/$CLASS/$BENCH-input.txt > /dev/null` \
               1>&3 2>&4; } 2>&1 )  # Captures time only.
            exec 3>&- 4>&-
            echo "$BENCH" $time >> results/runtime_$COMP-input.res
        done
	rm "src/$CLASS/$BENCH-input.txt"
    else
        $OTP/bin/erl -pa ebin/ $EBIN_DIRS -noshell -s run_benchmark run \
            $BENCH $COMP $ITERATIONS -s erlang halt
    fi
}

collect_results ()
{
    echo "Collecting results..."

    echo "### Benchmark BEAM/ErLLVM HiPE/ErLLVM BEAM HiPE ErLLVM (millisecs)" \
        > results/runtime.res
    pr -m -t results/runtime_beam.res results/runtime_hipe.res \
        results/runtime_erllvm.res \
        | gawk '{print $1 "\t" $2/$6 "\t" $4/$6 "\t\t" $2 "\t" $4 "\t" $6}' \
        >> results/runtime.res
    ## Print average performance results of current execution:
    awk '{btl += $2; htl += $3} END {print "Runtime BTL:", btl/(NR-1), \
        "Runtime HTL:", htl/(NR-1)}' results/runtime.res

    echo "### Standard deviation BEAM HiPE ErLLVM (millisecs)" \
        > results/runtime-err.res
    pr -m -t results/runtime_beam-err.res results/runtime_hipe-err.res \
        results/runtime_erllvm-err.res \
        | gawk '{print $1 "\t" $2 "\t" $4 "\t" $6}' \
        >> results/runtime-err.res
}

plot_diagram ()
{
    INPUT=$1
    HASH=`basename $INPUT .res`
    TMP_DIR=/dev/shm/erllvm-bench-diagrams
    SCRIPTS_DIR=gnuplot_scripts
    DIAGRAMS_DIR=diagrams
    TMP_PERF=$TMP_DIR/speedup.perf
    echo "Plotting results..."

    mkdir -p $TMP_DIR
    ## Copy speedup.perf template and append only speedups:
    cp $SCRIPTS_DIR/speedup.perf $TMP_PERF
    cat results/$INPUT | awk '{print $1 "\t " $2 "\t " $3}' >> $TMP_PERF

    ## Create diagram in diagram:
    $SCRIPTS_DIR/bargraph.pl $TMP_PERF > $DIAGRAMS_DIR/$HASH.eps 2> /dev/null
    rm -rf $TMP_DIR
}

spinner () {
    PROC=$1;COUNT=0
    echo -n "Please wait... "
    while [ -d /proc/$PROC ];do
        while [ "$COUNT" -lt 10 ];do
            echo -n '  ' ; sleep 0.1
            ((COUNT++))
        done
        until [ "$COUNT" -eq 0 ];do
            echo -n ' ' ; sleep 0.1
            ((COUNT -= 1))
        done
    done
    echo "done!"
}

usage ()
{
    cat << EOF
Usage: $0 options OTP_ROOT

This script runs the benchmarks using the provided OTP directory (first
non-option argument) as root and creates the corresponding diagrams.

In the OTP directory provided there should be subdirectories (or links)
including complete OTP installations.  There should also be files with
extension ".flags" specifying the compilation flags appropriate for each
OTP installation.

OPTIONS:
  -h    Show this message
  -a    Run all available benchmarks (default)
  -c    Benchmark class to run
  -b    Benchmark to run
  -n    Number of iterations (default=$ITERATIONS)

Examples:
  1) $0 -c shootout -n 3 ~/git/otp
  1) $0 -c shootout -b mandelbrot -n 3 ~/git/otp
  2) $0 -a ~/git/otp
  3) $0 -a -n 5 ~/git/otp
EOF
}

main ()
{
    while getopts "hadn:b:c:" OPTION; do
        case $OPTION in
            h|\?)
                usage
                exit 0
                ;;
            a)
                ACTION=run_all
                ;;
            c) ## Run *only* specified benchmark class:
                ACTION=run_class
                CLASS=$OPTARG
                ;;
	    b) ## Run *only* specified benchmark
		if [ "$CLASS" == "" ]; then
		    echo "You must always use -c with -b"
		    exit 1
		fi
		ACTION=run_benchmark
		BENCH=$OPTARG
		;;
            n)
                ITERATIONS=$OPTARG
                ;;
            d)
                DEBUG=1
                ;;
        esac
    done
    ## $1 is now the first non-option argument, $2 the second, etc
    shift $(($OPTIND - 1))
    OTP_ROOT=$1

    ## If ACTION is not set something went wrong (probably the script was
    ## called with no args):
    if [ -z $ACTION ]; then
        usage
        exit 1
    fi

    if [ $DEBUG -eq 1 ]; then
        cat << EOF
-- Debug info:
  Iter         = $ITERATIONS
  Run          = $ACTION
  Class        = $CLASS
  OTP          = $OTP_ROOT
EOF
    fi

    echo "Executing $ITERATIONS iterations/benchmark."
    for COMP_OTP_DIR in $OTP_ROOT/*; do
	COMP=`basename $COMP_OTP_DIR`
	if [[ "$COMP" == *.flags ]]; then
	   continue
	fi
        ## Proper compile
        make clean > /dev/null
        echo -n "  Re-compiling with $COMP. "
        ## Use the appropriate ERLC flags
        ERL_CFLAGS=
        if [ -r "$COMP.flags" ]; then
            ERL_CFLAGS=`cat "$COMP.flags"`
        fi
        make ERLC=$OTP_ROOT/$COMP/bin/erlc ERL_COMPILE_FLAGS="$ERL_CFLAGS" \
	  > /dev/null 2>&1 && echo "OK."
	# OK, screw the spinner...
	# > /dev/null 2>&1 &
        # spinner $(pidof make)

        ## Proper run
        echo "  Running $COMP..."
        OTP=$OTP_ROOT/$COMP
        $ACTION
    done

    exit 0

    ## Collect results in results/runtime.res:
    collect_results

    ## Plot results:
    plot_diagram runtime.res

    ## Backup all result files & diagrams to unique .res files:
    NEW_SUFFIX=`date +"%y.%m.%d-%H:%M:%S"`
    for c in "" "_beam" "_hipe" "_erllvm"; do
        mv results/runtime$c.res results/runtime$c-$NEW_SUFFIX.res
        mv results/runtime$c-err.res results/runtime$c-err-$NEW_SUFFIX.res
    done;
    mv diagrams/runtime.eps diagrams/runtime-$NEW_SUFFIX.eps
}

main $@
