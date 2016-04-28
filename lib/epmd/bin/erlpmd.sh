#!/bin/bash
exec erl -s erlpmd_ctl -noinput $@
