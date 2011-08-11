#!/bin/sh
exec erl -pa ebin deps/*/ebin -boot start_sasl \
    -sname hnf_dev \
    -s hnf \
    -s reloader
