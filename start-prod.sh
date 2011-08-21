#!/bin/sh
here=`dirname $0`
exec erl -pa $here/ebin $here/deps/*/ebin -boot start_sasl -setcookie bob \
    -sname hnf_prod \
    -config hnf \
    -s hnf \
    -s reloader \
    -detached
