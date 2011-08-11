#!/bin/sh

short=`hostname -s`
erl -sname poo@$short -remsh hnf_prod@$short -setcookie bob
