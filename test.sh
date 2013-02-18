#!/bin/bash

# ocamlmerlin must be in path

if test -n "$1" ; then
    out=`mktemp`
    ocamlmerlin < ./tests/$1.in  > $out
    diff ./tests/$1.out $out
    rm $out
else
    for file in tests/*.in ; do
        test_nb=`basename $file .in`
        temp_out=`mktemp`
        real_out=`echo "$file"|sed 's/\.in$/.out/'`
        ocamlmerlin < $file  > $temp_out
        diff $temp_out $real_out > /dev/null
        if [[ $? != 0 ]] ; then
            echo "$test_nb: FAIL"
        else
            echo "$test_nb: OK"
        fi
        rm $temp_out
    done
fi
