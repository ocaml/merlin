#!/usr/bin/env bash

if test -n "$1" ; then
    out=`mktemp`
    while test -n "$1"; do
      ./ocamlmerlin.native < ./tests/$1.in  > $out
      # Use more appropriate jsondiff if available
      if which jsondiff >& /dev/null; then
        DIFF="jsondiff -color"
      else
        DIFF="diff -u"
      fi
      $DIFF ./tests/$1.out $out
      shift 1
    done
    rm $out
else
    for file in tests/*.in ; do
        test_nb=`basename $file .in`
        temp_out=`mktemp`
        real_out=`echo "$file"|sed 's/\.in$/.out/'`
        ./ocamlmerlin.native < $file  > $temp_out
        diff $temp_out $real_out > /dev/null
        if [[ $? != 0 ]] ; then
            echo -e "$test_nb: \e[1;31mFAILED\e[0m"
            echo "    run ./test.sh $test_nb to have more informations"
        else
            echo -e "$test_nb: \e[1;32mOK\e[0m"
        fi
        rm $temp_out
    done
fi
