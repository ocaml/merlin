Test 1

  $ echo "let () = ()" | $MERLIN single case-analysis -start 1:4 -end 1:4 -filename stacktrace.ml | grep -E -v "Raised|Called|Re-raised"
  {
    "class": "error",
    "value": "Destruct not allowed on value_binding",
    "notifications": []
  }

Test 2

  $ $MERLIN single case-analysis -start 4:2 -end 4:1 -filename nonode.ml <<EOF \
  > let f (x : int option) = \
  >   match w with    \
  >  | _ -> ()        \
  > EOF
  {
    "class": "error",
    "value": "Nothing to do",
    "notifications": []
  }
