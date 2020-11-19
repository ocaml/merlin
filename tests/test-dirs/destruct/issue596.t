  $ $MERLIN single case-analysis -start 1:5 -end 1:5 -filename bug.ml <<EOF | sed -e 's/,_)/, _)/g'
  > let a = 1 in a + 1 ;;
  > EOF
  {
    "class": "error",
    "value": "Destruct not allowed on value_binding",
    "notifications": []
  }
