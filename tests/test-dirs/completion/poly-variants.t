Completion on polymorphic variant work, and no-one should say the contrary. Here
is the proof:

  $ cat > test.ml <<EOF
  > type sentence = [ \`How_are_you_today | \`I_am_good_thank_you ]
  > 
  > let v : sentence = \`How
  > EOF

  $ $MERLIN single complete-prefix -position 3:23 -prefix "\`How" \
  > -filename test.ml < test.ml | jq '.value.entries'
  [
    {
      "name": "`How_are_you_today",
      "kind": "Variant",
      "desc": "`How_are_you_today",
      "info": "",
      "deprecated": false
    }
  ]
