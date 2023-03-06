
  $ cat >test.ml <<EOF
  > let foo = object
  >   method bar i = i + 5
  >   method baz _k = "hello"
  > end
  > 
  > let something = foo#
  > EOF

FIXME: Upon typing `#` we should get a completion for bar and baz.
However this does not happen :-(. Interestingly utop gives bar and
baz as expected!

  $ $MERLIN single complete-prefix -position 6:21 -prefix "foo#" \
  > -filename test.ml < test.ml | jq
  {
    "class": "return",
    "value": {
      "entries": [],
      "context": null
    },
    "notifications": []
  }

