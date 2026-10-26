  $ cat >test.ml<<'EOF'
  > exception PinPon;;
  > raise PinPon;;
  > EOF

  $ $MERLIN single errors \
  > -filename test.ml <test.ml 
  {
    "class": "return",
    "value": [],
    "notifications": []
  }

  $ $MERLIN single occurrences -identifier-at 2:9 \
  > -filename test.ml <test.ml | jq '.value[].start'
  {
    "line": 1,
    "col": 10
  }
  {
    "line": 2,
    "col": 6
  }

We expect the same result as the previous query
  $ $MERLIN single occurrences -identifier-at 1:13 \
  > -filename test.ml <test.ml | jq '.value[].start'
  {
    "line": 1,
    "col": 10
  }
  {
    "line": 2,
    "col": 6
  }
