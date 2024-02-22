  $ cat > test.ml << EOF
  > let find_vowel x = 
  > match x with 
  > | 'A' -> 
  >   true
  > | 'E' -> 
  >   true
  > | 'I' -> 
  >   true
  > | 'O' -> 
  >   true 
  > | 'U' -> 
  >   true 
  > | _ -> 
  >   false
  > EOF

Test if location of next case is given
  $ $MERLIN single jump -target match-next-case -position 3:3 -filename test.ml < test.ml
  {
    "class": "return",
    "value": {
      "pos": {
        "line": 5,
        "col": 2
      }
    },
    "notifications": []
  }

Test if location of prev case is given
  $ $MERLIN single jump -target match-prev-case -position 5:2 -filename test.ml < test.ml
  {
    "class": "return",
    "value": {
      "pos": {
        "line": 3,
        "col": 2
      }
    },
    "notifications": []
  }

Test when cursor is not in a match statement
  $ $MERLIN single jump -target match-prev-case -position 1:2 -filename test.ml < test.ml
  {
    "class": "return",
    "value": "No matching target",
    "notifications": []
  }


Test when there's no next case
  $ $MERLIN single jump -target match-next-case -position 13:2 -filename test.ml < test.ml
  {
    "class": "return",
    "value": "No next case found",
    "notifications": []
  }

Test when there's no previous case
  $ $MERLIN single jump -target match-prev-case -position 3:2 -filename test.ml < test.ml
  {
    "class": "return",
    "value": "No previous case found",
    "notifications": []
  }

Test jump from case 'O' to the previous case
  $ $MERLIN single jump -target match-prev-case -position 9:2 -filename test.ml < test.ml
  {
    "class": "return",
    "value": {
      "pos": {
        "line": 7,
        "col": 2
      }
    },
    "notifications": []
  }

Test jump from case 'O' to the next case
  $ $MERLIN single jump -target match-next-case -position 9:2 -filename test.ml < test.ml
  {
    "class": "return",
    "value": {
      "pos": {
        "line": 11,
        "col": 2
      }
    },
    "notifications": []
  }
