  $ $MERLIN single case-analysis -start 2:10 -end 2:15 \
  > -filename main.ml <<EOF
  > let foo ?bar x = x
  > let () = foo ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 9
        },
        "end": {
          "line": 2,
          "col": 15
        }
      },
      "match foo () with | () -> _"
    ],
    "notifications": []
  }

$ $MERLIN single case-analysis -start 2:10 -end 2:15 \

  $ $MERLIN single case-analysis -start 2:10 -end 2:15 \
  > -filename main.ml <<EOF
  > let foo ?bar x = x
  > let () = foo ~bar:10 ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 9
        },
        "end": {
          "line": 2,
          "col": 23
        }
      },
      "match foo ~bar:10 () with | () -> _"
    ],
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 2:10 -end 2:15 \
  > -filename main.ml <<EOF
  > let foo ?bar x = x
  > let () = foo ?bar:(Some 10) ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 9
        },
        "end": {
          "line": 2,
          "col": 30
        }
      },
      "match foo ?bar:(Some 10) () with | () -> _"
    ],
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 2:10 -end 2:15 \
  > -filename main.ml <<EOF
  > let foo ?(bar = 10) x = x
  > let () = foo ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 9
        },
        "end": {
          "line": 2,
          "col": 15
        }
      },
      "match foo () with | () -> _"
    ],
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 2:10 -end 2:15 \
  > -filename main.ml <<EOF
  > let foo ?(bar = 10) x = x
  > let () = foo ~bar:15 ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 9
        },
        "end": {
          "line": 2,
          "col": 23
        }
      },
      "match foo ~bar:15 () with | () -> _"
    ],
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 2:10 -end 2:15 \
  > -filename main.ml <<EOF
  > let foo ?(bar = 10) x = x
  > let () = foo ?bar:None ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 9
        },
        "end": {
          "line": 2,
          "col": 25
        }
      },
      "match foo ?bar:None () with | () -> _"
    ],
    "notifications": []
  }

  $ $MERLIN single case-analysis -start 2:10 -end 2:15 \
  > -filename main.ml <<EOF
  > let foo ?(bar = 10) x = x
  > let () = foo ?bar:(Some 15) ()
  > EOF
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 2,
          "col": 9
        },
        "end": {
          "line": 2,
          "col": 30
        }
      },
      "match foo ?bar:(Some 15) () with | () -> _"
    ],
    "notifications": []
  }
