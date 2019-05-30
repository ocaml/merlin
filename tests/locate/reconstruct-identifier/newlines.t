We need to be careful about newlines in Lexer_ident:

  $ $MERLIN single locate -look-for ml -position 7:20 \
  > -filename ./newline_in_quotes.ml < ./newline_in_quotes.ml
  {
    "class": "return",
    "value": "Not in environment 'Foo_bar_lol'",
    "notifications": []
  }

  $ $MERLIN single locate -look-for ml -position 6:20 \
  > -filename ./escaped_newline.ml < ./escaped_newline.ml
  {
    "class": "return",
    "value": "Not in environment 'Foo_bar_lol'",
    "notifications": []
  }
