  $ dune build @check

FIXME
When the deifinition is in one of the implicit transitive dependencies Merlin
used to not find the file in the source path provided by Dune. 
  $ $MERLIN single locate -look-for ml -position 1:15 \
  > -filename bin/main.ml <bin/main.ml
  {
    "class": "return",
    "value": "'Lib1.t' seems to originate from 'Lib2' whose ML file could not be found",
    "notifications": []
  }
