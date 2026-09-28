  $ cat >pp.sh <<EOF
  > #!/bin/sh
  > sed 's/world/universe/g' \$1
  > EOF

  $ chmod a+x pp.sh

  $ cat >main.ml <<EOF
  > type world!;;
  > EOF

  $ cat >.merlin <<EOF
  > FLG -pp 'sh pp.sh'
  > EOF

  $ $MERLIN single dump -what ppxed-source -filename main.ml<main.ml |
  > tr -d '\n' | jq '.value' 
  "type universe"
