  $ $MERLIN single complete-prefix -position 3:17 \
  > -filename application_context < application_context.ml \
  > | tr '\n' ' ' | jq ".value.context"
  [
    "application",
    {
      "argument_type": "'a",
      "labels": [
        {
          "name": "~j",
          "type": "int"
        }
      ]
    }
  ]
