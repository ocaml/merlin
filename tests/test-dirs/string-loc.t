Ensure the Pexp_constant and Pconst_string nodes have different locations.

  $ echo '    "test"' | $MERLIN single dump -what parsetree -filename test.ml 
  {
    "class": "return",
    "value": "[
    structure_item (test.ml[1,0+4]..[1,0+10])
      Pstr_eval
      expression (test.ml[1,0+4]..[1,0+10])
        Pexp_constant PConst_string(\"test\",(test.ml[1,0+5]..[1,0+9]),None)
  ]
  
  
  ",
    "notifications": []
  }
