FIXME: the Pexp_constant and Pconst_string nodes have different locations.

  $ echo '    "test"' | $MERLIN single dump -what parsetree -filename test.ml 
  {
    "class": "return",
    "value": "[
    structure_item (test.ml[1,0+4]..[1,0+10])
      Pstr_eval
      expression (test.ml[1,0+4]..[1,0+10])
        Pexp_constant PConst_string(\"test\",(_none_[0,0+-1]..test.ml[1,0+9]),None)
  ]
  
  
  ",
    "notifications": []
  }
