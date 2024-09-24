let () =
  Alcotest.run "merlin-lib.sherlodoc"
    [ Type_expr_test.cases;
      Name_cost_test.cases;
      Type_distance_test.cases;
      Query_test.cases
    ]
