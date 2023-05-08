  $ ./run_json.exe test1.json -default -n 5
  1.1 (?) < Object : 
  [
    Class (1, []);
    Var {id=_.38, index=_.39, upb=Class (1, []), lwb=_.41};
    Interface (2, _.17);
    Null;
    Intersect ([Class (1, []) | _.94])
  ]
  Running generated query
  
  [
    Class (5, []);
    Var {id=_.38, index=_.39, upb=Class (5, []), lwb=_.41};
    Null;
    Intersect ([Class (5, []) | _.94]);
    Intersect ([_.96 [=/= Class (5, [])]; Class (5, []) | _.182])
  ]
