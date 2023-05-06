  $ ./run_json.exe test2.json -n 7
  Adding a class List with id  = 4
  Adding a class String with id  = 6
  Adding a class Int with id  = 7
  Running generated query
  
  [
    Var {id=_.38, index=_.39, upb=Class (4, [Wildcard (Some ((Extends, Class (1, []))))]), lwb=_.41};
    Null;
    Intersect ([Class (4, [Wildcard (Some ((Extends, Class (1, []))))]) | _.94]);
    Intersect ([_.96 [=/= Class (4, [Wildcard (Some ((Extends, Class (1, []))))])]; Class (4, [Wildcard (Some ((Extends, Class (1, []))))]) | _.170]);
    Intersect ([_.96 [=/= Class (4, [Wildcard (Some ((Extends, Class (1, []))))])]; _.172 [=/= Class (4, [Wildcard (Some ((Extends, Class (1, []))))])]; Class (4, [Wildcard (Some ((Extends, Class (1, []))))]) | _.196]);
    Intersect ([_.96 [=/= Class (4, [Wildcard (Some ((Extends, Class (1, []))))])]; _.172 [=/= Class (4, [Wildcard (Some ((Extends, Class (1, []))))])]; _.198 [=/= Class (4, [Wildcard (Some ((Extends, Class (1, []))))])]; Class (4, [Wildcard (Some ((Extends, Class (1, []))))]) | _.217]);
    Intersect ([_.96 [=/= Class (4, [Wildcard (Some ((Extends, Class (1, []))))])]; _.172 [=/= Class (4, [Wildcard (Some ((Extends, Class (1, []))))])]; _.198 [=/= Class (4, [Wildcard (Some ((Extends, Class (1, []))))])]; _.219 [=/= Class (4, [Wildcard (Some ((Extends, Class (1, []))))])]; Class (4, [Wildcard (Some ((Extends, Class (1, []))))]) | _.246])
  ]
