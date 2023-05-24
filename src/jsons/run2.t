  $ ./run_json.exe test2.json -n 7
  	 param_of_yojson: "{\"pname\":\"P1\",\"p_upper\":[[\"Class\",\"java.lang.Object\",[]]]}"
  Fallback: it's not  a type
  Running generated query
  
  [
    Var {id=_.38, index=_.39, upb=Class (6, [Wildcard (Some ((Extends, Class (4, []))))]), lwb=_.41};
    Null;
    Intersect ([Class (6, [Wildcard (Some ((Extends, Class (4, []))))]) | _.94]);
    Intersect ([_.96 [=/= Class (6, [Wildcard (Some ((Extends, Class (4, []))))])]; Class (6, [Wildcard (Some ((Extends, Class (4, []))))]) | _.168]);
    Intersect ([_.96 [=/= Class (6, [Wildcard (Some ((Extends, Class (4, []))))])]; _.170 [=/= Class (6, [Wildcard (Some ((Extends, Class (4, []))))])]; Class (6, [Wildcard (Some ((Extends, Class (4, []))))]) | _.200]);
    Intersect ([_.96 [=/= Class (6, [Wildcard (Some ((Extends, Class (4, []))))])]; _.170 [=/= Class (6, [Wildcard (Some ((Extends, Class (4, []))))])]; _.202 [=/= Class (6, [Wildcard (Some ((Extends, Class (4, []))))])]; Class (6, [Wildcard (Some ((Extends, Class (4, []))))]) | _.224]);
    Intersect ([_.96 [=/= Class (6, [Wildcard (Some ((Extends, Class (4, []))))])]; _.170 [=/= Class (6, [Wildcard (Some ((Extends, Class (4, []))))])]; _.202 [=/= Class (6, [Wildcard (Some ((Extends, Class (4, []))))])]; _.226 [=/= Class (6, [Wildcard (Some ((Extends, Class (4, []))))])]; Class (6, [Wildcard (Some ((Extends, Class (4, []))))]) | _.242])
  ]
