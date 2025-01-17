open OCanren
open OCanren.Std
open JGS
open JGS_Helpers
open MutableTypeTable

let pp_list f l =
  Printf.sprintf "\n[\n  %s\n]%!"
  @@ String.concat ";\n  " @@ Stdlib.List.map f l

let run_jtype ?(n = -1) ~msg query =
  Printf.printf "%s: %s\n\n" msg
  @@ pp_list pp_ljtype @@ Stream.take ~n
  @@ run q query (fun q -> q#reify HO.jtype_reify)

let run_jtypes ?(n = -1) ~msg query =
  Printf.printf "%s: %s\n\n" msg
  @@ pp_list (GT.show Std.List.logic pp_ljtype)
  @@ Stream.take ~n
  @@ run q query (fun q -> q#reify (Std.List.reify HO.jtype_reify))

let rec are_not_equal = function
  | [] -> success
  | x :: xs ->
      Stdlib.List.fold_left
        (fun acc y -> acc &&& (x =/= y))
        (are_not_equal xs) xs

let _ =
  let module SampleCT = SampleCT () in
  let module V = FO.Verifier (SampleCT) in
  let is_correct_type, ( -<- ), ( <-< ) =
    Closure.make_closure (module SampleCT) V.( -<- )
  in
  (* let ( <-< ) ta tb = failwith "Oh..." in
     let is_correct_type t =
       Closure.is_correct_type (module SampleCT) ~closure_subtyping:( <-< ) t
     in
     let ( -<- ) ta tb =
       Closure.( -<- )
         (module SampleCT)
         ~direct_subtyping:V.( -<- ) ~closure_subtyping:( <-< ) ~is_correct_type ta
         tb
     in *)
  let class_a = SampleCT.make_class [] SampleCT.object_t [] in
  let a = Class (class_a, []) in
  Printf.printf "Class A: %d\n" class_a;
  let class_b = SampleCT.make_class [] a [] in
  let b = Class (class_b, []) in
  Printf.printf "Class B extends A: %d\n" class_b;

  let class_c = SampleCT.make_class [] b [] in
  let c = Class (class_c, []) in
  Printf.printf "Class C extends B: %d\n" class_c;

  let class_a1 = SampleCT.make_class [] SampleCT.object_t [] in
  let a1 = Class (class_a1, []) in
  Printf.printf "Class A1: %d\n" class_a1;

  (****************************************************************************)

  (* Many answers with intersects and variables *)
  let __ _ = run_jtype ~msg:"? <-< A" ~n:10 (fun q -> q <-< jtype_inj a) in

  (* Many repeats of B, no mentions of C *)
  let __ _ =
    run_jtype ~msg:"? <-< A (without intersects vars and null)" ~n:10
      ( remove_intersercts_and_vars @@ fun q ->
        fresh () (q =/= !!HO.Null) (q <-< jtype_inj a) )
  in

  (* But we can get C if we explicitly ask *)
  let __ _ =
    run_jtype ~msg:"C <-< A" ~n:10
      ( remove_intersercts_and_vars @@ fun q ->
        fresh () (q === jtype_inj c) (q <-< jtype_inj a) )
  in

  (* Evaluates without answers? Seems like right behavior *)
  let __ _ =
    run_jtype ~msg:"A1 <-< A" ~n:1
      ( remove_intersercts_and_vars @@ fun q ->
        fresh () (q === jtype_inj a1) (q <-< jtype_inj a) )
  in

  (* How much 1 length paths from A to B? Only one. *)
  let _ =
    run_jtypes ~msg:"B <-1-< A" ~n:(-1) (fun q ->
        fresh (sub super)
          (super === jtype_inj a)
          (sub === jtype_inj b)
          (q === OCanren.Std.list Fun.id [ super; sub ])
          (sub -<- super))
  in

  (* How much 2 length paths from A to B?
     Seems like an infinite number of identical answers with the correct variable *)
  let _ =
    run_jtypes ~msg:"B <-2-< A" ~n:10 (fun q ->
        fresh (sub super t1)
          (are_not_equal [ sub; super; t1 ])
          (super === jtype_inj a)
          (sub === jtype_inj b)
          (q === OCanren.Std.list Fun.id [ super; t1; sub ])
          (t1 -<- super) (sub -<- t1))
  in

  (* How much 3 length paths from A to B?
     Evaluates without answers... *)
  let __ _ =
    run_jtypes ~msg:"B <-3-< A" ~n:1 (fun q ->
        fresh (sub super t1 t2)
          (are_not_equal [ sub; super; t1; t2 ])
          (super === jtype_inj a)
          (sub === jtype_inj b)
          (q === OCanren.Std.list Fun.id [ super; t1; t2; sub ])
          (t1 -<- super) (t2 -<- t1) (sub -<- t2))
  in

  (* How much 3 length paths from A to B?
     Evaluates without answers... *)
  let __ _ =
    run_jtypes ~msg:"B <-4-< A" ~n:1 (fun q ->
        fresh (sub super t1 t2 t3)
          (are_not_equal [ sub; super; t1; t2; t3 ])
          (super === jtype_inj a)
          (sub === jtype_inj b)
          (q === OCanren.Std.list Fun.id [ super; t1; t2; t3; sub ])
          (t1 -<- super) (t2 -<- t1) (t3 -<- t2) (sub -<- t3))
  in

  (* How much 1 length paths from A to С?
     Correct, no answers. *)
  let _ =
    run_jtypes ~msg:"C <-1-< A" ~n:(-1) (fun q ->
        fresh (sub super)
          (super === jtype_inj a)
          (sub === jtype_inj c)
          (q === OCanren.Std.list Fun.id [ super; sub ])
          (sub -<- super))
  in

  (* How much 1 length paths from A to С?
     Only one answer is given, then evaluates without answers.
     Seems like correct behavior. *)
  let _ =
    run_jtypes ~msg:"C <-2-< A" ~n:1 (fun q ->
        fresh (sub super t1)
          (are_not_equal [ sub; super; t1 ])
          (super === jtype_inj a)
          (sub === jtype_inj c)
          (q === OCanren.Std.list Fun.id [ super; t1; sub ])
          (t1 -<- super) (sub -<- t1))
  in

  (* How much 3 length paths from A to C?
     Evaluates without answers...
     This is strange, because we have a path like {C} <- {var(C, B)} <- {B} <- {A}.
     Maybe this answer is too deep in the search tree *)
  let __ _ =
    run_jtypes ~msg:"C <-3-< A" ~n:1 (fun q ->
        fresh (sub super t1 t2)
          (are_not_equal [ sub; super; t1; t2 ])
          (super === jtype_inj a)
          (sub === jtype_inj c)
          (q === OCanren.Std.list Fun.id [ super; t1; t2; sub ])
          (t1 -<- super) (t2 -<- t1) (sub -<- t2))
  in
  ()

let _ =
  let module SampleCT = SampleCT () in
  let module V = FO.Verifier (SampleCT) in
  let is_correct_type, ( -<- ), ( <-< ) =
    Closure.make_closure (module SampleCT) V.( -<- )
  in
  let class_int = SampleCT.make_class [] SampleCT.object_t [] in
  let int = Class (class_int, []) in
  Printf.printf "Class Int: %d\n" class_int;

  let type_var =
    Var { id = 42; index = 0; lwb = None; upb = SampleCT.object_t }
  in
  let interface_icollection = SampleCT.make_interface [ type_var ] [] in
  let icollection = Interface (interface_icollection, [ Type type_var ]) in
  Printf.printf "Interface ICollection: %d\n" interface_icollection;

  let class_list =
    SampleCT.make_class [ type_var ] SampleCT.object_t [ icollection ]
  in

  Printf.printf "Class List: %d\n" class_list;

  let int_collection = Interface (interface_icollection, [ Type int ]) in

  run_jtype ~n:1 ~msg:"? <-< ICollection<int>"
    ( remove_intersercts_and_vars @@ fun q ->
      fresh super
        (super === jtype_inj int_collection)
        (q =/= jtype_inj Null)
        (q <-< super) );
  ()
