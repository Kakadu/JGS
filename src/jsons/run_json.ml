type test_args = {
  mutable json_name : string;
  mutable run_default : bool;
  mutable answers_count : int;
}

let test_args = { json_name = ""; run_default = false; answers_count = 1 }

let () =
  Arg.parse
    [
      ( "-default",
        Arg.Unit (fun () -> test_args.run_default <- true),
        " Run a default table-agnostic query (tests only)" );
      ("-v", Arg.Unit CT_of_json.set_verbose, " More verbose output");
      ( "-n",
        Arg.Int (fun n -> test_args.answers_count <- n),
        " Numer of answers requested (default 1)" );
    ]
    (fun file -> test_args.json_name <- file)
    ""

let run_jtype ?(n = test_args.answers_count) query =
  let pp_list f l =
    Printf.sprintf "\n[\n  %s\n]%!"
    @@ String.concat ";\n  " @@ Stdlib.List.map f l
  in
  pp_list JGS2.pp_ljtype @@ OCanren.Stream.take ~n
  @@ OCanren.(run q) query (fun q -> q#reify JGS.HO.jtype_reify)

let () =
  let j = Yojson.Safe.from_file test_args.json_name in

  (* Format.printf "%a\n%!" (Yojson.Safe.pretty_print ~std:true) j; *)
  let open JGS2 in
  let (module CT : JGS2.SAMPLE_CLASSTABLE), goal =
    match CT_of_json.make_query j with
    | x -> x
    | exception Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, j) ->
        Format.eprintf "%s\n%!" (Printexc.to_string exn);
        Format.eprintf "%a\n%!" (Yojson.Safe.pretty_print ~std:true) j;
        exit 1
  in

  let module V = JGS.FO.Verifier (CT) in
  let rec ( <-< ) ta tb = ta -<- tb (* not complete! *)
  and ( -<- ) ta tb = V.( -<- ) ( <-< ) ta tb in
  (* let module MM = struct
       open OCanren

       type hack =
         ( JGS.HO.jtype_injected JGS.HO.targ_injected List.HO.list_injected,
           Std.Nat.injected,
           JGS.HO.jtype_injected,
           JGS.HO.jtype_injected Option.HO.option_injected,
           JGS.HO.jtype_injected List.HO.list_injected )
         JGS.HO.jtype_fuly
         ilogic

       let (_ :
             (hack -> hack -> bool ilogic -> Peano.HO.goal) ->
             hack ->
             hack ->
             bool ilogic ->
             Peano.HO.goal) =
         V.( -<- )

       let (_ : hack -> hack -> bool ilogic -> Peano.HO.goal) = ( -<- )
     end in *)
  let () =
    if test_args.run_default then
      Printf.printf "1.1 (?) < Object : %s\n"
      @@ run_jtype ~n:test_args.answers_count (fun typ ->
             let open OCanren in
             ( -<- ) typ (jtype_inj @@ SampleCT.object_t) !!true)
  in
  (* let e_d_b_a =
       let open JGS in
       Class
         ( class_e,
           [
             Type (Class (class_d, [ Type (Class (class_b, [])) ]));
             Type (Class (class_a, []));
           ] )
     in *)
  let (_ : JGS.jtype JGS.targ -> _) = targ_inj in
  Format.printf "Running generated query\n%!";
  print_endline @@ run_jtype (fun typ -> goal ( -<- ) jtype_inj typ)
