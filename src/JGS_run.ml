open OCanren
open OCanren.Std
open JGS
open JGS2

let sep () = Printf.printf "\n\n%s\n\n" @@ String.make 100 '*'

(**************************************************************************************************)
(************************************** Functional tests ******************************************)
(**************************************************************************************************)

let _ =
  Printf.printf "Fuctional tests:\n";

  let module V = Verifier (SampleCT) in
  let rec ( <-< ) ta tb = ta -<- tb (* not complete! *)
  and ( -<- ) ta tb = V.( -<- ) ( <-< ) ta tb in

  Printf.printf " 1 Object[] < Object (true) : %b\n"
    (Array SampleCT.object_t -<- SampleCT.object_t);
  Printf.printf " 2 Object[] < Cloneable (true) : %b\n"
    (Array SampleCT.object_t -<- SampleCT.cloneable_t);
  Printf.printf " 3 Object[] < Serializable (true) : %b\n"
    (Array SampleCT.object_t -<- SampleCT.serializable_t);

  Printf.printf " 4 Object < Object[] (false): %b\n"
    (SampleCT.object_t -<- Array SampleCT.object_t);
  Printf.printf " 5 Cloneable < Object[] (false): %b\n"
    (SampleCT.cloneable_t -<- Array SampleCT.object_t);
  Printf.printf " 6 Serializable < Object[] (false): %b\n"
    (SampleCT.serializable_t -<- Array SampleCT.object_t);

  Printf.printf " 7 Object[][] < Serializable[] (true) : %b\n"
    (Array (Array SampleCT.object_t) -<- Array SampleCT.serializable_t);

  (* class A {...} *)
  let class_a = SampleCT.make_class [] SampleCT.object_t [] in

  (* class B extends A {...} *)
  let class_b = SampleCT.make_class [] (Class (class_a, [])) [] in
  Printf.printf " 8 B < A (true) : %b\n"
    (Class (class_b, []) -<- Class (class_a, []));

  (* interface IA {...} *)
  let intf_a = SampleCT.make_interface [] [] in

  (* class C extends A implements IA {...} *)
  let class_c =
    SampleCT.make_class [] (Class (class_a, [])) [ Interface (intf_a, []) ]
  in
  Printf.printf " 9 C < A (true) : %b\n"
    (Class (class_c, []) -<- Class (class_a, []));
  Printf.printf "10 C < IA (true) : %b\n"
    (Class (class_c, []) -<- Interface (intf_a, []));

  (* interface IB extends IA {...} *)
  let intf_b = SampleCT.make_interface [] [ Interface (intf_a, []) ] in
  Printf.printf "11 IB < IA (true) : %b\n"
    (Interface (intf_b, []) -<- Interface (intf_a, []));

  (* class D<X> {...} *)
  let class_d =
    SampleCT.make_class [ SampleCT.object_t ] SampleCT.object_t []
  in

  (* class E<X, Y> {...} *)
  let class_e =
    SampleCT.make_class
      [ SampleCT.object_t; SampleCT.object_t ]
      SampleCT.object_t []
  in

  (* class F<X, Y> extends E<D<Y>, X> {...} *)
  let class_f =
    SampleCT.make_class
      [ SampleCT.object_t; SampleCT.object_t ]
      (Class
         ( class_e,
           [
             Type
               (Class
                  (class_d, [ Type (SampleCT.make_tvar 1 SampleCT.object_t) ]));
             Type (SampleCT.make_tvar 0 SampleCT.object_t);
           ] ))
      []
  in
  Printf.printf "12 F<A, B> < E<D<B>, A> (true) : %b\n"
    (Class (class_f, [ Type (Class (class_a, [])); Type (Class (class_b, [])) ])
    -<- Class
          ( class_e,
            [
              Type (Class (class_d, [ Type (Class (class_b, [])) ]));
              Type (Class (class_a, []));
            ] ))

(**************************************************************************************************)
(********************************* Relational tests (forward) *************************************)
(**************************************************************************************************)

let _ =
  sep ();
  SampleCT.reset ();

  Printf.printf "\n\nRelational tests (forward):\n";
  let module V = FO.Verifier (SampleCT) in
  let rec ( <-< ) ta tb = ta -<- tb (* not complete! *)
  and ( -<- ) ta tb = V.( -<- ) ( <-< ) ta tb in

  let run_bool query =
    [%show: GT.bool OCanren.logic GT.list] ()
    @@ Stream.take ~n:(-1)
    @@ run q query (fun q -> q#reify Std.Bool.reify)
  in

  Printf.printf " 1 Object[] < Object (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ Array SampleCT.object_t)
           (jtype_inj @@ SampleCT.object_t)
           q);

  Printf.printf " 2 Object[] < Cloneable (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ Array SampleCT.object_t)
           (jtype_inj @@ SampleCT.cloneable_t)
           q);

  Printf.printf " 3 Object[] < Serializable (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ Array SampleCT.object_t)
           (jtype_inj @@ SampleCT.serializable_t)
           q);

  Printf.printf " 4 Object < Object[] (false): %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ SampleCT.object_t)
           (jtype_inj @@ Array SampleCT.object_t)
           q);

  Printf.printf " 5 Cloneable < Object[] (false):%s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ SampleCT.cloneable_t)
           (jtype_inj @@ Array SampleCT.object_t)
           q);

  Printf.printf " 6 Serializable < Object[] (false): %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ SampleCT.serializable_t)
           (jtype_inj @@ Array SampleCT.object_t)
           q);

  Printf.printf " 7 Object[][] < Serializable[] (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ Array (Array SampleCT.object_t))
           (jtype_inj @@ Array SampleCT.serializable_t)
           q);

  (* class A {...} *)
  let class_a = SampleCT.make_class [] SampleCT.object_t [] in

  (* class B extends A {...} *)
  let class_b = SampleCT.make_class [] (Class (class_a, [])) [] in
  Printf.printf " 8 B < A (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ Class (class_b, []))
           (jtype_inj @@ Class (class_a, []))
           q);

  (* interface IA {...} *)
  let intf_a = SampleCT.make_interface [] [] in

  (* class C extends A implements IA {...} *)
  let class_c =
    SampleCT.make_class [] (Class (class_a, [])) [ Interface (intf_a, []) ]
  in
  Printf.printf " 9 C < A (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ Class (class_c, []))
           (jtype_inj @@ Class (class_a, []))
           q);

  Printf.printf "10 C < IA (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ Class (class_c, []))
           (jtype_inj @@ Interface (intf_a, []))
           q);

  (* interface IB extends IA {...} *)
  let intf_b = SampleCT.make_interface [] [ Interface (intf_a, []) ] in
  Printf.printf "11 IB < IA (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ Interface (intf_b, []))
           (jtype_inj @@ Interface (intf_a, []))
           q);

  (* class D<X> {...} *)
  let class_d =
    SampleCT.make_class [ SampleCT.object_t ] SampleCT.object_t []
  in

  (* class E<X, Y> {...} *)
  let class_e =
    SampleCT.make_class
      [ SampleCT.object_t; SampleCT.object_t ]
      SampleCT.object_t []
  in

  (* class F<X, Y> extends E<D<Y>, X> {...} *)
  let class_f =
    SampleCT.make_class
      [ SampleCT.object_t; SampleCT.object_t ]
      (Class
         ( class_e,
           [
             Type
               (Class
                  (class_d, [ Type (SampleCT.make_tvar 1 SampleCT.object_t) ]));
             Type (SampleCT.make_tvar 0 SampleCT.object_t);
           ] ))
      []
  in
  Printf.printf "12 F<A, B> < E<D<B>, A> (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj
           @@ Class
                ( class_f,
                  [ Type (Class (class_a, [])); Type (Class (class_b, [])) ] ))
           (jtype_inj
           @@ Class
                ( class_e,
                  [
                    Type (Class (class_d, [ Type (Class (class_b, [])) ]));
                    Type (Class (class_a, []));
                  ] ))
           q)

(**************************************************************************************************)
(********************************* Relational tests (backward) ************************************)
(**************************************************************************************************)

let _ =
  SampleCT.reset ();

  sep ();
  Printf.printf "\n\nRelational tests (backward):\n";
  let module V = FO.Verifier (SampleCT) in
  let rec ( <-< ) ta tb = ta -<- tb (* not complete! *)
  and ( -<- ) ta tb = V.( -<- ) ( <-< ) ta tb in

  let run_jtype ?(n = -1) query =
    let pp_list f l =
      Printf.sprintf "\n[\n  %s\n]%!"
      @@ String.concat ";\n  " @@ Stdlib.List.map f l
    in
    pp_list pp_ljtype @@ Stream.take ~n
    @@ run q query (fun q -> q#reify HO.jtype_reify)
  in

  Printf.printf "1.1 (?) < Object : %s\n"
  @@ run_jtype ~n:10 (fun q ->
         ( -<- ) q (jtype_inj @@ SampleCT.object_t) !!true);

  sep ();

  Printf.printf "1.2 Object[] < (?) : %s\n"
  @@ run_jtype ~n:(-1) (fun q ->
         ( -<- ) (jtype_inj @@ Array SampleCT.object_t) q !!true);

  sep ();

  Printf.printf "2 (?) < Cloneable : %s\n"
  @@ run_jtype ~n:10 (fun q ->
         ( -<- ) q (jtype_inj @@ SampleCT.cloneable_t) !!true);

  sep ();

  Printf.printf "3 (?) < Serializable : %s\n"
  @@ run_jtype ~n:10 (fun q ->
         ( -<- ) q (jtype_inj @@ SampleCT.serializable_t) !!true);

  sep ();

  Printf.printf "4.1 (?) < Object[] : %s\n"
  @@ run_jtype ~n:10 (fun q ->
         ( -<- ) q (jtype_inj @@ Array SampleCT.object_t) !!true);

  sep ();

  Printf.printf "4.2 Object < (?) : %s\n"
  @@ run_jtype ~n:(-1) (fun q ->
         ( -<- ) (jtype_inj @@ SampleCT.object_t) q !!true);

  sep ();

  Printf.printf "5 Cloneable < (?): %s\n"
  @@ run_jtype ~n:(-1) (fun q ->
         ( -<- ) (jtype_inj @@ SampleCT.cloneable_t) q !!true);

  sep ();

  Printf.printf "6 Serializable < (?) : %s\n"
  @@ run_jtype ~n:(-1) (fun q ->
         ( -<- ) (jtype_inj @@ SampleCT.serializable_t) q !!true);

  sep ();

  Printf.printf "7.1 (?) < Serializable[] : %s\n"
  @@ run_jtype ~n:10 (fun q ->
         ( -<- ) q (jtype_inj @@ Array SampleCT.serializable_t) !!true);

  sep ();

  Printf.printf "7.2 Object[][] < (?) : %s\n"
  @@ run_jtype ~n:(-1) (fun q ->
         ( -<- ) (jtype_inj @@ Array (Array SampleCT.object_t)) q !!true);

  sep ();

  (* class A {...} *)
  let class_a = SampleCT.make_class [] SampleCT.object_t [] in

  (* class B extends A {...} *)
  let class_b = SampleCT.make_class [] (Class (class_a, [])) [] in

  Printf.printf "Class A: %d\n\n" class_a;

  Printf.printf "Class B: %d\n\n" class_b;

  Printf.printf "8.1 (?) < A : %s\n"
  @@ run_jtype ~n:10 (fun q ->
         ( -<- ) q (jtype_inj @@ Class (class_a, [])) !!true);

  sep ();

  Printf.printf "8.2 B < (?) : %s\n"
  @@ run_jtype ~n:(-1) (fun q ->
         ( -<- ) (jtype_inj @@ Class (class_b, [])) q !!true);

  sep ();

  Printf.printf "8.3 (?) < B : %s\n"
  @@ run_jtype ~n:10 (fun q ->
         ( -<- ) q (jtype_inj @@ Class (class_b, [])) !!true);

  sep ();

  Printf.printf "8.4 A < (?) : %s\n"
  @@ run_jtype ~n:(-1) (fun q ->
         ( -<- ) (jtype_inj @@ Class (class_a, [])) q !!true);

  sep ();

  (* interface IA {...} *)
  let intf_a = SampleCT.make_interface [] [] in

  (* class C extends A implements IA {...} *)
  let class_c =
    SampleCT.make_class [] (Class (class_a, [])) [ Interface (intf_a, []) ]
  in

  Printf.printf "Interface A: %d\n\n" intf_a;

  Printf.printf "Class C: %d\n\n" class_c;

  Printf.printf "9 C < (?) : %s\n"
  @@ run_jtype ~n:(-1) (fun q ->
         ( -<- ) (jtype_inj @@ Class (class_c, [])) q !!true);

  sep ();

  Printf.printf "10.1 (?) < IA : %s\n"
  @@ run_jtype ~n:10 (fun q ->
         ( -<- ) q (jtype_inj @@ Interface (intf_a, [])) !!true);

  sep ();

  Printf.printf "10.2 C < (?) : %s\n"
  @@ run_jtype ~n:(-1) (fun q ->
         ( -<- ) (jtype_inj @@ Class (class_c, [])) q !!true);

  sep ();

  (* interface IB extends IA {...} *)
  let intf_b = SampleCT.make_interface [] [ Interface (intf_a, []) ] in

  Printf.printf "Interface B: %d\n\n" intf_b;

  Printf.printf "11 IB < (?) : %s\n"
  @@ run_jtype ~n:(-1) (fun q ->
         ( -<- ) (jtype_inj @@ Interface (intf_b, [])) q !!true);

  sep ();

  (* class D<X> {...} *)
  let class_d =
    SampleCT.make_class [ SampleCT.object_t ] SampleCT.object_t []
  in

  (* class E<X, Y> {...} *)
  let class_e =
    SampleCT.make_class
      [ SampleCT.object_t; SampleCT.object_t ]
      SampleCT.object_t []
  in

  (* class G<X> extends D <G<A>> *)
  let _class_g =
    SampleCT.make_class_fix
      ~params:(fun _self -> [ SampleCT.object_t ])
      (fun self ->
        Class (class_d, [ Type (Class (self, [ Type (Class (class_a, [])) ])) ]))
      (fun _self -> [])
  in

  (* class F<X, Y> extends E<D<Y>, X> {...} *)
  let class_f =
    SampleCT.make_class
      [ SampleCT.object_t; SampleCT.object_t ]
      (Class
         ( class_e,
           [
             Type
               (Class
                  (class_d, [ Type (SampleCT.make_tvar 1 SampleCT.object_t) ]));
             Type (SampleCT.make_tvar 0 SampleCT.object_t);
           ] ))
      []
  in
  Printf.printf "Class D<X>: %d\n\n" class_d;
  Printf.printf "Class E<X, Y>: %d\n\n" class_e;
  Printf.printf "Class F<X, Y> : %d\n\n" class_f;

  let f_ab =
    Class (class_f, [ Type (Class (class_a, [])); Type (Class (class_b, [])) ])
  in

  let e_d_b_a =
    Class
      ( class_e,
        [
          Type (Class (class_d, [ Type (Class (class_b, [])) ]));
          Type (Class (class_a, []));
        ] )
  in

  Printf.printf "12.1 (?) < E<D<B>, A> : %s\n"
  @@ run_jtype ~n:10 (fun q -> ( -<- ) q (jtype_inj @@ e_d_b_a) !!true);

  sep ();

  Printf.printf "12.2 (? - is class) < E<D<B>, A> : %s\n"
  @@ run_jtype ~n:10 (fun q ->
         fresh (a b)
           (q === !!(HO.Class (a, b)))
           (( -<- ) q (jtype_inj @@ e_d_b_a) !!true));

  sep ();

  Printf.printf "12.3 F<A, B> < (?) : %s\n"
  @@ run_jtype ~n:(-1) (fun q -> ( -<- ) (jtype_inj @@ f_ab) q !!true)
