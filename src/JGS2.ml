open OCanren
open OCanren.Std
open JGS

(**************************************************************************************************)
(************************************* Pretty-printing ********************************************)
(**************************************************************************************************)

let pp_lnat : Std.Nat.logic -> string =
 fun n ->
  let rec helper n =
    match n with
    | OCanren.Var _ -> (0, Some n)
    | Value Std.Nat.O -> (0, None)
    | Value (S n') ->
        let n, v = helper n' in
        (n + 1, v)
  in
  match helper n with
  | 0, None -> "0"
  | 0, Some v -> [%show: Std.Nat.logic] () v
  | n, None -> Printf.sprintf "%d" n
  | n, Some v -> Printf.sprintf "%d + %s" n @@ [%show: Std.Nat.logic] () v

type 'a x = 'a HO.targ

let rec pp_ltarg : HO.jtype_logic HO.targ_logic -> string =
 fun arg ->
  GT.show OCanren.logic
    (GT.show HO.targ_fuly pp_ljtype
       (GT.show Std.Option.logic
       @@ GT.show Std.Pair.logic (GT.show HO.polarity_logic) pp_ljtype))
    arg

and pp_ljtype : HO.jtype_logic -> string =
 fun t ->
  GT.show OCanren.logic
    (GT.show HO.jtype_fuly
       (GT.show Std.List.logic pp_ltarg)
       pp_lnat pp_ljtype
       (GT.show Std.Option.logic pp_ljtype)
       (GT.show Std.List.logic pp_ljtype))
    t

(**************************************************************************************************)
(**************************************** Injectors ***********************************************)
(**************************************************************************************************)

let pair_inj : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> ('b, 'd) Std.Pair.injected
    =
 fun f g (a, b) -> !!(f a, g b)

let polarity_inj : polarity -> HO.polarity_injected = function
  | Extends -> !!HO.Extends
  | Super -> !!HO.Super

let option_inj : ('a -> 'b) -> 'a option -> 'b Std.Option.injected =
 fun f -> function None -> Std.none () | Some x -> Std.some (f x)

let rec targ_inj : jtype targ -> HO.jtype_injected HO.targ_injected = function
  | Type t -> !!(HO.Type (jtype_inj t))
  | Wildcard x ->
      !!(HO.Wildcard (option_inj (pair_inj polarity_inj jtype_inj) x))

and jtype_inj : jtype -> HO.jtype_injected = function
  | Null -> !!HO.Null
  | Array t -> !!(HO.Array (jtype_inj t))
  | Class (id, args) -> !!(HO.Class (Std.nat id, Std.list targ_inj args))
  | Interface (id, args) ->
      !!(HO.Interface (Std.nat id, Std.list targ_inj args))
  | Var { id; index; upb; lwb } ->
      !!(HO.Var
           {
             id = Std.nat id;
             index = Std.nat index;
             upb = jtype_inj upb;
             lwb = option_inj jtype_inj lwb;
           })
  | Intersect l -> !!(HO.Intersect (Std.list jtype_inj l))

let idecl_inj : idecl -> HO.idecl_injected =
 fun { params; supers } ->
  !!HO.
      { params = Std.list jtype_inj params; supers = Std.list jtype_inj supers }

let cdecl_inj : cdecl -> HO.cdecl_injected =
 fun { params; super; supers } ->
  !!HO.
      {
        params = Std.list jtype_inj params;
        super = jtype_inj super;
        supers = Std.list jtype_inj supers;
      }

let decl_inj : decl -> HO.decl_injected = function
  | I i -> !!(HO.I (idecl_inj i))
  | C c -> !!(HO.C (cdecl_inj c))

(**************************************************************************************************)
(*************************** Functional-relational fuctor parameter *******************************)
(**************************************************************************************************)

module type SAMPLE_CLASSTABLE = sig
  val decl_by_id : int -> decl
  val object_t : jtype
  val cloneable_t : jtype
  val serializable_t : jtype
  val new_var : unit -> int
  val reset : unit -> unit
  val make_class : jtype list -> jtype -> jtype list -> int
  val make_tvar : int -> jtype -> jtype
  val make_interface : jtype list -> jtype list -> int

  val make_class_fix :
   params: (int -> jtype list) -> (int -> jtype) -> (int -> jtype list) -> int

  val make_interface_fix : (int -> jtype list) -> (int -> jtype list) -> int

  module HO : sig
    val decl_by_id :
      (OCanren__.Nat.injected -> goal) -> HO.decl_injected -> goal

    val object_t : HO.jtype_injected -> goal
    val cloneable_t : HO.jtype_injected -> goal
    val serializable_t : HO.jtype_injected -> goal
    val new_var : (GT.unit ilogic -> goal) -> OCanren__.Nat.injected -> goal
  end
end

module SampleCT : SAMPLE_CLASSTABLE = struct
  let reset_vars, new_id =
    let n = ref 1 in
    ( (fun () -> n := 1),
      fun () ->
        let i = !n in
        incr n;
        i )

  module M = Map.Make (struct
    type t = int

    let compare = compare
  end)

  let make_tvar index upb = Var { id = new_id (); index; upb; lwb = None }
  let m = ref M.empty
  let make_params params = Stdlib.List.mapi (fun i p -> make_tvar i p) params

  let reset_map, add_class, add_interface, decl_by_id, decl_by_id_rel =
    ( (fun () -> m := M.empty),
      (fun (c : cdecl) ->
        let id = new_id () in
        let d = C { c with params = make_params c.params } in
        m := M.add id d !m;
        id),
      (fun (i : idecl) ->
        let id = new_id () in
        let d = I { i with params = make_params i.params } in
        m := M.add id d !m;
        id),
      (fun id -> M.find id !m),
      fun id rez ->
        fresh id_val (id id_val)
          (let disjs =
             Stdlib.List.map (fun (k, v) ->
                 fresh () (id_val === Std.nat k) (rez === decl_inj v))
             @@ M.bindings !m
           in
           match disjs with [] -> failure | _ -> conde disjs) )

  let add_class_fix (c : int -> cdecl) =
    let id = new_id () in
    let c = c id in
    let d = C { c with params = make_params c.params } in
    m := M.add id d !m;
    id

  let add_interface_fix (i : int -> idecl) =
    let id = new_id () in
    let iface = i id in
    let d = I { iface with params = make_params iface.params } in
    m := M.add id d !m;
    id

  let reset () =
    reset_vars ();
    reset_map ()

  let make_tvar index upb = Var { id = new_id (); index; upb; lwb = None }
  let make_class params super supers = add_class { params; super; supers }
  let make_interface params supers = add_interface { params; supers }

  let make_class_fix ~params super supers =
    add_class_fix (fun id ->
        { params = params id; super = super id; supers = supers id })

  let make_interface_fix params supers =
    add_interface_fix (fun id -> { params = params id; supers = supers id })

  let top = Class (0, [])

  let object_t =
    let id = make_class [] top [] in
    Class (id, [])

  let cloneable_t =
    let id = make_interface [] [] in
    Interface (id, [])

  let serializable_t =
    let id = make_interface [] [] in
    Interface (id, [])

  let new_var = new_id

  module HO = struct
    let decl_by_id = decl_by_id_rel
    let top = Class (-1, [])
    let object_t x = x === jtype_inj object_t
    let cloneable_t x = x === jtype_inj cloneable_t
    let serializable_t x = x === jtype_inj serializable_t
    let new_var _ x = x === Std.nat (new_id ())
  end
end
