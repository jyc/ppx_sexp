open Asttypes
open Parsetree
open Ast_mapper
open Ast_helper

type sexp = [
    `Char of char
  | `Float of string
  | `Symbol of string
  | `Int of int
  | `Int32 of int32
  | `Int64 of int64
  | `Nativeint of nativeint
  | `String of string
  | `Bool of bool
  | `List of sexp list

  (* These last two types are not present in generated code. *)
  | `Expr of string * Parsetree.expression
  | `Splice of Parsetree.expression * [`Alias | `Raw]
  | `Error of Location.t * string
]

let atom_constr_of_alias, is_alias =
  let h = Hashtbl.create 1 in
  let () = begin
    Hashtbl.add h "char" "Char" ;
    Hashtbl.add h "float" "Float" ;
    Hashtbl.add h "symbol" "Symbol" ;
    Hashtbl.add h "int" "Int" ;
    Hashtbl.add h "int32" "Int32" ;
    Hashtbl.add h "int64" "Int64" ;
    Hashtbl.add h "nativeint" "Nativeint" ;
    Hashtbl.add h "string" "String" ;
    Hashtbl.add h "bool" "Bool" ;
    Hashtbl.add h "list" "List" ;
  end in
  Hashtbl.find h, Hashtbl.mem h

let string_of_id id =
  Longident.flatten id
  |> String.concat "."

let quote_id ?fudge id =
  let s = string_of_id id in
  `Symbol (
    match fudge with
    | None -> s
    | Some c ->
      if s.[String.length s - 1] = c then
        String.sub s 0 (String.length s - 1)
      else s
  )

let error loc msg =
  { pexp_desc =
      Pexp_extension ({ txt = "ocaml.error"; loc = loc },
                      PStr [{ pstr_desc =
                                Pstr_eval ({ pexp_desc =
                                               Pexp_constant (Const_string ("ppx_sexp: " ^ msg, None));
                                             pexp_loc = Location.none;
                                             pexp_attributes = [] },
                                           []);
                              pstr_loc = Location.none }]);
    pexp_loc = loc;
    pexp_attributes = [] }

(** quote_args' returns an alist in sexp form of the arguments in an
    application, given an AST subtree. Note that the result is not wrapped
    in an sexp. *)
let rec quote_args' ?fudge args =
  (List.fold_right
    (fun (l, x) a ->
       match l with
       | "" -> quote ?fudge x :: a
       | s -> `List [`Symbol s; quote ?fudge x] :: a)
    args [])

and quote_list ?fudge x =
  let rec quote_list' { pexp_desc } =
    match pexp_desc with
    | Pexp_construct ({ txt = Longident.Lident "::" }, Some { pexp_desc = Pexp_tuple xs }) ->
      begin match xs with
        | hd :: tl :: [] ->
          hd :: quote_list' tl
        | _ -> failwith "Internal error: cons constructor tuple contains more than two arguments."
      end
    | Pexp_construct ({ txt = Longident.Lident "[]" }, None) ->
      []
    | _ -> invalid_arg "Not a list."
  in `List (List.map (quote ?fudge) (quote_list' x))

and quote ?fudge ({ pexp_desc; pexp_loc } as x) =
  let quote' = quote ?fudge in
  match pexp_desc with
  | Pexp_ident { txt = id } ->
    quote_id ?fudge id
  | Pexp_constant const ->
    begin match const with
      | Const_int x -> `Int x
      | Const_char x -> `Char x
      (* The second element in the tuple is the quoted-string-id, see
         http://caml.inria.fr/pub/docs/manual-ocaml/extn.html#sec244 .*)
      | Const_string (x, _) -> `String x
      | Const_float x -> `Float x
      | Const_int32 x -> `Int32 x
      | Const_int64 x -> `Int64 x
      | Const_nativeint x -> `Nativeint x
    end
  | Pexp_apply (x, args) ->
    `List (quote' x :: quote_args' ?fudge args)
  | Pexp_tuple xs ->
    `List (List.map quote' xs)
  | Pexp_construct ({ txt = id }, x') ->
    begin match id, x' with
      | Longident.Lident "::", Some _ ->
        quote_list ?fudge x
      | Longident.Lident "true", Some _ ->
        `Bool true
      | Longident.Lident "false", Some _ ->
        `Bool false
      | _, Some x ->
        `List [quote_id id; quote' x]
      | _, None ->
        quote_id id
    end
  | Pexp_variant (l, x') ->
    begin match x' with
      | Some x ->
        `List [`String l; quote' x]
      | None ->
        `String l
    end
  (* Turn records into alists. *)
  | Pexp_record (sets, from) ->
    begin match from with
      | Some _ -> `Error (pexp_loc, "Records creations using 'with' cannot be quoted.")
      | None ->
        `List (List.map (fun ({ txt = id }, x) -> `List [quote_id ?fudge id; quote' x]) sets)
    end
  | Pexp_field (x, { txt = id }) ->
    begin match quote' x with
      | `Symbol s -> `Symbol (s ^ "." ^ string_of_id id)
      | _ -> `Error (pexp_loc, "Field assignments accessing objects not identified by simple identifiers cannot be quoted.")
    end
  | Pexp_setfield _ -> `Error (pexp_loc, "Field assignments cannot be quoted.")
  | Pexp_array xs ->
    `List (List.map quote' xs)
  (* We could support these, but it's not clear whether users would use these for their s-expressions. *)
  | Pexp_sequence _ -> failwith "Sequenced expressions cannot be quoted."
  | Pexp_extension ({ txt }, PStr ss) when txt = "in" || txt = "sp" || txt = "spls" || is_alias txt ->
    begin match ss with
      | { pstr_desc = Pstr_eval (x, _) } :: [] ->
        begin match txt with
          | "sp" -> `Splice (x, `Raw)
          | "spls" -> `Splice (x, `Alias)
          | _ -> `Expr (txt, x)
        end
      | _ -> `Error (pexp_loc, "Invalid body of insertion: must only contain one structure element.")
    end
  (* We could try to quote these, e.g. let x = 5, but the client would still be
     constrained to only use these OCaml keywords in a way that would be valid
     OCaml syntax, so it's unclear whether or not it's worth it. *)
  | Pexp_let _ | Pexp_function _ | Pexp_fun _ | Pexp_match _ | Pexp_try _
  | Pexp_ifthenelse _ | Pexp_while _ | Pexp_for _ | Pexp_constraint _
  | Pexp_coerce _ | Pexp_send _ | Pexp_new _ | Pexp_setinstvar _
  | Pexp_override _ | Pexp_letmodule _ | Pexp_assert _ | Pexp_lazy _
  | Pexp_poly _ | Pexp_object _ | Pexp_newtype _ | Pexp_pack _ | Pexp_open _
  | Pexp_extension _ ->
    `Error (pexp_loc, "OCaml keywords cannot be quoted.")

let rec encode mapper s = 
  let expr x =
    { pexp_desc = x; pexp_loc = Location.none; pexp_attributes = [] } in
  let constr ?args x = 
    expr (Pexp_construct ({ txt = Longident.Lident x; loc = Location.none }, args)) in
  let encode' v d =
    expr (Pexp_variant (v, Some { pexp_desc = d;
                                  pexp_loc = Location.none;
                                  pexp_attributes = [] })) in
  let const v c = encode' v (Pexp_constant c) in
  let extract_splice xs =
    let rec extract_splice' xs pre =
      match xs with
      | `Splice x :: tl ->
        Some (x, List.rev pre, tl)
      | hd :: tl ->
        extract_splice' tl (hd :: pre)
      | [] ->
        None
    in extract_splice' xs []
  in
  let nil = constr "[]" in
  let cons a b =
    constr "::" ~args:{ pexp_desc = Pexp_tuple [a; b];
                        pexp_loc = Location.none;
                        pexp_attributes = [] } in
  let ls xs = List.fold_right cons xs nil in
  (* [dels x] returns a Parsetree.expression that evaluates to the list contained in the `List x. *)
  let dels x =
    { pexp_desc =
        Pexp_let (Nonrecursive,
                  [{ pvb_pat =
                       { ppat_desc =
                           Ppat_variant ("List",
                                         Some { ppat_desc =
                                                  Ppat_var { txt = "x";
                                                             loc = Location.none};
                                                ppat_loc = Location.none;
                                                ppat_attributes = []
                                              }); 
                         ppat_loc = Location.none;
                         ppat_attributes = [] };
                     pvb_expr = x;
                     pvb_attributes = [];
                     pvb_loc = Location.none }],
                  { pexp_desc = Pexp_ident { txt = Longident.Lident "x"; loc = Location.none };
                    pexp_loc = Location.none;
                    pexp_attributes = [] }
                 );
      pexp_loc = Location.none;
      pexp_attributes = []
    } in
  let remap = mapper.expr mapper in

  match s with
  | `Char x ->
    const "Char" (Const_char x)
  | `Float x ->
    const "Float" (Const_float x)
  | `Symbol x ->
    const "Symbol" (Const_string (x, None))
  | `Int x ->
    const "Int" (Const_int x)
  | `Int32 x ->
    const "Int32" (Const_int32 x)
  | `Int64 x ->
    const "Int64" (Const_int64 x)
  | `Nativeint x ->
    const "Nativeint" (Const_nativeint x)
  | `String x ->
    const "String" (Const_string (x, None))
  | `Bool true ->
    constr "true"
  | `Bool false ->
    constr "false"
  | `List xs ->
    begin match extract_splice xs with
    | Some ((x, raw), pre, post) ->
      encode' "List" (Pexp_apply ({ pexp_desc = Pexp_ident { txt = Longident.Ldot (Longident.Lident "List", "concat");
                                                             loc = Location.none };
                                    pexp_loc = Location.none;
                                    pexp_attributes = []},
                                  [("", ls [ls (List.map (encode mapper) pre); 
                                            (match raw with
                                              | `Raw -> dels (remap x)
                                              | `Alias -> (remap x));
                                            (* We put it into a `List so we can recurse and handle recursive splices. *)
                                            dels (encode mapper (`List post))])]))
    | None ->
      expr (Pexp_variant ("List", Some (ls (List.map (encode mapper) xs))))
    end
  | `Splice _ ->
    failwith "Internal error: encountered bare splice."
  | `Expr ("in", x) -> (remap x)
  | `Expr (alias, x) ->
    expr (Pexp_variant (atom_constr_of_alias alias, Some (remap x)))
  | `Error (loc, msg) -> error loc msg

let sexp_mapper argv =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt = "sexp" }, PStr ss) } ->
        begin match ss with
          | { pstr_desc = Pstr_eval (expr, _) } :: [] ->
            encode mapper (quote expr)
          | _ -> failwith "The extension payload must contain only one s-expression."
        end
      | { pexp_desc = Pexp_extension ({ txt = "sfxp" }, PStr ss) } ->
        begin match ss with
          | { pstr_desc = Pstr_eval (expr, _) } :: [] ->
            begin match expr with
              | { pexp_desc = Pexp_tuple [{ pexp_desc = Pexp_constant (Const_char c) }; x] } ->
                encode mapper (quote ~fudge:c x)
              | _ -> failwith "Expected an application of the form [%sxfp fudge_char, expr]."
            end
          | _ -> failwith "The extension payload must contain only one s-expression."
        end
      | other -> default_mapper.expr mapper other }

let () =
  register "ppx_sexp" sexp_mapper
