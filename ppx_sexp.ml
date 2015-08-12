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
  | `Expr of string * Parsetree.structure
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

let rec quote_id id =
  string_of_id id
  |> fun x -> `Symbol x

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
let rec quote_args' args =
  (List.fold_right
    (fun (l, x) a ->
       match l with
       | "" -> quote x :: a
       | s -> `List [`Symbol s; quote x] :: a)
    args [])

and quote_list x =
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
  in `List (List.map quote (quote_list' x))

and quote ({ pexp_desc; pexp_loc } as x) =
  match pexp_desc with
  | Pexp_ident { txt = id } ->
    quote_id id
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
    `List (quote x :: quote_args' args)
  | Pexp_tuple xs ->
    `List (List.map quote xs)
  | Pexp_construct ({ txt = id }, x') ->
    begin match id, x' with
      | Longident.Lident "::", Some _ ->
        quote_list x
      | Longident.Lident "true", Some _ ->
        `Bool true
      | Longident.Lident "false", Some _ ->
        `Bool false
      | _, Some x ->
        `List [quote_id id; quote x]
      | _, None ->
        quote_id id
    end
  | Pexp_variant (l, x') ->
    begin match x' with
    | Some x ->
      `List [`String l; quote x]
    | None ->
      `String l
    end
  (* Turn records into alists. *)
  | Pexp_record (sets, from) ->
    begin match from with
    | Some _ -> `Error (pexp_loc, "Records creations using 'with' cannot be quoted.")
    | None ->
      `List (List.map (fun ({ txt = id }, x) -> `List [quote_id id; quote x]) sets)
    end
  | Pexp_field (x, { txt = id }) ->
    begin match quote x with
      | `Symbol s -> `Symbol (s ^ "." ^ string_of_id id)
      | _ -> `Error (pexp_loc, "Field assignments accessing objects not identified by simple identifiers cannot be quoted.")
    end
  | Pexp_setfield _ -> `Error (pexp_loc, "Field assignments cannot be quoted.")
  | Pexp_array xs ->
    `List (List.map quote xs)
  (* We could support these, but it's not clear whether users would use these for their s-expressions. *)
  | Pexp_sequence _ -> failwith "Sequenced expressions cannot be quoted."
  | Pexp_extension ({ txt }, PStr ss) when txt = "in" || is_alias txt ->
    begin match ss with
      | { pstr_desc = Pstr_eval (x, _) } :: [] ->
        `Expr (txt, x)
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

let rec encode s = 
  let expr x =
    { pexp_desc = x; pexp_loc = Location.none; pexp_attributes = [] } in
  let constr ?args x = 
    expr (Pexp_construct ({ txt = Longident.Lident x; loc = Location.none }, args)) in
  let encode' v d =
    expr (Pexp_variant (v, Some { pexp_desc = d;
                                  pexp_loc = Location.none;
                                  pexp_attributes = [] })) in
  let const v c = encode' v (Pexp_constant c) in
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
    let nil = constr "[]" in
    let cons a b =
      constr "::" ~args:{ pexp_desc = Pexp_tuple [a; b];
                          pexp_loc = Location.none;
                          pexp_attributes = [] } in
    expr (Pexp_variant ("List", Some (List.fold_right cons (List.map encode xs) nil)))
  | `Expr ("in", x) -> x
  | `Expr (alias, x) ->
    expr (Pexp_variant (atom_constr_of_alias alias, Some x))
  | `Error (loc, msg) -> error loc msg

let sexp_mapper argv =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt = "sexp" }, PStr ss) } ->
        begin match ss with
          | { pstr_desc = Pstr_eval (expr, _) } :: []->
            encode (quote expr)
          | _ -> failwith "The extension payload must contain only one s-expression."
        end
      | other -> default_mapper.expr mapper other }

let () =
  register "ppx_sexp" sexp_mapper
