(* The MIT License (MIT)
   Copyright (c) 2016 Leonardo Laguna Ruiz

   Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
   associated documentation files (the "Software"), to deal in the Software without restriction, including
   without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to
   the following conditions:

   The above copyright notice and this permission notice shall be included in all copies or substantial
   portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
   LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
   IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
   WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
   SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

open Migrate_parsetree
open Ast_406
module Pla_tokens = Pla__.Pla_tokens
module Pla_lex = Pla__.Pla_lex

let ocaml_version = Versions.ocaml_406

let migrate = Versions.migrate Versions.ocaml_current ocaml_version

module Pla = struct
  open Ast_mapper
  open Asttypes
  open Parsetree
  open Ast_helper

  let readFile _ path =
    if Sys.file_exists path then (
      let file = open_in path in
      let buffer = Buffer.create 16 in
      try
        while true do
          let c = input_char file in
          Buffer.add_char buffer c
        done ;
        ""
      with
      | End_of_file ->
          close_in file ;
          Buffer.contents buffer )
    else
      let msg = Printf.sprintf "Cannot open the file '%s' from the current directory '%s'" path (Sys.getcwd ()) in
      prerr_endline msg ;
      exit 1


  let buffer_id = "__buffer__"

  let makeLident (str : string) : Longident.t Location.loc = Longident.parse str |> Location.mknoloc

  let buffer = Exp.ident (makeLident buffer_id)

  let newline = Exp.ident (makeLident "Pla.buffer_newline")

  let indent = Exp.ident (makeLident "Pla.buffer_indent")

  let outdent = Exp.ident (makeLident "Pla.buffer_outdent")

  let append = Exp.ident (makeLident "Pla.buffer_append")

  let papply = Exp.ident (makeLident "Pla.buffer_apply")

  let pint = Exp.ident (makeLident "Pla.int")

  let pfloat = Exp.ident (makeLident "Pla.float")

  let pstring = Exp.ident (makeLident "Pla.string")

  let unit = Exp.construct (makeLident "()") None

  let offsetPosition (displacement : int) (pos1 : Lexing.position) (pos2 : Lexing.position) : Lexing.position =
    Lexing.
      { pos1 with
        pos_lnum = pos1.pos_lnum + pos2.pos_lnum - 1
      ; pos_bol = pos1.pos_bol + pos2.pos_bol
      ; pos_cnum = pos1.pos_cnum + pos2.pos_cnum + displacement
      }


  let offsetLocation (displacement : int) (loc1 : Location.t) (loc2 : Location.t) : Location.t =
    Location.
      { loc1 with
        loc_start = offsetPosition displacement loc1.loc_start loc2.loc_start
      ; loc_end = offsetPosition displacement loc1.loc_start loc2.loc_end
      }


  let mkVar (var_loc : Location.t) (v : string) = Location.mkloc (Longident.parse v) var_loc

  let template_type (t : Pla_tokens.vartype) =
    match t with
    | Pla_tokens.Int -> Typ.constr (makeLident "int") []
    | Pla_tokens.Float -> Typ.constr (makeLident "float") []
    | Pla_tokens.String -> Typ.constr (makeLident "string") []
    | Pla_tokens.Template -> Typ.constr (makeLident "Pla.t") []


  let no_label = Nolabel

  let constString s = Const.string s

  let makeExp (loc : Location.t) (displacement : int) (s : Pla_tokens.s) : expression =
    match s with
    | Pla_tokens.N -> Exp.apply ~loc newline [ no_label, buffer ]
    | Pla_tokens.I -> Exp.apply ~loc indent [ no_label, buffer ]
    | Pla_tokens.O -> Exp.apply ~loc outdent [ no_label, buffer ]
    | Pla_tokens.T txt -> Exp.apply ~loc append [ no_label, buffer; no_label, Exp.constant (constString txt) ]
    | Pla_tokens.V (v, vartype, loc_ref) ->
        let var_loc = offsetLocation displacement loc loc_ref in
        let v_exp = Exp.constraint_ ~loc:var_loc (Exp.ident ~loc:var_loc (mkVar var_loc v)) (template_type vartype) in
        ( match vartype with
        | Pla_tokens.Template -> Exp.apply ~loc papply [ no_label, v_exp; no_label, buffer ]
        | Pla_tokens.Int ->
            Exp.apply ~loc papply [ no_label, Exp.apply ~loc pint [ no_label, v_exp ]; no_label, buffer ]
        | Pla_tokens.Float ->
            Exp.apply ~loc papply [ no_label, Exp.apply ~loc pfloat [ no_label, v_exp ]; no_label, buffer ]
        | Pla_tokens.String ->
            Exp.apply ~loc papply [ no_label, Exp.apply ~loc pstring [ no_label, v_exp ]; no_label, buffer ] )


  let makeExpSeq (loc : Location.t) (displacement : int) (sl : Pla_tokens.s list) : expression =
    List.fold_right (fun a s -> Exp.sequence (makeExp loc displacement a) s) sl unit


  let makeTemplateExp (loc : Location.t) (displacement : int) (sl : Pla_tokens.s list) : expression =
    let pat = Pat.var (Location.mknoloc buffer_id) in
    let fun_ = Exp.fun_ ~loc no_label None pat (makeExpSeq loc displacement sl) in
    Exp.apply ~loc (Exp.ident (makeLident "Pla.make")) [ no_label, fun_ ]


  let mapper _config _cookies =
    { default_mapper with
      expr =
        (fun mapper expr ->
          match expr with
          | { pexp_desc = Pexp_constant (Pconst_string (text, Some "pla")); pexp_loc = loc; _ } ->
              let tokens = Pla_lex.tokenize text in
              let displacement = 5 in
              let pla_exp = makeTemplateExp loc displacement tokens in
              pla_exp
          | { pexp_desc =
                Pexp_extension
                  ( { txt = "pla"; _ }
                  , PStr
                      [ { pstr_desc =
                            Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string (text, _)); pexp_loc = loc; _ }, _)
                        ; _
                        }
                      ] )
            ; _
            } ->
              let tokens = Pla_lex.tokenize text in
              let displacement = 2 in
              let pla_exp = makeTemplateExp loc displacement tokens in
              pla_exp
          (* Files as templates *)
          | { pexp_desc = Pexp_constant (Pconst_string (path, Some "pla_file")); pexp_loc = loc; _ } ->
              let text = readFile loc path in
              let tokens = Pla_lex.tokenize text in
              let displacement = 5 in
              let pla_exp = makeTemplateExp loc displacement tokens in
              pla_exp
          | { pexp_desc =
                Pexp_extension
                  ( { txt = "pla_file"; _ }
                  , PStr
                      [ { pstr_desc =
                            Pstr_eval ({ pexp_desc = Pexp_constant (Pconst_string (path, _)); pexp_loc = loc; _ }, _)
                        ; _
                        }
                      ] )
            ; _
            } ->
              let text = readFile loc path in
              let tokens = Pla_lex.tokenize text in
              let displacement = 2 in
              let pla_exp = makeTemplateExp loc displacement tokens in
              pla_exp
          | _ -> default_mapper.expr mapper expr)
    }
end

let args_spec = []

let reset_args () = ()

let () = Driver.register ~name:"pla" ~args:args_spec ~reset_args ocaml_version Pla.mapper
