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

type buffer =
  { append : string -> unit
  ; content : unit -> string
  ; close : unit -> unit
  ; mutable indent : int
  ; mutable space : string
  ; mutable indented : bool
  }

type t = buffer -> unit

let buffer_new () =
  let b = Buffer.create 128 in
  { append = (fun s -> Buffer.add_string b s)
  ; content = (fun () -> Buffer.contents b)
  ; close = (fun () -> Buffer.clear b)
  ; indent = 0
  ; space = ""
  ; indented = false
  }


let buffer_new_file (file : string) =
  let c = open_out file in
  { append = (fun s -> output_string c s)
  ; content = (fun () -> "")
  ; close = (fun () -> close_out c)
  ; indent = 0
  ; space = ""
  ; indented = false
  }


let buffer_contents t : string = t.content ()

let buffer_close t : unit = t.close ()

let buffer_newline t =
  t.append "\n" ;
  t.indented <- false
  [@@inline always]


let buffer_indent t : unit =
  t.indent <- t.indent + 1 ;
  t.space <- String.make (t.indent * 3) ' ' ;
  buffer_newline t
  [@@inline always]


let buffer_outdent t : unit =
  t.indent <- t.indent - 1 ;
  if t.indent < 0 then
    failwith "Cannot outdent more" ;
  t.space <- String.make (t.indent * 3) ' '
  [@@inline always]


let buffer_append t (s : string) : unit =
  if not t.indented then begin
    t.append t.space ;
    t.indented <- true
  end ;
  t.append s
  [@@inline always]


let buffer_apply (t : t) (buffer : buffer) : unit = t buffer [@@inline always]

let make (f : buffer -> unit) : t = f [@@inline always]

(* Builtin templates *)

let unit : t = fun _ -> ()

let newline : t = fun buffer -> buffer_newline buffer

let comma : t = fun buffer -> buffer_append buffer ","

let commaspace : t = fun buffer -> buffer_append buffer ", "

let semi : t = fun buffer -> buffer_append buffer ";"

let space : t = fun buffer -> buffer_append buffer " "

(* Templates of basic types *)

let string (s : string) : t = fun buffer -> buffer_append buffer s

let string_quoted (s : string) : t =
 fun buffer ->
  buffer_append buffer "\"" ;
  buffer_append buffer s ;
  buffer_append buffer "\""


let int (i : int) : t = fun buffer -> buffer_append buffer (string_of_int i)

let float (f : float) : t = fun buffer -> buffer_append buffer (string_of_float f)

let bool (b : bool) : t =
 fun buffer ->
  let s = if b then "true" else "false" in
  buffer_append buffer s


(* Functions to wrap templates *)

let quote (t : t) : t =
 fun buffer ->
  buffer_append buffer "\"" ;
  t buffer ;
  buffer_append buffer "\""


let parenthesize (t : t) : t =
 fun buffer ->
  buffer_append buffer "(" ;
  t buffer ;
  buffer_append buffer ")"


let indent (t : t) : t =
 fun buffer ->
  buffer_indent buffer ;
  t buffer ;
  buffer_outdent buffer


let wrap (l : t) (r : t) (t : t) : t =
 fun buffer ->
  l buffer ;
  t buffer ;
  r buffer


(* Functions to append templates *)

let append (t1 : t) (t2 : t) : t =
 fun buffer ->
  t1 buffer ;
  t2 buffer


let join (elems : t list) : t = fun buffer -> List.iter (fun a -> a buffer) elems

let join_sep (sep : t) (elems : 'a list) : t =
 fun buffer ->
  let rec loop = function
    | [] -> ()
    | [ h ] -> h buffer
    | h :: t ->
        h buffer ;
        sep buffer ;
        loop t
  in
  loop elems


let join_sep_all (sep : t) (elems : 'a list) : t =
 fun buffer ->
  List.iter
    (fun h ->
      h buffer ;
      sep buffer)
    elems


let map_join (f : 'a -> t) (elems : 'a list) : t = fun buffer -> List.iter (fun a -> f a buffer) elems

let map_sep (sep : t) (f : 'a -> t) (elems : 'a list) : t =
 fun buffer ->
  let rec loop = function
    | [] -> ()
    | [ h ] -> (f h) buffer
    | h :: t ->
        (f h) buffer ;
        sep buffer ;
        loop t
  in
  loop elems


let map_sep_all (sep : t) (f : 'a -> t) (elems : 'a list) : t =
 fun buffer ->
  List.iter
    (fun h ->
      (f h) buffer ;
      sep buffer)
    elems


let ( ++ ) (t1 : t) (t2 : t) : t = append t1 t2

let print (t : t) : string =
  let buffer = buffer_new () in
  t buffer ;
  buffer_contents buffer


let write (file : string) (t : t) : unit =
  let buffer = buffer_new_file file in
  t buffer ;
  buffer_close buffer
