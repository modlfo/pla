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

type t = PlaBuffer.t -> unit

(* Builtin templates *)

let unit : t = fun buffer -> ()

let newline : t = fun buffer -> PlaBuffer.newline buffer

let comma : t = fun buffer -> PlaBuffer.append buffer ","

let commaspace : t = fun buffer -> PlaBuffer.append buffer ", "

let semi : t = fun buffer -> PlaBuffer.append buffer ";"

let space : t = fun buffer -> PlaBuffer.append buffer " "

(* Templates of basic types *)

let string (s:string) : t =
   fun buffer -> PlaBuffer.append buffer s

let quoted (s:string) : t =
   fun buffer ->
      PlaBuffer.append buffer "\"";
      PlaBuffer.append buffer s;
      PlaBuffer.append buffer "\""

let int (i:int) : t =
   fun buffer -> PlaBuffer.append buffer (string_of_int i)

let float (f:float) : t =
   fun buffer -> PlaBuffer.append buffer (string_of_float f)

(* Functions to combine templates *)

(** [append t1 t2] makes a new template with the contents of [t1] followed by the contents of [t2] *)
let append (t1:t) (t2:t) : t =
   fun buffer ->
      t1 buffer;
      t2 buffer

let join (elems:t list) : t =
   fun buffer -> List.iter (fun a -> a buffer) elems

let map_join (f:'a -> t) (elems:'a list) : t =
   fun buffer -> List.iter (fun a -> f a buffer) elems

let map_sep (sep:t) (f:'a -> t) (elems:'a list) : t =
   fun buffer ->
      let rec loop = function
         | []   -> ()
         | [h]  -> (f h) buffer
         | h::t ->
            (f h) buffer;
            sep buffer;
            loop t
      in loop elems

let map_sep_all (sep:t) (f:'a -> t) (elems:'a list) : t =
   fun buffer ->
      let rec loop = function
         | []   -> ()
         | h::t ->
            (f h) buffer;
            sep buffer;
            loop t
      in loop elems


let indent (t:t) : t =
   fun buffer ->
      PlaBuffer.indent buffer;
      t buffer;
      PlaBuffer.outdent buffer

let wrap (l:t) (r:t) (t:t) : t =
   fun buffer ->
      l buffer;
      t buffer;
      r buffer

let (++) (t1:t) (t2:t) : t =
   append t1 t2

let print (t:t) : string =
   let buffer = PlaBuffer.newBuffer () in
   t buffer;
   PlaBuffer.contents buffer

let write (file:string) (t:t) : unit =
   let buffer = PlaBuffer.newFile file in
   t buffer;
   PlaBuffer.close buffer

