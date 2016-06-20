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

(** Text buffer use by Pla *)
type t =
   {
      buffer           : Buffer.t;
      mutable indent   : int;
      mutable space    : string;
      mutable indented : bool;
   }

let empty () =
   {
      buffer   = Buffer.create 128;
      indent   = 0;
      space    = "";
      indented = false;
   }

let contents (t:t) : string =
   Buffer.contents t.buffer

let newline (t:t) =
   Buffer.add_string t.buffer "\n";
   t.indented <- false

let indent (t:t) : unit =
   t.indent <- t.indent +1;
   t.space  <- String.make (t.indent * 3) ' ';
   newline t

let outdent (t:t) : unit =
   t.indent <- t.indent - 1;
   if t.indent < 0 then
      failwith "Cannot outdent more";
   t.space <- String.make (t.indent * 3) ' '

let append (t:t) (s:string) : unit =
   if not t.indented then begin
      Buffer.add_string t.buffer t.space;
      t.indented <- true;
   end;
   Buffer.add_string t.buffer s


