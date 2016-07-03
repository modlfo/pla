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

type dest =
   | File   of out_channel
   | Buffer of Buffer.t


let appendToBuff (d:dest) (s:string) : unit =
   match d with
   | File(c)   -> output_string c s
   | Buffer(b) -> Buffer.add_string b s


(** Text buffer use by Pla *)
type t =
   {
      buffer           : dest;
      mutable indent   : int;
      mutable space    : string;
      mutable indented : bool;
   }

let newBuffer () =
   {
      buffer   = Buffer(Buffer.create 128);
      indent   = 0;
      space    = "";
      indented = false;
   }

let newFile (file:string) =
   {
      buffer   = File(open_out file);
      indent   = 0;
      space    = "";
      indented = false;
   }

let contents (t:t) : string =
   match t.buffer with
   | Buffer(b) -> Buffer.contents b
   | File _ -> ""

let close (t:t) : unit =
   match t.buffer with
   | Buffer _ -> ()
   | File(c) -> close_out c

let newline (t:t) =
   appendToBuff t.buffer "\n";
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
      appendToBuff t.buffer t.space;
      t.indented <- true;
   end;
   appendToBuff t.buffer s


