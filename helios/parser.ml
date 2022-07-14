
(* 
  I would prefer it if the parser could be immutable,
  but since the underlying file descriptor gets mutated, this does
  not seem to be possible unfortunately. 
*)
type parser = {
  socket : Unix.file_descr;
  mutable buffer : bytes;
  mutable buffer_ix : int;
  mutable buffer_size : int
}

exception ConnectionTerminated

(* 4096 because... uh... why not? *)
let buffer_capacity = 4096

let make (socket : Unix.file_descr): parser = {
    socket;
    buffer = Bytes.create buffer_capacity;
    buffer_ix = 0;
    buffer_size = 0;
  }

let rec peek (parser : parser): char =
  if parser.buffer_ix < parser.buffer_size then
    Bytes.get parser.buffer parser.buffer_ix
  else begin
    advance parser;
    peek parser
  end

and advance (parser : parser): unit =
  (* Can we just increment the index in the current buffer or do we have to refill? *)
  if parser.buffer_ix <= parser.buffer_size then begin
    parser.buffer_ix <- parser.buffer_ix + 1
  end else begin
    (* We have to refill the buffer *)
    parser.buffer_ix <- 0;
    parser.buffer_size <- Schedule.read parser.socket parser.buffer 0 buffer_capacity;
    if parser.buffer_size == 0 then
      raise ConnectionTerminated
end

let parse_while (parser : parser) (accept : char -> bool): string = 
  let rec go chars =
    let char = peek parser in
    if accept char then begin
      advance parser;
      go (char :: chars)
    end
    else
      (* TODO: Reversing is kind of inneficient here *)
      String.of_seq (List.to_seq (List.rev chars))
  in
  go []

let parse_satisfying (parser : parser) (pred : char -> bool): unit option =
  let char = peek parser in
  if pred char then begin
    advance parser;
    Some ()
  end
  else
    None

let parse_exact (parser : parser) (message : string): unit option =
  let rec go ix =
    if ix >= String.length message then
      Some ()
    else if peek parser = message.[ix] then begin
      advance parser;
      go (ix + 1)
    end
    else
      None
  in
  go 0

let parse_word (parser : parser): string option =
  match parse_while parser (fun c -> c != ' ' && c != '\n') with
  | "" -> None
  | str -> Some str

let parse_newline (parser : parser) : unit option =
  match peek parser with
  | '\r' ->
    advance parser;
    begin match peek parser with
    | '\n' ->
      advance parser;
      Some ()
    | _ -> None
    end
  | '\n' -> 
    advance parser; 
    Some ()
  | _ -> None

let parse_spaces (parser : parser) : unit option =
  match parse_while parser (fun c -> c == ' ') with
  | "" -> None
  | _ -> Some ()

let parse_line (parser : parser) : string option = 
  let str = parse_while parser (fun c -> c != '\r' && c != '\n') in
  Option.map (fun _ -> str) (parse_newline parser)
