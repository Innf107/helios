
type server

type request_body = {
  socket : Unix.file_descr;
}

type response_body = Unix.file_descr -> unit

type request = {
  req_method : string;
  path : string;
  headers : (string * string) array;
  body : request_body;
}

type response = {
  headers : (string * string) array;
  status : int;
  status_message : string;
  content_type : string;
  body : response_body;
}

let write_string fd str =
  let rec go written_so_far =
    let written = Schedule.write_substring fd str written_so_far (String.length str - written_so_far) in
    if written_so_far + written < String.length str then
      go (written_so_far + written)
    else
      ()
  in
  go 0

let string_body str = fun fd -> write_string fd str

(* TODO: Write bindings for 'sendfile' instead of streaming read / write *)
let file_body file_path = fun sock_fd ->
  let buffer_capacity = 4096 in
  let buffer = Bytes.create buffer_capacity in

  let file_fd = Unix.openfile file_path [Unix.O_RDONLY] 0 in

  let rec go () = 
    (* Stream the contents from the file to the socket *)
    let num_read = Schedule.read file_fd buffer 0 buffer_capacity in
    if num_read > 0 then begin
      let _ = Schedule.write sock_fd buffer 0 num_read in
      go ()
    end
    else
      ()
  in
  go ()

exception HttpParseError

let bind_parse_error opt cont =
  match opt with
  | Some x -> cont x
  | None -> raise HttpParseError


let parse_headers parser = 
  let rec go headers =
    (* Double newline marks the beggining of the body *)
    match Parser.parse_newline parser with
    | Some _ -> Array.of_list headers
    | None ->
      let (let*) x = bind_parse_error x in
      let key = Parser.parse_while parser (fun c -> c != ':') in
      Parser.advance parser;
      let* value = Parser.parse_line parser in
      go ((key, value) :: headers)
  in
  go [] 

let parse_request conn =
  let parser = Parser.make conn in

  (* We have to eta expand ( let* ) to recover full polymorphism in the face of the value restriction.*)
  let (let*) x = bind_parse_error x in

  let* req_method = Parser.parse_word parser in
  let* _ = Parser.parse_spaces parser in
  let* path = Parser.parse_word parser in
  let* _ = Parser.parse_spaces parser in
  let* _ = Parser.parse_exact parser "HTTP/1.1" in
  let* _ = Parser.parse_newline parser in
  let headers = parse_headers parser in
  { req_method
  ; path
  ; headers
  ; body = { socket = conn }
  }


let send_http : Unix.file_descr -> int -> string -> ?content_type:string -> (Unix.file_descr -> unit) -> unit
 = fun connection status status_msg ?(content_type = "text/html") write_headers_and_body ->
  write_string connection ("HTTP/1.1 " ^ string_of_int status ^ " " ^ status_msg ^ "\n");
  write_string connection ("Server: Helios\n");
  write_string connection ("Content-Type: " ^ content_type ^ "\n");
  write_headers_and_body connection

let send_response conn resp = 
  send_http conn resp.status resp.status_message ~content_type:resp.content_type 
    (fun conn -> 
      Array.iter (fun (k, v) -> write_string conn (k ^ ": " ^ v ^ "\n")) resp.headers;
      write_string conn "\n";
      resp.body conn
      )

let send_error conn err = 
  send_http conn 500 "Internal Server Error"
    (fun conn -> 
      write_string conn "\n";
      write_string conn ("<h1>Internal Server Error: " ^ Printexc.to_string err ^ "</h1>\n")
      )

(* I don't really know what to set this to tbh*)
let backlog = 1024

let rec create_threads count action = 
  match count with
  | _ when count <= 0 -> raise (Failure "create_threads: cannot create 0 or less threads")
  | 1 -> action 1
  | _ -> let _ = Domain.spawn (fun () -> action count) in create_threads (count - 1) action

let run ?(logger = Logger.stdout) ?(capabilities = 8) ~port handler = 
  let sock = Unix.socket ~cloexec:true Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock SO_REUSEPORT true;
  Unix.bind sock (Unix.ADDR_INET (Unix.inet_addr_any, port));
  Unix.listen sock backlog;
  logger.log ("Listening on *:" ^ string_of_int port);
  (* TODO: timeouts *)
  create_threads capabilities (fun id -> Schedule.run begin fun () ->
    let rec go () =
      try begin
      logger.log ("[" ^ string_of_int id ^ "]: Accepting new requests...");
      let connection, _ = Schedule.accept sock in
      logger.log ("[" ^ string_of_int id ^ "]: Connected!");
      let open Parser in
      Schedule.fork begin fun () ->
        begin
        try
          let request = parse_request connection in

          let response = handler request in

          send_response connection response
        with
          | HttpParseError -> begin
            logger.log ("[" ^ string_of_int id ^ "]: HTTP PARSE ERROR");
            try
              send_http connection 400 "Bad Request" (fun conn -> write_string conn "\n<h1>Bad Request</h1>")
            with
              err -> print_endline ("Error when sending '400: Bad Request': " ^ Printexc.to_string err)
            end
          | Parser.ConnectionTerminated ->
            logger.log ("[" ^ string_of_int id ^ "]: CONNECTION TERMINATED")
          | Unix.Unix_error (Unix.ECONNRESET, _, _) ->
            logger.log ("[" ^ string_of_int id ^ "]: CONNECTION RESET")
          | _ as err -> 
            logger.log ("[" ^ string_of_int id ^ "]: EXCEPTION: " ^ Printexc.to_string err);
            try
              send_error connection err
            with
              err -> print_endline ("Error when sending '500: Internal Server Error': " ^ Printexc.to_string err)
          end;
        Unix.close connection;
      end;
      go ()
    end
    with
    | err -> logger.log ("[" ^ string_of_int id ^ "]: CRITICAL EXCEPTION: " ^ Printexc.to_string err); go ()
    in
    go ()
  end)

(* TODO: Actually parse the url (including ? parameters and url decoding) *)
let split_path path = List.filter (fun x -> not (String.equal x "")) (String.split_on_char '/' path)

(* TODO: Generate a more efficient decision tree ahead of time *)
let simple_route ~fallback spec req =
  let path_components = split_path req.path in
  let rec go spec = function
    | [] -> 
      begin match List.find_opt (function ([], _) -> true | _ -> false) spec with
      | Some (_, cont) -> cont req
      | None -> fallback req
      end
    | (path :: paths) ->
      let as_valid_path = function 
        | (spec_path :: spec_paths, cont) when spec_path = path -> Some (spec_paths, cont)
        | _ -> None
      in
      let remaining = List.filter_map as_valid_path spec in
      go remaining paths
  in
  (* Only routes that match the request method are possible *)
  let spec = 
    List.filter_map 
      (fun (meth, path, cont) -> 
        if meth = req.req_method 
        then Some(split_path path, cont)
        else None) spec
  in
  go spec path_components


(*
  "GET", Lit "arg" (Str (Run (fun x -> ...)))   
*)

type _ spec =
  | Lit : string * 'a spec -> 'a spec
  | Str : 'a spec -> (string -> 'a) spec
  | End : (request -> response) spec

type some_spec = Spec : 'a spec * 'a -> some_spec

let rec split_lits : type a. a spec -> a spec = function
  | Lit (str, spec) -> 
    List.fold_right (fun x s -> Lit(x, s)) (split_path str) spec
  | Str spec -> Str (split_lits spec)
  | End -> End

let route ~fallback specs req =
  let path_components = split_path req.path in
  let specs = List.filter_map (fun (meth, spec) -> 
    if meth = req.req_method then 
      match spec with
      | Spec (spec, cont) -> Some (Spec (split_lits spec, cont))
    else 
      None) specs in

  let rec go specs = function
    | [] ->
      let as_empty : some_spec -> (request -> response) option = function
      | Spec (End, cont) -> Some cont
      | _ -> None
      in
      begin match List.find_map as_empty specs with
      | None -> fallback req
      | Some cont -> cont req
      end
    | (path :: path_components) ->
      let apply_path = function 
        | Spec ((Lit (spec_path, spec)), cont) when spec_path = path -> Some (Spec (spec, cont))
        | Spec ((Str spec, cont)) -> Some (Spec (spec, cont path))
        | _ -> None
      in
      let remaining = List.filter_map apply_path specs in
      go remaining path_components
  in
  go specs path_components

let (@@/) x s = Lit (x, s)
let (@/) f x = f x

let str : 'a spec -> (string -> 'a) spec = fun x -> Str x
