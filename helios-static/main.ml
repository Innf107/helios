
let content_type_of_path path =
  match Filename.extension path with
  (* appliction *)
  | ".js" -> "application/javascript"
  | ".json" -> "application/json"
  | ".xml" -> "application/xml"
  | ".ogg" -> "application/ogg"
  | ".pdf" -> "application/pdf"
  | ".zip" -> "application/zip"
  (* text *)
  | ".css" -> "text/css"
  | ".html" -> "text/html"
  (* image *)
  | ".jpg" -> "image/jpeg"
  | ".png" -> "image/png"
  | ".gif" -> "image/gif"
  | _ -> "text/plain"

type file_type = File | Dir | Nonexistant

let dir_preview path =
  let files = Sys.readdir path in
  "<html><body><h1>" ^ path ^ "</h1><ul>"
  ^ String.concat "\n" (List.map (fun file -> "<li><a href='" ^ file ^ "'>" ^ file ^ "</a></li>") (Array.to_list files))
  ^ "</ul></body></html>"

let rec handler : Helios.request -> Helios.response = 
  fun req ->     
  let path = 
    if req.path = "/" then
      "index.html"
    else
      (* Convert the absolute path (starting with a '/') to a local path
         We could also do some validation here to make sure paths don't escape to parent directories *)
      "." ^ req.path
  in
  let file_type = 
    try
      let stat = Unix.stat path in
      if stat.st_kind = S_REG then
        File
      else
        Dir
    with
      | Unix.Unix_error (ENOENT, _, _) -> Nonexistant
  in
  print_endline ("Receving request for file '" ^ path ^ "'");
  match file_type with
  | File -> 
    { headers = [||]
    ; status = 200
    ; status_message = "OK"
    ; content_type = content_type_of_path path
    ; body = Helios.file_body path
    }
  | Dir ->
    { headers = [||]
    ; status = 200
    ; status_message = "OK"
    ; content_type = "text/html"
    ; body = Helios.string_body (dir_preview path)
    }
  | Nonexistant ->
    { headers = [||]
    ; status = 404
    ; status_message = "Not found :("
    ; content_type = "text/html"
    ; body = Helios.string_body "<h1>404 - Not found :(</h1>"
    }


let () =
  let usage_message = "helios-static [OPTIONS]" in
  let port = ref 3000 in
  let no_backtrace = ref false in
  let capabilities = ref (-1) in
  
  let speclist = [
      ("--port", Arg.Set_int port, "Port to run the server on. Default: 3000");
      ("-p", Arg.Set_int port, "Alias for --port");
      ("--capabilities", Arg.Set_int capabilities, "Number");
      ("--no-backtrace", Arg.Set no_backtrace, "Disable exception backtraces");
    ] in

  Arg.parse speclist (fun _ -> ()) usage_message;

  if not !no_backtrace then
    Printexc.record_backtrace true;

  Helios.run ~port:!port ?capabilities:(if !capabilities == (-1) then None else Some(!capabilities)) handler

