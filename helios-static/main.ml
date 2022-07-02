
let content_type_of_path path =
  match Filename.extension path with
  | ".css" -> "text/css"
  | ".js" -> "application/javascript"
  | ".html" -> "text/html"
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
  Helios.run ~port:3000 handler

