
let on_404 req : Helios.response = {
  headers = [||];
  status = 404;
  status_message = "Not found";
  content_type = "text/html";
  body = Helios.file_body "static/404.html"
}

let () = 
  Helios.run ~port:5000 (Helios.route ~fallback:on_404
  [ ("GET", Spec (End, fun req -> 
    { headers = [||]
    ; status = 200
    ; status_message = "OK"
    ; content_type = "text/html"
    ; body = Helios.string_body "<h1>Hello, World!</h1>"
    }))
  ; ("GET", Spec (Lit ("test", End), fun req -> 
    { headers = [||]    
    ; status = 200
    ; status_message = "Awesome!"
    ; content_type = "text/html"
    ; body = Helios.string_body "<h1>Teeeest!</h1>"
    }))
  ; ("GET", Spec (Lit ("arg", Str End), fun arg req -> 
    { headers = [||]
    ; status = 200
    ; status_message = "OK"
    ; content_type = "text/html"
    ; body = Helios.string_body ("<h1>Arg: " ^ arg ^ "</h1>")
    }))
  ])
