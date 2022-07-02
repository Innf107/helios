
let on_404 req : Helios.response = {
  headers = [||];
  status = 404;
  status_message = "Not found";
  content_type = "text/html";
  body = Helios.file_body "example/assets/404.html"
}

let () = 
  let open Helios in
  Helios.run ~port:5000 (Helios.route ~fallback:on_404
  [ ("GET", Spec (End, fun req -> 
    { headers = [||]
    ; status = 200
    ; status_message = "OK"
    ; content_type = "text/html"
    ; body = Helios.string_body "<h1>Hello, World!</h1>"
    }))
  ; ("GET", Spec ("test" @/ End, fun req -> 
    { headers = [||]    
    ; status = 200
    ; status_message = "Awesome!"
    ; content_type = "text/html"
    ; body = Helios.string_body "<h1>Teeeest!</h1>"
    }))
  ; ("GET", Spec ("arg" @/ str @@/ End, fun arg req -> 
    { headers = [||]
    ; status = 200
    ; status_message = "OK"
    ; content_type = "text/html"
    ; body = Helios.string_body ("<h1>Arg: " ^ arg ^ "</h1>")
    }))
  ; ("GET", Spec ("multi/path/with/arg" @/ str @@/ str @@/ End, fun arg arg2 req ->     { headers = [||]
    ; status = 200
    ; status_message = "OK"
    ; content_type = "text/html"
    ; body = Helios.string_body ("<h1>multi path with arg: " ^ arg ^ ", " ^ arg2 ^ "</h1>")
    }))
  ])
