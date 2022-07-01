
type server

type request_body

type request = {
    req_method : string;
    path : string;
    headers : (string * string) array;
    body: request_body
}

type response_body 

type response = {
    headers : (string * string) array;
    status : int;
    status_message : string;
    content_type : string;
    body : response_body;
}
  
val string_body : string -> response_body

val file_body : string -> response_body

val run : ?logger:Logger.logger -> ?capabilities:int -> port:int -> (request -> response) -> unit

val route : fallback:(request -> response) -> (string * string * (request -> response)) list -> request -> response
