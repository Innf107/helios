
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

val simple_route : fallback:(request -> response) -> (string * string * (request -> response)) list -> request -> response


type _ spec =
  | Lit : string * 'a spec -> 'a spec
  | Str : 'a spec -> (string -> 'a) spec
  | End : (request -> response) spec

type some_spec = Spec : 'a spec * 'a -> some_spec

val route : fallback:(request -> response) -> (string * some_spec) list -> request -> response

val (@@/) : string -> 'a spec -> 'a spec
val (@/) : ('a spec -> 'b spec) -> 'a spec -> 'b spec

val str : 'a spec -> (string -> 'a) spec

