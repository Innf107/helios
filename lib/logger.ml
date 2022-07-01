
type logger = {
  log : string -> unit
}

let stdout = {
  log = print_endline;
}

let none = {
  log = fun _ -> ()
}