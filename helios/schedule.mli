type thunk = unit -> unit

val fork : thunk -> unit
val yield : unit -> unit

val run : thunk -> unit

(** Asynchronously perform an operation over a file descriptor.
    The file descriptor is set to non-blocking mode.
 *)
val as_non_blocking : (Unix.file_descr -> 'b) -> Unix.file_descr -> 'b

(** Marks a file descriptor as non_blocking and asynchronously reads from it.
    Same as `fun bytes i j -> as_non_blocking (fun fd -> Unix.read fd bytes i j) fd`
*)
val read : Unix.file_descr -> bytes -> int -> int -> int

(** Marks a file descriptor as non_blocking and asynchronously write it.
    Same as `fun bytes i j -> as_non_blocking (fun fd -> Unix.write fd bytes i j) fd`
*)
val write : Unix.file_descr -> bytes -> int -> int -> int

(** Same as `write`, but continues writing if it cannot write everything immediately
*)
val write_all : Unix.file_descr -> bytes -> int -> int -> unit

val write_substring : Unix.file_descr -> string -> int -> int -> int

val accept : ?cloexec:bool -> Unix.file_descr -> Unix.file_descr * Unix.sockaddr
