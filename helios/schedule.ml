open Effect
open Effect.Deep

type thunk = unit -> unit

type _ Effect.t += Fork : thunk -> unit Effect.t
type _ Effect.t += Yield : unit Effect.t

let fork thunk = perform (Fork thunk)
let yield ()   = perform Yield

let run main =
  let task_queue : thunk Queue.t = Queue.create () in
  let enqueue_task task = Queue.add task task_queue in
  let dequeue_task () =
    if Queue.is_empty task_queue then
      (fun () -> ())
    else
      Queue.pop task_queue
  in
  let rec handle f = match_with f () {
      exnc = begin fun exn -> raise exn end;
      effc = begin fun (type a) (eff : a Effect.t) -> 
        match eff with
        | Fork thunk -> Some begin fun (k : (a, unit) continuation) -> 
          enqueue_task (fun () -> continue k ());
          handle thunk
        end
        | Yield -> Some begin fun (k : (a, unit) continuation) ->
          enqueue_task (fun () -> continue k ());
          handle (dequeue_task ())
        end
        | _ -> None
        end;
      retc = fun x -> dequeue_task () ();
    }
  in
  handle main


let as_non_blocking : (Unix.file_descr -> 'b) -> Unix.file_descr -> 'b =
  fun f fd ->
    Unix.set_nonblock fd;
    let rec try_f () =
      match f fd with
      | exception Unix.Unix_error ((Unix.EWOULDBLOCK | Unix.EAGAIN), _, _) -> 
        yield ();
        try_f ()
      | result ->
        yield ();
        result
    in
    try_f ()

let read fd bytes i j = as_non_blocking (fun fd -> Unix.read fd bytes i j) fd

let accept ?cloexec fd = as_non_blocking (fun fd -> Unix.accept ?cloexec fd) fd
    