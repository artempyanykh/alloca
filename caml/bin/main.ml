module F = Format

let size_classes =
  [| 32; 64; 128; 256; 512; 1024; 4096; 1024 * 1024; 4 * 1024 * 1024 |]

let alloca_single nbytes =
  let nints = nbytes / Sys.int_size * 8 in
  Array.init nints Fun.id

let alloca nbytes =
  let data = Queue.create () in
  let rec loop remaining =
    if remaining > 0 then (
      let sc = Random.int (Array.length size_classes) in
      let el = alloca_single size_classes.(sc) in
      Queue.add el data;
      loop (remaining - size_classes.(sc)))
    else ()
  in
  loop nbytes;
  data

let run_gc () =
  Gc.full_major ();
  Gc.compact () ;
  Gc.print_stat stdout ;
  Out_channel.flush stdout

let main_loop () =
  let data = ref @@ Queue.create () in
  let rec loop () =
    F.printf "Input the # of megabytes > @.";
    match read_int_opt () with
    | Some num -> (
        data := alloca (num * 1024 * 1024);
        F.printf "Press ENTER to release the data@.";
        match read_line () with
        | _ ->
            data := Queue.create ();
            run_gc () ;
            loop ()
        | exception End_of_file -> ())
    | None ->
       run_gc () ;
       loop ()
  in
  loop ()

let () = main_loop ()
