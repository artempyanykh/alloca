module F = Format

let size_classes =
  [| 31; 63; 127; 255; 511; 1023; 2047 |]

let size_classes =
  [| 4*1024*1024 |]

(* jemalloc: threshold for mem release (<= retains, > releases) *)
let size_classes = [| 2047 |]

(* macos sys malloc: threshold for mem release (<= retains, > releases) *)
let size_classes = [| 1015 |]

let alloc_single nbytes =
  Bytes.make nbytes (Char.chr 0)

let alloc_many size_class nbytes =
  let nitems = nbytes / size_class + 1 in
  Array.init nitems (fun _ -> alloc_single size_class)

let run_gc () =
  Gc.full_major ();
  Gc.compact ();
  Gc.print_stat stdout;
  Out_channel.flush stdout

let parse_input () =
  let line = read_line () in
  match String.split_on_char ' ' line with
  | [mb_str; sz_str] ->
     (try
        Some (int_of_string mb_str, int_of_string sz_str)
      with _ -> None)
  | _ -> None 
    

let main_loop () =
  F.printf "Compiled with OCaml %s@." Sys.ocaml_version;
  let empty_array = Array.make 0 (Bytes.make 0 (Char.chr 0)) in
  let data = ref @@ empty_array in
  let rec loop () =
    F.printf "Input the # of megabytes and size class in bytes > @?";
    match parse_input () with
    | Some (mb, size_class) -> (
        data := alloc_many size_class (mb * 1024 * 1024);
        F.printf "Press ENTER to release the data@?";
        match read_line () with
        | _ ->
            data := empty_array;
            run_gc ();
            loop ()
        | exception End_of_file -> ())
    | None ->
        loop ()
  in
  loop ()

let () = main_loop ()
