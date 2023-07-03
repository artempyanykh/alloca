module F = Format

let size_classes =
  [| 32; 64; 128; 256; 512; 1024; 4096; 1024 * 1024; 4 * 1024 * 1024 |]

let alloc_single nbytes =
  let nints = nbytes / Sys.word_size * 8 in
  Array.init nints Fun.id

let alloc_many nbytes =
  let class_counts = Array.make (Array.length size_classes) 0 in
  let rec loop acc remaining =
    if remaining > 0 then (
      let n_size_class = Random.int (Array.length size_classes) in
      let size_class = size_classes.(n_size_class) in
      class_counts.(n_size_class) <- class_counts.(n_size_class) + size_class;
      let el = alloc_single size_class in
      loop (el :: acc) (remaining - size_class))
    else acc
  in
  let data = loop [] nbytes in
  let total_allocated = Array.fold_left ( + ) 0 class_counts in
  let pp_counts fmt xs =
    F.pp_print_seq
      ~pp_sep:(fun fmt () -> F.fprintf fmt ", ")
      F.pp_print_int fmt (Array.to_seq xs)
  in
  F.printf "Allocated %d bytes, by size class: {%a}@." total_allocated pp_counts
    class_counts;
  data

let run_gc () =
  Gc.full_major ();
  Gc.compact ();
  Gc.print_stat stdout;
  Out_channel.flush stdout

let main_loop () =
  let data = ref @@ [] in
  let rec loop () =
    F.printf "Input the # of megabytes > @?";
    match read_int_opt () with
    | Some num -> (
        data := alloc_many (num * 1024 * 1024);
        F.printf "Press ENTER to release the data@?";
        match read_line () with
        | _ ->
            data := [];
            run_gc ();
            loop ()
        | exception End_of_file -> ())
    | None ->
        run_gc ();
        loop ()
  in
  loop ()

let () = main_loop ()
