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

let div_up num denom =
  assert(num >= 0 && denom > 0) ;
  (num + denom - 1) / denom

let alloc_many size_classes nbytes =
  let nclasses = Array.length size_classes in
  let bytes_per_class = div_up nbytes nclasses in
  let els_per_class =
    Array.init nclasses
      (fun sc -> div_up bytes_per_class size_classes.(sc)) in
  let cum_els_per_class =
    els_per_class
    |> Array.fold_left_map (fun prev cur -> prev + cur, prev + cur) 0
    |> snd
    |> Array.mapi (fun i n -> i, n)
  in
  let alloc_by_num i =
    let sc = Array.find_opt (fun (_sc, n) -> n >= i) cum_els_per_class
             |> Option.get
             |> fst
    in
    let sc_size = size_classes.(sc) in
    alloc_single sc_size
  in
  let els_total = Array.fold_left (+) 0 els_per_class in
  Array.init els_total alloc_by_num

let run_gc () =
  Gc.compact ();
  Gc.print_stat stdout;
  Out_channel.flush stdout

let parse_input () =
  let line = read_line () in
  match String.split_on_char ' ' line with
  | mb_str :: sz_str1 :: sz_strs ->
     (try
        let mb = int_of_string mb_str in
        let size_classes = (sz_str1 :: sz_strs)
                           |> List.map int_of_string
                           |> Array.of_list
        in Some (mb, size_classes)
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
