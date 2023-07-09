module F = Format

let div_up num denom =
  assert (num >= 0 && denom > 0);
  (num + denom - 1) / denom

let bytes_to_words bytes = div_up (bytes * 8) Sys.word_size

type alloc_mix = { size_classes : int array; num_of_class : int array }

(** Calculate the number of objects of each size class such that [nbytes] is uniformly distributed between the classes.  *)
let calc_mix size_classes nbytes =
  let nclasses = Array.length size_classes in
  assert (nclasses >= 1 && nbytes > 0);
  let bytes_per_class = div_up nbytes nclasses in
  let num_of_class =
    Array.init nclasses (fun classnum ->
        div_up bytes_per_class size_classes.(classnum))
  in
  { size_classes; num_of_class }

let num_classes { size_classes; _ } = Array.length size_classes
let num_elements { num_of_class; _ } = Array.fold_left ( + ) 0 num_of_class

let pp_mix fmt { size_classes; num_of_class } =
  let pp_sep fmt () = F.fprintf fmt ", " in
  let pp_el fmt (i, size, num) =
    F.fprintf fmt "#%d (%db, %dwo) -> %d" i size (bytes_to_words size) num
  in
  let seq =
    Seq.zip (Array.to_seq size_classes) (Array.to_seq num_of_class)
    |> Seq.mapi (fun i (size, num) -> (i, size, num))
  in
  F.pp_print_seq ~pp_sep pp_el fmt seq

let alloc_single nbytes = Bytes.make nbytes (Char.chr 0)

let alloc_many size_classes nbytes =
  let alloc_mix = calc_mix size_classes nbytes in
  let alloc_next () =
    let rem_by_class, cur_class = (Array.copy alloc_mix.num_of_class, ref 0) in
    fun _ ->
      if rem_by_class.(!cur_class) <= 0 then incr cur_class;
      if !cur_class >= num_classes alloc_mix then
        failwith "Unexpected num of allocs";
      rem_by_class.(!cur_class) <- rem_by_class.(!cur_class) - 1;
      alloc_single alloc_mix.size_classes.(!cur_class)
  in
  let output = Array.init (num_elements alloc_mix) (alloc_next ()) in
  F.printf "Allocated by class {%a}@." pp_mix alloc_mix;
  output

let run_gc () =
  Gc.full_major ();
  Gc.compact ();
  Gc.print_stat stdout;
  Out_channel.flush stdout

let parse_input () =
  let line = read_line () in
  match String.split_on_char ' ' line with
  | mb_str :: sz_str1 :: sz_strs -> (
      try
        let mb = int_of_string mb_str in
        let size_classes =
          sz_str1 :: sz_strs |> List.map int_of_string |> Array.of_list
        in
        Some (mb, size_classes)
      with _ -> None)
  | _ -> None

let main_loop () =
  F.printf "Compiled with OCaml %s@." Sys.ocaml_version;
  let empty_array = Array.make 0 (Bytes.make 0 (Char.chr 0)) in
  let data = ref @@ empty_array in
  let rec loop () =
    F.printf
      "Input the # of megabytes and size class in bytes (space separated) > @?";
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
        run_gc ();
        loop ()
  in
  loop ()

let () = main_loop ()
