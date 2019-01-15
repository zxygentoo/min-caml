let () =
  let files = ref [] in
  Arg.parse
    [
      (
        "-inline",
        Arg.Int(fun n -> Pipeline.inline_threshold := n),
        "maximum size of functions to inline"
      );
      (
        "-iter",
        Arg.Int(fun m -> Pipeline.max_opt_iter := m),
        "maximum number of optimization iterations"
      )
    ]
    (fun file -> files := !files @ [file])
    (
      "Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
      Printf.sprintf
        "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..."
        Sys.argv.(0)
    );

  List.iter (fun file -> ignore (Pipeline.compile_file file)) !files
