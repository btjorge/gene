let prefix = "../progs/"

let progs = [ "max03_no_func"; "max06_no_func"; "max02"; "max03";
              "max04"; "max05"; "max06"; "abs"; "max3"; "max_imp"; "max_imp2" ]

let symbinterp file =
  let command =
    "../../bin/gene.exe --print-generic --print-formula " ^ prefix ^ file in
  print_endline command;
  if Sys.command command <> 0
  then failwith "error";
  ()

let () =
  List.iter symbinterp progs
