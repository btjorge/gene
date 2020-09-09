let prefix = "../progs/"

let progs = [ "max02"; "max03"; "max04"; "max05"; "max06"; "max3"; "max_imp";
              "max_imp2" ]

let symbinterp file =
  let command = "../../bin/gene.exe --generate-tests " ^ prefix ^ file in
  print_endline command;
  if Sys.command command <> 0
  then failwith "error";
  ()

let () =
  List.iter symbinterp progs
