(** Pantagruel CLI *)

let version = "0.1.0"

let usage =
  "pant [options] [file.pant]\n\n\
   Pantagruel: A specification language checker\n\n\
   If no file is given, reads from stdin.\n\n\
   Options:"

let print_ast = ref false
let print_json = ref false
let print_markdown = ref false
let do_format = ref false
let do_normalize = ref ""
let do_check = ref false
let check_bound = ref 3
let solver_cmd = ref "z3"
let solver_timeout = ref 30.0
let check_steps = ref 5
let module_path = ref "."
let files = ref []

let specs =
  [
    ( "--module-path",
      Arg.Set_string module_path,
      "DIR  Directory to search for imported modules (default: .)" );
    ("--ast", Arg.Set print_ast, "     Print AST (OCaml format) and exit");
    ("--json", Arg.Set print_json, "    Output JSON and exit");
    ( "--markdown",
      Arg.Set print_markdown,
      " Output rich Markdown with Unicode symbols and exit" );
    ( "--format",
      Arg.Set do_format,
      "   Format document with standard style and exit" );
    ( "--normalize",
      Arg.Set_string do_normalize,
      "TERM Output top-down normal form with respect to TERM and exit" );
    ( "--check",
      Arg.Set do_check,
      "   Run SMT verification (contradiction, invariant, precondition checks)"
    );
    ( "--bound",
      Arg.Set_int check_bound,
      "N    Domain element bound for SMT checking (default: 3)" );
    ("--solver", Arg.Set_string solver_cmd, "CMD  Solver command (default: z3)");
    ( "--steps",
      Arg.Set_int check_steps,
      "N    Number of BMC steps from initial state (default: 5, 0 to disable)"
    );
    ( "--timeout",
      Arg.Set_float solver_timeout,
      "SEC  Solver timeout in seconds (default: 30)" );
    ( "--version",
      Arg.Unit
        (fun () ->
          print_endline ("pant " ^ version);
          exit 0),
      "  Print version and exit" );
  ]

let add_file f = files := f :: !files

(** Format module errors for human-readable output *)
let format_module_error = function
  | Pantagruel.Module.ModuleNotFound name ->
      Printf.sprintf "error: Module '%s' not found" name
  | Pantagruel.Module.CyclicImport modules ->
      Printf.sprintf "error: Cyclic import: %s" (String.concat " -> " modules)
  | Pantagruel.Module.ParseError (_, msg) ->
      msg (* Already formatted by Error module *)

(** Run SMT verification on a type-checked document. Returns exit code: 0 =
    pass, 1 = violations found, 2 = solver not found *)
let run_smt_check env doc =
  if not (Pantagruel.Solver.solver_available ~solver:!solver_cmd ()) then begin
    Printf.eprintf "error: Solver '%s' not found in PATH\n" !solver_cmd;
    2
  end
  else begin
    let config =
      Pantagruel.Smt.{ bound = !check_bound; steps = !check_steps }
    in
    let queries = Pantagruel.Smt.generate_queries config env doc in
    if queries = [] then begin
      print_endline "No verification queries generated.";
      0
    end
    else begin
      let results =
        Pantagruel.Solver.verify_all ~solver:!solver_cmd
          ~timeout:!solver_timeout queries
      in
      let all_passed = ref true in
      List.iter
        (fun (r : Pantagruel.Solver.verification_result) ->
          print_endline r.message;
          if not r.passed then all_passed := false)
        results;
      if !all_passed then 0 else 1
    end
  end

(** Returns exit code: 0 = success, 1 = error/failure, 2 = solver not found *)
let check_doc doc =
  if !print_ast then begin
    print_endline (Pantagruel.Ast.show_document doc);
    0
  end
  else if !do_format then begin
    Pantagruel.Pretty.output_formatted doc;
    0
  end
  else begin
    (* Set up module registry *)
    let registry = Pantagruel.Module.scan_module_path !module_path in

    (* Check the document *)
    match Pantagruel.Module.check_with_imports registry doc with
    | Ok env ->
        List.iter
          (fun w -> prerr_endline (Pantagruel.Error.format_type_warning w))
          (Pantagruel.Check.get_warnings ());
        if !do_check then run_smt_check env doc
        else if !do_normalize <> "" then begin
          let root = !do_normalize in
          let all_names =
            List.concat_map
              (fun (ch : Pantagruel.Ast.chapter) ->
                List.map
                  (fun d ->
                    Pantagruel.Normalize.decl_name d.Pantagruel.Ast.value)
                  ch.head)
              doc.chapters
          in
          if not (List.mem root all_names) then begin
            Printf.eprintf "error: Term '%s' not found in document\n" root;
            1
          end
          else begin
            let normalized = Pantagruel.Normalize.normalize doc root in
            Pantagruel.Pretty.output_formatted normalized;
            0
          end
        end
        else begin
          if !print_json then Pantagruel.Json_output.output_json env doc
          else if !print_markdown then Pantagruel.Markdown_output.output env doc;
          0
        end
    | Error e ->
        prerr_endline (format_module_error e);
        1
  end

let check_file path =
  match Pantagruel.Module.parse_file path with
  | Error e ->
      prerr_endline (format_module_error e);
      1
  | Ok doc -> check_doc doc

let check_stdin () =
  match Pantagruel.Module.parse_channel "<stdin>" stdin with
  | Error e ->
      prerr_endline (format_module_error e);
      1
  | Ok doc -> check_doc doc

let () =
  Arg.parse specs add_file usage;

  match !files with
  | [] -> exit (check_stdin ())
  | _ ->
      let codes = List.map check_file (List.rev !files) in
      let worst = List.fold_left max 0 codes in
      exit worst
