(* @archlint.module shell
   @archlint.domain pantagruel.solver-output *)

(** Solver interface: invoke an SMT solver and parse results *)

include Solver_terms
include Solver_output
include Solver_interpretation

let default_solver = "z3"
let default_args = [ "-smt2"; "-in" ]
let default_timeout = 30.0

(** Check if a solver binary is available *)
let solver_available ?(solver = default_solver) () =
  let is_executable path =
    try
      Unix.access path [ Unix.X_OK ];
      true
    with Unix.Unix_error _ -> false
  in
  if String.contains solver '/' then is_executable solver
  else
    match Sys.getenv_opt "PATH" with
    | None -> false
    | Some path ->
        String.split_on_char ':' path
        |> List.exists (fun dir -> is_executable (Filename.concat dir solver))

(** Read all data from a file descriptor with a total deadline. Returns [None]
    on timeout, [Some output] on completion. *)
let read_with_timeout fd timeout =
  let buf = Buffer.create 256 in
  let chunk = Bytes.create 4096 in
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    let remaining = deadline -. Unix.gettimeofday () in
    if remaining <= 0.0 then None
    else
      match Unix.select [ fd ] [] [] remaining with
      | [], _, _ -> None
      | _ ->
          let n = Unix.read fd chunk 0 (Bytes.length chunk) in
          if n = 0 then Some (Buffer.contents buf |> String.trim)
          else (
            Buffer.add_subbytes buf chunk 0 n;
            loop ())
  in
  loop ()

(** Run a solver on an SMT-LIB2 string *)
let run_solver ?(solver = default_solver) ?(args = default_args)
    ?(timeout = default_timeout) smt2 =
  let argv = Array.of_list (solver :: args) in
  let stdin_read, stdin_write = Unix.pipe ~cloexec:true () in
  let stdout_read, stdout_write = Unix.pipe ~cloexec:true () in
  let pid =
    Unix.create_process solver argv stdin_read stdout_write Unix.stderr
  in
  Unix.close stdin_read;
  Unix.close stdout_write;
  (* Write SMT input and close stdin so the solver sees EOF *)
  let input_bytes = Bytes.of_string smt2 in
  let len = Bytes.length input_bytes in
  let rec write_all off =
    if off < len then
      let n = Unix.write stdin_write input_bytes off (len - off) in
      write_all (off + n)
  in
  (try write_all 0 with Unix.Unix_error _ -> ());
  Unix.close stdin_write;
  (* Read output with timeout *)
  let result =
    match read_with_timeout stdout_read timeout with
    | None ->
        (* Timeout: kill the solver process *)
        (try Unix.kill pid Sys.sigterm with Unix.Unix_error _ -> ());
        ignore (Unix.waitpid [] pid);
        Unix.close stdout_read;
        SolverError (Printf.sprintf "Solver timed out after %.0fs" timeout)
    | Some output -> (
        Unix.close stdout_read;
        let _pid, status = Unix.waitpid [] pid in
        match status with
        | Unix.WEXITED 0 -> parse_solver_output output
        | Unix.WEXITED code ->
            if String.length output > 0 then parse_solver_output output
            else SolverError (Printf.sprintf "Solver exited with code %d" code)
        | Unix.WSIGNALED sig_num ->
            SolverError (Printf.sprintf "Solver killed by signal %d" sig_num)
        | Unix.WSTOPPED sig_num ->
            SolverError (Printf.sprintf "Solver stopped by signal %d" sig_num))
  in
  result

(** Run all queries and return results *)
let verify_all ?(solver = default_solver) ?(timeout = default_timeout) queries =
  let results =
    List.map
      (fun query ->
        let result = run_solver ~solver ~timeout query.Smt.smt2 in
        interpret_result query result)
      queries
  in
  correlate_results results
