open Import
open Fiber.O

type command_result =
  { stdout : string
  ; stderr : string
  ; status : Unix.process_status
  }

let run_command prog stdin_value args : command_result Fiber.t =
  Fiber.of_thunk (fun () ->
      let stdin_i, stdin_o = Unix.pipe ~cloexec:true () in
      let stdout_i, stdout_o = Unix.pipe ~cloexec:true () in
      let stderr_i, stderr_o = Unix.pipe ~cloexec:true () in
      let pid =
        let argv = prog :: args in
        Spawn.spawn ~prog ~argv ~stdin:stdin_i ~stdout:stdout_o ~stderr:stderr_o
          ()
        |> Stdune.Pid.of_int
      in
      Unix.close stdin_i;
      Unix.close stdout_o;
      Unix.close stderr_o;
      let blockity =
        if Sys.win32 then
          `Blocking
        else (
          Unix.set_nonblock stdin_o;
          Unix.set_nonblock stdout_i;
          `Non_blocking true
        )
      in
      let make fd what =
        let fd = Lev_fiber.Fd.create fd blockity in
        Lev_fiber.Io.create fd what
      in
      let* stdin_o = make stdin_o Output in
      let* stdout_i = make stdout_i Input in
      let* stderr_i = make stderr_i Input in
      let stdin () =
        let+ () =
          Lev_fiber.Io.with_write stdin_o ~f:(fun w ->
              Lev_fiber.Io.Writer.add_string w stdin_value;
              Lev_fiber.Io.Writer.flush w)
        in
        Lev_fiber.Io.close stdin_o
      in
      let read from () =
        let+ res =
          Lev_fiber.Io.with_read from ~f:Lev_fiber.Io.Reader.to_string
        in
        Lev_fiber.Io.close from;
        res
      in
      let+ status, (stdout, stderr) =
        Fiber.fork_and_join
          (fun () -> Lev_fiber.waitpid ~pid:(Pid.to_int pid))
          (fun () ->
            Fiber.fork_and_join_unit stdin (fun () ->
                Fiber.fork_and_join (read stdout_i) (read stderr_i)))
      in
      { stdout; stderr; status })

type error =
  | Unsupported_syntax of Document.Syntax.t
  | Missing_binary of { binary : string }
  | Unexpected_result of { message : string }
  | Unknown_extension of Uri.t

type library_error =
  | Unsupported_in_library of Document.Syntax.t
  | Building_conf_failed of string
  | All_kinds_failed of Ocamlformat.Translation_unit.Error.t list

let message = function
  | Unsupported_syntax syntax ->
    sprintf "formatting %s files is not supported"
      (Document.Syntax.human_name syntax)
  | Missing_binary { binary } ->
    sprintf
      "Unable to find %s binary. You need to install %s manually to use the \
       formatting feature."
      binary binary
  | Unknown_extension uri ->
    Printf.sprintf "Unable to format. File %s has an unknown extension"
      (Uri.to_path uri)
  | Unexpected_result { message } -> message

let library_message = function
  | Unsupported_in_library syntax ->
    sprintf "Formatting %s files is not supported using ocamlformat library."
      (Document.Syntax.human_name syntax)
  | All_kinds_failed l ->
    let error_string =
      l
      |> List.map ~f:Ocamlformat.Translation_unit.Error.to_string
      |> String.concat ~sep:"\n- "
    in
    Printf.sprintf "All attemps at formatting failed: \n- %s" error_string
  | Building_conf_failed error_string ->
    Printf.sprintf "Building_conf_failed: %s" error_string

type formatter =
  | Reason of Document.Kind.t
  | Ocaml of Uri.t

let args = function
  | Ocaml uri -> [ sprintf "--name=%s" (Uri.to_path uri); "-" ]
  | Reason kind -> (
    [ "--parse"; "re"; "--print"; "re" ]
    @
    match kind with
    | Impl -> []
    | Intf -> [ "--interface=true" ])

let binary_name t =
  match t with
  | Ocaml _ -> "ocamlformat"
  | Reason _ -> "refmt"

let binary t =
  let name = binary_name t in
  match Bin.which name with
  | None -> Result.Error (Missing_binary { binary = name })
  | Some b -> Ok b

let formatter doc =
  match Document.syntax doc with
  | (Dune | Cram | Ocamllex | Menhir) as s -> Error (Unsupported_syntax s)
  | Ocaml -> Ok (Ocaml (Document.uri doc))
  | Reason -> Ok (Reason (Document.kind doc))

let exec bin args stdin =
  let refmt = Fpath.to_string bin in
  let+ res = run_command refmt stdin args in
  match res.status with
  | Unix.WEXITED 0 -> Result.Ok res.stdout
  | _ -> Result.Error (Unexpected_result { message = res.stderr })

let run doc : (TextEdit.t list, error) result Fiber.t =
  let res =
    let open Result.O in
    let* formatter = formatter doc in
    let args = args formatter in
    let+ binary = binary formatter in
    (binary, args, Document.source doc |> Merlin_kernel.Msource.text)
  in
  match res with
  | Error e -> Fiber.return (Error e)
  | Ok (binary, args, contents) ->
    exec binary args contents
    |> Fiber.map ~f:(Result.map ~f:(fun to_ -> Diff.edit ~from:contents ~to_))

open Ocamlformat

let format ?(input_name = "<library input>") ?(conf = Conf.default) ~kind source
    =
  Translation_unit.parse_and_format kind ~input_name ~source conf

let fiber_format ?(input_name = "<library input>") ?(conf = Conf.default) ~kind
    source =
  let+ () = Fiber.return () in
  format ~input_name ~conf ~kind source

let format_try_kinds ?(conf = Conf.default) x kinds =
  let+ () = Fiber.return () in
  Base.List.fold_until ~init:[]
    ~finish:(fun l -> Error (All_kinds_failed (List.rev l)))
    ~f:(fun l try_formatting ->
      match try_formatting ~conf x with
      | Ok formatted -> Stop (Ok formatted)
      | Error e -> Continue (e :: l))
    (List.map ~f:(fun kind -> format ~kind) kinds)

let format_try_all_kinds ?(conf = Conf.default) x =
  format_try_kinds ~conf x
    [ Core_type; Signature; Module_type; Expression; Use_file ]

let format_doc doc =
  let+ () = Fiber.return () in
  match Document.syntax doc with
  | (Ocamllex | Menhir) as s -> Error (Unsupported_in_library s)
  | Ocaml -> (
    let file = doc |> Document.uri |> Uri.to_string in
    let source = doc |> Document.source |> Merlin_kernel.Msource.text in
    let kind =
      match Document.kind doc with
      | Impl -> Ocamlformat.Syntax.Use_file
      | Intf -> Ocamlformat.Syntax.Signature
    in
    match
      Ocamlformat.Conf.build_config ~enable_outside_detected_project:false
        ~root:None ~file ~is_stdin:false
    with
    | Ok conf ->
      let formatted_content =
        if conf.Conf.opr_opts.disable then
          Ok source
        else
          format ~kind ~input_name:file ~conf source
          |> Result.map_error ~f:(fun err -> All_kinds_failed [ err ])
      in
      (* Result.map ~f:(fun to_ -> Diff.edit ~from:source ~to_)
         formatted_content *)
      Error (Building_conf_failed "ee")
    | Error e -> Error (Building_conf_failed e))
  | Reason -> Error (Unsupported_in_library Reason)

let type_conf =
  Base.List.fold_until ~init:Conf.default
    ~finish:(fun conf -> conf)
    ~f:(fun conf (name, value) ->
      match Conf.update_value conf ~name ~value with
      | Ok c -> Continue c
      | Error e -> Stop Conf.default)
    [ ("module-item-spacing", "compact"); ("margin", "63") ]

let format_type type_ =
  format_try_kinds ~conf:type_conf type_
    [ Syntax.Core_type; Syntax.Module_type ]

let format_expr expr =
  format_try_kinds ~conf:type_conf expr [ Syntax.Expression ]
