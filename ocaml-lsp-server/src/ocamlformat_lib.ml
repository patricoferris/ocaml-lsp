open Import
open Fiber.O

type library_error =
  | Unsupported_in_library of Document.Syntax.t
  | Building_conf_failed of string
  | All_kinds_failed of Ocamlformat.Translation_unit.Error.t list

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
