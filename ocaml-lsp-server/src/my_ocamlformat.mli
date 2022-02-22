(** Generic formatting facility for OCaml and Reason sources.

    Relies on [ocamlformat] for OCaml and [refmt] for reason *)

open Import

type error =
  | Unsupported_syntax of Document.Syntax.t
  | Missing_binary of { binary : string }
  | Unexpected_result of { message : string }
  | Unknown_extension of Uri.t

type library_error =
  | Unsupported_in_library of Document.Syntax.t
  | Building_conf_failed of string
  | All_kinds_failed of Ocamlformat.Translation_unit.Error.t list

val message : error -> string

val library_message : library_error -> string

val run : Document.t -> (TextEdit.t list, error) result Fiber.t

val format_doc : Document.t -> (TextEdit.t list, library_error) result Fiber.t

val format_type : string -> (string, library_error) result Fiber.t

val format_expr : string -> (string, library_error) result Fiber.t
