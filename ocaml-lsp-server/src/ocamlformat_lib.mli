open Import

type library_error =
  | Unsupported_in_library of Document.Syntax.t
  | Building_conf_failed of string
  | All_kinds_failed of Ocamlformat.Translation_unit.Error.t list

val library_message : library_error -> string

val format_doc : Document.t -> (TextEdit.t list, library_error) result Fiber.t

val format_type : string -> (string, library_error) result Fiber.t

val format_expr : string -> (string, library_error) result Fiber.t
