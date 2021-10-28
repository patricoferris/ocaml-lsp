type t

val create : unit -> t

val stop : t -> unit Fiber.t

val run : t -> unit Fiber.t

val get_external_config : t -> Merlin_kernel.Mconfig.t -> string -> Merlin_kernel.Mconfig.t Fiber.t
