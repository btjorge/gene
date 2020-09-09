type 'a t

val return : 'a -> 'a t
val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
val fail : 'a t
val pick : 'a list -> 'a t
val compute : 'a t -> 'a list
val merge : 'a t -> 'a t -> 'a t
val fmap : ('a -> 'b) -> 'a t -> 'b t
val conjug : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
