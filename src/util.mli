module L :
	sig
		val concat_map : ('a -> 'b list) -> 'a list -> 'b list
		val exists_one : ('a -> bool) -> 'a list -> bool
		val unique : ('a -> 'b) -> 'a list -> bool
	end
