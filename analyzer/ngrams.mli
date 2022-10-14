open! Import

val freq1 : Key.t -> data:float Char.Table.t -> float
val freq2 : Key.t * Key.t -> data:float String.Table.t -> float
val freq3 : Key.t * Key.t * Key.t -> data:float String.Table.t -> float
