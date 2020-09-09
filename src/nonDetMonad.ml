type 'a t = 'a list

let return x = [x]
let ( >>= ) m f = List.flatten (List.map f m)
let fail = []
let pick l = l
let compute m = m
let merge m1 m2 = m1 @ m2
let fmap f m = List.map f m
let conjug f m1 m2 = m1 >>= (fun v1 -> fmap (f v1) m2)
