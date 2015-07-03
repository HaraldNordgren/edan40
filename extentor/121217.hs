data Tree = Leaf String | Node Tree Tree String

subTree (Leaf i) (Leaf j)               = i == j
subTree (Node _ _ _) (Leaf _)           = False
subTree l@(Leaf i) (Node t1 t2 _)       = subTree l t1 || subTree l t2
subTree n@(Node t1 t2 i) (Node t3 t4 j) = i == j && subTree t1 t3 && subTree t2 t4 || subTree n t3 || subTree n t4
