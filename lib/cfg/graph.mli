(**This is a module representing digraphs*)

module Make(Edg: Sig.E) : Sig.G
       with type e = Edg.t
        and type ES.elt = Edg.t * string
