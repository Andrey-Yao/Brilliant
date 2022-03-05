open Sig

module MakeLabelled(VI: VIngredient)(EI: EIngredient):
Labelled with type e = EI.t
          and type v = VI.t


module MakeUnlabelled(VI: VIngredient):
Unlabelled with type v = VI.t

