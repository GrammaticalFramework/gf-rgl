concrete NamesPes of Names = CatPes ** open Prelude, ResPes in {

  lin
    GivenName gn = nameNP Animate Sg gn.s ;
    MaleSurname sn = nameNP Animate Sg sn.s ;
    FemaleSurname sn = nameNP Animate Sg sn.s ;
    PlSurname sn = nameNP Animate Pl sn.s ;
    FullName gn sn = nameNP Animate Sg (gn.s ++ sn.s) ;

    UseLN ln = nameNP Inanimate Sg ln.s ;
    PlainLN ln = nameNP Inanimate Sg ln.s ;
    InLN ln = {s = "در" ++ ln.s} ;
    AdjLN ap ln = {s = runtimeKasre ln.s ++ ap.s ! Bare} ;

  oper
    nameNP : Animacy -> Number -> Str -> NP = \animacy,number,s ->
      emptyNP ** {
        s = \\_ => s ;
        a = agrP3 number ;
        animacy = animacy
      } ;
}
