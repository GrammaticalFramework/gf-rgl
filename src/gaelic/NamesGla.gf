concrete NamesGla of Names = CatGla ** open Prelude, ResGla in {

-- An API layer to deal with names
-- Not part of the RGL API, but used in the AW project
-- So depends on your goals whether this is high or low priority to implement.
  lin
    -- : GN -> NP ;
    GivenName gn = nameNP gn.s Sg ;

    -- : SN -> NP ;
    MaleSurname sn = nameNP sn.s Sg ;

    -- : SN -> NP ;
    FemaleSurname sn = nameNP sn.s Sg ;

    -- : SN -> NP ;
    PlSurname sn = nameNP sn.s Pl ;

    -- : GN -> SN -> NP ;
    FullName gn sn = nameNP (gn.s ++ sn.s) Sg ;

  lin
    -- : LN -> NP ;
    UseLN ln = nameNP ln.s Sg ;

    -- : LN -> NP ;
    PlainLN ln = nameNP ln.s Sg ;

    -- : LN -> Adv ;
    InLN ln = {s = "ann an" ++ ln.s} ;

    -- : AP -> LN -> LN ;
    AdjLN ap ln = {s = ln.s ++ ap.s ! ASg NOM Masc} ;

  oper nameNP : Str -> Number -> LinNP = \s,n -> emptyNP ** {
    s = \\_ => s ; voc = s ; a = NotPron (DDef n Def)
    } ;
}
