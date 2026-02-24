--# -path=.:../abstract:../common
concrete DocumentationSco of Documentation = DocumentationEng - [InflectionA, InflectionA2] ** open ResSco, HTML in {

lin
  InflectionA, InflectionA2 = \adj -> {
    t  = "a" ;
    s1 = heading1 "Adjective" ;
    s2 = frameTable (
           tr (th ""       ++ th "nom" ++ th "gen") ++
           tr (th "posit"  ++ td (adj.s ! AAdj Posit  Nom) ++ td (adj.s ! AAdj Posit  Gen)) ++
           tr (th "compar" ++ td (getCompar Nom adj) ++ td (getCompar Gen adj)) ++
           tr (th "superl" ++ td (getSuperl Nom adj) ++ td (getSuperl Gen adj))
         ) ++
         heading1 "Adverb" ++
         paragraph (adj.s ! AAdv)
    } ;

}
