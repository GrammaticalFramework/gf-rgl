--# -path=.:../abstract:../common

concrete DocumentationIna of Documentation = CatIna ** open
  ResIna,
  Prelude,
  HTML
in {
flags coding=utf8 ;


lincat
  Inflection = {t : Str; s1,s2 : Str} ;
  Definition = {s : Str} ;
  Document = {s : Str} ;
  Tag = {s : Str} ;

{-
-} --# notpresent

lin
  InflectionN, InflectionN2, InflectionN3 = \noun -> {
    t = "s" ;
    s1 = heading1 "Substantivo" ;
    s2 = frameTable (
           tr (th "Singular"    ++ th "Plural") ++
           tr (td (noun.s ! Sg) ++ td (noun.s ! Pl))
           )
    } ;

  InflectionPN = \pn -> {
    t = "pn" ;
    s1 = heading1 ("Nombre Proprio") ;
    s2 = pn.s
    } ;

  InflectionGN = \gn -> {
    t = "pn" ;
    s1 = heading1 ("Nombre de Pila" ++
                   case gn.g of {
                     Male   => "(Mascule)" ;
                     Female => "(Femina)"
                   }) ;
    s2 = gn.s
    } ;

  InflectionSN = \sn -> {
    t = "sn" ;
    s1 = heading1 "Apellido" ;
    s2 = sn.s ! Male
    } ;

  InflectionLN = \ln -> {
    t = "nl" ;
    s1 = heading1 ("Nombre del Lugar") ;
    s2 = paragraph ln.s
    } ;

  InflectionA, InflectionA2 = \adj -> {
    t  = "a" ;
    s1 = heading1 "Adjectivo" ;
    s2 = paragraph (adj.s ! AAdj Posit)
    } ;

  InflectionAdv, InflectionAdV, InflectionAdA, InflectionAdN = \adv -> {
    t  = "adv" ;
    s1 = heading1 "Adverbio" ;
    s2 = paragraph adv.s
    } ;

  InflectionPrep p = {
    t  = "prep" ;
    s1 = heading1 "Preposition" ;
    s2 = paragraph p.s
    } ;

  InflectionV, InflectionV2, InflectionV3, InflectionV2V, InflectionV2S, InflectionV2Q, InflectionV2A, InflectionVV, InflectionVS, InflectionVQ, InflectionVA = \v -> {
    t  = "v" ;
    s1 = heading1 "Verbo" ;
    s2 = inflVerb v
    } ;

lin
  NoDefinition   t     = {s=t.s};
  MkDefinition   t d   = {s="<p><b>Definición:</b>"++t.s++d.s++"</p>"};
  MkDefinitionEx t d e = {s="<p><b>Definición:</b>"++t.s++d.s++"</p><p><b>Ejemplo:</b>"++e.s++"</p>"};

lin
  MkDocument b i e = ss (i.s1 ++ "<p style=\"font-size:20px\">"++b.s++"</p>" ++ i.s2 ++ paragraph e.s) ;  -- explanation appended in a new paragraph
  MkTag i = ss i.t ;

oper
  inflVerb : Verb -> Str = \verb ->
     frameTable (
       tr (th "infinitivo" ++
           td (verb.s ! VInf)) ++
       tr (th "presente" ++
           td (verb.s ! VPres)) ++
       tr (th "participio passato" ++
           td (verb.s ! VPPart)) ++
       tr (th "participio presente" ++
           td (verb.s ! VPresPart)) ++
       tr (th "passato" ++
           td (verb.s ! VPast)) ++
       tr (th "futuro" ++
           td (verb.s ! VFut)) ++
       tr (th "conditional" ++
           td (verb.s ! VCond))
       ) ;

{- --# notpresent
-}

}
