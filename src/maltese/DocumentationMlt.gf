--# -path=.:../abstract:../common:../prelude

-- DocumentationMlt.gf: Maltese inflection tables

concrete DocumentationMlt of Documentation = CatMlt ** open
  ResMlt,
  Prelude,
  HTML in {

flags
  coding = utf8 ;

lincat
  Inflection = {t : Str ; s1,s2 : Str} ;
  Definition = {s : Str} ;
  Document   = {s : Str} ;
  Tag        = {s : Str} ;

lin
  InflectionN, InflectionN2, InflectionN3 = \noun -> {
    t = "n" ;
    s1 = heading1 ("Noun" ++ parens (genderLabel noun.g)) ;
    s2 = frameTable (
      tr (th "" ++ th "form") ++
      tr (th "singulative" ++ td (noun.s ! Singulative)) ++
      tr (th "collective"  ++ td (noun.s ! Collective)) ++
      tr (th "dual"        ++ td (noun.s ! Dual)) ++
      tr (th "plural"      ++ td (noun.s ! Plural))
      )
    } ;

  InflectionPN, InflectionLN, InflectionGN, InflectionSN = \pn -> {
    t = "pn" ;
    s1 = heading1 ("Name" ++ parens (agrLabel pn.a)) ;
    s2 = paragraph pn.s
    } ;

  InflectionA, InflectionA2 = \adj -> {
    t = "a" ;
    s1 = heading1 "Adjective" ;
    s2 = frameTable (
      tr (th "" ++ th "form") ++
      tr (th "sg masc" ++ td (adj.s ! APosit (GSg Masc))) ++
      tr (th "sg fem"  ++ td (adj.s ! APosit (GSg Fem))) ++
      tr (th "pl"             ++ td (adj.s ! APosit GPl)) ++
      case adj.hasComp of {
        True  =>
          tr (th "comparative" ++ td (adj.s ! ACompar)) ++
          tr (th "superlative" ++ td (adj.s ! ASuperl)) ;
        False => []
        }
      )
    } ;

  InflectionV, InflectionV2, InflectionVV, InflectionVS, InflectionVQ,
  InflectionVA, InflectionV3, InflectionV2V, InflectionV2S, InflectionV2Q,
  InflectionV2A = \verb -> {
    t = "v" ;
    s1 = heading1 "Verb" ;
    s2 = inflVerb verb
    } ;

  InflectionAdv adv = {
    t = "adv" ;
    s1 = heading1 "Adverb" ;
    s2 = paragraph adv.s
    } ;

  InflectionAdV, InflectionAdA, InflectionAdN = \adv -> {
    t = "adv" ;
    s1 = heading1 "Adverb" ;
    s2 = paragraph adv.s
    } ;

  InflectionPrep prep = {
    t = "prep" ;
    s1 = heading1 "Preposition" ;
    s2 = frameTable (
      tr (th "indef" ++ td (prep.s ! Indefinite)) ++
      tr (th "def"   ++ td (prep.s ! Definite)) ++
      prepCliticRow "1p sg"      (mkAgr Sg P1 Masc) prep ++
      prepCliticRow "2p sg"      (mkAgr Sg P2 Masc) prep ++
      prepCliticRow "3p sg masc" (mkAgr Sg P3 Masc) prep ++
      prepCliticRow "3p sg fem"  (mkAgr Sg P3 Fem) prep ++
      prepCliticRow "1p pl"      (mkAgr Pl P1 Masc) prep ++
      prepCliticRow "2p pl"      (mkAgr Pl P2 Masc) prep ++
      prepCliticRow "3p pl"      (mkAgr Pl P3 Masc) prep
      )
    } ;

  NoDefinition t = {s = t.s} ;
  MkDefinition t d = {
    s = "<p><b>Definition:</b>" ++ t.s ++ d.s ++ "</p>"
    } ;
  MkDefinitionEx t d e = {
    s = "<p><b>Definition:</b>" ++ t.s ++ d.s ++
        "</p><p><b>Example:</b>" ++ e.s ++ "</p>"
    } ;

  MkDocument d i e = {
    s = i.s1 ++ d.s ++ i.s2 ++ paragraph e.s
    } ;
  MkTag i = {s = i.t} ;

oper
  inflVerb : Verb -> Str = \verb ->
    frameTable (
      tr (th "" ++ th "perfect" ++ th "imperfect") ++
      verbRow "1p sg"      (AgP1 Sg) verb ++
      verbRow "2p sg"      (AgP2 Sg) verb ++
      verbRow "3p sg masc" (AgP3Sg Masc) verb ++
      verbRow "3p sg fem"  (AgP3Sg Fem) verb ++
      verbRow "1p pl"      (AgP1 Pl) verb ++
      verbRow "2p pl"      (AgP2 Pl) verb ++
      verbRow "3p pl"      AgP3Pl verb
      ) ++
    heading2 "Imperative" ++
    frameTable (
      tr (th "sg" ++ td (verbForm verb (VImp Sg))) ++
      tr (th "pl"   ++ td (verbForm verb (VImp Pl)))
      ) ;

  verbRow : Str -> VAgr -> Verb -> Str = \label,agr,verb ->
    tr (th label ++
        td (verbForm verb (VPerf agr)) ++
        td (verbForm verb (VImpf agr))) ;

  verbForm : Verb -> VForm -> Str = \verb,form ->
    (verb.s ! form).s1 ;

  prepCliticRow : Str -> Agr -> Preposition -> Str = \label,agr,prep ->
    tr (th label ++ td (prep.enclitic ! agr)) ;

  genderLabel : Gender -> Str = \g -> case g of {
    Masc => "masc" ;
    Fem  => "fem"
    } ;

  numberLabel : Number -> Str = \n -> case n of {
    Sg => "sg" ;
    Pl => "pl"
    } ;

  personLabel : Person -> Str = \p -> case p of {
    P1 => "1p" ;
    P2 => "2p" ;
    P3 => "3p"
    } ;

  agrLabel : Agr -> Str = \a ->
    personLabel a.p ++ numberLabel a.n ++ genderLabel a.g ;

  parens : Str -> Str = \s -> "(" + s + ")" ;

}
