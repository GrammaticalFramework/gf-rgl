--# -path=.:../abstract:../common:../../prelude

concrete DocumentationTel of Documentation = CatTel ** open
  ResTel, Prelude, HTML in {

  lincat
    Inflection = {t : Str ; s1, s2 : Str} ;
    Definition = {s : Str} ;
    Document = {s : Str} ;
    Tag = {s : Str} ;

  lin
    InflectionN, InflectionN2, InflectionN3 = \noun -> {
      t = "n" ;
      s1 = heading1 ("Noun" ++ genderName noun.g) ;
      s2 = frameTable (
        tr (th "" ++ th "Sg" ++ th "Pl") ++
        tr (th "Dir" ++ td (noun.s ! Sg ! Dir) ++ td (noun.s ! Pl ! Dir)) ++
        tr (th "Obl" ++ td (noun.s ! Sg ! Obl) ++ td (noun.s ! Pl ! Obl))
        )
      } ;

    InflectionPN = \name -> {
      t = "pn" ;
      s1 = heading1 ("Proper name" ++ genderName name.g) ;
      s2 = frameTable (
        tr (th "Dir" ++ td (name.s ! Dir)) ++
        tr (th "Obl" ++ td (name.s ! Obl))
        )
      } ;

    InflectionLN, InflectionGN, InflectionSN = \name -> {
      t = "pn" ;
      s1 = heading1 "Name" ;
      s2 = paragraph name.s
      } ;

    InflectionA, InflectionA2 = \adj -> {
      t = "a" ;
      s1 = heading1 "Adjective" ;
      s2 = adjectiveTable adj
      } ;

    InflectionV, InflectionV2, InflectionVV, InflectionVS,
    InflectionVQ, InflectionVA, InflectionV3, InflectionV2V,
    InflectionV2S, InflectionV2Q, InflectionV2A = \verb -> {
      t = "v" ;
      s1 = heading1 "Verb" ;
      s2 = verbTable verb
      } ;

    InflectionAdv, InflectionAdV, InflectionAdA, InflectionAdN = \adv -> {
      t = "adv" ;
      s1 = heading1 "Adverb" ;
      s2 = paragraph adv.s
      } ;

    InflectionPrep = \prep -> {
      t = "prep" ;
      s1 = heading1 "Preposition" ;
      s2 = paragraph prep.s
      } ;

    NoDefinition title = {s = title.s} ;
    MkDefinition title definition = {
      s = paragraph ("<b>నిర్వచనం:</b>" ++ title.s ++ definition.s)
      } ;
    MkDefinitionEx title definition example = {
      s = paragraph ("<b>నిర్వచనం:</b>" ++ title.s ++ definition.s) ++
          paragraph ("<b>ఉదాహరణ:</b>" ++ example.s)
      } ;

    MkDocument definition inflection example = {
      s = inflection.s1 ++ definition.s ++ inflection.s2 ++ paragraph example.s
      } ;
    MkTag inflection = {s = inflection.t} ;

  oper
    genderName : Gender -> Str = \gender -> case gender of {
      Masc => "(Masc)" ;
      Fem => "(Fem)" ;
      Neutr => "(Neutr)"
      } ;

    adjectiveTable : Adjective -> Str = \adj ->
      frameTable (
        tr (th "" ++ th "Sg Dir" ++ th "Sg Obl" ++ th "Pl Dir" ++ th "Pl Obl") ++
        adjectiveRow "Masc" adj Masc ++
        adjectiveRow "Fem" adj Fem ++
        adjectiveRow "Neutr" adj Neutr
        ) ;

    adjectiveRow : Str -> Adjective -> Gender -> Str = \label,adj,gender ->
      tr (th label ++
          td (adj.s ! gender ! Sg ! Dir) ++
          td (adj.s ! gender ! Sg ! Obl) ++
          td (adj.s ! gender ! Pl ! Dir) ++
          td (adj.s ! gender ! Pl ! Obl)) ;

    verbTable : Verb -> Str = \verb ->
      heading2 "Basic forms" ++
      frameTable (
        tr (th "Infinitive" ++ td (verb.s ! VInf)) ++
        tr (th "Stem" ++ td (verb.s ! VStem)) ++
        tr (th "Absolutive" ++ td (verb.s ! VAbs)) ++
        tr (th "Request" ++ td (verb.s ! VReq)) ++
        tr (th "Imperative" ++ td (verb.s ! VImp)) ++
        tr (th "Future request" ++ td (verb.s ! VReqFut))
        ) ++
      heading2 "Imperfect" ++ genderNumberTable verb VImpf ++
      heading2 "Perfect" ++ genderNumberTable verb VPerf ++
      heading2 "Subjunctive" ++ personTable verb VSubj ++
      heading2 "Future" ++
        heading3 "Masculine" ++ futureTable verb Masc ++
        heading3 "Feminine" ++ futureTable verb Fem ++
        heading3 "Neuter" ++ futureTable verb Neutr ;

    genderNumberTable : Verb -> (Gender -> Number -> VForm) -> Str = \verb,form ->
      frameTable (
        tr (th "" ++ th "Sg" ++ th "Pl") ++
        tr (th "Masc" ++ td (verb.s ! form Masc Sg) ++ td (verb.s ! form Masc Pl)) ++
        tr (th "Fem" ++ td (verb.s ! form Fem Sg) ++ td (verb.s ! form Fem Pl)) ++
        tr (th "Neutr" ++ td (verb.s ! form Neutr Sg) ++ td (verb.s ! form Neutr Pl))
        ) ;

    personTable : Verb -> (Number -> Person -> VForm) -> Str = \verb,form ->
      frameTable (
        tr (th "" ++ th "P1" ++ th "P2" ++ th "P3") ++
        tr (th "Sg" ++ td (verb.s ! form Sg P1) ++ td (verb.s ! form Sg P2) ++ td (verb.s ! form Sg P3)) ++
        tr (th "Pl" ++ td (verb.s ! form Pl P1) ++ td (verb.s ! form Pl P2) ++ td (verb.s ! form Pl P3))
        ) ;

    futureTable : Verb -> Gender -> Str = \verb,gender ->
      frameTable (
        tr (th "" ++ th "P1" ++ th "P2" ++ th "P3") ++
        tr (th "Sg" ++ td (verb.s ! VFut Sg P1 gender) ++ td (verb.s ! VFut Sg P2 gender) ++ td (verb.s ! VFut Sg P3 gender)) ++
        tr (th "Pl" ++ td (verb.s ! VFut Pl P1 gender) ++ td (verb.s ! VFut Pl P2 gender) ++ td (verb.s ! VFut Pl P3 gender))
        ) ;
}
