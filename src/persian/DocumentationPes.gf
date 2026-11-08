--# -path=.:../abstract:../common:../../prelude

-- Persian inflection tables for dictionary and API documentation.

concrete DocumentationPes of Documentation = CatPes ** open
  ResPes, Prelude, HTML in {

  flags coding=utf8 ;

  lincat
    Inflection = {t : Str ; s1, s2 : Str} ;
    Definition = {s : Str} ;
    Document = {s : Str} ;
    Tag = {s : Str} ;

  lin
    InflectionN, InflectionN2, InflectionN3 = \noun -> {
      t = "n" ;
      s1 = heading1 "Noun" ;
      s2 = nounTable noun
      } ;

    InflectionPN = \name -> {
      t = "pn" ;
      s1 = heading1 "Proper name" ;
      s2 = paragraph name.s
      } ;

    InflectionLN = \name -> {
      t = "ln" ;
      s1 = heading1 "Location name" ;
      s2 = paragraph name.s
      } ;

    InflectionGN = \name -> {
      t = "gn" ;
      s1 = heading1 "Given name" ;
      s2 = paragraph name.s
      } ;

    InflectionSN = \name -> {
      t = "sn" ;
      s1 = heading1 "Surname" ;
      s2 = paragraph name.s
      } ;

    InflectionA, InflectionA2 = \adj -> {
      t = "a" ;
      s1 = heading1 "Adjective" ;
      s2 = adjectiveTable adj
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

    InflectionV, InflectionV2, InflectionVV, InflectionVS,
    InflectionVQ, InflectionVA, InflectionV3, InflectionV2V,
    InflectionV2S, InflectionV2Q, InflectionV2A = \verb -> {
      t = "v" ;
      s1 = heading1 "Verb" ;
      s2 = verbTable verb
      } ;

    InflectionCl = \clause -> {
      t = "cl" ;
      s1 = heading1 "Clause" ;
      s2 = clauseTable clause
      } ;

    NoDefinition title = {s = title.s} ;
    MkDefinition title definition = {
      s = paragraph ("<b>تعریف:</b>" ++ title.s ++ definition.s)
      } ;
    MkDefinitionEx title definition example = {
      s = paragraph ("<b>تعریف:</b>" ++ title.s ++ definition.s) ++
          paragraph ("<b>مثال:</b>" ++ example.s)
      } ;

    MkDocument definition inflection example = {
      s = inflection.s1 ++ definition.s ++ inflection.s2 ++ paragraph example.s
      } ;
    MkTag inflection = {s = inflection.t} ;

  oper
    nounTable : Noun -> Str = \noun ->
      frameTable (
        tr (th "Form" ++ th "Singular" ++ th "Plural") ++
        tr (th "Bare" ++ td (noun.s ! Sg ! Bare) ++ td (noun.s ! Pl ! Bare)) ++
        tr (th "Ezafe" ++ td (noun.s ! Sg ! Ezafe) ++ td (noun.s ! Pl ! Ezafe)) ++
        tr (th "Clitic" ++ td (noun.s ! Sg ! Clitic) ++ td (noun.s ! Pl ! Clitic)) ++
        tr (th "Possessive" ++ td (noun.s ! Sg ! Poss) ++ td (noun.s ! Pl ! Poss))
        ) ;

    adjectiveTable : Adjective -> Str = \adj ->
      frameTable (
        tr (th "Form" ++ th "Positive" ++ th "Comparative") ++
        adjectiveRow "Bare" Bare adj ++
        adjectiveRow "Ezafe" Ezafe adj ++
        adjectiveRow "Clitic" Clitic adj ++
        adjectiveRow "Possessive" Poss adj
        ) ++
      heading2 "Adverb" ++ paragraph adj.adv ;

    adjectiveRow : Str -> Mod -> Adjective -> Str = \label,mod,adj ->
      tr (th label ++
          td (adj.s ! Positive ! mod) ++
          td (adj.s ! Comparative ! mod)) ;

    verbTable : Verb -> Str = \verb ->
      heading2 "Basic forms" ++
      frameTable (
        tr (th "Infinitive" ++ td (verb.prefix ++ verb.s ! Inf)) ++
        tr (th "Past stem" ++ td (verb.prefix ++ verb.s ! PastStem)) ++
        tr (th "Present stem" ++ td (verb.prefix ++ verb.s ! PresStem)) ++
        tr (th "Perfect stem" ++ td (verb.prefix ++ verb.s ! PerfStem)) ++
        tr (th "Present participle" ++ td (verb.prefix ++ verb.s ! PastPart))
        ) ++
      heading2 "Present indicative" ++ finiteTable verb presentForm ++
      heading2 "Simple past" ++ finiteTable verb pastForm ++
      heading2 "Present perfect" ++ finiteTable verb perfectForm ++
      heading2 "Subjunctive" ++ finiteTable verb subjunctiveForm ++
      heading2 "Imperative" ++
      frameTable (
        tr (th "" ++ th "Singular" ++ th "Plural") ++
        polarityRow "Positive" (imperativeForm verb Pos) ++
        polarityRow "Negative" (imperativeForm verb Neg)
        ) ;

    finiteTable : Verb -> (Verb -> Polarity -> Agr -> Str) -> Str = \verb,form ->
      frameTable (
        tr (th "" ++ th "Person" ++ th "Singular" ++ th "Plural") ++
        finiteRows "Positive" Pos verb form ++
        finiteRows "Negative" Neg verb form
        ) ;

    finiteRows : Str -> Polarity -> Verb ->
                 (Verb -> Polarity -> Agr -> Str) -> Str = \label,pol,verb,form ->
      tr (intagAttr "th" "rowspan=\"3\"" label ++ th "1" ++
          td (form verb pol (Ag Sg P1)) ++ td (form verb pol (Ag Pl P1))) ++
      tr (th "2" ++ td (form verb pol (Ag Sg P2)) ++ td (form verb pol (Ag Pl P2))) ++
      tr (th "3" ++ td (form verb pol (Ag Sg P3)) ++ td (form verb pol (Ag Pl P3))) ;

    presentForm : Verb -> Polarity -> Agr -> Str = \verb,pol,agr ->
      verb.prefix ++ verb.s ! ImpPrefix pol ++ verb.s ! VAor pol agr ;

    pastForm : Verb -> Polarity -> Agr -> Str = \verb,pol,agr ->
      verb.prefix ++ verb.s ! VPast pol agr ;

    perfectForm : Verb -> Polarity -> Agr -> Str = \verb,pol,agr ->
      verb.prefix ++ verb.s ! VPerf pol agr ;

    subjunctiveForm : Verb -> Polarity -> Agr -> Str = \verb,pol,agr ->
      verb.prefix ++ verb.s ! VSubj pol agr ;

    imperativeForm : Verb -> Polarity -> (Number => Str) = \verb,pol ->
      table {
        Sg => verb.prefix ++ verb.s ! VImp pol Sg ;
        Pl => verb.prefix ++ verb.s ! VImp pol Pl
        } ;

    polarityRow : Str -> (Number => Str) -> Str = \label,forms ->
      tr (th label ++ td (forms ! Sg) ++ td (forms ! Pl)) ;

    clauseTable : Clause -> Str = \clause ->
      frameTable (
        tr (th "Tense" ++ th "Positive" ++ th "Negative") ++
        clauseRow "Present" (Ind Pres Simul) clause
        ++ clauseRow "Present perfect" (Ind Pres Anter) clause --# notpresent
        ++ clauseRow "Past" (Ind Past Simul) clause --# notpresent
        ++ clauseRow "Past perfect" (Ind Past Anter) clause --# notpresent
        ++ clauseRow "Future" (Ind Fut Simul) clause --# notpresent
        ++ clauseRow "Future perfect" (Ind Fut Anter) clause --# notpresent
        ++ clauseRow "Conditional" (Ind Cond Simul) clause --# notpresent
        ++ clauseRow "Conditional perfect" (Ind Cond Anter) clause --# notpresent
        ) ;

    clauseRow : Str -> TAnt -> Clause -> Str = \label,tense,clause ->
      tr (th label ++
          td (clause.s ! tense ! Pos ! ODir) ++
          td (clause.s ! tense ! Neg ! ODir)) ;
}
