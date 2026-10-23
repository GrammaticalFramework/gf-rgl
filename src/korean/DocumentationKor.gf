--# -path=.:../abstract:../common:../../prelude

-- Korean inflection tables for dictionary and API documentation.

concrete DocumentationKor of Documentation = CatKor ** open
  ResKor, Prelude, HTML in {

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

  InflectionPN = \name -> nameInflection "pn" "Proper name" name ;
  InflectionLN = \name -> nameInflection "ln" "Location name" name ;
  InflectionGN = \name -> nameInflection "gn" "Given name" name ;
  InflectionSN = \name -> nameInflection "sn" "Surname" name ;

  InflectionA, InflectionA2 = \adj -> {
    t = "a" ;
    s1 = heading1 "Adjective" ;
    s2 = predicateTable adj.s adj.past adj.pastAttr adj.futureAttr
    } ;

  InflectionV, InflectionVS, InflectionVQ, InflectionVA = \verb ->
    verbInflection (
      argumentTable1 verb.sc ++
      predicateTable verb.s verb.past verb.pastAttr verb.futureAttr
      ) ;

  InflectionV2, InflectionVV, InflectionV2A, InflectionV2V,
  InflectionV2S, InflectionV2Q = \verb -> verbInflection (
    argumentTable2 verb.sc verb.c2 verb.p2 ++
    predicateTable verb.s verb.past verb.pastAttr verb.futureAttr
    ) ;

  InflectionV3 = \verb -> verbInflection (
    argumentTable3 verb.sc verb.c2 verb.p2 verb.c3 verb.p3 ++
    predicateTable verb.s verb.past verb.pastAttr verb.futureAttr
    ) ;

  InflectionAdv, InflectionAdV, InflectionAdA, InflectionAdN = \adv -> {
    t = "adv" ;
    s1 = heading1 "Adverb" ;
    s2 = paragraph adv.s
    } ;

  InflectionPrep = \prep -> {
    t = "prep" ;
    s1 = heading1 "Postposition" ;
    s2 = postpositionTable prep
    } ;

  InflectionCl = \clause -> {
    t = "cl" ;
    s1 = heading1 "Clause" ;
    s2 = clauseTable clause
    } ;

  NoDefinition title = {s = title.s} ;
  MkDefinition title definition = {
    s = paragraph ("<b>정의:</b>" ++ title.s ++ definition.s)
    } ;
  MkDefinitionEx title definition example = {
    s = paragraph ("<b>정의:</b>" ++ title.s ++ definition.s) ++
        paragraph ("<b>예문:</b>" ++ example.s)
    } ;

  MkDocument definition inflection example = {
    s = inflection.s1 ++ definition.s ++ inflection.s2 ++ paragraph example.s
    } ;
  MkTag inflection = {s = inflection.t} ;

oper
  nounTable : Noun -> Str = \noun ->
    frameTable (
      tr (th "Form" ++ th "Inflection") ++
      nounRow "Bare" Bare noun.s ++
      nounRow "Topic" Topic noun.s ++
      nounRow "Subject" Subject noun.s ++
      nounRow "Object" Object noun.s
      ) ++
    frameTable (
      tr (th "Counter" ++ td noun.c.s) ++
      tr (th "Numeral system" ++ td (originName noun.c.origin))
      ) ;

  nounRow : Str -> NForm -> (NForm => Str) -> Str = \label,form,forms ->
    tr (th label ++ td (forms ! form)) ;

  originName : NumOrigin -> Str = \origin -> case origin of {
    NK => "Native Korean" ;
    SK => "Sino-Korean"
    } ;

  nameInflection : Str -> Str -> NounPhrase -> {t, s1, s2 : Str} =
    \tag,title,name -> {
      t = tag ;
      s1 = heading1 title ;
      s2 = frameTable (
        tr (th "Form" ++ th "Inflection") ++
        nounRow "Bare" Bare name.s ++
        nounRow "Topic" Topic name.s ++
        nounRow "Subject" Subject name.s ++
        nounRow "Object" Object name.s
        )
      } ;

  predicateTable : (VForm => Str) -> (Polarity => Str) ->
                   (Polarity => Str) -> (Polarity => Str) -> Str =
    \forms,pastForms,pastAttrForms,futureAttrForms ->
      heading2 "Finite forms" ++
      frameTable (
        tr (th "Tense" ++ th "Plain positive" ++ th "Plain negative" ++
            th "Polite positive" ++ th "Polite negative" ++
            th "Formal positive" ++ th "Formal negative") ++
        finiteRow "Present" (\\style,pol => forms ! VF style pol) ++
        finiteRow "Past"
          (\\style,pol => finishFinite style (pastForms ! pol)) ++
        finiteRow "Future"
          (\\style,pol => finishFinite style
            (glue (forms ! VStem pol) "겠")) ++
        finiteRow "Conditional"
          (\\style,pol => conditional style (pastForms ! pol))
        ) ++
      heading2 "Attributive forms" ++
      frameTable (
        tr (th "Tense" ++ th "Positive" ++ th "Negative") ++
        polarityRow "Present" (\\pol => forms ! VAttr pol) ++
        polarityRow "Past" pastAttrForms ++
        polarityRow "Future" futureAttrForms
        ) ++
      heading2 "Other forms" ++
      frameTable (
        tr (th "Form" ++ th "Positive" ++ th "Negative") ++
        polarityRow "Stem" (\\pol => forms ! VStem pol) ++
        tr (th "Imperative" ++
            td (forms ! VF Polite Pos) ++ td (forms ! VImpNeg))
        ) ;

  finiteRow : Str -> (Style => Polarity => Str) -> Str = \label,forms ->
    tr (th label ++
        td (forms ! Plain ! Pos) ++ td (forms ! Plain ! Neg) ++
        td (forms ! Polite ! Pos) ++ td (forms ! Polite ! Neg) ++
        td (forms ! Formal ! Pos) ++ td (forms ! Formal ! Neg)) ;

  polarityRow : Str -> (Polarity => Str) -> Str = \label,forms ->
    tr (th label ++ td (forms ! Pos) ++ td (forms ! Neg)) ;

  verbInflection : Str -> {t, s1, s2 : Str} = \content -> {
    t = "v" ;
    s1 = heading1 "Verb" ;
    s2 = content
    } ;

  argumentTable1 : NForm -> Str = \subjectCase ->
    heading2 "Arguments" ++
    frameTable (
      argumentHeader ++
      subjectRow subjectCase
      ) ;

  argumentTable2 : NForm -> NForm -> Postposition -> Str =
    \subjectCase,objectCase,objectPostposition ->
      heading2 "Arguments" ++
      frameTable (
        argumentHeader ++
        subjectRow subjectCase ++
        argumentRow "Object" objectCase objectPostposition
        ) ;

  argumentTable3 : NForm -> NForm -> Postposition ->
                   NForm -> Postposition -> Str =
    \subjectCase,objectCase,objectPostposition,thirdCase,thirdPostposition ->
      heading2 "Arguments" ++
      frameTable (
        argumentHeader ++
        subjectRow subjectCase ++
        argumentRow "Object 1" objectCase objectPostposition ++
        argumentRow "Object 2" thirdCase thirdPostposition
        ) ;

  argumentHeader : Str =
    tr (th "Argument" ++ th "Case" ++
        th "After vowel" ++ th "After consonant") ;

  subjectRow : NForm -> Str = \subjectCase ->
    tr (th "Subject" ++ td (caseName subjectCase) ++ td "—" ++ td "—") ;

  argumentRow : Str -> NForm -> Postposition -> Str = \label,nform,prep ->
    tr (th label ++ td (caseName nform) ++
        td (prep.s ! Vowel) ++ td (prep.s ! Consonant)) ;

  caseName : NForm -> Str = \nform -> case nform of {
    Bare => "none" ;
    Topic => "은/는" ;
    Subject => "이/가" ;
    Object => "을/를"
    } ;

  postpositionTable : Postposition -> Str = \prep ->
    frameTable (
      tr (th "After vowel" ++ td (prep.s ! Vowel)) ++
      tr (th "After consonant" ++ td (prep.s ! Consonant))
      ) ;

  clauseTable : Cl -> Str = \clause ->
    frameTable (
      tr (th "Tense" ++ th "Anteriority" ++
          th "Plain positive" ++ th "Plain negative" ++
          th "Polite positive" ++ th "Polite negative" ++
          th "Formal positive" ++ th "Formal negative") ++
      clauseRow "Present" Pres Simul clause ++
      clauseRow "Present" Pres Anter clause ++
      clauseRow "Past" Past Simul clause ++
      clauseRow "Past" Past Anter clause ++
      clauseRow "Future" Fut Simul clause ++
      clauseRow "Future" Fut Anter clause ++
      clauseRow "Conditional" Cond Simul clause ++
      clauseRow "Conditional" Cond Anter clause
      ) ;

  clauseRow : Str -> ResKor.Tense -> Anteriority -> Cl -> Str =
    \label,tense,anter,clause ->
      tr (th label ++ th (anteriorityName anter) ++
          td (clause.s ! tense ! anter ! Pos ! Statement Plain) ++
          td (clause.s ! tense ! anter ! Neg ! Statement Plain) ++
          td (clause.s ! tense ! anter ! Pos ! Statement Polite) ++
          td (clause.s ! tense ! anter ! Neg ! Statement Polite) ++
          td (clause.s ! tense ! anter ! Pos ! Statement Formal) ++
          td (clause.s ! tense ! anter ! Neg ! Statement Formal)) ;

  anteriorityName : Anteriority -> Str = \anter -> case anter of {
    Simul => "simultaneous" ;
    Anter => "anterior"
    } ;

}
