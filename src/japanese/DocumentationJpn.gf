concrete DocumentationJpn of Documentation = CatJpn ** open
  ResJpn, Prelude, HTML in {

lincat
  Inflection = {t : Str; s1,s2 : Str} ;
  Definition = {s : Str} ;
  Document = {s : Str} ;
  Tag      = {s : Str} ;

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
    s2 = adjectiveTable adj
    } ;

  InflectionAdv = \adv -> {
    t = "adv" ;
    s1 = heading1 "Adverb" ;
    s2 = styleTable adv.s
    } ;

  InflectionAdV, InflectionAdA, InflectionAdN = \adv -> {
    t = "adv" ;
    s1 = heading1 "Adverb" ;
    s2 = paragraph adv.s
    } ;

  InflectionPrep = \prep -> {
    t = "prep" ;
    s1 = heading1 "Postposition" ;
    s2 = paragraph prep.s
    } ;

  InflectionV = \verb -> verbInflection
    (verbTable verb.s verb.te verb.ba verb.a_stem verb.i_stem) ;

  InflectionV2 = \verb -> verbInflection (
    heading2 "Active" ++
    verbTable verb.s verb.te verb.ba verb.a_stem verb.i_stem ++
    heading2 "Passive" ++
    verbTable verb.pass verb.pass_te verb.pass_ba
      verb.pass_a_stem verb.pass_i_stem
    ) ;

  InflectionV3 = \verb -> verbInflection (
    heading2 "Speaker-oriented form" ++
    verbTable (verb.s ! Me) (verb.te ! Me) (verb.ba ! Me)
      (verb.a_stem ! Me) (verb.i_stem ! Me) ++
    heading2 "Other-person-oriented form" ++
    verbTable (verb.s ! SomeoneElse) (verb.te ! SomeoneElse)
      (verb.ba ! SomeoneElse) (verb.a_stem ! SomeoneElse)
      (verb.i_stem ! SomeoneElse)
    ) ;

  InflectionVV = \verb -> verbInflection (
    heading2 "Speaker as subject" ++
    verbTable (verb.s ! Me) (verb.te ! Me) (verb.ba ! Me)
      (verb.a_stem ! Me) (verb.i_stem ! Me) ++
    heading2 "Other person as subject" ++
    verbTable (verb.s ! SomeoneElse) (verb.te ! SomeoneElse)
      (verb.ba ! SomeoneElse) (verb.a_stem ! SomeoneElse)
      (verb.i_stem ! SomeoneElse)
    ) ;

  InflectionVS, InflectionVQ = \verb -> verbInflection
    (verbTable verb.s verb.te verb.ba verb.a_stem verb.i_stem) ;

  InflectionVA, InflectionV2V, InflectionV2S,
  InflectionV2Q, InflectionV2A = \verb -> verbInflection
    (verbTable verb.s verb.te verb.ba verb.a_stem verb.i_stem) ;

  InflectionCl = \clause -> {
    t = "cl" ;
    s1 = heading1 "Clause" ;
    s2 = clauseTable clause
    } ;

  NoDefinition title = {s = title.s} ;
  MkDefinition title definition = {
    s = paragraph ("<b>定義:</b>" ++ title.s ++ definition.s)
    } ;
  MkDefinitionEx title definition example = {
    s = paragraph ("<b>定義:</b>" ++ title.s ++ definition.s) ++
        paragraph ("<b>例:</b>" ++ example.s)
    } ;

  MkDocument definition inflection example = {
    s = inflection.s1 ++ definition.s ++ inflection.s2 ++ paragraph example.s
    } ;
  MkTag inflection = {s = inflection.t} ;

oper
  nounTable : Noun -> Str = \noun ->
    frameTable (
      tr (th "" ++ th "Singular" ++ th "Plural") ++
      tr (th "Plain" ++
          td (noun.s ! Sg ! Plain) ++ td (noun.s ! Pl ! Plain)) ++
      tr (th "Respectful" ++
          td (noun.s ! Sg ! Resp) ++ td (noun.s ! Pl ! Resp))
      ) ++
    frameTable (
      tr (th "Animacy" ++ td (animacyName noun.anim)) ++
      tr (th "Counter" ++ td noun.counter)
      ) ;

  nameInflection : Str -> Str -> PropNoun -> {t, s1, s2 : Str} =
    \tag,title,name -> {
      t = tag ;
      s1 = heading1 title ;
      s2 = styleTable name.s ++
           frameTable (tr (th "Animacy" ++ td (animacyName name.anim)))
      } ;

  animacyName : Animateness -> Str = \animacy -> case animacy of {
    Anim => "animate" ;
    Inanim => "inanimate"
    } ;

  styleTable : (Style => Str) -> Str = \forms ->
    frameTable (
      tr (th "Plain" ++ td (forms ! Plain)) ++
      tr (th "Respectful" ++ td (forms ! Resp))
      ) ;

  adjectiveTable : Adj -> Str = \adj ->
    heading2 "Predicative" ++
    finiteTable adj.pred ++
    heading2 "Other forms" ++
    frameTable (
      tr (th "Form" ++ th "Positive" ++ th "Negative") ++
      tr (th "Attributive" ++ td adj.attr ++ td "—") ++
      polarityRow "Te-form" adj.te ++
      polarityRow "Conditional" adj.ba ++
      polarityRow "Adverbial" adj.adv
      ) ;

  verbInflection : Str -> {t, s1, s2 : Str} = \content -> {
    t = "v" ;
    s1 = heading1 "Verb" ;
    s2 = content
    } ;

  verbTable : (Style => TTense => Polarity => Str) ->
              (Polarity => Str) -> (Polarity => Str) -> Str -> Str -> Str =
    \forms,teForm,baForm,aStem,iStem ->
      finiteTable forms ++
      heading2 "Other forms" ++
      frameTable (
        tr (th "Form" ++ th "Positive" ++ th "Negative") ++
        tr (th "A-stem" ++ td aStem ++ td "—") ++
        tr (th "I-stem" ++ td iStem ++ td "—") ++
        polarityRow "Te-form" teForm ++
        polarityRow "Conditional" baForm
        ) ;

  finiteTable : (Style => TTense => Polarity => Str) -> Str = \forms ->
    frameTable (
      tr (th "Tense" ++ th "Plain positive" ++ th "Plain negative" ++
          th "Respectful positive" ++ th "Respectful negative") ++
      finiteRow "Present" TPres forms ++
      finiteRow "Past" TPast forms ++
      finiteRow "Future" TFut forms
      ) ;

  finiteRow : Str -> TTense ->
              (Style => TTense => Polarity => Str) -> Str =
    \label,tense,forms ->
      tr (th label ++
          td (forms ! Plain ! tense ! Pos) ++
          td (forms ! Plain ! tense ! Neg) ++
          td (forms ! Resp ! tense ! Pos) ++
          td (forms ! Resp ! tense ! Neg)) ;

  polarityRow : Str -> (Polarity => Str) -> Str = \label,forms ->
    tr (th label ++ td (forms ! Pos) ++ td (forms ! Neg)) ;

  clauseTable : Cl -> Str = \clause ->
    heading2 "Plain" ++ clauseStyleTable Plain clause ++
    heading2 "Respectful" ++ clauseStyleTable Resp clause ;

  clauseStyleTable : Style -> Cl -> Str = \style,clause ->
    frameTable (
      tr (th "Tense" ++ th "が positive" ++ th "が negative" ++
          th "は positive" ++ th "は negative") ++
      clauseRow "Present" TPres style clause ++
      clauseRow "Past" TPast style clause ++
      clauseRow "Future" TFut style clause
      ) ;

  clauseRow : Str -> TTense -> Style -> Cl -> Str =
    \label,tense,style,clause ->
      tr (th label ++
          td (clause.s ! Ga ! style ! tense ! Pos) ++
          td (clause.s ! Ga ! style ! tense ! Neg) ++
          td (clause.s ! Wa ! style ! tense ! Pos) ++
          td (clause.s ! Wa ! style ! tense ! Neg)) ;

}
