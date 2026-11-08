--# -path=.:../abstract:../common
concrete DocumentationSwa of Documentation = CatSwa ** open
  ResSwa,ParamX,Prelude,
  HTML in {

lincat
  Inflection = {t : Str; s1,s2 : Str} ;
  Definition = {s : Str} ;
  Document = {s : Str} ;
  Tag      = {s : Str} ;

lin
  InflectionN, InflectionN2, InflectionN3 = \noun -> {
    t = "n" ;
    s1 = heading1 ("Noun (class" ++ className noun.g ++ BIND ++ ")") ;
    s2 = frameTable (
           tr (th "" ++ th "Sg" ++ th "Pl") ++
           tr (th "Nom" ++ td (noun.s ! Sg ! Nom) ++ td (noun.s ! Pl ! Nom)) ++
           tr (th "Loc" ++ td (noun.s ! Sg ! Loc) ++ td (noun.s ! Pl ! Loc))
         )
    } ;

  InflectionPN, InflectionLN, InflectionGN, InflectionSN = \name -> {
    t = "pn" ;
    s1 = heading1 ("Name (class" ++ className name.g ++ BIND ++ ")") ;
    s2 = frameTable (
           tr (th "Nom" ++ td (name.s ! Nom)) ++
           tr (th "Loc" ++ td (name.s ! Loc))
         )
    } ;

  InflectionA, InflectionA2 = \adj -> {
    t = "a" ;
    s1 = heading1 "Adjective" ;
    s2 = adjectiveTable adj.s
    } ;

  InflectionAdv, InflectionAdV, InflectionAdA, InflectionAdN = \adv -> {
    t = "adv" ;
    s1 = heading1 "Adverb" ;
    s2 = paragraph adv.s
    } ;

  InflectionPrep = \prep -> {
    t = "prep" ;
    s1 = heading1 "Preposition" ;
    s2 = prepositionTable prep.s
    } ;

  InflectionV, InflectionV2, InflectionVV, InflectionVS, InflectionVQ,
  InflectionVA, InflectionV3, InflectionV2V, InflectionV2S,
  InflectionV2Q, InflectionV2A = \verb -> {
    t = "v" ;
    s1 = heading1 "Verb" ;
    s2 = verbTable verb
    } ;

  InflectionCl = \clause -> {
    t = "cl" ;
    s1 = heading1 "Clause" ;
    s2 = frameTable (
           tr (th "Tense" ++ th "Positive" ++ th "Negative") ++
           clauseRow "Present" Pres Simul clause ++
           clauseRow "Present anterior" Pres Anter clause ++
           clauseRow "Past" Past Simul clause ++
           clauseRow "Past anterior" Past Anter clause ++
           clauseRow "Future" Fut Simul clause ++
           clauseRow "Future anterior" Fut Anter clause
         )
    } ;

lin
  NoDefinition   t     = {s=t.s};
  MkDefinition   t d   = {s="<p><b>Definition:</b>"++t.s++d.s++"</p>"};
  MkDefinitionEx t d e = {s="<p><b>Definition:</b>"++t.s++d.s++"</p><p><b>Example:</b>"++e.s++"</p>"};

lin
  MkDocument d i e = {s = i.s1 ++ d.s ++ i.s2 ++ paragraph e.s} ;
  MkTag i = {s = i.t} ;

oper
  className : Gender -> Str = \g -> case g of {
    G1 => "I" ; G2 => "II" ; G3 => "III" ; G4 => "IV" ;
    G5 => "V" ; G6 => "VI" ; G7 => "VII" ; G8 => "VIII" ;
    G9 => "IX" ; G10 => "X" ; G11 => "XI" ; G12 => "XII" ;
    G13 => "XIII"
    } ;

  adjectiveTable : (AForm => Str) -> Str = \forms ->
    frameTable (
      tr (th "Class" ++ th "Sg" ++ th "Pl") ++
      adjectiveRow G1 forms ++ adjectiveRow G2 forms ++
      adjectiveRow G3 forms ++ adjectiveRow G4 forms ++
      adjectiveRow G5 forms ++ adjectiveRow G6 forms ++
      adjectiveRow G7 forms ++ adjectiveRow G8 forms ++
      adjectiveRow G9 forms ++ adjectiveRow G10 forms ++
      adjectiveRow G11 forms ++ adjectiveRow G12 forms ++
      adjectiveRow G13 forms
    ) ;

  adjectiveRow : Gender -> (AForm => Str) -> Str = \g,forms ->
    tr (th (className g) ++
        td (forms ! AAdj g Sg) ++ td (forms ! AAdj g Pl)) ;

  prepositionTable : (Number => Gender => Str) -> Str = \forms ->
    frameTable (
      tr (th "Class" ++ th "Sg" ++ th "Pl") ++
      prepositionRow G1 forms ++ prepositionRow G2 forms ++
      prepositionRow G3 forms ++ prepositionRow G4 forms ++
      prepositionRow G5 forms ++ prepositionRow G6 forms ++
      prepositionRow G7 forms ++ prepositionRow G8 forms ++
      prepositionRow G9 forms ++ prepositionRow G10 forms ++
      prepositionRow G11 forms ++ prepositionRow G12 forms ++
      prepositionRow G13 forms
    ) ;

  prepositionRow : Gender -> (Number => Gender => Str) -> Str = \g,forms ->
    tr (th (className g) ++ td (forms ! Sg ! g) ++ td (forms ! Pl ! g)) ;

  verbTable : Verb -> Str = \verb ->
    heading2 "Non-finite forms" ++
    frameTable (
      tr (th "" ++ th "Positive" ++ th "Negative" ++
          th "Passive positive" ++ th "Passive negative") ++
      tr (th "Infinitive" ++
          td (verb.s ! Pos ! VInf) ++ td (verb.s ! Neg ! VInf) ++
          td (verb.passive ! Pos ! VInf) ++ td (verb.passive ! Neg ! VInf)) ++
      imperativeRow "Imperative singular" Sg verb ++
      imperativeRow "Imperative plural" Pl verb
    ) ++
    heading2 "Finite forms" ++ finiteTable verb.s ++
    heading2 "Passive finite forms" ++ finiteTable verb.passive ;

  imperativeRow : Str -> Number -> Verb -> Str = \label,n,verb ->
    tr (th label ++
        td (verb.s ! Pos ! VImp n) ++ td (verb.s ! Neg ! VImp n) ++
        td (verb.passive ! Pos ! VImp n) ++ td (verb.passive ! Neg ! VImp n)) ;

  finiteTable : (Polarity => VForm => Str) -> Str = \forms ->
    frameTable (
      tr (th "Agreement" ++
          th "Pres +" ++ th "Pres -" ++
          th "Past +" ++ th "Past -" ++
          th "Fut +" ++ th "Fut -") ++
      finiteRow "1 Sg" G1 Sg P1 forms ++
      finiteRow "2 Sg" G1 Sg P2 forms ++
      finiteRow "3 Sg (I)" G1 Sg P3 forms ++
      finiteRow "1 Pl" G1 Pl P1 forms ++
      finiteRow "2 Pl" G1 Pl P2 forms ++
      finiteRow "3 Pl (I)" G1 Pl P3 forms ++
      finiteClassRows G2 forms ++ finiteClassRows G3 forms ++
      finiteClassRows G4 forms ++ finiteClassRows G5 forms ++
      finiteClassRows G6 forms ++ finiteClassRows G7 forms ++
      finiteClassRows G8 forms ++ finiteClassRows G9 forms ++
      finiteClassRows G10 forms ++ finiteClassRows G11 forms ++
      finiteClassRows G12 forms ++ finiteClassRows G13 forms
    ) ;

  finiteClassRows : Gender -> (Polarity => VForm => Str) -> Str = \g,forms ->
    finiteRow (className g ++ " Sg") g Sg P3 forms ++
    finiteRow (className g ++ " Pl") g Pl P3 forms ;

  finiteRow : Str -> Gender -> Number -> Person ->
              (Polarity => VForm => Str) -> Str = \label,g,n,p,forms ->
    tr (th label ++
        td (forms ! Pos ! VPres g n p) ++ td (forms ! Neg ! VPres g n p) ++
        td (forms ! Pos ! VPast g n p) ++ td (forms ! Neg ! VPast g n p) ++
        td (forms ! Pos ! VFut g n p) ++ td (forms ! Neg ! VFut g n p)) ;

  clauseRow : Str -> ParamX.Tense -> Anteriority ->
              {s : ParamX.Tense => Anteriority => Polarity => Str} -> Str =
    \label,t,a,clause ->
      tr (th label ++ td (clause.s ! t ! a ! Pos) ++
          td (clause.s ! t ! a ! Neg)) ;

}
