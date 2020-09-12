--# -path=.:../abstract:../common

incomplete concrete DocumentationRusFunctor of Documentation = CatRus ** open
  Terminology, -- the interface
  ResRus,
  ParadigmsRus,
  (G = GrammarRus),
  (S = SyntaxRus),
  (ST = StructuralRus),
  (L = LexiconRus),
  Prelude,
  HTML
in {

lincat
  Inflection = {t : Str; s1,s2 : Str} ;
  Definition = {s : Str} ;
  Document = {s : Str} ;
  Tag = {s : Str} ;

{-
-} --# notpresent

oper
  heading : N -> Str = \n -> (nounHeading n).s ;

lin
  InflectionN, InflectionN2, InflectionN3 = \noun -> {
    t  = "s" ;
    s1 = heading1 (heading noun_Category) ;
    s2 = inflNoun noun
    } ;

  InflectionA, InflectionA2 = \adj -> {
    t  = "a" ;
    s1 = heading1 (heading adjective_Category) ;
    s2 = heading2 (heading feminine_Parameter) ++
         inflNoun (makeNFFromAF adj Fem Inanimate) ++
         heading2 (heading masculine_Parameter) ++
         inflNoun (makeNFFromAF adj Masc Inanimate) ++
         heading2 (heading neuter_Parameter) ++
         inflNoun (makeNFFromAF adj Neut Inanimate) ++
         heading2 (heading comparative_Parameter) ++
         frameTable (tr (tr (adj.comp)))
    } ;

  InflectionAdv adv = {
    t  = "adv" ;
    s1 = heading1 (heading adverb_Category) ;
    s2 = paragraph adv.s
    } ;

  InflectionPrep p = {
    t  = "prep" ;
    s1 = heading1 (heading preposition_Category) ;
    s2 = paragraph ((S.mkAdv (lin Prep p) S.it_NP).s ++ ";" ++ (S.mkAdv (lin Prep p) S.we_NP).s)
    } ;

  InflectionV v = {
    t  = "v" ;
    s1 = heading1 (heading verb_Category) ++
         paragraph (verbExample (S.mkCl S.she_NP v)) ;
    s2 = inflVerb v
    } ;

  InflectionV2 v = {
    t  = "v" ;
    s1 = heading1 (heading verb_Category) ++
         paragraph (verbExample (S.mkCl S.she_NP v S.something_NP)) ;
    s2 = inflVerb v
    } ;

  InflectionV3 v = {
    t  = "v" ;
    s1 = heading1 (heading verb_Category) ++
         paragraph (verbExample (S.mkCl S.she_NP v S.something_NP S.something_NP)) ;
    s2 = inflVerb v
    } ;

  InflectionV2V v = {
    t  = "v" ;
    s1 = heading1 (heading verb_Category) ++
         paragraph (verbExample (S.mkCl S.she_NP v S.we_NP (S.mkVP (L.sleep_V)))) ;
    s2 = inflVerb v
    } ;

  InflectionV2S v = {
    t  = "v" ;
    s1 = heading1 (heading verb_Category) ++
         paragraph (verbExample (S.mkCl S.she_NP v S.we_NP (lin S {s : Mood=>Str = \\m=>"..."}))) ;
    s2 = inflVerb v
    } ;

  InflectionV2Q v = {
    t  = "v" ;
    s1 = heading1 (heading verb_Category) ++
         paragraph (verbExample (S.mkCl S.she_NP v S.we_NP (lin QS {s: QForm=>Str = \\m=>"..."}))) ;
    s2 = inflVerb v
    } ;

  InflectionV2A v = {
    t  = "v" ;
    s1 = heading1 (heading verb_Category) ++
         paragraph (verbExample (S.mkCl S.she_NP v S.we_NP L.beautiful_A)) ;
    s2 = inflVerb v
    } ;

  InflectionVV vv = {
    t  = "v" ;
    s1 = heading1 (heading verb_Category) ++
         paragraph (verbExample (S.mkCl S.she_NP vv (S.mkVP (L.sleep_V)))) ;
    s2 = inflVerb vv.v
    } ;

  InflectionVS v = {
    t  = "v" ;
    s1 = heading1 (heading verb_Category) ++
         paragraph (verbExample (S.mkCl S.she_NP v (lin S {s : Mood=>Str = \\m=>"..."}))) ;
    s2 = inflVerb v
    } ;

  InflectionVQ v = {
    t  = "v" ;
    s1 = heading1 (heading verb_Category) ++
         paragraph (verbExample (S.mkCl S.she_NP v (lin QS {s : Mood=>Str = \\m=>"..."}))) ;
    s2 = inflVerb v
    } ;

  InflectionVA v = {
    t  = "v" ;
    s1 = heading1 (heading verb_Category) ++
         paragraph (verbExample (S.mkCl S.she_NP v L.beautiful_A)) ;
    s2 = inflVerb v
    } ;

oper
  verbExample : CatRus.Cl -> Str = \cl -> (S.mkUtt cl).s ;
{-
-} --# notpresent
  inflVerb : CatRus.V -> Str = \v ->
    let fut : Agr=>Str = \\a => verbFutAgree v a in
    let pres : Agr=>Str = \\a => verbPresAgree v a in
    let past : Agr=>Str = \\a => verbPastAgree v a "" in
    let imp : Agr=>Str = \\a => ((verbImperativeAgree v a).p1 ++ (verbImperativeAgree v a).p2) in
    let ppp : GenNum=>Str = \\gn => shortPastPassPart v gn in
    case v.asp of {Imperfective => "несовершенного вида" ; Perfective => "совершенного вида"} ++ "," ++
    case v.refltran of {
      Refl => "возвратный" ;
      Intrans => "непереходный" ;
      Trans => "переходный"} ++ "," ++
    heading2 (heading infinitive_Parameter) ++
    frameTable (
      tr (td v.inf ++ td v.infrefl)
    ) ++
    frameTable (
      tr (th (heading person_ParameterType) ++ th (heading number_ParameterType) ++ th (heading gender_ParameterType)
        ++ th (heading past_Parameter) ++ th (heading present_Parameter) ++ th (heading future_Parameter) ++ th (heading imperative_Parameter)) ++
      tr (th (heading person1_Parameter) ++ th (heading singular_Parameter) ++ th (heading masculine_Parameter)
        ++ td (past ! Ag (GSg Masc) P1) ++ td (pres ! Ag (GSg Masc) P1) ++ td(fut ! Ag (GSg Masc) P1) ++ td (imp ! Ag (GSg Masc) P1))  ++
      tr (th "" ++ th "" ++ th (heading feminine_Parameter)
        ++ td (past ! Ag (GSg Fem) P1) ++ td "" ++ td "") ++
      tr (th "" ++ th "" ++ th (heading neuter_Parameter)
        ++ td (past ! Ag (GSg Neut) P1) ++ td "" ++ td "") ++
      tr (th "" ++ th (heading plural_Parameter) ++ th ""
        ++ td (past ! Ag GPl P1) ++ td (pres ! Ag GPl P1) ++ td(fut ! Ag GPl P1)  ++ td (imp ! Ag GPl P1)) ++
      tr (th (heading person2_Parameter) ++ th (heading singular_Parameter) ++ th (heading masculine_Parameter)
        ++ td (past ! Ag (GSg Masc) P2) ++ td (pres ! Ag (GSg Masc) P2) ++ td (fut ! Ag (GSg Masc) P2) ++ td (imp ! Ag (GSg Masc) P2) ) ++
      tr (th "" ++ th "" ++ th (heading feminine_Parameter)
        ++ td (past ! Ag (GSg Fem) P2) ++ td "" ++ td "") ++
      tr (th "" ++ th "" ++ th (heading neuter_Parameter)
        ++ td (past ! Ag (GSg Neut) P2) ++ td "" ++ td "") ++
      tr (th "" ++ th (heading plural_Parameter) ++ th ""
        ++ td (past ! Ag GPl P2) ++ td (pres ! Ag GPl P2) ++ td(fut ! Ag GPl P2)  ++ td (imp ! Ag GPl P2)) ++
      tr (th (heading person3_Parameter) ++ th (heading singular_Parameter) ++ th (heading masculine_Parameter)
        ++ td (past ! Ag (GSg Masc) P3) ++ td (pres ! Ag (GSg Masc) P3) ++ td(fut ! Ag (GSg Masc) P3) ) ++
      tr (th "" ++ th "" ++ th (heading feminine_Parameter)
        ++ td (past ! Ag (GSg Fem) P3) ++ td "" ++ td "") ++
      tr (th "" ++ th "" ++ th (heading neuter_Parameter)
        ++ td (past ! Ag (GSg Neut) P3) ++ td "" ++ td "") ++
      tr (th "" ++ th (heading plural_Parameter) ++ th ""
        ++ td (past ! Ag GPl P3) ++ td (pres ! Ag GPl P3) ++ td (fut ! Ag GPl P3) ++ td (imp ! Ag GPl P3) )
    ) ++
    heading2 ("краткие причастия прошедшего времени") ++
      case v.refltran of {
        Trans => frameTable (
          tr (th (heading masculine_Parameter) ++ th (heading feminine_Parameter) ++ th (heading neuter_Parameter)
             ++ th (heading plural_Parameter)) ++
          tr (td (ppp ! (GSg Masc)) ++ td (ppp ! (GSg Fem)) ++ td (ppp ! (GSg Neut)) ++ td (ppp ! GPl))
          ) ;
        _ => "-"
        } ++
    heading2 ("деепричастия") ++
    frameTable (
      tr (th (heading past_Parameter) ++ th (heading present_Parameter)) ++
      tr (td (v.prtr ++ verbRefl v) ++ td (v.ptr ++ verbRefl v))
      )
    ;

  inflNoun : NounForms -> Str = \nf ->
    frameTable (
          tr (th "" ++ th (heading singular_Parameter) ++ th (heading plural_Parameter)) ++
          tr (th (heading nominative_Parameter) ++ td (nf.snom) ++ td (nf.pnom)) ++
          tr (th (heading genitive_Parameter) ++ td (nf.sgen) ++ td (nf.pgen)) ++
          tr (th (heading dative_Parameter) ++ td (nf.sdat) ++ td (nf.pdat)) ++
          tr (th (heading accusative_Parameter) ++ td (nf.sacc) ++ td (nf.pacc)) ++
          tr (th ("творительный") ++ td (nf.sins) ++ td (nf.pins)) ++
          tr (th ("предложный") ++ td (nf.sprep) ++ td (nf.pprep)) ++
          tr (th (heading partitive_Parameter) ++ td (nf.sptv) ++ td ("-")) ++
          tr (th ("местный") ++ td (nf.sloc) ++ td ("-")) ++
          tr (th ("звательный") ++ td (nf.svoc) ++ td ("-"))
          ) ;

lin
  -- : String -> Definition ;
  NoDefinition   t     = {s=t.s};
  -- : String -> String -> Definition ;
  MkDefinition   t d   = {s="<p><b>" ++ (heading formGr_N) ++ "</b>"++t.s++d.s++"</p>"};
  -- : String -> String -> String -> Definition ;
  MkDefinitionEx t d e = {s="<p><b>" ++ (heading formGr_N) ++ "</b>"++t.s++d.s++"</p><p><b>" ++ (heading exampleGr_N) ++":</b>"++e.s++"</p>"};

lin
  -- : Definition -> Inflection -> String -> Document ;
  MkDocument d i e = ss (i.s1 ++ d.s ++ i.s2 ++ paragraph e.s) ;  -- explanation appended in a new paragraph
  -- : Inflection -> Tag ;
  MkTag i = ss (i.t) ;

{- --# notpresent
-}

}
