--# -path=.:../abstract:../common

incomplete concrete DocumentationRusFunctor of Documentation = CatRus ** open
  Terminology, -- the interface
  ResRus,
  ParadigmsRus,
  (G = GrammarRus),
  (S = SyntaxRus),
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
         inflNoun (makeNFFromAF adj Neut Inanimate)
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
         paragraph (verbExample (S.mkCl S.she_NP v S.we_NP (lin S (ss "...")))) ;
    s2 = inflVerb v
    } ;

  InflectionV2Q v = {
    t  = "v" ;
    s1 = heading1 (heading verb_Category) ++
         paragraph (verbExample (S.mkCl S.she_NP v S.we_NP (lin QS (ss "...")))) ;
    s2 = inflVerb v
    } ;

  InflectionV2A v = {
    t  = "v" ;
    s1 = heading1 (heading verb_Category) ++
         paragraph (verbExample (S.mkCl S.she_NP v S.we_NP L.beautiful_A)) ;
    s2 = inflVerb v
    } ;

  InflectionVV v = {
    t  = "v" ;
    s1 = heading1 (heading verb_Category) ++
         paragraph (verbExample (S.mkCl S.she_NP v (S.mkVP (L.sleep_V)))) ;
    s2 = inflVerb v
    } ;

  InflectionVS v = {
    t  = "v" ;
    s1 = heading1 (heading verb_Category) ++
         paragraph (verbExample (S.mkCl S.she_NP v (lin S (ss "...")))) ;
    s2 = inflVerb v
    } ;

  InflectionVQ v = {
    t  = "v" ;
    s1 = heading1 (heading verb_Category) ++
         paragraph (verbExample (S.mkCl S.she_NP v (lin QS (ss "...")))) ;
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
  inflVerb : CatRus.V -> Str = \verb0 ->
     verb0.inf ;  -- TODO: make documentation

  inflNoun : NounForms -> Str = \nf ->
    frameTable (
          tr (th "" ++ th (heading singular_Parameter) ++ th (heading plural_Parameter)) ++
          tr (th (heading nominative_Parameter) ++ td (nf.snom) ++ td (nf.pnom)) ++
          tr (th (heading genitive_Parameter) ++ td (nf.sgen) ++ td (nf.pgen)) ++
          tr (th (heading dative_Parameter) ++ td (nf.sdat) ++ td (nf.pdat)) ++
          tr (th (heading accusative_Parameter) ++ td (nf.sacc) ++ td (nf.pacc)) ++
          tr (th ("творительный") ++ td (nf.sacc) ++ td (nf.pacc)) ++
          tr (th ("предложный") ++ td (nf.sacc) ++ td (nf.pacc)) ++
          tr (th (heading partitive_Parameter) ++ td (nf.sptv) ++ td ("-")) ++
          tr (th ("местный") ++ td (nf.sloc) ++ td ("-")) ++
          tr (th ("звательный") ++ td (nf.svoc) ++ td ("-"))
          ) ;

lin
  NoDefinition   t     = {s=t.s};
  MkDefinition   t d   = {s="<p><b>Определение:</b>"++t.s++d.s++"</p>"};
  MkDefinitionEx t d e = {s="<p><b>Определение:</b>"++t.s++d.s++"</p><p><b>Пример:</b>"++e.s++"</p>"};

lin
  MkDocument d i e = ss (i.s1 ++ d.s ++ i.s2 ++ paragraph e.s) ;  -- explanation appended in a new paragraph
  MkTag i = ss (i.t) ;

{- --# notpresent
-}

}
