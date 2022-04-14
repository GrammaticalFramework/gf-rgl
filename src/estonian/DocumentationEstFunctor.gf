--# -path=.:../abstract:../common

incomplete concrete DocumentationEstFunctor of Documentation = CatEst ** open
  Terminology, -- the interface
  ResEst,
  ParadigmsEst,
  (G = GrammarEst),
  (S = SyntaxEst),
  (L = LexiconEst),
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

  caseplus2nf : N -> ResEst.Number -> CasePlus -> Str = \noun,num,cas ->
    noun.s ! NCase num cas.c ++ cas.suf ;

  caseplus2af : (AForm => Str) -> ResEst.Number -> CasePlus -> Str = \adj,num,cas ->
    adj ! AN (NCase num cas.c) ++ cas.suf ;

lin
  InflectionN, InflectionN2, InflectionN3 = \noun -> {
    t  = "s" ;
    s1 = heading1 (heading noun_Category) ;
    s2 = inflNoun (caseplus2nf noun)
    } ;

  InflectionA, InflectionA2 = \adj ->
    let posit  : (AForm => Str) = adj.s ! Posit ;
       	compar : (AForm => Str) = adj.s ! Compar ;
      	superl : (AForm => Str) = adj.s ! Superl ;
     in
    {   t  = "a" ;
        s1 = heading1 (heading adjective_Category) ;
        s2 = inflNoun (caseplus2af posit) ++
             heading2 (heading comparative_Parameter) ++
             inflNoun (caseplus2af compar) ++
             heading2 (heading superlative_Parameter) ++
             inflNoun (caseplus2af superl)
    } ;

  InflectionAdv, InflectionAdV, InflectionAdA, InflectionAdN = \adv -> {
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
  verbExample : CatEst.Cl -> Str = \cl -> (S.mkUtt cl).s ;
{-
-} --# notpresent
  inflVerb : CatEst.V -> Str = \verb ->
     let
       --verb = sverb2verbSep verb0 ;
       vfin : ResEst.VForm -> Str = \f ->
         verb.s ! f ;
       vinf : ResEst.InfForms -> Str = \if ->
         applyInfFormsV if verb.s ;

       nounNounHeading : Parameter -> Parameter -> Str = \n1,n2 ->
         (S.mkUtt (G.PossNP (S.mkCN n1) (S.mkNP n2))).s ;
     in
       heading3 (nounNounHeading present_Parameter indicative_Parameter) ++
       frameTable (
         tr (th "" ++
             th (heading singular_Parameter) ++
             th (heading plural_Parameter)
             ++ th (heading passive_Parameter) --# notpresent
            ) ++
         tr (th "1.p"  ++ td (vfin (Presn Sg P1)) ++ td (vfin (Presn Pl P1))
             ++ intagAttr "td" "rowspan=3" (vfin (PassPresn True)) --# notpresent
            ) ++
         tr (th "2.p"  ++ td (vfin (Presn Sg P2)) ++ td (vfin (Presn Pl P2))) ++
         tr (th "3.p"  ++ td (vfin (Presn Sg P3)) ++ td (vfin (Presn Pl P3))) ++
         tr (th (heading negative_Parameter) ++
             intagAttr "td" "colspan=2 align=center" (vfin (Imper Sg)) ++ td (vfin (PassPresn False)))
         ) ++
       heading3 (nounNounHeading past_Parameter indicative_Parameter) ++
       frameTable (
         tr (th "" ++
             th (heading singular_Parameter) ++
             th (heading plural_Parameter)
             ++ th (heading passive_Parameter) --# notpresent
            ) ++
         tr (th "1.p"  ++ td (vfin (Impf Sg P1)) ++ td (vfin (Impf Pl P1))
             ++ intagAttr "td" "rowspan=3" (vfin (PassImpf True))) ++
         tr (th "2.p"  ++ td (vfin (Impf Sg P2)) ++ td (vfin (Impf Pl P2))) ++
         tr (th "3.p"  ++ td (vfin (Impf Sg P3)) ++ td (vfin (Impf Pl P3))) ++
         tr (th (heading negative_Parameter) ++
             td (vfin (PastPart Act)) ++
             td (vfin (PastPart Pass)) ++
             td (vfin (PassImpf False)))
         ) ++
       heading3 (nounNounHeading present_Parameter conditional_Parameter) ++
       frameTable (
         tr (th "" ++
             th (heading singular_Parameter) ++
             th (heading plural_Parameter)
             ++ th (heading passive_Parameter) --# notpresent
            ) ++
         tr (th "1.p"  ++ td (vfin (Condit Sg P1)) ++ td (vfin (Condit Pl P1))
             ++ intagAttr "td" "rowspan=3" "TODO pass condit (nt loetaks)" --# notpresent
            ) ++
         tr (th "2.p"  ++ td (vfin (Condit Sg P2)) ++ td (vfin (Condit Pl P2))) ++
         tr (th "3.p"  ++ td (vfin (Condit Sg P3)) ++ td (vfin (Condit Pl P3)))
         ) ++
       heading3 (nounNounHeading present_Parameter quotative_Parameter) ++
       frameTable (
         tr (th "" ++
             th (heading singular_Parameter) ++
             th (heading plural_Parameter)
             ++ th (heading passive_Parameter) --# notpresent
            )  ++
         tr (th "isik."  ++ td (vfin (Quotative Act))
             ++ intagAttr "td" "rowspan=3" (vfin (Quotative Act)) --# notpresent
            ) ++
         tr (th "umbis."  ++ td (vfin (Quotative Pass)) ++ td (vfin (Quotative Pass))
         )) ++
       heading3 (nounNounHeading present_Parameter imperative_Parameter) ++
       frameTable (
         tr (th "" ++
             th (heading singular_Parameter) ++
             th (heading plural_Parameter)
             ++ th (heading passive_Parameter) --# notpresent
            ) ++
         tr (th "1.p"  ++ td "" ++ td (vfin ImperP1Pl) ++
             intagAttr "td" "rowspan=3" (vfin ImperPass)) ++
         tr (th "2.p"  ++ td (vfin (Imper Sg)) ++ td (vfin (Imper Pl))) ++
         tr (th "3.p"  ++ td (vfin (ImperP3))  ++ td (vfin ImperP3))
         ) ++
       heading2 (nounPluralHeading nominal_form_ParameterType).s ++
       heading3 (heading infinitive_Parameter) ++
       frameTable (
         tr (intagAttr "th" "rowspan=2" "da" ++
             th (heading nominative_Parameter) ++ td (vinf InfDa)) ++
         tr (th (heading inessive_Parameter) ++ td (vinf InfDes))  ++

         tr (intagAttr "th" "rowspan=6" "ma" ++
             th (heading illative_Parameter) ++ td (vinf InfMa)) ++
         tr (th (heading inessive_Parameter) ++ td (vinf InfMas)) ++
         tr (th (heading elative_Parameter)  ++ td (vinf InfMast)) ++
         tr (th (heading abessive_Parameter) ++ td (vinf InfMata)) ++
         tr (th (heading abessive_Parameter) ++ td (vinf InfMaks)) ++
         tr (th (heading translative_Parameter) ++ td (vinf InfMine))

       ) ++
       heading3 (heading participle_Parameter) ++
       frameTable (
         tr (intagAttr "th" "rowspan=2" (heading present_Parameter) ++
             th (heading active_Parameter) ++
             td (vfin (PresPart Act))) ++
         tr (th (heading passive_Parameter) ++
             td (vfin (PresPart Pass))) ++

         tr (intagAttr "th" "rowspan=2" (heading perfect_Parameter) ++
             th (heading active_Parameter) ++
             td (vfin (PastPart Act ))) ++

         tr (th (heading passive_Parameter) ++
             td (vfin (PastPart Pass )))
          ) ; --}

  inflNoun : (Number -> CasePlus -> Str) -> Str = \nouns ->
    frameTable (
          tr (th ""          ++ th (heading singular_Parameter)               ++ th (heading plural_Parameter)) ++
          tr (th (heading nominative_Parameter)  ++ td (nouns Sg Nominative)  ++ td (nouns Pl Nominative)) ++
          tr (th (heading genitive_Parameter)    ++ td (nouns Sg Genitive)    ++ td (nouns Pl Genitive)) ++
          tr (th (heading partitive_Parameter)   ++ td (nouns Sg Partitive)   ++ td (nouns Pl Partitive)) ++
          tr (th (heading translative_Parameter) ++ td (nouns Sg Translative) ++ td (nouns Pl Translative)) ++
          tr (th (heading essive_Parameter)      ++ td (nouns Sg Essive)      ++ td (nouns Pl Essive)) ++
          tr (th (heading inessive_Parameter)    ++ td (nouns Sg Inessive)    ++ td (nouns Pl Inessive)) ++
          tr (th (heading elative_Parameter)     ++ td (nouns Sg Elative)     ++ td (nouns Pl Elative)) ++
          tr (th (heading illative_Parameter)    ++ td (nouns Sg Illative)    ++ td (nouns Pl Illative)) ++
          tr (th (heading adessive_Parameter)    ++ td (nouns Sg Adessive)    ++ td (nouns Pl Adessive)) ++
          tr (th (heading ablative_Parameter)    ++ td (nouns Sg Ablative)    ++ td (nouns Pl Ablative)) ++
          tr (th (heading allative_Parameter)    ++ td (nouns Sg Allative)    ++ td (nouns Pl Allative)) ++
          tr (th (heading abessive_Parameter)    ++ td (nouns Sg Abessive)    ++ td (nouns Pl Abessive)) ++
          tr (th (heading comitative_Parameter)  ++ td (nouns Sg Comitative)  ++ td (nouns Pl Comitative)) ++
          tr (th (heading instructive_Parameter) ++ td (nouns Sg Terminative) ++ td (nouns Pl Terminative))
          ) ;

lin
  NoDefinition   t     = {s=t.s};
  MkDefinition   t d   = {s="<p><b>Definitsioon:</b>"++t.s++d.s++"</p>"};
  MkDefinitionEx t d e = {s="<p><b>Definitsioon:</b>"++t.s++d.s++"</p><p><b>Eeskuju:</b>"++e.s++"</p>"};

lin
  MkDocument b i e = ss (i.s1 ++ paragraph b.s ++ i.s2 ++ paragraph e.s) ;  -- explanation appended in a new paragraph
  MkTag i = ss (i.t) ;

{- --# notpresent
-}

}
