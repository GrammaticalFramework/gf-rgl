--# -path=.:../abstract:../common

incomplete concrete DocumentationIceFunctor of Documentation = CatIce ** open 
  Terminology,  -- the interface that generates different documentation languages
  ResIce,
  ParadigmsIce,
  (G = GrammarIce),
  (S = SyntaxIce),
  (L = LexiconIce),
  Prelude,
  HTML
in {
flags coding=utf8 ;


lincat
  Inflection = {t : Str; s1,s2 : Str} ;
  Definition = {s : Str} ;
  Document = {s : Str} ;
  Tag = {s : Str} ;

oper
  heading : N -> Str = \n -> (nounHeading n).s ;

lin
  InflectionN, InflectionN2, InflectionN3 = \noun -> {
    t  = "s" ;
    s1 = heading1 (intag "i" (noun.s ! Sg ! Free ! Nom) ++ ":" ++ heading noun_Category ++ 
                   case noun.g of {
                     Masc   => "("+heading masculine_Parameter+")" ; 
                     Fem    => "("+heading feminine_Parameter+")" ;
                     Neutr  => "("+heading neuter_Parameter+")"
                   }) ;
    s2 = frameTable ( 
           tr (intagAttr "th" "rowspan=2" "" ++ intagAttr "th" "colspan=2" (heading singular_Parameter) ++ intagAttr "th" "colspan=2" (heading plural_Parameter)) ++
           tr (        th (heading indefinite_Parameter) ++ th (heading definite_Parameter) ++ th (heading indefinite_Parameter) ++ th (heading definite_Parameter)) ++
           tr (th (heading nominative_Parameter) ++ td (noun.s ! Sg ! Free ! Nom) ++ td (noun.s ! Sg ! Suffix ! Nom) ++ td (noun.s ! Pl ! Free ! Nom) ++ td (noun.s ! Pl ! Suffix ! Nom)) ++
           tr (th (heading accusative_Parameter) ++ td (noun.s ! Sg ! Free ! Acc) ++ td (noun.s ! Sg ! Suffix ! Acc) ++ td (noun.s ! Pl ! Free ! Acc) ++ td (noun.s ! Pl ! Suffix ! Acc)) ++
           tr (th (heading dative_Parameter)     ++ td (noun.s ! Sg ! Free ! Dat) ++ td (noun.s ! Sg ! Suffix ! Dat) ++ td (noun.s ! Pl ! Free ! Dat) ++ td (noun.s ! Pl ! Suffix ! Dat)) ++
           tr (th (heading genitive_Parameter)   ++ td (noun.s ! Sg ! Free ! Gen) ++ td (noun.s ! Sg ! Suffix ! Gen) ++ td (noun.s ! Pl ! Free ! Gen) ++ td (noun.s ! Pl ! Suffix ! Gen))
           )
    } ;

  InflectionA, InflectionA2 = \adj ->
    let
      gforms : (Number -> Gender -> Case -> AForm) -> Case -> Str = \d,c ->
        td (adj.s ! d Sg Masc  c) ++
        td (adj.s ! d Sg Fem   c) ++
	td (adj.s ! d Sg Neutr c) ++
        td (adj.s ! d Pl Masc  c) ++
        td (adj.s ! d Pl Fem   c) ++
	td (adj.s ! d Pl Neutr c) ;
	
      dtable : Str -> (Number -> Gender -> Case -> AForm) -> Str = \s,d ->
        paragraph (heading2 s ++ frameTable ( 
          tr (intagAttr "th" "rowspan=2" []  ++
	         th (heading masculine_Parameter) ++ th (heading feminine_Parameter) ++ th (heading neuter_Parameter) ++
                 th (heading masculine_Parameter) ++ th (heading feminine_Parameter) ++ th (heading neuter_Parameter)) ++
	  tr (intagAttr "th" "colspan=3" (heading singular_Parameter) ++ intagAttr "th" "colspan=3" (heading plural_Parameter)) ++
          tr (th (heading nominative_Parameter) ++ gforms d Nom) ++
          tr (th (heading accusative_Parameter) ++ gforms d Acc) ++
          tr (th (heading dative_Parameter)     ++ gforms d Dat) ++
	  tr (th (heading genitive_Parameter)   ++ gforms d Gen)
          )) ;
	  
    in { t  = "a" ;
         s1 = heading1 (intag "i" (adj.s ! APosit Strong Sg Masc Nom) ++ ":" ++ (nounHeading adjective_Category).s) ;
         s2 =
	   dtable (heading positive_Parameter ++ heading strong_Parameter) (APosit Strong) ++
	   dtable (heading positive_Parameter ++ heading weak_Parameter) (APosit Weak) ++
	   dtable (heading comparative_Parameter) ACompar ++
	   dtable (heading superlative_Parameter ++ heading strong_Parameter) (ASuperl Strong) ++
	   dtable (heading superlative_Parameter ++ heading weak_Parameter) (ASuperl Weak)
       } ;

{-
  InflectionAdv adv = {
    t  = "adverb" ;
    s1 = heading1 (heading adverb_Category) ;
    s2 = paragraph adv.s
    } ;

  InflectionPrep p = {
    t  = "präp" ;
    s1 = heading1 (heading preposition_Category) ;
    s2 = paragraph (S.mkAdv (lin Prep p) (S.mkNP S.a_Det L.computer_N)).s
    } ;
-}

  InflectionV v = {
    t  = "v" ;
    s1 = heading1 (intag "i" (v.s ! VInf Active) ++ ":" ++ heading verb_Category) ++  
         paragraph (verbExample (S.mkCl S.she_NP v)) ;
    s2 = inflVerb v
    } ;

  InflectionV2 v = {
    t  = "v" ;
    s1 = heading1 (intag "i" (v.s ! VInf Active) ++ ":" ++ heading verb_Category) ++  
         paragraph (verbExample (S.mkCl S.she_NP v S.i_NP)) ;
    s2 = inflVerb v
    } ;

  InflectionV3 v = {
    t  = "v" ;
    s1 = heading1 (intag "i" (v.s ! VInf Active) ++ ":" ++ heading verb_Category) ++  
         paragraph (verbExample (S.mkCl S.she_NP v S.it_NP S.i_NP)) ;
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
    s1 = heading1 (heading verb_Category) ;
    s2 = inflVerb v
    } ;

  InflectionV2Q v = {
    t  = "v" ;
    s1 = heading1 (heading verb_Category) ;
    s2 = inflVerb v
    } ;

  InflectionV2A v = {
    t  = "v" ;
    s1 = heading1 (heading verb_Category) ;
    s2 = inflVerb v
    } ;

  InflectionVV v = {
    t  = "v" ;
    s1 = heading1 (heading verb_Category) ++  
         paragraph (verbExample (S.mkCl S.she_NP (lin VV v) (S.mkVP (L.sleep_V)))) ;
    s2 = inflVerb v
    } ;

  InflectionVS v = {
    t  = "v" ;
    s1 = heading1 (heading verb_Category) ;
    s2 = inflVerb v
    } ;

  InflectionVQ v = {
    t  = "v" ;
    s1 = heading1 (heading verb_Category) ;
    s2 = inflVerb v
    } ;

  InflectionVA v = {
    t  = "v" ;
    s1 = heading1 (heading verb_Category) ;
    s2 = inflVerb v
    } ;

lin
  NoDefinition   t     = {s=t.s};
  MkDefinition   t d   = {s="<p><b>Definierung:</b>"++t.s++d.s++"</p>"};
  MkDefinitionEx t d e = {s="<p><b>Definierung:</b>"++t.s++d.s++"</p><p><b>Beispiel:</b>"++e.s++"</p>"};

  MkDocument d i e = ss (i.s1 ++ d.s ++ i.s2 ++ paragraph e.s) ;  -- explanation appended in a new paragraph
  MkTag i = ss i.t ;

oper 
  verbExample : CatIce.Cl -> Str = \cl ->
     (S.mkUtt cl).s 
     ++ ";" ++ (S.mkUtt (S.mkS S.anteriorAnt cl)).s  --# notpresent
     ;

  inflVerb : Verb -> Str = \verb -> 
     let 
       vfin : VForm -> Str = \f ->
         verb.s ! f ;
       gforms : Voice -> Number -> Person -> Str = \v,n,p -> 
         td (vfin (VPres v Indicative  n p)) ++
         td (vfin (VPres v Subjunctive n p)) ++
         td (vfin (VPast v Indicative  n p)) ++
         td (vfin (VPast v Subjunctive n p)) 
         ;
       voiceTable : Voice -> Str = \voice -> frameTable (
          tr (intagAttr "th" "colspan=2" "" ++ intagAttr "th" "colspan=2" (heading present_Parameter) ++  intagAttr "th" "colspan=2" (heading past_Parameter)) ++  
          tr (intagAttr "th" "colspan=2" "" ++ th (heading indicative_Parameter) ++ th (heading conjunctive_Parameter)  ++  
                       th (heading indicative_Parameter) ++ th (heading conjunctive_Parameter))  ++  
          tr (intagAttr "th" "rowspan=3" (heading singular_Parameter) ++ th "1"  ++ gforms voice Sg P1) ++
          tr (                                                th "2"  ++ gforms voice Sg P2) ++
          tr (                                                th "3"  ++ gforms voice Sg P3) ++
          tr (intagAttr "th" "rowspan=3" (heading plural_Parameter)   ++ th "1"  ++ gforms voice Pl P1) ++
          tr (                                                th "2"  ++ gforms voice Pl P2) ++
          tr (                                                th "3"  ++ gforms voice Pl P3) ++
	  tr (intagAttr "th" "colspan=2" (heading infinitive_Parameter) ++ intagAttr "td" "colspan=4" (vfin (VInf voice))) ++
	  tr (intagAttr "th" "colspan=2" (heading supine_Parameter)     ++ intagAttr "td" "colspan=4" (vfin (VSup voice)))
          ) ;
     in
     heading2 (heading active_Parameter) ++ voiceTable Active ++
     heading2 (heading middle_Parameter) ++ voiceTable Middle
     ;

}
