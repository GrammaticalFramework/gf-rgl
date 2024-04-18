	--# -path=.:../abstract:../common

-- documenting Arabic inflection
---- so far with English titles and tags, AR 2024-01-31

concrete DocumentationAra of Documentation = CatAra ** open
  ResAra,
  Prelude,
  HTML in {

lincat
  Inflection = {t : Str; s1,s2 : Str} ;
  Definition = {s : Str} ;
  Document = {s : Str} ;
  Tag      = {s : Str} ;

lin
  InflectionN, InflectionN2, InflectionN3 = \noun -> {
    t  = "n" ;
    s1 = heading1 ("Noun" ++ case noun.g of {
                               Masc  => "(masculine)";
                               Fem   => "(feminine)"
                             }) ;
    s2 = frameTable (
           tr (th ""   ++ th "nominative"               ++ th "genitive"    ++ th "accusative") ++
           tr (th "singular" ++ td (noun.s ! Sg ! Indef ! Nom) ++ td (noun.s ! Sg ! Indef ! Gen) ++
	       td (noun.s ! Sg ! Indef ! Acc)) ++
           tr (th "dual" ++ td (noun.s ! Dl ! Indef ! Nom) ++ td (noun.s ! Dl ! Indef ! Gen) ++
	       td (noun.s ! Dl ! Indef ! Acc)) ++
           tr (th "plural" ++ td (noun.s ! Pl ! Indef ! Nom) ++ td (noun.s ! Pl ! Indef ! Gen) ++
	       td (noun.s ! Pl ! Indef ! Acc)) 
         )
    } ;

  InflectionPN, InflectionGN, InflectionSN = \pn -> { ---- TODO: special for LN, GN, SN
    t  = "pn" ;
    s1 = heading1 ("Proper Name" ++
                    case pn.g of {
                      Masc  => "(masculine)";
                      Fem   => "(feminine)"
                    }) ;
    s2 = frameTable (
           tr (th "nominative"        ++ th "genitive"        ++ th "accusative") ++
           tr (td (pn.s ! Nom) ++ td (pn.s ! Gen) ++ td (pn.s ! Acc))
         )
    } ;

  InflectionA, InflectionA2 = \adj -> {
    t  = "a" ;
    s1 = heading1 "Adjective" ;
    s2 = frameTable (
           tr (th ""       ++ th "Nominative" ++ th "Genitive" ++ th "Accusative") ++
           tr (th "Positive Masculine Singular" ++
	     td (adj.s ! APosit Masc Sg Indef Nom) ++
	     td (adj.s ! APosit Masc Sg Indef Gen) ++
	     td (adj.s ! APosit Masc Sg Indef Acc)
	     ) ++
           tr (th "Positive Masculine Dual" ++
	     td (adj.s ! APosit Masc Dl Indef Nom) ++
	     td (adj.s ! APosit Masc Dl Indef Gen) ++
	     td (adj.s ! APosit Masc Dl Indef Acc)
	     ) ++
           tr (th "Positive Masculine Plural" ++
	     td (adj.s ! APosit Masc Pl Indef Nom) ++
	     td (adj.s ! APosit Masc Pl Indef Gen) ++
	     td (adj.s ! APosit Masc Pl Indef Acc)
	     ) ++
           tr (th "Positive Feminine Singular" ++
	     td (adj.s ! APosit Fem Sg Indef Nom) ++
	     td (adj.s ! APosit Fem Sg Indef Gen) ++
	     td (adj.s ! APosit Fem Sg Indef Acc)
	     ) ++
           tr (th "Positive Feminine Dual" ++
	     td (adj.s ! APosit Fem Dl Indef Nom) ++
	     td (adj.s ! APosit Fem Dl Indef Gen) ++
	     td (adj.s ! APosit Fem Dl Indef Acc)
	     ) ++
           tr (th "Positive Feminine Plural" ++
	     td (adj.s ! APosit Fem Pl Indef Nom) ++
	     td (adj.s ! APosit Fem Pl Indef Gen) ++
	     td (adj.s ! APosit Fem Pl Indef Acc)
	     ) ++
           tr (th "Comparative" ++
	     td (adj.s ! AComp Indef Nom) ++
	     td (adj.s ! AComp Indef Gen) ++
	     td (adj.s ! AComp Indef Acc)
	     )
         )
    } ;

  InflectionAdv, InflectionAdV, InflectionAdA, InflectionAdN = \adv -> {
    t = "adv" ;
    s1= heading1 "Adverb" ;
    s2= paragraph (adv.s) ;
    s3= ""
    } ;

  InflectionPrep = \prep -> {
    t = "prep" ;
    s1= heading1 "Preposition" ;
    s2= paragraph (prep.s) ;
    s3= ""
    } ;

  InflectionV,
    InflectionV3,  InflectionV2A,  InflectionV2Q,  InflectionV2S,  InflectionV2V, InflectionVA,  InflectionVQ,  InflectionVS,  InflectionVV ----  
    = \v -> {
    t = "v" ;
    s1= heading1 "Verb" ++
        paragraph (v.s ! (VPerf Act (Per3 Masc Sg)) ++
	           pp "subject");
    s2= inflVerb v
    } ;

  InflectionV2 v = {
    t = "v" ;
    s1= heading1 "Verb" ++
        paragraph (v.s ! (VPerf Act (Per3 Masc Sg)) ++
	           pp "subject" ++
	           pp "object") ;
    s2= inflVerb v
    } ;
{-
  InflectionV3 v = {
    t = "v" ;
    s1= heading1 "Verb" ++
        paragraph (pp "subject" ++
                   v.s ! VInf ++ v.p ++
                   v.c2 ++ pp "arg1" ++
                   v.c3 ++ pp "arg2") ;
    s2= inflVerb v
    } ;

  InflectionV2V v = {
    t = "v" ;
    s1= heading1 "Verb" ++
        paragraph (pp "subject" ++
                   v.s ! VInf ++ v.p ++
                   v.c2 ++ pp "object" ++
                   v.c3 ++ case v.typ of {
                             VVAux => pp "verb" ;
                             VVInf => "to" ++ pp "verb" ;
                             VVPresPart => pp "verb+ing"
                           }) ;
    s2= inflVerb v
    } ;

  InflectionV2S v = {
    t = "v" ;
    s1= heading1 "Verb" ++
        paragraph (pp "subject" ++
                   v.s ! VInf ++ v.p ++
                   v.c2 ++ pp "object" ++
                   "that" ++ pp "sentence") ;
    s2= inflVerb v
    } ;

  InflectionV2Q v = {
    t = "v" ;
    s1= heading1 "Verb" ++
        paragraph (pp "subject" ++
                   v.s ! VInf ++ v.p ++
                   pp "question") ;
    s2= inflVerb v
    } ;

  InflectionV2A v = {
    t = "v" ;
    s1= heading1 "Verb" ++
        paragraph (pp "subject" ++
                   v.s ! VInf ++ v.p ++
                   v.c2 ++ pp "object" ++
                   pp "adjective") ;
    s2= inflVerb v
    } ;

  InflectionVV v = {
    t = "v" ;
    s1= heading1 "Verb" ++
        paragraph (pp "subject" ++
                   v.s ! VVF VInf ++ v.p ++
                   case v.typ of {
                     VVAux => pp "verb" ;
                     VVInf => "to" ++ pp "verb" ;
                     VVPresPart => pp "verb+ing"
                   }) ;
    s2= frameTable (
          tr (th "infitive"      ++ td (v.s ! VVF VInf)) ++
          tr (th "present"       ++ td (v.s ! VVF VPres ++ "&#160;" ++ v.s ! VVPresNeg)) ++
          tr (th "past"          ++ td (v.s ! VVF VPast ++ "&#160;" ++ v.s ! VVPastNeg)) ++ --# notpresent
          tr (th "past part."    ++ td (v.s ! VVF VPPart)) ++
          tr (th "present part." ++ td (v.s ! VVF VPresPart))
        )
    } ;

  InflectionVS v = {
    t = "v" ;
    s1= heading1 "Verb" ++
        paragraph (pp "subject" ++
                   v.s ! VInf ++ v.p ++
                   "that" ++ pp "sentence") ;
    s2= inflVerb v
    } ;

  InflectionVQ v = {
    t = "v" ;
    s1= heading1 "Verb" ++
        paragraph (pp "subject" ++
                   v.s ! VInf ++ v.p ++
                   pp "question") ;
    s2= inflVerb v
    } ;

  InflectionVA v = {
    t = "v" ;
    s1= heading1 "Verb" ++
        paragraph (pp "subject" ++
                   v.s ! VInf ++ v.p ++
                   pp "adjective") ;
    s2= inflVerb v
    } ;
-}

oper
  inflVerb : Verb -> Str = \verb ->
    frameTable (
      tr (th "Active" ++ th "Perfect" ++ th "Imperfect Indicative" ++ th "Conjunctive" ++ th "Jussive" ++ th "Imperative") ++
      inflVerbRow "3 Masculine Singular" (Per3 Masc Sg) verb ++
      inflVerbRow "3 Feminine Singular"  (Per3 Fem  Sg) verb ++
      inflVerbRow "2 Masculine Singular" (Per2 Masc Sg) verb ++
      inflVerbRow "2 Feminine Singular"  (Per2 Fem  Sg) verb ++
      inflVerbRow "1 Singular"           (Per1 Sing) verb ++
      inflVerbRow "3 Masculine Dual"     (Per3 Masc Dl) verb ++
      inflVerbRow "3 Feminine Dual"      (Per3 Fem  Dl) verb ++
      inflVerbRow "2 Dual"               (Per2 Masc Dl) verb ++
      inflVerbRow "3 Masculine Plural"   (Per3 Masc Pl) verb ++
      inflVerbRow "3 Feminine Plural"    (Per3 Fem  Pl) verb ++
      inflVerbRow "2 Masculine Plural"   (Per2 Masc Pl) verb ++
      inflVerbRow "2 Feminine Plural"    (Per2 Fem  Pl) verb ++
      inflVerbRow "1 Plural"             (Per1 Plur) verb
    ) ;

  inflVerbRow : (h : Str) -> (pgn : PerGenNum) -> (verb : Verb) -> Str = \h, pgn, verb ->
      tr (th h  ++
          td (verb.s ! (VPerf Act pgn)) ++
          td (verb.s ! (VImpf Ind Act pgn)) ++
          td (verb.s ! (VImpf Cnj Act pgn)) ++
          td (verb.s ! (VImpf Jus Act pgn)) ++
	  td (imperativeForm pgn verb)
	  ) ;

   imperativeForm : PerGenNum -> Verb -> Str = \pgn, verb ->
     case pgn of {
       Per2 g n => verb.s ! VImp g n ;
       _ => "-"
       } ;
    

  pp : Str -> Str = \s -> "&lt;"+s+"&gt;";

lin
  NoDefinition   t     = {s=t.s};
  MkDefinition   t d   = {s="<p><b>Definition:</b>"++t.s++d.s++"</p>"};
  MkDefinitionEx t d e = {s="<p><b>Definition:</b>"++t.s++d.s++"</p><p><b>Example:</b>"++e.s++"</p>"};

lin
  MkDocument d i e = {s = i.s1 ++ d.s ++ i.s2 ++ paragraph e.s} ;
  MkTag i = {s = i.t} ;

}
