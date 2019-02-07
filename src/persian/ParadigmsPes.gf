--# -path=.:../abstract:../prelude:../common
--
----1 Persian Lexical Paradigms

resource ParadigmsPes = open
  Predef,
  Prelude,
  ResPes,
  (M=MorphoPes),
  CatPes
  in {

  flags optimize=all ;
   coding = utf8;

--2 Parameters

oper
  animate : Animacy ;
  inanimate : Animacy ;
  singular : Number;
  plural : Number;

  singular = Sg ; plural = Pl;

  animate = Animate ; inanimate = Inanimate ; --i
  mkN01 : (sg : Str) -> Animacy -> Noun ; -- Takes singular form and animacy, forms plural with ها
  mkN01 sg ani =
    let pl = zwnj sg "ها" ; -- Using zero-width non-joiner, defined in ResPes
    in M.mkN sg pl ani ;

  mkN02 : (sg : Str) -> Animacy -> Noun ; -- Takes singular form and animacy, pattern matches singular and forms plural with either گان, یان or ان
  mkN02 str ani = case last str of {
    "ه"       => M.mkN str (init str + "گان") ani ;
    ("ا"|"و") => M.mkN str (str + "یان")      ani ;
    _         => M.mkN str (str + "ان")       ani
  };

  mkN = overload {
    mkN : (sg : Str) -> N -- Takes singular form, returns an inanimate noun with ها as the plural form
      = \sg -> mkN01 sg inanimate ;
    mkN : (sg,pl : Str) -> N -- Takes singular and plural form, returns an inanimate noun
      = \sg,pl -> M.mkN sg pl inanimate ;
    mkN : (sg,pl : Str) -> Animacy -> N -- Worst-case constructor: takes singular and plural forms and animacy
      = \sg,pl,ani -> M.mkN sg pl ani
  } ;
{-

--2 Nouns


  mkN2 : N -> Prep -> Str -> N2;
  mkN2 = \n,p,c -> n ** {lock_N2 = <> ; c2 = p.s ; c3 = c } ;

  mkN3 : N -> Prep -> Str -> Str-> N3 ;
  mkN3 = \n,p,q,r -> n ** {lock_N3 = <> ; c2 = p.s ; c3 = q ; c4 = r} ;
-}
-- Compound Nouns

  mkCmpdNoun1 : Str -> N -> N
    = \s,noun -> noun ** {s =\\ez,n => s ++ noun.s ! ez ! n};
  mkCmpdNoun2 : N -> Str -> N
    = \noun,s -> noun ** {s =\\ez,n => noun.s ! ez ! n  ++ s};


-- Proper names
   mkPN : Str -> Animacy -> PN
    = \str,ani -> lin PN {s = str ; animacy = ani} ;


-- Personal Pronouns
  personalPN : Str -> Number -> Person -> Pron
    = \str,nn,p -> lin Pron {s = str ; a = Ag nn p ; ps = str};
{-
-- Demonstration Pronouns
  demoPN : Str -> Str -> Str -> Quant =
    \s1,s2,s3 -> let n = makeDemonPronForm s1 s2 s3 in {s = n.s ; a = defaultAgr ; lock_Quant = <>};
-- Determiner
-}
  mkDet = overload {
    mkDet : Str -> Number -> Det
      = \s1,n -> lin Det (makeDet s1 n False);
    mkDet : Str -> Number -> Bool -> Det
      = \s1,n,b -> lin Det (makeDet s1 n b)
  };
 {-
-- Intergative pronouns
  mkIP : (x1,x2,x3,x4:Str) -> Number -> Gender -> IP =
   \s1,s2,s3,s4,n,g -> let p = mkIntPronForm s1 s2 s3 s4 in { s = p.s ; n = n ; g = g ;  lock_IP = <>};

-- AdN
  mkAdN : Str -> AdN = \s -> ss s ;
-}
--2 Adjectives

  mkA = overload {
    mkA : Str -> A
      = \str -> lin A (mkAdj str str);
    mkA : Str-> Str -> A
      = \str,adv -> lin A (mkAdj str adv);
    mkA : Str -> Str -> A2
    = \a,c -> lin A2 (mkAdj a a ** {c2 = c})
  } ;

--2 Verbs
 mkV : Str -> Str -> V
      = \s1, s2 -> mkVerb s1 s2 ** {lock_V = <>} ;
  -- mkVerb takes both the Infinitive and the present root(root2) and is applied for iregular verbs
  haveVerb : V = lin V M.haveVerb ;
  beVerb : V = lin V M.beVerb ;
  mkV_1 : Str -> V
      = \s -> mkVerb1 s ** {lock_V = <>} ;

  mkV_2 : Str -> V
      = \s -> mkVerb2 s ** {lock_V = <>} ;

  mkV2 = overload {
--    mkV2 : Str -> V2
--      = \s -> mkV s **  {c2 = {s = [] ; c = VTrans} ; lock_V2 = <>} ;
    mkV2 : V -> V2
      = \v -> v ** {c2 = {s = [] ; ra = [] ; c = VTrans} ; lock_V2 = <>} ;
    mkV2 : V -> Str -> V2
      = \v,ra -> v ** {c2 = {ra = ra ; s = [] ; c = VTrans} ; lock_V2 = <>} ;
    mkV2 : V -> Str -> Bool -> V2
      = \v,p,b -> v ** {c2 = {ra = [] ; s = p ; c = VTrans} ; lock_V2 = <>} ;
    } ;

  mkV3 : V -> Str -> Str -> V3;
    mkV3 v p q = lin V3 (v ** {c2 = p ; c3 = q}) ;
  mkV2V : V -> Str -> Str -> Bool -> V2V ;
    mkV2V v s1 s2 b = lin V2V (v ** {isAux = b ; c1 = s1 ; c2 = s2}) ;

-- compund verbs
  compoundV = overload {
    compoundV : Str -> V -> V
      = \s,v -> v ** {s = \\vf => s ++ v.s ! vf} ;
    compoundV : Str -> V2 -> V
      = \s,v -> lin V {s = \\vf => s ++ v.s ! vf} ;
  };

  invarV : Str -> V -- for verbs like  " بایستن " ("must"), which don't inflect
    = \s -> lin V {s = \\_ => s} ;

----2 Adverbs
  mkAdv : Str -> Adv = \str -> lin Adv {s = str} ;

----2 Prepositions

  mkPrep : Str -> Prep ;
    mkPrep str = lin Prep {s = str};
{-
--3 Determiners and quantifiers

--  mkQuant : overload {
--    mkQuant : Pron -> Quant ;
--    mkQuant : (no_sg, no_pl, none_sg, : Str) -> Quant ;
--  } ;
-}
  mkQuant = overload {
--    mkQuant : Pron -> Quant = \p -> {s = \\_,_,c => p.s!c ;a = p.a ; lock_Quant = <>};
    mkQuant :  Str -> Str -> Quant
      = \sg,pl -> makeQuant sg pl;
  } ;

--2 Conjunctions
  mkConj = overload {
    mkConj : Str -> Conj                  -- and (plural agreement)
      = \y -> mk2Conj [] y plural ;
    mkConj : Str -> Number -> Conj        -- or (agrement number given as argument)
      = \y,n -> mk2Conj [] y n ;
    mkConj : Str -> Str -> Conj          -- both ... and (plural)
      = \x,y -> mk2Conj x y plural ;
    mkConj : Str -> Str -> Number -> Conj -- either ... or (agrement number given as argument)
     = mk2Conj
  } ;



--.



  mk2Conj : Str -> Str -> Number -> Conj = \x,y,n ->
   lin Conj (sd2 x y ** {n = n}) ;


}
