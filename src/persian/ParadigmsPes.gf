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
  Animacy : Type ; -- Argument to mkN
  animate : Animacy ;
  inanimate : Animacy ;

  Number : Type ; -- Argument to
  singular : Number ;
  plural : Number ;


--2 Nouns

  mkN = overload {
    mkN : (sg : Str) -> N -- Takes singular form, returns an inanimate noun with ها as the plural form
      = \sg -> mkN01 sg inanimate ;
    mkN : (sg : Str) -> Animacy -> N -- Takes singular form and animacy. Inaminate plural ها. Animate plural ان or an allomorph of it (یان or گان) depending on the singular form.
      = \sg,ani -> case ani of {
          Inanimate => mkN01 sg ani ;
          Animate   => mkN02 sg ani } ;
    -- mkN : (sg,pl : Str) -> N -- Takes singular and plural form, returns an inanimate noun
    --   = \sg,pl -> M.mkN sg pl inanimate ;
    mkN : (sg,pl : Str) -> Animacy -> N -- Worst-case constructor: takes singular and plural forms and animacy. Use for e.g. loanwords with Arabic plural.
      = \sg,pl,ani -> M.mkN sg pl ani
  } ;

  mkN2 : overload {
    mkN2 : (key : N) -> (to : Str) -> N2 -- Takes a noun and a complementiser, returns a N2.
  } ;

  mkN3 : overload {
    mkN3 : (distance : N) -> (from,to : Str) -> N3 -- Takes a noun and two complementisers, returns a N3.
  } ;

-- Compound Nouns

  cmpdN = overload {
    cmpdN : Str -> N -> N -- Compound noun with an invariable modifier /before/ the head. NB. no ezāfe.
      = mkCmpdNoun1 ; --  e.g. تخم مرغ 'chicken /egg/'
    cmpdN : N -> Str -> N -- Compound noun with an invariable modifier /after/ the head. NB. no ezāfe.
      = mkCmpdNoun2   --  e.g. مأمور پلیس '/officer/ police'.
    } ;

-- Proper names
  mkPN : Str -> Animacy -> PN -- Proper noun with given animacy
    = \str,ani -> lin PN {s = str ; animacy = ani} ;

-- Determiner

  mkDet = overload {
    mkDet : Str -> Number -> Det -- Takes a string, number (sg/pl) and returns a det which is not a numeral
      = \s1,n -> lin Det (makeDet s1 n False);
    mkDet : Str -> Number -> Bool -> Det -- Takes a string, number (sg/pl) and a Boolean for whether the det is a numeral
      = \s1,n,b -> lin Det (makeDet s1 n b)
  };

 {-

-- AdN
  mkAdN : Str -> AdN = \s -> ss s ;
-}
--2 Adjectives

  mkA : overload {
    mkA : Str -> A ; -- Regular adjective, same form for adjective and adverb.
    mkA : (adj,adv : Str) -> A -- Different forms for adjective and adverb.
  } ;

  mkA2 : (married,to : Str) -> A2 -- Takes string and complementiser, returns A2.
    = \a,c -> lin A2 (mkAdj a a ** {c2 = c}) ;

--2 Verbs
  mkV = overload {
    mkV : (inf : Str) -> V -- Takes infinitive. Use for predictable verbs: if it ends in vowel+دن, the present stem removes the vowel as well. If it ends in consonant+تن or consonant+دن, present stem only removes تن/دن.
      = regV ;
    mkV : (inf,pres : Str) -> V -- Takes infinitive and present root. Use for unpredictable verbs, e.g. دانستن with present stem دان, or irregular, e.g. کردن with present stem کن.
      = \s1, s2 -> lin V (mkVerb s1 s2)
  } ;

  haveVerb : V = lin V M.haveVerb ; -- The verb "have", to be used for light verb constructions: e.g. compoundV "دوست" haveVerb. NB. this has different imperative and VV forms from StructuralPes.have_V2.
  beVerb : V = lin V M.beVerb ; -- The verb "be", to be used for light verb constructions: e.g. compoundV "عاشق" beVerb.

  mkV2 : overload {
    mkV2 : Str -> V2 ; -- Predictable V2 out of string. No preposition, را for direct object.
    mkV2 : V -> V2 ; -- V2 out of V. No preposition, را for direct object.
    mkV2 : (listen : V) -> (to : Prep) -> V2 -- V2 out of V. Use given preposition, no را for direct object.
  } ;


  mkV3 : V -> Str -> Str -> V3 ; -- Takes a verb and two prepositions (can be empty), e.g. speak, with, about
    mkV3 v p q = lin V3 (v ** {c2 = p ; c3 = q}) ;

  mkV2V : V -> (cV : Str) -> (cN : Str) -> (isAux : Bool) -> V2V ; -- Verb, complementiser for the verb, complementiser for the noun, whether it's auxiliary.
    mkV2V v s1 s2 b = lin V2V (v ** {isAux = b ; c1 = s1 ; c2 = s2}) ;

-- compund verbs
  compoundV : overload {
    compoundV : Str -> V -> V -- Invariable prefix to a verb, e.g. compoundV "دوست" haveVerb
  } ;

  invarV : Str -> V -- for verbs like  " بایستن " ("must"), which don't inflect
    = \s -> lin V {s = \\_ => s} ;

----2 Adverbs
  mkAdv : Str -> Adv = \str -> lin Adv {s = str} ; -- Takes a string, returns an adverb.

----2 Prepositions

  mkPrep : Str -> Prep ; -- Takes a string, returns a preposition.
  mkPrep str = lin Prep {s = str};
{-
--3 Determiners and quantifiers

--  mkQuant : overload {
--    mkQuant : Pron -> Quant ;
--    mkQuant : (no_sg, no_pl, none_sg, : Str) -> Quant ;
--  } ;
-}


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
--2 Definitions of paradigms

-- The definitions should not bother the user of the API. So they are
-- hidden from the document.

  Number = ResPes.Number ;
  singular = Sg ;
  plural = Pl;

  Animacy = ResPes.Animacy ;
  animate = Animate ;
  inanimate = Inanimate ;

-- Removed mkV_1, mkV_2, mkN01 and mkN02 from public API, still available for
-- any applications that open ParadigmsPes. /IL 2019-02-08
  mkV_1 : Str -> V
    = \s -> lin V (mkVerb1 s) ;
  mkV_2 : Str -> V
    = \s -> lin V (mkVerb2 s) ;

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

  mk2Conj : Str -> Str -> Number -> Conj = \x,y,n ->
    lin Conj (sd2 x y ** {n = n}) ;

  mkA = overload {
    mkA : Str -> A -- Regular adjective, same adj and adv forms.
       = \str -> lin A (mkAdj str str);
    mkA : Str-> Str -> A -- Takes adj and adv forms
       = \str,adv -> lin A (mkAdj str adv);
    mkA : Str -> Str -> A2 -- Takes string and complementiser, returns A2. Hidden from public API, confusing naming. /IL
      = \a,c -> lin A2 (mkAdj a a ** {c2 = c})
   } ;


   -- Personal Pronouns
  personalPN : Str -> Number -> Person -> Pron -- Hidden from public API, confusing naming. /IL
    = \str,nn,p -> lin Pron {s = str ; a = Ag nn p ; ps = str};
   {-
    -- Demonstrative Pronouns
     demoPN : Str -> Str -> Str -> Quant =
       \s1,s2,s3 -> let n = makeDemonPronForm s1 s2 s3 in {s = n.s ; a = defaultAgr ; lock_Quant = <>};
   -- Interrogative pronouns
    mkIP : (x1,x2,x3,x4:Str) -> Number -> Gender -> IP =
     \s1,s2,s3,s4,n,g -> let p = mkIntPronForm s1 s2 s3 s4 in { s = p.s ; n = n ; g = g ;  lock_IP = <>};
   -}

   mkCmpdNoun1 : Str -> N -> N
     = \s,noun -> noun ** {s =\\ez,n => s ++ noun.s ! ez ! n};
   mkCmpdNoun2 : N -> Str -> N
     = \noun,s -> noun ** {s =\\ez,n => noun.s ! ez ! n  ++ s};

  compoundV = overload {
    compoundV : Str -> V -> V
      = \s,v -> v ** {s = \\vf => s ++ v.s ! vf} ;
    compoundV : Str -> V2 -> V -- hidden from public API
      = \s,v -> lin V {s = \\vf => s ++ v.s ! vf} ;
  };

  regV : Str -> V = \inf ->
    let pres : Str = case inf of {
          stem + ("ی"|"ا"|"و") + "دن" => stem ;
          stem + ("تن"|"دن")           => stem }
    in lin V (mkVerb inf pres) ;

  mkV2 = overload {
    mkV2 : Str -> V2 -- Predictable V2 with
      = \s -> lin V2 (regV s ** {c2 = {s = [] ; ra = "را" ; c = VTrans}}) ;
    mkV2 : V -> V2
      = \v -> lin V2 (v ** {c2 = {s = [] ; ra = "را" ; c = VTrans}}) ;
    mkV2 : V -> Prep -> V2
      = \v,p -> lin V2 (v ** {c2 = {ra = [] ; s = p.s ; c = VTrans}}) ;
    mkV2 : V -> Str -> V2
      = \v,ra -> lin V2 (v ** {c2 = {ra = ra ; s = [] ; c = VTrans}}) ;
    mkV2 : V -> Str -> Bool -> V2
      = \v,p,b -> lin V2 (v ** {c2 = {ra = [] ; s = p ; c = VTrans}}) ;
    } ;

  mkN2 = overload {
    mkN2 : N -> Str -> N2
      = \n,c -> lin N2 (n ** {c = c}) ;
    mkN2 : N -> Prep -> Str -> N2 -- hidden from puclic API
      = \n,p,c -> lin N2 (n ** {c = p.s ; c2 = c}) -- there is no c2
  } ;

  mkN3 = overload {
    mkN3 : N -> Str -> Str -> N3
      = \n,p,q -> lin N3 (n ** {c2=p ; c3=q}) ;
    mkN3 : N -> Prep -> Str -> Str -> N3 -- hidden from public API
      = \n,p,q,r -> lin N3 (n ** {c2 = p.s ; c3 = q ; c4 = r}) -- there is no c4
  } ;

  mkQuant = overload {
--    mkQuant : Pron -> Quant = \p -> {s = \\_,_,c => p.s!c ;a = p.a ; lock_Quant = <>};
    mkQuant :  Str -> Str -> Quant -- hidden from public API
      = \sg,pl -> makeQuant sg pl;
  } ;

}
