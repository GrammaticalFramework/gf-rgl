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
  human : Animacy ; -- e.g. /mkN "خواهر" human/ to get the plural خواهران.
  nonhuman : Animacy ; -- default animacy for mkN, not needed unless you want to make the animacy explicit or force a plural with ها.

  Number : Type ; -- Argument to mkDet and mkConj
  singular : Number ; -- e.g. mkConj "یا" singular
  plural : Number ; -- e.g. mkConj "و" plural

  VVForm : Type ; -- Argument to mkVV
  subjunctive : VVForm ; -- The verbal complement of VV is in subjunctive
  indicative : VVForm ;  -- The verbal complement of VV is in indicative

  Mod : Type ; -- Argument to mkDet and mkPrep
  ezafe : Mod ; -- e.g. mkPrep "برای" ezafe
  -- poss : Mod ; -- TODO is this needed?
  -- clitic : Mod ; -- TODO is this needed?
--2 Nouns

  mkN : overload {
    mkN : (sg : Str) -> N ; -- Takes singular form, returns a nonhuman noun with ها as the plural form.
    mkN : (sg : Str) -> Animacy -> N ; -- Takes singular form and animacy. Nonhuman plural ها. Human plural ان or an allomorph of it (یان or گان) depending on the singular form.
    mkN : (sg,pl : Str) -> Animacy -> N ; -- Worst-case constructor: takes singular, plural and animacy. Use for loanwords with Arabic plural, human plurals with ها and nonhuman plurals with ان or its allomorphs.
    mkN : (possStem : Str) -> N -> N -- Noun with an unexpected possessive stem, e.g. مه where ه is a consonant, not vowel.
  } ;

  mkN2 : overload {
    mkN2 : (key : N) -> (to : Str) -> N2 ; -- Takes a noun and a complementiser as a string, returns a N2.
    mkN2 : (key : N) -> (to : Prep) -> N2 -- Takes a noun and a complementiser as a Prep, returns a N2.
  } ;

  mkN3 : overload {
    mkN3 : (distance : N) -> (from,to : Str) -> N3 ; -- Takes a noun and two complementisers as strings, returns a N3.
    mkN3 : (distance : N) -> (from,to : Prep) -> N3 -- Takes a noun and two complementisers as Preps, returns a N3.
  } ;

-- Compound Nouns

  cmpdN = overload {
    cmpdN : Str -> N -> N -- Compound noun with an invariable modifier /before/ the head. NB. no ezāfe.
      = mkCmpdNoun1 ; --  e.g. تخم مرغ 'chicken /egg/'
    cmpdN : N -> Str -> N -- Compound noun with an invariable modifier /after/ the head. NB. no ezāfe.
      = mkCmpdNoun2 ;  --  e.g. مأمور پلیس '/officer/ police'.
    cmpdN : N -> N -> N -- Compound noun with ezafe (Nی N)
      = \n1,n2 -> n1 ** {
           s = \\n,m => n1.s ! n ! Ezafe ++ n2.s ! Sg ! m ;
           isCmpd = IsCmpd} ;
    } ;

-- Proper names
  mkPN : Str -> Animacy -> PN -- Proper noun with given animacy
    = \str,ani -> lin PN {s = str ; animacy = ani} ;

-- Determiner

  mkDet = overload {
    mkDet : Str -> Number -> Det -- Takes a string, number (sg/pl) and returns a det which is not a numeral
      = \s,n -> lin Det (makeDet s n False False);
    mkDet : Str -> Number -> (isNum : Bool) -> Det -- As above + a Boolean for whether the det is a numeral
      = \s,n,b -> lin Det (makeDet s n b False) ;
    mkDet : Str -> Number -> (isNum, isNeg : Bool) -> Det -- As above + a Boolean for whether the det is negative
      = \s,n,nu,ne -> lin Det (makeDet s n nu ne) ;
    mkDet : Str -> Number -> (isNum, isNeg : Bool) -> Mod -> Det -- As above + Mod for which form the determiner expects its argument to be (default bare)
      = \s,n,nu,ne,m -> lin Det (makeDet s n nu ne ** {mod=m})
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
  prefixA : A -> A ; -- Adjective that comes before the noun

  mkA2 : (married,to : Str) -> A2 -- Takes string and complementiser, returns A2.
    = \a,c -> lin A2 (mkAdj a a ** {c2 = c}) ;

--2 Verbs
  mkV = overload {
    mkV : (inf : Str) -> V -- Takes infinitive. Use for predictable verbs: if it ends in vowel+دن, the present stem removes the vowel as well. If it ends in consonant+تن or consonant+دن, present stem only removes تن/دن.
      = regV ;
    mkV : (inf,pres : Str) -> V -- Takes infinitive and present root. Use for unpredictable verbs, e.g. دانستن with present stem دان, or irregular, e.g. کردن with present stem کن.
      = \s1, s2 -> lin V (mkVerb s1 s2) ;
    mkV : Str -> V -> V -- Invariable prefix to a verb, e.g. mkV "دوست" haveVerb
      = compoundV ;
  } ;


  invarV : Str -> V  -- no inflection at all
    = \s -> lin V (M.invarV s);
  defV : (inf,pres,past : Str) -> V -- no personal forms, but past/present difference, like بایستن ('must'),
    = \i,pr,pa -> lin V (M.defectiveVerb i pr pa) ;

  haveVerb : V  -- The verb "have", to be used for light verb constructions: e.g. compoundV "دوست" haveVerb. NB. this has different imperative and VV forms from StructuralPes.have_V2.
    = lin V M.haveVerb ;
  beVerb : V  -- The verb "be", to be used for light verb constructions: e.g. compoundV "عاشق" beVerb.
    = lin V M.beVerb ;
  doVerb : V -- The verb "do", to be used for light verb constructions. In passive, is replaced by شدن.
    = lin V M.doVerb ;

  mkV2 : overload {
    mkV2 : Str -> V2 ; -- Predictable V2 out of string. No preposition, را for direct object.
    mkV2 : V -> V2 ; -- V2 out of V. No preposition, را for direct object.
    mkV2 : (listen : V) -> (to : Prep) -> V2 -- V2 out of V. Use given preposition, no را for direct object.
  } ;

  mkV3 = overload {
    mkV3 : Str -> V3 -- Predictable V3, را for direct object, no prepositions.
     = \s -> lin V3 (regV s ** {c2 = prepOrRa "را" ; c3 = noPrep}) ;
    mkV3 : V -> (dir,indir : Str) -> V3 -- Takes a verb and two prepositions or را as strings (can be empty).
     = \v,p,q -> lin V3 (v ** {c2 = prepOrRa p ; c3 = prepOrRa q}) ;
    mkV3 : V -> (dir,indir : Prep) -> V3 -- Takes a verb and two prepositions
     = \v,p,q -> lin V3 (v ** {c2 = p ; c3 = q})
  } ;

  mkVQ = overload {
   mkVQ : Str -> VQ -- predictable verb with question complement
    = \s -> lin VQ (regV s) ;
   mkVQ : V -> VQ -- VQ out of a verb
    = \v -> lin VQ v
  } ;

  mkVA = overload {
    mkVA : Str -> VA -- predictable verb with adjective complement
      = \s -> lin VA (regV s ** {c2 = noPrep}) ;
    mkVA : V -> VA -- VA out of a verb
      = \v -> lin VA (v ** {c2 = noPrep}) ;
    mkVA : V -> Prep -> VA -- VA out of a verb and preposition
      = \v,p -> lin VA (v ** {c2 = p}) ;
    } ;

  mkVS = overload {
   mkVS : Str -> VS -- predictable verb with sentence complement in subjunctive.
    = \s -> lin VS (regV s ** {compl=subjunctive}) ;
   mkVS : V -> VS -- VS out of a verb, sentence complement in subjunctive.
    = \v -> lin VS (v ** {compl=subjunctive}) ;
   mkVS : VVForm -> V -> VS -- sentence complement given as argument
    = \vvf,v -> lin VS (v ** {compl=vvf}) ;
  } ;

  mkVV = overload {
    mkVV : Str -> VV -- Predictable VV, subjunctive complement, is auxiliary.
     = \s -> lin VV (regV s ** {isAux = True ; compl = subjunctive ; isDef = False}) ;
    mkVV : V -> VV -- takes its VP complement in subjunctive. Is auxiliary.
     = \v -> v ** {isAux = True ; compl = subjunctive ; isDef = False} ;
    mkVV : VVForm -> V -> VV -- takes its VP complement in the given VVForm
     = \vvf,v -> v ** {isAux = True ; compl = vvf ; isDef = False} ;
    mkVV : (isAux : Bool) -> VVForm -> V -> VV -- takes its VP complement in the given VVForm. Whether it's auxiliary (T/F) given as the first argument.
     = \isAux,vvf,v -> v ** {isAux = isAux ; compl = vvf ; isDef = False}
  } ;

  defVV : VV -> VV = \vv -> vv ** {isDef=True} ;

  mkV2S = overload {
    mkV2S : Str -> V2S -- predictable morphology, direct object with را, sentence complement in subjunctive.
     = \s -> lin V2S (regV s ** {compl=subjunctive ; c2 = prepOrRa "را"}) ;
    mkV2S : V -> V2S  -- direct object with را, sentence complement in subjunctive.
     = \v -> lin V2S (v ** {compl=subjunctive ; c2 = prepOrRa "را"}) ;
    mkV2S : Prep -> VVForm -> V -> V2S  -- direct object and mood for sentence complement as arguments.
     = \prep,vvf,v -> lin V2S (v ** {compl=vvf ; c2 = prep}) ;
    mkV2S : V2 -> V2S -- direct object given by V2, sentence complement in subjunctive.
     = \v2 -> lin V2S (v2 ** {compl=subjunctive}) ;
    mkV2S : VS -> V2S -- direct object with را, sentence complement given by VS.
     = \vs -> lin V2S (vs ** {c2 = prepOrRa "را"})
    } ;

  mkV2V = overload {
    mkV2V : V -> (cN : Str) -> (isAux : Bool) -> V2V -- Verb, complementiser for the noun, whether it's auxiliary.
      = \v,s,b -> let vv : VV = mkVV b subjunctive v in
        lin V2V (vv ** {c2 = prepOrRa s}) ;
     mV2V : VV -> (cN : Str) -> V2V -- V2V out of VV + complementiser for the noun
      = \vv,s -> lin V2V (vv ** {c2 = prepOrRa s}) ;
     mV2V : VV -> V2V -- V2V out of VV, را for direct object
      = \vv -> lin V2V (vv ** {c2 = prepOrRa "را"})
  } ;


----2 Adverbs
  mkAdv : Str -> Adv -- Takes a string, returns an adverb.
    = \str -> lin Adv {s = str} ;

----2 Prepositions

  mkPrep = overload {
    mkPrep : Str -> Prep -- Takes a string, returns a preposition.
     = \str -> lin Prep (prepOrRa str) ;
    mkPrep : Str -> Mod -> Prep -- Takes a string and Mod (so far only option is ezafe), returns a preposition.
     = \str,m -> lin Prep ((prepOrRa str) ** {mod=m})
  } ;

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

  mkSubj = overload {
    mkSubj : Str -> Subj -- Takes its verbal complement in indicative.
      = \s -> mkSubj' s ;
    mkSubj : VVForm -> Str -> Subj -- Specify whether it takes complement in subjunctive or indicative.
      = \vvf,s -> mkSubj' s ** {compl=vvf}
  } ;

  mkInterj : Str -> Interj
    = \s -> lin Interj {s=s} ;

--.
--2 Definitions of paradigms

-- The definitions should not bother the user of the API. So they are
-- hidden from the document.

  Number = ResPes.Number ;
  singular = Sg ;
  plural = Pl;

  Animacy = ResPes.Animacy ;
  human = Animate ;
  nonhuman = Inanimate ;

  animate = human ;
  inanimate = nonhuman ;

  VVForm = ResPes.VVForm ;
  subjunctive = ResPes.Subj ;
  indicative = Indic ;

  Mod = ResPes.Mod ;
  ezafe = ResPes.Ezafe ;


  mkSubj' : Str -> Subj ;
  mkSubj' s = lin Subj (case s of {
    "آن" => {s = [] ; relpron = Ance ; compl = indicative} ;
     _    => {s = s ; relpron = Ke ; compl = indicative}
    }) ;

-- Removed mkV_1, mkV_2, mkN01 and mkN02 from public API, still available for
-- any applications that open ParadigmsPes. /IL 2019-02-08
  mkV_1 : Str -> V
    = \s -> lin V (mkVerb1 s) ;
  mkV_2 : Str -> V
    = \s -> lin V (mkVerb2 s) ;


  mkN = overload {
    mkN : (sg : Str) -> N -- Takes singular form, returns a noun with ها as the plural form.
      = \sg -> mkN01 sg inanimate ;
    mkN : (sg,pl : Str) -> N -- Takes singular and plural forms. Use for ان or its allomorphs, and loanwords with Arabic plural.
      = \sg,pl -> M.mkN sg pl inanimate ;
    mkN : (possStem : Str) -> N -> N -- Noun with an unexpected possessive stem, e.g. مه where ه is a consonant, not vowel.
      = \ps,n -> possStemN ps n ;

    -- hidden from API
    mkN : (sg : Str) -> Animacy -> N -- Takes singular form and animacy. Inanimate plural ها. Animate plural ان or an allomorph of it (یان or گان) depending on the singular form.
      = \sg,ani -> case ani of {
          Inanimate => mkN01 sg ani ;
          Animate   => mkN02 sg ani } ;
    mkN : (sg,pl : Str) -> Animacy -> N -- Worst-case constructor: takes singular and plural forms and animacy. Use for e.g. loanwords with Arabic plural, or animate nouns with ها as plural.
      = \sg,pl,ani -> M.mkN sg pl ani
  } ;

  possStemN : Str -> N -> N = \possStem,n -> n ** {
    s = table {num => table {Poss => possStem ;
                             mod  => n.s ! num ! mod}
              }

  } ;

  mkN01 : (sg : Str) -> Animacy -> Noun ; -- Takes singular form and animacy, forms plural with ها
  mkN01 sg ani =
    let pl : Str = case last sg of {
       --"د"|"ذ"|"ر"|"ز"|"ژ" => sg + "ها" ; -- these letters are separated by default
         _                     => zwnj sg "ها" } ; -- Using zero-width non-joiner, defined in MorphoPes
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
       = \str -> lin A (mkAdj str str) ;
    mkA : Str-> Str -> A -- Takes adj and adv forms
       = \str,adv -> lin A (mkAdj str adv);
    mkA : (pos,compar,adv : Str) -> A -- positive, comparative and adverb
       = \p,c,adv -> lin A (mkAdj p c adv);
    mkA : Str -> Str -> A2 -- Takes string and complementiser, returns A2. Hidden from public API, confusing naming. /IL
      = \a,c -> lin A2 (mkAdj a a ** {c2 = c})
   } ;

  -- Mostly for things that are not really adjectives, like adverbials /IL
  invarA : Str -> A = \str ->
    lin A (<mkAdj str str : Adjective> ** {s = \\_,_ => str}) ;

   prefixA a = a ** {isPre=True};

   preA : (adj,adv : Str) -> A = \adj,adv ->
     lin A (<mkAdj adj adv:Adjective> ** {isPre=True}) ;

   {-
    -- Demonstrative Pronouns
     demoPN : Str -> Str -> Str -> Quant =
       \s1,s2,s3 -> let n = makeDemonPronForm s1 s2 s3 in {s = n.s ; a = defaultAgr ; lock_Quant = <>};
   -- Interrogative pronouns
    mkIP : (x1,x2,x3,x4:Str) -> Number -> Gender -> IP =
     \s1,s2,s3,s4,n,g -> let p = mkIntPronForm s1 s2 s3 s4 in { s = p.s ; n = n ; g = g ;  lock_IP = <>};
   -}

  mkCmpdNoun1 : Str -> N -> N = \s,noun ->
    noun ** {
      s = \\ez,n => s ++ noun.s ! ez ! n ;
      isCmpd = IsCmpd} ;
  mkCmpdNoun2 : N -> Str -> N = \noun,s ->
    noun ** {
      s = \\ez,n => noun.s ! ez ! n ++ s ;
      isCmpd = IsCmpd};

-- hidden from public API
  compoundV = overload {
    compoundV : Str -> V -> V
      = \s,v -> v ** {
          prefix = s ;
          lightverb = case v.lightverb of {Kardan => Kardan ; _ => Light}
        } ;
    compoundV : Str -> V2 -> V -- hidden from public API
      = \s,v -> lin V (v ** {prefix = s}) ;
  };

  regV : Str -> V = \inf ->
    let pres : Str = case inf of {
          stem + ("ی"|"ا"|"و") + "دن" => stem ;
          stem + ("تن"|"دن")           => stem }
    in lin V (mkVerb inf pres) ;

  mkV2 = overload {
    mkV2 : Str -> V2 -- Predictable V2 with را
      = \s -> lin V2 (regV s ** {c2 = prepOrRa "را"}) ;
    mkV2 : Str -> V -> V2 -- Compound V2 with را
      = \s,v -> lin V2 (v ** {prefix = s ; c2 = prepOrRa "را"}) ;
    mkV2 : V -> V2
      = \v -> lin V2 (v ** {c2 = prepOrRa "را"}) ;
    mkV2 : V -> Prep -> V2
      = \v,p -> lin V2 (v ** {c2 = p}) ;
    mkV2 : V -> Str -> V2
      = \v,ra -> lin V2 (v ** {c2 = prepOrRa ra}) ;
    mkV2 : V -> Str -> Bool -> V2
      = \v,p,b -> lin V2 (v ** {c2 = prepOrRa p}) ;
    } ;

  prepOrRa : Str -> Compl = \s -> case s of {
    ra@("را"|"")
         => {s = [] ; ra = ra   ; mod=Bare ; isPrep = False} ;
    prep => {s = prep ; ra = [] ; mod=Bare ; isPrep = True}
    } ;
  noPrep = prepOrRa [] ;

  -- NB. The 'mod' field has different meaning for verbs and N2s.
  ezafeForN2 = {s = [] ; ra = [] ; mod=Ezafe ; isPrep = False} ;

  mkPost : Str -> Prep = \s -> lin Prep {s=[] ; ra=s ; mod=Bare ; isPrep = False} ;

  mkN2 = overload {
    mkN2 : Str -> N2 -- Predictable N2 without complement
      = \s -> lin N2 (mkN01 s inanimate ** {c2 = ezafeForN2 ; compl = []}) ;
    mkN2 : N -> N2 -- N2 from without complement
      = \n -> lin N2 (n ** {c2 = ezafeForN2 ; compl = []}) ;
    mkN2 : N -> Str -> N2
      = \n,c -> lin N2 (n ** {c2 = prepOrRa c ; compl = []}) ;
    mkN2 : N -> Prep -> Str -> N2 -- hidden from puclic API
      = \n,p,c -> lin N2 (n ** {c2 = p; compl = []})
  } ;

  mkN3 = overload {
    mkN3 : N -> Str -> Str -> N3
      = \n,p,q -> lin N3 (n ** {c2 = prepOrRa p ; c3 = prepOrRa q}) ;
    mkN3 : N -> Prep -> Prep -> N3
      = \n,p,q -> lin N3 (n ** {c2 = p ; c3 = q}) ;
    mkN3 : N -> Prep -> Str -> Str -> N3 -- hidden from public API
      = \n,p,q,r -> lin N3 (n ** {c2 =  p ; c3 = prepOrRa q ; c4 = r}) -- there is no c4
  } ;


  mkQuant = overload {
--    mkQuant : Pron -> Quant = \p -> {s = \\_,_,c => p.s!c ;a = p.a ; lock_Quant = <>};
    mkQuant :  Str -> Str -> Quant -- hidden from public API
      = \sg,pl -> makeQuant sg pl Bare False;
    mkQuant :  Str -> Str -> (isNeg : Bool) -> Quant -- hidden from public API
      = \sg,pl,isneg -> makeQuant sg pl Bare isneg;
    mkQuant :  Str -> Str -> Mod -> (isNeg : Bool) -> Quant -- hidden from public API
      = \sg,pl,mod,isneg -> makeQuant sg pl mod isneg;
  } ;

}
