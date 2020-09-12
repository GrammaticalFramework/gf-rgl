--1 Russian Lexical Paradigms

resource ParadigmsRus = open CatRus, ResRus, (R=ResRus), ParamRus, (Z=InflectionRus), Prelude, Maybe in {

--2 Parameters
--

oper
-- Abstracting gender. Gender is a parameter for mkN, mkN2, mkN3
  masculine : Gender
    = Masc ;
  feminine : Gender
    = Fem ;
  neuter : Gender
    = Neut ;

-- Abstracting numbers. Number is a parameter for mkPN, mkConj
  singular : Number
    = Sg ;
  plural : Number
    = Pl ;

-- Limiting number
  only_singular : MaybeNumber
    = JustSg ;
  only_plural : MaybeNumber
    = JustPl ;

-- Adjectives can have short and full form. ShortFormPreference type is a parameter for mkA
  short : ShortFormPreference
    = PrefShort ;
  full : ShortFormPreference
    = PreferFull ;

-- Animacy is needed for nouns and some pronouns.
  animate : Animacy
    = Animate ;
  inanimate : Animacy
    = Inanimate ;

-- Main cases:
  nominative : Case
    = Nom ;
  genitive : Case
    = Gen ;
  dative : Case
    = Dat ;
  accusative : Case
    = Acc ;
  instrumental : Case
    = Ins ;
  prepositional : Case
    = Pre ;

-- "Minor" cases:
  locative : Case   -- or Pre-2, some nouns have this different from prepositional case
    = Loc ;
  partitive : Case  -- or Gen-2. Some nouns like "tee", have this in addition to genitive, usually spoken language
    = Ptv ;
  vocative : Case   -- can be used in spoken language
    = VocRus ;

-- Degrees of adjectives
  positive : Degree
    = Posit ;
  comparative : Degree
    = Compar ;
  superlative : Degree
    = Superl ;

-- Aspects of verbs
  perfective : Aspect
    = Perfective ;
  imperfective : Aspect
    = Imperfective ;

-- Transitivity is verb's inherent characteristic. Can influence which forms are possible
  transitive : Transitivity
    = Transitive ;
  intransitive : Transitivity
    = Intransitive ;

-- Reflexivity is traditionally understood as inherent characteristic. Not to be confused with passive forms
   reflexive : Reflexivity
     = Reflexive ;
   non_reflexive : Reflexivity
     = NonReflexive ;

-- Voice
   active : Voice
     = Act ;
   passive : Voice
     = Pass ;

------------------------------
--2 Nouns

  mkN : overload {
    mkN : Str -> N ;     -- can guess declension and gender of some nouns given nominative
    mkN : Str -> Gender -> Animacy -> N ;  -- can guess declension of more nouns
    mkN : Str -> Gender -> Animacy -> (idx : Str) -> N ;  -- Fourth parameter is a declension type index (based on Zaliznyak's dictionary), for example, "1*a(1)"
    mkN : Str -> Gender -> Animacy -> (idx : Str) -> MaybeNumber -> N ;  -- Same, but number restrictions can be added
    mkN : A -> Gender -> Animacy -> N ;  -- for nouns, which decline as adjective
    mkN : A -> Gender -> Animacy -> MaybeNumber -> N ;  -- same, with possibility to limit number (usually to only_singular)
    mkN : N -> (link : Str) -> N -> N ; -- compound noun. Link can end on "-", in which case parts are glued together. First one characterizes the whole.
  } ;

  mkN2 : overload {
    mkN2 : N -> N2 ;
    mkN2 : N -> Prep -> N2 ;
    mkN2 : Str -> Gender -> Animacy -> (idx : Str) -> Prep -> N2 ; -- convenience for making N2. Fourth parameter is a declension type index (based on Zaliznyak's dictionary), for example, "1*f''(1)"
  } ;

  mkN3 : overload {
    mkN3 : N -> Prep -> Prep -> N3 ;
    mkN3 : Str -> Gender -> Animacy -> (idx : Str) -> Prep -> Prep -> N3 ;  -- convenience for making N2. Fourth parameter is a declension type index (based on Zaliznyak's dictionary), for example, "4*b"
  } ;

  mkPN : overload {
    mkPN : N -> PN ;
    mkPN : N -> Str -> N -> PN ; -- see compound noun
  } ;

--2 Adjectives

  mkA : overload {
    mkA : Str -> A ;  -- can guess declension of many adjectives given nominative masculine
    mkA : Str -> Str -> A ;  -- same, but comparative given as a second argument
    mkA : Str -> Str -> (idx : Str) -> A ; -- nom masc, comparative and third parameter is Zaliznyak's dictionary index, for example, "1a"
    mkA : Str -> Str -> (idx : Str) -> ShortFormPreference -> A ; -- same, but with short form preference given
    mkA : Str -> ZAIndex -> A ;
    mkA : Str -> Str -> ZAIndex -> A ;
    mkA : Str -> Str -> ZAIndex -> ShortFormPreference -> A ;
    mkA : A -> Str -> A -> A ;  -- Compound adjective like социально-экономический
    mkA : V -> Voice -> Tense -> A ;        -- make participles
  } ;

  ShortenA : A -> A ;

-- Two-place adjectives need a preposition

  mkA2 : overload {
    mkA2 : A -> Prep -> A2 ;
    } ;

  mkOrd : overload {
    mkOrd : (nom : Str) -> Ord ;
    } ;

-------------------------
--2 Verbs

  mkV : overload {
    mkV : (inf : Str) -> (sg1 : Str) -> V ;  -- guess some I conjugation verbs (not "ё") from infinitive and Sg P1, perfective, transitive
    mkV : (inf : Str) -> (sg1 : Str) -> (sg3 : Str) -> V ; -- guess verb forms given Inf, Sg P1, Sg P3, perfective, transitive
    mkV : Aspect -> (inf : Str) -> (sg1 : Str) -> (sg3 : Str) -> V ; -- same, but aspect as first parameter
    mkV : Aspect -> Transitivity -> (inf : Str) -> V ;  -- for irregular verbs
    mkV : Aspect -> Transitivity -> (inf : Str) -> (sg1 : Str) -> (sg3 : Str) -> V ;  -- aspect, transitivity, Inf, Sg P1, Sg P3
    mkV : Aspect -> Transitivity -> (inf : Str) -> (sg1 : Str) -> (sg3 : Str) -> (idx : Str) -> V    -- aspect, transitivity, Inf, Sg P1, Sg P3 and index from Zaliznyak's dictionary, eg "14a"
  } ;

  mkV2 : overload {
    mkV2 : V -> V2 ;  -- most common case with Acc and no preposition
    mkV2 : V -> Case -> V2 ; -- given case, but no preposition
    mkV2 : V -> Prep -> V2 ;
    } ;

  mkV3 : overload {
    mkV3 : V -> Case -> Case -> V3 ;
    mkV3 : V -> Prep -> Prep -> V3 ;
  } ;

   mkVS  : V -> VS ;
   mkVQ  : V -> VQ ;
   mkV2V : overload {
     mkV2V : V -> Prep -> V2V ;
   } ;
   mkV2S : overload {
    mkV2S : V -> Prep -> V2S ;
   } ;
   mkV2Q : overload {
    mkV2Q : V -> Prep -> V2Q ;
   } ;
   mkV2A : overload {
    mkV2A : V -> Prep -> V2A ;
   } ;

  dirV2 : V -> V2 ;
  tvDirDir : V -> V3 ;
  mkVV : V -> VV;

------------------------
--2 Adverbs, prepositions, conjunctions, ...

  mkAdv : overload {
    mkAdv : Str -> Adv ;
    } ;
  mkIAdv : Str -> IAdv ;
  mkConj : overload {
    mkConj : Str -> Number -> Conj ;  -- only middle conjunction
    mkConj : Str -> Str -> Number -> Conj ; -- two-part conjunction
    } ;

  mkInterj : Str -> Interj ;
  mkPrep : Str -> Case -> Prep ;

-- The definitions should not bother the user of the API. So they are
-- hidden from the document.
--.

------------------------------
-- Nouns

  nullPrep : Prep = lin Prep {s=[] ; c=Gen ; neggen=False ; hasPrep=False} ;

  mkN = overload {
    mkN : Str -> N
      = \nom -> lin N (guessNounForms nom) ;
    mkN : Str -> Animacy -> N
      = \nom,anim -> lin N ((guessNounForms nom) ** {anim=anim}) ;
    mkN : Str -> Gender -> Animacy -> N
      = \nom, g, anim -> lin N (guessLessNounForms nom g anim) ;
    mkN : Str -> Gender -> Animacy -> Z.ZNIndex -> N
      = \word, g, anim, z -> lin N (noMinorCases (Z.makeNoun word g anim z)) ;
    mkN : Str -> Gender -> Animacy -> Str -> N
      = \word, g, anim, zi -> lin N (noMinorCases (Z.makeNoun word g anim (Z.parseIndex zi))) ;
    mkN : Str -> Gender -> Animacy -> Str -> MaybeNumber -> N
      = \word, g, anim, zi, mbn -> lin N (applyMaybeNumber ((noMinorCases (Z.makeNoun word g anim (Z.parseIndex zi))) ** {mayben=mbn})) ;
    mkN : A -> Gender -> Animacy -> N
      = \a, g, anim -> lin N (makeNFFromAF a g anim) ;
    mkN : A -> Gender -> Animacy -> MaybeNumber -> N
      = \a, g, anim, mbn -> lin N (applyMaybeNumber ((makeNFFromAF a g anim) ** {mayben=mbn})) ;
    mkN : N -> Str -> N -> N
      = \n1,link,n2 -> lin N (mkCompoundN n1 link n2) ;

    -- For backwards compatibility:
    mkN : (nomSg, genSg, datSg, accSg, instSg, preposSg, prepos2Sg, nomPl, genPl, datPl, accPl, instPl, preposPl : Str) -> Gender -> Animacy -> N
      = \nomSg, genSg, datSg, accSg, instSg, preposSg, prepos2Sg, nomPl, genPl, datPl, accPl, instPl, preposPl, g, anim ->
        lin N {
          snom=nomSg;pnom=nomPl;sgen=genSg;pgen=genPl;sdat=datSg;pdat=datPl;sacc=accSg;pacc=accPl;sins=instSg;pins=instPl;sprep=preposSg;pprep=preposPl;
          sloc=prepos2Sg; sptv=genSg ; svoc=nomSg ;
          anim=anim;
          mayben=BothSgPl ;
          g=g
        } ;
  } ;

  mkN2 = overload {
    mkN2 : N -> N2
      = \n -> lin N2 (mkFun n nullPrep) ;
    mkN2 : N -> Prep -> N2
      = \n, p -> lin N2 (mkFun n p) ;
    mkN2 : Str -> Gender -> Animacy -> Str -> Prep -> N2
      = \word, g, anim, zi, p -> lin N2 (mkFun (noMinorCases (Z.makeNoun word g anim (Z.parseIndex zi))) p)   ;
  } ;

  nullPrep : Prep = lin Prep {s=[] ; c=Gen ; neggen=False ; hasPrep=False} ;

  mkN3 = overload {
    mkN3 : N -> Prep -> Prep -> N3
      = \n, p2, p3 -> lin N3 (mkFun2 n p2 p3) ;
    mkN3 : Str -> Gender -> Animacy -> Str -> Prep -> Prep -> N3
      = \word, g, anim, zi, p2, p3 -> lin N3 (mkFun2 (noMinorCases (Z.makeNoun word g anim (Z.parseIndex zi))) p2 p3) ;
  } ;

  mkPN = overload {
    mkPN : N -> PN
      = \n -> lin PN n ;
    mkPN : N -> Str -> N -> PN
      = \n1,link,n2 -> lin PN (mkCompoundN n1 link n2) ;
    mkPN : Str -> PN
      = \nom -> lin PN (guessNounForms nom) ;
    mkPN : Str -> Gender -> Animacy -> PN
      = \nom, g, anim -> lin PN (guessLessNounForms nom g anim) ;
    mkPN : Str -> Gender -> Number -> Animacy -> PN
      = \nom, g, n, anim -> lin PN (guessLessNounForms nom g anim) ;
    mkPN : Str -> Gender -> Animacy -> Z.ZNIndex -> PN
      = \word, g, anim, z -> lin PN (noMinorCases (Z.makeNoun word g anim z)) ;
    mkPN : Str -> Gender -> Animacy -> Str -> PN
      = \word, g, anim, zi -> lin PN (noMinorCases (Z.makeNoun word g anim (Z.parseIndex zi))) ;
  } ;

---------------------
-- Adjectives

  mkA = overload {
    mkA : Str -> A
      = \nom -> lin A (guessAdjectiveForms nom) ;
    mkA : Str -> Str -> A
      = \nom, comp -> lin A ((guessAdjectiveForms nom) ** {comp=comp}) ;
    mkA : Str -> Z.ZAIndex -> A
      = \nom, zi -> lin A (makeAdjectiveFormsUseIndex nom "" zi PreferFull) ;
    mkA : Str -> Str -> Str -> A
      = \nom, comp, zi -> lin A (makeAdjectiveForms nom comp zi PreferFull) ;
    mkA : Str -> Str -> Z.ZAIndex -> A
      = \nom, comp, zi -> lin A (makeAdjectiveFormsUseIndex nom comp zi PreferFull) ;
    mkA : Str -> Str -> Str -> ShortFormPreference -> A
      = \nom, comp, zi, spf -> lin A (makeAdjectiveForms nom comp zi spf) ;
    mkA : Str -> Str -> Z.ZAIndex -> ShortFormPreference -> A
      = \nom, comp, zi, spf -> lin A (makeAdjectiveFormsUseIndex nom comp zi spf) ;
    mkA : PronForms -> A
      = \pf -> pronToAdj pf ;
    mkA : A -> Str -> A -> A
      = \a1,link,a2 -> lin A (mkCompoundA a1 link a2) ;
    mkA : V -> Voice -> Tense -> A
      = \v,voice,t ->
        let refl = case v.refltran of {Refl => "ся" ; _ => ""} in
        case <voice,t> of {
          <Pass,Past|Cond> => lin A ( --# notpresent TODO: check
            guessAdjectiveForms (v.ppps + "ый") ** {  --# notpresent
              sm=v.pppss ;  --# notpresent
              sf=v.pppss + "а"; --# notpresent
              sn=v.pppss + "о"; --# notpresent
              sp=v.pppss + "ы" --# notpresent
              } --# notpresent
            ) ;--# notpresent
          <Pass,Pres> => lin A ( -- overgenerated
            let s : Str = case v.prpl1 of {
              f + #consonant + "ём" => (Predef.tk 2 v.prpl1) + "омый" ;
              _ => v.prpl1 + "ый"
              } in
            makeAdjectiveFormsUseIndex s "" (Z.ZA 1 Z.No Z.A_ Z.NoC) PreferFull) ;
          <Act,Past|Cond> => lin A ( --# notpresent
            let s : Str = case v.inf of { --# notpresent
              _ + ("сти"|"зти") => (Predef.tk 1 v.prsg1) + "ший" ;  --# notpresent TODO: check if not all of these cases are ok
              _  => (Predef.tk 1 v.psgm) + "вший" --# notpresent
              } in Z.onlyParticipleForms (makeAdjectiveFormsUseIndex (s + refl) "" (Z.ZA 4 Z.No Z.A_ Z.NoC) PreferFull)) ; --# notpresent
          <Act,Pres> => lin A (
            Z.onlyParticipleForms (
             makeAdjectiveFormsUseIndex (((Predef.tk 1 v.prpl3) + "щий") + refl) "" (Z.ZA 4 Z.No Z.A_ Z.NoC) PreferFull)) ;
          _ => Predef.error "Error: participle for this voice and tense does not exist"
          }  -- TODO: suppress comp and short for all but Pass Pres
  } ;

  ShortenA : A -> A
    = \a -> a ** {preferShort = PrefShort} ;

-- Two-place adjectives need a preposition and a case as extra arguments.

  mkA2 = overload {
    mkA2 : A -> Prep -> A2
      = \a,p -> lin A2 (a ** {c = p}) ;
    mkA2 : A -> Str -> Case -> A2
      = \a,p,cas -> lin A2 (a ** {c = mkPrep p cas}) ;
    } ;

  mkOrd = overload {
    mkOrd : (nom : Str) -> Ord
      = \nom -> lin Ord (guessAdjectiveForms nom) ;
    } ;

-------------------------
-- Verbs

  mkV = overload {
    mkV : Str -> Str -> V
      = \inf,sg1 -> lin V (guessVerbForms Perfective Transitive inf sg1 (Z.sg1StemFromVerb sg1 + "ет")) ;
    mkV : Str -> Str -> Str -> V
      = \inf,sg1,sg3 -> lin V (guessVerbForms Perfective Transitive inf sg1 sg3) ;
    mkV : Aspect -> Str -> Str -> V
      = \asp,inf,sg1 -> lin V (guessVerbForms asp Transitive inf sg1 (Z.sg1StemFromVerb sg1 + "ет")) ;
    mkV : Aspect -> Str -> Str -> Str -> V
      = \asp,inf,sg1,sg3 -> lin V (guessVerbForms asp Transitive inf sg1 sg3) ;
    mkV : Aspect -> Transitivity -> Str -> V  -- for irregular verbs
      = \asp,tran,inf -> lin V (guessIrregularVerbForms asp tran inf) ;
    mkV : Aspect -> Transitivity -> Str -> Str -> V
      = \asp,tran,inf,sg1 -> lin V (guessVerbForms asp tran inf sg1 (Z.sg1StemFromVerb sg1 + "ет")) ;
    mkV : Aspect -> Transitivity -> Str -> Str -> Str -> V
      = \asp,tran,inf,sg1,sg3 -> lin V (guessVerbForms asp tran inf sg1 sg3) ;
    mkV : Aspect -> Transitivity -> Str -> Str -> Str -> Str -> V
      = \asp,tran,inf,sg1,sg3,zv -> lin V (Z.makeVerb inf sg1 sg3 (Z.parseVerbIndex zv) asp tran (Z.infStemFromVerb inf).p2 ) ;
    mkV : Aspect -> Transitivity -> Str -> Str -> Str -> Z.ZVIndex -> V
      = \asp,tran,inf,sg1,sg3,zvi -> lin V (Z.makeVerb inf sg1 sg3 zvi asp tran (Z.infStemFromVerb inf).p2 ) ;
    -- For backwards compatibility:
    mkV : Aspect -> (presSg1,presSg2,presSg3,presPl1,presPl2,presPl3,pastSgMasc,imp,inf: Str) -> V
      = \asp, sgP1, sgP2, sgP3, plP1, plP2, plP3, sgMascPast, imperSgP2, inf ->
        lin V ((guessVerbForms asp Transitive inf sgP1 sgP3) ** {
          prsg1=Z.dropRefl sgP1 ;
          prsg2=Z.dropRefl sgP2 ;
          prsg3=Z.dropRefl sgP3 ;
          prpl1=Z.dropRefl plP1 ;
          prpl2=Z.dropRefl plP2 ;
          prpl3=Z.dropRefl plP3 ;
        })
  } ;

  mkV2 = overload {
    mkV2 : V -> V2
      = \vf -> lin V2 (vf ** {c={s=[] ; c=Acc ; neggen=True ; hasPrep=False}}) ;
    mkV2 : V -> Case -> V2
      = \vf, c -> lin V2 (vf ** {c={s=[] ; c=c ; neggen=False ; hasPrep=False}}) ;
    mkV2 : V -> Prep -> V2
      = \vf, prep -> lin V2 (vf ** {c=prep}) ;

    -- For backwards compatibility:
    mkV2 : V -> Str -> Case -> V2
      = \vf, prep_s, c -> lin V2 (vf ** {c={s=prep_s ; c=c ; neggen=False ; hasPrep=True}})
    } ;

  mkV3 = overload {
    mkV3 : V -> Case -> Case -> V3   -- "сложить письмо в конверт"
      = \vf, cas1, cas2 -> lin V3 (vf ** {c={s=[] ; c=cas1 ; neggen=False ; hasPrep=False} ; c2={s=[] ; c=cas2 ; neggen=False ; hasPrep=False}} ) ;
    mkV3 : V -> Prep -> Prep -> V3   -- "сложить письмо в конверт"
      = \vf, prep1, prep2 -> lin V3 (vf ** {c=prep1 ; c2=prep2} ) ;

    -- For backwards compatibility:
    mkV3 : V -> Str -> Str -> Case -> Case -> V3
      = \vf, prep1, prep2, cas1, cas2 -> lin V3 (vf ** {c={s=prep1 ; c=cas1 ; neggen=False ; hasPrep=True} ; c2={s=prep2 ; c=cas2 ; neggen=False ; hasPrep=True}} ) ;
  } ;


  dirV2 v = mkV2 v Acc ;
  tvDirDir v = mkV3 v Acc Dat ;
  mkVV = \v -> lin VV {v=v; modal=\\a=>[]} ;

  mkVS v = lin VS v ;
  mkVQ v = lin VQ v ;
  mkV2V = overload {
    mkV2V : V -> Prep -> V2V
      = \v, prep -> lin V2V (v ** {c=prep}) ;
    mkV2V : V -> Str -> Case -> V2V
      = \v, prep, cas -> lin V2V (v ** {c={s=prep ; c=cas ; neggen=False ; hasPrep=True}}) ;
  } ;
  mkV2S = overload {
     mkV2S : V -> Prep -> V2S
       = \v, prep -> lin V2S (v ** {c=prep}) ;
     mkV2S : V -> Str -> Case -> V2S
       = \v, prep, cas -> lin V2S (v ** {c={s=prep ; c=cas ; neggen=False ; hasPrep=True}}) ;
  } ;
  mkV2Q = overload {
     mkV2Q : V -> Prep -> V2Q
       = \v, prep -> lin V2Q (v ** {c=prep}) ;
     mkV2Q : V -> Str -> Case -> V2Q
       = \v, prep, cas -> lin V2Q (v ** {c={s=prep ; c=cas ; neggen=False ; hasPrep=True}}) ;
  } ;
  mkV2A = overload {
     mkV2A : V -> Prep -> V2A
       = \v, prep -> lin V2A (v ** {c=prep}) ;
     mkV2A : V -> Str -> Case -> V2A
       = \v, prep, cas -> lin V2A (v ** {c={s=prep ; c=cas ; neggen=False ; hasPrep=True}}) ;
  } ;

------------------------
-- Adverbs, prepositions, conjunctions, ...

  mkAdv = overload {
    mkAdv : Str -> Adv
      = \s -> lin Adv (makeAdverb s) ;
    } ;

  mkIAdv : Str -> IAdv
    = \s -> lin IAdv (makeAdverb s) ;

  mkConj = overload {
    mkConj : Str -> Number -> Conj
      = \s, n -> lin Conj {s1 = [] ; s2 = s ; n = n} ;
    mkConj : Str -> Str -> Number -> Conj
      = \s1, s2, n -> lin Conj {s1 = s1 ; s2 = s2 ; n = n} ;
  } ;

  mkInterj : Str -> Interj
    = \s -> lin Interj {s = s} ;

  mkPrep : Str -> Case -> Prep
    = \s,c -> lin Prep {s = s ; c = c ; neggen = False ; hasPrep = True} ;


oper
  on2_Prep = mkPrep "на" Acc ;

  -- for backwards compatibility only. Use mkV methods instead.
  -- These are deprecated!
param Conjugation = First | FirstE | Second | SecondA | Mixed | Dolzhen | Foreign ;
oper
  first, firstE, second, mixed, dolzhen, foreign : Conjugation ;
  first = First ; firstE = FirstE ; second = Second ; secondA = SecondA ; mixed = Mixed ; dolzhen = Dolzhen; foreign = Foreign;

  -- Do not use the following method as it is only approximate because it does not use most informative SgP3 amd
  -- SgP3 is being guessed instead from SgP1.
  regV : Aspect -> Conjugation -> (stemPresSg1,endPresSg1,pastSg1,imp,inf : Str) -> V ;
  regV asp bconj stemPresSg1 endPresSg1 pastSg1 imp inf =
    let sg1=stemPresSg1 + endPresSg1 in
    let sg3 : Str = case bconj of {
      First => (Z.sg1StemFromVerb sg1) + "ет" ;
      Mixed => (Z.sg1StemFromVerb sg1) + "чет" ;
      FirstE => (Z.sg1StemFromVerb sg1) + "ёт" ;
      Second | SecondA => (Z.sg1StemFromVerb sg1) + "ит" ;
      _ => (Z.sg1StemFromVerb sg1) + "ет"
      } in (guessVerbForms asp Transitive inf sg1 sg3) ** {lock_V=<>} ;
}
