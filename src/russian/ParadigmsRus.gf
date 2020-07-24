resource ParadigmsRus = open CatRus, ResRus, (R=ResRus), ParamRus, (Z=ZaliznyakAlgo), Prelude in {

----------------
-- Parameters

oper
  singular : Number
    = Sg ;
  plural : Number
    = Pl ;

  masculine : Gender
    = Masc ;
  feminine : Gender
    = Fem ;
  neuter : Gender
    = Neut ;

  short : ShortFormPreference
    = PrefShort ;
  full : ShortFormPreference
    = PrefFull ;

  animate : Animacy
    = Animate ;
  inanimate : Animacy
    = Inanimate ;

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

  locative : Case
    = Loc ;
  partitive : Case
    = Ptv ;
  vocative : Case
    = VocRus ;

  positive : Degree
    = Posit ;
  comparative : Degree
    = Compar ;
  superlative : Degree
    = Superl ;

  perfective : Aspect
    = Perfective ;
  imperfective : Aspect
    = Imperfective ;

  transitive : Transitivity
    = Transitive ;
  intransitive : Transitivity
    = Intransitive ;

------------------------------
-- Nouns

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
    mkN : A -> Gender -> Animacy -> N
      = \a, g, anim -> lin N (makeNFFromAF a g anim) ;
  } ;

  mkN2 = overload {
    mkN2 : N -> N2
      = \n -> lin N2 (mkFun n nullPrep) ;
    mkN2 : N -> Prep -> N2
      = \n, p -> lin N2 (mkFun n p) ;
    mkN2 : Str -> Gender -> Animacy -> Str -> Prep -> N2
      = \word, g, anim, zi, p -> lin N2 (mkFun (noMinorCases (Z.makeNoun word g anim (Z.parseIndex zi))) p)   ;
  } ;

  nullPrep : Prep = lin Prep {s=[]; c=Gen; hasPrep=False} ;

  mkN3 = overload {
    mkN3 : N -> Prep -> Prep -> N3
      = \n, p2, p3 -> lin N3 (mkFun2 n p2 p3) ;
    mkN3 : Str -> Gender -> Animacy -> Str -> Prep -> Prep -> N3
      = \word, g, anim, zi, p2, p3 -> lin N3 (mkFun2 (noMinorCases (Z.makeNoun word g anim (Z.parseIndex zi))) p2 p3) ;
  } ;

  mkPN = overload {
    mkPN : N -> PN
      = \n -> lin PN n ;
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
    mkA : Str -> Str -> Str -> A
      = \nom, comp, zi -> lin A (makeAdjectiveForms nom comp zi PrefFull) ;
    mkA : Str -> Str -> Str -> ShortFormPreference -> A
      = \nom, comp, zi, spf -> lin A (makeAdjectiveForms nom comp zi spf) ;
  } ;

-- Two-place adjectives need a preposition and a case as extra arguments.

  -- TODO: ? mkA2 : A -> Str -> Case -> A2 ;  -- "делим на"
  mkA2 : A -> Prep -> A2
    = \a,p -> lin A2 (a ** {c = p}) ;

  mkOrd = overload {   -- ord from adj. TODO: This shadows mkOrd from constructors...
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
    mkV : Aspect -> Transitivity -> Str -> Str -> V
      = \asp,tran,inf,sg1 -> lin V (guessVerbForms asp tran inf sg1 (Z.sg1StemFromVerb sg1 + "ет")) ;
    mkV : Aspect -> Transitivity -> Str -> Str -> Str -> V
      = \asp,tran,inf,sg1,sg3 -> lin V (guessVerbForms asp tran inf sg1 sg3) ;
    mkV : Aspect -> Transitivity -> Str -> Str -> Str -> Str -> V
      = \asp,tran,inf,sg1,sg3,zv -> lin V (Z.makeVerb inf sg1 sg3 (Z.parseVerbIndex zv) asp tran (Z.infStemFromVerb inf).p2 ) ;
  } ;

  mkV2 = overload {
    mkV2 : V -> V2
      = \vf -> lin V2 (vf ** {c={s=[] ; c=Acc ; hasPrep=False}}) ;
    mkV2 : V -> Case -> V2
      = \vf, c -> lin V2 (vf ** {c={s=[] ; c=c ; hasPrep=False}}) ;
    mkV2 : V -> Prep -> V2
      = \vf, prep -> lin V2 (vf ** {c=prep}) ;
    } ;

  mkV3 = overload {
    mkV3 : V -> Case -> Case -> V3   -- "сложить письмо в конверт"
      = \vf, cas1, cas2 -> lin V3 (vf ** {c={s=[] ; c=cas1 ; hasPrep=False} ; c2={s=[] ; c=cas2 ; hasPrep=False}} ) ;
    mkV3 : V -> Prep -> Prep -> V3   -- "сложить письмо в конверт"
      = \vf, prep1, prep2 -> lin V3 (vf ** {c=prep1 ; c2=prep2} ) ;
  } ;

  dirV2 : V -> V2 ;
  dirV2 v = mkV2 v Acc ;
  tvDirDir : V -> V3 ;
  tvDirDir v = mkV3 v Acc Dat ;

------------------------
-- Adverbs, prepositions, conjunctions, ...

  mkAdv : Str -> Adv
    = \s -> lin Adv (makeAdverb s) ;

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
    = \s,c -> lin Prep {s = s ; c = c ; hasPrep = True} ;
}