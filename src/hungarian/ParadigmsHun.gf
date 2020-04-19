resource ParadigmsHun = open
   CatHun, ResHun, ParamHun, NounHun, Prelude in {

oper

--2 Parameters
--
-- To abstract over number, valency and (some) case names,
-- we define the following identifiers. The application programmer
-- should always use these constants instead of the constructors
-- defined in $ResKor$.

  Harmony : Type ;
  harmA : Harmony ;
  harmE : Harmony ;
  harmO : Harmony ;

--2 Nouns

  mkN : overload {
    mkN : (sgnom : Str) -> N ; -- Predictable nouns from singular nominative. Accusative vowel is o for back harmony. No stem lowering (TODO better explanation/examples)
    mkN : (sgnom : Str) -> (sggen : Str) -> N ; -- Singular nominative and accusative. Takes care of cases like … TODO example
    mkN : (férfi : Str) -> (harm : Harmony) -> (ak : Str) -> N ; -- Noun with unpredictable vowel harmony and plural allomorph
  } ;

  mkPN : overload {
    mkPN : Str -> PN ; -- Singular PN out of a string
    mkPN : Str -> Number -> PN -- PN with a given number
    -- mkPN : N -> Number -> PN ;
    } ;

--2 Adjectives

  mkA : overload {
    mkA : (adj : Str) -> A ; -- Regular adjective, given in ??? form
    -- mkA : (kiga : Str) -> (jakda : A) -> A ; -- Compound adjective, e.g. 키가 작다 'short', literally 'height (is) small'. 키가 'height' given as string, 작다 'small' given as preconstructed A.
  } ;

  mkA2 : overload {
    mkA2 : Str -> Prep -> A2 ;
    mkA2 : Str -> Case -> A2 ;
    mkA2 : A -> Prep -> A2 ;
  } ;

--2 Verbs

  -- Verbs
  mkV : overload {
    mkV : (sg3 : Str) -> V ;    -- Predictable verb. Takes singular P3 form in present tense.
    -- mkV : (nore : Str) -> (hada : V) -> V ; -- Add a prefix to an existing verb, e.g. 노래+하다
  } ;

  copula : V ; -- The copula verb ''

  mkV2 : overload {
    mkV2 : (sg3 : Str) -> V2 ; -- Predictable verb. Takes singular P3 form in present tense, object case is accusative.
    mkV2 : V -> V2 ; -- Takes preconstructed V, object case is accusative.
    } ;

  -- mkV3 : overload {
  --   } ;

  -- mkVV : overload {
  --  } ;

  --
  -- mkVA : Str -> VA
  --   = \s -> lin VA (regV s) ;
  -- mkVQ : Str -> VQ
  --   = \s -> lin VQ (regV s) ;
  -- mkVS : Str -> VS
  --   = \s -> lin VS (regV s) ;
  --
  -- mkV2A : Str -> V2A
  --   = \s -> lin V2A (regV s ** {c2 = noPrep}) ;
  -- mkV2V : Str -> V2V
  --   = \s -> lin V2V (regV s ** {c2 = noPrep}) ;
  -- mkV2Q : Str -> V2Q
  --   = \s -> lin V2Q (regV s ** {c2 = noPrep}) ;

  -----

--2 Structural categories

  mkPrep : overload {
    mkPrep : Str -> Prep ; -- Postposition, takes nominative
    mkPrep : Str -> Case -> Prep ; -- Postposition and case
    } ;

  prePrep : Str -> Case -> Prep -- Preposition
    = \s,c -> lin Prep {pr=s ; s=[] ; c=c} ;

  casePrep : Case -> Prep ; -- No postposition, only case

  -- mkConj : (_,_ : Str) -> Number -> Conj = \s1,s2,num ->
  --   lin Conj { s = s1 ; s2 = s2 } ;

  -- mkSubj : Str -> Bool -> Subj = \s,b ->
  --   lin Subj { } ;

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;

  mkAdV : Str -> AdV
    = \s -> lin AdV {s = s} ;

  mkAdA : Str -> AdA
    = \s -> lin AdA {s = s} ;


--.
-------------------------------------------------------------------------------
-- The definitions should not bother the user of the API. So they are
-- hidden from the document.

  Harmony : Type = ResHun.Harm ;
  harmA = ResHun.H_a ;
  harmE = ResHun.H_e ;
  harmO = ResHun.H_o ;

  mkN = overload {
    mkN : Str -> N =
      \s -> lin N (regNoun s) ;

    mkN : Str -> Str -> N =
        \n,a-> lin N (regNounNomAcc n a) ;

    mkN : Str -> Harmony -> N =
      \s,h -> lin N (mkNounHarm h (pluralAllomorph s) s) ;

    mkN : Str -> (plural : Str) -> Harmony -> N =
      \s,pl,h -> lin N (mkNounHarm h pl s) ;
    } ;


  mkN2 = overload {
    mkN2 : Str -> N2 = \s -> lin N2 (regNoun s) ;
    mkN2 : N   -> N2 = \n -> lin N2 n ;
   } ;

  mkPN = overload {
    mkPN : Str -> PN = \s -> lin PN (defNP s Sg) ;
    mkPN : Str -> Number -> PN = \s,n -> lin PN (defNP s n) ;
    -- mkPN : N -> Number -> PN ;
    } ;

  mkA = overload {
    mkA : (adj : Str) -> A = \s -> lin A (mkAdj s) ;
    -- mkA : (kiga : Str) -> (jakda : A) -> A = \kiga,jakda ->
    --   jakda ** {s = \\af => kiga ++ jakda.s ! af} ;
    } ;

  mkA2 = overload {
    mkA2 : A -> A2 = \a -> a ** {c2 = casePrep Nom} ;
    mkA2 : Str -> Prep -> A2 = \s,p ->
      lin A2 {s = (mkAdj s).s ; c2 = p} ;
    mkA2 : Str -> Case -> A2 = \s,c ->
      lin A2 {s = (mkAdj s).s ; c2 = casePrep c} ;
    mkA2 : A -> Prep -> A2 = \a,p ->
      lin A2 (a ** {c2 = p}) ;
    } ;

  mkV = overload {
    mkV : (sg3 : Str) -> V = \v -> lin V (mkVerb v) ;
    -- mkV : (nore : Str) -> (hada : V) -> V = \nore,hada -> hada ** {
    --   s = \\vf => nore + hada.s ! vf} ;
  } ;

  copula = ResHun.copula ;

  mkV2 = overload {
    mkV2 : (plain : Str) -> V2 = \v2 -> lin V2 (mkVerb2 v2) ;
    mkV2 : V -> V2 = vtov2 ;
    } ;

  mkV3 = overload {
    mkV3 : (plain : Str) -> V3 = \v3 -> lin V3 (mkVerb3 v3) ;
    } ;
  --
  -- mkVV = overload {
  --  } ;

  mkPrep = overload {
    mkPrep : (e : Str) -> Prep
      = \str -> lin Prep (ResHun.mkPrep str) ;
    mkPrep : Str -> Case -> Prep
      = \str,c -> lin Prep (ResHun.mkPrep str ** {c = c}) ;
    } ;

  casePrep : Case -> Prep
    = \c -> lin Prep (ResHun.mkPrep [] ** {c = c}) ;
--------------------------------------------------------------------------------

}
