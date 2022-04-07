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
    mkN : (sgnom : Str) -> N ; -- Predictable nouns from singular nominative. Accusative vowel is o/ö, no stem lowering. Use: `mkN "nap"` for nap, napot.
    mkN : (sgnom, sggen : Str) -> N ; -- Singular nominative and accusative. Use: `mkN "név" "nevet"`
    mkN : (sgnom, sggen, plnom : Str) -> N ; -- Singular nominative, singular accusative, plural nominative. Use: `mkN "falu" "falut" "falvak"`
    mkN : (sgnom, sggen, plnom, sgnom_possdSg3 : Str) -> N ; -- Singular nominative, singular accusative, plural nominative, singular nominative possessed by 3rd person singular. Use: `mkN "virág" "virágot" "virágok" "virága"` (would give "virágja" otherwise)
    mkN : (unoka : Str) -> (testvér : N) -> N ; -- Compound noun. Use: `mkN "unoka" (mkN "testvér")` (would give wrong harmony with `mkN "unokatestvér"`)
  } ;

  mkPN : overload {
    mkPN : Str -> PN ; -- Singular PN out of a string
    mkPN : Str -> Number -> PN -- PN with a given number
    -- mkPN : N -> Number -> PN ;
    } ;

--2 Adjectives

  mkA : overload {
    mkA : (sgnom : Str) -> A ; -- Regular adjective, given in singular nominative
    mkA : (sgnom, sgacc : Str) -> A ; -- Singular nominative and accusative
    mkA : N -> A ; -- Adjective from a noun. mkN has more paradigms, so anything irregular goes via N.
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
    = \s,c -> lin Prep (ResHun.prepos c s) ;

  casePrep : Case -> Prep ; -- No postposition, only case

  -- mkConj : (_,_ : Str) -> Number -> Conj = \s1,s2,num ->
  --   lin Conj { s = s1 ; s2 = s2 } ;

  -- mkSubj : Str -> Bool -> Subj = \s,b ->
  --   lin Subj { } ;

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s ; isPre=False} ;

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
    mkN : Str -> N = \s ->
      let mw : Multiword = splitMultiword s ;
       in case mw of {
          <pr,n> => multiwordN pr (regNoun n)
      } ;

    mkN : Str -> Str -> N = \n,a ->
      let mwn : Multiword = splitMultiword n ;
          mwa : Multiword = splitMultiword a ;
       in multiwordN mwn.p1 (regNounNomAcc mwn.p2 mwa.p2) ;

    mkN : Str -> Str -> Str -> N = \n,a,pln ->
      let mwn : Multiword = splitMultiword n ;
          mwa : Multiword = splitMultiword a ;
          mwpln : Multiword = splitMultiword pln ;
       in multiwordN mwn.p1 (regNounNomAccPl mwn.p2 mwa.p2 mwpln.p2) ;

    mkN : (x1,_,_,x4 : Str) -> N = \n,a,pln,possd ->
      let mwn : Multiword = splitMultiword n ;
          mwa : Multiword = splitMultiword a ;
          mwpln : Multiword = splitMultiword pln ;
          mwpossd : Multiword = splitMultiword possd ;
       in multiwordN mwn.p1 (regNoun4 mwn.p2 mwa.p2 mwpln.p2 mwpossd.p2) ;

    mkN : (unoka : Str) -> (testvér : N) -> N = compoundN ;

    ---------------------------------------------------------------------
    -- Not in the visible API. TODO remove, improve or document better --
    ---------------------------------------------------------------------

    -- Worst case with 9 strings.
    mkN : (x1,_,_,_,_,_,_,_,x9 : Str) -> N =
      \nomsg,accsg,supsg,allsg,nompl,f,g,h,i ->
      lin N (worstCaseNoun nomsg accsg supsg allsg nompl
                           f g h i (harmFromSgAll allsg)) ;

    -- Noun with unpredictable vowel harmony and plural allomorph
    mkN : Str -> (plural : Str) -> Harmony -> N =
      \s,pl,h -> lin N (mkNounHarm h pl s)

    } ;

   multiwordN : Str -> Noun -> N =
     \prefix,n -> case prefix of {
       _ + "-" => compoundN prefix n ;
       _       => lin N (n ** {s = \\x => prefix ++ n.s ! x})
     } ;

   compoundN : (unoka : Str) -> (testvér : Noun) -> N = -- Compound noun: e.g. `mkN "unoka" (mkN "testvér")`.
     \prefix,n -> lin N (n ** {s = \\x => prefix + n.s ! x}) ;

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
    mkA : (sgnom : Str) -> A = \s -> lin A (mkAdj s) ;
    mkA : (sgnom,sgacc : Str) -> A = \nom,acc ->
      lin A (mkAdj2 nom (regNounNomAcc nom acc)) ;
    mkA : N -> A = \noun ->
      let sgnom : Str = noun.s ! SgNom in
      lin A (mkAdj2 sgnom noun) ;
    } ;

  mkA2 = overload {
    mkA2 : A -> A2 = \a -> a ** {c2 = casePrep Nom ; isPost = False} ;
    mkA2 : Str -> Prep -> A2 = \s,p ->
      lin A2 ((mkAdj s) ** {c2 = p ; isPost = False}) ;
    mkA2 : Str -> Case -> A2 = \s,c ->
      lin A2 ((mkAdj s) ** {c2 = casePrep c ; isPost = False}) ;
    mkA2 : A -> Prep -> A2 = \a,p ->
      lin A2 (a ** {c2 = p ; isPost = False}) ;
    } ;

  mkV = overload {
    mkV : (sg3 : Str) -> V = \v -> lin V (mkVerb v) ;
    -- mkV : (nore : Str) -> (hada : V) -> V = \nore,hada -> hada ** {
    --   s = \\vf => nore + hada.s ! vf} ;
    mkV : (x1,_,_,_,_,_,x7 : Str) -> V = \sg1,sg2,sg3,pl1,pl2,pl3,inf ->
      lin V (mkVerbFull sg1 sg2 sg3 pl1 pl2 pl3 inf) ;
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
      = \str -> lin Prep (ResHun.nomAdp str) ;
    mkPrep : Str -> Case -> Prep
      = \str,c -> lin Prep (ResHun.caseAdp c str) ;
    } ;

  casePrep : Case -> Prep
    = \c -> lin Prep (ResHun.caseAdp c) ;
--------------------------------------------------------------------------------

}
