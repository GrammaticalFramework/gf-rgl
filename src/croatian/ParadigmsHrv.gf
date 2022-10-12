resource ParadigmsHrv = open CatHrv, ResHrv, (R=ResHrv), Prelude in {

----------------
-- Parameters

oper
  singular : Number
    = Sg ;
  plural : Number
    = Pl ;

  mascAnimate : Gender
    = Masc Anim ;
  mascInanimate : Gender
    = Masc Inanim ;
  masculine : Gender
    = Masc Inanim ;
  feminine : Gender
    = Fem ;
  neuter : Gender
    = Neutr ;

  nominative : Case
    = Nom ;
  genitive : Case
    = Gen ;
  dative : Case
    = Dat ;
  accusative : Case
    = Acc ;
  vocative : Case
    = R.Voc ;
  locative : Case
    = Loc ;
  instrumental : Case
    = Ins ;

------------------------------
-- Nouns

oper

  mkN = overload {
    mkN : (sgnom : Str) -> N        -- guessing gender
      = \sgnom -> lin N (smartLexNoun sgnom) ;
    mkN : (sgnom : Str) -> Gender -> N 
      = \sgnom, g -> lin N (mkgLexNoun sgnom g) ;
    mkN : NForms -> Gender -> N     -- the worst case
      = \nfs,g -> lin N (nfs ** {g = g}) ;
    } ;

-- The following standard declensions can be used with good accuracy.
-- However, they have some defaults that may have to be overwritten.
-- This can be done easily by overriding those formes with record extension (**).

  NForms = {snom,sgen,sdat,sacc,svoc,sins,pnom,pgen,pdat,pacc : Str} ;

  izvorNForms : Str -> NForms
    = izvorN ;
  nokatNForms : Str -> NForms
    = nokatN ;
  gradaninNForms : Str -> NForms
    = gradaninN ;
  vojnikNForms : Str -> NForms
    = vojnikN ;
  bubregNForms : Str -> NForms
    = bubregN ;
  trbuhNForms : Str -> NForms
    = trbuhN ;
  cvorakNForms : Str -> NForms
    = cvorakN ;
  panjNForms : Str -> NForms
    = panjN ;
  suzanjNForms : Str -> NForms
    = suzanjN ;
  pristNForms : Str -> NForms
    = pristN ;
  stricNForms : Str -> NForms
    = stricN ;
  klinacNForms : Str -> NForms
    = klinacN ;
  posjetilacNForms : Str -> NForms
    = posjetilacN ;
  pepeoNForms : Str -> NForms
    = pepeoN ;
  ugaoNForms : Str -> NForms
    = ugaoN ;
  bifeNForms : Str -> NForms
    = bifeN ;
  ziriNForms : Str -> NForms
    = ziriN ;
  taksiNForms : Str -> NForms
    = taksiN ;
  koljenoNForms : Str -> NForms
    = koljenoN ;
  jedroNForms : Str -> NForms
    = jedroN ;
  poljeNForms : Str -> NForms
    = poljeN ;
  zenaNForms : Str -> NForms
    = zenaN ;

  PNForms : Type = {snom, sgen, sdat, sacc, svoc, sins : Str} ;

  mkPN = overload {
    mkPN : Str -> PN
      = \s -> 
        let
	  nf = smartLexNoun s ;
	  n = nounFormsNoun nf nf.g
	in lin PN {
	  s = \\c => n.s ! Sg ! c ; ---- TODO check this
          g = nf.g
          } ;
    mkPN : PNForms -> Gender -> PN
      = \fs, g -> lin PN {
        s = table {
          Nom => fs.snom ;
          Gen => fs.sgen ;
          Dat | Loc => fs.sdat ;
          Acc => fs.sacc ;
          Voc => fs.svoc ;
          Ins => fs.sins 
          } ;
	g = g
        } ;
    } ;

---------------------
-- Adjectives

-- Only positive forms so far ----

  mkA = overload {
    mkA : Str -> A
      = \s ->
        let
	  velik = velikA s ;
	  velikiji = regComparAForms velik ; 
	in lin A {
	  posit = velik ;
	  compar = velikiji ;
	  superl = superlAForms velikiji
	  } ;
    mkA : (pos, comp : Str) -> A
      = \pos, comp -> lin A {
          posit = velikA pos ;
	  compar = velikA comp ;
	  superl = superlAForms (velikA comp)
	  } ;
    mkA : (posit : AForms) -> (compar : Str) -> A
      = \posit,compar -> lin A {
          posit = posit ;
	  compar = velikA compar ;
	  superl = superlAForms (velikA compar)
	  } ;
    mkA : (posit, compar : AForms) -> A
      = \posit,compar -> lin A {
          posit = posit ;
	  compar = compar ;
	  superl = superlAForms compar
	  } ;
    mkA : (posit : AForms) -> A
      = \posit ->
          let
            compar = regComparAForms posit
          in lin A {
            posit = posit ;
	    compar = compar ;
	    superl = superlAForms compar
	    } ;
    } ;

  invarA : Str -> A
    = \s -> lin A {posit,compar,superl = invarAForms s} ; ---- TODO compar, superl?

  AForms : Type
    = R.AdjForms ;

-- the complete definition of AForms is
--   {msnom, fsnom, nsnom, msgen, fsgen, msdat,
--    fsdat, fsacc, msloc, msins, fsins, mpnom, pgen : Str} ;

  velikAForms : Str -> AForms
    = velikA ;
    
  invarAForms : Str -> AForms
    = \s -> invarAdjForms s ;


-------------------------
-- Verbs

  mkV = overload {
    mkV : (raditi : Str) -> V
      = \s -> lin V {s = smartVerbForms s} ;
    mkV : (raditi, radem, radio : Str) -> V
      = \raditi, radem, radio ->
           lin V {s = aeiVerbForms raditi radem radio} ;
    mkV : VerbForms -> V
      = \vf -> lin V {s = vf} ;
    } ;


  mkV2 = overload {
    mkV2 : V -> V2
      = \v -> lin V2 {s = v.s ;
           c = {s = [] ; c = accusative ; hasPrep = False}} ;
    mkV2 : V -> Case -> V2
      = \v,c -> lin V2 {s = v.s ;
           c = {s = [] ; c = c ; hasPrep = False}} ;
    mkV2 : V -> Prep -> V2 
      = \v,c -> lin V2 {s = v.s ; c = c} ;
    } ;

------------------------
-- Adverbs, prepositions, conjunctions, ...

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;

  mkPrep = overload {
    mkPrep : Str -> Prep    -- genitive prepositions
      = \s -> lin Prep {s = s ; c = genitive ; hasPrep = True} ;
    mkPrep : Case -> Prep   -- oblique cases, empty string
      = \c -> lin Prep {s = [] ; c = c ; hasPrep = False} ;
    mkPrep : Str -> Case -> Prep
      = \s,c -> lin Prep {s = s ; c = c ; hasPrep = True} ;
    } ;
    
  mkConj : Str -> Conj
    = \s -> lin Conj {s1 = [] ; s2 = s} ;

  mkAdN : Str -> AdN
    = \s -> lin AdN {s = s} ;
    
  mkOrd : Str -> Ord
    = \s -> lin Ord (velikA s) ;

  ifPluralNP : NP -> Bool
    = \np -> case np.a of {
      Ag _ Pl _ => True ;
      _ => False
      } ;

}
