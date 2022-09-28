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
    = Voc ;
  locative : Case
    = Loc ;
  instrumental : Case
    = Ins ;

------------------------------
-- Nouns

oper

  mkN = overload {
    mkN : (nom : Str) -> N
      = \nom -> lin N (guessNounForms nom) ;
    mkN : (nom,gen : Str) -> Gender -> N ---- TODO
      = \nom,gen,g -> lin N (guessNounForms nom) ;
    } ;

-- The following standard declensions can be used with good accuracy.
-- However, they have some defaults that may have to be overwritten.
-- This can be done easily by overriding those formes with record extension (**).
-- The default extensions are shown in comments; if the default is correct, no extension is needed.
-- Notice that some paradigms take two arguments, some take one.

---- TODO

-- The full definition of the noun record is
-- {
--  snom,sgen,sdat,sacc,svoc,sloc,sins, pnom,pgen,pdat,pacc,ploc,pins : Str ;
--  g : Gender
-- }


---------------------
-- Adjectives

-- Only positive forms so far ----

  mkA = overload {
    mkA : Str -> A
      = \s -> lin A (velikA s)
    } ;


-- the full definition of the adjective record is
-- {
--    msnom, fsnom, nsnom, msgen, fsgen, msdat, fsacc, msloc, msins, fsins,
--    ampnom, pgen, pins : Str
-- }
--

-------------------------
-- Verbs

  mkV2 = overload {
    mkV2 : VerbForms -> V2
      = \vf -> lin V2 {s = vf ; c = {s = [] ; c = Acc ; hasPrep = False}} ;
    mkV2 : VerbForms -> Case -> V2
      = \vf,c -> lin V2 {s = vf ; c = {s = [] ; c = c ; hasPrep = False}} ;
    mkV2 : VerbForms -> ComplementCase -> V2 
      = \vf,c -> lin V2 {s = vf ; c = c} ;
    } ;

------------------------
-- Adverbs, prepositions, conjunctions, ...

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;

  mkPrep : Str -> Case -> Prep
    = \s,c -> lin Prep {s = s ; c = c ; hasPrep = True} ; ---- True if s /= ""
    
  mkConj : Str -> Conj
    = \s -> lin Conj {s1 = [] ; s2 = s} ;


}
