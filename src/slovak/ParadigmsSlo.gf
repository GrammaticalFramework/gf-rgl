resource ParadigmsSlo = open CatSlo, ResSlo, (R=ResSlo), Prelude in {

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
    mkN : (nom,gen : Str) -> Gender -> N
      = \nom,gen,g -> lin N (declensionNounForms nom gen g) ;
    } ;

-- The following standard declensions can be used with good accuracy.
-- However, they have some defaults that may have to be overwritten.
-- This can be done easily by overriding those formes with record extension (**).
-- The default extensions are shown in comments; if the default is correct, no extension is needed.
-- Notice that some paradigms take two arguments, some take one.

  chlapN : Str -> N  
    = \s -> lin N (R.chlapN s) ;
  hrdinaN : Str -> N 
    = \s -> lin N (R.hrdinaN s) ;
  dubN : Str -> N  
    = \s -> lin N (R.dubN s) ;
  strojN : Str -> N
    = \s -> lin N (R.strojN s) ;
  ponyN : Str -> N
    = \s -> lin N (R.ponyN s) ;
  zenaN : (snom, pgen : Str) -> N
    = \s,p -> lin N (R.zenaN s) ** {pgen = p} ;
  ulicaN : (snom, pgen : Str) -> N
    = \s,p -> lin N (R.ulicaN s) ** {pgen = p} ;
  dlanN  : (snom, pgen : Str) -> N
    = \s,p -> lin N (R.dlanN s p) ;
  kostN  : (snom, pgen : Str) -> N
    = \s,p -> lin N (R.kostN s p) ;
  mestoN : (snom, pgen : Str) -> N
    = \s,p -> lin N (R.mestoN s) ** {pgen = p} ;
  srdceN : (snom, pgen : Str) -> N
    = \s,p -> lin N (R.srdceN s) ** {pgen = p} ;
  vysvedcenieN : Str -> N
    = \s -> lin N (R.vysvedcenieN s) ;
  dievcaN : Str -> N
    = \s -> lin N (R.dievcaN s) ;
  dievceniecN : Str -> N
    = \s -> lin N (R.dievceniecN s) ;

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
      = \s -> lin A (guessAdjForms s)
    } ;

  peknyA : Str -> A
    = \s -> lin A (R.peknyA s) ;
  krasnyA : Str -> A
    = \s -> lin A (R.krasnyA s) ;
  cudziA : Str -> A
    = \s -> lin A (R.cudziA s) ;
  rydziA : Str -> A
    = \s -> lin A (R.rydziA s) ;
  otcovA : Str -> A
    = \s -> lin A (R.otcovA s) ;
  paviA  : Str -> A
    = \s -> lin A (R.paviA s) ;

  invarA : Str -> A
    = \s -> lin A (invarAdjForms s) ;

  mkA2 : A -> Prep -> A2
    = \a,p -> lin A2 (a ** {c = p}) ;

-- the full definition of the adjective record is
-- {
--    msnom, fsnom, nsnom, msgen, fsgen, msdat, fsacc, msloc, msins, fsins,
--    ampnom, pgen, pins : Str
-- }
--

-------------------------
-- Verbs

  mkV2 = overload {
    mkV2 : VerbForms -> VerbForms ** {c : ComplementCase}
      = \vf -> vf ** {c = {s = [] ; c = Acc ; hasPrep = False}} ;
    mkV2 : VerbForms -> Case -> VerbForms ** {c : ComplementCase}
      = \vf,c -> vf ** {c = {s = [] ; c = c ; hasPrep = False}} ;
    mkV2 : VerbForms -> ComplementCase -> VerbForms ** {c : ComplementCase}
      = \vf,c -> vf ** {c = c} ;
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
