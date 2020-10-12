resource ParadigmsCze = open CatCze, ResCze, Prelude in {

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
    = ResCze.Voc ;
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

  panN : Str -> N       -- default ** {pnom = +i}
    = \s -> lin N (declPAN s) ;
  predsedaN : Str -> N  -- default ** {sgen = +i}
    = \s -> lin N (declPREDSEDA s) ;
  hradN : Str -> N      -- default ** {sgen,sloc = +u}
    = \s -> lin N (declHRAD s) ;
  zenaN : Str -> N      -- default ** {pgen = zen}
    = \s -> lin N (declZENA s) ;
  mestoN : Str -> N     -- default ** {sloc = +u ; pgen = mest ; ploc = +ech}
    = \s -> lin N (declMESTO s) ;
  muzN : Str -> N
    = \s -> lin N (declMUZ s) ;
  soudceN : Str -> N    -- default ** {sdat,sloc = +i ; pnom = +i}
    = \s -> lin N (declSOUDCE s) ;
  strojN : Str -> N
    = \s -> lin N (declSTROJ s) ;
  ruzeN : Str -> N
    = \s -> lin N (declRUZE s) ;
  pisenN : Str -> N
    = \s -> lin N (declPISEN s) ;
  kostN : Str -> N
    = \s -> lin N (declKOST s) ;
  kureN : Str -> N
    = \s -> lin N (declKURE s) ;
  moreN : Str -> N     -- default ** {pgen = +í}
    = \s -> lin N (declMORE s) ;
  staveniN : Str -> N
    = \s -> lin N (declSTAVENI s) ;

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
      = \s -> lin A (guessAdjForms s) ;
    } ;

  mladyA : Str -> A
    = \s -> lin A (mladyAdjForms s) ;
  jarniA : Str -> A
    = \s -> lin A (jarniAdjForms s) ;
  otcuvA : Str -> A
    = \s -> lin A (otcuvAdjForms s) ;
  matcinA : Str -> A
    = \s -> lin A (matcinAdjForms s) ;

  invarA : Str -> A
    = \s -> lin A (invarAdjForms s) ;

  mkA2 : A -> Prep -> A2
    = \a,p -> lin A2 (a ** {c = p}) ;

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
