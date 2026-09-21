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
    -- Select a default paradigm; mixed endings and stem alternations may
    -- still need lexical overrides on the result.
    mkN : (nom,gen : Str) -> Gender -> N
      = \nom,gen,g -> lin N (declensionNounForms nom gen g) ;
    } ;

  mkPN = overload {
    -- Indeclinable name: every case uses the supplied string.
    mkPN : Str -> Gender -> PN = \s,g -> lin PN {s = \\_ => s ; g = g} ;
    -- Inflected name: use the noun paradigm's singular cases and gender.
    mkPN : N -> PN = \n -> lin PN {s = (nounFormsNoun n).s ! Sg ; g = n.g} ;
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
--  g,gPl : Gender
-- }


---------------------
-- Adjectives

-- Guess regular comparison; supply a principal part for exceptions, or
-- nonExist as the comparative for a positive-only adjective.

  mkA = overload {
    mkA : Str -> A
      = \s -> lin A (degreeAdjForms s (guessComparative s)) ;
    mkA : (positive,comparative : Str) -> A
      = \p,c -> lin A (degreeAdjForms p c) ;
    } ;

  -- Declension constructors supply positive forms only.
  mladyA : Str -> A
    = \s -> lin A (positiveAdj (mladyAdjForms s)) ;
  jarniA : Str -> A
    = \s -> lin A (positiveAdj (jarniAdjForms s)) ;
  otcuvA : Str -> A
    = \s -> lin A (positiveAdj (otcuvAdjForms s)) ;
  matcinA : Str -> A
    = \s -> lin A (positiveAdj (matcinAdjForms s)) ;

  invarA : Str -> A
    = \s -> lin A (positiveAdj (invarAdjForms s)) ;

  -- Short adjectives supply predicates, not attributive AP forms.
  shortAP : (m,f,n,mp,fp,np : Str) -> AP = \m,f,n,mp,fp,np ->
    let ap : Adjective = {
    s = \\g,num,c => case <num,c,g> of {
      <Sg,Nom|ResCze.Voc,Masc _> => m ; <Sg,Nom|ResCze.Voc,Fem> => f ; <Sg,Nom|ResCze.Voc,Neutr> => n ;
      <Pl,Nom|ResCze.Voc,Masc Anim> => mp ; <Pl,Nom|ResCze.Voc,Neutr> => np ;
      <Pl,Nom|ResCze.Voc,_> => fp ; _ => nonExist
      }
    } in lin AP {
      s = \\_,_,_ => nonExist ; pred = shortPredicate ap ; isPost = True
      } ;

  mkA2 : A -> Prep -> A2
    = \a,p -> lin A2 (a ** {c = p}) ;

-------------------------
-- Verbs

  -- Class constructors, not guesses from an arbitrary infinitive.
  -- kupovat: -ovat, -uji, -oval, -uj; kryt: -ýt/-ít, -yji/-iji, -yl/-il.
  kupovatV : Str -> V = \s -> lin V (iii_kupovatVerbForms s) ;
  krytV : Str -> V = \s -> lin V (iii_krýtVerbForms s) ;

  -- Full present and imperative forms, with masculine singular/plural past
  -- participles. Keep this twelve-field input compatible as VerbForms grows.
  -- Storing past participles does not yet implement past-tense clauses.
  VerbPrincipalParts : Type = PositiveVerbForms ;

  mkV = overload {
    mkV : VerbPrincipalParts -> V = \v -> lin V (withNeg v) ;
    mkV : (inf,p1sg,p2sg,p3sg,p1pl,p2pl,p3pl,pastsg,pastpl,imp2sg,imp1pl,imp2pl : Str) -> V =
      \inf,p1sg,p2sg,p3sg,p1pl,p2pl,p3pl,pastsg,pastpl,imp2sg,imp1pl,imp2pl -> lin V (withNeg {
        inf = inf ; pressg1 = p1sg ; pressg2 = p2sg ; pressg3 = p3sg ;
        prespl1 = p1pl ; prespl2 = p2pl ; prespl3 = p3pl ;
        pastpartsg = pastsg ; pastpartpl = pastpl ;
        impsg2 = imp2sg ; imppl1 = imp1pl ; imppl2 = imp2pl
        }) ;
    } ;

  -- Lexical reflexive clitics. The case-based interface accepts only Acc/Dat.
  seV : V -> V = \v -> reflV v Acc ;
  siV : V -> V = \v -> reflV v Dat ;
  reflV : V -> Case -> V = \v,c -> v ** {
    isRefl = True ; refl = case c of {Acc => "se" ; Dat => "si" ; _ => nonExist}
    } ;

  mkVS : V -> VS = \v -> lin VS v ;
  mkVQ : V -> VQ = \v -> lin VQ v ;
  -- Ordinary VV: the infinitive retains its own clitic domain.
  mkVV : V -> VV = \v -> lin VV (v ** {isAux = False}) ;
  -- Modals allow their infinitive's clitics in the finite clause. A lexical
  -- reflexive on the matrix verb blocks this climbing.
  mkModalVV : V -> VV = \v -> lin VV (v ** {isAux = notB v.isRefl}) ;
  -- Third-person singular gender selects personal and possessive forms.
  -- A no-op keeps lexical overrides; conversion uses the standard target forms.
  genderPron : Gender -> Pron -> Pron = \g,p -> case p.a of {
    Ag old Sg P3 => case <g,old> of {
      <Fem,Fem> | <Neutr,Neutr> |
      <Masc Anim,Masc Anim> | <Masc Inanim,Masc Inanim> => p ;
      _ => lin Pron ((mkPron (Ag g Sg P3)) ** {isDrop = p.isDrop})
      } ;
    _ => p ** {
      a = case p.a of {Ag _ n person => Ag g n person ; AgPol _ => AgPol g ; AgQuant _ => AgQuant g} ;
      nom = case p.a of {
        Ag _ Pl P3 => (personalPron (Ag g Pl P3)).nom ; _ => p.nom
        }
      }
    } ;

  mkV2 = overload {
    mkV2 : V -> V2
      = \v -> lin V2 (v ** {c = {s = [] ; c = Acc ; hasPrep = False}}) ;
    mkV2 : V -> Case -> V2
      = \v,c -> lin V2 (v ** {c = {s = [] ; c = c ; hasPrep = False}}) ;
    mkV2 : V -> Prep -> V2
      = \v,p -> lin V2 (v ** {c = p}) ;
    } ;

  mkV3 = overload {
    mkV3 : V -> V3
      = \v -> lin V3 (v ** {c = {s = [] ; c = Acc ; hasPrep = False} ;
                           c2 = {s = [] ; c = Dat ; hasPrep = False}}) ;
    mkV3 : V -> Prep -> Prep -> V3
      = \v,p,p2 -> lin V3 (v ** {c = p ; c2 = p2}) ;
    } ;

------------------------
-- Adverbs, prepositions, conjunctions, ...
 
  mkAdA : Str -> AdA
    = \s -> lin AdA {s = s} ;

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;

  mkPrep : Str -> Case -> Prep
    = \s,c -> lin Prep {s = s ; c = c ; hasPrep = True} ; ---- True if s /= ""

  mkConj : Str -> Conj
    = \s -> lin Conj {s1 = [] ; s2 = s} ;


}
