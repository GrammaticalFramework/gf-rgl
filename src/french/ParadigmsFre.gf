--# -path=.:../romance:../common:../abstract:../../prelude

--1 French Lexical Paradigms
--
-- Aarne Ranta 2001 - 2006
--
-- This is an API for the user of the resource grammar 
-- for adding lexical items. It gives functions for forming
-- expressions of open categories: nouns, adjectives, verbs.
-- 
-- Closed categories (determiners, pronouns, conjunctions) are
-- accessed through the resource syntax API, $Structural.gf$. 
--
-- The main difference with $MorphoFre.gf$ is that the types
-- referred to are compiled resource grammar types. We have moreover
-- had the design principle of always having existing forms, rather
-- than stems, as string arguments of the paradigms.
--
-- The structure of functions for each word class $C$ is the following:
-- first we give a handful of patterns that aim to cover all
-- regular cases. Then we give a worst-case function $mkC$, which serves as an
-- escape to construct the most irregular words of type $C$.
-- However, this function should only seldom be needed: we have a
-- separate module [``IrregFre`` ../../french/IrregFre.gf],
-- which covers all irregularly inflected verbs.

resource ParadigmsFre = 
  open 
    (Predef=Predef), 
    Prelude, 
    MorphoFre, 
    BeschFre,
    CatFre in {

  flags optimize=all ;
    coding=utf8 ;

--2 Parameters 
--
-- To abstract over gender names, we define the following identifiers.

oper
  Gender : Type ; 

  masculine : Gender ;
  feminine  : Gender ;

-- To abstract over number names, we define the following.

  Number : Type ;  --%

  singular : Number ; --%
  plural   : Number ; --%

-- Prepositions used in many-argument functions are either strings
-- (including the 'accusative' empty string) or strings that
-- amalgamate with the following word (the 'genitive' "de" and the
-- 'dative' "à").

  accusative : Prep ; -- direct object case
  genitive   : Prep ; -- genitive, constructed with "de"
  dative     : Prep ; -- dative, usually constructed with "à"

  mkPrep : overload {
    mkPrep : Str -> Prep  ;         -- simple preposition (other than "de" and "à")
    mkPrep : Str -> Prep -> Prep ;  -- complex preposition e.g. "à côté de"
    } ;


--2 Nouns

  mkN : overload {

-- The regular function uses heuristics to compute the
-- plural and the gender from the singular. The plural 
-- heuristic currently
-- covers the cases "pas-pas", "prix-prix", "nez-nez", 
-- "bijou-bijoux", "cheveu-cheveux", "plateau-plateaux", "cheval-chevaux".
-- The gender heuristic is less reliable: it treats as feminine all
-- nouns ending with "e" and "ion", all others as masculine.

    mkN : (cheval : Str) -> N ;  -- predictable, with variations like cheval-chevaux

-- Adding gender information widens the scope of the regular pattern.

    mkN : (foie : Str) -> Gender -> N ; --%

-- In the worst case, both singular and plural forms and the gender are needed. 

    mkN : (oeil,yeux : Str) -> Gender -> N ; -- worst-case noun

--3 Compound nouns 
--
-- Some nouns are ones where the first part is inflected as a noun but
-- the second part is not inflected. e.g. "numéro de téléphone". 
-- They could be formed in syntax, but we give a shortcut here since
-- they are frequent in lexica.

    mkN : N -> Str -> N -- compound noun, e.g. numéro + de téléphone
  } ; 




--3 Relational nouns 
-- 
-- Relational nouns ("fille de x") need a case and a preposition. 

  mkN2 : N -> Prep -> N2 ; -- e.g. fille + genitive

-- The most common cases are the genitive "de" and the dative "à", 
-- with the empty preposition.

  deN2 : N -> N2 ; --%
  aN2  : N -> N2 ; --%

-- Three-place relational nouns ("la connection de x à y") need two prepositions.

  mkN3 : N -> Prep -> Prep -> N3 ; -- e.g. connection + genitive + dative


--3 Relational common noun phrases
--
-- In some cases, you may want to make a complex $CN$ into a
-- relational noun (e.g. "la vieille église de"). However, $N2$ and
-- $N3$ are purely lexical categories. But you can use the $AdvCN$
-- and $PrepNP$ constructions to build phrases like this.

-- 
--3 Proper names and noun phrases
--
-- Proper names need a string and a gender. If no gender is given, the
-- feminine is used for strings ending with "e", the masculine for other strings.

  mkPN  : overload {
    mkPN : Str -> PN ; -- feminine if ends with "e", otherwise masculine
    mkPN : Str -> Gender -> PN ; -- gender deviant from the simple rule
    mkPN : N -> PN ; -- gender inherited from noun
    } ;



--2 Adjectives

  mkA : overload {

-- For regular adjectives, all forms are derived from the
-- masculine singular. The heuristic takes into account certain
-- deviant endings: "banal-banale-banaux", "chinois-chinoise-chinois", 
-- "heureux-heureuse-heureux", "italien-italienne", "jeune-jeune",
-- "amer-amère", "carré- - -carrément", "joli- - -joliment".

    mkA : (cher : Str) -> A ; -- predictable, e.g. cher-chère

-- Often just the feminine singular is deviant.

    mkA : (sec,seche : Str) -> A ; -- unpredictable feminine

-- The masculine plural can also be deviant.

    mkA : (banal,banale,banaux : Str) -> A ;

-- This is the worst-case paradigm for the positive forms, except for "vieux/vieil".

    mkA : (banal,banale,banaux,banalement : Str) -> A ; -- almost worst-case adjective

-- This is the worst-case paradigm for the positive forms, used for "vieux/vieil".
    mkA : (vieux,vieil,vieille,vieuxs,vieuxment : Str) -> A ; -- worst-case adjetive

-- If comparison forms are irregular (i.e. not formed by "plus", e.g.
-- "bon-meilleur"), the positive and comparative can be given as separate
-- adjectives.

    mkA : A -> A -> A ; -- irregular comparison, e.g. bon-meilleur

    mkA : A -> CopulaType -> A -- force copula type
      
  } ;

-- The functions create by default postfix adjectives. To switch
-- them to prefix ones (i.e. ones placed before the noun in
-- modification, as in "petite maison"), the following function is
-- provided.

  prefixA : A -> A ; -- adjective that comes before noun, e.g. petit


--3 Two-place adjectives
--
-- Two-place adjectives need a preposition for their second argument.

  mkA2 : A -> Prep -> A2 ; -- e.g. supérieur + dative


--2 Adverbs

-- Adverbs are not inflected. Most lexical ones have position
-- after the verb. 

  mkAdv : Str -> Adv ; -- ordinary adverb

-- Some appear next to the verb (e.g. "toujours").

  mkAdV : Str -> AdV ; -- sentential adverb, e.g. toujours

-- Adverbs modifying adjectives and sentences can also be formed.

  mkAdA : Str -> AdA ; -- modify adjective, e.g. très


--2 Verbs
--
-- Irregular verbs are given in the module $IrregFre$. 
-- If a verb should be missing in that list, the module
-- $BeschFre$ gives all the patterns of the "Bescherelle" book.
-- 
-- Regular verbs are ones with the infinitive "er" or "ir", the
-- latter with plural present indicative forms as "finissons".
-- The regular verb function in the first conjugation recognizes
-- these endings, as well as the variations among
-- "aimer, céder, placer, peser, jeter, placer, manger, assiéger, payer".
--
-- Sometimes, however, it is not predictable which variant of the "er"
-- conjugation is to be selected. Then it is better to use the function
-- that gives the third person singular present indicative and future 
-- (("il") "jette", "jettera") as second argument.

  mkV : overload {
    mkV : (finir : Str) -> V ; -- regular 1/2/3 conjugation
    mkV : (jeter,jette : Str) -> V ; -- 1st and 2nd conjugation variations
    mkV : (jeter,jette,jettera : Str) -> V ; -- 1st conjugation variations

-- Here is a paradigm that works for most irregular verbs.

    mkV : (tenir,tiens,tenons,tiennent,tint,tiendra,tenu : Str) -> V ;

-- Here is a worst-case paradigm.

    mkV : (tenir,tiens,tient,tenons,tenez,tiennent,tienne,tenions,tiensI,tint,tiendra,tenu : Str) -> V ;

-- The $IrregFre$ list gives some verbs as two-place. These verbs can be
-- reused as one-place verbs.

    mkV : V2 -> V ; -- make 2-place to 1-place (e.g. from IrregFre)

-- Particle verbs ("avoir" + "besoin")

    mkV : V -> Str -> V  ;
  } ;

-- The function $mkV$ gives the default compound auxiliary "avoir".
-- To change it to "être", use the following function. 

  etreV : V -> V ; -- force auxiliary to be être (default avoir)

-- This function turns a verb into reflexive, which implies the auxiliary "être".

  reflV : V -> V ; -- reflexive, implies auxiliary être, e.g. se demander


--3 Two-place verbs
--
-- Two-place verbs need a preposition, except the special case with direct object.
-- (transitive verbs). 

  mkV2 = overload {
    mkV2 : Str -> V2  --%
    = \s -> dirV2 (regV s) ;
    mkV2 : V -> V2  -- direct transitive
    = dirV2 ;  
    mkV2 : V -> Prep -> V2 -- e.g. se fier + genitive
    = mmkV2
  } ;


--3 Three-place verbs
--
-- Three-place (ditransitive) verbs need two prepositions, of which
-- the first one or both can be absent.

  mkV3 : overload {
    mkV3 : V -> V3 ;                -- donner (+ accusative + dative)    
    mkV3 : V -> Prep -> V3 ;        -- placer (+ accusative) + dans
    mkV3 : V -> Prep -> Prep -> V3  -- parler + dative + genitive
    } ;

--3 Other complement patterns
--
-- Verbs and adjectives can take complements such as sentences,
-- questions, verb phrases, and adjectives.

  mkV0  : V -> V0 ;  --%
  mkVS  : V -> VS ;
  subjVS  : V -> VS ;
  mkVV  : V -> VV ;  -- plain infinitive: "je veux parler"
  deVV  : V -> VV ;  -- "j'essaie de parler"
  aVV   : V -> VV ;  -- "j'arrive à parler"
  mkV2S  : overload {
    mkV2S : V -> V2S ;
    mkV2S : V -> Prep -> V2S ;
    } ;
  mkV2V  : overload {
    mkV2V : V -> V2V ;
    mkV2V : V -> Prep -> Prep -> V2V ;
    } ;
  mkVA  : V -> VA ;

  mkV2A : overload {
    mkV2A : V -> V2A ;
    mkV2A : V -> Prep -> Prep -> V2A ;
    } ;

  mkVQ  : V -> VQ ;
  mkV2Q : V -> Prep -> V2Q ;

  mkAS  : A -> AS ; --%
  mkA2S : A -> Prep -> A2S ; --%
  mkAV  : A -> Prep -> AV ; --%
  mkA2V : A -> Prep -> Prep -> A2V ; --%

-- Notice: categories $AS, A2S, AV, A2V$ are just $A$,
-- and the second argument is given as an adverb. Likewise 
-- $V0$ is just $V$.

  V0 : Type ; --%
  AS, A2S, AV, A2V : Type ; --%

--.
--2 Definitions of the paradigms
--
-- The definitions should not bother the user of the API. So they are
-- hidden from the document.


  Gender = MorphoFre.Gender ; 
  Number = MorphoFre.Number ;
  masculine = Masc ;
  feminine = Fem ;
  singular = Sg ;
  plural = Pl ;

  Preposition = Compl ;
  accusative = complAcc ** {lock_Prep = <>} ;
  genitive = complGen ** {lock_Prep = <>} ;
  dative = complDat ** {lock_Prep = <>} ;
  mkPrep = overload {
    mkPrep : Str -> Prep  = \p -> {s = p ; c = CPrep PNul ; isDir = False ; lock_Prep = <>} ;
    mkPrep : Str -> Prep -> Prep = \s,c-> {s = s ; c = c.c ; isDir = False ; lock_Prep = <>}
    } ;

  --- obsolete
  Preposition : Type ;
  mkPreposition : Str -> Preposition ;
  mkPreposition s = mkPrep s ;

  regGenN : Str -> Gender -> N ;
  regN : Str -> N ;
  mk2N  : (oeil,yeux : Str) -> Gender -> N ;
  mk2N x y g = mkCNomIrreg x y g ** {lock_N = <>} ;
  regN x = regGenN x g where {
    g = case <x : Str> of {
     _ + ("e" | "ion") => Fem ;
     _ => Masc
     } 
    } ;
  regGenN x g = mkNomReg x g ** {lock_N = <>} ;
  compN : N -> Str -> N ;
  compN x y = {s = \\n => x.s ! n ++ y ; g = x.g ; lock_N = <>} ;

  mkN = overload {
    mkN : Str -> N = regN ;
    mkN : Str -> Gender -> N = regGenN ; 
    mkN : (oeil,yeux : Str) -> Gender -> N = mk2N ;
    mkN : N -> Str -> N = compN 
  } ; 


  mkN2 = \n,p -> n ** {lock_N2 = <> ; c2 = p} ;
  deN2 n = mkN2 n genitive ;
  aN2 n = mkN2 n dative ;
  mkN3 = \n,p,q -> n ** {lock_N3 = <> ; c2 = p ; c3 = q} ;

  regPN x = mk2PN x g where {
    g = case last x of {
      "e" => feminine ;
      _ => masculine
      }
    } ;

  mkPN = overload {
    mkPN : Str -> PN = regPN ;
    mkPN : Str -> Gender -> PN = \x,g -> lin PN {s = x ; g = g} ;
    mkPN : N -> PN = \x -> lin PN {s = x.s ! Sg ; g = x.g} ;
    } ;

  mk4A a b c d = mk5A a a b c d ;
  mk5A a b c d e = compADeg {s = \\_ => (mkAdj' a b c d e).s ; isPre = False ; copTyp = serCopula ; lock_A = <>} ;
  regA a = compADeg {s = \\_ => (mkAdjReg a).s ; isPre = False ; copTyp = serCopula ; lock_A = <>} ;
  prefA a = {s = a.s ; isPre = True ; copTyp = a.copTyp ; lock_A = <>} ;
  adjCopula a cop = a ** {copTyp = cop} ;

  mkA2 a p = a ** {c2 = p ; lock_A2 = <>} ;

  mkA = overload {
    mkA : Str -> A = regA ;
    mkA : (sec,seche : Str) -> A = \sec,seche -> mk4A sec seche (sec + "s") (seche + "ment") ; 
    mkA : (banal,banale,banaux : Str) -> A = \sec,seche,secs -> mk4A sec seche secs (seche + "ment") ; 
    mkA : (banal,banale,banaux,banalement : Str) -> A = mk4A ;
    mkA : (vieux,vieil,vieille,vieuxs,vieuxment : Str) -> A = mk5A ;
    mkA : A -> A -> A = mkADeg ;
    mkA : A -> CopulaType -> A = adjCopula ;
  };

  prefixA a = {s = a.s ; isPre = True ; copTyp = a.copTyp ; lock_A = <>} ;

  mkAdv x = ss x ** {lock_Adv = <>} ;
  mkAdV x = ss x ** {lock_AdV = <>} ;
  mkAdA x = ss x ** {lock_AdA = <>} ;

  regV x = let v = vvf (mkVerbReg x) in {s = v ; vtyp = VTyp VHabere (getVerbT v) ; lock_V = <> ; p = []} ;
  reg3V x y z = let v = vvf (mkVerb3Reg x y z) in {s = v ; vtyp = VTyp VHabere (getVerbT v) ; lock_V = <> ; p = []} ;
  etreV v = v ** {vtyp = VTyp VEsse (getVTypT v.vtyp)} ;
  reflV v = v ** {vtyp = vRefl v.vtyp} ;

  mmkV3 v p q = v ** {c2 = p ; c3 = q ; lock_V3 = <>} ;
  dirV3 v p = mmkV3 v accusative p ;
  dirdirV3 v = mmkV3 v dative accusative ;

  mkV3 = overload {
    mkV3 : V -> V3 = dirdirV3 ;               -- donner,_,_
    mkV3 : V -> Prep -> V3 = dirV3 ;          -- placer,_,sur
    mkV3 : V -> Prep -> Prep -> V3 = mmkV3    -- parler, à, de
    } ;

  V0 : Type = V ;
  AS, AV : Type = A ;
  A2S, A2V : Type = A2 ;

  mkV0  v = v ** {lock_V0 = <>} ;
  mkVS  v = v ** {m = \\_ => Indic ; lock_VS = <>} ; 
  subjVS  v = v ** {m = \\_ => Conjunct ; lock_VS = <>} ;

  mkV2S = overload {
    mkV2S : V -> V2S = \v -> mmkV2 v dative ** {mn,mp = Indic ; lock_V2S = <>} ;
    mkV2S : V -> Prep -> V2S = \v,p -> mmkV2 v p ** {mn,mp = Indic ; lock_V2S = <>} ;
    } ;

  mkVV  v = v ** {c2 = complAcc ; lock_VV = <>} ;
  deVV  v = v ** {c2 = complGen ; lock_VV = <>} ;
  aVV  v = v ** {c2 = complDat ; lock_VV = <>} ;

  mkV2V = overload {
    mkV2V : V -> V2V                 = \v -> mmkV3 v accusative dative ** {lock_V2V = <>} ;
    mkV2V : V -> Prep -> Prep -> V2V = \v,p,q -> mmkV3 v p q ** {lock_V2V = <>} ;
    } ;

  mkV2A = overload {
    mkV2A : V -> V2A                 = \v -> mmkV3 v accusative dative ** {lock_V2A = <>} ;
    mkV2A : V -> Prep -> Prep -> V2A = \v,p,q -> mmkV3 v p q ** {lock_V2A = <>} ;
    } ;

  mkVA  v = v ** {lock_VA = <>} ;

  mkVQ  v = v ** {lock_VQ = <>} ;
  mkV2Q v p = mmkV2 v p ** {lock_V2Q = <>} ;

  mkAS  v = v ** {lock_AS = <>} ; ---- more moods
  mkA2S v p = mkA2 v p ** {lock_A2S = <>} ;
  mkAV  v p = v ** {c = p.p1 ; s2 = p.p2 ; lock_AV = <>} ;
  mkA2V v p q = mkA2 v p ** {s3 = q.p2 ; c3 = q.p1 ; lock_A2V = <>} ;

--------------------------- obsolete

  makeNP : Str -> Gender -> Number -> NP ; 
  makeNP x g n = {s = (pn2np {s=x;g= g}).s; a = agrP3 g n ; hasClit = False ; isPol = False ; isNeg = False ; lock_NP = <>} ;
  regPN : Str -> PN ; 
  mk2PN : Str -> Gender -> PN = \x,g -> {s = x ; g = g} ** {lock_PN = <>} ;

  mkADeg : A -> A -> A ;
  compADeg : A -> A ;

  regA : Str -> A ;
  mk4A : (banal,banale,banaux,banalement : Str) -> A ;
  mk5A : (vieux,vieil,vieille,vieuxs,vieuxment : Str) -> A ; -- worst-case adjetive

  prefA : A -> A ;
  adjCopula : A -> CopulaType -> A ;

  mkADeg a b = 
    {s = table {Posit => a.s ! Posit ; _ => b.s ! Posit} ; isPre = a.isPre ; copTyp = a.copTyp ; lock_A = <>} ;
  compADeg a = 
    {s = table {Posit => a.s ! Posit ; _ => \\f => "plus" ++ a.s ! Posit ! f} ; 
     isPre = a.isPre ;
     copTyp = a.copTyp ;
     lock_A = <>} ;

  mkV = overload {
    mkV : Str -> V = regV ;
    mkV : (jeter,jette : Str) -> V = 
      \x,y -> let v = vvf (mkVerb2Reg x y) in {s = v ; vtyp = VTyp VHabere (getVerbT v) ; lock_V = <> ; p = []} ;
    mkV : (jeter,jette,jettera : Str) -> V = reg3V ;
    mkV : V2 -> V = v2V ;
    mkV : (tenir,tiens,tenons,tiennent,tint,tiendra,tenu : Str) -> V
    = \tenir,tiens,tenons,tiennent,tint,tiendra,tenu -> 
      let v = vvf (mkVerb7 tenir tiens tenons tiennent tint tiendra tenu) in
      {s = v ; vtyp = VTyp VHabere (getVerbT v) ; lock_V = <> ; p = []} ;
    mkV : (tenir,tiens,tient,tenons,tenez,tiennent,tienne,tenions,tiensI,tint,tiendra,tenu : Str) -> V
    = \tenir,tiens,tient,tenons,tenez,tiennent,tienne,tenions,tiensI,tint,tiendra,tenu -> 
      let v = vvf (mkVerb12 tenir tiens tient tenons tenez tiennent tienne tenions tiensI tint tiendra tenu) in
      {s = v ; vtyp = VTyp VHabere (getVerbT v) ; lock_V = <> ; p = []} ;
   mkV : V -> V
    = \v -> v ;
    mkV : V -> Str -> V 
    = \v,p -> v ** {p = p} ;  ---- to recognize particles in dict, not yet in lincat V
  } ;

  regV : Str -> V ;
  reg3V : (jeter,jette,jettera : Str) -> V ;

  mmkV2 : V -> Prep -> V2 ;
  mmkV2 v p = v ** {c2 = p ; lock_V2 = <>} ;
  dirV2 : V -> V2 = \v -> mmkV2 v accusative ;
  v2V : V2 -> V ;
  v2V v = v ** {lock_V = <>} ;

  mmkV3    : V -> Prep -> Prep -> V3 ;  -- parler, à, de
  dirV3    : V -> Prep -> V3 ;          -- donner,_,à
  dirdirV3 : V -> V3 ;                  -- donner,_,_

  getVerbT : (VF => Str) -> VBool = \v -> case last (v ! (VFin (VPres Indic) Sg P3)) of {
    "a" | "e" => VTrue ; -- parle-t-il, va-t-il
    _ => VFalse  -- prend-il
    } ;


} ;
