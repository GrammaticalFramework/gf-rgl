--# -path=.:../romance:../common:../abstract:../prelude

-- ATTENTION: this module is documented with gfdoc; please be mindful --%
-- of the way comments are structured; read gfdoc's documentation if --%
-- you intend to change any comments in any way. --%

--1 Portuguese Lexical Paradigms
--
--
-- This is an API for the user of the resource grammar for adding
-- lexical items. It gives functions for forming expressions of open
-- categories: nouns, adjectives, verbs.
--
-- Closed categories (determiners, pronouns, conjunctions) are
-- accessed through the resource syntax API, $Structural.gf$.
--
-- The main difference with $MorphoPor.gf$ is that the types referred
-- to are compiled resource grammar types. We have moreover had the
-- design principle of always having existing forms, rather than
-- stems, as string arguments of the paradigms. Another important
-- difference with $MorphoPor.gf$ is that this API is committed to
-- backward-compatibility in a way that $MorphoPor.gf$ is not, so you
-- are discouraged to use the latter.
--
-- The structure of functions for each word class $C$ is the
-- following: first we give a handful of patterns that aim to cover
-- all regular cases. Then we give a worst-case function $mkC$, which
-- serves as an escape to construct the most irregular words of type
-- $C$. For verbs, there is a fairly complete list of irregular verbs
-- in [``IrregPor`` ../../portuguese/IrregPor.gf].

resource ParadigmsPor =
  open
    (Predef=Predef),
    Prelude,
    MorphoPor,
    BeschPor,
    CatPor in {

  flags optimize=all ;
        coding=utf8 ;

--2 Parameters
--
-- To abstract over gender names, we define the following identifiers.

oper
  Gender : Type ;
  Gender = MorphoPor.Gender ;

  masculine : Gender ;
  masculine = Masc ;

  feminine  : Gender ;
  feminine = Fem ;

-- To abstract over number names, we define the following.

  Number : Type ;
  Number = MorphoPor.Number ;

  singular : Number ;
  singular = Sg ;

  plural   : Number ;
  plural = Pl ;

-- Prepositions used in many-argument functions are either strings
-- (including the 'accusative' empty string) or strings that
-- amalgamate with the following word (the 'genitive' "de" and the
-- 'dative' "a").

  accusative : Prep ; -- direct object
  accusative = lin Prep complAcc ;

  genitive   : Prep ; -- preposition "de" and its contractions
  genitive = lin Prep complGen ;

  dative     : Prep ; -- preposition "a" and its contractions
  dative = lin Prep complDat ;

  mkPrep = overload {
    mkPrep : Str -> Prep -- other preposition
      = \p -> lin Prep {s = p ; c = Acc ; isDir = False} ;
    mkPrep : Str -> Case -> Prep -- compound prepositions, e.g. "antes de", made as ``mkPrep "antes" genitive``
      = \p,c -> lin Prep {s = p ; c = c ; isDir = False}
  } ;

--2 Nouns

  regN : Str -> N ; --%
  regN x = lin N (mkNomReg x) ;

  femN  : N -> N ; --%
  femN n = n ** {g = feminine} ;

  mascN : N -> N ; --%
  mascN n = n ** {g = masculine} ;

  mk2N : (bastão, bastões : Str) -> Gender -> N ; --%
  mk2N x y g = lin N (mkNounIrreg x y g) ;

  -- The regular function takes the singular form and the gender, and
  -- computes the plural and the gender by a heuristic (see MorphoPor
  -- for which heuristic).
  mkN = overload {

    mkN : (luz : Str) -> N -- predictable nouns
      = regN ;

    mkN : (alemão, alemães : Str) -> N -- force noun plural, guess gender
      = \s,p -> regN s ** {s = numForms s p} ;
    -- force gender
    mkN : (mapa : Str) -> Gender -> N -- force gender, guess plural
      = \s,g -> regN s ** {g = g} ;

    mkN : (bastão,bastões : Str) -> Gender -> N -- the worst case demands two forms (singular + plural) and the gender.
      = mk2N
    } ;

--3 Compound nouns
--
-- Some nouns are ones where the first part is inflected as a noun but
-- the second part is not inflected. e.g. "número de telefone".  They
-- could be formed in syntax, but we give a shortcut here since they
-- are frequent in lexica.

  compN : N -> Str -> N ; -- compound with non-inflecting second part, e.g. "número" + "de telefone"
  compN x y = {s = \\n => x.s ! n ++ y ; g = x.g ; lock_N = <>} ;

  compNN : N -> N -> N ; -- compound with inflecting second part, e.g. "forma" + "finita"
  compNN x y = x ** {s = \\n => x.s ! n ++ y.s ! n} ;

--3 Relational nouns
--
-- Relational nouns ("filha de x") need a case and a preposition.

  mkN2 : N -> Prep -> N2 ; -- relational noun with preposition, e.g. "comida para (viagem)"
  mkN2 = \n,p -> lin N2 (n ** {c2 = p}) ;

-- The most common cases are the genitive "de" and the dative "a",
-- with the empty preposition.

  deN2 : N -> N2 ; -- relational noun with preposition "de", e.g. "filho de (fulano)"
  deN2 n = mkN2 n genitive ;

  aN2  : N -> N2 ; -- relational noun with preposition "a", e.g. "molho a francesa"
  aN2 n = mkN2 n dative ;

-- Three-place relational nouns ("a conexão de x a y") need two
-- prepositions.
  mkN3 : N -> Prep -> Prep -> N3 ; -- relational noun with two prepositions for two complements, e.g. "a relação de (fulana) com (cicrana)"
  mkN3 = \n,p,q -> lin N3 (n ** {c2 = p ; c3 = q}) ;

--3 Relational common noun phrases
--
-- In some cases, you may want to make a complex $CN$ into a
-- relational noun (e.g. "the old town hall of"). However, $N2$ and
-- $N3$ are purely lexical categories. But you can use the $AdvCN$ and
-- $PrepNP$ constructions to build phrases like this.

--
--3 Proper names and noun phrases
--
-- Proper names need a string and a gender.  The default gender is
-- feminine for names ending with "a", otherwise masculine.

  regPN : Str -> PN ; --%
  regPN x = mk2PN x g where {
    g = case last x of {
      "a" => feminine ;
      _ => masculine
      }
    } ;

  mk2PN  : Str -> Gender -> PN ; --%
  mk2PN x g = lin PN {s = x ; g = g} ;

  mkPN = overload {

    mkPN : (Anna : Str) -> PN -- regular proper noun: feminine for "-a", else masculine
      = regPN ;

    mkPN : (Pilar : Str) -> Gender -> PN -- force gender of proper noun
      = mk2PN ;
    mkPN : N -> PN -- build proper noun from noun, taking gender and singular form
      = \n -> lin PN {s = n.s ! Sg ; g = n.g} ;
    } ;

--2 Adjectives
  compADeg : A -> A ; --%
  compADeg a = a ** {
    s = table {
      Posit => a.s ! Posit ;
      _ => \\f => "mais" ++  a.s ! Posit ! f
      } ;
    } ;

  liftAdj : Adj -> A ; --%
  liftAdj adj = compADeg (lin A {s = \\_ => adj.s ; isPre = False ; copTyp = serCopula}) ;

  regA : Str -> A ; --%
  regA a = liftAdj (mkAdjReg a) ;

  mk2A : (patrão,patroa : Str) -> A ; --%
  mk2A ms fs = liftAdj (mkAdjReg2 ms fs) ;

  mk4A : (bobão,bobona,bobões,bobonas : Str) -> A ; --%
  mk4A a b c d = liftAdj (mkAdj4 a b c d) ;

  mk5A : (preto,preta,pretos,pretas,pretamente : Str) -> A ; --%
  mk5A a b c d e = liftAdj (mkAdj a b c d e) ;

  adjCopula : A -> CopulaType -> A ; --%
  adjCopula a cop = a ** {copTyp = cop} ;

  mkADeg : A -> A -> A ; --%
  mkADeg a b = a ** {
    s = table {
      Posit => a.s ! Posit ;
      _ => b.s ! Posit
        -- Compar => b.s ! Posit ;
        -- Superl => "o" ++ b.s ! Posit ;
      }
    } ;

  invarA : Str -> A ; -- invariable adjective, e.g. "simples"
  invarA a = liftAdj (mkAdj4 a a a a) ;

  mkNonInflectA : A -> Str -> A ;
  mkNonInflectA blanco hueso = blanco ** {
    s = \\x,y => blanco.s ! x ! y ++ hueso
    } ;

  mkA = overload {

    -- For regular adjectives, all forms are derived from the
    -- masculine singular. The types of adjectives that are recognized
    -- are "alto", "forte", "útil" and others. Comparison is formed by "mais".
    mkA : (bobo : Str) -> A -- predictable adjective
      = regA ;

    mkA : (espanhol,espanhola : Str) -> A -- some adjectives need the feminine form separately
      = mk2A ;

    mkA : (burrão,burrona,burrões,burronas : Str) -> A -- provide masculine and feminine singular and plural forms (very rarely does one need to specify the adverbial form)
      = mk4A ;

    mkA : (gabarolas,gabarolas,gabarolas,gabarolas,gabarolamente : Str) -> A -- one-place adjectives compared with "mais" need five forms in the worst case (masc and fem singular, masc and fem plural, adverbial).
      = mk5A ;

    --
    mkA : (bom : A) -> (melhor : A) -> A -- two separate adjectives are given: the positive ("bom"), and the comparative ("melhor"). Comparison with "mais" is the default.
      = mkADeg ;

    mkA : (blanco : A) -> (hueso : Str) -> A -- noninflecting component after the adjective
      = mkNonInflectA ;

    mkA : A -> CopulaType -> A -- force copula type, e.g. "João está doente", "João é doente". Choose among ``serCopula``, ``estarCopula``, and ``ficarCopula``
      = adjCopula ;

    } ;

-- The functions above create postfix adjectives. To switch them to
-- prefix ones (i.e. ones placed before the noun in modification, as
-- in "bom vinho"), the following function is provided.

    prefixA : A -> A ; -- adjective before noun (default after noun)
    prefixA a = a ** {isPre = True} ;

--3 Two-place adjectives
--
-- Two-place adjectives need a preposition for their second argument.

  mkA2 : A -> Prep -> A2 ; -- two-place adjectives, e.g. "casado" + "com"
  mkA2 a p = lin A2 a ** {c2 = p} ;

--2 Adverbs

-- Adverbs are not inflected. Most lexical ones have position after
-- the verb.

  mkAdv : Str -> Adv ; -- after the verb adverb
  mkAdv x = lin Adv (ss x) ;

-- Some appear next to the verb (e.g. "sempre").

  mkAdV : Str -> AdV ; -- before the verb adverb, e.g. "nunca"
  mkAdV x = lin AdV (ss x) ;

-- Adverbs modifying adjectives and sentences can also be formed.

  mkAdA : Str -> AdA ; -- adverb modifying adjectives, e.g. "muito"
  mkAdA x = lin AdA (ss x) ;

  mkAdN : Str -> AdN ; -- adverb modifying numeral, e.g. "pelo menos"
  mkAdN x = lin AdN (ss x) ;

--2 Verbs

  regV : Str -> V ; --%
  regV s = case s of {
    chamar + "-se" => reflV (regV' chamar) ;
    _ => regV' s
    } ;

  regV' : Str -> V ; --%
  regV' v =
    let
      xr = Predef.dp 2 v ; -- -ar
      z  = Predef.dp 1 (Predef.tk 2 v) ; -- i in -iar
      paradigm = case xr of {
        "ir" => case z of {
          "g" => redigir_Besch ;
          "a" => sair_Besch ;
          "u" => distribuir_Besch ;
          _ => garantir_Besch
          } ;
        "er" => case z of {
          "c" => aquecer_Besch ;
          "g" => proteger_Besch ;
          "o" => moer_Besch ;
          _ => vender_Besch
            } ;
        "ar" => case z of {
          "c" => ficar_Besch ;
          "ç" => começar_Besch ;
          "e" => recear_Besch ;
          "g" => chegar_Besch ;
          "i" => anunciar_Besch ;
          "j" => viajar_Besch ;
          "o" => perdoar_Besch ;
          "u" => suar_Besch ;
          _ => comprar_Besch
          } ;
        "or" | "ôr" => pôr_Besch ;
        _ => comprar_Besch -- hole
        }
    in lin V (verboV (paradigm v)) ;

  mkV = overload {

    -- Regular verbs are ones inflected like "cortar", "dever", or
    -- "partir".  This smart paradigm recognizes other patterns, but
    -- it is not perfect. The module $BeschPor$ gives the complete set
    -- of "Bescherelle" conjugations.

    mkV : (pagar : Str) -> V -- predictable verb, e.g., "comer", "chamar-se"
      = regV ;

    mkV : (abrir,aberto : Str) -> V -- deviant past participle, e.g. abrir - aberto
      = \v,pp -> special_ppV (regV v) pp ;

-- Most irregular verbs are found in $IrregPor$. If this is not
-- enough, the module $BeschPor$ gives all the patterns of the
-- "Bescherelle" book. To use them in the category $V$, wrap them with
-- the function

    mkV : Verbum -> V -- import verb constructed with BeschPor
      = \v -> lin V (verboV v) ;

    mkV : V -> Str -> V -- particle verb
      = \v,p -> v ** {p = p} ;

    } ;

-- To form reflexive verbs:

  reflV : V -> V ; -- force reflexive verb, e.g. ``reflV "chamar"``
  reflV v = v ** {vtyp = VRefl} ;

-- Verbs with a deviant passive participle: just give the participle
-- in masculine singular form as second argument.

  special_ppV : V -> Str -> V ; -- force past participle, e.g. abrir - aberto
  special_ppV ve pa = ve ** {
    s = table {
      VPart g n => (mkAdjReg pa).s ! (genNum2Aform g n) ;
      p => ve.s ! p
      }
    } ;


--3 Two-place verbs
--
-- Two-place verbs need a preposition, except the special case with
-- direct object.  (transitive verbs).
  dirV2 : V -> V2 ; --%
  dirV2 v = mk2V2 v accusative ;

  mk2V2  : V -> Prep -> V2 ; --%
  mk2V2 v p = lin V2 (v ** {c2 = p}) ;

  mkV2 = overload {
    mkV2 : (amar : Str) -> V2 -- predictable verb with direct object
      = \s -> dirV2 (regV s) ;
    mkV2 : V -> V2 -- verb with direct object (no preposition)
      = dirV2 ;
    mkV2 : V -> Prep -> V2 -- verb with other object
      = mk2V2
    } ;

-- You can reuse a $V2$ verb in $V$.

  v2V : V2 -> V ; -- coerce ``V2`` to ``V``
  v2V v = lin V v ;

--3 Three-place verbs
--
-- Three-place (ditransitive) verbs need two prepositions, of which
-- the first one or both can be absent.

  mkV3 = overload {
    mkV3 : (dar : Str) -> V3 -- dar (+ accusative + dative), e.g. "ele dá um cachorro a Paris"
      = \s -> dirdirV3 (regV s) ;
    mkV3 : V -> V3 -- dar (+ accusative + dative)
      = dirdirV3 ;
    mkV3 : V -> Prep -> V3 -- force one preposition, e.g. "ele vende a João um cachorro"
      = dirV3 ;
    mkV3 : V -> Prep -> Prep -> V3 -- force prepositions, e.g. "ela fala de Paris a João"
      = mmkV3
    } ;

  dirV3    : V -> Prep -> V3 ; --%
  dirV3 v p = mmkV3 v accusative p ;

  dirdirV3 : V -> V3 ; --%
  dirdirV3 v = mmkV3 v dative accusative ;

  mmkV3 : V -> Prep -> Prep -> V3 ; --%
  mmkV3 v p q = lin V3 v ** {c2 = p ; c3 = q} ;

--3 Other complement patterns
--
-- Verbs and adjectives can take complements such as sentences,
-- questions, verb phrases, and adjectives.

  -- Notice: $V0$ is just $V$.
  V0 : Type -- zero-place verbs, e.g. "chover"
    = V ;

  mkV0  : V -> V0 ; -- "chover"
  mkV0  v = lin V0 v ;

  mkVS  : V -> VS ; -- complement sentence in the indicative mood, e.g. "eu sei que meu cachorro viverá"
  mkVS  v = lin VS v ** {m = \\_ => Indic} ;

  subjVS  : V -> VS ; -- complement sentence in the subjunctive mood, e.g. "eu temo que meu cachorro morra"
  subjVS  v = lin VS v ** {m = \\_ => Conjunct} ;

  mkVV : V -> VV ; -- plain infinitive: "quero falar"
  mkVV = makeVV accusative ;

  deVV : V -> VV ; -- "terminar de falar"
  deVV = makeVV genitive ;

  aVV : V -> VV ; -- "aprender a falar"
  aVV = makeVV dative ;

  makeVV : Prep -> V -> VV ; --%
  makeVV p v = lin VV v ** {c2 = p} ;

  mkVA : V -> VA ; -- "ela se tornou direta"
  mkVA v = lin VA v ;

  mkVQ  : V -> VQ ; -- "nós nos perguntamos se você ama alguém"
  mkVQ v = lin VQ v ;

  mkV2Q : V -> Prep -> V2Q ; -- "pergunte a João se ele bebe água"
  mkV2Q v p = lin V2Q (mk2V2 v p) ;

  mmkV2 : V -> Prep -> V2 ; --%
  mmkV2 v p = lin V2 (v ** {c2 = p}) ;

  mkV2S = overload {
    mkV2S : V -> V2S -- "ele respondeu a João que ela vivia"
      = \v -> lin V2S (mmkV2 v dative ** {mn,mp = Indic}) ;
    mkV2S : V -> Prep -> V2S -- force preposition
      = \v,p -> lin V2S (mmkV2 v p ** {mn,mp = Indic}) ;
    } ;

  mkV2V = overload {
    mkV2V : V -> V2V -- verb with verb complement in the accusative and NP complement in the dative
      = \v -> lin V2V (mmkV3 v accusative dative) ;
    mkV2V : V -> Prep -> Prep -> V2V -- "ele rogou a Paris para viver"
      = \v,p,q -> lin V2V (mmkV3 v p q) ;
    } ;

  mkV2A = overload {
    mkV2A : V -> V2A -- verb with NP and AP complement (in the dative)
      = \v -> lin V2A (mmkV3 v accusative dative) ;
    mkV2A : V -> Prep -> Prep -> V2A -- ele pintou a casa de branco
      = \v,p,q -> lin V2A (mmkV3 v p q) ;
    } ;

  mkAS  : A -> AS ; --%
  mkAS  v = lin AS v ;

  mkA2S : A -> Prep -> A2S ; --%
  mkA2S v p = lin A2S (mkA2 v p) ;

  mkAV  : A -> Prep -> AV ; --%
  mkAV  v p = lin AV (v ** {c = p.p1 ; s2 = p.p2}) ;

  mkA2V : A -> Prep -> Prep -> A2V ; --%
  mkA2V v p q = lin A2V (mkA2 v p ** {s3 = q.p2 ; c3 = q.p1}) ;

-- Notice: categories $AS, A2S, AV, A2V$ are just $A$, and the second
-- argument is given as an adverb.
  AS, A2S, AV, A2V  : Type ; --%
  AS, AV : Type = A ; --%
  A2S, A2V : Type = A2 ; --%


  ---
  -- orphan definitions

-- To form a noun phrase that can also be plural,
-- you can use the worst-case function.
  makeNP : Str -> Gender -> Number -> NP ; --%
  makeNP x g n = {s = (pn2np (mk2PN x g)).s;
                  a = agrP3 g n ;
                  hasClit = False ;
                  isPol = False ;
                  isNeg = False} ** {lock_NP = <>} ;

  reflVerboV : Verbum -> V = \ve -> reflV (lin V (verboV ve)) ; --%


} ;
