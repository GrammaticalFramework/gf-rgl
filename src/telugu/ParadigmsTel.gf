--# -path=.:../abstract:../../prelude:../common

--1 Telugu Lexical Paradigms

resource ParadigmsTel = open
  (Predef=Predef),
  Prelude,
  ResTel,
  CatTel
  in {

--2 Parameters

oper
  masculine : Gender = Masc ;
  feminine : Gender = Fem ;


--2 Nouns

  mkN = overload {
    mkN : Str -> N
      = \s -> wallNoun s ** {lock_N = <>} ;
    mkN : Str -> Gender -> N
      = \s,g -> reggNoun s g ** {lock_N = <>} ;
    mkN : Str -> Str -> N
      = \s,p -> mkNoun s s p p Neutr ** {lock_N = <>} ;
    mkN : Gender -> N -> N
      = \g,n -> n ** {g = g} ;
    mkN : (x1,_,_,x4 : Str) -> Gender -> N
      = \sd,so,pd,po,g -> mkNoun sd so pd po g ** {lock_N = <>} ;
    } ;


----3 Proper names and noun phrases
----
---- Proper names, with a regular genitive, are formed from strings.
--

  mkPN = overload {
    mkPN : Str -> PN = \s -> let n = regNoun s in {s = n.s ! Sg ; g = n.g ; lock_PN = <>} ;
    mkPN : N -> PN = \n -> {s = n.s ! Sg ; g = n.g ; lock_PN = <>} ;
    mkPN : N -> Gender -> PN = \n,g -> {s = n.s ! Sg ; g = g ; lock_PN = <>} ;
    } ;

  mkLN : Str -> LN = \s -> lin LN {s = s} ;
  mkGN : Str -> GN = \s -> lin GN {s = s} ;
  mkSN : Str -> SN = \s -> lin SN {s = s} ;

----2 Adjectives
--
  mkA = overload {
    ---- For regular adjectives
    mkA : Str -> A
      = \s -> regAdjective s ** {lock_A = <>} ;
    mkA : (x1,_,x3 : Str) -> A
      = \msd,m,f -> mkAdjective msd m f ** {lock_A = <>} ;
    } ;

----2 Verbs
----
--
---- Verbs are constructed by the function $mkV$, which takes a varying
---- number of arguments.
--

  mkV = overload {
    mkV : Str -> V
      = \s -> regVerb s ** {lock_V = <>} ;
    mkV : (x1,_,_,_,_,_,_,_,_,_,_,_,_,_,x15 : Str) -> V
      = \inf,stem,ims,imp,ifs,ifp,pms,pmp,pfs,pfp,ss1,ss2,sp2,sp3,r ->
           mkVerb inf stem ims imp ifs ifp pms pmp pfs pfp ss1 ss2 sp2 sp3 r **
             {lock_V = <>} ;
    } ;

----3 Two-place verbs
----
---- Two-place verbs need a preposition, except the special case with direct object.
---- (transitive verbs). Notice that a particle comes from the $V$.
--

  mkV2 = overload {
    mkV2 : Str -> V2
      = \s -> regVerb s ** {c2 = {s = [] ; c = VTrans} ; lock_V2 = <>} ;
    mkV2 : V -> V2
      = \v -> v ** {c2 = {s = [] ; c = VTrans} ; lock_V2 = <>} ;
    mkV2 : V -> Str -> V2
      = \v,p -> v ** {c2 = {s = p ; c = VTransPost} ; lock_V2 = <>} ;
    } ;

----3 Three-place verbs
----
---- Three-place (ditransitive) verbs need two prepositions, of which
---- the first one or both can be absent.
--

  mkV3 : V -> Prep -> Prep -> V3 = \v,p,q -> v ** {
    c2 = {s = p.s ; c = VTransPost} ;
    c3 = {s = q.s ; c = VTransPost} ;
    lock_V3 = <>
    } ;

----3 Other complement patterns
----
---- Verbs and adjectives can take complements such as sentences,
---- questions, verb phrases, and adjectives.
--
  mkVS : V -> VS = \v -> v ** {lock_VS = <>} ;
  mkVV : V -> VV = \v -> lin VV (v ** {isAux = False}) ;
  mkVQ : V -> VQ = \v -> v ** {lock_VQ = <>} ;
  mkVA : V -> VA = \v -> v ** {lock_VA = <>} ;
  mkV0 : V -> V = \v -> v ;
  mkV2S : V -> Prep -> V2S = \v,p -> v ** {
    c2 = {s = p.s ; c = VTransPost} ; lock_V2S = <>
    } ;
  mkV2Q : V -> Prep -> V2Q = \v,p -> v ** {
    c2 = {s = p.s ; c = VTransPost} ; lock_V2Q = <>
    } ;
  mkV2A : V -> Prep -> V2A = \v,p -> v ** {
    c2 = {s = p.s ; c = VTransPost} ; lock_V2A = <>
    } ;
  mkV2V : V -> Prep -> Prep -> V2V = \v,p,_ -> v ** {
    c2 = {s = p.s ; c = VTransPost} ; lock_V2V = <>
    } ;

----2 Prepositions
----
---- A preposition as used for rection in the lexicon, as well as to
---- build $PP$s in the resource API, just requires a string.
--

  noPrep : Prep = {s = [] ; lock_Prep = <>} ;
  mkPrep : Str -> Prep = \s -> {s = s ; lock_Prep = <>} ;

  dirV2 : V -> V2 = mkV2 ;
  prepV2 : V -> Prep -> V2 = \v,p -> mkV2 v p.s ;
  partV : V -> Str -> V = \v,_ -> v ;

----3 Relational nouns
----
---- Relational nouns ("daughter of x") need a preposition.
--

  mkN2 : N -> Prep -> N2 = \n,p -> n ** {c2 = p.s ; lock_N2 = <>} ;

---- Three-place relational nouns ("the connection from x to y") need two prepositions.
--

  mkN3 : N -> Prep -> Prep -> N3 = \n,p,q -> n ** {c2 = p.s ; c3 = q.s ; lock_N3 = <>} ;

----3 Two-place adjectives
----
---- Two-place adjectives need a preposition for their second argument.
--

  mkA2 : A -> Prep -> A2 = \a,p -> a ** {c2 = p.s ; lock_A2 = <>} ;
  mkA2V : A -> Prep -> A2 = mkA2 ;
  mkAS : A -> A = \a -> a ;
  mkAV : A -> A = \a -> a ;

----2 Adverbs
--
---- Adverbs are not inflected. Most lexical ones have position
---- after the verb. Some can be preverbal (e.g. "always").
--

  mkAdv : Str -> Adv = \s -> {s = s ; lock_Adv = <>} ;
  mkAdV : Str -> AdV = \s -> lin AdV {s = s} ;

---- Adverbs modifying adjectives and sentences can also be formed.
--
--  mkAdA : Str -> AdA ;
--

  mkAdA : Str -> AdA = \s -> lin AdA {s = s} ;
  mkAdN : Str -> AdN = \s -> lin AdN {s = s} ;

  mkInterj : Str -> Interj = \s -> lin Interj {s = s} ;
  mkVoc : Str -> Voc = \s -> lin Voc {s = s} ;

}
