resource ParadigmsSom = open CatSom, ResSom, Prelude in {

oper

--2 Parameters
--
-- To abstract over number, valency and (some) case names,
-- we define the following identifiers. The application programmer
-- should always use these constants instead of the constructors
-- defined in $ResSom$.
  Number : Type ;
  sg : Number ;
  pl : Number ;

  Case : Type ;
  absolutive : Case ;
  nominative : Case ;

  Agr : Type ;
  sgFem : Agr ;
  sgMasc : Agr ;
  plAgr : Agr ;

  Gender : Type ;
  masc : Gender ;
  fem : Gender ;

  Preposition : Type ;
  ka : Preposition ;
  ku : Preposition ;
  la : Preposition ;
  u  : Preposition ;


--2 Nouns

  mkN : overload {
    mkN : (bisad : Str) -> N ; -- Predictable nouns
    mkN : (shimbir : Str) -> (fem : Gender) -> N ; -- Unpredictable gender
    mkN : (maalin,maalmo : Str) -> Gender -> N ; -- Consonant cluster in stem
    --mkN : N -> Gender -> N ; -- Otherwise predictable but not gender (TODO does this even happen?)
  } ;

  mkPN : overload {
    mkPN : Str -> PN ; -- Proper noun, default agr. P3 Sg Masc.
    mkPN : Str -> Agr -> PN -- Proper noun, another agr.
    } ;

--2 Adjectives

  mkA : (yar : Str) -> CatSom.A ;

  -- mkA2 : Str -> Prep -> A2 ;

--2 Verbs

-- Smart paradigms
  mkV : Str -> V ;

  mkV2 : overload {
    mkV2 : (akhri : Str) -> V2 ; -- Regular verbs, no preposition
    mkV2 : (_ : Str) -> (_ku : Preposition) -> V2 ; -- Regular verb, prep.
    mkV2 : V -> Preposition -> V2 ; -- Already constructed verb with preposition
    } ;

  -- TODO: actual constructors
  -- mkVA : Str -> VA = \s -> lin VA (mkVerb  s) ;
  --
  -- mkV2A : Str -> V2A = \s -> lin V2A (mkVerb s) ;
  -- mkVQ : Str -> VQ = \s -> lin VQ (mkVerb s) ;
  -- mkVS : Str -> VS = \s -> lin VS (mkVerb s) ;
  --
  -- mkV2V : Str -> V2V = \s -> lin V2V (mkVerb s) ;
  -- mkV2S : Str -> V2S = \s -> lin V2S (mkVerb s) ;
  -- mkV2Q : Str -> V2Q = \s -> lin V2Q (mkVerb s) ;
  -- mkV3 : Str -> V3 = \s -> lin V3 (mkVerb s) ;


  -----

--2 Structural categories

  mkPrep = overload {
    mkPrep : Str -> Prep = \s ->
      lin Prep (ResSom.mkPrep s s s s s s) ;
    mkPrep : (x1,_,_,_,_,x6 : Str) -> Prep = \a,b,c,d,e,f ->
      lin Prep (ResSom.mkPrep a b c d e f) ;
    mkPrep : Preposition -> Prep = \p ->
      lin Prep (prepTable ! p) ;
  } ;

  -- mkConj : (_,_ : Str) -> Number -> Conj = \s1,s2,num ->
  --   lin Conj { s = s1 ; s2 = s2 } ;

  -- mkSubj : Str -> Bool -> Subj = \s,b ->
  --   lin Subj { } ;

  mkAdv : Str -> Adv = \s -> lin Adv {s = s ; s2 = []} ;

  mkAdV : Str -> AdV = \s -> lin AdV {s = s} ;

  mkAdA : Str -> AdA = \s -> lin AdA {s = s} ;


--.
-------------------------------------------------------------------------------
-- The definitions should not bother the user of the API. So they are
-- hidden from the document.

  Number = ResSom.Number ;
  sg = Sg ;
  pl = Pl ;

  Case = ResSom.Case ;
  absolutive = Abs ;
  nominative = Nom ;

  Agr = ResSom.Agreement ;
  sgFem = Sg3 Fem ;
  sgMasc = Sg3 Masc ;
  plAgr = Pl3 ;

  Gender = ResSom.Gender ;
  masc = Masc ;
  fem = Fem ;

  Preposition = ResSom.Preposition ;
  ka = ResSom.ka ;
  ku = ResSom.ku ;
  la = ResSom.la ;
  u  = ResSom.u ;
  ------------------------

  mkN = overload {
    mkN : Str -> N                   = \s   -> lin N (mkN1 s) ;
    mkN : Str -> Gender -> N         = \s,g -> lin N (mkNg s g) ;
    mkN : (_,_ : Str) -> Gender -> N = \s,t,g -> lin N (nMaalin s t g) ;
    --mkN : N -> Gender -> N           = \n,g -> n ** {g = g }
  } ;

  mkPN = overload {
    mkPN : Str -> PN = \s -> lin PN (mkPNoun s sgMasc) ;
    mkPN : Str -> Agr -> PN = \s,a -> lin PN (mkPNoun s a)
    } ;

  mkA : (yar : Str) -> CatSom.A = \s -> lin A (mkAdj s) ;

  mkV : Str -> V = \s -> lin V (regV s) ;

  regV : Str -> Verb = \s -> cSug s ; --case s of {
    -- _ + #c + #c + "o" => cJoogso s ;
    -- _           + "o" => cQaado s ; ----
    -- _           + "i" => cKari s ;
    -- _          + "ee" => cYaree s ;
    -- _                 => cSug s
    -- } ;

  mkV2 = overload {
    mkV2 : Str -> V2 = \s -> lin V2 (regV s ** {c2 = noPrep}) ;
    mkV2 : Str -> Preposition -> V2 = \s,p -> lin V2 (regV s ** {c2 = p}) ;
    mkV2 : V -> Preposition -> V2 = \v,p -> lin V2 (v ** {c2 = p}) ;
    } ;
--------------------------------------------------------------------------------

}
