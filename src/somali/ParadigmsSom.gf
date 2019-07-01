resource ParadigmsSom = open CatSom, ResSom, ParamSom, Prelude in {

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
  noPrep : Preposition ;


--2 Nouns

  mkN : overload {
    mkN : (bisad : Str) -> N ; -- Predictable nouns
    mkN : (shimbir : Str) -> (fem : Gender) -> N ; -- Unpredictable gender
    mkN : (maalin,maalmo : Str) -> Gender -> N ; -- Consonant cluster in stem
    --mkN : N -> Gender -> N ; -- Otherwise predictable but not gender (TODO does this even happen?)
  } ;

  mkN2 : overload {
    mkN2 : Str -> N2 ; -- Predictable N2
    mkN2 : N -> N2 -- N2 out of noun
   } ;

  mkPN : overload {
    mkPN : Str -> PN ; -- Proper noun, default agr. P3 Sg Masc.
    mkPN : Str -> Agr -> PN -- Proper noun, another agr.
    } ;

--2 Adjectives

  mkA : overload {
    mkA : (yar : Str) -> A ;
    mkA : (sg,pl : Str) -> A
  } ;

  -- mkA2 : Str -> Prep -> A2 ;

--2 Verbs

  -- Verbs
  mkV : overload {
    mkV : (imp : Str) -> V ;    -- Predictable verb: imperative form as argument
    mkV : (imp,sg1,pl2 : Str) -> V ; -- Less predictable verb: imperative, 1st person singular and 2nd person plural
    mkV : Str -> V -> V  -- Add a prefix to an existing verb, e.g. u baahan+ahay
  } ;

  copula : V ; -- The copula verb 'ahay'

  mkV2 : overload {
    mkV2 : (sug : Str) -> V2 ; -- Predictable verb: imperative form, no preposition
    mkV2 : (sug : Str) -> (_ku : Preposition) -> V2 ; -- Regular verb, imperative and preposition
    mkV2 : V -> Preposition -> V2 ; -- Already constructed verb with preposition
    } ;

  mkV3 : overload {
    mkV3 : (sug : Str) -> V3 ; -- Predictable verb: imperative form, no preposition
    mkV3 : (sug : Str) -> (_,_ : Preposition) -> V2 ; -- Regular verb, imperative and preposition
    mkV3 : V -> (_,_ : Preposition) -> V2 ; -- Already constructed verb with preposition
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
    mkPrep : Str -> CatSom.Prep = \s ->
      lin Prep ((ResSom.mkPrep s s s s s s) ** {
        c2=noPrep ; sii,dhex,berri=[]}) ;
    mkPrep : (x1,_,_,_,_,x6 : Str) -> CatSom.Prep = \a,b,c,d,e,f ->
      lin Prep ((ResSom.mkPrep a b c d e f) ** {
        c2=noPrep ; sii,dhex,berri=[]}) ;
    mkPrep : Preposition -> CatSom.Prep = \p ->
      lin Prep ((prep p) ** {sii,dhex,berri=[]}) ;
    mkPrep : CatSom.Prep -> (x1,x2,x3 : Str) -> CatSom.Prep = \p,s,t,u ->
      p ** {berri = s ; sii = t ; dhex = u} ;
    } ;

  -- mkConj : (_,_ : Str) -> Number -> Conj = \s1,s2,num ->
  --   lin Conj { s = s1 ; s2 = s2 } ;

  -- mkSubj : Str -> Bool -> Subj = \s,b ->
  --   lin Subj { } ;

  mkAdv : Str -> Adv = \s -> lin Adv {
    berri = s ;
    c2 = noPrep ;
    np = emptyNP ;
    sii,dhex = []
    } ;

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
  ka = ResSom.Ka ;
  ku = ResSom.Ku ;
  la = ResSom.La ;
  u  = ResSom.U ;
  noPrep = ResSom.NoPrep ;
  ------------------------

  mkN = overload {
    mkN : Str -> N                   = \s   -> lin N (mkN1 s) ;
    mkN : Str -> Gender -> N         = \s,g -> lin N (mkNg s g) ;
    mkN : (_,_ : Str) -> Gender -> N = \s,t,g -> lin N (nMaalin s t g) ;
    --mkN : N -> Gender -> N           = \n,g -> n ** {g = g }
    } ;

  shortPossN : N -> N -- force N to take short form of possessive suffix, e.g. family members
   = \n -> n ** {shortPoss = True} ;

  mkN2 = overload {
    mkN2 : Str -> N2 = \s -> lin N2 (mkN1 s) ;
    mkN2 : N   -> N2 = \n -> lin N2 n ;
   } ;

  mkPN = overload {
    mkPN : Str -> PN        = \s -> lin PN (mkPNoun s sgMasc) ;
    mkPN : Str -> Agr -> PN = \s,a -> lin PN (mkPNoun s a)
    } ;

  mkA = overload {
    mkA : (yar : Str)   -> A = \s -> lin A (duplA s) ;
    mkA : (sg,pl : Str) -> A = \s,p -> lin A (mkAdj s p)
    } ;

  mkV = overload {
    mkV : (imp : Str) -> V = \v -> lin V (regV v) ;
    mkV : (imp,pl2,sg1 : Str) -> V = \i,p,s -> lin V (mkVerb i p s) ;
    mkV : Str -> V -> V = \s,v -> lin V (prefixV s v)
  } ;

  copula = ResSom.copula ;

  regV : Str -> Verb = \s -> case s of {
    _ + #c + #c + "o" => cJoogso s ;
    _           + "o" => cQaado s ; ----
    _           + "i" => cKari s ;
    _          + "ee" => cYaree s ;
    _                 => cSug s
    } ;

  mkV2 = overload {
    mkV2 : Str -> V2 = \s -> lin V2 (regV s ** {c2 = noPrep}) ;
    mkV2 : Str -> Preposition -> V2 = \s,p -> lin V2 (regV s ** {c2 = p}) ;
    mkV2 : V -> Preposition -> V2 = \v,p -> lin V2 (v ** {c2 = p}) ;
    } ;

  mkV3 = overload {
    mkV3 : (sug : Str) -> V3 = \s -> lin V3 (regV s ** {c2,c3 = noPrep}) ;
    mkV3 : (sug : Str) -> (_,_ : Preposition) -> V3 = \s,p,q -> lin V3 (regV s ** {c2 = p ; c3 = q}) ;
    mkV3 : V -> (_,_ : Preposition) -> V2 = \v,p,q -> lin V3 (v ** {c2 = p ; c3 = q}) ;
    } ;
--------------------------------------------------------------------------------

}
