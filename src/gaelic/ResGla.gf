resource ResGla = open Prelude, Predef in {

--------------------------------------------------------------------------------
-- General notes

-- ** Naming **
{-
I'm using the naming scheme for lincats and opers as explained here:
https://inariksit.github.io/gf/2018/08/28/gf-gotchas.html#my-naming-scheme-for-lincats-and-opers
-}

-- ** File structure **
-- The rest of this module is organised as follows:

      -----------------------------
      -- Grammatical categor(y|ies)

      {-
      General comments on the cat(s)

      params related to the cat(s)

      opers related to the cat(s)
      -}

--------------------------------------------------------------------------------
-- Nouns


param
  Gender = Masc | Fem ;
  CoreCase = Nom | Gen | Dat ;
  Case = CC CoreCase | Voc ;
  NPCase = NPC CoreCase | NPVoc | NPLenited ;
  Number = Sg
         | Pl
         ;
  Person = P1 | P2 | P3 ;
  Definiteness = Definite | Indefinite ; -- Some prepositions govern different case when definite vs. indefinite

oper
  npc2cc : NPCase -> CoreCase = \npc -> case npc of {
    NPC c => c ;
    _     => Nom ------ TODO check? NPVoc and NPLenited (which is not a case but this is cheaper)
  } ;
  npc2c : NPCase -> Case = \npc -> case npc of {
    NPC c => CC c ;
    NPVoc => Voc ;
    _     => CC Nom ---- this is just lenited TODO does this make any sense
  } ;

param
  NForm =
      Indef Number CoreCase
    | Def Number Case
    | Lenited -- keep this separate from case, because some prepositions' requirement of lenited form overrides the usual case requirement
    | Dual -- only after number 2, only for a handful of nouns. TODO: does it have different cases?
    ;

  {-
  * The 1st person singular, 2nd person singular and 3rd person singular masculine forms here trigger lenition (indicated with a superscript L).
  * 1st and 2nd person plurals trigger the prefixation of n- onto words beginning with vowels (nasalization), This is indicated with a superscript N. the pronunciation of the a consonant following these and the 3rd person plural is also frequently voiced or nasalized.
  * Finally the 3rd person feminine forms prefix an <h> onto words beginning with a vowel. This is indicated with H.
  -}

oper
  getNForm : DType -> NPCase -> NForm = \d,c ->
     case <d,c> of {
      <_, NPLenited>             => Lenited ; -- bit of a hack
      <DDef n Indefinite,NPVoc>  => Def n Voc ;
      <DDef n Indefinite,NPC c>  => Indef n c ;
      <DDef n Definite,npc>     => Def n (npc2c npc) ;
      <DPoss n _,NPVoc>          => Indef n Nom ; -- as per Michal on Discord https://discord.com/channels/865093807343140874/865094084683366400/1409838154550087711 . TODO: Def or Indef nom ????
      <DPoss n _,npc>            => Def n (npc2c npc) -- ????????????????
    } ;

  LinN : Type = {
    base,                -- tunnag     fuil      loch      fear    litir
    gen,                 -- tunnaige   fala      locha     fir     litreach ("de-palatalised")
    pl,                  -- tunnagan             lochan    fir     litrichean
    -- TODO: for nouns that only use suffixes, should these just show theoretical forms?
    lenited,             -- thunnag    fhuil     loch      fhear
    palatalised,         -- tunnaig    fuil      loch      fir
    lenited_palatalised  -- thunnaig   fhuil     loch      fhir
    : Str ;
    g : Gender
    } ;

  smartN = overload {
    smartN : (nom,gen,pl : Str) -> Gender -> LinN = \loch,locha,lochan,g -> {
      gen = locha ;
      pl = lochan ;
      base,
      lenited,
      palatalised,
      lenited_palatalised = loch ;
      g = g
      } ;
    smartN : (base : Str) -> Gender -> LinN = \tunnag,g -> {
      base = tunnag ;
      gen = fm (tunnaig + "e") tunnaig ;
      pl = fm (tunnag + "an") tunnaig ; -- for other allomorphs, use 4-argument paradigm
      lenited = thunnag ;
      palatalised = tunnaig ;
      lenited_palatalised = thunnaig ;
      g = g
      } where {
          tunnaig : Str = palatalise tunnag ;
          thunnag : Str = lenite tunnag ;
          thunnaig : Str = lenite tunnaig ;
          fm : Str -> Str -> Str = \fem,masc -> case g of {
            Fem => fem ; Masc => masc }
        }
  } ;

  vowel : pattern Str = #("a"|"à"|"e"|"i"|"ì"|"o"|"u") ; -- more accents?
  diphthong : pattern Str = #("ea"|"oi") ;
  lenitable : pattern Str = #("b"|"c"|"f"|"g"|"m"|"p"|"d"|"t"|"s") ;
  labial : pattern Str = #("b"|"f"|"m"|"p") ;

  palatalise : Str -> Str = \lamh -> case lamh of {
    f@? + "ea" + r      => f + "i" + r ; -- TODO is this irregular?
    boireann@(_ + (#vowel|#diphthong) + ? + _ + (#vowel|#diphthong) + ? + _)
      + a@#vowel + ch => boireann + a + "i" + ch ;
    tunn@(_ + (#vowel|#diphthong) + ? + _)
      + a@#vowel + g  => tunn + a + "i" + g ;
    l + a@#vowel + mh => l + a + "i" + mh ;
    _                 => lamh } ;

  lenite : Str -> Str = \tunnag -> case tunnag of {
    "s" + ("p"|"g"|"m"|"t") + _ => tunnag ; -- sp, sg, sm, st don't lenite
    t@#lenitable + "h" + _ => tunnag ; -- don't lenite twice
    t@#lenitable + unnag   => t + "h" + unnag ;
    _                      => tunnag } ;


  -- For inflection paradigms, see http://www.grammaticalframework.org/doc/tutorial/gf-tutorial.html#toc56
  mkNoun : (b,g,pl,l,p,lp : Str) -> Gender -> LinN = \b,gen,pl,l,p,lp,g -> {
    base = b ;                 -- tunnag     fuil      loch      fear    litir
    gen = gen ;                -- tunnaige   fala      locha     fir     litreach
    pl  = pl ;                 -- tunnagan             lochan    fir     litrichean
    lenited = l ;              -- thunnag    fhuil     loch      fhear   litir ?
    palatalised = p ;          -- tunnaig    fuil      loch      fir     litir ?
    lenited_palatalised = lp ; -- thunnaig   fhuil     loch      fhir    litir ?
    g = g ;
    } ;

  -- TODO: no idea if this is even remotely correct
  -- can always replace morphology with Katya's automated tool
  useN : LinN -> LinCN = \n -> n ** {
    s = table {
          Indef Sg Nom => n.base ;
          Indef Sg Gen => n.gen ;
          Indef Sg Dat => fm n.palatalised n.base ;
          Def Sg (CC Nom) => n.base ;
          Def Sg (CC Gen) => fm n.gen n.lenited_palatalised ;
          Def Sg (CC Dat) => fm n.palatalised n.lenited ;
          Def Sg Voc => fm n.lenited n.lenited_palatalised ;
          Indef Pl Nom => fm n.pl n.palatalised ;
          Indef Pl Gen => n.lenited ;
          Indef Pl Dat => fm n.pl n.palatalised ; -- TODO: is this correct?
          Def Pl (CC Nom) => n.pl ;
          Def Pl (CC Gen) => n.base ;
          Def Pl (CC Dat) => n.pl ;
          Def Pl Voc => glue n.lenited "a" ;
          Dual => fm n.palatalised n.base ; -- TODO: is this correct? only for 1-syllable feminine nouns?
          Lenited => n.lenited
          }
    } where {
      fm : Str -> Str -> Str = \fem,masc -> case n.g of {
          Fem => fem ;
          Masc => masc
        }
      };

  LinCN : Type = {
    s : NForm =>
        Str ;
    g : Gender ;
    -- ** postmod/premod/… : Str -- if needed? determiners can put stuff after head but it only comes at NP
  } ;

  linCN : LinCN -> Str = \cn -> cn.s ! Indef Sg Nom
                     --      ++ cn.postmod   -- If there is another field, use here
                                ;


-- some test nouns — TODO: do smart paradigms
tunnag_N : LinN = {
    base = "tunnag" ;
    gen = "tunnaige" ;
    pl = "tunnagan" ;
    lenited = "thunnag" ;
    palatalised = "tunnaig" ;
    lenited_palatalised = "thunnaig" ;

    g = Fem ;
    } ;

boireannach_N : LinN = {
    base = "boireannach" ;
    pl,gen = "boireannaich" ;
    lenited = "bhoireannach" ;
    palatalised = "boireannaich" ;
    lenited_palatalised = "bhoireannaich" ;
    g = Masc ;
    } ;

---------------------------------------------
-- Proper noun

oper
  LinPN : Type = {
    s : Str ;
    n : Number ; -- Proper nouns often have already an inherent number; you don't usually say "a Paris / many Parises"
    g : Gender ; -- inherent gender/noun class, if your language has that
  } ;

---------------------------------------------
-- Numeral

-- Used in NumeralGla
  param
    CardOrd = NCard | NOrd ;

  oper
    LinNumeral : Type = {s : CardOrd => Str ; n : Number} ;

    mkNumeral : (card, ord : Str) -> LinNumeral = \card,ord -> {
      s = table {
        NCard => card ; -- aon(a) -- TODO: allomorph of this depends on the following word?
        NOrd  => ord    -- a' chiad
        } ;
      n = Pl ; -- NB. singular for 1, 2, 20 + multiples of 20 and 100 (Lamb, p. 218)
      } ;

---------------------------------------------
-- Pronoun


oper
  LinPron : Type = {
    s : CoreCase => Str ;
    a : PronAgr ;
    poss : Str ; -- if a case is needed, it comes from the Prep! TODO verify this (do we ever need a dative for poss pron without a prep present? some preps merge, others not, but the pronoun is present in all the preps. why this way—I counted on there being fewer pronouns than prepositions.)
    empty : Str ; -- to prevent metavariables
    } ;

  -- TODO: nicer API where you can give Person, Number, Gender etc.
  -- not this weird unintuitive Agr param
  mkPron : (subj,poss : Str) -> PronAgr -> LinPron = \subj,poss,agr -> {
    s = table {
          Nom => subj ;
          _   => "gam"  -- TODO fix this
        } ;
    poss = poss ;
    a = agr ;
    empty = []
    } ;

---------------------------------------------
-- NP

{-
In the RGL, a NP may come from a common noun, proper noun or pronoun.
Pronouns are the only ones that have an inherent person (nouns are almost always 3rd person! please give me counterexamples if you can think of any.)
So we can often say that NP's lincat is the same as Prons.

NB. for later, when you want to make Pron into possessives, you may need more fields in LinPron than in LinNP.
That's why I'm copying over the definition below, instead of the neater `LinNP : Type = LinPron`.
-}


oper
  LinNP : Type = {
    art,        -- to be replaced with a combo coming from Prep, if argument of PrepNP? see Lamb p. 225
                -- TODO: is that an issue when the allomorph has been chosen by an inherent param in CN?
                -- does that param need to be kept in LinNP, and Prep need an inflection table from that param?
                -- or do we have an exhaustive list of prepositions that merge, and we can make that into a param and put on a LHS here?

    s : NPCase => Str ; -- TODO: is lenition a separate dimension from case?
    empty : Str ; -- to avoid metavariables
    a : Agr ; -- includes whether it's pron and whether it's definite. TODO: probably can make even leaner (wasn't a prio so far).
    } ;

  linNP : LinNP -> Str = \np -> np.art ! (NPC Nom) ++ np.s ! (NPC Nom) ;

  emptyNP : LinNP = {
    s,art = \\_ => [] ;
    a = NotPron (DDef Sg Indefinite) ; -- we assume pronouns are definite by default. also it just matters for PrepNP.
    empty = [] ;
  } ;

--------------------------------------------------------------------------------
-- Det, Quant, Card, Ord

param
  QuantForm = QSg Gender CoreCase | QPl CoreCase ;
  QType = QDef Definiteness | QPoss PronAgr ;
  DType = DDef Number Definiteness | DPoss Number PronAgr ;

  -- The minimum forms that preposition merges with
  PrepAgr = PrepBase | PrepDefiniteArticle Number | PrepObjectPron PronAgr | PrepPossPron PronAgr ;

oper
  agr2pagr : Agr -> PrepAgr = \a -> case a of {
    NotPron (DDef n Definite) => PrepDefiniteArticle n ;
    NotPron (DPoss n agr)     => PrepPossPron agr ;
    IsPron agr                => PrepObjectPron agr ;
    NotPron _                 => PrepBase
  } ;

  getQuantForm : Number -> Gender -> Case -> QuantForm = \n,g,c -> case <n,c> of {
    <Sg,CC c> => QSg g c ;
    <Sg,_>  => QSg g Nom ; --- ??????
    <Pl,CC c> => QPl c ;
    <Pl,_>  => QPl Nom  --- ??????
  } ;

  getArt : LinQuant -> Number -> Gender -> Case -> Str = \quant,n,g,c -> case c of {
    Voc => "" ; -- TODO: add empty field to article to not get metavariables
    _   => quant.s ! getQuantForm n g c
  } ;

  LinQuant : Type = {
    s  -- quantifier in a context, e.g. 'this (cat) (is nice)'
     : QuantForm => Str ;
    sp : Str ;  -- quantifier as standalone, e.g. 'this (is nice)'
    qt : QType ; -- Definite, Indefinite or Possessive
    } ;

  LinDet : Type = {
    s,s2 : Gender => Case => Str ;
    sp : Str ;
    dt : DType ; -- includes number
    } ;

  LinNum : Type = {
    s : Str ;
    n : Number ;
    } ;

  -- Can you reuse your mkNoun? Do nouns and quantifiers inflect the same way?
  mkQuant : Str -> QType -> LinQuant = \this,qt -> {
    s = \\_ => this ;
    sp = this ;
    qt = qt ;
    } ;

  mkDet : (seven, teen : Str) -> Definiteness -> Number -> LinDet = \aon, deug, defi, num -> {
    s = \\_,_ => aon ;
    s2 = \\_,_ => deug ;
    sp = aon ;
    dt = DDef num defi
  } ;

-- Allomorphs of the definite article

  AN, AN_L, NA, NAN : Str ;
  AN = pre {
        #vowel  => "an t-" ++ BIND ;
        #labial => "am" ;
        _       => "an" } ;

  -- N.B. lenition comes from a different param, this is just a shorthand
  AN_L = pre {
          "b"|"m"|"p"|"c"|"g" => "a'" ;
          "f"                 => "an" ;
          "sl"|"sn"|"sr"|
          "sa"|"sà"|"si"|"sì"|
          "se"|"so"|"su"      => "an t-" ++ BIND ;
          _                   => "an" } ;
  NA = pre {
        #vowel  => "na h-" ++ BIND ;
        _       => "na" } ;
  NAN = pre {
        #labial => "nam" ;
        _       => "nan" } ;

  defArt : LinQuant = {
    s = table {
          QSg Masc Nom => AN ;
          QSg Masc _   => AN_L ;
          QSg Fem Gen  => NA ;
          QSg Fem _    => AN_L ;
          QPl Gen => NAN ;
          QPl _   => NA
        } ;
    sp = "an" ; --- meaningless for DefArt
    qt = QDef Definite ;
    } ;
--------------------------------------------------------------------------------
-- Adpositions

{- The main use of Prep is in the fun

      PrepNP : Prep -> NP -> Adv

   Despite the name of the RGL category, a 'Prep' can be a preposition, postposition,
   or just an instruction to choose a particular case from the NP.
   A language may use one, two or all these strategies.

-}

-- TODO: prepositions can merge with articles
-- Lamb, page 210: obair _sa_ cheàrdaich 'working _in+the_ forge'

-- more on preps: Lamb, p.224

param
  PronAgr = Sg1 | Sg2 | Sg3 Gender | Pl1 | Pl2 | Pl3 ;
  -- PronType = Object | Possessive ;
  Agr      = NotPron DType | IsPron PronAgr ;

oper
  getDefi : Agr -> Definiteness = \a -> case a of {
    NotPron (DDef n d) => d ;
    _                  => Definite
  } ;

  LinPrep : Type = {
    s : PrepAgr => Str ; -- bare: aig 'on', inflected: agam 'on me', agad 'on you', …
    c2 : Definiteness => CoreCase ; -- most often dative
    replacesObjPron : Bool ; -- NP has to keep track of if it comes from a Pron

    -- If your language has both pre- and postpositions, you need an inherent parameter in Prep to record which one a given Prep is.
    -- position : PreOrPost ;

    -- Some cause lenition—is that separate from case?
    } ;

  PrepForms : Type = {base, sg1, sg2, sg3M, sg3F, pl1, pl2, pl3 : Str} ;

  H, N : Str ;
  H = pre {#vowel  => "h"  ++ BIND ; _ => []} ;
  N = pre {#vowel  => "n-" ++ BIND ; _ => []} ;


  invarPrepForms : Str -> PrepForms = \str ->
    {base=str ; sg1=str++"mo^L"; sg2=str++"do^L"; sg3M=str++"a^L";
     sg3F=str++"a"++H;  pl1=str++"àr"++N;  pl2=str++"ùr"++N; pl3=str++AN} ; -- AN is defined as an allomorph to def art, TODO does the possessive add t- before vowel?

  mkPrep : (replacesObjPron : Bool)
        -> (indef,defi : CoreCase)
        -> (objForms, possForms : PrepForms)
        -> LinPrep =
      \replaces,casIndef,casDef,objForms,possForms -> {
        s = table {
              PrepBase => aig ;
              PrepDefiniteArticle Sg => aig + "✨" ++ BIND ++ AN ;  -- TODO: merge with article!!!!!!
              PrepDefiniteArticle Pl => aig + "✨" ++ BIND ++ NA ;  -- TODO: merge with article!!!!!!
              PrepObjectPron Sg1 => agam ;
              PrepObjectPron Sg2 => agad ;
              PrepObjectPron (Sg3 Masc) => aige ;
              PrepObjectPron (Sg3 Fem) => aice ;
              PrepObjectPron Pl1 => againn ;
              PrepObjectPron Pl2 => agaibh ;
              PrepObjectPron Pl3 => aca ;
              PrepPossPron Sg1 => gam ;
              PrepPossPron Sg2 => gad ;
              PrepPossPron (Sg3 Masc) => ga_L ;
              PrepPossPron (Sg3 Fem) => ga_H ;
              PrepPossPron Pl1 => gar ;
              PrepPossPron Pl2 => gur ;
              PrepPossPron Pl3 => gan } ;
        c2 = table {Indefinite => casIndef ; Definite => casDef} ;
        replacesObjPron = replaces
        } where {
            aig = objForms.base ; agam = objForms.sg1 ; agad = objForms.sg2 ;
            aige = objForms.sg3M ; aice = objForms.sg3F ;
            againn = objForms.pl1 ; agaibh = objForms.pl2 ; aca = objForms.pl3 ;
            gam = possForms.sg1 ; gad = possForms.sg2 ;
            ga_L = possForms.sg3M ; ga_H = possForms.sg3F ;
            gar = possForms.pl1 ; gur = possForms.pl2 ; gan = possForms.pl3 ;
        } ;

  smartPrep : (objForms, possForms : PrepForms) -> LinPrep =
    mkPrep True Dat Dat ;

  emptyPrep : LinPrep = {
    s = \\_ => [] ;
    poss = \\_ => [] ;
    c2 = \\_ => Dat ;
    replacesObjPron = False
  } ;

  aigPrep : LinPrep =
    smartPrep
      {base="aig"; sg1="agam"; sg2="agad"; sg3M="aige"; sg3F="aice";  pl1="againn";  pl2="agaibh"; pl3="aca"}
      {base="aig"; sg1="'gam^L"; sg2="'gad^L"; sg3M="'ga^L"; sg3F="'ga"++H;  pl1="'gar"++N;  pl2="'gur"++N; pl3="'gan"} ;
  airPrep : LinPrep =
    smartPrep
      {base="air"; sg1="orm"; sg2="ort"; sg3M="air"; sg3F="oirre";  pl1="oirrn";  pl2="oirbh"; pl3="orra"}
      (invarPrepForms "air") ;

  annPrep : LinPrep =
    smartPrep
      {base="ann"; sg1="annam"; sg2="annad"; sg3M="ann"; sg3F="innte";  pl1="annainn";  pl2="annaibh"; pl3="annta"}
      {base="ann"; sg1="'nam^L"; sg2="'nad^L"; sg3M="'na^L"; sg3F="'na"++H;  pl1="'nar"++N;  pl2="'nur"++N; pl3="'nan"} ;

  àsPrep  : LinPrep =
    smartPrep
      {base="às"; sg1="asam"; sg2="asad"; sg3M="às"; sg3F="aiste";  pl1="asainn";  pl2="asaibh"; pl3="asda"}
      (invarPrepForms "às") ;

  bhoPrep : LinPrep =
    smartPrep
      {base="bho"; sg1="bhuam"; sg2="bhuat"; sg3M="bhuaithe"; sg3F="bhuaipe";  pl1="bhuainn";  pl2="buaibh"; pl3="bhuapa"}
      {base="bho"; sg1="bhom^L"; sg2="bhod^L"; sg3M="bho a^L"; sg3F="bho a"++H;  pl1="bhor"++N;  pl2="bhu"++N; pl3="bhon"} ;
{-  dePrep  : LinPrep = …-}

  doPrep  : LinPrep =
    smartPrep
      {base="do"; sg1="dhomh"; sg2="dhut"; sg3M="dha"; sg3F="dhi";  pl1="dhuinn";  pl2="dhuibh"; pl3="dhiubh"}
      {base="bho"; sg1="dom^L"; sg2="dod^L"; sg3M="dha^L"; sg3F="dha"++H;  pl1="dor"++N;  pl2="dhur"++N; pl3="don"} ;

{-  eadarPrep : LinPrep = …-}
{-  foPrep  : LinPrep = …-}
  guPrep  : LinPrep =
    smartPrep
      {base="gu"; sg1="ugam"; sg2="ugad"; sg3M="uige"; sg3F="uice";  pl1="ugainn";  pl2="ugaibh"; pl3="uca"}
      {base="gu"; sg1="gum^L"; sg2="gud^L"; sg3M="gu a^L"; sg3F="gu a"++H;  pl1="gar"++N;  pl2="gur"++N; pl3="gun"} ;

--------------------------------------------------------------------------------
-- Adjectives
-- Lamb p. 220 basic morphology, degree
-- Lamb p. 246: predicative adjectives

  LinA : Type = SS ;
  LinA2 : Type = LinA ;

  mkAdj : Str -> LinA = \str -> {s = str} ;

  AdjPhrase : Type = LinA ; -- ** {compar : Str} ;
--------------------------------------------------------------------------------
-- Verbs

param
  VAgr = VSg1 | VSg2 | VSg3 | VPl1 | VPl2 | VPl3 ;
  VForm = VInf | VPres VAgr | VPast VAgr ; -- TODO

oper
  nagr2vagr : Agr -> VAgr = \a -> case a of {
    NotPron (DDef Sg _) => VSg3 ;
    NotPron (DDef Pl _) => VPl3 ;

    -- this is the number of the possessee—number of possessor only matters for PrepNP!
    NotPron (DPoss Sg _)     => VSg3 ;
    NotPron (DPoss Pl _)     => VPl3 ;

    -- this is subject pronoun, which agrees with verb
    IsPron Sg1     => VSg1 ;
    IsPron Sg2     => VSg2 ;
    IsPron (Sg3 _) => VSg3 ;
    IsPron Pl1     => VPl1 ;
    IsPron Pl2     => VPl2 ;
    IsPron Pl3     => VPl3
  } ;

  LinV : Type = {
    s : VForm => Str
    } ;

  LinV2 : Type = LinV ** {
    c2 : LinPrep ;
    } ;

  mkVerb : Str -> LinV = \str -> {
    s = table {
      _ => str
      }
    } ;

  copula : LinV = {s = \\_ => "TODO: copula"} ; -- often useful

------------------
-- VP
-- Lamb p. 229
-- "tense, aspect, modality, voice, person and number. There are contrasts to be seen, as above, between inflected and periphrastic forms and, as a whole, periphrasis is more productive."

  LinVP : Type = {
    s : VForm => Str ;
    } ;

  LinVPSlash : Type = LinVP ** {
    c2 : LinPrep ;
    } ;

  linVP : LinVP -> Str = \vp -> vp.s ! VInf ;

--------------------------------------------------------------------------------
-- Cl, S

  -- Operations for clauses, sentences
  LinCl : Type = {
    subj : Str ;
    pred : Str ; -- TODO: depend on Temp and Pol
  } ;

  linCl : LinCl -> Str = \cl -> cl.subj ++ cl.pred ;

}
