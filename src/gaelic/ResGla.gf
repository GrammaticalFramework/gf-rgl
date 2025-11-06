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
  Case = Nom Mutation | Dat Mutation | Gen ;
  Mutation = Lenited | NoMutation ;
  Number = Sg
         | Pl
         ;
  Person = P1 | P2 | P3 ;
  Species = Indef | Def ; -- Some prepositions govern different case when definite vs. indefinite

oper
  NOM : Case = Nom NoMutation ; -- shorthand

oper
  LinN : Type = {s: Case => Species => Number => Str; voc: Number => Str; g: Gender} ;

  mk5N nom dat gen pl pal g =
      { s = table {
              Nom _ => table {
                       Indef => table {
                                  Sg => nom ;
                                  Pl => pl
                                } ;
                       Def => table {
                                Sg => nom ;
                                Pl => fm pl pal
                              }
                     } ;
              Dat _ => table {
                       Indef => table {
                                  Sg => dat ;
                                  Pl => pl
                                } ;
                       Def => table {
                                Sg => fm pal (lenite nom) ;
                                Pl => pl
                              }
                     } ;
              Gen => table {
                       Indef => table {
                                  Sg => gen ;
                                  Pl => lenite nom
                                } ;
                       Def => table {
                                Sg => fm pal (lenite pal) ;
                                Pl => nom
                              }
                     }
            } ;
        voc = table {
                Sg => fm (lenite nom) (lenite pal) ;
                Pl => lenite nom+"a"
              } ;
        g = g
      }
  where {
    fm : Str -> Str -> Str = \fem,masc -> case g of {
           Fem => fem ;
           Masc => masc
        }
  };

  vowel : pattern Str = #("a"|"à"|"e"|"i"|"ì"|"o"|"u") ; -- more accents?
  diphthong : pattern Str = #("ea"|"oi") ;
  lenitable : pattern Str = #("b"|"c"|"f"|"g"|"m"|"p"|"d"|"t"|"s") ;
  labial : pattern Str = #("b"|"f"|"m"|"p") ;

  palatalise : Str -> Str = \lamh -> case lamh of {
    gr+"ea"+nn@("c"|"d"|"l"|"n"|"p"|"r"|"s"|"m"|"ch"|"dh"|"ll"|"mh"|"nn"|"sg") => gr+"i"+nn ;
    boireann + a@#vowel + ch@("b"|"c"|"d"|"g"|"l"|"p"|"r"|"s"|"t"|"m"|"n"|"bh"|"ch"|"cs"|"dh"|"gh"|"ll"|"lm"|"lt"|"mh"|"nc"|"ng"|"nn"|"nt"|"nnd"|"rc"|"rd"|"rm"|"rn"|"rs"|"rt"|"sg"|"th"|"ths"|"rbh") => boireann + a + "i" + ch ;
    _                 => lamh } ;

  lenite : Str -> Str = \tunnag -> case tunnag of {
    "s" + ("p"|"g"|"m"|"t") + _ => tunnag ; -- sp, sg, sm, st don't lenite
    t@#lenitable + "h" + _ => tunnag ; -- don't lenite twice
    t@#lenitable + unnag   => t + "h" + unnag ;
    _                      => tunnag } ;


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
    s : Case => Str ;
    a : PronAgr ;
    poss : Str ; -- if a case is needed, it comes from the Prep! TODO verify this (do we ever need a dative for poss pron without a prep present? some preps merge, others not, but the pronoun is present in all the preps. why this way—I counted on there being fewer pronouns than prepositions.)
    empty : Str ; -- to prevent metavariables
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

    s : Case => Str ; -- TODO: is lenition a separate dimension from case?
    empty : Str ; -- to avoid metavariables
    a : Agr ; -- includes whether it's pron and whether it's definite. TODO: probably can make even leaner (wasn't a prio so far).
    } ;

  linNP : LinNP -> Str = \np -> np.art ! NOM ++ np.s ! NOM ;

  emptyNP : LinNP = {
    s,art = \\_ => [] ;
    a = NotPron (DDef Sg Indef) ; -- we assume pronouns are definite by default. also it just matters for PrepNP.
    empty = [] ;
  } ;

--------------------------------------------------------------------------------
-- Det, Quant, Card, Ord

param
  QuantForm = QSg Gender Case | QPl Case ;
  QType = QDef Species | QPoss PronAgr ;
  DType = DDef Number Species | DPoss Number PronAgr ;

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
    <Sg,c> => QSg g c ;
    <Pl,c> => QPl c
  } ;

  getArt : LinQuant -> Number -> Gender -> Case -> Str = \quant,n,g,c ->
    quant.s ! getQuantForm n g c ;

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

  mkDet : (seven, teen : Str) -> Species -> Number -> LinDet = \aon, deug, defi, num -> {
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
          QSg Masc (Nom _) => AN ;
          QSg Masc _   => AN_L ;
          QSg Fem Gen  => NA ;
          QSg Fem _    => AN_L ;
          QPl Gen => NAN ;
          QPl _   => NA
        } ;
    sp = "an" ; --- meaningless for DefArt
    qt = QDef Def ;
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
  getDefi : Agr -> Species = \a -> case a of {
    NotPron (DDef n d) => d ;
    _                  => Def
  } ;

  LinPrep : Type = {
    s : PrepAgr => Str ; -- bare: aig 'on', inflected: agam 'on me', agad 'on you', …
    c2 : Species => Case ; -- most often dative
    replacesObjPron : Bool ; -- NP has to keep track of if it comes from a Pron

    -- If your language has both pre- and postpositions, you need an inherent parameter in Prep to record which one a given Prep is.
    -- position : PreOrPost ;

    -- Some cause lenition—is that separate from case?
    } ;

  PrepForms : Type = {base, sg1, sg2, sg3M, sg3F, pl1, pl2, pl3 : Str} ;

  h, n, LENITION_DEBUG : Str ;
  h = pre {#vowel  => "h"  ++ BIND ; _ => []} ;
  n = pre {#vowel  => "n-" ++ BIND ; _ => []} ;
  LENITION_DEBUG = "^L" ; -- Only for debugging purposes—replace with empty string for production


  invarPrepForms : Str -> PrepForms = \str ->
    {base=str ; sg1=str++"mo" + LENITION_DEBUG; sg2=str++"do" + LENITION_DEBUG; sg3M=str++"a" + LENITION_DEBUG;
     sg3F=str++"a"++h;  pl1=str++"àr"++n;  pl2=str++"ùr"++n; pl3=str++AN} ; -- AN is defined as an allomorph to def art, TODO does the possessive add t- before vowel?

  mkLinPrep : (replacesObjPron : Bool)
        -> (indef,defi : Case)
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
    mkLinPrep True (Dat Lenited) (Dat Lenited) ;

  mkPrep = overload {
    mkPrep : (objForms, possForms : PrepForms) -> LinPrep = smartPrep ;
    mkPrep : (objForms, possForms : PrepForms) -> Mutation -> LinPrep =
      \obj,poss,mutation -> mkLinPrep True (Dat mutation) (Dat mutation) obj poss ;
    mkPrep : (replacesObjPron : Bool) -> (indef,defi : Case)
          -> (objForms, possForms : PrepForms) -> LinPrep = mkLinPrep
  } ;

  emptyPrep : LinPrep = {
    s = \\_ => [] ;
    c2 = \\_ => Dat Lenited ;
    replacesObjPron = False
  } ;

  aigPrep : LinPrep =
    mkPrep
      {base="aig"; sg1="agam"; sg2="agad"; sg3M="aige"; sg3F="aice";  pl1="againn";  pl2="agaibh"; pl3="aca"}
      {base="aig"; sg1="'gam" + LENITION_DEBUG; sg2="'gad" + LENITION_DEBUG; sg3M="'ga" + LENITION_DEBUG; sg3F="'ga"++h;  pl1="'gar"++n;  pl2="'gur"++n; pl3="'gan"}
      NoMutation ;

  airPrep : LinPrep =
    mkPrep
      {base="air"; sg1="orm"; sg2="ort"; sg3M="air"; sg3F="oirre";  pl1="oirrn";  pl2="oirbh"; pl3="orra"}
      (invarPrepForms "air")
      NoMutation ;

  annPrep : LinPrep =
    mkPrep
      {base="ann"; sg1="annam"; sg2="annad"; sg3M="ann"; sg3F="innte";  pl1="annainn";  pl2="annaibh"; pl3="annta"}
      {base="ann"; sg1="'nam" + LENITION_DEBUG; sg2="'nad" + LENITION_DEBUG; sg3M="'na" + LENITION_DEBUG; sg3F="'na"++h;  pl1="'nar"++n;  pl2="'nur"++n; pl3="'nan"}
      NoMutation ;

  àsPrep  : LinPrep =
    mkPrep
      {base="às"; sg1="asam"; sg2="asad"; sg3M="às"; sg3F="aiste";  pl1="asainn";  pl2="asaibh"; pl3="asda"}
      (invarPrepForms "às")
      NoMutation ;

  bhoPrep : LinPrep =
    mkPrep
      {base="bho"; sg1="bhuam"; sg2="bhuat"; sg3M="bhuaithe"; sg3F="bhuaipe";  pl1="bhuainn";  pl2="buaibh"; pl3="bhuapa"}
      {base="bho"; sg1="bhom" + LENITION_DEBUG; sg2="bhod" + LENITION_DEBUG; sg3M="bho a" + LENITION_DEBUG; sg3F="bho a"++h;  pl1="bhor"++n;  pl2="bhu"++n; pl3="bhon"}
      Lenited ;
{-  dePrep  : LinPrep = …-}

  doPrep  : LinPrep =
    mkPrep
      {base="do"; sg1="dhomh"; sg2="dhut"; sg3M="dha"; sg3F="dhi";  pl1="dhuinn";  pl2="dhuibh"; pl3="dhiubh"}
      {base="bho"; sg1="dom" + LENITION_DEBUG; sg2="dod" + LENITION_DEBUG; sg3M="dha" + LENITION_DEBUG; sg3F="dha"++h;  pl1="dor"++n;  pl2="dhur"++n; pl3="don"}
      Lenited ;

{-  eadarPrep : LinPrep = …-}
{-  foPrep  : LinPrep = …-}
  guPrep  : LinPrep =
    mkPrep
      True             {-replaces object pronoun-}
      (Dat NoMutation) {-governs dative when indefinite, no mutation-}
      Gen              {-governs genitive when definite-}
      {base="gu"; sg1="ugam"; sg2="ugad"; sg3M="uige"; sg3F="uice";  pl1="ugainn";  pl2="ugaibh"; pl3="uca"}
      {base="gu"; sg1="gum" + LENITION_DEBUG; sg2="gud" + LENITION_DEBUG; sg3M="gu a" + LENITION_DEBUG; sg3F="gu a"++h;  pl1="gar"++n;  pl2="gur"++n; pl3="gun"}
      ;

--------------------------------------------------------------------------------
-- Adjectives
-- Lamb p. 220 basic morphology, degree
-- Lamb p. 246: predicative adjectives

param
  AForm = ASg Case Gender | APl ;

oper
  LinA : Type = {s: AForm => Str; voc: Gender => Str; compar: Str} ; -- 686
  oper mkAdj : (_,_,_,_,_,_,_,_,_,_ : Str) -> LinA =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10 ->
          { s = table {
                  ASg (Nom _) Masc => f1 ;
                  ASg (Nom _) Fem => f2 ;
                  ASg (Dat _) Masc => f3 ;
                  ASg (Dat _) Fem => f4 ;
                  ASg Gen Masc => f5 ;
                  ASg Gen Fem => f6 ;
                  APl => f7
                } ;
            voc = table {
                    Masc => f8 ;
                    Fem => f9
                  } ;
            compar = f10 ;
          } ;

  LinA2 : Type = LinA ;

  LinAP : Type = LinA ; -- ** {compar : Str} ;
--------------------------------------------------------------------------------
-- Verbs

param
  VForm = Indep | Dep ;
  
oper
  LinV = {s: Str; conditional: Number => Str; imperative: Person => Number => Str; future, past : VForm => Str; noun, participle: Str} ;

  LinV2 : Type = LinV ** {
    c2 : LinPrep ;
    } ;

  mkVerb : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> LinV =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15 ->
          { s = f1 ;
            conditional = table {
                            Sg => f2 ;
                            Pl => f3
                          } ;
            imperative = table {
                           P1 => table {
                                   Sg => f4 ;
                                   Pl => f5
                                 } ;
                           P2 => table {
                                   Sg => f6 ;
                                   Pl => f7
                                 } ;
                           P3 => table {
                                   Sg => f8 ;
                                   Pl => f9
                                 }
                         } ;
            future = table {
                       Indep => f10 ;
                       Dep   => f11
                     } ;
            past   = table {
                       Indep => f12 ;
                       Dep   => f13
                     } ;
            noun = f14 ;
            participle = f15
          } ;

------------------
-- VP
-- Lamb p. 229
-- "tense, aspect, modality, voice, person and number. There are contrasts to be seen, as above, between inflected and periphrastic forms and, as a whole, periphrasis is more productive."

  LinVP : Type = LinV ;

  LinVPSlash : Type = LinVP ** {
    c2 : LinPrep ;
    } ;

--------------------------------------------------------------------------------
-- Cl, S

  -- Operations for clauses, sentences
  LinCl : Type = {
    subj : Str ;
    pred : Str ; -- TODO: depend on Temp and Pol
  } ;

  linCl : LinCl -> Str = \cl -> cl.subj ++ cl.pred ;


oper mkNoun : (_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Gender -> LinN =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,g ->
          { s = table {
                  Nom _ => table {
                           Indef => table {
                                      Sg => f1 ;
                                      Pl => f2
                                    } ;
                           Def => table {
                                    Sg => f3 ;
                                    Pl => f4
                                  }
                         } ;
                  Dat _ => table {
                           Indef => table {
                                      Sg => f5 ;
                                      Pl => f6
                                    } ;
                           Def => table {
                                    Sg => f7 ;
                                    Pl => f8
                                  }
                         } ;
                  Gen => table {
                           Indef => table {
                                      Sg => f9 ;
                                      Pl => f10
                                    } ;
                           Def => table {
                                    Sg => f11 ;
                                    Pl => f12
                                  }
                         }
                } ;
            voc = table {
                    Sg => f13 ;
                    Pl => f14
                  } ;
            g = g
          } ;

}
