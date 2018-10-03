incomplete concrete CatBantu of Cat = 
  CommonX - [Pol]
  ** open ResBantu, Prelude, ParamX in {

  flags optimize=all_subs ;
    coding=utf8 ;

  lincat

 Pol = {s : Str ; b : Bool} ;
  
    S  = {s : Str} ;
    QS = {s : QForm => Str} ;
    RS = {s : Agr => Str ; c : NPCase} ; -- c for it clefts
    SSlash = {s : Str ; c2 : Str} ;

-- Sentence

  --  Cl = {s : Tense => Anteriority => Bool =>  Str} ;
 --   ClSlash = {
 --     s : ResKam.Tense => Anteriority => Bool => Str ;
--      } ;
--
-- Question

   -- QCl = {s : ResKam.Tense => Anteriority => Polarity => QForm => Str} ;
    IP = {s : NPCase => Str ; n : Number} ;
    IComp = {s : Str} ;    
    IDet = {s : Str ; n : Number} ;
    IQuant = {s : Number => Str} ;

-- Relative
{-}
    RCl = {
      s : ResKam.Tense => Anteriority => Polarity => Agr => Str ; 
      c : NPCase
      } ;
    RP = {s : RCase => Str ; a : RAgr} ;
-}
-- Verb

    VP = ResBantu.VP ;
    VPSlash = ResBantu.SlashVP ;
    Comp = {s : Agr => Str} ; 

-- Adjective  
  AP = {s : Gender => Number =>  Str  };

-- Noun

    CN = CNoun; 
    NP = {s : NPCase => Str ; a : Agr} ;
    Pron = {s: PronForm=>Str; a : Agr};-- Pronoun ;
    Det = {s : DetForm =>  Str ; n : Number } ;
    Predet = {s : Gender =>Str} ;
    Ord = { s : Gender => Str } ;
    Num  = {s : Gender => Str ; n : Number } ;
    Card = {s : Gender => Str ; n : Number} ;
    Quant = {s : Number => Gender => Str  } ;
    DAP = {s : DetForm =>  Str ; n : Number} ;

-- Numeral

    Numeral = {s : CardOrd => Gender => Str ; n : Number} ;
 
   Digits  = {s : CardOrd => Gender => Str ; n : Number} ;

-- Structural

    Conj = {s1,s2 : Str ; n : Number} ;

    Subj = {s : Str} ;
    Prep = {s: Number => Gender => Str} ;

-- Open lexical classes, e.g. Lexicon
  V, VS, VQ, VA, V3,V2,VV, V2S, V2Q, V2V,V2A= Verb ; 
   -- V2, VV, V2S, V2Q = Verb ** {c2 : Str} ;
   -- V3, V2V,V2A = Verb ** {c2, c3 : Prep} ;
    A = {s : AForm =>  Str } ; 
    A2 = {s :AForm =>  Str }** {c2 : Str} ;

    N = {s : Number => Case => Str ; g : Gender} ;
--    N2 = {s : Number => Case => Str ; g : Gender} ** {c2 : Str} ;
    N2 = {s : Number => Case => Str ; g : Gender} ** {c2 : Prep} ; 
    N3 = {s : Number => Case => Str ; g : Gender} ** {c2,c3 : Prep} ; 
    PN = {s : Case => Str ; g : Gender} ;
   --Tense = {s : Str ; t : ResKam.Tense} ;

  linref
    SSlash = \ss -> ss.s ++ ss.c2 ;
  --  ClSlash = \cls -> cls.s ! Pres ! Simul ! CPos ! oDir ++ cls.c2 ;

   -- VP = \vp -> infVP VVAux vp Simul CPos (agrP3 Sg) ;
  --  VPSlash = \vps -> infVP VVAux vps Simul CPos (agrP3 Sg) ++ vps.c2;
--
    Conj = \conj -> conj.s1 ++ conj.s2 ;

 --   V, VS, VQ, VA = \v -> infVP VVAux (predV v) Simul CPos (agrP3 Sg);
  --  V2, V2A, V2Q, V2S = \v -> infVP VVAux (predV v) Simul CPos (agrP3 Sg) ++ v.c2;
   -- V3 = \v -> infVP VVAux (predV v) Simul CPos (agrP3 Sg) ++ v.c2 ++ v.c3;
  --  VV = \v -> infVP VVAux (predVV v) Simul CPos (agrP3 Sg) ;
   -- V2V = \v -> infVP VVAux (predVc v) Simul CPos (agrP3 Sg) ;
    Det =\d -> d.s!Obj G1;
    CN = \cn -> cn.s!Sg!Nom ++  cn.s2!Sg ;
   -- A = \a -> a.s ! AAdj G1 Sg ;
   -- A2 = \a -> a.s ! AAdj G1 Sg ++ a.c2 ;

    N = \n -> n.s ! Sg ! Nom ;
    N2 = \n -> n.s ! Sg ! Nom ++ n.c2.s!Sg!G1 ; 
        N3 = \n -> n.s ! Sg ! Nom ++ n.c2.s!Sg!G1  ++ n.c3.s!Sg!G1  ; 
}
