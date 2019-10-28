incomplete concrete CatBantu of Cat = 
  CommonX - [Adv]
  ** open ResBantu, Prelude, ParamX in {

  flags optimize=all_subs ;
    coding=utf8 ;

  lincat
    S  = {s : Str} ;
    QS = {s : QForm => Str} ; 
    RS = {s : Agr => Str } ; 
    SSlash = {s : Str ; c2 : Str} ;

-- Sentence

 Cl= {s : Polarity => Tense => Anteriority => Str};
 ClSlash = { s : Polarity => Tense => Anteriority => Str};
 Imp = {s : Polarity => ImpForm => Str} ;
-- Question

   QCl = QClause ; --{s : Polarity => Tense => Anteriority => QForm => Str} ;
    IP = {s : Str ; n : Number} ;
    IComp = {s : Str} ;    
    IDet = {s :  Cgender  =>  Str ; n : Number} ;
    IQuant = {s : Number =>  Cgender => Str } ;

-- Relative
    RCl = {s : Polarity => Tense => Anteriority => Str}; 
    RP = {s :  Cgender=> Number=> Str; a : Agr} ;

-- Verb

    VP = ResBantu.VerbPhrase ;
    VPSlash = ResBantu.SlashVP ;
    Comp =  ResBantu.Comp; -- {s : Agr => Str} ; 

-- Adjective  
  AP = {s :  Cgender => Number =>  Str  };

-- Noun

    CN = CNoun; 
    NP = ResBantu.NounPhrase ;--{s : NPCase => Str ; a : Agr} ;
    Pron = {s: PronForm=>Str; a : Agr};-- Pronoun ;
    Det = {s :  Cgender  =>  Str ; n : Number ; isPre: Bool} ;
    Predet = {s :  Cgender =>Str} ;
    Ord = { s :  Cgender => Str } ;
    Num  = {s :  Cgender => Str ; n : Number } ;
    Card = {s :  Cgender => Str ; n : Number} ;
    Quant = {s : Number =>  Cgender => Str  } ;
    DAP = {s :  Cgender  =>  Str ; n : Number ; isPre: Bool} ;

-- Numeral

    Numeral = {s : CardOrd =>  Cgender => Str ; n : Number} ;
 
   Digits  = {s : CardOrd =>  Cgender => Str ; n : Number} ;

-- Structural

    Conj = {s1,s2 : Str ; n : Number} ;

    Subj = {s : Str} ;
    Prep = ResBantu.Preposition; --{s: Number =>  Cgender => Str} ;
   Adv  = {s : Agr => Str } ;
-- Open lexical classes, e.g. Lexicon
   V ,VS, VQ, VA, VV, V2S, V2Q, V2V,V2A= Verb ; 
   V2 = Verb ** {c2 : Prep} ;
   V3= Verb ** {c2, c3 : Prep} ;
   A = {s : AForm =>  Str } ; 
   A2 = {s :AForm =>  Str }** {c2 : Str} ;
   N = {s : Number => Case => Str ; g :  Cgender} ;
   N2 = {s : Number => Case => Str ; g :  Cgender} ** {c2 : Prep} ; 
   N3 = {s : Number => Case => Str ; g :  Cgender} ** {c2,c3 : Prep} ; 
   PN = {s : Case => Str ; g :  Cgender} ;
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
   -- Det =\d -> d.s!Obj G1;
    CN = \cn -> cn.s!Sg!Nom ++  cn.s2!Sg ;
   -- A = \a -> a.s ! AAdj firstGender Sg ;
    --A2 = \a -> a.s ! AAdj firstGender Sg ++ a.c2 ;

    N = \n -> n.s ! Sg ! Nom ;
    N2 = \n -> n.s ! Sg ! Nom ++ n.c2.s!Sg!firstGender ; 
    N3 = \n -> n.s ! Sg ! Nom ++ n.c2.s!Sg!firstGender  ++ n.c3.s!Sg!firstGender  ; 
}
