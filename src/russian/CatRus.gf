concrete CatRus of Cat = CommonX - [AdV, mkAdV] ** open ResRus, Prelude in {
flags coding=utf8 ; optimize=all ;
lincat
  N = ResRus.NounForms ;
  PN = {
    s : Case => Str ;
    g : Gender ;
    anim : Animacy ;
    n : Number ;
  } ;
  GN = {
    s : Case => Str ;
    g : Sex ;
  } ;
  SN = {
    s : Sex => Case => Str ;
    p : Case => Str ;
  } ;
  LN = {
    s : Case => Str ;
    c : ResRus.ComplementCase ;
    g : Gender ;
    n : Number ;
    anim : Animacy
  } ;
  N2 = ResRus.Noun2Forms ;
  N3 = ResRus.Noun3Forms ;

  A, Ord = ResRus.AdjForms ;
  A2 = ResRus.AdjForms ** {c : ComplementCase} ;

  AdV = {
     s : Str ;
     p : Polarity
  } ;

  V, VS, VQ, VA = ResRus.VerbForms ;
  V2, V2S, V2Q, V2A, V2V = ResRus.VerbForms2 ;
  V3 = ResRus.VerbForms3 ;
  VV = {v : ResRus.VerbForms ; modal : AgrTable} ;

  CN = ResRus.Noun ;

  NP = ResRus.NounPhrase ;
  VP = ResRus.VP ;

  VPSlash = ResRus.VPSlash ;

  AP = ResRus.Adjective ** {isPost : Bool} ;

  S = {s : Mood => Str} ;
  SSlash = {s : Mood => Str; c: ComplementCase} ;
  Cl = {
    subj : Str ;
    adv : Str ;
    verb : VerbForms ;
    dep : Str ;  -- dependent infinitives and such
    compl : PolarityTable ;
    a : Agr
    } ;
  ClSlash = {
    subj : Str ;
    adv : Str ;
    verb : VerbForms ;
    dep : Str ;  -- dependent infinitives and such
    compl : PolarityTable ;
    a : Agr ;
    c : ComplementCase
    } ;
  Imp = {s: Polarity => GenNum => Str} ;
  Comp = {s : AgrTable ; adv : Str ; cop : CopulaType } ;

  Det, DAP = {
    s : DetTable ;
    type : DetType ; -- main purpose is to avoid emptiness of articles, but can be reused later for something else
    c : Case ;
    size : NumSize
    } ;
  Predet = ResRus.Adjective ** {size : NumSize} ;
  IQuant = ResRus.Adjective ** {g: Gender; c: Case} ;
  Quant = ResRus.Adjective ** {c: Case; type: DetType} ;
  Numeral = NumeralForms ;
  Num = NumDet ;
  Card = NumDet ;
  Digits = {s : Str ; size: NumSize; tail: DTail} ;
  Decimal = {s : Str ; size: NumSize; hasDot : Bool} ;

  QS  = {s : QForm => Str} ;
  QCl = {
    subj : Str ;
    adv : Str ;
    verb : VerbForms ;
    dep : Str ;  -- dependent infinitives and such
    compl : PolarityTable ;
    a : Agr
    } ;

  Pron = ResRus.PronounForms ;
  IP = ResRus.IPronounForms ;
  RP = ResRus.RPronounForms ;
  IComp = {s : AgrTable ; adv : Str ; cop : CopulaType } ;
  IDet = {
    s : DetTable ;
    g : Gender ;
    size : NumSize ;
    c : Case
  } ;
  RS  = {s : AdjTable} ;
  RCl = {
    subj : AdjTable ;
    adv : AgrTable ;
    verb : VerbForms ;
    dep : Str ;  -- dependent infinitives and such
    compl : ComplTable ;
    a : MaybeAgr
    } ;

  Prep = ResRus.ComplementCase ;
  Conj = {s1,s2 : Str ; n : Number} ;

linref
  N = \s -> s.snom ;
  PN,LN = \s -> s.s ! Nom ;
  Pron = \s -> s.nom ;
  N2 = \s -> s.snom ++ s.c2.s ;
  N3 = \s -> s.snom ++ s.c2.s ++ s.c3.s ;
  A = \s -> case s.preferShort of {PrefShort => s.sm ; _ => s.msnom} ;
  A2 = \s -> case s.preferShort of {PrefShort => s.sm ; _ => s.msnom} ++ s.c.s ;  -- ?
  V = \s -> verbInf s ;
  V2 = \s -> (verbInf s) ++ s.c.s ;
  V2V = \s -> (verbInf s) ++ s.c.s ;
  V2A = \s -> (verbInf s) ++ s.c.s ;
  V3 = \s -> (verbInf s) ++ s.c.s ++ s.c2.s ;
  Ord = \s -> s.nsnom ;
  S = \s -> s.s ! Ind ;
  SSlash = \s -> s.s ! Ind ++ s.c.s ;  --?
  VP = \s -> s.adv ! Ag (GSg Neut) P3 ++ (verbInf s.verb) ++ s.dep ++ s.compl ! Pos ! Ag (GSg Neut) P3 ;
  Comp = \s -> copula.inf ++ s.s ! Ag (GSg Neut) P3 ++ s.adv ;
  IComp = \s -> s.s ! Ag (GSg Neut) P3 ++ s.adv ++ copula.inf;
  VPSlash = \s -> let vp : VP
                            =  {verb = s.verb ;
                                adv = s.adv ;
                                dep = s.dep ;
                                compl = \\p, a => s.compl1 ! p ! a ++ s.c.s ++ s.compl2 ! p ! a ;
                                p = s.p
                               }
         in vp.adv ! Ag (GSg Neut) P3 ++ (verbInf vp.verb) ++ vp.dep ++ vp.compl ! Pos ! Ag (GSg Neut) P3 ;
  Cl = \s -> s.subj ++ s.adv ++ (verbInf s.verb) ++ s.dep ++ s.compl ! Pos ;
  ClSlash = \s -> s.subj ++ s.adv ++ (verbInf s.verb) ++ s.dep ++ s.compl ! Pos ;
  QCl = \s -> s.subj ++ s.adv ++ (verbInf s.verb) ++ s.dep ++ s.compl ! Pos ;
  RCl = \s -> s.subj ! GSg Neut ! Inanimate ! Nom ++ s.adv ! Ag (GSg Neut) P3 ++ (verbInf s.verb) ++ s.dep ++ s.compl ! Pos ! Ag (GSg Neut) P3  ;
  IP = \s -> s.nom ;
  RP = \s -> s.s!GSg Neut!Inanimate!Nom ;
}
