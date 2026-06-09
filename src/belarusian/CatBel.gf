concrete CatBel of Cat = CommonX ** open ResBel, (R = ParamX) in {

lincat
  S = {s : Str} ;
  QS = {s : Str} ;
  RS = {s : Str} ;
  Cl = {s : R.Tense => R.Polarity => Str} ;
  ClSlash = {s : R.Tense => R.Polarity => Str; c : Compl} ;
  SSlash = {s : Str; c : Compl} ;
  Imp = {s : R.Polarity => Number => Str} ;

  QCl = {s : R.Tense => R.Polarity => Str} ;
  IP = NPhrase ;
  IComp = {s : Str} ;
  IDet = {s : Case => Gender => Str; n : Number} ;
  IQuant = {s : Case => Gender => Number => Str} ;

  RCl = {s : R.Tense => R.Polarity => Str} ;
  RP = {s : Str} ;

  VP = VPhrase ;
  Comp = {s : Agr => Str} ;
  VPSlash = VSlash ;

  N = Noun ;
  N2 = Noun ** {c2 : Compl} ;
  N3 = Noun ** {c2,c3 : Compl} ;
  CN = CommonNoun ;
  NP = NPhrase ;
  Pron = {s: Case => Str; a: Agr} ;
  Det = {s : Case => Gender => Str; n : Number} ;
  Predet = {s : Case => Gender => Number => Str} ;
  Quant = {s : Case => Gender => Number => Str} ;
  Num = {s : Case => Gender => Str; n : Number} ;
  Card = {s : Str; n : Number} ;
  ACard = {s : Str; n : Number} ;
  Ord = Adj ;
  DAP = {s : Case => Gender => Str; n : Number} ;

  Numeral = {s : Str} ;
  Digits = {s : Str} ;
  Decimal = {s : Str} ;

  Conj = {s : Str; n : Number} ;
  Subj = {s : Str} ;
  Prep = Compl ;

  V = Verb ;
  VV,VS,VQ,VA = Verb ;
  V2 = Verb ** {c2 : Compl} ;
  V3,V2A,V2S,V2Q,V2V = Verb ** {c2,c3 : Compl} ;
  A = Adj ;
  A2 = Adj ** {c2 : Compl} ;
  AP = AdjPhrase ;

  LN = {s : Case => Str; g : Gender; n : Number} ;
  PN = {s : Case => Str; g : Gender; n : Number} ;
  GN = {s : Str; g : Gender} ;
  SN = {s : Str} ;

linref
  V,VV,V2,V3,V2A,V2S,V2Q,V2V = \v -> v.infinitive ;
  N,N2,N3 = \n -> n.s ! Nom ! Sg ;
  A,A2,AP,Ord = \a -> a.s ! Nom ! GSg Masc ;
  NP,IP = \np -> np.s ! Nom ;
  S,QS,RS = \s -> s.s ;

}
