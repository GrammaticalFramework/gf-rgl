--# -path=.:abstract:common:prelude

concrete CatLav of Cat = CommonX - [Adv, CAdv] ** open ResLav, Prelude in {

flags

  coding = utf8 ;
  optimize = all_subs ;

lincat

  -- Sentences and clauses

  S, QS = {s : Str} ;

  RS = {s : Agreement => Str} ;

  Cl = {s : VMood => Polarity => Str} ;

  ClSlash = {s : VMood => Polarity => Str ; prep : Preposition} ;

  SSlash = {s : Str ; prep : Preposition} ;

  Imp = {s : Polarity => Number => Str} ;

  -- Questions and interrogatives

  QCl = {s : VMood => Polarity => Str} ;

  IP = {s : Case => Str ; num : Number} ;

  -- TODO: IComp = {s : Str ; agr : Agreement} ;

  IDet = {s : Gender => Str ; num : Number} ;

  IQuant = {s : Gender => Number => Str} ;

  -- Relative clauses and pronouns

  RCl = {s : VMood => Polarity => Agreement => Str} ;

  RP = {s : Gender => Case => Str} ;

  -- Verb phrases

  VP = ResLav.VP ;

  VPSlash = ResLav.VPSlash ;

  Comp = {s : Agreement => Str} ;

  -- Adjectival phrases

  AP = {s : Definiteness => Gender => Number => Case => Str} ;

  -- Nouns and noun phrases

  CN = {s : Definiteness => Number => Case => Str ; gend : Gender ; isRel : Bool} ;

  NP = {s : Case => Str ; agr : Agreement ; pol : Polarity ; isRel : Bool ; isPron : Bool} ;

  Pron = Pronoun ;

  Det = {s : Gender => Case => Str ; num : Number ; defin : Definiteness ; pol : Polarity} ;

  Predet = {s : Gender => Str} ;

  Quant = {s : Gender => Number => Case => Str ; defin : Definiteness ; pol : Polarity} ;

  Num = {s : Gender => Case => Str ; num : Number ; hasCard : Bool} ;

  Card = {s : Gender => Case => Str ; num : Number} ;

  Ord = {s : Gender => Case => Str} ;

  -- Numerals

  Numeral = {s : CardOrd => Gender => Case => Str ; num : Number} ;

  Digits = {s : CardOrd => Str ; num : Number} ;
  Decimal = {s : CardOrd => Str ; num : Number ; hasDot : Bool} ;

  -- Structural words

  Conj = {s1, s2 : Str ; num : Number} ;

  Subj = {s : Str} ;

  Prep = Preposition ;

  -- Words of open classes

  V, VV, VQ, VA = Verb ;

  V2, V2V, V2Q, V2A = Verb ** {rightVal : Preposition} ;

  V3 = Verb ** {rightVal1, rightVal2 : Preposition} ;

  VS = Verb ** {conj : Subj} ;

  V2S = Verb ** {conj : Subj ; rightVal : Preposition} ;

  A = Adjective ;

  A2 = Adjective ** {prep : Preposition} ;

  N = Noun ;

  N2 = Noun ** {prep : Preposition ; isPre : Bool} ;

  N3 = Noun ** {prep1, prep2 : Preposition ; isPre1, isPre2 : Bool} ;

  PN,LN = ProperNoun ;
  GN = {s : Case => Str ; gend : Gender} ;
  SN = {s : Sex => Case => Str; pl : Case => Str} ;

  -- Overriden from CommonX

  Adv = {s : Str ; isPron : Bool} ;

  CAdv = {s, prep : Str ; deg : Degree} ;
  
lindef
  V2 = \s -> {s=\\_,_=>s; leftVal=Nom; rightVal={s=[]; c=\\_ => Acc}} ;
  V3 = \s -> {s=\\_,_=>s; leftVal=Nom; rightVal1={s=[]; c=\\_ => Acc}; rightVal2={s=[]; c=\\_ => Dat}} ;

linref
  V2 = \v -> v.s ! Pos ! VInf ++ v.rightVal.s ;
  V3 = \v -> v.s ! Pos ! VInf ++ v.rightVal1.s ++ v.rightVal2.s ;

}
