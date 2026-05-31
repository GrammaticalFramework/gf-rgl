concrete CatFao of Cat = CommonX ** open ResFao, Prelude in {

lincat N = Noun ;
lincat N2 = Noun ** {c2 : Compl} ;
lincat N3 = Noun ** {c2,c3 : Compl} ;
lincat A = Adj ;
lincat A2 = Adj ** {c2 : Compl} ;
lincat V = Verb ;
lincat VV,VS,VQ,VA = Verb ;
lincat V2 = Verb ** {c2 : Compl} ;
lincat V3,V2A,V2S,V2Q,V2V = Verb ** {c2,c3 : Compl} ;
lincat VP = VerbPhrase ;
lincat VPSlash = Verb ** {c2 : Compl ; sc : Str} ;
lincat Comp = {s : Gender => Number => Str} ;
lincat Cl = Clause ;
lincat ClSlash = {s : Tense => Polarity => Str ; c2 : Compl} ;
lincat SSlash = {s : Str ; c2 : Compl} ;
lincat Imp = {s : Polarity => Number => Str} ;
lincat Prep = Compl ;
lincat CN = CommonNoun ;
lincat NP, Pron = {s : Case => Str ; g : Gender ; n : Number ; p : Person} ;
lincat QS = {s : Str} ;
lincat QCl = {s : Tense => Polarity => Str} ;
lincat IP = {s : Str ; n : Number} ;
lincat IComp = {s : Str} ;
lincat IDet = {s : Str ; n : Number} ;
lincat IQuant = {s : Str} ;
lincat RCl = {s : Tense => Polarity => Gender => PersNum => Str} ;
lincat RS = {s : Gender => PersNum => Str} ;
lincat RP = {s : Str} ;
lincat AP = AdjPhrase ;
lincat Det = {s : Gender => Case => Str ; n : Number ; sp : Species} ;
lincat Predet = {s : Str} ;
lincat Quant = {s : Bool => Gender => Number => Case => Str; sp : Species} ;
lincat Num = {s : Gender => Case => Str ; n : Number ; hasCard : Bool} ;
lincat Card = {s : Gender => Case => Str ; n : Number} ;
lincat ACard = {s : Str} ;
lincat Ord = {s : Gender => Number => Case => Str} ;
lincat DAP = {s : Gender => Case => Str ; n : Number ; sp : Species} ;
lincat S = {s : Str} ;
lincat Numeral = {s : Gender => Case => Str ; n : Number} ;
lincat Digits = {s : Str ; n : Number} ;
lincat Decimal = {s : Str ; n : Number ; hasDot : Bool} ;
lincat Conj = {s : Str} ;
lincat Subj = {s : Str} ;

lincat LN,SN,GN,PN = {s : Str} ;

linref V = \v -> v.Nonfinite ++ v.particle ;
}
