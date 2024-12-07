concrete CatSqi of Cat = CommonX ** open ParamX, Prelude, ResSqi in {

lincat N = Noun ;
lincat N2 = Noun ** {c2 : Compl} ;
lincat N3 = Noun ** {c2,c3 : Compl} ;
lincat A = Adj ;
lincat A2 = Adj ** {c2 : Compl} ;
lincat V, VA, VV, VS, VQ = Verb ;
lincat V2, V2S, V2Q = Verb ** {c2 : Compl} ;
lincat V3, V2A, V2V = Verb ** {c2,c3 : Compl} ;
lincat Prep = Compl ;
lincat Interj = {s : Str} ;
lincat Voc = {s : Str} ;

lincat Numeral = {s : Str} ;
lincat Digits  = {s : Str; n : Number; tail : DTail} ;
lincat Decimal = {s : Str; n : Number; hasDot : Bool} ;

lincat AP = {s: Species => Case => Gender => Number => Str} ;
lincat CN = Noun ;
lincat Num = {s : Str; n : Number} ;
lincat Quant = {s : Case => Gender => Number => Str; spec : Species} ;
lincat Det = {s : Case => Gender => Str; spec : Species; n : Number} ;
lincat NP = {s: Case => Str; a : Agr} ;
lincat Pron = {s: Case => Str; acc_clit, dat_clit : Str; a : Agr} ;

lincat PConj = {s : Str} ;
lincat Phr, Utt = {s : Str} ;

}
