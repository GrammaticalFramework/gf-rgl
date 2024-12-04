concrete CatAlb of Cat = CommonX ** open ParamX, Prelude, ResAlb in {

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

}
