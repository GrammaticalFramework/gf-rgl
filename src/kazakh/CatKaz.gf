concrete CatKaz of Cat = CommonX ** open ResKaz in {

lincat N = Noun ;
lincat N2 = Noun ** {c2 : Compl} ;
lincat N3 = Noun ** {c2,c3 : Compl} ;
lincat V, VA, VV, VS, VQ = Verb ;
lincat V2, V2S, V2Q = Verb ** {c2 : Compl} ;
lincat V3, V2A, V2V = Verb ** {c2,c3 : Compl} ;
lincat A = {s : Str} ;
lincat A2 = A ** {c2 : Compl} ;
lincat Prep = Compl ;
lincat Interj = {s : Str} ;
lincat Voc = {s : Str} ;

}
