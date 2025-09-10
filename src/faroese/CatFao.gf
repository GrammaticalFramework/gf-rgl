concrete CatFao of Cat = CommonX ** open ResFao in {

lincat N = Noun ;
lincat N2 = Noun ** {c2 : Compl} ;
lincat N3 = Noun ** {c2,c3 : Compl} ;
lincat A = Adj ;
lincat A2 = Adj ** {c2 : Compl} ;
lincat V = Verb ;
lincat VV,VS,VQ,VA = Verb ;
lincat V2 = Verb ** {c2 : Compl} ;
lincat V3,V2A,V2S,V2Q,V2V = Verb ** {c2,c3 : Compl} ;
lincat Prep = Compl ;
lincat CN = CommonNoun ;
lincat AP = AdjPhrase ;
lincat S = {s : Str} ;

lincat LN,SN,GN,PN = {s : Str} ;

}
