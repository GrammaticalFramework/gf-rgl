concrete CatUkr of Cat = CommonX ** open ResUkr in {

lincat N = N ;
lincat N2 = N ** {c2 : Compl} ;
lincat N3 = N ** {c2,c3 : Compl} ;
lincat V = V ;
lincat VV,VS,VQ,VA = V ;
lincat V2 = V ** {c2 : Compl} ;
lincat V3,V2A,V2S,V2Q,V2V = V ** {c2,c3 : Compl} ;
lincat A = A ;
lincat A2 = A ** {c2 : Compl} ;
lincat Prep = Compl ;
lincat CN = CommonNoun ;
lincat AP = AdjPhrase ;
lincat S = {s : Str} ;

lincat LN,SN,GN,PN = {s : Str} ;

linref V,VV,V2,V3,V2A,V2S,V2Q,V2V = \v -> v.infinitive ;

}
