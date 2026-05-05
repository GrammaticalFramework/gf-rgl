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
lincat VP = VerbPhrase ;
lincat VPSlash = Verb ** {c2 : Compl} ;
lincat Comp = {s : Gender => Number => Str} ;
lincat Cl = Clause ;
lincat Prep = Compl ;
lincat CN = CommonNoun ;
lincat NP, Pron = {s : Case => Str ; g : Gender ; n : Number ; p : Person} ;
lincat RCl = {s : Tense => Polarity => Gender => PersNum => Str} ;
lincat RS = {s : Gender => PersNum => Str} ;
lincat RP = {s : Str} ;
lincat AP = AdjPhrase ;
lincat Det = {s : Gender => Case => Str ; n : Number ; sp : Species} ;
lincat Quant = {s : Gender => Number => Case => Str ; sp : Species} ;
lincat Num = {s : Gender => Case => Str ; n : Number} ;
lincat S = {s : Str} ;

lincat LN,SN,GN,PN = {s : Str} ;

linref V = \v -> v.Nonfinite ++ v.particle ;
}
