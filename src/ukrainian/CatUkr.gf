concrete CatUkr of Cat = CommonX ** open ResUkr, (R = ParamX) in {

lincat N = N ;
lincat N2 = N ** {c2 : Compl} ;
lincat N3 = N ** {c2,c3 : Compl} ;
lincat Pron = {s: Case => Str; g: Gender; n : Number; p: Person} ;
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
lincat QS = {s : Str} ;
lincat RS = {s : Gender => Number => Str} ;
lincat Cl = {s : R.Tense => R.Polarity => Str} ;
lincat ClSlash = {s : R.Tense => R.Polarity => Str; c : Compl} ;
lincat SSlash = {s : Str; c : Compl} ;
lincat RCl = {s : Gender => Number => Str} ;
lincat QCl = {s : R.Tense => R.Polarity => Str} ;
lincat RP = {s : Gender => Number => Case => Str} ;
lincat IP = {s : Case => Str; g : Gender; n : Number; p : Person} ;
lincat IComp = {s : Str} ;
lincat IDet = {s : Case => Gender => Str; n : Number} ;
lincat IQuant = {s : Case => Gender => Number => Str} ;
lincat VP = {s : R.Tense => R.Polarity => Gender => Number => Person => Str; inf : Str; imp : R.Polarity => Number => Str} ;
lincat VPSlash = {s : R.Tense => R.Polarity => Gender => Number => Person => Str; inf : Str; imp : R.Polarity => Number => Str; c : Compl; post : Str} ;
lincat Comp = {s : Gender => Number => Str} ;
lincat NP = {s : Case => Str; g : Gender; n : Number; p : Person} ;
lincat Det = {s : Case => Gender => Str; n : Number} ;
lincat Predet = {s : Case => Gender => Number => Str} ;
lincat Quant = {s : Case => Gender => Number => Str} ;
lincat Num = {s : Str; n : Number} ;
lincat Card = {s : Str; n : Number} ;
lincat ACard = {s : Str; n : Number} ;
lincat Ord = A ;
lincat DAP = {s : Case => Gender => Str; n : Number} ;
lincat Imp = {s : R.Polarity => Number => Str} ;
lincat Conj = {s1,s2 : Str; n : Number} ;
lincat Subj = {s : Str} ;
lincat Numeral = {s : Str} ;
lincat Digits = {s : Str} ;
lincat Decimal = {s : Str} ;

lincat LN,SN,GN,PN = {s : Str} ;

linref V,VV,V2,V3,V2A,V2S,V2Q,V2V = \v -> v.infinitive ;
linref NP = \np -> np.s ! Nom ;
linref CN = \cn -> cn.s ! Nom ! Sg ;

}
