concrete CatMkd of Cat = CommonX - [Temp,Tense] ** open ParamX, ResMkd, Prelude, Predef in {

lincat
  Temp  = {s : Str ; t : ResMkd.Tense ; a : Anteriority} ;
  Tense = {s : Str ; t : ResMkd.Tense} ;

lincat N = Noun ;
lincat N2 = Noun ** {c2 : Compl} ;
lincat N3 = Noun ** {c2,c3 : Compl} ;
lincat CN = {s: Species => Number => Str; count_form: Str; vocative: Number => Str; g: Gender} ;
lincat V, VA, VV, VS, VQ = Verb ;
lincat V2, V2S, V2Q = Verb ** {c2 : Compl} ;
lincat V3, V2A, V2V = Verb ** {c2,c3 : Compl} ;
lincat A = Adj ;
lincat A2 = Adj ** {c2 : Compl} ;
lincat Pron = Pronoun ;
lincat Prep = Compl ;
lincat Decimal = {s : Str; n : Number; hasDot : Bool} ;
lincat Digits = {s : Str; n : Number; tail : DTail} ;

linref N,N2,N3 = \n -> n.s ! Indef ! Sg ;
linref V, VA, VV, VS, VQ, V2, V2S, V2Q, V3, V2A, V2V = 
  \v -> v.present ! Imperfective ! Sg ! P3 ++
        case v.vtype of {
          VNormal => [] ;
          VMedial Acc => "се" ;
          VMedial Dat => "си"
        } ;
linref A, A2 = \a -> a.s ! Indef ! GSg Masc ;

lincat AP = {s : Species => GenNum => Str; isPre : Bool} ;
lincat NP = {s : Role => Str; vocative: Str; a : Agr} ;
lincat Num = {s : Str; n : NNumber} ;
lincat Quant = {s : GenNum => Str; sp : Species} ;
lincat Ord = {s : Species => GenNum => Str} ;
lincat Det, DAP = {s : Gender => Str; n : NNumber; sp : Species} ;

lincat VP = Verb ** {compl : Agr => Str} ;
lincat VPSlash = Verb ** {compl : Agr => Str; c2 : Compl} ;
lincat S,QS = {s : Str} ;
lincat RS = {s : GenNum => Str} ;
lincat Cl = {s : Order => ResMkd.Tense => Anteriority => Polarity => Str} ;
lincat QCl = {s : ResMkd.Tense => Anteriority => Polarity => Str} ;
lincat RCl = {s : GenNum => ResMkd.Tense => Anteriority => Polarity => Str} ;
lincat RP = {s : GenNum => Str} ;
lincat IP = {s : Str; g : GenNum} ;
lincat IQuant = {s : GenNum => Str} ;
lincat IDet = {s : Gender => Str; n : Number} ;
lincat Subj = {s : Str} ;
lincat Imp = {s : Polarity => GenNum => Str} ;
lincat Comp = {s : GenNum => Str} ;

lincat Conj = {s : Str; sep : Ints 3; n : Number} ;

}
