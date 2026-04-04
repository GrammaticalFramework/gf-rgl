concrete CatMkd of Cat = CommonX ** open ResMkd, Prelude in {

lincat N = Noun ;
lincat N2 = Noun ** {c2 : Compl} ;
lincat N3 = Noun ** {c2,c3 : Compl} ;
lincat CN = {s: Species => Number => Str; count_form: Str; vocative: Number => Str; g: Gender} ;
lincat V, VA, VV, VS, VQ = Verb ;
lincat V2, V2S, V2Q = Verb ** {c2 : Compl} ;
lincat V3, V2A, V2V = Verb ** {c2,c3 : Compl} ;
lincat A = Adj ;
lincat A2 = Adj ** {c2 : Compl} ;
lincat Pron = Pron ;
lincat Prep = Compl ;
lincat Decimal = {s : Str; n : Number; hasDot : Bool} ;
lincat Digits = {s : Str; n : Number; tail : DTail} ;

linref N,N2,N3 = \n -> n.s ! Indef ! Sg ;
linref V, VA, VV, VS, VQ, V2, V2S, V2Q, V3, V2A, V2V = 
  \v -> v.present ! Imperfective ! Sg ! P3 ++
        case v.isRefl of {
          True  => "се" ;
          False => []
        } ;
linref A, A2 = \a -> a.s ! Indef ! GSg Masc ;

lincat AP = {s : Species => GenNum => Str; isPre : Bool} ;
lincat NP = {s : Role => Str; vocative: Str; g : GenNum; p : Person} ;
lincat Num = {s : Str; n : Number} ;
lincat Quant = {s : Str; sp : Species} ;
lincat Det = {s : Str; n : Number; sp : Species} ;

lincat VP = {present : Aspect => Number => Person => Str;
             aorist : Number => Person => Str;
             imperfect : Aspect => Number => Person => Str;
             imperative : Aspect => Number => Str;
             participle : {aorist : Aspect => GenNum => Str; perfect : Aspect => Str}} ;
lincat VPSlash = {present : Aspect => Number => Person => Str;
                  aorist : Number => Person => Str;
                  imperfect : Aspect => Number => Person => Str;
                  imperative : Aspect => Number => Str;
                  participle : {aorist : Aspect => GenNum => Str;
                                imperfect : GenNum => Str; perfect : Aspect => Str;
                                adjectival : Aspect => Str; adverbial : Str};
                  noun_from_verb : Str; isRefl : Bool; c2 : Compl} ;
lincat Cl = {present : Aspect => Str;
             aorist : Str;
             participle : {aorist : Aspect => Str; perfect : Aspect => Str}} ;
lincat IP = {s : Str} ;
lincat Subj = {s : Str} ;

}
