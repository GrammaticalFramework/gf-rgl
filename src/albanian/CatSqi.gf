concrete CatSqi of Cat = CommonX ** open ParamX,Prelude,ResSqi in {
  lincat A = Adj ;
  linref A = \a -> case a.clit of {True=>"i"; False=>[]} ++ a.s ! Nom ! Masc ! Sg ;
  lincat A2 = Adj ** {c2 : Compl} ;
  linref A2 = \a -> case a.clit of {True=>"i"; False=>[]} ++ a.s ! Nom ! Masc ! Sg ++ a.c2.s ;
  lincat V, VA, VV, VS, VQ = Verb ;
  lincat V2 = Verb ** {c2 : Compl} ;
  lincat V2S, V2Q, V3, V2A, V2V = Verb ** {c2,c3 : Compl} ;
  lincat Prep = Compl ;
  lincat ACard = {s : Str} ;
  lincat AP = {s : Species => Case => Gender => Number => Str} ;
  lincat CN = Noun ;
  lincat Card = {s : Str} ;
  lincat Cl = {s : ParamX.Tense => Anteriority => Polarity => Str} ;
  lincat ClSlash = {s : ParamX.Tense => Anteriority => Polarity => Str; c2 : Compl} ;
  lincat Comp = {s : Agr => Str} ;
  lincat Conj = {s : Str} ;
  lincat DAP = {s : Str; n : Number} ;
  lincat Decimal = {s : Str; n : Number; hasDot : Bool} ;
  lincat Det = {
    s : Case => Gender => Str;
    post : Species => Case => Gender => Str;
    sp : Species;
    n : Number
    } ;
  lincat Digits = {s : Str; n : Number; tail : DTail} ;
  lincat GN = {s : Str} ;
  lincat IComp = {s : Str} ;
  lincat IDet = {s : Gender => Str; n : Number} ;
  lincat IP = {s : Str; a : Agr} ;
  lincat IQuant = {s : Gender => Str} ;
  lincat Imp = {s : Polarity => Number => Str} ;
  lincat LN = {s : Str} ;
  lincat N = Noun ;
  lincat N2 = {s : Species => Case => Number => Str; g : Gender;
               c2 : Compl} ;
  lincat N3 = {s : Species => Case => Number => Str; g : Gender;
               c2 : Compl; c3 : Compl} ;
  lincat NP = {s : Case => Str; a : Agr} ;
  lincat Num = {s : Str; n : Number} ;
  lincat Numeral = {s : Str} ;
  lincat Ord = {s : Case => Gender => Number => Str} ;
  lincat PN = {s : Str} ;
  lincat Predet = {s : Str} ;
  lincat Pron = {s: Case => Str; acc_clit, dat_clit : Str; a : Agr} ;
  lincat QCl = {s : ParamX.Tense => Anteriority => Polarity => Str} ;
  lincat QS = {s : Str} ;
  lincat Quant = {
    s : Case => Gender => Number => Str;
    sp : Species;
    isPoss : Bool
    } ;
  lincat RCl = {s : Agr => ParamX.Tense => Anteriority => Polarity => Str} ;
  lincat RP = {s : Case => GenNum => Str} ;
  lincat RS = {s : Agr => Str} ;
  lincat S = {s : Str} ;
  lincat SN = {s : Str} ;
  lincat SSlash = {s : Str; c2 : Compl} ;
  lincat Subj = {s : Str} ;
  lincat VP = {indicative : Tense => Number => Person => Gender => Case => Str;
               subjunctive : Number => Person => Gender => Case => Str;
               imperative : Number => Case => Str;
               participle : Agr => Case => Str;
               pres_optative : Number => Person => Case => Str;
               perf_optative : Number => Person => Case => Str;
               pres_admirative : Number => Person => Case => Str;
               imperf_admirative : Number => Person => Case => Str;
               vtype : VType} ;

  lincat VPSlash = {indicative : Tense => Number => Person => Str;
                    subjunctive : Number => Person => Str;
                    imperative : Number => Str; participle : Str;
                    pres_optative : Number => Person => Str;
                    perf_optative : Number => Person => Str;
                    pres_admirative : Number => Person => Str;
                    imperf_admirative : Number => Person => Str;
                    vtype : VType;
                    c2 : Compl} ;
}
