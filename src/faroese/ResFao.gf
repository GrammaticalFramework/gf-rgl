resource ResFao = ParamX - [Tense,Pres,Past] ** {

param Species = Indef | Def ;
param Case = Nom | Acc | Dat | Gen ;
param Gender = Masc | Fem | Neuter ;
oper Noun = {s: Species => Number => Case => Str; g : Gender} ; -- 2135
oper mkNoun : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Gender -> Noun =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,g ->
          { s = table {
                  Indef => table {
                             Sg => table {
                                     Nom => f1 ;
                                     Acc => f2 ;
                                     Dat => f3 ;
                                     Gen => f4
                                   } ;
                             Pl => table {
                                     Nom => f5 ;
                                     Acc => f6 ;
                                     Dat => f7 ;
                                     Gen => f8
                                   }
                           } ;
                  Def => table {
                           Sg => table {
                                   Nom => f9 ;
                                   Acc => f10 ;
                                   Dat => f11 ;
                                   Gen => f12
                                 } ;
                           Pl => table {
                                   Nom => f13 ;
                                   Acc => f14 ;
                                   Dat => f15 ;
                                   Gen => f16
                                 }
                         }
                } ;
            g = g
          } ;


oper Adj = {s: Gender => Number => Case => Str} ; -- 346
oper mkAdj : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Adj =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24 ->
          { s = table {
                  Masc => table {
                            Sg => table {
                                    Nom => f1 ;
                                    Acc => f2 ;
                                    Dat => f3 ;
                                    Gen => f4
                                  } ;
                            Pl => table {
                                    Nom => f5 ;
                                    Acc => f6 ;
                                    Dat => f7 ;
                                    Gen => f8
                                  }
                          } ;
                  Fem => table {
                           Sg => table {
                                   Nom => f9 ;
                                   Acc => f10 ;
                                   Dat => f11 ;
                                   Gen => f12
                                 } ;
                           Pl => table {
                                   Nom => f13 ;
                                   Acc => f14 ;
                                   Dat => f15 ;
                                   Gen => f16
                                 }
                         } ;
                  Neuter=> table {
                             Sg => table {
                                     Nom => f17 ;
                                     Acc => f18 ;
                                     Dat => f19 ;
                                     Gen => f20
                                   } ;
                             Pl => table {
                                     Nom => f21 ;
                                     Acc => f22 ;
                                     Dat => f23 ;
                                     Gen => f24
                                   }
                           }
                }
          } ;

param Tense = Pres | Past ;

param PersNum = PSg Person | PPl ;
oper persNum : Number -> Person -> PersNum =
       \n,p -> case n of {
                 Sg => PSg p ;
                 Pl => PPl
               } ;
oper persNumNumber : PersNum -> Number =
       \pn -> case pn of {
                PSg _ => Sg ;
                PPl   => Pl
              } ;

oper Verb = {Converb: Str; imperative: Number => Str; Indicative: Tense => PersNum => Str; Nonfinite: Str; Participle: Tense => Str ; particle : Str} ; -- 596
oper mkVerb : (_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Verb =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14 ->
          { Converb = f1 ;
            imperative = table {
                           Sg => f2 ;
                           Pl => f3
                         } ;
            Indicative = table {
                           Pres => table {
                                     PSg P1 => f4 ;
                                     PSg P2 => f5 ;
                                     PSg P3 => f6 ;
                                     PPl => f7
                                   } ;
                           Past => table {
                                     PSg P1 => f8 ;
                                     PSg P2 => f9 ;
                                     PSg P3 => f10 ;
                                     PPl => f11
                                   }
                         } ;
            Nonfinite = f12 ;
            Participle = table {
                           Pres => f13 ;
                           Past => f14
                         } ;
           particle = []
          } ;


oper Compl = {s : Str; c : Case} ;
oper noPrep : Compl = {s=""; c=Acc} ;

oper CommonNoun = Noun ;
oper AdjPhrase = Adj ;
oper VerbPhrase = {
  Converb : Str ;
  Indicative : Tense => Polarity => Gender => PersNum => Str ;
  Nonfinite : Str ;
  Participle : Tense => Str ;
} ;
oper Clause = {
  Converb : Str ;
  Indicative : Tense => Polarity => Str ;
  Nonfinite : Str ;
  Participle : Tense => Str
} ;

oper
  copula : Tense => PersNum => Str =
    table {
      Pres => table {
                PSg P1 => "eri" ;
                PSg P2 => "ert" ;
                PSg P3 => "er" ;
                PPl => "eru"
              } ;
      Past => table {
                PSg P1 => "var" ;
                PSg P2 => "vart" ;
                PSg P3 => "var" ;
                PPl => "vóru"
              }
    } ;

oper
  negStr : Polarity -> Str = \pol -> case pol of {
    Pos => [] ;
    Neg => "ikki"
  } ;

  mkNP : Str -> Gender -> Number -> Person -> {s : Case => Str ; g : Gender ; n : Number ; p : Person} =
    \str,g,n,p -> {
      s = \\_ => str ;
      g = g ;
      n = n ;
      p = p
    } ;

  mkCN : Str -> Gender -> CommonNoun =
    \str,g -> {
      s = \\_,_,_ => str ;
      g = g
    } ;

  mkVP : Str -> VerbPhrase =
    \str -> {
      Converb = str ;
      Indicative = \\_,pol,_,_ => str ++ negStr pol ;
      Nonfinite = str ;
      Participle = \\_ => str
    } ;

}
