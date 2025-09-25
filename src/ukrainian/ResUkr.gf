resource ResUkr = {

param Case = Nom | Acc | Dat | Gen | Loc | Instr ;
param Number = Sg | Pl ;
param Gender = Masc | Neuter | Fem ;
oper N = {s: Case => Number => Str; Voc: Number => Str; g: Gender} ; -- 11407
oper mkN : (_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Gender -> N =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,g ->
          { s = table {
                  Nom => table {
                           Sg => f1 ;
                           Pl => f2
                         } ;
                  Acc => table {
                           Sg => f3 ;
                           Pl => f4
                         } ;
                  Dat => table {
                           Sg => f5 ;
                           Pl => f6
                         } ;
                  Gen => table {
                           Sg => f7 ;
                           Pl => f8
                         } ;
                  Loc => table {
                           Sg => f9 ;
                           Pl => f10
                         } ;
                  Instr => table {
                             Sg => f11 ;
                             Pl => f12
                           }
                } ;
            Voc = table {
                    Sg => f13 ;
                    Pl => f14
                  } ;
            g = g
          } ;


param Aspect = Perf | Imperf ;
param Person = P1 | P2 | P3 ;
param Tense = Past | Pres ;
oper V = {active: Aspect => {Past: Str; Pres: Person => Number => Str}; imperative1: Str; imperative2: Number => Str; infinitive: Str; participle: Gender => Number => Str; passive: Aspect => Tense => Str} ; -- 4822
oper mkV : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> V =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24,f25,f26,f27,f28 ->
          { active = table {
                       Imperf => { Past = f1 ;
                                   Pres = table {
                                            P1 => table {
                                                    Sg => f2 ;
                                                    Pl => f3
                                                  } ;
                                            P2 => table {
                                                    Sg => f4 ;
                                                    Pl => f5
                                                  } ;
                                            P3 => table {
                                                    Sg => f6 ;
                                                    Pl => f7
                                                  }
                                          }
                                 } ;
                       Perf => { Past = f8 ;
                                 Pres = table {
                                          P1 => table {
                                                  Sg => f9 ;
                                                  Pl => f10
                                                } ;
                                          P2 => table {
                                                  Sg => f11 ;
                                                  Pl => f12
                                                } ;
                                          P3 => table {
                                                  Sg => f13 ;
                                                  Pl => f14
                                                }
                                        }
                               }
                     } ;
            imperative1 = f15 ;
            imperative2 = table {
                            Sg => f16 ;
                            Pl => f17
                          } ;
            infinitive = f18 ;
            participle = table {
                           Masc => table {
                                     Sg => f19 ;
                                     Pl => f20
                                   } ;
                           Fem => table {
                                    Sg => f21 ;
                                    Pl => f22
                                  } ;
                           Neuter => table {
                                       Sg => f23 ;
                                       Pl => f24
                                     }
                         } ;
            passive = table {
                        Imperf => table {
                                    Pres => f25 ;
                                    Past => f26
                                  } ;
                        Perf => table {
                                  Pres => f27 ;
                                  Past => f28
                                }
                      }
          } ;

param GenNum = GSg Gender | GPl ;
oper A = {s: Case => GenNum => Str} ; -- 4394
oper mkA : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> A =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24 ->
          { s = table {
                  Nom => table {
                           GSg Masc => f1 ;
                           GSg Fem => f2 ;
                           GSg Neuter => f3 ;
                           GPl => f4
                         } ;
                  Acc => table {
                           GSg Masc => f5 ;
                           GSg Fem => f6 ;
                           GSg Neuter => f7 ;
                           GPl => f8
                         } ;
                  Dat => table {
                           GSg Masc => f9 ;
                           GSg Fem => f10 ;
                           GSg Neuter => f11 ;
                           GPl => f12
                         } ;
                  Gen => table {
                           GSg Masc => f13 ;
                           GSg Fem => f14 ;
                           GSg Neuter => f15 ;
                           GPl => f16
                         } ;
                  Loc => table {
                           GSg Masc => f17 ;
                           GSg Fem => f18 ;
                           GSg Neuter => f19 ;
                           GPl => f20
                         } ;
                  Instr => table {
                             GSg Masc => f21 ;
                             GSg Fem => f22 ;
                             GSg Neuter => f23 ;
                             GPl => f24
                           }
                }
          } ;


oper Compl = {s : Str; c : Case} ;
oper noPrep : Compl = {s=""; c=Acc} ;

oper CommonNoun = {s : Str} ;
oper AdjPhrase = {s : Str} ;

}
