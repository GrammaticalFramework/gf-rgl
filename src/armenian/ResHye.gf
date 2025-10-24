resource ResHye = {

param Aspect = Non_Past | Perfect ;
param Person = P1 | P3 | P2 ;
param Number = Sg | Pl ;
param Case = Nom | Dat | Ablat | Instr | Loc ;
param PartType = Resultative | Subject ;
oper Verb = {s: Str; Causative: Str; Conditional: Aspect => Person => Number => Str; Converb: {Imperfective: Str; FutCon1: Str; FutCon2: Str; Negative: Str; Perfective: Str; Simultaneous: Str}; Imperative_Jussive: Number => Str; Passive: Str; Past: Person => Number => Str; Participle: PartType => Str; Subjunctive: Aspect => Person => Number => Str} ; -- 898
oper mkVerb : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Verb =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24,f25,f26,f27,f28,f29,f30,f31,f32,f33,f34,f35,f36,f37,f38,f39,f40,f41,f42,f43 ->
          { s = f1 ;
            Causative = f2 ;
            Conditional = table {
                            Perfect => table {
                                         P1 => table {
                                                 Sg => f3 ;
                                                 Pl => f4
                                               } ;
                                         P2 => table {
                                                 Sg => f5 ;
                                                 Pl => f6
                                               } ;
                                         P3 => table {
                                                 Sg => f7 ;
                                                 Pl => f8
                                               }
                                       } ;
                            Non_Past => table {
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
                          } ;
            Converb = { Imperfective = f15 ;
                        FutCon1 = f16 ;
                        FutCon2 = f17 ;
                        Negative = f18 ;
                        Perfective = f19 ;
                        Simultaneous = f20
                      } ;
            Imperative_Jussive = table {
                                   Sg => f21 ;
                                   Pl => f22
                                 } ;
            Passive = f23 ;
            Past = table {
                     P1 => table {
                             Sg => f24 ;
                             Pl => f25
                           } ;
                     P2 => table {
                             Sg => f26 ;
                             Pl => f27
                           } ;
                     P3 => table {
                             Sg => f28 ;
                             Pl => f29
                           }
                   } ;
            Participle = table {
                           Resultative => f30 ;
                           Subject => f31
                         } ;
            Subjunctive = table {
                            Perfect => table {
                                         P1 => table {
                                                 Sg => f32 ;
                                                 Pl => f33
                                               } ;
                                         P2 => table {
                                                 Sg => f34 ;
                                                 Pl => f35
                                               } ;
                                         P3 => table {
                                                 Sg => f36 ;
                                                 Pl => f37
                                               }
                                       } ;
                            Non_Past => table {
                                          P1 => table {
                                                  Sg => f38 ;
                                                  Pl => f39
                                                } ;
                                          P2 => table {
                                                  Sg => f40 ;
                                                  Pl => f41
                                                } ;
                                          P3 => table {
                                                  Sg => f42 ;
                                                  Pl => f43
                                                }
                                        }
                          }
          } ;


oper Noun = {s: Case => Number => Str; def_dat: Number => Str; def_nom: Number => Str; poss1: Case => Number => Str; poss2: Case => Number => Str} ; -- 4880
oper mkNoun : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Noun =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24,f25,f26,f27,f28,f29,f30,f31,f32,f33,f34 ->
          { s = table {
                  Nom => table {
                           Sg => f1 ;
                           Pl => f2
                         } ;
                  Dat => table {
                           Sg => f3 ;
                           Pl => f4
                         } ;
                  Ablat => table {
                             Sg => f5 ;
                             Pl => f6
                           } ;
                  Instr => table {
                             Sg => f7 ;
                             Pl => f8
                           } ;
                  Loc => table {
                           Sg => f9 ;
                           Pl => f10
                         }
                } ;
            def_dat = table {
                        Sg => f11 ;
                        Pl => f12
                      } ;
            def_nom = table {
                        Sg => f13 ;
                        Pl => f14
                      } ;
            poss1 = table {
                      Nom => table {
                               Sg => f15 ;
                               Pl => f16
                             } ;
                      Dat => table {
                               Sg => f17 ;
                               Pl => f18
                             } ;
                      Ablat => table {
                                 Sg => f19 ;
                                 Pl => f20
                               } ;
                      Instr => table {
                                 Sg => f21 ;
                                 Pl => f22
                               } ;
                      Loc => table {
                               Sg => f23 ;
                               Pl => f24
                             }
                    } ;
            poss2 = table {
                      Nom => table {
                               Sg => f25 ;
                               Pl => f26
                             } ;
                      Dat => table {
                               Sg => f27 ;
                               Pl => f28
                             } ;
                      Ablat => table {
                                 Sg => f29 ;
                                 Pl => f30
                               } ;
                      Instr => table {
                                 Sg => f31 ;
                                 Pl => f32
                               } ;
                      Loc => table {
                               Sg => f33 ;
                               Pl => f34
                             }
                    }
          } ;


oper Adj = {s: Case => Number => Str; def_dat: Number => Str; def_nom: Number => Str; poss1: Case => Number => Str; poss2: Case => Number => Str} ; -- 1608
oper mkAdj : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Adj =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24,f25,f26,f27,f28,f29,f30,f31,f32,f33,f34 ->
          { s = table {
                  Nom => table {
                           Sg => f1 ;
                           Pl => f2
                         } ;
                  Dat => table {
                           Sg => f3 ;
                           Pl => f4
                         } ;
                  Ablat => table {
                             Sg => f5 ;
                             Pl => f6
                           } ;
                  Instr => table {
                             Sg => f7 ;
                             Pl => f8
                           } ;
                  Loc => table {
                           Sg => f9 ;
                           Pl => f10
                         }
                } ;
            def_dat = table {
                        Sg => f11 ;
                        Pl => f12
                      } ;
            def_nom = table {
                        Sg => f13 ;
                        Pl => f14
                      } ;
            poss1 = table {
                      Nom => table {
                               Sg => f15 ;
                               Pl => f16
                             } ;
                      Dat => table {
                               Sg => f17 ;
                               Pl => f18
                             } ;
                      Ablat => table {
                                 Sg => f19 ;
                                 Pl => f20
                               } ;
                      Instr => table {
                                 Sg => f21 ;
                                 Pl => f22
                               } ;
                      Loc => table {
                               Sg => f23 ;
                               Pl => f24
                             }
                    } ;
            poss2 = table {
                      Nom => table {
                               Sg => f25 ;
                               Pl => f26
                             } ;
                      Dat => table {
                               Sg => f27 ;
                               Pl => f28
                             } ;
                      Ablat => table {
                                 Sg => f29 ;
                                 Pl => f30
                               } ;
                      Instr => table {
                                 Sg => f31 ;
                                 Pl => f32
                               } ;
                      Loc => table {
                               Sg => f33 ;
                               Pl => f34
                             }
                    }
          } ;


oper Compl = {s : Str; c : Case} ;
oper noPrep : Compl = {s=""; c=Dat} ;

}
