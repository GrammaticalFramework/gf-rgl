resource ResKaz = {

oper Compl = {s : Str} ;

param Case = Acc | Nom | Instr | Ablat | Gen | Loc | Dat ;
param Number = Sg | Pl ;
param Possession = Poss3Pl | Poss2Pl Formality | Poss1Pl | Poss2Sg Formality | Poss3Sg | Poss1Sg ;
param Formality = Formal | Informal ;
oper Noun = {s: Case => Number => Str; poss: Possession => Number => Str} ; -- 1651
oper mkNoun : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Noun =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24,f25,f26,f27,f28,f29,f30 ->
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
                  Ablat => table {
                             Sg => f9 ;
                             Pl => f10
                           } ;
                  Instr => table {
                             Sg => f11 ;
                             Pl => f12
                           } ;
                  Loc => table {
                           Sg => f13 ;
                           Pl => f14
                         }
                } ;
            poss = table {
                     Poss1Pl => table {
                                  Sg => f15 ;
                                  Pl => f16
                                } ;
                     Poss1Sg => table {
                                  Sg => f17 ;
                                  Pl => f18
                                } ;
                     Poss2Sg Informal => table {
                                           Sg => f19 ;
                                           Pl => f20
                                         } ;
                     Poss2Sg Formal => table {
                                         Sg => f21 ;
                                         Pl => f22
                                       } ;
                     Poss2Pl Informal => table {
                                           Sg => f23 ;
                                           Pl => f24
                                         } ;
                     Poss2Pl Formal => table {
                                         Sg => f25 ;
                                         Pl => f26
                                       } ;
                     Poss3Sg => table {
                                  Sg => f27 ;
                                  Pl => f28
                                } ;
                     Poss3Pl => table {
                                  Sg => f29 ;
                                  Pl => f30
                                }
                   }
          } ;


param Polarity = Neg | Pos ;
param Number = Sg | Pl ;
param Formality = Formal | Informal ;
oper Verb = {Infinitive: Str; Indicative: {Fut: Str; Pres: {Progressive: Polarity => {P1: {noFormality: Number => Str}; P2: Number => Str; P3: {noFormality: Number => Str}}; noAspect: Polarity => {P1: {noFormality: Number => Str}; P2: Number => Str; P3: {noFormality: Number => Str}}}; Past: {Perfect: Polarity => {P1: {noFormality: Number => Str}; P2: Number => Str; P3: {noFormality: Number => Str}}; Progressive: Polarity => {P1: {noFormality: Number => Str}; P2: Number => Str; P3: {noFormality: Number => Str}}; noAspect: Polarity => {P1: {noFormality: Number => Str}; P2: Number => Str; P3: {noFormality: Number => Str}}}}; Imperative_Jussive: Polarity => Formality => Number => Str; Subjunctive: {P1: {noFormality: Number => Str}; P2: Number => Str; P3: {noFormality: Number => Str}}} ; -- 113
oper mkVerb : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Verb =
       \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15,f16,f17,f18,f19,f20,f21,f22,f23,f24,f25,f26,f27,f28,f29,f30,f31,f32,f33,f34,f35,f36,f37,f38,f39,f40,f41,f42,f43,f44,f45,f46,f47,f48,f49,f50,f51,f52,f53,f54,f55,f56,f57,f58,f59,f60,f61,f62,f63,f64,f65,f66,f67,f68,f69,f70,f71,f72,f73,f74,f75,f76,f77,f78,f79,f80,f81,f82,f83,f84,f85,f86,f87,f88,f89,f90,f91,f92,f93,f94,f95,f96,f97,f98 ->
          { Infinitive = f1 ;
            Indicative = { Fut = f2 ;
                           Pres = { Progressive = table {
                                                    Pos => { P1 = { noFormality = table {
                                                                                    Sg => f3 ;
                                                                                    Pl => f4
                                                                                  }
                                                                  } ;
                                                             P2 = table {
                                                                    Sg => f5 ;
                                                                    Pl => f6
                                                                  } ;
                                                             P3 = { noFormality = table {
                                                                                    Sg => f7 ;
                                                                                    Pl => f8
                                                                                  }
                                                                  }
                                                           } ;
                                                    Neg => { P1 = { noFormality = table {
                                                                                    Sg => f9 ;
                                                                                    Pl => f10
                                                                                  }
                                                                  } ;
                                                             P2 = table {
                                                                    Sg => f11 ;
                                                                    Pl => f12
                                                                  } ;
                                                             P3 = { noFormality = table {
                                                                                    Sg => f13 ;
                                                                                    Pl => f14
                                                                                  }
                                                                  }
                                                           }
                                                  } ;
                                    noAspect = table {
                                                 Pos => { P1 = { noFormality = table {
                                                                                 Sg => f15 ;
                                                                                 Pl => f16
                                                                               }
                                                               } ;
                                                          P2 = table {
                                                                 Sg => f17 ;
                                                                 Pl => f18
                                                               } ;
                                                          P3 = { noFormality = table {
                                                                                 Sg => f19 ;
                                                                                 Pl => f20
                                                                               }
                                                               }
                                                        } ;
                                                 Neg => { P1 = { noFormality = table {
                                                                                 Sg => f21 ;
                                                                                 Pl => f22
                                                                               }
                                                               } ;
                                                          P2 = table {
                                                                 Sg => f23 ;
                                                                 Pl => f24
                                                               } ;
                                                          P3 = { noFormality = table {
                                                                                 Sg => f25 ;
                                                                                 Pl => f26
                                                                               }
                                                               }
                                                        }
                                               }
                                  } ;
                           Past = { Perfect = table {
                                                Pos => { P1 = { noFormality = table {
                                                                                Sg => f27 ;
                                                                                Pl => f28
                                                                              }
                                                              } ;
                                                         P2 = table {
                                                                Sg => f29 ;
                                                                Pl => f30
                                                              } ;
                                                         P3 = { noFormality = table {
                                                                                Sg => f31 ;
                                                                                Pl => f32
                                                                              }
                                                              }
                                                       } ;
                                                Neg => { P1 = { noFormality = table {
                                                                                Sg => f33 ;
                                                                                Pl => f34
                                                                              }
                                                              } ;
                                                         P2 = table {
                                                                Sg => f35 ;
                                                                Pl => f36
                                                              } ;
                                                         P3 = { noFormality = table {
                                                                                Sg => f37 ;
                                                                                Pl => f38
                                                                              }
                                                              }
                                                       }
                                              } ;
                                    Progressive = table {
                                                    Pos => { P1 = { noFormality = table {
                                                                                    Sg => f39 ;
                                                                                    Pl => f40
                                                                                  }
                                                                  } ;
                                                             P2 = table {
                                                                    Sg => f41 ;
                                                                    Pl => f42
                                                                  } ;
                                                             P3 = { noFormality = table {
                                                                                    Sg => f43 ;
                                                                                    Pl => f44
                                                                                  }
                                                                  }
                                                           } ;
                                                    Neg => { P1 = { noFormality = table {
                                                                                    Sg => f45 ;
                                                                                    Pl => f46
                                                                                  }
                                                                  } ;
                                                             P2 = table {
                                                                    Sg => f47 ;
                                                                    Pl => f48
                                                                  } ;
                                                             P3 = { noFormality = table {
                                                                                    Sg => f49 ;
                                                                                    Pl => f50
                                                                                  }
                                                                  }
                                                           }
                                                  } ;
                                    noAspect = table {
                                                 Pos => { P1 = { noFormality = table {
                                                                                 Sg => f51 ;
                                                                                 Pl => f52
                                                                               }
                                                               } ;
                                                          P2 = table {
                                                                 Sg => f53 ;
                                                                 Pl => f54
                                                               } ;
                                                          P3 = { noFormality = table {
                                                                                 Sg => f55 ;
                                                                                 Pl => f56
                                                                               }
                                                               }
                                                        } ;
                                                 Neg => { P1 = { noFormality = table {
                                                                                 Sg => f57 ;
                                                                                 Pl => f58
                                                                               }
                                                               } ;
                                                          P2 = table {
                                                                 Sg => f59 ;
                                                                 Pl => f60
                                                               } ;
                                                          P3 = { noFormality = table {
                                                                                 Sg => f61 ;
                                                                                 Pl => f62
                                                                               }
                                                               }
                                                        }
                                               }
                                  }
                         } ;
            Imperative_Jussive = table {
                                   Pos => table {
                                            Informal => table {
                                                          Sg => f63 ;
                                                          Pl => f64
                                                        } ;
                                            Formal => table {
                                                        Sg => f65 ;
                                                        Pl => f66
                                                      }
                                          } ;
                                   Neg => table {
                                            Informal => table {
                                                          Sg => f67 ;
                                                          Pl => f68
                                                        } ;
                                            Formal => table {
                                                        Sg => f69 ;
                                                        Pl => f70
                                                      }
                                          }
                                 } ;
            Subjunctive = { P1 = { noFormality = table {
                                                   Sg => f71 ;
                                                   Pl => f72
                                                 }
                                 } ;
                            P2 = table {
                                   Sg => f73 ;
                                   Pl => f74
                                 } ;
                            P3 = { noFormality = table {
                                                   Sg => f75 ;
                                                   Pl => f76
                                                 }
                                 }
                          }
          } ;



}
