resource MorphoKaz = open CatKaz, ResKaz, Predef in {

oper

mkN001 : Str -> N ;
mkN001 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"лар" --guessed
                 } ;
          Acc => table {
                   Sg => base_1+"ты" ;
                   Pl => base_1+"лерді" --guessed
                 } ;
          Dat => table {
                   Sg => base_1+"қа" ;
                   Pl => base_1+"ларға" --guessed
                 } ;
          Gen => table {
                   Sg => base_1+"тың" ;
                   Pl => base_1+"лардың" --guessed
                 } ;
          Ablat => table {
                     Sg => base_1+"тан" ;
                     Pl => base_1+"дардан" --guessed
                   } ;
          Instr => table {
                     Sg => base_1+"пен" ;
                     Pl => base_1+"дармен" --guessed
                   } ;
          Loc => table {
                   Sg => base_1+"та" ;
                   Pl => base_1+"лерде" --guessed
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"іміз" ; --guessed
                          Pl => base_1+"ларымыз" --guessed
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ім" ; --guessed
                          Pl => base_1+"ларым" --guessed
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"іңіз" ; --guessed
                                   Pl => base_1+"ларыңыз" --guessed
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ің" ; --guessed
                                 Pl => base_1+"ларың" --guessed
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"і" ; --guessed
                          Pl => base_1+"дары" --guessed
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN002 : Str -> N ;
mkN002 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"дар"
                 } ;
          Acc => table {
                   Sg => base_1+"ды" ;
                   Pl => base_1+"дарды"
                 } ;
          Dat => table {
                   Sg => base_1+"ға" ;
                   Pl => base_1+"дарға"
                 } ;
          Gen => table {
                   Sg => base_1+"ның" ;
                   Pl => base_1+"дардың"
                 } ;
          Ablat => table {
                     Sg => base_1+"нан" ;
                     Pl => base_1+"дардан"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"дармен"
                   } ;
          Loc => table {
                   Sg => base_1+"да" ;
                   Pl => base_1+"дарда"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"ымыз" ;
                          Pl => base_1+"дарымыз"
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ым" ;
                          Pl => base_1+"дарым"
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"ыңыз" ;
                                   Pl => base_1+"дарыңыз"
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ың" ;
                                 Pl => base_1+"дарың"
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"ы" ;
                          Pl => base_1+"дары"
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN003 : Str -> N ;
mkN003 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"лар"
                 } ;
          Acc => table {
                   Sg => base_1+"да" ;
                   Pl => base_1+"ларда"
                 } ;
          Dat => table {
                   Sg => base_1+"ға" ;
                   Pl => base_1+"ларға"
                 } ;
          Gen => table {
                   Sg => base_1+"ның" ;
                   Pl => base_1+"лардың"
                 } ;
          Ablat => table {
                     Sg => base_1+"дан" ;
                     Pl => base_1+"лардан"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"лармен"
                   } ;
          Loc => table {
                   Sg => base_1+"ны" ;
                   Pl => base_1+"ларды"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"мыз" ;
                          Pl => base_1+"ларымыз"
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"м" ;
                          Pl => base_1+"ларым"
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"ңыз" ;
                                   Pl => base_1+"ларыңыз"
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ң" ;
                                 Pl => base_1+"ларың"
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"сы" ;
                          Pl => base_1+"лары"
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN004 : Str -> N ;
mkN004 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"лар" --guessed
                 } ;
          Acc => table {
                   Sg => base_1+"ды" ;
                   Pl => base_1+"дарды" --guessed
                 } ;
          Dat => table {
                   Sg => base_1+"ға" ;
                   Pl => base_1+"ларға" --guessed
                 } ;
          Gen => table {
                   Sg => base_1+"ның" ;
                   Pl => base_1+"лардың" --guessed
                 } ;
          Ablat => table {
                     Sg => base_1+"дан" ;
                     Pl => base_1+"дардан" --guessed
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"дармен" --guessed
                   } ;
          Loc => table {
                   Sg => base_1+"да" ;
                   Pl => base_1+"дарда" --guessed
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"іміз" ; --guessed
                          Pl => base_1+"ларымыз" --guessed
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ым" ; --guessed
                          Pl => base_1+"ларым" --guessed
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"ыңыз" ; --guessed
                                   Pl => base_1+"ларыңыз" --guessed
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ың" ; --guessed
                                 Pl => base_1+"ларың" --guessed
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"ы" ; --guessed
                          Pl => base_1+"дары" --guessed
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN005 : Str -> N ;
mkN005 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"дер"
                 } ;
          Acc => table {
                   Sg => base_1+"ді" ;
                   Pl => base_1+"дерді"
                 } ;
          Dat => table {
                   Sg => base_1+"ге" ;
                   Pl => base_1+"дерке"
                 } ;
          Gen => table {
                   Sg => base_1+"нің" ;
                   Pl => base_1+"дердің"
                 } ;
          Ablat => table {
                     Sg => base_1+"ден" ;
                     Pl => base_1+"дерден"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"дермен"
                   } ;
          Loc => table {
                   Sg => base_1+"де" ;
                   Pl => base_1+"дерде"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"іміз" ;
                          Pl => base_1+"деріміз"
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ім" ;
                          Pl => base_1+"дерім"
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"іңіз" ;
                                   Pl => base_1+"деріңіз"
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ің" ;
                                 Pl => base_1+"дерің"
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"і" ;
                          Pl => base_1+"дері"
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN006 : Str -> N ;
mkN006 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"тар"
                 } ;
          Acc => table {
                   Sg => base_1+"та" ;
                   Pl => base_1+"тарда"
                 } ;
          Dat => table {
                   Sg => base_1+"қа" ;
                   Pl => base_1+"тарға"
                 } ;
          Gen => table {
                   Sg => base_1+"тың" ;
                   Pl => base_1+"тардың"
                 } ;
          Ablat => table {
                     Sg => base_1+"тан" ;
                     Pl => base_1+"тардан"
                   } ;
          Instr => table {
                     Sg => base_1+"пен" ;
                     Pl => base_1+"тармен"
                   } ;
          Loc => table {
                   Sg => base_1+"ты" ;
                   Pl => base_1+"тарды"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"іміз" ; --guessed
                          Pl => base_1+"ларымыз" --guessed
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ім" ; --guessed
                          Pl => base_1+"ларым" --guessed
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"іңіз" ; --guessed
                                   Pl => base_1+"ларыңыз" --guessed
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ің" ; --guessed
                                 Pl => base_1+"ларың" --guessed
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"і" ; --guessed
                          Pl => base_1+"дары" --guessed
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN007 : Str -> N ;
mkN007 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"тер"
                 } ;
          Acc => table {
                   Sg => base_1+"ті" ;
                   Pl => base_1+"терді"
                 } ;
          Dat => table {
                   Sg => base_1+"ке" ;
                   Pl => base_1+"терге"
                 } ;
          Gen => table {
                   Sg => base_1+"тің" ;
                   Pl => base_1+"тердің"
                 } ;
          Ablat => table {
                     Sg => base_1+"тен" ;
                     Pl => base_1+"терден"
                   } ;
          Instr => table {
                     Sg => base_1+"пен" ;
                     Pl => base_1+"термен"
                   } ;
          Loc => table {
                   Sg => base_1+"те" ;
                   Pl => base_1+"терде"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"іміз" ;
                          Pl => base_1+"теріміз"
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ім" ;
                          Pl => base_1+"терім"
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"іңіз" ;
                                   Pl => base_1+"теріңіз"
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ің" ;
                                 Pl => base_1+"терің"
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"і" ;
                          Pl => base_1+"тері"
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN008 : Str -> N ;
mkN008 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"лар" --guessed
                 } ;
          Acc => table {
                   Sg => base_1+"ді" ;
                   Pl => base_1+"лерді" --guessed
                 } ;
          Dat => table {
                   Sg => base_1+"ге" ;
                   Pl => base_1+"лерге" --guessed
                 } ;
          Gen => table {
                   Sg => base_1+"дің" ;
                   Pl => base_1+"лердің" --guessed
                 } ;
          Ablat => table {
                     Sg => base_1+"ден" ;
                     Pl => base_1+"лерден" --guessed
                   } ;
          Instr => table {
                     Sg => base_1+"бен" ;
                     Pl => base_1+"лермен" --guessed
                   } ;
          Loc => table {
                   Sg => base_1+"де" ;
                   Pl => base_1+"лерде" --guessed
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"іміз" ; --guessed
                          Pl => base_1+"деріміз" --guessed
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ім" ; --guessed
                          Pl => base_1+"дерім" --guessed
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"іңіз" ; --guessed
                                   Pl => base_1+"деріңіз" --guessed
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ің" ; --guessed
                                 Pl => base_1+"дерің" --guessed
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"і" ; --guessed
                          Pl => base_1+"дері" --guessed
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN009 : Str -> N ;
mkN009 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"дар"
                 } ;
          Acc => table {
                   Sg => base_1+"да" ;
                   Pl => base_1+"дарда"
                 } ;
          Dat => table {
                   Sg => base_1+"ға" ;
                   Pl => base_1+"дарға"
                 } ;
          Gen => table {
                   Sg => base_1+"ның" ;
                   Pl => base_1+"дардың"
                 } ;
          Ablat => table {
                     Sg => base_1+"нан" ;
                     Pl => base_1+"дардан"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"дармен"
                   } ;
          Loc => table {
                   Sg => base_1+"ды" ;
                   Pl => base_1+"дарды"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"ымыз" ; --guessed
                          Pl => base_1+"ларымыз" --guessed
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ым" ; --guessed
                          Pl => base_1+"дарым" --guessed
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"ыңыз" ; --guessed
                                   Pl => base_1+"дарыңыз" --guessed
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ың" ; --guessed
                                 Pl => base_1+"дарың" --guessed
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"ы" ; --guessed
                          Pl => base_1+"дары" --guessed
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN010 : Str -> N ;
mkN010 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"лар"
                 } ;
          Acc => table {
                   Sg => base_1+"ды" ;
                   Pl => base_1+"ларды"
                 } ;
          Dat => table {
                   Sg => base_1+"ға" ;
                   Pl => base_1+"ларға"
                 } ;
          Gen => table {
                   Sg => base_1+"дың" ;
                   Pl => base_1+"лардың"
                 } ;
          Ablat => table {
                     Sg => base_1+"дан" ;
                     Pl => base_1+"лардан"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"лармен"
                   } ;
          Loc => table {
                   Sg => base_1+"да" ;
                   Pl => base_1+"ларда"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"ымыз" ;
                          Pl => base_1+"ларымыз"
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ым" ;
                          Pl => base_1+"ларым"
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"ыңыз" ;
                                   Pl => base_1+"ларыңыз"
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ың" ;
                                 Pl => base_1+"ларың"
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"ы" ;
                          Pl => base_1+"лары"
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN011 : Str -> N ;
mkN011 base =
  lin N
  { s = table {
          Nom => table {
                   Sg => base ;
                   Pl => nonExist
                 } ;
          Acc => table {
                   Sg => base+"ді" ;
                   Pl => nonExist
                 } ;
          Dat => table {
                   Sg => base+"ге" ;
                   Pl => nonExist
                 } ;
          Gen => table {
                   Sg => base+"дің" ;
                   Pl => nonExist
                 } ;
          Ablat => table {
                     Sg => base+"ден" ;
                     Pl => nonExist
                   } ;
          Instr => table {
                     Sg => base+"мен" ;
                     Pl => nonExist
                   } ;
          Loc => table {
                   Sg => base+"де" ;
                   Pl => nonExist
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => nonExist ;
                          Pl => nonExist
                        } ;
             Poss1Sg => table {
                          Sg => nonExist ;
                          Pl => nonExist
                        } ;
             Poss2Sg Informal => table {
                                   Sg => nonExist ;
                                   Pl => nonExist
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => nonExist ;
                                 Pl => nonExist
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => nonExist ;
                          Pl => nonExist
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN012 : Str -> N ;
mkN012 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"дар"
                 } ;
          Acc => table {
                   Sg => base_1+"ды" ;
                   Pl => base_1+"дарды"
                 } ;
          Dat => table {
                   Sg => base_1+"ға" ;
                   Pl => base_1+"дарға"
                 } ;
          Gen => table {
                   Sg => base_1+"дың" ;
                   Pl => base_1+"дардың"
                 } ;
          Ablat => table {
                     Sg => base_1+"дан" ;
                     Pl => base_1+"дардан"
                   } ;
          Instr => table {
                     Sg => base_1+"бен" ;
                     Pl => base_1+"дармен"
                   } ;
          Loc => table {
                   Sg => base_1+"да" ;
                   Pl => base_1+"дарда"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"ымыз" ;
                          Pl => base_1+"дарымыз"
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ым" ;
                          Pl => base_1+"дарым"
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"ыңыз" ;
                                   Pl => base_1+"дарыңыз"
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ың" ;
                                 Pl => base_1+"дарың"
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"ы" ;
                          Pl => base_1+"дары"
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN013 : Str -> N ;
mkN013 base =
  case base of {
    base_1+"қ" => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"қ" ;
                       Pl => base_1+"қтар"
                     } ;
              Acc => table {
                       Sg => base_1+"қты" ;
                       Pl => base_1+"қтарды"
                     } ;
              Dat => table {
                       Sg => base_1+"ққа" ;
                       Pl => base_1+"қтарға"
                     } ;
              Gen => table {
                       Sg => base_1+"қтың" ;
                       Pl => base_1+"қтардың"
                     } ;
              Ablat => table {
                         Sg => base_1+"қтан" ;
                         Pl => base_1+"қтардан"
                       } ;
              Instr => table {
                         Sg => base_1+"қпен" ;
                         Pl => base_1+"қтармен"
                       } ;
              Loc => table {
                       Sg => base_1+"қта" ;
                       Pl => base_1+"қтарда"
                     }
            } ;
        poss = table {
                 Poss1Pl => table {
                              Sg => base_1+"ғымыз" ;
                              Pl => base_1+"қтарымыз"
                            } ;
                 Poss1Sg => table {
                              Sg => base_1+"ғым" ;
                              Pl => base_1+"қтарым"
                            } ;
                 Poss2Sg Informal => table {
                                       Sg => base_1+"ғыңыз" ;
                                       Pl => base_1+"қтарыңыз"
                                     } ;
                 Poss2Sg Formal => table {
                                     Sg => base_1+"ғың" ;
                                     Pl => base_1+"қтарың"
                                   } ;
                 Poss2Pl Informal => table {
                                       Sg => nonExist ; --guessed
                                       Pl => nonExist --guessed
                                     } ;
                 Poss2Pl Formal => table {
                                     Sg => nonExist ; --guessed
                                     Pl => nonExist --guessed
                                   } ;
                 Poss3Sg => table {
                              Sg => base_1+"ғы" ;
                              Pl => base_1+"қтары"
                            } ;
                 Poss3Pl => table {
                              Sg => nonExist ; --guessed
                              Pl => nonExist --guessed
                            }
               }
      };
    _ => error "Can't apply paradigm mkN013"
  } ;

mkN014 : Str -> N ;
mkN014 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"дер"
                 } ;
          Acc => table {
                   Sg => base_1+"ді" ;
                   Pl => base_1+"дерді"
                 } ;
          Dat => table {
                   Sg => base_1+"ге" ;
                   Pl => base_1+"дерге"
                 } ;
          Gen => table {
                   Sg => base_1+"нің" ;
                   Pl => base_1+"дердің"
                 } ;
          Ablat => table {
                     Sg => base_1+"нен" ;
                     Pl => base_1+"дерден"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"дермен"
                   } ;
          Loc => table {
                   Sg => base_1+"де" ;
                   Pl => base_1+"дерде"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"іміз" ; --guessed
                          Pl => base_1+"деріміз" --guessed
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ім" ; --guessed
                          Pl => base_1+"дерім" --guessed
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"іңіз" ; --guessed
                                   Pl => base_1+"деріңіз" --guessed
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ің" ; --guessed
                                 Pl => base_1+"дерің" --guessed
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"і" ; --guessed
                          Pl => base_1+"дері" --guessed
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN015 : Str -> N ;
mkN015 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"дар"
                 } ;
          Acc => table {
                   Sg => base_1+"ды" ;
                   Pl => base_1+"дарды"
                 } ;
          Dat => table {
                   Sg => base_1+"ға" ;
                   Pl => base_1+"дарға"
                 } ;
          Gen => table {
                   Sg => base_1+"дың" ;
                   Pl => base_1+"дардың"
                 } ;
          Ablat => table {
                     Sg => base_1+"дан" ;
                     Pl => base_1+"дардан"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"дармен"
                   } ;
          Loc => table {
                   Sg => base_1+"да" ;
                   Pl => base_1+"дарда"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"ымыз" ;
                          Pl => base_1+"дарымыз"
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ым" ;
                          Pl => base_1+"дарым"
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"ыңыз" ;
                                   Pl => base_1+"дарыңыз"
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ың" ;
                                 Pl => base_1+"дарың"
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"ы" ;
                          Pl => base_1+"дары"
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN016 : Str -> N ;
mkN016 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"дер"
                 } ;
          Acc => table {
                   Sg => base_1+"ді" ;
                   Pl => base_1+"дерді"
                 } ;
          Dat => table {
                   Sg => base_1+"ге" ;
                   Pl => base_1+"дерге"
                 } ;
          Gen => table {
                   Sg => base_1+"дің" ;
                   Pl => base_1+"дердің"
                 } ;
          Ablat => table {
                     Sg => base_1+"ден" ;
                     Pl => base_1+"дерден"
                   } ;
          Instr => table {
                     Sg => base_1+"бен" ;
                     Pl => base_1+"дермен"
                   } ;
          Loc => table {
                   Sg => base_1+"де" ;
                   Pl => base_1+"дерде"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"іміз" ; --guessed
                          Pl => base_1+"деріміз" --guessed
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ім" ; --guessed
                          Pl => base_1+"дерім" --guessed
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"іңіз" ; --guessed
                                   Pl => base_1+"деріңіз" --guessed
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ің" ; --guessed
                                 Pl => base_1+"дерің" --guessed
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"і" ; --guessed
                          Pl => base_1+"дері" --guessed
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN017 : Str -> N ;
mkN017 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"лар"
                 } ;
          Acc => table {
                   Sg => base_1+"ны" ;
                   Pl => base_1+"ларды"
                 } ;
          Dat => table {
                   Sg => base_1+"ға" ;
                   Pl => base_1+"ларға"
                 } ;
          Gen => table {
                   Sg => base_1+"ның" ;
                   Pl => base_1+"лардың"
                 } ;
          Ablat => table {
                     Sg => base_1+"дан" ;
                     Pl => base_1+"лардан"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"лармен"
                   } ;
          Loc => table {
                   Sg => base_1+"да" ;
                   Pl => base_1+"ларда"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"ымыз" ; --guessed
                          Pl => base_1+"ларымыз" --guessed
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ым" ; --guessed
                          Pl => base_1+"ларым" --guessed
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"ыңыз" ; --guessed
                                   Pl => base_1+"ларыңыз" --guessed
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ың" ; --guessed
                                 Pl => base_1+"ларың" --guessed
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"ы" ; --guessed
                          Pl => base_1+"лары" --guessed
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN018 : Str -> N ;
mkN018 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"дар"
                 } ;
          Acc => table {
                   Sg => base_1+"ды" ;
                   Pl => base_1+"дарды"
                 } ;
          Dat => table {
                   Sg => base_1+"ға" ;
                   Pl => base_1+"дарға"
                 } ;
          Gen => table {
                   Sg => base_1+"дың" ;
                   Pl => base_1+"дардың"
                 } ;
          Ablat => table {
                     Sg => base_1+"дан" ;
                     Pl => base_1+"дардан"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"дармен"
                   } ;
          Loc => table {
                   Sg => base_1+"да" ;
                   Pl => base_1+"дарда"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"ымыз" ;
                          Pl => base_1+"дар"
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ым" ;
                          Pl => base_1+"дарым"
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"ыңыз" ;
                                   Pl => base_1+"дарыңыз"
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ың" ;
                                 Pl => base_1+"дарың"
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"ы" ;
                          Pl => base_1+"дары"
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN019 : Str -> N ;
mkN019 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"лер"
                 } ;
          Acc => table {
                   Sg => base_1+"ні" ;
                   Pl => base_1+"лерді"
                 } ;
          Dat => table {
                   Sg => base_1+"ге" ;
                   Pl => base_1+"лерге"
                 } ;
          Gen => table {
                   Sg => base_1+"нің" ;
                   Pl => base_1+"лердің"
                 } ;
          Ablat => table {
                     Sg => base_1+"ден" ;
                     Pl => base_1+"лерден"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"лермен"
                   } ;
          Loc => table {
                   Sg => base_1+"де" ;
                   Pl => base_1+"лерде"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"міз" ;
                          Pl => base_1+"леріміз"
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"м" ;
                          Pl => base_1+"лерім"
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"ңіз" ;
                                   Pl => base_1+"лерңіз"
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ң" ;
                                 Pl => base_1+"лерің"
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"сі" ;
                          Pl => base_1+"лерсі"
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN020 : Str -> N ;
mkN020 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"лер"
                 } ;
          Acc => table {
                   Sg => base_1+"ні" ;
                   Pl => base_1+"лерді"
                 } ;
          Dat => table {
                   Sg => base_1+"ге" ;
                   Pl => base_1+"лерге"
                 } ;
          Gen => table {
                   Sg => base_1+"нің" ;
                   Pl => base_1+"лердің"
                 } ;
          Ablat => table {
                     Sg => base_1+"ден" ;
                     Pl => base_1+"лерден"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"лермен"
                   } ;
          Loc => table {
                   Sg => base_1+"де" ;
                   Pl => base_1+"лерде"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"міз" ;
                          Pl => base_1+"леріміз"
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"м" ;
                          Pl => base_1+"лерім"
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"ңіз" ;
                                   Pl => base_1+"леріңіз"
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ң" ;
                                 Pl => base_1+"лерің"
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"сі" ;
                          Pl => base_1+"лерсі"
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN021 : Str -> N ;
mkN021 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"лер"
                 } ;
          Acc => table {
                   Sg => base_1+"ні" ;
                   Pl => base_1+"лерді"
                 } ;
          Dat => table {
                   Sg => base_1+"ге" ;
                   Pl => base_1+"лерге"
                 } ;
          Gen => table {
                   Sg => base_1+"нің" ;
                   Pl => base_1+"лердің"
                 } ;
          Ablat => table {
                     Sg => base_1+"ден" ;
                     Pl => base_1+"лерден"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"лермен"
                   } ;
          Loc => table {
                   Sg => base_1+"де" ;
                   Pl => base_1+"лерде"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"міз" ;
                          Pl => base_1+"леріміз"
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"м" ;
                          Pl => base_1+"лерім"
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"ңіз" ;
                                   Pl => base_1+"леріңіз"
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ң" ;
                                 Pl => base_1+"лерің"
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1 ;
                          Pl => base_1+"лері"
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN022 : Str -> N ;
mkN022 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"лер"
                 } ;
          Acc => table {
                   Sg => base_1+"ні" ;
                   Pl => base_1+"лерді"
                 } ;
          Dat => table {
                   Sg => base_1+"ге" ;
                   Pl => base_1+"лерге"
                 } ;
          Gen => table {
                   Sg => base_1+"нің" ;
                   Pl => base_1+"лердің"
                 } ;
          Ablat => table {
                     Sg => base_1+"ден" ;
                     Pl => base_1+"лерден"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"лермен"
                   } ;
          Loc => table {
                   Sg => base_1+"де" ;
                   Pl => base_1+"лерде"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"міз" ;
                          Pl => base_1+"леріміз"
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"м" ;
                          Pl => base_1+"лерім"
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"ңіз" ;
                                   Pl => base_1+"леріңіз"
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ң" ;
                                 Pl => base_1+"лерің"
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"сі" ;
                          Pl => base_1+"лері"
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN023 : Str -> N ;
mkN023 base =
  case base of {
    base_1+"к" => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"к" ;
                       Pl => base_1+"ктер"
                     } ;
              Acc => table {
                       Sg => base_1+"кті" ;
                       Pl => base_1+"ктерді"
                     } ;
              Dat => table {
                       Sg => base_1+"кке" ;
                       Pl => base_1+"ктерге"
                     } ;
              Gen => table {
                       Sg => base_1+"ктің" ;
                       Pl => base_1+"ктердің"
                     } ;
              Ablat => table {
                         Sg => base_1+"ктен" ;
                         Pl => base_1+"ктерден"
                       } ;
              Instr => table {
                         Sg => base_1+"кпен" ;
                         Pl => base_1+"ктермен"
                       } ;
              Loc => table {
                       Sg => base_1+"кте" ;
                       Pl => base_1+"ктерде"
                     }
            } ;
        poss = table {
                 Poss1Pl => table {
                              Sg => base_1+"гіміз" ;
                              Pl => base_1+"ктеріміз"
                            } ;
                 Poss1Sg => table {
                              Sg => base_1+"гім" ;
                              Pl => base_1+"ктерім"
                            } ;
                 Poss2Sg Informal => table {
                                       Sg => base_1+"гіңіз" ;
                                       Pl => base_1+"ктеріңіз"
                                     } ;
                 Poss2Sg Formal => table {
                                     Sg => base_1+"гің" ;
                                     Pl => base_1+"ктерің"
                                   } ;
                 Poss2Pl Informal => table {
                                       Sg => nonExist ; --guessed
                                       Pl => nonExist --guessed
                                     } ;
                 Poss2Pl Formal => table {
                                     Sg => nonExist ; --guessed
                                     Pl => nonExist --guessed
                                   } ;
                 Poss3Sg => table {
                              Sg => base_1+"гі" ;
                              Pl => base_1+"ктері"
                            } ;
                 Poss3Pl => table {
                              Sg => nonExist ; --guessed
                              Pl => nonExist --guessed
                            }
               }
      };
    _ => error "Can't apply paradigm mkN023"
  } ;

mkN024 : Str -> N ;
mkN024 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"лер"
                 } ;
          Acc => table {
                   Sg => base_1+"ді" ;
                   Pl => base_1+"лерді"
                 } ;
          Dat => table {
                   Sg => base_1+"ге" ;
                   Pl => base_1+"лерге"
                 } ;
          Gen => table {
                   Sg => base_1+"дің" ;
                   Pl => base_1+"лердің"
                 } ;
          Ablat => table {
                     Sg => base_1+"ден" ;
                     Pl => base_1+"лерден"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"лермен"
                   } ;
          Loc => table {
                   Sg => base_1+"де" ;
                   Pl => base_1+"лерде"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"іміз" ; --guessed
                          Pl => base_1+"леріміз" --guessed
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ім" ; --guessed
                          Pl => base_1+"лерім" --guessed
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"іңіз" ; --guessed
                                   Pl => base_1+"деріңіз" --guessed
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ің" ; --guessed
                                 Pl => base_1+"лерің" --guessed
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"і" ; --guessed
                          Pl => base_1+"дері" --guessed
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN025 : Str -> N ;
mkN025 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"дер"
                 } ;
          Acc => table {
                   Sg => base_1+"ді" ;
                   Pl => base_1+"дерді"
                 } ;
          Dat => table {
                   Sg => base_1+"ге" ;
                   Pl => base_1+"дерге"
                 } ;
          Gen => table {
                   Sg => base_1+"дің" ;
                   Pl => base_1+"дердің"
                 } ;
          Ablat => table {
                     Sg => base_1+"ден" ;
                     Pl => base_1+"дерден"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"дермен"
                   } ;
          Loc => table {
                   Sg => base_1+"де" ;
                   Pl => base_1+"дерде"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"іміз" ;
                          Pl => base_1+"деріміз"
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ім" ;
                          Pl => base_1+"дерім"
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"іңіз" ;
                                   Pl => base_1+"деріңіз"
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ің" ;
                                 Pl => base_1+"дерің"
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"і" ;
                          Pl => base_1+"дері"
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN026 : Str -> N ;
mkN026 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"дер"
                 } ;
          Acc => table {
                   Sg => base_1+"ді" ;
                   Pl => base_1+"дерді"
                 } ;
          Dat => table {
                   Sg => base_1+"ге" ;
                   Pl => base_1+"дерге"
                 } ;
          Gen => table {
                   Sg => base_1+"дің" ;
                   Pl => base_1+"дердің"
                 } ;
          Ablat => table {
                     Sg => base_1+"ден" ;
                     Pl => base_1+"дерден"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"дермен"
                   } ;
          Loc => table {
                   Sg => "әй"+base_1+"де" ;
                   Pl => base_1+"дерде"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"іміз" ;
                          Pl => base_1+"деріміз"
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ім" ;
                          Pl => base_1+"дерім"
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"іңіз" ;
                                   Pl => base_1+"деріңіз"
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ің" ;
                                 Pl => base_1+"дерің"
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"і" ;
                          Pl => base_1+"дері"
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN027 : Str -> N ;
mkN027 base =
  case base of {
    base_1+"қ" => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"қ" ;
                       Pl => base_1+"қтар"
                     } ;
              Acc => table {
                       Sg => base_1+"қты" ;
                       Pl => base_1+"қтарды"
                     } ;
              Dat => table {
                       Sg => base_1+"ққа" ;
                       Pl => base_1+"қтарға"
                     } ;
              Gen => table {
                       Sg => base_1+"қтың" ;
                       Pl => base_1+"қтардың"
                     } ;
              Ablat => table {
                         Sg => base_1+"қтан" ;
                         Pl => base_1+"қтардан"
                       } ;
              Instr => table {
                         Sg => base_1+"қпен" ;
                         Pl => base_1+"қтармен"
                       } ;
              Loc => table {
                       Sg => base_1+"қта" ;
                       Pl => base_1+"қтарда"
                     }
            } ;
        poss = table {
                 Poss1Pl => table {
                              Sg => base_1+"ғымыз" ;
                              Pl => base_1+"қтарымыз"
                            } ;
                 Poss1Sg => table {
                              Sg => base_1+"ғым" ;
                              Pl => base_1+"қтартарым"
                            } ;
                 Poss2Sg Informal => table {
                                       Sg => base_1+"ғыңыз" ;
                                       Pl => base_1+"қтарыңыз"
                                     } ;
                 Poss2Sg Formal => table {
                                     Sg => base_1+"ғың" ;
                                     Pl => base_1+"қтарың"
                                   } ;
                 Poss2Pl Informal => table {
                                       Sg => nonExist ; --guessed
                                       Pl => nonExist --guessed
                                     } ;
                 Poss2Pl Formal => table {
                                     Sg => nonExist ; --guessed
                                     Pl => nonExist --guessed
                                   } ;
                 Poss3Sg => table {
                              Sg => base_1+"ғы" ;
                              Pl => base_1+"қтары"
                            } ;
                 Poss3Pl => table {
                              Sg => nonExist ; --guessed
                              Pl => nonExist --guessed
                            }
               }
      };
    _ => error "Can't apply paradigm mkN027"
  } ;

mkN028 : Str -> N ;
mkN028 base =
  case base of {
    base_1+"п" => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"п" ;
                       Pl => base_1+"птар"
                     } ;
              Acc => table {
                       Sg => base_1+"пты" ;
                       Pl => base_1+"птарды"
                     } ;
              Dat => table {
                       Sg => base_1+"пқа" ;
                       Pl => base_1+"птарға"
                     } ;
              Gen => table {
                       Sg => base_1+"птың" ;
                       Pl => base_1+"птардың"
                     } ;
              Ablat => table {
                         Sg => base_1+"птан" ;
                         Pl => base_1+"птардан"
                       } ;
              Instr => table {
                         Sg => base_1+"ппен" ;
                         Pl => base_1+"птармен"
                       } ;
              Loc => table {
                       Sg => base_1+"пта" ;
                       Pl => base_1+"птарда"
                     }
            } ;
        poss = table {
                 Poss1Pl => table {
                              Sg => base_1+"бымыз" ;
                              Pl => base_1+"птарымыз"
                            } ;
                 Poss1Sg => table {
                              Sg => base_1+"бым" ;
                              Pl => base_1+"птарым"
                            } ;
                 Poss2Sg Informal => table {
                                       Sg => base_1+"быңыз" ;
                                       Pl => base_1+"птарыңыз"
                                     } ;
                 Poss2Sg Formal => table {
                                     Sg => base_1+"бың" ;
                                     Pl => base_1+"птарың"
                                   } ;
                 Poss2Pl Informal => table {
                                       Sg => nonExist ; --guessed
                                       Pl => nonExist --guessed
                                     } ;
                 Poss2Pl Formal => table {
                                     Sg => nonExist ; --guessed
                                     Pl => nonExist --guessed
                                   } ;
                 Poss3Sg => table {
                              Sg => base_1+"бы" ;
                              Pl => base_1+"птары"
                            } ;
                 Poss3Pl => table {
                              Sg => nonExist ; --guessed
                              Pl => nonExist --guessed
                            }
               }
      };
    _ => error "Can't apply paradigm mkN028"
  } ;

mkN029 : Str -> N ;
mkN029 base =
  case base of {
    base_1+"е"+base_2@? => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"е"+base_2 ;
                       Pl => base_1+"е"+base_2+"дер"
                     } ;
              Acc => table {
                       Sg => base_1+"е"+base_2+"і" ;
                       Pl => base_1+"е"+base_2+"дерді"
                     } ;
              Dat => table {
                       Sg => base_1+"аға"+base_2 ;
                       Pl => base_1+"е"+base_2+"дерге"
                     } ;
              Gen => table {
                       Sg => base_1+"е"+base_2+"ің" ;
                       Pl => base_1+"е"+base_2+"дердің"
                     } ;
              Ablat => table {
                         Sg => base_1+"е"+base_2+"нен" ;
                         Pl => base_1+"е"+base_2+"дерден"
                       } ;
              Instr => table {
                         Sg => base_1+"е"+base_2+"імен" ;
                         Pl => base_1+"е"+base_2+"дермен"
                       } ;
              Loc => table {
                       Sg => base_1+"е"+base_2+"де" ;
                       Pl => base_1+"е"+base_2+"дерде"
                     }
            } ;
        poss = table {
                 Poss1Pl => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            } ;
                 Poss1Sg => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            } ;
                 Poss2Sg Informal => table {
                                       Sg => nonExist ;
                                       Pl => nonExist
                                     } ;
                 Poss2Sg Formal => table {
                                     Sg => nonExist ;
                                     Pl => nonExist
                                   } ;
                 Poss2Pl Informal => table {
                                       Sg => nonExist ; --guessed
                                       Pl => nonExist --guessed
                                     } ;
                 Poss2Pl Formal => table {
                                     Sg => nonExist ; --guessed
                                     Pl => nonExist --guessed
                                   } ;
                 Poss3Sg => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            } ;
                 Poss3Pl => table {
                              Sg => nonExist ; --guessed
                              Pl => nonExist --guessed
                            }
               }
      };
    _ => error "Can't apply paradigm mkN029"
  } ;

mkN030 : Str -> N ;
mkN030 base =
  case base of {
    base_1+"п" => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"п" ;
                       Pl => base_1+"птер"
                     } ;
              Acc => table {
                       Sg => base_1+"пті" ;
                       Pl => base_1+"птерді"
                     } ;
              Dat => table {
                       Sg => base_1+"пке" ;
                       Pl => base_1+"птерге"
                     } ;
              Gen => table {
                       Sg => base_1+"птің" ;
                       Pl => base_1+"птердің"
                     } ;
              Ablat => table {
                         Sg => base_1+"птен" ;
                         Pl => base_1+"птерден"
                       } ;
              Instr => table {
                         Sg => base_1+"ппен" ;
                         Pl => base_1+"птермен"
                       } ;
              Loc => table {
                       Sg => base_1+"пте" ;
                       Pl => base_1+"птерде"
                     }
            } ;
        poss = table {
                 Poss1Pl => table {
                              Sg => base_1+"біміз" ;
                              Pl => base_1+"птеріміз"
                            } ;
                 Poss1Sg => table {
                              Sg => base_1+"бім" ;
                              Pl => base_1+"птерім"
                            } ;
                 Poss2Sg Informal => table {
                                       Sg => base_1+"біңіз" ;
                                       Pl => base_1+"птеріңіз"
                                     } ;
                 Poss2Sg Formal => table {
                                     Sg => base_1+"бің" ;
                                     Pl => base_1+"птерің"
                                   } ;
                 Poss2Pl Informal => table {
                                       Sg => nonExist ; --guessed
                                       Pl => nonExist --guessed
                                     } ;
                 Poss2Pl Formal => table {
                                     Sg => nonExist ; --guessed
                                     Pl => nonExist --guessed
                                   } ;
                 Poss3Sg => table {
                              Sg => base_1+"бі" ;
                              Pl => base_1+"тері"
                            } ;
                 Poss3Pl => table {
                              Sg => nonExist ; --guessed
                              Pl => nonExist --guessed
                            }
               }
      };
    _ => error "Can't apply paradigm mkN030"
  } ;

mkN031 : Str -> N ;
mkN031 base =
  case base of {
    base_1+base_2@?+"п" => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+base_2+"п" ;
                       Pl => base_1+base_2+"птер"
                     } ;
              Acc => table {
                       Sg => base_1+base_2+"пті" ;
                       Pl => base_1+base_2+"птерді"
                     } ;
              Dat => table {
                       Sg => base_1+base_2+"пке" ;
                       Pl => base_1+base_2+"птерге"
                     } ;
              Gen => table {
                       Sg => base_1+base_2+"птің" ;
                       Pl => base_1+base_2+"птердің"
                     } ;
              Ablat => table {
                         Sg => base_1+base_2+"птен" ;
                         Pl => base_1+base_2+"птерден"
                       } ;
              Instr => table {
                         Sg => base_1+base_2+"ппен" ;
                         Pl => base_1+base_2+"птермен"
                       } ;
              Loc => table {
                       Sg => base_1+base_2+"пте" ;
                       Pl => base_1+base_2+"птерде"
                     }
            } ;
        poss = table {
                 Poss1Pl => table {
                              Sg => base_1+"п"+base_2+"міз" ;
                              Pl => base_1+base_2+"птеріміз"
                            } ;
                 Poss1Sg => table {
                              Sg => base_1+"п"+base_2+"м" ;
                              Pl => base_1+base_2+"птерім"
                            } ;
                 Poss2Sg Informal => table {
                                       Sg => base_1+"п"+base_2+"ңіз" ;
                                       Pl => base_1+base_2+"птеріңіз"
                                     } ;
                 Poss2Sg Formal => table {
                                     Sg => base_1+"п"+base_2+"ң" ;
                                     Pl => base_1+base_2+"птерің"
                                   } ;
                 Poss2Pl Informal => table {
                                       Sg => nonExist ; --guessed
                                       Pl => nonExist --guessed
                                     } ;
                 Poss2Pl Formal => table {
                                     Sg => nonExist ; --guessed
                                     Pl => nonExist --guessed
                                   } ;
                 Poss3Sg => table {
                              Sg => base_1+"п"+base_2 ;
                              Pl => base_1+base_2+"птері"
                            } ;
                 Poss3Pl => table {
                              Sg => nonExist ; --guessed
                              Pl => nonExist --guessed
                            }
               }
      };
    _ => error "Can't apply paradigm mkN031"
  } ;

mkN032 : Str -> N ;
mkN032 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"дар"
                 } ;
          Acc => table {
                   Sg => base_1+"ды" ;
                   Pl => base_1+"дарды"
                 } ;
          Dat => table {
                   Sg => base_1+"ға" ;
                   Pl => base_1+"дарға"
                 } ;
          Gen => table {
                   Sg => base_1+"ның" ;
                   Pl => base_1+"дардың"
                 } ;
          Ablat => table {
                     Sg => base_1+"дан" ;
                     Pl => base_1+"дардан"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"дармен"
                   } ;
          Loc => table {
                   Sg => base_1+"да" ;
                   Pl => base_1+"дарда"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"ымыз" ; --guessed
                          Pl => base_1+"ларымыз" --guessed
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ым" ; --guessed
                          Pl => base_1+"дарым" --guessed
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"ыңыз" ; --guessed
                                   Pl => base_1+"дарыңыз" --guessed
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ың" ; --guessed
                                 Pl => base_1+"дарың" --guessed
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"ы" ; --guessed
                          Pl => base_1+"дары" --guessed
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN033 : Str -> N ;
mkN033 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"тер"
                 } ;
          Acc => table {
                   Sg => base_1+"ті" ;
                   Pl => base_1+"терде"
                 } ;
          Dat => table {
                   Sg => base_1+"кеге" ;
                   Pl => base_1+"терді"
                 } ;
          Gen => table {
                   Sg => base_1+"тің" ;
                   Pl => base_1+"тердің"
                 } ;
          Ablat => table {
                     Sg => base_1+"тен" ;
                     Pl => base_1+"терден"
                   } ;
          Instr => table {
                     Sg => base_1+"пен" ;
                     Pl => base_1+"термен"
                   } ;
          Loc => table {
                   Sg => base_1+"те" ;
                   Pl => base_1+"терде"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"іміз" ;
                          Pl => base_1+"теріміз"
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ім" ;
                          Pl => base_1+"терім"
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"іңіз" ;
                                   Pl => base_1+"теріңіз"
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ің" ;
                                 Pl => base_1+"терің"
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"і" ;
                          Pl => base_1+"тері"
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN034 : Str -> N ;
mkN034 base =
  case base of {
    base_1+"ус"+base_2@(?+?+?) => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ус"+base_2 ;
                       Pl => base_1+"ус"+base_2+"лар"
                     } ;
              Acc => table {
                       Sg => base_1+"ус"+base_2+"ды" ;
                       Pl => base_1+"ус"+base_2+"ларды"
                     } ;
              Dat => table {
                       Sg => base_1+"ус"+base_2+"ға" ;
                       Pl => base_1+"ус"+base_2+"ларға"
                     } ;
              Gen => table {
                       Sg => base_1+"ус"+base_2+"дың" ;
                       Pl => base_1+base_2+"лардың"
                     } ;
              Ablat => table {
                         Sg => base_1+"ус"+base_2+"дан" ;
                         Pl => base_1+"ус"+base_2+"лардан"
                       } ;
              Instr => table {
                         Sg => base_1+"ус"+base_2+"мен" ;
                         Pl => base_1+"ус"+base_2+"лармен"
                       } ;
              Loc => table {
                       Sg => base_1+"ус"+base_2+"да" ;
                       Pl => base_1+"ус"+base_2+"ларда"
                     }
            } ;
        poss = table {
                 Poss1Pl => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            } ;
                 Poss1Sg => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            } ;
                 Poss2Sg Informal => table {
                                       Sg => nonExist ;
                                       Pl => nonExist
                                     } ;
                 Poss2Sg Formal => table {
                                     Sg => nonExist ;
                                     Pl => nonExist
                                   } ;
                 Poss2Pl Informal => table {
                                       Sg => nonExist ; --guessed
                                       Pl => nonExist --guessed
                                     } ;
                 Poss2Pl Formal => table {
                                     Sg => nonExist ; --guessed
                                     Pl => nonExist --guessed
                                   } ;
                 Poss3Sg => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            } ;
                 Poss3Pl => table {
                              Sg => nonExist ; --guessed
                              Pl => nonExist --guessed
                            }
               }
      };
    _ => error "Can't apply paradigm mkN034"
  } ;

mkN035 : Str -> N ;
mkN035 base =
  case base of {
    base_1+base_2@?+"н" => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+base_2+"н" ;
                       Pl => base_1+base_2+"ндар"
                     } ;
              Acc => table {
                       Sg => base_1+base_2+"нды" ;
                       Pl => base_1+base_2+"ндарды"
                     } ;
              Dat => table {
                       Sg => base_1+base_2+"нға" ;
                       Pl => base_1+base_2+"ндарға"
                     } ;
              Gen => table {
                       Sg => base_1+"н"+base_2+"ның" ;
                       Pl => base_1+base_2+"ндардың"
                     } ;
              Ablat => table {
                         Sg => base_1+"н"+base_2+"нан" ;
                         Pl => base_1+base_2+"ндардан"
                       } ;
              Instr => table {
                         Sg => base_1+"н"+base_2+"мен" ;
                         Pl => base_1+base_2+"ндармен"
                       } ;
              Loc => table {
                       Sg => base_1+"н"+base_2+"на" ;
                       Pl => base_1+base_2+"ндарда"
                     }
            } ;
        poss = table {
                 Poss1Pl => table {
                              Sg => base_1+"н"+base_2+"мыз" ;
                              Pl => base_1+base_2+"ндарымыз"
                            } ;
                 Poss1Sg => table {
                              Sg => base_1+"н"+base_2+"м" ;
                              Pl => base_1+base_2+"ндарым"
                            } ;
                 Poss2Sg Informal => table {
                                       Sg => base_1+"н"+base_2+"ңыз" ;
                                       Pl => base_1+base_2+"ндарыңыз"
                                     } ;
                 Poss2Sg Formal => table {
                                     Sg => base_1+"н"+base_2+"ң" ;
                                     Pl => base_1+base_2+"ндарың"
                                   } ;
                 Poss2Pl Informal => table {
                                       Sg => nonExist ; --guessed
                                       Pl => nonExist --guessed
                                     } ;
                 Poss2Pl Formal => table {
                                     Sg => nonExist ; --guessed
                                     Pl => nonExist --guessed
                                   } ;
                 Poss3Sg => table {
                              Sg => base_1+"н"+base_2 ;
                              Pl => base_1+base_2+"ндары"
                            } ;
                 Poss3Pl => table {
                              Sg => nonExist ; --guessed
                              Pl => nonExist --guessed
                            }
               }
      };
    _ => error "Can't apply paradigm mkN035"
  } ;

mkN036 : Str -> N ;
mkN036 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"тер"
                 } ;
          Acc => table {
                   Sg => base_1+"ті" ;
                   Pl => base_1+"терді"
                 } ;
          Dat => table {
                   Sg => base_1+"ке" ;
                   Pl => base_1+"терге"
                 } ;
          Gen => table {
                   Sg => base_1+"тің" ;
                   Pl => base_1+"тердің"
                 } ;
          Ablat => table {
                     Sg => base_1+"тен" ;
                     Pl => base_1+"терден"
                   } ;
          Instr => table {
                     Sg => base_1+"пен" ;
                     Pl => base_1+"термер"
                   } ;
          Loc => table {
                   Sg => base_1+"те" ;
                   Pl => base_1+"терде"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"іміз" ;
                          Pl => base_1+"теріміз"
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ім" ;
                          Pl => base_1+"терім"
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"іңіз" ;
                                   Pl => base_1+"теріңіз"
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ің" ;
                                 Pl => base_1+"терің"
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"і" ;
                          Pl => base_1+"тері"
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN037 : Str -> N ;
mkN037 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"лар" --guessed
                 } ;
          Acc => table {
                   Sg => base_1+"то" ;
                   Pl => base_1+"лерді" --guessed
                 } ;
          Dat => table {
                   Sg => base_1+"ҡа" ;
                   Pl => base_1+"ларға" --guessed
                 } ;
          Gen => table {
                   Sg => base_1+"тоң" ;
                   Pl => base_1+"лардың" --guessed
                 } ;
          Ablat => table {
                     Sg => base_1+"тан" ;
                     Pl => base_1+"дардан" --guessed
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ; --guessed
                     Pl => base_1+"дармен" --guessed
                   } ;
          Loc => table {
                   Sg => base_1+"та" ;
                   Pl => base_1+"лерде" --guessed
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"обоҙ" ;
                          Pl => base_1+"ларымыз" --guessed
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ом" ;
                          Pl => base_1+"ларым" --guessed
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"оғоҙ" ;
                                   Pl => base_1+"ларыңыз" --guessed
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"оң" ;
                                 Pl => base_1+"ларың" --guessed
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"о" ;
                          Pl => base_1+"дары" --guessed
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN038 : Str -> N ;
mkN038 base =
  case base of {
    base_1+"л" => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"л" ;
                       Pl => base_1+"лар" --guessed
                     } ;
              Acc => table {
                       Sg => base_1+"ны" ;
                       Pl => base_1+"лерді" --guessed
                     } ;
              Dat => table {
                       Sg => base_1+"ған" ;
                       Pl => base_1+"ларға" --guessed
                     } ;
              Gen => table {
                       Sg => base_1+"ның" ;
                       Pl => base_1+"лардың" --guessed
                     } ;
              Ablat => table {
                         Sg => base_1+"дан" ;
                         Pl => base_1+"лардан" --guessed
                       } ;
              Instr => table {
                         Sg => base_1+"нымен" ;
                         Pl => base_1+"лармен" --guessed
                       } ;
              Loc => table {
                       Sg => base_1+"нда" ;
                       Pl => base_1+"лерде" --guessed
                     }
            } ;
        poss = table {
                 Poss1Pl => table {
                              Sg => base_1+"іміз" ; --guessed
                              Pl => base_1+"ларымыз" --guessed
                            } ;
                 Poss1Sg => table {
                              Sg => base_1+"ім" ; --guessed
                              Pl => base_1+"ларым" --guessed
                            } ;
                 Poss2Sg Informal => table {
                                       Sg => base_1+"іңіз" ; --guessed
                                       Pl => base_1+"ларыңыз" --guessed
                                     } ;
                 Poss2Sg Formal => table {
                                     Sg => base_1+"ің" ; --guessed
                                     Pl => base_1+"ларың" --guessed
                                   } ;
                 Poss2Pl Informal => table {
                                       Sg => nonExist ; --guessed
                                       Pl => nonExist --guessed
                                     } ;
                 Poss2Pl Formal => table {
                                     Sg => nonExist ; --guessed
                                     Pl => nonExist --guessed
                                   } ;
                 Poss3Sg => table {
                              Sg => base_1+"і" ; --guessed
                              Pl => base_1+"дары" --guessed
                            } ;
                 Poss3Pl => table {
                              Sg => nonExist ; --guessed
                              Pl => nonExist --guessed
                            }
               }
      };
    _ => error "Can't apply paradigm mkN038"
  } ;

mkN039 : Str -> N ;
mkN039 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"лар" --guessed
                 } ;
          Acc => table {
                   Sg => base_1+"де" ;
                   Pl => base_1+"лерді" --guessed
                 } ;
          Dat => table {
                   Sg => base_1+"ге" ;
                   Pl => base_1+"ларға" --guessed
                 } ;
          Gen => table {
                   Sg => base_1+"дің" ;
                   Pl => base_1+"лардың" --guessed
                 } ;
          Ablat => table {
                     Sg => base_1+"ден" ;
                     Pl => base_1+"лерден" --guessed
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"лермен" --guessed
                   } ;
          Loc => table {
                   Sg => base_1+"ді" ;
                   Pl => base_1+"лерде" --guessed
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"іміз" ; --guessed
                          Pl => base_1+"ларымыз" --guessed
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ім" ; --guessed
                          Pl => base_1+"ларым" --guessed
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"іңіз" ; --guessed
                                   Pl => base_1+"ларыңыз" --guessed
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ің" ; --guessed
                                 Pl => base_1+"ларың" --guessed
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"і" ; --guessed
                          Pl => base_1+"дері" --guessed
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN040 : Str -> N ;
mkN040 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"лар" --guessed
                 } ;
          Acc => table {
                   Sg => base_1+"ді" ; --guessed
                   Pl => base_1+"лерді" --guessed
                 } ;
          Dat => table {
                   Sg => base_1+"іне" ;
                   Pl => base_1+"ларға" --guessed
                 } ;
          Gen => table {
                   Sg => base_1+"інің" ;
                   Pl => base_1+"лардың" --guessed
                 } ;
          Ablat => table {
                     Sg => base_1+"ден" ;
                     Pl => base_1+"лерден" --guessed
                   } ;
          Instr => table {
                     Sg => base_1+"імен" ;
                     Pl => base_1+"лермен" --guessed
                   } ;
          Loc => table {
                   Sg => base_1+"да" ; --guessed
                   Pl => base_1+"лерде" --guessed
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"іміз" ; --guessed
                          Pl => base_1+"ларымыз" --guessed
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ім" ; --guessed
                          Pl => base_1+"ларым" --guessed
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"іңіз" ; --guessed
                                   Pl => base_1+"ларыңыз" --guessed
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ің" ; --guessed
                                 Pl => base_1+"ларың" --guessed
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"і" ; --guessed
                          Pl => base_1+"дері" --guessed
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN041 : Str -> N ;
mkN041 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"лар"
                 } ;
          Acc => table {
                   Sg => base_1+"ны" ;
                   Pl => base_1+"ларды"
                 } ;
          Dat => table {
                   Sg => base_1+"ға" ;
                   Pl => base_1+"ларға"
                 } ;
          Gen => table {
                   Sg => base_1+"ның" ;
                   Pl => base_1+"лардың"
                 } ;
          Ablat => table {
                     Sg => base_1+"дан" ;
                     Pl => base_1+"лардан"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"лармен"
                   } ;
          Loc => table {
                   Sg => base_1+"да" ;
                   Pl => base_1+"ларда"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"мыз" ;
                          Pl => base_1+"ларымыз"
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"м" ;
                          Pl => base_1+"ларым"
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"ңыз" ;
                                   Pl => base_1+"ларыңыз"
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ң" ;
                                 Pl => base_1+"ларың"
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"сы" ;
                          Pl => base_1+"лар"
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN042 : Str -> N ;
mkN042 base =
  case base of {
    base_1+"п" => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"п" ;
                       Pl => base_1+"птер"
                     } ;
              Acc => table {
                       Sg => base_1+"пті" ;
                       Pl => base_1+"птерді"
                     } ;
              Dat => table {
                       Sg => base_1+"пке" ;
                       Pl => base_1+"птерге"
                     } ;
              Gen => table {
                       Sg => base_1+"птің" ;
                       Pl => base_1+"птердің"
                     } ;
              Ablat => table {
                         Sg => base_1+"птен" ;
                         Pl => base_1+"птерден"
                       } ;
              Instr => table {
                         Sg => base_1+"ппен" ;
                         Pl => base_1+"птермен"
                       } ;
              Loc => table {
                       Sg => base_1+"пте" ;
                       Pl => base_1+"птерде"
                     }
            } ;
        poss = table {
                 Poss1Pl => table {
                              Sg => base_1+"біміз" ;
                              Pl => base_1+"птеріміз"
                            } ;
                 Poss1Sg => table {
                              Sg => base_1+"бім" ;
                              Pl => base_1+"птерім"
                            } ;
                 Poss2Sg Informal => table {
                                       Sg => base_1+"біңіз" ;
                                       Pl => base_1+"птеріңіз"
                                     } ;
                 Poss2Sg Formal => table {
                                     Sg => base_1+"бің" ;
                                     Pl => base_1+"птерің"
                                   } ;
                 Poss2Pl Informal => table {
                                       Sg => nonExist ; --guessed
                                       Pl => nonExist --guessed
                                     } ;
                 Poss2Pl Formal => table {
                                     Sg => nonExist ; --guessed
                                     Pl => nonExist --guessed
                                   } ;
                 Poss3Sg => table {
                              Sg => base_1+"бі" ;
                              Pl => base_1+"птері"
                            } ;
                 Poss3Pl => table {
                              Sg => nonExist ; --guessed
                              Pl => nonExist --guessed
                            }
               }
      };
    _ => error "Can't apply paradigm mkN042"
  } ;

mkN043 : Str -> N ;
mkN043 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"лар"
                 } ;
          Acc => table {
                   Sg => base_1+"ны" ;
                   Pl => base_1+"ларды"
                 } ;
          Dat => table {
                   Sg => base_1+"ға" ;
                   Pl => base_1+"ларға"
                 } ;
          Gen => table {
                   Sg => base_1+"ның" ;
                   Pl => base_1+"лардың"
                 } ;
          Ablat => table {
                     Sg => base_1+"дан" ;
                     Pl => base_1+"лардан"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"лармен"
                   } ;
          Loc => table {
                   Sg => base_1+"да" ;
                   Pl => base_1+"ларда"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"амыз" ;
                          Pl => base_1+"ларымыз"
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"м" ;
                          Pl => base_1+"ларым"
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"ңыз" ;
                                   Pl => base_1+"ларыңыз"
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ң" ;
                                 Pl => base_1+"ларың"
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"сы" ;
                          Pl => base_1+"лары"
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN044 : Str -> N ;
mkN044 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1
                 } ;
          Acc => table {
                   Sg => base_1+"ды" ;
                   Pl => base_1+"дарды"
                 } ;
          Dat => table {
                   Sg => base_1+"ға" ;
                   Pl => base_1+"дарға"
                 } ;
          Gen => table {
                   Sg => base_1+"дың" ;
                   Pl => base_1+"дардың"
                 } ;
          Ablat => table {
                     Sg => base_1+"дан" ;
                     Pl => base_1+"дардан"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"дармен"
                   } ;
          Loc => table {
                   Sg => base_1+"да" ;
                   Pl => base_1+"дарда"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"ымыз" ; --guessed
                          Pl => base_1+"ларымыз" --guessed
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ым" ; --guessed
                          Pl => base_1+"дарым" --guessed
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"ыңыз" ; --guessed
                                   Pl => base_1+"дарыңыз" --guessed
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ың" ; --guessed
                                 Pl => base_1+"дарың" --guessed
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"ы" ; --guessed
                          Pl => base_1+"дары" --guessed
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN045 : Str -> N ;
mkN045 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1
                 } ;
          Acc => table {
                   Sg => base_1+"ді" ;
                   Pl => base_1+"лерді"
                 } ;
          Dat => table {
                   Sg => base_1+"ге" ;
                   Pl => base_1+"лерге"
                 } ;
          Gen => table {
                   Sg => base_1+"дің" ;
                   Pl => base_1+"лердің"
                 } ;
          Ablat => table {
                     Sg => base_1+"ден" ;
                     Pl => base_1+"лерден"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"лермен"
                   } ;
          Loc => table {
                   Sg => base_1+"де" ;
                   Pl => base_1+"лерде"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"іміз" ; --guessed
                          Pl => base_1+"леріміз" --guessed
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ім" ; --guessed
                          Pl => base_1+"лерім" --guessed
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"іңіз" ; --guessed
                                   Pl => base_1+"леріңіз" --guessed
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ің" ; --guessed
                                 Pl => base_1+"лерің" --guessed
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"і" ; --guessed
                          Pl => base_1+"дері" --guessed
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN046 : Str -> N ;
mkN046 base =
  case base of {
    base_1+"қ" => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"қ" ;
                       Pl => base_1+"қтар"
                     } ;
              Acc => table {
                       Sg => base_1+"қты" ;
                       Pl => base_1+"қтарды"
                     } ;
              Dat => table {
                       Sg => base_1+"ққа" ;
                       Pl => base_1+"қтарға"
                     } ;
              Gen => table {
                       Sg => base_1+"қтың" ;
                       Pl => base_1+"қтардың"
                     } ;
              Ablat => table {
                         Sg => base_1+"қтан" ;
                         Pl => base_1+"қтардан"
                       } ;
              Instr => table {
                         Sg => base_1+"қпен" ;
                         Pl => base_1+"қтармен"
                       } ;
              Loc => table {
                       Sg => base_1+"қта" ;
                       Pl => base_1+"қтарда"
                     }
            } ;
        poss = table {
                 Poss1Pl => table {
                              Sg => base_1+"ғың" ;
                              Pl => base_1+"қтарың"
                            } ;
                 Poss1Sg => table {
                              Sg => base_1+"ғым" ;
                              Pl => base_1+"қтарымыз"
                            } ;
                 Poss2Sg Informal => table {
                                       Sg => base_1+"ғыңыз" ;
                                       Pl => base_1+"қтарыңыз"
                                     } ;
                 Poss2Sg Formal => table {
                                     Sg => base_1+"ғ" ;
                                     Pl => base_1+"қтар"
                                   } ;
                 Poss2Pl Informal => table {
                                       Sg => nonExist ; --guessed
                                       Pl => nonExist --guessed
                                     } ;
                 Poss2Pl Formal => table {
                                     Sg => nonExist ; --guessed
                                     Pl => nonExist --guessed
                                   } ;
                 Poss3Sg => table {
                              Sg => base_1+"ғы" ;
                              Pl => base_1+"қтары"
                            } ;
                 Poss3Pl => table {
                              Sg => nonExist ; --guessed
                              Pl => nonExist --guessed
                            }
               }
      };
    _ => error "Can't apply paradigm mkN046"
  } ;

mkN047 : Str -> N ;
mkN047 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"дар"
                 } ;
          Acc => table {
                   Sg => base_1+"ды" ;
                   Pl => base_1+"дарды"
                 } ;
          Dat => table {
                   Sg => base_1+"ға" ;
                   Pl => base_1+"дарға"
                 } ;
          Gen => table {
                   Sg => base_1+"ның" ;
                   Pl => base_1+"дардың"
                 } ;
          Ablat => table {
                     Sg => base_1+"нан" ;
                     Pl => base_1+"дардан"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"дармен"
                   } ;
          Loc => table {
                   Sg => base_1+"да" ;
                   Pl => base_1+"дарда"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"мыз" ;
                          Pl => base_1+"дармыз"
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ым" ;
                          Pl => base_1+"дарым"
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"ыңыз" ;
                                   Pl => base_1+"дарыңыз"
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ың" ;
                                 Pl => base_1+"дарың"
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"ы" ;
                          Pl => base_1+"дары"
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN048 : Str -> N ;
mkN048 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"лар"
                 } ;
          Acc => table {
                   Sg => base_1+"ны" ;
                   Pl => base_1+"ларды"
                 } ;
          Dat => table {
                   Sg => base_1+"ға" ;
                   Pl => base_1+"ларға"
                 } ;
          Gen => table {
                   Sg => base_1+"ның" ;
                   Pl => base_1+"лардың"
                 } ;
          Ablat => table {
                     Sg => base_1+"дан" ;
                     Pl => base_1+"лардан"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"лармен"
                   } ;
          Loc => table {
                   Sg => base_1+"да" ;
                   Pl => base_1+"ларда"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"мыз" ;
                          Pl => base_1+"ларымыз"
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"м" ;
                          Pl => base_1+"ларым"
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"ңыз" ;
                                   Pl => base_1+"ларыңыз"
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ң" ;
                                 Pl => base_1+"ларың"
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"сы" ;
                          Pl => base_1+"ларысы"
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN049 : Str -> N ;
mkN049 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"тар"
                 } ;
          Acc => table {
                   Sg => base_1+"ты" ;
                   Pl => base_1+"тарды"
                 } ;
          Dat => table {
                   Sg => base_1+"қа" ;
                   Pl => base_1+"тардың"
                 } ;
          Gen => table {
                   Sg => base_1+"тың" ;
                   Pl => base_1+"тардың"
                 } ;
          Ablat => table {
                     Sg => base_1+"тан" ;
                     Pl => base_1+"тардан"
                   } ;
          Instr => table {
                     Sg => base_1+"пен" ;
                     Pl => base_1+"тармен"
                   } ;
          Loc => table {
                   Sg => base_1+"та" ;
                   Pl => base_1+"тарда"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"ымыз" ;
                          Pl => base_1+"тарымыз"
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ым" ;
                          Pl => base_1+"тарым"
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"ыңыз" ;
                                   Pl => base_1+"тарыңыз"
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ың" ;
                                 Pl => base_1+"тарың"
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"ы" ;
                          Pl => base_1+"тары"
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN050 : Str -> N ;
mkN050 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"лар"
                 } ;
          Acc => table {
                   Sg => base_1+"ды" ;
                   Pl => base_1+"ларды"
                 } ;
          Dat => table {
                   Sg => base_1+"ға" ;
                   Pl => base_1+"ларға"
                 } ;
          Gen => table {
                   Sg => base_1+"дың" ;
                   Pl => base_1+"лардың"
                 } ;
          Ablat => table {
                     Sg => base_1+"да" ;
                     Pl => base_1+"лардан"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"лармен"
                   } ;
          Loc => table {
                   Sg => base_1+"да" ;
                   Pl => base_1+"ларда"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"ымыз" ;
                          Pl => base_1+"ларымыз"
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ым" ;
                          Pl => base_1+"ларым"
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"ыңыз" ;
                                   Pl => base_1+"ларыңыз"
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ың" ;
                                 Pl => base_1+"ларың"
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"ы" ;
                          Pl => base_1+"лары"
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN051 : Str -> N ;
mkN051 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"лер"
                 } ;
          Acc => table {
                   Sg => base_1+"ді" ;
                   Pl => base_1+"лерді"
                 } ;
          Dat => table {
                   Sg => base_1+"ге" ;
                   Pl => base_1+"лерге"
                 } ;
          Gen => table {
                   Sg => base_1+"дің" ;
                   Pl => base_1+"лердің"
                 } ;
          Ablat => table {
                     Sg => base_1+"ден" ;
                     Pl => base_1+"лерден"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"лермен"
                   } ;
          Loc => table {
                   Sg => base_1+"де" ;
                   Pl => base_1+"лерде"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"іміз" ;
                          Pl => base_1+"леріміз"
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ім" ;
                          Pl => base_1+"лерім"
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"іңіз" ;
                                   Pl => base_1+"леріңіз"
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ің" ;
                                 Pl => base_1+"лерің"
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"і" ;
                          Pl => base_1+"лер"
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN052 : Str -> N ;
mkN052 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"дер"
                 } ;
          Acc => table {
                   Sg => base_1+"ді" ;
                   Pl => base_1+"дерді"
                 } ;
          Dat => table {
                   Sg => base_1+"ге" ;
                   Pl => base_1+"мдерге"
                 } ;
          Gen => table {
                   Sg => base_1+"нің" ;
                   Pl => base_1+"дердің"
                 } ;
          Ablat => table {
                     Sg => base_1+"нен" ;
                     Pl => base_1+"дерден"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"дермен"
                   } ;
          Loc => table {
                   Sg => base_1+"де" ;
                   Pl => base_1+"дерде"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"іміз" ;
                          Pl => base_1+"деріміз"
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ім" ;
                          Pl => base_1+"дерім"
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"іңіз" ;
                                   Pl => base_1+"деріңіз"
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ің" ;
                                 Pl => base_1+"дерің"
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"і" ;
                          Pl => base_1+"дері"
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN053 : Str -> N ;
mkN053 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"ттар"
                 } ;
          Acc => table {
                   Sg => base_1+"ты" ;
                   Pl => base_1+"тарды"
                 } ;
          Dat => table {
                   Sg => base_1+"қа" ;
                   Pl => base_1+"тарға"
                 } ;
          Gen => table {
                   Sg => base_1+"тың" ;
                   Pl => base_1+"тардың"
                 } ;
          Ablat => table {
                     Sg => base_1+"тан" ;
                     Pl => base_1+"тардан"
                   } ;
          Instr => table {
                     Sg => base_1+"пен" ;
                     Pl => base_1+"тармен"
                   } ;
          Loc => table {
                   Sg => base_1+"та" ;
                   Pl => base_1+"тарда"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"іміз" ; --guessed
                          Pl => base_1+"ларымыз" --guessed
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ым" ; --guessed
                          Pl => base_1+"ларым" --guessed
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"ыңыз" ; --guessed
                                   Pl => base_1+"ларыңыз" --guessed
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ың" ; --guessed
                                 Pl => base_1+"ларың" --guessed
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"ы" ; --guessed
                          Pl => base_1+"дары" --guessed
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN054 : Str -> N ;
mkN054 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"лар"
                 } ;
          Acc => table {
                   Sg => base_1+"ны" ;
                   Pl => base_1+"ды"
                 } ;
          Dat => table {
                   Sg => base_1+"ға" ;
                   Pl => base_1+"ларға"
                 } ;
          Gen => table {
                   Sg => base_1+"ның" ;
                   Pl => base_1+"лардың"
                 } ;
          Ablat => table {
                     Sg => base_1+"дан" ;
                     Pl => base_1+"дан"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"мен"
                   } ;
          Loc => table {
                   Sg => base_1+"да" ;
                   Pl => base_1+"да"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"іміз" ; --guessed
                          Pl => base_1+"ларымыз" --guessed
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ым" ; --guessed
                          Pl => base_1+"ларым" --guessed
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"ыңыз" ; --guessed
                                   Pl => base_1+"ларыңыз" --guessed
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ың" ; --guessed
                                 Pl => base_1+"ларың" --guessed
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"ы" ; --guessed
                          Pl => base_1+"дары" --guessed
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN055 : Str -> N ;
mkN055 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"лар" --guessed
                 } ;
          Acc => table {
                   Sg => base_1+"ні" ;
                   Pl => base_1+"дерді" --guessed
                 } ;
          Dat => table {
                   Sg => base_1+"ге" ;
                   Pl => base_1+"ларға" --guessed
                 } ;
          Gen => table {
                   Sg => base_1+"нің" ;
                   Pl => base_1+"лардың" --guessed
                 } ;
          Ablat => table {
                     Sg => base_1+"ден" ;
                     Pl => base_1+"лардан" --guessed
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"лармен" --guessed
                   } ;
          Loc => table {
                   Sg => base_1+"де" ;
                   Pl => base_1+"дерде" --guessed
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"іміз" ; --guessed
                          Pl => base_1+"ларымыз" --guessed
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ім" ; --guessed
                          Pl => base_1+"ларым" --guessed
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"іңіз" ; --guessed
                                   Pl => base_1+"ларыңыз" --guessed
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ің" ; --guessed
                                 Pl => base_1+"ларың" --guessed
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"і" ; --guessed
                          Pl => base_1+"дері" --guessed
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN056 : Str -> N ;
mkN056 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"дар" --guessed
                 } ;
          Acc => table {
                   Sg => base_1+"ны" ;
                   Pl => base_1+"лерді" --guessed
                 } ;
          Dat => table {
                   Sg => base_1+"ға" ;
                   Pl => base_1+"дарға" --guessed
                 } ;
          Gen => table {
                   Sg => base_1+"ның" ;
                   Pl => base_1+"дардың" --guessed
                 } ;
          Ablat => table {
                     Sg => base_1+"дан" ;
                     Pl => base_1+"дардан" --guessed
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"дармен" --guessed
                   } ;
          Loc => table {
                   Sg => base_1+"да" ;
                   Pl => base_1+"лерде" --guessed
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"іміз" ; --guessed
                          Pl => base_1+"леріміз" --guessed
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ім" ; --guessed
                          Pl => base_1+"дарым" --guessed
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"іңіз" ; --guessed
                                   Pl => base_1+"дарыңыз" --guessed
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ің" ; --guessed
                                 Pl => base_1+"дарың" --guessed
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"і" ; --guessed
                          Pl => base_1+"дары" --guessed
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN057 : Str -> N ;
mkN057 base_1 =
  lin N
  { s = table {
          Nom => table {
                   Sg => base_1 ;
                   Pl => base_1+"лер"
                 } ;
          Acc => table {
                   Sg => base_1+"ні" ;
                   Pl => base_1+"лерді"
                 } ;
          Dat => table {
                   Sg => base_1+"ге" ;
                   Pl => base_1+"лерге"
                 } ;
          Gen => table {
                   Sg => base_1+"нің" ;
                   Pl => base_1+"лердің"
                 } ;
          Ablat => table {
                     Sg => base_1+"ден" ;
                     Pl => base_1+"лерден"
                   } ;
          Instr => table {
                     Sg => base_1+"мен" ;
                     Pl => base_1+"лермен"
                   } ;
          Loc => table {
                   Sg => base_1+"де" ;
                   Pl => base_1+"лерде"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => base_1+"іміз" ; --guessed
                          Pl => base_1+"ларымыз" --guessed
                        } ;
             Poss1Sg => table {
                          Sg => base_1+"ім" ; --guessed
                          Pl => base_1+"ларым" --guessed
                        } ;
             Poss2Sg Informal => table {
                                   Sg => base_1+"іңіз" ; --guessed
                                   Pl => base_1+"ларыңыз" --guessed
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => base_1+"ің" ; --guessed
                                 Pl => base_1+"ларың" --guessed
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => base_1+"і" ; --guessed
                          Pl => base_1+"дері" --guessed
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkN058 : Str -> N ;
mkN058 base =
  case base of {
    base_1+"е"+base_2@? => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"е"+base_2 ;
                       Pl => nonExist
                     } ;
              Acc => table {
                       Sg => base_1+"е"+base_2+"і" ;
                       Pl => nonExist
                     } ;
              Dat => table {
                       Sg => base_1+"аға"+base_2 ;
                       Pl => nonExist
                     } ;
              Gen => table {
                       Sg => base_1+"е"+base_2+"ің" ;
                       Pl => nonExist
                     } ;
              Ablat => table {
                         Sg => base_1+"е"+base_2+"нен" ;
                         Pl => nonExist
                       } ;
              Instr => table {
                         Sg => base_1+"е"+base_2+"імен" ;
                         Pl => nonExist
                       } ;
              Loc => table {
                       Sg => base_1+"е"+base_2+"де" ;
                       Pl => nonExist
                     }
            } ;
        poss = table {
                 Poss1Pl => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            } ;
                 Poss1Sg => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            } ;
                 Poss2Sg Informal => table {
                                       Sg => nonExist ;
                                       Pl => nonExist
                                     } ;
                 Poss2Sg Formal => table {
                                     Sg => nonExist ;
                                     Pl => nonExist
                                   } ;
                 Poss2Pl Informal => table {
                                       Sg => nonExist ; --guessed
                                       Pl => nonExist --guessed
                                     } ;
                 Poss2Pl Formal => table {
                                     Sg => nonExist ; --guessed
                                     Pl => nonExist --guessed
                                   } ;
                 Poss3Sg => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            } ;
                 Poss3Pl => table {
                              Sg => nonExist ; --guessed
                              Pl => nonExist --guessed
                            }
               }
      };
    _ => error "Can't apply paradigm mkN058"
  } ;

mkN059 : Str -> Str -> N ;
mkN059 base_1 base_2 =
  lin N
  { s = table {
          Nom => table {
                   Sg => nonExist ;
                   Pl => base_1+"а"+base_2
                 } ;
          Acc => table {
                   Sg => nonExist ;
                   Pl => base_1+"а"+base_2+"ды"
                 } ;
          Dat => table {
                   Sg => nonExist ;
                   Pl => base_1+"а"+base_2+"ға"
                 } ;
          Gen => table {
                   Sg => nonExist ;
                   Pl => base_1+"ы"+base_2+"дың"
                 } ;
          Ablat => table {
                     Sg => nonExist ;
                     Pl => base_1+"а"+base_2+"дан"
                   } ;
          Instr => table {
                     Sg => nonExist ;
                     Pl => base_1+"а"+base_2+"мен"
                   } ;
          Loc => table {
                   Sg => nonExist ;
                   Pl => base_1+"а"+base_2+"да"
                 }
        } ;
    poss = table {
             Poss1Pl => table {
                          Sg => nonExist ;
                          Pl => nonExist
                        } ;
             Poss1Sg => table {
                          Sg => nonExist ;
                          Pl => nonExist
                        } ;
             Poss2Sg Informal => table {
                                   Sg => nonExist ;
                                   Pl => nonExist
                                 } ;
             Poss2Sg Formal => table {
                                 Sg => nonExist ;
                                 Pl => nonExist
                               } ;
             Poss2Pl Informal => table {
                                   Sg => nonExist ; --guessed
                                   Pl => nonExist --guessed
                                 } ;
             Poss2Pl Formal => table {
                                 Sg => nonExist ; --guessed
                                 Pl => nonExist --guessed
                               } ;
             Poss3Sg => table {
                          Sg => nonExist ;
                          Pl => nonExist
                        } ;
             Poss3Pl => table {
                          Sg => nonExist ; --guessed
                          Pl => nonExist --guessed
                        }
           }
  } ;

mkV001 : Str -> V ;
mkV001 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                 Sg => base_1+"ып отырмын" ;
                                                                 Pl => base_1+"ып отырмыз"
                                                               } ;
                                                         P2 => table {
                                                                 Sg => base_1+"ып отырсың" ;
                                                                 Pl => base_1+"ып отырсыңдар"
                                                               } ;
                                                         P3 => table {
                                                                 Sg => base_1+"ып отырсыз" ;
                                                                 Pl => base_1+"ып отырсыздар"
                                                               }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                 Sg => base_1+"ып отыр" ;
                                                                 Pl => base_1+"ып отыр"
                                                               } ;
                                                         P2 => table {
                                                                Sg => base_1+"ып отырмаймын" ;
                                                                Pl => base_1+"ып отырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ып отырмайсың" ;
                                                                Pl => base_1+"ып отырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                              Sg => base_1+"ып отырмайсыз" ;
                                                              Pl => base_1+"ып отырмайсыздар"
                                                            } ;
                                                      P2 => table {
                                                             Sg => base_1+"ып отырмай" ;
                                                             Pl => base_1+"ып отырмай"
                                                           } ;
                                                      P3 => table {
                                                              Sg => base_1+"амын" ;
                                                              Pl => base_1+"амыз"
                                                            }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                              Sg => base_1+"асың" ;
                                                              Pl => base_1+"асыңдар"
                                                            } ;
                                                      P2 => table {
                                                             Sg => base_1+"асыз" ;
                                                             Pl => base_1+"асыздар"
                                                           } ;
                                                      P3 => table {
                                                              Sg => base_1+"ады" ;
                                                              Pl => base_1+"ады"
                                                            }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"амаймын" ;
                                                            Pl => base_1+"амайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"амайсың" ;
                                                            Pl => base_1+"амайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"амайсыз" ;
                                                            Pl => base_1+"амайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"амайды" ;
                                                            Pl => base_1+"амаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"қанмын" ;
                                                            Pl => base_1+"қанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg =>base_1+"қансың" ;
                                                            Pl => base_1+"қансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"қансыз" ;
                                                                Pl => base_1+"қансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"қан" ;
                                                                Pl => base_1+"қан"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"пағандым" ;
                                                                Pl => base_1+"пағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"пағандың" ;
                                                                Pl => base_1+"пағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"пағандыңыз" ;
                                                                Pl => base_1+"пағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"пағанды" ;
                                                                Pl => base_1+"пағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"атынмын" ;
                                                             Pl => base_1+"атынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"атынсың" ;
                                                             Pl => base_1+"атынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"атынсыз" ;
                                                             Pl => base_1+"атынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"атын" ;
                                                             Pl => base_1+"атын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"патынмын" ;
                                                             Pl => base_1+"патынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"патынсың" ;
                                                             Pl => base_1+"патынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"патынсыз" ;
                                                      Pl => base_1+"патынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"патын" ;
                                                    Pl => base_1+"патын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"тым" ;
                                                      Pl => base_1+"тық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"тың" ;
                                                    Pl => base_1+"тыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"тыңыз" ;
                               Pl => base_1+"тыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"ты" ;
                               Pl => base_1+"ты"
                             } ;
                        P3 => table {
                                Sg => base_1+"падым" ;
                                Pl => base_1+"падық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV001"
  } ;

mkV002 : Str -> V ;
mkV002 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"п тұрмын" ;
                                                                Pl => base_1+"п тұрмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"п тұрсың" ;
                                                                Pl => base_1+"п тұрсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"п тұрсыз" ;
                                                                Pl => base_1+"п тұрсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"п тұр" ;
                                                                Pl => base_1+"п тұр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"п тұрмаймын" ;
                                                                Pl => base_1+"п тұрмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"п тұрмайсың" ;
                                                                Pl => base_1+"п тұрмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"п тұрмайсыз" ;
                                                             Pl => base_1+"п тұрмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"п тұрмай" ;
                                                             Pl => base_1+"п тұрмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ймын" ;
                                                             Pl => base_1+"ймыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"йсың" ;
                                                             Pl => base_1+"йсыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"йсыз" ;
                                                             Pl => base_1+"йсыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"йды" ;
                                                             Pl => base_1+"йды"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"ймаймын" ;
                                                            Pl => base_1+"ймайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ймайсың" ;
                                                            Pl => base_1+"ймайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ймайсыз" ;
                                                            Pl => base_1+"ймайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"ймайды" ;
                                                            Pl => base_1+"ймаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ғанмын" ;
                                                            Pl => base_1+"ғанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ғансың" ;
                                                            Pl => base_1+"ғансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ғансыз" ;
                                                                Pl => base_1+"ғансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ған" ;
                                                                Pl => base_1+"ған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағандым" ;
                                                                Pl => base_1+"мағандық"
                                                              }
                                                       } ;
                                                Neg => table { 
                                                         P1 => table {
                                                                Sg => base_1+"мағандың" ;
                                                                Pl => base_1+"мағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"мағандыңыз" ;
                                                                Pl => base_1+"мағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағанды" ;
                                                                Pl => base_1+"мағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"йтынмын" ;
                                                             Pl => base_1+"йтынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"йтынсың" ;
                                                             Pl => base_1+"йтынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"йтынсыз" ;
                                                             Pl => base_1+"йтынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"йтын" ;
                                                             Pl => base_1+"йтын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"матынмын" ;
                                                             Pl => base_1+"матынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"матынсың" ;
                                                             Pl => base_1+"матынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"матынсыз" ;
                                                      Pl => base_1+"матынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"матын" ;
                                                    Pl => base_1+"матын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"дым" ;
                                                      Pl => base_1+"дық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"дың" ;
                                                    Pl => base_1+"дыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"дыңыз" ;
                               Pl => base_1+"дыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"ды" ;
                               Pl => base_1+"ды"
                             } ;
                        P3 => table {
                               Sg => base_1+"мадым" ;
                               Pl => base_1+"мадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV002"
  } ;

mkV003 : Str -> V ;
mkV003 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ып жатырмын" ;
                                                                Pl => base_1+"ып жатырмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ып жатырсың" ;
                                                                Pl => base_1+"ып жатырсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ып жатырсыз" ;
                                                                Pl => base_1+"ып жатырсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"ып жатыр" ;
                                                                Pl => base_1+"ып жатыр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ып жатырмаймын" ;
                                                                Pl => base_1+"ып жатырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ып жатырмайсың" ;
                                                                Pl => base_1+"ып жатырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"ып жатырмайсыз" ;
                                                             Pl => base_1+"ып жатырмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"ып жатырмай" ;
                                                             Pl => base_1+"ып жатырмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"амын" ;
                                                             Pl => base_1+"амыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"асың" ;
                                                             Pl => base_1+"асыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"асыз" ;
                                                             Pl => base_1+"асыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ады" ;
                                                             Pl => base_1+"ады"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"амаймын" ;
                                                            Pl => base_1+"амайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"амайсың" ;
                                                            Pl => base_1+"амайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"амайсыз" ;
                                                            Pl => base_1+"амайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"амайды" ;
                                                            Pl => base_1+"амаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ғанмын" ;
                                                            Pl => base_1+"ғанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ғансың" ;
                                                            Pl => base_1+"ғансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ғансыз" ;
                                                                Pl => base_1+"ғансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ған" ;
                                                                Pl => base_1+"ған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағандым" ;
                                                                Pl => base_1+"мағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"мағандың" ;
                                                                Pl => base_1+"мағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"мағандыңыз" ;
                                                                Pl => base_1+"мағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағанды" ;
                                                                Pl => base_1+"мағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"атынмын" ;
                                                             Pl => base_1+"атынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"атынсың" ;
                                                             Pl => base_1+"атынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"атынсыз" ;
                                                             Pl => base_1+"атынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                       P1 => table {
                                                             Sg => base_1+"атын" ;
                                                             Pl => base_1+"атын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"матынмын" ;
                                                             Pl => base_1+"матынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"матынсың" ;
                                                             Pl => base_1+"матынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"матынсыз" ;
                                                      Pl => base_1+"матынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"матын" ;
                                                    Pl => base_1+"матын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"дым" ;
                                                      Pl => base_1+"дық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"дың" ;
                                                    Pl => base_1+"дыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"дыңыз" ;
                               Pl => base_1+"дыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"ды" ;
                               Pl => base_1+"ды"
                             } ;
                        P3 => table {
                               Sg => base_1+"мадым" ;
                               Pl => base_1+"мадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV003"
  } ;

mkV004 : Str -> V ;
mkV004 base =
  case base of {
    base_1+"лу" => lin V
      { Infinitive = base_1+"лу" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"лып тұрмын" ;
                                                                Pl => base_1+"лып тұрмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"лып тұрсың" ;
                                                                Pl => base_1+"лып тұрсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"лып тұрсыз" ;
                                                                Pl => base_1+"лып тұрсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"лып тұр" ;
                                                                Pl => base_1+"лып тұр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"лып тұрмаймын" ;
                                                                Pl => base_1+"лып тұрмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"лып тұрмайсың" ;
                                                                Pl => base_1+"лып тұрмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"лып тұрмайсыз" ;
                                                             Pl => base_1+"лып тұрмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"лып тұрмай" ;
                                                             Pl => base_1+"лып тұрмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ламын" ;
                                                             Pl => base_1+"ламыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"ласың" ;
                                                             Pl => base_1+"ласыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"ласыз" ;
                                                             Pl => base_1+"ласыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"лады" ;
                                                             Pl => base_1+"лады"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"ламаймын" ;
                                                            Pl => base_1+"ламайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ламайсың" ;
                                                            Pl => base_1+"ламайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ламайсыз" ;
                                                            Pl => base_1+"ламайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"ламайды" ;
                                                            Pl => base_1+"ламаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => "шыд"+base_1+"ғанмын" ;
                                                            Pl => "шыд"+base_1+"ғанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"лғансың" ;
                                                            Pl => base_1+"лғансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => "шыд"+base_1+"ғансыз" ;
                                                                Pl => base_1+"лғансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "шыд"+base_1+"ған" ;
                                                                Pl => base_1+"лған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"лмағандым" ;
                                                                Pl => "шыд"+base_1+"мағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"лмағандың" ;
                                                                Pl => base_1+"лмағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"лмағандыңыз" ;
                                                                Pl => base_1+"лмағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "шыд"+base_1+"мағанды" ;
                                                                Pl => "шыд"+base_1+"мағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"латынмын" ;
                                                             Pl => base_1+"латынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"латынсың" ;
                                                             Pl => base_1+"латынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"латынсыз" ;
                                                             Pl => base_1+"латынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"латын" ;
                                                             Pl => base_1+"латын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"лматынмын" ;
                                                             Pl => base_1+"лматынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"лматынсың" ;
                                                             Pl => base_1+"лматынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"лматынсыз" ;
                                                      Pl => base_1+"лматынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"лматын" ;
                                                    Pl => base_1+"лматын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"лдым" ;
                                                      Pl => base_1+"лдық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"лдың" ;
                                                    Pl => base_1+"лдыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"лдыңыз" ;
                               Pl => base_1+"лдыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"лды" ;
                               Pl => base_1+"лды"
                             } ;
                        P3 => table {
                               Sg => base_1+"лмадым" ;
                               Pl => base_1+"лмадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV004"
  } ;

mkV005 : Str -> V ;
mkV005 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ып тұрмын" ;
                                                                Pl => base_1+"ып тұрмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ып тұрсың" ;
                                                                Pl => base_1+"ып тұрсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ып тұрсыз" ;
                                                                Pl => base_1+"ып тұрсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"ып тұр" ;
                                                                Pl => base_1+"ып тұр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ып тұрмаймын" ;
                                                                Pl => base_1+"ып тұрмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ып тұрмайсың" ;
                                                                Pl => base_1+"ып тұрмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"ып тұрмайсыз" ;
                                                             Pl => base_1+"ып тұрмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"ып тұрмай" ;
                                                             Pl => base_1+"ып тұрмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"амын" ;
                                                             Pl => base_1+"амыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"асың" ;
                                                             Pl => base_1+"асыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"асыз" ;
                                                             Pl => base_1+"асыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ады" ;
                                                             Pl => base_1+"ады"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"амаймын" ;
                                                            Pl => base_1+"амайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"амайсың" ;
                                                            Pl => base_1+"амайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"амайсыз" ;
                                                            Pl => base_1+"амайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"амайды" ;
                                                            Pl => base_1+"амаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"қанмын" ;
                                                            Pl => base_1+"қанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"қансың" ;
                                                            Pl => base_1+"қансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"қансыз" ;
                                                                Pl => base_1+"қансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"қан" ;
                                                                Pl => base_1+"қан"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"пағандым" ;
                                                                Pl => base_1+"пағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"пағандың" ;
                                                                Pl => base_1+"пағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"пағандыңыз" ;
                                                                Pl => base_1+"пағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"пағанды" ;
                                                                Pl => base_1+"пағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"атынмын" ;
                                                             Pl => base_1+"атынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"атынсың" ;
                                                             Pl => base_1+"атынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"атынсыз" ;
                                                             Pl => base_1+"атынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"атын" ;
                                                             Pl => base_1+"атын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"патынмын" ;
                                                             Pl => base_1+"патынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"патынсың" ;
                                                             Pl => base_1+"патынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"патынсыз" ;
                                                      Pl => base_1+"патынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"патын" ;
                                                    Pl => base_1+"патын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"тым" ;
                                                      Pl => base_1+"тық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"тың" ;
                                                    Pl => base_1+"тыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"тыңыз" ;
                               Pl => base_1+"тыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"ты" ;
                               Pl => base_1+"ты"
                             } ;
                        P3 => table {
                               Sg => base_1+"падым" ;
                               Pl => base_1+"падық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV005"
  } ;

mkV006 : Str -> V ;
mkV006 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"п жатырмын" ;
                                                                Pl => base_1+"п жатырмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"п жатырсың" ;
                                                                Pl => base_1+"п жатырсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"п жатырсыз" ;
                                                                Pl => base_1+"п жатырсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"п жатыр" ;
                                                                Pl => base_1+"п жатыр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"п жатырмаймын" ;
                                                                Pl => base_1+"п жатырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"п жатырмайсың" ;
                                                                Pl => base_1+"п жатырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"п жатырмайсыз" ;
                                                             Pl => base_1+"п жатырмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"п жатырмай" ;
                                                             Pl => base_1+"п жатырмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ймын" ;
                                                             Pl => base_1+"ймыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"йсың" ;
                                                             Pl => base_1+"йсыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"йсыз" ;
                                                             Pl => base_1+"йсыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"йды" ;
                                                             Pl => base_1+"йды"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"ймаймын" ;
                                                            Pl => base_1+"ймайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ймайсың" ;
                                                            Pl => base_1+"ймайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ймайсыз" ;
                                                            Pl => base_1+"ймайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"ймайды" ;
                                                            Pl => base_1+"ймаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ғанмын" ;
                                                            Pl => base_1+"ғанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ғансың" ;
                                                            Pl => base_1+"ғансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ғансыз" ;
                                                                Pl => base_1+"ғансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ған" ;
                                                                Pl => base_1+"ған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағандым" ;
                                                                Pl => base_1+"мағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"мағандың" ;
                                                                Pl => base_1+"мағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"мағандыңыз" ;
                                                                Pl => base_1+"мағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағанды" ;
                                                                Pl => base_1+"мағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"йтынмын" ;
                                                             Pl => base_1+"йтынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"йтынсың" ;
                                                             Pl => base_1+"йтынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"йтынсыз" ;
                                                             Pl => base_1+"йтынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"йтын" ;
                                                             Pl => base_1+"йтын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"матынмын" ;
                                                             Pl => base_1+"матынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"матынсың" ;
                                                             Pl => base_1+"матынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"матынсыз" ;
                                                      Pl => base_1+"матынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"матын" ;
                                                    Pl => base_1+"матын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"дым" ;
                                                      Pl => base_1+"дық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"дың" ;
                                                    Pl => base_1+"дыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"дыңыз" ;
                               Pl => base_1+"дыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"ды" ;
                               Pl => base_1+"ды"
                             } ;
                        P3 => table {
                               Sg => base_1+"мадым" ;
                               Pl => base_1+"мадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV006"
  } ;

mkV007 : Str -> V ;
mkV007 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"iп жатырмын" ;
                                                                Pl => base_1+"iп жатырмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"iп жатырсың" ;
                                                                Pl => base_1+"iп жатырсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"iп жатырсыз" ;
                                                                Pl => base_1+"iп жатырсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"iп жатыр" ;
                                                                Pl => base_1+"iп жатыр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"iп жатырмаймын" ;
                                                                Pl => base_1+"iп жатырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"iп жатырмайсың" ;
                                                                Pl => base_1+"iп жатырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"iп жатырмайсыз" ;
                                                             Pl => base_1+"iп жатырмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"iп жатырмай" ;
                                                             Pl => base_1+"iп жатырмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"емiн" ;
                                                             Pl => base_1+"емiз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"есiң" ;
                                                             Pl => base_1+"есiңдер"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"есiз" ;
                                                             Pl => base_1+"есiздер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"едi" ;
                                                             Pl => base_1+"едi"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"емеймiн" ;
                                                            Pl => base_1+"емейсiңдер"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"емейсiң" ;
                                                            Pl => base_1+"емейсiздер"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"емейсiз" ;
                                                            Pl => base_1+"емейдi"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"емейдi" ;
                                                            Pl => base_1+"емеймiз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"кенмiн" ;
                                                            Pl => base_1+"кенмiз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"кенсiң" ;
                                                            Pl => base_1+"кенсiңдер"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"кенсiз" ;
                                                                Pl => base_1+"кенсiздер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"кен" ;
                                                                Pl => base_1+"кен"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"пегендiм" ;
                                                                Pl => base_1+"пегендiк"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"пегендың" ;
                                                                Pl => base_1+"пегендыңдер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"пегендыңiз" ;
                                                                Pl => base_1+"пегендыңiздер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"пегендi" ;
                                                                Pl => base_1+"пегендi"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"етiнмiн" ;
                                                             Pl => base_1+"етiнмiз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"етiнсiң" ;
                                                             Pl => base_1+"етiнсiңдер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"етiнсiз" ;
                                                             Pl => base_1+"етiнсiздер"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"етiн" ;
                                                             Pl => base_1+"етiн"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"петiнмiн" ;
                                                             Pl => base_1+"петiнмiз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"петiнсiң" ;
                                                             Pl => base_1+"петiнсiңдер"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"петiнсiз" ;
                                                      Pl => base_1+"петiнсiздер"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"петiн" ;
                                                    Pl => base_1+"петiн"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"тiм" ;
                                                      Pl => base_1+"тiк"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"тiң" ;
                                                    Pl => base_1+"тiңдер"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"тiңiз" ;
                               Pl => base_1+"тiңiздер"
                             } ;
                        P2 => table {
                               Sg => base_1+"тi" ;
                               Pl => base_1+"тi"
                             } ;
                        P3 => table {
                               Sg => base_1+"педiм" ;
                               Pl => base_1+"педiк"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV007"
  } ;

mkV008 : Str -> V ;
mkV008 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"iп тұрмын" ;
                                                                Pl => base_1+"iп тұрмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"iп тұрсың" ;
                                                                Pl => base_1+"iп тұрсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"iп тұрсыз" ;
                                                                Pl => base_1+"iп тұрсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"iп тұр" ;
                                                                Pl => base_1+"iп тұр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"iп тұрмаймын" ;
                                                                Pl => base_1+"iп тұрмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"iп тұрмайсың" ;
                                                                Pl => base_1+"iп тұрмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"iп тұрмайсыз" ;
                                                             Pl => base_1+"iп тұрмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"iп тұрмай" ;
                                                             Pl => base_1+"iп тұрмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"iмiн" ;
                                                             Pl => base_1+"iмiз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"iсiң" ;
                                                             Pl => base_1+"iсiңдер"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"iсiз" ;
                                                             Pl => base_1+"iсiздер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"iдi" ;
                                                             Pl => base_1+"iдi"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"iмеймiн" ;
                                                            Pl => base_1+"iмейсiңдер"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"iмейсiң" ;
                                                            Pl => base_1+"iмейсiздер"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"iмейсiз" ;
                                                            Pl => base_1+"iмейдi"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"iмейдi" ;
                                                            Pl => base_1+"iмеймiз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"iгенмiн" ;
                                                            Pl => base_1+"iгенмiз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"iгенсiң" ;
                                                            Pl => base_1+"iгенсiңдер"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"iгенсiз" ;
                                                                Pl => base_1+"iгенсiздер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"iген" ;
                                                                Pl => base_1+"iген"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"iмегендiм" ;
                                                                Pl => base_1+"iмегендiк"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"iмегендың" ;
                                                                Pl => base_1+"iмегендыңдер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"iмегендыңiз" ;
                                                                Pl => base_1+"iмегендыңiздер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"iмегендi" ;
                                                                Pl => base_1+"iмегендi"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"етiнмiн" ;
                                                             Pl => base_1+"етiнмiз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"етiнсiң" ;
                                                             Pl => base_1+"етiнсiңдер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"етiнсiз" ;
                                                             Pl => base_1+"етiнсiздер"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"етiн" ;
                                                             Pl => base_1+"етiн"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"еметiнмiн" ;
                                                             Pl => base_1+"еметiнмiз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"еметiнсiң" ;
                                                             Pl => base_1+"еметiнсiңдер"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"еметiнсiз" ;
                                                      Pl => base_1+"еметiнсiздер"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"еметiн" ;
                                                    Pl => base_1+"еметiн"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"iдым" ;
                                                      Pl => base_1+"iдық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"iдың" ;
                                                    Pl => base_1+"iдыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"iдыңыз" ;
                               Pl => base_1+"iдыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"iды" ;
                               Pl => base_1+"iды"
                             } ;
                        P3 => table {
                               Sg => base_1+"iмедiм" ;
                               Pl => base_1+"iмедiк"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV008"
  } ;

mkV009 : Str -> V ;
mkV009 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"п тұрмын" ;
                                                                Pl => base_1+"п тұрмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"п тұрсың" ;
                                                                Pl => base_1+"п тұрсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"п тұрсыз" ;
                                                                Pl => base_1+"п тұрсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"п тұр" ;
                                                                Pl => base_1+"п тұр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"п тұрмаймын" ;
                                                                Pl => base_1+"п тұрмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"п тұрмайсың" ;
                                                                Pl => base_1+"п тұрмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"п тұрмайсыз" ;
                                                             Pl => base_1+"п тұрмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"п тұрмай" ;
                                                             Pl => base_1+"п тұрмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ймiн" ;
                                                             Pl => base_1+"ймiз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"йсiң" ;
                                                             Pl => base_1+"йсiңдер"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"йсiз" ;
                                                             Pl => base_1+"йсiздер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"йдi" ;
                                                             Pl => base_1+"йдi"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"ймеймiн" ;
                                                            Pl => base_1+"ймейсiңдер"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ймейсiң" ;
                                                            Pl => base_1+"ймейсiздер"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ймейсiз" ;
                                                            Pl => base_1+"ймейдi"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"ймейдi" ;
                                                            Pl => base_1+"ймеймiз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"генмiн" ;
                                                            Pl => base_1+"генмiз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"генсiң" ;
                                                            Pl => base_1+"генсiңдер"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"генсiз" ;
                                                                Pl => base_1+"генсiздер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ген" ;
                                                                Pl => base_1+"ген"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мегендiм" ;
                                                                Pl => base_1+"мегендiк"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"мегендың" ;
                                                                Pl => base_1+"мегендыңдер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"мегендыңiз" ;
                                                                Pl => base_1+"мегендыңiздер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мегендi" ;
                                                                Pl => base_1+"мегендi"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"йтiнмiн" ;
                                                             Pl => base_1+"йтiнмiз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"йтiнсiң" ;
                                                             Pl => base_1+"йтiнсiңдер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"йтiнсiз" ;
                                                             Pl => base_1+"йтiнсiздер"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"йтiн" ;
                                                             Pl => base_1+"йтiн"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"метiнмiн" ;
                                                             Pl => base_1+"метiнмiз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"метiнсiң" ;
                                                             Pl => base_1+"метiнсiңдер"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"метiнсiз" ;
                                                      Pl => base_1+"метiнсiздер"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"метiн" ;
                                                    Pl => base_1+"метiн"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"дiм" ;
                                                      Pl => base_1+"дiк"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"дiң" ;
                                                    Pl => base_1+"дiңдер"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"дiңiз" ;
                               Pl => base_1+"дiңiздер"
                             } ;
                        P2 => table {
                               Sg => base_1+"дi" ;
                               Pl => base_1+"дi"
                             } ;
                        P3 => table {
                               Sg => base_1+"медiм" ;
                               Pl => base_1+"медiк"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV009"
  } ;

mkV010 : Str -> V ;
mkV010 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"iп тұрмын" ;
                                                                Pl => base_1+"iп тұрмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"iп тұрсың" ;
                                                                Pl => base_1+"iп тұрсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"iп тұрсыз" ;
                                                                Pl => base_1+"iп тұрсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"iп тұр" ;
                                                                Pl => base_1+"iп тұр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"iп тұрмаймын" ;
                                                                Pl => base_1+"iп тұрмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"iп тұрмайсың" ;
                                                                Pl => base_1+"iп тұрмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"iп тұрмайсыз" ;
                                                             Pl => base_1+"iп тұрмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"iп тұрмай" ;
                                                             Pl => base_1+"iп тұрмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"емiн" ;
                                                             Pl => base_1+"емiз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"есiң" ;
                                                             Pl => base_1+"есiңдер"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"есiз" ;
                                                             Pl => base_1+"есiздер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"едi" ;
                                                             Pl => base_1+"едi"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"емеймiн" ;
                                                            Pl => base_1+"емейсiңдер"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"емейсiң" ;
                                                            Pl => base_1+"емейсiздер"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"емейсiз" ;
                                                            Pl => base_1+"емейдi"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"емейдi" ;
                                                            Pl => base_1+"емеймiз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"кенмiн" ;
                                                            Pl => base_1+"кенмiз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"кенсiң" ;
                                                            Pl => base_1+"кенсiңдер"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"кенсiз" ;
                                                                Pl => base_1+"кенсiздер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"кен" ;
                                                                Pl => base_1+"кен"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"пегендiм" ;
                                                                Pl => base_1+"пегендiк"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"пегендың" ;
                                                                Pl => base_1+"пегендыңдер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"пегендыңiз" ;
                                                                Pl => base_1+"пегендыңiздер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"пегендi" ;
                                                                Pl => base_1+"пегендi"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"етiнмiн" ;
                                                             Pl => base_1+"етiнмiз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"етiнсiң" ;
                                                             Pl => base_1+"етiнсiңдер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"етiнсiз" ;
                                                             Pl => base_1+"етiнсiздер"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"етiн" ;
                                                             Pl => base_1+"етiн"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"петiнмiн" ;
                                                             Pl => base_1+"петiнмiз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"петiнсiң" ;
                                                             Pl => base_1+"петiнсiңдер"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"петiнсiз" ;
                                                      Pl => base_1+"петiнсiздер"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"петiн" ;
                                                    Pl => base_1+"петiн"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"тiм" ;
                                                      Pl => base_1+"тiк"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"тiң" ;
                                                    Pl => base_1+"тiңдер"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"тiңiз" ;
                               Pl => base_1+"тiңiздер"
                             } ;
                        P2 => table {
                               Sg => base_1+"тi" ;
                               Pl => base_1+"тi"
                             } ;
                        P3 => table {
                               Sg => base_1+"педiм" ;
                               Pl => base_1+"педiк"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV010"
  } ;

mkV011 : Str -> V ;
mkV011 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"п жүрмiн" ;
                                                                Pl => base_1+"п жүрмiз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"п жүрсiң" ;
                                                                Pl => base_1+"п жүрсiңдер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"п жүрсiз" ;
                                                                Pl => base_1+"п жүрсiздер"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"п жүр" ;
                                                                Pl => base_1+"п жүр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"п жүрмеймын" ;
                                                                Pl => base_1+"п жүрсiздермыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"п жүрмейсiң" ;
                                                                Pl => base_1+"п жүрмейсiңдер"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"п жүрмейсiз" ;
                                                             Pl => base_1+"п жүрмейсiздер"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"п жүрмей" ;
                                                             Pl => base_1+"п жүрмей"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ймын" ;
                                                             Pl => base_1+"ймыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"йсың" ;
                                                             Pl => base_1+"йсыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"йсыз" ;
                                                             Pl => base_1+"йсыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"йды" ;
                                                             Pl => base_1+"йды"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"ймаймын" ;
                                                            Pl => base_1+"ймайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ймайсың" ;
                                                            Pl => base_1+"ймайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ймайсыз" ;
                                                            Pl => base_1+"ймайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"ймайды" ;
                                                            Pl => base_1+"ймаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ғанмын" ;
                                                            Pl => base_1+"ғанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ғансың" ;
                                                            Pl => base_1+"ғансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ғансыз" ;
                                                                Pl => base_1+"ғансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ған" ;
                                                                Pl => base_1+"ған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағандым" ;
                                                                Pl => base_1+"мағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"мағандың" ;
                                                                Pl => base_1+"мағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"мағандыңыз" ;
                                                                Pl => base_1+"мағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағанды" ;
                                                                Pl => base_1+"мағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"йтынмын" ;
                                                             Pl => base_1+"йтынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"йтынсың" ;
                                                             Pl => base_1+"йтынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"йтынсыз" ;
                                                             Pl => base_1+"йтынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"йтын" ;
                                                             Pl => base_1+"йтын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"матынмын" ;
                                                             Pl => base_1+"матынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"матынсың" ;
                                                             Pl => base_1+"матынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"матынсыз" ;
                                                      Pl => base_1+"матынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"матын" ;
                                                    Pl => base_1+"матын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"дым" ;
                                                      Pl => base_1+"дық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"дың" ;
                                                    Pl => base_1+"дыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"дыңыз" ;
                               Pl => base_1+"дыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"ды" ;
                               Pl => base_1+"ды"
                             } ;
                        P3 => table {
                               Sg => base_1+"мадым" ;
                               Pl => base_1+"мадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV011"
  } ;

mkV012 : Str -> V ;
mkV012 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ып отырмын" ;
                                                                Pl => base_1+"ып отырмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ып отырсың" ;
                                                                Pl => base_1+"ып отырсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ып отырсыз" ;
                                                                Pl => base_1+"ып отырсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"ып отыр" ;
                                                                Pl => base_1+"ып отыр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ып отырмаймын" ;
                                                                Pl => base_1+"ып отырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ып отырмайсың" ;
                                                                Pl => base_1+"ып отырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"ып отырмайсыз" ;
                                                             Pl => base_1+"ып отырмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"ып отырмай" ;
                                                             Pl => base_1+"ып отырмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"имын" ;
                                                             Pl => base_1+"имыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"исың" ;
                                                             Pl => base_1+"исыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"исыз" ;
                                                             Pl => base_1+"исыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"иды" ;
                                                             Pl => base_1+"иды"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"имаймын" ;
                                                            Pl => base_1+"имайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"имайсың" ;
                                                            Pl => base_1+"имайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"имайсыз" ;
                                                            Pl => base_1+"имайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"имайды" ;
                                                            Pl => base_1+"имаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ығанмын" ;
                                                            Pl => base_1+"ығанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ығансың" ;
                                                            Pl => base_1+"ығансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ығансыз" ;
                                                                Pl => base_1+"ығансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ыған" ;
                                                                Pl => base_1+"ыған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ымадым" ;
                                                                Pl => base_1+"ымағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"ымадың" ;
                                                                Pl => base_1+"ымадыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ымадыңыз" ;
                                                                Pl => base_1+"ымадыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ымағанды" ;
                                                                Pl => base_1+"ымағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"атынмын" ;
                                                             Pl => base_1+"атынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"атынсың" ;
                                                             Pl => base_1+"атынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"атынсыз" ;
                                                             Pl => base_1+"атынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"атын" ;
                                                             Pl => base_1+"атын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"патынмын" ;
                                                             Pl => base_1+"патынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"патынсың" ;
                                                             Pl => base_1+"патынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"патынсыз" ;
                                                      Pl => base_1+"патынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"патын" ;
                                                    Pl => base_1+"патын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"ыдым" ;
                                                      Pl => base_1+"ыдық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"ыдың" ;
                                                    Pl => base_1+"ыдыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"ыдыңыз" ;
                               Pl => base_1+"ыдыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"ыды" ;
                               Pl => base_1+"ыды"
                             } ;
                        P3 => table {
                               Sg => base_1+"ымадым" ;
                               Pl => base_1+"ымадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV012"
  } ;

mkV013 : Str -> V ;
mkV013 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"мын" ;
                                                                Pl => base_1+"мыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"сың" ;
                                                                Pl => base_1+"сыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"сыз" ;
                                                                Pl => base_1+"сыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1 ;
                                                                Pl => base_1
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"маймын" ;
                                                                Pl => base_1+"маймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"майсың" ;
                                                                Pl => base_1+"майсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"майсыз" ;
                                                             Pl => base_1+"майсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"май" ;
                                                             Pl => base_1+"май"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"амын" ;
                                                             Pl => base_1+"амыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"асың" ;
                                                             Pl => base_1+"асыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"асыз" ;
                                                             Pl => base_1+"асыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ады" ;
                                                             Pl => base_1+"ады"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"амаймын" ;
                                                            Pl => base_1+"амайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"амайсың" ;
                                                            Pl => base_1+"амайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"амайсыз" ;
                                                            Pl => base_1+"амайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"амайды" ;
                                                            Pl => base_1+"амаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ғанмын" ;
                                                            Pl => base_1+"ғанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ғансың" ;
                                                            Pl => base_1+"ғансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ғансыз" ;
                                                                Pl => base_1+"ғансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ған" ;
                                                                Pl => base_1+"ған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағандым" ;
                                                                Pl => base_1+"мағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                 Sg => base_1+"мағандың" ;
                                                                 Pl => base_1+"мағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"мағандыңыз" ;
                                                                Pl => base_1+"мағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағанды" ;
                                                                Pl => base_1+"мағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                              Sg => base_1+"атынмын" ;
                                                              Pl => base_1+"атынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"атынсың" ;
                                                             Pl => base_1+"атынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"атынсыз" ;
                                                             Pl => base_1+"атынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                              Sg => base_1+"атын" ;
                                                              Pl => base_1+"атын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"матынмын" ;
                                                             Pl => base_1+"матынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"матынсың" ;
                                                             Pl => base_1+"матынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"матынсыз" ;
                                                      Pl => base_1+"матынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"матын" ;
                                                    Pl => base_1+"матын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"дым" ;
                                                      Pl => base_1+"дық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"дың" ;
                                                    Pl => base_1+"дыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                                Sg => base_1+"дыңыз" ;
                                Pl => base_1+"дыңыздар"
                              } ;
                        P2 => table {
                               Sg => base_1+"ды" ;
                               Pl => base_1+"ды"
                             } ;
                        P3 => table {
                               Sg => base_1+"мадым" ;
                               Pl => base_1+"мадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV013"
  } ;

mkV014 : Str -> V ;
mkV014 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                 Sg => base_1+"амын" ;
                                                                 Pl => base_1+"амыз"
                                                               } ;
                                                         P2 => table {
                                                                Sg => base_1+"асың" ;
                                                                Pl => base_1+"арсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"асыз" ;
                                                                Pl => base_1+"асыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"ады" ;
                                                                Pl => base_1+"ады"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"амаймын" ;
                                                                Pl => base_1+"амаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"амайсың" ;
                                                                Pl => base_1+"армайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"амайсыз" ;
                                                             Pl => base_1+"амайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"амайды" ;
                                                             Pl => base_1+"амайды"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"амын" ;
                                                             Pl => base_1+"амыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"асың" ;
                                                             Pl => base_1+"асыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"асыз" ;
                                                             Pl => base_1+"асыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ады" ;
                                                             Pl => base_1+"ады"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"амаймын" ;
                                                            Pl => base_1+"амайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"амайсың" ;
                                                            Pl => base_1+"амайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"амайсыз" ;
                                                            Pl => base_1+"амайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"амайды" ;
                                                            Pl => base_1+"амаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ғанмын" ;
                                                            Pl => base_1+"ғанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ғансың" ;
                                                            Pl => base_1+"ғансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ғансыз" ;
                                                                Pl => base_1+"ғансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ған" ;
                                                                Pl => base_1+"ған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағандым" ;
                                                                Pl => base_1+"мағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"мағандың" ;
                                                                Pl => base_1+"мағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"мағандыңыз" ;
                                                                Pl => base_1+"мағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағанды" ;
                                                                Pl => base_1+"мағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"атынмын" ;
                                                             Pl => base_1+"атынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"атынсың" ;
                                                             Pl => base_1+"атынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"атынсыз" ;
                                                             Pl => base_1+"атынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"атын" ;
                                                             Pl => base_1+"атын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"матынмын" ;
                                                             Pl => base_1+"матынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"матынсың" ;
                                                             Pl => base_1+"матынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"матынсыз" ;
                                                      Pl => base_1+"матынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"матын" ;
                                                    Pl => base_1+"матын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"дым" ;
                                                      Pl => base_1+"дық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"дың" ;
                                                    Pl => base_1+"дыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"дыңыз" ;
                               Pl => base_1+"дыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"ды" ;
                               Pl => base_1+"ды"
                             } ;
                        P3 => table {
                               Sg => base_1+"мадым" ;
                               Pl => base_1+"мадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV014"
  } ;

mkV015 : Str -> V ;
mkV015 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"а жатырмын" ;
                                                                Pl => base_1+"а жатырмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"а жатырсың" ;
                                                                Pl => base_1+"а жатырсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"а жатырсыз" ;
                                                                Pl => base_1+"а жатырсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"а жатыр" ;
                                                                Pl => base_1+"а жатыр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"а жатырмаймын" ;
                                                                Pl => base_1+"а жатырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"а жатырмайсың" ;
                                                                Pl => base_1+"а жатырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"а жатырмайсыз" ;
                                                             Pl => base_1+"а жатырмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"а жатырмай" ;
                                                             Pl => base_1+"а жатырмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"амын" ;
                                                             Pl => base_1+"амыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"асың" ;
                                                             Pl => base_1+"асыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"асыз" ;
                                                             Pl => base_1+"асыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ады" ;
                                                             Pl => base_1+"ады"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"амаймын" ;
                                                            Pl => base_1+"амайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"амайсың" ;
                                                            Pl => base_1+"амайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"амайсыз" ;
                                                            Pl => base_1+"амайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"амайды" ;
                                                            Pl => base_1+"амаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ғанмын" ;
                                                            Pl => base_1+"ғанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ғансың" ;
                                                            Pl => base_1+"ғансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ғансыз" ;
                                                                Pl => base_1+"ғансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ған" ;
                                                                Pl => base_1+"ған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағандым" ;
                                                                Pl => base_1+"мағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"мағандың" ;
                                                                Pl => base_1+"мағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"мағандыңыз" ;
                                                                Pl => base_1+"мағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағанды" ;
                                                                Pl => base_1+"мағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"атынмын" ;
                                                             Pl => base_1+"атынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"атынсың" ;
                                                             Pl => base_1+"атынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"атынсыз" ;
                                                             Pl => base_1+"атынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"атын" ;
                                                             Pl => base_1+"атын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"матынмын" ;
                                                             Pl => base_1+"матынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"матынсың" ;
                                                             Pl => base_1+"матынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"матынсыз" ;
                                                      Pl => base_1+"матынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"матын" ;
                                                    Pl => base_1+"матын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"дым" ;
                                                      Pl => base_1+"дық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"дың" ;
                                                    Pl => base_1+"дыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"дыңыз" ;
                               Pl => base_1+"дыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"ды" ;
                               Pl => base_1+"ды"
                             } ;
                        P3 => table {
                               Sg => base_1+"мадым" ;
                               Pl => base_1+"мадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV015"
  } ;

mkV016 : Str -> V ;
mkV016 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"iп тұрмын" ;
                                                                Pl => base_1+"iп тұрмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"iп тұрсың" ;
                                                                Pl => base_1+"iп тұрсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"iп тұрсыз" ;
                                                                Pl => base_1+"iп тұрсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"iп тұр" ;
                                                                Pl => base_1+"iп тұр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"iп тұрмаймын" ;
                                                                Pl => base_1+"iп тұрмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"iп тұрмайсың" ;
                                                                Pl => base_1+"iп тұрмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"iп тұрмайсыз" ;
                                                             Pl => base_1+"iп тұрмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"iп тұрмай" ;
                                                             Pl => base_1+"iп тұрмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"емiн" ;
                                                             Pl => base_1+"емiз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"есiң" ;
                                                             Pl => base_1+"есiңдер"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"есiз" ;
                                                             Pl => base_1+"есiздер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"едi" ;
                                                             Pl => base_1+"едi"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"емеймiн" ;
                                                            Pl => base_1+"емейсiңдер"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"емейсiң" ;
                                                            Pl => base_1+"емейсiздер"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"емейсiз" ;
                                                            Pl => base_1+"емейдi"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"емейдi" ;
                                                            Pl => base_1+"емеймiз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"генмiн" ;
                                                            Pl => base_1+"генмiз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"генсiң" ;
                                                            Pl => base_1+"генсiңдер"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"генсiз" ;
                                                                Pl => base_1+"генсiздер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ген" ;
                                                                Pl => base_1+"ген"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мегендiм" ;
                                                                Pl => base_1+"мегендiк"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"мегендың" ;
                                                                Pl => base_1+"мегендыңдер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"мегендыңiз" ;
                                                                Pl => base_1+"мегендыңiздер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мегендi" ;
                                                                Pl => base_1+"мегендi"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"етiнмiн" ;
                                                             Pl => base_1+"етiнмiз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"етiнсiң" ;
                                                             Pl => base_1+"етiнсiңдер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"етiнсiз" ;
                                                             Pl => base_1+"етiнсiздер"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"етiн" ;
                                                             Pl => base_1+"етiн"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"метiнмiн" ;
                                                             Pl => base_1+"метiнмiз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"метiнсiң" ;
                                                             Pl => base_1+"метiнсiңдер"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"метiнсiз" ;
                                                      Pl => base_1+"метiнсiздер"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"метiн" ;
                                                    Pl => base_1+"метiн"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"дiм" ;
                                                      Pl => base_1+"дiк"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"дiң" ;
                                                    Pl => base_1+"дiңдер"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"дiңiз" ;
                               Pl => base_1+"дiңiздер"
                             } ;
                        P2 => table {
                               Sg => base_1+"дi" ;
                               Pl => base_1+"дi"
                             } ;
                        P3 => table {
                               Sg => base_1+"медiм" ;
                               Pl => base_1+"медiк"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV016"
  } ;

mkV017 : Str -> V ;
mkV017 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ып тұрмын" ;
                                                                Pl => base_1+"ып тұр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ып тұрсың" ;
                                                                Pl => base_1+"ып тұр"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ып тұрсыз" ;
                                                                Pl => base_1+"ып тұр"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"ып тұр" ;
                                                                Pl => base_1+"ып тұр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ып тұрмай" ;
                                                                Pl => base_1+"ып тұрмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ып тұрмай" ;
                                                                Pl => base_1+"ып тұрмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"ып тұрмай" ;
                                                             Pl => base_1+"ып тұрмай"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"ып тұрмай" ;
                                                             Pl => base_1+"ып тұрмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ады" ;
                                                             Pl => base_1+"амыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"асың" ;
                                                             Pl => base_1+"асыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"асыз" ;
                                                             Pl => base_1+"асыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ады" ;
                                                             Pl => base_1+"ады"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"амайды" ;
                                                            Pl => base_1+"амайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"амайды" ;
                                                            Pl => base_1+"амайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"амайды" ;
                                                            Pl => base_1+"амайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"амайды" ;
                                                            Pl => base_1+"амайды"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ғанмын" ;
                                                            Pl => base_1+"ғанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ғансың" ;
                                                            Pl => base_1+"ғансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ғансыз" ;
                                                                Pl => base_1+"ғансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ған" ;
                                                                Pl => base_1+"ған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағандым" ;
                                                                Pl => base_1+"мағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"мағандың" ;
                                                                Pl => base_1+"мағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"мағандыңыз" ;
                                                                Pl => base_1+"мағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағанды" ;
                                                                Pl => base_1+"мағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"атынмын" ;
                                                             Pl => base_1+"атынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"атынсың" ;
                                                             Pl => base_1+"атынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"атынсыз" ;
                                                             Pl => base_1+"атынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"атын" ;
                                                             Pl => base_1+"атын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"матынмын" ;
                                                             Pl => base_1+"матынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"матынсың" ;
                                                             Pl => base_1+"матынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"матынсыз" ;
                                                      Pl => base_1+"матынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"матын" ;
                                                    Pl => base_1+"матын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"дым" ;
                                                      Pl => base_1+"дық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"дың" ;
                                                    Pl => base_1+"ды"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"дыңыз" ;
                               Pl => base_1+"ды"
                             } ;
                        P2 => table {
                               Sg => base_1+"ды" ;
                               Pl => base_1+"ды"
                             } ;
                        P3 => table {
                               Sg => base_1+"мадым" ;
                               Pl => base_1+"мадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV017"
  } ;

mkV018 : Str -> V ;
mkV018 base =
  case base of {
    base_1+"ып"+base_2@?+"қ"+base_3@?+"лу" => lin V
      { Infinitive = base_1+"ып"+base_2+"қ"+base_3+"лу" ;
        Indicative = { Fut = nonExist ;
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"а"+base_2+"ж"+base_3+"тырмын" ;
                                                                Pl => base_1+"а"+base_2+"ж"+base_3+"тырмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"а"+base_2+"ж"+base_3+"тырсың" ;
                                                                Pl => base_1+"а"+base_2+"ж"+base_3+"тырсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"а"+base_2+"ж"+base_3+"тырсыз" ;
                                                                Pl => base_1+"а"+base_2+"ж"+base_3+"тырсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"а"+base_2+"ж"+base_3+"тыр" ;
                                                                Pl => base_1+"а"+base_2+"ж"+base_3+"тыр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"а"+base_2+"ж"+base_3+"тырмаймын" ;
                                                                Pl => base_1+"а"+base_2+"ж"+base_3+"тырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"а"+base_2+"ж"+base_3+"тырмайсың" ;
                                                                Pl => base_1+"а"+base_2+"ж"+base_3+"тырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"ып"+base_2+"қ"+base_3+"лып тұрмайсыз" ;
                                                             Pl => base_1+"а"+base_2+"ж"+base_3+"тырмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"а"+base_2+"ж"+base_3+"тырмай" ;
                                                             Pl => base_1+"а"+base_2+"ж"+base_3+"тырмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ып"+base_2+"қ"+base_3+"ламын" ;
                                                             Pl => base_1+"ып"+base_2+"қ"+base_3+"ламыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"ып"+base_2+"қ"+base_3+"ласың" ;
                                                             Pl => base_1+"ып"+base_2+"қ"+base_3+"ласыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"ып"+base_2+"қ"+base_3+"ласыз" ;
                                                             Pl => base_1+"ып"+base_2+"қ"+base_3+"ласыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ып"+base_2+"қ"+base_3+"лады" ;
                                                             Pl => base_1+"ып"+base_2+"қ"+base_3+"лады"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"ып"+base_2+"қ"+base_3+"ламаймын" ;
                                                            Pl => base_1+"ып"+base_2+"қ"+base_3+"ламайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ып"+base_2+"қ"+base_3+"ламайсың" ;
                                                            Pl => base_1+"ып"+base_2+"қ"+base_3+"ламайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ып"+base_2+"қ"+base_3+"ламайсыз" ;
                                                            Pl => base_1+"ып"+base_2+"қ"+base_3+"ламайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"ып"+base_2+"қ"+base_3+"ламайды" ;
                                                            Pl => base_1+"ып"+base_2+"қ"+base_3+"ламаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ып"+base_2+"қ"+base_3+"лғанмын" ;
                                                            Pl => base_1+"ып"+base_2+"қ"+base_3+"лғанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ып"+base_2+"қ"+base_3+"лғансың" ;
                                                            Pl => base_1+"ып"+base_2+"қ"+base_3+"лғансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ып"+base_2+"қ"+base_3+"лғансыз" ;
                                                                Pl => base_1+"ып"+base_2+"қ"+base_3+"лғансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ып"+base_2+"қ"+base_3+"лған" ;
                                                                Pl => base_1+"ып"+base_2+"қ"+base_3+"лған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ып"+base_2+"қ"+base_3+"лмағандым" ;
                                                                Pl => base_1+"ып"+base_2+"қ"+base_3+"лмағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"ып"+base_2+"қ"+base_3+"лмағандың" ;
                                                                Pl => base_1+"ып"+base_2+"қ"+base_3+"лмағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ып"+base_2+"қ"+base_3+"лмағандыңыз" ;
                                                                Pl => base_1+"ып"+base_2+"қ"+base_3+"лмағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ып"+base_2+"қ"+base_3+"лмағанды" ;
                                                                Pl => base_1+"ып"+base_2+"қ"+base_3+"лмағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"ып"+base_2+"қ"+base_3+"латынмын" ;
                                                             Pl => base_1+"ып"+base_2+"қ"+base_3+"латынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"ып"+base_2+"қ"+base_3+"латынсың" ;
                                                             Pl => base_1+"ып"+base_2+"қ"+base_3+"латынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ып"+base_2+"қ"+base_3+"латынсыз" ;
                                                             Pl => base_1+"ып"+base_2+"қ"+base_3+"латынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"ып"+base_2+"қ"+base_3+"латын" ;
                                                             Pl => base_1+"ып"+base_2+"қ"+base_3+"латын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"ып"+base_2+"қ"+base_3+"лматынмын" ;
                                                             Pl => base_1+"ып"+base_2+"қ"+base_3+"лматынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ып"+base_2+"қ"+base_3+"лматынсың" ;
                                                             Pl => base_1+"ып"+base_2+"қ"+base_3+"лматынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"ып"+base_2+"қ"+base_3+"лматынсыз" ;
                                                      Pl => base_1+"ып"+base_2+"қ"+base_3+"лматынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"ып"+base_2+"қ"+base_3+"лматын" ;
                                                    Pl => base_1+"ып"+base_2+"қ"+base_3+"лматын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"ып"+base_2+"қ"+base_3+"лдым" ;
                                                      Pl => base_1+"ып"+base_2+"қ"+base_3+"лдық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"ып"+base_2+"қ"+base_3+"лдың" ;
                                                    Pl => base_1+"ып"+base_2+"қ"+base_3+"лдыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"ып"+base_2+"қ"+base_3+"лдыңыз" ;
                               Pl => base_1+"ып"+base_2+"қ"+base_3+"лдыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"ып"+base_2+"қ"+base_3+"лды" ;
                               Pl => base_1+"ып"+base_2+"қ"+base_3+"лды"
                             } ;
                        P3 => table {
                               Sg => base_1+"ып"+base_2+"қ"+base_3+"лмадым" ;
                               Pl => base_1+"ып"+base_2+"қ"+base_3+"лмадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV018"
  } ;

mkV019 : Str -> V ;
mkV019 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"п жүрмiн" ;
                                                                Pl => base_1+"п жүрмiз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"п жүрсiң" ;
                                                                Pl => base_1+"п жүрсiңдер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"п жүрсiз" ;
                                                                Pl => base_1+"п жүрсiздер"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"п жүр" ;
                                                                Pl => base_1+"п жүр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"п жүрмеймын" ;
                                                                Pl => base_1+"п жүрсiздермыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"п жүрмейсiң" ;
                                                                Pl => base_1+"п жүрмейсiңдер"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"п жүрмейсiз" ;
                                                             Pl => base_1+"п жүрмейсiздер"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"п жүрмей" ;
                                                             Pl => base_1+"п жүрмей"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ймiн" ;
                                                             Pl => base_1+"ймiз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"йсiң" ;
                                                             Pl => base_1+"йсiңдер"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"йсiз" ;
                                                             Pl => base_1+"йсiздер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"йдi" ;
                                                             Pl => base_1+"йдi"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"ймеймiн" ;
                                                            Pl => base_1+"ймейсiңдер"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ймейсiң" ;
                                                            Pl => base_1+"ймейсiздер"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ймейсiз" ;
                                                            Pl => base_1+"ймейдi"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"ймейдi" ;
                                                            Pl => base_1+"ймеймiз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"генмiн" ;
                                                            Pl => base_1+"генмiз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"генсiң" ;
                                                            Pl => base_1+"генсiңдер"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"генсiз" ;
                                                                Pl => base_1+"генсiздер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ген" ;
                                                                Pl => base_1+"ген"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мегендiм" ;
                                                                Pl => base_1+"мегендiк"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"мегендың" ;
                                                                Pl => base_1+"мегендыңдер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"мегендыңiз" ;
                                                                Pl => base_1+"мегендыңiздер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мегендi" ;
                                                                Pl => base_1+"мегендi"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"йтiнмiн" ;
                                                             Pl => base_1+"йтiнмiз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"йтiнсiң" ;
                                                             Pl => base_1+"йтiнсiңдер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"йтiнсiз" ;
                                                             Pl => base_1+"йтiнсiздер"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"йтiн" ;
                                                             Pl => base_1+"йтiн"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"метiнмiн" ;
                                                             Pl => base_1+"метiнмiз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"метiнсiң" ;
                                                             Pl => base_1+"метiнсiңдер"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"метiнсiз" ;
                                                      Pl => base_1+"метiнсiздер"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"метiн" ;
                                                    Pl => base_1+"метiн"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"дiм" ;
                                                      Pl => base_1+"дiк"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"дiң" ;
                                                    Pl => base_1+"дiңдер"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"дiңiз" ;
                               Pl => base_1+"дiңiздер"
                             } ;
                        P2 => table {
                               Sg => base_1+"дi" ;
                               Pl => base_1+"дi"
                             } ;
                        P3 => table {
                               Sg => base_1+"медiм" ;
                               Pl => base_1+"медiк"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV019"
  } ;

mkV020 : Str -> V ;
mkV020 base =
  case base of {
    base_1+"бу" => lin V
      { Infinitive = base_1+"бу" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ып отырмын" ;
                                                                Pl => base_1+"ып отырмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ып отырсың" ;
                                                                Pl => base_1+"ып отырсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ып отырсыз" ;
                                                                Pl => base_1+"ып отырсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"ып отыр" ;
                                                                Pl => base_1+"ып отыр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ып отырмаймын" ;
                                                                Pl => base_1+"ып отырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ып отырмайсың" ;
                                                                Pl => base_1+"ып отырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"ып отырмайсыз" ;
                                                             Pl => base_1+"ып отырмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"ып отырмай" ;
                                                             Pl => base_1+"ып отырмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"бамын" ;
                                                             Pl => base_1+"бамыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"басың" ;
                                                             Pl => base_1+"басыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"басыз" ;
                                                             Pl => base_1+"басыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"бады" ;
                                                             Pl => base_1+"бады"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"бамаймын" ;
                                                            Pl => base_1+"бамайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"бамайсың" ;
                                                            Pl => base_1+"бамайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"бамайсыз" ;
                                                            Pl => base_1+"бамайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"бамайды" ;
                                                            Pl => base_1+"бамаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"пқанмын" ;
                                                            Pl => base_1+"пқанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"пқансың" ;
                                                            Pl => base_1+"пқансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"пқансыз" ;
                                                                Pl => base_1+"пқансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"пқан" ;
                                                                Pl => base_1+"пқан"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ппағандым" ;
                                                                Pl => base_1+"ппағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"ппағандың" ;
                                                                Pl => base_1+"ппағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ппағандыңыз" ;
                                                                Pl => base_1+"ппағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ппағанды" ;
                                                                Pl => base_1+"ппағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"батынмын" ;
                                                             Pl => base_1+"батынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"батынсың" ;
                                                             Pl => base_1+"батынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"батынсыз" ;
                                                             Pl => base_1+"батынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"батын" ;
                                                             Pl => base_1+"батын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"ппатынмын" ;
                                                             Pl => base_1+"ппатынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ппатынсың" ;
                                                             Pl => base_1+"ппатынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"ппатынсыз" ;
                                                      Pl => base_1+"ппатынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"ппатын" ;
                                                    Pl => base_1+"ппатын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"птым" ;
                                                      Pl => base_1+"птық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"птың" ;
                                                    Pl => base_1+"птыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"птыңыз" ;
                               Pl => base_1+"птыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"пты" ;
                               Pl => base_1+"пты"
                             } ;
                        P3 => table {
                               Sg => base_1+"ппадым" ;
                               Pl => base_1+"ппадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV020"
  } ;

mkV021 : Str -> V ;
mkV021 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ып жатырмын" ;
                                                                Pl => base_1+"ып жатырмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ып жатырсың" ;
                                                                Pl => base_1+"ып жатырсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ып жатырсыз" ;
                                                                Pl => base_1+"ып жатырсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"ып жатыр" ;
                                                                Pl => base_1+"ып жатыр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ып жатырмаймын" ;
                                                                Pl => base_1+"ып жатырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ып жатырмайсың" ;
                                                                Pl => base_1+"ып жатырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"ып жатырмайсыз" ;
                                                             Pl => base_1+"ып жатырмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"ып жатырмай" ;
                                                             Pl => base_1+"ып жатырмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"амын" ;
                                                             Pl => base_1+"амыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"асың" ;
                                                             Pl => base_1+"асыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"асыз" ;
                                                             Pl => base_1+"асыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ады" ;
                                                             Pl => base_1+"ады"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"амаймын" ;
                                                            Pl => base_1+"амайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"амайсың" ;
                                                            Pl => base_1+"амайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"амайсыз" ;
                                                            Pl => base_1+"амайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"амайды" ;
                                                            Pl => base_1+"амаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ғанмын" ;
                                                            Pl => base_1+"ғанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ғансың" ;
                                                            Pl => base_1+"ғансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ғансыз" ;
                                                                Pl => base_1+"ғансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ған" ;
                                                                Pl => base_1+"ған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"бағандым" ;
                                                                Pl => base_1+"бағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"бағандың" ;
                                                                Pl => base_1+"бағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"бағандыңыз" ;
                                                                Pl => base_1+"бағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"бағанды" ;
                                                                Pl => base_1+"бағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"атынмын" ;
                                                             Pl => base_1+"атынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"атынсың" ;
                                                             Pl => base_1+"атынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"атынсыз" ;
                                                             Pl => base_1+"атынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"атын" ;
                                                             Pl => base_1+"атын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"батынмын" ;
                                                             Pl => base_1+"батынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"батынсың" ;
                                                             Pl => base_1+"батынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"батынсыз" ;
                                                      Pl => base_1+"батынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"батын" ;
                                                    Pl => base_1+"батын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"дым" ;
                                                      Pl => base_1+"дық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"дың" ;
                                                    Pl => base_1+"дыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"дыңыз" ;
                               Pl => base_1+"дыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"ды" ;
                               Pl => base_1+"ды"
                             } ;
                        P3 => table {
                               Sg => base_1+"бадым" ;
                               Pl => base_1+"бадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV021"
  } ;

mkV022 : Str -> V ;
mkV022 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ырмын" ;
                                                                Pl => base_1+"ырмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ырсың" ;
                                                                Pl => base_1+"ырсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ырсыз" ;
                                                                Pl => base_1+"ырсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"ыр" ;
                                                                Pl => base_1+"ыр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ырмаймын" ;
                                                                Pl => base_1+"ырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ырмайсың" ;
                                                                Pl => base_1+"ырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"ырмайсыз" ;
                                                             Pl => base_1+"ырмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"ырмай" ;
                                                             Pl => base_1+"ырмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"амын" ;
                                                             Pl => base_1+"амыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"асың" ;
                                                             Pl => base_1+"асыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"асыз" ;
                                                             Pl => base_1+"асыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ады" ;
                                                             Pl => base_1+"ады"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"амаймын" ;
                                                            Pl => base_1+"амайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"амайсың" ;
                                                            Pl => base_1+"амайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"амайсыз" ;
                                                            Pl => base_1+"амайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"амайды" ;
                                                            Pl => base_1+"амаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"қанмын" ;
                                                            Pl => base_1+"қанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"қансың" ;
                                                            Pl => base_1+"қансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"қансыз" ;
                                                                Pl => base_1+"қансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"қан" ;
                                                                Pl => base_1+"қан"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"пағандым" ;
                                                                Pl => base_1+"пағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"пағандың" ;
                                                                Pl => base_1+"пағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"пағандыңыз" ;
                                                                Pl => base_1+"пағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"пағанды" ;
                                                                Pl => base_1+"пағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"атынмын" ;
                                                             Pl => base_1+"атынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"атынсың" ;
                                                             Pl => base_1+"атынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"атынсыз" ;
                                                             Pl => base_1+"атынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"атын" ;
                                                             Pl => base_1+"атын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"патынмын" ;
                                                             Pl => base_1+"патынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"патынсың" ;
                                                             Pl => base_1+"патынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"патынсыз" ;
                                                      Pl => base_1+"патынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"патын" ;
                                                    Pl => base_1+"патын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"тым" ;
                                                      Pl => base_1+"тық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"тың" ;
                                                    Pl => base_1+"тыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"тыңыз" ;
                               Pl => base_1+"тыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"ты" ;
                               Pl => base_1+"ты"
                             } ;
                        P3 => table {
                               Sg => base_1+"падым" ;
                               Pl => base_1+"падық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV022"
  } ;

mkV023 : Str -> V ;
mkV023 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"iп жатырмын" ;
                                                                Pl => base_1+"iп жатырмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"iп жатырсың" ;
                                                                Pl => base_1+"iп жатырсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"iп жатырсыз" ;
                                                                Pl => base_1+"iп жатырсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"iп жатыр" ;
                                                                Pl => base_1+"iп жатыр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"iп жатырмаймын" ;
                                                                Pl => base_1+"iп жатырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"iп жатырмайсың" ;
                                                                Pl => base_1+"iп жатырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"iп жатырмайсыз" ;
                                                             Pl => base_1+"iп жатырмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"iп жатырмай" ;
                                                             Pl => base_1+"iп жатырмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"емiн" ;
                                                             Pl => base_1+"емiз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"есiң" ;
                                                             Pl => base_1+"есiңдер"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"есiз" ;
                                                             Pl => base_1+"есiздер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"едi" ;
                                                             Pl => base_1+"едi"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"емеймiн" ;
                                                            Pl => base_1+"емейсiңдер"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"емейсiң" ;
                                                            Pl => base_1+"емейсiздер"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"емейсiз" ;
                                                            Pl => base_1+"емейдi"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"емейдi" ;
                                                            Pl => base_1+"емеймiз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"генмiн" ;
                                                            Pl => base_1+"генмiз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"генсiң" ;
                                                            Pl => base_1+"генсiңдер"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"генсiз" ;
                                                                Pl => base_1+"генсiздер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ген" ;
                                                                Pl => base_1+"ген"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мегендiм" ;
                                                                Pl => base_1+"мегендiк"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"мегендың" ;
                                                                Pl => base_1+"мегендыңдер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"мегендыңiз" ;
                                                                Pl => base_1+"мегендыңiздер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мегендi" ;
                                                                Pl => base_1+"мегендi"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"етiнмiн" ;
                                                             Pl => base_1+"етiнмiз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"етiнсiң" ;
                                                             Pl => base_1+"етiнсiңдер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"етiнсiз" ;
                                                             Pl => base_1+"етiнсiздер"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"етiн" ;
                                                             Pl => base_1+"етiн"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"метiнмiн" ;
                                                             Pl => base_1+"метiнмiз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"метiнсiң" ;
                                                             Pl => base_1+"метiнсiңдер"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"метiнсiз" ;
                                                      Pl => base_1+"метiнсiздер"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"метiн" ;
                                                    Pl => base_1+"метiн"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"дiм" ;
                                                      Pl => base_1+"дiк"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"дiң" ;
                                                    Pl => base_1+"дiңдер"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"дiңiз" ;
                               Pl => base_1+"дiңiздер"
                             } ;
                        P2 => table {
                               Sg => base_1+"дi" ;
                               Pl => base_1+"дi"
                             } ;
                        P3 => table {
                               Sg => base_1+"медiм" ;
                               Pl => base_1+"медiк"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV023"
  } ;

mkV024 : Str -> V ;
mkV024 base =
  case base of {
    "жұ"+base_1+"у" => lin V
      { Infinitive = "жұ"+base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => "жұ"+base_1+"ып жатырмын" ;
                                                                Pl => "жұ"+base_1+"ып жатырмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "жұ"+base_1+"ып жатырсың" ;
                                                                Pl => "жұ"+base_1+"ып жатырсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "жұ"+base_1+"ып жатырсыз" ;
                                                                Pl => "жұ"+base_1+"ып жатырсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => "жұ"+base_1+"ып жатыр" ;
                                                                Pl => "жұ"+base_1+"ып жатыр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "жұ"+base_1+"ып жатырмаймын" ;
                                                                Pl => "жұ"+base_1+"ып жатырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "жұ"+base_1+"ып жатырмайсың" ;
                                                                Pl => "жұ"+base_1+"ып жатырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => "жұ"+base_1+"ып жатырмайсыз" ;
                                                             Pl => "жұ"+base_1+"ып жатырмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "жұ"+base_1+"ып жатырмай" ;
                                                             Pl => "жұ"+base_1+"ып жатырмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "жұ"+base_1+"амын" ;
                                                             Pl => "жұ"+base_1+"амыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => "жұ"+base_1+"асың" ;
                                                             Pl => "жұ"+base_1+"асыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "жұ"+base_1+"асыз" ;
                                                             Pl => "жұ"+base_1+"асыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "жұ"+base_1+"ады" ;
                                                             Pl => "жұ"+base_1+"ады"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => "жұ"+base_1+"амаймын" ;
                                                            Pl => "жұ"+base_1+"амайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => "жұ"+base_1+"амайсың" ;
                                                            Pl => "жұ"+base_1+"амайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => "жұ"+base_1+"амайсыз" ;
                                                            Pl => "жұ"+base_1+"амайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => "жұ"+base_1+"амайды" ;
                                                            Pl => "жұ"+base_1+"амаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => "қыды"+base_1+"ғанмын" ;
                                                            Pl => "қыды"+base_1+"ғанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => "қыды"+base_1+"ғансың" ;
                                                            Pl => "қыды"+base_1+"ғансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => "қыды"+base_1+"ғансыз" ;
                                                                Pl => "қыды"+base_1+"ғансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "қыды"+base_1+"ған" ;
                                                                Pl => "қыды"+base_1+"ған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "қыды"+base_1+"мағандым" ;
                                                                Pl => "қыды"+base_1+"мағандық"

                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => "қыды"+base_1+"мағандың" ;
                                                                Pl => "қыды"+base_1+"мағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "қыды"+base_1+"мағандыңыз" ;
                                                                Pl => "қыды"+base_1+"мағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "қыды"+base_1+"мағанды" ;
                                                                Pl => "қыды"+base_1+"мағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => "жұ"+base_1+"атынмын" ;
                                                             Pl => "жұ"+base_1+"атынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "жұ"+base_1+"атынсың" ;
                                                             Pl => "жұ"+base_1+"атынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "жұ"+base_1+"атынсыз" ;
                                                             Pl => "жұ"+base_1+"атынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => "жұ"+base_1+"атын" ;
                                                             Pl => "жұ"+base_1+"атын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "жұ"+base_1+"матынмын" ;
                                                             Pl => "жұ"+base_1+"матынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "жұ"+base_1+"матынсың" ;
                                                             Pl => "жұ"+base_1+"матынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => "жұ"+base_1+"матынсыз" ;
                                                      Pl => "жұ"+base_1+"матынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => "жұ"+base_1+"матын" ;
                                                    Pl => "жұ"+base_1+"матын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => "жұ"+base_1+"дым" ;
                                                      Pl => "жұ"+base_1+"дық"
                                                    } ;
                                        Formal => table {
                                                    Sg => "жұ"+base_1+"дың" ;
                                                    Pl => "жұ"+base_1+"дыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => "жұ"+base_1+"дыңыз" ;
                               Pl => "жұ"+base_1+"дыңыздар"
                             } ;
                        P2 => table {
                               Sg => "жұ"+base_1+"ды" ;
                               Pl => "жұ"+base_1+"ды"
                             } ;
                        P3 => table {
                               Sg => "жұ"+base_1+"мадым" ;
                               Pl => "жұ"+base_1+"мадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV024"
  } ;

mkV025 : Str -> V ;
mkV025 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"п отырмын" ;
                                                                Pl => base_1+"п отырмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"п отырсың" ;
                                                                Pl => base_1+"п отырсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"п отырсыз" ;
                                                                Pl => base_1+"п отырсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"п отыр" ;
                                                                Pl => base_1+"п отыр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"п отырмаймын" ;
                                                                Pl => base_1+"п отырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"п отырмайсың" ;
                                                                Pl => base_1+"п отырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"п отырмайсыз" ;
                                                             Pl => base_1+"п отырмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"п отырмай" ;
                                                             Pl => base_1+"п отырмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ймiн" ;
                                                             Pl => base_1+"ймiз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"йсiң" ;
                                                             Pl => base_1+"йсiңдер"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"йсiз" ;
                                                             Pl => base_1+"йсiздер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"йдi" ;
                                                             Pl => base_1+"йдi"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"ймеймiн" ;
                                                            Pl => base_1+"ймейсiңдер"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ймейсiң" ;
                                                            Pl => base_1+"ймейсiздер"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ймейсiз" ;
                                                            Pl => base_1+"ймейдi"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"ймейдi" ;
                                                            Pl => base_1+"ймеймiз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"генмiн" ;
                                                            Pl => base_1+"генмiз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"генсiң" ;
                                                            Pl => base_1+"генсiңдер"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"генсiз" ;
                                                                Pl => base_1+"генсiздер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ген" ;
                                                                Pl => base_1+"ген"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мегендiм" ;
                                                                Pl => base_1+"мегендiк"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"мегендың" ;
                                                                Pl => base_1+"мегендыңдер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"мегендыңiз" ;
                                                                Pl => base_1+"мегендыңiздер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мегендi" ;
                                                                Pl => base_1+"мегендi"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"йтiнмiн" ;
                                                             Pl => base_1+"йтiнмiз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"йтiнсiң" ;
                                                             Pl => base_1+"йтiнсiңдер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"йтiнсiз" ;
                                                             Pl => base_1+"йтiнсiздер"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"йтiн" ;
                                                             Pl => base_1+"йтiн"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"метiнмiн" ;
                                                             Pl => base_1+"метiнмiз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"метiнсiң" ;
                                                             Pl => base_1+"метiнсiңдер"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"метiнсiз" ;
                                                      Pl => base_1+"метiнсiздер"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"метiн" ;
                                                    Pl => base_1+"метiн"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"дiм" ;
                                                      Pl => base_1+"дiк"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"дiң" ;
                                                    Pl => base_1+"дiңдер"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"дiңiз" ;
                               Pl => base_1+"дiңiздер"
                             } ;
                        P2 => table {
                               Sg => base_1+"дi" ;
                               Pl => base_1+"дi"
                             } ;
                        P3 => table {
                               Sg => base_1+"медiм" ;
                               Pl => base_1+"медiк"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV025"
  } ;

mkV026 : Str -> V ;
mkV026 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                 Sg => base_1+"п отырмын" ;
                                                                 Pl => base_1+"п отырмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"п отырсың" ;
                                                                Pl => base_1+"п отырсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"п отырсыз" ;
                                                                Pl => base_1+"п отырсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                 Sg => base_1+"п отыр" ;
                                                                 Pl => base_1+"п отыр"
                                                               } ;
                                                         P2 => table {
                                                                Sg => base_1+"п отырмаймын" ;
                                                                Pl => base_1+"п отырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"п отырмайсың" ;
                                                                Pl => base_1+"п отырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                              Sg => base_1+"п отырмайсыз" ;
                                                              Pl => base_1+"п отырмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"п отырмай" ;
                                                             Pl => base_1+"п отырмай"
                                                           } ;
                                                      P3 => table {
                                                              Sg => base_1+"ймын" ;
                                                              Pl => base_1+"ймыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                              Sg => base_1+"йсың" ;
                                                              Pl => base_1+"йсыңдар"
                                                            } ;
                                                      P2 => table {
                                                             Sg => base_1+"йсыз" ;
                                                             Pl => base_1+"йсыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"йды" ;
                                                             Pl => base_1+"йды"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                             Sg => base_1+"ймаймын" ;
                                                             Pl => base_1+"ймайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ймайсың" ;
                                                            Pl => base_1+"ймайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ймайсыз" ;
                                                            Pl => base_1+"ймайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                             Sg => base_1+"ймайды" ;
                                                             Pl => base_1+"ймаймыз"
                                                           } ;
                                                     P2 => table {
                                                            Sg => base_1+"ғанмын" ;
                                                            Pl => base_1+"ғанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ғансың" ;
                                                            Pl => base_1+"ғансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                 Sg => base_1+"ғансыз" ;
                                                                 Pl => base_1+"ғансыздар"
                                                               } ;
                                                         P2 => table {
                                                                Sg => base_1+"ған" ;
                                                                Pl => base_1+"ған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағандым" ;
                                                                Pl => base_1+"мағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                 Sg => base_1+"мағандың" ;
                                                                 Pl => base_1+"мағандыңдар"
                                                               } ;
                                                         P2 => table {
                                                                Sg => base_1+"мағандыңыз" ;
                                                                Pl => base_1+"мағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағанды" ;
                                                                Pl => base_1+"мағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                              Sg => base_1+"йтынмын" ;
                                                              Pl => base_1+"йтынмыз"
                                                            } ;
                                                      P2 => table {
                                                             Sg => base_1+"йтынсың" ;
                                                             Pl => base_1+"йтынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"йтынсыз" ;
                                                             Pl => base_1+"йтынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                              Sg => base_1+"йтын" ;
                                                              Pl => base_1+"йтын"
                                                            } ;
                                                      P2 => table {
                                                             Sg => base_1+"матынмын" ;
                                                             Pl => base_1+"матынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"матынсың" ;
                                                             Pl => base_1+"матынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"матынсыз" ;
                                                      Pl => base_1+"матынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"матын" ;
                                                    Pl => base_1+"матын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"дым" ;
                                                      Pl => base_1+"дық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"дың" ;
                                                    Pl => base_1+"дыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                                Sg => base_1+"дыңыз" ;
                                Pl => base_1+"дыңыздар"
                              } ;
                        P2 => table {
                               Sg => base_1+"ды" ;
                               Pl => base_1+"ды"
                             } ;
                        P3 => table {
                               Sg => base_1+"мадым" ;
                               Pl => base_1+"мадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV026"
  } ;

mkV027 : Str -> V ;
mkV027 base =
  case base of {
    base_1+"ю" => lin V
      { Infinitive = base_1+"ю" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                 Sg => base_1+"п тұрмын" ;
                                                                 Pl => base_1+"п тұрмыз"
                                                               } ;
                                                         P2 => table {
                                                                Sg => base_1+"п тұрсың" ;
                                                                Pl => base_1+"п тұрсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                 Sg => base_1+"п тұрсыз" ;
                                                                 Pl => base_1+"п тұрсыздар"
                                                               }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                 Sg => base_1+"п тұр" ;
                                                                 Pl => base_1+"п тұр"
                                                               } ;
                                                         P2 => table {
                                                                Sg => base_1+"п тұрмаймын" ;
                                                                Pl => base_1+"п тұрмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"п тұрмайсың" ;
                                                                Pl => base_1+"п тұрмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"п тұрмайсыз" ;
                                                             Pl => base_1+"п тұрмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"п тұрмай" ;
                                                             Pl => base_1+"п тұрмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ямын" ;
                                                             Pl => base_1+"ямыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"ясың" ;
                                                             Pl => base_1+"ясыңдар"
                                                            } ;
                                                      P2 => table {
                                                             Sg => base_1+"ясыз" ;
                                                             Pl => base_1+"ясыздар"
                                                           } ;
                                                      P3 => table {
                                                              Sg => base_1+"яды" ;
                                                              Pl => base_1+"яды"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"ямаймын" ;
                                                            Pl => base_1+"ямайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ямайсың" ;
                                                            Pl => base_1+"ямайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ямайсыз" ;
                                                            Pl => base_1+"ямайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"ямайды" ;
                                                            Pl => base_1+"ямаймыз"
                                                           } ;
                                                     P2 => table {
                                                            Sg => base_1+"ғанмын" ;
                                                            Pl => base_1+"ғанмыз"
                                                           } ;
                                                     P3 => table {
                                                            Sg => base_1+"ғансың" ;
                                                            Pl => base_1+"ғансыңдар"
                                                           }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ғансыз" ;
                                                                Pl => base_1+"ғансыздар"
                                                               } ;
                                                         P2 => table {
                                                                Sg => base_1+"ған" ;
                                                                Pl => base_1+"ған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағандым" ;
                                                                Pl => base_1+"мағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"мағандың" ;
                                                                Pl => base_1+"мағандыңдар"
                                                               } ;
                                                         P2 => table {
                                                                Sg => base_1+"мағандыңыз" ;
                                                                Pl => base_1+"мағандыңыздар"
                                                               } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағанды" ;
                                                                Pl => base_1+"мағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"ятынмын" ;
                                                             Pl => base_1+"ятынмыз"
                                                            } ;
                                                      P2 => table {
                                                             Sg => base_1+"ятынсың" ;
                                                             Pl => base_1+"ятынсыңдар"
                                                            } ;
                                                      P3 => table {
                                                             Sg => base_1+"ятынсыз" ;
                                                             Pl => base_1+"ятынсыздар"
                                                            }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"ятын" ;
                                                             Pl => base_1+"ятын"
                                                            } ;
                                                      P2 => table {
                                                             Sg => base_1+"маятынмын" ;
                                                             Pl => base_1+"маятынмыз"
                                                            } ;
                                                      P3 => table {
                                                             Sg => base_1+"маятынсың" ;
                                                             Pl => base_1+"маятынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"маятынсыз" ;
                                                      Pl => base_1+"маятынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"маятын" ;
                                                    Pl => base_1+"маятын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"дым" ;
                                                      Pl => base_1+"дық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"дың" ;
                                                    Pl => base_1+"дыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"дыңыз" ;
                               Pl => base_1+"дыңыздар"
                              } ;
                        P2 => table {
                               Sg => base_1+"ды" ;
                               Pl => base_1+"ды"
                              } ;
                        P3 => table {
                               Sg => base_1+"мадым" ;
                               Pl => base_1+"мадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV027"
  } ;

mkV028 : Str -> V ;
mkV028 base =
  case base of {
    "керек "+base_1+"у" => lin V
      { Infinitive = "керек "+base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => "керек "+base_1+"iп жатырмын" ;
                                                                Pl => "керек "+base_1+"iп жатырмыз"
                                                               } ;
                                                         P2 => table {
                                                                Sg => "керек "+base_1+"iп жатырсың" ;
                                                                Pl => "керек "+base_1+"iп жатырсыңдар"
                                                               } ;
                                                         P3 => table {
                                                                Sg => "керек "+base_1+"iп жатырсыз" ;
                                                                Pl => "керек "+base_1+"iп жатырсыздар"
                                                               }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => "керек "+base_1+"iп жатыр" ;
                                                                Pl => "керек "+base_1+"iп жатыр"
                                                               } ;
                                                         P2 => table {
                                                                Sg => "керек "+base_1+"iп жатырмаймын" ;
                                                                Pl => "керек "+base_1+"iп жатырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "керек "+base_1+"iп жатырмайсың" ;
                                                                Pl => "керек "+base_1+"iп жатырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => "керек "+base_1+"iп жатырмайсыз" ;
                                                             Pl => "керек "+base_1+"iп жатырмайсыздар"
                                                            } ;
                                                      P2 => table {
                                                             Sg => "керек "+base_1+"iп жатырмай" ;
                                                             Pl => "керек "+base_1+"iп жатырмай"
                                                            } ;
                                                      P3 => table {
                                                             Sg => "керек "+base_1+"емiн" ;
                                                             Pl => "керек "+base_1+"емiз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => "керек "+base_1+"есiң" ;
                                                             Pl => "керек "+base_1+"есiңдер"
                                                            } ;
                                                      P2 => table {
                                                             Sg => "керек "+base_1+"есiз" ;
                                                             Pl => "керек "+base_1+"есiздер"
                                                            } ;
                                                      P3 => table {
                                                             Sg => "керек "+base_1+"едi" ;
                                                             Pl => "керек "+base_1+"едi"
                                                            }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => "керек "+base_1+"емеймiн" ;
                                                            Pl => "керек "+base_1+"емейсiңдер"
                                                           } ;
                                                     P2 => table {
                                                            Sg => "керек "+base_1+"емейсiң" ;
                                                            Pl => "керек "+base_1+"емейсiздер"
                                                          } ;
                                                     P3 => table {
                                                            Sg => "керек "+base_1+"емейсiз" ;
                                                            Pl => "керек "+base_1+"емейдi"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => "керек "+base_1+"емейдi" ;
                                                            Pl => "керек "+base_1+"емеймiз"
                                                           } ;
                                                     P2 => table {
                                                            Sg => "керек "+base_1+"кенмiн" ;
                                                            Pl => "керек "+base_1+"кенмiз"
                                                           } ;
                                                     P3 => table {
                                                            Sg => "керек "+base_1+"кенсiң" ;
                                                            Pl => "керек "+base_1+"кенсiңдер"
                                                           }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => "керек "+base_1+"кенсiз" ;
                                                                Pl => "керек "+base_1+"кенсiздер"
                                                               } ;
                                                         P2 => table {
                                                                Sg => "керек "+base_1+"кен" ;
                                                                Pl => "керек "+base_1+"кен"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "керек "+base_1+"пегендiм" ;
                                                                Pl => "керек "+base_1+"пегендiк"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => "керек "+base_1+"пегендың" ;
                                                                Pl => "керек "+base_1+"пегендыңдер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "керек "+base_1+"пегендыңiз" ;
                                                                Pl => "керек "+base_1+"пегендыңiздер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "керек "+base_1+"пегендi" ;
                                                                Pl => "керек "+base_1+"пегендi"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => "керек "+base_1+"етiнмiн" ;
                                                             Pl => "керек "+base_1+"етiнмiз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "керек "+base_1+"етiнсiң" ;
                                                             Pl => "керек "+base_1+"етiнсiңдер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "керек "+base_1+"етiнсiз" ;
                                                             Pl => "керек "+base_1+"етiнсiздер"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => "керек "+base_1+"етiн" ;
                                                             Pl => "керек "+base_1+"етiн"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "керек "+base_1+"петiнмiн" ;
                                                             Pl => "керек "+base_1+"петiнмiз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "керек "+base_1+"петiнсiң" ;
                                                             Pl => "керек "+base_1+"петiнсiңдер"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => "керек "+base_1+"петiнсiз" ;
                                                      Pl => "керек "+base_1+"петiнсiздер"
                                                    } ;
                                        Formal => table {
                                                    Sg => "керек "+base_1+"петiн" ;
                                                    Pl => "керек "+base_1+"петiн"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => "керек "+base_1+"тiм" ;
                                                      Pl => "керек "+base_1+"тiк"
                                                    } ;
                                        Formal => table {
                                                    Sg => "керек "+base_1+"тiң" ;
                                                    Pl => "керек "+base_1+"тiңдер"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => "керек "+base_1+"тiңiз" ;
                               Pl => "керек "+base_1+"тiңiздер"
                             } ;
                        P2 => table {
                               Sg => "керек "+base_1+"тi" ;
                               Pl => "керек "+base_1+"тi"
                             } ;
                        P3 => table {
                               Sg => "керек "+base_1+"педiм" ;
                               Pl => "керек "+base_1+"педiк"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV028"
  } ;

mkV029 : Str -> V ;
mkV029 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"е жатырмын" ;
                                                                Pl => base_1+"е жатырмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"е жатырсың" ;
                                                                Pl => base_1+"е жатырсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"е жатырсыз" ;
                                                                Pl => base_1+"е жатырсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"е жатыр" ;
                                                                Pl => base_1+"е жатыр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"е жатырмаймын" ;
                                                                Pl => base_1+"е жатырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"е жатырмайсың" ;
                                                                Pl => base_1+"е жатырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"е жатырмайсыз" ;
                                                             Pl => base_1+"е жатырмайсыздар"
                                                            } ;
                                                      P2 => table {
                                                             Sg => base_1+"е жатырмай" ;
                                                             Pl => base_1+"е жатырмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"емiн" ;
                                                             Pl => base_1+"едi"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"есiң" ;
                                                             Pl => base_1+"есiңдер"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"есiз" ;
                                                             Pl => base_1+"едi"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"едi" ;
                                                             Pl => base_1+"едi"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"емеймiн" ;
                                                            Pl => base_1+"емейсiңдер"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"емейсiң" ;
                                                            Pl => base_1+"емейсiздер"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"емейсiз" ;
                                                            Pl => base_1+"емейдi"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"емейдi" ;
                                                            Pl => base_1+"емеймiз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"генмiн" ;
                                                            Pl => base_1+"генмiз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"генсiң" ;
                                                            Pl => base_1+"генсiңдер"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"генсiз" ;
                                                                Pl => base_1+"генсiздер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ген" ;
                                                                Pl => base_1+"ген"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мегендiм" ;
                                                                Pl => base_1+"мегендiк"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"мегендың" ;
                                                                Pl => base_1+"мегендыңдер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"мегендыңiз" ;
                                                                Pl => base_1+"мегендыңiздер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мегендi" ;
                                                                Pl => base_1+"мегендi"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"етiнмiн" ;
                                                             Pl => base_1+"етiнмiз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"етiнсiң" ;
                                                             Pl => base_1+"етiнсiңдер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"етiнсiз" ;
                                                             Pl => base_1+"етiнсiздер"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"етiн" ;
                                                             Pl => base_1+"етiн"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"метiнмiн" ;
                                                             Pl => base_1+"метiнмiз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"метiнсiң" ;
                                                             Pl => base_1+"метiнсiңдер"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"метiнсiз" ;
                                                      Pl => base_1+"метiнсiздер"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"метiн" ;
                                                    Pl => base_1+"метiн"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"дiм" ;
                                                      Pl => base_1+"дi"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"дiң" ;
                                                    Pl => base_1+"дi"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"дiңiз" ;
                               Pl => base_1+"дi"
                             } ;
                        P2 => table {
                               Sg => base_1+"дi" ;
                               Pl => base_1+"дi"
                             } ;
                        P3 => table {
                               Sg => base_1+"медiм" ;
                               Pl => base_1+"медi"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV029"
  } ;

mkV030 : Str -> V ;
mkV030 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"iп жатырмын" ;
                                                                Pl => base_1+"iп жатырмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"iп жатырсың" ;
                                                                Pl => base_1+"iп жатырсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"iп жатырсыз" ;
                                                                Pl => base_1+"iп жатырсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"iп жатыр" ;
                                                                Pl => base_1+"iп жатыр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"iп жатырмаймын" ;
                                                                Pl => base_1+"iп жатырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"iп жатырмайсың" ;
                                                                Pl => base_1+"iп жатырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"iп жатырмайсыз" ;
                                                             Pl => base_1+"iп жатырмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"iп жатырмай" ;
                                                             Pl => base_1+"iп жатырмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"емiн" ;
                                                             Pl => base_1+"емiз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"есiң" ;
                                                             Pl => base_1+"есiңдер"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"есiз" ;
                                                             Pl => base_1+"есiздер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"едi" ;
                                                             Pl => base_1+"едi"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"емеймiн" ;
                                                            Pl => base_1+"емейсiңдер"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"емейсiң" ;
                                                            Pl => base_1+"емейсiздер"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"емейсiз" ;
                                                            Pl => base_1+"емейдi"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"емейдi" ;
                                                            Pl => base_1+"емеймiз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"кенмiн" ;
                                                            Pl => base_1+"кенмiз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"кенсiң" ;
                                                            Pl => base_1+"кенсiңдер"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"кенсiз" ;
                                                                Pl => base_1+"кенсiздер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"кен" ;
                                                                Pl => base_1+"кен"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"пегендiм" ;
                                                                Pl => base_1+"пегендiк"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"пегендың" ;
                                                                Pl => base_1+"пегендыңдер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"пегендыңiз" ;
                                                                Pl => base_1+"пегендыңiздер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"пегендi" ;
                                                                Pl => base_1+"пегендi"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"етiнмiн" ;
                                                             Pl => base_1+"етiнмiз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"етiнсiң" ;
                                                             Pl => base_1+"етiнсiңдер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"етiнсiз" ;
                                                             Pl => base_1+"етiнсiздер"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"етiн" ;
                                                             Pl => base_1+"етiн"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"петiнмiн" ;
                                                             Pl => base_1+"петiнмiз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"петiнсiң" ;
                                                             Pl => base_1+"петiнсiңдер"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"петiнсiз" ;
                                                      Pl => base_1+"петiнсiздер"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"петiн" ;
                                                    Pl => base_1+"петiн"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"дым" ;
                                                      Pl => base_1+"дық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"дың" ;
                                                    Pl => base_1+"дыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"дыңыз" ;
                               Pl => base_1+"дыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"ды" ;
                               Pl => base_1+"ды"
                             } ;
                        P3 => table {
                               Sg => base_1+"педiм" ;
                               Pl => base_1+"педiк"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV030"
  } ;

mkV031 : Str -> V ;
mkV031 base =
  case base of {
    "қаж"+base_1+" болу" => lin V
      { Infinitive = "қаж"+base_1+" болу" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => "қаж"+base_1+" болып тұрмын" ;
                                                                Pl => "қаж"+base_1+" болып тұрмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "қаж"+base_1+" болып тұрсың" ;
                                                                Pl => "қаж"+base_1+" болып тұрсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "қаж"+base_1+" болып тұрсыз" ;
                                                                Pl => "қаж"+base_1+" болып тұрсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => "қаж"+base_1+" болып тұр" ;
                                                                Pl => "қаж"+base_1+" болып тұр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "қаж"+base_1+" болып тұрмаймын" ;
                                                                Pl => "қаж"+base_1+" болып тұрмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "қаж"+base_1+" болып тұрмайсың" ;
                                                                Pl => "қаж"+base_1+" болып тұрмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => "қаж"+base_1+" болып тұрмайсыз" ;
                                                             Pl => "қаж"+base_1+" болып тұрмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "қаж"+base_1+" болып тұрмай" ;
                                                             Pl => "қаж"+base_1+" болып тұрмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "қаж"+base_1+" боламын" ;
                                                             Pl => "қаж"+base_1+" боламыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => "қаж"+base_1+" боласың" ;
                                                             Pl => "қаж"+base_1+" боласыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "қаж"+base_1+" боласыз" ;
                                                             Pl => "қаж"+base_1+" боласыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "қаж"+base_1+" болады" ;
                                                             Pl => "қаж"+base_1+" болады"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => "қаж"+base_1+" боламаймын" ;
                                                            Pl => "қаж"+base_1+" боламайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => "қаж"+base_1+" боламайсың" ;
                                                            Pl => "қаж"+base_1+" боламайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => "қаж"+base_1+" боламайсыз" ;
                                                            Pl => "қаж"+base_1+" боламайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => "қаж"+base_1+" боламайды" ;
                                                            Pl => "қаж"+base_1+" боламаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => "қаж"+base_1+" болғанмын" ;
                                                            Pl => "қаж"+base_1+" болғанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => "қаж"+base_1+" болғансың" ;
                                                            Pl => "қаж"+base_1+" болғансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => "қаж"+base_1+" болғансыз" ;
                                                                Pl => "қаж"+base_1+" болғансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "қаж"+base_1+" болған" ;
                                                                Pl => "қаж"+base_1+" болған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "қаж"+base_1+" болмағандым" ;
                                                                Pl => "қаж"+base_1+" болмағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => "қаж"+base_1+" болмағандың" ;
                                                                Pl => "қаж"+base_1+" болмағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "қаж"+base_1+" болмағандыңыз" ;
                                                                Pl => "қаж"+base_1+" болмағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "қаж"+base_1+" болмағанды" ;
                                                                Pl => "қаж"+base_1+" болмағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => "қаж"+base_1+" болатынмын" ;
                                                             Pl => "қаж"+base_1+" болатынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "қаж"+base_1+" болатынсың" ;
                                                             Pl => "қаж"+base_1+" болатынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "қаж"+base_1+" болатынсыз" ;
                                                             Pl => "қаж"+base_1+" болатынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => "қаж"+base_1+" болатын" ;
                                                             Pl => "қаж"+base_1+" болатын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "қаж"+base_1+" болматынмын" ;
                                                             Pl => "қаж"+base_1+" болматынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "қаж"+base_1+" болматынсың" ;
                                                             Pl => "қаж"+base_1+" болматынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => "қаж"+base_1+" болматынсыз" ;
                                                      Pl => "қаж"+base_1+" болматынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => "қаж"+base_1+" болматын" ;
                                                    Pl => "қаж"+base_1+" болматын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => "қаж"+base_1+" болдым" ;
                                                      Pl => "қаж"+base_1+" болдық"
                                                    } ;
                                        Formal => table {
                                                    Sg => "қаж"+base_1+" болдың" ;
                                                    Pl => "қаж"+base_1+" болдыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => "қаж"+base_1+" болдыңыз" ;
                               Pl => "қаж"+base_1+" болдыңыздар"
                             } ;
                        P2 => table {
                               Sg => "қаж"+base_1+" болды" ;
                               Pl => "қаж"+base_1+" болды"
                             } ;
                        P3 => table {
                               Sg => "қаж"+base_1+" болмадым" ;
                               Pl => "қаж"+base_1+" болмадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV031"
  } ;

mkV032 : Str -> V ;
mkV032 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"п жатырмын" ;
                                                                Pl => base_1+"п жатырмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"п жатырсың" ;
                                                                Pl => base_1+"п жатырсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"п жатырсыз" ;
                                                                Pl => base_1+"п жатырсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"п жатыр" ;
                                                                Pl => base_1+"п жатыр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"п жатырмаймын" ;
                                                                Pl => base_1+"п жатырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"п жатырмайсың" ;
                                                                Pl => base_1+"п жатырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"п жатырмайсыз" ;
                                                             Pl => base_1+"п жатырмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"п жатырмай" ;
                                                             Pl => base_1+"п жатырмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ймын" ;
                                                             Pl => base_1+"ймыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"йсың" ;
                                                             Pl => base_1+"йсыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"йсыз" ;
                                                             Pl => base_1+"йсыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"йды" ;
                                                             Pl => base_1+"йды"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"ймаймын" ;
                                                            Pl => base_1+"ймайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ймайсың" ;
                                                            Pl => base_1+"ймайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ймайсыз" ;
                                                            Pl => base_1+"ймайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"ймайды" ;
                                                            Pl => base_1+"ймаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ғанмын" ;
                                                            Pl => base_1+"ғанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ғансың" ;
                                                            Pl => base_1+"ғансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ғансыз" ;
                                                                Pl => base_1+"ғансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ған" ;
                                                                Pl => base_1+"ған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мегендiм" ;
                                                                Pl => base_1+"мегендiк"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"мегендың" ;
                                                                Pl => base_1+"мегендыңдер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"мегендыңiз" ;
                                                                Pl => base_1+"мегендыңiздер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мегендi" ;
                                                                Pl => base_1+"мегендi"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"йтынмын" ;
                                                             Pl => base_1+"йтынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"йтынсың" ;
                                                             Pl => base_1+"йтынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"йтынсыз" ;
                                                             Pl => base_1+"йтынсыздар"

                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"йтын" ;
                                                             Pl => base_1+"йтын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"матынмын" ;
                                                             Pl => base_1+"матынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"матынсың" ;
                                                             Pl => base_1+"матынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"матынсыз" ;
                                                      Pl => base_1+"матынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"матын" ;
                                                    Pl => base_1+"матын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"дым" ;
                                                      Pl => base_1+"дық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"дың" ;
                                                    Pl => base_1+"дыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"дыңыз" ;
                               Pl => base_1+"дыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"ды" ;
                               Pl => base_1+"ды"
                             } ;
                        P3 => table {
                               Sg => base_1+"мадым" ;
                               Pl => base_1+"мадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV032"
  } ;

mkV033 : Str -> V ;
mkV033 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ып тұрмын" ;
                                                                Pl => base_1+"ып тұрмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ып тұрсың" ;
                                                                Pl => base_1+"ып тұрсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ып тұрсыз" ;
                                                                Pl => base_1+"ып тұрсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"ып тұр" ;
                                                                Pl => base_1+"ып тұр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ып тұрмаймын" ;
                                                                Pl => base_1+"ып тұрмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ып тұрмайсың" ;
                                                                Pl => base_1+"ып тұрмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"ып тұрмайсыз" ;
                                                             Pl => base_1+"ып тұрмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"ып тұрмай" ;
                                                             Pl => base_1+"ып тұрмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"амын" ;
                                                             Pl => base_1+"амыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"асың" ;
                                                             Pl => base_1+"асыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"асыз" ;
                                                             Pl => base_1+"асыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ады" ;
                                                             Pl => base_1+"ады"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"амаймын" ;
                                                            Pl => base_1+"амайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"амайсың" ;
                                                            Pl => base_1+"амайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"амайсыз" ;
                                                            Pl => base_1+"амайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"амайды" ;
                                                            Pl => base_1+"амаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ғанмын" ;
                                                            Pl => base_1+"ғанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ғансың" ;
                                                            Pl => base_1+"ғансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ғансыз" ;
                                                                Pl => base_1+"ғансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ған" ;
                                                                Pl => base_1+"ған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағандым" ;
                                                                Pl => base_1+"мағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"мағандың" ;
                                                                Pl => base_1+"мағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"мағандыңыз" ;
                                                                Pl => base_1+"мағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағанды" ;
                                                                Pl => base_1+"мағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"атынмын" ;
                                                             Pl => base_1+"атынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"атынсың" ;
                                                             Pl => base_1+"атынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"атынсыз" ;
                                                             Pl => base_1+"атынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"атын" ;
                                                             Pl => base_1+"атын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"матынмын" ;
                                                             Pl => base_1+"матынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"матынсың" ;
                                                             Pl => base_1+"матынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"матынсыз" ;
                                                      Pl => base_1+"матынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"матын" ;
                                                    Pl => base_1+"матын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"дым" ;
                                                      Pl => base_1+"дық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"дың" ;
                                                    Pl => base_1+"дыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"дыңыз" ;
                               Pl => base_1+"дыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"ды" ;
                               Pl => base_1+"ды"
                             } ;
                        P3 => table {
                               Sg => base_1+"мадым" ;
                               Pl => base_1+"мадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV033"
  } ;

mkV034 : Str -> V ;
mkV034 base =
  case base of {
    "қол"+base_1+base_2@(?+?)+"у" => lin V
      { Infinitive = "қол"+base_1+base_2+"у" ;
        Indicative = { Fut = nonExist ;
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => "қол"+base_1+base_2+"ып жатырмын" ;
                                                                Pl => "қол"+base_1+base_2+"ып жатырмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "қол"+base_1+base_2+"ып жатырсың" ;
                                                                Pl => "қол"+base_1+base_2+"ып жатырсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "қол"+base_1+base_2+"ып жатырсыз" ;
                                                                Pl => "қол"+base_1+base_2+"ып жатырсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => "қол"+base_1+base_2+"ып жатыр" ;
                                                                Pl => "қол"+base_1+base_2+"ып жатыр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "қол"+base_1+base_2+"ып жатырмаймын" ;
                                                                Pl => "қол"+base_1+base_2+"ып жатырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "қол"+base_1+base_2+"ып жатырмайсың" ;
                                                                Pl => "қол"+base_1+base_2+"ып жатырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => "қол"+base_1+base_2+"ып жатырмайсыз" ;
                                                             Pl => "қол"+base_1+base_2+"ып жатырмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "қол"+base_1+base_2+"ып жатырмай" ;
                                                             Pl => "қол"+base_1+base_2+"ып жатырмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "қол"+base_1+base_2+"амын" ;
                                                             Pl => "қол"+base_1+base_2+"амыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => "қол"+base_1+base_2+"асың" ;
                                                             Pl => "қол"+base_1+base_2+"асыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "қол"+base_1+base_2+"асыз" ;
                                                             Pl => "қол"+base_1+base_2+"асыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "қол"+base_1+base_2+"ады" ;
                                                             Pl => "қол"+base_1+base_2+"ады"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => "қол"+base_1+base_2+"амаймын" ;
                                                            Pl => "қол"+base_1+base_2+"амайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => "қол"+base_1+base_2+"амайсың" ;
                                                            Pl => "қол"+base_1+base_2+"амайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => "қол"+base_1+base_2+"амайсыз" ;
                                                            Pl => "қол"+base_1+base_2+"амайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => "қол"+base_1+base_2+"амайды" ;
                                                            Pl => "қол"+base_1+base_2+"амаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => "пай"+base_1+"ал"+base_2+"ғанмын" ;
                                                            Pl => "пай"+base_1+"ал"+base_2+"ғанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => "пай"+base_1+"ал"+base_2+"ғансың" ;
                                                            Pl => "пай"+base_1+"ал"+base_2+"ғансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => "пай"+base_1+"ал"+base_2+"ғансыз" ;
                                                                Pl => "пай"+base_1+"ал"+base_2+"ғансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "пай"+base_1+"ал"+base_2+"ған" ;
                                                                Pl => "пай"+base_1+"ал"+base_2+"ған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "пай"+base_1+"ал"+base_2+"мағандым" ;
                                                                Pl => "пай"+base_1+"ал"+base_2+"мағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => "пай"+base_1+"ал"+base_2+"мағандың" ;
                                                                Pl => "пай"+base_1+"ал"+base_2+"мағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "пай"+base_1+"ал"+base_2+"мағандыңыз" ;
                                                                Pl => "пай"+base_1+"ал"+base_2+"мағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "пай"+base_1+"ал"+base_2+"мағанды" ;
                                                                Pl => "пай"+base_1+"ал"+base_2+"мағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => "қол"+base_1+base_2+"атынмын" ;
                                                             Pl => "қол"+base_1+base_2+"атынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "қол"+base_1+base_2+"атынсың" ;
                                                             Pl => "қол"+base_1+base_2+"атынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "қол"+base_1+base_2+"атынсыз" ;
                                                             Pl => "қол"+base_1+base_2+"атынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => "қол"+base_1+base_2+"атын" ;
                                                             Pl => "қол"+base_1+base_2+"атын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "қол"+base_1+base_2+"матынмын" ;
                                                             Pl => "қол"+base_1+base_2+"матынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "қол"+base_1+base_2+"матынсың" ;
                                                             Pl => "қол"+base_1+base_2+"матынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => "қол"+base_1+base_2+"матынсыз" ;
                                                      Pl => "қол"+base_1+base_2+"матынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => "қол"+base_1+base_2+"матын" ;
                                                    Pl => "қол"+base_1+base_2+"матын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => "қол"+base_1+base_2+"дым" ;
                                                      Pl => "қол"+base_1+base_2+"дық"
                                                    } ;
                                        Formal => table {
                                                    Sg => "қол"+base_1+base_2+"дың" ;
                                                    Pl => "қол"+base_1+base_2+"дыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => "қол"+base_1+base_2+"дыңыз" ;
                               Pl => "қол"+base_1+base_2+"дыңыздар"
                             } ;
                        P2 => table {
                               Sg => "қол"+base_1+base_2+"ды" ;
                               Pl => "қол"+base_1+base_2+"ды"
                             } ;
                        P3 => table {
                               Sg => "қол"+base_1+base_2+"мадым" ;
                               Pl => "қол"+base_1+base_2+"мадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV034"
  } ;

mkV035 : Str -> V ;
mkV035 base =
  case base of {
    base_1+"ю" => lin V
      { Infinitive = base_1+"ю" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"п тұрмын" ;
                                                                Pl => base_1+"п тұрмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"п тұрсың" ;
                                                                Pl => base_1+"п тұрсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"п тұрсыз" ;
                                                                Pl => base_1+"п тұрсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"п тұр" ;
                                                                Pl => base_1+"п тұр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"п тұрмаймын" ;
                                                                Pl => base_1+"п тұрмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"п тұрмайсың" ;
                                                                Pl => base_1+"п тұрмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"п тұрмайсыз" ;
                                                             Pl => base_1+"п тұрмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"п тұрмай" ;
                                                             Pl => base_1+"п тұрмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ямын" ;
                                                             Pl => base_1+"ямыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"ясың" ;
                                                             Pl => base_1+"ясыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"ясыз" ;
                                                             Pl => base_1+"ясыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"яды" ;
                                                             Pl => base_1+"яды"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"ямаймын" ;
                                                            Pl => base_1+"ямайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ямайсың" ;
                                                            Pl => base_1+"ямайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ямайсыз" ;
                                                            Pl => base_1+"ямайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"ямайды" ;
                                                            Pl => base_1+"ямаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ғанмын" ;
                                                            Pl => base_1+"ғанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ғансың" ;
                                                            Pl => base_1+"ғансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ғансыз" ;
                                                                Pl => base_1+"ғансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ған" ;
                                                                Pl => base_1+"ған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағандым" ;
                                                                Pl => base_1+"мағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"мағандың" ;
                                                                Pl => base_1+"мағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"мағандыңыз" ;
                                                                Pl => base_1+"мағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағанды" ;
                                                                Pl => base_1+"мағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"ятынмын" ;
                                                             Pl => base_1+"ятынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"ятынсың" ;
                                                             Pl => base_1+"ятынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ятынсыз" ;
                                                             Pl => base_1+"ятынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"ятын" ;
                                                             Pl => base_1+"ятын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"маятынмын" ;
                                                             Pl => base_1+"маятынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"маятынсың" ;
                                                             Pl => base_1+"маятынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"маятынсыз" ;
                                                      Pl => base_1+"маятынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"маятын" ;
                                                    Pl => base_1+"маятын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"йдым" ;
                                                      Pl => base_1+"йдық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"йдың" ;
                                                    Pl => base_1+"йдыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"йдыңыз" ;
                               Pl => base_1+"йдыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"йды" ;
                               Pl => base_1+"йды"
                             } ;
                        P3 => table {
                               Sg => base_1+"падым" ;
                               Pl => base_1+"падық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV035"
  } ;

mkV036 : Str -> V ;
mkV036 base =
  case base of {
    "қыды"+base_1+"у" => lin V
      { Infinitive = "қыды"+base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => "қыды"+base_1+"ып жүрмiн" ;
                                                                Pl => "қыды"+base_1+"ып жүрмiз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "қыды"+base_1+"ып жүрсiң" ;
                                                                Pl => "қыды"+base_1+"ып жүрсiңдер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "қыды"+base_1+"ып жүрсiз" ;
                                                                Pl => "қыды"+base_1+"ып жүрсiздер"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => "қыды"+base_1+"ып жүр" ;
                                                                Pl => "қыды"+base_1+"ып жүр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "қыды"+base_1+"ып жүрмеймын" ;
                                                                Pl => "қыды"+base_1+"ып жүрсiздермыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "қыды"+base_1+"ып жүрмейсiң" ;
                                                                Pl => "қыды"+base_1+"ып жүрмейсiңдер"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => "қыды"+base_1+"ып жүрмейсiз" ;
                                                             Pl => "қыды"+base_1+"ып жүрмейсiздер"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "қыды"+base_1+"ып жүрмей" ;
                                                             Pl => "қыды"+base_1+"ып жүрмей"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "қыды"+base_1+"амын" ;
                                                             Pl => "қыды"+base_1+"амыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => "қыды"+base_1+"асың" ;
                                                             Pl => "қыды"+base_1+"асыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "қыды"+base_1+"асыз" ;
                                                             Pl => "қыды"+base_1+"асыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "қыды"+base_1+"ады" ;
                                                             Pl => "қыды"+base_1+"ады"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => "қыды"+base_1+"амаймын" ;
                                                            Pl => "қыды"+base_1+"амайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => "қыды"+base_1+"амайсың" ;
                                                            Pl => "қыды"+base_1+"амайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => "қыды"+base_1+"амайсыз" ;
                                                            Pl => "қыды"+base_1+"амайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => "қыды"+base_1+"амайды" ;
                                                            Pl => "қыды"+base_1+"амаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => "жұ"+base_1+"ғанмын" ;
                                                            Pl => "жұ"+base_1+"ғанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => "жұ"+base_1+"ғансың" ;
                                                            Pl => "жұ"+base_1+"ғансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => "жұ"+base_1+"ғансыз" ;
                                                                Pl => "жұ"+base_1+"ғансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "жұ"+base_1+"ған" ;
                                                                Pl => "жұ"+base_1+"ған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "жұ"+base_1+"мағандым" ;
                                                                Pl => "жұ"+base_1+"мағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => "жұ"+base_1+"мағандың" ;
                                                                Pl => "жұ"+base_1+"мағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "жұ"+base_1+"мағандыңыз" ;
                                                                Pl => "жұ"+base_1+"мағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "жұ"+base_1+"мағанды" ;
                                                                Pl => "жұ"+base_1+"мағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => "қыды"+base_1+"атынмын" ;
                                                             Pl => "қыды"+base_1+"атынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "қыды"+base_1+"атынсың" ;
                                                             Pl => "қыды"+base_1+"атынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "қыды"+base_1+"атынсыз" ;
                                                             Pl => "қыды"+base_1+"атынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => "қыды"+base_1+"атын" ;
                                                             Pl => "қыды"+base_1+"атын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "қыды"+base_1+"матынмын" ;
                                                             Pl => "қыды"+base_1+"матынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "қыды"+base_1+"матынсың" ;
                                                             Pl => "қыды"+base_1+"матынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => "қыды"+base_1+"матынсыз" ;
                                                      Pl => "қыды"+base_1+"матынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => "қыды"+base_1+"матын" ;
                                                    Pl => "қыды"+base_1+"матын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => "қыды"+base_1+"дым" ;
                                                      Pl => "қыды"+base_1+"дық"
                                                    } ;
                                        Formal => table {
                                                    Sg => "қыды"+base_1+"дың" ;
                                                    Pl => "қыды"+base_1+"дыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => "қыды"+base_1+"дыңыз" ;
                               Pl => "қыды"+base_1+"дыңыздар"
                             } ;
                        P2 => table {
                               Sg => "қыды"+base_1+"ды" ;
                               Pl => "қыды"+base_1+"ды"
                             } ;
                        P3 => table {
                               Sg => "қыды"+base_1+"мадым" ;
                               Pl => "қыды"+base_1+"мадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV036"
  } ;

mkV037 : Str -> V ;
mkV037 base =
  case base of {
    "машинамен "+base_1+"у" => lin V
      { Infinitive = "машинамен "+base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"мiн" ;
                                                                Pl => base_1+"мiз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"сiң" ;
                                                                Pl => base_1+"сiңдер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"сiз" ;
                                                                Pl => base_1+"сiздер"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1 ;
                                                                Pl => base_1
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"меймын" ;
                                                                Pl => base_1+"сiздермыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мейсiң" ;
                                                                Pl => base_1+"мейсiңдер"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"мейсiз" ;
                                                             Pl => base_1+"мейсiздер"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"мей" ;
                                                             Pl => base_1+"мей"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "машинамен "+base_1+"емiн" ;
                                                             Pl => "машинамен "+base_1+"емiз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => "машинамен "+base_1+"есiң" ;
                                                             Pl => "машинамен "+base_1+"есiңдер"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "машинамен "+base_1+"есiз" ;
                                                             Pl => "машинамен "+base_1+"есiздер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "машинамен "+base_1+"едi" ;
                                                             Pl => "машинамен "+base_1+"едi"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => "машинамен "+base_1+"емеймiн" ;
                                                            Pl => "машинамен "+base_1+"емейсiңдер"
                                                          } ;
                                                     P2 => table {
                                                            Sg => "машинамен "+base_1+"емейсiң" ;
                                                            Pl => "машинамен "+base_1+"емейсiздер"
                                                          } ;
                                                     P3 => table {
                                                            Sg => "машинамен "+base_1+"емейсiз" ;
                                                            Pl => "машинамен "+base_1+"емейдi"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => "машинамен "+base_1+"емейдi" ;
                                                            Pl => "машинамен "+base_1+"емеймiз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => "машинамен "+base_1+"генмiн" ;
                                                            Pl => "машинамен "+base_1+"генмiз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => "машинамен "+base_1+"генсiң" ;
                                                            Pl => "машинамен "+base_1+"генсiңдер"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => "машинамен "+base_1+"генсiз" ;
                                                                Pl => "машинамен "+base_1+"генсiздер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "машинамен "+base_1+"ген" ;
                                                                Pl => "машинамен "+base_1+"ген"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "машинамен "+base_1+"мегендiм" ;
                                                                Pl => "машинамен "+base_1+"мегендiк"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => "машинамен "+base_1+"мегендың" ;
                                                                Pl => "машинамен "+base_1+"мегендыңдер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "машинамен "+base_1+"мегендыңiз" ;
                                                                Pl => "машинамен "+base_1+"мегендыңiздер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "машинамен "+base_1+"мегендi" ;
                                                                Pl => "машинамен "+base_1+"мегендi"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => "машинамен "+base_1+"етiнмiн" ;
                                                             Pl => "машинамен "+base_1+"етiнмiз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "машинамен "+base_1+"етiнсiң" ;
                                                             Pl => "машинамен "+base_1+"етiнсiңдер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "машинамен "+base_1+"етiнсiз" ;
                                                             Pl => "машинамен "+base_1+"етiнсiздер"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => "машинамен "+base_1+"етiн" ;
                                                             Pl => "машинамен "+base_1+"етiн"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "машинамен "+base_1+"метiнмiн" ;
                                                             Pl => "машинамен "+base_1+"метiнмiз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "машинамен "+base_1+"метiнсiң" ;
                                                             Pl => "машинамен "+base_1+"метiнсiңдер"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => "машинамен "+base_1+"метiнсiз" ;
                                                      Pl => "машинамен "+base_1+"метiнсiздер"
                                                    } ;
                                        Formal => table {
                                                    Sg => "машинамен "+base_1+"метiн" ;
                                                    Pl => "машинамен "+base_1+"метiн"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => "машинамен "+base_1+"дiм" ;
                                                      Pl => "машинамен "+base_1+"дiк"
                                                    } ;
                                        Formal => table {
                                                    Sg => "машинамен "+base_1+"дiң" ;
                                                    Pl => "машинамен "+base_1+"дiңдер"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => "машинамен "+base_1+"дiңiз" ;
                               Pl => "машинамен "+base_1+"дiңiздер"
                             } ;
                        P2 => table {
                               Sg => "машинамен "+base_1+"дi" ;
                               Pl => "машинамен "+base_1+"дi"
                             } ;
                        P3 => table {
                               Sg => "машинамен "+base_1+"медiм" ;
                               Pl => "машинамен "+base_1+"медiк"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV037"
  } ;

mkV038 : Str -> V ;
mkV038 base =
  case base of {
    "пай"+base_1+"л"+base_2@(?+?)+"у" => lin V
      { Infinitive = "пай"+base_1+"л"+base_2+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => "пай"+base_1+"л"+base_2+"ып тұрмын" ;
                                                                Pl => "пай"+base_1+"л"+base_2+"ып тұрмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "пай"+base_1+"л"+base_2+"ып тұрсың" ;
                                                                Pl => "пай"+base_1+"л"+base_2+"ып тұрсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "пай"+base_1+"л"+base_2+"ып тұрсыз" ;
                                                                Pl => "пай"+base_1+"л"+base_2+"ып тұрсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => "пай"+base_1+"л"+base_2+"ып тұр" ;
                                                                Pl => "пай"+base_1+"л"+base_2+"ып тұр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "пай"+base_1+"л"+base_2+"ып тұрмаймын" ;
                                                                Pl => "пай"+base_1+"л"+base_2+"ып тұрмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "пай"+base_1+"л"+base_2+"ып тұрмайсың" ;
                                                                Pl => "пай"+base_1+"л"+base_2+"ып тұрмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => "пай"+base_1+"л"+base_2+"ып тұрмайсыз" ;
                                                             Pl => "пай"+base_1+"л"+base_2+"ып тұрмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "пай"+base_1+"л"+base_2+"ып тұрмай" ;
                                                             Pl => "пай"+base_1+"л"+base_2+"ып тұрмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "пай"+base_1+"л"+base_2+"амын" ;
                                                             Pl => "пай"+base_1+"л"+base_2+"амыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => "пай"+base_1+"л"+base_2+"асың" ;
                                                             Pl => "пай"+base_1+"л"+base_2+"асыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "пай"+base_1+"л"+base_2+"асыз" ;
                                                             Pl => "пай"+base_1+"л"+base_2+"асыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "пай"+base_1+"л"+base_2+"ады" ;
                                                             Pl => "пай"+base_1+"л"+base_2+"ады"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => "пай"+base_1+"л"+base_2+"амаймын" ;
                                                            Pl => "пай"+base_1+"л"+base_2+"амайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => "пай"+base_1+"л"+base_2+"амайсың" ;
                                                            Pl => "пай"+base_1+"л"+base_2+"амайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => "пай"+base_1+"л"+base_2+"амайсыз" ;
                                                            Pl => "пай"+base_1+"л"+base_2+"амайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => "пай"+base_1+"л"+base_2+"амайды" ;
                                                            Pl => "пай"+base_1+"л"+base_2+"амаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => "қол"+base_1+"нғ"+base_2+"мын" ;
                                                            Pl => "қол"+base_1+"нғ"+base_2+"мыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => "қол"+base_1+"нғ"+base_2+"сың" ;
                                                            Pl => "қол"+base_1+"нғ"+base_2+"сыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => "қол"+base_1+"нғ"+base_2+"сыз" ;
                                                                Pl => "қол"+base_1+"нғ"+base_2+"сыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "қол"+base_1+"нғ"+base_2 ;
                                                                Pl => "қол"+base_1+"нғ"+base_2
                                                              } ;
                                                         P3 => table {
                                                                Sg => "қол"+base_1+"нмағ"+base_2+"дым" ;
                                                                Pl => "қол"+base_1+"нмағ"+base_2+"дық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => "қол"+base_1+"нмағ"+base_2+"дың" ;
                                                                Pl => "қол"+base_1+"нмағ"+base_2+"дыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "қол"+base_1+"нмағ"+base_2+"дыңыз" ;
                                                                Pl => "қол"+base_1+"нмағ"+base_2+"дыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "қол"+base_1+"нмағ"+base_2+"ды" ;
                                                                Pl => "қол"+base_1+"нмағ"+base_2+"ды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => "пай"+base_1+"л"+base_2+"атынмын" ;
                                                             Pl => "пай"+base_1+"л"+base_2+"атынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "пай"+base_1+"л"+base_2+"атынсың" ;
                                                             Pl => "пай"+base_1+"л"+base_2+"атынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "пай"+base_1+"л"+base_2+"атынсыз" ;
                                                             Pl => "пай"+base_1+"л"+base_2+"атынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => "пай"+base_1+"л"+base_2+"атын" ;
                                                             Pl => "пай"+base_1+"л"+base_2+"атын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "пай"+base_1+"л"+base_2+"матынмын" ;
                                                             Pl => "пай"+base_1+"л"+base_2+"матынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "пай"+base_1+"л"+base_2+"матынсың" ;
                                                             Pl => "пай"+base_1+"л"+base_2+"матынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => "пай"+base_1+"л"+base_2+"матынсыз" ;
                                                      Pl => "пай"+base_1+"л"+base_2+"матынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => "пай"+base_1+"л"+base_2+"матын" ;
                                                    Pl => "пай"+base_1+"л"+base_2+"матын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => "пай"+base_1+"л"+base_2+"дым" ;
                                                      Pl => "пай"+base_1+"л"+base_2+"дық"
                                                    } ;
                                        Formal => table {
                                                    Sg => "пай"+base_1+"л"+base_2+"дың" ;
                                                    Pl => "пай"+base_1+"л"+base_2+"дыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => "пай"+base_1+"л"+base_2+"дыңыз" ;
                               Pl => "пай"+base_1+"л"+base_2+"дыңыздар"
                             } ;
                        P2 => table {
                               Sg => "пай"+base_1+"л"+base_2+"ды" ;
                               Pl => "пай"+base_1+"л"+base_2+"ды"
                             } ;
                        P3 => table {
                               Sg => "пай"+base_1+"л"+base_2+"мадым" ;
                               Pl => "пай"+base_1+"л"+base_2+"мадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV038"
  } ;

mkV039 : Str -> V ;
mkV039 base =
  case base of {
    base_1+"iну" => lin V
      { Infinitive = base_1+"iну" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"iнiп тұрмын" ;
                                                                Pl => base_1+"iнiп тұрмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"iнiп тұрсың" ;
                                                                Pl => base_1+"iнiп тұрсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"iнiп тұрсыз" ;
                                                                Pl => base_1+"iнiп тұрсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"iнiп тұр" ;
                                                                Pl => base_1+"iнiп тұр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"iнiп тұрмаймын" ;
                                                                Pl => base_1+"iнiп тұрмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"iнiп тұрмайсың" ;
                                                                Pl => base_1+"iнiп тұрмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"iнiп тұрмайсыз" ;
                                                             Pl => base_1+"iнiп тұрмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"iнiп тұрмай" ;
                                                             Pl => base_1+"iнiп тұрмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"iнемiн" ;
                                                             Pl => base_1+"iнемiз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"iнесiң" ;
                                                             Pl => base_1+"iнесiңдер"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"iнесiз" ;
                                                             Pl => base_1+"iнесiздер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"iнедi" ;
                                                             Pl => base_1+"iнедi"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"iнемеймiн" ;
                                                            Pl => base_1+"iнемейсiңдер"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"iнемейсiң" ;
                                                            Pl => base_1+"iнемейсiздер"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"iнемейсiз" ;
                                                            Pl => base_1+"iнемейдi"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"iнемейдi" ;
                                                            Pl => base_1+"iнемеймiз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"генмiн" ;
                                                            Pl => base_1+"генмiз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"генсiң" ;
                                                            Pl => base_1+"генсiңдер"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"генсiз" ;
                                                                Pl => base_1+"генсiздер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ген" ;
                                                                Pl => base_1+"ген"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мегендiм" ;
                                                                Pl => base_1+"мегендiк"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"мегендың" ;
                                                                Pl => base_1+"мегендыңдер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"мегендыңiз" ;
                                                                Pl => base_1+"мегендыңiздер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мегендi" ;
                                                                Pl => base_1+"мегендi"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"iнетiнмiн" ;
                                                             Pl => base_1+"iнетiнмiз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"iнетiнсiң" ;
                                                             Pl => base_1+"iнетiнсiңдер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"iнетiнсiз" ;
                                                             Pl => base_1+"iнетiнсiздер"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"iнетiн" ;
                                                             Pl => base_1+"iнетiн"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"iнметiнмiн" ;
                                                             Pl => base_1+"iнметiнмiз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"iнметiнсiң" ;
                                                             Pl => base_1+"iнметiнсiңдер"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"iнметiнсiз" ;
                                                      Pl => base_1+"iнметiнсiздер"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"iнметiн" ;
                                                    Pl => base_1+"iнметiн"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"iндiм" ;
                                                      Pl => base_1+"iндiк"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"iндiң" ;
                                                    Pl => base_1+"iндiңдер"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"iндiңiз" ;
                               Pl => base_1+"iндiңiздер"
                             } ;
                        P2 => table {
                               Sg => base_1+"iндi" ;
                               Pl => base_1+"iндi"
                             } ;
                        P3 => table {
                               Sg => base_1+"iнмедiм" ;
                               Pl => base_1+"iнмедiк"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV039"
  } ;

mkV040 : Str -> V ;
mkV040 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"iп тұрмын" ;
                                                                Pl => base_1+"iп тұрмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"iп тұрсың" ;
                                                                Pl => base_1+"iп тұрсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"iп тұрсыз" ;
                                                                Pl => base_1+"iп тұрсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"iп тұр" ;
                                                                Pl => base_1+"iп тұр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"iп тұрмаймын" ;
                                                                Pl => base_1+"iп тұрмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"iп тұрмайсың" ;
                                                                Pl => base_1+"iп тұрмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"iп тұрмайсыз" ;
                                                             Pl => base_1+"iп тұрмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"iп тұрмай" ;
                                                             Pl => base_1+"iп тұрмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"емiн" ;
                                                             Pl => base_1+"емiз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"есiң" ;
                                                             Pl => base_1+"есiңдер"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"есiз" ;
                                                             Pl => base_1+"есiздер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"едi" ;
                                                             Pl => base_1+"едi"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"емеймiн" ;
                                                            Pl => base_1+"емейсiңдер"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"емейсiң" ;
                                                            Pl => base_1+"емейсiздер"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"емейсiз" ;
                                                            Pl => base_1+"емейдi"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"емейдi" ;
                                                            Pl => base_1+"емеймiз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"iнгенмiн" ;
                                                            Pl => base_1+"iнгенмiз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"iнгенсiң" ;
                                                            Pl => base_1+"iнгенсiңдер"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"iнгенсiз" ;
                                                                Pl => base_1+"iнгенсiздер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"iнген" ;
                                                                Pl => base_1+"iнген"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"iнмегендiм" ;
                                                                Pl => base_1+"iнмегендiк"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"iнмегендың" ;
                                                                Pl => base_1+"iнмегендыңдер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"iнмегендыңiз" ;
                                                                Pl => base_1+"iнмегендыңiздер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"iнмегендi" ;
                                                                Pl => base_1+"iнмегендi"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"етiнмiн" ;
                                                             Pl => base_1+"етiнмiз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"етiнсiң" ;
                                                             Pl => base_1+"етiнсiңдер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"етiнсiз" ;
                                                             Pl => base_1+"етiнсiздер"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"етiн" ;
                                                             Pl => base_1+"етiн"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"бетiнмiн" ;
                                                             Pl => base_1+"бетiнмiз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"бетiнсiң" ;
                                                             Pl => base_1+"бетiнсiңдер"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"бетiнсiз" ;
                                                      Pl => base_1+"бетiнсiздер"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"бетiн" ;
                                                    Pl => base_1+"бетiн"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"дiм" ;
                                                      Pl => base_1+"дiк"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"дiң" ;
                                                    Pl => base_1+"дiңдер"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"дiңiз" ;
                               Pl => base_1+"дiңiздер"
                             } ;
                        P2 => table {
                               Sg => base_1+"дi" ;
                               Pl => base_1+"дi"
                             } ;
                        P3 => table {
                               Sg => base_1+"бедiм" ;
                               Pl => base_1+"бедiк"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV040"
  } ;

mkV041 : Str -> V ;
mkV041 base =
  case base of {
    base_1+"су" => lin V
      { Infinitive = base_1+"су" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"сiп тұрмын" ;
                                                                Pl => base_1+"сiп тұрмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"сiп тұрсың" ;
                                                                Pl => base_1+"сiп тұрсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"сiп тұрсыз" ;
                                                                Pl => base_1+"сiп тұрсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"сiп тұр" ;
                                                                Pl => base_1+"сiп тұр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"сiп тұрмаймын" ;
                                                                Pl => base_1+"сiп тұрмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"сiп тұрмайсың" ;
                                                                Pl => base_1+"сiп тұрмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"сiп тұрмайсыз" ;
                                                             Pl => base_1+"сiп тұрмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"сiп тұрмай" ;
                                                             Pl => base_1+"сiп тұрмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"семiн" ;
                                                             Pl => base_1+"семiз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"сесiң" ;
                                                             Pl => base_1+"сесiңдер"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"сесiз" ;
                                                             Pl => base_1+"сесiздер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"седi" ;
                                                             Pl => base_1+"седi"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"семеймiн" ;
                                                            Pl => base_1+"семейсiңдер"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"семейсiң" ;
                                                            Pl => base_1+"семейсiздер"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"семейсiз" ;
                                                            Pl => base_1+"семейдi"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"семейдi" ;
                                                            Pl => base_1+"семеймiз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"скенмiн" ;
                                                            Pl => base_1+"скенмiз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"скенсiң" ;
                                                            Pl => base_1+"скенсiңдер"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"скенсiз" ;
                                                                Pl => base_1+"скенсiздер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"скен" ;
                                                                Pl => base_1+"скен"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"спегендiм" ;
                                                                Pl => base_1+"спегендiк"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"спегендың" ;
                                                                Pl => base_1+"спегендыңдер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"спегендыңiз" ;
                                                                Pl => base_1+"спегендыңiздер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"спегендi" ;
                                                                Pl => base_1+"спегендi"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"йтiнмiн" ;
                                                             Pl => base_1+"йтiнмiз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"йтiнсiң" ;
                                                             Pl => base_1+"йтiнсiңдер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"йтiнсiз" ;
                                                             Pl => base_1+"йтiнсiздер"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"йтiн" ;
                                                             Pl => base_1+"йтiн"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"петiнмiн" ;
                                                             Pl => base_1+"петiнмiз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"петiнсiң" ;
                                                             Pl => base_1+"петiнсiңдер"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"петiнсiз" ;
                                                      Pl => base_1+"петiнсiздер"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"петiн" ;
                                                    Pl => base_1+"петiн"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"стiм" ;
                                                      Pl => base_1+"стiк"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"стiң" ;
                                                    Pl => base_1+"стiңдер"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"стiңiз" ;
                               Pl => base_1+"стiңiздер"
                             } ;
                        P2 => table {
                               Sg => base_1+"стi" ;
                               Pl => base_1+"стi"
                             } ;
                        P3 => table {
                               Sg => base_1+"спедiм" ;
                               Pl => base_1+"спедiк"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV041"
  } ;

mkV042 : Str -> V ;
mkV042 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"п жатырмын" ;
                                                                Pl => base_1+"п жатырмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"п жатырсың" ;
                                                                Pl => base_1+"п жатырсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"п жатырсыз" ;
                                                                Pl => base_1+"п жатырсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"п жатыр" ;
                                                                Pl => base_1+"п жатыр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"п жатырмаймын" ;
                                                                Pl => base_1+"п жатырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"п жатырмайсың" ;
                                                                Pl => base_1+"п жатырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"п жатырмайсыз" ;
                                                             Pl => base_1+"п жатырмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"п жатырмай" ;
                                                             Pl => base_1+"п жатырмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ймiн" ;
                                                             Pl => base_1+"ймiз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"йсiң" ;
                                                             Pl => base_1+"йсiңдер"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"йсiз" ;
                                                             Pl => base_1+"йсiздер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"йдi" ;
                                                             Pl => base_1+"йдi"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"ймеймiн" ;
                                                            Pl => base_1+"ймейсiңдер"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ймейсiң" ;
                                                            Pl => base_1+"ймейсiздер"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ймейсiз" ;
                                                            Pl => base_1+"ймейдi"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"ймейдi" ;
                                                            Pl => base_1+"ймеймiз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"генмiн" ;
                                                            Pl => base_1+"генмiз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"генсiң" ;
                                                            Pl => base_1+"генсiңдер"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"генсiз" ;
                                                                Pl => base_1+"генсiздер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ген" ;
                                                                Pl => base_1+"ген"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мегендiм" ;
                                                                Pl => base_1+"мегендiк"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"мегендың" ;
                                                                Pl => base_1+"мегендыңдер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"мегендыңiз" ;
                                                                Pl => base_1+"мегендыңiздер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мегендi" ;
                                                                Pl => base_1+"мегендi"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"сетiнмiн" ;
                                                             Pl => base_1+"сетiнмiз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"сетiнсiң" ;
                                                             Pl => base_1+"сетiнсiңдер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"сетiнсiз" ;
                                                             Pl => base_1+"сетiнсiздер"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"сетiн" ;
                                                             Pl => base_1+"сетiн"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"сметiнмiн" ;
                                                             Pl => base_1+"сметiнмiз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"сметiнсiң" ;
                                                             Pl => base_1+"сметiнсiңдер"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"сметiнсiз" ;
                                                      Pl => base_1+"сметiнсiздер"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"сметiн" ;
                                                    Pl => base_1+"сметiн"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"дiм" ;
                                                      Pl => base_1+"дiк"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"дiң" ;
                                                    Pl => base_1+"дiңдер"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"дiңiз" ;
                               Pl => base_1+"дiңiздер"
                             } ;
                        P2 => table {
                               Sg => base_1+"дi" ;
                               Pl => base_1+"дi"
                             } ;
                        P3 => table {
                               Sg => base_1+"медiм" ;
                               Pl => base_1+"медiк"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV042"
  } ;

mkV043 : Str -> V ;
mkV043 base =
  case base of {
    base_1+"ю" => lin V
      { Infinitive = base_1+"ю" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"йiп тұрмын" ;
                                                                Pl => base_1+"йiп тұрмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"йiп тұрсың" ;
                                                                Pl => base_1+"йiп тұрсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"йiп тұрсыз" ;
                                                                Pl => base_1+"йiп тұрсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"йiп тұр" ;
                                                                Pl => base_1+"йiп тұр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"йiп тұрмаймын" ;
                                                                Pl => base_1+"йiп тұрмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"йiп тұрмайсың" ;
                                                                Pl => base_1+"йiп тұрмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"йiп тұрмайсыз" ;
                                                             Pl => base_1+"йiп тұрмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"йiп тұрмай" ;
                                                             Pl => base_1+"йiп тұрмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"йемiн" ;
                                                             Pl => base_1+"йемiз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"йесiң" ;
                                                             Pl => base_1+"йесiңдер"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"йесiз" ;
                                                             Pl => base_1+"йесiздер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"йедi" ;
                                                             Pl => base_1+"йедi"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"йемеймiн" ;
                                                            Pl => base_1+"йемейсiңдер"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"йемейсiң" ;
                                                            Pl => base_1+"йемейсiздер"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"йемейсiз" ;
                                                            Pl => base_1+"йемейдi"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"йемейдi" ;
                                                            Pl => base_1+"йемеймiз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"генмiн" ;
                                                            Pl => base_1+"генмiз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"генсiң" ;
                                                            Pl => base_1+"генсiңдер"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"генсiз" ;
                                                                Pl => base_1+"генсiздер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ген" ;
                                                                Pl => base_1+"ген"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мегендiм" ;
                                                                Pl => base_1+"мегендiк"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"мегендың" ;
                                                                Pl => base_1+"мегендыңдер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"мегендыңiз" ;
                                                                Pl => base_1+"мегендыңiздер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мегендi" ;
                                                                Pl => base_1+"мегендi"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"йетiнмiн" ;
                                                             Pl => base_1+"йетiнмiз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"йетiнсiң" ;
                                                             Pl => base_1+"йетiнсiңдер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"йетiнсiз" ;
                                                             Pl => base_1+"йетiнсiздер"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"йетiн" ;
                                                             Pl => base_1+"йетiн"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"метiнмiн" ;
                                                             Pl => base_1+"метiнмiз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"метiнсiң" ;
                                                             Pl => base_1+"метiнсiңдер"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"метiнсiз" ;
                                                      Pl => base_1+"метiнсiздер"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"метiн" ;
                                                    Pl => base_1+"метiн"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"йдiм" ;
                                                      Pl => base_1+"йдiк"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"йдiң" ;
                                                    Pl => base_1+"йдiңдер"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"йдiңiз" ;
                               Pl => base_1+"йдiңiздер"
                             } ;
                        P2 => table {
                               Sg => base_1+"йдi" ;
                               Pl => base_1+"йдi"
                             } ;
                        P3 => table {
                               Sg => base_1+"ймедiм" ;
                               Pl => base_1+"ймедiк"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV043"
  } ;

mkV044 : Str -> V ;
mkV044 base =
  case base of {
    base_1+"бу" => lin V
      { Infinitive = base_1+"бу" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ып отырмын" ;
                                                                Pl => base_1+"ып отырмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ып отырсың" ;
                                                                Pl => base_1+"ып отырсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ып отырсыз" ;
                                                                Pl => base_1+"ып отырсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"ып отыр" ;
                                                                Pl => base_1+"ып отыр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ып отырмаймын" ;
                                                                Pl => base_1+"ып отырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ып отырмайсың" ;
                                                                Pl => base_1+"ып отырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"ып отырмайсыз" ;
                                                             Pl => base_1+"ып отырмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"ып отырмай" ;
                                                             Pl => base_1+"ып отырмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"бамын" ;
                                                             Pl => base_1+"бамыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"басың" ;
                                                             Pl => base_1+"басыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"басыз" ;
                                                             Pl => base_1+"басыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"бады" ;
                                                             Pl => base_1+"бады"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"бамаймын" ;
                                                            Pl => base_1+"бамайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"бамайсың" ;
                                                            Pl => base_1+"бамайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"бамайсыз" ;
                                                            Pl => base_1+"бамайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"бамайды" ;
                                                            Pl => base_1+"бамаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"бқанмын" ;
                                                            Pl => base_1+"бқанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"бқансың" ;
                                                            Pl => base_1+"бқансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"бқансыз" ;
                                                                Pl => base_1+"бқансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"бқан" ;
                                                                Pl => base_1+"бқан"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ппағандым" ;
                                                                Pl => base_1+"ппағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"ппағандың" ;
                                                                Pl => base_1+"ппағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ппағандыңыз" ;
                                                                Pl => base_1+"ппағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ппағанды" ;
                                                                Pl => base_1+"ппағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"батынмын" ;
                                                             Pl => base_1+"батынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"батынсың" ;
                                                             Pl => base_1+"батынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"батынсыз" ;
                                                             Pl => base_1+"батынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"батын" ;
                                                             Pl => base_1+"батын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"ппатынмын" ;
                                                             Pl => base_1+"ппатынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ппатынсың" ;
                                                             Pl => base_1+"ппатынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"ппатынсыз" ;
                                                      Pl => base_1+"ппатынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"ппатын" ;
                                                    Pl => base_1+"ппатын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"птым" ;
                                                      Pl => base_1+"птық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"птың" ;
                                                    Pl => base_1+"птыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"птыңыз" ;
                               Pl => base_1+"птыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"пты" ;
                               Pl => base_1+"пты"
                             } ;
                        P3 => table {
                               Sg => base_1+"ппадым" ;
                               Pl => base_1+"ппадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV044"
  } ;

mkV045 : Str -> V ;
mkV045 base =
  case base of {
    "телефон "+base_1+"лу" => lin V
      { Infinitive = "телефон "+base_1+"лу" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => "телефон "+base_1+"лып тұрмын" ;
                                                                Pl => "телефон "+base_1+"лып тұрмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "телефон "+base_1+"лып тұрсың" ;
                                                                Pl => "телефон "+base_1+"лып тұрсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "телефон "+base_1+"лып тұрсыз" ;
                                                                Pl => "телефон "+base_1+"лып тұрсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => "телефон "+base_1+"лып тұр" ;
                                                                Pl => "телефон "+base_1+"лып тұр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "телефон "+base_1+"лып тұрмаймын" ;
                                                                Pl => "телефон "+base_1+"лып тұрмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "телефон "+base_1+"лып тұрмайсың" ;
                                                                Pl => "телефон "+base_1+"лып тұрмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => "телефон "+base_1+"лып тұрмайсыз" ;
                                                             Pl => "телефон "+base_1+"лып тұрмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "телефон "+base_1+"лып тұрмай" ;
                                                             Pl => "телефон "+base_1+"лып тұрмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"қырамын" ;
                                                             Pl => base_1+"қырамыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"қырасың" ;
                                                             Pl => base_1+"қырасыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"қырасыз" ;
                                                             Pl => base_1+"қырасыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"қырады" ;
                                                             Pl => base_1+"қырады"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"қырамаймын" ;
                                                            Pl => base_1+"қырамайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"қырамайсың" ;
                                                            Pl => base_1+"қырамайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"қырамайсыз" ;
                                                            Pl => base_1+"қырамайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"қырамайды" ;
                                                            Pl => base_1+"қырамаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => "телефон "+base_1+"лғанмын" ;
                                                            Pl => "телефон "+base_1+"лғанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => "телефон "+base_1+"лғансың" ;
                                                            Pl => "телефон "+base_1+"лғансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => "телефон "+base_1+"лғансыз" ;
                                                                Pl => "телефон "+base_1+"лғансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "телефон "+base_1+"лған" ;
                                                                Pl => "телефон "+base_1+"лған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "телефон "+base_1+"лмағандым" ;
                                                                Pl => "телефон "+base_1+"лмағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => "телефон "+base_1+"лмағандың" ;
                                                                Pl => "телефон "+base_1+"лмағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "телефон "+base_1+"лмағандыңыз" ;
                                                                Pl => "телефон "+base_1+"лмағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "телефон "+base_1+"лмағанды" ;
                                                                Pl => "телефон "+base_1+"лмағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"қыратынмын" ;
                                                             Pl => base_1+"қыратынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"қыратынсың" ;
                                                             Pl => base_1+"қыратынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"қыратынсыз" ;
                                                             Pl => base_1+"қыратынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"қыратын" ;
                                                             Pl => base_1+"қыратын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"қырматынмын" ;
                                                             Pl => base_1+"қырматынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"қырматынсың" ;
                                                             Pl => base_1+"қырматынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"қырматынсыз" ;
                                                      Pl => base_1+"қырматынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"қырматын" ;
                                                    Pl => base_1+"қырматын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => "телефон "+base_1+"лдым" ;
                                                      Pl => "телефон "+base_1+"лдық"
                                                    } ;
                                        Formal => table {
                                                    Sg => "телефон "+base_1+"лдың" ;
                                                    Pl => "телефон "+base_1+"лдыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => "телефон "+base_1+"лдыңыз" ;
                               Pl => "телефон "+base_1+"лдыңыздар"
                             } ;
                        P2 => table {
                               Sg => "телефон "+base_1+"лды" ;
                               Pl => "телефон "+base_1+"лды"
                             } ;
                        P3 => table {
                               Sg => "телефон "+base_1+"лмадым" ;
                               Pl => "телефон "+base_1+"лмадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV045"
  } ;

mkV046 : Str -> V ;
mkV046 base =
  case base of {
    base_1+"ию" => lin V
      { Infinitive = base_1+"ию" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ип тұрмын" ;
                                                                Pl => base_1+"ип тұрмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ип тұрсың" ;
                                                                Pl => base_1+"ип тұрсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ип тұрсыз" ;
                                                                Pl => base_1+"ип тұрсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"ип тұр" ;
                                                                Pl => base_1+"ип тұр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ип тұрмаймын" ;
                                                                Pl => base_1+"ип тұрмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ип тұрмайсың" ;
                                                                Pl => base_1+"ип тұрмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"ип тұрмайсыз" ;
                                                             Pl => base_1+"ип тұрмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"ип тұрмай" ;
                                                             Pl => base_1+"ип тұрмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"йемiн" ;
                                                             Pl => base_1+"йемiз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"йесiң" ;
                                                             Pl => base_1+"йесiңдер"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"йесiз" ;
                                                             Pl => base_1+"йесiздер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"йедi" ;
                                                             Pl => base_1+"йедi"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"йемеймiн" ;
                                                            Pl => base_1+"йемейсiңдер"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"йемейсiң" ;
                                                            Pl => base_1+"йемейсiздер"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"йемейсiз" ;
                                                            Pl => base_1+"йемейдi"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"йемейдi" ;
                                                            Pl => base_1+"йемеймiз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"игенмiн" ;
                                                            Pl => base_1+"игенмiз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"игенсiң" ;
                                                            Pl => base_1+"игенсiңдер"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"игенсiз" ;
                                                                Pl => base_1+"игенсiздер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"иген" ;
                                                                Pl => base_1+"иген"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"имегендiм" ;
                                                                Pl => base_1+"имегендiк"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"имегендың" ;
                                                                Pl => base_1+"имегендыңдер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"имегендыңiз" ;
                                                                Pl => base_1+"имегендыңiздер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"имегендi" ;
                                                                Pl => base_1+"имегендi"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"йетiнмiн" ;
                                                             Pl => base_1+"йетiнмiз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"йетiнсiң" ;
                                                             Pl => base_1+"йетiнсiңдер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"йетiнсiз" ;
                                                             Pl => base_1+"йетiнсiздер"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"йетiн" ;
                                                             Pl => base_1+"йетiн"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"иметiнмiн" ;
                                                             Pl => base_1+"иметiнмiз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"иметiнсiң" ;
                                                             Pl => base_1+"иметiнсiңдер"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"иметiнсiз" ;
                                                      Pl => base_1+"иметiнсiздер"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"иметiн" ;
                                                    Pl => base_1+"иметiн"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"идiм" ;
                                                      Pl => base_1+"идiк"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"идiң" ;
                                                    Pl => base_1+"идiңдер"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"идiңiз" ;
                               Pl => base_1+"идiңiздер"
                             } ;
                        P2 => table {
                               Sg => base_1+"идi" ;
                               Pl => base_1+"идi"
                             } ;
                        P3 => table {
                               Sg => base_1+"имедiм" ;
                               Pl => base_1+"имедiк"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV046"
  } ;

mkV047 : Str -> V ;
mkV047 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"iп жүрмiн" ;
                                                                Pl => base_1+"iп жүрмiз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"iп жүрсiң" ;
                                                                Pl => base_1+"iп жүрсiңдер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"iп жүрсiз" ;
                                                                Pl => base_1+"iп жүрсiздер"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"iп жүр" ;
                                                                Pl => base_1+"iп жүр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"iп жүрмеймын" ;
                                                                Pl => base_1+"iп жүрсiздермыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"iп жүрмейсiң" ;
                                                                Pl => base_1+"iп жүрмейсiңдер"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"iп жүрмейсiз" ;
                                                             Pl => base_1+"iп жүрмейсiздер"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"iп жүрмей" ;
                                                             Pl => base_1+"iп жүрмей"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"емiн" ;
                                                             Pl => base_1+"емiз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"есiң" ;
                                                             Pl => base_1+"есiңдер"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"есiз" ;
                                                             Pl => base_1+"есiздер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"едi" ;
                                                             Pl => base_1+"едi"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"емеймiн" ;
                                                            Pl => base_1+"емейсiңдер"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"емейсiң" ;
                                                            Pl => base_1+"емейсiздер"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"емейсiз" ;
                                                            Pl => base_1+"емейдi"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"емейдi" ;
                                                            Pl => base_1+"емеймiз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"кенмiн" ;
                                                            Pl => base_1+"кенмiз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"кенсiң" ;
                                                            Pl => base_1+"кенсiңдер"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"кенсiз" ;
                                                                Pl => base_1+"кенсiздер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"кен" ;
                                                                Pl => base_1+"кен"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"пегендiм" ;
                                                                Pl => base_1+"пегендiк"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"пегендың" ;
                                                                Pl => base_1+"пегендыңдер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"пегендыңiз" ;
                                                                Pl => base_1+"пегендыңiздер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"пегендi" ;
                                                                Pl => base_1+"пегендi"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"етiнмiн" ;
                                                             Pl => base_1+"етiнмiз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"етiнсiң" ;
                                                             Pl => base_1+"етiнсiңдер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"етiнсiз" ;
                                                             Pl => base_1+"етiнсiздер"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"етiн" ;
                                                             Pl => base_1+"етiн"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"петiнмiн" ;
                                                             Pl => base_1+"петiнмiз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"петiнсiң" ;
                                                             Pl => base_1+"петiнсiңдер"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"петiнсiз" ;
                                                      Pl => base_1+"петiнсiздер"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"петiн" ;
                                                    Pl => base_1+"петiн"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"тiм" ;
                                                      Pl => base_1+"тiк"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"тiң" ;
                                                    Pl => base_1+"тiңдер"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"тiңiз" ;
                               Pl => base_1+"тiңiздер"
                             } ;
                        P2 => table {
                               Sg => base_1+"тi" ;
                               Pl => base_1+"тi"
                             } ;
                        P3 => table {
                               Sg => base_1+"педiм" ;
                               Pl => base_1+"педiк"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV047"
  } ;

mkV048 : Str -> V ;
mkV048 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"мын" ;
                                                                Pl => base_1+"мыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"сың" ;
                                                                Pl => base_1+"сыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"сыз" ;
                                                                Pl => base_1+"сыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1 ;
                                                                Pl => base_1
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"сыздармаймын" ;
                                                                Pl => base_1+"сыздармаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"сыздармайсың" ;
                                                                Pl => base_1+"сыздармайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"сыздармайсыз" ;
                                                             Pl => base_1+"сыздармайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"сыздармай" ;
                                                             Pl => base_1+"май"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"амын" ;
                                                             Pl => base_1+"амыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"асың" ;
                                                             Pl => base_1+"асыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"асыз" ;
                                                             Pl => base_1+"асыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ады" ;
                                                             Pl => base_1+"ады"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"амаймын" ;
                                                            Pl => base_1+"амайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"амайсың" ;
                                                            Pl => base_1+"амайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"амайсыз" ;
                                                            Pl => base_1+"амайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"амайды" ;
                                                            Pl => base_1+"амаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ғанмын" ;
                                                            Pl => base_1+"ғанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ғансың" ;
                                                            Pl => base_1+"ғансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ғансыз" ;
                                                                Pl => base_1+"ғансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ған" ;
                                                                Pl => base_1+"ған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағандым" ;
                                                                Pl => base_1+"мағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"мағандың" ;
                                                                Pl => base_1+"мағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"мағандыңыз" ;
                                                                Pl => base_1+"мағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"мағанды" ;
                                                                Pl => base_1+"мағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"атынмын" ;
                                                             Pl => base_1+"атынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"атынсың" ;
                                                             Pl => base_1+"атынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"атынсыз" ;
                                                             Pl => base_1+"атынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"атын" ;
                                                             Pl => base_1+"атын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"матынмын" ;
                                                             Pl => base_1+"матынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"матынсың" ;
                                                             Pl => base_1+"матынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"матынсыз" ;
                                                      Pl => base_1+"матынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"матын" ;
                                                    Pl => base_1+"матын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"дым" ;
                                                      Pl => base_1+"дық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"дың" ;
                                                    Pl => base_1+"дыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"дыңыз" ;
                               Pl => base_1+"дыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"ды" ;
                               Pl => base_1+"ды"
                             } ;
                        P3 => table {
                               Sg => base_1+"мадым" ;
                               Pl => base_1+"мадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV048"
  } ;

mkV049 : Str -> V ;
mkV049 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ып жатырмын" ;
                                                                Pl => base_1+"ып жатырмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ып жатырсың" ;
                                                                Pl => base_1+"ып жатырсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ып жатырсыз" ;
                                                                Pl => base_1+"ып жатырсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"ып жатыр" ;
                                                                Pl => base_1+"ып жатыр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ып жатырмаймын" ;
                                                                Pl => base_1+"ып жатырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ып жатырмайсың" ;
                                                                Pl => base_1+"ып жатырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"ып жатырмайсыз" ;
                                                             Pl => base_1+"ып жатырмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"ып жатырмай" ;
                                                             Pl => base_1+"ып жатырмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"амын" ;
                                                             Pl => base_1+"амыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"асың" ;
                                                             Pl => base_1+"асыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"асыз" ;
                                                             Pl => base_1+"асыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ады" ;
                                                             Pl => base_1+"ады"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"амаймын" ;
                                                            Pl => base_1+"амайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"амайсың" ;
                                                            Pl => base_1+"амайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"амайсыз" ;
                                                            Pl => base_1+"амайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"амайды" ;
                                                            Pl => base_1+"амаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"қанмын" ;
                                                            Pl => base_1+"қанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"қансың" ;
                                                            Pl => base_1+"қансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"қансыз" ;
                                                                Pl => base_1+"қансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"қан" ;
                                                                Pl => base_1+"қан"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"пағандым" ;
                                                                Pl => base_1+"пағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"пағандың" ;
                                                                Pl => base_1+"пағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"пағандыңыз" ;
                                                                Pl => base_1+"пағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"пағанды" ;
                                                                Pl => base_1+"пағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"атынмын" ;
                                                             Pl => base_1+"атынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"атынсың" ;
                                                             Pl => base_1+"атынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"атынсыз" ;
                                                             Pl => base_1+"атынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"атын" ;
                                                             Pl => base_1+"атын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"патынмын" ;
                                                             Pl => base_1+"патынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"патынсың" ;
                                                             Pl => base_1+"патынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"патынсыз" ;
                                                      Pl => base_1+"патынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"патын" ;
                                                    Pl => base_1+"патын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"дым" ;
                                                      Pl => base_1+"дық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"дың" ;
                                                    Pl => base_1+"дыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"дыңыз" ;
                               Pl => base_1+"дыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"ды" ;
                               Pl => base_1+"ды"
                             } ;
                        P3 => table {
                               Sg => base_1+"падым" ;
                               Pl => base_1+"падық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV049"
  } ;

mkV050 : Str -> V ;
mkV050 base =
  case base of {
    base_1+"у" => lin V
      { Infinitive = base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ып жатырмын" ;
                                                                Pl => base_1+"ып жатырмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ып жатырсың" ;
                                                                Pl => base_1+"ып жатырсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ып жатырсыз" ;
                                                                Pl => base_1+"ып жатырсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"ып жатыр" ;
                                                                Pl => base_1+"ып жатыр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ып жатырмаймын" ;
                                                                Pl => base_1+"ып жатырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ып жатырмайсың" ;
                                                                Pl => base_1+"ып жатырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"ып жатырмайсыз" ;
                                                             Pl => base_1+"ып жатырмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"ып жатырмай" ;
                                                             Pl => base_1+"ып жатырмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"амын" ;
                                                             Pl => base_1+"амыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"асың" ;
                                                             Pl => base_1+"асыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"асыз" ;
                                                             Pl => base_1+"асыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ады" ;
                                                             Pl => base_1+"ады"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"амаймын" ;
                                                            Pl => base_1+"амайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"амайсың" ;
                                                            Pl => base_1+"амайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"амайсыз" ;
                                                            Pl => base_1+"амайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"амайды" ;
                                                            Pl => base_1+"амаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"қанмын" ;
                                                            Pl => base_1+"қанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"қансың" ;
                                                            Pl => base_1+"қансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"қансыз" ;
                                                                Pl => base_1+"қансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"қан" ;
                                                                Pl => base_1+"қан"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"пағандым" ;
                                                                Pl => base_1+"пағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"пағандың" ;
                                                                Pl => base_1+"пағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"пағандыңыз" ;
                                                                Pl => base_1+"пағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"пағанды" ;
                                                                Pl => base_1+"пағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"атынмын" ;
                                                             Pl => base_1+"атынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"атынсың" ;
                                                             Pl => base_1+"атынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"атынсыз" ;
                                                             Pl => base_1+"атынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"атын" ;
                                                             Pl => base_1+"атын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"патынмын" ;
                                                             Pl => base_1+"патынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"патынсың" ;
                                                             Pl => base_1+"патынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"патынсыз" ;
                                                      Pl => base_1+"патынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"патын" ;
                                                    Pl => base_1+"патын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"тым" ;
                                                      Pl => base_1+"тық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"тың" ;
                                                    Pl => base_1+"тыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"тыңыз" ;
                               Pl => base_1+"тыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"ты" ;
                               Pl => base_1+"ты"
                             } ;
                        P3 => table {
                               Sg => base_1+"падым" ;
                               Pl => base_1+"падық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV050"
  } ;

mkV051 : Str -> V ;
mkV051 base =
  case base of {
    base_1+"у"+base_2@(?+?+?+?+?+?+?+?+?)+"у" => lin V
      { Infinitive = base_1+"у"+base_2+"у" ;
        Indicative = { Fut = nonExist ;
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+base_2+"iп жатырмын" ;
                                                                Pl => base_1+base_2+"iп жатырмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+base_2+"iп жатырсың" ;
                                                                Pl => base_1+base_2+"iп жатырсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+base_2+"iп жатырсыз" ;
                                                                Pl => base_1+base_2+"iп жатырсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+base_2+"iп жатыр" ;
                                                                Pl => base_1+base_2+"iп жатыр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+base_2+"iп жатырмаймын" ;
                                                                Pl => base_1+base_2+"iп жатырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+base_2+"iп жатырмайсың" ;
                                                                Pl => base_1+base_2+"iп жатырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+base_2+"iп жатырмайсыз" ;
                                                             Pl => base_1+base_2+"iп жатырмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+base_2+"iп жатырмай" ;
                                                             Pl => base_1+base_2+"iп жатырмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+base_2+"емiн" ;
                                                             Pl => base_1+base_2+"емiз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+base_2+"есiң" ;
                                                             Pl => base_1+base_2+"есiңдер"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+base_2+"есiз" ;
                                                             Pl => base_1+base_2+"есiздер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+base_2+"едi" ;
                                                             Pl => base_1+base_2+"едi"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+base_2+"емеймiн" ;
                                                            Pl => base_1+base_2+"емейсiңдер"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+base_2+"емейсiң" ;
                                                            Pl => base_1+base_2+"емейсiздер"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+base_2+"емейсiз" ;
                                                            Pl => base_1+base_2+"емейдi"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+base_2+"емейдi" ;
                                                            Pl => base_1+base_2+"емеймiз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+base_2+"генмiн" ;
                                                            Pl => base_1+base_2+"генмiз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+base_2+"генсiң" ;
                                                            Pl => base_1+base_2+"генсiңдер"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+base_2+"генсiз" ;
                                                                Pl => base_1+base_2+"генсiздер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+base_2+"ген" ;
                                                                Pl => base_1+base_2+"ген"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+base_2+"мегендiм" ;
                                                                Pl => base_1+base_2+"мегендiк"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+base_2+"мегендың" ;
                                                                Pl => base_1+base_2+"мегендыңдер"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+base_2+"мегендыңiз" ;
                                                                Pl => base_1+base_2+"мегендыңiздер"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+base_2+"мегендi" ;
                                                                Pl => base_1+base_2+"мегендi"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+base_2+"етiнмiн" ;
                                                             Pl => base_1+base_2+"етiнмiз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+base_2+"етiнсiң" ;
                                                             Pl => base_1+base_2+"етiнсiңдер"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+base_2+"етiнсiз" ;
                                                             Pl => base_1+base_2+"етiнсiздер"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+base_2+"етiн" ;
                                                             Pl => base_1+base_2+"етiн"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+base_2+"метiнмiн" ;
                                                             Pl => base_1+base_2+"метiнмiз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+base_2+"метiнсiң" ;
                                                             Pl => base_1+base_2+"метiнсiңдер"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+base_2+"метiнсiз" ;
                                                      Pl => base_1+base_2+"метiнсiздер"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+base_2+"метiн" ;
                                                    Pl => base_1+base_2+"метiн"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+base_2+"дiм" ;
                                                      Pl => base_1+base_2+"дiк"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+base_2+"дiң" ;
                                                    Pl => base_1+base_2+"дiңдер"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+base_2+"дiңiз" ;
                               Pl => base_1+base_2+"дiңiздер"
                             } ;
                        P2 => table {
                               Sg => base_1+base_2+"дi" ;
                               Pl => base_1+base_2+"дi"
                             } ;
                        P3 => table {
                               Sg => base_1+base_2+"медiм" ;
                               Pl => base_1+base_2+"медiк"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV051"
  } ;

mkV052 : Str -> V ;
mkV052 base =
  case base of {
    base_1+"қ"+base_2@?+"ру" => lin V
      { Infinitive = base_1+"қ"+base_2+"ру" ;
        Indicative = { Fut = nonExist ;
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"қ"+base_2+"рып тұрмын" ;
                                                                Pl => base_1+"қ"+base_2+"рып тұрмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"қ"+base_2+"рып жатырсың" ;
                                                                Pl => base_1+"қ"+base_2+"рып тұрсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"қ"+base_2+"рып жатырсыз" ;
                                                                Pl => base_1+"қ"+base_2+"рып тұрсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"қ"+base_2+"рып жатыр" ;
                                                                Pl => base_1+"қ"+base_2+"рып тұр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"қ"+base_2+"рып тұрмаймын" ;
                                                                Pl => base_1+"қ"+base_2+"рып жатырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"қ"+base_2+"рып жатырмайсың" ;
                                                                Pl => base_1+"қ"+base_2+"рып жатырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"қ"+base_2+"рып жатырмайсыз" ;
                                                             Pl => base_1+"қ"+base_2+"рып тұрмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"қ"+base_2+"рып тұрмай" ;
                                                             Pl => base_1+"қ"+base_2+"рып тұрмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "телефон "+base_1+"лам"+base_2+"н" ;
                                                             Pl => "телефон "+base_1+"лам"+base_2+"з"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"қ"+base_2+"расың" ;
                                                             Pl => "телефон "+base_1+"лас"+base_2+"ңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "телефон "+base_1+"лас"+base_2+"з" ;
                                                             Pl => "телефон "+base_1+"лас"+base_2+"здар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "телефон "+base_1+"лад"+base_2 ;
                                                             Pl => "телефон "+base_1+"лад"+base_2
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => "телефон "+base_1+"ламайм"+base_2+"н" ;
                                                            Pl => "телефон "+base_1+"ламайс"+base_2+"ңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => "телефон "+base_1+"ламайс"+base_2+"ң" ;
                                                            Pl => "телефон "+base_1+"ламайс"+base_2+"здар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => "телефон "+base_1+"ламайс"+base_2+"з" ;
                                                            Pl => base_1+"қ"+base_2+"рамайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => "телефон "+base_1+"ламайд"+base_2 ;
                                                            Pl => "телефон "+base_1+"ламайм"+base_2+"з"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"қ"+base_2+"рғанмын" ;
                                                            Pl => base_1+"қ"+base_2+"рғанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"қ"+base_2+"рғансың" ;
                                                            Pl => base_1+"қ"+base_2+"рғансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"қ"+base_2+"рғансыз" ;
                                                                Pl => base_1+"қ"+base_2+"рғансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"қ"+base_2+"рған" ;
                                                                Pl => base_1+"қ"+base_2+"рған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"қ"+base_2+"рмағандым" ;
                                                                Pl => base_1+"қ"+base_2+"рмағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"қ"+base_2+"рмағандың" ;
                                                                Pl => base_1+"қ"+base_2+"рмағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"қ"+base_2+"рмағандыңыз" ;
                                                                Pl => base_1+"қ"+base_2+"рмағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"қ"+base_2+"рмағанды" ;
                                                                Pl => base_1+"қ"+base_2+"рмағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => "телефон "+base_1+"лат"+base_2+"нмын" ;
                                                             Pl => "телефон "+base_1+"лат"+base_2+"нмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"қ"+base_2+"ратынсың" ;
                                                             Pl => "телефон "+base_1+"лат"+base_2+"нсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "телефон "+base_1+"лат"+base_2+"нсыз" ;
                                                             Pl => base_1+"қ"+base_2+"ратынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"қ"+base_2+"ратын" ;
                                                             Pl => base_1+"қ"+base_2+"ратын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "телефон "+base_1+"лмат"+base_2+"нмын" ;
                                                             Pl => base_1+"қ"+base_2+"рматынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "телефон "+base_1+"лмат"+base_2+"нсың" ;
                                                             Pl => "телефон "+base_1+"лмат"+base_2+"нсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => "телефон "+base_1+"лмат"+base_2+"нсыз" ;
                                                      Pl => base_1+"қ"+base_2+"рматынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"қ"+base_2+"рматын" ;
                                                    Pl => base_1+"қ"+base_2+"рматын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"қ"+base_2+"рдым" ;
                                                      Pl => base_1+"қ"+base_2+"рдық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"қ"+base_2+"рдың" ;
                                                    Pl => base_1+"қ"+base_2+"рдыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"қ"+base_2+"рдыңыз" ;
                               Pl => base_1+"қ"+base_2+"рдыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"қ"+base_2+"рды" ;
                               Pl => base_1+"қ"+base_2+"рды"
                             } ;
                        P3 => table {
                               Sg => base_1+"қ"+base_2+"рмадым" ;
                               Pl => base_1+"қ"+base_2+"рмадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV052"
  } ;

mkV053 : Str -> V ;
mkV053 base =
  case base of {
    base_1+"ғу" => lin V
      { Infinitive = base_1+"ғу" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ғып жатырмын" ;
                                                                Pl => base_1+"ғып жатырмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ғып жатырсың" ;
                                                                Pl => base_1+"ғып жатырсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ғып жатырсыз" ;
                                                                Pl => base_1+"ғып жатырсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"ғып жатыр" ;
                                                                Pl => base_1+"ғып жатыр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ғып жатырмаймын" ;
                                                                Pl => base_1+"ғып жатырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"ғып жатырмайсың" ;
                                                                Pl => base_1+"ғып жатырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"ғып жатырмайсыз" ;
                                                             Pl => base_1+"ғып жатырмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"ғып жатырмай" ;
                                                             Pl => base_1+"ғып жатырмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ғамын" ;
                                                             Pl => base_1+"ғамыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"ғасың" ;
                                                             Pl => base_1+"ғасыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"ғасыз" ;
                                                             Pl => base_1+"ғасыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ғады" ;
                                                             Pl => base_1+"ғады"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => base_1+"ғамаймын" ;
                                                            Pl => base_1+"ғамайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ғамайсың" ;
                                                            Pl => base_1+"ғамайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ғамайсыз" ;
                                                            Pl => base_1+"ғамайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => base_1+"ғамайды" ;
                                                            Pl => base_1+"ғамаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"ққанмын" ;
                                                            Pl => base_1+"ққанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"ққансың" ;
                                                            Pl => base_1+"ққансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"ққансыз" ;
                                                                Pl => base_1+"ққансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"ққан" ;
                                                                Pl => base_1+"ққан"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"қпағандым" ;
                                                                Pl => base_1+"қпағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"қпағандың" ;
                                                                Pl => base_1+"қпағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"қпағандыңыз" ;
                                                                Pl => base_1+"қпағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"қпағанды" ;
                                                                Pl => base_1+"қпағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => base_1+"ғатынмын" ;
                                                             Pl => base_1+"ғатынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"ғатынсың" ;
                                                             Pl => base_1+"ғатынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"ғатынсыз" ;
                                                             Pl => base_1+"ғатынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => base_1+"ғатын" ;
                                                             Pl => base_1+"ғатын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => base_1+"қпатынмын" ;
                                                             Pl => base_1+"қпатынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => base_1+"қпатынсың" ;
                                                             Pl => base_1+"қпатынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => base_1+"қпатынсыз" ;
                                                      Pl => base_1+"қпатынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"қпатын" ;
                                                    Pl => base_1+"қпатын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => base_1+"қтым" ;
                                                      Pl => base_1+"қтық"
                                                    } ;
                                        Formal => table {
                                                    Sg => base_1+"қтың" ;
                                                    Pl => base_1+"қтыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => base_1+"қтыңыз" ;
                               Pl => base_1+"қтыңыздар"
                             } ;
                        P2 => table {
                               Sg => base_1+"қты" ;
                               Pl => base_1+"қты"
                             } ;
                        P3 => table {
                               Sg => base_1+"қпадым" ;
                               Pl => base_1+"қпадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV053"
  } ;

mkV054 : Str -> V ;
mkV054 base =
  case base of {
    "шыд"+base_1+"у" => lin V
      { Infinitive = "шыд"+base_1+"у" ;
        Indicative = { Fut = nonExist ; --guessed
                       Pres = { Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => "шыд"+base_1+"п жатырмын" ;
                                                                Pl => "шыд"+base_1+"п жатырмыз"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "шыд"+base_1+"п жатырсың" ;
                                                                Pl => "шыд"+base_1+"п жатырсыңдар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "шыд"+base_1+"п жатырсыз" ;
                                                                Pl => "шыд"+base_1+"п жатырсыздар"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => "шыд"+base_1+"п жатыр" ;
                                                                Pl => "шыд"+base_1+"п жатыр"
                                                              } ;
                                                         P2 => table {
                                                                Sg => "шыд"+base_1+"п жатырмаймын" ;
                                                                Pl => "шыд"+base_1+"п жатырмаймыз"
                                                              } ;
                                                         P3 => table {
                                                                Sg => "шыд"+base_1+"п жатырмайсың" ;
                                                                Pl => "шыд"+base_1+"п жатырмайсыңдар"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => "шыд"+base_1+"п жатырмайсыз" ;
                                                             Pl => "шыд"+base_1+"п жатырмайсыздар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "шыд"+base_1+"п жатырмай" ;
                                                             Pl => "шыд"+base_1+"п жатырмай"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "шыд"+base_1+"ймын" ;
                                                             Pl => "шыд"+base_1+"ймыз"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => "шыд"+base_1+"йсың" ;
                                                             Pl => "шыд"+base_1+"йсыңдар"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "шыд"+base_1+"йсыз" ;
                                                             Pl => "шыд"+base_1+"йсыздар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "шыд"+base_1+"йды" ;
                                                             Pl => "шыд"+base_1+"йды"
                                                           }
                                                    }
                                           }
                              } ;
                       Past = { Perfect = table {
                                            Pos => table {
                                                     P1 => table {
                                                            Sg => "шыд"+base_1+"ймаймын" ;
                                                            Pl => "шыд"+base_1+"ймайсыңдар"
                                                          } ;
                                                     P2 => table {
                                                            Sg => "шыд"+base_1+"ймайсың" ;
                                                            Pl => "шыд"+base_1+"ймайсыздар"
                                                          } ;
                                                     P3 => table {
                                                            Sg => "шыд"+base_1+"ймайсыз" ;
                                                            Pl => "шыд"+base_1+"ймайды"
                                                          }
                                                   } ;
                                            Neg => table {
                                                     P1 => table {
                                                            Sg => "шыд"+base_1+"ймайды" ;
                                                            Pl => "шыд"+base_1+"ймаймыз"
                                                          } ;
                                                     P2 => table {
                                                            Sg => base_1+"лғанмын" ;
                                                            Pl => base_1+"лғанмыз"
                                                          } ;
                                                     P3 => table {
                                                            Sg => base_1+"лғансың" ;
                                                            Pl => base_1+"лғансыңдар"
                                                          }
                                                   }
                                          } ;
                                Progressive = table {
                                                Pos => table {
                                                         P1 => table {
                                                                Sg => base_1+"лғансыз" ;
                                                                Pl => base_1+"лғансыздар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"лған" ;
                                                                Pl => base_1+"лған"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"лмағандым" ;
                                                                Pl => base_1+"лмағандық"
                                                              }
                                                       } ;
                                                Neg => table {
                                                         P1 => table {
                                                                Sg => base_1+"лмағандың" ;
                                                                Pl => base_1+"лмағандыңдар"
                                                              } ;
                                                         P2 => table {
                                                                Sg => base_1+"лмағандыңыз" ;
                                                                Pl => base_1+"лмағандыңыздар"
                                                              } ;
                                                         P3 => table {
                                                                Sg => base_1+"лмағанды" ;
                                                                Pl => base_1+"лмағанды"
                                                              }
                                                       }
                                              } ;
                                noAspect = table {
                                             Pos => table {
                                                      P1 => table {
                                                             Sg => "шыд"+base_1+"йтынмын" ;
                                                             Pl => "шыд"+base_1+"йтынмыз"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "шыд"+base_1+"йтынсың" ;
                                                             Pl => "шыд"+base_1+"йтынсыңдар"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "шыд"+base_1+"йтынсыз" ;
                                                             Pl => "шыд"+base_1+"йтынсыздар"
                                                           }
                                                    } ;
                                             Neg => table {
                                                      P1 => table {
                                                             Sg => "шыд"+base_1+"йтын" ;
                                                             Pl => "шыд"+base_1+"йтын"
                                                           } ;
                                                      P2 => table {
                                                             Sg => "шыд"+base_1+"матынмын" ;
                                                             Pl => "шыд"+base_1+"матынмыз"
                                                           } ;
                                                      P3 => table {
                                                             Sg => "шыд"+base_1+"матынсың" ;
                                                             Pl => "шыд"+base_1+"матынсыңдар"
                                                           }
                                                    }
                                           }
                              }
                     } ;
        Imperative_Jussive = table {
                               Pos => table {
                                        Informal => table {
                                                      Sg => "шыд"+base_1+"матынсыз" ;
                                                      Pl => "шыд"+base_1+"матынсыздар"
                                                    } ;
                                        Formal => table {
                                                    Sg => "шыд"+base_1+"матын" ;
                                                    Pl => "шыд"+base_1+"матын"
                                                  }
                                      } ;
                               Neg => table {
                                        Informal => table {
                                                      Sg => "шыд"+base_1+"дым" ;
                                                      Pl => "шыд"+base_1+"дық"
                                                    } ;
                                        Formal => table {
                                                    Sg => "шыд"+base_1+"дың" ;
                                                    Pl => "шыд"+base_1+"дыңдар"
                                                  }
                                      }
                             } ;
        Subjunctive = table {
                        P1 => table {
                               Sg => "шыд"+base_1+"дыңыз" ;
                               Pl => "шыд"+base_1+"дыңыздар"
                             } ;
                        P2 => table {
                               Sg => "шыд"+base_1+"ды" ;
                               Pl => "шыд"+base_1+"ды"
                             } ;
                        P3 => table {
                               Sg => "шыд"+base_1+"мадым" ;
                               Pl => "шыд"+base_1+"мадық"
                             }
                      }
      };
    _ => error "Can't apply paradigm mkV054"
  } ;
}
