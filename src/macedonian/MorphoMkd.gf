resource MorphoMkd = open CatMkd, ResMkd, Predef, Prelude in {

oper

mkN001 : Str -> N ;
mkN001 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"и"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"от" ;
                               Pl => base_1+"ите"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ов" ;
                            Pl => base_1+"иве"
                          } ;
          Def Distal => table {
                          Sg => base_1+"он" ;
                          Pl => base_1+"ине"
                        }
        } ;
    count_form = base_1+"а" ;
    vocative = table {
                 Sg => base_1+"у" ;
                 Pl => base_1+"и"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN002 : Str -> N ;
mkN002 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"ови" --guessed
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"от" ;
                               Pl => base_1+"овите" --guessed
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ов" ;
                            Pl => base_1+"овиве" --guessed
                          } ;
          Def Distal => table {
                          Sg => base_1+"он" ;
                          Pl => base_1+"овине" --guessed
                        }
        } ;
    count_form = base_1+"а" ; --guessed
    vocative = table {
                 Sg => base_1+"у" ;
                 Pl => base_1+"ови" --guessed
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN003 : Str -> N ;
mkN003 base =
  case base of {
    base_1+"е"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"е"+base_2 ;
                         Pl => base_1+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"е"+base_2+"от" ;
                                   Pl => base_1+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"е"+base_2+"ов" ;
                                Pl => base_1+base_2+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"е"+base_2+"он" ;
                              Pl => base_1+base_2+"ине"
                            }
            } ;
        count_form = base_1+"е"+base_2+"а" ;
        vocative = table {
                     Sg => base_1+"е"+base_2+"у" ;
                     Pl => base_1+base_2+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN003"
  } ;

mkN004 : Str -> N ;
mkN004 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"ја" --guessed
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"то" ;
                               Pl => base_1+"јата" --guessed
                             } ;
          Def Proximal => table {
                            Sg => base_1+"во" ;
                            Pl => base_1+"јава" --guessed
                          } ;
          Def Distal => table {
                          Sg => base_1+"но" ;
                          Pl => base_1+"јана" --guessed
                        }
        } ;
    count_form = base_1+"ја" ; --guessed
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1+"ја" --guessed
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Neuter
  } ;

mkN005 : Str -> N ;
mkN005 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"ови"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"от" ;
                               Pl => base_1+"овите"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ов" ;
                            Pl => base_1+"овиве"
                          } ;
          Def Distal => table {
                          Sg => base_1+"он" ;
                          Pl => base_1+"овине"
                        }
        } ;
    count_form = base_1+"а" ;
    vocative = table {
                 Sg => base_1+"у" ;
                 Pl => base_1+"ови"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN006 : Str -> N ;
mkN006 base =
  case base of {
    base_1+"а" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ата" ;
                                   Pl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ава" ;
                                Pl => base_1+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"ана" ;
                              Pl => base_1+"ине"
                            }
            } ;
        count_form = base_1+"и" ;
        vocative = table {
                     Sg => base_1+"е" ;
                     Pl => base_1+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN006"
  } ;

mkN007 : Str -> N ;
mkN007 base =
  case base of {
    base_1+"а" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а" ;
                         Pl => base_1+"и" --guessed
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ата" ;
                                   Pl => base_1+"ите" --guessed
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ава" ;
                                Pl => base_1+"иве" --guessed
                              } ;
              Def Distal => table {
                              Sg => base_1+"ана" ;
                              Pl => base_1+"ине" --guessed
                            }
            } ;
        count_form = base_1+"и" ; --guessed
        vocative = table {
                     Sg => base_1+"о" ;
                     Pl => base_1+"и" --guessed
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN007"
  } ;

mkN008 : Str -> N ;
mkN008 base =
  case base of {
    base_1+"а" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ата" ;
                                   Pl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ава" ;
                                Pl => base_1+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"ана" ;
                              Pl => base_1+"ине"
                            }
            } ;
        count_form = base_1+"и" ;
        vocative = table {
                     Sg => base_1+"о" ;
                     Pl => base_1+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN008"
  } ;

mkN009 : Str -> N ;
mkN009 base =
  case base of {
    base_1+"е" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"е" ;
                         Pl => base_1+"иња"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ето" ;
                                   Pl => base_1+"ињата"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ево" ;
                                Pl => base_1+"ињава"
                              } ;
              Def Distal => table {
                              Sg => base_1+"ено" ;
                              Pl => base_1+"ињана"
                            }
            } ;
        count_form = base_1+"иња" ;
        vocative = table {
                     Sg => base_1+"е" ;
                     Pl => base_1+"иња"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Neuter
      };
    _ => error "Can't apply paradigm mkN009"
  } ;

mkN010 : Str -> N ;
mkN010 base =
  case base of {
    base_1+"к" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"к" ;
                         Pl => base_1+"ци"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"кот" ;
                                   Pl => base_1+"ците"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ков" ;
                                Pl => base_1+"циве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"кон" ;
                              Pl => base_1+"цине"
                            }
            } ;
        count_form = base_1+"ка" ;
        vocative = table {
                     Sg => base_1+"ку" ;
                     Pl => base_1+"ци"
                   } ;
         rel = \\_,_ => base_1 ;
         relType = Pref ;
         g = Masc
      };
    _ => error "Can't apply paradigm mkN010"
  } ;

mkN011 : Str -> N ;
mkN011 base =
  case base of {
    base_1+"ко" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ко" ;
                         Pl => base_1+"чи"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"кото" ;
                                   Pl => base_1+"чите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ково" ;
                                Pl => base_1+"чиве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"коно" ;
                              Pl => base_1+"чине"
                            }
            } ;
        count_form = base_1+"чи" ;
        vocative = table {
                     Sg => base_1+"ко" ;
                     Pl => base_1+"чи"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Neuter
      };
    _ => error "Can't apply paradigm mkN011"
  } ;

mkN012 : Str -> N ;
mkN012 base =
  case base of {
    base_1+"о" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"о" ;
                         Pl => base_1+"а"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ото" ;
                                   Pl => base_1+"ата"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ово" ;
                                Pl => base_1+"ава"
                              } ;
              Def Distal => table {
                              Sg => base_1+"оно" ;
                              Pl => base_1+"ана"
                            }
            } ;
        count_form = base_1+"а" ;
        vocative = table {
                     Sg => base_1+"о" ;
                     Pl => base_1+"а"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Neuter
      };
    _ => error "Can't apply paradigm mkN012"
  } ;

mkN013 : Str -> N ;
mkN013 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"и"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"та" ; --guessed
                               Pl => base_1+"ите" --guessed
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ва" ; --guessed
                            Pl => base_1+"иве" --guessed
                          } ;
          Def Distal => table {
                          Sg => base_1+"на" ; --guessed
                          Pl => base_1+"ине" --guessed
                        }
        } ;
    count_form = base_1+"и" ;
    vocative = table {
                 Sg => base_1 ; --guessed
                 Pl => base_1+"и" --guessed
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN014 : Str -> N ;
mkN014 base =
  case base of {
    base_1+"ин" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ин" ;
                         Pl => base_1+"а"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"инот" ;
                                   Pl => base_1+"ата"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"инов" ;
                                Pl => base_1+"ава"
                              } ;
              Def Distal => table {
                              Sg => base_1+"инон" ;
                              Pl => base_1+"ана"
                            }
            } ;
        count_form = base_1+"а" ;
        vocative = table {
                     Sg => base_1+"ине" ;
                     Pl => base_1+"а"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN014"
  } ;

mkN015 : Str -> N ;
mkN015 base =
  case base of {
    base_1+"н" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"н" ;
                         Pl => base_1
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"нот" ;
                                   Pl => base_1+"те"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"нов" ;
                                Pl => base_1+"ве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"нон" ;
                              Pl => base_1+"не"
                            }
            } ;
        count_form = base_1+"на" ;
        vocative = table {
                     Sg => base_1+"ну" ;
                     Pl => base_1
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN015"
  } ;

mkN016 : Str -> N ;
mkN016 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"еви" --guessed
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"от" ; --guessed
                               Pl => base_1+"евите" --guessed
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ов" ; --guessed
                            Pl => base_1+"евиве" --guessed
                          } ;
          Def Distal => table {
                          Sg => base_1+"он" ; --guessed
                          Pl => base_1+"евине" --guessed
                        }
        } ;
    count_form = base_1+"а" ; --guessed
    vocative = table {
                 Sg => base_1+"у" ; --guessed
                 Pl => base_1+"еви" --guessed
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN017 : Str -> N ;
mkN017 base =
  case base of {
    base_1+"а" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ата" ;
                                   Pl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ава" ;
                                Pl => base_1+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"ана" ;
                              Pl => base_1+"ине"
                            }
            } ;
        count_form = base_1+"и" ;
        vocative = table {
                     Sg => base_1+"о" ;
                     Pl => base_1+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN017"
  } ;

mkN018 : Str -> N ;
mkN018 base =
  case base of {
    base_1+"а" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а" ;
                         Pl => nonExist
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ата" ; --guessed
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ава" ; --guessed
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => base_1+"ана" ; --guessed
                              Pl => nonExist
                            }
            } ;
        count_form = nonExist ;
        vocative = table {
                     Sg => base_1+"о" ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN018"
  } ;

mkN019 : Str -> N ;
mkN019 base =
  case base of {
    base_1+"г" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"г" ;
                         Pl => base_1+"зи"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"гот" ;
                                   Pl => base_1+"зите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"гов" ;
                                Pl => base_1+"зиве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"гон" ;
                              Pl => base_1+"зине"
                            }
            } ;
        count_form = base_1+"га" ;
        vocative = table {
                     Sg => base_1+"гу" ;
                     Pl => base_1+"зи"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN019"
  } ;

mkN020 : Str -> N ;
mkN020 base =
  case base of {
    base_1+"ес" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ес" ;
                         Pl => base_1+"ци"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"есот" ;
                                   Pl => base_1+"ците"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"есов" ;
                                Pl => base_1+"циве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"есон" ;
                              Pl => base_1+"цине"
                            }
            } ;
        count_form = base_1+"еса" ;
        vocative = table {
                     Sg => base_1+"есу" ;
                     Pl => base_1+"ци"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN020"
  } ;

mkN021 : Str -> N ;
mkN021 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"еви"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"от" ;
                               Pl => base_1+"евите"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ов" ;
                            Pl => base_1+"евиве"
                          } ;
          Def Distal => table {
                          Sg => base_1+"он" ;
                          Pl => base_1+"евине"
                        }
        } ;
    count_form = base_1+"а" ;
    vocative = table {
                 Sg => base_1+"у" ;
                 Pl => base_1+"еви"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN022 : Str -> N ;
mkN022 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"и"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"от" ;
                               Pl => base_1+"ите"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ов" ;
                            Pl => base_1+"иве"
                          } ;
          Def Distal => table {
                          Sg => base_1+"он" ;
                          Pl => base_1+"ине"
                        }
        } ;
    count_form = base_1+"а" ;
    vocative = table {
                 Sg => base_1+"е" ;
                 Pl => base_1+"и"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN023 : Str -> N ;
mkN023 base =
  case base of {
    base_1+"ја" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ја" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"јата" ;
                                   Pl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"јава" ;
                                Pl => base_1+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"јана" ;
                              Pl => base_1+"ине"
                            }
            } ;
        count_form = base_1+"и" ;
        vocative = table {
                     Sg => base_1+"јо" ;
                     Pl => base_1+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN023"
  } ;

mkN024 : Str -> N ;
mkN024 base =
  case base of {
    base_1+"т" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"т" ;
                         Pl => base_1+"ќа"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"тот" ;
                                   Pl => base_1+"ќата"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"тов" ;
                                Pl => base_1+"ќава"
                              } ;
              Def Distal => table {
                              Sg => base_1+"тон" ;
                              Pl => base_1+"ќана"
                            }
            } ;
        count_form = base_1+"та" ;
        vocative = table {
                     Sg => base_1+"те" ;
                     Pl => base_1+"ќа"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN024"
  } ;

mkN025 : Str -> N ;
mkN025 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"ови" --guessed
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"от" ;
                               Pl => base_1+"овите" --guessed
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ов" ;
                            Pl => base_1+"овиве" --guessed
                          } ;
          Def Distal => table {
                          Sg => base_1+"он" ;
                          Pl => base_1+"овине" --guessed
                        }
        } ;
    count_form = base_1+"а" ; --guessed
    vocative = table {
                 Sg => base_1+"е" ;
                 Pl => base_1+"ови" --guessed
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN026 : Str -> N ;
mkN026 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"ја"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"то" ;
                               Pl => base_1+"јата"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"во" ;
                            Pl => base_1+"јава"
                          } ;
          Def Distal => table {
                          Sg => base_1+"но" ;
                          Pl => base_1+"јана"
                        }
        } ;
    count_form = base_1+"ја" ;
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1+"ја"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Neuter
  } ;

mkN027 : Str -> N ;
mkN027 base =
  case base of {
    base_1+"а"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а"+base_2 ;
                         Pl => base_1+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"а"+base_2+"от" ;
                                   Pl => base_1+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"а"+base_2+"ов" ;
                                Pl => base_1+base_2+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"а"+base_2+"он" ;
                              Pl => base_1+base_2+"ине"
                            }
            } ;
        count_form = base_1+"а"+base_2+"а" ;
        vocative = table {
                     Sg => base_1+"а"+base_2+"е" ;
                     Pl => base_1+base_2+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN027"
  } ;

mkN028 : Str -> N ;
mkN028 base =
  case base of {
    base_1+"е" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"е" ;
                         Pl => base_1+"а"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ето" ;
                                   Pl => base_1+"ата"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ево" ;
                                Pl => base_1+"ава"
                              } ;
              Def Distal => table {
                              Sg => base_1+"ено" ;
                              Pl => base_1+"ана"
                            }
            } ;
        count_form = base_1+"а" ;
        vocative = table {
                     Sg => base_1+"е" ;
                     Pl => base_1+"а"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN028"
  } ;

mkN029 : Str -> N ;
mkN029 base =
  case base of {
    base_1+"г" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"г" ;
                         Pl => base_1+"гови"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"гот" ;
                                   Pl => base_1+"говите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"гов" ;
                                Pl => base_1+"говиве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"гон" ;
                              Pl => base_1+"говине"
                            }
            } ;
        count_form = base_1+"га" ;
        vocative = table {
                     Sg => base_1+"же" ;
                     Pl => base_1+"гови"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN029"
  } ;

mkN030 : Str -> N ;
mkN030 base =
  case base of {
    base_1+"га" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"га" ;
                         Pl => base_1+"зе"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"гата" ;
                                   Pl => base_1+"зете"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"гава" ;
                                Pl => base_1+"зеве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"гана" ;
                              Pl => base_1+"зене"
                            }
            } ;
        count_form = base_1+"зе" ;
        vocative = table {
                     Sg => base_1+"го" ;
                     Pl => base_1+"зе"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN030"
  } ;

mkN031 : Str -> N ;
mkN031 base =
  case base of {
    base_1+"а" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а" ;
                         Pl => base_1+"и" --guessed
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ата" ;
                                   Pl => base_1+"ите" --guessed
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ава" ;
                                Pl => base_1+"иве" --guessed
                              } ;
              Def Distal => table {
                              Sg => base_1+"ана" ;
                              Pl => base_1+"ине" --guessed
                            }
            } ;
        count_form = base_1+"и" ; --guessed
        vocative = table {
                     Sg => base_1+"е" ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN031"
  } ;

mkN032 : Str -> N ;
mkN032 base =
  case base of {
    base_1+"тец" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"тец" ;
                         Pl => base_1+"тци"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"тецот" ;
                                   Pl => base_1+"тците"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"тецов" ;
                                Pl => base_1+"тциве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"тецон" ;
                              Pl => base_1+"тцине"
                            }
            } ;
        count_form = base_1+"теца" ;
        vocative = table {
                     Sg => base_1+"че" ;
                     Pl => base_1+"тци"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN032"
  } ;

mkN033 : Str -> N ;
mkN033 base =
  case base of {
    base_1+"е" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"е" ;
                         Pl => base_1+"ја"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ето" ;
                                   Pl => base_1+"јата"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ево" ;
                                Pl => base_1+"јава"
                              } ;
              Def Distal => table {
                              Sg => base_1+"ено" ;
                              Pl => base_1+"јана"
                            }
            } ;
        count_form = base_1+"ја" ;
        vocative = table {
                     Sg => base_1+"е" ;
                     Pl => base_1+"ја"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Neuter
      };
    _ => error "Can't apply paradigm mkN033"
  } ;

mkN034 : Str -> N ;
mkN034 base =
  case base of {
    base_1+"е" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"е" ;
                         Pl => base_1+"а"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ето" ;
                                   Pl => base_1+"ата"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ево" ;
                                Pl => base_1+"ава"
                              } ;
              Def Distal => table {
                              Sg => base_1+"ено" ;
                              Pl => base_1+"ана"
                            }
            } ;
        count_form = base_1+"а" ;
        vocative = table {
                     Sg => base_1+"е" ;
                     Pl => base_1+"а"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Neuter
      };
    _ => error "Can't apply paradigm mkN034"
  } ;

mkN035 : Str -> N ;
mkN035 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"и"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"та" ;
                               Pl => base_1+"ите"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ва" ;
                            Pl => base_1+"иве"
                          } ;
          Def Distal => table {
                          Sg => base_1+"на" ;
                          Pl => base_1+"ине"
                        }
        } ;
    count_form = base_1+"и" ;
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1+"и"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Fem
  } ;

mkN036 : Str -> N ;
mkN036 base =
  case base of {
    base_1+"а"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а"+base_2 ;
                         Pl => base_1+base_2+"ови" --guessed
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+base_2+"от" ;
                                   Pl => base_1+base_2+"овите" --guessed
                                 } ;
              Def Proximal => table {
                                Sg => base_1+base_2+"ов" ;
                                Pl => base_1+base_2+"овиве" --guessed
                              } ;
              Def Distal => table {
                              Sg => base_1+base_2+"он" ;
                              Pl => base_1+base_2+"овине" --guessed
                            }
            } ;
        count_form = base_1+"а"+base_2+"а" ; --guessed
        vocative = table {
                     Sg => base_1+base_2+"у" ;
                     Pl => base_1+base_2+"ови" --guessed
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN036"
  } ;

mkN037 : Str -> N ;
mkN037 base =
  case base of {
    base_1+"х" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"х" ;
                         Pl => base_1+"си"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"хот" ;
                                   Pl => base_1+"сите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"хов" ;
                                Pl => base_1+"сиве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"хон" ;
                              Pl => base_1+"сине"
                            }
            } ;
        count_form = base_1+"ха" ;
        vocative = table {
                     Sg => base_1+"ху" ;
                     Pl => base_1+"си"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN037"
  } ;

mkN038 : Str -> N ;
mkN038 base =
  case base of {
    base_1+"а" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а" ;
                         Pl => base_1+"и" --guessed
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ата" ; --guessed
                                   Pl => base_1+"ите" --guessed
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ава" ; --guessed
                                Pl => base_1+"иве" --guessed
                              } ;
              Def Distal => table {
                              Sg => base_1+"ана" ; --guessed
                              Pl => base_1+"ине" --guessed
                            }
            } ;
        count_form = base_1+"и" ; --guessed
        vocative = table {
                     Sg => base_1+"о" ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN038"
  } ;

mkN039 : Str -> N ;
mkN039 base =
  case base of {
    base_1+"ј" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ј" ;
                         Pl => base_1+"еви"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"јот" ;
                                   Pl => base_1+"евите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"јов" ;
                                Pl => base_1+"евиве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"јон" ;
                              Pl => base_1+"евине"
                            }
            } ;
        count_form = base_1+"ја" ;
        vocative = table {
                     Sg => base_1+"ју" ;
                     Pl => base_1+"еви"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN039"
  } ;

mkN040 : Str -> N ;
mkN040 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"а"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"то" ;
                               Pl => base_1+"ата"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"во" ;
                            Pl => base_1+"ава"
                          } ;
          Def Distal => table {
                          Sg => base_1+"но" ;
                          Pl => base_1+"ана"
                        }
        } ;
    count_form = base_1+"а" ;
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1+"а"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Neuter
  } ;

mkN041 : Str -> N ;
mkN041 base =
  case base of {
    base_1+"те" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"те" ;
                         Pl => base_1+"ца"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"тето" ;
                                   Pl => base_1+"цата"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"тево" ;
                                Pl => base_1+"цава"
                              } ;
              Def Distal => table {
                              Sg => base_1+"тено" ;
                              Pl => base_1+"цана"
                            }
            } ;
        count_form = base_1+"ца" ;
        vocative = table {
                     Sg => base_1+"те" ;
                     Pl => base_1+"ца"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Neuter
      };
    _ => error "Can't apply paradigm mkN041"
  } ;

mkN042 : Str -> N ;
mkN042 base =
  case base of {
    base_1+"н" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"н" ;
                         Pl => base_1+"ња"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"нот" ;
                                   Pl => base_1+"њата"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"нов" ;
                                Pl => base_1+"њава"
                              } ;
              Def Distal => table {
                              Sg => base_1+"нон" ;
                              Pl => base_1+"њана"
                            }
            } ;
        count_form = base_1+"на" ;
        vocative = table {
                     Sg => base_1+"ну" ;
                     Pl => base_1+"ња"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN042"
  } ;

mkN043 : Str -> N ;
mkN043 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"и"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"а" ;
                               Pl => base_1+"ите"
                             } ;
          Def Proximal => table {
                            Sg => nonExist ;
                            Pl => base_1+"иве"
                          } ;
          Def Distal => table {
                          Sg => nonExist ;
                          Pl => base_1+"ине"
                        }
        } ;
    count_form = base_1+"и" ;
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1+"и"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Fem
  } ;

mkN044 : Str -> N ;
mkN044 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"ови"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"то" ; --guessed
                               Pl => nonExist
                             } ;
          Def Proximal => table {
                            Sg => base_1+"во" ; --guessed
                            Pl => nonExist
                          } ;
          Def Distal => table {
                          Sg => base_1+"но" ; --guessed
                          Pl => nonExist
                        }
        } ;
    count_form = base_1+"ови" ;
    vocative = table {
                 Sg => base_1 ; --guessed
                 Pl => nonExist
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN045 : Str -> N ;
mkN045 base =
  case base of {
    base_1+"а" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ата" ; --guessed
                                   Pl => base_1+"ите" --guessed
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ава" ; --guessed
                                Pl => base_1+"иве" --guessed
                              } ;
              Def Distal => table {
                              Sg => base_1+"ана" ; --guessed
                              Pl => base_1+"ине" --guessed
                            }
            } ;
        count_form = base_1+"и" ;
        vocative = table {
                     Sg => base_1+"е" ; --guessed
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN045"
  } ;

mkN046 : Str -> N ;
mkN046 base =
  case base of {
    base_1+"а"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а"+base_2 ;
                         Pl => base_1+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"а"+base_2+"от" ;
                                   Pl => base_1+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"а"+base_2+"ов" ;
                                Pl => base_1+base_2+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"а"+base_2+"он" ;
                              Pl => base_1+base_2+"ине"
                            }
            } ;
        count_form = base_1+"а"+base_2+"а" ;
        vocative = table {
                     Sg => base_1+"а"+base_2+"у" ;
                     Pl => base_1+base_2+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN046"
  } ;

mkN047 : Str -> N ;
mkN047 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"а"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"то" ;
                               Pl => base_1+"ата"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"во" ;
                            Pl => base_1+"ава"
                          } ;
          Def Distal => table {
                          Sg => base_1+"но" ;
                          Pl => base_1+"ана"
                        }
        } ;
    count_form = base_1+"а" ;
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1+"а"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN048 : Str -> N ;
mkN048 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"-а"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"-то" ;
                               Pl => base_1+"-ата"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"-во" ;
                            Pl => base_1+"-ава"
                          } ;
          Def Distal => table {
                          Sg => base_1+"-но" ;
                          Pl => base_1+"-ана"
                        }
        } ;
    count_form = base_1+"-а" ;
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1+"-а"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Neuter
  } ;

mkN049 : Str -> N ;
mkN049 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"вци" --guessed
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"то" ;
                               Pl => base_1+"вците" --guessed
                             } ;
          Def Proximal => table {
                            Sg => base_1+"во" ;
                            Pl => base_1+"вциве" --guessed
                          } ;
          Def Distal => table {
                          Sg => base_1+"но" ;
                          Pl => base_1+"вцине" --guessed
                        }
        } ;
    count_form = base_1+"вци" ; --guessed
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1+"вци" --guessed
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN050 : Str -> N ;
mkN050 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"и" --guessed
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"а" ;
                               Pl => base_1+"те" --guessed
                             } ;
          Def Proximal => table {
                            Sg => nonExist ;
                            Pl => base_1+"ве" --guessed
                          } ;
          Def Distal => table {
                          Sg => nonExist ;
                          Pl => base_1+"не" --guessed
                        }
        } ;
    count_form = base_1+"и" ; --guessed
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1 --guessed
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Fem
  } ;

mkN051 : Str -> N ;
mkN051 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"и" --guessed
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"та" ;
                               Pl => base_1+"ите" --guessed
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ва" ; --guessed
                            Pl => base_1+"иве" --guessed
                          } ;
          Def Distal => table {
                          Sg => base_1+"на" ; --guessed
                          Pl => base_1+"ине" --guessed
                        }
        } ;
    count_form = base_1+"и" ; --guessed
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1+"и" --guessed
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Fem
  } ;

mkN052 : Str -> N ;
mkN052 base =
  case base of {
    base_1+"е"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"е"+base_2 ;
                         Pl => base_1+base_2+"ови"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"е"+base_2+"от" ;
                                   Pl => base_1+base_2+"овите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"е"+base_2+"ов" ;
                                Pl => base_1+base_2+"овиве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"е"+base_2+"он" ;
                              Pl => base_1+base_2+"овине"
                            }
            } ;
        count_form = base_1+"е"+base_2+"а" ;
        vocative = table {
                     Sg => base_1+"е"+base_2+"у" ;
                     Pl => base_1+base_2+"ови"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN052"
  } ;

mkN053 : Str -> N ;
mkN053 base =
  case base of {
    base_1+"ја" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ја" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"јата" ;
                                   Pl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"јава" ;
                                Pl => base_1+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"јана" ;
                              Pl => base_1+"ине"
                            }
            } ;
        count_form = base_1+"и" ;
        vocative = table {
                     Sg => base_1+"јо" ;
                     Pl => base_1+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN053"
  } ;

mkN054 : Str -> N ;
mkN054 base =
  case base of {
    "човек" => lin N
      { s = table {
              Indef => table {
                         Sg => "човек" ;
                         Pl => "луѓе"
                       } ;
              Def Unspecified => table {
                                   Sg => "човекот" ;
                                   Pl => "луѓето"
                                 } ;
              Def Proximal => table {
                                Sg => "човеков" ;
                                Pl => "луѓево"
                              } ;
              Def Distal => table {
                              Sg => "човекон" ;
                              Pl => "луѓено"
                            }
            } ;
        count_form = "души" ;
        vocative = table {
                     Sg => "човеку" ;
                     Pl => "луѓе"
                   } ;
        rel = \\_,_ => "човек" ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN054"
  } ;

mkN055 : Str -> N ;
mkN055 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"и"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"та" ;
                               Pl => base_1+"ите"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ва" ;
                            Pl => base_1+"иве"
                          } ;
          Def Distal => table {
                          Sg => base_1+"на" ;
                          Pl => base_1+"ине"
                        }
        } ;
    count_form = base_1+"и" ;
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1+"и"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN056 : Str -> N ;
mkN056 base =
  case base of {
    base_1+"а"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а"+base_2 ;
                         Pl => base_1+base_2+"ови"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+base_2+"от" ;
                                   Pl => base_1+base_2+"овите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+base_2+"ов" ;
                                Pl => base_1+base_2+"овиве"
                              } ;
              Def Distal => table {
                              Sg => base_1+base_2+"он" ;
                              Pl => base_1+base_2+"овине"
                            }
            } ;
        count_form = base_1+"а"+base_2+"а" ;
        vocative = table {
                     Sg => base_1+base_2+"у" ;
                     Pl => base_1+base_2+"ови"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN056"
  } ;

mkN057 : Str -> N ;
mkN057 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"ови"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"та" ;
                               Pl => base_1+"овите"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ва" ;
                            Pl => base_1+"овиве"
                          } ;
          Def Distal => table {
                          Sg => base_1+"на" ;
                          Pl => base_1+"овине"
                        }
        } ;
    count_form = base_1+"а" ;
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1+"ови"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN058 : Str -> N ;
mkN058 base =
  case base of {
    base_1+"о"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"о"+base_2 ;
                         Pl => base_1+base_2+"овци"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"о"+base_2+"от" ;
                                   Pl => base_1+base_2+"овците"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"о"+base_2+"ов" ;
                                Pl => base_1+base_2+"овциве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"о"+base_2+"он" ;
                              Pl => base_1+base_2+"овцине"
                            }
            } ;
        count_form = base_1+"о"+base_2+"а" ;
        vocative = table {
                     Sg => base_1+base_2+"е" ;
                     Pl => base_1+base_2+"овци"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN058"
  } ;

mkN059 : Str -> N ;
mkN059 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"ци"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"от" ;
                               Pl => base_1+"ците"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ов" ;
                            Pl => base_1+"циве"
                          } ;
          Def Distal => table {
                          Sg => base_1+"он" ;
                          Pl => base_1+"цине"
                        }
        } ;
    count_form = base_1+"а" ;
    vocative = table {
                 Sg => base_1+"у" ;
                 Pl => base_1+"ци"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN060 : Str -> N ;
mkN060 base =
  case base of {
    base_1+"а" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ата" ;
                                   Pl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ава" ;
                                Pl => base_1+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"ана" ;
                              Pl => base_1+"ине"
                            }
            } ;
        count_form = base_1+"и" ;
        vocative = table {
                     Sg => base_1+"е" ;
                     Pl => base_1+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN060"
  } ;

mkN061 : Str -> N ;
mkN061 base =
  case base of {
    base_1+"ч"+base_2@?+"н" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ч"+base_2+"н" ;
                         Pl => base_1+"ц"+base_2
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ч"+base_2+"нот" ;
                                   Pl => base_1+"ц"+base_2+"те"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ч"+base_2+"нов" ;
                                Pl => base_1+"ц"+base_2+"ве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"ч"+base_2+"нон" ;
                              Pl => base_1+"ц"+base_2+"не"
                            }
            } ;
        count_form = base_1+"ч"+base_2+"на" ;
        vocative = table {
                     Sg => base_1+"ч"+base_2+"ну" ;
                     Pl => base_1+"ц"+base_2
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN061"
  } ;

mkN062 : Str -> N ;
mkN062 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"ки"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"от" ;
                               Pl => base_1+"ките"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ов" ;
                            Pl => base_1+"киве"
                          } ;
          Def Distal => table {
                          Sg => base_1+"он" ;
                          Pl => base_1+"кине"
                        }
        } ;
    count_form = base_1+"а" ;
    vocative = table {
                 Sg => base_1+"у" ;
                 Pl => base_1+"ки"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN063 : Str -> N ;
mkN063 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1 --guessed
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"то" ; --guessed
                               Pl => base_1+"та" --guessed
                             } ;
          Def Proximal => table {
                            Sg => base_1+"во" ; --guessed
                            Pl => base_1+"ва" --guessed
                          } ;
          Def Distal => table {
                          Sg => base_1+"но" ; --guessed
                          Pl => base_1+"на" --guessed
                        }
        } ;
    count_form = base_1 ; --guessed
    vocative = table {
                 Sg => base_1 ; --guessed
                 Pl => base_1 --guessed
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Neuter
  } ;

mkN064 : Str -> N ;
mkN064 base =
  case base of {
    base_1+"о"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"о"+base_2 ;
                         Pl => base_1+base_2+"ишта"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"о"+base_2+"от" ;
                                   Pl => base_1+base_2+"иштата"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"о"+base_2+"ов" ;
                                Pl => base_1+base_2+"иштава"
                              } ;
              Def Distal => table {
                              Sg => base_1+"о"+base_2+"он" ;
                              Pl => base_1+base_2+"иштана"
                            }
            } ;
        count_form = base_1+"о"+base_2+"а" ;
        vocative = table {
                     Sg => base_1+"о"+base_2+"у" ;
                     Pl => base_1+base_2+"ишта"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN064"
  } ;

mkN065 : Str -> N ;
mkN065 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"ови"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"от" ;
                               Pl => base_1+"овите"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ов" ;
                            Pl => base_1+"овиве"
                          } ;
          Def Distal => table {
                          Sg => base_1+"он" ;
                          Pl => base_1+"овине"
                        }
        } ;
    count_form = base_1+"а" ;
    vocative = table {
                 Sg => base_1+"е" ;
                 Pl => base_1+"ови"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN066 : Str -> N ;
mkN066 base =
  case base of {
    base_1+"т" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"т" ;
                         Pl => base_1+"ти"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"та" ;
                                   Pl => base_1+"тите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ва" ;
                                Pl => base_1+"тиве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"на" ;
                              Pl => base_1+"тине"
                            }
            } ;
        count_form = base_1+"ти" ;
        vocative = table {
                     Sg => base_1+"т" ;
                     Pl => base_1+"ти"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN066"
  } ;

mkN067 : Str -> N ;
mkN067 base =
  case base of {
    base_1+"ј" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ј" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"јот" ;
                                   Pl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"јов" ;
                                Pl => base_1+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"јон" ;
                              Pl => base_1+"ине"
                            }
            } ;
        count_form = base_1+"ја" ;
        vocative = table {
                     Sg => base_1+"ју" ;
                     Pl => base_1+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN067"
  } ;

mkN068 : Str -> N ;
mkN068 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"и" --guessed
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"та" ;
                               Pl => base_1+"ите" --guessed
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ва" ;
                            Pl => base_1+"иве" --guessed
                          } ;
          Def Distal => table {
                          Sg => base_1+"на" ;
                          Pl => base_1+"ине" --guessed
                        }
        } ;
    count_form = base_1+"и" ; --guessed
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1+"и" --guessed
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Fem
  } ;

mkN069 : Str -> N ;
mkN069 base =
  case base of {
    base_1+"зе"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"зе"+base_2 ;
                         Pl => base_1+"с"+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"зе"+base_2+"от" ;
                                   Pl => base_1+"с"+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"зе"+base_2+"ов" ;
                                Pl => base_1+"с"+base_2+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"зе"+base_2+"он" ;
                              Pl => base_1+"с"+base_2+"ине"
                            }
            } ;
        count_form = base_1+"зе"+base_2+"а" ;
        vocative = table {
                     Sg => base_1+"зе"+base_2+"у" ;
                     Pl => base_1+"с"+base_2+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN069"
  } ;

mkN070 : Str -> N ;
mkN070 base =
  case base of {
    base_1+"к" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"к" ;
                         Pl => base_1+"ци"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"кта" ;
                                   Pl => base_1+"ците"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ква" ;
                                Pl => base_1+"циве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"кна" ;
                              Pl => base_1+"цине"
                            }
            } ;
        count_form = base_1+"ка" ;
        vocative = table {
                     Sg => base_1+"к" ;
                     Pl => base_1+"ци"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN070"
  } ;

mkN071 : Str -> N ;
mkN071 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1 --guessed
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"та" ;
                               Pl => base_1+"те" --guessed
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ва" ;
                            Pl => base_1+"ве" --guessed
                          } ;
          Def Distal => table {
                          Sg => base_1+"на" ;
                          Pl => base_1+"не" --guessed
                        }
        } ;
    count_form = base_1 ; --guessed
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1 --guessed
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN072 : Str -> N ;
mkN072 base =
  case base of {
    base_1+"о" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"о" ;
                         Pl => base_1+"ја"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ото" ;
                                   Pl => base_1+"јата"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ово" ;
                                Pl => base_1+"јава"
                              } ;
              Def Distal => table {
                              Sg => base_1+"оно" ;
                              Pl => base_1+"јана"
                            }
            } ;
        count_form = base_1+"ја" ;
        vocative = table {
                     Sg => base_1+"о" ;
                     Pl => base_1+"ја"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Neuter
      };
    _ => error "Can't apply paradigm mkN072"
  } ;

mkN073 : Str -> N ;
mkN073 base =
  case base of {
    base_1+"ше"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ше"+base_2 ;
                         Pl => base_1+"в"+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ше"+base_2+"от" ;
                                   Pl => base_1+"в"+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ше"+base_2+"ов" ;
                                Pl => base_1+"в"+base_2+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"ше"+base_2+"он" ;
                              Pl => base_1+"в"+base_2+"ине"
                            }
            } ;
        count_form = base_1+"ше"+base_2+"а" ;
        vocative = table {
                     Sg => base_1+"ше"+base_2+"у" ;
                     Pl => base_1+"в"+base_2+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN073"
  } ;

mkN074 : Str -> N ;
mkN074 base =
  case base of {
    base_1+"ка" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ка" ;
                         Pl => base_1+"це"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ката" ;
                                   Pl => base_1+"цете"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"кава" ;
                                Pl => base_1+"цеве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"кана" ;
                              Pl => base_1+"цене"
                            }
            } ;
        count_form = base_1+"це" ;
        vocative = table {
                     Sg => base_1+"ко" ;
                     Pl => base_1+"це"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN074"
  } ;

mkN075 : Str -> N ;
mkN075 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"от" ; --guessed
                               Pl => base_1+"те"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ов" ; --guessed
                            Pl => base_1+"ве"
                          } ;
          Def Distal => table {
                          Sg => base_1+"он" ; --guessed
                          Pl => base_1+"не"
                        }
        } ;
    count_form = base_1 ;
    vocative = table {
                 Sg => base_1 ; --guessed
                 Pl => base_1
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN076 : Str -> N ;
mkN076 base =
  case base of {
    base_1+"о" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"о" ;
                         Pl => base_1+"ена"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ото" ;
                                   Pl => base_1+"ената"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ово" ;
                                Pl => base_1+"енава"
                              } ;
              Def Distal => table {
                              Sg => base_1+"оно" ;
                              Pl => base_1+"енана"
                            }
            } ;
        count_form = base_1+"ена" ;
        vocative = table {
                     Sg => base_1+"о" ;
                     Pl => base_1+"ена"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Neuter
      };
    _ => error "Can't apply paradigm mkN076"
  } ;

mkN077 : Str -> N ;
mkN077 base =
  case base of {
    base_1+"е" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"е" ;
                         Pl => base_1+"ја"
                       } ;
              Def Unspecified => table {
                                   Sg => nonExist ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => nonExist ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            }
            } ;
        count_form = base_1+"ја" ;
        vocative = table {
                     Sg => nonExist ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Neuter
      };
    _ => error "Can't apply paradigm mkN077"
  } ;

mkN078 : Str -> N ;
mkN078 base =
  case base of {
    base_1+"бе"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"бе"+base_2 ;
                         Pl => base_1+"п"+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"бе"+base_2+"от" ;
                                   Pl => base_1+"п"+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"бе"+base_2+"ов" ;
                                Pl => base_1+"п"+base_2+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"бе"+base_2+"он" ;
                              Pl => base_1+"п"+base_2+"ине"
                            }
            } ;
        count_form = base_1+"бе"+base_2+"а" ;
        vocative = table {
                     Sg => base_1+"бе"+base_2+"у" ;
                     Pl => base_1+"п"+base_2+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN078"
  } ;

mkN079 : Str -> N ;
mkN079 base =
  case base of {
    base_1+"е"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"е"+base_2 ;
                         Pl => base_1+"ј"+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"е"+base_2+"от" ;
                                   Pl => base_1+"ј"+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"е"+base_2+"ов" ;
                                Pl => base_1+"ј"+base_2+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"е"+base_2+"он" ;
                              Pl => base_1+"ј"+base_2+"ине"
                            }
            } ;
        count_form = base_1+"е"+base_2+"а" ;
        vocative = table {
                     Sg => base_1+"е"+base_2+"у" ;
                     Pl => base_1+"ј"+base_2+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN079"
  } ;

mkN080 : Str -> N ;
mkN080 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"и"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"а" ; --guessed
                               Pl => base_1+"ите"
                             } ;
          Def Proximal => table {
                            Sg => nonExist ;
                            Pl => base_1+"иве"
                          } ;
          Def Distal => table {
                          Sg => nonExist ;
                          Pl => base_1+"ине"
                        }
        } ;
    count_form = base_1+"и" ;
    vocative = table {
                 Sg => base_1 ; --guessed
                 Pl => base_1+"и"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN081 : Str -> N ;
mkN081 base =
  case base of {
    base_1+"о" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"о" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ото" ;
                                   Pl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ово" ;
                                Pl => base_1+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"оно" ;
                              Pl => base_1+"ине"
                            }
            } ;
        count_form = base_1+"и" ;
        vocative = table {
                     Sg => base_1+"о" ;
                     Pl => base_1+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Neuter
      };
    _ => error "Can't apply paradigm mkN081"
  } ;

mkN082 : Str -> N ;
mkN082 base =
  case base of {
    base_1+"а" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ата" ;
                                   Pl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ава" ;
                                Pl => base_1+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"ана" ;
                              Pl => base_1+"ине"
                            }
            } ;
        count_form = base_1+"и" ;
        vocative = table {
                     Sg => base_1+"а" ;
                     Pl => base_1+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN082"
  } ;

mkN083 : Str -> N ;
mkN083 base =
  case base of {
    base_1+"ка" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ка" ;
                         Pl => base_1+"ци"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ката" ;
                                   Pl => base_1+"ците"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"кава" ;
                                Pl => base_1+"циве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"кана" ;
                              Pl => base_1+"цине"
                            }
            } ;
        count_form = base_1+"ци" ;
        vocative = table {
                     Sg => base_1+"ко" ;
                     Pl => base_1+"ци"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN083"
  } ;

mkN084 : Str -> N ;
mkN084 base =
  case base of {
    base_1+"о" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"о" ;
                         Pl => base_1+"вци"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ото" ;
                                   Pl => base_1+"вците"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ово" ;
                                Pl => base_1+"вциве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"оно" ;
                              Pl => base_1+"вцине"
                            }
            } ;
        count_form = base_1+"вци" ;
        vocative = table {
                     Sg => base_1+"о" ;
                     Pl => base_1+"вци"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN084"
  } ;

mkN085 : Str -> N ;
mkN085 base =
  case base of {
    base_1+"ј" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ј" ;
                         Pl => base_1+"ишта"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"јот" ;
                                   Pl => base_1+"иштата"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"јов" ;
                                Pl => base_1+"иштава"
                              } ;
              Def Distal => table {
                              Sg => base_1+"јон" ;
                              Pl => base_1+"иштана"
                            }
            } ;
        count_form = base_1+"ја" ;
        vocative = table {
                     Sg => base_1+"ју" ;
                     Pl => base_1+"ишта"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN085"
  } ;

mkN086 : Str -> N ;
mkN086 base =
  case base of {
    base_1+"а" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"аот" ;
                                   Pl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"аов" ;
                                Pl => base_1+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"аон" ;
                              Pl => base_1+"ине"
                            }
            } ;
        count_form = base_1+"аа" ;
        vocative = table {
                     Sg => base_1+"ау" ;
                     Pl => base_1+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN086"
  } ;

mkN087 : Str -> N ;
mkN087 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"и"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"та" ;
                               Pl => base_1+"ите"
                             } ;
          Def Proximal => table {
                            Sg => nonExist ;
                            Pl => base_1+"иве"
                          } ;
          Def Distal => table {
                          Sg => nonExist ;
                          Pl => base_1+"ине"
                        }
        } ;
    count_form = base_1+"и" ;
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1+"и"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Fem
  } ;

mkN088 : Str -> N ;
mkN088 base =
  case base of {
    base_1+"а" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а" ;
                         Pl => nonExist
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ата" ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ава" ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => base_1+"ана" ;
                              Pl => nonExist
                            }
            } ;
        count_form = nonExist ;
        vocative = table {
                     Sg => base_1+"о" ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN088"
  } ;

mkN089 : Str -> N ;
mkN089 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"вци"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"то" ;
                               Pl => base_1+"вците"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"во" ;
                            Pl => base_1+"вциве"
                          } ;
          Def Distal => table {
                          Sg => base_1+"но" ;
                          Pl => base_1+"вцине"
                        }
        } ;
    count_form = base_1+"вци" ;
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1+"вци"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN090 : Str -> N ;
mkN090 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1 --guessed
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"а" ; --guessed
                               Pl => base_1+"те" --guessed
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ов" ; --guessed
                            Pl => base_1+"ве" --guessed
                          } ;
          Def Distal => table {
                          Sg => base_1+"он" ; --guessed
                          Pl => base_1+"не" --guessed
                        }
        } ;
    count_form = base_1 ; --guessed
    vocative = table {
                 Sg => base_1 ; --guessed
                 Pl => base_1 --guessed
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Fem
  } ;

mkN091 : Str -> N ;
mkN091 base =
  case base of {
    base_1+"к" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"к" ;
                         Pl => base_1+"ци"
                       } ;
              Def Unspecified => table {
                                   Sg => nonExist ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => nonExist ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            }
            } ;
        count_form = base_1+"ци" ;
        vocative = table {
                     Sg => nonExist ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN091"
  } ;

mkN092 : Str -> N ;
mkN092 base =
  case base of {
    base_1+"а"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а"+base_2 ;
                         Pl => base_1+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+base_2+"от" ;
                                   Pl => base_1+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+base_2+"ов" ;
                                Pl => base_1+base_2+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+base_2+"он" ;
                              Pl => base_1+base_2+"ине"
                            }
            } ;
        count_form = base_1+base_2+"а" ;
        vocative = table {
                     Sg => base_1+base_2+"у" ;
                     Pl => base_1+base_2+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN092"
  } ;

mkN093 : Str -> N ;
mkN093 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"еви"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"та" ; --guessed
                               Pl => nonExist
                             } ;
          Def Proximal => table {
                            Sg => nonExist ;
                            Pl => nonExist
                          } ;
          Def Distal => table {
                          Sg => nonExist ;
                          Pl => nonExist
                        }
        } ;
    count_form = base_1+"еви" ;
    vocative = table {
                 Sg => base_1 ; --guessed
                 Pl => nonExist
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN094 : Str -> N ;
mkN094 base =
  case base of {
    base_1+"ец" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ец" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ецот" ;
                                   Pl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ецов" ;
                                Pl => base_1+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"ецон" ;
                              Pl => base_1+"ине"
                            }
            } ;
        count_form = base_1+"еца" ;
        vocative = table {
                     Sg => base_1+"ецу" ;
                     Pl => base_1+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN094"
  } ;

mkN095 : Str -> N ;
mkN095 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"тта" ; --guessed
                               Pl => base_1+"те"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"во" ; --guessed
                            Pl => base_1+"ве"
                          } ;
          Def Distal => table {
                          Sg => base_1+"но" ; --guessed
                          Pl => base_1+"не"
                        }
        } ;
    count_form = base_1 ;
    vocative = table {
                 Sg => base_1 ; --guessed
                 Pl => base_1
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Fem
  } ;

mkN096 : Str -> N ;
mkN096 base =
  case base of {
    base_1+"с"+base_2@?+base_3@?+"о" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"с"+base_2+base_3+"о" ;
                         Pl => base_1+base_2+"с"+base_3+"а"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"с"+base_2+base_3+"ото" ;
                                   Pl => base_1+base_2+"с"+base_3+"ата"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"с"+base_2+base_3+"ово" ;
                                Pl => base_1+base_2+"с"+base_3+"ава"
                              } ;
              Def Distal => table {
                              Sg => base_1+"с"+base_2+base_3+"оно" ;
                              Pl => base_1+base_2+"с"+base_3+"ана"
                            }
            } ;
        count_form = base_1+base_2+"с"+base_3+"а" ;
        vocative = table {
                     Sg => base_1+"с"+base_2+base_3+"о" ;
                     Pl => base_1+base_2+"с"+base_3+"а"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Neuter
      };
    _ => error "Can't apply paradigm mkN096"
  } ;

mkN097 : Str -> N ;
mkN097 base =
  case base of {
    base_1+"во" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"во" ;
                         Pl => base_1+"ши"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"вото" ;
                                   Pl => base_1+"шите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"вово" ;
                                Pl => base_1+"шиве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"воно" ;
                              Pl => base_1+"шине"
                            }
            } ;
        count_form = base_1+"ши" ;
        vocative = table {
                     Sg => base_1+"во" ;
                     Pl => base_1+"ши"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Neuter
      };
    _ => error "Can't apply paradigm mkN097"
  } ;

mkN098 : Str -> N ;
mkN098 base =
  case base of {
    "чов"+base_1+"к" => lin N
      { s = table {
              Indef => table {
                         Sg => "чов"+base_1+"к" ;
                         Pl => "луѓ"+base_1
                       } ;
              Def Unspecified => table {
                                   Sg => "чов"+base_1+"кот" ;
                                   Pl => "луѓ"+base_1+"то"
                                 } ;
              Def Proximal => table {
                                Sg => "чов"+base_1+"ков" ;
                                Pl => "луѓ"+base_1+"во"
                              } ;
              Def Distal => table {
                              Sg => "чов"+base_1+"кон" ;
                              Pl => "луѓ"+base_1+"но"
                            }
            } ;
        count_form = "луѓ"+base_1 ;
        vocative = table {
                     Sg => "чов"+base_1+"ку" ;
                     Pl => "луѓ"+base_1
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN098"
  } ;

mkN099 : Str -> N ;
mkN099 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"и"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"а" ;
                               Pl => base_1+"ите"
                             } ;
          Def Proximal => table {
                            Sg => nonExist ;
                            Pl => base_1+"иве"
                          } ;
          Def Distal => table {
                          Sg => nonExist ;
                          Pl => base_1+"ине"
                        }
        } ;
    count_form = base_1+"и" ;
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1+"и"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN100 : Str -> N ;
mkN100 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"и"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"а" ;
                               Pl => nonExist
                             } ;
          Def Proximal => table {
                            Sg => nonExist ;
                            Pl => nonExist
                          } ;
          Def Distal => table {
                          Sg => nonExist ;
                          Pl => nonExist
                        }
        } ;
    count_form = base_1+"и" ;
    vocative = table {
                 Sg => base_1 ;
                 Pl => nonExist
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Fem
  } ;

mkN101 : Str -> N ;
mkN101 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"та" ; --guessed
                               Pl => base_1+"та"
                             } ;
          Def Proximal => table {
                            Sg => nonExist ;
                            Pl => base_1+"ва"
                          } ;
          Def Distal => table {
                          Sg => nonExist ;
                          Pl => base_1+"на"
                        }
        } ;
    count_form = base_1 ;
    vocative = table {
                 Sg => base_1 ; --guessed
                 Pl => base_1
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN102 : Str -> N ;
mkN102 base =
  case base of {
    base_1+"а"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а"+base_2 ;
                         Pl => base_1+base_2+"а"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"а"+base_2+"от" ;
                                   Pl => base_1+base_2+"ата"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"а"+base_2+"ов" ;
                                Pl => base_1+base_2+"ава"
                              } ;
              Def Distal => table {
                              Sg => base_1+"а"+base_2+"он" ;
                              Pl => base_1+base_2+"ана"
                            }
            } ;
        count_form = base_1+base_2+"а" ;
        vocative = table {
                     Sg => base_1+base_2+"у" ;
                     Pl => base_1+base_2+"а"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN102"
  } ;

mkN103 : Str -> N ;
mkN103 base =
  case base of {
    base_1+"е" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"е" ;
                         Pl => base_1+"иња"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ето" ;
                                   Pl => base_1+"ињата"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ево" ;
                                Pl => base_1+"ињава"
                              } ;
              Def Distal => table {
                              Sg => base_1+"ено" ;
                              Pl => base_1+"ињана"
                            }
            } ;
        count_form = base_1+"иња" ;
        vocative = table {
                     Sg => base_1+"е" ;
                     Pl => base_1+"иња"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN103"
  } ;

mkN104 : Str -> N ;
mkN104 base =
  case base of {
    base_1+"љ" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"љ" ;
                         Pl => base_1+"ли"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"љот" ;
                                   Pl => base_1+"лите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"љов" ;
                                Pl => base_1+"ливе"
                              } ;
              Def Distal => table {
                              Sg => base_1+"љон" ;
                              Pl => base_1+"лине"
                            }
            } ;
        count_form = base_1+"ља" ;
        vocative = table {
                     Sg => base_1+"љу" ;
                     Pl => base_1+"ли"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN104"
  } ;

mkN105 : Str -> N ;
mkN105 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"ишта"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"от" ;
                               Pl => base_1+"иштата"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ов" ;
                            Pl => base_1+"иштава"
                          } ;
          Def Distal => table {
                          Sg => base_1+"он" ;
                          Pl => base_1+"иштана"
                        }
        } ;
    count_form = base_1+"а" ;
    vocative = table {
                 Sg => base_1+"у" ;
                 Pl => base_1+"ишта"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN106 : Str -> N ;
mkN106 base =
  case base of {
    base_1+"о" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"о" ;
                         Pl => base_1+"а"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ото" ;
                                   Pl => base_1+"ата"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ово" ;
                                Pl => base_1+"ава"
                              } ;
              Def Distal => table {
                              Sg => base_1+"оно" ;
                              Pl => base_1+"ана"
                            }
            } ;
        count_form = base_1+"а" ;
        vocative = table {
                     Sg => base_1+"о" ;
                     Pl => base_1+"а"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN106"
  } ;

mkN107 : Str -> N ;
mkN107 base =
  case base of {
    base_1+"а" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ата" ;
                                   Pl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ава" ;
                                Pl => base_1+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"ана" ;
                              Pl => base_1+"ине"
                            }
            } ;
        count_form = base_1+"и" ;
        vocative = table {
                     Sg => base_1+"о" ;
                     Pl => base_1+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Neuter
      };
    _ => error "Can't apply paradigm mkN107"
  } ;

mkN108 : Str -> N ;
mkN108 base =
  case base of {
    base_1+"г"+base_2@(?+?+?+?+?+?)+"к" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"г"+base_2+"к" ;
                         Pl => base_1+"д"+base_2+"ци"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"г"+base_2+"кот" ;
                                   Pl => base_1+"д"+base_2+"ците"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"г"+base_2+"ков" ;
                                Pl => base_1+"д"+base_2+"циве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"г"+base_2+"кон" ;
                              Pl => base_1+"д"+base_2+"цине"
                            }
            } ;
        count_form = base_1+"г"+base_2+"ка" ;
        vocative = table {
                     Sg => base_1+"г"+base_2+"ку" ;
                     Pl => base_1+"д"+base_2+"ци"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN108"
  } ;

mkN109 : Str -> N ;
mkN109 base =
  case base of {
    base_1+"т" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"т" ;
                         Pl => base_1+"ти"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"тта" ;
                                   Pl => base_1+"тите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ва" ;
                                Pl => base_1+"тиве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"на" ;
                              Pl => base_1+"тине"
                            }
            } ;
        count_form = base_1+"ти" ;
        vocative = table {
                     Sg => base_1+"т" ;
                     Pl => base_1+"ти"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN109"
  } ;

mkN110 : Str -> N ;
mkN110 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"и" --guessed
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"тта" ;
                               Pl => base_1+"ите" --guessed
                             } ;
          Def Proximal => table {
                            Sg => nonExist ;
                            Pl => base_1+"иве" --guessed
                          } ;
          Def Distal => table {
                          Sg => nonExist ;
                          Pl => base_1+"ине" --guessed
                        }
        } ;
    count_form = base_1+"и" ; --guessed
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1+"и" --guessed
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Fem
  } ;

mkN111 : Str -> N ;
mkN111 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"а"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"та" ; --guessed
                               Pl => nonExist
                             } ;
          Def Proximal => table {
                            Sg => nonExist ;
                            Pl => nonExist
                          } ;
          Def Distal => table {
                          Sg => nonExist ;
                          Pl => nonExist
                        }
        } ;
    count_form = base_1+"а" ;
    vocative = table {
                 Sg => base_1 ; --guessed
                 Pl => nonExist
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN112 : Str -> N ;
mkN112 base =
  case base of {
    base_1+"е" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"е" ;
                         Pl => base_1+"иња"
                       } ;
              Def Unspecified => table {
                                   Sg => nonExist ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => nonExist ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            }
            } ;
        count_form = base_1+"иња" ;
        vocative = table {
                     Sg => nonExist ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Neuter
      };
    _ => error "Can't apply paradigm mkN112"
  } ;

mkN113 : Str -> N ;
mkN113 base =
  case base of {
    base_1+"ок" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ок" ;
                         Pl => base_1+"ци"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"окот" ;
                                   Pl => base_1+"ците"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"оков" ;
                                Pl => base_1+"циве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"окон" ;
                              Pl => base_1+"цине"
                            }
            } ;
        count_form = base_1+"ока" ;
        vocative = table {
                     Sg => base_1+"оку" ;
                     Pl => base_1+"ци"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN113"
  } ;

mkN114 : Str -> N ;
mkN114 base =
  case base of {
    base_1+"тенце" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"тенце" ;
                         Pl => base_1+"чиња"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"тенцето" ;
                                   Pl => base_1+"чињата"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"тенцево" ;
                                Pl => base_1+"чињава"
                              } ;
              Def Distal => table {
                              Sg => base_1+"тенцено" ;
                              Pl => base_1+"чињана"
                            }
            } ;
        count_form = base_1+"чиња" ;
        vocative = table {
                     Sg => base_1+"тенце" ;
                     Pl => base_1+"чиња"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Neuter
      };
    _ => error "Can't apply paradigm mkN114"
  } ;

mkN115 : Str -> N ;
mkN115 base =
  case base of {
    base_1+"л" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"л" ;
                         Pl => base_1+"ови"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"лот" ;
                                   Pl => base_1+"овите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"лов" ;
                                Pl => base_1+"овиве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"лон" ;
                              Pl => base_1+"овине"
                            }
            } ;
        count_form = base_1+"ла" ;
        vocative = table {
                     Sg => base_1+"лу" ;
                     Pl => base_1+"ови"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN115"
  } ;

mkN116 : Str -> N ;
mkN116 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"и"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"от" ;
                               Pl => base_1+"ите"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ов" ;
                            Pl => base_1+"иве"
                          } ;
          Def Distal => table {
                          Sg => base_1+"он" ;
                          Pl => base_1+"ине"
                        }
        } ;
    count_form = base_1+"а" ;
    vocative = table {
                 Sg => base_1+"у" ;
                 Pl => base_1+"и"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Fem
  } ;

mkN117 : Str -> N ;
mkN117 base =
  case base of {
    base_1+"ја" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ја" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"јаот" ;
                                   Pl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"јаов" ;
                                Pl => base_1+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"јаон" ;
                              Pl => base_1+"ине"
                            }
            } ;
        count_form = base_1+"јаа" ;
        vocative = table {
                     Sg => base_1+"јау" ;
                     Pl => base_1+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN117"
  } ;

mkN118 : Str -> N ;
mkN118 base =
  case base of {
    base_1+"о"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"о"+base_2 ;
                         Pl => base_1+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"о"+base_2+"от" ;
                                   Pl => base_1+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"о"+base_2+"ов" ;
                                Pl => base_1+base_2+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"о"+base_2+"он" ;
                              Pl => base_1+base_2+"ине"
                            }
            } ;
        count_form = base_1+"о"+base_2+"а" ;
        vocative = table {
                     Sg => base_1+"о"+base_2+"у" ;
                     Pl => base_1+base_2+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN118"
  } ;

mkN119 : Str -> N ;
mkN119 base =
  case base of {
    base_1+"ц"+base_2@?+"ф"+base_3@(?+?)+"а"+base_4@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ц"+base_2+"ф"+base_3+"а"+base_4 ;
                         Pl => nonExist
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ф"+base_2+"ц"+base_3+base_4+"от" ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ф"+base_2+"ц"+base_3+base_4+"ов" ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => base_1+"ф"+base_2+"ц"+base_3+base_4+"он" ;
                              Pl => nonExist
                            }
            } ;
        count_form = nonExist ;
        vocative = table {
                     Sg => base_1+"ф"+base_2+"ц"+base_3+base_4+"у" ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN119"
  } ;

mkN120 : Str -> N ;
mkN120 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"от" ;
                               Pl => base_1+"те"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ов" ;
                            Pl => base_1+"ве"
                          } ;
          Def Distal => table {
                          Sg => base_1+"он" ;
                          Pl => base_1+"не"
                        }
        } ;
    count_form = base_1 ;
    vocative = table {
                 Sg => nonExist ;
                 Pl => nonExist
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN121 : Str -> N ;
mkN121 base =
  case base of {
    "плеон"+base_1+"а"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => "плеон"+base_1+"а"+base_2 ;
                         Pl => "сарк"+base_1+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => "сарк"+base_1+base_2+"от" ;
                                   Pl => "сарк"+base_1+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => "сарк"+base_1+base_2+"ов" ;
                                Pl => "сарк"+base_1+base_2+"иве"
                              } ;
              Def Distal => table {
                              Sg => "сарк"+base_1+base_2+"он" ;
                              Pl => "сарк"+base_1+base_2+"ине"
                            }
            } ;
        count_form = "сарк"+base_1+base_2+"а" ;
        vocative = table {
                     Sg => "сарк"+base_1+base_2+"у" ;
                     Pl => "сарк"+base_1+base_2+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN121"
  } ;

mkN122 : Str -> N ;
mkN122 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"а" ; --guessed
                               Pl => base_1+"та"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"во" ; --guessed
                            Pl => base_1+"ва"
                          } ;
          Def Distal => table {
                          Sg => base_1+"но" ; --guessed
                          Pl => base_1+"на"
                        }
        } ;
    count_form = base_1 ;
    vocative = table {
                 Sg => base_1 ; --guessed
                 Pl => base_1
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Neuter
  } ;

mkN123 : Str -> N ;
mkN123 base =
  case base of {
    base_1+"е" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"е" ;
                         Pl => base_1+"а"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ето" ;
                                   Pl => base_1+"ата"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ево" ;
                                Pl => base_1+"ава"
                              } ;
              Def Distal => table {
                              Sg => base_1+"ено" ;
                              Pl => base_1+"ана"
                            }
            } ;
        count_form = base_1+"а" ;
        vocative = table {
                     Sg => base_1+"е" ;
                     Pl => base_1+"а"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN123"
  } ;

mkN124 : Str -> N ;
mkN124 base =
  case base of {
    base_1+"а" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ата" ;
                                   Pl => base_1+"ите" --guessed
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ава" ;
                                Pl => base_1+"иве" --guessed
                              } ;
              Def Distal => table {
                              Sg => base_1+"ана" ;
                              Pl => base_1+"ине" --guessed
                            }
            } ;
        count_form = base_1+"и" ;
        vocative = table {
                     Sg => base_1+"е" ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN124"
  } ;

mkN125 : Str -> N ;
mkN125 base =
  case base of {
    base_1+"о" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"о" ;
                         Pl => base_1+"а"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ото" ;
                                   Pl => base_1+"ата"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ово" ;
                                Pl => base_1+"ава"
                              } ;
              Def Distal => table {
                              Sg => base_1+"оно" ;
                              Pl => base_1+"ана"
                            }
            } ;
        count_form = base_1+"а" ;
        vocative = table {
                     Sg => base_1+"о" ;
                     Pl => base_1+"а"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN125"
  } ;

mkN126 : Str -> N ;
mkN126 base =
  case base of {
    base_1+"ја" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ја" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => nonExist ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => nonExist ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            }
            } ;
        count_form = base_1+"и" ;
        vocative = table {
                     Sg => nonExist ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN126"
  } ;

mkN127 : Str -> N ;
mkN127 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"ња"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"то" ;
                               Pl => base_1+"њата"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"во" ;
                            Pl => base_1+"њава"
                          } ;
          Def Distal => table {
                          Sg => base_1+"но" ;
                          Pl => base_1+"њана"
                        }
        } ;
    count_form = base_1+"ња" ;
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1+"ња"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Neuter
  } ;

mkN128 : Str -> N ;
mkN128 base =
  case base of {
    base_1+"к" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"к" ;
                         Pl => base_1+"ци"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"кот" ;
                                   Pl => base_1+"ците"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ков" ;
                                Pl => base_1+"циве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"кон" ;
                              Pl => base_1+"цине"
                            }
            } ;
        count_form = base_1+"ка" ;
        vocative = table {
                     Sg => base_1+"ку" ;
                     Pl => base_1+"ци"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Neuter
      };
    _ => error "Can't apply paradigm mkN128"
  } ;

mkN129 : Str -> N ;
mkN129 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"иња"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"от" ;
                               Pl => base_1+"ињата"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ов" ;
                            Pl => base_1+"ињава"
                          } ;
          Def Distal => table {
                          Sg => base_1+"он" ;
                          Pl => base_1+"ињана"
                        }
        } ;
    count_form = base_1+"а" ;
    vocative = table {
                 Sg => base_1+"у" ;
                 Pl => base_1+"иња"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN130 : Str -> N ;
mkN130 base =
  case base of {
    base_1+"в" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"в" ;
                         Pl => base_1+"си"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"виот" ;
                                   Pl => base_1+"сите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"виов" ;
                                Pl => base_1+"сиве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"вион" ;
                              Pl => base_1+"сине"
                            }
            } ;
        count_form = base_1+"си" ;
        vocative = table {
                     Sg => nonExist ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN130"
  } ;

mkN131 : Str -> N ;
mkN131 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"и"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"от" ;
                               Pl => base_1+"ите"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ов" ;
                            Pl => base_1+"иве"
                          } ;
          Def Distal => table {
                          Sg => base_1+"он" ;
                          Pl => base_1+"ине"
                        }
        } ;
    count_form = base_1+"а" ;
    vocative = table {
                 Sg => base_1+"е" ;
                 Pl => base_1+"и"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Fem
  } ;

mkN132 : Str -> N ;
mkN132 base =
  case base of {
    base_1+"о" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"о" ;
                         Pl => base_1+"вци"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ото" ;
                                   Pl => base_1+"вците"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ово" ;
                                Pl => base_1+"вциве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"оно" ;
                              Pl => base_1+"вцине"
                            }
            } ;
        count_form = base_1+"вци" ;
        vocative = table {
                     Sg => base_1+"о" ;
                     Pl => base_1+"вци"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Neuter
      };
    _ => error "Can't apply paradigm mkN132"
  } ;

mkN133 : Str -> N ;
mkN133 base =
  case base of {
    base_1+"’"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"’"+base_2 ;
                         Pl => base_1+"‘"+base_2+"ови"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"’"+base_2+"от" ;
                                   Pl => base_1+"‘"+base_2+"овите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"’"+base_2+"ов" ;
                                Pl => base_1+"‘"+base_2+"овиве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"’"+base_2+"он" ;
                              Pl => base_1+"‘"+base_2+"овине"
                            }
            } ;
        count_form = base_1+"’"+base_2+"а" ;
        vocative = table {
                     Sg => base_1+"’"+base_2+"у" ;
                     Pl => base_1+"‘"+base_2+"ови"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN133"
  } ;

mkN134 : Str -> N ;
mkN134 base =
  case base of {
    base_1+"е"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"е"+base_2 ;
                         Pl => base_1+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"е"+base_2+"от" ;
                                   Pl => base_1+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"е"+base_2+"ов" ;
                                Pl => base_1+base_2+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"е"+base_2+"он" ;
                              Pl => base_1+base_2+"ине"
                            }
            } ;
        count_form = base_1+"е"+base_2+"а" ;
        vocative = table {
                     Sg => base_1+"е"+base_2+"у" ;
                     Pl => base_1+base_2+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN134"
  } ;

mkN135 : Str -> N ;
mkN135 base =
  case base of {
    base_1+"е"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"е"+base_2 ;
                         Pl => base_1+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+base_2+"иот" ;
                                   Pl => base_1+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+base_2+"иов" ;
                                Pl => base_1+base_2+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+base_2+"ион" ;
                              Pl => base_1+base_2+"ине"
                            }
            } ;
        count_form = base_1+base_2+"и" ;
        vocative = table {
                     Sg => nonExist ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN135"
  } ;

mkN136 : Str -> N ;
mkN136 base =
  case base of {
    base_1+base_2@(?+?+?+?)+"е"+base_3@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+base_2+"е"+base_3 ;
                         Pl => base_1+"д"+base_2+base_3+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+base_2+"е"+base_3+"от" ;
                                   Pl => base_1+"д"+base_2+base_3+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+base_2+"е"+base_3+"ов" ;
                                Pl => base_1+"д"+base_2+base_3+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+base_2+"е"+base_3+"он" ;
                              Pl => base_1+"д"+base_2+base_3+"ине"
                            }
            } ;
        count_form = base_1+base_2+"е"+base_3+"а" ;
        vocative = table {
                     Sg => base_1+base_2+"е"+base_3+"у" ;
                     Pl => base_1+"д"+base_2+base_3+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN136"
  } ;

mkN137 : Str -> N ;
mkN137 base =
  case base of {
    base_1+"в" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"в" ;
                         Pl => nonExist
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"вта" ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"вва" ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => base_1+"вна" ;
                              Pl => nonExist
                            }
            } ;
        count_form = nonExist ;
        vocative = table {
                     Sg => base_1+"о" ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN137"
  } ;

mkN138 : Str -> N ;
mkN138 base =
  case base of {
    base_1+"ј" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ј" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"јот" ;
                                   Pl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"јов" ;
                                Pl => base_1+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"јон" ;
                              Pl => base_1+"ине"
                            }
            } ;
        count_form = base_1+"ја" ;
        vocative = table {
                     Sg => base_1+"ју" ;
                     Pl => base_1+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN138"
  } ;

mkN139 : Str -> N ;
mkN139 base =
  case base of {
    base_1+"о"+base_2@(?+?+?+?+?+?+?+?+?+?+?+?+?)+"о" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"о"+base_2+"о" ;
                         Pl => base_1+"а"+base_2+"а"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"о"+base_2+"ото" ;
                                   Pl => base_1+"а"+base_2+"ата"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"о"+base_2+"ово" ;
                                Pl => base_1+"а"+base_2+"ава"
                              } ;
              Def Distal => table {
                              Sg => base_1+"о"+base_2+"оно" ;
                              Pl => base_1+"а"+base_2+"ана"
                            }
            } ;
        count_form = base_1+"а"+base_2+"а" ;
        vocative = table {
                     Sg => base_1+"о"+base_2+"о" ;
                     Pl => base_1+"а"+base_2+"а"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Neuter
      };
    _ => error "Can't apply paradigm mkN139"
  } ;

mkN140 : Str -> N ;
mkN140 base =
  case base of {
    base_1+"же"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"же"+base_2 ;
                         Pl => base_1+"ш"+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"же"+base_2+"от" ;
                                   Pl => base_1+"ш"+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"же"+base_2+"ов" ;
                                Pl => base_1+"ш"+base_2+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"же"+base_2+"он" ;
                              Pl => base_1+"ш"+base_2+"ине"
                            }
            } ;
        count_form = base_1+"же"+base_2+"а" ;
        vocative = table {
                     Sg => base_1+"же"+base_2+"у" ;
                     Pl => base_1+"ш"+base_2+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN140"
  } ;

mkN141 : Str -> N ;
mkN141 base =
  case base of {
    base_1+"со"+base_2@(?+?+?+?+?) => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"со"+base_2 ;
                         Pl => nonExist
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+base_2+"а" ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => nonExist ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            }
            } ;
        count_form = nonExist ;
        vocative = table {
                     Sg => base_1+"со"+base_2 ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN141"
  } ;

mkN142 : Str -> N ;
mkN142 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"ови" --guessed
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"а" ;
                               Pl => nonExist
                             } ;
          Def Proximal => table {
                            Sg => nonExist ;
                            Pl => nonExist
                          } ;
          Def Distal => table {
                          Sg => nonExist ;
                          Pl => nonExist
                        }
        } ;
    count_form = base_1+"ови" ; --guessed
    vocative = table {
                 Sg => base_1 ;
                 Pl => nonExist
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Neuter
  } ;

mkN143 : Str -> N ;
mkN143 base =
  case base of {
    base_1+"з"+base_2@(?+?+?+?+?+?) => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"з"+base_2 ;
                         Pl => nonExist
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+base_2+"а" ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => nonExist ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            }
            } ;
        count_form = nonExist ;
        vocative = table {
                     Sg => base_1+"з"+base_2 ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN143"
  } ;

mkN144 : Str -> N ;
mkN144 base =
  case base of {
    base_1+"ос"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ос"+base_2 ;
                         Pl => nonExist
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+base_2+"а" ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => nonExist ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            }
            } ;
        count_form = nonExist ;
        vocative = table {
                     Sg => base_1+"ос"+base_2 ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN144"
  } ;

mkN145 : Str -> N ;
mkN145 base =
  case base of {
    base_1+"с"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"с"+base_2 ;
                         Pl => nonExist
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+base_2+"а" ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => nonExist ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            }
            } ;
        count_form = nonExist ;
        vocative = table {
                     Sg => base_1+"с"+base_2 ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN145"
  } ;

mkN146 : Str -> N ;
mkN146 base =
  case base of {
    base_1+base_2@?+"н"+base_3@(?+?+?+?+?+?+?+?+?+?) => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+base_2+"н"+base_3 ;
                         Pl => nonExist
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"н"+base_2+base_3+"а" ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => nonExist ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            }
            } ;
        count_form = nonExist ;
        vocative = table {
                     Sg => base_1+base_2+"н"+base_3 ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN146"
  } ;

mkN147 : Str -> N ;
mkN147 base =
  case base of {
    base_1+"т" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"т" ;
                         Pl => nonExist
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"тта" ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"тва" ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => base_1+"тна" ;
                              Pl => nonExist
                            }
            } ;
        count_form = nonExist ;
        vocative = table {
                     Sg => base_1+"о" ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN147"
  } ;

mkN148 : Str -> N ;
mkN148 base =
  case base of {
    base_1+"ец" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ец" ;
                         Pl => base_1+"ци"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ецот" ;
                                   Pl => base_1+"ците"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ецов" ;
                                Pl => base_1+"циве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"ецон" ;
                              Pl => base_1+"цине"
                            }
            } ;
        count_form = base_1+"еца" ;
        vocative = table {
                     Sg => base_1+"че" ;
                     Pl => base_1+"ци"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN148"
  } ;

mkN149 : Str -> N ;
mkN149 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"и"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"от" ;
                               Pl => base_1+"ите"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ов" ;
                            Pl => base_1+"иве"
                          } ;
          Def Distal => table {
                          Sg => base_1+"он" ;
                          Pl => base_1+"ине"
                        }
        } ;
    count_form = base_1+"а" ;
    vocative = table {
                 Sg => base_1+"у" ;
                 Pl => base_1+"и"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Neuter
  } ;

mkN150 : Str -> N ;
mkN150 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"вци"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"-то" ; --guessed
                               Pl => base_1+"вците" --guessed
                             } ;
          Def Proximal => table {
                            Sg => base_1+"-во" ; --guessed
                            Pl => base_1+"вциве" --guessed
                          } ;
          Def Distal => table {
                          Sg => base_1+"-но" ; --guessed
                          Pl => base_1+"вцине" --guessed
                        }
        } ;
    count_form = base_1+"вци" ;
    vocative = table {
                 Sg => base_1 ; --guessed
                 Pl => base_1+"вци" --guessed
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN151 : Str -> N ;
mkN151 base =
  case base of {
    base_1+"а" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ата" ; --guessed
                                   Pl => base_1+"ите" --guessed
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ава" ; --guessed
                                Pl => base_1+"иве" --guessed
                              } ;
              Def Distal => table {
                              Sg => base_1+"ана" ; --guessed
                              Pl => base_1+"ине" --guessed
                            }
            } ;
        count_form = base_1+"и" ;
        vocative = table {
                     Sg => base_1+"е" ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN151"
  } ;

mkN152 : Str -> N ;
mkN152 base =
  case base of {
    base_1+"о" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"о" ;
                         Pl => base_1+"а"
                       } ;
              Def Unspecified => table {
                                   Sg => nonExist ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => nonExist ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            }
            } ;
        count_form = base_1+"а" ;
        vocative = table {
                     Sg => nonExist ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Neuter
      };
    _ => error "Can't apply paradigm mkN152"
  } ;

mkN153 : Str -> N ;
mkN153 base =
  case base of {
    base_1+"о"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"о"+base_2 ;
                         Pl => base_1+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => nonExist ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => nonExist ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            }
            } ;
        count_form = base_1+base_2+"и" ;
        vocative = table {
                     Sg => nonExist ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN153"
  } ;

mkN154 : Str -> N ;
mkN154 base =
  case base of {
    base_1+"а" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ата" ;
                                   Pl => base_1+"ите" --guessed
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ава" ;
                                Pl => base_1+"иве" --guessed
                              } ;
              Def Distal => table {
                              Sg => base_1+"ана" ;
                              Pl => base_1+"ине" --guessed
                            }
            } ;
        count_form = base_1+"и" ;
        vocative = table {
                     Sg => base_1+"о" ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN154"
  } ;

mkN155 : Str -> N ;
mkN155 base =
  case base of {
    base_1+"н"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"н"+base_2 ;
                         Pl => base_1+"њ"+base_2
                       } ;
              Def Unspecified => table {
                                   Sg => nonExist ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => nonExist ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            }
            } ;
        count_form = base_1+"њ"+base_2 ;
        vocative = table {
                     Sg => nonExist ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN155"
  } ;

mkN156 : Str -> N ;
mkN156 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"и"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"тта" ; --guessed
                               Pl => base_1+"ите" --guessed
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ва" ; --guessed
                            Pl => base_1+"иве" --guessed
                          } ;
          Def Distal => table {
                          Sg => base_1+"на" ; --guessed
                          Pl => base_1+"ине" --guessed
                        }
        } ;
    count_form = base_1+"и" ;
    vocative = table {
                 Sg => base_1 ; --guessed
                 Pl => base_1+"и" --guessed
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Fem
  } ;

mkN157 : Str -> N ;
mkN157 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => nonExist
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"та" ;
                               Pl => nonExist
                             } ;
          Def Proximal => table {
                            Sg => nonExist ;
                            Pl => nonExist
                          } ;
          Def Distal => table {
                          Sg => nonExist ;
                          Pl => nonExist
                        }
        } ;
    count_form = nonExist ;
    vocative = table {
                 Sg => base_1 ;
                 Pl => nonExist
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN158 : Str -> N ;
mkN158 base =
  case base of {
    base_1+"ј" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ј" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => nonExist ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => nonExist ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            }
            } ;
        count_form = base_1+"и" ;
        vocative = table {
                     Sg => nonExist ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN158"
  } ;

mkN159 : Str -> N ;
mkN159 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"ови"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"то" ; --guessed
                               Pl => nonExist
                             } ;
          Def Proximal => table {
                            Sg => base_1+"во" ; --guessed
                            Pl => nonExist
                          } ;
          Def Distal => table {
                          Sg => base_1+"но" ; --guessed
                          Pl => nonExist
                        }
        } ;
    count_form = base_1+"ови" ;
    vocative = table {
                 Sg => base_1 ; --guessed
                 Pl => nonExist
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Neuter
  } ;

mkN160 : Str -> N ;
mkN160 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"и"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"тта" ;
                               Pl => base_1+"ите"
                             } ;
          Def Proximal => table {
                            Sg => nonExist ;
                            Pl => base_1+"иве"
                          } ;
          Def Distal => table {
                          Sg => nonExist ;
                          Pl => base_1+"ине"
                        }
        } ;
    count_form = base_1+"и" ;
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1+"и"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Fem
  } ;

mkN161 : Str -> N ;
mkN161 base =
  case base of {
    base_1+"к"+base_2@?+"т" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"к"+base_2+"т" ;
                         Pl => base_1+base_2+"ци"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"к"+base_2+"тот" ;
                                   Pl => base_1+base_2+"ците"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"к"+base_2+"тов" ;
                                Pl => base_1+base_2+"циве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"к"+base_2+"тон" ;
                              Pl => base_1+base_2+"цине"
                            }
            } ;
        count_form = base_1+"к"+base_2+"та" ;
        vocative = table {
                     Sg => base_1+"к"+base_2+"ту" ;
                     Pl => base_1+base_2+"ци"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN161"
  } ;

mkN162 : Str -> N ;
mkN162 base =
  case base of {
    base_1+"е"+base_2@(?+?+?+?) => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"е"+base_2 ;
                         Pl => nonExist
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+base_2+"а" ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => nonExist ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            }
            } ;
        count_form = nonExist ;
        vocative = table {
                     Sg => base_1+"е"+base_2 ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN162"
  } ;

mkN163 : Str -> N ;
mkN163 base =
  case base of {
    base_1+"а"+base_2@(?+?+?+?)+"к" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а"+base_2+"к" ;
                         Pl => base_1+"a"+base_2+"ци"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"а"+base_2+"кот" ;
                                   Pl => base_1+"a"+base_2+"ците"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"а"+base_2+"ков" ;
                                Pl => base_1+"a"+base_2+"циве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"а"+base_2+"кон" ;
                              Pl => base_1+"a"+base_2+"цине"
                            }
            } ;
        count_form = base_1+"а"+base_2+"ка" ;
        vocative = table {
                     Sg => base_1+"а"+base_2+"ку" ;
                     Pl => base_1+"a"+base_2+"ци"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN163"
  } ;

mkN164 : Str -> N ;
mkN164 base =
  case base of {
    base_1+base_2@(?+?+?+?) => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+base_2 ;
                         Pl => nonExist
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"с"+base_2+"а" ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => nonExist ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            }
            } ;
        count_form = nonExist ;
        vocative = table {
                     Sg => base_1+base_2 ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN164"
  } ;

mkN165 : Str -> N ;
mkN165 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"то" ; --guessed
                               Pl => base_1+"та"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"во" ; --guessed
                            Pl => base_1+"ва"
                          } ;
          Def Distal => table {
                          Sg => base_1+"но" ; --guessed
                          Pl => base_1+"на"
                        }
        } ;
    count_form = base_1 ;
    vocative = table {
                 Sg => base_1 ; --guessed
                 Pl => base_1
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Fem
  } ;

mkN166 : Str -> N ;
mkN166 base =
  case base of {
    base_1+"з"+base_2@?+base_3@(?+?+?+?) => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"з"+base_2+base_3 ;
                         Pl => base_1+"з"+base_2+base_3+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"з"+base_2+base_3+"от" ;
                                   Pl => base_1+"з"+base_2+base_3+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"з"+base_2+base_3+"ов" ;
                                Pl => base_1+"з"+base_2+base_3+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"з"+base_2+base_3+"он" ;
                              Pl => base_1+"з"+base_2+base_3+"ине"
                            }
            } ;
        count_form = base_1+"з"+base_2+base_3+"а" ;
        vocative = table {
                     Sg => base_1+base_2+"з"+base_3+"е" ;
                     Pl => base_1+"з"+base_2+base_3+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN166"
  } ;

mkN167 : Str -> N ;
mkN167 base =
  case base of {
    base_1+"ед"+base_2@(?+?+?+?+?) => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ед"+base_2 ;
                         Pl => nonExist
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"абот"+base_2+"а" ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => nonExist ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            }
            } ;
        count_form = nonExist ;
        vocative = table {
                     Sg => base_1+"ед"+base_2 ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN167"
  } ;

mkN168 : Str -> N ;
mkN168 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"и"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"јата" ;
                               Pl => base_1+"ите"
                             } ;
          Def Proximal => table {
                            Sg => nonExist ;
                            Pl => base_1+"иве"
                          } ;
          Def Distal => table {
                          Sg => nonExist ;
                          Pl => base_1+"ине"
                        }
        } ;
    count_form = base_1+"и" ;
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1+"и"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Fem
  } ;

mkN169 : Str -> N ;
mkN169 base =
  case base of {
    base_1+"ка" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ка" ;
                         Pl => base_1+"чен"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ката" ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"кава" ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => base_1+"кана" ;
                              Pl => nonExist
                            }
            } ;
        count_form = base_1+"чен" ;
        vocative = table {
                     Sg => base_1+"ко" ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN169"
  } ;

mkN170 : Str -> N ;
mkN170 base =
  case base of {
    base_1+"до"+base_2@?+base_3@(?+?+?+?) => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"до"+base_2+base_3 ;
                         Pl => nonExist
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+base_2+"од"+base_3+"а" ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => nonExist ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            }
            } ;
        count_form = nonExist ;
        vocative = table {
                     Sg => base_1+"до"+base_2+base_3 ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN170"
  } ;

mkN171 : Str -> N ;
mkN171 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"и"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"ата" ;
                               Pl => base_1+"ите"
                             } ;
          Def Proximal => table {
                            Sg => nonExist ;
                            Pl => base_1+"иве"
                          } ;
          Def Distal => table {
                          Sg => nonExist ;
                          Pl => base_1+"ине"
                        }
        } ;
    count_form = base_1+"и" ;
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1+"и"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Fem
  } ;

mkN172 : Str -> N ;
mkN172 base =
  case base of {
    base_1+"е" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"е" ;
                         Pl => base_1+"а"
                       } ;
              Def Unspecified => table {
                                   Sg => nonExist ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => nonExist ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            }
            } ;
        count_form = base_1+"а" ;
        vocative = table {
                     Sg => nonExist ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Neuter
      };
    _ => error "Can't apply paradigm mkN172"
  } ;

mkN173 : Str -> N ;
mkN173 base =
  case base of {
    base_1+"о" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"о" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ото" ;
                                   Pl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ово" ;
                                Pl => base_1+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"оно" ;
                              Pl => base_1+"ине"
                            }
            } ;
        count_form = base_1+"и" ;
        vocative = table {
                     Sg => nonExist ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Neuter
      };
    _ => error "Can't apply paradigm mkN173"
  } ;

mkN174 : Str -> N ;
mkN174 base =
  case base of {
    base_1+"к" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"к" ;
                         Pl => base_1+"ции"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"кот" ;
                                   Pl => base_1+"циите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ков" ;
                                Pl => base_1+"цииве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"кон" ;
                              Pl => base_1+"циине"
                            }
            } ;
        count_form = base_1+"ка" ;
        vocative = table {
                     Sg => base_1+"ку" ;
                     Pl => base_1+"ции"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN174"
  } ;

mkN175 : Str -> N ;
mkN175 base =
  case base of {
    base_1+"ја" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ја" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => nonExist ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => nonExist ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            }
            } ;
        count_form = base_1+"и" ;
        vocative = table {
                     Sg => nonExist ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN175"
  } ;

mkN176 : Str -> N ;
mkN176 base =
  case base of {
    base_1+"на" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"на" ;
                         Pl => base_1+"ње"
                       } ;
              Def Unspecified => table {
                                   Sg => nonExist ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => nonExist ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => nonExist ;
                              Pl => nonExist
                            }
            } ;
        count_form = base_1+"ње" ;
        vocative = table {
                     Sg => nonExist ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN176"
  } ;

mkN177 : Str -> N ;
mkN177 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"вци" --guessed
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"то" ;
                               Pl => base_1+"вците" --guessed
                             } ;
          Def Proximal => table {
                            Sg => base_1+"во" ;
                            Pl => base_1+"вциве" --guessed
                          } ;
          Def Distal => table {
                          Sg => base_1+"но" ;
                          Pl => base_1+"вцине" --guessed
                        }
        } ;
    count_form = base_1+"вци" ; --guessed
    vocative = table {
                 Sg => base_1 ; --guessed
                 Pl => base_1+"вци" --guessed
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Neuter
  } ;

mkN178 : Str -> N ;
mkN178 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"вци"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"то" ;
                               Pl => base_1+"вците"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"во" ;
                            Pl => base_1+"вциве"
                          } ;
          Def Distal => table {
                          Sg => base_1+"но" ;
                          Pl => base_1+"вцине"
                        }
        } ;
    count_form = base_1+"вци" ;
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1+"вци"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Neuter
  } ;

mkN179 : Str -> N ;
mkN179 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"и"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"от" ;
                               Pl => base_1+"ите"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ов" ;
                            Pl => base_1+"иве"
                          } ;
          Def Distal => table {
                          Sg => base_1+"он" ;
                          Pl => base_1+"ине"
                        }
        } ;
    count_form = base_1+"а" ;
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1+"и"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN180 : Str -> N ;
mkN180 base =
  case base of {
    base_1+"з"+base_2@(?+?+?+?+?)+"а"+base_3@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"з"+base_2+"а"+base_3 ;
                         Pl => nonExist
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"с"+base_2+base_3+"от" ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"с"+base_2+base_3+"ов" ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => base_1+"с"+base_2+base_3+"он" ;
                              Pl => nonExist
                            }
            } ;
        count_form = nonExist ;
        vocative = table {
                     Sg => base_1+"с"+base_2+base_3+"у" ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN180"
  } ;

mkN181 : Str -> N ;
mkN181 base =
  case base of {
    base_1+"а" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ата" ;
                                   Pl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ава" ;
                                Pl => base_1+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"ана" ;
                              Pl => base_1+"ине"
                            }
            } ;
        count_form = base_1+"и" ;
        vocative = table {
                     Sg => base_1+"е" ;
                     Pl => base_1+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Neuter
      };
    _ => error "Can't apply paradigm mkN181"
  } ;

mkN182 : Str -> N ;
mkN182 base =
  case base of {
    base_1+"чов"+base_2@?+"к" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"чов"+base_2+"к" ;
                         Pl => base_1+"луѓ"+base_2
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"чов"+base_2+"кот" ;
                                   Pl => base_1+"луѓ"+base_2+"то"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"чов"+base_2+"ков" ;
                                Pl => base_1+"луѓ"+base_2+"во"
                              } ;
              Def Distal => table {
                              Sg => base_1+"чов"+base_2+"кон" ;
                              Pl => base_1+"луѓ"+base_2+"но"
                            }
            } ;
        count_form = base_1+"луѓ"+base_2 ;
        vocative = table {
                     Sg => base_1+"чов"+base_2+"ку" ;
                     Pl => base_1+"луѓ"+base_2+"то"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN182"
  } ;

mkN183 : Str -> N ;
mkN183 base =
  case base of {
    base_1+"и"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"и"+base_2 ;
                         Pl => base_1+base_2+"ови"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"и"+base_2+"от" ;
                                   Pl => base_1+base_2+"овите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"и"+base_2+"ов" ;
                                Pl => base_1+base_2+"овиве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"и"+base_2+"он" ;
                              Pl => base_1+base_2+"овине"
                            }
            } ;
        count_form = base_1+"и"+base_2+"а" ;
        vocative = table {
                     Sg => base_1+"и"+base_2+"у" ;
                     Pl => base_1+base_2+"ови"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN183"
  } ;

mkN184 : Str -> N ;
mkN184 base =
  case base of {
    base_1+base_2@(?+?+?+?+?+?)+"е"+base_3@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+base_2+"е"+base_3 ;
                         Pl => base_1+"о"+base_2+base_3+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+base_2+"е"+base_3+"от" ;
                                   Pl => base_1+"о"+base_2+base_3+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+base_2+"е"+base_3+"ов" ;
                                Pl => base_1+"о"+base_2+base_3+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+base_2+"е"+base_3+"он" ;
                              Pl => base_1+"о"+base_2+base_3+"ине"
                            }
            } ;
        count_form = base_1+base_2+"е"+base_3+"а" ;
        vocative = table {
                     Sg => base_1+base_2+"е"+base_3+"у" ;
                     Pl => base_1+"о"+base_2+base_3+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN184"
  } ;

mkN185 : Str -> N ;
mkN185 base =
  case base of {
    "м"+base_1+"ец" => lin N
      { s = table {
              Indef => table {
                         Sg => "м"+base_1+"ец" ;
                         Pl => "m"+base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => "м"+base_1+"ецот" ;
                                   Pl => "m"+base_1+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => "м"+base_1+"ецов" ;
                                Pl => "m"+base_1+"иве"
                              } ;
              Def Distal => table {
                              Sg => "м"+base_1+"ецон" ;
                              Pl => "m"+base_1+"ине"
                            }
            } ;
        count_form = "м"+base_1+"еца" ;
        vocative = table {
                     Sg => "м"+base_1+"ецу" ;
                     Pl => "m"+base_1+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN185"
  } ;

mkN186 : Str -> N ;
mkN186 base =
  case base of {
    base_1+"а"+base_2@(?+?+?)+"е"+base_3@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а"+base_2+"е"+base_3 ;
                         Pl => base_1+base_2+base_3+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"а"+base_2+"е"+base_3+"от" ;
                                   Pl => base_1+base_2+base_3+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"а"+base_2+"е"+base_3+"ов" ;
                                Pl => base_1+base_2+base_3+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"а"+base_2+"е"+base_3+"он" ;
                              Pl => base_1+base_2+base_3+"ине"
                            }
            } ;
        count_form = base_1+"а"+base_2+"е"+base_3+"а" ;
        vocative = table {
                     Sg => base_1+"а"+base_2+"е"+base_3+"у" ;
                     Pl => base_1+base_2+base_3+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN186"
  } ;

mkN187 : Str -> N ;
mkN187 base =
  case base of {
    base_1+"е"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"е"+base_2 ;
                         Pl => base_1+"ј"+base_2+"ови"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"е"+base_2+"от" ;
                                   Pl => base_1+"ј"+base_2+"овите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"е"+base_2+"ов" ;
                                Pl => base_1+"ј"+base_2+"овиве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"е"+base_2+"он" ;
                              Pl => base_1+"ј"+base_2+"овине"
                            }
            } ;
        count_form = base_1+"е"+base_2+"а" ;
        vocative = table {
                     Sg => base_1+"е"+base_2+"у" ;
                     Pl => base_1+"ј"+base_2+"ови"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN187"
  } ;

mkN188 : Str -> N ;
mkN188 base =
  case base of {
    base_1+"ак" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"ак" ;
                         Pl => base_1+"ци"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"акот" ;
                                   Pl => base_1+"ците"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"аков" ;
                                Pl => base_1+"циве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"акон" ;
                              Pl => base_1+"цине"
                            }
            } ;
        count_form = base_1+"ака" ;
        vocative = table {
                     Sg => base_1+"аку" ;
                     Pl => base_1+"ци"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN188"
  } ;

mkN189 : Str -> N ;
mkN189 base =
  case base of {
    base_1+"тчов"+base_2@?+"к" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"тчов"+base_2+"к" ;
                         Pl => base_1+"длуѓ"+base_2
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"тчов"+base_2+"кот" ;
                                   Pl => base_1+"длуѓ"+base_2+"то"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"тчов"+base_2+"ков" ;
                                Pl => base_1+"длуѓ"+base_2+"во"
                              } ;
              Def Distal => table {
                              Sg => base_1+"тчов"+base_2+"кон" ;
                              Pl => base_1+"длуѓ"+base_2+"но"
                            }
            } ;
        count_form = base_1+"длуѓ"+base_2 ;
        vocative = table {
                     Sg => base_1+"тчов"+base_2+"ку" ;
                     Pl => base_1+"длуѓ"+base_2+"то"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN189"
  } ;

mkN190 : Str -> N ;
mkN190 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => nonExist
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"то" ;
                               Pl => nonExist
                             } ;
          Def Proximal => table {
                            Sg => base_1+"во" ;
                            Pl => nonExist
                          } ;
          Def Distal => table {
                          Sg => base_1+"но" ;
                          Pl => nonExist
                        }
        } ;
    count_form = nonExist ;
    vocative = table {
                 Sg => base_1 ;
                 Pl => nonExist
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Fem
  } ;

mkN191 : Str -> N ;
mkN191 base =
  case base of {
    base_1+"о"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"о"+base_2 ;
                         Pl => base_1+base_2+"ови"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+base_2+"от" ;
                                   Pl => base_1+base_2+"овите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+base_2+"ов" ;
                                Pl => base_1+base_2+"овиве"
                              } ;
              Def Distal => table {
                              Sg => base_1+base_2+"он" ;
                              Pl => base_1+base_2+"овине"
                            }
            } ;
        count_form = base_1+base_2+"а" ;
        vocative = table {
                     Sg => base_1+base_2+"у" ;
                     Pl => base_1+base_2+"ови"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN191"
  } ;

mkN192 : Str -> N ;
mkN192 base =
  case base of {
    base_1+"г"+base_2@(?+?+?) => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"г"+base_2 ;
                         Pl => nonExist
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"г"+base_2+"от" ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"г"+base_2+"ов" ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => base_1+"г"+base_2+"он" ;
                              Pl => nonExist
                            }
            } ;
        count_form = nonExist ;
        vocative = table {
                     Sg => base_1+base_2+"е" ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN192"
  } ;

mkN193 : Str -> N ;
mkN193 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => nonExist
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"–то" ;
                               Pl => nonExist
                             } ;
          Def Proximal => table {
                            Sg => base_1+"–во" ;
                            Pl => nonExist
                          } ;
          Def Distal => table {
                          Sg => base_1+"–но" ;
                          Pl => nonExist
                        }
        } ;
    count_form = nonExist ;
    vocative = table {
                 Sg => base_1 ;
                 Pl => nonExist
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Neuter
  } ;

mkN194 : Str -> N ;
mkN194 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"еи"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"от" ;
                               Pl => base_1+"еите"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ов" ;
                            Pl => base_1+"еиве"
                          } ;
          Def Distal => table {
                          Sg => base_1+"он" ;
                          Pl => base_1+"еине"
                        }
        } ;
    count_form = base_1+"а" ;
    vocative = table {
                 Sg => base_1+"у" ;
                 Pl => base_1+"еи"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN195 : Str -> N ;
mkN195 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"вци"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"-то" ;
                               Pl => base_1+"вците"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"-во" ;
                            Pl => base_1+"вциве"
                          } ;
          Def Distal => table {
                          Sg => base_1+"-но" ;
                          Pl => base_1+"вцине"
                        }
        } ;
    count_form = base_1+"вци" ;
    vocative = table {
                 Sg => base_1 ;
                 Pl => base_1+"вци"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Masc
  } ;

mkN196 : Str -> N ;
mkN196 base =
  case base of {
    base_1+"в" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"в" ;
                         Pl => base_1+"си"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"вот" ;
                                   Pl => base_1+"сите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"вов" ;
                                Pl => base_1+"сиве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"вон" ;
                              Pl => base_1+"сине"
                            }
            } ;
        count_form = base_1+"ва" ;
        vocative = table {
                     Sg => base_1+"ву" ;
                     Pl => base_1+"си"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN196"
  } ;

mkN197 : Str -> N ;
mkN197 base =
  case base of {
    base_1+"а" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ата" ;
                                   Pl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ава" ;
                                Pl => base_1+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"ана" ;
                              Pl => base_1+"ине"
                            }
            } ;
        count_form = base_1+"и" ;
        vocative = table {
                     Sg => nonExist ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN197"
  } ;

mkN198 : Str -> N ;
mkN198 base =
  case base of {
    base_1+"н" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"н" ;
                         Pl => base_1+"а"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"нто" ;
                                   Pl => base_1+"ата"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"нво" ;
                                Pl => base_1+"ава"
                              } ;
              Def Distal => table {
                              Sg => base_1+"нно" ;
                              Pl => base_1+"ана"
                            }
            } ;
        count_form = base_1+"а" ;
        vocative = table {
                     Sg => base_1+"н" ;
                     Pl => base_1+"а"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN198"
  } ;

mkN199 : Str -> N ;
mkN199 base =
  case base of {
    base_1+"и" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"и" ;
                         Pl => base_1
                       } ;
              Def Unspecified => table {
                                   Sg => nonExist ;
                                   Pl => base_1+"те"
                                 } ;
              Def Proximal => table {
                                Sg => nonExist ;
                                Pl => base_1+"ве"
                              } ;
              Def Distal => table {
                              Sg => nonExist ;
                              Pl => base_1+"не"
                            }
            } ;
        count_form = base_1 ;
        vocative = table {
                     Sg => nonExist ;
                     Pl => base_1
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN199"
  } ;

mkN200 : Str -> N ;
mkN200 base =
  case base of {
    "подраж"+base_1+base_2@?+"л" => lin N
      { s = table {
              Indef => table {
                         Sg => "подраж"+base_1+base_2+"л" ;
                         Pl => "подраж"+base_1+base_2+"ли"
                       } ;
              Def Unspecified => table {
                                   Sg => "подраж"+base_1+base_2+"лот" ;
                                   Pl => "подраж"+base_1+base_2+"лите"
                                 } ;
              Def Proximal => table {
                                Sg => "подраж"+base_1+base_2+"лов" ;
                                Pl => "подраж"+base_1+base_2+"ливе"
                              } ;
              Def Distal => table {
                              Sg => "подраж"+base_1+base_2+"лон" ;
                              Pl => "подраж"+base_1+base_2+"лине"
                            }
            } ;
        count_form = "подраж"+base_1+base_2+"ла" ;
        vocative = table {
                     Sg => "имит"+base_1+"ор"+base_2 ;
                     Pl => "подраж"+base_1+base_2+"ли"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN200"
  } ;

mkN201 : Str -> N ;
mkN201 base =
  case base of {
    base_1+"е" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"е" ;
                         Pl => base_1+"иња"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ето" ;
                                   Pl => base_1+"ињата"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ево" ;
                                Pl => base_1+"ињава"
                              } ;
              Def Distal => table {
                              Sg => base_1+"ено" ;
                              Pl => base_1+"ињана"
                            }
            } ;
        count_form = base_1+"иња" ;
        vocative = table {
                     Sg => base_1+"е" ;
                     Pl => base_1+"иња"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Fem
      };
    _ => error "Can't apply paradigm mkN201"
  } ;

mkN202 : Str -> N ;
mkN202 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => nonExist
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"от" ;
                               Pl => nonExist
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ов" ;
                            Pl => nonExist
                          } ;
          Def Distal => table {
                          Sg => base_1+"он" ;
                          Pl => nonExist
                        }
        } ;
    count_form = nonExist ;
    vocative = table {
                 Sg => base_1+"у" ;
                 Pl => nonExist
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Neuter
  } ;

mkN203 : Str -> N ;
mkN203 base =
  case base of {
    base_1+"а" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"а" ;
                         Pl => nonExist
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"ата" ;
                                   Pl => nonExist
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"ава" ;
                                Pl => nonExist
                              } ;
              Def Distal => table {
                              Sg => base_1+"ана" ;
                              Pl => nonExist
                            }
            } ;
        count_form = nonExist ;
        vocative = table {
                     Sg => base_1+"о" ;
                     Pl => nonExist
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Neuter
      };
    _ => error "Can't apply paradigm mkN203"
  } ;

mkN204 : Str -> N ;
mkN204 base_1 =
  lin N
  { s = table {
          Indef => table {
                     Sg => base_1 ;
                     Pl => base_1+"и"
                   } ;
          Def Unspecified => table {
                               Sg => base_1+"та" ;
                               Pl => base_1+"ите"
                             } ;
          Def Proximal => table {
                            Sg => base_1+"ва" ;
                            Pl => base_1+"иве"
                          } ;
          Def Distal => table {
                          Sg => base_1+"на" ;
                          Pl => base_1+"ине"
                        }
        } ;
    count_form = base_1+"и" ;
    vocative = table {
                 Sg => base_1+"о" ;
                 Pl => base_1+"и"
               } ;
    rel = \\_,_ => base_1 ;
    relType = Pref ;
    g = Fem
  } ;

mkN205 : Str -> N ;
mkN205 base =
  case base of {
    base_1+"р" => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"р" ;
                         Pl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"рта" ;
                                   Pl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"рва" ;
                                Pl => base_1+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"рна" ;
                              Pl => base_1+"ине"
                            }
            } ;
        count_form = base_1+"и" ;
        vocative = table {
                     Sg => base_1+"о" ;
                     Pl => base_1+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN205"
  } ;

mkN206 : Str -> N ;
mkN206 base =
  case base of {
    base_1+"е"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => base_1+"е"+base_2 ;
                         Pl => base_1+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   Sg => base_1+"е"+base_2+"от" ;
                                   Pl => base_1+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                Sg => base_1+"е"+base_2+"ов" ;
                                Pl => base_1+base_2+"иве"
                              } ;
              Def Distal => table {
                              Sg => base_1+"е"+base_2+"он" ;
                              Pl => base_1+base_2+"ине"
                            }
            } ;
        count_form = base_1+"е"+base_2+"а" ;
        vocative = table {
                     Sg => base_1+"е"+base_2+"е" ;
                     Pl => base_1+base_2+"и"
                   } ;
        rel = \\_,_ => base_1 ;
        relType = Pref ;
        g = Masc
      };
    _ => error "Can't apply paradigm mkN206"
  } ;

mkV001 : Str -> V ;
mkV001 base_1 =
  lin V
  { present = \\_ =>
              table {
                Sg => table {
                        P1 => base_1+"м" ;
                        P2 => base_1+"ш" ;
                        P3 => base_1
                      } ;
                Pl => table {
                        P1 => base_1+"ме" ;
                        P2 => base_1+"те" ;
                        P3 => base_1+"ат"
                      }
              } ;
    aorist = table {
               Sg => table {
                       P1 => base_1+"в" ; --guessed
                       P2 => base_1 ; --guessed
                       P3 => base_1 --guessed
                     } ;
               Pl => table {
                       P1 => base_1+"вме" ; --guessed
                       P2 => base_1+"вте" ; --guessed
                       P3 => base_1+"а" --guessed
                     }
             } ;
    imperfect = \\_ =>
                table {
                  Sg => table {
                          P1 => base_1+"в" ;
                          P2 => base_1+"ше" ;
                          P3 => base_1+"ше"
                        } ;
                  Pl => table {
                          P1 => base_1+"вме" ;
                          P2 => base_1+"вте" ;
                          P3 => base_1+"а"
                        }
                } ;
    Imperative = \\_ =>
                 table {
                   Sg => base_1+"ј" ;
                   Pl => base_1+"јте"
                 } ;
    participle = { aorist = \\_ => 
                            table {
                              GSg Masc => base_1+"л" ; --guessed
                              GSg Fem => base_1+"ла" ; --guessed
                              GSg Neuter => base_1+"ло" ; --guessed
                              GPl => base_1+"ле" --guessed
                            } ;
                   imperfect = table {
                                 GSg Masc => base_1+"л" ;
                                 GSg Fem => base_1+"ла" ;
                                 GSg Neuter => base_1+"ло" ;
                                 GPl => base_1+"ле"
                               } ;
                   perfect = \\_ => nonExist ;
                   adjectival = \\_ => base_1+"н" ;
                   adverbial = base_1+"јќи"
                 } ;
    noun_from_verb = base_1+"ње" ;
    isRefl = False
  } ;

mkV002 : Str -> V ;
mkV002 base_1 =
  lin V
  { present = \\_ =>
              table {
                Sg => table {
                        P1 => base_1+"м" ;
                        P2 => base_1+"ш" ;
                        P3 => base_1
                      } ;
                Pl => table {
                        P1 => base_1+"ме" ;
                        P2 => base_1+"те" ;
                        P3 => base_1+"ат"
                      }
              } ;
    aorist = table {
               Sg => table {
                       P1 => base_1+"в" ; --guessed
                       P2 => base_1 ; --guessed
                       P3 => base_1 --guessed
                     } ;
               Pl => table {
                       P1 => base_1+"вме" ; --guessed
                       P2 => base_1+"вте" ; --guessed
                       P3 => base_1+"а" --guessed
                     }
             } ;
    imperfect = \\_ =>
                table {
                  Sg => table {
                          P1 => base_1+"в" ;
                          P2 => base_1+"ше" ;
                          P3 => base_1+"ше"
                        } ;
                  Pl => table {
                          P1 => base_1+"вме" ;
                          P2 => base_1+"вте" ;
                          P3 => base_1+"а"
                        }
                } ;
    Imperative = \\_ =>
                 table {
                   Sg => base_1+"ј" ;
                   Pl => base_1+"јте"
                 } ;
    participle = { aorist = \\_ =>
                            table {
                              GSg Masc => base_1+"л" ; --guessed
                              GSg Fem => base_1+"ла" ; --guessed
                              GSg Neuter => base_1+"ло" ; --guessed
                              GPl => base_1+"ле" --guessed
                            } ;
                   imperfect = table {
                                 GSg Masc => base_1+"л" ;
                                 GSg Fem => base_1+"ла" ;
                                 GSg Neuter => base_1+"ло" ;
                                 GPl => base_1+"ле"
                               } ;
                   perfect = \\_ => nonExist ;
                   adjectival = \\_ => base_1+"н" ; --guessed
                   adverbial = base_1+"јќи"
                 } ;
    noun_from_verb = base_1+"ње" ;
    isRefl = False
  } ;

mkV003 : Str -> V ;
mkV003 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"иш" ;
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ;
                            P2 => base_1+"ите" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ив" ; --guessed
                           P2 => base_1+"и" ; --guessed
                           P3 => base_1+"и" --guessed
                         } ;
                   Pl => table {
                           P1 => base_1+"ивме" ; --guessed
                           P2 => base_1+"ивте" ; --guessed
                           P3 => base_1+"ија" --guessed
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ил" ; --guessed
                                  GSg Fem => base_1+"ила" ; --guessed
                                  GSg Neuter => base_1+"ило" ; --guessed
                                  GPl => base_1+"иле" --guessed
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ен" ;
                       adverbial = base_1+"ејќи"
                     } ;
        noun_from_verb = base_1+"ење" ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV003"
  } ;

mkV004 : Str -> V ;
mkV004 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"иш" ;
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ;
                            P2 => base_1+"ите" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ив" ; --guessed
                           P2 => base_1+"и" ; --guessed
                           P3 => base_1+"и" --guessed
                         } ;
                   Pl => table {
                           P1 => base_1+"ивме" ; --guessed
                           P2 => base_1+"ивте" ; --guessed
                           P3 => base_1+"ија" --guessed
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ил" ; --guessed
                                  GSg Fem => base_1+"ила" ; --guessed
                                  GSg Neuter => base_1+"ило" ; --guessed
                                  GPl => base_1+"иле" --guessed
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ет" ; --guessed
                       adverbial = base_1+"ејќи"
                     } ;
        noun_from_verb = base_1+"ење" ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV004"
  } ;

mkV005 : Str -> V ;
mkV005 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"иш" ;
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ;
                            P2 => base_1+"ите" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ив" ;
                           P2 => base_1+"и" ;
                           P3 => base_1+"и"
                         } ;
                   Pl => table {
                           P1 => base_1+"ивме" ;
                           P2 => base_1+"ивте" ;
                           P3 => base_1+"ија"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ил" ;
                                  GSg Fem => base_1+"ила" ;
                                  GSg Neuter => base_1+"ило" ;
                                  GPl => base_1+"иле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ен" ;
                       adverbial = base_1+"ејќи" --guessed
                     } ;
        noun_from_verb = base_1+"ење" ; --guessed
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV005"
  } ;

mkV006 : Str -> V ;
mkV006 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ев" ; --guessed
                           P2 => base_1+"е" ; --guessed
                           P3 => base_1+"е" --guessed
                         } ;
                   Pl => table {
                           P1 => base_1+"евме" ; --guessed
                           P2 => base_1+"евте" ; --guessed
                           P3 => base_1+"еа" --guessed
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ел" ; --guessed
                                  GSg Fem => base_1+"ела" ; --guessed
                                  GSg Neuter => base_1+"ело" ; --guessed
                                  GPl => base_1+"еле" --guessed
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ен" ;
                       adverbial = base_1+"ејќи"
                     } ;
        noun_from_verb = base_1+"ење" ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV006"
  } ;

mkV007 : Str -> V ;
mkV007 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ав" ;
                           P2 => base_1+"а" ;
                           P3 => base_1+"а"
                         } ;
                   Pl => table {
                           P1 => base_1+"авме" ;
                           P2 => base_1+"авте" ;
                           P3 => base_1+"аа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ал" ;
                                  GSg Fem => base_1+"ала" ;
                                  GSg Neuter => base_1+"ало" ;
                                  GPl => base_1+"але"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ат" ;
                       adverbial = base_1+"ејќи" --guessed
                     } ;
        noun_from_verb = base_1+"ење" ; --guessed
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV007"
  } ;

mkV008 : Str -> V ;
mkV008 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ; --guessed
                            P2 => base_1+"иш" ; --guessed
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ; --guessed
                            P2 => base_1+"ите" ; --guessed
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ив" ; --guessed
                           P2 => base_1+"и" ; --guessed
                           P3 => base_1+"и" --guessed
                         } ;
                   Pl => table {
                           P1 => base_1+"ивме" ; --guessed
                           P2 => base_1+"ивте" ; --guessed
                           P3 => base_1+"ија" --guessed
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ; --guessed
                              P2 => base_1+"еше" ; --guessed
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ; --guessed
                              P2 => base_1+"евте" ; --guessed
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ => 
                     table {
                       Sg => base_1+"и" ; --guessed
                       Pl => base_1+"ете" --guessed
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ил" ; --guessed
                                  GSg Fem => base_1+"ила" ; --guessed
                                  GSg Neuter => base_1+"ило" ; --guessed
                                  GPl => base_1+"иле" --guessed
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ; --guessed
                                     GSg Fem => base_1+"ела" ; --guessed
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ет" ; --guessed
                       adverbial = base_1+"ејќи" --guessed
                     } ;
        noun_from_verb = base_1+"ење" ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV008"
  } ;

mkV009 : Str -> V ;
mkV009 base_1 =
  lin V
  { present = \\_ =>
              table {
                Sg => table {
                        P1 => base_1+"м" ;
                        P2 => base_1+"ш" ;
                        P3 => base_1
                      } ;
                Pl => table {
                        P1 => base_1+"ме" ;
                        P2 => base_1+"те" ;
                        P3 => base_1+"ат"
                      }
              } ;
    aorist = table {
               Sg => table {
                       P1 => base_1+"в" ;
                       P2 => base_1 ;
                       P3 => base_1
                     } ;
               Pl => table {
                       P1 => base_1+"вме" ;
                       P2 => base_1+"вте" ;
                       P3 => base_1+"а"
                     }
             } ;
    imperfect = \\_ =>
                table {
                  Sg => table {
                          P1 => base_1+"в" ;
                          P2 => base_1+"ше" ;
                          P3 => base_1+"ше"
                        } ;
                  Pl => table {
                          P1 => base_1+"вме" ;
                          P2 => base_1+"вте" ;
                          P3 => base_1+"а"
                        }
                } ;
    Imperative = \\_ =>
                 table {
                   Sg => base_1+"ј" ;
                   Pl => base_1+"јте"
                 } ;
    participle = { aorist = \\_ => 
                            table {
                              GSg Masc => base_1+"л" ;
                              GSg Fem => base_1+"ла" ;
                              GSg Neuter => base_1+"ло" ;
                              GPl => base_1+"ле"
                            } ;
                   imperfect = table {
                                 GSg Masc => base_1+"л" ;
                                 GSg Fem => base_1+"ла" ;
                                 GSg Neuter => base_1+"ло" ;
                                 GPl => base_1+"ле"
                               } ;
                   perfect = \\_ => nonExist ;
                   adjectival = \\_ => base_1+"н" ;
                   adverbial = base_1+"јќи" --guessed
                 } ;
    noun_from_verb = base_1+"ње" ; --guessed
    isRefl = False
  } ;

mkV010 : Str -> V ;
mkV010 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"иш" ;
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ;
                            P2 => base_1+"ите" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ив" ; --guessed
                           P2 => base_1+"и" ; --guessed
                           P3 => base_1+"и" --guessed
                         } ;
                   Pl => table {
                           P1 => base_1+"ивме" ; --guessed
                           P2 => base_1+"ивте" ; --guessed
                           P3 => base_1+"ија" --guessed
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ил" ; --guessed
                                  GSg Fem => base_1+"ила" ; --guessed
                                  GSg Neuter => base_1+"ило" ; --guessed
                                  GPl => base_1+"иле" --guessed
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ет" ;
                       adverbial = base_1+"ејќи"
                     } ;
        noun_from_verb = base_1+"ење" ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV010"
  } ;

mkV011 : Str -> V ;
mkV011 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"јам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"јат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"в" ; --guessed
                           P2 => base_1 ; --guessed
                           P3 => base_1 --guessed
                         } ;
                   Pl => table {
                           P1 => base_1+"вме" ; --guessed
                           P2 => base_1+"вте" ; --guessed
                           P3 => base_1+"ја" --guessed
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ;
                       Pl => base_1+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"л" ; --guessed
                                  GSg Fem => base_1+"ла" ; --guessed
                                  GSg Neuter => base_1+"ло" ; --guessed
                                  GPl => base_1+"ле" --guessed
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ен" ;
                       adverbial = base_1+"ејќи"
                     } ;
        noun_from_verb = base_1+"ење" ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV011"
  } ;

mkV012 : Str -> V ;
mkV012 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"јам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"јат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"в" ; --guessed
                           P2 => base_1 ; --guessed
                           P3 => base_1 --guessed
                         } ;
                   Pl => table {
                           P1 => base_1+"вме" ; --guessed
                           P2 => base_1+"вте" ; --guessed
                           P3 => base_1+"ја" --guessed
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ;
                       Pl => base_1+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"л" ; --guessed
                                  GSg Fem => base_1+"ла" ; --guessed
                                  GSg Neuter => base_1+"ло" ; --guessed
                                  GPl => base_1+"ле" --guessed
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ен" ; --guessed
                       adverbial = base_1+"ејќи"
                     } ;
        noun_from_verb = base_1+"ење" ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV012"
  } ;

mkV013 : Str -> V ;
mkV013 base_1 =
  lin V
  { present = \\_ =>
              table {
                Sg => table {
                        P1 => base_1+"м" ;
                        P2 => base_1+"ш" ;
                        P3 => base_1
                      } ;
                Pl => table {
                        P1 => base_1+"ме" ;
                        P2 => base_1+"те" ;
                        P3 => base_1+"ат"
                      }
              } ;
    aorist = table {
               Sg => table {
                       P1 => base_1+"в" ;
                       P2 => base_1 ;
                       P3 => base_1
                     } ;
               Pl => table {
                       P1 => base_1+"вме" ;
                       P2 => base_1+"вте" ;
                       P3 => base_1+"а"
                     }
             } ;
    imperfect = \\_ =>
                table {
                  Sg => table {
                          P1 => base_1+"в" ;
                          P2 => base_1+"ше" ;
                          P3 => base_1+"ше"
                        } ;
                  Pl => table {
                          P1 => base_1+"вме" ;
                          P2 => base_1+"вте" ;
                          P3 => base_1+"а"
                        }
                } ;
    Imperative = \\_ =>
                 table {
                   Sg => base_1+"ј" ;
                   Pl => base_1+"јте"
                 } ;
    participle = { aorist = \\_ => 
                            table {
                              GSg Masc => base_1+"л" ;
                              GSg Fem => base_1+"ла" ;
                              GSg Neuter => base_1+"ло" ;
                              GPl => base_1+"ле"
                            } ;
                   imperfect = table {
                                 GSg Masc => base_1+"л" ;
                                 GSg Fem => base_1+"ла" ;
                                 GSg Neuter => base_1+"ло" ;
                                 GPl => base_1+"ле"
                               } ;
                   perfect = \\_ => nonExist ;
                   adjectival = \\_ => base_1+"н" ;
                   adverbial = base_1+"јќи"
                 } ;
    noun_from_verb = base_1+"ње" ;
    isRefl = False
  } ;

mkV014 : Str -> V ;
mkV014 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"иш" ;
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ;
                            P2 => base_1+"ите" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ив" ;
                           P2 => base_1+"и" ;
                           P3 => base_1+"и"
                         } ;
                   Pl => table {
                           P1 => base_1+"ивме" ;
                           P2 => base_1+"ивте" ;
                           P3 => base_1+"ија"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ил" ;
                                  GSg Fem => base_1+"ила" ;
                                  GSg Neuter => base_1+"ило" ;
                                  GPl => base_1+"иле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ет" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = base_1+"ење" ; --guessed
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV014"
  } ;

mkV015 : Str -> V ;
mkV015 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"иш" ;
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ;
                            P2 => base_1+"ите" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ив" ;
                           P2 => base_1+"и" ;
                           P3 => base_1+"и"
                         } ;
                   Pl => table {
                           P1 => base_1+"ивме" ;
                           P2 => base_1+"ивте" ;
                           P3 => base_1+"ија"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ил" ;
                                  GSg Fem => base_1+"ила" ;
                                  GSg Neuter => base_1+"ило" ;
                                  GPl => base_1+"иле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ен" ; --guessed
                       adverbial = base_1+"ејќи" --guessed
                     } ;
        noun_from_verb = base_1+"ење" ; --guessed
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV015"
  } ;

mkV016 : Str -> V ;
mkV016 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"јам" ;
                            P2 => base_1+"иш" ;
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ;
                            P2 => base_1+"ите" ;
                            P3 => base_1+"јат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ив" ;
                           P2 => base_1+"и" ;
                           P3 => base_1+"и"
                         } ;
                   Pl => table {
                           P1 => base_1+"ивме" ;
                           P2 => base_1+"ивте" ;
                           P3 => base_1+"ија"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ;
                       Pl => base_1+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ил" ;
                                  GSg Fem => base_1+"ила" ;
                                  GSg Neuter => base_1+"ило" ;
                                  GPl => base_1+"иле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ен" ;
                       adverbial = base_1+"ејќи" --guessed
                     } ;
        noun_from_verb = base_1+"ење" ; --guessed
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV016"
  } ;

mkV017 : Str -> V ;
mkV017 base_1 =
  lin V
  { present = \\_ =>
              table {
                Sg => table {
                        P1 => base_1+"м" ;
                        P2 => base_1+"ш" ;
                        P3 => base_1
                      } ;
                Pl => table {
                        P1 => base_1+"ме" ;
                        P2 => base_1+"те" ;
                        P3 => base_1+"ат"
                      }
              } ;
    aorist = table {
               Sg => table {
                       P1 => base_1+"в" ;
                       P2 => base_1 ;
                       P3 => base_1
                     } ;
               Pl => table {
                       P1 => base_1+"вме" ;
                       P2 => base_1+"вте" ;
                       P3 => base_1+"а"
                     }
             } ;
    imperfect = \\_ =>
                table {
                  Sg => table {
                          P1 => base_1+"в" ;
                          P2 => base_1+"ше" ;
                          P3 => base_1+"ше"
                        } ;
                  Pl => table {
                          P1 => base_1+"вме" ;
                          P2 => base_1+"вте" ;
                          P3 => base_1+"а"
                        }
                } ;
    Imperative = \\_ =>
                 table {
                   Sg => base_1+"ј" ;
                   Pl => base_1+"јте"
                 } ;
    participle = { aorist = \\_ => 
                            table {
                              GSg Masc => base_1+"л" ;
                              GSg Fem => base_1+"ла" ;
                              GSg Neuter => base_1+"ло" ;
                              GPl => base_1+"ле"
                            } ;
                   imperfect = table {
                                 GSg Masc => base_1+"л" ;
                                 GSg Fem => base_1+"ла" ;
                                 GSg Neuter => base_1+"ло" ;
                                 GPl => base_1+"ле"
                               } ;
                   perfect = \\_ => nonExist ;
                   adjectival = \\_ => base_1+"т" ; --guessed
                   adverbial = base_1+"јќи" --guessed
                 } ;
    noun_from_verb = base_1+"ње" ; --guessed
    isRefl = False
  } ;

mkV018 : Str -> V ;
mkV018 base =
  case base of {
    base_1+"де" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"дам" ;
                            P2 => base_1+"деш" ;
                            P3 => base_1+"де"
                          } ;
                    Pl => table {
                            P1 => base_1+"деме" ;
                            P2 => base_1+"дете" ;
                            P3 => base_1+"дат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"дов" ;
                           P2 => base_1+"де" ;
                           P3 => base_1+"де"
                         } ;
                   Pl => table {
                           P1 => base_1+"довме" ;
                           P2 => base_1+"довте" ;
                           P3 => base_1+"доа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"дев" ;
                              P2 => base_1+"деше" ;
                              P3 => base_1+"деше"
                            } ;
                      Pl => table {
                              P1 => base_1+"девме" ;
                              P2 => base_1+"девте" ;
                              P3 => base_1+"деа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ;
                       Pl => base_1+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"л" ;
                                  GSg Fem => base_1+"ла" ;
                                  GSg Neuter => base_1+"ло" ;
                                  GPl => base_1+"ле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"дел" ;
                                     GSg Fem => base_1+"дела" ;
                                     GSg Neuter => base_1+"дело" ;
                                     GPl => base_1+"деле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ден" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV018"
  } ;

mkV019 : Str -> V ;
mkV019 base =
  case base of {
    base_1+"зе" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"зам" ;
                            P2 => base_1+"зеш" ;
                            P3 => base_1+"зе"
                          } ;
                    Pl => table {
                            P1 => base_1+"земе" ;
                            P2 => base_1+"зете" ;
                            P3 => base_1+"зат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"гов" ;
                           P2 => base_1+"зе" ;
                           P3 => base_1+"зе"
                         } ;
                   Pl => table {
                           P1 => base_1+"говме" ;
                           P2 => base_1+"говте" ;
                           P3 => base_1+"гоа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"зев" ;
                              P2 => base_1+"зеше" ;
                              P3 => base_1+"зеше"
                            } ;
                      Pl => table {
                              P1 => base_1+"зевме" ;
                              P2 => base_1+"зевте" ;
                              P3 => base_1+"зеа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"зи" ;
                       Pl => base_1+"зете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"гол" ;
                                  GSg Fem => base_1+"гла" ;
                                  GSg Neuter => base_1+"гло" ;
                                  GPl => base_1+"гле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"зел" ;
                                     GSg Fem => base_1+"зела" ;
                                     GSg Neuter => base_1+"зело" ;
                                     GPl => base_1+"зеле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"зен" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV019"
  } ;

mkV020 : Str -> V ;
mkV020 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ав" ;
                           P2 => base_1+"а" ;
                           P3 => base_1+"а"
                         } ;
                   Pl => table {
                           P1 => base_1+"авме" ;
                           P2 => base_1+"авте" ;
                           P3 => base_1+"аа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ал" ;
                                  GSg Fem => base_1+"ала" ;
                                  GSg Neuter => base_1+"ало" ;
                                  GPl => base_1+"але"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ан" ;
                       adverbial = base_1+"ејќи"
                     } ;
        noun_from_verb = base_1+"ење" ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV020"
  } ;

mkV021 : Str -> V ;
mkV021 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ев" ;
                           P2 => base_1+"е" ;
                           P3 => base_1+"е"
                         } ;
                   Pl => table {
                           P1 => base_1+"евме" ;
                           P2 => base_1+"евте" ;
                           P3 => base_1+"еа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ел" ;
                                  GSg Fem => base_1+"ела" ;
                                  GSg Neuter => base_1+"ело" ;
                                  GPl => base_1+"еле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ен" ;
                       adverbial = base_1+"ејќи" --guessed
                     } ;
        noun_from_verb = base_1+"ење" ; --guessed
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV021"
  } ;

mkV022 : Str -> V ;
mkV022 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"в" ; --guessed
                           P2 => base_1 ; --guessed
                           P3 => base_1 --guessed
                         } ;
                   Pl => table {
                           P1 => base_1+"вме" ; --guessed
                           P2 => base_1+"вте" ; --guessed
                           P3 => base_1+"а" --guessed
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ;
                       Pl => base_1+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"л" ; --guessed
                                  GSg Fem => base_1+"ла" ; --guessed
                                  GSg Neuter => base_1+"ло" ; --guessed
                                  GPl => base_1+"ле" --guessed
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ен" ;
                       adverbial = base_1+"ејќи"
                     } ;
        noun_from_verb = base_1+"ење" ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV022"
  } ;

mkV023 : Str -> V ;
mkV023 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ав" ; --guessed
                           P2 => base_1+"а" ; --guessed
                           P3 => base_1+"а" --guessed
                         } ;
                   Pl => table {
                           P1 => base_1+"авме" ; --guessed
                           P2 => base_1+"авте" ; --guessed
                           P3 => base_1+"аа" --guessed
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ал" ; --guessed
                                  GSg Fem => base_1+"ала" ; --guessed
                                  GSg Neuter => base_1+"ало" ; --guessed
                                  GPl => base_1+"але" --guessed
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ет" ; --guessed
                       adverbial = base_1+"ејќи"
                     } ;
        noun_from_verb = base_1+"ење" ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV023"
  } ;

mkV024 : Str -> V ;
mkV024 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"иш" ;
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ;
                            P2 => base_1+"ите" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ов" ;
                           P2 => base_1+"е" ;
                           P3 => base_1+"е"
                         } ;
                   Pl => table {
                           P1 => base_1+"овме" ;
                           P2 => base_1+"овте" ;
                           P3 => base_1+"оа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ел" ;
                                  GSg Fem => base_1+"ела" ;
                                  GSg Neuter => base_1+"ело" ;
                                  GPl => base_1+"еле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ен" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = base_1+"ење" ; --guessed
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV024"
  } ;

mkV025 : Str -> V ;
mkV025 base =
  case base of {
    base_1+"се" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"сам" ;
                            P2 => base_1+"сеш" ;
                            P3 => base_1+"се"
                          } ;
                    Pl => table {
                            P1 => base_1+"семе" ;
                            P2 => base_1+"сете" ;
                            P3 => base_1+"сат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"сов" ;
                           P2 => base_1+"се" ;
                           P3 => base_1+"се"
                         } ;
                   Pl => table {
                           P1 => base_1+"совме" ;
                           P2 => base_1+"совте" ;
                           P3 => base_1+"соа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"сев" ;
                              P2 => base_1+"сеше" ;
                              P3 => base_1+"сеше"
                            } ;
                      Pl => table {
                              P1 => base_1+"севме" ;
                              P2 => base_1+"севте" ;
                              P3 => base_1+"сеа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"си" ;
                       Pl => base_1+"сете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"л" ;
                                  GSg Fem => base_1+"ла" ;
                                  GSg Neuter => base_1+"ло" ;
                                  GPl => base_1+"ле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"сел" ;
                                     GSg Fem => base_1+"села" ;
                                     GSg Neuter => base_1+"село" ;
                                     GPl => base_1+"селе"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"сен" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV025"
  } ;

mkV026 : Str -> V ;
mkV026 base =
  case base of {
    base_1+"де" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"дам" ;
                            P2 => base_1+"деш" ;
                            P3 => base_1+"де"
                          } ;
                    Pl => table {
                            P1 => base_1+"деме" ;
                            P2 => base_1+"дете" ;
                            P3 => base_1+"дат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"дов" ;
                           P2 => base_1+"де" ;
                           P3 => base_1+"де"
                         } ;
                   Pl => table {
                           P1 => base_1+"довме" ;
                           P2 => base_1+"довте" ;
                           P3 => base_1+"доа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"дев" ;
                              P2 => base_1+"деше" ;
                              P3 => base_1+"деше"
                            } ;
                      Pl => table {
                              P1 => base_1+"девме" ;
                              P2 => base_1+"девте" ;
                              P3 => base_1+"деа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ди" ;
                       Pl => base_1+"дете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"л" ;
                                  GSg Fem => base_1+"ла" ;
                                  GSg Neuter => base_1+"ло" ;
                                  GPl => base_1+"ле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"дел" ;
                                     GSg Fem => base_1+"дела" ;
                                     GSg Neuter => base_1+"дело" ;
                                     GPl => base_1+"деле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ден" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV026"
  } ;

mkV027 : Str -> V ;
mkV027 base =
  case base of {
    base_1+"те" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"там" ;
                            P2 => base_1+"теш" ;
                            P3 => base_1+"те"
                          } ;
                    Pl => table {
                            P1 => base_1+"теме" ;
                            P2 => base_1+"тете" ;
                            P3 => base_1+"тат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"тов" ;
                           P2 => base_1+"те" ;
                           P3 => base_1+"те"
                         } ;
                   Pl => table {
                           P1 => base_1+"товме" ;
                           P2 => base_1+"товте" ;
                           P3 => base_1+"тоа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"тев" ;
                              P2 => base_1+"теше" ;
                              P3 => base_1+"теше"
                            } ;
                      Pl => table {
                              P1 => base_1+"тевме" ;
                              P2 => base_1+"тевте" ;
                              P3 => base_1+"теа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ти" ;
                       Pl => base_1+"тете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"л" ;
                                  GSg Fem => base_1+"ла" ;
                                  GSg Neuter => base_1+"ло" ;
                                  GPl => base_1+"ле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"тел" ;
                                     GSg Fem => base_1+"тела" ;
                                     GSg Neuter => base_1+"тело" ;
                                     GPl => base_1+"теле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"тен" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV027"
  } ;

mkV028 : Str -> V ;
mkV028 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ав" ;
                           P2 => base_1+"а" ;
                           P3 => base_1+"а"
                         } ;
                   Pl => table {
                           P1 => base_1+"авме" ;
                           P2 => base_1+"авте" ;
                           P3 => base_1+"аа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ал" ;
                                  GSg Fem => base_1+"ала" ;
                                  GSg Neuter => base_1+"ало" ;
                                  GPl => base_1+"але"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ан" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = base_1+"ење" ; --guessed
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV028"
  } ;

mkV029 : Str -> V ;
mkV029 base =
  case base of {
    base_1+"че" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"чам" ;
                            P2 => base_1+"чеш" ;
                            P3 => base_1+"че"
                          } ;
                    Pl => table {
                            P1 => base_1+"чеме" ;
                            P2 => base_1+"чете" ;
                            P3 => base_1+"чат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ков" ;
                           P2 => base_1+"че" ;
                           P3 => base_1+"че"
                         } ;
                   Pl => table {
                           P1 => base_1+"ковме" ;
                           P2 => base_1+"ковте" ;
                           P3 => base_1+"коа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"чев" ;
                              P2 => base_1+"чеше" ;
                              P3 => base_1+"чеше"
                            } ;
                      Pl => table {
                              P1 => base_1+"чевме" ;
                              P2 => base_1+"чевте" ;
                              P3 => base_1+"чеа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"чи" ;
                       Pl => base_1+"чете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"кол" ;
                                  GSg Fem => base_1+"кла" ;
                                  GSg Neuter => base_1+"кло" ;
                                  GPl => base_1+"кле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"чел" ;
                                     GSg Fem => base_1+"чела" ;
                                     GSg Neuter => base_1+"чело" ;
                                     GPl => base_1+"челе"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"чен" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV029"
  } ;

mkV030 : Str -> V ;
mkV030 base =
  case base of {
    base_1+"чее" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"чеам" ;
                            P2 => base_1+"чееш" ;
                            P3 => base_1+"чее"
                          } ;
                    Pl => table {
                            P1 => base_1+"чееме" ;
                            P2 => base_1+"чеете" ;
                            P3 => base_1+"чеат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ков" ;
                           P2 => base_1+"чее" ;
                           P3 => base_1+"чее"
                         } ;
                   Pl => table {
                           P1 => base_1+"ковме" ;
                           P2 => base_1+"ковте" ;
                           P3 => base_1+"коа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"чеев" ;
                              P2 => base_1+"чееше" ;
                              P3 => base_1+"чееше"
                            } ;
                      Pl => table {
                              P1 => base_1+"чеевме" ;
                              P2 => base_1+"чеевте" ;
                              P3 => base_1+"чееа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"чеи" ;
                       Pl => base_1+"чеете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"кол" ;
                                  GSg Fem => base_1+"кла" ;
                                  GSg Neuter => base_1+"кло" ;
                                  GPl => base_1+"кле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"чеел" ;
                                     GSg Fem => base_1+"чеела" ;
                                     GSg Neuter => base_1+"чеело" ;
                                     GPl => base_1+"чееле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"чеен" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV030"
  } ;

mkV031 : Str -> V ;
mkV031 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"иш" ;
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ;
                            P2 => base_1+"ите" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ев" ;
                           P2 => base_1+"е" ;
                           P3 => base_1+"е"
                         } ;
                   Pl => table {
                           P1 => base_1+"евме" ;
                           P2 => base_1+"евте" ;
                           P3 => base_1+"еа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ел" ;
                                  GSg Fem => base_1+"ела" ;
                                  GSg Neuter => base_1+"ело" ;
                                  GPl => base_1+"еле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ен" ; --guessed
                       adverbial = nonExist
                     } ;
        noun_from_verb = base_1+"ење" ; --guessed
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV031"
  } ;

mkV032 : Str -> V ;
mkV032 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"јам" ;
                            P2 => base_1+"иш" ;
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ;
                            P2 => base_1+"ите" ;
                            P3 => base_1+"јат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ив" ; --guessed
                           P2 => base_1+"и" ; --guessed
                           P3 => base_1+"и" --guessed
                         } ;
                   Pl => table {
                           P1 => base_1+"ивме" ; --guessed
                           P2 => base_1+"ивте" ; --guessed
                           P3 => base_1+"ија" --guessed
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ;
                       Pl => base_1+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ил" ; --guessed
                                  GSg Fem => base_1+"ила" ; --guessed
                                  GSg Neuter => base_1+"ило" ; --guessed
                                  GPl => base_1+"иле" --guessed
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ен" ;
                       adverbial = base_1+"ејќи"
                     } ;
        noun_from_verb = base_1+"ење" ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV032"
  } ;

mkV033 : Str -> V ;
mkV033 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ев" ;
                           P2 => base_1+"е" ;
                           P3 => base_1+"е"
                         } ;
                   Pl => table {
                           P1 => base_1+"евме" ;
                           P2 => base_1+"евте" ;
                           P3 => base_1+"еа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ел" ;
                                  GSg Fem => base_1+"ела" ;
                                  GSg Neuter => base_1+"ело" ;
                                  GPl => base_1+"еле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ет" ; --guessed
                       adverbial = base_1+"ејќи" --guessed
                     } ;
        noun_from_verb = base_1+"ење" ; --guessed
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV033"
  } ;

mkV034 : Str -> V ;
mkV034 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ов" ;
                           P2 => base_1+"е" ;
                           P3 => base_1+"е"
                         } ;
                   Pl => table {
                           P1 => base_1+"овме" ;
                           P2 => base_1+"овте" ;
                           P3 => base_1+"оа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ол" ;
                                  GSg Fem => base_1+"ла" ;
                                  GSg Neuter => base_1+"ло" ;
                                  GPl => base_1+"ле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ен" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = base_1+"ење" ; --guessed
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV034"
  } ;

mkV035 : Str -> V ;
mkV035 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ав" ; --guessed
                           P2 => base_1+"а" ; --guessed
                           P3 => base_1+"а" --guessed
                         } ;
                   Pl => table {
                           P1 => base_1+"авме" ; --guessed
                           P2 => base_1+"авте" ; --guessed
                           P3 => base_1+"аја" --guessed
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ;
                       Pl => base_1+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ал" ; --guessed
                                  GSg Fem => base_1+"ала" ; --guessed
                                  GSg Neuter => base_1+"ало" ; --guessed
                                  GPl => base_1+"але" --guessed
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ан" ; --guessed
                       adverbial = base_1+"ејќи"
                     } ;
        noun_from_verb = base_1+"ење" ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV035"
  } ;

mkV036 : Str -> V ;
mkV036 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ав" ;
                           P2 => base_1+"а" ;
                           P3 => base_1+"а"
                         } ;
                   Pl => table {
                           P1 => base_1+"авме" ;
                           P2 => base_1+"авте" ;
                           P3 => base_1+"аа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ал" ;
                                  GSg Fem => base_1+"ала" ;
                                  GSg Neuter => base_1+"ало" ;
                                  GPl => base_1+"але"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ат" ; --guessed
                       adverbial = base_1+"ејќи" --guessed
                     } ;
        noun_from_verb = base_1+"ење" ; --guessed
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV036"
  } ;

mkV037 : Str -> V ;
mkV037 base =
  case base of {
    base_1+"е"+base_2@?+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"е"+base_2+"ам" ;
                            P2 => base_1+"е"+base_2+"еш" ;
                            P3 => base_1+"е"+base_2+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"е"+base_2+"еме" ;
                            P2 => base_1+"е"+base_2+"ете" ;
                            P3 => base_1+"е"+base_2+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+base_2+"ав" ;
                           P2 => base_1+base_2+"а" ;
                           P3 => base_1+base_2+"а"
                         } ;
                   Pl => table {
                           P1 => base_1+base_2+"авме" ;
                           P2 => base_1+base_2+"авте" ;
                           P3 => base_1+base_2+"аа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"е"+base_2+"ев" ;
                              P2 => base_1+"е"+base_2+"еше" ;
                              P3 => base_1+"е"+base_2+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"е"+base_2+"евме" ;
                              P2 => base_1+"е"+base_2+"евте" ;
                              P3 => base_1+"е"+base_2+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"е"+base_2+"и" ;
                       Pl => base_1+"е"+base_2+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+base_2+"ал" ;
                                  GSg Fem => base_1+base_2+"ала" ;
                                  GSg Neuter => base_1+base_2+"ало" ;
                                  GPl => base_1+base_2+"але"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"е"+base_2+"ел" ;
                                     GSg Fem => base_1+"е"+base_2+"ела" ;
                                     GSg Neuter => base_1+"е"+base_2+"ело" ;
                                     GPl => base_1+"е"+base_2+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+base_2+"ан" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV037"
  } ;

mkV038 : Str -> V ;
mkV038 base =
  case base of {
    base_1+"зе" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"зам" ;
                            P2 => base_1+"зеш" ;
                            P3 => base_1+"зе"
                          } ;
                    Pl => table {
                            P1 => base_1+"земе" ;
                            P2 => base_1+"зете" ;
                            P3 => base_1+"зат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"гов" ;
                           P2 => base_1+"зе" ;
                           P3 => base_1+"зе"
                         } ;
                   Pl => table {
                           P1 => base_1+"говме" ;
                           P2 => base_1+"говте" ;
                           P3 => base_1+"гоа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"зев" ;
                              P2 => base_1+"зеше" ;
                              P3 => base_1+"зеше"
                            } ;
                      Pl => table {
                              P1 => base_1+"зевме" ;
                              P2 => base_1+"зевте" ;
                              P3 => base_1+"зеа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"зи" ;
                       Pl => base_1+"зете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"гол" ;
                                  GSg Fem => base_1+"гла" ;
                                  GSg Neuter => base_1+"гло" ;
                                  GPl => base_1+"гле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"зел" ;
                                     GSg Fem => base_1+"зела" ;
                                     GSg Neuter => base_1+"зело" ;
                                     GPl => base_1+"зеле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => nonExist ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV038"
  } ;

mkV039 : Str -> V ;
mkV039 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"јам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"јат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"в" ;
                           P2 => base_1 ;
                           P3 => base_1
                         } ;
                   Pl => table {
                           P1 => base_1+"вме" ;
                           P2 => base_1+"вте" ;
                           P3 => base_1+"ја"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ;
                       Pl => base_1+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"л" ;
                                  GSg Fem => base_1+"ла" ;
                                  GSg Neuter => base_1+"ло" ;
                                  GPl => base_1+"ле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ен" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV039"
  } ;

mkV040 : Str -> V ;
mkV040 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"иш" ;
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ;
                            P2 => base_1+"ите" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ив" ;
                           P2 => base_1+"и" ;
                           P3 => base_1+"и"
                         } ;
                   Pl => table {
                           P1 => base_1+"ивме" ;
                           P2 => base_1+"ивте" ;
                           P3 => base_1+"ија"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ил" ;
                                  GSg Fem => base_1+"ила" ;
                                  GSg Neuter => base_1+"ило" ;
                                  GPl => base_1+"иле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ен" ;
                       adverbial = base_1+"ејќи"
                     } ;
        noun_from_verb = base_1+"ење" ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV040"
  } ;

mkV041 : Str -> V ;
mkV041 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ав" ;
                           P2 => base_1+"а" ;
                           P3 => base_1+"а"
                         } ;
                   Pl => table {
                           P1 => base_1+"авме" ;
                           P2 => base_1+"авте" ;
                           P3 => base_1+"аја"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ;
                       Pl => base_1+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ал" ;
                                  GSg Fem => base_1+"ала" ;
                                  GSg Neuter => base_1+"ало" ;
                                  GPl => base_1+"але"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ан" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV041"
  } ;

mkV042 : Str -> V ;
mkV042 base =
  case base of {
    base_1+"о"+base_2@?+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"о"+base_2+"ам" ;
                            P2 => base_1+"о"+base_2+"еш" ;
                            P3 => base_1+"о"+base_2+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"о"+base_2+"еме" ;
                            P2 => base_1+"о"+base_2+"ете" ;
                            P3 => base_1+"о"+base_2+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+base_2+"ав" ;
                           P2 => base_1+base_2+"а" ;
                           P3 => base_1+base_2+"а"
                         } ;
                   Pl => table {
                           P1 => base_1+base_2+"авме" ;
                           P2 => base_1+base_2+"авте" ;
                           P3 => base_1+base_2+"аа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"о"+base_2+"ев" ;
                              P2 => base_1+"о"+base_2+"еше" ;
                              P3 => base_1+"о"+base_2+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"о"+base_2+"евме" ;
                              P2 => base_1+"о"+base_2+"евте" ;
                              P3 => base_1+"о"+base_2+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"о"+base_2+"и" ;
                       Pl => base_1+"о"+base_2+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+base_2+"ал" ;
                                  GSg Fem => base_1+base_2+"ала" ;
                                  GSg Neuter => base_1+base_2+"ало" ;
                                  GPl => base_1+base_2+"але"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"о"+base_2+"ел" ;
                                     GSg Fem => base_1+"о"+base_2+"ела" ;
                                     GSg Neuter => base_1+"о"+base_2+"ело" ;
                                     GPl => base_1+"о"+base_2+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+base_2+"ан" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV042"
  } ;

mkV043 : Str -> V ;
mkV043 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"јам" ; --guessed
                            P2 => base_1+"иш" ; --guessed
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ; --guessed
                            P2 => base_1+"ите" ; --guessed
                            P3 => base_1+"јат" --guessed
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ав" ; --guessed
                           P2 => base_1+"а" ; --guessed
                           P3 => base_1+"а" --guessed
                         } ;
                   Pl => table {
                           P1 => base_1+"авме" ; --guessed
                           P2 => base_1+"авте" ; --guessed
                           P3 => base_1+"аа" --guessed
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ; --guessed
                              P2 => base_1+"еше" ; --guessed
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ; --guessed
                              P2 => base_1+"евте" ; --guessed
                              P3 => base_1+"еја" --guessed
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ; --guessed
                       Pl => base_1+"јте" --guessed
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ал" ; --guessed
                                  GSg Fem => base_1+"ала" ; --guessed
                                  GSg Neuter => base_1+"ало" ; --guessed
                                  GPl => base_1+"але" --guessed
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ; --guessed
                                     GSg Fem => base_1+"ела" ; --guessed
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле" --guessed
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ен" ; --guessed
                       adverbial = base_1+"ејќи" --guessed
                     } ;
        noun_from_verb = base_1+"ење" ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV043"
  } ;

mkV044 : Str -> V ;
mkV044 base_1 =
  lin V
  { present = \\_ =>
              table {
                Sg => table {
                        P1 => base_1+"м" ;
                        P2 => base_1+"ш" ;
                        P3 => base_1
                      } ;
                Pl => table {
                        P1 => base_1+"ме" ;
                        P2 => base_1+"те" ;
                        P3 => base_1+"ат"
                      }
              } ;
    aorist = table {
               Sg => table {
                       P1 => base_1+"в" ; --guessed
                       P2 => base_1 ; --guessed
                       P3 => base_1 --guessed
                     } ;
               Pl => table {
                       P1 => base_1+"вме" ; --guessed
                       P2 => base_1+"вте" ; --guessed
                       P3 => base_1+"а" --guessed
                     }
             } ;
    imperfect = \\_ =>
                table {
                  Sg => table {
                          P1 => base_1+"в" ;
                          P2 => base_1+"ше" ;
                          P3 => base_1+"ше"
                        } ;
                  Pl => table {
                          P1 => base_1+"вме" ;
                          P2 => base_1+"вте" ;
                          P3 => base_1+"а"
                        }
                } ;
    Imperative = \\_ =>
                 table {
                   Sg => base_1+"ј" ;
                   Pl => base_1+"јте"
                 } ;
    participle = { aorist = \\_ => 
                            table {
                              GSg Masc => base_1+"л" ; --guessed
                              GSg Fem => base_1+"ла" ; --guessed
                              GSg Neuter => base_1+"ло" ; --guessed
                              GPl => base_1+"ле" --guessed
                            } ;
                   imperfect = table {
                                 GSg Masc => base_1+"л" ;
                                 GSg Fem => base_1+"ла" ;
                                 GSg Neuter => base_1+"ло" ;
                                 GPl => base_1+"ле"
                               } ;
                   perfect = \\_ => nonExist ;
                   adjectival = \\_ => base_1+"т" ;
                   adverbial = base_1+"јќи"
                 } ;
    noun_from_verb = base_1+"ње" ;
    isRefl = False
  } ;

mkV045 : Str -> V ;
mkV045 base =
  case base of {
    base_1+"де" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"дам" ;
                            P2 => base_1+"деш" ;
                            P3 => base_1+"де"
                          } ;
                    Pl => table {
                            P1 => base_1+"деме" ;
                            P2 => base_1+"дете" ;
                            P3 => base_1+"дат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"дов" ;
                           P2 => base_1+"де" ;
                           P3 => base_1+"де"
                         } ;
                   Pl => table {
                           P1 => base_1+"довме" ;
                           P2 => base_1+"довте" ;
                           P3 => base_1+"доа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"дев" ;
                              P2 => base_1+"деше" ;
                              P3 => base_1+"деше"
                            } ;
                      Pl => table {
                              P1 => base_1+"девме" ;
                              P2 => base_1+"девте" ;
                              P3 => base_1+"деа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ди" ;
                       Pl => base_1+"дете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"шол" ;
                                  GSg Fem => base_1+"шла" ;
                                  GSg Neuter => base_1+"шло" ;
                                  GPl => base_1+"шле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"дел" ;
                                     GSg Fem => base_1+"дела" ;
                                     GSg Neuter => base_1+"дело" ;
                                     GPl => base_1+"деле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ден" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV045"
  } ;

mkV046 : Str -> V ;
mkV046 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"м" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => nonExist ;
                           P2 => nonExist ;
                           P3 => nonExist
                         } ;
                   Pl => table {
                           P1 => nonExist ;
                           P2 => nonExist ;
                           P3 => nonExist
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ;
                       Pl => base_1+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => nonExist ;
                                  GSg Fem => nonExist ;
                                  GSg Neuter => nonExist ;
                                  GPl => nonExist
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ен" ;
                       adverbial = base_1+"ејќи"
                     } ;
        noun_from_verb = base_1+"ење" ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV046"
  } ;

mkV047 : Str -> V ;
mkV047 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ; --guessed
                            P2 => base_1+"еш" ; --guessed
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ; --guessed
                            P2 => base_1+"ете" ; --guessed
                            P3 => base_1+"ат" --guessed
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ав" ; --guessed
                           P2 => base_1+"а" ; --guessed
                           P3 => base_1+"а" --guessed
                         } ;
                   Pl => table {
                           P1 => base_1+"авме" ; --guessed
                           P2 => base_1+"авте" ; --guessed
                           P3 => base_1+"аа" --guessed
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ; --guessed
                              P2 => base_1+"еше" ; --guessed
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ; --guessed
                              P2 => base_1+"евте" ; --guessed
                              P3 => base_1+"еа" --guessed
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете" --guessed
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ал" ; --guessed
                                  GSg Fem => base_1+"ала" ; --guessed
                                  GSg Neuter => base_1+"ало" ; --guessed
                                  GPl => base_1+"але" --guessed
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ; --guessed
                                     GSg Fem => base_1+"ела" ; --guessed
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле" --guessed
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ет" ; --guessed
                       adverbial = base_1+"ејќи" --guessed
                     } ;
        noun_from_verb = base_1+"ење" ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV047"
  } ;

mkV048 : Str -> V ;
mkV048 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ов" ; --guessed
                           P2 => base_1+"е" ; --guessed
                           P3 => base_1+"е" --guessed
                         } ;
                   Pl => table {
                           P1 => base_1+"овме" ; --guessed
                           P2 => base_1+"овте" ; --guessed
                           P3 => base_1+"оа" --guessed
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ол" ; --guessed
                                  GSg Fem => base_1+"ла" ; --guessed
                                  GSg Neuter => base_1+"ло" ; --guessed
                                  GPl => base_1+"ле" --guessed
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ет" ;
                       adverbial = base_1+"ејќи"
                     } ;
        noun_from_verb = base_1+"ење" ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV048"
  } ;

mkV049 : Str -> V ;
mkV049 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"јам" ;
                            P2 => base_1+"иш" ;
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ;
                            P2 => base_1+"ите" ;
                            P3 => base_1+"јат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"јав" ; --guessed
                           P2 => base_1+"ја" ; --guessed
                           P3 => base_1+"ја" --guessed
                         } ;
                   Pl => table {
                           P1 => base_1+"јавме" ; --guessed
                           P2 => base_1+"јавте" ; --guessed
                           P3 => base_1+"јаа" --guessed
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ;
                       Pl => base_1+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"јал" ; --guessed
                                  GSg Fem => base_1+"јала" ; --guessed
                                  GSg Neuter => base_1+"јало" ; --guessed
                                  GPl => base_1+"јале" --guessed
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"јан" ; --guessed
                       adverbial = base_1+"ејќи"
                     } ;
        noun_from_verb = base_1+"ење" ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV049"
  } ;

mkV050 : Str -> V ;
mkV050 base =
  case base of {
    base_1+base_2@(?+?+?+?) => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+base_2+"м" ;
                            P2 => base_1+base_2+"ш" ;
                            P3 => base_1+base_2
                          } ;
                    Pl => table {
                            P1 => base_1+base_2+"ме" ;
                            P2 => base_1+base_2+"те" ;
                            P3 => base_1+base_2+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+base_2+"в" ;
                           P2 => base_1+base_2 ;
                           P3 => base_1+base_2
                         } ;
                   Pl => table {
                           P1 => base_1+base_2+"вме" ;
                           P2 => base_1+base_2+"вте" ;
                           P3 => base_1+base_2+"а"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+base_2+"в" ;
                              P2 => base_1+base_2+"ше" ;
                              P3 => base_1+base_2+"ше"
                            } ;
                      Pl => table {
                              P1 => base_1+base_2+"вме" ;
                              P2 => base_1+base_2+"вте" ;
                              P3 => base_1+base_2+"а"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+base_2+"ј" ;
                       Pl => base_1+base_2+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+base_2+"л" ;
                                  GSg Fem => base_1+base_2+"ла" ;
                                  GSg Neuter => base_1+base_2+"ло" ;
                                  GPl => base_1+base_2+"ле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+base_2+"л" ;
                                     GSg Fem => base_1+base_2+"ла" ;
                                     GSg Neuter => base_1+base_2+"ло" ;
                                     GPl => base_1+base_2+"ле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+base_2+"н" ;
                       adverbial = base_1+"и"+base_2+"јќи"
                     } ;
        noun_from_verb = base_1+"и"+base_2+"ње" ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV050"
  } ;

mkV051 : Str -> V ;
mkV051 base_1 =
  lin V
  { present = \\_ =>
              table {
                Sg => table {
                        P1 => base_1+"м" ;
                        P2 => base_1+"ш" ;
                        P3 => base_1
                      } ;
                Pl => table {
                        P1 => base_1+"ме" ;
                        P2 => base_1+"те" ;
                        P3 => base_1+"ат"
                      }
              } ;
    aorist = table {
               Sg => table {
                       P1 => base_1+"в" ;
                       P2 => base_1 ;
                       P3 => base_1
                     } ;
               Pl => table {
                       P1 => base_1+"вме" ;
                       P2 => base_1+"вте" ;
                       P3 => base_1+"а"
                     }
             } ;
    imperfect = \\_ =>
                table {
                  Sg => table {
                          P1 => base_1+"в" ;
                          P2 => base_1+"ше" ;
                          P3 => base_1+"ше"
                        } ;
                  Pl => table {
                          P1 => base_1+"вме" ;
                          P2 => base_1+"вте" ;
                          P3 => base_1+"а"
                        }
                } ;
    Imperative = \\_ =>
                 table {
                   Sg => base_1+"ј" ;
                   Pl => base_1+"јте"
                 } ;
    participle = { aorist = \\_ => 
                            table {
                              GSg Masc => base_1+"л" ;
                              GSg Fem => base_1+"ла" ;
                              GSg Neuter => base_1+"ло" ;
                              GPl => base_1+"ле"
                            } ;
                   imperfect = table {
                                 GSg Masc => base_1+"л" ;
                                 GSg Fem => base_1+"ла" ;
                                 GSg Neuter => base_1+"ло" ;
                                 GPl => base_1+"ле"
                               } ;
                   perfect = \\_ => nonExist ;
                   adjectival = \\_ => base_1+"т" ; --guessed
                   adverbial = base_1+"јќи"
                 } ;
    noun_from_verb = base_1+"ње" ;
    isRefl = False
  } ;

mkV052 : Str -> V ;
mkV052 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"иш" ;
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ;
                            P2 => base_1+"ите" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ав" ;
                           P2 => base_1+"а" ;
                           P3 => base_1+"а"
                         } ;
                   Pl => table {
                           P1 => base_1+"авме" ;
                           P2 => base_1+"авте" ;
                           P3 => base_1+"аа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ал" ;
                                  GSg Fem => base_1+"ала" ;
                                  GSg Neuter => base_1+"ало" ;
                                  GPl => base_1+"але"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ан" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV052"
  } ;

mkV053 : Str -> V ;
mkV053 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ; --guessed
                            P2 => base_1+"иш" ; --guessed
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ; --guessed
                            P2 => base_1+"ите" ; --guessed
                            P3 => base_1+"ат" --guessed
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ев" ; --guessed
                           P2 => base_1+"е" ; --guessed
                           P3 => base_1+"е"
                         } ;
                   Pl => table {
                           P1 => base_1+"евме" ; --guessed
                           P2 => base_1+"евте" ; --guessed
                           P3 => base_1+"еа" --guessed
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ; --guessed
                              P2 => base_1+"еше" ; --guessed
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ; --guessed
                              P2 => base_1+"евте" ; --guessed
                              P3 => base_1+"еа" --guessed
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ; --guessed
                       Pl => base_1+"ете" --guessed
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ел" ; --guessed
                                  GSg Fem => base_1+"ела" ; --guessed
                                  GSg Neuter => base_1+"ело" ;
                                  GPl => base_1+"еле" --guessed
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ; --guessed
                                     GSg Fem => base_1+"ела" ; --guessed
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле" --guessed
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ен" ; --guessed
                       adverbial = base_1+"ејќи" --guessed
                     } ;
        noun_from_verb = base_1+"ење" ; --guessed
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV053"
  } ;

mkV054 : Str -> V ;
mkV054 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"иш" ;
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ;
                            P2 => base_1+"ите" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ев" ;
                           P2 => base_1+"е" ;
                           P3 => base_1+"е"
                         } ;
                   Pl => table {
                           P1 => base_1+"евме" ;
                           P2 => base_1+"евте" ;
                           P3 => base_1+"еа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ел" ;
                                  GSg Fem => base_1+"ела" ;
                                  GSg Neuter => base_1+"ело" ;
                                  GPl => base_1+"еле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ен" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV054"
  } ;

mkV055 : Str -> V ;
mkV055 base_1 =
  lin V
  { present = \\_ =>
              table {
                Sg => table {
                        P1 => base_1+"м" ;
                        P2 => base_1+"ш" ;
                        P3 => base_1
                      } ;
                Pl => table {
                        P1 => base_1+"ме" ;
                        P2 => base_1+"те" ;
                        P3 => base_1+"ат"
                      }
              } ;
    aorist = table {
               Sg => table {
                       P1 => base_1+"в" ;
                       P2 => base_1 ;
                       P3 => base_1
                     } ;
               Pl => table {
                       P1 => base_1+"вме" ;
                       P2 => base_1+"вте" ;
                       P3 => base_1+"а"
                     }
             } ;
    imperfect = \\_ =>
                table {
                  Sg => table {
                          P1 => base_1+"в" ;
                          P2 => base_1+"ше" ;
                          P3 => base_1+"ше"
                        } ;
                  Pl => table {
                          P1 => base_1+"вме" ;
                          P2 => base_1+"вте" ;
                          P3 => base_1+"а"
                        }
                } ;
    Imperative = \\_ =>
                 table {
                   Sg => base_1+"ј" ;
                   Pl => base_1+"јте"
                 } ;
    participle = { aorist = \\_ => 
                            table {
                              GSg Masc => base_1+"л" ;
                              GSg Fem => base_1+"ла" ;
                              GSg Neuter => base_1+"ло" ;
                              GPl => base_1+"ле"
                            } ;
                   imperfect = table {
                                 GSg Masc => base_1+"л" ;
                                 GSg Fem => base_1+"ла" ;
                                 GSg Neuter => base_1+"ло" ;
                                 GPl => base_1+"ле"
                               } ;
                   perfect = \\_ => nonExist ;
                   adjectival = \\_ => base_1+"т" ;
                   adverbial = nonExist
                 } ;
    noun_from_verb = base_1+"ње" ; --guessed
    isRefl = False
  } ;

mkV056 : Str -> V ;
mkV056 base =
  case base of {
    base_1+"јде" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"јдам" ;
                            P2 => base_1+"јдеш" ;
                            P3 => base_1+"јде"
                          } ;
                    Pl => table {
                            P1 => base_1+"јдеме" ;
                            P2 => base_1+"јдете" ;
                            P3 => base_1+"јдат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"јдов" ;
                           P2 => base_1+"јде" ;
                           P3 => base_1+"јде"
                         } ;
                   Pl => table {
                           P1 => base_1+"јдовме" ;
                           P2 => base_1+"јдовте" ;
                           P3 => base_1+"јдоа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"јдев" ;
                              P2 => base_1+"јдеше" ;
                              P3 => base_1+"јдеше"
                            } ;
                      Pl => table {
                              P1 => base_1+"јдевме" ;
                              P2 => base_1+"јдевте" ;
                              P3 => base_1+"јдеа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"јди" ;
                       Pl => base_1+"јдете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"шол" ;
                                  GSg Fem => base_1+"шла" ;
                                  GSg Neuter => base_1+"шло" ;
                                  GPl => base_1+"шле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"јдел" ;
                                     GSg Fem => base_1+"јдела" ;
                                     GSg Neuter => base_1+"јдело" ;
                                     GPl => base_1+"јделе"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"јден" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV056"
  } ;

mkV057 : Str -> V ;
mkV057 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"иш" ;
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ;
                            P2 => base_1+"ите" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ив" ; --guessed
                           P2 => base_1+"и" ; --guessed
                           P3 => base_1+"и" --guessed
                         } ;
                   Pl => table {
                           P1 => base_1+"ивме" ; --guessed
                           P2 => base_1+"ивте" ; --guessed
                           P3 => base_1+"ија" --guessed
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ;
                       Pl => base_1+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ил" ; --guessed
                                  GSg Fem => base_1+"ила" ; --guessed
                                  GSg Neuter => base_1+"ило" ; --guessed
                                  GPl => base_1+"иле" --guessed
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ен" ;
                       adverbial = base_1+"ејќи"
                     } ;
        noun_from_verb = base_1+"ење" ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV057"
  } ;

mkV058 : Str -> V ;
mkV058 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"иш" ;
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ;
                            P2 => base_1+"ите" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ав" ;
                           P2 => base_1+"а" ;
                           P3 => base_1+"а"
                         } ;
                   Pl => table {
                           P1 => base_1+"авме" ;
                           P2 => base_1+"авте" ;
                           P3 => base_1+"аа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ал" ;
                                  GSg Fem => base_1+"ала" ;
                                  GSg Neuter => base_1+"ало" ;
                                  GPl => base_1+"але"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => nonExist ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV058"
  } ;

mkV059 : Str -> V ;
mkV059 base =
  case base of {
    base_1+"ме" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"мам" ;
                            P2 => base_1+"меш" ;
                            P3 => base_1+"ме"
                          } ;
                    Pl => table {
                            P1 => base_1+"меме" ;
                            P2 => base_1+"мете" ;
                            P3 => base_1+"мат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"дов" ;
                           P2 => base_1+"де" ;
                           P3 => base_1+"де"
                         } ;
                   Pl => table {
                           P1 => base_1+"довме" ;
                           P2 => base_1+"довте" ;
                           P3 => base_1+"доа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"мев" ;
                              P2 => base_1+"меше" ;
                              P3 => base_1+"меше"
                            } ;
                      Pl => table {
                              P1 => base_1+"мевме" ;
                              P2 => base_1+"мевте" ;
                              P3 => base_1+"меа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ми" ;
                       Pl => base_1+"мете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"л" ;
                                  GSg Fem => base_1+"ла" ;
                                  GSg Neuter => base_1+"ло" ;
                                  GPl => base_1+"ле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"мел" ;
                                     GSg Fem => base_1+"мела" ;
                                     GSg Neuter => base_1+"мело" ;
                                     GPl => base_1+"меле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"мен" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV059"
  } ;

mkV060 : Str -> V ;
mkV060 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"јав" ;
                           P2 => base_1+"ја" ;
                           P3 => base_1+"ја"
                         } ;
                   Pl => table {
                           P1 => base_1+"јавме" ;
                           P2 => base_1+"јавте" ;
                           P3 => base_1+"јаа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ;
                       Pl => base_1+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"јал" ;
                                  GSg Fem => base_1+"јала" ;
                                  GSg Neuter => base_1+"јало" ;
                                  GPl => base_1+"јале"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"јан" ; --guessed
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV060"
  } ;

mkV061 : Str -> V ;
mkV061 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ; --guessed
                            P2 => base_1+"иш" ; --guessed
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ; --guessed
                            P2 => base_1+"ите" ; --guessed
                            P3 => base_1+"ат" --guessed
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ив" ; --guessed
                           P2 => base_1+"и" ; --guessed
                           P3 => base_1+"и"
                         } ;
                   Pl => table {
                           P1 => base_1+"ивме" ; --guessed
                           P2 => base_1+"ивте" ; --guessed
                           P3 => base_1+"ија" --guessed
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ; --guessed
                              P2 => base_1+"еше" ; --guessed
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ; --guessed
                              P2 => base_1+"евте" ; --guessed
                              P3 => base_1+"еја" --guessed
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ; --guessed
                       Pl => base_1+"јте" --guessed
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ил" ; --guessed
                                  GSg Fem => base_1+"ила" ; --guessed
                                  GSg Neuter => base_1+"ило" ;
                                  GPl => base_1+"иле" --guessed
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ; --guessed
                                     GSg Fem => base_1+"ела" ; --guessed
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле" --guessed
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ен" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV061"
  } ;

mkV062 : Str -> V ;
mkV062 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"иш" ;
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ;
                            P2 => base_1+"ите" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ов" ;
                           P2 => base_1+"е" ;
                           P3 => base_1+"е"
                         } ;
                   Pl => table {
                           P1 => base_1+"овме" ;
                           P2 => base_1+"овте" ;
                           P3 => base_1+"оа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ел" ;
                                  GSg Fem => base_1+"ела" ;
                                  GSg Neuter => base_1+"ело" ;
                                  GPl => base_1+"еле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => nonExist ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV062"
  } ;

mkV063 : Str -> V ;
mkV063 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"иш" ;
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ;
                            P2 => base_1+"ите" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ив" ;
                           P2 => base_1+"и" ;
                           P3 => base_1+"и"
                         } ;
                   Pl => table {
                           P1 => base_1+"ивме" ;
                           P2 => base_1+"ивте" ;
                           P3 => base_1+"ија"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ;
                       Pl => base_1+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ил" ;
                                  GSg Fem => base_1+"ила" ;
                                  GSg Neuter => base_1+"ило" ;
                                  GPl => base_1+"иле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ен" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV063"
  } ;

mkV064 : Str -> V ;
mkV064 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ав" ;
                           P2 => base_1+"а" ;
                           P3 => base_1+"а"
                         } ;
                   Pl => table {
                           P1 => base_1+"авме" ;
                           P2 => base_1+"авте" ;
                           P3 => base_1+"аја"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ;
                       Pl => base_1+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ал" ;
                                  GSg Fem => base_1+"ала" ;
                                  GSg Neuter => base_1+"ало" ;
                                  GPl => base_1+"але"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => nonExist ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV064"
  } ;

mkV065 : Str -> V ;
mkV065 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"в" ;
                           P2 => base_1 ;
                           P3 => base_1
                         } ;
                   Pl => table {
                           P1 => base_1+"вме" ;
                           P2 => base_1+"вте" ;
                           P3 => base_1+"а"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ;
                       Pl => base_1+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"л" ;
                                  GSg Fem => base_1+"ла" ;
                                  GSg Neuter => base_1+"ло" ;
                                  GPl => base_1+"ле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ен" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV065"
  } ;

mkV066 : Str -> V ;
mkV066 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ав" ;
                           P2 => base_1+"а" ;
                           P3 => base_1+"а"
                         } ;
                   Pl => table {
                           P1 => base_1+"авме" ;
                           P2 => base_1+"авте" ;
                           P3 => base_1+"аа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ал" ;
                                  GSg Fem => base_1+"ала" ;
                                  GSg Neuter => base_1+"ало" ;
                                  GPl => base_1+"але"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ат" ;
                       adverbial = base_1+"ејќи"
                     } ;
        noun_from_verb = base_1+"ење" ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV066"
  } ;

mkV067 : Str -> V ;
mkV067 base =
  case base of {
    base_1+"те"+base_2@?+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"те"+base_2+"ам" ;
                            P2 => base_1+"те"+base_2+"еш" ;
                            P3 => base_1+"те"+base_2+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"те"+base_2+"еме" ;
                            P2 => base_1+"те"+base_2+"ете" ;
                            P3 => base_1+"те"+base_2+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+base_2+"ав" ;
                           P2 => base_1+base_2+"а" ;
                           P3 => base_1+base_2+"а"
                         } ;
                   Pl => table {
                           P1 => base_1+base_2+"авме" ;
                           P2 => base_1+base_2+"авте" ;
                           P3 => base_1+base_2+"аа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"те"+base_2+"ев" ;
                              P2 => base_1+"те"+base_2+"еше" ;
                              P3 => base_1+"те"+base_2+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"те"+base_2+"евме" ;
                              P2 => base_1+"те"+base_2+"евте" ;
                              P3 => base_1+"те"+base_2+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"те"+base_2+"и" ;
                       Pl => base_1+"те"+base_2+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+base_2+"ал" ;
                                  GSg Fem => base_1+base_2+"ала" ;
                                  GSg Neuter => base_1+base_2+"ало" ;
                                  GPl => base_1+base_2+"але"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"те"+base_2+"ел" ;
                                     GSg Fem => base_1+"те"+base_2+"ела" ;
                                     GSg Neuter => base_1+"те"+base_2+"ело" ;
                                     GPl => base_1+"те"+base_2+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+base_2+"ан" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV067"
  } ;

mkV068 : Str -> V ;
mkV068 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"јам" ;
                            P2 => base_1+"иш" ;
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ;
                            P2 => base_1+"ите" ;
                            P3 => base_1+"јат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"јав" ;
                           P2 => base_1+"ја" ;
                           P3 => base_1+"ја"
                         } ;
                   Pl => table {
                           P1 => base_1+"јавме" ;
                           P2 => base_1+"јавте" ;
                           P3 => base_1+"јаа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ;
                       Pl => base_1+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"јал" ;
                                  GSg Fem => base_1+"јала" ;
                                  GSg Neuter => base_1+"јало" ;
                                  GPl => base_1+"јале"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"јан" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV068"
  } ;

mkV069 : Str -> V ;
mkV069 base =
  case base of {
    base_1+"јде" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"јдам" ;
                            P2 => base_1+"јдеш" ;
                            P3 => base_1+"јде"
                          } ;
                    Pl => table {
                            P1 => base_1+"јдеме" ;
                            P2 => base_1+"јдете" ;
                            P3 => base_1+"јдат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"јдов" ;
                           P2 => base_1+"јде" ;
                           P3 => base_1+"јде"
                         } ;
                   Pl => table {
                           P1 => base_1+"јдовме" ;
                           P2 => base_1+"јдовте" ;
                           P3 => base_1+"јдоа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"јдев" ;
                              P2 => base_1+"јдеше" ;
                              P3 => base_1+"јдеше"
                            } ;
                      Pl => table {
                              P1 => base_1+"јдевме" ;
                              P2 => base_1+"јдевте" ;
                              P3 => base_1+"јдеа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"јди" ;
                       Pl => base_1+"јдете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"шол" ;
                                  GSg Fem => base_1+"шла" ;
                                  GSg Neuter => base_1+"шло" ;
                                  GPl => base_1+"шле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"јдел" ;
                                     GSg Fem => base_1+"јдела" ;
                                     GSg Neuter => base_1+"јдело" ;
                                     GPl => base_1+"јделе"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => nonExist ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV069"
  } ;

mkV070 : Str -> V ;
mkV070 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"иш" ;
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ;
                            P2 => base_1+"ите" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ев" ;
                           P2 => base_1+"е" ;
                           P3 => base_1+"е"
                         } ;
                   Pl => table {
                           P1 => base_1+"евме" ;
                           P2 => base_1+"евте" ;
                           P3 => base_1+"еа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ел" ;
                                  GSg Fem => base_1+"ела" ;
                                  GSg Neuter => base_1+"ело" ;
                                  GPl => base_1+"еле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ет" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV070"
  } ;

mkV071 : Str -> V ;
mkV071 base =
  case base of {
    base_1+"че" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"чам" ;
                            P2 => base_1+"чеш" ;
                            P3 => base_1+"че"
                          } ;
                    Pl => table {
                            P1 => base_1+"чеме" ;
                            P2 => base_1+"чете" ;
                            P3 => base_1+"чат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"кав" ;
                           P2 => base_1+"ка" ;
                           P3 => base_1+"ка"
                         } ;
                   Pl => table {
                           P1 => base_1+"кавме" ;
                           P2 => base_1+"кавте" ;
                           P3 => base_1+"каа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"чев" ;
                              P2 => base_1+"чеше" ;
                              P3 => base_1+"чеше"
                            } ;
                      Pl => table {
                              P1 => base_1+"чевме" ;
                              P2 => base_1+"чевте" ;
                              P3 => base_1+"чеа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"чи" ;
                       Pl => base_1+"чете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"кал" ;
                                  GSg Fem => base_1+"кала" ;
                                  GSg Neuter => base_1+"кало" ;
                                  GPl => base_1+"кале"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"чел" ;
                                     GSg Fem => base_1+"чела" ;
                                     GSg Neuter => base_1+"чело" ;
                                     GPl => base_1+"челе"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"кан" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV071"
  } ;

mkV072 : Str -> V ;
mkV072 base =
  case base of {
    base_1+"че" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"чам" ;
                            P2 => base_1+"чеш" ;
                            P3 => base_1+"че"
                          } ;
                    Pl => table {
                            P1 => base_1+"чеме" ;
                            P2 => base_1+"чете" ;
                            P3 => base_1+"чат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"кав" ;
                           P2 => base_1+"ка" ;
                           P3 => base_1+"ка"
                         } ;
                   Pl => table {
                           P1 => base_1+"кавме" ;
                           P2 => base_1+"кавте" ;
                           P3 => base_1+"каа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"чев" ;
                              P2 => base_1+"чеше" ;
                              P3 => base_1+"чеше"
                            } ;
                      Pl => table {
                              P1 => base_1+"чевме" ;
                              P2 => base_1+"чевте" ;
                              P3 => base_1+"чеа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"чи" ;
                       Pl => base_1+"чете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"кал" ;
                                  GSg Fem => base_1+"кала" ;
                                  GSg Neuter => base_1+"кало" ;
                                  GPl => base_1+"кале"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"чел" ;
                                     GSg Fem => base_1+"чела" ;
                                     GSg Neuter => base_1+"чело" ;
                                     GPl => base_1+"челе"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => nonExist ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV072"
  } ;

mkV073 : Str -> V ;
mkV073 base =
  case base of {
    base_1+"ие" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ијам" ;
                            P2 => base_1+"иеш" ;
                            P3 => base_1+"ие"
                          } ;
                    Pl => table {
                            P1 => base_1+"иеме" ;
                            P2 => base_1+"иете" ;
                            P3 => base_1+"ијат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ав" ;
                           P2 => base_1+"а" ;
                           P3 => base_1+"а"
                         } ;
                   Pl => table {
                           P1 => base_1+"авме" ;
                           P2 => base_1+"авте" ;
                           P3 => base_1+"аа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"иев" ;
                              P2 => base_1+"иеше" ;
                              P3 => base_1+"иеше"
                            } ;
                      Pl => table {
                              P1 => base_1+"иевме" ;
                              P2 => base_1+"иевте" ;
                              P3 => base_1+"иеја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"иј" ;
                       Pl => base_1+"ијте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ал" ;
                                  GSg Fem => base_1+"ала" ;
                                  GSg Neuter => base_1+"ало" ;
                                  GPl => base_1+"але"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"иел" ;
                                     GSg Fem => base_1+"иела" ;
                                     GSg Neuter => base_1+"иело" ;
                                     GPl => base_1+"иеле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ан" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV073"
  } ;

mkV074 : Str -> V ;
mkV074 base =
  case base of {
    base_1+"ие" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ијам" ;
                            P2 => base_1+"иеш" ;
                            P3 => base_1+"ие"
                          } ;
                    Pl => table {
                            P1 => base_1+"иеме" ;
                            P2 => base_1+"иете" ;
                            P3 => base_1+"ијат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ав" ;
                           P2 => base_1+"а" ;
                           P3 => base_1+"а"
                         } ;
                   Pl => table {
                           P1 => base_1+"авме" ;
                           P2 => base_1+"авте" ;
                           P3 => base_1+"аа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"иев" ;
                              P2 => base_1+"иеше" ;
                              P3 => base_1+"иеше"
                            } ;
                      Pl => table {
                              P1 => base_1+"иевме" ;
                              P2 => base_1+"иевте" ;
                              P3 => base_1+"иеја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"иј" ;
                       Pl => base_1+"ијте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ал" ;
                                  GSg Fem => base_1+"ала" ;
                                  GSg Neuter => base_1+"ало" ;
                                  GPl => base_1+"але"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"иел" ;
                                     GSg Fem => base_1+"иела" ;
                                     GSg Neuter => base_1+"иело" ;
                                     GPl => base_1+"иеле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => nonExist ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV074"
  } ;

mkV075 : Str -> V ;
mkV075 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ => 
                  table {
                    Sg => table {
                            P1 => nonExist ;
                            P2 => nonExist ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => nonExist ;
                            P2 => nonExist ;
                            P3 => nonExist
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => nonExist ;
                           P2 => nonExist ;
                           P3 => base_1+"а"
                         } ;
                   Pl => table {
                           P1 => nonExist ;
                           P2 => nonExist ;
                           P3 => nonExist
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => nonExist ;
                              P2 => nonExist ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => nonExist ;
                              P2 => nonExist ;
                              P3 => nonExist
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => nonExist
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => nonExist ;
                                  GSg Fem => nonExist ;
                                  GSg Neuter => base_1+"ало" ;
                                  GPl => nonExist
                                } ;
                       imperfect = table {
                                     GSg Masc => nonExist ;
                                     GSg Fem => nonExist ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => nonExist
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ат" ; --guessed
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV075"
  } ;

mkV076 : Str -> V ;
mkV076 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"јав" ;
                           P2 => base_1+"ја" ;
                           P3 => base_1+"ја"
                         } ;
                   Pl => table {
                           P1 => base_1+"јавме" ;
                           P2 => base_1+"јавте" ;
                           P3 => base_1+"јаа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ;
                       Pl => base_1+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"јал" ;
                                  GSg Fem => base_1+"јала" ;
                                  GSg Neuter => base_1+"јало" ;
                                  GPl => base_1+"јале"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"јан" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV076"
  } ;

mkV077 : Str -> V ;
mkV077 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"в" ;
                           P2 => base_1 ;
                           P3 => base_1
                         } ;
                   Pl => table {
                           P1 => base_1+"вме" ;
                           P2 => base_1+"вте" ;
                           P3 => base_1+"а"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ;
                       Pl => base_1+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"л" ;
                                  GSg Fem => base_1+"ла" ;
                                  GSg Neuter => base_1+"ло" ;
                                  GPl => base_1+"ле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => nonExist ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV077"
  } ;

mkV078 : Str -> V ;
mkV078 base =
  case base of {
    base_1+"ие" => lin V
      { present = \\_ => 
                  table {
                    Sg => table {
                            P1 => base_1+"ијам" ;
                            P2 => base_1+"иеш" ;
                            P3 => base_1+"ие"
                          } ;
                    Pl => table {
                            P1 => base_1+"иеме" ;
                            P2 => base_1+"иете" ;
                            P3 => base_1+"ијат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ев" ;
                           P2 => base_1+"е" ;
                           P3 => base_1+"е"
                         } ;
                   Pl => table {
                           P1 => base_1+"евме" ;
                           P2 => base_1+"евте" ;
                           P3 => base_1+"еа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"иев" ;
                              P2 => base_1+"иеше" ;
                              P3 => base_1+"иеше"
                            } ;
                      Pl => table {
                              P1 => base_1+"иевме" ;
                              P2 => base_1+"иевте" ;
                              P3 => base_1+"иеја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"иј" ;
                       Pl => base_1+"ијте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ел" ;
                                  GSg Fem => base_1+"ела" ;
                                  GSg Neuter => base_1+"ело" ;
                                  GPl => base_1+"еле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"иел" ;
                                     GSg Fem => base_1+"иела" ;
                                     GSg Neuter => base_1+"иело" ;
                                     GPl => base_1+"иеле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"иен" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV078"
  } ;

mkV079 : Str -> V ;
mkV079 base =
  case base of {
    base_1+base_2@?+"ле" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+base_2+"лам" ;
                            P2 => base_1+base_2+"леш" ;
                            P3 => base_1+base_2+"ле"
                          } ;
                    Pl => table {
                            P1 => base_1+base_2+"леме" ;
                            P2 => base_1+base_2+"лете" ;
                            P3 => base_1+base_2+"лат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"л"+base_2+"в" ;
                           P2 => base_1+"л"+base_2 ;
                           P3 => base_1+"л"+base_2
                         } ;
                   Pl => table {
                           P1 => base_1+"л"+base_2+"вме" ;
                           P2 => base_1+"л"+base_2+"вте" ;
                           P3 => base_1+"л"+base_2+"а"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+base_2+"лев" ;
                              P2 => base_1+base_2+"леше" ;
                              P3 => base_1+base_2+"леше"
                            } ;
                      Pl => table {
                              P1 => base_1+base_2+"левме" ;
                              P2 => base_1+base_2+"левте" ;
                              P3 => base_1+base_2+"леа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+base_2+"ли" ;
                       Pl => base_1+base_2+"лете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"л"+base_2+"л" ;
                                  GSg Fem => base_1+"л"+base_2+"ла" ;
                                  GSg Neuter => base_1+"л"+base_2+"ло" ;
                                  GPl => base_1+"л"+base_2+"ле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+base_2+"лел" ;
                                     GSg Fem => base_1+base_2+"лела" ;
                                     GSg Neuter => base_1+base_2+"лело" ;
                                     GPl => base_1+base_2+"леле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+base_2+"лен" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV079"
  } ;

mkV080 : Str -> V ;
mkV080 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"јав" ;
                           P2 => base_1+"ја" ;
                           P3 => base_1+"ја"
                         } ;
                   Pl => table {
                           P1 => base_1+"јавме" ;
                           P2 => base_1+"јавте" ;
                           P3 => base_1+"јаа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ;
                       Pl => base_1+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"јал" ;
                                  GSg Fem => base_1+"јала" ;
                                  GSg Neuter => base_1+"јало" ;
                                  GPl => base_1+"јале"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ен" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV080"
  } ;

mkV081 : Str -> V ;
mkV081 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => nonExist ;
                            P2 => nonExist ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => nonExist ;
                            P2 => nonExist ;
                            P3 => nonExist
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => nonExist ;
                           P2 => nonExist ;
                           P3 => base_1+"а"
                         } ;
                   Pl => table {
                           P1 => nonExist ;
                           P2 => nonExist ;
                           P3 => nonExist
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => nonExist ;
                              P2 => nonExist ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => nonExist ;
                              P2 => nonExist ;
                              P3 => nonExist
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => nonExist
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => nonExist ;
                                  GSg Fem => nonExist ;
                                  GSg Neuter => base_1+"ало" ;
                                  GPl => nonExist
                                } ;
                       imperfect = table {
                                     GSg Masc => nonExist ;
                                     GSg Fem => nonExist ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => nonExist
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ат" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV081"
  } ;

mkV082 : Str -> V ;
mkV082 base =
  case base of {
    base_1+"же" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"жам" ;
                            P2 => base_1+"жеш" ;
                            P3 => base_1+"же"
                          } ;
                    Pl => table {
                            P1 => base_1+"жеме" ;
                            P2 => base_1+"жете" ;
                            P3 => base_1+"жат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"гов" ;
                           P2 => base_1+"же" ;
                           P3 => base_1+"же"
                         } ;
                   Pl => table {
                           P1 => base_1+"говме" ;
                           P2 => base_1+"говте" ;
                           P3 => base_1+"гоа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"жев" ;
                              P2 => base_1+"жеше" ;
                              P3 => base_1+"жеше"
                            } ;
                      Pl => table {
                              P1 => base_1+"жевме" ;
                              P2 => base_1+"жевте" ;
                              P3 => base_1+"жеа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"жи" ;
                       Pl => base_1+"жете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"гол" ;
                                  GSg Fem => base_1+"гла" ;
                                  GSg Neuter => base_1+"гло" ;
                                  GPl => base_1+"гле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"жел" ;
                                     GSg Fem => base_1+"жела" ;
                                     GSg Neuter => base_1+"жело" ;
                                     GPl => base_1+"желе"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"жен" ; --guessed
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV082"
  } ;

mkV083 : Str -> V ;
mkV083 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"јам" ; --guessed
                            P2 => base_1+"иш" ; --guessed
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ; --guessed
                            P2 => base_1+"ите" ; --guessed
                            P3 => base_1+"јат" --guessed
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ив" ; --guessed
                           P2 => base_1+"и" ; --guessed
                           P3 => base_1+"и"
                         } ;
                   Pl => table {
                           P1 => base_1+"ивме" ; --guessed
                           P2 => base_1+"ивте" ; --guessed
                           P3 => base_1+"ија" --guessed
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ; --guessed
                              P2 => base_1+"еше" ; --guessed
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ; --guessed
                              P2 => base_1+"евте" ; --guessed
                              P3 => base_1+"еја" --guessed
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ; --guessed
                       Pl => base_1+"јте" --guessed
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ил" ; --guessed
                                  GSg Fem => base_1+"ила" ; --guessed
                                  GSg Neuter => base_1+"ило" ;
                                  GPl => base_1+"иле" --guessed
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ; --guessed
                                     GSg Fem => base_1+"ела" ; --guessed
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле" --guessed
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => nonExist ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV083"
  } ;

mkV084 : Str -> V ;
mkV084 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"јам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"јат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"в" ;
                           P2 => base_1 ;
                           P3 => base_1
                         } ;
                   Pl => table {
                           P1 => base_1+"вме" ;
                           P2 => base_1+"вте" ;
                           P3 => base_1+"ја"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ;
                       Pl => base_1+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"л" ;
                                  GSg Fem => base_1+"ла" ;
                                  GSg Neuter => base_1+"ло" ;
                                  GPl => base_1+"ле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => nonExist ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV084"
  } ;

mkV085 : Str -> V ;
mkV085 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"јам" ;
                            P2 => base_1+"иш" ;
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ;
                            P2 => base_1+"ите" ;
                            P3 => base_1+"јат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ив" ;
                           P2 => base_1+"и" ;
                           P3 => base_1+"и"
                         } ;
                   Pl => table {
                           P1 => base_1+"ивме" ;
                           P2 => base_1+"ивте" ;
                           P3 => base_1+"ија"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ;
                       Pl => base_1+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ил" ;
                                  GSg Fem => base_1+"ила" ;
                                  GSg Neuter => base_1+"ило" ;
                                  GPl => base_1+"иле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => nonExist ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV085"
  } ;

mkV086 : Str -> V ;
mkV086 base =
  case base of {
    base_1+"же" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"жам" ;
                            P2 => base_1+"жеш" ;
                            P3 => base_1+"же"
                          } ;
                    Pl => table {
                            P1 => base_1+"жеме" ;
                            P2 => base_1+"жете" ;
                            P3 => base_1+"жат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"гов" ;
                           P2 => base_1+"же" ;
                           P3 => base_1+"же"
                         } ;
                   Pl => table {
                           P1 => base_1+"говме" ;
                           P2 => base_1+"говте" ;
                           P3 => base_1+"гоа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"жев" ;
                              P2 => base_1+"жеше" ;
                              P3 => base_1+"жеше"
                            } ;
                      Pl => table {
                              P1 => base_1+"жевме" ;
                              P2 => base_1+"жевте" ;
                              P3 => base_1+"жеа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"жи" ;
                       Pl => base_1+"жете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"гол" ;
                                  GSg Fem => base_1+"гла" ;
                                  GSg Neuter => base_1+"гло" ;
                                  GPl => base_1+"гле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"жел" ;
                                     GSg Fem => base_1+"жела" ;
                                     GSg Neuter => base_1+"жело" ;
                                     GPl => base_1+"желе"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"жен" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV086"
  } ;

mkV087 : Str -> V ;
mkV087 base =
  case base of {
    base_1+"де" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"дам" ;
                            P2 => base_1+"деш" ;
                            P3 => base_1+"де"
                          } ;
                    Pl => table {
                            P1 => base_1+"деме" ;
                            P2 => base_1+"дете" ;
                            P3 => base_1+"дат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"дов" ;
                           P2 => base_1+"де" ;
                           P3 => base_1+"де"
                         } ;
                   Pl => table {
                           P1 => base_1+"довме" ;
                           P2 => base_1+"довте" ;
                           P3 => base_1+"доа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"дев" ;
                              P2 => base_1+"деше" ;
                              P3 => base_1+"деше"
                            } ;
                      Pl => table {
                              P1 => base_1+"девме" ;
                              P2 => base_1+"девте" ;
                              P3 => base_1+"деа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ди" ;
                       Pl => base_1+"дете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"шол" ;
                                  GSg Fem => base_1+"шла" ;
                                  GSg Neuter => base_1+"шло" ;
                                  GPl => base_1+"шле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"дел" ;
                                     GSg Fem => base_1+"дела" ;
                                     GSg Neuter => base_1+"дело" ;
                                     GPl => base_1+"деле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => nonExist ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV087"
  } ;

mkV088 : Str -> V ;
mkV088 base =
  case base of {
    base_1+"че" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"чам" ;
                            P2 => base_1+"чеш" ;
                            P3 => base_1+"че"
                          } ;
                    Pl => table {
                            P1 => base_1+"чеме" ;
                            P2 => base_1+"чете" ;
                            P3 => base_1+"чат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ков" ;
                           P2 => base_1+"че" ;
                           P3 => base_1+"че"
                         } ;
                   Pl => table {
                           P1 => base_1+"ковме" ;
                           P2 => base_1+"ковте" ;
                           P3 => base_1+"коа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"чев" ;
                              P2 => base_1+"чеше" ;
                              P3 => base_1+"чеше"
                            } ;
                      Pl => table {
                              P1 => base_1+"чевме" ;
                              P2 => base_1+"чевте" ;
                              P3 => base_1+"чеа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"чи" ;
                       Pl => base_1+"чете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"кол" ;
                                  GSg Fem => base_1+"кла" ;
                                  GSg Neuter => base_1+"кло" ;
                                  GPl => base_1+"кле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"чел" ;
                                     GSg Fem => base_1+"чела" ;
                                     GSg Neuter => base_1+"чело" ;
                                     GPl => base_1+"челе"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => nonExist ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV088"
  } ;

mkV089 : Str -> V ;
mkV089 base =
  case base of {
    base_1+base_2@(?+?)+"фи"+base_3@(?+?) => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+base_2+"фи"+base_3+"м" ;
                            P2 => base_1+base_2+"фи"+base_3+"ш" ;
                            P3 => base_1+base_2+"фи"+base_3
                          } ;
                    Pl => table {
                            P1 => base_1+base_2+"фи"+base_3+"ме" ;
                            P2 => base_1+base_2+"фи"+base_3+"те" ;
                            P3 => base_1+base_2+"фи"+base_3+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+base_2+"фи"+base_3+"в" ;
                           P2 => base_1+base_2+"фи"+base_3 ;
                           P3 => base_1+base_2+"фи"+base_3
                         } ;
                   Pl => table {
                           P1 => base_1+base_2+"фи"+base_3+"вме" ;
                           P2 => base_1+base_2+"фи"+base_3+"вте" ;
                           P3 => base_1+base_2+"фи"+base_3+"а"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+base_2+"фи"+base_3+"в" ;
                              P2 => base_1+base_2+"фи"+base_3+"ше" ;
                              P3 => base_1+base_2+"фи"+base_3+"ше"
                            } ;
                      Pl => table {
                              P1 => base_1+base_2+"фи"+base_3+"вме" ;
                              P2 => base_1+base_2+"фи"+base_3+"вте" ;
                              P3 => base_1+base_2+"фи"+base_3+"а"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+base_2+"фи"+base_3+"ј" ;
                       Pl => base_1+base_2+"фи"+base_3+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+base_2+"фи"+base_3+"л" ;
                                  GSg Fem => base_1+base_2+"фи"+base_3+"ла" ;
                                  GSg Neuter => base_1+base_2+"фи"+base_3+"ло" ;
                                  GPl => base_1+base_2+"фи"+base_3+"ле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+base_2+"фи"+base_3+"л" ;
                                     GSg Fem => base_1+base_2+"фи"+base_3+"ла" ;
                                     GSg Neuter => base_1+base_2+"фи"+base_3+"ло" ;
                                     GPl => base_1+base_2+"фи"+base_3+"ле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+base_2+"фи"+base_3+"н" ;
                       adverbial = base_1+"фи"+base_2+base_3+"јќи"
                     } ;
        noun_from_verb = base_1+"фи"+base_2+base_3+"ње" ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV089"
  } ;

mkV090 : Str -> V ;
mkV090 base_1 =
  lin V
  { present = \\_ =>
              table {
                Sg => table {
                        P1 => nonExist ;
                        P2 => nonExist ;
                        P3 => base_1
                      } ;
                Pl => table {
                        P1 => nonExist ;
                        P2 => nonExist ;
                        P3 => base_1+"ат" --guessed
                      }
              } ;
    aorist = table {
               Sg => table {
                       P1 => nonExist ;
                       P2 => nonExist ;
                       P3 => base_1 --guessed
                     } ;
               Pl => table {
                       P1 => nonExist ;
                       P2 => nonExist ;
                       P3 => nonExist
                     }
             } ;
    imperfect = \\_ =>
                table {
                  Sg => table {
                          P1 => nonExist ;
                          P2 => nonExist ;
                          P3 => base_1+"ше"
                        } ;
                  Pl => table {
                          P1 => nonExist ;
                          P2 => nonExist ;
                          P3 => base_1+"а" --guessed
                        }
                } ;
    Imperative = \\_ =>
                 table {
                   Sg => base_1+"ј" ;
                   Pl => nonExist
                 } ;
    participle = { aorist = \\_ => 
                            table {
                              GSg Masc => nonExist ;
                              GSg Fem => nonExist ;
                              GSg Neuter => base_1+"ло" ; --guessed
                              GPl => nonExist
                            } ;
                   imperfect = table {
                                 GSg Masc => nonExist ;
                                 GSg Fem => nonExist ;
                                 GSg Neuter => base_1+"ло" ;
                                 GPl => base_1+"ле" --guessed
                               } ;
                   perfect = \\_ => nonExist ;
                   adjectival = \\_ => nonExist ;
                   adverbial = nonExist
                 } ;
    noun_from_verb = base_1+"ње" ;
    isRefl = False
  } ;

mkV091 : Str -> V ;
mkV091 base_1 =
  lin V
  { present = \\_ =>
              table {
                Sg => table {
                        P1 => nonExist ;
                        P2 => nonExist ;
                        P3 => base_1
                      } ;
                Pl => table {
                        P1 => nonExist ;
                        P2 => nonExist ;
                        P3 => base_1+"ат"
                      }
              } ;
    aorist = table {
               Sg => table {
                       P1 => nonExist ;
                       P2 => nonExist ;
                       P3 => base_1 --guessed
                     } ;
               Pl => table {
                       P1 => nonExist ;
                       P2 => nonExist ;
                       P3 => nonExist
                     }
             } ;
    imperfect = \\_ =>
                table {
                  Sg => table {
                          P1 => nonExist ;
                          P2 => nonExist ;
                          P3 => base_1+"ше"
                        } ;
                  Pl => table {
                          P1 => nonExist ;
                          P2 => nonExist ;
                          P3 => base_1+"а"
                        }
                } ;
    Imperative = \\_ =>
                 table {
                   Sg => base_1+"ј" ;
                   Pl => nonExist
                 } ;
    participle = { aorist = \\_ => 
                            table {
                              GSg Masc => nonExist ;
                              GSg Fem => nonExist ;
                              GSg Neuter => base_1+"ло" ; --guessed
                              GPl => nonExist
                            } ;
                   imperfect = table {
                                 GSg Masc => nonExist ;
                                 GSg Fem => nonExist ;
                                 GSg Neuter => base_1+"ло" ;
                                 GPl => base_1+"ле"
                               } ;
                   perfect = \\_ => nonExist ;
                   adjectival = \\_ => nonExist ;
                   adverbial = nonExist
                 } ;
    noun_from_verb = base_1+"ње" ;
    isRefl = False
  } ;

mkV092 : Str -> V ;
mkV092 base =
  case base of {
    base_1+"те" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"там" ;
                            P2 => base_1+"теш" ;
                            P3 => base_1+"те"
                          } ;
                    Pl => table {
                            P1 => base_1+"теме" ;
                            P2 => base_1+"тете" ;
                            P3 => base_1+"тат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"тов" ;
                           P2 => base_1+"те" ;
                           P3 => base_1+"те"
                         } ;
                   Pl => table {
                           P1 => base_1+"товме" ;
                           P2 => base_1+"товте" ;
                           P3 => base_1+"тоа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"тев" ;
                              P2 => base_1+"теше" ;
                              P3 => base_1+"теше"
                            } ;
                      Pl => table {
                              P1 => base_1+"тевме" ;
                              P2 => base_1+"тевте" ;
                              P3 => base_1+"теа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ти" ;
                       Pl => base_1+"тете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"л" ;
                                  GSg Fem => base_1+"ла" ;
                                  GSg Neuter => base_1+"ло" ;
                                  GPl => base_1+"ле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"тел" ;
                                     GSg Fem => base_1+"тела" ;
                                     GSg Neuter => base_1+"тело" ;
                                     GPl => base_1+"теле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => nonExist ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV092"
  } ;

mkV093 : Str -> V ;
mkV093 base =
  case base of {
    "и"+base_1+"те" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => "и"+base_1+"там" ;
                            P2 => "и"+base_1+"теш" ;
                            P3 => "и"+base_1+"те"
                          } ;
                    Pl => table {
                            P1 => "и"+base_1+"теме" ;
                            P2 => "и"+base_1+"тете" ;
                            P3 => "и"+base_1+"тат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => "и"+base_1+"тов" ;
                           P2 => "и"+base_1+"те" ;
                           P3 => "и"+base_1+"те"
                         } ;
                   Pl => table {
                           P1 => "и"+base_1+"товме" ;
                           P2 => "и"+base_1+"товте" ;
                           P3 => "и"+base_1+"тоа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => "и"+base_1+"тев" ;
                              P2 => "и"+base_1+"теше" ;
                              P3 => "и"+base_1+"теше"
                            } ;
                      Pl => table {
                              P1 => "и"+base_1+"тевме" ;
                              P2 => "и"+base_1+"тевте" ;
                              P3 => "и"+base_1+"теа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => "и"+base_1+"ти" ;
                       Pl => "и"+base_1+"тете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"л" ;
                                  GSg Fem => base_1+"ла" ;
                                  GSg Neuter => base_1+"ло" ;
                                  GPl => base_1+"ле"
                                } ;
                       imperfect = table {
                                     GSg Masc => "и"+base_1+"тел" ;
                                     GSg Fem => "и"+base_1+"тела" ;
                                     GSg Neuter => "и"+base_1+"тело" ;
                                     GPl => "и"+base_1+"теле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => "и"+base_1+"тен" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV093"
  } ;

mkV094 : Str -> V ;
mkV094 base =
  case base of {
    base_1+"е" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"ам" ;
                            P2 => base_1+"еш" ;
                            P3 => base_1+"е"
                          } ;
                    Pl => table {
                            P1 => base_1+"еме" ;
                            P2 => base_1+"ете" ;
                            P3 => base_1+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"ов" ;
                           P2 => base_1+"е" ;
                           P3 => base_1+"е"
                         } ;
                   Pl => table {
                           P1 => base_1+"овме" ;
                           P2 => base_1+"овте" ;
                           P3 => base_1+"оа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и" ;
                       Pl => base_1+"ете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"ол" ;
                                  GSg Fem => base_1+"ла" ;
                                  GSg Neuter => base_1+"ло" ;
                                  GPl => base_1+"ле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => nonExist ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV094"
  } ;

mkV097 : Str -> V ;
mkV097 base =
  case base of {
    base_1+"же" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"жам" ;
                            P2 => base_1+"жеш" ;
                            P3 => base_1+"же"
                          } ;
                    Pl => table {
                            P1 => base_1+"жеме" ;
                            P2 => base_1+"жете" ;
                            P3 => base_1+"жат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"гав" ;
                           P2 => base_1+"га" ;
                           P3 => base_1+"га"
                         } ;
                   Pl => table {
                           P1 => base_1+"гавме" ;
                           P2 => base_1+"гавте" ;
                           P3 => base_1+"гаа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"жев" ;
                              P2 => base_1+"жеше" ;
                              P3 => base_1+"жеше"
                            } ;
                      Pl => table {
                              P1 => base_1+"жевме" ;
                              P2 => base_1+"жевте" ;
                              P3 => base_1+"жеа"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"жи" ;
                       Pl => base_1+"жете"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"гал" ;
                                  GSg Fem => base_1+"гала" ;
                                  GSg Neuter => base_1+"гало" ;
                                  GPl => base_1+"гале"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"жел" ;
                                     GSg Fem => base_1+"жела" ;
                                     GSg Neuter => base_1+"жело" ;
                                     GPl => base_1+"желе"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"ган" ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV097"
  } ;

mkV098 : Str -> V ;
mkV098 base =
  case base of {
    base_1+"и"+base_2@(?+?) => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"и"+base_2+"м" ;
                            P2 => base_1+"и"+base_2+"ш" ;
                            P3 => base_1+"и"+base_2
                          } ;
                    Pl => table {
                            P1 => base_1+"и"+base_2+"ме" ;
                            P2 => base_1+"и"+base_2+"те" ;
                            P3 => base_1+"и"+base_2+"ат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"и"+base_2+"в" ;
                           P2 => base_1+"и"+base_2 ;
                           P3 => base_1+"и"+base_2
                         } ;
                   Pl => table {
                           P1 => base_1+"и"+base_2+"вме" ;
                           P2 => base_1+"и"+base_2+"вте" ;
                           P3 => base_1+"и"+base_2+"а"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"и"+base_2+"в" ;
                              P2 => base_1+"и"+base_2+"ше" ;
                              P3 => base_1+"и"+base_2+"ше"
                            } ;
                      Pl => table {
                              P1 => base_1+"и"+base_2+"вме" ;
                              P2 => base_1+"и"+base_2+"вте" ;
                              P3 => base_1+"и"+base_2+"а"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"и"+base_2+"ј" ;
                       Pl => base_1+"и"+base_2+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"и"+base_2+"л" ;
                                  GSg Fem => base_1+"и"+base_2+"ла" ;
                                  GSg Neuter => base_1+"и"+base_2+"ло" ;
                                  GPl => base_1+"и"+base_2+"ле"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"и"+base_2+"л" ;
                                     GSg Fem => base_1+"и"+base_2+"ла" ;
                                     GSg Neuter => base_1+"и"+base_2+"ло" ;
                                     GPl => base_1+"и"+base_2+"ле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => base_1+"и"+base_2+"н" ;
                       adverbial = base_1+base_2+"јќи"
                     } ;
        noun_from_verb = base_1+base_2+"ње" ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV098"
  } ;

mkV099 : Str -> V ;
mkV099 base =
  case base of {
    base_1+"и" => lin V
      { present = \\_ =>
                  table {
                    Sg => table {
                            P1 => base_1+"јам" ;
                            P2 => base_1+"иш" ;
                            P3 => base_1+"и"
                          } ;
                    Pl => table {
                            P1 => base_1+"име" ;
                            P2 => base_1+"ите" ;
                            P3 => base_1+"јат"
                          }
                  } ;
        aorist = table {
                   Sg => table {
                           P1 => base_1+"јав" ;
                           P2 => base_1+"ја" ;
                           P3 => base_1+"ја"
                         } ;
                   Pl => table {
                           P1 => base_1+"јавме" ;
                           P2 => base_1+"јавте" ;
                           P3 => base_1+"јаа"
                         }
                 } ;
        imperfect = \\_ =>
                    table {
                      Sg => table {
                              P1 => base_1+"ев" ;
                              P2 => base_1+"еше" ;
                              P3 => base_1+"еше"
                            } ;
                      Pl => table {
                              P1 => base_1+"евме" ;
                              P2 => base_1+"евте" ;
                              P3 => base_1+"еја"
                            }
                    } ;
        Imperative = \\_ =>
                     table {
                       Sg => base_1+"ј" ;
                       Pl => base_1+"јте"
                     } ;
        participle = { aorist = \\_ => 
                                table {
                                  GSg Masc => base_1+"јал" ;
                                  GSg Fem => base_1+"јала" ;
                                  GSg Neuter => base_1+"јало" ;
                                  GPl => base_1+"јале"
                                } ;
                       imperfect = table {
                                     GSg Masc => base_1+"ел" ;
                                     GSg Fem => base_1+"ела" ;
                                     GSg Neuter => base_1+"ело" ;
                                     GPl => base_1+"еле"
                                   } ;
                       perfect = \\_ => nonExist ;
                       adjectival = \\_ => nonExist ;
                       adverbial = nonExist
                     } ;
        noun_from_verb = nonExist ;
        isRefl = False
      };
    _ => error "Can't apply paradigm mkV099"
  } ;

mkV101 : Str -> V ;
mkV101 base_1 =
  lin V
  { present = \\_ =>
              table {
                Sg => table {
                        P1 => nonExist ;
                        P2 => nonExist ;
                        P3 => base_1
                      } ;
                Pl => table {
                        P1 => nonExist ;
                        P2 => nonExist ;
                        P3 => nonExist
                      }
              } ;
    aorist = table {
               Sg => table {
                       P1 => nonExist ;
                       P2 => nonExist ;
                       P3 => base_1
                     } ;
               Pl => table {
                       P1 => nonExist ;
                       P2 => nonExist ;
                       P3 => nonExist
                     }
             } ;
    imperfect = \\_ =>
                table {
                  Sg => table {
                          P1 => nonExist ;
                          P2 => nonExist ;
                          P3 => base_1+"ше"
                        } ;
                  Pl => table {
                          P1 => nonExist ;
                          P2 => nonExist ;
                          P3 => nonExist
                        }
                } ;
    Imperative = \\_ =>
                 table {
                   Sg => base_1+"ј" ;
                   Pl => nonExist
                 } ;
    participle = { aorist = \\_ => 
                            table {
                              GSg Masc => nonExist ;
                              GSg Fem => nonExist ;
                              GSg Neuter => base_1+"ло" ;
                              GPl => nonExist
                            } ;
                   imperfect = table {
                                 GSg Masc => nonExist ;
                                 GSg Fem => nonExist ;
                                 GSg Neuter => base_1+"ло" ;
                                 GPl => nonExist
                               } ;
                   perfect = \\_ => nonExist ;
                   adjectival = \\_ => nonExist ;
                   adverbial = nonExist
                 } ;
    noun_from_verb = nonExist ;
    isRefl = False
  } ;

mkA001 : Str -> A ;
mkA001 base =
  case base of {
    base_1+"и" => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"и" ;
                         GSg Fem => base_1+"а" ;
                         GSg Neuter => base_1+"о" ;
                         GPl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+"иот" ;
                                   GSg Fem => base_1+"ата" ;
                                   GSg Neuter => base_1+"ото" ;
                                   GPl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+"иов" ;
                                GSg Fem => base_1+"ава" ;
                                GSg Neuter => base_1+"ово" ;
                                GPl => base_1+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+"ион" ;
                              GSg Fem => base_1+"ана" ;
                              GSg Neuter => base_1+"оно" ;
                              GPl => base_1+"ине"
                            }
            } ;
        adverb = base_1+"и" --guessed
      };
    _ => error "Can't apply paradigm mkA001"
  } ;

mkA002 : Str -> A ;
mkA002 base_1 =
  lin A
  { s = table {
          Indef => table {
                     GSg Masc => base_1 ;
                     GSg Fem => base_1+"а" ;
                     GSg Neuter => base_1+"е" ;
                     GPl => base_1+"и"
                   } ;
          Def Unspecified => table {
                               GSg Masc => base_1+"иот" ;
                               GSg Fem => base_1+"ата" ;
                               GSg Neuter => base_1+"ето" ;
                               GPl => base_1+"ите"
                             } ;
          Def Proximal => table {
                            GSg Masc => base_1+"иов" ;
                            GSg Fem => base_1+"ава" ;
                            GSg Neuter => base_1+"ево" ;
                            GPl => base_1+"иве"
                          } ;
          Def Distal => table {
                          GSg Masc => base_1+"ион" ;
                          GSg Fem => base_1+"ана" ;
                          GSg Neuter => base_1+"ено" ;
                          GPl => base_1+"ине"
                        }
        } ;
    adverb = nonExist
  } ;

mkA003 : Str -> A ;
mkA003 base =
  case base of {
    base_1+"е"+base_2@? => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"е"+base_2 ;
                         GSg Fem => base_1+base_2+"а" ;
                         GSg Neuter => base_1+base_2+"о" ;
                         GPl => base_1+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+base_2+"иот" ;
                                   GSg Fem => base_1+base_2+"ата" ;
                                   GSg Neuter => base_1+base_2+"ото" ;
                                   GPl => base_1+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+base_2+"иов" ;
                                GSg Fem => base_1+base_2+"ава" ;
                                GSg Neuter => base_1+base_2+"ово" ;
                                GPl => base_1+base_2+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+base_2+"ион" ;
                              GSg Fem => base_1+base_2+"ана" ;
                              GSg Neuter => base_1+base_2+"оно" ;
                              GPl => base_1+base_2+"ине"
                            }
            } ;
        adverb = base_1+base_2+"о" --guessed
      };
    _ => error "Can't apply paradigm mkA003"
  } ;

mkA004 : Str -> A ;
mkA004 base_1 =
  lin A
  { s = table {
          Indef => table {
                     GSg Masc => base_1 ;
                     GSg Fem => base_1+"а" ;
                     GSg Neuter => base_1+"о" ;
                     GPl => base_1+"и"
                   } ;
          Def Unspecified => table {
                               GSg Masc => base_1+"иот" ;
                               GSg Fem => base_1+"ата" ;
                               GSg Neuter => base_1+"ото" ;
                               GPl => base_1+"ите"
                             } ;
          Def Proximal => table {
                            GSg Masc => base_1+"иов" ;
                            GSg Fem => base_1+"ава" ;
                            GSg Neuter => base_1+"ово" ;
                            GPl => base_1+"иве"
                          } ;
          Def Distal => table {
                          GSg Masc => base_1+"ион" ;
                          GSg Fem => base_1+"ана" ;
                          GSg Neuter => base_1+"оно" ;
                          GPl => base_1+"ине"
                        }
        } ;
    adverb = base_1+"о" --guessed
  } ;

mkA005 : Str -> A ;
mkA005 base_1 =
  lin A
  { s = table {
          Indef => table {
                     GSg Masc => base_1 ;
                     GSg Fem => base_1+"а" ; --guessed
                     GSg Neuter => base_1+"о" ; --guessed
                     GPl => base_1+"и" --guessed
                   } ;
          Def Unspecified => table {
                               GSg Masc => base_1+"иот" ; --guessed
                               GSg Fem => base_1+"ата" ; --guessed
                               GSg Neuter => base_1+"ото" ; --guessed
                               GPl => base_1+"ите" --guessed
                             } ;
          Def Proximal => table {
                            GSg Masc => base_1+"иов" ; --guessed
                            GSg Fem => base_1+"ава" ; --guessed
                            GSg Neuter => base_1+"ово" ; --guessed
                            GPl => base_1+"иве" --guessed
                          } ;
          Def Distal => table {
                          GSg Masc => base_1+"ион" ; --guessed
                          GSg Fem => base_1+"ана" ; --guessed
                          GSg Neuter => base_1+"оно" ; --guessed
                          GPl => base_1+"ине" --guessed
                        }
        } ;
    adverb = base_1+"о" --guessed
  } ;

mkA006 : Str -> A ;
mkA006 base =
  case base of {
    base_1+"а"+base_2@? => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"а"+base_2 ;
                         GSg Fem => base_1+base_2+"а" ;
                         GSg Neuter => base_1+base_2+"о" ;
                         GPl => base_1+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+base_2+"иот" ;
                                   GSg Fem => base_1+base_2+"ата" ;
                                   GSg Neuter => base_1+base_2+"ото" ;
                                   GPl => base_1+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+base_2+"иов" ;
                                GSg Fem => base_1+base_2+"ава" ;
                                GSg Neuter => base_1+base_2+"ово" ;
                                GPl => base_1+base_2+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+base_2+"ион" ;
                              GSg Fem => base_1+base_2+"ана" ;
                              GSg Neuter => base_1+base_2+"оно" ;
                              GPl => base_1+base_2+"ине"
                            }
            } ;
        adverb = base_1+base_2+"о" --guessed
      };
    _ => error "Can't apply paradigm mkA006"
  } ;

mkA007 : Str -> A ;
mkA007 base =
  case base of {
    base_1+"е"+base_2@? => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"е"+base_2 ;
                         GSg Fem => base_1+base_2+"а" ;
                         GSg Neuter => base_1+base_2+"о" ;
                         GPl => base_1+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+base_2+"иот" ; --guessed
                                   GSg Fem => base_1+base_2+"ата" ; --guessed
                                   GSg Neuter => base_1+base_2+"ото" ; --guessed
                                   GPl => base_1+base_2+"ите" --guessed
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+base_2+"иов" ; --guessed
                                GSg Fem => base_1+base_2+"ава" ; --guessed
                                GSg Neuter => base_1+base_2+"ово" ; --guessed
                                GPl => base_1+base_2+"иве" --guessed
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+base_2+"ион" ; --guessed
                              GSg Fem => base_1+base_2+"ана" ; --guessed
                              GSg Neuter => base_1+base_2+"оно" ; --guessed
                              GPl => base_1+base_2+"ине" --guessed
                            }
            } ;
        adverb = base_1+base_2+"о" --guessed
      };
    _ => error "Can't apply paradigm mkA007"
  } ;

mkA008 : Str -> A ;
mkA008 base =
  case base of {
    base_1+"е"+base_2@? => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"е"+base_2 ;
                         GSg Fem => base_1+base_2+"а" ;
                         GSg Neuter => base_1+base_2+"о" ;
                         GPl => base_1+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+base_2+"иот" ;
                                   GSg Fem => base_1+base_2+"ата" ;
                                   GSg Neuter => base_1+base_2+"ото" ;
                                   GPl => base_1+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+base_2+"иов" ;
                                GSg Fem => base_1+base_2+"ава" ;
                                GSg Neuter => base_1+base_2+"ово" ;
                                GPl => base_1+base_2+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+base_2+"ион" ;
                              GSg Fem => base_1+base_2+"ана" ;
                              GSg Neuter => base_1+base_2+"оно" ;
                              GPl => base_1+base_2+"ине"
                            }
            } ;
        adverb = base_1+base_2+"о"
      };
    _ => error "Can't apply paradigm mkA008"
  } ;

mkA009 : Str -> A ;
mkA009 base =
  case base of {
    base_1+"е"+base_2@? => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"е"+base_2 ;
                         GSg Fem => base_1+"ј"+base_2+"а" ;
                         GSg Neuter => base_1+"ј"+base_2+"о" ;
                         GPl => base_1+"ј"+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+"ј"+base_2+"иот" ;
                                   GSg Fem => base_1+"ј"+base_2+"ата" ;
                                   GSg Neuter => base_1+"ј"+base_2+"ото" ;
                                   GPl => base_1+"ј"+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+"ј"+base_2+"иов" ;
                                GSg Fem => base_1+"ј"+base_2+"ава" ;
                                GSg Neuter => base_1+"ј"+base_2+"ово" ;
                                GPl => base_1+"ј"+base_2+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+"ј"+base_2+"ион" ;
                              GSg Fem => base_1+"ј"+base_2+"ана" ;
                              GSg Neuter => base_1+"ј"+base_2+"оно" ;
                              GPl => base_1+"ј"+base_2+"ине"
                            }
            } ;
        adverb = base_1+"ј"+base_2+"о" --guessed
      };
    _ => error "Can't apply paradigm mkA009"
  } ;

mkA010 : Str -> A ;
mkA010 base_1 =
  lin A
  { s = table {
          Indef => table {
                     GSg Masc => base_1 ;
                     GSg Fem => base_1+"а" ;
                     GSg Neuter => base_1+"о" ;
                     GPl => base_1+"и"
                   } ;
          Def Unspecified => table {
                               GSg Masc => base_1+"иот" ;
                               GSg Fem => base_1+"ата" ;
                               GSg Neuter => base_1+"ото" ;
                               GPl => base_1+"ите"
                             } ;
          Def Proximal => table {
                            GSg Masc => base_1+"иов" ;
                            GSg Fem => base_1+"ава" ;
                            GSg Neuter => base_1+"ово" ;
                            GPl => base_1+"иве"
                          } ;
          Def Distal => table {
                          GSg Masc => base_1+"ион" ;
                          GSg Fem => base_1+"ана" ;
                          GSg Neuter => base_1+"оно" ;
                          GPl => base_1+"ине"
                        }
        } ;
    adverb = base_1+"о"
  } ;

mkA011 : Str -> A ;
mkA011 base =
  case base of {
    base_1+"е"+base_2@(?+?+?+?+?+?)+"и" => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"е"+base_2+"и" ;
                         GSg Fem => base_1+"а"+base_2+"а" ;
                         GSg Neuter => base_1+"а"+base_2+"о" ;
                         GPl => base_1+"а"+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+"а"+base_2+"иот" ;
                                   GSg Fem => base_1+"а"+base_2+"ата" ;
                                   GSg Neuter => base_1+"а"+base_2+"ото" ;
                                   GPl => base_1+"а"+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+"а"+base_2+"иов" ;
                                GSg Fem => base_1+"а"+base_2+"ава" ;
                                GSg Neuter => base_1+"а"+base_2+"ово" ;
                                GPl => base_1+"а"+base_2+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+"а"+base_2+"ион" ;
                              GSg Fem => base_1+"а"+base_2+"ана" ;
                              GSg Neuter => base_1+"а"+base_2+"оно" ;
                              GPl => base_1+"а"+base_2+"ине"
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA011"
  } ;

mkA012 : Str -> A ;
mkA012 base =
  case base of {
    base_1+"те"+base_2@? => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"те"+base_2 ;
                         GSg Fem => base_1+base_2+"а" ;
                         GSg Neuter => base_1+base_2+"о" ;
                         GPl => base_1+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+base_2+"иот" ;
                                   GSg Fem => base_1+base_2+"ата" ;
                                   GSg Neuter => base_1+base_2+"ото" ;
                                   GPl => base_1+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+base_2+"иов" ;
                                GSg Fem => base_1+base_2+"ава" ;
                                GSg Neuter => base_1+base_2+"ово" ;
                                GPl => base_1+base_2+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+base_2+"ион" ;
                              GSg Fem => base_1+base_2+"ана" ;
                              GSg Neuter => base_1+base_2+"оно" ;
                              GPl => base_1+base_2+"ине"
                            }
            } ;
        adverb = base_1+base_2+"о" --guessed
      };
    _ => error "Can't apply paradigm mkA012"
  } ;

mkA013 : Str -> A ;
mkA013 base =
  case base of {
    base_1+"до"+base_2@? => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"до"+base_2 ;
                         GSg Fem => base_1+"т"+base_2+"а" ;
                         GSg Neuter => base_1+"т"+base_2+"о" ;
                         GPl => base_1+"т"+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+"т"+base_2+"иот" ;
                                   GSg Fem => base_1+"т"+base_2+"ата" ;
                                   GSg Neuter => base_1+"т"+base_2+"ото" ;
                                   GPl => base_1+"т"+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+"т"+base_2+"иов" ;
                                GSg Fem => base_1+"т"+base_2+"ава" ;
                                GSg Neuter => base_1+"т"+base_2+"ово" ;
                                GPl => base_1+"т"+base_2+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+"т"+base_2+"ион" ;
                              GSg Fem => base_1+"т"+base_2+"ана" ;
                              GSg Neuter => base_1+"т"+base_2+"оно" ;
                              GPl => base_1+"т"+base_2+"ине"
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA013"
  } ;

mkA014 : Str -> A ;
mkA014 base =
  case base of {
    base_1+"ој" => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"ој" ;
                         GSg Fem => base_1+"аа" ;
                         GSg Neuter => base_1+"оа" ;
                         GPl => base_1+"ие"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => nonExist ;
                                   GSg Fem => nonExist ;
                                   GSg Neuter => nonExist ;
                                   GPl => nonExist
                                 } ;
              Def Proximal => table {
                                GSg Masc => nonExist ;
                                GSg Fem => nonExist ;
                                GSg Neuter => nonExist ;
                                GPl => nonExist
                              } ;
              Def Distal => table {
                              GSg Masc => nonExist ;
                              GSg Fem => nonExist ;
                              GSg Neuter => nonExist ;
                              GPl => nonExist
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA014"
  } ;

mkA015 : Str -> A ;
mkA015 base =
  case base of {
    base_1+"ј" => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"ј" ;
                         GSg Fem => base_1+"ја" ;
                         GSg Neuter => base_1+"е" ;
                         GPl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+"jот" ;
                                   GSg Fem => base_1+"jата" ;
                                   GSg Neuter => base_1+"ето" ;
                                   GPl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+"jов" ;
                                GSg Fem => base_1+"jава" ;
                                GSg Neuter => base_1+"ево" ;
                                GPl => base_1+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+"jон" ;
                              GSg Fem => base_1+"jана" ;
                              GSg Neuter => base_1+"ено" ;
                              GPl => base_1+"ине"
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA015"
  } ;

mkA016 : Str -> A ;
mkA016 base =
  case base of {
    base_1+"ј" => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"ј" ;
                         GSg Fem => base_1+"ја" ;
                         GSg Neuter => base_1+"е" ;
                         GPl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => nonExist ;
                                   GSg Fem => nonExist ;
                                   GSg Neuter => nonExist ;
                                   GPl => nonExist
                                 } ;
              Def Proximal => table {
                                GSg Masc => nonExist ;
                                GSg Fem => nonExist ;
                                GSg Neuter => nonExist ;
                                GPl => nonExist
                              } ;
              Def Distal => table {
                              GSg Masc => nonExist ;
                              GSg Fem => nonExist ;
                              GSg Neuter => nonExist ;
                              GPl => nonExist
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA016"
  } ;

mkA017 : Str -> A ;
mkA017 base =
  case base of {
    base_1+"ој" => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"ој" ;
                         GSg Fem => base_1+"аа" ;
                         GSg Neuter => base_1+"а" ;
                         GPl => base_1+"ие"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => nonExist ;
                                   GSg Fem => nonExist ;
                                   GSg Neuter => nonExist ;
                                   GPl => nonExist
                                 } ;
              Def Proximal => table {
                                GSg Masc => nonExist ;
                                GSg Fem => nonExist ;
                                GSg Neuter => nonExist ;
                                GPl => nonExist
                              } ;
              Def Distal => table {
                              GSg Masc => nonExist ;
                              GSg Fem => nonExist ;
                              GSg Neuter => nonExist ;
                              GPl => nonExist
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA017"
  } ;

mkA018 : Str -> A ;
mkA018 base =
  case base of {
    base_1+"жо"+base_2@? => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"жо"+base_2 ;
                         GSg Fem => base_1+"ш"+base_2+"а" ;
                         GSg Neuter => base_1+"ш"+base_2+"о" ;
                         GPl => base_1+"ш"+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+"ш"+base_2+"иот" ;
                                   GSg Fem => base_1+"ш"+base_2+"ата" ;
                                   GSg Neuter => base_1+"ш"+base_2+"ото" ;
                                   GPl => base_1+"ш"+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+"ш"+base_2+"иов" ;
                                GSg Fem => base_1+"ш"+base_2+"ава" ;
                                GSg Neuter => base_1+"ш"+base_2+"ово" ;
                                GPl => base_1+"ш"+base_2+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+"ш"+base_2+"ион" ;
                              GSg Fem => base_1+"ш"+base_2+"ана" ;
                              GSg Neuter => base_1+"ш"+base_2+"оно" ;
                              GPl => base_1+"ш"+base_2+"ине"
                            }
            } ;
        adverb = base_1+"ш"+base_2+"о" --guessed
      };
    _ => error "Can't apply paradigm mkA018"
  } ;

mkA019 : Str -> A ;
mkA019 base =
  case base of {
    base_1+"о"+base_2@? => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"о"+base_2 ;
                         GSg Fem => base_1+base_2+"а" ;
                         GSg Neuter => base_1+base_2+"о" ;
                         GPl => base_1+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+base_2+"иот" ;
                                   GSg Fem => base_1+base_2+"ата" ;
                                   GSg Neuter => base_1+base_2+"ото" ;
                                   GPl => base_1+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+base_2+"иов" ;
                                GSg Fem => base_1+base_2+"ава" ;
                                GSg Neuter => base_1+base_2+"ово" ;
                                GPl => base_1+base_2+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+base_2+"ион" ;
                              GSg Fem => base_1+base_2+"ана" ;
                              GSg Neuter => base_1+base_2+"оно" ;
                              GPl => base_1+base_2+"ине"
                            }
            } ;
        adverb = base_1+base_2+"о" --guessed
      };
    _ => error "Can't apply paradigm mkA019"
  } ;

mkA020 : Str -> A ;
mkA020 base =
  case base of {
    base_1+"и" => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"и" ;
                         GSg Fem => base_1+"а" ;
                         GSg Neuter => base_1+"о" ;
                         GPl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+"иот" ;
                                   GSg Fem => base_1+"ата" ;
                                   GSg Neuter => base_1+"ото" ;
                                   GPl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+"иов" ;
                                GSg Fem => base_1+"ава" ;
                                GSg Neuter => base_1+"ово" ;
                                GPl => base_1+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+"ион" ;
                              GSg Fem => base_1+"ана" ;
                              GSg Neuter => base_1+"оно" ;
                              GPl => base_1+"ине"
                            }
            } ;
        adverb = base_1+"и"
      };
    _ => error "Can't apply paradigm mkA020"
  } ;

mkA021 : Str -> A ;
mkA021 base =
  case base of {
    base_1+"а"+base_2@? => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"а"+base_2 ;
                         GSg Fem => base_1+base_2+"а" ;
                         GSg Neuter => base_1+base_2+"о" ;
                         GPl => base_1+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+base_2+"иот" ;
                                   GSg Fem => base_1+base_2+"ата" ;
                                   GSg Neuter => base_1+base_2+"ото" ;
                                   GPl => base_1+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+base_2+"иов" ;
                                GSg Fem => base_1+base_2+"ава" ;
                                GSg Neuter => base_1+base_2+"ово" ;
                                GPl => base_1+base_2+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+base_2+"ион" ;
                              GSg Fem => base_1+base_2+"ана" ;
                              GSg Neuter => base_1+base_2+"оно" ;
                              GPl => base_1+base_2+"ине"
                            }
            } ;
        adverb = base_1+base_2+"о"
      };
    _ => error "Can't apply paradigm mkA021"
  } ;

mkA022 : Str -> A ;
mkA022 base =
  case base of {
    base_1+base_2@(?+?+?+?+?+?+?)+"и" => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+base_2+"и" ;
                         GSg Fem => base_1+"да"+base_2+"а" ;
                         GSg Neuter => base_1+"да"+base_2+"о" ;
                         GPl => base_1+"да"+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+"да"+base_2+"иот" ;
                                   GSg Fem => base_1+"да"+base_2+"ата" ;
                                   GSg Neuter => base_1+"да"+base_2+"ото" ;
                                   GPl => base_1+"да"+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+"да"+base_2+"иов" ;
                                GSg Fem => base_1+"да"+base_2+"ава" ;
                                GSg Neuter => base_1+"да"+base_2+"ово" ;
                                GPl => base_1+"да"+base_2+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+"да"+base_2+"ион" ;
                              GSg Fem => base_1+"да"+base_2+"ана" ;
                              GSg Neuter => base_1+"да"+base_2+"оно" ;
                              GPl => base_1+"да"+base_2+"ине"
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA022"
  } ;

mkA023 : Str -> A ;
mkA023 base =
  case base of {
    base_1+"о"+base_2@? => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"о"+base_2 ;
                         GSg Fem => base_1+base_2+"а" ;
                         GSg Neuter => base_1+base_2+"о" ;
                         GPl => base_1+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+base_2+"иот" ; --guessed
                                   GSg Fem => base_1+base_2+"ата" ; --guessed
                                   GSg Neuter => base_1+base_2+"ото" ; --guessed
                                   GPl => base_1+base_2+"ите" --guessed
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+base_2+"иов" ; --guessed
                                GSg Fem => base_1+base_2+"ава" ; --guessed
                                GSg Neuter => base_1+base_2+"ово" ; --guessed
                                GPl => base_1+base_2+"иве" --guessed
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+base_2+"ион" ; --guessed
                              GSg Fem => base_1+base_2+"ана" ; --guessed
                              GSg Neuter => base_1+base_2+"оно" ; --guessed
                              GPl => base_1+base_2+"ине" --guessed
                            }
            } ;
        adverb = base_1+base_2+"о" --guessed
      };
    _ => error "Can't apply paradigm mkA023"
  } ;

mkA024 : Str -> A ;
mkA024 base_1 =
  lin A
  { s = table {
          Indef => table {
                     GSg Masc => nonExist ;
                     GSg Fem => nonExist ;
                     GSg Neuter => base_1+"ѐ" ;
                     GPl => nonExist
                   } ;
          Def Unspecified => table {
                               GSg Masc => base_1+"иот" ;
                               GSg Fem => base_1+"ета" ;
                               GSg Neuter => base_1+"ето" ;
                               GPl => base_1+"ите"
                             } ;
          Def Proximal => table {
                            GSg Masc => base_1+"иов" ;
                            GSg Fem => base_1+"ева" ;
                            GSg Neuter => base_1+"ево" ;
                            GPl => base_1+"иве"
                          } ;
          Def Distal => table {
                          GSg Masc => base_1+"ион" ;
                          GSg Fem => base_1+"ена" ;
                          GSg Neuter => base_1+"ено" ;
                          GPl => base_1+"ине"
                        }
        } ;
    adverb = nonExist
  } ;

mkA025 : Str -> A ;
mkA025 base =
  case base of {
    base_1+"е"+base_2@? => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"е"+base_2 ;
                         GSg Fem => base_1+"ј"+base_2+"а" ;
                         GSg Neuter => base_1+"ј"+base_2+"о" ;
                         GPl => base_1+"ј"+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+"ј"+base_2+"иот" ;
                                   GSg Fem => base_1+"ј"+base_2+"ата" ;
                                   GSg Neuter => base_1+"ј"+base_2+"ото" ;
                                   GPl => base_1+"ј"+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+"ј"+base_2+"иов" ;
                                GSg Fem => base_1+"ј"+base_2+"ава" ;
                                GSg Neuter => base_1+"ј"+base_2+"ово" ;
                                GPl => base_1+"ј"+base_2+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+"ј"+base_2+"ион" ;
                              GSg Fem => base_1+"ј"+base_2+"ана" ;
                              GSg Neuter => base_1+"ј"+base_2+"оно" ;
                              GPl => base_1+"ј"+base_2+"ине"
                            }
            } ;
        adverb = base_1+"ј"+base_2+"о"
      };
    _ => error "Can't apply paradigm mkA025"
  } ;

mkA026 : Str -> A ;
mkA026 base =
  case base of {
    base_1+"зо"+base_2@? => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"зо"+base_2 ;
                         GSg Fem => base_1+"с"+base_2+"а" ;
                         GSg Neuter => base_1+"с"+base_2+"о" ;
                         GPl => base_1+"с"+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+"с"+base_2+"иот" ;
                                   GSg Fem => base_1+"с"+base_2+"ата" ;
                                   GSg Neuter => base_1+"с"+base_2+"ото" ;
                                   GPl => base_1+"с"+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+"с"+base_2+"иов" ;
                                GSg Fem => base_1+"с"+base_2+"ава" ;
                                GSg Neuter => base_1+"с"+base_2+"ово" ;
                                GPl => base_1+"с"+base_2+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+"с"+base_2+"ион" ;
                              GSg Fem => base_1+"с"+base_2+"ана" ;
                              GSg Neuter => base_1+"с"+base_2+"оно" ;
                              GPl => base_1+"с"+base_2+"ине"
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA026"
  } ;

mkA027 : Str -> A ;
mkA027 base =
  case base of {
    base_1+"о"+base_2@? => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"о"+base_2 ;
                         GSg Fem => base_1+base_2+"а" ;
                         GSg Neuter => base_1+base_2+"о" ;
                         GPl => base_1+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+base_2+"иот" ;
                                   GSg Fem => base_1+base_2+"ата" ;
                                   GSg Neuter => base_1+base_2+"ото" ;
                                   GPl => base_1+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+base_2+"иов" ;
                                GSg Fem => base_1+base_2+"ава" ;
                                GSg Neuter => base_1+base_2+"ово" ;
                                GPl => base_1+base_2+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+base_2+"ион" ;
                              GSg Fem => base_1+base_2+"ана" ;
                              GSg Neuter => base_1+base_2+"оно" ;
                              GPl => base_1+base_2+"ине"
                            }
            } ;
        adverb = base_1+base_2+"о"
      };
    _ => error "Can't apply paradigm mkA027"
  } ;

mkA028 : Str -> A ;
mkA028 base =
  case base of {
    base_1+"зок" => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"зок" ;
                         GSg Fem => base_1+"ска" ;
                         GSg Neuter => base_1+"ско" ;
                         GPl => base_1+"ски"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+"скиот" ;
                                   GSg Fem => base_1+"ската" ;
                                   GSg Neuter => base_1+"ското" ;
                                   GPl => base_1+"ските"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+"скиов" ;
                                GSg Fem => base_1+"скава" ;
                                GSg Neuter => base_1+"сково" ;
                                GPl => base_1+"скиве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+"скион" ;
                              GSg Fem => base_1+"скана" ;
                              GSg Neuter => base_1+"сконо" ;
                              GPl => base_1+"скине"
                            }
            } ;
        adverb = base_1+"зу"
      };
    _ => error "Can't apply paradigm mkA028"
  } ;

mkA029 : Str -> A ;
mkA029 base =
  case base of {
    base_1+"ј"+base_2@(?+?+?) => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"ј"+base_2 ;
                         GSg Fem => base_1+"ја"+base_2 ;
                         GSg Neuter => base_1+"е"+base_2 ;
                         GPl => base_1+"и"+base_2
                       } ;
              Def Unspecified => table {
                                   GSg Masc => nonExist ;
                                   GSg Fem => nonExist ;
                                   GSg Neuter => nonExist ;
                                   GPl => nonExist
                                 } ;
              Def Proximal => table {
                                GSg Masc => nonExist ;
                                GSg Fem => nonExist ;
                                GSg Neuter => nonExist ;
                                GPl => nonExist
                              } ;
              Def Distal => table {
                              GSg Masc => nonExist ;
                              GSg Fem => nonExist ;
                              GSg Neuter => nonExist ;
                              GPl => nonExist
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA029"
  } ;

mkA030 : Str -> A ;
mkA030 base_1 =
  lin A
  { s = table {
          Indef => table {
                     GSg Masc => base_1 ;
                     GSg Fem => base_1+"а" ;
                     GSg Neuter => base_1+"о" ;
                     GPl => base_1+"и"
                   } ;
          Def Unspecified => table {
                               GSg Masc => nonExist ;
                               GSg Fem => base_1+"ата" ; --guessed
                               GSg Neuter => base_1+"ото" ; --guessed
                               GPl => base_1+"ите" --guessed
                             } ;
          Def Proximal => table {
                            GSg Masc => nonExist ;
                            GSg Fem => base_1+"ава" ; --guessed
                            GSg Neuter => base_1+"ово" ; --guessed
                            GPl => base_1+"иве" --guessed
                          } ;
          Def Distal => table {
                          GSg Masc => nonExist ;
                          GSg Fem => base_1+"ана" ; --guessed
                          GSg Neuter => base_1+"оно" ; --guessed
                          GPl => base_1+"ине" --guessed
                        }
        } ;
    adverb = nonExist
  } ;

mkA031 : Str -> A ;
mkA031 base =
  case base of {
    base_1+"н"+base_2@(?+?+?)+"е"+base_3@? => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"н"+base_2+"е"+base_3 ;
                         GSg Fem => base_1+base_2+base_3+"а" ;
                         GSg Neuter => base_1+base_2+base_3+"о" ;
                         GPl => base_1+base_2+base_3+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+base_2+base_3+"иот" ;
                                   GSg Fem => base_1+base_2+base_3+"ата" ;
                                   GSg Neuter => base_1+base_2+base_3+"ото" ;
                                   GPl => base_1+base_2+base_3+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+base_2+base_3+"иов" ;
                                GSg Fem => base_1+base_2+base_3+"ава" ;
                                GSg Neuter => base_1+base_2+base_3+"ово" ;
                                GPl => base_1+base_2+base_3+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+base_2+base_3+"ион" ;
                              GSg Fem => base_1+base_2+base_3+"ана" ;
                              GSg Neuter => base_1+base_2+base_3+"оно" ;
                              GPl => base_1+base_2+base_3+"ине"
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA031"
  } ;

mkA032 : Str -> A ;
mkA032 base =
  case base of {
    base_1+"жо"+base_2@? => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"жо"+base_2 ;
                         GSg Fem => base_1+"ш"+base_2+"а" ;
                         GSg Neuter => base_1+"ш"+base_2+"о" ;
                         GPl => base_1+"ш"+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+"ш"+base_2+"иот" ;
                                   GSg Fem => base_1+"ш"+base_2+"ата" ;
                                   GSg Neuter => base_1+"ш"+base_2+"ото" ;
                                   GPl => base_1+"ш"+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+"ш"+base_2+"иов" ;
                                GSg Fem => base_1+"ш"+base_2+"ава" ;
                                GSg Neuter => base_1+"ш"+base_2+"ово" ;
                                GPl => base_1+"ш"+base_2+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+"ш"+base_2+"ион" ;
                              GSg Fem => base_1+"ш"+base_2+"ана" ;
                              GSg Neuter => base_1+"ш"+base_2+"оно" ;
                              GPl => base_1+"ш"+base_2+"ине"
                            }
            } ;
        adverb = base_1+"ш"+base_2+"о"
      };
    _ => error "Can't apply paradigm mkA032"
  } ;

mkA033 : Str -> A ;
mkA033 base =
  case base of {
    base_1+"ен" => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"ен" ; --guessed
                         GSg Fem => base_1+"а" ;
                         GSg Neuter => base_1+"о" ;
                         GPl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+"иот" ; --guessed
                                   GSg Fem => base_1+"ата" ;
                                   GSg Neuter => base_1+"ото" ;
                                   GPl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+"иов" ; --guessed
                                GSg Fem => base_1+"ава" ;
                                GSg Neuter => base_1+"ово" ;
                                GPl => base_1+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+"ион" ; --guessed
                              GSg Fem => base_1+"ана" ;
                              GSg Neuter => base_1+"оно" ;
                              GPl => base_1+"ине"
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA033"
  } ;

mkA034 : Str -> A ;
mkA034 base =
  case base of {
    base_1+"те"+base_2@? => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"те"+base_2 ;
                         GSg Fem => base_1+base_2+"а" ;
                         GSg Neuter => base_1+base_2+"о" ;
                         GPl => base_1+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+base_2+"иот" ;
                                   GSg Fem => base_1+base_2+"ата" ;
                                   GSg Neuter => base_1+base_2+"ото" ;
                                   GPl => base_1+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+base_2+"иов" ;
                                GSg Fem => base_1+base_2+"ава" ;
                                GSg Neuter => base_1+base_2+"ово" ;
                                GPl => base_1+base_2+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+base_2+"ион" ;
                              GSg Fem => base_1+base_2+"ана" ;
                              GSg Neuter => base_1+base_2+"оно" ;
                              GPl => base_1+base_2+"ине"
                            }
            } ;
        adverb = base_1+base_2+"о"
      };
    _ => error "Can't apply paradigm mkA034"
  } ;

mkA035 : Str -> A ;
mkA035 base =
  case base of {
    "’"+base_1 => lin A
      { s = table {
              Indef => table {
                         GSg Masc => "’"+base_1 ;
                         GSg Fem => "‘"+base_1+"а" ;
                         GSg Neuter => "‘"+base_1+"о" ;
                         GPl => "‘"+base_1+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => "‘"+base_1+"иот" ;
                                   GSg Fem => "‘"+base_1+"ата" ;
                                   GSg Neuter => "‘"+base_1+"ото" ;
                                   GPl => "‘"+base_1+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => "‘"+base_1+"иов" ;
                                GSg Fem => "‘"+base_1+"ава" ;
                                GSg Neuter => "‘"+base_1+"ово" ;
                                GPl => "‘"+base_1+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => "‘"+base_1+"ион" ;
                              GSg Fem => "‘"+base_1+"ана" ;
                              GSg Neuter => "‘"+base_1+"оно" ;
                              GPl => "‘"+base_1+"ине"
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA035"
  } ;

mkA036 : Str -> A ;
mkA036 base =
  case base of {
    base_1+base_2@(?+?+?+?+?+?+?)+"и" => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+base_2+"и" ;
                         GSg Fem => base_1+"по"+base_2+"а" ;
                         GSg Neuter => base_1+"по"+base_2+"о" ;
                         GPl => base_1+"по"+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+"по"+base_2+"иот" ;
                                   GSg Fem => base_1+"по"+base_2+"ата" ;
                                   GSg Neuter => base_1+"по"+base_2+"ото" ;
                                   GPl => base_1+"по"+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+"по"+base_2+"иов" ;
                                GSg Fem => base_1+"по"+base_2+"ава" ;
                                GSg Neuter => base_1+"по"+base_2+"ово" ;
                                GPl => base_1+"по"+base_2+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+"по"+base_2+"ион" ;
                              GSg Fem => base_1+"по"+base_2+"ана" ;
                              GSg Neuter => base_1+"по"+base_2+"оно" ;
                              GPl => base_1+"по"+base_2+"ине"
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA036"
  } ;

mkA037 : Str -> A ;
mkA037 base =
  case base of {
    base_1+"ен" => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"ен" ;
                         GSg Fem => base_1+"а" ;
                         GSg Neuter => base_1+"о" ;
                         GPl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+"иот" ;
                                   GSg Fem => base_1+"ата" ;
                                   GSg Neuter => base_1+"ото" ;
                                   GPl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+"иов" ;
                                GSg Fem => base_1+"ава" ;
                                GSg Neuter => base_1+"ово" ;
                                GPl => base_1+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+"ион" ;
                              GSg Fem => base_1+"ана" ;
                              GSg Neuter => base_1+"оно" ;
                              GPl => base_1+"ине"
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA037"
  } ;

mkA038 : Str -> A ;
mkA038 base =
  case base of {
    base_1+base_2@(?+?+?+?+?+?)+"и" => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+base_2+"и" ;
                         GSg Fem => base_1+"о"+base_2+"а" ;
                         GSg Neuter => base_1+"о"+base_2+"о" ;
                         GPl => base_1+"о"+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+"о"+base_2+"иот" ;
                                   GSg Fem => base_1+"о"+base_2+"ата" ;
                                   GSg Neuter => base_1+"о"+base_2+"ото" ;
                                   GPl => base_1+"о"+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+"о"+base_2+"иов" ;
                                GSg Fem => base_1+"о"+base_2+"ава" ;
                                GSg Neuter => base_1+"о"+base_2+"ово" ;
                                GPl => base_1+"о"+base_2+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+"о"+base_2+"ион" ;
                              GSg Fem => base_1+"о"+base_2+"ана" ;
                              GSg Neuter => base_1+"о"+base_2+"оно" ;
                              GPl => base_1+"о"+base_2+"ине"
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA038"
  } ;

mkA039 : Str -> A ;
mkA039 base =
  case base of {
    base_1+"и"+base_2@(?+?+?+?)+"е"+base_3@? => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"и"+base_2+"е"+base_3 ;
                         GSg Fem => base_1+"о"+base_2+base_3+"а" ;
                         GSg Neuter => base_1+"о"+base_2+base_3+"о" ;
                         GPl => base_1+"о"+base_2+base_3+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+"о"+base_2+base_3+"иот" ;
                                   GSg Fem => base_1+"о"+base_2+base_3+"ата" ;
                                   GSg Neuter => base_1+"о"+base_2+base_3+"ото" ;
                                   GPl => base_1+"о"+base_2+base_3+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+"о"+base_2+base_3+"иов" ;
                                GSg Fem => base_1+"о"+base_2+base_3+"ава" ;
                                GSg Neuter => base_1+"о"+base_2+base_3+"ово" ;
                                GPl => base_1+"о"+base_2+base_3+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+"о"+base_2+base_3+"ион" ;
                              GSg Fem => base_1+"о"+base_2+base_3+"ана" ;
                              GSg Neuter => base_1+"о"+base_2+base_3+"оно" ;
                              GPl => base_1+"о"+base_2+base_3+"ине"
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA039"
  } ;

mkA040 : Str -> A ;
mkA040 base =
  case base of {
    base_1+"ча"+base_2@?+base_3@(?+?)+"и" => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"ча"+base_2+base_3+"и" ;
                         GSg Fem => base_1+base_2+"а"+base_3+"а" ;
                         GSg Neuter => base_1+base_2+"а"+base_3+"о" ;
                         GPl => base_1+base_2+"а"+base_3+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+base_2+"а"+base_3+"иот" ;
                                   GSg Fem => base_1+base_2+"а"+base_3+"ата" ;
                                   GSg Neuter => base_1+base_2+"а"+base_3+"ото" ;
                                   GPl => base_1+base_2+"а"+base_3+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+base_2+"а"+base_3+"иов" ;
                                GSg Fem => base_1+base_2+"а"+base_3+"ава" ;
                                GSg Neuter => base_1+base_2+"а"+base_3+"ово" ;
                                GPl => base_1+base_2+"а"+base_3+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+base_2+"а"+base_3+"ион" ;
                              GSg Fem => base_1+base_2+"а"+base_3+"ана" ;
                              GSg Neuter => base_1+base_2+"а"+base_3+"оно" ;
                              GPl => base_1+base_2+"а"+base_3+"ине"
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA040"
  } ;

mkA041 : Str -> A ;
mkA041 base =
  case base of {
    base_1+"а" => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"а" ;
                         GSg Fem => base_1+"а" ;
                         GSg Neuter => base_1+"о" ;
                         GPl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+"иот" ;
                                   GSg Fem => base_1+"ата" ;
                                   GSg Neuter => base_1+"ото" ;
                                   GPl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+"иов" ;
                                GSg Fem => base_1+"ава" ;
                                GSg Neuter => base_1+"ово" ;
                                GPl => base_1+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+"ион" ;
                              GSg Fem => base_1+"ана" ;
                              GSg Neuter => base_1+"оно" ;
                              GPl => base_1+"ине"
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA041"
  } ;

mkA042 : Str -> A ;
mkA042 base =
  case base of {
    base_1+"л"+base_2@(?+?+?+?+?+?)+"и" => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"л"+base_2+"и" ;
                         GSg Fem => base_1+"ч"+base_2+"а" ;
                         GSg Neuter => base_1+"ч"+base_2+"о" ;
                         GPl => base_1+"ч"+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+"ч"+base_2+"иот" ;
                                   GSg Fem => base_1+"ч"+base_2+"ата" ;
                                   GSg Neuter => base_1+"ч"+base_2+"ото" ;
                                   GPl => base_1+"ч"+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+"ч"+base_2+"иов" ;
                                GSg Fem => base_1+"ч"+base_2+"ава" ;
                                GSg Neuter => base_1+"ч"+base_2+"ово" ;
                                GPl => base_1+"ч"+base_2+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+"ч"+base_2+"ион" ;
                              GSg Fem => base_1+"ч"+base_2+"ана" ;
                              GSg Neuter => base_1+"ч"+base_2+"оно" ;
                              GPl => base_1+"ч"+base_2+"ине"
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA042"
  } ;

mkA043 : Str -> A ;
mkA043 base =
  case base of {
    base_1+base_2@(?+?+?)+"е"+base_3@? => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+base_2+"е"+base_3 ;
                         GSg Fem => base_1+"а"+base_2+base_3+"а" ;
                         GSg Neuter => base_1+"а"+base_2+base_3+"о" ;
                         GPl => base_1+"а"+base_2+base_3+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+"а"+base_2+base_3+"иот" ;
                                   GSg Fem => base_1+"а"+base_2+base_3+"ата" ;
                                   GSg Neuter => base_1+"а"+base_2+base_3+"ото" ;
                                   GPl => base_1+"а"+base_2+base_3+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+"а"+base_2+base_3+"иов" ;
                                GSg Fem => base_1+"а"+base_2+base_3+"ава" ;
                                GSg Neuter => base_1+"а"+base_2+base_3+"ово" ;
                                GPl => base_1+"а"+base_2+base_3+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+"а"+base_2+base_3+"ион" ;
                              GSg Fem => base_1+"а"+base_2+base_3+"ана" ;
                              GSg Neuter => base_1+"а"+base_2+base_3+"оно" ;
                              GPl => base_1+"а"+base_2+base_3+"ине"
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA043"
  } ;

mkA044 : Str -> A ;
mkA044 base =
  case base of {
    "прво"+base_1+"и" => lin A
      { s = table {
              Indef => table {
                         GSg Masc => "прво"+base_1+"и" ;
                         GSg Fem => base_1+"а" ;
                         GSg Neuter => base_1+"о" ;
                         GPl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+"иот" ;
                                   GSg Fem => base_1+"ата" ;
                                   GSg Neuter => base_1+"ото" ;
                                   GPl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+"иов" ;
                                GSg Fem => base_1+"ава" ;
                                GSg Neuter => base_1+"ово" ;
                                GPl => base_1+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+"ион" ;
                              GSg Fem => base_1+"ана" ;
                              GSg Neuter => base_1+"оно" ;
                              GPl => base_1+"ине"
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA044"
  } ;

mkA045 : Str -> A ;
mkA045 base =
  case base of {
    base_1+base_2@?+"те"+base_3@? => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+base_2+"те"+base_3 ;
                         GSg Fem => base_1+"т"+base_2+base_3+"а" ;
                         GSg Neuter => base_1+"т"+base_2+base_3+"о" ;
                         GPl => base_1+"т"+base_2+base_3+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+"т"+base_2+base_3+"иот" ;
                                   GSg Fem => base_1+"т"+base_2+base_3+"ата" ;
                                   GSg Neuter => base_1+"т"+base_2+base_3+"ото" ;
                                   GPl => base_1+"т"+base_2+base_3+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+"т"+base_2+base_3+"иов" ;
                                GSg Fem => base_1+"т"+base_2+base_3+"ава" ;
                                GSg Neuter => base_1+"т"+base_2+base_3+"ово" ;
                                GPl => base_1+"т"+base_2+base_3+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+"т"+base_2+base_3+"ион" ;
                              GSg Fem => base_1+"т"+base_2+base_3+"ана" ;
                              GSg Neuter => base_1+"т"+base_2+base_3+"оно" ;
                              GPl => base_1+"т"+base_2+base_3+"ине"
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA045"
  } ;

mkA046 : Str -> A ;
mkA046 base =
  case base of {
    base_1+base_2@?+"д"+base_3@(?+?+?+?+?+?+?+?)+"и" => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+base_2+"д"+base_3+"и" ;
                         GSg Fem => base_1+"д"+base_2+base_3+"а" ;
                         GSg Neuter => base_1+"д"+base_2+base_3+"о" ;
                         GPl => base_1+"д"+base_2+base_3+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+"д"+base_2+base_3+"иот" ;
                                   GSg Fem => base_1+"д"+base_2+base_3+"ата" ;
                                   GSg Neuter => base_1+"д"+base_2+base_3+"ото" ;
                                   GPl => base_1+"д"+base_2+base_3+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+"д"+base_2+base_3+"иов" ;
                                GSg Fem => base_1+"д"+base_2+base_3+"ава" ;
                                GSg Neuter => base_1+"д"+base_2+base_3+"ово" ;
                                GPl => base_1+"д"+base_2+base_3+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+"д"+base_2+base_3+"ион" ;
                              GSg Fem => base_1+"д"+base_2+base_3+"ана" ;
                              GSg Neuter => base_1+"д"+base_2+base_3+"оно" ;
                              GPl => base_1+"д"+base_2+base_3+"ине"
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA046"
  } ;

mkA047 : Str -> A ;
mkA047 base =
  case base of {
    "осн"+base_1+"в"+base_2@(?+?+?)+"и" => lin A
      { s = table {
              Indef => table {
                         GSg Masc => "осн"+base_1+"в"+base_2+"и" ;
                         GSg Fem => "б"+base_1+"р"+base_2+"а" ;
                         GSg Neuter => "б"+base_1+"р"+base_2+"о" ;
                         GPl => "б"+base_1+"р"+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => "б"+base_1+"р"+base_2+"иот" ;
                                   GSg Fem => "б"+base_1+"р"+base_2+"ата" ;
                                   GSg Neuter => "б"+base_1+"р"+base_2+"ото" ;
                                   GPl => "б"+base_1+"р"+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => "б"+base_1+"р"+base_2+"иов" ;
                                GSg Fem => "б"+base_1+"р"+base_2+"ава" ;
                                GSg Neuter => "б"+base_1+"р"+base_2+"ово" ;
                                GPl => "б"+base_1+"р"+base_2+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => "б"+base_1+"р"+base_2+"ион" ;
                              GSg Fem => "б"+base_1+"р"+base_2+"ана" ;
                              GSg Neuter => "б"+base_1+"р"+base_2+"оно" ;
                              GPl => "б"+base_1+"р"+base_2+"ине"
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA047"
  } ;

mkA048 : Str -> A ;
mkA048 base =
  case base of {
    base_1+"ти" => lin A
      { s = table {
              Indef => table {
                         GSg Masc => base_1+"ти" ;
                         GSg Fem => base_1+"а" ;
                         GSg Neuter => base_1+"о" ;
                         GPl => base_1+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => base_1+"иот" ;
                                   GSg Fem => base_1+"ата" ;
                                   GSg Neuter => base_1+"ото" ;
                                   GPl => base_1+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => base_1+"иов" ;
                                GSg Fem => base_1+"ава" ;
                                GSg Neuter => base_1+"ово" ;
                                GPl => base_1+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => base_1+"ион" ;
                              GSg Fem => base_1+"ана" ;
                              GSg Neuter => base_1+"оно" ;
                              GPl => base_1+"ине"
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA048"
  } ;

mkA049 : Str -> A ;
mkA049 base =
  case base of {
    "б"+base_1+"г"+base_2@?+"в"+base_3@(?+?)+"и" => lin A
      { s = table {
              Indef => table {
                         GSg Masc => "б"+base_1+"г"+base_2+"в"+base_3+"и" ;
                         GSg Fem => "дир"+base_1+"кт"+base_2+"р"+base_3+"а" ;
                         GSg Neuter => "дир"+base_1+"кт"+base_2+"р"+base_3+"о" ;
                         GPl => "дир"+base_1+"кт"+base_2+"р"+base_3+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => "дир"+base_1+"кт"+base_2+"р"+base_3+"иот" ;
                                   GSg Fem => "дир"+base_1+"кт"+base_2+"р"+base_3+"ата" ;
                                   GSg Neuter => "дир"+base_1+"кт"+base_2+"р"+base_3+"ото" ;
                                   GPl => "дир"+base_1+"кт"+base_2+"р"+base_3+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => "дир"+base_1+"кт"+base_2+"р"+base_3+"иов" ;
                                GSg Fem => "дир"+base_1+"кт"+base_2+"р"+base_3+"ава" ;
                                GSg Neuter => "дир"+base_1+"кт"+base_2+"р"+base_3+"ово" ;
                                GPl => "дир"+base_1+"кт"+base_2+"р"+base_3+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => "дир"+base_1+"кт"+base_2+"р"+base_3+"ион" ;
                              GSg Fem => "дир"+base_1+"кт"+base_2+"р"+base_3+"ана" ;
                              GSg Neuter => "дир"+base_1+"кт"+base_2+"р"+base_3+"оно" ;
                              GPl => "дир"+base_1+"кт"+base_2+"р"+base_3+"ине"
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA049"
  } ;

mkA050 : Str -> A ;
mkA050 base =
  case base of {
    "н"+base_1+base_2@(?+?+?+?+?+?+?) => lin A
      { s = table {
              Indef => table {
                         GSg Masc => "н"+base_1+base_2 ;
                         GSg Fem => "р"+base_1+"з"+base_2+"а" ;
                         GSg Neuter => "р"+base_1+"з"+base_2+"о" ;
                         GPl => "р"+base_1+"з"+base_2+"и"
                       } ;
              Def Unspecified => table {
                                   GSg Masc => "р"+base_1+"з"+base_2+"иот" ;
                                   GSg Fem => "р"+base_1+"з"+base_2+"ата" ;
                                   GSg Neuter => "р"+base_1+"з"+base_2+"ото" ;
                                   GPl => "р"+base_1+"з"+base_2+"ите"
                                 } ;
              Def Proximal => table {
                                GSg Masc => "р"+base_1+"з"+base_2+"иов" ;
                                GSg Fem => "р"+base_1+"з"+base_2+"ава" ;
                                GSg Neuter => "р"+base_1+"з"+base_2+"ово" ;
                                GPl => "р"+base_1+"з"+base_2+"иве"
                              } ;
              Def Distal => table {
                              GSg Masc => "р"+base_1+"з"+base_2+"ион" ;
                              GSg Fem => "р"+base_1+"з"+base_2+"ана" ;
                              GSg Neuter => "р"+base_1+"з"+base_2+"оно" ;
                              GPl => "р"+base_1+"з"+base_2+"ине"
                            }
            } ;
        adverb = nonExist
      };
    _ => error "Can't apply paradigm mkA050"
  } ;

mkAdv : Str -> Adv ;
mkAdv base_1 =
  lin Adv
  { s = base_1
  } ;

mkPron : (_,_,_,_,_,_,_,_,_,_,_,_,_,_,_ : Str) -> Pron =
  \f1,f2,f3,f4,f5,f6,f7,f8,f9,f10,f11,f12,f13,f14,f15 ->
      { s = table {
              RSubj => f1 ;
              RObj Acc => f2 ;
              RObj Dat => f4 ;
              RPrep => f6
            } ;
        clitic =
            table {
              Acc => f3 ;
              Dat => f5
            } ;
        poss = 
            table {
              Indef => table {
                         GSg Masc   => f7 ;
                         GSg Fem    => f9 ;
                         GSg Neuter => f11 ;
                         GPl        => f13
                       } ;
              Def _ => table {
                         GSg Masc   => f8 ;
                         GSg Fem    => f10 ;
                         GSg Neuter => f12 ;
                         GPl        => f14
                       }
            } ;
        poss_clitic = f15
      } ;

}
