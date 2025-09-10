resource MorphoFao = open CatFao, ResFao, Predef in {

oper

mkN001 : Str -> N ;
mkN001 base =
  case base of {
    base_1+"i" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"i" ;
                                 Acc => base_1+"a" ;
                                 Dat => base_1+"a" ;
                                 Gen => base_1+"a"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ar" ;
                                 Acc => base_1+"ar" ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"in" ;
                               Acc => base_1+"an" ;
                               Dat => base_1+"anum" ;
                               Gen => base_1+"ans"
                             } ;
                       Pl => table {
                               Nom => base_1+"arnir" ;
                               Acc => base_1+"arnar" ;
                               Dat => base_1+"unum" ;
                               Gen => base_1+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN001"
  } ;

mkN002 : Str -> N ;
mkN002 base =
  case base of {
    base_1 => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1 ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ð" ;
                                 Acc => base_1+"ð" ;
                                 Dat => base_1+"num" ;
                                 Gen => base_1+"sins"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN002"
  } ;

mkN003 : Str -> N ;
mkN003 base =
  case base of {
    base_1+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ur" ;
                                 Acc => base_1 ;
                                 Dat => base_1+"i" ;
                                 Gen => base_1
                               } ;
                         Pl => table {
                                 Nom => base_1+"ar" ;
                                 Acc => base_1+"ar" ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"urin" ;
                               Acc => base_1+"in" ;
                               Dat => base_1+"inum" ;
                               Gen => base_1+"ins"
                             } ;
                       Pl => table {
                               Nom => base_1+"arnir" ;
                               Acc => base_1+"arnar" ;
                               Dat => base_1+"unum" ;
                               Gen => base_1+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN003"
  } ;

mkN004 : Str -> N ;
mkN004 base =
  case base of {
    base_1+"a"+base_2@("tn"|"rn"|"rp"|"lv"|"vn"|"ld"|"rk"|"rð"|"rv"|"gn"|?) => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2 ;
                                 Acc => base_1+"a"+base_2 ;
                                 Dat => base_1+"a"+base_2+"i" ;
                                 Gen => base_1+"a"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2 ;
                                 Acc => base_1+"ø"+base_2 ;
                                 Dat => base_1+"ø"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"a"+base_2+"ið" ;
                               Acc => base_1+"a"+base_2+"ið" ;
                               Dat => base_1+"a"+base_2+"inum" ;
                               Gen => base_1+"a"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"ø"+base_2+"ini" ;
                               Acc => base_1+"ø"+base_2+"ini" ;
                               Dat => base_1+"ø"+base_2+"unum" ;
                               Gen => base_1+"a"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN004"
  } ;

mkN005 : Str -> N ;
mkN005 base =
  case base of {
    base_1+"a" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a" ;
                                 Acc => base_1+"u" ;
                                 Dat => base_1+"u" ;
                                 Gen => base_1+"u"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ur" ;
                                 Acc => base_1+"ur" ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"an" ;
                               Acc => base_1+"una" ;
                               Dat => base_1+"uni" ;
                               Gen => base_1+"unnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"urnar" ;
                               Acc => base_1+"urnar" ;
                               Dat => base_1+"unum" ;
                               Gen => base_1+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN005"
  } ;

mkN006 : Str -> N ;
mkN006 base =
  case base of {
    base_1 => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1 ;
                                 Gen => base_1+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ar" ;
                                 Acc => base_1+"ar" ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"in" ;
                               Acc => base_1+"ina" ;
                               Dat => base_1+"ini" ;
                               Gen => base_1+"arinnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"arnar" ;
                               Acc => base_1+"arnar" ;
                               Dat => base_1+"unum" ;
                               Gen => base_1+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN006"
  } ;

mkN007 : Str -> N ;
mkN007 base =
  case base of {
    base_1 => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1 ;
                                 Gen => base_1+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"in" ;
                                 Acc => base_1+"ina" ;
                                 Dat => base_1+"ini" ;
                                 Gen => base_1+"arinnar"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN007"
  } ;

mkN008 : Str -> N ;
mkN008 base =
  case base of {
    base_1 => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1+"i" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ið" ;
                                 Acc => base_1+"ið" ;
                                 Dat => base_1+"inum" ;
                                 Gen => base_1+"sins"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"in" ;
                               Acc => base_1+"in" ;
                               Dat => base_1+"inum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN008"
  } ;

mkN009 : Str -> N ;
mkN009 base =
  case base of {
    base_1+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ur" ;
                                 Acc => base_1 ;
                                 Dat => base_1+"i" ;
                                 Gen => base_1+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ir" ;
                                 Acc => base_1+"ir" ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"urin" ;
                               Acc => base_1+"in" ;
                               Dat => base_1+"inum" ;
                               Gen => base_1+"arins"
                             } ;
                       Pl => table {
                               Nom => base_1+"irnir" ;
                               Acc => base_1+"irnar" ;
                               Dat => base_1+"unum" ;
                               Gen => base_1+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN009"
  } ;

mkN010 : Str -> N ;
mkN010 base =
  case base of {
    base_1+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ur" ;
                                 Acc => base_1 ;
                                 Dat => base_1+"i" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ar" ;
                                 Acc => base_1+"ar" ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"urin" ;
                               Acc => base_1+"in" ;
                               Dat => base_1+"inum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"arnir" ;
                               Acc => base_1+"arnar" ;
                               Dat => base_1+"unum" ;
                               Gen => base_1+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN010"
  } ;

mkN011 : Str -> N ;
mkN011 base =
  case base of {
    base_1+"u"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"u"+base_2 ;
                                 Acc => base_1+"u"+base_2 ;
                                 Dat => base_1+"u"+base_2 ;
                                 Gen => base_1+"a"+base_2
                               } ;
                         Pl => table {
                                 Nom => base_1+base_2+"ar" ;
                                 Acc => base_1+base_2+"ar" ;
                                 Dat => base_1+base_2+"um" ;
                                 Gen => base_1+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"u"+base_2+"in" ;
                               Acc => base_1+base_2+"ina" ;
                               Dat => base_1+base_2+"ini" ;
                               Gen => base_1+"a"+base_2+"innar"
                             } ;
                       Pl => table {
                               Nom => base_1+base_2+"arnar" ;
                               Acc => base_1+base_2+"arnar" ;
                               Dat => base_1+base_2+"unum" ;
                               Gen => base_1+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN011"
  } ;

mkN012 : Str -> N ;
mkN012 base =
  case base of {
    base_1+"a" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a" ;
                                 Acc => base_1+"u" ;
                                 Dat => base_1+"u" ;
                                 Gen => base_1+"u"
                               } ;
                         Pl => table {
                                 Nom => base_1+"an" ;
                                 Acc => base_1+"una" ;
                                 Dat => base_1+"uni" ;
                                 Gen => base_1+"unnar"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN012"
  } ;

mkN013 : Str -> N ;
mkN013 base =
  case base of {
    "ær" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => "ær" ;
                                 Acc => "ær" ;
                                 Dat => "ær" ;
                                 Gen => "ær"
                               } ;
                         Pl => table {
                                 Nom => "ær" ;
                                 Acc => "ær" ;
                                 Dat => "óm" ;
                                 Gen => "áa"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => "ærin" ;
                               Acc => "ærina" ;
                               Dat => "ærini" ;
                               Gen => "ærinnar"
                             } ;
                       Pl => table {
                               Nom => "ærnar" ;
                               Acc => "ærnar" ;
                               Dat => "ónum" ;
                               Gen => "áanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN013"
  } ;

mkN014 : Str -> N ;
mkN014 base =
  case base of {
    base_1+"i" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"i" ;
                                 Acc => base_1+"i" ;
                                 Dat => base_1+"i" ;
                                 Gen => base_1+"is"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ir" ;
                                 Acc => base_1+"ir" ;
                                 Dat => base_1+"jum" ;
                                 Gen => base_1+"ja"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ið" ;
                               Acc => base_1+"ið" ;
                               Dat => base_1+"inum" ;
                               Gen => base_1+"isins"
                             } ;
                       Pl => table {
                               Nom => base_1+"ini" ;
                               Acc => base_1+"ini" ;
                               Dat => base_1+"junum" ;
                               Gen => base_1+"janna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN014"
  } ;

mkN015 : Str -> N ;
mkN015 base =
  case base of {
    base_1 => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1 ;
                                 Gen => base_1+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ir" ;
                                 Acc => base_1+"ir" ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"in" ;
                               Acc => base_1+"ina" ;
                               Dat => base_1+"ini" ;
                               Gen => base_1+"arinnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"irnar" ;
                               Acc => base_1+"irnar" ;
                               Dat => base_1+"unum" ;
                               Gen => base_1+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN015"
  } ;

mkN016 : Str -> N ;
mkN016 base =
  case base of {
    base_1+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ur" ;
                                 Acc => base_1 ;
                                 Dat => base_1+"i" ;
                                 Gen => base_1+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"urin" ;
                                 Acc => base_1+"in" ;
                                 Dat => base_1+"inum" ;
                                 Gen => base_1+"arins"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN016"
  } ;

mkN017 : Str -> N ;
mkN017 base =
  case base of {
    base_1+"a"+base_2@(?+?)+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"ur" ;
                                 Acc => base_1+"a"+base_2 ;
                                 Dat => base_1+"a"+base_2+"i" ;
                                 Gen => base_1+"a"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"a"+base_2+"ar" ;
                                 Acc => base_1+"a"+base_2+"ar" ;
                                 Dat => base_1+"o"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"a"+base_2+"urin" ;
                               Acc => base_1+"a"+base_2+"in" ;
                               Dat => base_1+"a"+base_2+"inum" ;
                               Gen => base_1+"a"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+"arnir" ;
                               Acc => base_1+"a"+base_2+"arnar" ;
                               Dat => base_1+"o"+base_2+"unum" ;
                               Gen => base_1+"a"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN017"
  } ;

mkN018 : Str -> N ;
mkN018 base =
  case base of {
    base_1+"ø"+base_2@("v"|"k"|(?+?)) => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ø"+base_2 ;
                                 Acc => base_1+"ø"+base_2 ;
                                 Dat => base_1+"ø"+base_2 ;
                                 Gen => base_1+"a"+base_2+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"a"+base_2+"ir" ;
                                 Acc => base_1+"a"+base_2+"ir" ;
                                 Dat => base_1+"ø"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ø"+base_2+"in" ;
                               Acc => base_1+"ø"+base_2+"ina" ;
                               Dat => base_1+"ø"+base_2+"ini" ;
                               Gen => base_1+"a"+base_2+"arinnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+"irnar" ;
                               Acc => base_1+"a"+base_2+"irnar" ;
                               Dat => base_1+"ø"+base_2+"unum" ;
                               Gen => base_1+"a"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN018"
  } ;

mkN019 : Str -> N ;
mkN019 base =
  case base of {
    base_1 => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1+"i" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ið" ;
                               Acc => base_1+"ið" ;
                               Dat => base_1+"inum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"ini" ;
                               Acc => base_1+"ini" ;
                               Dat => base_1+"unum" ;
                               Gen => base_1+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN019"
  } ;

mkN020 : Str -> N ;
mkN020 base =
  case base of {
    "a"+base_1 => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => "a"+base_1 ;
                                 Acc => "a"+base_1 ;
                                 Dat => "a"+base_1+"i" ;
                                 Gen => "a"+base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => "ø"+base_1 ;
                                 Acc => "ø"+base_1 ;
                                 Dat => "ø"+base_1+"um" ;
                                 Gen => "a"+base_1+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => "a"+base_1+"ið" ;
                               Acc => "a"+base_1+"ið" ;
                               Dat => "a"+base_1+"inum" ;
                               Gen => "a"+base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => "ø"+base_1+"ini" ;
                               Acc => "ø"+base_1+"ini" ;
                               Dat => "ø"+base_1+"unum" ;
                               Gen => "a"+base_1+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN020"
  } ;

mkN021 : Str -> N ;
mkN021 base =
  case base of {
    base_1+"i" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"i" ;
                                 Acc => base_1+"a" ;
                                 Dat => base_1+"a" ;
                                 Gen => base_1+"a"
                               } ;
                         Pl => table {
                                 Nom => base_1+"in" ;
                                 Acc => base_1+"an" ;
                                 Dat => base_1+"anum" ;
                                 Gen => base_1+"ans"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN021"
  } ;

mkN022 : Str -> N ;
mkN022 base =
  case base of {
    base_1+"æ" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"æ" ;
                                 Acc => base_1+"æ" ;
                                 Dat => base_1+"æi" ;
                                 Gen => base_1+"æs"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø" ;
                                 Acc => base_1+"ø" ;
                                 Dat => base_1+"øum" ;
                                 Gen => base_1+"æa"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"æið" ;
                               Acc => base_1+"æið" ;
                               Dat => base_1+"æinum" ;
                               Gen => base_1+"æsins"
                             } ;
                       Pl => table {
                               Nom => base_1+"øini" ;
                               Acc => base_1+"øini" ;
                               Dat => base_1+"øunum" ;
                               Gen => base_1+"æanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN022"
  } ;

mkN023 : Str -> N ;
mkN023 base =
  case base of {
    "a"+base_1+"u"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => "a"+base_1+"u"+base_2 ;
                                 Acc => "a"+base_1+"u"+base_2 ;
                                 Dat => "a"+base_1+base_2+"i" ;
                                 Gen => "a"+base_1+"u"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => "a"+base_1+base_2+"ar" ;
                                 Acc => "a"+base_1+base_2+"ar" ;
                                 Dat => "ø"+base_1+base_2+"um" ;
                                 Gen => "a"+base_1+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => "a"+base_1+"u"+base_2+"in" ;
                               Acc => "a"+base_1+"u"+base_2+"in" ;
                               Dat => "a"+base_1+base_2+"inum" ;
                               Gen => "a"+base_1+"u"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => "a"+base_1+base_2+"arnir" ;
                               Acc => "a"+base_1+base_2+"arnar" ;
                               Dat => "ø"+base_1+base_2+"unum" ;
                               Gen => "a"+base_1+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN023"
  } ;

mkN024 : Str -> N ;
mkN024 base =
  case base of {
    base_1+"u"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"u"+base_2 ;
                                 Acc => base_1+"u"+base_2 ;
                                 Dat => base_1+base_2+"i" ;
                                 Gen => base_1+"u"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+base_2+"ar" ;
                                 Acc => base_1+base_2+"ar" ;
                                 Dat => base_1+base_2+"um" ;
                                 Gen => base_1+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"u"+base_2+"in" ;
                               Acc => base_1+"u"+base_2+"in" ;
                               Dat => base_1+base_2+"inum" ;
                               Gen => base_1+"u"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+base_2+"arnir" ;
                               Acc => base_1+base_2+"arnar" ;
                               Dat => base_1+base_2+"unum" ;
                               Gen => base_1+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN024"
  } ;

mkN025 : Str -> N ;
mkN025 base =
  case base of {
    base_1 => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1 ;
                                 Gen => base_1
                               } ;
                         Pl => table {
                                 Nom => base_1+"n" ;
                                 Acc => base_1+"na" ;
                                 Dat => base_1+"ni" ;
                                 Gen => base_1+"nnar"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN025"
  } ;

mkN026 : Str -> N ;
mkN026 base =
  case base of {
    base_1 => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1+"i" ;
                                 Gen => base_1
                               } ;
                         Pl => table {
                                 Nom => base_1+"ið" ;
                                 Acc => base_1+"ið" ;
                                 Dat => base_1+"inum" ;
                                 Gen => base_1+"ins"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN026"
  } ;

mkN027 : Str -> N ;
mkN027 base =
  case base of {
    base_1 => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1+"i" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ið" ;
                               Acc => base_1+"ið" ;
                               Dat => base_1+"num" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"ini" ;
                               Acc => base_1+"ini" ;
                               Dat => base_1+"unum" ;
                               Gen => base_1+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN027"
  } ;

mkN028 : Str -> N ;
mkN028 base =
  case base of {
    base_1 => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1+"i" ;
                                 Gen => base_1
                               } ;
                         Pl => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ið" ;
                               Acc => base_1+"ið" ;
                               Dat => base_1+"inum" ;
                               Gen => base_1+"ins"
                             } ;
                       Pl => table {
                               Nom => base_1+"ini" ;
                               Acc => base_1+"ini" ;
                               Dat => base_1+"unum" ;
                               Gen => base_1+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN028"
  } ;

mkN029 : Str -> N ;
mkN029 base =
  case base of {
    base_1+"kur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"kur" ;
                                 Acc => base_1+"k" ;
                                 Dat => base_1+"ki" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"kar" ;
                                 Acc => base_1+"kar" ;
                                 Dat => base_1+"kum" ;
                                 Gen => base_1+"ka"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"kurin" ;
                               Acc => base_1+"kin" ;
                               Dat => base_1+"kinum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"karnir" ;
                               Acc => base_1+"karnar" ;
                               Dat => base_1+"kunum" ;
                               Gen => base_1+"kanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN029"
  } ;

mkN030 : Str -> N ;
mkN030 base =
  case base of {
    base_1+"u"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"u"+base_2 ;
                                 Acc => base_1+"u"+base_2 ;
                                 Dat => base_1+base_2+"i" ;
                                 Gen => base_1+"u"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"u"+base_2+"in" ;
                                 Acc => base_1+"u"+base_2+"in" ;
                                 Dat => base_1+base_2+"inum" ;
                                 Gen => base_1+"u"+base_2+"sins"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN030"
  } ;

mkN031 : Str -> N ;
mkN031 base =
  case base of {
    base_1+"i" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"i" ;
                                 Acc => base_1+"i" ;
                                 Dat => base_1+"i" ;
                                 Gen => base_1+"is"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ir" ;
                                 Acc => base_1+"ir" ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ið" ;
                               Acc => base_1+"ið" ;
                               Dat => base_1+"inum" ;
                               Gen => base_1+"isins"
                             } ;
                       Pl => table {
                               Nom => base_1+"ini" ;
                               Acc => base_1+"ini" ;
                               Dat => base_1+"unum" ;
                               Gen => base_1+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN031"
  } ;

mkN032 : Str -> N ;
mkN032 base =
  case base of {
    base_1+"aður" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"aður" ;
                                 Acc => base_1+"ann" ;
                                 Dat => base_1+"anni" ;
                                 Gen => base_1+"ans"
                               } ;
                         Pl => table {
                                 Nom => base_1+"enn" ;
                                 Acc => base_1+"enn" ;
                                 Dat => base_1+"onnum" ;
                                 Gen => base_1+"anna"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"aðurin" ;
                               Acc => base_1+"annin" ;
                               Dat => base_1+"annum" ;
                               Gen => base_1+"ansins"
                             } ;
                       Pl => table {
                               Nom => base_1+"enninir" ;
                               Acc => base_1+"enninar" ;
                               Dat => base_1+"onnunum" ;
                               Gen => base_1+"annanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN032"
  } ;

mkN033 : Str -> N ;
mkN033 base =
  case base of {
    base_1+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ur" ;
                                 Acc => base_1+"ur" ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"na"
                               } ;
                         Pl => table {
                                 Nom => base_1+"uni" ;
                                 Acc => base_1+"uni" ;
                                 Dat => base_1+"unum" ;
                                 Gen => base_1+"nanna"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN033"
  } ;

mkN034 : Str -> N ;
mkN034 base =
  case base of {
    base_1+"æ"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"æ"+base_2 ;
                                 Acc => base_1+"æ"+base_2 ;
                                 Dat => base_1+"æ"+base_2+"i" ;
                                 Gen => base_1+"æ"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2 ;
                                 Acc => base_1+"ø"+base_2 ;
                                 Dat => base_1+"ø"+base_2+"um" ;
                                 Gen => base_1+"æ"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"æ"+base_2+"ið" ;
                               Acc => base_1+"æ"+base_2+"ið" ;
                               Dat => base_1+"æ"+base_2+"inum" ;
                               Gen => base_1+"æ"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"ø"+base_2+"ini" ;
                               Acc => base_1+"ø"+base_2+"ini" ;
                               Dat => base_1+"ø"+base_2+"unum" ;
                               Gen => base_1+"æ"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN034"
  } ;

mkN035 : Str -> N ;
mkN035 base =
  case base of {
    base_1+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ur" ;
                                 Acc => base_1 ;
                                 Dat => base_1+"i" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"urin" ;
                                 Acc => base_1+"in" ;
                                 Dat => base_1+"inum" ;
                                 Gen => base_1+"sins"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN035"
  } ;

mkN036 : Str -> N ;
mkN036 base =
  case base of {
    base_1+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ur" ;
                                 Acc => base_1 ;
                                 Dat => base_1+"i" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ir" ;
                                 Acc => base_1+"ir" ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"urin" ;
                               Acc => base_1+"in" ;
                               Dat => base_1+"inum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"irnir" ;
                               Acc => base_1+"irnar" ;
                               Dat => base_1+"unum" ;
                               Gen => base_1+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN036"
  } ;

mkN037 : Str -> N ;
mkN037 base =
  case base of {
    base_1+"pur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"pur" ;
                                 Acc => base_1+"p" ;
                                 Dat => base_1+"pi" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"par" ;
                                 Acc => base_1+"par" ;
                                 Dat => base_1+"pum" ;
                                 Gen => base_1+"pa"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"purin" ;
                               Acc => base_1+"pin" ;
                               Dat => base_1+"pinum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"parnir" ;
                               Acc => base_1+"parnar" ;
                               Dat => base_1+"punum" ;
                               Gen => base_1+"panna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN037"
  } ;

mkN038 : Str -> N ;
mkN038 base =
  case base of {
    base_1+"a"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2 ;
                                 Acc => base_1+"a"+base_2 ;
                                 Dat => base_1+base_2+"i" ;
                                 Gen => base_1+"a"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+base_2+"ar" ;
                                 Acc => base_1+base_2+"ar" ;
                                 Dat => base_1+base_2+"um" ;
                                 Gen => base_1+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"a"+base_2+"in" ;
                               Acc => base_1+"a"+base_2+"in" ;
                               Dat => base_1+base_2+"inum" ;
                               Gen => base_1+"a"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+base_2+"arnir" ;
                               Acc => base_1+base_2+"arnar" ;
                               Dat => base_1+base_2+"unum" ;
                               Gen => base_1+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN038"
  } ;

mkN039 : Str -> N ;
mkN039 base =
  case base of {
    base_1+base_2@?+"l" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+base_2+"l" ;
                                 Acc => base_1+base_2+"l" ;
                                 Dat => base_1+"l"+base_2 ;
                                 Gen => base_1+base_2+"ls"
                               } ;
                         Pl => table {
                                 Nom => base_1+base_2+"lin" ;
                                 Acc => base_1+base_2+"lin" ;
                                 Dat => base_1+"l"+base_2+"num" ;
                                 Gen => base_1+base_2+"lsins"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN039"
  } ;

mkN040 : Str -> N ;
mkN040 base =
  case base of {
    base_1+"i" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"i" ;
                                 Acc => base_1+"ja" ;
                                 Dat => base_1+"ja" ;
                                 Gen => base_1+"ja"
                               } ;
                         Pl => table {
                                 Nom => base_1+"jar" ;
                                 Acc => base_1+"jar" ;
                                 Dat => base_1+"jum" ;
                                 Gen => base_1+"ja"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"in" ;
                               Acc => base_1+"jan" ;
                               Dat => base_1+"janum" ;
                               Gen => base_1+"jans"
                             } ;
                       Pl => table {
                               Nom => base_1+"jarnir" ;
                               Acc => base_1+"jarnar" ;
                               Dat => base_1+"junum" ;
                               Gen => base_1+"janna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN040"
  } ;

mkN041 : Str -> N ;
mkN041 base =
  case base of {
    base_1+"ógv" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ógv" ;
                                 Acc => base_1+"ógv" ;
                                 Dat => base_1+"ógv" ;
                                 Gen => base_1+"óar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"øur" ;
                                 Acc => base_1+"øur" ;
                                 Dat => base_1+"óm" ;
                                 Gen => base_1+"ógva"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ógvin" ;
                               Acc => base_1+"ónna" ;
                               Dat => base_1+"ónni" ;
                               Gen => base_1+"óarinnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"ørnar" ;
                               Acc => base_1+"ørnar" ;
                               Dat => base_1+"ónum" ;
                               Gen => base_1+"ógvanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN041"
  } ;

mkN042 : Str -> N ;
mkN042 base =
  case base of {
    base_1+"u"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"u"+base_2 ;
                                 Acc => base_1+"u"+base_2 ;
                                 Dat => base_1+base_2+"i" ;
                                 Gen => base_1+"u"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"u"+base_2 ;
                                 Acc => base_1+"u"+base_2 ;
                                 Dat => base_1+base_2+"um" ;
                                 Gen => base_1+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+base_2+"ið" ;
                               Acc => base_1+base_2+"ið" ;
                               Dat => base_1+base_2+"inum" ;
                               Gen => base_1+"u"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+base_2+"ini" ;
                               Acc => base_1+base_2+"ini" ;
                               Dat => base_1+base_2+"unum" ;
                               Gen => base_1+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN042"
  } ;

mkN043 : Str -> N ;
mkN043 base =
  case base of {
    base_1+"nur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"nur" ;
                                 Acc => base_1+"n" ;
                                 Dat => base_1+"ni" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"nar" ;
                                 Acc => base_1+"nar" ;
                                 Dat => base_1+"num" ;
                                 Gen => base_1+"na"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"nurin" ;
                               Acc => base_1+"nin" ;
                               Dat => base_1+"ninum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"narnir" ;
                               Acc => base_1+"narnar" ;
                               Dat => base_1+"nunum" ;
                               Gen => base_1+"nanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN043"
  } ;

mkN044 : Str -> N ;
mkN044 base =
  case base of {
    base_1+"a" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a" ;
                                 Acc => base_1+"a" ;
                                 Dat => base_1+"a" ;
                                 Gen => base_1+"a"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ur" ;
                                 Acc => base_1+"ur" ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"na"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"að" ;
                               Acc => base_1+"að" ;
                               Dat => base_1+"anum" ;
                               Gen => base_1+"ans"
                             } ;
                       Pl => table {
                               Nom => base_1+"uni" ;
                               Acc => base_1+"uni" ;
                               Dat => base_1+"unum" ;
                               Gen => base_1+"nanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN044"
  } ;

mkN045 : Str -> N ;
mkN045 base =
  case base of {
    base_1+"i"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"i"+base_2 ;
                                 Acc => base_1+"i"+base_2 ;
                                 Dat => base_1+base_2+"i" ;
                                 Gen => base_1+"i"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+base_2+"ar" ;
                                 Acc => base_1+base_2+"ar" ;
                                 Dat => base_1+base_2+"um" ;
                                 Gen => base_1+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"i"+base_2+"in" ;
                               Acc => base_1+"i"+base_2+"in" ;
                               Dat => base_1+base_2+"inum" ;
                               Gen => base_1+"i"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+base_2+"arnir" ;
                               Acc => base_1+base_2+"arnar" ;
                               Dat => base_1+base_2+"unum" ;
                               Gen => base_1+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN045"
  } ;

mkN046 : Str -> N ;
mkN046 base =
  case base of {
    base_1 => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1+"i" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1+"jum" ;
                                 Gen => base_1+"ja"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ið" ;
                               Acc => base_1+"ið" ;
                               Dat => base_1+"inum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"ini" ;
                               Acc => base_1+"ini" ;
                               Dat => base_1+"junum" ;
                               Gen => base_1+"janna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN046"
  } ;

mkN047 : Str -> N ;
mkN047 base =
  case base of {
    base_1+"ú"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ú"+base_2 ;
                                 Acc => base_1+"ú"+base_2 ;
                                 Dat => base_1+"ú"+base_2 ;
                                 Gen => base_1+"ú"+base_2+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ý"+base_2 ;
                                 Acc => base_1+"ý"+base_2 ;
                                 Dat => base_1+"ú"+base_2+"um" ;
                                 Gen => base_1+"ú"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ú"+base_2+"in" ;
                               Acc => base_1+"ú"+base_2+"ina" ;
                               Dat => base_1+"ú"+base_2+"ini" ;
                               Gen => base_1+"ú"+base_2+"arinnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"ý"+base_2+"nar" ;
                               Acc => base_1+"ý"+base_2+"nar" ;
                               Dat => base_1+"ú"+base_2+"unum" ;
                               Gen => base_1+"ú"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN047"
  } ;

mkN048 : Str -> N ;
mkN048 base =
  case base of {
    base_1+"tur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"tur" ;
                                 Acc => base_1+"t" ;
                                 Dat => base_1+"ti" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"tir" ;
                                 Acc => base_1+"tir" ;
                                 Dat => base_1+"tum" ;
                                 Gen => base_1+"ta"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"turin" ;
                               Acc => base_1+"tin" ;
                               Dat => base_1+"tinum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"tirnir" ;
                               Acc => base_1+"tirnar" ;
                               Dat => base_1+"tunum" ;
                               Gen => base_1+"tanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN048"
  } ;

mkN049 : Str -> N ;
mkN049 base =
  case base of {
    base_1 => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1+"i" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ið" ;
                                 Acc => base_1+"ið" ;
                                 Dat => base_1+"num" ;
                                 Gen => base_1+"sins"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN049"
  } ;

mkN050 : Str -> N ;
mkN050 base =
  case base of {
    base_1+"á"+base_2@("ð"|(?+?))+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"á"+base_2+"ur" ;
                                 Acc => base_1+"á"+base_2 ;
                                 Dat => base_1+"á"+base_2+"i" ;
                                 Gen => base_1+"á"+base_2+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"æ"+base_2+"ir" ;
                                 Acc => base_1+"æ"+base_2+"ir" ;
                                 Dat => base_1+"á"+base_2+"um" ;
                                 Gen => base_1+"á"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"á"+base_2+"urin" ;
                               Acc => base_1+"á"+base_2+"in" ;
                               Dat => base_1+"á"+base_2+"inum" ;
                               Gen => base_1+"á"+base_2+"arins"
                             } ;
                       Pl => table {
                               Nom => base_1+"æ"+base_2+"irnir" ;
                               Acc => base_1+"æ"+base_2+"irnar" ;
                               Dat => base_1+"á"+base_2+"unum" ;
                               Gen => base_1+"á"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN050"
  } ;

mkN051 : Str -> N ;
mkN051 base =
  case base of {
    base_1+"o"+base_2@(?+?) => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"o"+base_2 ;
                                 Acc => base_1+"o"+base_2 ;
                                 Dat => base_1+"o"+base_2 ;
                                 Gen => base_1+"a"+base_2+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"a"+base_2+"ir" ;
                                 Acc => base_1+"a"+base_2+"ir" ;
                                 Dat => base_1+"o"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"o"+base_2+"in" ;
                               Acc => base_1+"o"+base_2+"ina" ;
                               Dat => base_1+"o"+base_2+"ini" ;
                               Gen => base_1+"a"+base_2+"arinnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+"irnar" ;
                               Acc => base_1+"a"+base_2+"irnar" ;
                               Dat => base_1+"o"+base_2+"unum" ;
                               Gen => base_1+"a"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN051"
  } ;

mkN052 : Str -> N ;
mkN052 base =
  case base of {
    base_1+"a"+base_2@?+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"ur" ;
                                 Acc => base_1+"a"+base_2 ;
                                 Dat => base_1+"a"+base_2+"i" ;
                                 Gen => base_1+"a"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"a"+base_2+"ir" ;
                                 Acc => base_1+"a"+base_2+"ir" ;
                                 Dat => base_1+"ø"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"a"+base_2+"urin" ;
                               Acc => base_1+"a"+base_2+"in" ;
                               Dat => base_1+"a"+base_2+"inum" ;
                               Gen => base_1+"a"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+"irnir" ;
                               Acc => base_1+"a"+base_2+"irnar" ;
                               Dat => base_1+"ø"+base_2+"unum" ;
                               Gen => base_1+"a"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN052"
  } ;

mkN053 : Str -> N ;
mkN053 base =
  case base of {
    base_1+"ir" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ir" ;
                                 Acc => base_1+"ir" ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               } ;
                         Pl => table {
                                 Nom => base_1+"irnar" ;
                                 Acc => base_1+"irnar" ;
                                 Dat => base_1+"unum" ;
                                 Gen => base_1+"anna"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN053"
  } ;

mkN054 : Str -> N ;
mkN054 base =
  case base of {
    base_1+"m" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"m" ;
                                 Acc => base_1+"m" ;
                                 Dat => base_1+"mi" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"m" ;
                                 Acc => base_1+"m" ;
                                 Dat => base_1+"mum" ;
                                 Gen => base_1+"ma"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"mið" ;
                               Acc => base_1+"mið" ;
                               Dat => base_1+"minum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"mini" ;
                               Acc => base_1+"mini" ;
                               Dat => base_1+"munum" ;
                               Gen => base_1+"manna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN054"
  } ;

mkN055 : Str -> N ;
mkN055 base =
  case base of {
    base_1+"ó"+base_2@(?+?)+"i" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ó"+base_2+"i" ;
                                 Acc => base_1+"ó"+base_2+"a" ;
                                 Dat => base_1+"ó"+base_2+"a" ;
                                 Gen => base_1+"ó"+base_2+"a"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2+"ur" ;
                                 Acc => base_1+"ø"+base_2+"ur" ;
                                 Dat => base_1+"ó"+base_2+"um" ;
                                 Gen => base_1+"ó"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ó"+base_2+"in" ;
                               Acc => base_1+"ó"+base_2+"an" ;
                               Dat => base_1+"ó"+base_2+"anum" ;
                               Gen => base_1+"ó"+base_2+"ans"
                             } ;
                       Pl => table {
                               Nom => base_1+"ø"+base_2+"urnir" ;
                               Acc => base_1+"ø"+base_2+"urnar" ;
                               Dat => base_1+"ó"+base_2+"unum" ;
                               Gen => base_1+"ó"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN055"
  } ;

mkN056 : Str -> N ;
mkN056 base =
  case base of {
    base_1+"a"+base_2@?+"lur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"lur" ;
                                 Acc => base_1+"a"+base_2+"l" ;
                                 Dat => base_1+"a"+base_2+"li" ;
                                 Gen => base_1+"a"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"a"+base_2+"lar" ;
                                 Acc => base_1+"a"+base_2+"lar" ;
                                 Dat => base_1+"ø"+base_2+"lum" ;
                                 Gen => base_1+"a"+base_2+"la"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"a"+base_2+"lurin" ;
                               Acc => base_1+"a"+base_2+"lin" ;
                               Dat => base_1+"a"+base_2+"linum" ;
                               Gen => base_1+"a"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+"larnir" ;
                               Acc => base_1+"a"+base_2+"larnar" ;
                               Dat => base_1+"ø"+base_2+"lunum" ;
                               Gen => base_1+"a"+base_2+"lanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN056"
  } ;

mkN057 : Str -> N ;
mkN057 base =
  case base of {
    base_1+"ø"+base_2@("ð"|(?+?))+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ø"+base_2+"ur" ;
                                 Acc => base_1+"ø"+base_2 ;
                                 Dat => base_1+"ø"+base_2+"i" ;
                                 Gen => base_1+"a"+base_2+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2+"urin" ;
                                 Acc => base_1+"ø"+base_2+"in" ;
                                 Dat => base_1+"ø"+base_2+"inum" ;
                                 Gen => base_1+"a"+base_2+"arins"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN057"
  } ;

mkN058 : Str -> N ;
mkN058 base =
  case base of {
    base_1+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ur" ;
                                 Acc => base_1+"ur" ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               } ;
                         Pl => table {
                                 Nom => base_1+"urnar" ;
                                 Acc => base_1+"urnar" ;
                                 Dat => base_1+"unum" ;
                                 Gen => base_1+"anna"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN058"
  } ;

mkN059 : Str -> N ;
mkN059 base =
  case base of {
    base_1+"ó"+base_2@?+"i"+base_3@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ó"+base_2+"i"+base_3 ;
                                 Acc => base_1+"ó"+base_2+"u"+base_3 ;
                                 Dat => base_1+"ó"+base_2+"u"+base_3 ;
                                 Gen => base_1+"ó"+base_2+"u"+base_3
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2+"u"+base_3 ;
                                 Acc => base_1+"ø"+base_2+"u"+base_3 ;
                                 Dat => base_1+"ø"+base_2+base_3+"um" ;
                                 Gen => base_1+"ø"+base_2+base_3+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ó"+base_2+"i"+base_3+"in" ;
                               Acc => base_1+"ó"+base_2+"u"+base_3+"in" ;
                               Dat => base_1+"ó"+base_2+"u"+base_3+"num" ;
                               Gen => base_1+"ó"+base_2+"u"+base_3+"ins"
                             } ;
                       Pl => table {
                               Nom => base_1+"ø"+base_2+"u"+base_3+"nir" ;
                               Acc => base_1+"ø"+base_2+"u"+base_3+"nar" ;
                               Dat => base_1+"ø"+base_2+base_3+"unum" ;
                               Gen => base_1+"ø"+base_2+base_3+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN059"
  } ;

mkN060 : Str -> N ;
mkN060 base =
  case base of {
    base_1+"o"+base_2@?+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"o"+base_2+"ur" ;
                                 Acc => base_1+"o"+base_2 ;
                                 Dat => base_1+"y"+base_2+"i" ;
                                 Gen => base_1+"o"+base_2+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"y"+base_2+"ir" ;
                                 Acc => base_1+"y"+base_2+"ir" ;
                                 Dat => base_1+"y"+base_2+"um" ;
                                 Gen => base_1+"o"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"o"+base_2+"urin" ;
                               Acc => base_1+"o"+base_2+"in" ;
                               Dat => base_1+"y"+base_2+"inum" ;
                               Gen => base_1+"o"+base_2+"arins"
                             } ;
                       Pl => table {
                               Nom => base_1+"y"+base_2+"irnir" ;
                               Acc => base_1+"y"+base_2+"irnar" ;
                               Dat => base_1+"y"+base_2+"unum" ;
                               Gen => base_1+"o"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN060"
  } ;

mkN061 : Str -> N ;
mkN061 base =
  case base of {
    base_1+"ó"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ó"+base_2 ;
                                 Acc => base_1+"ó"+base_2 ;
                                 Dat => base_1+"ó"+base_2 ;
                                 Gen => base_1+"ó"+base_2+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2+"ur" ;
                                 Acc => base_1+"ø"+base_2+"ur" ;
                                 Dat => base_1+"ó"+base_2+"um" ;
                                 Gen => base_1+"ó"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ó"+base_2+"in" ;
                               Acc => base_1+"ó"+base_2+"ina" ;
                               Dat => base_1+"ó"+base_2+"ini" ;
                               Gen => base_1+"ó"+base_2+"arinnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"ø"+base_2+"urnar" ;
                               Acc => base_1+"ø"+base_2+"urnar" ;
                               Dat => base_1+"ó"+base_2+"unum" ;
                               Gen => base_1+"ó"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN061"
  } ;

mkN062 : Str -> N ;
mkN062 base =
  case base of {
    base_1+"a"+base_2@?+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"ur" ;
                                 Acc => base_1+"a"+base_2 ;
                                 Dat => base_1+"a"+base_2 ;
                                 Gen => base_1+"a"+base_2+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"a"+base_2+"ir" ;
                                 Acc => base_1+"a"+base_2+"ir" ;
                                 Dat => base_1+"ø"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"a"+base_2+"urin" ;
                               Acc => base_1+"a"+base_2+"in" ;
                               Dat => base_1+"a"+base_2+"num" ;
                               Gen => base_1+"a"+base_2+"arins"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+"irnir" ;
                               Acc => base_1+"a"+base_2+"irnar" ;
                               Dat => base_1+"ø"+base_2+"unum" ;
                               Gen => base_1+"a"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN062"
  } ;

mkN063 : Str -> N ;
mkN063 base =
  case base of {
    base_1+"g" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"g" ;
                                 Acc => base_1+"g" ;
                                 Dat => base_1+"gi" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"gið" ;
                                 Acc => base_1+"gið" ;
                                 Dat => base_1+"ginum" ;
                                 Gen => base_1+"sins"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN063"
  } ;

mkN064 : Str -> N ;
mkN064 base =
  case base of {
    base_1+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ur" ;
                                 Acc => base_1 ;
                                 Dat => base_1+"i" ;
                                 Gen => base_1+"jar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"urin" ;
                                 Acc => base_1+"in" ;
                                 Dat => base_1+"inum" ;
                                 Gen => base_1+"jarins"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN064"
  } ;

mkN065 : Str -> N ;
mkN065 base =
  case base of {
    base_1+"a"+base_2@?+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"ur" ;
                                 Acc => base_1+"a"+base_2 ;
                                 Dat => base_1+"e"+base_2+"i" ;
                                 Gen => base_1+"a"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"a"+base_2+"ar" ;
                                 Acc => base_1+"a"+base_2+"ar" ;
                                 Dat => base_1+"ø"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"a"+base_2+"urin" ;
                               Acc => base_1+"a"+base_2+"in" ;
                               Dat => base_1+"e"+base_2+"num" ;
                               Gen => base_1+"a"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+"arnir" ;
                               Acc => base_1+"a"+base_2+"arnar" ;
                               Dat => base_1+"ø"+base_2+"unum" ;
                               Gen => base_1+"a"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN065"
  } ;

mkN066 : Str -> N ;
mkN066 base =
  case base of {
    base_1+"a"+base_2@("l"|(?+?))+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"ur" ;
                                 Acc => base_1+"a"+base_2 ;
                                 Dat => base_1+"a"+base_2+"i" ;
                                 Gen => base_1+"a"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"a"+base_2+"ar" ;
                                 Acc => base_1+"a"+base_2+"ar" ;
                                 Dat => base_1+"ø"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"a"+base_2+"urin" ;
                               Acc => base_1+"a"+base_2+"in" ;
                               Dat => base_1+"a"+base_2+"inum" ;
                               Gen => base_1+"a"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+"arnir" ;
                               Acc => base_1+"a"+base_2+"arnar" ;
                               Dat => base_1+"ø"+base_2+"unum" ;
                               Gen => base_1+"a"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN066"
  } ;

mkN067 : Str -> N ;
mkN067 base =
  case base of {
    base_1+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ur" ;
                                 Acc => base_1 ;
                                 Dat => base_1+"i" ;
                                 Gen => base_1
                               } ;
                         Pl => table {
                                 Nom => base_1+"ir" ;
                                 Acc => base_1+"ir" ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"urin" ;
                               Acc => base_1+"in" ;
                               Dat => base_1+"inum" ;
                               Gen => base_1+"ins"
                             } ;
                       Pl => table {
                               Nom => base_1+"irnir" ;
                               Acc => base_1+"irnar" ;
                               Dat => base_1+"unum" ;
                               Gen => base_1+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN067"
  } ;

mkN068 : Str -> N ;
mkN068 base =
  case base of {
    base_1+"k" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"k" ;
                                 Acc => base_1+"k" ;
                                 Dat => base_1+"ki" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"k" ;
                                 Acc => base_1+"k" ;
                                 Dat => base_1+"kum" ;
                                 Gen => base_1+"ka"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"kið" ;
                               Acc => base_1+"kið" ;
                               Dat => base_1+"kinum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"kini" ;
                               Acc => base_1+"kini" ;
                               Dat => base_1+"kunum" ;
                               Gen => base_1+"kanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN068"
  } ;

mkN069 : Str -> N ;
mkN069 base =
  case base of {
    base_1+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ur" ;
                                 Acc => base_1 ;
                                 Dat => base_1+"i" ;
                                 Gen => base_1
                               } ;
                         Pl => table {
                                 Nom => base_1+"urin" ;
                                 Acc => base_1+"in" ;
                                 Dat => base_1+"inum" ;
                                 Gen => base_1+"ins"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN069"
  } ;

mkN070 : Str -> N ;
mkN070 base =
  case base of {
    base_1+"o"+base_2@(?+?) => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"o"+base_2 ;
                                 Acc => base_1+"o"+base_2 ;
                                 Dat => base_1+"o"+base_2 ;
                                 Gen => base_1+"o"+base_2+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"e"+base_2+"ur" ;
                                 Acc => base_1+"e"+base_2+"ur" ;
                                 Dat => base_1+"o"+base_2+"um" ;
                                 Gen => base_1+"o"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"o"+base_2+"in" ;
                               Acc => base_1+"o"+base_2+"ina" ;
                               Dat => base_1+"o"+base_2+"ini" ;
                               Gen => base_1+"o"+base_2+"arinnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"e"+base_2+"urnar" ;
                               Acc => base_1+"e"+base_2+"urnar" ;
                               Dat => base_1+"o"+base_2+"unum" ;
                               Gen => base_1+"o"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN070"
  } ;

mkN071 : Str -> N ;
mkN071 base =
  case base of {
    base_1+"ó"+base_2@?+"ti"+base_3@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ó"+base_2+"ti"+base_3 ;
                                 Acc => base_1+"ó"+base_2+"tu"+base_3 ;
                                 Dat => base_1+"ó"+base_2+"tu"+base_3 ;
                                 Gen => base_1+"ó"+base_2+"tu"+base_3
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2+"u"+base_3 ;
                                 Acc => base_1+"ø"+base_2+"u"+base_3 ;
                                 Dat => base_1+"ø"+base_2+base_3+"um" ;
                                 Gen => base_1+"ø"+base_2+base_3+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ó"+base_2+"ti"+base_3+"in" ;
                               Acc => base_1+"ót"+base_2+base_3+"ina" ;
                               Dat => base_1+"ót"+base_2+base_3+"ini" ;
                               Gen => base_1+"ó"+base_2+"tu"+base_3+"innar"
                             } ;
                       Pl => table {
                               Nom => base_1+"ø"+base_2+"u"+base_3+"nar" ;
                               Acc => base_1+"ø"+base_2+"u"+base_3+"nar" ;
                               Dat => base_1+"ø"+base_2+base_3+"unum" ;
                               Gen => base_1+"ø"+base_2+base_3+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN071"
  } ;

mkN072 : Str -> N ;
mkN072 base =
  case base of {
    base_1+"o"+base_2@(?+?)+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"o"+base_2+"ur" ;
                                 Acc => base_1+"o"+base_2 ;
                                 Dat => base_1+"ei"+base_2+"i" ;
                                 Gen => base_1+"ei"+base_2+"jar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ei"+base_2+"ir" ;
                                 Acc => base_1+"ei"+base_2+"ir" ;
                                 Dat => base_1+"ei"+base_2+"jum" ;
                                 Gen => base_1+"ei"+base_2+"ja"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"o"+base_2+"urin" ;
                               Acc => base_1+"o"+base_2+"in" ;
                               Dat => base_1+"ei"+base_2+"inum" ;
                               Gen => base_1+"ei"+base_2+"jarins"
                             } ;
                       Pl => table {
                               Nom => base_1+"ei"+base_2+"irnir" ;
                               Acc => base_1+"ei"+base_2+"irnar" ;
                               Dat => base_1+"ei"+base_2+"junum" ;
                               Gen => base_1+"ei"+base_2+"janna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN072"
  } ;

mkN073 : Str -> N ;
mkN073 base =
  case base of {
    base_1+"u"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"u"+base_2 ;
                                 Acc => base_1+"u"+base_2 ;
                                 Dat => base_1+base_2+"i" ;
                                 Gen => base_1+"u"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+base_2+"ið" ;
                                 Acc => base_1+base_2+"ið" ;
                                 Dat => base_1+base_2+"inum" ;
                                 Gen => base_1+"u"+base_2+"sins"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN073"
  } ;

mkN074 : Str -> N ;
mkN074 base =
  case base of {
    base_1+"y"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"y"+base_2 ;
                                 Acc => base_1+"y"+base_2 ;
                                 Dat => base_1+"u"+base_2+"um" ;
                                 Gen => base_1+"u"+base_2+"a"
                               } ;
                         Pl => table {
                                 Nom => base_1+"y"+base_2+"nar" ;
                                 Acc => base_1+"y"+base_2+"nar" ;
                                 Dat => base_1+"u"+base_2+"unum" ;
                                 Gen => base_1+"u"+base_2+"anna"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN074"
  } ;

mkN075 : Str -> N ;
mkN075 base =
  case base of {
    base_1+"ø"+base_2@("l"|(?+?)) => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ø"+base_2 ;
                                 Acc => base_1+"ø"+base_2 ;
                                 Dat => base_1+"ø"+base_2 ;
                                 Gen => base_1+"a"+base_2+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2+"in" ;
                                 Acc => base_1+"ø"+base_2+"ina" ;
                                 Dat => base_1+"ø"+base_2+"ini" ;
                                 Gen => base_1+"a"+base_2+"arinnar"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN075"
  } ;

mkN076 : Str -> N ;
mkN076 base =
  case base of {
    base_1+"d" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"d" ;
                                 Acc => base_1+"d" ;
                                 Dat => base_1+"di" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"d" ;
                                 Acc => base_1+"d" ;
                                 Dat => base_1+"dum" ;
                                 Gen => base_1+"da"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"dið" ;
                               Acc => base_1+"dið" ;
                               Dat => base_1+"dinum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"dini" ;
                               Acc => base_1+"dini" ;
                               Dat => base_1+"dunum" ;
                               Gen => base_1+"danna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN076"
  } ;

mkN077 : Str -> N ;
mkN077 base =
  case base of {
    base_1+"f" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"f" ;
                                 Acc => base_1+"f" ;
                                 Dat => base_1+"fi" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"f" ;
                                 Acc => base_1+"f" ;
                                 Dat => base_1+"fum" ;
                                 Gen => base_1+"fa"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"fið" ;
                               Acc => base_1+"fið" ;
                               Dat => base_1+"finum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"fini" ;
                               Acc => base_1+"fini" ;
                               Dat => base_1+"funum" ;
                               Gen => base_1+"fanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN077"
  } ;

mkN078 : Str -> N ;
mkN078 base =
  case base of {
    base_1+"l" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"l" ;
                                 Acc => base_1+"l" ;
                                 Dat => base_1+"li" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"l" ;
                                 Acc => base_1+"l" ;
                                 Dat => base_1+"lum" ;
                                 Gen => base_1+"la"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"lið" ;
                               Acc => base_1+"lið" ;
                               Dat => base_1+"linum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"lini" ;
                               Acc => base_1+"lini" ;
                               Dat => base_1+"lunum" ;
                               Gen => base_1+"lanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN078"
  } ;

mkN079 : Str -> N ;
mkN079 base =
  case base of {
    base_1+"i" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"i" ;
                                 Acc => base_1+"i" ;
                                 Dat => base_1+"i" ;
                                 Gen => base_1+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ar" ;
                                 Acc => base_1+"ar" ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"in" ;
                               Acc => base_1+"ina" ;
                               Dat => base_1+"ini" ;
                               Gen => base_1+"arinnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"arnar" ;
                               Acc => base_1+"arnar" ;
                               Dat => base_1+"unum" ;
                               Gen => base_1+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN079"
  } ;

mkN080 : Str -> N ;
mkN080 base =
  case base of {
    base_1+"r" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"r" ;
                                 Acc => base_1+"r" ;
                                 Dat => base_1+"ri" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"r" ;
                                 Acc => base_1+"r" ;
                                 Dat => base_1+"rum" ;
                                 Gen => base_1+"ra"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"rið" ;
                               Acc => base_1+"rið" ;
                               Dat => base_1+"rinum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"rini" ;
                               Acc => base_1+"rini" ;
                               Dat => base_1+"runum" ;
                               Gen => base_1+"ranna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN080"
  } ;

mkN081 : Str -> N ;
mkN081 base =
  case base of {
    base_1+"u"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"u"+base_2 ;
                                 Acc => base_1+"u"+base_2 ;
                                 Dat => base_1+"u"+base_2 ;
                                 Gen => base_1+base_2+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+base_2+"ar" ;
                                 Acc => base_1+base_2+"ar" ;
                                 Dat => base_1+base_2+"um" ;
                                 Gen => base_1+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"u"+base_2+"in" ;
                               Acc => base_1+base_2+"ina" ;
                               Dat => base_1+base_2+"ini" ;
                               Gen => base_1+base_2+"arinnar"
                             } ;
                       Pl => table {
                               Nom => base_1+base_2+"arnar" ;
                               Acc => base_1+base_2+"arnar" ;
                               Dat => base_1+base_2+"unum" ;
                               Gen => base_1+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN081"
  } ;

mkN082 : Str -> N ;
mkN082 base =
  case base of {
    base_1+"n"+base_2@(?+?+?+?)+"um" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"n"+base_2+"um" ;
                                 Acc => base_1+"n"+base_2+"um" ;
                                 Dat => base_1+"n"+base_2+"i" ;
                                 Gen => base_1+"n"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"n"+base_2+"um" ;
                                 Acc => base_1+"n"+base_2+"um" ;
                                 Dat => base_1+"n"+base_2+"um" ;
                                 Gen => base_1+"n"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"n"+base_2+"ið" ;
                               Acc => base_1+"n"+base_2+"ið" ;
                               Dat => base_1+"n"+base_2+"num" ;
                               Gen => base_1+"n"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"n"+base_2+"ini" ;
                               Acc => base_1+"n"+base_2+"ini" ;
                               Dat => base_1+"n"+base_2+"unum" ;
                               Gen => base_1+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN082"
  } ;

mkN083 : Str -> N ;
mkN083 base =
  case base of {
    base_1+"aði"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"aði"+base_2 ;
                                 Acc => base_1+"aði"+base_2 ;
                                 Dat => base_1+"aði"+base_2 ;
                                 Gen => base_1+"aði"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ed"+base_2+"ar" ;
                                 Acc => base_1+"ed"+base_2+"ar" ;
                                 Dat => base_1+"ed"+base_2+"um" ;
                                 Gen => base_1+"ed"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"aði"+base_2+"in" ;
                               Acc => base_1+"aði"+base_2+"in" ;
                               Dat => base_1+"aði"+base_2+"inum" ;
                               Gen => base_1+"aði"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"ed"+base_2+"arnir" ;
                               Acc => base_1+"ed"+base_2+"arnar" ;
                               Dat => base_1+"ed"+base_2+"unum" ;
                               Gen => base_1+"ed"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN083"
  } ;

mkN084 : Str -> N ;
mkN084 base =
  case base of {
    base_1+"æ" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"æ" ;
                                 Acc => base_1+"æ" ;
                                 Dat => base_1+"æ" ;
                                 Gen => base_1+"íggjar"
                               } ;
                         Pl => table {
                                 Nom => nonExist ;
                                 Acc => nonExist ;
                                 Dat => nonExist ;
                                 Gen => nonExist
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN084"
  } ;

mkN085 : Str -> N ;
mkN085 base =
  case base of {
    base_1+"a"+base_2@?+"l" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"l" ;
                                 Acc => base_1+"a"+base_2+"l" ;
                                 Dat => base_1+"a"+base_2+"li" ;
                                 Gen => base_1+"a"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2+"l" ;
                                 Acc => base_1+"ø"+base_2+"l" ;
                                 Dat => base_1+"ø"+base_2+"lum" ;
                                 Gen => base_1+"a"+base_2+"la"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"a"+base_2+"lið" ;
                               Acc => base_1+"a"+base_2+"lið" ;
                               Dat => base_1+"a"+base_2+"linum" ;
                               Gen => base_1+"a"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"ø"+base_2+"lini" ;
                               Acc => base_1+"ø"+base_2+"lini" ;
                               Dat => base_1+"ø"+base_2+"lunum" ;
                               Gen => base_1+"a"+base_2+"lanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN085"
  } ;

mkN086 : Str -> N ;
mkN086 base =
  case base of {
    base_1+"ar" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ar" ;
                                 Acc => base_1+"ar" ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               } ;
                         Pl => table {
                                 Nom => base_1+"arnir" ;
                                 Acc => base_1+"arnar" ;
                                 Dat => base_1+"unum" ;
                                 Gen => base_1+"anna"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN086"
  } ;

mkN087 : Str -> N ;
mkN087 base =
  case base of {
    base_1+"t" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"t" ;
                                 Acc => base_1+"t" ;
                                 Dat => base_1+"ti" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"tið" ;
                                 Acc => base_1+"tið" ;
                                 Dat => base_1+"tinum" ;
                                 Gen => base_1+"sins"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN087"
  } ;

mkN088 : Str -> N ;
mkN088 base =
  case base of {
    base_1+"ø"+base_2@(?+?) => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ø"+base_2 ;
                                 Acc => base_1+"ø"+base_2 ;
                                 Dat => base_1+"ø"+base_2 ;
                                 Gen => base_1+"a"+base_2+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2+"ir" ;
                                 Acc => base_1+"ø"+base_2+"ir" ;
                                 Dat => base_1+"ø"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ø"+base_2+"in" ;
                               Acc => base_1+"ø"+base_2+"ina" ;
                               Dat => base_1+"ø"+base_2+"ini" ;
                               Gen => base_1+"a"+base_2+"arinnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"ø"+base_2+"irnar" ;
                               Acc => base_1+"ø"+base_2+"irnar" ;
                               Dat => base_1+"ø"+base_2+"unum" ;
                               Gen => base_1+"a"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN088"
  } ;

mkN089 : Str -> N ;
mkN089 base =
  case base of {
    base_1+"ø"+base_2@?+"u"+base_3@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ø"+base_2+"u"+base_3 ;
                                 Acc => base_1+"ø"+base_2+"u"+base_3 ;
                                 Dat => base_1+"ø"+base_2+"u"+base_3 ;
                                 Gen => base_1+"a"+base_2+base_3+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"a"+base_2+base_3+"ar" ;
                                 Acc => base_1+"a"+base_2+base_3+"ar" ;
                                 Dat => base_1+"ø"+base_2+base_3+"um" ;
                                 Gen => base_1+"a"+base_2+base_3+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ø"+base_2+"u"+base_3+"in" ;
                               Acc => base_1+"ø"+base_2+base_3+"ina" ;
                               Dat => base_1+"ø"+base_2+base_3+"ini" ;
                               Gen => base_1+"a"+base_2+base_3+"arinnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+base_3+"arnar" ;
                               Acc => base_1+"a"+base_2+base_3+"arnar" ;
                               Dat => base_1+"ø"+base_2+base_3+"unum" ;
                               Gen => base_1+"a"+base_2+base_3+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN089"
  } ;

mkN090 : Str -> N ;
mkN090 base =
  case base of {
    base_1+"jø"+base_2@(?+?)+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"jø"+base_2+"ur" ;
                                 Acc => base_1+"jø"+base_2 ;
                                 Dat => base_1+"i"+base_2+"i" ;
                                 Gen => base_1+"ja"+base_2+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"i"+base_2+"ir" ;
                                 Acc => base_1+"i"+base_2+"ir" ;
                                 Dat => base_1+"jø"+base_2+"um" ;
                                 Gen => base_1+"ja"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"jø"+base_2+"urin" ;
                               Acc => base_1+"jø"+base_2+"in" ;
                               Dat => base_1+"i"+base_2+"inum" ;
                               Gen => base_1+"ja"+base_2+"arins"
                             } ;
                       Pl => table {
                               Nom => base_1+"i"+base_2+"irnir" ;
                               Acc => base_1+"i"+base_2+"irnar" ;
                               Dat => base_1+"jø"+base_2+"unum" ;
                               Gen => base_1+"ja"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN090"
  } ;

mkN091 : Str -> N ;
mkN091 base =
  case base of {
    base_1+"a"+base_2@?+"g" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"g" ;
                                 Acc => base_1+"a"+base_2+"g" ;
                                 Dat => base_1+"a"+base_2+"gi" ;
                                 Gen => base_1+"a"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2+"g" ;
                                 Acc => base_1+"ø"+base_2+"g" ;
                                 Dat => base_1+"ø"+base_2+"gum" ;
                                 Gen => base_1+"a"+base_2+"ga"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"a"+base_2+"gið" ;
                               Acc => base_1+"a"+base_2+"gið" ;
                               Dat => base_1+"a"+base_2+"ginum" ;
                               Gen => base_1+"a"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"ø"+base_2+"gini" ;
                               Acc => base_1+"ø"+base_2+"gini" ;
                               Dat => base_1+"ø"+base_2+"gunum" ;
                               Gen => base_1+"a"+base_2+"ganna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN091"
  } ;

mkN092 : Str -> N ;
mkN092 base =
  case base of {
    base_1 => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1 ;
                                 Gen => base_1+"jar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"jar" ;
                                 Acc => base_1+"jar" ;
                                 Dat => base_1+"jum" ;
                                 Gen => base_1+"ja"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"in" ;
                               Acc => base_1+"ina" ;
                               Dat => base_1+"ini" ;
                               Gen => base_1+"jarinnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"jarnar" ;
                               Acc => base_1+"jarnar" ;
                               Dat => base_1+"junum" ;
                               Gen => base_1+"janna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN092"
  } ;

mkN093 : Str -> N ;
mkN093 base =
  case base of {
    base_1+"ø"+base_2@("r"|"l"|(?+?))+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ø"+base_2+"ur" ;
                                 Acc => base_1+"ø"+base_2 ;
                                 Dat => base_1+"ø"+base_2+"i" ;
                                 Gen => base_1+"a"+base_2+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2+"ir" ;
                                 Acc => base_1+"ø"+base_2+"ir" ;
                                 Dat => base_1+"ø"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ø"+base_2+"urin" ;
                               Acc => base_1+"ø"+base_2+"in" ;
                               Dat => base_1+"ø"+base_2+"inum" ;
                               Gen => base_1+"a"+base_2+"arins"
                             } ;
                       Pl => table {
                               Nom => base_1+"ø"+base_2+"irnir" ;
                               Acc => base_1+"ø"+base_2+"irnar" ;
                               Dat => base_1+"ø"+base_2+"unum" ;
                               Gen => base_1+"a"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN093"
  } ;

mkN094 : Str -> N ;
mkN094 base =
  case base of {
    base_1+"ø"+base_2@(?+?)+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ø"+base_2+"ur" ;
                                 Acc => base_1+"ø"+base_2 ;
                                 Dat => base_1+"ø"+base_2+"i" ;
                                 Gen => base_1+"a"+base_2+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2+"ar" ;
                                 Acc => base_1+"ø"+base_2+"ar" ;
                                 Dat => base_1+"ø"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ø"+base_2+"urin" ;
                               Acc => base_1+"ø"+base_2+"in" ;
                               Dat => base_1+"ø"+base_2+"inum" ;
                               Gen => base_1+"a"+base_2+"arins"
                             } ;
                       Pl => table {
                               Nom => base_1+"ø"+base_2+"arnir" ;
                               Acc => base_1+"ø"+base_2+"arnar" ;
                               Dat => base_1+"ø"+base_2+"unum" ;
                               Gen => base_1+"a"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN094"
  } ;

mkN095 : Str -> N ;
mkN095 base =
  case base of {
    base_1+"u"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"u"+base_2 ;
                                 Acc => base_1+"u"+base_2 ;
                                 Dat => base_1+base_2+"um" ;
                                 Gen => base_1+base_2+"a"
                               } ;
                         Pl => table {
                                 Nom => base_1+base_2+"ini" ;
                                 Acc => base_1+base_2+"ini" ;
                                 Dat => base_1+base_2+"unum" ;
                                 Gen => base_1+base_2+"anna"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN095"
  } ;

mkN096 : Str -> N ;
mkN096 base =
  case base of {
    base_1+"a"+base_2@(?+?) => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2 ;
                                 Acc => base_1+"a"+base_2 ;
                                 Dat => base_1+"a"+base_2+"i" ;
                                 Gen => base_1+"a"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"o"+base_2 ;
                                 Acc => base_1+"o"+base_2 ;
                                 Dat => base_1+"o"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"a"+base_2+"ið" ;
                               Acc => base_1+"a"+base_2+"ið" ;
                               Dat => base_1+"a"+base_2+"inum" ;
                               Gen => base_1+"a"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"o"+base_2+"ini" ;
                               Acc => base_1+"o"+base_2+"ini" ;
                               Dat => base_1+"o"+base_2+"unum" ;
                               Gen => base_1+"a"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN096"
  } ;

mkN097 : Str -> N ;
mkN097 base =
  case base of {
    base_1+"ó"+base_2@?+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ó"+base_2+"ur" ;
                                 Acc => base_1+"ó"+base_2 ;
                                 Dat => base_1+"ó"+base_2+"i" ;
                                 Gen => base_1+"ó"+base_2+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2+"ur" ;
                                 Acc => base_1+"ø"+base_2+"ur" ;
                                 Dat => base_1+"ó"+base_2+"um" ;
                                 Gen => base_1+"ó"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ó"+base_2+"urin" ;
                               Acc => base_1+"ó"+base_2+"in" ;
                               Dat => base_1+"ó"+base_2+"inum" ;
                               Gen => base_1+"ó"+base_2+"arins"
                             } ;
                       Pl => table {
                               Nom => base_1+"ø"+base_2+"urnir" ;
                               Acc => base_1+"ø"+base_2+"urnar" ;
                               Dat => base_1+"ó"+base_2+"unum" ;
                               Gen => base_1+"ó"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN097"
  } ;

mkN098 : Str -> N ;
mkN098 base =
  case base of {
    base_1+"i" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"i" ;
                                 Acc => base_1+"a" ;
                                 Dat => base_1+"a" ;
                                 Gen => base_1+"a"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ur" ;
                                 Acc => base_1+"ur" ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"in" ;
                               Acc => base_1+"an" ;
                               Dat => base_1+"anum" ;
                               Gen => base_1+"ans"
                             } ;
                       Pl => table {
                               Nom => base_1+"urnir" ;
                               Acc => base_1+"urnar" ;
                               Dat => base_1+"unum" ;
                               Gen => base_1+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN098"
  } ;

mkN099 : Str -> N ;
mkN099 base =
  case base of {
    base_1+"fi"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"fi"+base_2 ;
                                 Acc => base_1+"fi"+base_2 ;
                                 Dat => base_1+base_2+"i" ;
                                 Gen => base_1+"fi"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+base_2+"ar" ;
                                 Acc => base_1+base_2+"ar" ;
                                 Dat => base_1+base_2+"um" ;
                                 Gen => base_1+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"fi"+base_2+"in" ;
                               Acc => base_1+"fi"+base_2+"in" ;
                               Dat => base_1+base_2+"inum" ;
                               Gen => base_1+"fi"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+base_2+"arnir" ;
                               Acc => base_1+base_2+"arnar" ;
                               Dat => base_1+base_2+"unum" ;
                               Gen => base_1+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN099"
  } ;

mkN100 : Str -> N ;
mkN100 base =
  case base of {
    base_1+"á"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"á"+base_2 ;
                                 Acc => base_1+"á"+base_2 ;
                                 Dat => base_1+"á"+base_2 ;
                                 Gen => base_1+"á"+base_2+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"æ"+base_2 ;
                                 Acc => base_1+"æ"+base_2 ;
                                 Dat => base_1+"á"+base_2+"um" ;
                                 Gen => base_1+"á"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"á"+base_2+"in" ;
                               Acc => base_1+"á"+base_2+"ina" ;
                               Dat => base_1+"á"+base_2+"ini" ;
                               Gen => base_1+"á"+base_2+"arinnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"æ"+base_2+"nar" ;
                               Acc => base_1+"æ"+base_2+"nar" ;
                               Dat => base_1+"á"+base_2+"unum" ;
                               Gen => base_1+"á"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN100"
  } ;

mkN101 : Str -> N ;
mkN101 base =
  case base of {
    base_1+"fur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"fur" ;
                                 Acc => base_1+"f" ;
                                 Dat => base_1+"fi" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"far" ;
                                 Acc => base_1+"far" ;
                                 Dat => base_1+"fum" ;
                                 Gen => base_1+"fa"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"furin" ;
                               Acc => base_1+"fin" ;
                               Dat => base_1+"finum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"farnir" ;
                               Acc => base_1+"farnar" ;
                               Dat => base_1+"funum" ;
                               Gen => base_1+"fanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN101"
  } ;

mkN102 : Str -> N ;
mkN102 base =
  case base of {
    base_1+"ógv" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ógv" ;
                                 Acc => base_1+"ógv" ;
                                 Dat => base_1+"ógv" ;
                                 Gen => base_1+"áar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"áir" ;
                                 Acc => base_1+"áir" ;
                                 Dat => base_1+"áum" ;
                                 Gen => base_1+"áa"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ógvin" ;
                               Acc => base_1+"ónna" ;
                               Dat => base_1+"ónni" ;
                               Gen => base_1+"áarinnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"áirnar" ;
                               Acc => base_1+"áirnar" ;
                               Dat => base_1+"áunum" ;
                               Gen => base_1+"áanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN102"
  } ;

mkN103 : Str -> N ;
mkN103 base =
  case base of {
    base_1+"a"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2 ;
                                 Acc => base_1+"a"+base_2 ;
                                 Dat => base_1+"a"+base_2+"i" ;
                                 Gen => base_1+"a"+base_2
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2 ;
                                 Acc => base_1+"ø"+base_2 ;
                                 Dat => base_1+"ø"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"a"+base_2+"ið" ;
                               Acc => base_1+"a"+base_2+"ið" ;
                               Dat => base_1+"a"+base_2+"inum" ;
                               Gen => base_1+"a"+base_2+"ins"
                             } ;
                       Pl => table {
                               Nom => base_1+"ø"+base_2+"ini" ;
                               Acc => base_1+"ø"+base_2+"ini" ;
                               Dat => base_1+"ø"+base_2+"unum" ;
                               Gen => base_1+"a"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN103"
  } ;

mkN104 : Str -> N ;
mkN104 base =
  case base of {
    base_1 => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1 ;
                                 Gen => base_1
                               } ;
                         Pl => table {
                                 Nom => base_1+"r" ;
                                 Acc => base_1+"r" ;
                                 Dat => nonExist ;
                                 Gen => nonExist
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"n" ;
                               Acc => base_1+"na" ;
                               Dat => base_1+"ni" ;
                               Gen => base_1+"nnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"rnar" ;
                               Acc => base_1+"rnar" ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN104"
  } ;

mkN105 : Str -> N ;
mkN105 base =
  case base of {
    base_1+"ggj" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ggj" ;
                                 Acc => base_1+"ggj" ;
                                 Dat => base_1+"ggj" ;
                                 Gen => base_1+"ggjar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ggjar" ;
                                 Acc => base_1+"ggjar" ;
                                 Dat => base_1+"ggjum" ;
                                 Gen => base_1+"ggja"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ggin" ;
                               Acc => base_1+"nna" ;
                               Dat => base_1+"nni" ;
                               Gen => base_1+"ggjarinnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"ggjarnar" ;
                               Acc => base_1+"ggjarnar" ;
                               Dat => base_1+"ggjunum" ;
                               Gen => base_1+"ggjanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN105"
  } ;

mkN106 : Str -> N ;
mkN106 base =
  case base of {
    base_1+"a"+base_2@?+"i" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"i" ;
                                 Acc => base_1+"a"+base_2+"a" ;
                                 Dat => base_1+"a"+base_2+"a" ;
                                 Gen => base_1+"a"+base_2+"a"
                               } ;
                         Pl => table {
                                 Nom => base_1+"a"+base_2+"ar" ;
                                 Acc => base_1+"a"+base_2+"ar" ;
                                 Dat => base_1+"ø"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"a"+base_2+"in" ;
                               Acc => base_1+"a"+base_2+"an" ;
                               Dat => base_1+"a"+base_2+"anum" ;
                               Gen => base_1+"a"+base_2+"ans"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+"arnir" ;
                               Acc => base_1+"a"+base_2+"arnar" ;
                               Dat => base_1+"ø"+base_2+"unum" ;
                               Gen => base_1+"a"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN106"
  } ;

mkN107 : Str -> N ;
mkN107 base =
  case base of {
    base_1+"a"+base_2@?+"a"+base_3@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"a"+base_3 ;
                                 Acc => base_1+"a"+base_2+"a"+base_3 ;
                                 Dat => base_1+"a"+base_2+base_3+"i" ;
                                 Gen => base_1+"a"+base_2+"a"+base_3+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"a"+base_2+base_3+"ar" ;
                                 Acc => base_1+"a"+base_2+base_3+"ar" ;
                                 Dat => base_1+"o"+base_2+base_3+"um" ;
                                 Gen => base_1+"a"+base_2+base_3+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"a"+base_2+"a"+base_3+"in" ;
                               Acc => base_1+"a"+base_2+"a"+base_3+"an" ;
                               Dat => base_1+"a"+base_2+base_3+"inum" ;
                               Gen => base_1+"a"+base_2+"a"+base_3+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+base_3+"arnir" ;
                               Acc => base_1+"a"+base_2+base_3+"arnar" ;
                               Dat => base_1+"o"+base_2+base_3+"unum" ;
                               Gen => base_1+"a"+base_2+base_3+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN107"
  } ;

mkN108 : Str -> N ;
mkN108 base =
  case base of {
    base_1+"p" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"p" ;
                                 Acc => base_1+"p" ;
                                 Dat => base_1+"pi" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"p" ;
                                 Acc => base_1+"p" ;
                                 Dat => base_1+"pum" ;
                                 Gen => base_1+"pa"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"pið" ;
                               Acc => base_1+"pið" ;
                               Dat => base_1+"pinum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"pini" ;
                               Acc => base_1+"pini" ;
                               Dat => base_1+"punum" ;
                               Gen => base_1+"panna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN108"
  } ;

mkN109 : Str -> N ;
mkN109 base =
  case base of {
    base_1+"tur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"tur" ;
                                 Acc => base_1+"t" ;
                                 Dat => base_1+"ti" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"tar" ;
                                 Acc => base_1+"tar" ;
                                 Dat => base_1+"tum" ;
                                 Gen => base_1+"ta"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"turin" ;
                               Acc => base_1+"tin" ;
                               Dat => base_1+"tinum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"tarnir" ;
                               Acc => base_1+"tarnar" ;
                               Dat => base_1+"tunum" ;
                               Gen => base_1+"tanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN109"
  } ;

mkN110 : Str -> N ;
mkN110 base =
  case base of {
    base_1+"gv" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"gv" ;
                                 Acc => base_1+"gv" ;
                                 Dat => base_1+"gv" ;
                                 Gen => base_1+"gvar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"gvin" ;
                                 Acc => base_1+"nna" ;
                                 Dat => base_1+"nni" ;
                                 Gen => base_1+"gvarinnar"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN110"
  } ;

mkN111 : Str -> N ;
mkN111 base =
  case base of {
    base_1+"ar" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ar" ;
                                 Acc => base_1+"ar" ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               } ;
                         Pl => table {
                                 Nom => base_1+"arnar" ;
                                 Acc => base_1+"arnar" ;
                                 Dat => base_1+"unum" ;
                                 Gen => base_1+"anna"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN111"
  } ;

mkN112 : Str -> N ;
mkN112 base =
  case base of {
    base_1+"a"+base_2@(?+?) => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2 ;
                                 Acc => base_1+"a"+base_2 ;
                                 Dat => base_1+"a"+base_2 ;
                                 Gen => base_1+"a"+base_2+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"e"+base_2+"ir" ;
                                 Acc => base_1+"e"+base_2+"ir" ;
                                 Dat => base_1+"e"+base_2+"um" ;
                                 Gen => base_1+"e"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"a"+base_2+"in" ;
                               Acc => base_1+"a"+base_2+"ina" ;
                               Dat => base_1+"a"+base_2+"ini" ;
                               Gen => base_1+"a"+base_2+"arinnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"e"+base_2+"irnar" ;
                               Acc => base_1+"e"+base_2+"irnar" ;
                               Dat => base_1+"e"+base_2+"unum" ;
                               Gen => base_1+"e"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN112"
  } ;

mkN113 : Str -> N ;
mkN113 base =
  case base of {
    base_1+"ma"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ma"+base_2 ;
                                 Acc => base_1+"ma"+base_2 ;
                                 Dat => base_1+base_2+"i" ;
                                 Gen => base_1+"ma"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+base_2+"ar" ;
                                 Acc => base_1+base_2+"ar" ;
                                 Dat => base_1+base_2+"um" ;
                                 Gen => base_1+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ma"+base_2+"in" ;
                               Acc => base_1+"ma"+base_2+"in" ;
                               Dat => base_1+base_2+"inum" ;
                               Gen => base_1+"ma"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+base_2+"arnir" ;
                               Acc => base_1+base_2+"arnar" ;
                               Dat => base_1+base_2+"unum" ;
                               Gen => base_1+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN113"
  } ;

mkN114 : Str -> N ;
mkN114 base =
  case base of {
    base_1+"a"+base_2@(?+?)+"a" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"a" ;
                                 Acc => base_1+"a"+base_2+"a" ;
                                 Dat => base_1+"a"+base_2+"a" ;
                                 Gen => base_1+"a"+base_2+"a"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2+"ur" ;
                                 Acc => base_1+"ø"+base_2+"ur" ;
                                 Dat => base_1+"ø"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"na"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"a"+base_2+"að" ;
                               Acc => base_1+"a"+base_2+"að" ;
                               Dat => base_1+"a"+base_2+"anum" ;
                               Gen => base_1+"a"+base_2+"ans"
                             } ;
                       Pl => table {
                               Nom => base_1+"ø"+base_2+"uni" ;
                               Acc => base_1+"ø"+base_2+"uni" ;
                               Dat => base_1+"ø"+base_2+"unum" ;
                               Gen => base_1+"a"+base_2+"nanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN114"
  } ;

mkN115 : Str -> N ;
mkN115 base =
  case base of {
    base_1 => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ini" ;
                                 Acc => base_1+"ini" ;
                                 Dat => base_1+"unum" ;
                                 Gen => base_1+"anna"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN115"
  } ;

mkN116 : Str -> N ;
mkN116 base =
  case base of {
    base_1+"g"+base_2@?+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"g"+base_2+"ur" ;
                                 Acc => base_1+"g"+base_2 ;
                                 Dat => base_1+"g"+base_2+"i" ;
                                 Gen => base_1+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"g"+base_2+"ar" ;
                                 Acc => base_1+"g"+base_2+"ar" ;
                                 Dat => base_1+"g"+base_2+"um" ;
                                 Gen => base_1+"g"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"g"+base_2+"urin" ;
                               Acc => base_1+"g"+base_2+"in" ;
                               Dat => base_1+"g"+base_2+"inum" ;
                               Gen => base_1+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"g"+base_2+"arnir" ;
                               Acc => base_1+"g"+base_2+"arnar" ;
                               Dat => base_1+"g"+base_2+"unum" ;
                               Gen => base_1+"g"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN116"
  } ;

mkN117 : Str -> N ;
mkN117 base =
  case base of {
    base_1+"o"+base_2@(?+?) => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"o"+base_2 ;
                                 Acc => base_1+"o"+base_2 ;
                                 Dat => base_1+"o"+base_2 ;
                                 Gen => base_1+"a"+base_2+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"e"+base_2+"ur" ;
                                 Acc => base_1+"e"+base_2+"ur" ;
                                 Dat => base_1+"o"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"o"+base_2+"in" ;
                               Acc => base_1+"o"+base_2+"ina" ;
                               Dat => base_1+"o"+base_2+"ini" ;
                               Gen => base_1+"a"+base_2+"arinnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"e"+base_2+"urnar" ;
                               Acc => base_1+"e"+base_2+"urnar" ;
                               Dat => base_1+"o"+base_2+"unum" ;
                               Gen => base_1+"a"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN117"
  } ;

mkN118 : Str -> N ;
mkN118 base =
  case base of {
    base_1+"d" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"d" ;
                                 Acc => base_1+"d" ;
                                 Dat => base_1+"di" ;
                                 Gen => base_1+"uðs"
                               } ;
                         Pl => table {
                                 Nom => base_1+"d" ;
                                 Acc => base_1+"d" ;
                                 Dat => base_1+"dum" ;
                                 Gen => base_1+"da"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"dini" ;
                               Acc => base_1+"dini" ;
                               Dat => base_1+"dinum" ;
                               Gen => base_1+"uðsins"
                             } ;
                       Pl => table {
                               Nom => base_1+"dini" ;
                               Acc => base_1+"dini" ;
                               Dat => base_1+"dunum" ;
                               Gen => base_1+"danna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN118"
  } ;

mkN119 : Str -> N ;
mkN119 base =
  case base of {
    base_1+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ur" ;
                                 Acc => base_1+"ur" ;
                                 Dat => base_1+"di" ;
                                 Gen => base_1+"uðs"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ur" ;
                                 Acc => base_1+"ur" ;
                                 Dat => base_1+"dum" ;
                                 Gen => base_1+"da"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"urini" ;
                               Acc => base_1+"urini" ;
                               Dat => base_1+"dinum" ;
                               Gen => base_1+"uðsins"
                             } ;
                       Pl => table {
                               Nom => base_1+"urini" ;
                               Acc => base_1+"urini" ;
                               Dat => base_1+"dunum" ;
                               Gen => base_1+"danna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN119"
  } ;

mkN120 : Str -> N ;
mkN120 base =
  case base of {
    base_1+"ggj" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ggj" ;
                                 Acc => base_1+"ggj" ;
                                 Dat => base_1+"ggi" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ggj" ;
                                 Acc => base_1+"ggj" ;
                                 Dat => base_1+"ggjum" ;
                                 Gen => base_1+"ggja"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ggið" ;
                               Acc => base_1+"ggið" ;
                               Dat => base_1+"gginum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"ggini" ;
                               Acc => base_1+"ggini" ;
                               Dat => base_1+"ggjunum" ;
                               Gen => base_1+"ggjanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN120"
  } ;

mkN121 : Str -> N ;
mkN121 base =
  case base of {
    base_1+"ø"+base_2@("k"|(?+?)) => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ø"+base_2 ;
                                 Acc => base_1+"ø"+base_2 ;
                                 Dat => base_1+"ø"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"a"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2+"ini" ;
                                 Acc => base_1+"ø"+base_2+"ini" ;
                                 Dat => base_1+"ø"+base_2+"unum" ;
                                 Gen => base_1+"a"+base_2+"anna"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN121"
  } ;

mkN122 : Str -> N ;
mkN122 base =
  case base of {
    base_1+"ggj" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ggj" ;
                                 Acc => base_1+"ggj" ;
                                 Dat => base_1+"ggi" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ggið" ;
                                 Acc => base_1+"ggið" ;
                                 Dat => base_1+"gginum" ;
                                 Gen => base_1+"sins"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN122"
  } ;

mkN123 : Str -> N ;
mkN123 base =
  case base of {
    base_1+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ur" ;
                                 Acc => base_1 ;
                                 Dat => base_1+"i" ;
                                 Gen => base_1+"jar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"jar" ;
                                 Acc => base_1+"jar" ;
                                 Dat => base_1+"jum" ;
                                 Gen => base_1+"ja"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"urin" ;
                               Acc => base_1+"in" ;
                               Dat => base_1+"inum" ;
                               Gen => base_1+"jarins"
                             } ;
                       Pl => table {
                               Nom => base_1+"jarnir" ;
                               Acc => base_1+"jarnar" ;
                               Dat => base_1+"junum" ;
                               Gen => base_1+"janna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN123"
  } ;

mkN124 : Str -> N ;
mkN124 base =
  case base of {
    base_1+"gv" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"gv" ;
                                 Acc => base_1+"gv" ;
                                 Dat => base_1+"gvi" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"gvið" ;
                                 Acc => base_1+"gvið" ;
                                 Dat => base_1+"gvnum" ;
                                 Gen => base_1+"sins"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN124"
  } ;

mkN125 : Str -> N ;
mkN125 base =
  case base of {
    base_1+"ø"+base_2@?+"u"+base_3@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ø"+base_2+"u"+base_3 ;
                                 Acc => base_1+"ø"+base_2+"u"+base_3 ;
                                 Dat => base_1+"ø"+base_2+base_3+"i" ;
                                 Gen => base_1+"ø"+base_2+"u"+base_3+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"a"+base_2+base_3+"ir" ;
                                 Acc => base_1+"a"+base_2+base_3+"ir" ;
                                 Dat => base_1+"ø"+base_2+base_3+"um" ;
                                 Gen => base_1+"a"+base_2+base_3+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ø"+base_2+"u"+base_3+"in" ;
                               Acc => base_1+"ø"+base_2+"u"+base_3+"in" ;
                               Dat => base_1+"ø"+base_2+base_3+"inum" ;
                               Gen => base_1+"ø"+base_2+"u"+base_3+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+base_3+"irnir" ;
                               Acc => base_1+"a"+base_2+base_3+"irnar" ;
                               Dat => base_1+"ø"+base_2+base_3+"unum" ;
                               Gen => base_1+"a"+base_2+base_3+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN125"
  } ;

mkN126 : Str -> N ;
mkN126 base =
  case base of {
    base_1+"a"+base_2@?+"a"+base_3@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"a"+base_3 ;
                                 Acc => base_1+"a"+base_2+"a"+base_3 ;
                                 Dat => base_1+"a"+base_2+"a"+base_3+"i" ;
                                 Gen => base_1+"a"+base_2+"a"+base_3+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2+"u"+base_3 ;
                                 Acc => base_1+"ø"+base_2+"u"+base_3 ;
                                 Dat => base_1+"ø"+base_2+"u"+base_3+"um" ;
                                 Gen => base_1+"a"+base_2+"a"+base_3+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"a"+base_2+"a"+base_3+"ið" ;
                               Acc => base_1+"a"+base_2+"a"+base_3+"ið" ;
                               Dat => base_1+"a"+base_2+"a"+base_3+"inum" ;
                               Gen => base_1+"a"+base_2+"a"+base_3+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"ø"+base_2+base_3+"ini" ;
                               Acc => base_1+"ø"+base_2+base_3+"ini" ;
                               Dat => base_1+"ø"+base_2+base_3+"unum" ;
                               Gen => base_1+"a"+base_2+"a"+base_3+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN126"
  } ;

mkN127 : Str -> N ;
mkN127 base =
  case base of {
    base_1+"u"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"u"+base_2 ;
                                 Acc => base_1+"u"+base_2 ;
                                 Dat => base_1+base_2+"i" ;
                                 Gen => base_1+base_2+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+base_2+"ar" ;
                                 Acc => base_1+base_2+"ar" ;
                                 Dat => base_1+base_2+"um" ;
                                 Gen => base_1+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"u"+base_2+"in" ;
                               Acc => base_1+"u"+base_2+"in" ;
                               Dat => base_1+base_2+"inum" ;
                               Gen => base_1+base_2+"arins"
                             } ;
                       Pl => table {
                               Nom => base_1+base_2+"arnir" ;
                               Acc => base_1+base_2+"arnar" ;
                               Dat => base_1+base_2+"unum" ;
                               Gen => base_1+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN127"
  } ;

mkN128 : Str -> N ;
mkN128 base =
  case base of {
    base_1+"a"+base_2@?+"tur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"tur" ;
                                 Acc => base_1+"a"+base_2+"t" ;
                                 Dat => base_1+"a"+base_2+"ti" ;
                                 Gen => base_1+"a"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"a"+base_2+"tar" ;
                                 Acc => base_1+"a"+base_2+"tar" ;
                                 Dat => base_1+"ø"+base_2+"tum" ;
                                 Gen => base_1+"a"+base_2+"ta"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"a"+base_2+"turin" ;
                               Acc => base_1+"a"+base_2+"tin" ;
                               Dat => base_1+"a"+base_2+"tinum" ;
                               Gen => base_1+"a"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+"tarnir" ;
                               Acc => base_1+"a"+base_2+"tarnar" ;
                               Dat => base_1+"ø"+base_2+"tunum" ;
                               Gen => base_1+"a"+base_2+"tanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN128"
  } ;

mkN129 : Str -> N ;
mkN129 base =
  case base of {
    base_1+"a"+base_2@?+"kur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"kur" ;
                                 Acc => base_1+"a"+base_2+"k" ;
                                 Dat => base_1+"a"+base_2+"ki" ;
                                 Gen => base_1+"a"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"a"+base_2+"kar" ;
                                 Acc => base_1+"a"+base_2+"kar" ;
                                 Dat => base_1+"ø"+base_2+"kum" ;
                                 Gen => base_1+"a"+base_2+"ka"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"a"+base_2+"kurin" ;
                               Acc => base_1+"a"+base_2+"kin" ;
                               Dat => base_1+"a"+base_2+"kinum" ;
                               Gen => base_1+"a"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+"karnir" ;
                               Acc => base_1+"a"+base_2+"karnar" ;
                               Dat => base_1+"ø"+base_2+"kunum" ;
                               Gen => base_1+"a"+base_2+"kanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN129"
  } ;

mkN130 : Str -> N ;
mkN130 base =
  case base of {
    base_1+"a"+base_2@?+"pur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"pur" ;
                                 Acc => base_1+"a"+base_2+"p" ;
                                 Dat => base_1+"a"+base_2+"pi" ;
                                 Gen => base_1+"a"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"a"+base_2+"par" ;
                                 Acc => base_1+"a"+base_2+"par" ;
                                 Dat => base_1+"ø"+base_2+"pum" ;
                                 Gen => base_1+"a"+base_2+"pa"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"a"+base_2+"purin" ;
                               Acc => base_1+"a"+base_2+"pin" ;
                               Dat => base_1+"a"+base_2+"pinum" ;
                               Gen => base_1+"a"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+"parnir" ;
                               Acc => base_1+"a"+base_2+"parnar" ;
                               Dat => base_1+"ø"+base_2+"punum" ;
                               Gen => base_1+"a"+base_2+"panna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN130"
  } ;

mkN131 : Str -> N ;
mkN131 base =
  case base of {
    base_1+"gj" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"gj" ;
                                 Acc => base_1+"gj" ;
                                 Dat => base_1+"gi" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"gj" ;
                                 Acc => base_1+"gj" ;
                                 Dat => base_1+"gjum" ;
                                 Gen => base_1+"gja"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"gið" ;
                               Acc => base_1+"gið" ;
                               Dat => base_1+"num" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"gini" ;
                               Acc => base_1+"gini" ;
                               Dat => base_1+"gjunum" ;
                               Gen => base_1+"gjanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN131"
  } ;

mkN132 : Str -> N ;
mkN132 base =
  case base of {
    base_1+"úgv" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"úgv" ;
                                 Acc => base_1+"úgv" ;
                                 Dat => base_1+"úgv" ;
                                 Gen => base_1+"úgvar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ýr" ;
                                 Acc => base_1+"ýr" ;
                                 Dat => base_1+"úm" ;
                                 Gen => base_1+"úgva"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"úgvin" ;
                               Acc => base_1+"únna" ;
                               Dat => base_1+"únni" ;
                               Gen => base_1+"úgvarinnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"ýrnar" ;
                               Acc => base_1+"ýrnar" ;
                               Dat => base_1+"únum" ;
                               Gen => base_1+"úgvanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN132"
  } ;

mkN133 : Str -> N ;
mkN133 base =
  case base of {
    base_1+"ir" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ir" ;
                                 Acc => base_1+"ir" ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ini" ;
                                 Acc => base_1+"ini" ;
                                 Dat => base_1+"unum" ;
                                 Gen => base_1+"anna"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN133"
  } ;

mkN134 : Str -> N ;
mkN134 base =
  case base of {
    base_1 => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1 ;
                                 Gen => base_1
                               } ;
                         Pl => table {
                                 Nom => base_1+"ð" ;
                                 Acc => base_1+"ð" ;
                                 Dat => base_1+"num" ;
                                 Gen => base_1+"ns"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN134"
  } ;

mkN135 : Str -> N ;
mkN135 base =
  case base of {
    base_1+"ó"+base_2@?+"i"+base_3@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ó"+base_2+"i"+base_3 ;
                                 Acc => base_1+"ó"+base_2+"u"+base_3 ;
                                 Dat => base_1+"ó"+base_2+"u"+base_3 ;
                                 Gen => base_1+"ó"+base_2+"u"+base_3
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2+"u"+base_3 ;
                                 Acc => base_1+"ø"+base_2+"u"+base_3 ;
                                 Dat => base_1+"ø"+base_2+base_3+"um" ;
                                 Gen => base_1+"ø"+base_2+base_3+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ó"+base_2+"i"+base_3+"in" ;
                               Acc => base_1+"ó"+base_2+"u"+base_3+"ina" ;
                               Dat => base_1+"ó"+base_2+"u"+base_3+"ini" ;
                               Gen => base_1+"ó"+base_2+"u"+base_3+"innar"
                             } ;
                       Pl => table {
                               Nom => base_1+"ø"+base_2+"u"+base_3+"nar" ;
                               Acc => base_1+"ø"+base_2+"u"+base_3+"nar" ;
                               Dat => base_1+"ø"+base_2+base_3+"unum" ;
                               Gen => base_1+"ø"+base_2+base_3+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN135"
  } ;

mkN136 : Str -> N ;
mkN136 base =
  case base of {
    base_1+"tu"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"tu"+base_2 ;
                                 Acc => base_1+"tu"+base_2 ;
                                 Dat => base_1+base_2+"i" ;
                                 Gen => base_1+"tu"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+base_2+"ar" ;
                                 Acc => base_1+base_2+"ar" ;
                                 Dat => base_1+base_2+"um" ;
                                 Gen => base_1+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"tu"+base_2+"in" ;
                               Acc => base_1+"tu"+base_2+"in" ;
                               Dat => base_1+base_2+"inum" ;
                               Gen => base_1+"tu"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+base_2+"arnir" ;
                               Acc => base_1+base_2+"arnar" ;
                               Dat => base_1+base_2+"unum" ;
                               Gen => base_1+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN136"
  } ;

mkN137 : Str -> N ;
mkN137 base =
  case base of {
    base_1+"á"+base_2@?+"t" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"á"+base_2+"t" ;
                                 Acc => base_1+"á"+base_2+"t" ;
                                 Dat => base_1+"á"+base_2+"t" ;
                                 Gen => base_1+"á"+base_2+"tar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"æ"+base_2+"ur" ;
                                 Acc => base_1+"æ"+base_2+"ur" ;
                                 Dat => base_1+"á"+base_2+"tum" ;
                                 Gen => base_1+"á"+base_2+"ta"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"á"+base_2+"tin" ;
                               Acc => base_1+"á"+base_2+"tina" ;
                               Dat => base_1+"á"+base_2+"tini" ;
                               Gen => base_1+"á"+base_2+"tarinnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"æ"+base_2+"urnar" ;
                               Acc => base_1+"á"+base_2+"turnar" ;
                               Dat => base_1+"á"+base_2+"tunum" ;
                               Gen => base_1+"á"+base_2+"tanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN137"
  } ;

mkN138 : Str -> N ;
mkN138 base =
  case base of {
    base_1 => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1+"i" ;
                                 Gen => base_1
                               } ;
                         Pl => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1+"jum" ;
                                 Gen => base_1+"ja"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ið" ;
                               Acc => base_1+"ið" ;
                               Dat => base_1+"inum" ;
                               Gen => base_1+"ins"
                             } ;
                       Pl => table {
                               Nom => base_1+"ini" ;
                               Acc => base_1+"ini" ;
                               Dat => base_1+"junum" ;
                               Gen => base_1+"janna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN138"
  } ;

mkN139 : Str -> N ;
mkN139 base =
  case base of {
    base_1@?+"ø"+base_2 => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ø"+base_2 ;
                                 Acc => base_1+"ø"+base_2 ;
                                 Dat => base_1+"ø"+base_2 ;
                                 Gen => base_1+"a"+base_2+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"a"+base_2+"ar" ;
                                 Acc => base_1+"a"+base_2+"ar" ;
                                 Dat => base_1+"ø"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ø"+base_2+"in" ;
                               Acc => base_1+"ø"+base_2+"ina" ;
                               Dat => base_1+"ø"+base_2+"ini" ;
                               Gen => base_1+"a"+base_2+"arinnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+"arnar" ;
                               Acc => base_1+"a"+base_2+"arnar" ;
                               Dat => base_1+"ø"+base_2+"unum" ;
                               Gen => base_1+"a"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN139"
  } ;

mkN140 : Str -> N ;
mkN140 base =
  case base of {
    base_1+"ma"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ma"+base_2 ;
                                 Acc => base_1+"ma"+base_2 ;
                                 Dat => base_1+"ma"+base_2+"i" ;
                                 Gen => base_1+"ma"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"mu"+base_2 ;
                                 Acc => base_1+"mu"+base_2 ;
                                 Dat => base_1+base_2+"um" ;
                                 Gen => base_1+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ma"+base_2+"ið" ;
                               Acc => base_1+"ma"+base_2+"ið" ;
                               Dat => base_1+"ma"+base_2+"inum" ;
                               Gen => base_1+"ma"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+base_2+"ini" ;
                               Acc => base_1+"mu"+base_2+"ini" ;
                               Dat => base_1+base_2+"unum" ;
                               Gen => base_1+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN140"
  } ;

mkN141 : Str -> N ;
mkN141 base =
  case base of {
    base_1+"dur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"dur" ;
                                 Acc => base_1+"d" ;
                                 Dat => base_1+"di" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"dar" ;
                                 Acc => base_1+"dar" ;
                                 Dat => base_1+"dum" ;
                                 Gen => base_1+"da"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"durin" ;
                               Acc => base_1+"din" ;
                               Dat => base_1+"dinum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"darnir" ;
                               Acc => base_1+"darnar" ;
                               Dat => base_1+"dunum" ;
                               Gen => base_1+"danna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN141"
  } ;

mkN142 : Str -> N ;
mkN142 base =
  case base of {
    "ø"+base_1 => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => "ø"+base_1 ;
                                 Acc => "ø"+base_1 ;
                                 Dat => "ø"+base_1 ;
                                 Gen => "a"+base_1+"ar"
                               } ;
                         Pl => table {
                                 Nom => "ø"+base_1+"ir" ;
                                 Acc => "ø"+base_1+"ir" ;
                                 Dat => "ø"+base_1+"um" ;
                                 Gen => "a"+base_1+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => "ø"+base_1+"in" ;
                               Acc => "ø"+base_1+"ina" ;
                               Dat => "ø"+base_1+"ini" ;
                               Gen => "a"+base_1+"arinnar"
                             } ;
                       Pl => table {
                               Nom => "ø"+base_1+"irnar" ;
                               Acc => "ø"+base_1+"irnar" ;
                               Dat => "ø"+base_1+"unum" ;
                               Gen => "a"+base_1+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN142"
  } ;

mkN143 : Str -> N ;
mkN143 base =
  case base of {
    "o"+base_1 => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => "o"+base_1 ;
                                 Acc => "o"+base_1 ;
                                 Dat => "o"+base_1 ;
                                 Gen => "a"+base_1+"ar"
                               } ;
                         Pl => table {
                                 Nom => "a"+base_1+"ir" ;
                                 Acc => "a"+base_1+"ir" ;
                                 Dat => "o"+base_1+"um" ;
                                 Gen => "a"+base_1+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => "o"+base_1+"in" ;
                               Acc => "o"+base_1+"ina" ;
                               Dat => "o"+base_1+"ini" ;
                               Gen => "a"+base_1+"arinnar"
                             } ;
                       Pl => table {
                               Nom => "a"+base_1+"irnar" ;
                               Acc => "a"+base_1+"irnar" ;
                               Dat => "o"+base_1+"unum" ;
                               Gen => "a"+base_1+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN143"
  } ;

mkN144 : Str -> N ;
mkN144 base =
  case base of {
    "ø"+base_1 => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => "ø"+base_1 ;
                                 Acc => "ø"+base_1 ;
                                 Dat => "ø"+base_1 ;
                                 Gen => "a"+base_1+"ar"
                               } ;
                         Pl => table {
                                 Nom => "a"+base_1+"ir" ;
                                 Acc => "a"+base_1+"ir" ;
                                 Dat => "ø"+base_1+"um" ;
                                 Gen => "a"+base_1+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => "ø"+base_1+"in" ;
                               Acc => "ø"+base_1+"ina" ;
                               Dat => "ø"+base_1+"ini" ;
                               Gen => "a"+base_1+"arinnar"
                             } ;
                       Pl => table {
                               Nom => "a"+base_1+"irnar" ;
                               Acc => "a"+base_1+"irnar" ;
                               Dat => "ø"+base_1+"unum" ;
                               Gen => "a"+base_1+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN144"
  } ;

mkN145 : Str -> N ;
mkN145 base =
  case base of {
    base_1+"p" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"p" ;
                                 Acc => base_1+"p" ;
                                 Dat => base_1+"pi" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"pið" ;
                                 Acc => base_1+"pið" ;
                                 Dat => base_1+"pinum" ;
                                 Gen => base_1+"sins"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN145"
  } ;

mkN146 : Str -> N ;
mkN146 base =
  case base of {
    base_1+"kur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"kur" ;
                                 Acc => base_1+"k" ;
                                 Dat => base_1+"ki" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"kurin" ;
                                 Acc => base_1+"kin" ;
                                 Dat => base_1+"kinum" ;
                                 Gen => base_1+"sins"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN146"
  } ;

mkN147 : Str -> N ;
mkN147 base =
  case base of {
    base_1+"pur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"pur" ;
                                 Acc => base_1+"p" ;
                                 Dat => base_1+"pi" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"purin" ;
                                 Acc => base_1+"pin" ;
                                 Dat => base_1+"pinum" ;
                                 Gen => base_1+"sins"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN147"
  } ;

mkN148 : Str -> N ;
mkN148 base =
  case base of {
    base_1+"g"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"g"+base_2 ;
                                 Acc => base_1+"g"+base_2 ;
                                 Dat => base_1+"g"+base_2+"i" ;
                                 Gen => base_1+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"g"+base_2 ;
                                 Acc => base_1+"g"+base_2 ;
                                 Dat => base_1+"g"+base_2+"um" ;
                                 Gen => base_1+"g"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"g"+base_2+"ið" ;
                               Acc => base_1+"g"+base_2+"ið" ;
                               Dat => base_1+"g"+base_2+"num" ;
                               Gen => base_1+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"g"+base_2+"ini" ;
                               Acc => base_1+"g"+base_2+"ini" ;
                               Dat => base_1+"g"+base_2+"unum" ;
                               Gen => base_1+"g"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN148"
  } ;

mkN149 : Str -> N ;
mkN149 base =
  case base of {
    base_1+"lur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"lur" ;
                                 Acc => base_1+"l" ;
                                 Dat => base_1+"li" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"lar" ;
                                 Acc => base_1+"lar" ;
                                 Dat => base_1+"lum" ;
                                 Gen => base_1+"la"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"lurin" ;
                               Acc => base_1+"lin" ;
                               Dat => base_1+"linum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"larnir" ;
                               Acc => base_1+"larnar" ;
                               Dat => base_1+"lunum" ;
                               Gen => base_1+"lanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN149"
  } ;

mkN150 : Str -> N ;
mkN150 base =
  case base of {
    base_1+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ur" ;
                                 Acc => base_1 ;
                                 Dat => base_1+"i" ;
                                 Gen => base_1+"jar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ir" ;
                                 Acc => base_1+"ir" ;
                                 Dat => base_1+"jum" ;
                                 Gen => base_1+"ja"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"urin" ;
                               Acc => base_1+"in" ;
                               Dat => base_1+"inum" ;
                               Gen => base_1+"jarins"
                             } ;
                       Pl => table {
                               Nom => base_1+"irnir" ;
                               Acc => base_1+"irnar" ;
                               Dat => base_1+"junum" ;
                               Gen => base_1+"janna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN150"
  } ;

mkN151 : Str -> N ;
mkN151 base =
  case base of {
    base_1+"a"+base_2@(?+?)+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"ur" ;
                                 Acc => base_1+"a"+base_2 ;
                                 Dat => base_1+"a"+base_2+"i" ;
                                 Gen => base_1+"a"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"a"+base_2+"ir" ;
                                 Acc => base_1+"a"+base_2+"ir" ;
                                 Dat => base_1+"o"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"a"+base_2+"urin" ;
                               Acc => base_1+"a"+base_2+"in" ;
                               Dat => base_1+"a"+base_2+"inum" ;
                               Gen => base_1+"a"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+"irnir" ;
                               Acc => base_1+"a"+base_2+"irnar" ;
                               Dat => base_1+"o"+base_2+"unum" ;
                               Gen => base_1+"a"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN151"
  } ;

mkN152 : Str -> N ;
mkN152 base =
  case base of {
    base_1+"n" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"n" ;
                                 Acc => base_1+"n" ;
                                 Dat => base_1+"ni" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"n" ;
                                 Acc => base_1+"n" ;
                                 Dat => base_1+"num" ;
                                 Gen => base_1+"na"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"nið" ;
                               Acc => base_1+"nið" ;
                               Dat => base_1+"ninum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"nini" ;
                               Acc => base_1+"nini" ;
                               Dat => base_1+"nunum" ;
                               Gen => base_1+"nanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN152"
  } ;

mkN153 : Str -> N ;
mkN153 base =
  case base of {
    base_1+"gvur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"gvur" ;
                                 Acc => base_1+"gv" ;
                                 Dat => base_1+"gvi" ;
                                 Gen => base_1+"var"
                               } ;
                         Pl => table {
                                 Nom => base_1+"gvar" ;
                                 Acc => base_1+"gvar" ;
                                 Dat => base_1+"gvum" ;
                                 Gen => base_1+"gva"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"gvurin" ;
                               Acc => base_1+"gvin" ;
                               Dat => base_1+"num" ;
                               Gen => base_1+"varins"
                             } ;
                       Pl => table {
                               Nom => base_1+"gvarnir" ;
                               Acc => base_1+"gvarnar" ;
                               Dat => base_1+"gvunum" ;
                               Gen => base_1+"gvanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN153"
  } ;

mkN154 : Str -> N ;
mkN154 base =
  case base of {
    base_1+"g" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"g" ;
                                 Acc => base_1+"g" ;
                                 Dat => base_1+"gi" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"g" ;
                                 Acc => base_1+"g" ;
                                 Dat => base_1+"gum" ;
                                 Gen => base_1+"ga"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"gið" ;
                               Acc => base_1+"gið" ;
                               Dat => base_1+"ginum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"gini" ;
                               Acc => base_1+"gini" ;
                               Dat => base_1+"gunum" ;
                               Gen => base_1+"ganna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN154"
  } ;

mkN155 : Str -> N ;
mkN155 base =
  case base of {
    base_1+"t" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"t" ;
                                 Acc => base_1+"t" ;
                                 Dat => base_1+"ti" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"t" ;
                                 Acc => base_1+"t" ;
                                 Dat => base_1+"tum" ;
                                 Gen => base_1+"ta"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"tið" ;
                               Acc => base_1+"tið" ;
                               Dat => base_1+"tinum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"tini" ;
                               Acc => base_1+"tini" ;
                               Dat => base_1+"tunum" ;
                               Gen => base_1+"tanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN155"
  } ;

mkN156 : Str -> N ;
mkN156 base =
  case base of {
    base_1+"gvur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"gvur" ;
                                 Acc => base_1+"gv" ;
                                 Dat => base_1+"gvi" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"gvurin" ;
                                 Acc => base_1+"gvin" ;
                                 Dat => base_1+"gvinum" ;
                                 Gen => base_1+"sins"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN156"
  } ;

mkN157 : Str -> N ;
mkN157 base =
  case base of {
    base_1+"o"+base_2@(?+?) => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"o"+base_2 ;
                                 Acc => base_1+"o"+base_2 ;
                                 Dat => base_1+"o"+base_2 ;
                                 Gen => base_1+"ei"+base_2+"jar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ei"+base_2+"ir" ;
                                 Acc => base_1+"ei"+base_2+"ir" ;
                                 Dat => base_1+"ei"+base_2+"jum" ;
                                 Gen => base_1+"ei"+base_2+"ja"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"o"+base_2+"in" ;
                               Acc => base_1+"o"+base_2+"ina" ;
                               Dat => base_1+"o"+base_2+"ini" ;
                               Gen => base_1+"ei"+base_2+"jarrinar"
                             } ;
                       Pl => table {
                               Nom => base_1+"ei"+base_2+"irnar" ;
                               Acc => base_1+"ei"+base_2+"irnar" ;
                               Dat => base_1+"ei"+base_2+"junum" ;
                               Gen => base_1+"ei"+base_2+"janna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN157"
  } ;

mkN158 : Str -> N ;
mkN158 base =
  case base of {
    base_1+"ó"+base_2@?+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ó"+base_2+"ur" ;
                                 Acc => base_1+"ó"+base_2 ;
                                 Dat => base_1+"ó"+base_2+"i" ;
                                 Gen => base_1+"ó"+base_2+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2+"ir" ;
                                 Acc => base_1+"ø"+base_2+"ir" ;
                                 Dat => base_1+"ø"+base_2+"um" ;
                                 Gen => base_1+"ó"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ó"+base_2+"urin" ;
                               Acc => base_1+"ó"+base_2+"in" ;
                               Dat => base_1+"ó"+base_2+"inum" ;
                               Gen => base_1+"ó"+base_2+"arins"
                             } ;
                       Pl => table {
                               Nom => base_1+"ø"+base_2+"irnir" ;
                               Acc => base_1+"ø"+base_2+"irnar" ;
                               Dat => base_1+"ø"+base_2+"unum" ;
                               Gen => base_1+"ó"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN158"
  } ;

mkN159 : Str -> N ;
mkN159 base =
  case base of {
    base_1+"ma"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ma"+base_2 ;
                                 Acc => base_1+"ma"+base_2 ;
                                 Dat => base_1+base_2+"i" ;
                                 Gen => base_1+"ma"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"mu"+base_2 ;
                                 Acc => base_1+"mu"+base_2 ;
                                 Dat => base_1+base_2+"um" ;
                                 Gen => base_1+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+base_2+"ið" ;
                               Acc => base_1+base_2+"ið" ;
                               Dat => base_1+base_2+"inum" ;
                               Gen => base_1+"ma"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+base_2+"ini" ;
                               Acc => base_1+base_2+"ini" ;
                               Dat => base_1+base_2+"unum" ;
                               Gen => base_1+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN159"
  } ;

mkN160 : Str -> N ;
mkN160 base =
  case base of {
    base_1+"pur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"pur" ;
                                 Acc => base_1+"p" ;
                                 Dat => base_1+"pi" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"pir" ;
                                 Acc => base_1+"pir" ;
                                 Dat => base_1+"pum" ;
                                 Gen => base_1+"pa"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"purin" ;
                               Acc => base_1+"pin" ;
                               Dat => base_1+"pinum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"pirnir" ;
                               Acc => base_1+"pirnar" ;
                               Dat => base_1+"punum" ;
                               Gen => base_1+"panna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN160"
  } ;

mkN161 : Str -> N ;
mkN161 base =
  case base of {
    base_1+"i"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"i"+base_2 ;
                                 Acc => base_1+"u"+base_2 ;
                                 Dat => base_1+"u"+base_2 ;
                                 Gen => base_1+"u"+base_2
                               } ;
                         Pl => table {
                                 Nom => base_1+base_2+"ar" ;
                                 Acc => base_1+base_2+"ar" ;
                                 Dat => base_1+base_2+"um" ;
                                 Gen => base_1+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"i"+base_2+"in" ;
                               Acc => base_1+base_2+"ina" ;
                               Dat => base_1+base_2+"ini" ;
                               Gen => base_1+"u"+base_2+"innar"
                             } ;
                       Pl => table {
                               Nom => base_1+base_2+"arnar" ;
                               Acc => base_1+base_2+"arnar" ;
                               Dat => base_1+base_2+"unum" ;
                               Gen => base_1+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN161"
  } ;

mkN162 : Str -> N ;
mkN162 base =
  case base of {
    base_1+"a"+base_2@(?+?)+"i" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"i" ;
                                 Acc => base_1+"a"+base_2+"a" ;
                                 Dat => base_1+"a"+base_2+"a" ;
                                 Gen => base_1+"a"+base_2+"a"
                               } ;
                         Pl => table {
                                 Nom => base_1+"a"+base_2+"ar" ;
                                 Acc => base_1+"a"+base_2+"ar" ;
                                 Dat => base_1+"o"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"a"+base_2+"in" ;
                               Acc => base_1+"a"+base_2+"an" ;
                               Dat => base_1+"a"+base_2+"anum" ;
                               Gen => base_1+"a"+base_2+"ans"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+"arnir" ;
                               Acc => base_1+"a"+base_2+"arnar" ;
                               Dat => base_1+"o"+base_2+"unum" ;
                               Gen => base_1+"a"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN162"
  } ;

mkN163 : Str -> N ;
mkN163 base =
  case base of {
    base_1+"k" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"k" ;
                                 Acc => base_1+"k" ;
                                 Dat => base_1+"ki" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"kið" ;
                                 Acc => base_1+"kið" ;
                                 Dat => base_1+"kinum" ;
                                 Gen => base_1+"sins"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN163"
  } ;

mkN164 : Str -> N ;
mkN164 base =
  case base of {
    base_1+"g" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"g" ;
                                 Acc => base_1+"g" ;
                                 Dat => base_1+"gi" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"g" ;
                                 Acc => base_1+"g" ;
                                 Dat => base_1+"gjum" ;
                                 Gen => base_1+"gja"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"gið" ;
                               Acc => base_1+"gið" ;
                               Dat => base_1+"ginum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"gini" ;
                               Acc => base_1+"gini" ;
                               Dat => base_1+"gjunum" ;
                               Gen => base_1+"gjanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN164"
  } ;

mkN165 : Str -> N ;
mkN165 base =
  case base of {
    base_1+"o"+base_2@(?+?) => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"o"+base_2 ;
                                 Acc => base_1+"o"+base_2 ;
                                 Dat => base_1+"o"+base_2 ;
                                 Gen => base_1+"a"+base_2+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"e"+base_2 ;
                                 Acc => base_1+"e"+base_2 ;
                                 Dat => base_1+"o"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"o"+base_2+"in" ;
                               Acc => base_1+"o"+base_2+"ina" ;
                               Dat => base_1+"o"+base_2+"ini" ;
                               Gen => base_1+"a"+base_2+"arinnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"e"+base_2+"inar" ;
                               Acc => base_1+"e"+base_2+"inar" ;
                               Dat => base_1+"o"+base_2+"unum" ;
                               Gen => base_1+"a"+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN165"
  } ;

mkN166 : Str -> N ;
mkN166 base =
  case base of {
    base_1+"gv" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"gv" ;
                                 Acc => base_1+"gv" ;
                                 Dat => base_1+"gv" ;
                                 Gen => base_1+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"gvir" ;
                                 Acc => base_1+"gvir" ;
                                 Dat => base_1+"gvum" ;
                                 Gen => base_1+"gva"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"gvin" ;
                               Acc => base_1+"nna" ;
                               Dat => base_1+"nni" ;
                               Gen => base_1+"arinnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"gvirnar" ;
                               Acc => base_1+"gvirnar" ;
                               Dat => base_1+"gvunum" ;
                               Gen => base_1+"gvanna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN166"
  } ;

mkN167 : Str -> N ;
mkN167 base =
  case base of {
    base_1+"ni"+base_2@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ni"+base_2 ;
                                 Acc => base_1+"ni"+base_2 ;
                                 Dat => base_1+base_2+"i" ;
                                 Gen => base_1+"ni"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+base_2+"ar" ;
                                 Acc => base_1+base_2+"ar" ;
                                 Dat => base_1+base_2+"um" ;
                                 Gen => base_1+base_2+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ni"+base_2+"in" ;
                               Acc => base_1+"ni"+base_2+"in" ;
                               Dat => base_1+base_2+"inum" ;
                               Gen => base_1+"ni"+base_2+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+base_2+"arnir" ;
                               Acc => base_1+base_2+"arnar" ;
                               Dat => base_1+base_2+"unum" ;
                               Gen => base_1+base_2+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN167"
  } ;

mkN168 : Str -> N ;
mkN168 base =
  case base of {
    base_1+"ur" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ur" ;
                                 Acc => base_1 ;
                                 Dat => base_1+"i" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ir" ;
                                 Acc => base_1+"ir" ;
                                 Dat => base_1+"jum" ;
                                 Gen => base_1+"ja"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"urin" ;
                               Acc => base_1+"in" ;
                               Dat => base_1+"inum" ;
                               Gen => base_1+"sins"
                             } ;
                       Pl => table {
                               Nom => base_1+"irnir" ;
                               Acc => base_1+"irnar" ;
                               Dat => base_1+"junum" ;
                               Gen => base_1+"janna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN168"
  } ;

mkN169 : Str -> N ;
mkN169 base =
  case base of {
    base_1+"l" => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"l" ;
                                 Acc => base_1+"l" ;
                                 Dat => base_1+"li" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"lið" ;
                                 Acc => base_1+"lið" ;
                                 Dat => base_1+"linum" ;
                                 Gen => base_1+"sins"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             } ;
                       Pl => table {
                               Nom => nonExist ;
                               Acc => nonExist ;
                               Dat => nonExist ;
                               Gen => nonExist
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN169"
  } ;

mkN170 : Str -> N ;
mkN170 base =
  case base of {
    base_1+"ø"+base_2@(?+?+?)+"u"+base_3@? => lin N
      { s = table {
              Indef => table {
                         Sg => table {
                                 Nom => base_1+"ø"+base_2+"u"+base_3 ;
                                 Acc => base_1+"ø"+base_2+"u"+base_3 ;
                                 Dat => base_1+"ø"+base_2+base_3+"i" ;
                                 Gen => base_1+"a"+base_2+base_3+"ar"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2+base_3+"ir" ;
                                 Acc => base_1+"ø"+base_2+base_3+"ir" ;
                                 Dat => base_1+"ø"+base_2+base_3+"um" ;
                                 Gen => base_1+"a"+base_2+base_3+"a"
                               }
                       } ;
              Def => table {
                       Sg => table {
                               Nom => base_1+"ø"+base_2+"u"+base_3+"in" ;
                               Acc => base_1+"ø"+base_2+"u"+base_3+"in" ;
                               Dat => base_1+"ø"+base_2+base_3+"inum" ;
                               Gen => base_1+"a"+base_2+base_3+"arins"
                             } ;
                       Pl => table {
                               Nom => base_1+"ø"+base_2+base_3+"irnir" ;
                               Acc => base_1+"ø"+base_2+base_3+"irnar" ;
                               Dat => base_1+"ø"+base_2+base_3+"unum" ;
                               Gen => base_1+"a"+base_2+base_3+"anna"
                             }
                     }
            }
      };
    _ => error "Can't apply paradigm mkN170"
  } ;

mkA001 : Str -> A ;
mkA001 base =
  case base of {
    base_1+"dur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"dur" ;
                                Acc => base_1+"dan" ;
                                Dat => base_1+"dum" ;
                                Gen => base_1+"ds"
                              } ;
                        Pl => table {
                                Nom => base_1+"dir" ;
                                Acc => base_1+"dar" ;
                                Dat => base_1+"dum" ;
                                Gen => base_1+"da"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"d" ;
                               Acc => base_1+"da" ;
                               Dat => base_1+"dari" ;
                               Gen => base_1+"dar"
                             } ;
                       Pl => table {
                               Nom => base_1+"dar" ;
                               Acc => base_1+"dar" ;
                               Dat => base_1+"dum" ;
                               Gen => base_1+"da"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"t" ;
                                 Acc => base_1+"t" ;
                                 Dat => base_1+"dum" ;
                                 Gen => base_1+"ds"
                               } ;
                         Pl => table {
                                 Nom => base_1+"d" ;
                                 Acc => base_1+"d" ;
                                 Dat => base_1+"dum" ;
                                 Gen => base_1+"da"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA001"
  } ;

mkA002 : Str -> A ;
mkA002 base =
  case base of {
    base_1+"a"+base_2@(?+?)+"ur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"a"+base_2+"ur" ;
                                Acc => base_1+"a"+base_2+"an" ;
                                Dat => base_1+"ø"+base_2+"um" ;
                                Gen => base_1+"a"+base_2+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"a"+base_2+"ir" ;
                                Acc => base_1+"a"+base_2+"ar" ;
                                Dat => base_1+"ø"+base_2+"um" ;
                                Gen => base_1+"a"+base_2+"a"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"ø"+base_2 ;
                               Acc => base_1+"a"+base_2+"a" ;
                               Dat => base_1+"a"+base_2+"ari" ;
                               Gen => base_1+"a"+base_2+"ar"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+"ar" ;
                               Acc => base_1+"a"+base_2+"ar" ;
                               Dat => base_1+"ø"+base_2+"um" ;
                               Gen => base_1+"a"+base_2+"a"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2 ;
                                 Acc => base_1+"a"+base_2 ;
                                 Dat => base_1+"ø"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2 ;
                                 Acc => base_1+"ø"+base_2 ;
                                 Dat => base_1+"ø"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"a"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA002"
  } ;

mkA003 : Str -> A ;
mkA003 base =
  case base of {
    base_1+"ur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"ur" ;
                                Acc => base_1+"an" ;
                                Dat => base_1+"um" ;
                                Gen => base_1+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"ir" ;
                                Acc => base_1+"ar" ;
                                Dat => base_1+"um" ;
                                Gen => base_1+"a"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1 ;
                               Acc => base_1+"a" ;
                               Dat => base_1+"ari" ;
                               Gen => base_1+"ar"
                             } ;
                       Pl => table {
                               Nom => base_1+"ar" ;
                               Acc => base_1+"ar" ;
                               Dat => base_1+"um" ;
                               Gen => base_1+"a"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA003"
  } ;

mkA004 : Str -> A ;
mkA004 base =
  case base of {
    base_1+"a"+base_2@?+"u"+base_3@? => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"a"+base_2+"u"+base_3 ;
                                Acc => base_1+"a"+base_2+base_3+"an" ;
                                Dat => base_1+"ø"+base_2+base_3+"um" ;
                                Gen => base_1+"a"+base_2+"u"+base_3+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"a"+base_2+base_3+"ir" ;
                                Acc => base_1+"a"+base_2+base_3+"ar" ;
                                Dat => base_1+"ø"+base_2+base_3+"um" ;
                                Gen => base_1+"a"+base_2+base_3+"a"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"ø"+base_2+"u"+base_3 ;
                               Acc => base_1+"a"+base_2+base_3+"a" ;
                               Dat => base_1+"a"+base_2+base_3+"ari" ;
                               Gen => base_1+"a"+base_2+base_3+"ar"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+base_3+"ar" ;
                               Acc => base_1+"a"+base_2+base_3+"ar" ;
                               Dat => base_1+"ø"+base_2+base_3+"um" ;
                               Gen => base_1+"a"+base_2+base_3+"a"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"u"+base_3+"t" ;
                                 Acc => base_1+"a"+base_2+"u"+base_3+"t" ;
                                 Dat => base_1+"ø"+base_2+base_3+"um" ;
                                 Gen => base_1+"a"+base_2+"u"+base_3+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2+"u"+base_3 ;
                                 Acc => base_1+"ø"+base_2+"u"+base_3 ;
                                 Dat => base_1+"ø"+base_2+base_3+"um" ;
                                 Gen => base_1+"a"+base_2+base_3+"a"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA004"
  } ;

mkA005 : Str -> A ;
mkA005 base =
  case base of {
    "a"+base_1+"lur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => "a"+base_1+"lur" ;
                                Acc => "a"+base_1+"lan" ;
                                Dat => "ø"+base_1+"lum" ;
                                Gen => "a"+base_1+"s"
                              } ;
                        Pl => table {
                                Nom => "a"+base_1+"lir" ;
                                Acc => "a"+base_1+"lar" ;
                                Dat => "ø"+base_1+"lum" ;
                                Gen => "a"+base_1+"la"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => "ø"+base_1+"l" ;
                               Acc => "a"+base_1+"la" ;
                               Dat => "a"+base_1+"lari" ;
                               Gen => "a"+base_1+"lar"
                             } ;
                       Pl => table {
                               Nom => "a"+base_1+"lar" ;
                               Acc => "a"+base_1+"lar" ;
                               Dat => "ø"+base_1+"lum" ;
                               Gen => "a"+base_1+"la"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => "a"+base_1+"t" ;
                                 Acc => "a"+base_1+"t" ;
                                 Dat => "ø"+base_1+"lum" ;
                                 Gen => "a"+base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => "ø"+base_1+"l" ;
                                 Acc => "ø"+base_1+"l" ;
                                 Dat => "ø"+base_1+"lum" ;
                                 Gen => "a"+base_1+"la"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA005"
  } ;

mkA006 : Str -> A ;
mkA006 base =
  case base of {
    base_1+"nur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"nur" ;
                                Acc => base_1+"nan" ;
                                Dat => base_1+"num" ;
                                Gen => base_1+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"nir" ;
                                Acc => base_1+"nar" ;
                                Dat => base_1+"num" ;
                                Gen => base_1+"na"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"n" ;
                               Acc => base_1+"na" ;
                               Dat => base_1+"nari" ;
                               Gen => base_1+"nar"
                             } ;
                       Pl => table {
                               Nom => base_1+"nar" ;
                               Acc => base_1+"nar" ;
                               Dat => base_1+"num" ;
                               Gen => base_1+"na"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"t" ;
                                 Acc => base_1+"t" ;
                                 Dat => base_1+"num" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"n" ;
                                 Acc => base_1+"n" ;
                                 Dat => base_1+"num" ;
                                 Gen => base_1+"na"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA006"
  } ;

mkA007 : Str -> A ;
mkA007 base =
  case base of {
    base_1+"ur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"ur" ;
                                Acc => base_1+"an" ;
                                Dat => base_1+"um" ;
                                Gen => base_1+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"ir" ;
                                Acc => base_1+"ar" ;
                                Dat => base_1+"um" ;
                                Gen => base_1+"a"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1 ;
                               Acc => base_1+"a" ;
                               Dat => base_1+"ari" ;
                               Gen => base_1+"ar"
                             } ;
                       Pl => table {
                               Nom => base_1+"ar" ;
                               Acc => base_1+"ar" ;
                               Dat => base_1+"um" ;
                               Gen => base_1+"a"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"t" ;
                                 Acc => base_1+"t" ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA007"
  } ;

mkA008 : Str -> A ;
mkA008 base =
  case base of {
    base_1+"ur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"ur" ;
                                Acc => base_1+"an" ;
                                Dat => base_1+"um" ;
                                Gen => base_1
                              } ;
                        Pl => table {
                                Nom => base_1+"ir" ;
                                Acc => base_1+"ar" ;
                                Dat => base_1+"um" ;
                                Gen => base_1+"a"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1 ;
                               Acc => base_1+"a" ;
                               Dat => base_1+"ari" ;
                               Gen => base_1+"ar"
                             } ;
                       Pl => table {
                               Nom => base_1+"ar" ;
                               Acc => base_1+"ar" ;
                               Dat => base_1+"um" ;
                               Gen => base_1+"a"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"t" ;
                                 Acc => base_1+"t" ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1
                               } ;
                         Pl => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA008"
  } ;

mkA009 : Str -> A ;
mkA009 base =
  case base of {
    base_1+"in" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"in" ;
                                Acc => base_1+"nan" ;
                                Dat => base_1+"num" ;
                                Gen => base_1+"ins"
                              } ;
                        Pl => table {
                                Nom => base_1+"nir" ;
                                Acc => base_1+"nar" ;
                                Dat => base_1+"num" ;
                                Gen => base_1+"na"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"in" ;
                               Acc => base_1+"na" ;
                               Dat => base_1+"nari" ;
                               Gen => base_1+"nar"
                             } ;
                       Pl => table {
                               Nom => base_1+"nar" ;
                               Acc => base_1+"nar" ;
                               Dat => base_1+"num" ;
                               Gen => base_1+"na"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"ið" ;
                                 Acc => base_1+"ið" ;
                                 Dat => base_1+"num" ;
                                 Gen => base_1+"ins"
                               } ;
                         Pl => table {
                                 Nom => base_1+"in" ;
                                 Acc => base_1+"in" ;
                                 Dat => base_1+"num" ;
                                 Gen => base_1+"na"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA009"
  } ;

mkA010 : Str -> A ;
mkA010 base =
  case base of {
    base_1+"ður" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"ður" ;
                                Acc => base_1+"ðan" ;
                                Dat => base_1+"ðum" ;
                                Gen => base_1+"ðs"
                              } ;
                        Pl => table {
                                Nom => base_1+"ðir" ;
                                Acc => base_1+"ðar" ;
                                Dat => base_1+"ðum" ;
                                Gen => base_1+"ða"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"ð" ;
                               Acc => base_1+"ða" ;
                               Dat => base_1+"ðari" ;
                               Gen => base_1+"ðar"
                             } ;
                       Pl => table {
                               Nom => base_1+"ðar" ;
                               Acc => base_1+"ðar" ;
                               Dat => base_1+"ðum" ;
                               Gen => base_1+"ða"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"tt" ;
                                 Acc => base_1+"tt" ;
                                 Dat => base_1+"ðum" ;
                                 Gen => base_1+"ðs"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ð" ;
                                 Acc => base_1+"ð" ;
                                 Dat => base_1+"ðum" ;
                                 Gen => base_1+"ða"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA010"
  } ;

mkA011 : Str -> A ;
mkA011 base =
  case base of {
    base_1+"tur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"tur" ;
                                Acc => base_1+"tan" ;
                                Dat => base_1+"tum" ;
                                Gen => base_1+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"tir" ;
                                Acc => base_1+"tar" ;
                                Dat => base_1+"tum" ;
                                Gen => base_1+"ta"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"t" ;
                               Acc => base_1+"ta" ;
                               Dat => base_1+"tari" ;
                               Gen => base_1+"tar"
                             } ;
                       Pl => table {
                               Nom => base_1+"tar" ;
                               Acc => base_1+"tar" ;
                               Dat => base_1+"tum" ;
                               Gen => base_1+"ta"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"t" ;
                                 Acc => base_1+"t" ;
                                 Dat => base_1+"tum" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"t" ;
                                 Acc => base_1+"t" ;
                                 Dat => base_1+"tum" ;
                                 Gen => base_1+"ta"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA011"
  } ;

mkA012 : Str -> A ;
mkA012 base =
  case base of {
    base_1+"gvin" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"gvin" ;
                                Acc => base_1+"nan" ;
                                Dat => base_1+"num" ;
                                Gen => base_1+"gvins"
                              } ;
                        Pl => table {
                                Nom => base_1+"nir" ;
                                Acc => base_1+"nar" ;
                                Dat => base_1+"num" ;
                                Gen => base_1+"na"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"gvin" ;
                               Acc => base_1+"na" ;
                               Dat => base_1+"nari" ;
                               Gen => base_1+"nar"
                             } ;
                       Pl => table {
                               Nom => base_1+"nar" ;
                               Acc => base_1+"nar" ;
                               Dat => base_1+"num" ;
                               Gen => base_1+"na"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"gvið" ;
                                 Acc => base_1+"gvið" ;
                                 Dat => base_1+"num" ;
                                 Gen => base_1+"gvins"
                               } ;
                         Pl => table {
                                 Nom => base_1+"gvin" ;
                                 Acc => base_1+"gvin" ;
                                 Dat => base_1+"num" ;
                                 Gen => base_1+"na"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA012"
  } ;

mkA013 : Str -> A ;
mkA013 base =
  case base of {
    base_1+"gur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"gur" ;
                                Acc => base_1+"gan" ;
                                Dat => base_1+"gum" ;
                                Gen => base_1+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"gir" ;
                                Acc => base_1+"gar" ;
                                Dat => base_1+"gum" ;
                                Gen => base_1+"ga"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"g" ;
                               Acc => base_1+"ga" ;
                               Dat => base_1+"gari" ;
                               Gen => base_1+"gar"
                             } ;
                       Pl => table {
                               Nom => base_1+"gar" ;
                               Acc => base_1+"gar" ;
                               Dat => base_1+"gum" ;
                               Gen => base_1+"ga"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"t" ;
                                 Acc => base_1+"t" ;
                                 Dat => base_1+"gum" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"g" ;
                                 Acc => base_1+"g" ;
                                 Dat => base_1+"gum" ;
                                 Gen => base_1+"ga"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA013"
  } ;

mkA014 : Str -> A ;
mkA014 base =
  case base of {
    base_1+"ddur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"ddur" ;
                                Acc => base_1+"ddan" ;
                                Dat => base_1+"ddum" ;
                                Gen => base_1+"øs"
                              } ;
                        Pl => table {
                                Nom => base_1+"ddir" ;
                                Acc => base_1+"ddar" ;
                                Dat => base_1+"ddum" ;
                                Gen => base_1+"dda"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"dd" ;
                               Acc => base_1+"dda" ;
                               Dat => base_1+"ddari" ;
                               Gen => base_1+"ddar"
                             } ;
                       Pl => table {
                               Nom => base_1+"ddar" ;
                               Acc => base_1+"ddar" ;
                               Dat => base_1+"ddum" ;
                               Gen => base_1+"dda"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"tt" ;
                                 Acc => base_1+"tt" ;
                                 Dat => base_1+"ddum" ;
                                 Gen => base_1+"øs"
                               } ;
                         Pl => table {
                                 Nom => base_1+"dd" ;
                                 Acc => base_1+"dd" ;
                                 Dat => base_1+"ddum" ;
                                 Gen => base_1+"dda"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA014"
  } ;

mkA015 : Str -> A ;
mkA015 base =
  case base of {
    base_1+"a"+base_2@("ng"|"m"|(?+?+?))+"ur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"a"+base_2+"ur" ;
                                Acc => base_1+"a"+base_2+"an" ;
                                Dat => base_1+"o"+base_2+"um" ;
                                Gen => base_1+"a"+base_2+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"a"+base_2+"ir" ;
                                Acc => base_1+"a"+base_2+"ar" ;
                                Dat => base_1+"o"+base_2+"um" ;
                                Gen => base_1+"a"+base_2+"a"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"o"+base_2 ;
                               Acc => base_1+"a"+base_2+"a" ;
                               Dat => base_1+"a"+base_2+"ari" ;
                               Gen => base_1+"a"+base_2+"ar"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+"ar" ;
                               Acc => base_1+"a"+base_2+"ar" ;
                               Dat => base_1+"o"+base_2+"um" ;
                               Gen => base_1+"a"+base_2+"a"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"t" ;
                                 Acc => base_1+"a"+base_2+"t" ;
                                 Dat => base_1+"o"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"o"+base_2 ;
                                 Acc => base_1+"o"+base_2 ;
                                 Dat => base_1+"o"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"a"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA015"
  } ;

mkA016 : Str -> A ;
mkA016 base =
  case base of {
    base_1+"mur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"mur" ;
                                Acc => base_1+"man" ;
                                Dat => base_1+"mum" ;
                                Gen => base_1+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"mir" ;
                                Acc => base_1+"mar" ;
                                Dat => base_1+"mum" ;
                                Gen => base_1+"madimra"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"m" ;
                               Acc => base_1+"ma" ;
                               Dat => base_1+"mari" ;
                               Gen => base_1+"mar"
                             } ;
                       Pl => table {
                               Nom => base_1+"mar" ;
                               Acc => base_1+"mar" ;
                               Dat => base_1+"mum" ;
                               Gen => base_1+"madimra"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"t" ;
                                 Acc => base_1+"t" ;
                                 Dat => base_1+"mum" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"m" ;
                                 Acc => base_1+"m" ;
                                 Dat => base_1+"mum" ;
                                 Gen => base_1+"madimra"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA016"
  } ;

mkA017 : Str -> A ;
mkA017 base =
  case base of {
    base_1+"ur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"ur" ;
                                Acc => base_1+"an" ;
                                Dat => base_1+"um" ;
                                Gen => base_1+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"ir" ;
                                Acc => base_1+"ar" ;
                                Dat => base_1+"um" ;
                                Gen => base_1+"a"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1 ;
                               Acc => base_1+"a" ;
                               Dat => base_1+"ari" ;
                               Gen => base_1+"ar"
                             } ;
                       Pl => table {
                               Nom => base_1+"ar" ;
                               Acc => base_1+"ar" ;
                               Dat => base_1+"um" ;
                               Gen => base_1+"a"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"tt" ;
                                 Acc => base_1+"tt" ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1 ;
                                 Acc => base_1 ;
                                 Dat => base_1+"um" ;
                                 Gen => base_1+"a"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA017"
  } ;

mkA018 : Str -> A ;
mkA018 base =
  case base of {
    base_1+"kur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"kur" ;
                                Acc => base_1+"kan" ;
                                Dat => base_1+"kum" ;
                                Gen => base_1+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"kir" ;
                                Acc => base_1+"kar" ;
                                Dat => base_1+"kum" ;
                                Gen => base_1+"kadøkra"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"k" ;
                               Acc => base_1+"ka" ;
                               Dat => base_1+"kari" ;
                               Gen => base_1+"kar"
                             } ;
                       Pl => table {
                               Nom => base_1+"kar" ;
                               Acc => base_1+"kar" ;
                               Dat => base_1+"kum" ;
                               Gen => base_1+"kadøkra"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"t" ;
                                 Acc => base_1+"t" ;
                                 Dat => base_1+"kum" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"k" ;
                                 Acc => base_1+"k" ;
                                 Dat => base_1+"kum" ;
                                 Gen => base_1+"kadøkra"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA018"
  } ;

mkA019 : Str -> A ;
mkA019 base =
  case base of {
    base_1+"tin" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"tin" ;
                                Acc => base_1+"nan" ;
                                Dat => base_1+"num" ;
                                Gen => base_1+"tins"
                              } ;
                        Pl => table {
                                Nom => base_1+"nir" ;
                                Acc => base_1+"nar" ;
                                Dat => base_1+"num" ;
                                Gen => base_1+"na"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"tin" ;
                               Acc => base_1+"na" ;
                               Dat => base_1+"nari" ;
                               Gen => base_1+"nar"
                             } ;
                       Pl => table {
                               Nom => base_1+"nar" ;
                               Acc => base_1+"nar" ;
                               Dat => base_1+"num" ;
                               Gen => base_1+"na"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"tið" ;
                                 Acc => base_1+"tið" ;
                                 Dat => base_1+"num" ;
                                 Gen => base_1+"tins"
                               } ;
                         Pl => table {
                                 Nom => base_1+"tin" ;
                                 Acc => base_1+"tin" ;
                                 Dat => base_1+"num" ;
                                 Gen => base_1+"na"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA019"
  } ;

mkA020 : Str -> A ;
mkA020 base =
  case base of {
    base_1+"a"+base_2@?+"dur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"a"+base_2+"dur" ;
                                Acc => base_1+"a"+base_2+"dan" ;
                                Dat => base_1+"ø"+base_2+"dum" ;
                                Gen => base_1+"a"+base_2+"ds"
                              } ;
                        Pl => table {
                                Nom => base_1+"a"+base_2+"dir" ;
                                Acc => base_1+"a"+base_2+"dar" ;
                                Dat => base_1+"ø"+base_2+"dum" ;
                                Gen => base_1+"a"+base_2+"da"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"ø"+base_2+"d" ;
                               Acc => base_1+"a"+base_2+"da" ;
                               Dat => base_1+"a"+base_2+"dari" ;
                               Gen => base_1+"a"+base_2+"dar"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+"dar" ;
                               Acc => base_1+"a"+base_2+"dar" ;
                               Dat => base_1+"ø"+base_2+"dum" ;
                               Gen => base_1+"a"+base_2+"da"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"t" ;
                                 Acc => base_1+"a"+base_2+"t" ;
                                 Dat => base_1+"ø"+base_2+"dum" ;
                                 Gen => base_1+"a"+base_2+"ds"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2+"d" ;
                                 Acc => base_1+"ø"+base_2+"d" ;
                                 Dat => base_1+"ø"+base_2+"dum" ;
                                 Gen => base_1+"a"+base_2+"da"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA020"
  } ;

mkA021 : Str -> A ;
mkA021 base =
  case base of {
    base_1+"a"+base_2@?+"lur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"a"+base_2+"lur" ;
                                Acc => base_1+"a"+base_2+"lan" ;
                                Dat => base_1+"ø"+base_2+"lum" ;
                                Gen => base_1+"a"+base_2+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"a"+base_2+"lir" ;
                                Acc => base_1+"a"+base_2+"lar" ;
                                Dat => base_1+"ø"+base_2+"lum" ;
                                Gen => base_1+"a"+base_2+"la"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"ø"+base_2+"l" ;
                               Acc => base_1+"a"+base_2+"la" ;
                               Dat => base_1+"a"+base_2+"lari" ;
                               Gen => base_1+"a"+base_2+"lar"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+"lar" ;
                               Acc => base_1+"a"+base_2+"lar" ;
                               Dat => base_1+"ø"+base_2+"lum" ;
                               Gen => base_1+"a"+base_2+"la"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"t" ;
                                 Acc => base_1+"a"+base_2+"t" ;
                                 Dat => base_1+"ø"+base_2+"lum" ;
                                 Gen => base_1+"a"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2+"l" ;
                                 Acc => base_1+"ø"+base_2+"l" ;
                                 Dat => base_1+"ø"+base_2+"lum" ;
                                 Gen => base_1+"a"+base_2+"la"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA021"
  } ;

mkA022 : Str -> A ;
mkA022 base =
  case base of {
    base_1+"a"+base_2@?+"ur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"a"+base_2+"ur" ;
                                Acc => base_1+"a"+base_2+"an" ;
                                Dat => base_1+"ø"+base_2+"um" ;
                                Gen => base_1+"a"+base_2+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"a"+base_2+"ir" ;
                                Acc => base_1+"a"+base_2+"ar" ;
                                Dat => base_1+"ø"+base_2+"um" ;
                                Gen => base_1+"a"+base_2+"a"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"ø"+base_2 ;
                               Acc => base_1+"a"+base_2+"a" ;
                               Dat => base_1+"a"+base_2+"ari" ;
                               Gen => base_1+"a"+base_2+"ar"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+"ar" ;
                               Acc => base_1+"a"+base_2+"ar" ;
                               Dat => base_1+"ø"+base_2+"um" ;
                               Gen => base_1+"a"+base_2+"a"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"t" ;
                                 Acc => base_1+"a"+base_2+"t" ;
                                 Dat => base_1+"ø"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2 ;
                                 Acc => base_1+"ø"+base_2 ;
                                 Dat => base_1+"ø"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"a"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA022"
  } ;

mkA023 : Str -> A ;
mkA023 base =
  case base of {
    base_1+"rin" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"rin" ;
                                Acc => base_1+"nan" ;
                                Dat => base_1+"num" ;
                                Gen => base_1+"rins"
                              } ;
                        Pl => table {
                                Nom => base_1+"nir" ;
                                Acc => base_1+"nar" ;
                                Dat => base_1+"num" ;
                                Gen => base_1+"na"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"rin" ;
                               Acc => base_1+"na" ;
                               Dat => base_1+"nari" ;
                               Gen => base_1+"nar"
                             } ;
                       Pl => table {
                               Nom => base_1+"nar" ;
                               Acc => base_1+"nar" ;
                               Dat => base_1+"num" ;
                               Gen => base_1+"na"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"rið" ;
                                 Acc => base_1+"rið" ;
                                 Dat => base_1+"num" ;
                                 Gen => base_1+"rins"
                               } ;
                         Pl => table {
                                 Nom => base_1+"rin" ;
                                 Acc => base_1+"rin" ;
                                 Dat => base_1+"num" ;
                                 Gen => base_1+"na"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA023"
  } ;

mkA024 : Str -> A ;
mkA024 base =
  case base of {
    base_1+"lur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"lur" ;
                                Acc => base_1+"lan" ;
                                Dat => base_1+"lum" ;
                                Gen => base_1+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"lir" ;
                                Acc => base_1+"lar" ;
                                Dat => base_1+"lum" ;
                                Gen => base_1+"lafulra"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"l" ;
                               Acc => base_1+"la" ;
                               Dat => base_1+"lari" ;
                               Gen => base_1+"lar"
                             } ;
                       Pl => table {
                               Nom => base_1+"lar" ;
                               Acc => base_1+"lar" ;
                               Dat => base_1+"lum" ;
                               Gen => base_1+"lafulra"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"t" ;
                                 Acc => base_1+"t" ;
                                 Dat => base_1+"lum" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"l" ;
                                 Acc => base_1+"l" ;
                                 Dat => base_1+"lum" ;
                                 Gen => base_1+"lafulra"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA024"
  } ;

mkA025 : Str -> A ;
mkA025 base =
  case base of {
    base_1+"a"+base_2@?+"a"+base_3@? => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"a"+base_2+"a"+base_3 ;
                                Acc => base_1+"a"+base_2+base_3+"an" ;
                                Dat => base_1+"o"+base_2+base_3+"um" ;
                                Gen => base_1+"a"+base_2+"a"+base_3+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"a"+base_2+base_3+"ir" ;
                                Acc => base_1+"a"+base_2+base_3+"ar" ;
                                Dat => base_1+"o"+base_2+base_3+"um" ;
                                Gen => base_1+"a"+base_2+base_3+"a"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"o"+base_2+"u"+base_3 ;
                               Acc => base_1+"a"+base_2+base_3+"a" ;
                               Dat => base_1+"a"+base_2+base_3+"ari" ;
                               Gen => base_1+"a"+base_2+base_3+"ar"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+base_3+"ar" ;
                               Acc => base_1+"a"+base_2+base_3+"ar" ;
                               Dat => base_1+"o"+base_2+base_3+"um" ;
                               Gen => base_1+"a"+base_2+base_3+"a"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"a"+base_3+"t" ;
                                 Acc => base_1+"a"+base_2+"a"+base_3+"t" ;
                                 Dat => base_1+"o"+base_2+base_3+"um" ;
                                 Gen => base_1+"a"+base_2+base_3+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"o"+base_2+"u"+base_3 ;
                                 Acc => base_1+"o"+base_2+"u"+base_3 ;
                                 Dat => base_1+"o"+base_2+base_3+"um" ;
                                 Gen => base_1+"a"+base_2+base_3+"a"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA025"
  } ;

mkA026 : Str -> A ;
mkA026 base =
  case base of {
    base_1+"óður" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"óður" ;
                                Acc => base_1+"óðan" ;
                                Dat => base_1+"óðum" ;
                                Gen => base_1+"óðs"
                              } ;
                        Pl => table {
                                Nom => base_1+"óðir" ;
                                Acc => base_1+"óðar" ;
                                Dat => base_1+"óðum" ;
                                Gen => base_1+"óða"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"óð" ;
                               Acc => base_1+"óða" ;
                               Dat => base_1+"óðari" ;
                               Gen => base_1+"óðar"
                             } ;
                       Pl => table {
                               Nom => base_1+"óðar" ;
                               Acc => base_1+"óðar" ;
                               Dat => base_1+"óðum" ;
                               Gen => base_1+"óða"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"ott" ;
                                 Acc => base_1+"ott" ;
                                 Dat => base_1+"óðum" ;
                                 Gen => base_1+"óðs"
                               } ;
                         Pl => table {
                                 Nom => base_1+"óð" ;
                                 Acc => base_1+"óð" ;
                                 Dat => base_1+"óðum" ;
                                 Gen => base_1+"óða"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA026"
  } ;

mkA027 : Str -> A ;
mkA027 base =
  case base of {
    base_1+"ei"+base_2@?+"a"+base_3@?+"ur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"ei"+base_2+"a"+base_3+"ur" ;
                                Acc => base_1+"a"+base_2+base_3+"an" ;
                                Dat => base_1+"a"+base_2+base_3+"um" ;
                                Gen => base_1+"ei"+base_2+"a"+base_3+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"a"+base_2+base_3+"ir" ;
                                Acc => base_1+"a"+base_2+base_3+"ar" ;
                                Dat => base_1+"a"+base_2+base_3+"um" ;
                                Gen => base_1+"ei"+base_2+"a"+base_3+"a"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"ei"+base_2+"u"+base_3 ;
                               Acc => base_1+"a"+base_2+base_3+"a" ;
                               Dat => base_1+"a"+base_2+base_3+"ari" ;
                               Gen => base_1+"ei"+base_2+"a"+base_3+"ar"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+base_3+"ar" ;
                               Acc => base_1+"a"+base_2+base_3+"ar" ;
                               Dat => base_1+"a"+base_2+base_3+"um" ;
                               Gen => base_1+"ei"+base_2+"a"+base_3+"a"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"ei"+base_2+"a"+base_3+"t" ;
                                 Acc => base_1+"ei"+base_2+"a"+base_3+"t" ;
                                 Dat => base_1+"a"+base_2+base_3+"um" ;
                                 Gen => base_1+"ei"+base_2+"a"+base_3+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ei"+base_2+"u"+base_3 ;
                                 Acc => base_1+"ei"+base_2+"u"+base_3 ;
                                 Dat => base_1+"a"+base_2+base_3+"um" ;
                                 Gen => base_1+"ei"+base_2+"a"+base_3+"a"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA027"
  } ;

mkA028 : Str -> A ;
mkA028 base =
  case base of {
    base_1+"pin" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"pin" ;
                                Acc => base_1+"nan" ;
                                Dat => base_1+"num" ;
                                Gen => base_1+"pins"
                              } ;
                        Pl => table {
                                Nom => base_1+"nir" ;
                                Acc => base_1+"nar" ;
                                Dat => base_1+"num" ;
                                Gen => base_1+"na"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"pin" ;
                               Acc => base_1+"na" ;
                               Dat => base_1+"nari" ;
                               Gen => base_1+"nar"
                             } ;
                       Pl => table {
                               Nom => base_1+"nar" ;
                               Acc => base_1+"nar" ;
                               Dat => base_1+"num" ;
                               Gen => base_1+"na"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"pið" ;
                                 Acc => base_1+"pið" ;
                                 Dat => base_1+"num" ;
                                 Gen => base_1+"pins"
                               } ;
                         Pl => table {
                                 Nom => base_1+"pin" ;
                                 Acc => base_1+"pin" ;
                                 Dat => base_1+"num" ;
                                 Gen => base_1+"na"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA028"
  } ;

mkA029 : Str -> A ;
mkA029 base =
  case base of {
    base_1+"a"+base_2@?+"sur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"a"+base_2+"sur" ;
                                Acc => base_1+"a"+base_2+"san" ;
                                Dat => base_1+"ø"+base_2+"sum" ;
                                Gen => base_1+"a"+base_2+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"a"+base_2+"sir" ;
                                Acc => base_1+"a"+base_2+"sar" ;
                                Dat => base_1+"ø"+base_2+"sum" ;
                                Gen => base_1+"a"+base_2+"sa"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"ø"+base_2+"s" ;
                               Acc => base_1+"a"+base_2+"sa" ;
                               Dat => base_1+"a"+base_2+"sari" ;
                               Gen => base_1+"a"+base_2+"sar"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+"sar" ;
                               Acc => base_1+"a"+base_2+"sar" ;
                               Dat => base_1+"ø"+base_2+"sum" ;
                               Gen => base_1+"a"+base_2+"sa"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"t" ;
                                 Acc => base_1+"a"+base_2+"t" ;
                                 Dat => base_1+"ø"+base_2+"sum" ;
                                 Gen => base_1+"a"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2+"s" ;
                                 Acc => base_1+"ø"+base_2+"s" ;
                                 Dat => base_1+"ø"+base_2+"sum" ;
                                 Gen => base_1+"a"+base_2+"sa"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA029"
  } ;

mkA030 : Str -> A ;
mkA030 base =
  case base of {
    base_1+"ddur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"ddur" ;
                                Acc => base_1+"ddan" ;
                                Dat => base_1+"ddum" ;
                                Gen => base_1+"ds"
                              } ;
                        Pl => table {
                                Nom => base_1+"ddir" ;
                                Acc => base_1+"ddar" ;
                                Dat => base_1+"ddum" ;
                                Gen => base_1+"dda"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"dd" ;
                               Acc => base_1+"dda" ;
                               Dat => base_1+"ddari" ;
                               Gen => base_1+"ddar"
                             } ;
                       Pl => table {
                               Nom => base_1+"ddar" ;
                               Acc => base_1+"ddar" ;
                               Dat => base_1+"ddum" ;
                               Gen => base_1+"dda"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"tt" ;
                                 Acc => base_1+"tt" ;
                                 Dat => base_1+"ddum" ;
                                 Gen => base_1+"ds"
                               } ;
                         Pl => table {
                                 Nom => base_1+"dd" ;
                                 Acc => base_1+"dd" ;
                                 Dat => base_1+"ddum" ;
                                 Gen => base_1+"dda"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA030"
  } ;

mkA031 : Str -> A ;
mkA031 base =
  case base of {
    base_1+"a"+base_2@?+"ur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"a"+base_2+"ur" ;
                                Acc => base_1+"a"+base_2+"an" ;
                                Dat => base_1+"ø"+base_2+"um" ;
                                Gen => base_1+"a"+base_2+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"a"+base_2+"ir" ;
                                Acc => base_1+"a"+base_2+"ar" ;
                                Dat => base_1+"ø"+base_2+"um" ;
                                Gen => base_1+"a"+base_2+"alatra"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"ø"+base_2 ;
                               Acc => base_1+"a"+base_2+"a" ;
                               Dat => base_1+"a"+base_2+"ari" ;
                               Gen => base_1+"a"+base_2+"ar"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+"ar" ;
                               Acc => base_1+"a"+base_2+"ar" ;
                               Dat => base_1+"ø"+base_2+"um" ;
                               Gen => base_1+"a"+base_2+"alatra"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"t" ;
                                 Acc => base_1+"a"+base_2+"t" ;
                                 Dat => base_1+"ø"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2 ;
                                 Acc => base_1+"ø"+base_2 ;
                                 Dat => base_1+"ø"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"alatra"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA031"
  } ;

mkA032 : Str -> A ;
mkA032 base =
  case base of {
    base_1+"ðin" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"ðin" ;
                                Acc => base_1+"dnan" ;
                                Dat => base_1+"dnum" ;
                                Gen => base_1+"ðins"
                              } ;
                        Pl => table {
                                Nom => base_1+"dnir" ;
                                Acc => base_1+"dnar" ;
                                Dat => base_1+"dnum" ;
                                Gen => base_1+"dna"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"ðin" ;
                               Acc => base_1+"dna" ;
                               Dat => base_1+"dnari" ;
                               Gen => base_1+"dnar"
                             } ;
                       Pl => table {
                               Nom => base_1+"dnar" ;
                               Acc => base_1+"dnar" ;
                               Dat => base_1+"dnum" ;
                               Gen => base_1+"dna"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"ðið" ;
                                 Acc => base_1+"ðið" ;
                                 Dat => base_1+"dnum" ;
                                 Gen => base_1+"ðins"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ðin" ;
                                 Acc => base_1+"ðin" ;
                                 Dat => base_1+"dnum" ;
                                 Gen => base_1+"dna"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA032"
  } ;

mkA033 : Str -> A ;
mkA033 base =
  case base of {
    base_1+"ggjur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"ggjur" ;
                                Acc => base_1+"ggjan" ;
                                Dat => base_1+"ggjum" ;
                                Gen => base_1+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"ggir" ;
                                Acc => base_1+"ggjar" ;
                                Dat => base_1+"ggjum" ;
                                Gen => base_1+"ggja"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"ggj" ;
                               Acc => base_1+"ggja" ;
                               Dat => base_1+"ggjari" ;
                               Gen => base_1+"ggjar"
                             } ;
                       Pl => table {
                               Nom => base_1+"ggjar" ;
                               Acc => base_1+"ggjar" ;
                               Dat => base_1+"ggjum" ;
                               Gen => base_1+"ggja"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"tt" ;
                                 Acc => base_1+"tt" ;
                                 Dat => base_1+"ggjum" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ggj" ;
                                 Acc => base_1+"ggj" ;
                                 Dat => base_1+"ggjum" ;
                                 Gen => base_1+"ggja"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA033"
  } ;

mkA034 : Str -> A ;
mkA034 base =
  case base of {
    base_1+"a"+base_2@?+"ur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"a"+base_2+"ur" ;
                                Acc => base_1+"a"+base_2+"an" ;
                                Dat => base_1+"ø"+base_2+"um" ;
                                Gen => base_1+"a"+base_2+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"a"+base_2+"ir" ;
                                Acc => base_1+"a"+base_2+"ar" ;
                                Dat => base_1+"ø"+base_2+"um" ;
                                Gen => base_1+"a"+base_2+"amakra"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"ø"+base_2 ;
                               Acc => base_1+"a"+base_2+"a" ;
                               Dat => base_1+"a"+base_2+"ari" ;
                               Gen => base_1+"a"+base_2+"ar"
                             } ;
                       Pl => table {
                               Nom => base_1+"a"+base_2+"ar" ;
                               Acc => base_1+"a"+base_2+"ar" ;
                               Dat => base_1+"ø"+base_2+"um" ;
                               Gen => base_1+"a"+base_2+"amakra"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"a"+base_2+"t" ;
                                 Acc => base_1+"a"+base_2+"t" ;
                                 Dat => base_1+"ø"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"ø"+base_2 ;
                                 Acc => base_1+"ø"+base_2 ;
                                 Dat => base_1+"ø"+base_2+"um" ;
                                 Gen => base_1+"a"+base_2+"amakra"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA034"
  } ;

mkA035 : Str -> A ;
mkA035 base =
  case base of {
    base_1+"il" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"il" ;
                                Acc => base_1+"lan" ;
                                Dat => base_1+"lum" ;
                                Gen => base_1+"ils"
                              } ;
                        Pl => table {
                                Nom => base_1+"lir" ;
                                Acc => base_1+"lar" ;
                                Dat => base_1+"lum" ;
                                Gen => base_1+"la"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"il" ;
                               Acc => base_1+"la" ;
                               Dat => base_1+"lari" ;
                               Gen => base_1+"lar"
                             } ;
                       Pl => table {
                               Nom => base_1+"lar" ;
                               Acc => base_1+"lar" ;
                               Dat => base_1+"lum" ;
                               Gen => base_1+"la"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"ið" ;
                                 Acc => base_1+"ið" ;
                                 Dat => base_1+"lum" ;
                                 Gen => base_1+"ils"
                               } ;
                         Pl => table {
                                 Nom => base_1+"il" ;
                                 Acc => base_1+"il" ;
                                 Dat => base_1+"lum" ;
                                 Gen => base_1+"la"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA035"
  } ;

mkA036 : Str -> A ;
mkA036 base =
  case base of {
    base_1+"lur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"lur" ;
                                Acc => base_1+"lan" ;
                                Dat => base_1+"lum" ;
                                Gen => base_1+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"lir" ;
                                Acc => base_1+"lar" ;
                                Dat => base_1+"lum" ;
                                Gen => base_1+"la"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"l" ;
                               Acc => base_1+"la" ;
                               Dat => base_1+"lari" ;
                               Gen => base_1+"lar"
                             } ;
                       Pl => table {
                               Nom => base_1+"lar" ;
                               Acc => base_1+"lar" ;
                               Dat => base_1+"lum" ;
                               Gen => base_1+"la"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"t" ;
                                 Acc => base_1+"t" ;
                                 Dat => base_1+"lum" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"l" ;
                                 Acc => base_1+"l" ;
                                 Dat => base_1+"lum" ;
                                 Gen => base_1+"la"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA036"
  } ;

mkA037 : Str -> A ;
mkA037 base =
  case base of {
    base_1+"gvur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"gvur" ;
                                Acc => base_1+"gvan" ;
                                Dat => base_1+"gvum" ;
                                Gen => base_1+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"gvir" ;
                                Acc => base_1+"gvar" ;
                                Dat => base_1+"gvum" ;
                                Gen => base_1+"gva"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"gv" ;
                               Acc => base_1+"gva" ;
                               Dat => base_1+"gvari" ;
                               Gen => base_1+"gvar"
                             } ;
                       Pl => table {
                               Nom => base_1+"gvar" ;
                               Acc => base_1+"gvar" ;
                               Dat => base_1+"gvum" ;
                               Gen => base_1+"gva"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"tt" ;
                                 Acc => base_1+"tt" ;
                                 Dat => base_1+"gvum" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"gv" ;
                                 Acc => base_1+"gv" ;
                                 Dat => base_1+"gvum" ;
                                 Gen => base_1+"gva"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA037"
  } ;

mkA038 : Str -> A ;
mkA038 base =
  case base of {
    base_1+"u"+base_2@? => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"u"+base_2 ;
                                Acc => base_1+base_2+"an" ;
                                Dat => base_1+base_2+"um" ;
                                Gen => base_1+"u"+base_2+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+base_2+"ir" ;
                                Acc => base_1+base_2+"ar" ;
                                Dat => base_1+base_2+"um" ;
                                Gen => base_1+base_2+"a"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"u"+base_2 ;
                               Acc => base_1+base_2+"a" ;
                               Dat => base_1+base_2+"ari" ;
                               Gen => base_1+base_2+"ar"
                             } ;
                       Pl => table {
                               Nom => base_1+base_2+"ar" ;
                               Acc => base_1+base_2+"ar" ;
                               Dat => base_1+base_2+"um" ;
                               Gen => base_1+base_2+"a"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"u"+base_2+"t" ;
                                 Acc => base_1+"u"+base_2+"t" ;
                                 Dat => base_1+base_2+"um" ;
                                 Gen => base_1+"u"+base_2+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"u"+base_2 ;
                                 Acc => base_1+"u"+base_2 ;
                                 Dat => base_1+base_2+"um" ;
                                 Gen => base_1+base_2+"a"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA038"
  } ;

mkA039 : Str -> A ;
mkA039 base =
  case base of {
    base_1+"rur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"rur" ;
                                Acc => base_1+"ran" ;
                                Dat => base_1+"rum" ;
                                Gen => base_1+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"rir" ;
                                Acc => base_1+"rar" ;
                                Dat => base_1+"rum" ;
                                Gen => base_1+"ra"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"r" ;
                               Acc => base_1+"ra" ;
                               Dat => base_1+"rari" ;
                               Gen => base_1+"rar"
                             } ;
                       Pl => table {
                               Nom => base_1+"rar" ;
                               Acc => base_1+"rar" ;
                               Dat => base_1+"rum" ;
                               Gen => base_1+"ra"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"t" ;
                                 Acc => base_1+"t" ;
                                 Dat => base_1+"rum" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"r" ;
                                 Acc => base_1+"r" ;
                                 Dat => base_1+"rum" ;
                                 Gen => base_1+"ra"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA039"
  } ;

mkA040 : Str -> A ;
mkA040 base =
  case base of {
    base_1+"lur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"lur" ;
                                Acc => base_1+"lan" ;
                                Dat => base_1+"lum" ;
                                Gen => base_1+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"lir" ;
                                Acc => base_1+"lar" ;
                                Dat => base_1+"lum" ;
                                Gen => base_1+"lavilra"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"l" ;
                               Acc => base_1+"la" ;
                               Dat => base_1+"lari" ;
                               Gen => base_1+"lar"
                             } ;
                       Pl => table {
                               Nom => base_1+"lar" ;
                               Acc => base_1+"lar" ;
                               Dat => base_1+"lum" ;
                               Gen => base_1+"lavilra"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"t" ;
                                 Acc => base_1+"t" ;
                                 Dat => base_1+"lum" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"l" ;
                                 Acc => base_1+"l" ;
                                 Dat => base_1+"lum" ;
                                 Gen => base_1+"lavilra"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA040"
  } ;

mkA041 : Str -> A ;
mkA041 base =
  case base of {
    base_1+"sur" => lin A
      { s = table {
              Masc => table {
                        Sg => table {
                                Nom => base_1+"sur" ;
                                Acc => base_1+"san" ;
                                Dat => base_1+"sum" ;
                                Gen => base_1+"s"
                              } ;
                        Pl => table {
                                Nom => base_1+"sir" ;
                                Acc => base_1+"sar" ;
                                Dat => base_1+"sum" ;
                                Gen => base_1+"sa"
                              }
                      } ;
              Fem => table {
                       Sg => table {
                               Nom => base_1+"s" ;
                               Acc => base_1+"sa" ;
                               Dat => base_1+"sari" ;
                               Gen => base_1+"sar"
                             } ;
                       Pl => table {
                               Nom => base_1+"sar" ;
                               Acc => base_1+"sar" ;
                               Dat => base_1+"sum" ;
                               Gen => base_1+"sa"
                             }
                     } ;
              Neutr => table {
                         Sg => table {
                                 Nom => base_1+"t" ;
                                 Acc => base_1+"t" ;
                                 Dat => base_1+"sum" ;
                                 Gen => base_1+"s"
                               } ;
                         Pl => table {
                                 Nom => base_1+"s" ;
                                 Acc => base_1+"s" ;
                                 Dat => base_1+"sum" ;
                                 Gen => base_1+"sa"
                               }
                       }
            }
      };
    _ => error "Can't apply paradigm mkA041"
  } ;

mkV001 : Str -> V ;
mkV001 base =
  case base of {
    base_1+"a" => lin V
      { Converb = base_1+"að" ;
        Imperative_Jussive = table {
                               Sg => base_1+"a" ;
                               Pl => base_1+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"i" ;
                                 PSg P2 => base_1+"ar" ;
                                 PSg P3 => base_1+"ar" ;
                                 PPl => base_1+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"aði" ;
                                 PSg P2 => base_1+"aði" ;
                                 PSg P3 => base_1+"aði" ;
                                 PPl => base_1+"aðu"
                               }
                     } ;
        Nonfinite = base_1+"a" ;
        Particle = table {
                     Pres => base_1+"andi" ;
                     Past => base_1+"aður"
                   }
      };
    _ => error "Can't apply paradigm mkV001"
  } ;

mkV002 : Str -> V ;
mkV002 base =
  case base of {
    "ei"+base_1+"a" => lin V
      { Converb = "hi"+base_1+"ið" ;
        Imperative_Jussive = table {
                               Sg => "ei"+base_1 ;
                               Pl => "ei"+base_1+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => "ei"+base_1+"i" ;
                                 PSg P2 => "ei"+base_1+"ur" ;
                                 PSg P3 => "ei"+base_1+"ur" ;
                                 PPl => "ei"+base_1+"a"
                               } ;
                       Past => table {
                                 PSg P1 => "hæ"+base_1 ;
                                 PSg P2 => "hæ"+base_1 ;
                                 PSg P3 => "hæ"+base_1 ;
                                 PPl => "hi"+base_1+"u"
                               }
                     } ;
        Nonfinite = "ei"+base_1+"a" ;
        Particle = table {
                     Pres => "ei"+base_1+"andi" ;
                     Past => "hi"+base_1+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV002"
  } ;

mkV003 : Str -> V ;
mkV003 base =
  case base of {
    "a"+base_1+"a" => lin V
      { Converb = "i"+base_1+"ið" ;
        Imperative_Jussive = table {
                               Sg => "a"+base_1 ;
                               Pl => "a"+base_1+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => "a"+base_1+"i" ;
                                 PSg P2 => "e"+base_1+"ur" ;
                                 PSg P3 => "e"+base_1+"ur" ;
                                 PPl => "a"+base_1+"a"
                               } ;
                       Past => table {
                                 PSg P1 => "ó"+base_1 ;
                                 PSg P2 => "ó"+base_1+"st" ;
                                 PSg P3 => "ó"+base_1 ;
                                 PPl => "ó"+base_1+"u"
                               }
                     } ;
        Nonfinite = "a"+base_1+"a" ;
        Particle = table {
                     Pres => "a"+base_1+"andi" ;
                     Past => "i"+base_1+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV003"
  } ;

mkV004 : Str -> V ;
mkV004 base =
  case base of {
    "a"+base_1+"a" => lin V
      { Converb = "a"+base_1+"ið" ;
        Imperative_Jussive = table {
                               Sg => "a"+base_1 ;
                               Pl => "a"+base_1+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => "a"+base_1+"i" ;
                                 PSg P2 => "e"+base_1+"ur" ;
                                 PSg P3 => "e"+base_1+"ur" ;
                                 PPl => "a"+base_1+"a"
                               } ;
                       Past => table {
                                 PSg P1 => "ó"+base_1 ;
                                 PSg P2 => "ó"+base_1+"st" ;
                                 PSg P3 => "ó"+base_1 ;
                                 PPl => "ó"+base_1+"u"
                               }
                     } ;
        Nonfinite = "a"+base_1+"a" ;
        Particle = table {
                     Pres => "a"+base_1+"andi" ;
                     Past => "a"+base_1+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV004"
  } ;

mkV005 : Str -> V ;
mkV005 base =
  case base of {
    base_1+"a"+base_2@(?+?) => lin V
      { Converb = base_1+"a"+base_2 ;
        Imperative_Jussive = table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"i"+base_2 ;
                                 PSg P2 => base_1+"a"+base_2 ;
                                 PSg P3 => base_1+"a"+base_2 ;
                                 PPl => base_1+"a"+base_2
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"aði"+base_2 ;
                                 PSg P2 => base_1+"aði"+base_2 ;
                                 PSg P3 => base_1+"aði"+base_2 ;
                                 PPl => base_1+"aðu"+base_2
                               }
                     } ;
        Nonfinite = base_1+"a"+base_2 ;
        Particle = table {
                     Pres => nonExist ;
                     Past => nonExist
                   }
      };
    _ => error "Can't apply paradigm mkV005"
  } ;

mkV006 : Str -> V ;
mkV006 base =
  case base of {
    base_1+"ða" => lin V
      { Converb = base_1+"tt" ;
        Imperative_Jussive = table {
                               Sg => base_1+"ð" ;
                               Pl => base_1+"ðið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ði" ;
                                 PSg P2 => base_1+"ðir" ;
                                 PSg P3 => base_1+"ðir" ;
                                 PPl => base_1+"ða"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ddi" ;
                                 PSg P2 => base_1+"ddi" ;
                                 PSg P3 => base_1+"ddi" ;
                                 PPl => base_1+"ddu"
                               }
                     } ;
        Nonfinite = base_1+"ða" ;
        Particle = table {
                     Pres => base_1+"ðandi" ;
                     Past => base_1+"ddur"
                   }
      };
    _ => error "Can't apply paradigm mkV006"
  } ;

mkV007 : Str -> V ;
mkV007 base =
  case base of {
    "eiga" => lin V
      { Converb = "átt" ;
        Imperative_Jussive = table {
                               Sg => "eig" ;
                               Pl => "eigið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => "eigi" ;
                                 PSg P2 => "eigureigir" ;
                                 PSg P3 => "eigureigir" ;
                                 PPl => "eiga"
                               } ;
                       Past => table {
                                 PSg P1 => "áttiár" ;
                                 PSg P2 => "áttiár" ;
                                 PSg P3 => "áttiár" ;
                                 PPl => "áttu"
                               }
                     } ;
        Nonfinite = "eiga" ;
        Particle = table {
                     Pres => "eigandi" ;
                     Past => nonExist
                   }
      };
    _ => error "Can't apply paradigm mkV007"
  } ;

mkV008 : Str -> V ;
mkV008 base =
  case base of {
    base_1+"e"+base_2@?+"a" => lin V
      { Converb = base_1+"o"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"e"+base_2 ;
                               Pl => base_1+"e"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"e"+base_2+"i" ;
                                 PSg P2 => base_1+"e"+base_2+"t" ;
                                 PSg P3 => base_1+"e"+base_2 ;
                                 PPl => base_1+"e"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2 ;
                                 PSg P2 => base_1+"a"+base_2+"t" ;
                                 PSg P3 => base_1+"a"+base_2 ;
                                 PPl => base_1+"ó"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"e"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"e"+base_2+"andi" ;
                     Past => base_1+"o"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV008"
  } ;

mkV009 : Str -> V ;
mkV009 base =
  case base of {
    base_1+"a" => lin V
      { Converb = base_1+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1 ;
                               Pl => base_1+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"i" ;
                                 PSg P2 => base_1+"ir" ;
                                 PSg P3 => base_1+"ir" ;
                                 PPl => base_1+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"di" ;
                                 PSg P2 => base_1+"di" ;
                                 PSg P3 => base_1+"di" ;
                                 PPl => base_1+"du"
                               }
                     } ;
        Nonfinite = base_1+"a" ;
        Particle = table {
                     Pres => base_1+"andi" ;
                     Past => base_1+"dur"
                   }
      };
    _ => error "Can't apply paradigm mkV009"
  } ;

mkV010 : Str -> V ;
mkV010 base =
  case base of {
    base_1+"da" => lin V
      { Converb = base_1+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"d" ;
                               Pl => base_1+"dið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"di" ;
                                 PSg P2 => base_1+"dir" ;
                                 PSg P3 => base_1+"dir" ;
                                 PPl => base_1+"da"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"di" ;
                                 PSg P2 => base_1+"di" ;
                                 PSg P3 => base_1+"di" ;
                                 PPl => base_1+"du"
                               }
                     } ;
        Nonfinite = base_1+"da" ;
        Particle = table {
                     Pres => base_1+"dandi" ;
                     Past => base_1+"dur"
                   }
      };
    _ => error "Can't apply paradigm mkV010"
  } ;

mkV011 : Str -> V ;
mkV011 base =
  case base of {
    base_1+"e"+base_2@?+"ja" => lin V
      { Converb = base_1+"a"+base_2+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"e"+base_2+"j" ;
                               Pl => base_1+"e"+base_2+"jið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"e"+base_2+"ji" ;
                                 PSg P2 => base_1+"e"+base_2+"jir" ;
                                 PSg P3 => base_1+"e"+base_2+"jir" ;
                                 PPl => base_1+"e"+base_2+"ja"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2+"di" ;
                                 PSg P2 => base_1+"a"+base_2+"di" ;
                                 PSg P3 => base_1+"a"+base_2+"di" ;
                                 PPl => base_1+"a"+base_2+"du"
                               }
                     } ;
        Nonfinite = base_1+"e"+base_2+"ja" ;
        Particle = table {
                     Pres => base_1+"e"+base_2+"jandi" ;
                     Past => base_1+"a"+base_2+"dur"
                   }
      };
    _ => error "Can't apply paradigm mkV011"
  } ;

mkV012 : Str -> V ;
mkV012 base =
  case base of {
    base_1+"i"+base_2@?+"ja" => lin V
      { Converb = base_1+"i"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"i"+base_2 ;
                               Pl => base_1+"i"+base_2+"jið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"i"+base_2+"ji" ;
                                 PSg P2 => base_1+"i"+base_2+"ur" ;
                                 PSg P3 => base_1+"i"+base_2+"ur" ;
                                 PPl => base_1+"i"+base_2+"ja"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2 ;
                                 PSg P2 => base_1+"a"+base_2+"st" ;
                                 PSg P3 => base_1+"a"+base_2 ;
                                 PPl => base_1+"ó"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"i"+base_2+"ja" ;
        Particle = table {
                     Pres => base_1+"i"+base_2+"jandi" ;
                     Past => base_1+"i"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV012"
  } ;

mkV013 : Str -> V ;
mkV013 base =
  case base of {
    base_1+"i"+base_2@?+"da" => lin V
      { Converb = base_1+"u"+base_2+"dið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"i"+base_2+"d" ;
                               Pl => base_1+"i"+base_2+"dið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"i"+base_2+"di" ;
                                 PSg P2 => base_1+"i"+base_2+"dur" ;
                                 PSg P3 => base_1+"i"+base_2+"dur" ;
                                 PPl => base_1+"i"+base_2+"da"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2+"t" ;
                                 PSg P2 => base_1+"a"+base_2+"st" ;
                                 PSg P3 => base_1+"a"+base_2+"t" ;
                                 PPl => base_1+"u"+base_2+"du"
                               }
                     } ;
        Nonfinite = base_1+"i"+base_2+"da" ;
        Particle = table {
                     Pres => base_1+"i"+base_2+"dandi" ;
                     Past => base_1+"u"+base_2+"din"
                   }
      };
    _ => error "Can't apply paradigm mkV013"
  } ;

mkV014 : Str -> V ;
mkV014 base =
  case base of {
    base_1+"í"+base_2@?+"a" => lin V
      { Converb = base_1+"i"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"í"+base_2 ;
                               Pl => base_1+"í"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"í"+base_2+"i" ;
                                 PSg P2 => base_1+"í"+base_2+"ur" ;
                                 PSg P3 => base_1+"í"+base_2+"ur" ;
                                 PPl => base_1+"í"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ei"+base_2 ;
                                 PSg P2 => base_1+"ei"+base_2+"st" ;
                                 PSg P3 => base_1+"ei"+base_2 ;
                                 PPl => base_1+"i"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"í"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"í"+base_2+"andi" ;
                     Past => base_1+"i"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV014"
  } ;

mkV015 : Str -> V ;
mkV015 base =
  case base of {
    base_1+"jó"+base_2@?+"a" => lin V
      { Converb = base_1+"o"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"jó"+base_2 ;
                               Pl => base_1+"jó"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"jó"+base_2+"i" ;
                                 PSg P2 => base_1+"ý"+base_2+"ur" ;
                                 PSg P3 => base_1+"ý"+base_2+"ur" ;
                                 PPl => base_1+"jó"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ey"+base_2 ;
                                 PSg P2 => base_1+"ey"+base_2+"st" ;
                                 PSg P3 => base_1+"ey"+base_2 ;
                                 PPl => base_1+"u"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"jó"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"jó"+base_2+"andi" ;
                     Past => base_1+"o"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV015"
  } ;

mkV016 : Str -> V ;
mkV016 base =
  case base of {
    base_1+"gja" => lin V
      { Converb = base_1+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"g" ;
                               Pl => base_1+"gið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"gi" ;
                                 PSg P2 => base_1+"gir" ;
                                 PSg P3 => base_1+"gir" ;
                                 PPl => base_1+"gja"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"di" ;
                                 PSg P2 => base_1+"di" ;
                                 PSg P3 => base_1+"di" ;
                                 PPl => base_1+"du"
                               }
                     } ;
        Nonfinite = base_1+"gja" ;
        Particle = table {
                     Pres => base_1+"gjandi" ;
                     Past => base_1+"dur"
                   }
      };
    _ => error "Can't apply paradigm mkV016"
  } ;

mkV017 : Str -> V ;
mkV017 base =
  case base of {
    base_1+"e"+base_2@?+"na" => lin V
      { Converb = base_1+"u"+base_2+"nið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"e"+base_2+"n" ;
                               Pl => base_1+"e"+base_2+"nið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"e"+base_2+"ni" ;
                                 PSg P2 => base_1+"e"+base_2+"nur" ;
                                 PSg P3 => base_1+"e"+base_2+"nur" ;
                                 PPl => base_1+"e"+base_2+"na"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2+"n" ;
                                 PSg P2 => base_1+"a"+base_2+"st" ;
                                 PSg P3 => base_1+"a"+base_2+"n" ;
                                 PPl => base_1+"u"+base_2+"nu"
                               }
                     } ;
        Nonfinite = base_1+"e"+base_2+"na" ;
        Particle = table {
                     Pres => base_1+"e"+base_2+"nandi" ;
                     Past => base_1+"u"+base_2+"nin"
                   }
      };
    _ => error "Can't apply paradigm mkV017"
  } ;

mkV018 : Str -> V ;
mkV018 base =
  case base of {
    base_1+"e"+base_2@("m"|(?+?))+"a" => lin V
      { Converb = base_1+"o"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"e"+base_2 ;
                               Pl => base_1+"e"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"e"+base_2+"i" ;
                                 PSg P2 => base_1+"e"+base_2+"ur" ;
                                 PSg P3 => base_1+"e"+base_2+"ur" ;
                                 PPl => base_1+"e"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2 ;
                                 PSg P2 => base_1+"a"+base_2+"st" ;
                                 PSg P3 => base_1+"a"+base_2 ;
                                 PPl => base_1+"u"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"e"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"e"+base_2+"andi" ;
                     Past => base_1+"o"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV018"
  } ;

mkV019 : Str -> V ;
mkV019 base =
  case base of {
    base_1+"ó"+base_2@?+"a" => lin V
      { Converb = base_1+"o"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"ó"+base_2 ;
                               Pl => base_1+"ó"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ó"+base_2+"i" ;
                                 PSg P2 => base_1+"ý"+base_2+"ur" ;
                                 PSg P3 => base_1+"ý"+base_2+"ur" ;
                                 PPl => base_1+"ó"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ey"+base_2 ;
                                 PSg P2 => base_1+"ey"+base_2+"st" ;
                                 PSg P3 => base_1+"ey"+base_2 ;
                                 PPl => base_1+"u"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"ó"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"ó"+base_2+"andi" ;
                     Past => base_1+"o"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV019"
  } ;

mkV020 : Str -> V ;
mkV020 base =
  case base of {
    base_1+"a" => lin V
      { Converb = base_1+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1 ;
                               Pl => base_1+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"i" ;
                                 PSg P2 => base_1+"ir" ;
                                 PSg P3 => base_1+"ir" ;
                                 PPl => base_1+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ti" ;
                                 PSg P2 => base_1+"ti" ;
                                 PSg P3 => base_1+"ti" ;
                                 PPl => base_1+"tu"
                               }
                     } ;
        Nonfinite = base_1+"a" ;
        Particle = table {
                     Pres => base_1+"andi" ;
                     Past => base_1+"tur"
                   }
      };
    _ => error "Can't apply paradigm mkV020"
  } ;

mkV021 : Str -> V ;
mkV021 base =
  case base of {
    base_1+"a" => lin V
      { Converb = base_1+"að" ;
        Imperative_Jussive = table {
                               Sg => base_1+"a" ;
                               Pl => base_1+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"i" ;
                                 PSg P2 => base_1+"ar" ;
                                 PSg P3 => base_1+"ar" ;
                                 PPl => base_1+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ti" ;
                                 PSg P2 => base_1+"ti" ;
                                 PSg P3 => base_1+"ti" ;
                                 PPl => base_1+"tu"
                               }
                     } ;
        Nonfinite = base_1+"a" ;
        Particle = table {
                     Pres => base_1+"andi" ;
                     Past => base_1+"tur"
                   }
      };
    _ => error "Can't apply paradigm mkV021"
  } ;

mkV022 : Str -> V ;
mkV022 base =
  case base of {
    base_1+"úgva" => lin V
      { Converb = base_1+"úð" ;
        Imperative_Jussive = table {
                               Sg => base_1+"úgv" ;
                               Pl => base_1+"úgvið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"úgvi" ;
                                 PSg P2 => base_1+"ýrt" ;
                                 PSg P3 => base_1+"ýr" ;
                                 PPl => base_1+"úgva"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"úði" ;
                                 PSg P2 => base_1+"úði" ;
                                 PSg P3 => base_1+"úði" ;
                                 PPl => base_1+"úðu"
                               }
                     } ;
        Nonfinite = base_1+"úgva" ;
        Particle = table {
                     Pres => base_1+"úgvandi" ;
                     Past => base_1+"úgvin"
                   }
      };
    _ => error "Can't apply paradigm mkV022"
  } ;

mkV023 : Str -> V ;
mkV023 base =
  case base of {
    base_1+"a" => lin V
      { Converb = base_1+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1 ;
                               Pl => base_1+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"i" ;
                                 PSg P2 => base_1+"ur" ;
                                 PSg P3 => base_1+"ur" ;
                                 PPl => base_1+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ti" ;
                                 PSg P2 => base_1+"ti" ;
                                 PSg P3 => base_1+"ti" ;
                                 PPl => base_1+"tu"
                               }
                     } ;
        Nonfinite = base_1+"a" ;
        Particle = table {
                     Pres => base_1+"andi" ;
                     Past => base_1+"tur"
                   }
      };
    _ => error "Can't apply paradigm mkV023"
  } ;

mkV024 : Str -> V ;
mkV024 base =
  case base of {
    base_1+"e"+base_2@?+base_3@?+"a" => lin V
      { Converb = base_1+"o"+base_2+base_3+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"e"+base_2+base_3 ;
                               Pl => base_1+"e"+base_2+base_3+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"e"+base_2+base_3+"i" ;
                                 PSg P2 => base_1+"e"+base_2+base_3+"ur" ;
                                 PSg P3 => base_1+"e"+base_2+base_3+"ur" ;
                                 PPl => base_1+"e"+base_2+base_3+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2+base_3 ;
                                 PSg P2 => base_1+"a"+base_2+"s"+base_3 ;
                                 PSg P3 => base_1+"a"+base_2+base_3 ;
                                 PPl => base_1+"u"+base_2+base_3+"u"
                               }
                     } ;
        Nonfinite = base_1+"e"+base_2+base_3+"a" ;
        Particle = table {
                     Pres => base_1+"e"+base_2+base_3+"andi" ;
                     Past => base_1+"o"+base_2+base_3+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV024"
  } ;

mkV025 : Str -> V ;
mkV025 base =
  case base of {
    base_1+"ggja" => lin V
      { Converb = base_1+"ð" ;
        Imperative_Jussive = table {
                               Sg => base_1+"ggj" ;
                               Pl => base_1+"ggið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ggi" ;
                                 PSg P2 => base_1+"rt" ;
                                 PSg P3 => base_1+"r" ;
                                 PPl => base_1+"ggja"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ði" ;
                                 PSg P2 => base_1+"ði" ;
                                 PSg P3 => base_1+"ði" ;
                                 PPl => base_1+"ðu"
                               }
                     } ;
        Nonfinite = base_1+"ggja" ;
        Particle = table {
                     Pres => base_1+"ggjandi" ;
                     Past => nonExist
                   }
      };
    _ => error "Can't apply paradigm mkV025"
  } ;

mkV026 : Str -> V ;
mkV026 base =
  case base of {
    base_1+"aga" => lin V
      { Converb = base_1+"igið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"ag" ;
                               Pl => base_1+"agið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"agi" ;
                                 PSg P2 => base_1+"egur" ;
                                 PSg P3 => base_1+"egur" ;
                                 PPl => base_1+"aga"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ó" ;
                                 PSg P2 => base_1+"óst" ;
                                 PSg P3 => base_1+"ó" ;
                                 PPl => base_1+"ógu"
                               }
                     } ;
        Nonfinite = base_1+"aga" ;
        Particle = table {
                     Pres => base_1+"agandi" ;
                     Past => base_1+"igin"
                   }
      };
    _ => error "Can't apply paradigm mkV026"
  } ;

mkV027 : Str -> V ;
mkV027 base =
  case base of {
    base_1+"e"+base_2@?+"ka" => lin V
      { Converb = base_1+"u"+base_2+"kið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"e"+base_2+"k" ;
                               Pl => base_1+"e"+base_2+"kið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"e"+base_2+"ki" ;
                                 PSg P2 => base_1+"e"+base_2+"kur" ;
                                 PSg P3 => base_1+"e"+base_2+"kur" ;
                                 PPl => base_1+"e"+base_2+"ka"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2+"k" ;
                                 PSg P2 => base_1+"a"+base_2+"st" ;
                                 PSg P3 => base_1+"a"+base_2+"k" ;
                                 PPl => base_1+"u"+base_2+"ku"
                               }
                     } ;
        Nonfinite = base_1+"e"+base_2+"ka" ;
        Particle = table {
                     Pres => base_1+"e"+base_2+"kandi" ;
                     Past => base_1+"u"+base_2+"kin"
                   }
      };
    _ => error "Can't apply paradigm mkV027"
  } ;

mkV028 : Str -> V ;
mkV028 base =
  case base of {
    base_1+"e"+base_2@?+"a" => lin V
      { Converb = base_1+"i"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"e"+base_2 ;
                               Pl => base_1+"e"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"e"+base_2+"i" ;
                                 PSg P2 => base_1+"e"+base_2+"ur" ;
                                 PSg P3 => base_1+"e"+base_2+"ur" ;
                                 PPl => base_1+"e"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2 ;
                                 PSg P2 => base_1+"a"+base_2+"st" ;
                                 PSg P3 => base_1+"a"+base_2 ;
                                 PPl => base_1+"ó"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"e"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"e"+base_2+"andi" ;
                     Past => base_1+"i"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV028"
  } ;

mkV029 : Str -> V ;
mkV029 base =
  case base of {
    base_1+"a" => lin V
      { Converb = base_1+"að" ;
        Imperative_Jussive = table {
                               Sg => base_1+"a" ;
                               Pl => base_1+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"i" ;
                                 PSg P2 => base_1+"ir" ;
                                 PSg P3 => base_1+"ir" ;
                                 PPl => base_1+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"di" ;
                                 PSg P2 => base_1+"di" ;
                                 PSg P3 => base_1+"di" ;
                                 PPl => base_1+"du"
                               }
                     } ;
        Nonfinite = base_1+"a" ;
        Particle = table {
                     Pres => base_1+"andi" ;
                     Past => base_1+"aður"
                   }
      };
    _ => error "Can't apply paradigm mkV029"
  } ;

mkV030 : Str -> V ;
mkV030 base =
  case base of {
    "ve"+base_1+"a" => lin V
      { Converb = "ve"+base_1+"ið" ;
        Imperative_Jussive = table {
                               Sg => "ve"+base_1 ;
                               Pl => "ve"+base_1+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => "e"+base_1+"i" ;
                                 PSg P2 => "e"+base_1+"t" ;
                                 PSg P3 => "e"+base_1 ;
                                 PPl => "e"+base_1+"u"
                               } ;
                       Past => table {
                                 PSg P1 => "va"+base_1 ;
                                 PSg P2 => "va"+base_1+"t" ;
                                 PSg P3 => "va"+base_1 ;
                                 PPl => "vó"+base_1+"u"
                               }
                     } ;
        Nonfinite = "ve"+base_1+"a" ;
        Particle = table {
                     Pres => "ve"+base_1+"andi" ;
                     Past => nonExist
                   }
      };
    _ => error "Can't apply paradigm mkV030"
  } ;

mkV031 : Str -> V ;
mkV031 base =
  case base of {
    base_1+"na" => lin V
      { Converb = base_1+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"na" ;
                               Pl => base_1+"nið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ni" ;
                                 PSg P2 => base_1+"nir" ;
                                 PSg P3 => base_1+"nir" ;
                                 PPl => base_1+"na"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"di" ;
                                 PSg P2 => base_1+"di" ;
                                 PSg P3 => base_1+"di" ;
                                 PPl => base_1+"du"
                               }
                     } ;
        Nonfinite = base_1+"na" ;
        Particle = table {
                     Pres => base_1+"nandi" ;
                     Past => base_1+"dur"
                   }
      };
    _ => error "Can't apply paradigm mkV031"
  } ;

mkV032 : Str -> V ;
mkV032 base =
  case base of {
    base_1+"a"+base_2@(?+?)+"a" => lin V
      { Converb = base_1+"a"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"a"+base_2 ;
                               Pl => base_1+"a"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"a"+base_2+"i" ;
                                 PSg P2 => base_1+"e"+base_2+"ur" ;
                                 PSg P3 => base_1+"e"+base_2+"ur" ;
                                 PPl => base_1+"a"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2+"fell" ;
                                 PSg P2 => base_1+"a"+base_2 ;
                                 PSg P3 => base_1+"a"+base_2+"fell" ;
                                 PPl => base_1+"u"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"a"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"a"+base_2+"andi" ;
                     Past => base_1+"a"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV032"
  } ;

mkV033 : Str -> V ;
mkV033 base =
  case base of {
    base_1+"a"+base_2@?+"a" => lin V
      { Converb = base_1+"a"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"a"+base_2 ;
                               Pl => base_1+"a"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"a"+base_2+"i" ;
                                 PSg P2 => base_1+"e"+base_2+"t" ;
                                 PSg P3 => base_1+"e"+base_2 ;
                                 PPl => base_1+"a"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ó"+base_2 ;
                                 PSg P2 => base_1+"ó"+base_2+"t" ;
                                 PSg P3 => base_1+"ó"+base_2 ;
                                 PPl => base_1+"ó"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"a"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"a"+base_2+"andi" ;
                     Past => base_1+"a"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV033"
  } ;

mkV034 : Str -> V ;
mkV034 base =
  case base of {
    base_1+"áa" => lin V
      { Converb = base_1+"ingið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"á" ;
                               Pl => base_1+"áið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ái" ;
                                 PSg P2 => base_1+"ært" ;
                                 PSg P3 => base_1+"ær" ;
                                 PPl => base_1+"áa"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ekk" ;
                                 PSg P2 => base_1+"ekst" ;
                                 PSg P3 => base_1+"ekk" ;
                                 PPl => base_1+"ingu"
                               }
                     } ;
        Nonfinite = base_1+"áa" ;
        Particle = table {
                     Pres => nonExist ;
                     Past => base_1+"ingin"
                   }
      };
    _ => error "Can't apply paradigm mkV034"
  } ;

mkV035 : Str -> V ;
mkV035 base =
  case base of {
    base_1+"la" => lin V
      { Converb = base_1+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"la" ;
                               Pl => base_1+"lið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"li" ;
                                 PSg P2 => base_1+"lir" ;
                                 PSg P3 => base_1+"lir" ;
                                 PPl => base_1+"la"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"di" ;
                                 PSg P2 => base_1+"di" ;
                                 PSg P3 => base_1+"di" ;
                                 PPl => base_1+"du"
                               }
                     } ;
        Nonfinite = base_1+"la" ;
        Particle = table {
                     Pres => base_1+"landi" ;
                     Past => base_1+"dur"
                   }
      };
    _ => error "Can't apply paradigm mkV035"
  } ;

mkV036 : Str -> V ;
mkV036 base =
  case base of {
    base_1+"ða"+base_2@(?+?) => lin V
      { Converb = base_1+"da"+base_2 ;
        Imperative_Jussive = table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ði"+base_2 ;
                                 PSg P2 => base_1+"ða"+base_2 ;
                                 PSg P3 => base_1+"ða"+base_2 ;
                                 PPl => base_1+"ða"+base_2
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"daði"+base_2 ;
                                 PSg P2 => base_1+"daði"+base_2 ;
                                 PSg P3 => base_1+"daði"+base_2 ;
                                 PPl => base_1+"daðu"+base_2
                               }
                     } ;
        Nonfinite = base_1+"ða"+base_2 ;
        Particle = table {
                     Pres => nonExist ;
                     Past => nonExist
                   }
      };
    _ => error "Can't apply paradigm mkV036"
  } ;

mkV037 : Str -> V ;
mkV037 base =
  case base of {
    base_1+"a" => lin V
      { Converb = base_1 ;
        Imperative_Jussive = table {
                               Sg => base_1 ;
                               Pl => base_1+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"i" ;
                                 PSg P2 => base_1+"ir" ;
                                 PSg P3 => base_1+"ir" ;
                                 PPl => base_1+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"i" ;
                                 PSg P2 => base_1+"i" ;
                                 PSg P3 => base_1+"i" ;
                                 PPl => base_1+"u"
                               }
                     } ;
        Nonfinite = base_1+"a" ;
        Particle = table {
                     Pres => base_1+"andi" ;
                     Past => base_1+"ur"
                   }
      };
    _ => error "Can't apply paradigm mkV037"
  } ;

mkV038 : Str -> V ;
mkV038 base =
  case base of {
    base_1+"e"+base_2@?+"ja" => lin V
      { Converb = base_1+"a"+base_2+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"e"+base_2 ;
                               Pl => base_1+"e"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"e"+base_2+"i" ;
                                 PSg P2 => base_1+"e"+base_2+"ir" ;
                                 PSg P3 => base_1+"e"+base_2+"ir" ;
                                 PPl => base_1+"e"+base_2+"ja"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2+"ti" ;
                                 PSg P2 => base_1+"a"+base_2+"ti" ;
                                 PSg P3 => base_1+"a"+base_2+"ti" ;
                                 PPl => base_1+"a"+base_2+"tu"
                               }
                     } ;
        Nonfinite = base_1+"e"+base_2+"ja" ;
        Particle = table {
                     Pres => base_1+"e"+base_2+"jandi" ;
                     Past => base_1+"a"+base_2+"tur"
                   }
      };
    _ => error "Can't apply paradigm mkV038"
  } ;

mkV039 : Str -> V ;
mkV039 base =
  case base of {
    base_1+"na" => lin V
      { Converb = base_1+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"n" ;
                               Pl => base_1+"nið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ni" ;
                                 PSg P2 => base_1+"nir" ;
                                 PSg P3 => base_1+"nir" ;
                                 PPl => base_1+"na"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ti" ;
                                 PSg P2 => base_1+"ti" ;
                                 PSg P3 => base_1+"ti" ;
                                 PPl => base_1+"tu"
                               }
                     } ;
        Nonfinite = base_1+"na" ;
        Particle = table {
                     Pres => base_1+"nandi" ;
                     Past => base_1+"tur"
                   }
      };
    _ => error "Can't apply paradigm mkV039"
  } ;

mkV040 : Str -> V ;
mkV040 base =
  case base of {
    base_1+"ú"+base_2@?+"va" => lin V
      { Converb = base_1+"o"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"ú"+base_2 ;
                               Pl => base_1+"ú"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ú"+base_2+"vi" ;
                                 PSg P2 => base_1+"ý"+base_2+"ur" ;
                                 PSg P3 => base_1+"ý"+base_2+"ur" ;
                                 PPl => base_1+"ú"+base_2+"va"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ey"+base_2 ;
                                 PSg P2 => base_1+"ey"+base_2+"st" ;
                                 PSg P3 => base_1+"ey"+base_2 ;
                                 PPl => base_1+"u"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"ú"+base_2+"va" ;
        Particle = table {
                     Pres => base_1+"ú"+base_2+"vandi" ;
                     Past => base_1+"o"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV040"
  } ;

mkV041 : Str -> V ;
mkV041 base =
  case base of {
    base_1+"y"+base_2@?+"a" => lin V
      { Converb = base_1+"u"+base_2+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"y"+base_2 ;
                               Pl => base_1+"y"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"y"+base_2+"i" ;
                                 PSg P2 => base_1+"y"+base_2+"ur" ;
                                 PSg P3 => base_1+"y"+base_2+"ur" ;
                                 PPl => base_1+"y"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"u"+base_2+"ti" ;
                                 PSg P2 => base_1+"u"+base_2+"ti" ;
                                 PSg P3 => base_1+"u"+base_2+"ti" ;
                                 PPl => base_1+"u"+base_2+"tu"
                               }
                     } ;
        Nonfinite = base_1+"y"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"y"+base_2+"andi" ;
                     Past => base_1+"u"+base_2+"tur"
                   }
      };
    _ => error "Can't apply paradigm mkV041"
  } ;

mkV042 : Str -> V ;
mkV042 base =
  case base of {
    base_1+"e"+base_2@?+"ja" => lin V
      { Converb = base_1+"a"+base_2+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"e"+base_2 ;
                               Pl => base_1+"e"+base_2+"jið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"e"+base_2+"ji" ;
                                 PSg P2 => base_1+"e"+base_2+"ur" ;
                                 PSg P3 => base_1+"e"+base_2+"ur" ;
                                 PPl => base_1+"e"+base_2+"ja"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2+"di" ;
                                 PSg P2 => base_1+"a"+base_2+"di" ;
                                 PSg P3 => base_1+"a"+base_2+"di" ;
                                 PPl => base_1+"a"+base_2+"du"
                               }
                     } ;
        Nonfinite = base_1+"e"+base_2+"ja" ;
        Particle = table {
                     Pres => base_1+"e"+base_2+"jandi" ;
                     Past => base_1+"a"+base_2+"dur"
                   }
      };
    _ => error "Can't apply paradigm mkV042"
  } ;

mkV043 : Str -> V ;
mkV043 base =
  case base of {
    base_1+"a" => lin V
      { Converb = base_1+"tt" ;
        Imperative_Jussive = table {
                               Sg => base_1 ;
                               Pl => base_1+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"i" ;
                                 PSg P2 => base_1+"ir" ;
                                 PSg P3 => base_1+"ir" ;
                                 PPl => base_1+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ddi" ;
                                 PSg P2 => base_1+"ddi" ;
                                 PSg P3 => base_1+"ddi" ;
                                 PPl => base_1+"ddu"
                               }
                     } ;
        Nonfinite = base_1+"a" ;
        Particle = table {
                     Pres => base_1+"andi" ;
                     Past => base_1+"ddur"
                   }
      };
    _ => error "Can't apply paradigm mkV043"
  } ;

mkV044 : Str -> V ;
mkV044 base =
  case base of {
    base_1+"ú"+base_2@?+"a" => lin V
      { Converb = base_1+"o"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"ú"+base_2 ;
                               Pl => base_1+"ú"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ú"+base_2+"i" ;
                                 PSg P2 => base_1+"ý"+base_2+"ur" ;
                                 PSg P3 => base_1+"ý"+base_2+"ur" ;
                                 PPl => base_1+"ú"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ey"+base_2 ;
                                 PSg P2 => base_1+"ey"+base_2+"st" ;
                                 PSg P3 => base_1+"ey"+base_2 ;
                                 PPl => base_1+"u"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"ú"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"ú"+base_2+"andi" ;
                     Past => base_1+"o"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV044"
  } ;

mkV045 : Str -> V ;
mkV045 base =
  case base of {
    base_1+"la" => lin V
      { Converb = base_1+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"l" ;
                               Pl => base_1+"lið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"li" ;
                                 PSg P2 => base_1+"lir" ;
                                 PSg P3 => base_1+"lir" ;
                                 PPl => base_1+"la"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ti" ;
                                 PSg P2 => base_1+"ti" ;
                                 PSg P3 => base_1+"ti" ;
                                 PPl => base_1+"tu"
                               }
                     } ;
        Nonfinite = base_1+"la" ;
        Particle = table {
                     Pres => base_1+"landi" ;
                     Past => base_1+"tur"
                   }
      };
    _ => error "Can't apply paradigm mkV045"
  } ;

mkV046 : Str -> V ;
mkV046 base =
  case base of {
    base_1+"a"+base_2@?+"a" => lin V
      { Converb = base_1+"a"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"a"+base_2 ;
                               Pl => base_1+"a"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"a"+base_2+"i" ;
                                 PSg P2 => base_1+"e"+base_2+"ur" ;
                                 PSg P3 => base_1+"e"+base_2+"ur" ;
                                 PPl => base_1+"a"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ó"+base_2 ;
                                 PSg P2 => base_1+"ó"+base_2+"st" ;
                                 PSg P3 => base_1+"ó"+base_2 ;
                                 PPl => base_1+"ó"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"a"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"a"+base_2+"andi" ;
                     Past => base_1+"a"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV046"
  } ;

mkV047 : Str -> V ;
mkV047 base =
  case base of {
    base_1+"anga" => lin V
      { Converb = base_1+"ingið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"akk" ;
                               Pl => base_1+"angið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"angi" ;
                                 PSg P2 => base_1+"ongur" ;
                                 PSg P3 => base_1+"ongur" ;
                                 PPl => base_1+"anga"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ekk" ;
                                 PSg P2 => base_1+"ekst" ;
                                 PSg P3 => base_1+"ekk" ;
                                 PPl => base_1+"ingu"
                               }
                     } ;
        Nonfinite = base_1+"anga" ;
        Particle = table {
                     Pres => base_1+"angandi" ;
                     Past => base_1+"ingin"
                   }
      };
    _ => error "Can't apply paradigm mkV047"
  } ;

mkV048 : Str -> V ;
mkV048 base =
  case base of {
    base_1+"ja"+base_2@?+"da" => lin V
      { Converb = base_1+"o"+base_2+"dið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"ja"+base_2+"d" ;
                               Pl => base_1+"ja"+base_2+"dið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ja"+base_2+"di" ;
                                 PSg P2 => base_1+"e"+base_2+"dur" ;
                                 PSg P3 => base_1+"e"+base_2+"dur" ;
                                 PPl => base_1+"ja"+base_2+"da"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2+"t" ;
                                 PSg P2 => base_1+"a"+base_2+"t" ;
                                 PSg P3 => base_1+"a"+base_2+"t" ;
                                 PPl => base_1+"u"+base_2+"du"
                               }
                     } ;
        Nonfinite = base_1+"ja"+base_2+"da" ;
        Particle = table {
                     Pres => base_1+"ja"+base_2+"dandi" ;
                     Past => base_1+"o"+base_2+"din"
                   }
      };
    _ => error "Can't apply paradigm mkV048"
  } ;

mkV049 : Str -> V ;
mkV049 base =
  case base of {
    base_1+"e"+base_2@?+"a" => lin V
      { Converb = base_1+"jø"+base_2+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"e"+base_2 ;
                               Pl => base_1+"e"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"e"+base_2+"i" ;
                                 PSg P2 => base_1+"e"+base_2+"t" ;
                                 PSg P3 => base_1+"e"+base_2 ;
                                 PPl => base_1+"e"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"jø"+base_2+"di" ;
                                 PSg P2 => base_1+"jø"+base_2+"di" ;
                                 PSg P3 => base_1+"jø"+base_2+"di" ;
                                 PPl => base_1+"jø"+base_2+"du"
                               }
                     } ;
        Nonfinite = base_1+"e"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"e"+base_2+"andi" ;
                     Past => base_1+"jø"+base_2+"dur"
                   }
      };
    _ => error "Can't apply paradigm mkV049"
  } ;

mkV050 : Str -> V ;
mkV050 base =
  case base of {
    base_1+"jó"+base_2@?+"a" => lin V
      { Converb = base_1+"o"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"jó"+base_2 ;
                               Pl => base_1+"jó"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"jó"+base_2+"i" ;
                                 PSg P2 => base_1+"ý"+base_2+"ur" ;
                                 PSg P3 => base_1+"ý"+base_2+"ur" ;
                                 PPl => base_1+"jó"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ey"+base_2 ;
                                 PSg P2 => base_1+"ey"+base_2+"t" ;
                                 PSg P3 => base_1+"ey"+base_2 ;
                                 PPl => base_1+"u"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"jó"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"jó"+base_2+"andi" ;
                     Past => base_1+"o"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV050"
  } ;

mkV051 : Str -> V ;
mkV051 base =
  case base of {
    base_1+"e"+base_2@?+"pa" => lin V
      { Converb = base_1+"o"+base_2+"pið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"e"+base_2+"p" ;
                               Pl => base_1+"e"+base_2+"pið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"e"+base_2+"pi" ;
                                 PSg P2 => base_1+"e"+base_2+"pur" ;
                                 PSg P3 => base_1+"e"+base_2+"pur" ;
                                 PPl => base_1+"e"+base_2+"pa"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2+"p" ;
                                 PSg P2 => base_1+"a"+base_2+"st" ;
                                 PSg P3 => base_1+"a"+base_2+"p" ;
                                 PPl => base_1+"u"+base_2+"pu"
                               }
                     } ;
        Nonfinite = base_1+"e"+base_2+"pa" ;
        Particle = table {
                     Pres => base_1+"e"+base_2+"pandi" ;
                     Past => base_1+"o"+base_2+"pin"
                   }
      };
    _ => error "Can't apply paradigm mkV051"
  } ;

mkV052 : Str -> V ;
mkV052 base =
  case base of {
    base_1+"y"+base_2@?+"ja" => lin V
      { Converb = base_1+"u"+base_2+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"y"+base_2 ;
                               Pl => base_1+"y"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"y"+base_2+"i" ;
                                 PSg P2 => base_1+"y"+base_2+"ur" ;
                                 PSg P3 => base_1+"y"+base_2+"ur" ;
                                 PPl => base_1+"y"+base_2+"ja"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"u"+base_2+"di" ;
                                 PSg P2 => base_1+"u"+base_2+"di" ;
                                 PSg P3 => base_1+"u"+base_2+"di" ;
                                 PPl => base_1+"u"+base_2+"du"
                               }
                     } ;
        Nonfinite = base_1+"y"+base_2+"ja" ;
        Particle = table {
                     Pres => base_1+"y"+base_2+"jandi" ;
                     Past => base_1+"u"+base_2+"dur"
                   }
      };
    _ => error "Can't apply paradigm mkV052"
  } ;

mkV053 : Str -> V ;
mkV053 base =
  case base of {
    base_1+"á"+base_2@?+"a" => lin V
      { Converb = base_1+"á"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"á"+base_2 ;
                               Pl => base_1+"á"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"á"+base_2+"i" ;
                                 PSg P2 => base_1+"æ"+base_2+"ur" ;
                                 PSg P3 => base_1+"æ"+base_2+"ur" ;
                                 PPl => base_1+"á"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"æ"+base_2 ;
                                 PSg P2 => base_1+"æ"+base_2+"st" ;
                                 PSg P3 => base_1+"æ"+base_2 ;
                                 PPl => base_1+"ó"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"á"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"á"+base_2+"andi" ;
                     Past => base_1+"á"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV053"
  } ;

mkV054 : Str -> V ;
mkV054 base =
  case base of {
    base_1+"a"+base_2@?+"a" => lin V
      { Converb = base_1+"i"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"a"+base_2 ;
                               Pl => base_1+"a"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"a"+base_2+"i" ;
                                 PSg P2 => base_1+"e"+base_2+"ur" ;
                                 PSg P3 => base_1+"e"+base_2+"ur" ;
                                 PPl => base_1+"a"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ó"+base_2 ;
                                 PSg P2 => base_1+"ó"+base_2+"st" ;
                                 PSg P3 => base_1+"ó"+base_2 ;
                                 PPl => base_1+"ó"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"a"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"a"+base_2+"andi" ;
                     Past => base_1+"i"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV054"
  } ;

mkV055 : Str -> V ;
mkV055 base =
  case base of {
    base_1+"ógva" => lin V
      { Converb = base_1+"óð" ;
        Imperative_Jussive = table {
                               Sg => base_1+"ógv" ;
                               Pl => base_1+"ógvið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ógvi" ;
                                 PSg P2 => base_1+"ørt" ;
                                 PSg P3 => base_1+"ør" ;
                                 PPl => base_1+"ógva"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"óði" ;
                                 PSg P2 => base_1+"óði" ;
                                 PSg P3 => base_1+"óði" ;
                                 PPl => base_1+"óðu"
                               }
                     } ;
        Nonfinite = base_1+"ógva" ;
        Particle = table {
                     Pres => base_1+"ógvandi" ;
                     Past => base_1+"ógvin"
                   }
      };
    _ => error "Can't apply paradigm mkV055"
  } ;

mkV056 : Str -> V ;
mkV056 base =
  case base of {
    base_1+"a"+base_2@?+"da" => lin V
      { Converb = base_1+"i"+base_2+"dið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"a"+base_2+"d" ;
                               Pl => base_1+"a"+base_2+"dið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"a"+base_2+"di" ;
                                 PSg P2 => base_1+"e"+base_2+"dur" ;
                                 PSg P3 => base_1+"e"+base_2+"dur" ;
                                 PPl => base_1+"a"+base_2+"da"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"e"+base_2+"t" ;
                                 PSg P2 => base_1+"e"+base_2+"tst" ;
                                 PSg P3 => base_1+"e"+base_2+"t" ;
                                 PPl => base_1+"i"+base_2+"du"
                               }
                     } ;
        Nonfinite = base_1+"a"+base_2+"da" ;
        Particle = table {
                     Pres => base_1+"a"+base_2+"dandi" ;
                     Past => base_1+"i"+base_2+"din"
                   }
      };
    _ => error "Can't apply paradigm mkV056"
  } ;

mkV057 : Str -> V ;
mkV057 base =
  case base of {
    base_1+"anga" => lin V
      { Converb = base_1+"ingið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"ang" ;
                               Pl => base_1+"angið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"angi" ;
                                 PSg P2 => base_1+"ongur" ;
                                 PSg P3 => base_1+"ongur" ;
                                 PPl => base_1+"anga"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ekk" ;
                                 PSg P2 => base_1+"ekst" ;
                                 PSg P3 => base_1+"ekk" ;
                                 PPl => base_1+"ingu"
                               }
                     } ;
        Nonfinite = base_1+"anga" ;
        Particle = table {
                     Pres => base_1+"angandi" ;
                     Past => base_1+"ingin"
                   }
      };
    _ => error "Can't apply paradigm mkV057"
  } ;

mkV058 : Str -> V ;
mkV058 base =
  case base of {
    base_1+"a"+base_2@?+"a" => lin V
      { Converb = base_1+"a"+base_2+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"a"+base_2 ;
                               Pl => base_1+"a"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"a"+base_2+"i" ;
                                 PSg P2 => base_1+"e"+base_2+"ur" ;
                                 PSg P3 => base_1+"e"+base_2+"ur" ;
                                 PPl => base_1+"a"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"e"+base_2+"ði" ;
                                 PSg P2 => base_1+"e"+base_2+"ði" ;
                                 PSg P3 => base_1+"e"+base_2+"ði" ;
                                 PPl => base_1+"ø"+base_2+"du"
                               }
                     } ;
        Nonfinite = base_1+"a"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"a"+base_2+"andi" ;
                     Past => base_1+"a"+base_2+"dur"
                   }
      };
    _ => error "Can't apply paradigm mkV058"
  } ;

mkV059 : Str -> V ;
mkV059 base =
  case base of {
    base_1+"ei"+base_2@(?+?)+"ja" => lin V
      { Converb = base_1+"o"+base_2+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"ei"+base_2+"j" ;
                               Pl => base_1+"ei"+base_2+"jið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ei"+base_2+"i" ;
                                 PSg P2 => base_1+"ei"+base_2+"ir" ;
                                 PSg P3 => base_1+"ei"+base_2+"ir" ;
                                 PPl => base_1+"ei"+base_2+"ja"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"o"+base_2+"di" ;
                                 PSg P2 => base_1+"o"+base_2+"di" ;
                                 PSg P3 => base_1+"o"+base_2+"di" ;
                                 PPl => base_1+"o"+base_2+"du"
                               }
                     } ;
        Nonfinite = base_1+"ei"+base_2+"ja" ;
        Particle = table {
                     Pres => base_1+"ei"+base_2+"jandi" ;
                     Past => base_1+"o"+base_2+"dur"
                   }
      };
    _ => error "Can't apply paradigm mkV059"
  } ;

mkV060 : Str -> V ;
mkV060 base =
  case base of {
    base_1+"vø"+base_2@(?+?)+"a" => lin V
      { Converb = base_1+"o"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"vø"+base_2 ;
                               Pl => base_1+"vø"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"vø"+base_2+"i" ;
                                 PSg P2 => base_1+"vø"+base_2+"ur" ;
                                 PSg P3 => base_1+"vø"+base_2+"ur" ;
                                 PPl => base_1+"vø"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"va"+base_2 ;
                                 PSg P2 => base_1+"va"+base_2+"st" ;
                                 PSg P3 => base_1+"va"+base_2 ;
                                 PPl => base_1+"vu"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"vø"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"vø"+base_2+"andi" ;
                     Past => base_1+"o"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV060"
  } ;

mkV061 : Str -> V ;
mkV061 base =
  case base of {
    base_1+"y"+base_2@?+"gja" => lin V
      { Converb = base_1+"u"+base_2+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"y"+base_2+"g" ;
                               Pl => base_1+"y"+base_2+"gið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"y"+base_2+"gi" ;
                                 PSg P2 => base_1+"y"+base_2+"gur" ;
                                 PSg P3 => base_1+"y"+base_2+"gur" ;
                                 PPl => base_1+"y"+base_2+"gja"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"u"+base_2+"di" ;
                                 PSg P2 => base_1+"u"+base_2+"di" ;
                                 PSg P3 => base_1+"u"+base_2+"di" ;
                                 PPl => base_1+"u"+base_2+"du"
                               }
                     } ;
        Nonfinite = base_1+"y"+base_2+"gja" ;
        Particle = table {
                     Pres => base_1+"y"+base_2+"gjandi" ;
                     Past => base_1+"u"+base_2+"dur"
                   }
      };
    _ => error "Can't apply paradigm mkV061"
  } ;

mkV062 : Str -> V ;
mkV062 base =
  case base of {
    base_1+"o"+base_2@?+"a" => lin V
      { Converb = base_1+"o"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"o"+base_2 ;
                               Pl => base_1+"o"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"o"+base_2+"i" ;
                                 PSg P2 => base_1+"e"+base_2+"ur" ;
                                 PSg P3 => base_1+"e"+base_2+"ur" ;
                                 PPl => base_1+"o"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"o"+base_2 ;
                                 PSg P2 => base_1+"o"+base_2+"st" ;
                                 PSg P3 => base_1+"o"+base_2 ;
                                 PPl => base_1+"o"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"o"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"o"+base_2+"andi" ;
                     Past => base_1+"o"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV062"
  } ;

mkV063 : Str -> V ;
mkV063 base =
  case base of {
    base_1+"pa" => lin V
      { Converb = base_1+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"p" ;
                               Pl => base_1+"pið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"pi" ;
                                 PSg P2 => base_1+"pir" ;
                                 PSg P3 => base_1+"pir" ;
                                 PPl => base_1+"pa"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ti" ;
                                 PSg P2 => base_1+"ti" ;
                                 PSg P3 => base_1+"ti" ;
                                 PPl => base_1+"tu"
                               }
                     } ;
        Nonfinite = base_1+"pa" ;
        Particle = table {
                     Pres => base_1+"pandi" ;
                     Past => base_1+"tur"
                   }
      };
    _ => error "Can't apply paradigm mkV063"
  } ;

mkV064 : Str -> V ;
mkV064 base =
  case base of {
    base_1+"úgva" => lin V
      { Converb = base_1+"ovið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"úg" ;
                               Pl => base_1+"úgið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"úgvi" ;
                                 PSg P2 => base_1+"ývur" ;
                                 PSg P3 => base_1+"ývur" ;
                                 PPl => base_1+"úgva"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"eyv" ;
                                 PSg P2 => base_1+"eyvst" ;
                                 PSg P3 => base_1+"eyv" ;
                                 PPl => base_1+"uvu"
                               }
                     } ;
        Nonfinite = base_1+"úgva" ;
        Particle = table {
                     Pres => base_1+"úgvandi" ;
                     Past => base_1+"ovin"
                   }
      };
    _ => error "Can't apply paradigm mkV064"
  } ;

mkV065 : Str -> V ;
mkV065 base =
  case base of {
    base_1+"u"+base_2@?+"na" => lin V
      { Converb = base_1+"u"+base_2+"nað" ;
        Imperative_Jussive = table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"a"+base_2+"n" ;
                                 PSg P2 => base_1+"a"+base_2+"st" ;
                                 PSg P3 => base_1+"a"+base_2+"n" ;
                                 PPl => base_1+"u"+base_2+"nu"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"u"+base_2+"di" ;
                                 PSg P2 => base_1+"u"+base_2+"di" ;
                                 PSg P3 => base_1+"u"+base_2+"di" ;
                                 PPl => base_1+"u"+base_2+"du"
                               }
                     } ;
        Nonfinite = base_1+"u"+base_2+"na" ;
        Particle = table {
                     Pres => nonExist ;
                     Past => nonExist
                   }
      };
    _ => error "Can't apply paradigm mkV065"
  } ;

mkV066 : Str -> V ;
mkV066 base =
  case base of {
    base_1+"ø"+base_2@?+"a" => lin V
      { Converb = base_1+"ø"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"ø"+base_2 ;
                               Pl => base_1+"ø"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ø"+base_2+"i" ;
                                 PSg P2 => base_1+"ø"+base_2+"ur" ;
                                 PSg P3 => base_1+"ø"+base_2+"ur" ;
                                 PPl => base_1+"ø"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2 ;
                                 PSg P2 => base_1+"a"+base_2+"st" ;
                                 PSg P3 => base_1+"a"+base_2 ;
                                 PPl => base_1+"ó"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"ø"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"ø"+base_2+"andi" ;
                     Past => base_1+"ø"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV066"
  } ;

mkV067 : Str -> V ;
mkV067 base =
  case base of {
    base_1+"sa" => lin V
      { Converb = base_1+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"s" ;
                               Pl => base_1+"sið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"si" ;
                                 PSg P2 => base_1+"sir" ;
                                 PSg P3 => base_1+"sir" ;
                                 PPl => base_1+"sa"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ti" ;
                                 PSg P2 => base_1+"ti" ;
                                 PSg P3 => base_1+"ti" ;
                                 PPl => base_1+"tu"
                               }
                     } ;
        Nonfinite = base_1+"sa" ;
        Particle = table {
                     Pres => base_1+"sandi" ;
                     Past => base_1+"tur"
                   }
      };
    _ => error "Can't apply paradigm mkV067"
  } ;

mkV068 : Str -> V ;
mkV068 base =
  case base of {
    base_1+"a" => lin V
      { Converb = base_1+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1 ;
                               Pl => base_1+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"i" ;
                                 PSg P2 => base_1+"r" ;
                                 PSg P3 => base_1+"r" ;
                                 PPl => base_1+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ði" ;
                                 PSg P2 => base_1+"ði" ;
                                 PSg P3 => base_1+"ði" ;
                                 PPl => base_1+"ðu"
                               }
                     } ;
        Nonfinite = base_1+"a" ;
        Particle = table {
                     Pres => base_1+"andi" ;
                     Past => base_1+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV068"
  } ;

mkV069 : Str -> V ;
mkV069 base =
  case base of {
    base_1+"e"+base_2@?+"gja" => lin V
      { Converb = base_1+"a"+base_2+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"e"+base_2+"gj" ;
                               Pl => base_1+"e"+base_2+"gjið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"e"+base_2+"gi" ;
                                 PSg P2 => base_1+"e"+base_2+"gur" ;
                                 PSg P3 => base_1+"e"+base_2+"gur" ;
                                 PPl => base_1+"e"+base_2+"gja"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"e"+base_2+"ði" ;
                                 PSg P2 => base_1+"e"+base_2+"ði" ;
                                 PSg P3 => base_1+"e"+base_2+"ði" ;
                                 PPl => base_1+"ø"+base_2+"du"
                               }
                     } ;
        Nonfinite = base_1+"e"+base_2+"gja" ;
        Particle = table {
                     Pres => base_1+"e"+base_2+"gjandi" ;
                     Past => base_1+"a"+base_2+"dur"
                   }
      };
    _ => error "Can't apply paradigm mkV069"
  } ;

mkV070 : Str -> V ;
mkV070 base =
  case base of {
    base_1+"iggja" => lin V
      { Converb = base_1+"igið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"igg" ;
                               Pl => base_1+"iggið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"iggi" ;
                                 PSg P2 => base_1+"iggur" ;
                                 PSg P3 => base_1+"iggur" ;
                                 PPl => base_1+"iggja"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"á" ;
                                 PSg P2 => base_1+"ást" ;
                                 PSg P3 => base_1+"á" ;
                                 PPl => base_1+"ógu"
                               }
                     } ;
        Nonfinite = base_1+"iggja" ;
        Particle = table {
                     Pres => base_1+"iggjandi" ;
                     Past => base_1+"igin"
                   }
      };
    _ => error "Can't apply paradigm mkV070"
  } ;

mkV071 : Str -> V ;
mkV071 base =
  case base of {
    base_1+"e"+base_2@?+"a" => lin V
      { Converb = base_1+"i"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"e"+base_2 ;
                               Pl => base_1+"e"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"e"+base_2+"i" ;
                                 PSg P2 => base_1+"e"+base_2+"ur" ;
                                 PSg P3 => base_1+"e"+base_2+"ur" ;
                                 PPl => base_1+"e"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2 ;
                                 PSg P2 => base_1+"a"+base_2+"t" ;
                                 PSg P3 => base_1+"a"+base_2 ;
                                 PPl => base_1+"ó"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"e"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"e"+base_2+"andi" ;
                     Past => base_1+"i"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV071"
  } ;

mkV072 : Str -> V ;
mkV072 base =
  case base of {
    base_1+"a"+base_2@?+"a" => lin V
      { Converb = base_1+"a"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"a"+base_2 ;
                               Pl => base_1+"a"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"a"+base_2+"i" ;
                                 PSg P2 => base_1+"e"+base_2+"ur" ;
                                 PSg P3 => base_1+"e"+base_2+"ur" ;
                                 PPl => base_1+"a"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"æ"+base_2 ;
                                 PSg P2 => base_1+"æ"+base_2+"st" ;
                                 PSg P3 => base_1+"æ"+base_2 ;
                                 PPl => base_1+"ó"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"a"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"a"+base_2+"andi" ;
                     Past => base_1+"a"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV072"
  } ;

mkV073 : Str -> V ;
mkV073 base =
  case base of {
    base_1+"á"+base_2@?+"a" => lin V
      { Converb = base_1+"á"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"á"+base_2 ;
                               Pl => base_1+"á"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"á"+base_2+"i" ;
                                 PSg P2 => base_1+"e"+base_2+"ur" ;
                                 PSg P3 => base_1+"e"+base_2+"ur" ;
                                 PPl => base_1+"á"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"æ"+base_2 ;
                                 PSg P2 => base_1+"æ"+base_2+"st" ;
                                 PSg P3 => base_1+"æ"+base_2 ;
                                 PPl => base_1+"ó"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"á"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"á"+base_2+"andi" ;
                     Past => base_1+"á"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV073"
  } ;

mkV074 : Str -> V ;
mkV074 base =
  case base of {
    base_1+"ey"+base_2@?+"a" => lin V
      { Converb = base_1+"o"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"ey"+base_2 ;
                               Pl => base_1+"ey"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ey"+base_2+"i" ;
                                 PSg P2 => base_1+"oy"+base_2+"ur" ;
                                 PSg P3 => base_1+"oy"+base_2+"ur" ;
                                 PPl => base_1+"ey"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ey"+base_2 ;
                                 PSg P2 => base_1+"ey"+base_2+"st" ;
                                 PSg P3 => base_1+"ey"+base_2 ;
                                 PPl => base_1+"u"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"ey"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"ey"+base_2+"andi" ;
                     Past => base_1+"o"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV074"
  } ;

mkV075 : Str -> V ;
mkV075 base =
  case base of {
    base_1+"ega" => lin V
      { Converb = nonExist ;
        Imperative_Jussive = table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"á" ;
                                 PSg P2 => base_1+"ást" ;
                                 PSg P3 => base_1+"á" ;
                                 PPl => base_1+"ugu"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"átti" ;
                                 PSg P2 => base_1+"átti" ;
                                 PSg P3 => base_1+"átti" ;
                                 PPl => base_1+"áttu"
                               }
                     } ;
        Nonfinite = base_1+"ega" ;
        Particle = table {
                     Pres => nonExist ;
                     Past => nonExist
                   }
      };
    _ => error "Can't apply paradigm mkV075"
  } ;

mkV076 : Str -> V ;
mkV076 base =
  case base of {
    base_1+"u"+base_2@?+"na" => lin V
      { Converb = base_1+"u"+base_2+"nað" ;
        Imperative_Jussive = table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"a"+base_2 ;
                                 PSg P2 => base_1+"a"+base_2+"st" ;
                                 PSg P3 => base_1+"a"+base_2 ;
                                 PPl => base_1+"u"+base_2+"nu"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"u"+base_2+"di" ;
                                 PSg P2 => base_1+"u"+base_2+"di" ;
                                 PSg P3 => base_1+"u"+base_2+"di" ;
                                 PPl => base_1+"u"+base_2+"du"
                               }
                     } ;
        Nonfinite = base_1+"u"+base_2+"na" ;
        Particle = table {
                     Pres => nonExist ;
                     Past => nonExist
                   }
      };
    _ => error "Can't apply paradigm mkV076"
  } ;

mkV077 : Str -> V ;
mkV077 base =
  case base of {
    base_1+"ja" => lin V
      { Converb = base_1+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1 ;
                               Pl => base_1+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"i" ;
                                 PSg P2 => base_1+"ir" ;
                                 PSg P3 => base_1+"ir" ;
                                 PPl => base_1+"ja"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ti" ;
                                 PSg P2 => base_1+"ti" ;
                                 PSg P3 => base_1+"ti" ;
                                 PPl => base_1+"tu"
                               }
                     } ;
        Nonfinite = base_1+"ja" ;
        Particle = table {
                     Pres => base_1+"jandi" ;
                     Past => base_1+"tur"
                   }
      };
    _ => error "Can't apply paradigm mkV077"
  } ;

mkV078 : Str -> V ;
mkV078 base =
  case base of {
    base_1+"ja" => lin V
      { Converb = base_1+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1 ;
                               Pl => base_1+"jið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ji" ;
                                 PSg P2 => base_1+"ur" ;
                                 PSg P3 => base_1+"ur" ;
                                 PPl => base_1+"ja"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"di" ;
                                 PSg P2 => base_1+"di" ;
                                 PSg P3 => base_1+"di" ;
                                 PPl => base_1+"du"
                               }
                     } ;
        Nonfinite = base_1+"ja" ;
        Particle = table {
                     Pres => base_1+"jandi" ;
                     Past => base_1+"dur"
                   }
      };
    _ => error "Can't apply paradigm mkV078"
  } ;

mkV079 : Str -> V ;
mkV079 base =
  case base of {
    base_1+"ða" => lin V
      { Converb = base_1+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"ð" ;
                               Pl => base_1+"ðið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ði" ;
                                 PSg P2 => base_1+"ðir" ;
                                 PSg P3 => base_1+"ðir" ;
                                 PPl => base_1+"ða"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"di" ;
                                 PSg P2 => base_1+"di" ;
                                 PSg P3 => base_1+"di" ;
                                 PPl => base_1+"du"
                               }
                     } ;
        Nonfinite = base_1+"ða" ;
        Particle = table {
                     Pres => base_1+"ðandi" ;
                     Past => base_1+"dur"
                   }
      };
    _ => error "Can't apply paradigm mkV079"
  } ;

mkV080 : Str -> V ;
mkV080 base =
  case base of {
    base_1+"áa" => lin V
      { Converb = base_1+"átt" ;
        Imperative_Jussive = table {
                               Sg => base_1+"á" ;
                               Pl => base_1+"áið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ái" ;
                                 PSg P2 => base_1+"ær" ;
                                 PSg P3 => base_1+"ær" ;
                                 PPl => base_1+"áa"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"áddi" ;
                                 PSg P2 => base_1+"áddi" ;
                                 PSg P3 => base_1+"áddi" ;
                                 PPl => base_1+"áddu"
                               }
                     } ;
        Nonfinite = base_1+"áa" ;
        Particle = table {
                     Pres => base_1+"áandi" ;
                     Past => base_1+"áddur"
                   }
      };
    _ => error "Can't apply paradigm mkV080"
  } ;

mkV081 : Str -> V ;
mkV081 base =
  case base of {
    base_1+"áða" => lin V
      { Converb = base_1+"átt" ;
        Imperative_Jussive = table {
                               Sg => base_1+"áð" ;
                               Pl => base_1+"áðið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"áði" ;
                                 PSg P2 => base_1+"æður" ;
                                 PSg P3 => base_1+"æður" ;
                                 PPl => base_1+"áða"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"addi" ;
                                 PSg P2 => base_1+"addi" ;
                                 PSg P3 => base_1+"addi" ;
                                 PPl => base_1+"addu"
                               }
                     } ;
        Nonfinite = base_1+"áða" ;
        Particle = table {
                     Pres => base_1+"áðandi" ;
                     Past => base_1+"áðin"
                   }
      };
    _ => error "Can't apply paradigm mkV081"
  } ;

mkV082 : Str -> V ;
mkV082 base =
  case base of {
    base_1+"ða"+base_2@(?+?) => lin V
      { Converb = base_1+"ð"+base_2 ;
        Imperative_Jussive = table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ði"+base_2 ;
                                 PSg P2 => base_1+"ða"+base_2 ;
                                 PSg P3 => base_1+"ða"+base_2 ;
                                 PPl => base_1+"ða"+base_2
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ddi"+base_2 ;
                                 PSg P2 => base_1+"ddi"+base_2 ;
                                 PSg P3 => base_1+"ddi"+base_2 ;
                                 PPl => base_1+"ddu"+base_2
                               }
                     } ;
        Nonfinite = base_1+"ða"+base_2 ;
        Particle = table {
                     Pres => nonExist ;
                     Past => nonExist
                   }
      };
    _ => error "Can't apply paradigm mkV082"
  } ;

mkV083 : Str -> V ;
mkV083 base =
  case base of {
    base_1+"ø"+base_2@?+"ka" => lin V
      { Converb = base_1+"o"+base_2+"kið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"ø"+base_2+"k" ;
                               Pl => base_1+"ø"+base_2+"kið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ø"+base_2+"ki" ;
                                 PSg P2 => base_1+"ø"+base_2+"kur" ;
                                 PSg P3 => base_1+"ø"+base_2+"kur" ;
                                 PPl => base_1+"ø"+base_2+"ka"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2+"k" ;
                                 PSg P2 => base_1+"a"+base_2+"st" ;
                                 PSg P3 => base_1+"a"+base_2+"k" ;
                                 PPl => base_1+"u"+base_2+"ku"
                               }
                     } ;
        Nonfinite = base_1+"ø"+base_2+"ka" ;
        Particle = table {
                     Pres => base_1+"ø"+base_2+"kandi" ;
                     Past => base_1+"o"+base_2+"kin"
                   }
      };
    _ => error "Can't apply paradigm mkV083"
  } ;

mkV084 : Str -> V ;
mkV084 base =
  case base of {
    base_1+"íggja" => lin V
      { Converb = base_1+"æð" ;
        Imperative_Jussive = table {
                               Sg => base_1+"íggj" ;
                               Pl => base_1+"íggið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"íggi" ;
                                 PSg P2 => base_1+"ært" ;
                                 PSg P3 => base_1+"ær" ;
                                 PPl => base_1+"íggja"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"á" ;
                                 PSg P2 => base_1+"ást" ;
                                 PSg P3 => base_1+"á" ;
                                 PPl => base_1+"óu"
                               }
                     } ;
        Nonfinite = base_1+"íggja" ;
        Particle = table {
                     Pres => base_1+"íggjandi" ;
                     Past => base_1+"æddur"
                   }
      };
    _ => error "Can't apply paradigm mkV084"
  } ;

mkV085 : Str -> V ;
mkV085 base =
  case base of {
    base_1+"kja" => lin V
      { Converb = base_1+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"k" ;
                               Pl => base_1+"kið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ki" ;
                                 PSg P2 => base_1+"kir" ;
                                 PSg P3 => base_1+"kir" ;
                                 PPl => base_1+"kja"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ti" ;
                                 PSg P2 => base_1+"ti" ;
                                 PSg P3 => base_1+"ti" ;
                                 PPl => base_1+"tu"
                               }
                     } ;
        Nonfinite = base_1+"kja" ;
        Particle = table {
                     Pres => base_1+"kjandi" ;
                     Past => base_1+"tur"
                   }
      };
    _ => error "Can't apply paradigm mkV085"
  } ;

mkV086 : Str -> V ;
mkV086 base =
  case base of {
    base_1+"i"+base_2@?+"a" => lin V
      { Converb = base_1+"a"+base_2+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"i"+base_2 ;
                               Pl => base_1+"i"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"i"+base_2+"i" ;
                                 PSg P2 => base_1+"i"+base_2+"ur" ;
                                 PSg P3 => base_1+"i"+base_2+"ur" ;
                                 PPl => base_1+"i"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"e"+base_2+"ði" ;
                                 PSg P2 => base_1+"e"+base_2+"ði" ;
                                 PSg P3 => base_1+"e"+base_2+"ði" ;
                                 PPl => base_1+"ø"+base_2+"du"
                               }
                     } ;
        Nonfinite = base_1+"i"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"i"+base_2+"andi" ;
                     Past => base_1+"a"+base_2+"dur"
                   }
      };
    _ => error "Can't apply paradigm mkV086"
  } ;

mkV087 : Str -> V ;
mkV087 base =
  case base of {
    base_1+"i"+base_2@?+"a" => lin V
      { Converb = base_1+"i"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"i"+base_2 ;
                               Pl => base_1+"i"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"i"+base_2+"i" ;
                                 PSg P2 => base_1+"i"+base_2+"ur" ;
                                 PSg P3 => base_1+"i"+base_2+"ur" ;
                                 PPl => base_1+"i"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2 ;
                                 PSg P2 => "sat"+base_1+base_2 ;
                                 PSg P3 => base_1+"a"+base_2 ;
                                 PPl => base_1+"ó"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"i"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"i"+base_2+"andi" ;
                     Past => base_1+"i"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV087"
  } ;

mkV088 : Str -> V ;
mkV088 base =
  case base of {
    base_1+"u"+base_2@?+"a" => lin V
      { Converb = base_1+"u"+base_2+"að" ;
        Imperative_Jussive = table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"a"+base_2 ;
                                 PSg P2 => base_1+"a"+base_2+"t" ;
                                 PSg P3 => base_1+"a"+base_2 ;
                                 PPl => base_1+"u"+base_2+"u"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"u"+base_2+"di" ;
                                 PSg P2 => base_1+"u"+base_2+"di" ;
                                 PSg P3 => base_1+"u"+base_2+"di" ;
                                 PPl => base_1+"u"+base_2+"du"
                               }
                     } ;
        Nonfinite = base_1+"u"+base_2+"a" ;
        Particle = table {
                     Pres => nonExist ;
                     Past => nonExist
                   }
      };
    _ => error "Can't apply paradigm mkV088"
  } ;

mkV089 : Str -> V ;
mkV089 base =
  case base of {
    base_1+"ei"+base_2@(?+?)+"ja" => lin V
      { Converb = base_1+"o"+base_2+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"ei"+base_2+"j" ;
                               Pl => base_1+"ei"+base_2+"jið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ei"+base_2+"i" ;
                                 PSg P2 => base_1+"ei"+base_2+"ir" ;
                                 PSg P3 => base_1+"ei"+base_2+"ir" ;
                                 PPl => base_1+"ei"+base_2+"ja"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"o"+base_2+"ti" ;
                                 PSg P2 => base_1+"o"+base_2+"ti" ;
                                 PSg P3 => base_1+"o"+base_2+"ti" ;
                                 PPl => base_1+"o"+base_2+"tu"
                               }
                     } ;
        Nonfinite = base_1+"ei"+base_2+"ja" ;
        Particle = table {
                     Pres => base_1+"ei"+base_2+"jandi" ;
                     Past => base_1+"o"+base_2+"tur"
                   }
      };
    _ => error "Can't apply paradigm mkV089"
  } ;

mkV090 : Str -> V ;
mkV090 base =
  case base of {
    base_1+"ja" => lin V
      { Converb = base_1+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"j" ;
                               Pl => base_1+"jið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ji" ;
                                 PSg P2 => base_1+"jir" ;
                                 PSg P3 => base_1+"jir" ;
                                 PPl => base_1+"ja"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"di" ;
                                 PSg P2 => base_1+"di" ;
                                 PSg P3 => base_1+"di" ;
                                 PPl => base_1+"du"
                               }
                     } ;
        Nonfinite = base_1+"ja" ;
        Particle = table {
                     Pres => base_1+"jandi" ;
                     Past => base_1+"dur"
                   }
      };
    _ => error "Can't apply paradigm mkV090"
  } ;

mkV091 : Str -> V ;
mkV091 base =
  case base of {
    base_1+"jó"+base_2@?+"a" => lin V
      { Converb = base_1+"o"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"jó"+base_2 ;
                               Pl => base_1+"jó"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"jó"+base_2+"i" ;
                                 PSg P2 => base_1+"jý"+base_2+"ur" ;
                                 PSg P3 => base_1+"jý"+base_2+"ur" ;
                                 PPl => base_1+"jó"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ey"+base_2 ;
                                 PSg P2 => base_1+"ey"+base_2+"st" ;
                                 PSg P3 => base_1+"ey"+base_2 ;
                                 PPl => base_1+"u"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"jó"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"jó"+base_2+"andi" ;
                     Past => base_1+"o"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV091"
  } ;

mkV092 : Str -> V ;
mkV092 base =
  case base of {
    base_1+"áa" => lin V
      { Converb = base_1+"igið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"á" ;
                               Pl => base_1+"áið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ái" ;
                                 PSg P2 => base_1+"ært" ;
                                 PSg P3 => base_1+"ær" ;
                                 PPl => base_1+"áa"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ó" ;
                                 PSg P2 => base_1+"óst" ;
                                 PSg P3 => base_1+"ó" ;
                                 PPl => base_1+"ógu"
                               }
                     } ;
        Nonfinite = base_1+"áa" ;
        Particle = table {
                     Pres => base_1+"áandi" ;
                     Past => base_1+"igin"
                   }
      };
    _ => error "Can't apply paradigm mkV092"
  } ;

mkV093 : Str -> V ;
mkV093 base =
  case base of {
    base_1+"y"+base_2@?+"ja" => lin V
      { Converb = base_1+"u"+base_2+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"y"+base_2 ;
                               Pl => base_1+"y"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"y"+base_2+"ji" ;
                                 PSg P2 => base_1+"y"+base_2+"t" ;
                                 PSg P3 => base_1+"y"+base_2 ;
                                 PPl => base_1+"y"+base_2+"ja"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"u"+base_2+"di" ;
                                 PSg P2 => base_1+"u"+base_2+"di" ;
                                 PSg P3 => base_1+"u"+base_2+"di" ;
                                 PPl => base_1+"u"+base_2+"du"
                               }
                     } ;
        Nonfinite = base_1+"y"+base_2+"ja" ;
        Particle = table {
                     Pres => base_1+"y"+base_2+"jandi" ;
                     Past => base_1+"u"+base_2+"dur"
                   }
      };
    _ => error "Can't apply paradigm mkV093"
  } ;

mkV094 : Str -> V ;
mkV094 base =
  case base of {
    base_1+"i"+base_2@?+"na" => lin V
      { Converb = base_1+"u"+base_2+"nið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"i"+base_2+"n" ;
                               Pl => base_1+"i"+base_2+"nið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"i"+base_2+"ni" ;
                                 PSg P2 => base_1+"i"+base_2+"nur" ;
                                 PSg P3 => base_1+"i"+base_2+"nur" ;
                                 PPl => base_1+"i"+base_2+"na"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2+"n" ;
                                 PSg P2 => base_1+"a"+base_2+"st" ;
                                 PSg P3 => base_1+"a"+base_2+"n" ;
                                 PPl => base_1+"u"+base_2+"nu"
                               }
                     } ;
        Nonfinite = base_1+"i"+base_2+"na" ;
        Particle = table {
                     Pres => base_1+"i"+base_2+"nandi" ;
                     Past => base_1+"u"+base_2+"nin"
                   }
      };
    _ => error "Can't apply paradigm mkV094"
  } ;

mkV095 : Str -> V ;
mkV095 base =
  case base of {
    base_1+"anda" => lin V
      { Converb = base_1+"aðið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"att" ;
                               Pl => base_1+"andið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"andi" ;
                                 PSg P2 => base_1+"endur" ;
                                 PSg P3 => base_1+"endur" ;
                                 PPl => base_1+"anda"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"óð" ;
                                 PSg P2 => base_1+"óðst" ;
                                 PSg P3 => base_1+"óð" ;
                                 PPl => base_1+"óðu"
                               }
                     } ;
        Nonfinite = base_1+"anda" ;
        Particle = table {
                     Pres => base_1+"andandi" ;
                     Past => nonExist
                   }
      };
    _ => error "Can't apply paradigm mkV095"
  } ;

mkV096 : Str -> V ;
mkV096 base =
  case base of {
    base_1+"ei"+base_2@?+"ja" => lin V
      { Converb = base_1+"o"+base_2+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"ei"+base_2 ;
                               Pl => base_1+"ei"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ei"+base_2+"i" ;
                                 PSg P2 => base_1+"ei"+base_2+"ir" ;
                                 PSg P3 => base_1+"ei"+base_2+"ir" ;
                                 PPl => base_1+"ei"+base_2+"ja"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"o"+base_2+"ti" ;
                                 PSg P2 => base_1+"o"+base_2+"ti" ;
                                 PSg P3 => base_1+"o"+base_2+"ti" ;
                                 PPl => base_1+"o"+base_2+"tu"
                               }
                     } ;
        Nonfinite = base_1+"ei"+base_2+"ja" ;
        Particle = table {
                     Pres => base_1+"ei"+base_2+"jandi" ;
                     Past => base_1+"o"+base_2+"tur"
                   }
      };
    _ => error "Can't apply paradigm mkV096"
  } ;

mkV097 : Str -> V ;
mkV097 base =
  case base of {
    base_1+"inga" => lin V
      { Converb = base_1+"ungið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"ikk" ;
                               Pl => base_1+"ingið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ingi" ;
                                 PSg P2 => base_1+"ingur" ;
                                 PSg P3 => base_1+"ingur" ;
                                 PPl => base_1+"inga"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"akk" ;
                                 PSg P2 => base_1+"akst" ;
                                 PSg P3 => base_1+"akk" ;
                                 PPl => base_1+"ungu"
                               }
                     } ;
        Nonfinite = base_1+"inga" ;
        Particle = table {
                     Pres => base_1+"ingandi" ;
                     Past => base_1+"ungin"
                   }
      };
    _ => error "Can't apply paradigm mkV097"
  } ;

mkV098 : Str -> V ;
mkV098 base =
  case base of {
    base_1+"ja"+base_2@?+"a" => lin V
      { Converb = base_1+"o"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"ja"+base_2 ;
                               Pl => base_1+"ja"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ja"+base_2+"i" ;
                                 PSg P2 => base_1+"je"+base_2+"ur" ;
                                 PSg P3 => base_1+"je"+base_2+"ur" ;
                                 PPl => base_1+"ja"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"jó"+base_2 ;
                                 PSg P2 => base_1+"jó"+base_2+"st" ;
                                 PSg P3 => base_1+"jó"+base_2 ;
                                 PPl => base_1+"jó"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"ja"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"ja"+base_2+"andi" ;
                     Past => base_1+"o"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV098"
  } ;

mkV099 : Str -> V ;
mkV099 base =
  case base of {
    base_1+"o"+base_2@?+"a" => lin V
      { Converb = base_1+"o"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"o"+base_2 ;
                               Pl => base_1+"o"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"o"+base_2+"i" ;
                                 PSg P2 => base_1+base_2+"evur" ;
                                 PSg P3 => base_1+base_2+"evur" ;
                                 PPl => base_1+"o"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+base_2+"av" ;
                                 PSg P2 => base_1+base_2+"avst" ;
                                 PSg P3 => base_1+base_2+"av" ;
                                 PPl => base_1+base_2+"óvu"
                               }
                     } ;
        Nonfinite = base_1+"o"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"o"+base_2+"andi" ;
                     Past => nonExist
                   }
      };
    _ => error "Can't apply paradigm mkV099"
  } ;

mkV100 : Str -> V ;
mkV100 base =
  case base of {
    base_1+"í"+base_2@?+"ja" => lin V
      { Converb = base_1+"i"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"í"+base_2+"j" ;
                               Pl => base_1+"í"+base_2+"jið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"í"+base_2+"i" ;
                                 PSg P2 => base_1+"í"+base_2+"ur" ;
                                 PSg P3 => base_1+"í"+base_2+"ur" ;
                                 PPl => base_1+"í"+base_2+"ja"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ei"+base_2 ;
                                 PSg P2 => base_1+"ei"+base_2+"st" ;
                                 PSg P3 => base_1+"ei"+base_2 ;
                                 PPl => base_1+"i"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"í"+base_2+"ja" ;
        Particle = table {
                     Pres => base_1+"í"+base_2+"jandi" ;
                     Past => base_1+"i"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV100"
  } ;

mkV101 : Str -> V ;
mkV101 base =
  case base of {
    base_1+"i"+base_2@?+"ja" => lin V
      { Converb = base_1+"o"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"i"+base_2 ;
                               Pl => base_1+"i"+base_2+"jið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"i"+base_2+"ji" ;
                                 PSg P2 => base_1+"i"+base_2+"ur" ;
                                 PSg P3 => base_1+"i"+base_2+"ur" ;
                                 PPl => base_1+"i"+base_2+"ja"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2 ;
                                 PSg P2 => base_1+"a"+base_2+"st" ;
                                 PSg P3 => base_1+"a"+base_2 ;
                                 PPl => base_1+"u"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"i"+base_2+"ja" ;
        Particle = table {
                     Pres => base_1+"i"+base_2+"jandi" ;
                     Past => nonExist
                   }
      };
    _ => error "Can't apply paradigm mkV101"
  } ;

mkV102 : Str -> V ;
mkV102 base =
  case base of {
    base_1+"ja" => lin V
      { Converb = base_1+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1 ;
                               Pl => base_1+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"i" ;
                                 PSg P2 => base_1+"ir" ;
                                 PSg P3 => base_1+"ir" ;
                                 PPl => base_1+"ja"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"di" ;
                                 PSg P2 => base_1+"di" ;
                                 PSg P3 => base_1+"di" ;
                                 PPl => base_1+"du"
                               }
                     } ;
        Nonfinite = base_1+"ja" ;
        Particle = table {
                     Pres => base_1+"jandi" ;
                     Past => base_1+"dur"
                   }
      };
    _ => error "Can't apply paradigm mkV102"
  } ;

mkV103 : Str -> V ;
mkV103 base =
  case base of {
    base_1+"ø"+base_2@?+"ja" => lin V
      { Converb = base_1+"o"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"ø"+base_2+"j" ;
                               Pl => base_1+"ø"+base_2+"jið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"ø"+base_2+"ji" ;
                                 PSg P2 => base_1+"ø"+base_2+"t" ;
                                 PSg P3 => base_1+"ø"+base_2 ;
                                 PPl => base_1+"ø"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"ó"+base_2 ;
                                 PSg P2 => base_1+"ó"+base_2+"st" ;
                                 PSg P3 => base_1+"ó"+base_2 ;
                                 PPl => base_1+"ó"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"ø"+base_2+"ja" ;
        Particle = table {
                     Pres => base_1+"ø"+base_2+"jandi" ;
                     Past => base_1+"o"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV103"
  } ;

mkV104 : Str -> V ;
mkV104 base =
  case base of {
    base_1+"y"+base_2@(?+?)+"ja" => lin V
      { Converb = base_1+"u"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"y"+base_2+"j" ;
                               Pl => base_1+"y"+base_2+"jið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"y"+base_2+"i" ;
                                 PSg P2 => base_1+"y"+base_2+"ur" ;
                                 PSg P3 => base_1+"y"+base_2+"ur" ;
                                 PPl => base_1+"y"+base_2+"ja"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2 ;
                                 PSg P2 => base_1+"a"+base_2 ;
                                 PSg P3 => base_1+"a"+base_2 ;
                                 PPl => base_1+"u"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"y"+base_2+"ja" ;
        Particle = table {
                     Pres => base_1+"y"+base_2+"jandi" ;
                     Past => base_1+"u"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV104"
  } ;

mkV105 : Str -> V ;
mkV105 base =
  case base of {
    base_1+"i"+base_2@?+"a" => lin V
      { Converb = base_1+"a"+base_2+"t" ;
        Imperative_Jussive = table {
                               Sg => base_1+"i"+base_2 ;
                               Pl => base_1+"i"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"i"+base_2+"i" ;
                                 PSg P2 => base_1+"i"+base_2+"ur" ;
                                 PSg P3 => base_1+"i"+base_2+"ur" ;
                                 PPl => base_1+"i"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2+"di" ;
                                 PSg P2 => base_1+"a"+base_2+"di" ;
                                 PSg P3 => base_1+"a"+base_2+"di" ;
                                 PPl => base_1+"a"+base_2+"du"
                               }
                     } ;
        Nonfinite = base_1+"i"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"i"+base_2+"andi" ;
                     Past => base_1+"a"+base_2+"dur"
                   }
      };
    _ => error "Can't apply paradigm mkV105"
  } ;

mkV106 : Str -> V ;
mkV106 base =
  case base of {
    base_1+"e"+base_2@?+"fa" => lin V
      { Converb = base_1+"o"+base_2+"fið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"e"+base_2+"f" ;
                               Pl => base_1+"e"+base_2+"fið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"e"+base_2+"fi" ;
                                 PSg P2 => base_1+"e"+base_2+"fur" ;
                                 PSg P3 => base_1+"e"+base_2+"fur" ;
                                 PPl => base_1+"e"+base_2+"fa"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2+"f" ;
                                 PSg P2 => base_1+"a"+base_2+"st" ;
                                 PSg P3 => base_1+"a"+base_2+"f" ;
                                 PPl => base_1+"u"+base_2+"fu"
                               }
                     } ;
        Nonfinite = base_1+"e"+base_2+"fa" ;
        Particle = table {
                     Pres => base_1+"e"+base_2+"fandi" ;
                     Past => base_1+"o"+base_2+"fin"
                   }
      };
    _ => error "Can't apply paradigm mkV106"
  } ;

mkV107 : Str -> V ;
mkV107 base =
  case base of {
    base_1+"a"+base_2@(?+?)+"a" => lin V
      { Converb = base_1+"a"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"a"+base_2 ;
                               Pl => base_1+"a"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"a"+base_2+"i" ;
                                 PSg P2 => base_1+"e"+base_2+"ur" ;
                                 PSg P3 => base_1+"e"+base_2+"ur" ;
                                 PPl => base_1+"a"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2 ;
                                 PSg P2 => base_1+"a"+base_2 ;
                                 PSg P3 => base_1+"a"+base_2 ;
                                 PPl => base_1+"u"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"a"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"a"+base_2+"andi" ;
                     Past => base_1+"a"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV107"
  } ;

mkV108 : Str -> V ;
mkV108 base =
  case base of {
    base_1+"e"+base_2@(?+?)+"a" => lin V
      { Converb = nonExist ;
        Imperative_Jussive = table {
                               Sg => base_1+"e"+base_2 ;
                               Pl => base_1+"e"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"e"+base_2+"i" ;
                                 PSg P2 => base_1+"e"+base_2+"ur" ;
                                 PSg P3 => base_1+"e"+base_2+"ur" ;
                                 PPl => base_1+"e"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2 ;
                                 PSg P2 => base_1+"a"+base_2 ;
                                 PSg P3 => base_1+"a"+base_2 ;
                                 PPl => base_1+"ó"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"e"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"e"+base_2+"andi" ;
                     Past => base_1+"o"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV108"
  } ;

mkV109 : Str -> V ;
mkV109 base =
  case base of {
    base_1+base_2@?+base_3@?+"a" => lin V
      { Converb = base_1+base_2+base_3+"að" ;
        Imperative_Jussive = table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"e"+base_2+base_3 ;
                                 PSg P2 => base_1+"e"+base_2+base_3+"st" ;
                                 PSg P3 => base_1+"e"+base_2+base_3 ;
                                 PPl => base_1+base_2+base_3+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+base_2+"s"+base_3+"i" ;
                                 PSg P2 => base_1+base_2+"s"+base_3+"i" ;
                                 PSg P3 => base_1+base_2+"s"+base_3+"i" ;
                                 PPl => base_1+base_2+"s"+base_3+"u"
                               }
                     } ;
        Nonfinite = base_1+base_2+base_3+"a" ;
        Particle = table {
                     Pres => nonExist ;
                     Past => nonExist
                   }
      };
    _ => error "Can't apply paradigm mkV109"
  } ;

mkV110 : Str -> V ;
mkV110 base =
  case base of {
    "d"+base_1+"finite" => lin V
      { Converb = "v"+base_1+"rð" ;
        Imperative_Jussive = table {
                               Sg => "v"+base_1+"rðsins" ;
                               Pl => "v"+base_1+"rðanna"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => "v"+base_1+"rð" ;
                                 PSg P2 => "v"+base_1+"rðið" ;
                                 PSg P3 => "v"+base_1+"rð" ;
                                 PPl => "v"+base_1+"rðini"
                               } ;
                       Past => table {
                                 PSg P1 => "v"+base_1+"rð" ;
                                 PSg P2 => "v"+base_1+"rðið" ;
                                 PSg P3 => "v"+base_1+"rð" ;
                                 PPl => "v"+base_1+"rðini"
                               }
                     } ;
        Nonfinite = "d"+base_1+"finite" ;
        Particle = table {
                     Pres => "v"+base_1+"rð" ;
                     Past => "v"+base_1+"rð"
                   }
      };
    _ => error "Can't apply paradigm mkV110"
  } ;

mkV111 : Str -> V ;
mkV111 base =
  case base of {
    base_1+"e"+base_2@?+"a" => lin V
      { Converb = base_1+"o"+base_2+"ið" ;
        Imperative_Jussive = table {
                               Sg => base_1+"e"+base_2 ;
                               Pl => base_1+"e"+base_2+"ið"
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1+"e"+base_2+"i" ;
                                 PSg P2 => base_1+"e"+base_2+"ur" ;
                                 PSg P3 => base_1+"e"+base_2+"ur" ;
                                 PPl => base_1+"e"+base_2+"a"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"a"+base_2 ;
                                 PSg P2 => base_1+"a"+base_2+"st" ;
                                 PSg P3 => base_1+"a"+base_2 ;
                                 PPl => base_1+"ó"+base_2+"u"
                               }
                     } ;
        Nonfinite = base_1+"e"+base_2+"a" ;
        Particle = table {
                     Pres => base_1+"e"+base_2+"andi" ;
                     Past => base_1+"o"+base_2+"in"
                   }
      };
    _ => error "Can't apply paradigm mkV111"
  } ;

mkV112 : Str -> V ;
mkV112 base =
  case base of {
    base_1+"ja" => lin V
      { Converb = base_1+"að" ;
        Imperative_Jussive = table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
        Indicative = table {
                       Pres => table {
                                 PSg P1 => base_1 ;
                                 PSg P2 => base_1+"t" ;
                                 PSg P3 => base_1 ;
                                 PPl => base_1+"ja"
                               } ;
                       Past => table {
                                 PSg P1 => base_1+"di" ;
                                 PSg P2 => base_1+"di" ;
                                 PSg P3 => base_1+"di" ;
                                 PPl => base_1+"du"
                               }
                     } ;
        Nonfinite = base_1+"ja" ;
        Particle = table {
                     Pres => nonExist ;
                     Past => nonExist
                   }
      };
    _ => error "Can't apply paradigm mkV112"
  } ;
}