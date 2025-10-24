resource MorphoHye = open CatHye, ResHye, Predef in {

oper

mkV001 : Str -> V ;
mkV001 base =
  case base of {
    base_1+"ել" => lin V
      { s = base_1+"ել" ;
        Causative = base_1+"եցնել" ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => "կ"+base_1+"եի" ;
                                             Pl => "կ"+base_1+"եինք"
                                           } ;
                                     P2 => table {
                                             Sg => "կ"+base_1+"եիր" ;
                                             Pl => "կ"+base_1+"եիք"
                                           } ;
                                     P3 => table {
                                             Sg => "կ"+base_1+"եր" ;
                                             Pl => "կ"+base_1+"եին"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => "կ"+base_1+"եմ" ;
                                              Pl => "կ"+base_1+"ենք"
                                            } ;
                                      P2 => table {
                                              Sg => "կ"+base_1+"ես" ;
                                              Pl => "կ"+base_1+"եք"
                                            } ;
                                      P3 => table {
                                              Sg => "կ"+base_1+"ի" ;
                                              Pl => "կ"+base_1+"են"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = base_1+"ում" ;
                    FutCon1 = base_1+"ելու" ;
                    FutCon2 = base_1+"ելիք" ;
                    Negative = base_1+"ի" ;
                    Perfective = base_1+"ել" ;
                    Simultaneous = base_1+"ելիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => base_1+"ի՛ր" ;
                               Pl => base_1+"ե՛ք"
                             } ;
        Passive = base_1+"վել" ;
        Past = table {
                 P1 => table {
                         Sg => base_1+"եցի" ;
                         Pl => base_1+"եցինք"
                       } ;
                 P2 => table {
                         Sg => base_1+"եցիր" ;
                         Pl => base_1+"եցիք"
                       } ;
                 P3 => table {
                         Sg => base_1+"եց" ;
                         Pl => base_1+"եցին"
                       }
               } ;
        Participle = table {
                       Resultative => base_1+"ած" ;
                       Subject => base_1+"ող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+"եի" ;
                                             Pl => base_1+"եինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+"եիր" ;
                                             Pl => base_1+"եիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+"եր" ;
                                             Pl => base_1+"եին"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+"եմ" ;
                                              Pl => base_1+"ենք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+"ես" ;
                                              Pl => base_1+"եք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+"ի" ;
                                              Pl => base_1+"են"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV001"
  } ;

mkV002 : Str -> V ;
mkV002 base =
  case base of {
    base_1+"ալ" => lin V
      { s = base_1+"ալ" ;
        Causative = base_1+"ացնել" ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => "կ"+base_1+"այի" ;
                                             Pl => "կ"+base_1+"այինք"
                                           } ;
                                     P2 => table {
                                             Sg => "կ"+base_1+"այիր" ;
                                             Pl => "կ"+base_1+"այիք"
                                           } ;
                                     P3 => table {
                                             Sg => "կ"+base_1+"ար" ;
                                             Pl => "կ"+base_1+"ային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => "կ"+base_1+"ամ" ;
                                              Pl => "կ"+base_1+"անք"
                                            } ;
                                      P2 => table {
                                              Sg => "կ"+base_1+"աս" ;
                                              Pl => "կ"+base_1+"աք"
                                            } ;
                                      P3 => table {
                                              Sg => "կ"+base_1+"ա" ;
                                              Pl => "կ"+base_1+"ան"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = base_1+"ում" ;
                    FutCon1 = base_1+"ալու" ;
                    FutCon2 = base_1+"ալիք" ;
                    Negative = base_1+"ա" ;
                    Perfective = base_1+"ացել" ;
                    Simultaneous = base_1+"ալիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => base_1+"ա՛" ;
                               Pl => base_1+"ացե՛ք"
                             } ;
        Passive = nonExist ;
        Past = table {
                 P1 => table {
                         Sg => base_1+"ացի" ;
                         Pl => base_1+"ացինք"
                       } ;
                 P2 => table {
                         Sg => base_1+"ացիր" ;
                         Pl => base_1+"ացիք"
                       } ;
                 P3 => table {
                         Sg => base_1+"աց" ;
                         Pl => base_1+"ացին"
                       }
               } ;
        Participle = table {
                       Resultative => base_1+"ացած" ;
                       Subject => base_1+"ացող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+"այի" ;
                                             Pl => base_1+"այինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+"այիր" ;
                                             Pl => base_1+"այիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+"ար" ;
                                             Pl => base_1+"ային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+"ամ" ;
                                              Pl => base_1+"անք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+"աս" ;
                                              Pl => base_1+"աք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+"ա" ;
                                              Pl => base_1+"ան"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV002"
  } ;

mkV003 : Str -> V ;
mkV003 base =
  case base of {
    base_1+"նել" => lin V
      { s = base_1+"նել" ;
        Causative = nonExist ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => "կ"+base_1+"նեի" ;
                                             Pl => "կ"+base_1+"նեինք"
                                           } ;
                                     P2 => table {
                                             Sg => "կ"+base_1+"նեիր" ;
                                             Pl => "կ"+base_1+"նեիք"
                                           } ;
                                     P3 => table {
                                             Sg => "կ"+base_1+"ներ" ;
                                             Pl => "կ"+base_1+"նեին"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => "կ"+base_1+"նեմ" ;
                                              Pl => "կ"+base_1+"նենք"
                                            } ;
                                      P2 => table {
                                              Sg => "կ"+base_1+"նես" ;
                                              Pl => "կ"+base_1+"նեք"
                                            } ;
                                      P3 => table {
                                              Sg => "կ"+base_1+"նի" ;
                                              Pl => "կ"+base_1+"նեն"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = base_1+"նում" ;
                    FutCon1 = base_1+"նելու" ;
                    FutCon2 = base_1+"նելիք" ;
                    Negative = base_1+"նի" ;
                    Perfective = base_1+"րել" ;
                    Simultaneous = base_1+"նելիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => base_1+"րո՛ւ" ;
                               Pl => base_1+"րե՛ք"
                             } ;
        Passive = base_1+"վել" ;
        Past = table {
                 P1 => table {
                         Sg => base_1+"րի" ;
                         Pl => base_1+"րինք"
                       } ;
                 P2 => table {
                         Sg => base_1+"րիր" ;
                         Pl => base_1+"րիք"
                       } ;
                 P3 => table {
                         Sg => base_1+"րեց" ;
                         Pl => base_1+"րին"
                       }
               } ;
        Participle = table {
                       Resultative => base_1+"րած" ;
                       Subject => base_1+"նող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+"նեի" ;
                                             Pl => base_1+"նեինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+"նեիր" ;
                                             Pl => base_1+"նեիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+"ներ" ;
                                             Pl => base_1+"նեին"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+"նեմ" ;
                                              Pl => base_1+"նենք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+"նես" ;
                                              Pl => base_1+"նեք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+"նի" ;
                                              Pl => base_1+"նեն"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV003"
  } ;

mkV004 : Str -> V ;
mkV004 base =
  case base of {
    base_1+"նալ" => lin V
      { s = base_1+"նալ" ;
        Causative = base_1+"ցնել" ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => "կ"+base_1+"նայի" ;
                                             Pl => "կ"+base_1+"նայինք"
                                           } ;
                                     P2 => table {
                                             Sg => "կ"+base_1+"նայիր" ;
                                             Pl => "կ"+base_1+"նայիք"
                                           } ;
                                     P3 => table {
                                             Sg => "կ"+base_1+"նար" ;
                                             Pl => "կ"+base_1+"նային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => "կ"+base_1+"նամ" ;
                                              Pl => "կ"+base_1+"նանք"
                                            } ;
                                      P2 => table {
                                              Sg => "կ"+base_1+"նաս" ;
                                              Pl => "կ"+base_1+"նաք"
                                            } ;
                                      P3 => table {
                                              Sg => "կ"+base_1+"նա" ;
                                              Pl => "կ"+base_1+"նան"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = base_1+"նում" ;
                    FutCon1 = base_1+"նալու" ;
                    FutCon2 = base_1+"նալիք" ;
                    Negative = base_1+"նա" ;
                    Perfective = base_1+"ցել" ;
                    Simultaneous = base_1+"նալիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => base_1+"ցի՛ր" ;
                               Pl => base_1+"ցե՛ք"
                             } ;
        Passive = nonExist ;
        Past = table {
                 P1 => table {
                         Sg => base_1+"ցա" ;
                         Pl => base_1+"ցանք"
                       } ;
                 P2 => table {
                         Sg => base_1+"ցար" ;
                         Pl => base_1+"ցաք"
                       } ;
                 P3 => table {
                         Sg => base_1+"ցավ" ;
                         Pl => base_1+"ցան"
                       }
               } ;
        Participle = table {
                       Resultative => base_1+"ցած" ;
                       Subject => base_1+"ցող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+"նայի" ;
                                             Pl => base_1+"նայինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+"նայիր" ;
                                             Pl => base_1+"նայիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+"նար" ;
                                             Pl => base_1+"նային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+"նամ" ;
                                              Pl => base_1+"նանք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+"նաս" ;
                                              Pl => base_1+"նաք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+"նա" ;
                                              Pl => base_1+"նան"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV004"
  } ;

mkV005 : Str -> V ;
mkV005 base =
  case base of {
    base_1+"ռնալ" => lin V
      { s = base_1+"ռնալ" ;
        Causative = base_1+"րձնել" ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => "կ"+base_1+"ռնայի" ;
                                             Pl => "կ"+base_1+"ռնայինք"
                                           } ;
                                     P2 => table {
                                             Sg => "կ"+base_1+"ռնայիր" ;
                                             Pl => "կ"+base_1+"ռնայիք"
                                           } ;
                                     P3 => table {
                                             Sg => "կ"+base_1+"ռնար" ;
                                             Pl => "կ"+base_1+"ռնային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => "կ"+base_1+"ռնամ" ;
                                              Pl => "կ"+base_1+"ռնանք"
                                            } ;
                                      P2 => table {
                                              Sg => "կ"+base_1+"ռնաս" ;
                                              Pl => "կ"+base_1+"ռնաք"
                                            } ;
                                      P3 => table {
                                              Sg => "կ"+base_1+"ռնա" ;
                                              Pl => "կ"+base_1+"ռնան"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = base_1+"ռնում" ;
                    FutCon1 = base_1+"ռնալու" ;
                    FutCon2 = base_1+"ռնալիք" ;
                    Negative = base_1+"ռնա" ;
                    Perfective = base_1+"րձել" ;
                    Simultaneous = base_1+"ռնալիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => base_1+"րձի՛ր" ;
                               Pl => base_1+"րձե՛ք"
                             } ;
        Passive = nonExist ;
        Past = table {
                 P1 => table {
                         Sg => base_1+"րձեցի" ;
                         Pl => base_1+"րձեցինք"
                       } ;
                 P2 => table {
                         Sg => base_1+"րձեցիր" ;
                         Pl => base_1+"րձեցիք"
                       } ;
                 P3 => table {
                         Sg => base_1+"րձեց" ;
                         Pl => base_1+"րձեցին"
                       }
               } ;
        Participle = table {
                       Resultative => base_1+"րձած" ;
                       Subject => base_1+"րձող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+"ռնայի" ;
                                             Pl => base_1+"ռնայինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+"ռնայիր" ;
                                             Pl => base_1+"ռնայիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+"ռնար" ;
                                             Pl => base_1+"ռնային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+"ռնամ" ;
                                              Pl => base_1+"ռնանք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+"ռնաս" ;
                                              Pl => base_1+"ռնաք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+"ռնա" ;
                                              Pl => base_1+"ռնան"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV005"
  } ;

mkV006 : Str -> V ;
mkV006 base =
  case base of {
    "երթալ" => lin V
      { s = "երթալ" ;
        Causative = "գնացնել" ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => "կերթայի" ;
                                             Pl => "կերթայինք"
                                           } ;
                                     P2 => table {
                                             Sg => "կերթայիր" ;
                                             Pl => "կերթայիք"
                                           } ;
                                     P3 => table {
                                             Sg => "կերթար" ;
                                             Pl => "կերթային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => "կերթամ" ;
                                              Pl => "կերթանք"
                                            } ;
                                      P2 => table {
                                              Sg => "կերթաս" ;
                                              Pl => "կերթաք"
                                            } ;
                                      P3 => table {
                                              Sg => "կերթա" ;
                                              Pl => "կերթան"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = "երթում" ;
                    FutCon1 = "երթալու" ;
                    FutCon2 = "երթալիք" ;
                    Negative = "երթա" ;
                    Perfective = "գնացել" ;
                    Simultaneous = "երթալիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => "երթա՛" ;
                               Pl => "գնացե՛ք"
                             } ;
        Passive = nonExist ;
        Past = table {
                 P1 => table {
                         Sg => "գնացի" ;
                         Pl => "գնացինք"
                       } ;
                 P2 => table {
                         Sg => "գնացիր" ;
                         Pl => "գնացիք"
                       } ;
                 P3 => table {
                         Sg => "գնաց" ;
                         Pl => "գնացին"
                       }
               } ;
        Participle = table {
                       Resultative => "գնացած" ;
                       Subject => "գնացող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => "երթայի" ;
                                             Pl => "երթայինք"
                                           } ;
                                     P2 => table {
                                             Sg => "երթայիր" ;
                                             Pl => "երթայիք"
                                           } ;
                                     P3 => table {
                                             Sg => "երթար" ;
                                             Pl => "երթային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => "երթամ" ;
                                              Pl => "երթանք"
                                            } ;
                                      P2 => table {
                                              Sg => "երթաս" ;
                                              Pl => "երթաք"
                                            } ;
                                      P3 => table {
                                              Sg => "երթա" ;
                                              Pl => "երթան"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV006"
  } ;

mkV007 : Str -> V ;
mkV007 base =
  case base of {
    "էթալ" => lin V
      { s = "էթալ" ;
        Causative = "գնացնել" ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => "կէթայի" ;
                                             Pl => "կէթայինք"
                                           } ;
                                     P2 => table {
                                             Sg => "կէթայիր" ;
                                             Pl => "կէթայիք"
                                           } ;
                                     P3 => table {
                                             Sg => "կէթար" ;
                                             Pl => "կէթային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => "կէթամ" ;
                                              Pl => "կէթանք"
                                            } ;
                                      P2 => table {
                                              Sg => "կէթաս" ;
                                              Pl => "կէթաք"
                                            } ;
                                      P3 => table {
                                              Sg => "կէթա" ;
                                              Pl => "կէթան"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = "էթում" ;
                    FutCon1 = "էթալու" ;
                    FutCon2 = "էթալիք" ;
                    Negative = "էթա" ;
                    Perfective = "գնացել" ;
                    Simultaneous = "էթալիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => "էթա՛" ;
                               Pl => "գնացե՛ք"
                             } ;
        Passive = nonExist ;
        Past = table {
                 P1 => table {
                         Sg => "գնացի" ;
                         Pl => "գնացինք"
                       } ;
                 P2 => table {
                         Sg => "գնացիր" ;
                         Pl => "գնացիք"
                       } ;
                 P3 => table {
                         Sg => "գնաց" ;
                         Pl => "գնացին"
                       }
               } ;
        Participle = table {
                       Resultative => "գնացած" ;
                       Subject => "գնացող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => "էթայի" ;
                                             Pl => "էթայինք"
                                           } ;
                                     P2 => table {
                                             Sg => "էթայիր" ;
                                             Pl => "էթայիք"
                                           } ;
                                     P3 => table {
                                             Sg => "էթար" ;
                                             Pl => "էթային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => "էթամ" ;
                                              Pl => "էթանք"
                                            } ;
                                      P2 => table {
                                              Sg => "էթաս" ;
                                              Pl => "էթաք"
                                            } ;
                                      P3 => table {
                                              Sg => "էթա" ;
                                              Pl => "էթան"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV007"
  } ;

mkV008 : Str -> V ;
mkV008 base =
  case base of {
    base_1+"նել" => lin V
      { s = base_1+"նել" ;
        Causative = base_1+"ցնել" ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => "կ"+base_1+"նեի" ;
                                             Pl => "կ"+base_1+"նեինք"
                                           } ;
                                     P2 => table {
                                             Sg => "կ"+base_1+"նեիր" ;
                                             Pl => "կ"+base_1+"նեիք"
                                           } ;
                                     P3 => table {
                                             Sg => "կ"+base_1+"ներ" ;
                                             Pl => "կ"+base_1+"նեին"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => "կ"+base_1+"նեմ" ;
                                              Pl => "կ"+base_1+"նենք"
                                            } ;
                                      P2 => table {
                                              Sg => "կ"+base_1+"նես" ;
                                              Pl => "կ"+base_1+"նեք"
                                            } ;
                                      P3 => table {
                                              Sg => "կ"+base_1+"նի" ;
                                              Pl => "կ"+base_1+"նեն"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = base_1+"նում" ;
                    FutCon1 = base_1+"նելու" ;
                    FutCon2 = base_1+"նելիք" ;
                    Negative = base_1+"նի" ;
                    Perfective = base_1+"ել" ;
                    Simultaneous = base_1+"նելիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => base_1+"ի՛ր" ;
                               Pl => base_1+"ե՛ք"
                             } ;
        Passive = base_1+"նվել" ;
        Past = table {
                 P1 => table {
                         Sg => base_1+"ա" ;
                         Pl => base_1+"անք"
                       } ;
                 P2 => table {
                         Sg => base_1+"ար" ;
                         Pl => base_1+"աք"
                       } ;
                 P3 => table {
                         Sg => base_1+"ավ" ;
                         Pl => base_1+"ան"
                       }
               } ;
        Participle = table {
                       Resultative => base_1+"ած" ;
                       Subject => base_1+"նող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+"նեի" ;
                                             Pl => base_1+"նեինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+"նեիր" ;
                                             Pl => base_1+"նեիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+"ներ" ;
                                             Pl => base_1+"նեին"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+"նեմ" ;
                                              Pl => base_1+"նենք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+"նես" ;
                                              Pl => base_1+"նեք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+"նի" ;
                                              Pl => base_1+"նեն"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV008"
  } ;

mkV009 : Str -> V ;
mkV009 base =
  case base of {
    "ըլնել" => lin V
      { s = "ըլնել" ;
        Causative = nonExist ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => "կըլնեի" ;
                                             Pl => "կըլնեինք"
                                           } ;
                                     P2 => table {
                                             Sg => "կըլնեիր" ;
                                             Pl => "կըլնեիք"
                                           } ;
                                     P3 => table {
                                             Sg => "կըլներ" ;
                                             Pl => "կըլնեին"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => "կըլնեմ" ;
                                              Pl => "կըլնենք"
                                            } ;
                                      P2 => table {
                                              Sg => "կըլնես" ;
                                              Pl => "կըլնեք"
                                            } ;
                                      P3 => table {
                                              Sg => "կըլնի" ;
                                              Pl => "կըլնեն"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = "ըլնում" ;
                    FutCon1 = "ըլնելու" ;
                    FutCon2 = "ըլնելիք" ;
                    Negative = "ըլնի" ;
                    Perfective = "էղել" ;
                    Simultaneous = "ըլնելիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => "էղի՛ր" ;
                               Pl => "էղե՛ք"
                             } ;
        Passive = nonExist ;
        Past = table {
                 P1 => table {
                         Sg => "էղա" ;
                         Pl => "էղանք"
                       } ;
                 P2 => table {
                         Sg => "էղար" ;
                         Pl => "էղաք"
                       } ;
                 P3 => table {
                         Sg => "էղավ" ;
                         Pl => "էղան"
                       }
               } ;
        Participle = table {
                       Resultative => "էղած" ;
                       Subject => "ըլնող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => "ըլնեի" ;
                                             Pl => "ըլնեինք"
                                           } ;
                                     P2 => table {
                                             Sg => "ըլնեիր" ;
                                             Pl => "ըլնեիք"
                                           } ;
                                     P3 => table {
                                             Sg => "ըլներ" ;
                                             Pl => "ըլնեին"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => "ըլնեմ" ;
                                              Pl => "ըլնենք"
                                            } ;
                                      P2 => table {
                                              Sg => "ըլնես" ;
                                              Pl => "ըլնեք"
                                            } ;
                                      P3 => table {
                                              Sg => "ըլնի" ;
                                              Pl => "ըլնեն"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV009"
  } ;

mkV010 : Str -> V ;
mkV010 base =
  case base of {
    base_1+"նալ" => lin V
      { s = base_1+"նալ" ;
        Causative = base_1+"ցնել" ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => "կ"+base_1+"նայի" ;
                                             Pl => "կ"+base_1+"նայինք"
                                           } ;
                                     P2 => table {
                                             Sg => "կ"+base_1+"նայիր" ;
                                             Pl => "կ"+base_1+"նայիք"
                                           } ;
                                     P3 => table {
                                             Sg => "կ"+base_1+"նար" ;
                                             Pl => "կ"+base_1+"նային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => "կ"+base_1+"նամ" ;
                                              Pl => "կ"+base_1+"նանք"
                                            } ;
                                      P2 => table {
                                              Sg => "կ"+base_1+"նաս" ;
                                              Pl => "կ"+base_1+"նաք"
                                            } ;
                                      P3 => table {
                                              Sg => "կ"+base_1+"նա" ;
                                              Pl => "կ"+base_1+"նան"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = base_1+"նում" ;
                    FutCon1 = base_1+"նալու" ;
                    FutCon2 = base_1+"նալիք" ;
                    Negative = base_1+"նա" ;
                    Perfective = base_1+"ցել" ;
                    Simultaneous = base_1+"նալիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => base_1+"՛" ;
                               Pl => base_1+"ցե՛ք"
                             } ;
        Passive = nonExist ;
        Past = table {
                 P1 => table {
                         Sg => base_1+"ցի" ;
                         Pl => base_1+"ցինք"
                       } ;
                 P2 => table {
                         Sg => base_1+"ցիր" ;
                         Pl => base_1+"ցիք"
                       } ;
                 P3 => table {
                         Sg => base_1+"ց" ;
                         Pl => base_1+"ցին"
                       }
               } ;
        Participle = table {
                       Resultative => base_1+"ցած" ;
                       Subject => base_1+"ցող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+"նայի" ;
                                             Pl => base_1+"նայինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+"նայիր" ;
                                             Pl => base_1+"նայիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+"նար" ;
                                             Pl => base_1+"նային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+"նամ" ;
                                              Pl => base_1+"նանք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+"նաս" ;
                                              Pl => base_1+"նաք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+"նա" ;
                                              Pl => base_1+"նան"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV010"
  } ;

mkV011 : Str -> V ;
mkV011 base =
  case base of {
    base_1+"ալ" => lin V
      { s = base_1+"ալ" ;
        Causative = base_1+"ացնել" ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => "կ"+base_1+"այի" ;
                                             Pl => "կ"+base_1+"այինք"
                                           } ;
                                     P2 => table {
                                             Sg => "կ"+base_1+"այիր" ;
                                             Pl => "կ"+base_1+"այիք"
                                           } ;
                                     P3 => table {
                                             Sg => "կ"+base_1+"ար" ;
                                             Pl => "կ"+base_1+"ային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => "կ"+base_1+"ամ" ;
                                              Pl => "կ"+base_1+"անք"
                                            } ;
                                      P2 => table {
                                              Sg => "կ"+base_1+"աս" ;
                                              Pl => "կ"+base_1+"աք"
                                            } ;
                                      P3 => table {
                                              Sg => "կ"+base_1+"այ" ;
                                              Pl => "կ"+base_1+"ան"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = base_1+"ում" ;
                    FutCon1 = base_1+"ալու" ;
                    FutCon2 = base_1+"ալիք" ;
                    Negative = base_1+"այ" ;
                    Perfective = base_1+"ացել" ;
                    Simultaneous = base_1+"ալիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => base_1+"ա՛" ;
                               Pl => base_1+"ացէ՛ք"
                             } ;
        Passive = nonExist ;
        Past = table {
                 P1 => table {
                         Sg => base_1+"ացի" ;
                         Pl => base_1+"ացինք"
                       } ;
                 P2 => table {
                         Sg => base_1+"ացիր" ;
                         Pl => base_1+"ացիք"
                       } ;
                 P3 => table {
                         Sg => base_1+"աց" ;
                         Pl => base_1+"ացին"
                       }
               } ;
        Participle = table {
                       Resultative => base_1+"ացած" ;
                       Subject => base_1+"ացող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+"այի" ;
                                             Pl => base_1+"այինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+"այիր" ;
                                             Pl => base_1+"այիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+"ար" ;
                                             Pl => base_1+"ային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+"ամ" ;
                                              Pl => base_1+"անք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+"աս" ;
                                              Pl => base_1+"աք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+"այ" ;
                                              Pl => base_1+"ան"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV011"
  } ;

mkV012 : Str -> V ;
mkV012 base =
  case base of {
    base_1+"ել" => lin V
      { s = base_1+"ել" ;
        Causative = base_1+"եցնել" ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => "կ"+base_1+"էի" ;
                                             Pl => "կ"+base_1+"էինք"
                                           } ;
                                     P2 => table {
                                             Sg => "կ"+base_1+"էիր" ;
                                             Pl => "կ"+base_1+"էիք"
                                           } ;
                                     P3 => table {
                                             Sg => "կ"+base_1+"էր" ;
                                             Pl => "կ"+base_1+"էին"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => "կ"+base_1+"եմ" ;
                                              Pl => "կ"+base_1+"ենք"
                                            } ;
                                      P2 => table {
                                              Sg => "կ"+base_1+"ես" ;
                                              Pl => "կ"+base_1+"էք"
                                            } ;
                                      P3 => table {
                                              Sg => "կ"+base_1+"ի" ;
                                              Pl => "կ"+base_1+"են"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = base_1+"ում" ;
                    FutCon1 = base_1+"ելու" ;
                    FutCon2 = base_1+"ելիք" ;
                    Negative = base_1+"ի" ;
                    Perfective = base_1+"ել" ;
                    Simultaneous = base_1+"ելիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => base_1+"ի՛ր" ;
                               Pl => base_1+"է՛ք"
                             } ;
        Passive = base_1+"ուել" ;
        Past = table {
                 P1 => table {
                         Sg => base_1+"եցի" ;
                         Pl => base_1+"եցինք"
                       } ;
                 P2 => table {
                         Sg => base_1+"եցիր" ;
                         Pl => base_1+"եցիք"
                       } ;
                 P3 => table {
                         Sg => base_1+"եց" ;
                         Pl => base_1+"եցին"
                       }
               } ;
        Participle = table {
                       Resultative => base_1+"ած" ;
                       Subject => base_1+"ող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+"էի" ;
                                             Pl => base_1+"էինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+"էիր" ;
                                             Pl => base_1+"էիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+"էր" ;
                                             Pl => base_1+"էին"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+"եմ" ;
                                              Pl => base_1+"ենք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+"ես" ;
                                              Pl => base_1+"էք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+"ի" ;
                                              Pl => base_1+"են"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV012"
  } ;

mkV013 : Str -> V ;
mkV013 base =
  case base of {
    base_1@?+base_2+"ել" => lin V
      { s = base_1+base_2+"ել" ;
        Causative = base_1+base_2+"եցնել" ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+"կ"+base_2+"եի" ;
                                             Pl => base_1+"կ"+base_2+"եինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+"կ"+base_2+"եիր" ;
                                             Pl => base_1+"կ"+base_2+"եիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+"կ"+base_2+"եր" ;
                                             Pl => base_1+"կ"+base_2+"եին"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+"կ"+base_2+"եմ" ;
                                              Pl => base_1+"կ"+base_2+"ենք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+"կ"+base_2+"ես" ;
                                              Pl => base_1+"կ"+base_2+"եք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+"կ"+base_2+"ի" ;
                                              Pl => base_1+"կ"+base_2+"են"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = base_1+base_2+"ում" ;
                    FutCon1 = base_1+base_2+"ելու" ;
                    FutCon2 = base_1+base_2+"ելիք" ;
                    Negative = base_1+base_2+"ի" ;
                    Perfective = base_1+base_2+"ել" ;
                    Simultaneous = base_1+base_2+"ելիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => base_1+base_2+"ի՛ր" ;
                               Pl => base_1+base_2+"ե՛ք"
                             } ;
        Passive = base_1+base_2+"վել" ;
        Past = table {
                 P1 => table {
                         Sg => base_1+base_2+"եցի" ;
                         Pl => base_1+base_2+"եցինք"
                       } ;
                 P2 => table {
                         Sg => base_1+base_2+"եցիր" ;
                         Pl => base_1+base_2+"եցիք"
                       } ;
                 P3 => table {
                         Sg => base_1+base_2+"եց" ;
                         Pl => base_1+base_2+"եցին"
                       }
               } ;
        Participle = table {
                       Resultative => base_1+base_2+"ած" ;
                       Subject => base_1+base_2+"ող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+base_2+"եի" ;
                                             Pl => base_1+base_2+"եինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+base_2+"եիր" ;
                                             Pl => base_1+base_2+"եիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+base_2+"եր" ;
                                             Pl => base_1+base_2+"եին"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+base_2+"եմ" ;
                                              Pl => base_1+base_2+"ենք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+base_2+"ես" ;
                                              Pl => base_1+base_2+"եք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+base_2+"ի" ;
                                              Pl => base_1+base_2+"են"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV013"
  } ;

mkV014 : Str -> V ;
mkV014 base =
  case base of {
    base_1+base_2@(?+?)+"նել" => lin V
      { s = base_1+base_2+"նել" ;
        Causative = nonExist ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+"կ"+base_2+"նեի" ;
                                             Pl => base_1+"կ"+base_2+"նեինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+"կ"+base_2+"նեիր" ;
                                             Pl => base_1+"կ"+base_2+"նեիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+"կ"+base_2+"ներ" ;
                                             Pl => base_1+"կ"+base_2+"նեին"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+"կ"+base_2+"նեմ" ;
                                              Pl => base_1+"կ"+base_2+"նենք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+"կ"+base_2+"նես" ;
                                              Pl => base_1+"կ"+base_2+"նեք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+"կ"+base_2+"նի" ;
                                              Pl => base_1+"կ"+base_2+"նեն"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = base_1+base_2+"նում" ;
                    FutCon1 = base_1+base_2+"նելու" ;
                    FutCon2 = base_1+base_2+"նելիք" ;
                    Negative = base_1+base_2+"նի" ;
                    Perfective = base_1+base_2+"ել" ;
                    Simultaneous = base_1+base_2+"նելիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => base_1+base_2+"ի՛ր" ;
                               Pl => base_1+base_2+"ե՛ք"
                             } ;
        Passive = base_1+base_2+"նվել" ;
        Past = table {
                 P1 => table {
                         Sg => base_1+base_2+"ա" ;
                         Pl => base_1+base_2+"անք"
                       } ;
                 P2 => table {
                         Sg => base_1+base_2+"ար" ;
                         Pl => base_1+base_2+"աք"
                       } ;
                 P3 => table {
                         Sg => base_1+base_2+"ավ" ;
                         Pl => base_1+base_2+"ան"
                       }
               } ;
        Participle = table {
                       Resultative => base_1+base_2+"ած" ;
                       Subject => base_1+base_2+"նող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+base_2+"նեի" ;
                                             Pl => base_1+base_2+"նեինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+base_2+"նեիր" ;
                                             Pl => base_1+base_2+"նեիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+base_2+"ներ" ;
                                             Pl => base_1+base_2+"նեին"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+base_2+"նեմ" ;
                                              Pl => base_1+base_2+"նենք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+base_2+"նես" ;
                                              Pl => base_1+base_2+"նեք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+base_2+"նի" ;
                                              Pl => base_1+base_2+"նեն"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV014"
  } ;

mkV015 : Str -> V ;
mkV015 base =
  case base of {
    base_1@?+base_2+"նալ" => lin V
      { s = base_1+base_2+"նալ" ;
        Causative = base_1+base_2+"ցնել" ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+"կ"+base_2+"նայի" ;
                                             Pl => base_1+"կ"+base_2+"նայինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+"կ"+base_2+"նայիր" ;
                                             Pl => base_1+"կ"+base_2+"նայիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+"կ"+base_2+"նար" ;
                                             Pl => base_1+"կ"+base_2+"նային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+"կ"+base_2+"նամ" ;
                                              Pl => base_1+"կ"+base_2+"նանք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+"կ"+base_2+"նաս" ;
                                              Pl => base_1+"կ"+base_2+"նաք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+"կ"+base_2+"նա" ;
                                              Pl => base_1+"կ"+base_2+"նան"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = base_1+base_2+"նում" ;
                    FutCon1 = base_1+base_2+"նալու" ;
                    FutCon2 = base_1+base_2+"նալիք" ;
                    Negative = base_1+base_2+"նա" ;
                    Perfective = base_1+base_2+"ցել" ;
                    Simultaneous = base_1+base_2+"նալիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => base_1+base_2+"ցի՛ր" ;
                               Pl => base_1+base_2+"ցե՛ք"
                             } ;
        Passive = nonExist ;
        Past = table {
                 P1 => table {
                         Sg => base_1+base_2+"ցա" ;
                         Pl => base_1+base_2+"ցանք"
                       } ;
                 P2 => table {
                         Sg => base_1+base_2+"ցար" ;
                         Pl => base_1+base_2+"ցաք"
                       } ;
                 P3 => table {
                         Sg => base_1+base_2+"ցավ" ;
                         Pl => base_1+base_2+"ցան"
                       }
               } ;
        Participle = table {
                       Resultative => base_1+base_2+"ցած" ;
                       Subject => base_1+base_2+"ցող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+base_2+"նայի" ;
                                             Pl => base_1+base_2+"նայինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+base_2+"նայիր" ;
                                             Pl => base_1+base_2+"նայիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+base_2+"նար" ;
                                             Pl => base_1+base_2+"նային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+base_2+"նամ" ;
                                              Pl => base_1+base_2+"նանք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+base_2+"նաս" ;
                                              Pl => base_1+base_2+"նաք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+base_2+"նա" ;
                                              Pl => base_1+base_2+"նան"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV015"
  } ;

mkV016 : Str -> V ;
mkV016 base =
  case base of {
    base_1+base_2@(?+?+?+?)+"ել" => lin V
      { s = base_1+base_2+"ել" ;
        Causative = nonExist ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+"կ"+base_2+"էի" ;
                                             Pl => base_1+"կ"+base_2+"էինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+"կ"+base_2+"էիր" ;
                                             Pl => base_1+"կ"+base_2+"էիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+"կ"+base_2+"էր" ;
                                             Pl => base_1+"կ"+base_2+"էին"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+"կ"+base_2+"եմ" ;
                                              Pl => base_1+"կ"+base_2+"ենք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+"կ"+base_2+"ես" ;
                                              Pl => base_1+"կ"+base_2+"էք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+"կ"+base_2+"ի" ;
                                              Pl => base_1+"կ"+base_2+"են"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = base_1+base_2+"ում" ;
                    FutCon1 = base_1+base_2+"ելու" ;
                    FutCon2 = base_1+base_2+"ելիք" ;
                    Negative = base_1+base_2+"ի" ;
                    Perfective = base_1+base_2+"ել" ;
                    Simultaneous = base_1+base_2+"ելիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => base_1+base_2+"ի՛ր" ;
                               Pl => base_1+base_2+"է՛ք"
                             } ;
        Passive = base_1+base_2+"ուել" ;
        Past = table {
                 P1 => table {
                         Sg => base_1+base_2+"եցի" ;
                         Pl => base_1+base_2+"եցինք"
                       } ;
                 P2 => table {
                         Sg => base_1+base_2+"եցիր" ;
                         Pl => base_1+base_2+"եցիք"
                       } ;
                 P3 => table {
                         Sg => base_1+base_2+"եց" ;
                         Pl => base_1+base_2+"եցին"
                       }
               } ;
        Participle = table {
                       Resultative => base_1+base_2+"ած" ;
                       Subject => base_1+base_2+"ող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+base_2+"էի" ;
                                             Pl => base_1+base_2+"էինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+base_2+"էիր" ;
                                             Pl => base_1+base_2+"էիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+base_2+"էր" ;
                                             Pl => base_1+base_2+"էին"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+base_2+"եմ" ;
                                              Pl => base_1+base_2+"ենք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+base_2+"ես" ;
                                              Pl => base_1+base_2+"էք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+base_2+"ի" ;
                                              Pl => base_1+base_2+"են"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV016"
  } ;

mkV017 : Str -> V ;
mkV017 base =
  case base of {
    base_1+base_2@(?+?+?)+"նել" => lin V
      { s = base_1+base_2+"նել" ;
        Causative = nonExist ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+"կ"+base_2+"նեի" ;
                                             Pl => base_1+"կ"+base_2+"նեինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+"կ"+base_2+"նեիր" ;
                                             Pl => base_1+"կ"+base_2+"նեիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+"կ"+base_2+"ներ" ;
                                             Pl => base_1+"կ"+base_2+"նեին"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+"կ"+base_2+"նեմ" ;
                                              Pl => base_1+"կ"+base_2+"նենք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+"կ"+base_2+"նես" ;
                                              Pl => base_1+"կ"+base_2+"նեք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+"կ"+base_2+"նի" ;
                                              Pl => base_1+"կ"+base_2+"նեն"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = base_1+base_2+"նում" ;
                    FutCon1 = base_1+base_2+"նելու" ;
                    FutCon2 = base_1+base_2+"նելիք" ;
                    Negative = base_1+base_2+"նի" ;
                    Perfective = base_1+base_2+"րել" ;
                    Simultaneous = base_1+base_2+"նելիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => base_1+base_2+"րո՛ւ" ;
                               Pl => base_1+base_2+"րե՛ք"
                             } ;
        Passive = base_1+base_2+"վել" ;
        Past = table {
                 P1 => table {
                         Sg => base_1+base_2+"րի" ;
                         Pl => base_1+base_2+"րինք"
                       } ;
                 P2 => table {
                         Sg => base_1+base_2+"րիր" ;
                         Pl => base_1+base_2+"րիք"
                       } ;
                 P3 => table {
                         Sg => base_1+base_2+"րեց" ;
                         Pl => base_1+base_2+"րին"
                       }
               } ;
        Participle = table {
                       Resultative => base_1+base_2+"րած" ;
                       Subject => base_1+base_2+"նող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+base_2+"նեի" ;
                                             Pl => base_1+base_2+"նեինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+base_2+"նեիր" ;
                                             Pl => base_1+base_2+"նեիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+base_2+"ներ" ;
                                             Pl => base_1+base_2+"նեին"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+base_2+"նեմ" ;
                                              Pl => base_1+base_2+"նենք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+base_2+"նես" ;
                                              Pl => base_1+base_2+"նեք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+base_2+"նի" ;
                                              Pl => base_1+base_2+"նեն"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV017"
  } ;

mkV018 : Str -> V ;
mkV018 base =
  case base of {
    base_1+"ել" => lin V
      { s = base_1+"ել" ;
        Causative = nonExist ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+"կեի" ;
                                             Pl => base_1+"կեինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+"կեիր" ;
                                             Pl => base_1+"կեիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+"կեր" ;
                                             Pl => base_1+"կեին"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+"կեմ" ;
                                              Pl => base_1+"կենք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+"կես" ;
                                              Pl => base_1+"կեք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+"կի" ;
                                              Pl => base_1+"կեն"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = base_1+"ում" ;
                    FutCon1 = base_1+"ելու" ;
                    FutCon2 = base_1+"ելիք" ;
                    Negative = base_1+"ի" ;
                    Perfective = base_1+"ել" ;
                    Simultaneous = base_1+"ելիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => base_1+"ի՛ր" ;
                               Pl => base_1+"ե՛ք"
                             } ;
        Passive = base_1+"վել" ;
        Past = table {
                 P1 => table {
                         Sg => base_1+"եցի" ;
                         Pl => base_1+"եցինք"
                       } ;
                 P2 => table {
                         Sg => base_1+"եցիր" ;
                         Pl => base_1+"եցիք"
                       } ;
                 P3 => table {
                         Sg => base_1+"եց" ;
                         Pl => base_1+"եցին"
                       }
               } ;
        Participle = table {
                       Resultative => base_1+"ած" ;
                       Subject => base_1+"ող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+"եի" ;
                                             Pl => base_1+"եինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+"եիր" ;
                                             Pl => base_1+"եիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+"եր" ;
                                             Pl => base_1+"եին"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+"եմ" ;
                                              Pl => base_1+"ենք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+"ես" ;
                                              Pl => base_1+"եք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+"ի" ;
                                              Pl => base_1+"են"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV018"
  } ;

mkV019 : Str -> V ;
mkV019 base =
  case base of {
    base_1+base_2@(?+?+?)+"ալ" => lin V
      { s = base_1+base_2+"ալ" ;
        Causative = base_1+base_2+"ացնել" ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+"կ"+base_2+"այի" ;
                                             Pl => base_1+"կ"+base_2+"այինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+"կ"+base_2+"այիր" ;
                                             Pl => base_1+"կ"+base_2+"այիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+"կ"+base_2+"ար" ;
                                             Pl => base_1+"կ"+base_2+"ային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+"կ"+base_2+"ամ" ;
                                              Pl => base_1+"կ"+base_2+"անք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+"կ"+base_2+"աս" ;
                                              Pl => base_1+"կ"+base_2+"աք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+"կ"+base_2+"ա" ;
                                              Pl => base_1+"կ"+base_2+"ան"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = base_1+base_2+"ում" ;
                    FutCon1 = base_1+base_2+"ալու" ;
                    FutCon2 = base_1+base_2+"ալիք" ;
                    Negative = base_1+base_2+"ա" ;
                    Perfective = base_1+base_2+"ացել" ;
                    Simultaneous = base_1+base_2+"ալիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => base_1+base_2+"ա՛" ;
                               Pl => base_1+base_2+"ացե՛ք"
                             } ;
        Passive = nonExist ;
        Past = table {
                 P1 => table {
                         Sg => base_1+base_2+"ացի" ;
                         Pl => base_1+base_2+"ացինք"
                       } ;
                 P2 => table {
                         Sg => base_1+base_2+"ացիր" ;
                         Pl => base_1+base_2+"ացիք"
                       } ;
                 P3 => table {
                         Sg => base_1+base_2+"աց" ;
                         Pl => base_1+base_2+"ացին"
                       }
               } ;
        Participle = table {
                       Resultative => base_1+base_2+"ացած" ;
                       Subject => base_1+base_2+"ացող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+base_2+"այի" ;
                                             Pl => base_1+base_2+"այինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+base_2+"այիր" ;
                                             Pl => base_1+base_2+"այիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+base_2+"ար" ;
                                             Pl => base_1+base_2+"ային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+base_2+"ամ" ;
                                              Pl => base_1+base_2+"անք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+base_2+"աս" ;
                                              Pl => base_1+base_2+"աք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+base_2+"ա" ;
                                              Pl => base_1+base_2+"ան"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV019"
  } ;

mkV020 : Str -> V ;
mkV020 base =
  case base of {
    base_1+"նալ" => lin V
      { s = base_1+"նալ" ;
        Causative = base_1+"ցնել" ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => "կ"+base_1+"նայի" ;
                                             Pl => "կ"+base_1+"նայինք"
                                           } ;
                                     P2 => table {
                                             Sg => "կ"+base_1+"նայիր" ;
                                             Pl => "կ"+base_1+"նայիք"
                                           } ;
                                     P3 => table {
                                             Sg => "կ"+base_1+"նար" ;
                                             Pl => "կ"+base_1+"նային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => "կ"+base_1+"նամ" ;
                                              Pl => "կ"+base_1+"նանք"
                                            } ;
                                      P2 => table {
                                              Sg => "կ"+base_1+"նաս" ;
                                              Pl => "կ"+base_1+"նաք"
                                            } ;
                                      P3 => table {
                                              Sg => "կ"+base_1+"նայ" ;
                                              Pl => "կ"+base_1+"նան"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = base_1+"նում" ;
                    FutCon1 = base_1+"նալու" ;
                    FutCon2 = base_1+"նալիք" ;
                    Negative = base_1+"նայ" ;
                    Perfective = base_1+"ցել" ;
                    Simultaneous = base_1+"նալիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => base_1+"ցի՛ր" ;
                               Pl => base_1+"ցէ՛ք"
                             } ;
        Passive = nonExist ;
        Past = table {
                 P1 => table {
                         Sg => base_1+"ցայ" ;
                         Pl => base_1+"ցանք"
                       } ;
                 P2 => table {
                         Sg => base_1+"ցար" ;
                         Pl => base_1+"ցաք"
                       } ;
                 P3 => table {
                         Sg => base_1+"ցաւ" ;
                         Pl => base_1+"ցան"
                       }
               } ;
        Participle = table {
                       Resultative => base_1+"ցած" ;
                       Subject => base_1+"ցող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+"նայի" ;
                                             Pl => base_1+"նայինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+"նայիր" ;
                                             Pl => base_1+"նայիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+"նար" ;
                                             Pl => base_1+"նային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+"նամ" ;
                                              Pl => base_1+"նանք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+"նաս" ;
                                              Pl => base_1+"նաք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+"նայ" ;
                                              Pl => base_1+"նան"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV020"
  } ;

mkV021 : Str -> V ;
mkV021 base =
  case base of {
    base_1+base_2@(?+?)+"ել" => lin V
      { s = base_1+base_2+"ել" ;
        Causative = nonExist ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => "կ"+base_1+base_2+"էի" ;
                                             Pl => "կ"+base_1+base_2+"էինք"
                                           } ;
                                     P2 => table {
                                             Sg => "կ"+base_1+base_2+"էիր" ;
                                             Pl => "կ"+base_1+base_2+"էիք"
                                           } ;
                                     P3 => table {
                                             Sg => "կ"+base_1+base_2+"էր" ;
                                             Pl => "կ"+base_1+base_2+"էին"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => "կ"+base_1+base_2+"եմ" ;
                                              Pl => "կ"+base_1+base_2+"ենք"
                                            } ;
                                      P2 => table {
                                              Sg => "կ"+base_1+base_2+"ես" ;
                                              Pl => "կ"+base_1+base_2+"էք"
                                            } ;
                                      P3 => table {
                                              Sg => "կ"+base_1+base_2+"ի" ;
                                              Pl => "կ"+base_1+base_2+"են"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = base_1+"ւ"+base_2+"մ" ;
                    FutCon1 = base_1+base_2+"ելու" ;
                    FutCon2 = base_1+base_2+"ելիք" ;
                    Negative = base_1+base_2+"ի" ;
                    Perfective = base_1+base_2+"ել" ;
                    Simultaneous = base_1+base_2+"ելիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => base_1+base_2+"ի՛ր" ;
                               Pl => base_1+base_2+"է՛ք"
                             } ;
        Passive = nonExist ;
        Past = table {
                 P1 => table {
                         Sg => base_1+base_2+"եցի" ;
                         Pl => base_1+base_2+"եցինք"
                       } ;
                 P2 => table {
                         Sg => base_1+base_2+"եցիր" ;
                         Pl => base_1+base_2+"եցիք"
                       } ;
                 P3 => table {
                         Sg => base_1+base_2+"եց" ;
                         Pl => base_1+base_2+"եցին"
                       }
               } ;
        Participle = table {
                       Resultative => base_1+base_2+"ած" ;
                       Subject => base_1+base_2+"ող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+base_2+"էի" ;
                                             Pl => base_1+base_2+"էինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+base_2+"էիր" ;
                                             Pl => base_1+base_2+"էիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+base_2+"էր" ;
                                             Pl => base_1+base_2+"էին"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+base_2+"եմ" ;
                                              Pl => base_1+base_2+"ենք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+base_2+"ես" ;
                                              Pl => base_1+base_2+"էք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+base_2+"ի" ;
                                              Pl => base_1+base_2+"են"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV021"
  } ;

mkV022 : Str -> V ;
mkV022 base =
  case base of {
    base_1+"ռնալ" => lin V
      { s = base_1+"ռնալ" ;
        Causative = base_1+"րձնել" ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => "կ"+base_1+"ռնայի" ;
                                             Pl => "կ"+base_1+"ռնայինք"
                                           } ;
                                     P2 => table {
                                             Sg => "կ"+base_1+"ռնայիր" ;
                                             Pl => "կ"+base_1+"ռնայիք"
                                           } ;
                                     P3 => table {
                                             Sg => "կ"+base_1+"ռնար" ;
                                             Pl => "կ"+base_1+"ռնային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => "կ"+base_1+"ռնամ" ;
                                              Pl => "կ"+base_1+"ռնանք"
                                            } ;
                                      P2 => table {
                                              Sg => "կ"+base_1+"ռնաս" ;
                                              Pl => "կ"+base_1+"ռնաք"
                                            } ;
                                      P3 => table {
                                              Sg => "կ"+base_1+"ռնա" ;
                                              Pl => "կ"+base_1+"ռնան"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = base_1+"ռնում" ;
                    FutCon1 = base_1+"ռնալու" ;
                    FutCon2 = base_1+"ռնալիք" ;
                    Negative = base_1+"ռնա" ;
                    Perfective = base_1+"րձել" ;
                    Simultaneous = base_1+"ռնալիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => base_1+"րձի՛ր" ;
                               Pl => base_1+"րձե՛ք"
                             } ;
        Passive = nonExist ;
        Past = table {
                 P1 => table {
                         Sg => base_1+"րձա" ;
                         Pl => base_1+"րձանք"
                       } ;
                 P2 => table {
                         Sg => base_1+"րձար" ;
                         Pl => base_1+"րձաք"
                       } ;
                 P3 => table {
                         Sg => base_1+"րձավ" ;
                         Pl => base_1+"րձան"
                       }
               } ;
        Participle = table {
                       Resultative => base_1+"րձած" ;
                       Subject => base_1+"րձող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+"ռնայի" ;
                                             Pl => base_1+"ռնայինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+"ռնայիր" ;
                                             Pl => base_1+"ռնայիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+"ռնար" ;
                                             Pl => base_1+"ռնային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+"ռնամ" ;
                                              Pl => base_1+"ռնանք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+"ռնաս" ;
                                              Pl => base_1+"ռնաք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+"ռնա" ;
                                              Pl => base_1+"ռնան"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV022"
  } ;

mkV023 : Str -> V ;
mkV023 base =
  case base of {
    base_1+"նալ" => lin V
      { s = base_1+"նալ" ;
        Causative = base_1+"սնել" ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => "կ"+base_1+"նայի" ;
                                             Pl => "կ"+base_1+"նայինք"
                                           } ;
                                     P2 => table {
                                             Sg => "կ"+base_1+"նայիր" ;
                                             Pl => "կ"+base_1+"նայիք"
                                           } ;
                                     P3 => table {
                                             Sg => "կ"+base_1+"նար" ;
                                             Pl => "կ"+base_1+"նային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => "կ"+base_1+"նամ" ;
                                              Pl => "կ"+base_1+"նանք"
                                            } ;
                                      P2 => table {
                                              Sg => "կ"+base_1+"նաս" ;
                                              Pl => "կ"+base_1+"նաք"
                                            } ;
                                      P3 => table {
                                              Sg => "կ"+base_1+"նա" ;
                                              Pl => "կ"+base_1+"նան"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = base_1+"նում" ;
                    FutCon1 = base_1+"նալու" ;
                    FutCon2 = base_1+"նալիք" ;
                    Negative = base_1+"նա" ;
                    Perfective = base_1+"սել" ;
                    Simultaneous = base_1+"նալիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => nonExist ;
                               Pl => base_1+"սե՛ք"
                             } ;
        Passive = nonExist ;
        Past = table {
                 P1 => table {
                         Sg => base_1+"սա" ;
                         Pl => base_1+"սանք"
                       } ;
                 P2 => table {
                         Sg => base_1+"սար" ;
                         Pl => base_1+"սաք"
                       } ;
                 P3 => table {
                         Sg => base_1+"սավ" ;
                         Pl => base_1+"սան"
                       }
               } ;
        Participle = table {
                       Resultative => base_1+"սած" ;
                       Subject => base_1+"սող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+"նայի" ;
                                             Pl => base_1+"նայինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+"նայիր" ;
                                             Pl => base_1+"նայիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+"նար" ;
                                             Pl => base_1+"նային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+"նամ" ;
                                              Pl => base_1+"նանք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+"նաս" ;
                                              Pl => base_1+"նաք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+"նա" ;
                                              Pl => base_1+"նան"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV023"
  } ;

mkV024 : Str -> V ;
mkV024 base =
  case base of {
    base_1+"նալ" => lin V
      { s = base_1+"նալ" ;
        Causative = base_1+"նել" ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => "կ"+base_1+"նայի" ;
                                             Pl => "կ"+base_1+"նայինք"
                                           } ;
                                     P2 => table {
                                             Sg => "կ"+base_1+"նայիր" ;
                                             Pl => "կ"+base_1+"նայիք"
                                           } ;
                                     P3 => table {
                                             Sg => "կ"+base_1+"նար" ;
                                             Pl => "կ"+base_1+"նային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => "կ"+base_1+"նամ" ;
                                              Pl => "կ"+base_1+"նանք"
                                            } ;
                                      P2 => table {
                                              Sg => "կ"+base_1+"նաս" ;
                                              Pl => "կ"+base_1+"նաք"
                                            } ;
                                      P3 => table {
                                              Sg => "կ"+base_1+"նա" ;
                                              Pl => "կ"+base_1+"նան"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = base_1+"նում" ;
                    FutCon1 = base_1+"նալու" ;
                    FutCon2 = base_1+"նալիք" ;
                    Negative = base_1+"նա" ;
                    Perfective = base_1+"ել" ;
                    Simultaneous = base_1+"նալիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => nonExist ;
                               Pl => base_1+"ե՛ք"
                             } ;
        Passive = nonExist ;
        Past = table {
                 P1 => table {
                         Sg => base_1+"ա" ;
                         Pl => base_1+"անք"
                       } ;
                 P2 => table {
                         Sg => base_1+"ար" ;
                         Pl => base_1+"աք"
                       } ;
                 P3 => table {
                         Sg => base_1+"ավ" ;
                         Pl => base_1+"ան"
                       }
               } ;
        Participle = table {
                       Resultative => base_1+"ած" ;
                       Subject => base_1+"ող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+"նայի" ;
                                             Pl => base_1+"նայինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+"նայիր" ;
                                             Pl => base_1+"նայիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+"նար" ;
                                             Pl => base_1+"նային"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+"նամ" ;
                                              Pl => base_1+"նանք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+"նաս" ;
                                              Pl => base_1+"նաք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+"նա" ;
                                              Pl => base_1+"նան"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV024"
  } ;

mkV025 : Str -> V ;
mkV025 base =
  case base of {
    base_1+base_2@?+"ել" => lin V
      { s = base_1+base_2+"ել" ;
        Causative = nonExist ;
        Conditional = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => "կ"+base_1+base_2+"եի" ;
                                             Pl => "կ"+base_1+base_2+"եինք"
                                           } ;
                                     P2 => table {
                                             Sg => "կ"+base_1+base_2+"եիր" ;
                                             Pl => "կ"+base_1+base_2+"եիք"
                                           } ;
                                     P3 => table {
                                             Sg => "կ"+base_1+base_2+"եր" ;
                                             Pl => "կ"+base_1+base_2+"եին"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => "կ"+base_1+base_2+"եմ" ;
                                              Pl => "կ"+base_1+base_2+"ենք"
                                            } ;
                                      P2 => table {
                                              Sg => "կ"+base_1+base_2+"ես" ;
                                              Pl => "կ"+base_1+base_2+"եք"
                                            } ;
                                      P3 => table {
                                              Sg => "կ"+base_1+base_2+"ի" ;
                                              Pl => "կ"+base_1+base_2+"են"
                                            }
                                    }
                      } ;
        Converb = { Imperfective = base_1+base_2+"ում" ;
                    FutCon1 = base_1+base_2+"ելու" ;
                    FutCon2 = base_1+base_2+"ելիք" ;
                    Negative = base_1+base_2+"ի" ;
                    Perfective = base_1+base_2+"ել" ;
                    Simultaneous = base_1+base_2+"ելիս"
                  } ;
        Imperative_Jussive = table {
                               Sg => base_1+"՛"+base_2 ;
                               Pl => base_1+base_2+"ե՛ք"
                             } ;
        Passive = nonExist ;
        Past = table {
                 P1 => table {
                         Sg => base_1+base_2+"եցի" ;
                         Pl => base_1+base_2+"եցինք"
                       } ;
                 P2 => table {
                         Sg => base_1+base_2+"եցիր" ;
                         Pl => base_1+base_2+"եցիք"
                       } ;
                 P3 => table {
                         Sg => base_1+base_2+"եց" ;
                         Pl => base_1+base_2+"եցին"
                       }
               } ;
        Participle = table {
                       Resultative => base_1+base_2+"ած" ;
                       Subject => base_1+base_2+"ող"
                     } ;
        Subjunctive = table {
                        Perfect => table {
                                     P1 => table {
                                             Sg => base_1+base_2+"եի" ;
                                             Pl => base_1+base_2+"եինք"
                                           } ;
                                     P2 => table {
                                             Sg => base_1+base_2+"եիր" ;
                                             Pl => base_1+base_2+"եիք"
                                           } ;
                                     P3 => table {
                                             Sg => base_1+base_2+"եր" ;
                                             Pl => base_1+base_2+"եին"
                                           }
                                   } ;
                        Non_Past => table {
                                      P1 => table {
                                              Sg => base_1+base_2+"եմ" ;
                                              Pl => base_1+base_2+"ենք"
                                            } ;
                                      P2 => table {
                                              Sg => base_1+base_2+"ես" ;
                                              Pl => base_1+base_2+"եք"
                                            } ;
                                      P3 => table {
                                              Sg => base_1+base_2+"ի" ;
                                              Pl => base_1+base_2+"են"
                                            }
                                    }
                      }
      };
    _ => error "Can't apply paradigm mkV025"
  } ;

mkN001 : Str -> N ;
mkN001 base =
  case base of {
    base_1 => lin N
      { s = table {
              Nom => table {
                       Sg => base_1 ;
                       Pl => base_1+"ներ"
                     } ;
              Dat => table {
                       Sg => base_1+"ի" ;
                       Pl => base_1+"ների"
                     } ;
              Ablat => table {
                         Sg => base_1+"ից" ;
                         Pl => base_1+"ներից"
                       } ;
              Instr => table {
                         Sg => base_1+"ով" ;
                         Pl => base_1+"ներով"
                       } ;
              Loc => table {
                       Sg => base_1+"ում" ;
                       Pl => base_1+"ներում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"ին" ;
                    Pl => base_1+"ներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ը" ;
                    Pl => base_1+"ները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ս" ;
                           Pl => base_1+"ներս"
                         } ;
                  Dat => table {
                           Sg => base_1+"իս" ;
                           Pl => base_1+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"իցս" ;
                             Pl => base_1+"ներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովս" ;
                             Pl => base_1+"ներովս"
                           } ;
                  Loc => table {
                           Sg => base_1+"ումս" ;
                           Pl => base_1+"ներումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"դ" ;
                           Pl => base_1+"ներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"իդ" ;
                           Pl => base_1+"ներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"իցդ" ;
                             Pl => base_1+"ներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովդ" ;
                             Pl => base_1+"ներովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+"ումդ" ;
                           Pl => base_1+"ներումդ"
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
              Nom => table {
                       Sg => base_1 ;
                       Pl => base_1+"ներ"
                     } ;
              Dat => table {
                       Sg => base_1+"յի" ;
                       Pl => base_1+"ների"
                     } ;
              Ablat => table {
                         Sg => base_1+"յից" ;
                         Pl => base_1+"ներից"
                       } ;
              Instr => table {
                         Sg => base_1+"յով" ;
                         Pl => base_1+"ներով"
                       } ;
              Loc => table {
                       Sg => base_1+"յում" ;
                       Pl => base_1+"ներում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"յին" ;
                    Pl => base_1+"ներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ն" ;
                    Pl => base_1+"ները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ս" ;
                           Pl => base_1+"ներս"
                         } ;
                  Dat => table {
                           Sg => base_1+"յիս" ;
                           Pl => base_1+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"յիցս" ;
                             Pl => base_1+"ներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"յովս" ;
                             Pl => base_1+"ներովս"
                           } ;
                  Loc => table {
                           Sg => base_1+"յումս" ;
                           Pl => base_1+"ներումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"դ" ;
                           Pl => base_1+"ներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"յիդ" ;
                           Pl => base_1+"ներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"յիցդ" ;
                             Pl => base_1+"ներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"յովդ" ;
                             Pl => base_1+"ներովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+"յումդ" ;
                           Pl => base_1+"ներումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkN002"
  } ;

mkN003 : Str -> N ;
mkN003 base =
  case base of {
    base_1+"ի" => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ի" ;
                       Pl => base_1+"իներ"
                     } ;
              Dat => table {
                       Sg => base_1+"ու" ;
                       Pl => base_1+"իների"
                     } ;
              Ablat => table {
                         Sg => base_1+"ուց" ;
                         Pl => base_1+"իներից"
                       } ;
              Instr => table {
                         Sg => base_1+"ով" ;
                         Pl => base_1+"իներով"
                       } ;
              Loc => table {
                       Sg => base_1+"ում" ;
                       Pl => base_1+"իներում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"ուն" ;
                    Pl => base_1+"իներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ին" ;
                    Pl => base_1+"իները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"իս" ;
                           Pl => base_1+"իներս"
                         } ;
                  Dat => table {
                           Sg => base_1+"ուս" ;
                           Pl => base_1+"իներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ուցս" ;
                             Pl => base_1+"իներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովս" ;
                             Pl => base_1+"իներովս"
                           } ;
                  Loc => table {
                           Sg => base_1+"ումս" ;
                           Pl => base_1+"իներումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"իդ" ;
                           Pl => base_1+"իներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"ուդ" ;
                           Pl => base_1+"իներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ուցդ" ;
                             Pl => base_1+"իներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովդ" ;
                             Pl => base_1+"իներովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+"ումդ" ;
                           Pl => base_1+"իներումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkN003"
  } ;

mkN004 : Str -> N ;
mkN004 base =
  case base of {
    base_1+"ու"+base_2@? => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ու"+base_2 ;
                       Pl => base_1+"ու"+base_2+"ներ"
                     } ;
              Dat => table {
                       Sg => base_1+base_2+"ան" ;
                       Pl => base_1+"ու"+base_2+"ների"
                     } ;
              Ablat => table {
                         Sg => base_1+"ու"+base_2+"ից" ;
                         Pl => base_1+"ու"+base_2+"ներից"
                       } ;
              Instr => table {
                         Sg => base_1+base_2+"ամբ" ;
                         Pl => base_1+"ու"+base_2+"ներով"
                       } ;
              Loc => table {
                       Sg => base_1+"ու"+base_2+"ում" ;
                       Pl => base_1+"ու"+base_2+"ներում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+base_2+"անը" ;
                    Pl => base_1+"ու"+base_2+"ներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ու"+base_2+"ը" ;
                    Pl => base_1+"ու"+base_2+"ները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ու"+base_2+"ս" ;
                           Pl => base_1+"ու"+base_2+"ներս"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"անս" ;
                           Pl => base_1+"ու"+base_2+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ու"+base_2+"իցս" ;
                             Pl => base_1+"ու"+base_2+"ներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ու"+base_2+"ովս" ;
                             Pl => base_1+"ու"+base_2+"ներովս"
                           } ;
                  Loc => table {
                           Sg => base_1+"ու"+base_2+"ումս" ;
                           Pl => base_1+"ու"+base_2+"ներումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ու"+base_2+"դ" ;
                           Pl => base_1+"ու"+base_2+"ներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"անդ" ;
                           Pl => base_1+"ու"+base_2+"ներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ու"+base_2+"իցդ" ;
                             Pl => base_1+"ու"+base_2+"ներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ու"+base_2+"ովդ" ;
                             Pl => base_1+"ու"+base_2+"ներովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+"ու"+base_2+"ումդ" ;
                           Pl => base_1+"ու"+base_2+"ներումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkN004"
  } ;

mkN005 : Str -> N ;
mkN005 base =
  case base of {
    base_1+"ու"+base_2@("ղթ"|"րչ"|"ղտ"|"մբ"|"րմ"|"նջ"|"նդ"|"րձ"|"րդ"|"րծք"|?) => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ու"+base_2 ;
                       Pl => base_1+base_2+"եր"
                     } ;
              Dat => table {
                       Sg => base_1+base_2+"ի" ;
                       Pl => base_1+base_2+"երի"
                     } ;
              Ablat => table {
                         Sg => base_1+base_2+"ից" ;
                         Pl => base_1+base_2+"երից"
                       } ;
              Instr => table {
                         Sg => base_1+base_2+"ով" ;
                         Pl => base_1+base_2+"երով"
                       } ;
              Loc => table {
                       Sg => base_1+base_2+"ում" ;
                       Pl => base_1+base_2+"երում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+base_2+"ին" ;
                    Pl => base_1+base_2+"երին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ու"+base_2+"ը" ;
                    Pl => base_1+base_2+"երը"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ու"+base_2+"ս" ;
                           Pl => base_1+base_2+"երս"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"իս" ;
                           Pl => base_1+base_2+"երիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"իցս" ;
                             Pl => base_1+base_2+"երիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ովս" ;
                             Pl => base_1+base_2+"երովս"
                           } ;
                  Loc => table {
                           Sg => base_1+base_2+"ումս" ;
                           Pl => base_1+base_2+"երումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ու"+base_2+"դ" ;
                           Pl => base_1+base_2+"երդ"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"իդ" ;
                           Pl => base_1+base_2+"երիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"իցդ" ;
                             Pl => base_1+base_2+"երիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ովդ" ;
                             Pl => base_1+base_2+"երովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+base_2+"ումդ" ;
                           Pl => base_1+base_2+"երումդ"
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
              Nom => table {
                       Sg => base_1 ;
                       Pl => base_1+"ներ"
                     } ;
              Dat => table {
                       Sg => base_1+"ի" ;
                       Pl => base_1+"ների"
                     } ;
              Ablat => table {
                         Sg => base_1+"ից" ;
                         Pl => base_1+"ներից"
                       } ;
              Instr => table {
                         Sg => base_1+"ով" ;
                         Pl => base_1+"ներով"
                       } ;
              Loc => table {
                       Sg => base_1+"ում" ;
                       Pl => base_1+"ներում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"ին" ;
                    Pl => base_1+"ներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ն" ;
                    Pl => base_1+"ները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ս" ;
                           Pl => base_1+"ներս"
                         } ;
                  Dat => table {
                           Sg => base_1+"իս" ;
                           Pl => base_1+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"իցս" ;
                             Pl => base_1+"ներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովս" ;
                             Pl => base_1+"ներովս"
                           } ;
                  Loc => table {
                           Sg => base_1+"ումս" ;
                           Pl => base_1+"ներումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"դ" ;
                           Pl => base_1+"ներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"իդ" ;
                           Pl => base_1+"ներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"իցդ" ;
                             Pl => base_1+"ներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովդ" ;
                             Pl => base_1+"ներովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+"ումդ" ;
                           Pl => base_1+"ներումդ"
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
              Nom => table {
                       Sg => base_1 ;
                       Pl => base_1+"եր"
                     } ;
              Dat => table {
                       Sg => base_1+"ի" ;
                       Pl => base_1+"երի"
                     } ;
              Ablat => table {
                         Sg => base_1+"ից" ;
                         Pl => base_1+"երից"
                       } ;
              Instr => table {
                         Sg => base_1+"ով" ;
                         Pl => base_1+"երով"
                       } ;
              Loc => table {
                       Sg => base_1+"ում" ;
                       Pl => base_1+"երում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"ին" ;
                    Pl => base_1+"երին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ը" ;
                    Pl => base_1+"երը"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ս" ;
                           Pl => base_1+"երս"
                         } ;
                  Dat => table {
                           Sg => base_1+"իս" ;
                           Pl => base_1+"երիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"իցս" ;
                             Pl => base_1+"երիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովս" ;
                             Pl => base_1+"երովս"
                           } ;
                  Loc => table {
                           Sg => base_1+"ումս" ;
                           Pl => base_1+"երումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"դ" ;
                           Pl => base_1+"երդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"իդ" ;
                           Pl => base_1+"երիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"իցդ" ;
                             Pl => base_1+"երիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովդ" ;
                             Pl => base_1+"երովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+"ումդ" ;
                           Pl => base_1+"երումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkN007"
  } ;

mkN008 : Str -> N ;
mkN008 base =
  case base of {
    base_1+"իւն" => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"իւն" ;
                       Pl => base_1+"իւններ"
                     } ;
              Dat => table {
                       Sg => base_1+"եան" ;
                       Pl => base_1+"իւնների"
                     } ;
              Ablat => table {
                         Sg => base_1+"իւնից" ;
                         Pl => base_1+"իւններից"
                       } ;
              Instr => table {
                         Sg => base_1+"եամբ" ;
                         Pl => base_1+"իւններով"
                       } ;
              Loc => table {
                       Sg => base_1+"իւնում" ;
                       Pl => base_1+"իւններում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"եանը" ;
                    Pl => base_1+"իւններին"
                  } ;
        def_nom = table {
                    Sg => base_1+"իւնը" ;
                    Pl => base_1+"իւնները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"իւնս" ;
                           Pl => base_1+"իւններս"
                         } ;
                  Dat => table {
                           Sg => base_1+"եանս" ;
                           Pl => base_1+"իւններիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"իւնիցս" ;
                             Pl => base_1+"իւններիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"եամբս" ;
                             Pl => base_1+"իւններովս"
                           } ;
                  Loc => table {
                           Sg => base_1+"իւնումս" ;
                           Pl => base_1+"իւններումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"իւնդ" ;
                           Pl => base_1+"իւններդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"եանդ" ;
                           Pl => base_1+"իւններիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"իւնիցդ" ;
                             Pl => base_1+"իւններիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"եամբդ" ;
                             Pl => base_1+"իւններովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+"իւնումդ" ;
                           Pl => base_1+"իւններումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkN008"
  } ;

mkN009 : Str -> N ;
mkN009 base =
  case base of {
    base_1+"ուն" => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ուն" ;
                       Pl => base_1+"ուններ"
                     } ;
              Dat => table {
                       Sg => base_1+"վան" ;
                       Pl => base_1+"ունների"
                     } ;
              Ablat => table {
                         Sg => base_1+"ունից" ;
                         Pl => base_1+"ուններից"
                       } ;
              Instr => table {
                         Sg => base_1+"վամբ" ;
                         Pl => base_1+"ուններով"
                       } ;
              Loc => table {
                       Sg => base_1+"ունում" ;
                       Pl => base_1+"ուններում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"վանը" ;
                    Pl => base_1+"ուններին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ունը" ;
                    Pl => base_1+"ունները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ունս" ;
                           Pl => base_1+"ուններս"
                         } ;
                  Dat => table {
                           Sg => base_1+"վանս" ;
                           Pl => base_1+"ուններիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ունիցս" ;
                             Pl => base_1+"ուններիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ունովս" ;
                             Pl => base_1+"ուններովս"
                           } ;
                  Loc => table {
                           Sg => base_1+"ունումս" ;
                           Pl => base_1+"ուններումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ունդ" ;
                           Pl => base_1+"ուններդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"վանդ" ;
                           Pl => base_1+"ուններիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ունիցդ" ;
                             Pl => base_1+"ուններիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ունովդ" ;
                             Pl => base_1+"ուններովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+"ունումդ" ;
                           Pl => base_1+"ուններումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkN009"
  } ;

mkN010 : Str -> N ;
mkN010 base =
  case base of {
    base_1+"ուն" => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ուն" ;
                       Pl => base_1+"ուններ"
                     } ;
              Dat => table {
                       Sg => base_1+"ան" ;
                       Pl => base_1+"ունների"
                     } ;
              Ablat => table {
                         Sg => base_1+"ունից" ;
                         Pl => base_1+"ուններից"
                       } ;
              Instr => table {
                         Sg => base_1+"ամբ" ;
                         Pl => base_1+"ուններով"
                       } ;
              Loc => table {
                       Sg => base_1+"ունում" ;
                       Pl => base_1+"ուններում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"անը" ;
                    Pl => base_1+"ուններին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ունը" ;
                    Pl => base_1+"ունները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ունս" ;
                           Pl => base_1+"ուններս"
                         } ;
                  Dat => table {
                           Sg => base_1+"անս" ;
                           Pl => base_1+"ուններիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ունիցս" ;
                             Pl => base_1+"ուններիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ամբս" ;
                             Pl => base_1+"ուններովս"
                           } ;
                  Loc => table {
                           Sg => base_1+"ունումս" ;
                           Pl => base_1+"ուններումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ունդ" ;
                           Pl => base_1+"ուններդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"անդ" ;
                           Pl => base_1+"ուններիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ունիցդ" ;
                             Pl => base_1+"ուններիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ամբդ" ;
                             Pl => base_1+"ուններովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+"ունումդ" ;
                           Pl => base_1+"ուններումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkN010"
  } ;

mkN011 : Str -> N ;
mkN011 base =
  case base of {
    base_1+"ե"+base_2@? => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ե"+base_2 ;
                       Pl => base_1+"ե"+base_2+"ներ"
                     } ;
              Dat => table {
                       Sg => base_1+"ո"+base_2 ;
                       Pl => base_1+"ե"+base_2+"ների"
                     } ;
              Ablat => table {
                         Sg => base_1+"ո"+base_2+"ից" ;
                         Pl => base_1+"ե"+base_2+"ներից"
                       } ;
              Instr => table {
                         Sg => base_1+"ո"+base_2+"ով" ;
                         Pl => base_1+"ե"+base_2+"ներով"
                       } ;
              Loc => table {
                       Sg => nonExist ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"ո"+base_2+"ը" ;
                    Pl => base_1+"ե"+base_2+"ներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ե"+base_2+"ը" ;
                    Pl => base_1+"ե"+base_2+"ները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ե"+base_2+"ս" ;
                           Pl => base_1+"ե"+base_2+"ներս"
                         } ;
                  Dat => table {
                           Sg => base_1+"ո"+base_2+"ս" ;
                           Pl => base_1+"ե"+base_2+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ո"+base_2+"իցս" ;
                             Pl => base_1+"ե"+base_2+"ներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ո"+base_2+"ովս" ;
                             Pl => base_1+"ե"+base_2+"ներովս"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ե"+base_2+"դ" ;
                           Pl => base_1+"ե"+base_2+"ներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"ո"+base_2+"դ" ;
                           Pl => base_1+"ե"+base_2+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ո"+base_2+"իցդ" ;
                             Pl => base_1+"ե"+base_2+"ներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ո"+base_2+"ովդ" ;
                             Pl => base_1+"ե"+base_2+"ներովդ"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkN011"
  } ;

mkN012 : Str -> N ;
mkN012 base =
  case base of {
    base_1+"ու" => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ու" ;
                       Pl => base_1+"ուներ"
                     } ;
              Dat => table {
                       Sg => base_1+"վի" ;
                       Pl => base_1+"ուների"
                     } ;
              Ablat => table {
                         Sg => base_1+"վից" ;
                         Pl => base_1+"ուներից"
                       } ;
              Instr => table {
                         Sg => base_1+"վով" ;
                         Pl => base_1+"ուներով"
                       } ;
              Loc => table {
                       Sg => base_1+"վում" ;
                       Pl => base_1+"ուներում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"վին" ;
                    Pl => base_1+"ուներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ուն" ;
                    Pl => base_1+"ուները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ուս" ;
                           Pl => base_1+"ուներս"
                         } ;
                  Dat => table {
                           Sg => base_1+"վիս" ;
                           Pl => base_1+"ուներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"վիցս" ;
                             Pl => base_1+"ուներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"վովս" ;
                             Pl => base_1+"ուներովս"
                           } ;
                  Loc => table {
                           Sg => base_1+"վումս" ;
                           Pl => base_1+"ուներումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ուդ" ;
                           Pl => base_1+"ուներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"վիդ" ;
                           Pl => base_1+"ուներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"վիցդ" ;
                             Pl => base_1+"ուներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"վովդ" ;
                             Pl => base_1+"ուներովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+"վումդ" ;
                           Pl => base_1+"ուներումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkN012"
  } ;

mkN013 : Str -> N ;
mkN013 base =
  case base of {
    base_1+"ի"+base_2@("շկ"|"ստ"|"նք"|"նձ"|"նջ"|"սպ"|?) => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ի"+base_2 ;
                       Pl => base_1+"ի"+base_2+"ներ"
                     } ;
              Dat => table {
                       Sg => base_1+base_2+"ի" ;
                       Pl => base_1+"ի"+base_2+"ների"
                     } ;
              Ablat => table {
                         Sg => base_1+base_2+"ից" ;
                         Pl => base_1+"ի"+base_2+"ներից"
                       } ;
              Instr => table {
                         Sg => base_1+base_2+"ով" ;
                         Pl => base_1+"ի"+base_2+"ներով"
                       } ;
              Loc => table {
                       Sg => base_1+base_2+"ում" ;
                       Pl => base_1+"ի"+base_2+"ներում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+base_2+"ին" ;
                    Pl => base_1+"ի"+base_2+"ներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ի"+base_2+"ը" ;
                    Pl => base_1+"ի"+base_2+"ները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ի"+base_2+"ս" ;
                           Pl => base_1+"ի"+base_2+"ներս"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"իս" ;
                           Pl => base_1+"ի"+base_2+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"իցս" ;
                             Pl => base_1+"ի"+base_2+"ներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ովս" ;
                             Pl => base_1+"ի"+base_2+"ներովս"
                           } ;
                  Loc => table {
                           Sg => base_1+base_2+"ումս" ;
                           Pl => base_1+"ի"+base_2+"ներումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ի"+base_2+"դ" ;
                           Pl => base_1+"ի"+base_2+"ներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"իդ" ;
                           Pl => base_1+"ի"+base_2+"ներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"իցդ" ;
                             Pl => base_1+"ի"+base_2+"ներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ովդ" ;
                             Pl => base_1+"ի"+base_2+"ներովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+base_2+"ումդ" ;
                           Pl => base_1+"ի"+base_2+"ներումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkN013"
  } ;

mkN014 : Str -> N ;
mkN014 base =
  case base of {
    base_1+"ի"+base_2@? => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ի"+base_2 ;
                       Pl => nonExist
                     } ;
              Dat => table {
                       Sg => base_1+base_2+"վա" ;
                       Pl => nonExist
                     } ;
              Ablat => table {
                         Sg => base_1+base_2+"վանից" ;
                         Pl => nonExist
                       } ;
              Instr => table {
                         Sg => base_1+base_2+"ով" ;
                         Pl => nonExist
                       } ;
              Loc => table {
                       Sg => base_1+base_2+"ում" ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+base_2+"վան" ;
                    Pl => nonExist
                  } ;
        def_nom = table {
                    Sg => base_1+"ի"+base_2+"ը" ;
                    Pl => nonExist
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ի"+base_2+"ս" ;
                           Pl => nonExist
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"վաս" ;
                           Pl => nonExist
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"վանիցս" ;
                             Pl => nonExist
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ովս" ;
                             Pl => nonExist
                           } ;
                  Loc => table {
                           Sg => base_1+base_2+"ումս" ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ի"+base_2+"դ" ;
                           Pl => nonExist
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"վադ" ;
                           Pl => nonExist
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"վանիցդ" ;
                             Pl => nonExist
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ովդ" ;
                             Pl => nonExist
                           } ;
                  Loc => table {
                           Sg => base_1+base_2+"ումդ" ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkN014"
  } ;

mkN015 : Str -> N ;
mkN015 base =
  case base of {
    base_1+"ի"+base_2@("նչ"|"շտ"|"րք"|"րտ"|"րգ"|"նդ"|?) => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ի"+base_2 ;
                       Pl => base_1+base_2+"եր"
                     } ;
              Dat => table {
                       Sg => base_1+base_2+"ի" ;
                       Pl => base_1+base_2+"երի"
                     } ;
              Ablat => table {
                         Sg => base_1+base_2+"ից" ;
                         Pl => base_1+base_2+"երից"
                       } ;
              Instr => table {
                         Sg => base_1+base_2+"ով" ;
                         Pl => base_1+base_2+"երով"
                       } ;
              Loc => table {
                       Sg => base_1+base_2+"ում" ;
                       Pl => base_1+base_2+"երում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+base_2+"ին" ;
                    Pl => base_1+base_2+"երին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ի"+base_2+"ը" ;
                    Pl => base_1+base_2+"երը"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ի"+base_2+"ս" ;
                           Pl => base_1+base_2+"երս"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"իս" ;
                           Pl => base_1+base_2+"երիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"իցս" ;
                             Pl => base_1+base_2+"երիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ովս" ;
                             Pl => base_1+base_2+"երովս"
                           } ;
                  Loc => table {
                           Sg => base_1+base_2+"ումս" ;
                           Pl => base_1+base_2+"երումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ի"+base_2+"դ" ;
                           Pl => base_1+base_2+"երդ"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"իդ" ;
                           Pl => base_1+base_2+"երիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"իցդ" ;
                             Pl => base_1+base_2+"երիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ովդ" ;
                             Pl => base_1+base_2+"երովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+base_2+"ումդ" ;
                           Pl => base_1+base_2+"երումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkN015"
  } ;

mkN016 : Str -> N ;
mkN016 base =
  case base of {
    base_1 => lin N
      { s = table {
              Nom => table {
                       Sg => base_1 ;
                       Pl => nonExist
                     } ;
              Dat => table {
                       Sg => base_1+"վա" ;
                       Pl => nonExist
                     } ;
              Ablat => table {
                         Sg => base_1+"վանից" ;
                         Pl => base_1+"ներից" --guessed
                       } ;
              Instr => table {
                         Sg => base_1+"ով" ;
                         Pl => nonExist
                       } ;
              Loc => table {
                       Sg => base_1+"ում" ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"վան" ;
                    Pl => nonExist
                  } ;
        def_nom = table {
                    Sg => base_1+"ը" ;
                    Pl => nonExist
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ս" ;
                           Pl => nonExist
                         } ;
                  Dat => table {
                           Sg => base_1+"վաս" ;
                           Pl => nonExist
                         } ;
                  Ablat => table {
                             Sg => base_1+"վանիցս" ;
                             Pl => base_1+"ներիցս" --guessed
                           } ;
                  Instr => table {
                             Sg => base_1+"ովս" ;
                             Pl => nonExist
                           } ;
                  Loc => table {
                           Sg => base_1+"ումս" ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"դ" ;
                           Pl => base_1+"ներդ" --guessed
                         } ;
                  Dat => table {
                           Sg => base_1+"վադ" ;
                           Pl => nonExist
                         } ;
                  Ablat => table {
                             Sg => base_1+"վանիցդ" ;
                             Pl => nonExist
                           } ;
                  Instr => table {
                             Sg => base_1+"ովդ" ;
                             Pl => nonExist
                           } ;
                  Loc => table {
                           Sg => base_1+"ումդ" ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkN016"
  } ;

mkN017 : Str -> N ;
mkN017 base =
  case base of {
    base_1 => lin N
      { s = table {
              Nom => table {
                       Sg => base_1 ;
                       Pl => base_1+"եր"
                     } ;
              Dat => table {
                       Sg => base_1+"ի" ;
                       Pl => base_1+"երի"
                     } ;
              Ablat => table {
                         Sg => base_1+"ից" ;
                         Pl => base_1+"երից"
                       } ;
              Instr => table {
                         Sg => base_1+"ով" ;
                         Pl => base_1+"երով"
                       } ;
              Loc => table {
                       Sg => base_1+"ում" ;
                       Pl => base_1+"երում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"ին" ;
                    Pl => base_1+"երին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ն" ;
                    Pl => base_1+"երը"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ս" ;
                           Pl => base_1+"երս"
                         } ;
                  Dat => table {
                           Sg => base_1+"իս" ;
                           Pl => base_1+"երիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"իցս" ;
                             Pl => base_1+"երիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովս" ;
                             Pl => base_1+"երովս"
                           } ;
                  Loc => table {
                           Sg => base_1+"ումս" ;
                           Pl => base_1+"երումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"դ" ;
                           Pl => base_1+"երդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"իդ" ;
                           Pl => base_1+"երիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"իցդ" ;
                             Pl => base_1+"երիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովդ" ;
                             Pl => base_1+"երովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+"ումդ" ;
                           Pl => base_1+"երումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkN017"
  } ;

mkN018 : Str -> N ;
mkN018 base =
  case base of {
    base_1 => lin N
      { s = table {
              Nom => table {
                       Sg => base_1 ;
                       Pl => base_1+"ներ"
                     } ;
              Dat => table {
                       Sg => base_1+"ոջ" ;
                       Pl => base_1+"ների"
                     } ;
              Ablat => table {
                         Sg => base_1+"ոջից" ;
                         Pl => base_1+"ներից"
                       } ;
              Instr => table {
                         Sg => base_1+"ոջով" ;
                         Pl => base_1+"ներով"
                       } ;
              Loc => table {
                       Sg => nonExist ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"ոջը" ;
                    Pl => base_1+"ներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ը" ;
                    Pl => base_1+"ները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ս" ;
                           Pl => base_1+"ներս"
                         } ;
                  Dat => table {
                           Sg => base_1+"ոջս" ;
                           Pl => base_1+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ոջիցս" ;
                             Pl => base_1+"ներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ոջովս" ;
                             Pl => base_1+"ներովս"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"դ" ;
                           Pl => base_1+"ներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"ոջդ" ;
                           Pl => base_1+"ներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ոջիցդ" ;
                             Pl => base_1+"ներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ոջովդ" ;
                             Pl => base_1+"ներովդ"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkN018"
  } ;

mkN019 : Str -> N ;
mkN019 base =
  case base of {
    base_1+"ձն"+base_2@?+"նուն" => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ձն"+base_2+"նուն" ;
                       Pl => base_1+"ձն"+base_2+"նուններ"
                     } ;
              Dat => table {
                       Sg => base_1+"վ"+base_2+"ն" ;
                       Pl => base_1+"ձն"+base_2+"նունների"
                     } ;
              Ablat => table {
                         Sg => base_1+"ձն"+base_2+"նունից" ;
                         Pl => base_1+"ձն"+base_2+"նուններից"
                       } ;
              Instr => table {
                         Sg => base_1+"վ"+base_2+"մբ" ;
                         Pl => base_1+"ձն"+base_2+"նուններով"
                       } ;
              Loc => table {
                       Sg => base_1+"ձն"+base_2+"նունում" ;
                       Pl => base_1+"ձն"+base_2+"նուններում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"վ"+base_2+"նը" ;
                    Pl => base_1+"ձն"+base_2+"նուններին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ձն"+base_2+"նունը" ;
                    Pl => base_1+"ձն"+base_2+"նունները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ձն"+base_2+"նունս" ;
                           Pl => base_1+"ձն"+base_2+"նուններս"
                         } ;
                  Dat => table {
                           Sg => base_1+"վ"+base_2+"նս" ;
                           Pl => base_1+"ձն"+base_2+"նուններիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ձն"+base_2+"նունիցս" ;
                             Pl => base_1+"ձն"+base_2+"նուններիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ձն"+base_2+"նունովս" ;
                             Pl => base_1+"ձն"+base_2+"նուններովս"
                           } ;
                  Loc => table {
                           Sg => base_1+"ձն"+base_2+"նունումս" ;
                           Pl => base_1+"ձն"+base_2+"նուններումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ձն"+base_2+"նունդ" ;
                           Pl => base_1+"ձն"+base_2+"նուններդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"վ"+base_2+"նդ" ;
                           Pl => base_1+"ձն"+base_2+"նուններիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ձն"+base_2+"նունիցդ" ;
                             Pl => base_1+"ձն"+base_2+"նուններիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ձն"+base_2+"նունովդ" ;
                             Pl => base_1+"ձն"+base_2+"նուններովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+"ձն"+base_2+"նունումդ" ;
                           Pl => base_1+"ձն"+base_2+"նուններումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkN019"
  } ;

mkN020 : Str -> N ;
mkN020 base =
  case base of {
    base_1+"ու"+base_2@? => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ու"+base_2 ;
                       Pl => base_1+"ու"+base_2+"ներ"
                     } ;
              Dat => table {
                       Sg => base_1+base_2+"ան" ;
                       Pl => base_1+"ու"+base_2+"ների"
                     } ;
              Ablat => table {
                         Sg => base_1+"ու"+base_2+"ից" ;
                         Pl => base_1+"ու"+base_2+"ներից"
                       } ;
              Instr => table {
                         Sg => base_1+"ու"+base_2+"ով" ;
                         Pl => base_1+"ու"+base_2+"ներով"
                       } ;
              Loc => table {
                       Sg => base_1+"ու"+base_2+"ում" ;
                       Pl => base_1+"ու"+base_2+"ներում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+base_2+"անը" ;
                    Pl => base_1+"ու"+base_2+"ներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ու"+base_2+"ը" ;
                    Pl => base_1+"ու"+base_2+"ները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ու"+base_2+"ս" ;
                           Pl => base_1+"ու"+base_2+"ներս"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"անս" ;
                           Pl => base_1+"ու"+base_2+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ու"+base_2+"իցս" ;
                             Pl => base_1+"ու"+base_2+"ներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ու"+base_2+"ովս" ;
                             Pl => base_1+"ու"+base_2+"ներովս"
                           } ;
                  Loc => table {
                           Sg => base_1+"ու"+base_2+"ումս" ;
                           Pl => base_1+"ու"+base_2+"ներումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ու"+base_2+"դ" ;
                           Pl => base_1+"ու"+base_2+"ներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"անդ" ;
                           Pl => base_1+"ու"+base_2+"ներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ու"+base_2+"իցդ" ;
                             Pl => base_1+"ու"+base_2+"ներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ու"+base_2+"ովդ" ;
                             Pl => base_1+"ու"+base_2+"ներովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+"ու"+base_2+"ումդ" ;
                           Pl => base_1+"ու"+base_2+"ներումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkN020"
  } ;

mkN021 : Str -> N ;
mkN021 base =
  case base of {
    base_1+"վա"+base_2@? => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"վա"+base_2 ;
                       Pl => base_1+"վա"+base_2+"ներ"
                     } ;
              Dat => table {
                       Sg => base_1+base_2+"ո" ;
                       Pl => base_1+"վա"+base_2+"ների"
                     } ;
              Ablat => table {
                         Sg => base_1+base_2+"ուց" ;
                         Pl => base_1+"վա"+base_2+"ներից"
                       } ;
              Instr => table {
                         Sg => base_1+base_2+"ով" ;
                         Pl => base_1+"վա"+base_2+"ներով"
                       } ;
              Loc => table {
                       Sg => nonExist ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+base_2+"ուն" ;
                    Pl => base_1+"վա"+base_2+"ներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"վա"+base_2+"ը" ;
                    Pl => base_1+"վա"+base_2+"ները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         } ;
                  Dat => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         } ;
                  Ablat => table {
                             Sg => nonExist ;
                             Pl => nonExist
                           } ;
                  Instr => table {
                             Sg => nonExist ;
                             Pl => nonExist
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         } ;
                  Dat => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         } ;
                  Ablat => table {
                             Sg => nonExist ;
                             Pl => nonExist
                           } ;
                  Instr => table {
                             Sg => nonExist ;
                             Pl => nonExist
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkN021"
  } ;

mkN022 : Str -> N ;
mkN022 base =
  case base of {
    base_1+"եւ"+base_2@(?+?+?+?)+"ի"+base_3@? => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"եւ"+base_2+"ի"+base_3 ;
                       Pl => base_1+"եւ"+base_2+"ի"+base_3+"ներ"
                     } ;
              Dat => table {
                       Sg => base_1+"և"+base_2+base_3+"ի" ;
                       Pl => base_1+"եւ"+base_2+"ի"+base_3+"ների"
                     } ;
              Ablat => table {
                         Sg => base_1+"և"+base_2+base_3+"ից" ;
                         Pl => base_1+"եւ"+base_2+"ի"+base_3+"ներից"
                       } ;
              Instr => table {
                         Sg => base_1+"և"+base_2+base_3+"ով" ;
                         Pl => base_1+"եւ"+base_2+"ի"+base_3+"ներով"
                       } ;
              Loc => table {
                       Sg => base_1+"և"+base_2+base_3+"ում" ;
                       Pl => base_1+"եւ"+base_2+"ի"+base_3+"ներում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"և"+base_2+base_3+"ին" ;
                    Pl => base_1+"եւ"+base_2+"ի"+base_3+"ներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"եւ"+base_2+"ի"+base_3+"ը" ;
                    Pl => base_1+"եւ"+base_2+"ի"+base_3+"ները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"եւ"+base_2+"ի"+base_3+"ս" ;
                           Pl => base_1+"եւ"+base_2+"ի"+base_3+"ներս"
                         } ;
                  Dat => table {
                           Sg => base_1+"և"+base_2+base_3+"իս" ;
                           Pl => base_1+"եւ"+base_2+"ի"+base_3+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"և"+base_2+base_3+"իցս" ;
                             Pl => base_1+"եւ"+base_2+"ի"+base_3+"ներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"և"+base_2+base_3+"ովս" ;
                             Pl => base_1+"եւ"+base_2+"ի"+base_3+"ներովս"
                           } ;
                  Loc => table {
                           Sg => base_1+"և"+base_2+base_3+"ումս" ;
                           Pl => base_1+"եւ"+base_2+"ի"+base_3+"ներումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"եւ"+base_2+"ի"+base_3+"դ" ;
                           Pl => base_1+"եւ"+base_2+"ի"+base_3+"ներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"և"+base_2+base_3+"իդ" ;
                           Pl => base_1+"եւ"+base_2+"ի"+base_3+"ներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"և"+base_2+base_3+"իցդ" ;
                             Pl => base_1+"եւ"+base_2+"ի"+base_3+"ներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"և"+base_2+base_3+"ովդ" ;
                             Pl => base_1+"եւ"+base_2+"ի"+base_3+"ներովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+"և"+base_2+base_3+"ումդ" ;
                           Pl => base_1+"եւ"+base_2+"ի"+base_3+"ներումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkN022"
  } ;

mkN023 : Str -> N ;
mkN023 base =
  case base of {
    base_1+"ու"+base_2@? => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ու"+base_2 ;
                       Pl => base_1+base_2+"ներ"
                     } ;
              Dat => table {
                       Sg => base_1+base_2+"ան" ;
                       Pl => base_1+base_2+"ների"
                     } ;
              Ablat => table {
                         Sg => base_1+base_2+"նից" ;
                         Pl => base_1+base_2+"ներից"
                       } ;
              Instr => table {
                         Sg => base_1+base_2+"նով" ;
                         Pl => base_1+base_2+"ներով"
                       } ;
              Loc => table {
                       Sg => nonExist ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+base_2+"անը" ;
                    Pl => base_1+base_2+"ներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ու"+base_2+"ը" ;
                    Pl => base_1+base_2+"ները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ու"+base_2+"ս" ;
                           Pl => base_1+base_2+"ներս"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"անս" ;
                           Pl => base_1+base_2+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"նիցս" ;
                             Pl => base_1+base_2+"ներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"նովս" ;
                             Pl => base_1+base_2+"ներովս"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ու"+base_2+"դ" ;
                           Pl => base_1+base_2+"ներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"անդ" ;
                           Pl => base_1+base_2+"ներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"նիցդ" ;
                             Pl => base_1+base_2+"ներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"նովդ" ;
                             Pl => base_1+base_2+"ներովդ"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkN023"
  } ;

mkN024 : Str -> N ;
mkN024 base =
  case base of {
    base_1+"ու"+base_2@(?+?)+base_3@? => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ու"+base_2+base_3 ;
                       Pl => base_1+base_2+base_3+"եր"
                     } ;
              Dat => table {
                       Sg => base_1+base_2+"ե"+base_3 ;
                       Pl => base_1+base_2+base_3+"երի"
                     } ;
              Ablat => table {
                         Sg => base_1+base_2+base_3+"ից" ;
                         Pl => base_1+base_2+base_3+"երից"
                       } ;
              Instr => table {
                         Sg => base_1+base_2+base_3+"ով" ;
                         Pl => base_1+base_2+base_3+"երով"
                       } ;
              Loc => table {
                       Sg => nonExist ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+base_2+"ե"+base_3+"ը" ;
                    Pl => base_1+base_2+base_3+"երին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ու"+base_2+base_3+"ը" ;
                    Pl => base_1+base_2+base_3+"երը"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         } ;
                  Dat => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         } ;
                  Ablat => table {
                             Sg => nonExist ;
                             Pl => nonExist
                           } ;
                  Instr => table {
                             Sg => nonExist ;
                             Pl => nonExist
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         } ;
                  Dat => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         } ;
                  Ablat => table {
                             Sg => nonExist ;
                             Pl => nonExist
                           } ;
                  Instr => table {
                             Sg => nonExist ;
                             Pl => nonExist
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkN024"
  } ;

mkN025 : Str -> N ;
mkN025 base =
  case base of {
    base_1+base_2@?+"ւյր" => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+base_2+"ւյր" ;
                       Pl => base_1+base_2+"ւյրեր"
                     } ;
              Dat => table {
                       Sg => base_1+"ր"+base_2+"ջ" ;
                       Pl => base_1+base_2+"ւյրերի"
                     } ;
              Ablat => table {
                         Sg => base_1+"ր"+base_2+"ջից" ;
                         Pl => base_1+base_2+"ւյրերից"
                       } ;
              Instr => table {
                         Sg => base_1+"ր"+base_2+"ջով" ;
                         Pl => base_1+base_2+"ւյրերով"
                       } ;
              Loc => table {
                       Sg => nonExist ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"ր"+base_2+"ջը" ;
                    Pl => base_1+base_2+"ւյրերին"
                  } ;
        def_nom = table {
                    Sg => base_1+base_2+"ւյրը" ;
                    Pl => base_1+base_2+"ւյրերը"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+base_2+"ւյրս" ;
                           Pl => base_1+base_2+"ւյրերս"
                         } ;
                  Dat => table {
                           Sg => base_1+"ր"+base_2+"ջս" ;
                           Pl => base_1+base_2+"ւյրերիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ր"+base_2+"ջիցս" ;
                             Pl => base_1+base_2+"ւյրերիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ր"+base_2+"ջովս" ;
                             Pl => base_1+base_2+"ւյրերովս"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+base_2+"ւյրդ" ;
                           Pl => base_1+base_2+"ւյրերդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"ր"+base_2+"ջդ" ;
                           Pl => base_1+base_2+"ւյրերիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ր"+base_2+"ջիցդ" ;
                             Pl => base_1+base_2+"ւյրերիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ր"+base_2+"ջովդ" ;
                             Pl => base_1+base_2+"ւյրերովդ"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
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
              Nom => table {
                       Sg => base_1 ;
                       Pl => nonExist
                     } ;
              Dat => table {
                       Sg => base_1+"ան" ;
                       Pl => nonExist
                     } ;
              Ablat => table {
                         Sg => base_1+"նից" ;
                         Pl => base_1+"ներից" --guessed
                       } ;
              Instr => table {
                         Sg => base_1+"նով" ;
                         Pl => nonExist
                       } ;
              Loc => table {
                       Sg => nonExist ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"անը" ;
                    Pl => nonExist
                  } ;
        def_nom = table {
                    Sg => base_1+"ը" ;
                    Pl => nonExist
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ս" ;
                           Pl => nonExist
                         } ;
                  Dat => table {
                           Sg => base_1+"անս" ;
                           Pl => nonExist
                         } ;
                  Ablat => table {
                             Sg => base_1+"նիցս" ;
                             Pl => base_1+"ներիցս" --guessed
                           } ;
                  Instr => table {
                             Sg => base_1+"նովս" ;
                             Pl => nonExist
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"դ" ;
                           Pl => base_1+"ներդ" --guessed
                         } ;
                  Dat => table {
                           Sg => base_1+"անդ" ;
                           Pl => nonExist
                         } ;
                  Ablat => table {
                             Sg => base_1+"նիցդ" ;
                             Pl => nonExist
                           } ;
                  Instr => table {
                             Sg => base_1+"նովդ" ;
                             Pl => nonExist
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
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
              Nom => table {
                       Sg => base_1 ;
                       Pl => base_1+"եր"
                     } ;
              Dat => table {
                       Sg => base_1+"ու" ;
                       Pl => base_1+"երի"
                     } ;
              Ablat => table {
                         Sg => base_1+"ուց" ;
                         Pl => base_1+"երից"
                       } ;
              Instr => table {
                         Sg => base_1+"ով" ;
                         Pl => base_1+"երով"
                       } ;
              Loc => table {
                       Sg => nonExist ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"ուն" ;
                    Pl => base_1+"երին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ն" ;
                    Pl => base_1+"երը"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ս" ;
                           Pl => base_1+"երս"
                         } ;
                  Dat => table {
                           Sg => base_1+"ուս" ;
                           Pl => base_1+"երիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ուցս" ;
                             Pl => base_1+"երիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովս" ;
                             Pl => base_1+"երովս"
                           } ;
                  Loc => table {
                           Sg => base_1+"ումս" ;
                           Pl => base_1+"երումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"դ" ;
                           Pl => base_1+"երդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"ուդ" ;
                           Pl => base_1+"երիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ուցդ" ;
                             Pl => base_1+"երիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովդ" ;
                             Pl => base_1+"երովդ"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkN027"
  } ;

mkN028 : Str -> N ;
mkN028 base =
  case base of {
    base_1+"ու"+base_2@? => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ու"+base_2 ;
                       Pl => base_1+base_2+"եր"
                     } ;
              Dat => table {
                       Sg => base_1+"ա"+base_2 ;
                       Pl => base_1+base_2+"երի"
                     } ;
              Ablat => table {
                         Sg => base_1+base_2+"ից" ;
                         Pl => base_1+base_2+"երից"
                       } ;
              Instr => table {
                         Sg => base_1+base_2+"ով" ;
                         Pl => base_1+base_2+"երով"
                       } ;
              Loc => table {
                       Sg => base_1+base_2+"ում" ;
                       Pl => base_1+base_2+"երում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"ա"+base_2+"ը" ;
                    Pl => base_1+base_2+"երին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ու"+base_2+"ը" ;
                    Pl => base_1+base_2+"երը"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ու"+base_2+"ս" ;
                           Pl => base_1+base_2+"երս"
                         } ;
                  Dat => table {
                           Sg => base_1+"ա"+base_2+"ս" ;
                           Pl => base_1+base_2+"երիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"իցս" ;
                             Pl => base_1+base_2+"երիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ովս" ;
                             Pl => base_1+base_2+"երովս"
                           } ;
                  Loc => table {
                           Sg => base_1+base_2+"ումս" ;
                           Pl => base_1+base_2+"երումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ու"+base_2+"դ" ;
                           Pl => base_1+base_2+"երդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"ա"+base_2+"դ" ;
                           Pl => base_1+base_2+"երիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"իցդ" ;
                             Pl => base_1+base_2+"երիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ովդ" ;
                             Pl => base_1+base_2+"երովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+base_2+"ումդ" ;
                           Pl => base_1+base_2+"երումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkN028"
  } ;

mkN029 : Str -> N ;
mkN029 base =
  case base of {
    base_1+"եւու"+base_2@? => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"եւու"+base_2 ;
                       Pl => base_1+"եւու"+base_2+"ներ"
                     } ;
              Dat => table {
                       Sg => base_1+"և"+base_2+"ան" ;
                       Pl => base_1+"եւու"+base_2+"ների"
                     } ;
              Ablat => table {
                         Sg => base_1+"եւու"+base_2+"ից" ;
                         Pl => base_1+"եւու"+base_2+"ներից"
                       } ;
              Instr => table {
                         Sg => base_1+"և"+base_2+"ամբ" ;
                         Pl => base_1+"եւու"+base_2+"ներով"
                       } ;
              Loc => table {
                       Sg => base_1+"եւու"+base_2+"ում" ;
                       Pl => base_1+"եւու"+base_2+"ներում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"և"+base_2+"անը" ;
                    Pl => base_1+"եւու"+base_2+"ներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"եւու"+base_2+"ը" ;
                    Pl => base_1+"եւու"+base_2+"ները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"եւու"+base_2+"ս" ;
                           Pl => base_1+"եւու"+base_2+"ներս"
                         } ;
                  Dat => table {
                           Sg => base_1+"և"+base_2+"անս" ;
                           Pl => base_1+"եւու"+base_2+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"եւու"+base_2+"իցս" ;
                             Pl => base_1+"եւու"+base_2+"ներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"եւու"+base_2+"ովս" ;
                             Pl => base_1+"եւու"+base_2+"ներովս"
                           } ;
                  Loc => table {
                           Sg => base_1+"եւու"+base_2+"ումս" ;
                           Pl => base_1+"եւու"+base_2+"ներումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"եւու"+base_2+"դ" ;
                           Pl => base_1+"եւու"+base_2+"ներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"և"+base_2+"անդ" ;
                           Pl => base_1+"եւու"+base_2+"ներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"եւու"+base_2+"իցդ" ;
                             Pl => base_1+"եւու"+base_2+"ներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"եւու"+base_2+"ովդ" ;
                             Pl => base_1+"եւու"+base_2+"ներովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+"եւու"+base_2+"ումդ" ;
                           Pl => base_1+"եւու"+base_2+"ներումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkN029"
  } ;

mkN030 : Str -> N ;
mkN030 base =
  case base of {
    base_1+"ու"+base_2@(?+?) => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ու"+base_2 ;
                       Pl => base_1+"ու"+base_2+"ներ"
                     } ;
              Dat => table {
                       Sg => base_1+base_2+"յան" ;
                       Pl => base_1+"ու"+base_2+"ների"
                     } ;
              Ablat => table {
                         Sg => base_1+base_2+"ից" ;
                         Pl => base_1+"ու"+base_2+"ներից"
                       } ;
              Instr => table {
                         Sg => base_1+base_2+"յամբ" ;
                         Pl => base_1+"ու"+base_2+"ներով"
                       } ;
              Loc => table {
                       Sg => base_1+base_2+"ում" ;
                       Pl => base_1+"ու"+base_2+"ներում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+base_2+"յանը" ;
                    Pl => base_1+"ու"+base_2+"ներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ու"+base_2+"ը" ;
                    Pl => base_1+"ու"+base_2+"ները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ու"+base_2+"ս" ;
                           Pl => base_1+"ու"+base_2+"ներս"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"յանս" ;
                           Pl => base_1+"ու"+base_2+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"իցս" ;
                             Pl => base_1+"ու"+base_2+"ներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ովս" ;
                             Pl => base_1+"ու"+base_2+"ներովս"
                           } ;
                  Loc => table {
                           Sg => base_1+base_2+"ումս" ;
                           Pl => base_1+"ու"+base_2+"ներումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ու"+base_2+"դ" ;
                           Pl => base_1+"ու"+base_2+"ներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"յանդ" ;
                           Pl => base_1+"ու"+base_2+"ներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"իցդ" ;
                             Pl => base_1+"ու"+base_2+"ներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ովդ" ;
                             Pl => base_1+"ու"+base_2+"ներովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+base_2+"ումդ" ;
                           Pl => base_1+"ու"+base_2+"ներումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkN030"
  } ;

mkN031 : Str -> N ;
mkN031 base =
  case base of {
    base_1+"ու"+base_2@("շտ"|"րդ"|?) => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ու"+base_2 ;
                       Pl => base_1+"ու"+base_2+"ներ"
                     } ;
              Dat => table {
                       Sg => base_1+base_2+"ի" ;
                       Pl => base_1+"ու"+base_2+"ների"
                     } ;
              Ablat => table {
                         Sg => base_1+base_2+"ից" ;
                         Pl => base_1+"ու"+base_2+"ներից"
                       } ;
              Instr => table {
                         Sg => base_1+base_2+"ով" ;
                         Pl => base_1+"ու"+base_2+"ներով"
                       } ;
              Loc => table {
                       Sg => base_1+base_2+"ում" ;
                       Pl => base_1+"ու"+base_2+"ներում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+base_2+"ին" ;
                    Pl => base_1+"ու"+base_2+"ներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ու"+base_2+"ը" ;
                    Pl => base_1+"ու"+base_2+"ները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ու"+base_2+"ս" ;
                           Pl => base_1+"ու"+base_2+"ներս"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"իս" ;
                           Pl => base_1+"ու"+base_2+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"իցս" ;
                             Pl => base_1+"ու"+base_2+"ներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ովս" ;
                             Pl => base_1+"ու"+base_2+"ներովս"
                           } ;
                  Loc => table {
                           Sg => base_1+base_2+"ումս" ;
                           Pl => base_1+"ու"+base_2+"ներումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ու"+base_2+"դ" ;
                           Pl => base_1+"ու"+base_2+"ներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"իդ" ;
                           Pl => base_1+"ու"+base_2+"ներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"իցդ" ;
                             Pl => base_1+"ու"+base_2+"ներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ովդ" ;
                             Pl => base_1+"ու"+base_2+"ներովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+base_2+"ումդ" ;
                           Pl => base_1+"ու"+base_2+"ներումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkN031"
  } ;

mkN032 : Str -> N ;
mkN032 base =
  case base of {
    base_1+"ուն" => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ուն" ;
                       Pl => base_1+"ուններ"
                     } ;
              Dat => table {
                       Sg => base_1+"վա" ;
                       Pl => base_1+"ունների"
                     } ;
              Ablat => table {
                         Sg => base_1+"վանից" ;
                         Pl => base_1+"ուններից"
                       } ;
              Instr => table {
                         Sg => base_1+"ունով" ;
                         Pl => base_1+"ուններով"
                       } ;
              Loc => table {
                       Sg => nonExist ;
                       Pl => base_1+"ուններում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"վան" ;
                    Pl => base_1+"ուններին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ունը" ;
                    Pl => base_1+"ունները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ունս" ;
                           Pl => base_1+"ուններս"
                         } ;
                  Dat => table {
                           Sg => base_1+"վաս" ;
                           Pl => base_1+"ուններիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"վանիցս" ;
                             Pl => base_1+"ուններիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ունովս" ;
                             Pl => base_1+"ուններովս"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => base_1+"ուններումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ունդ" ;
                           Pl => base_1+"ուններդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"վադ" ;
                           Pl => base_1+"ուններիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"վանիցդ" ;
                             Pl => base_1+"ուններիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ունովդ" ;
                             Pl => base_1+"ուններովդ"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => base_1+"ուններումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkN032"
  } ;

mkN033 : Str -> N ;
mkN033 base =
  case base of {
    base_1+"ու" => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ու" ;
                       Pl => base_1+"վեր"
                     } ;
              Dat => table {
                       Sg => base_1+"վի" ;
                       Pl => base_1+"վերի"
                     } ;
              Ablat => table {
                         Sg => base_1+"վից" ;
                         Pl => base_1+"վերից"
                       } ;
              Instr => table {
                         Sg => base_1+"վով" ;
                         Pl => base_1+"վերով"
                       } ;
              Loc => table {
                       Sg => nonExist ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"վին" ;
                    Pl => base_1+"վերին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ուն" ;
                    Pl => base_1+"վերը"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ուս" ;
                           Pl => base_1+"վերս"
                         } ;
                  Dat => table {
                           Sg => base_1+"վիս" ;
                           Pl => base_1+"վերիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"վիցս" ;
                             Pl => base_1+"վերիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"վովս" ;
                             Pl => base_1+"վերովս"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ուդ" ;
                           Pl => base_1+"վերդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"վիդ" ;
                           Pl => base_1+"վերիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"վիցդ" ;
                             Pl => base_1+"վերիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"վովդ" ;
                             Pl => base_1+"վերովդ"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkN033"
  } ;

mkN034 : Str -> N ;
mkN034 base =
  case base of {
    base_1+"ի"+base_2@? => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ի"+base_2 ;
                       Pl => base_1+"ա"+base_2+"այք"
                     } ;
              Dat => table {
                       Sg => base_1+base_2+"ոջ" ;
                       Pl => base_1+"ա"+base_2+"անց"
                     } ;
              Ablat => table {
                         Sg => base_1+base_2+"ոջից" ;
                         Pl => base_1+"ա"+base_2+"անցից"
                       } ;
              Instr => table {
                         Sg => base_1+base_2+"ոջով" ;
                         Pl => base_1+"ա"+base_2+"անցով"
                       } ;
              Loc => table {
                       Sg => nonExist ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+base_2+"ոջը" ;
                    Pl => nonExist
                  } ;
        def_nom = table {
                    Sg => base_1+"ի"+base_2+"ը" ;
                    Pl => nonExist
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ի"+base_2+"ս" ;
                           Pl => base_1+"ա"+base_2+"այքս"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"ոջս" ;
                           Pl => base_1+"ա"+base_2+"անցս"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"ոջիցս" ;
                             Pl => base_1+"ա"+base_2+"անցիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ոջովս" ;
                             Pl => base_1+"ա"+base_2+"անցովս"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ի"+base_2+"դ" ;
                           Pl => base_1+"ա"+base_2+"այքդ"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"ոջդ" ;
                           Pl => base_1+"ա"+base_2+"անցդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"ոջիցդ" ;
                             Pl => base_1+"ա"+base_2+"անցիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ոջովդ" ;
                             Pl => base_1+"ա"+base_2+"անցովդ"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkN034"
  } ;

mkN035 : Str -> N ;
mkN035 base =
  case base of {
    base_1+"ի"+base_2@? => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ի"+base_2 ;
                       Pl => base_1+"ի"+base_2+"ներ"
                     } ;
              Dat => table {
                       Sg => base_1+base_2+"ա" ;
                       Pl => base_1+"ի"+base_2+"ների"
                     } ;
              Ablat => table {
                         Sg => base_1+base_2+"անից" ;
                         Pl => base_1+"ի"+base_2+"ներից"
                       } ;
              Instr => table {
                         Sg => base_1+base_2+"անով" ;
                         Pl => base_1+"ի"+base_2+"ներով"
                       } ;
              Loc => table {
                       Sg => nonExist ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+base_2+"ան" ;
                    Pl => base_1+"ի"+base_2+"ներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ի"+base_2+"ը" ;
                    Pl => base_1+"ի"+base_2+"ները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ի"+base_2+"ս" ;
                           Pl => base_1+"ի"+base_2+"ներս"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"անս" ;
                           Pl => base_1+"ի"+base_2+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"անիցս" ;
                             Pl => base_1+"ի"+base_2+"ներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"անովս" ;
                             Pl => base_1+"ի"+base_2+"ներովս"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ի"+base_2+"դ" ;
                           Pl => base_1+"ի"+base_2+"ներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"անդ" ;
                           Pl => base_1+"ի"+base_2+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"անիցդ" ;
                             Pl => base_1+"ի"+base_2+"ներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"անովդ" ;
                             Pl => base_1+"ի"+base_2+"ներովդ"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkN035"
  } ;

mkN036 : Str -> N ;
mkN036 base =
  case base of {
    base_1+"այ"+base_2@? => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"այ"+base_2 ;
                       Pl => base_1+"այ"+base_2+"եր"
                     } ;
              Dat => table {
                       Sg => base_1+"ո"+base_2 ;
                       Pl => base_1+"այ"+base_2+"երի"
                     } ;
              Ablat => table {
                         Sg => base_1+"ո"+base_2+"ից" ;
                         Pl => base_1+"այ"+base_2+"երից"
                       } ;
              Instr => table {
                         Sg => base_1+"ո"+base_2+"ով" ;
                         Pl => base_1+"այ"+base_2+"երով"
                       } ;
              Loc => table {
                       Sg => nonExist ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"ո"+base_2+"ը" ;
                    Pl => base_1+"այ"+base_2+"երին"
                  } ;
        def_nom = table {
                    Sg => base_1+"այ"+base_2+"ը" ;
                    Pl => base_1+"այ"+base_2+"երը"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"այ"+base_2+"ս" ;
                           Pl => base_1+"այ"+base_2+"երս"
                         } ;
                  Dat => table {
                           Sg => base_1+"ո"+base_2+"ս" ;
                           Pl => base_1+"այ"+base_2+"երիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ո"+base_2+"իցս" ;
                             Pl => base_1+"այ"+base_2+"երիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ո"+base_2+"ովս" ;
                             Pl => base_1+"այ"+base_2+"երովս"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"այ"+base_2+"դ" ;
                           Pl => base_1+"այ"+base_2+"երդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"ո"+base_2+"դ" ;
                           Pl => base_1+"այ"+base_2+"երիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ո"+base_2+"իցդ" ;
                             Pl => base_1+"այ"+base_2+"երիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ո"+base_2+"ովդ" ;
                             Pl => base_1+"այ"+base_2+"երովդ"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkN036"
  } ;

mkN037 : Str -> N ;
mkN037 base =
  case base of {
    base_1+"ե"+base_2@? => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ե"+base_2 ;
                       Pl => base_1+"ե"+base_2+"եր"
                     } ;
              Dat => table {
                       Sg => base_1+"ո"+base_2 ;
                       Pl => base_1+"ե"+base_2+"երի"
                     } ;
              Ablat => table {
                         Sg => base_1+"ո"+base_2+"ից" ;
                         Pl => base_1+"ե"+base_2+"երից"
                       } ;
              Instr => table {
                         Sg => base_1+"ո"+base_2+"ով" ;
                         Pl => base_1+"ե"+base_2+"երով"
                       } ;
              Loc => table {
                       Sg => nonExist ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"ո"+base_2+"ը" ;
                    Pl => base_1+"ե"+base_2+"երին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ե"+base_2+"ը" ;
                    Pl => base_1+"ե"+base_2+"երը"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ե"+base_2+"ս" ;
                           Pl => base_1+"ե"+base_2+"երս"
                         } ;
                  Dat => table {
                           Sg => base_1+"ո"+base_2+"ս" ;
                           Pl => base_1+"ե"+base_2+"երիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ո"+base_2+"իցս" ;
                             Pl => base_1+"ե"+base_2+"երիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ո"+base_2+"ովս" ;
                             Pl => base_1+"ե"+base_2+"երովս"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ե"+base_2+"դ" ;
                           Pl => base_1+"ե"+base_2+"երդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"ո"+base_2+"դ" ;
                           Pl => base_1+"ե"+base_2+"երիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ո"+base_2+"իցդ" ;
                             Pl => base_1+"ե"+base_2+"երիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ո"+base_2+"ովդ" ;
                             Pl => base_1+"ե"+base_2+"երովդ"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkN037"
  } ;

mkN038 : Str -> N ;
mkN038 base =
  case base of {
    base_1+"ու" => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ու" ;
                       Pl => nonExist
                     } ;
              Dat => table {
                       Sg => base_1+"վա" ;
                       Pl => nonExist
                     } ;
              Ablat => table {
                         Sg => base_1+"վանից" ;
                         Pl => nonExist
                       } ;
              Instr => table {
                         Sg => base_1+"ով" ;
                         Pl => nonExist
                       } ;
              Loc => table {
                       Sg => base_1+"ում" ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"վան" ;
                    Pl => nonExist
                  } ;
        def_nom = table {
                    Sg => base_1+"ուն" ;
                    Pl => nonExist
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ուս" ;
                           Pl => nonExist
                         } ;
                  Dat => table {
                           Sg => base_1+"վաս" ;
                           Pl => nonExist
                         } ;
                  Ablat => table {
                             Sg => base_1+"վանիցս" ;
                             Pl => nonExist
                           } ;
                  Instr => table {
                             Sg => base_1+"ովս" ;
                             Pl => nonExist
                           } ;
                  Loc => table {
                           Sg => base_1+"ումս" ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ուդ" ;
                           Pl => nonExist
                         } ;
                  Dat => table {
                           Sg => base_1+"վադ" ;
                           Pl => nonExist
                         } ;
                  Ablat => table {
                             Sg => base_1+"վանիցդ" ;
                             Pl => nonExist
                           } ;
                  Instr => table {
                             Sg => base_1+"ովդ" ;
                             Pl => nonExist
                           } ;
                  Loc => table {
                           Sg => base_1+"ումդ" ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkN038"
  } ;

mkN039 : Str -> N ;
mkN039 base =
  case base of {
    base_1+base_2@?+"ւյր" => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+base_2+"ւյր" ;
                       Pl => base_1+base_2+"ւյրներ"
                     } ;
              Dat => table {
                       Sg => base_1+"ր"+base_2+"ջ" ;
                       Pl => base_1+base_2+"ւյրների"
                     } ;
              Ablat => table {
                         Sg => base_1+"ր"+base_2+"ջից" ;
                         Pl => base_1+base_2+"ւյրներից"
                       } ;
              Instr => table {
                         Sg => base_1+"ր"+base_2+"ջով" ;
                         Pl => base_1+base_2+"ւյրներով"
                       } ;
              Loc => table {
                       Sg => nonExist ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"ր"+base_2+"ջը" ;
                    Pl => base_1+base_2+"ւյրներին"
                  } ;
        def_nom = table {
                    Sg => base_1+base_2+"ւյրը" ;
                    Pl => base_1+base_2+"ւյրները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+base_2+"ւյրս" ;
                           Pl => base_1+base_2+"ւյրներս"
                         } ;
                  Dat => table {
                           Sg => base_1+"ր"+base_2+"ջս" ;
                           Pl => base_1+base_2+"ւյրներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ր"+base_2+"ջիցս" ;
                             Pl => base_1+base_2+"ւյրներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ր"+base_2+"ջովս" ;
                             Pl => base_1+base_2+"ւյրներովս"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+base_2+"ւյրդ" ;
                           Pl => base_1+base_2+"ւյրներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"ր"+base_2+"ջդ" ;
                           Pl => base_1+base_2+"ւյրներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ր"+base_2+"ջիցդ" ;
                             Pl => base_1+base_2+"ւյրներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ր"+base_2+"ջովդ" ;
                             Pl => base_1+base_2+"ւյրներովդ"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkN039"
  } ;

mkN040 : Str -> N ;
mkN040 base =
  case base of {
    base_1+"այ"+base_2@? => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"այ"+base_2 ;
                       Pl => base_1+"այ"+base_2+"ներ"
                     } ;
              Dat => table {
                       Sg => base_1+"ո"+base_2 ;
                       Pl => base_1+"այ"+base_2+"ների"
                     } ;
              Ablat => table {
                         Sg => base_1+"ո"+base_2+"ից" ;
                         Pl => base_1+"այ"+base_2+"ներից"
                       } ;
              Instr => table {
                         Sg => base_1+"ո"+base_2+"ով" ;
                         Pl => base_1+"այ"+base_2+"ներով"
                       } ;
              Loc => table {
                       Sg => nonExist ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"ո"+base_2+"ը" ;
                    Pl => base_1+"այ"+base_2+"ներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"այ"+base_2+"ը" ;
                    Pl => base_1+"այ"+base_2+"ները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"այ"+base_2+"ս" ;
                           Pl => base_1+"այ"+base_2+"ներս"
                         } ;
                  Dat => table {
                           Sg => base_1+"ո"+base_2+"ս" ;
                           Pl => base_1+"այ"+base_2+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ո"+base_2+"իցս" ;
                             Pl => base_1+"այ"+base_2+"ներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ո"+base_2+"ովս" ;
                             Pl => base_1+"այ"+base_2+"ներովս"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"այ"+base_2+"դ" ;
                           Pl => base_1+"այ"+base_2+"ներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"ո"+base_2+"դ" ;
                           Pl => base_1+"այ"+base_2+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ո"+base_2+"իցդ" ;
                             Pl => base_1+"այ"+base_2+"ներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ո"+base_2+"ովդ" ;
                             Pl => base_1+"այ"+base_2+"ներովդ"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkN040"
  } ;

mkN041 : Str -> N ;
mkN041 base =
  case base of {
    base_1+"ե"+base_2@? => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ե"+base_2 ;
                       Pl => nonExist
                     } ;
              Dat => table {
                       Sg => base_1+"ի"+base_2+"ի" ;
                       Pl => nonExist
                     } ;
              Ablat => table {
                         Sg => base_1+"ի"+base_2+"ից" ;
                         Pl => nonExist
                       } ;
              Instr => table {
                         Sg => base_1+"ի"+base_2+"ով" ;
                         Pl => nonExist
                       } ;
              Loc => table {
                       Sg => base_1+"ի"+base_2+"ում" ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"ի"+base_2+"ին" ;
                    Pl => nonExist
                  } ;
        def_nom = table {
                    Sg => base_1+"ե"+base_2+"ը" ;
                    Pl => nonExist
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ե"+base_2+"ս" ;
                           Pl => nonExist
                         } ;
                  Dat => table {
                           Sg => base_1+"ի"+base_2+"իս" ;
                           Pl => nonExist
                         } ;
                  Ablat => table {
                             Sg => base_1+"ի"+base_2+"իցս" ;
                             Pl => nonExist
                           } ;
                  Instr => table {
                             Sg => base_1+"ի"+base_2+"ովս" ;
                             Pl => nonExist
                           } ;
                  Loc => table {
                           Sg => base_1+"ի"+base_2+"ումս" ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ե"+base_2+"դ" ;
                           Pl => nonExist
                         } ;
                  Dat => table {
                           Sg => base_1+"ի"+base_2+"իդ" ;
                           Pl => nonExist
                         } ;
                  Ablat => table {
                             Sg => base_1+"ի"+base_2+"իցդ" ;
                             Pl => nonExist
                           } ;
                  Instr => table {
                             Sg => base_1+"ի"+base_2+"ովդ" ;
                             Pl => nonExist
                           } ;
                  Loc => table {
                           Sg => base_1+"ի"+base_2+"ումդ" ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkN041"
  } ;

mkN042 : Str -> N ;
mkN042 base =
  case base of {
    base_1+"ի" => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ի" ;
                       Pl => base_1+"իներ"
                     } ;
              Dat => table {
                       Sg => base_1+"ու" ;
                       Pl => base_1+"իների"
                     } ;
              Ablat => table {
                         Sg => base_1+"ուց" ;
                         Pl => base_1+"իներից"
                       } ;
              Instr => table {
                         Sg => base_1+"ով" ;
                         Pl => base_1+"իներով"
                       } ;
              Loc => table {
                       Sg => nonExist ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"ուն" ;
                    Pl => base_1+"իներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ին" ;
                    Pl => base_1+"իները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"իս" ;
                           Pl => base_1+"իներս"
                         } ;
                  Dat => table {
                           Sg => base_1+"ուս" ;
                           Pl => base_1+"ունս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ուցս" ;
                             Pl => base_1+"իներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովս" ;
                             Pl => base_1+"իներովս"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"իդ" ;
                           Pl => base_1+"իներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"ուդ" ;
                           Pl => base_1+"իներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ուցդ" ;
                             Pl => base_1+"իներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովդ" ;
                             Pl => base_1+"իներովդ"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkN042"
  } ;

mkN043 : Str -> N ;
mkN043 base =
  case base of {
    base_1+"ե"+base_2@? => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ե"+base_2 ;
                       Pl => base_1+"ե"+base_2+"եր"
                     } ;
              Dat => table {
                       Sg => base_1+"ի"+base_2+"ոջ" ;
                       Pl => base_1+"ե"+base_2+"երի"
                     } ;
              Ablat => table {
                         Sg => base_1+"ի"+base_2+"ոջից" ;
                         Pl => base_1+"ե"+base_2+"երից"
                       } ;
              Instr => table {
                         Sg => base_1+"ի"+base_2+"ոջով" ;
                         Pl => base_1+"ե"+base_2+"երով"
                       } ;
              Loc => table {
                       Sg => nonExist ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"ի"+base_2+"ոջը" ;
                    Pl => base_1+"ե"+base_2+"երին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ե"+base_2+"ը" ;
                    Pl => base_1+"ե"+base_2+"երը"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ե"+base_2+"ս" ;
                           Pl => base_1+"ե"+base_2+"երս"
                         } ;
                  Dat => table {
                           Sg => base_1+"ի"+base_2+"ոջս" ;
                           Pl => base_1+"ե"+base_2+"երիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ի"+base_2+"ոջիցս" ;
                             Pl => base_1+"ե"+base_2+"երիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ի"+base_2+"ոջովս" ;
                             Pl => base_1+"ե"+base_2+"երովս"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ե"+base_2+"դ" ;
                           Pl => base_1+"ե"+base_2+"երդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"ի"+base_2+"ոջդ" ;
                           Pl => base_1+"ե"+base_2+"երիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ի"+base_2+"ոջիցդ" ;
                             Pl => base_1+"ե"+base_2+"երիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ի"+base_2+"ոջովդ" ;
                             Pl => base_1+"ե"+base_2+"երովդ"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkN043"
  } ;

mkN044 : Str -> N ;
mkN044 base =
  case base of {
    base_1+"ու"+base_2@? => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ու"+base_2 ;
                       Pl => nonExist
                     } ;
              Dat => table {
                       Sg => base_1+base_2+"ոջ" ;
                       Pl => nonExist
                     } ;
              Ablat => table {
                         Sg => base_1+base_2+"ոջից" ;
                         Pl => nonExist
                       } ;
              Instr => table {
                         Sg => base_1+base_2+"ոջով" ;
                         Pl => nonExist
                       } ;
              Loc => table {
                       Sg => nonExist ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+base_2+"ոջը" ;
                    Pl => nonExist
                  } ;
        def_nom = table {
                    Sg => base_1+"ու"+base_2+"ը" ;
                    Pl => nonExist
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ու"+base_2+"ս" ;
                           Pl => nonExist
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"ոջս" ;
                           Pl => nonExist
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"ոջիցս" ;
                             Pl => nonExist
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ոջովս" ;
                             Pl => nonExist
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ու"+base_2+"դ" ;
                           Pl => nonExist
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"ոջդ" ;
                           Pl => nonExist
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"ոջիցդ" ;
                             Pl => nonExist
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ոջովդ" ;
                             Pl => nonExist
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkN044"
  } ;

mkN045 : Str -> N ;
mkN045 base =
  case base of {
    base_1+"ը" => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ը" ;
                       Pl => nonExist
                     } ;
              Dat => table {
                       Sg => base_1+"վա" ;
                       Pl => nonExist
                     } ;
              Ablat => table {
                         Sg => base_1+"վանից" ;
                         Pl => nonExist
                       } ;
              Instr => table {
                         Sg => base_1+"ով" ;
                         Pl => nonExist
                       } ;
              Loc => table {
                       Sg => base_1+"ում" ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"վան" ;
                    Pl => nonExist
                  } ;
        def_nom = table {
                    Sg => base_1+"ը" ;
                    Pl => nonExist
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ըս" ;
                           Pl => nonExist
                         } ;
                  Dat => table {
                           Sg => base_1+"վաս" ;
                           Pl => nonExist
                         } ;
                  Ablat => table {
                             Sg => base_1+"վանիցս" ;
                             Pl => nonExist
                           } ;
                  Instr => table {
                             Sg => base_1+"ովս" ;
                             Pl => nonExist
                           } ;
                  Loc => table {
                           Sg => base_1+"ումս" ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ըդ" ;
                           Pl => nonExist
                         } ;
                  Dat => table {
                           Sg => base_1+"վադ" ;
                           Pl => nonExist
                         } ;
                  Ablat => table {
                             Sg => base_1+"վանիցդ" ;
                             Pl => nonExist
                           } ;
                  Instr => table {
                             Sg => base_1+"ովդ" ;
                             Pl => nonExist
                           } ;
                  Loc => table {
                           Sg => base_1+"ումդ" ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkN045"
  } ;

mkN046 : Str -> N ;
mkN046 base =
  case base of {
    base_1+"ի"+base_2@? => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ի"+base_2 ;
                       Pl => base_1+"ա"+base_2+"այք" --guessed
                     } ;
              Dat => table {
                       Sg => base_1+base_2+"ոջ" ;
                       Pl => base_1+"ա"+base_2+"անց" --guessed
                     } ;
              Ablat => table {
                         Sg => base_1+base_2+"ոջից" ;
                         Pl => base_1+"ա"+base_2+"անցից" --guessed
                       } ;
              Instr => table {
                         Sg => base_1+base_2+"ոջով" ;
                         Pl => base_1+"ա"+base_2+"անցով" --guessed
                       } ;
              Loc => table {
                       Sg => nonExist ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+base_2+"ոջը" ;
                    Pl => base_1+"ի"+base_2+"ներին" --guessed
                  } ;
        def_nom = table {
                    Sg => base_1+"ի"+base_2+"ը" ;
                    Pl => base_1+"ի"+base_2+"ները" --guessed
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ի"+base_2+"ս" ;
                           Pl => base_1+"ա"+base_2+"այքս" --guessed
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"ոջս" ;
                           Pl => base_1+"ա"+base_2+"անցս" --guessed
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"ոջիցս" ;
                             Pl => base_1+"ա"+base_2+"անցիցս" --guessed
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ոջովս" ;
                             Pl => base_1+"ա"+base_2+"անցովս" --guessed
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ի"+base_2+"դ" ;
                           Pl => base_1+"ա"+base_2+"այքդ" --guessed
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"ոջդ" ;
                           Pl => base_1+"ա"+base_2+"անցդ" --guessed
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"ոջիցդ" ;
                             Pl => base_1+"ա"+base_2+"անցիցդ" --guessed
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ոջովդ" ;
                             Pl => base_1+"ա"+base_2+"անցովդ" --guessed
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkN046"
  } ;

mkN047 : Str -> N ;
mkN047 base =
  case base of {
    base_1 => lin N
      { s = table {
              Nom => table {
                       Sg => base_1 ;
                       Pl => base_1+"իկ"
                     } ;
              Dat => table {
                       Sg => base_1+"ու" ;
                       Pl => base_1+"կանց"
                     } ;
              Ablat => table {
                         Sg => base_1+"ուց" ;
                         Pl => base_1+"կանցից"
                       } ;
              Instr => table {
                         Sg => base_1+"ով" ;
                         Pl => base_1+"կանցով"
                       } ;
              Loc => table {
                       Sg => nonExist ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"ուն" ;
                    Pl => nonExist
                  } ;
        def_nom = table {
                    Sg => base_1+"ը" ;
                    Pl => nonExist
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ս" ;
                           Pl => base_1+"իկս"
                         } ;
                  Dat => table {
                           Sg => base_1+"ուս" ;
                           Pl => base_1+"կանցս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ուցս" ;
                             Pl => base_1+"կանցիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովս" ;
                             Pl => base_1+"կանցովս"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"դ" ;
                           Pl => base_1+"իկդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"ուդ" ;
                           Pl => base_1+"կանցդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ուցդ" ;
                             Pl => base_1+"կանցիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովդ" ;
                             Pl => base_1+"կանցովդ"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkN047"
  } ;

mkN048 : Str -> N ;
mkN048 base =
  case base of {
    base_1+"ու"+base_2@(?+?) => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ու"+base_2 ;
                       Pl => base_1+"ու"+base_2+"ներ"
                     } ;
              Dat => table {
                       Sg => base_1+base_2+"յան" ;
                       Pl => base_1+"ու"+base_2+"ների"
                     } ;
              Ablat => table {
                         Sg => base_1+"ու"+base_2+"ից" ;
                         Pl => base_1+"ու"+base_2+"ներից"
                       } ;
              Instr => table {
                         Sg => base_1+base_2+"յամբ" ;
                         Pl => base_1+"ու"+base_2+"ներով"
                       } ;
              Loc => table {
                       Sg => base_1+"ու"+base_2+"ում" ;
                       Pl => base_1+"ու"+base_2+"ներում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+base_2+"յանը" ;
                    Pl => base_1+"ու"+base_2+"ներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ու"+base_2+"ը" ;
                    Pl => base_1+"ու"+base_2+"ները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ու"+base_2+"ս" ;
                           Pl => base_1+"ու"+base_2+"ներս"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"յանս" ;
                           Pl => base_1+"ու"+base_2+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ու"+base_2+"իցս" ;
                             Pl => base_1+"ու"+base_2+"ներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ու"+base_2+"ովս" ;
                             Pl => base_1+"ու"+base_2+"ներովս"
                           } ;
                  Loc => table {
                           Sg => base_1+"ու"+base_2+"ումս" ;
                           Pl => base_1+"ու"+base_2+"ներումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ու"+base_2+"դ" ;
                           Pl => base_1+"ու"+base_2+"ներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"յանդ" ;
                           Pl => base_1+"ու"+base_2+"ներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ու"+base_2+"իցդ" ;
                             Pl => base_1+"ու"+base_2+"ներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ու"+base_2+"ովդ" ;
                             Pl => base_1+"ու"+base_2+"ներովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+"ու"+base_2+"ումդ" ;
                           Pl => base_1+"ու"+base_2+"ներումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkN048"
  } ;

mkN049 : Str -> N ;
mkN049 base =
  case base of {
    base_1+"ի"+base_2@? => lin N
      { s = table {
              Nom => table {
                       Sg => base_1+"ի"+base_2 ;
                       Pl => base_1+"ի"+base_2+"ներ"
                     } ;
              Dat => table {
                       Sg => base_1+base_2+"ոջ" ;
                       Pl => base_1+"ի"+base_2+"ների"
                     } ;
              Ablat => table {
                         Sg => base_1+base_2+"ոջից" ;
                         Pl => base_1+"ի"+base_2+"ներից"
                       } ;
              Instr => table {
                         Sg => base_1+base_2+"ոջով" ;
                         Pl => base_1+"ի"+base_2+"ներով"
                       } ;
              Loc => table {
                       Sg => nonExist ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+base_2+"ոջը" ;
                    Pl => base_1+"ի"+base_2+"ներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ի"+base_2+"ը" ;
                    Pl => base_1+"ի"+base_2+"ները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ի"+base_2+"ս" ;
                           Pl => base_1+"ի"+base_2+"ներս"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"ոջս" ;
                           Pl => base_1+"ի"+base_2+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"ոջիցս" ;
                             Pl => base_1+"ի"+base_2+"ներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ոջովս" ;
                             Pl => base_1+"ի"+base_2+"ներովս"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ի"+base_2+"դ" ;
                           Pl => base_1+"ի"+base_2+"ներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"ոջդ" ;
                           Pl => base_1+"ի"+base_2+"ներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"ոջիցդ" ;
                             Pl => base_1+"ի"+base_2+"ներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ոջովդ" ;
                             Pl => base_1+"ի"+base_2+"ներովդ"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkN049"
  } ;

mkA001 : Str -> A ;
mkA001 base =
  case base of {
    base_1 => lin A
      { s = table {
              Nom => table {
                       Sg => base_1 ;
                       Pl => base_1+"ներ"
                     } ;
              Dat => table {
                       Sg => base_1+"ի" ;
                       Pl => base_1+"ների"
                     } ;
              Ablat => table {
                         Sg => base_1+"ից" ;
                         Pl => base_1+"ներից"
                       } ;
              Instr => table {
                         Sg => base_1+"ով" ;
                         Pl => base_1+"ներով"
                       } ;
              Loc => table {
                       Sg => base_1+"ում" ;
                       Pl => base_1+"ներում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"ին" ;
                    Pl => base_1+"ներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ը" ;
                    Pl => base_1+"ները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ս" ;
                           Pl => base_1+"ներս"
                         } ;
                  Dat => table {
                           Sg => base_1+"իս" ;
                           Pl => base_1+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"իցս" ;
                             Pl => base_1+"ներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովս" ;
                             Pl => base_1+"ներովս"
                           } ;
                  Loc => table {
                           Sg => base_1+"ումս" ;
                           Pl => base_1+"ներումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"դ" ;
                           Pl => base_1+"ներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"իդ" ;
                           Pl => base_1+"ներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"իցդ" ;
                             Pl => base_1+"ներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովդ" ;
                             Pl => base_1+"ներովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+"ումդ" ;
                           Pl => base_1+"ներումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkA001"
  } ;

mkA002 : Str -> A ;
mkA002 base =
  case base of {
    base_1 => lin A
      { s = table {
              Nom => table {
                       Sg => base_1 ;
                       Pl => base_1+"ներ"
                     } ;
              Dat => table {
                       Sg => base_1+"ի" ;
                       Pl => base_1+"ների"
                     } ;
              Ablat => table {
                         Sg => base_1+"ից" ;
                         Pl => base_1+"ներից"
                       } ;
              Instr => table {
                         Sg => base_1+"ով" ;
                         Pl => base_1+"ներով"
                       } ;
              Loc => table {
                       Sg => base_1+"ում" ;
                       Pl => base_1+"ներում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"ին" ;
                    Pl => base_1+"ներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ն" ;
                    Pl => base_1+"ները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ս" ;
                           Pl => base_1+"ներս"
                         } ;
                  Dat => table {
                           Sg => base_1+"իս" ;
                           Pl => base_1+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"իցս" ;
                             Pl => base_1+"ներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովս" ;
                             Pl => base_1+"ներովս"
                           } ;
                  Loc => table {
                           Sg => base_1+"ումս" ;
                           Pl => base_1+"ներումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"դ" ;
                           Pl => base_1+"ներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"իդ" ;
                           Pl => base_1+"ներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"իցդ" ;
                             Pl => base_1+"ներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովդ" ;
                             Pl => base_1+"ներովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+"ումդ" ;
                           Pl => base_1+"ներումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkA002"
  } ;

mkA003 : Str -> A ;
mkA003 base =
  case base of {
    base_1+"ի" => lin A
      { s = table {
              Nom => table {
                       Sg => base_1+"ի" ;
                       Pl => base_1+"իներ"
                     } ;
              Dat => table {
                       Sg => base_1+"ու" ;
                       Pl => base_1+"իների"
                     } ;
              Ablat => table {
                         Sg => base_1+"ուց" ;
                         Pl => base_1+"իներից"
                       } ;
              Instr => table {
                         Sg => base_1+"ով" ;
                         Pl => base_1+"իներով"
                       } ;
              Loc => table {
                       Sg => base_1+"ում" ;
                       Pl => base_1+"իներում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"ուն" ;
                    Pl => base_1+"իներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ին" ;
                    Pl => base_1+"իները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"իս" ;
                           Pl => base_1+"իներս"
                         } ;
                  Dat => table {
                           Sg => base_1+"ուս" ;
                           Pl => base_1+"իներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ուցս" ;
                             Pl => base_1+"իներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովս" ;
                             Pl => base_1+"իներովս"
                           } ;
                  Loc => table {
                           Sg => base_1+"ումս" ;
                           Pl => base_1+"իներումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"իդ" ;
                           Pl => base_1+"իներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"ուդ" ;
                           Pl => base_1+"իներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"ուցդ" ;
                             Pl => base_1+"իներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովդ" ;
                             Pl => base_1+"իներովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+"ումդ" ;
                           Pl => base_1+"իներումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkA003"
  } ;

mkA004 : Str -> A ;
mkA004 base =
  case base of {
    base_1 => lin A
      { s = table {
              Nom => table {
                       Sg => base_1 ;
                       Pl => base_1+"ներ"
                     } ;
              Dat => table {
                       Sg => base_1+"յի" ;
                       Pl => base_1+"ների"
                     } ;
              Ablat => table {
                         Sg => base_1+"յից" ;
                         Pl => base_1+"ներից"
                       } ;
              Instr => table {
                         Sg => base_1+"յով" ;
                         Pl => base_1+"ներով"
                       } ;
              Loc => table {
                       Sg => base_1+"յում" ;
                       Pl => base_1+"ներում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"յին" ;
                    Pl => base_1+"ներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ն" ;
                    Pl => base_1+"ները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ս" ;
                           Pl => base_1+"ներս"
                         } ;
                  Dat => table {
                           Sg => base_1+"յիս" ;
                           Pl => base_1+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"յիցս" ;
                             Pl => base_1+"ներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"յովս" ;
                             Pl => base_1+"ներովս"
                           } ;
                  Loc => table {
                           Sg => base_1+"յումս" ;
                           Pl => base_1+"ներումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"դ" ;
                           Pl => base_1+"ներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"յիդ" ;
                           Pl => base_1+"ներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"յիցդ" ;
                             Pl => base_1+"ներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"յովդ" ;
                             Pl => base_1+"ներովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+"յումդ" ;
                           Pl => base_1+"ներումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkA004"
  } ;

mkA005 : Str -> A ;
mkA005 base =
  case base of {
    base_1 => lin A
      { s = table {
              Nom => table {
                       Sg => base_1 ;
                       Pl => base_1+"եր"
                     } ;
              Dat => table {
                       Sg => base_1+"ի" ;
                       Pl => base_1+"երի"
                     } ;
              Ablat => table {
                         Sg => base_1+"ից" ;
                         Pl => base_1+"երից"
                       } ;
              Instr => table {
                         Sg => base_1+"ով" ;
                         Pl => base_1+"երով"
                       } ;
              Loc => table {
                       Sg => base_1+"ում" ;
                       Pl => base_1+"երում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"ին" ;
                    Pl => base_1+"երին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ը" ;
                    Pl => base_1+"երը"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ս" ;
                           Pl => base_1+"երս"
                         } ;
                  Dat => table {
                           Sg => base_1+"իս" ;
                           Pl => base_1+"երիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"իցս" ;
                             Pl => base_1+"երիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովս" ;
                             Pl => base_1+"երովս"
                           } ;
                  Loc => table {
                           Sg => base_1+"ումս" ;
                           Pl => base_1+"երումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"դ" ;
                           Pl => base_1+"երդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"իդ" ;
                           Pl => base_1+"երիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"իցդ" ;
                             Pl => base_1+"երիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովդ" ;
                             Pl => base_1+"երովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+"ումդ" ;
                           Pl => base_1+"երումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkA005"
  } ;

mkA006 : Str -> A ;
mkA006 base =
  case base of {
    base_1+"ի"+base_2@("շտ"|?) => lin A
      { s = table {
              Nom => table {
                       Sg => base_1+"ի"+base_2 ;
                       Pl => base_1+"ի"+base_2+"ներ"
                     } ;
              Dat => table {
                       Sg => base_1+base_2+"ի" ;
                       Pl => base_1+"ի"+base_2+"ների"
                     } ;
              Ablat => table {
                         Sg => base_1+base_2+"ից" ;
                         Pl => base_1+"ի"+base_2+"ներից"
                       } ;
              Instr => table {
                         Sg => base_1+base_2+"ով" ;
                         Pl => base_1+"ի"+base_2+"ներով"
                       } ;
              Loc => table {
                       Sg => base_1+base_2+"ում" ;
                       Pl => base_1+"ի"+base_2+"ներում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+base_2+"ին" ;
                    Pl => base_1+"ի"+base_2+"ներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ի"+base_2+"ը" ;
                    Pl => base_1+"ի"+base_2+"ները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ի"+base_2+"ս" ;
                           Pl => base_1+"ի"+base_2+"ներս"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"իս" ;
                           Pl => base_1+"ի"+base_2+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"իցս" ;
                             Pl => base_1+"ի"+base_2+"ներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ովս" ;
                             Pl => base_1+"ի"+base_2+"ներովս"
                           } ;
                  Loc => table {
                           Sg => base_1+base_2+"ումս" ;
                           Pl => base_1+"ի"+base_2+"ներումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ի"+base_2+"դ" ;
                           Pl => base_1+"ի"+base_2+"ներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"իդ" ;
                           Pl => base_1+"ի"+base_2+"ներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"իցդ" ;
                             Pl => base_1+"ի"+base_2+"ներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ովդ" ;
                             Pl => base_1+"ի"+base_2+"ներովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+base_2+"ումդ" ;
                           Pl => base_1+"ի"+base_2+"ներումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkA006"
  } ;

mkA007 : Str -> A ;
mkA007 base =
  case base of {
    base_1+base_2@(?+?+?+?+?) => lin A
      { s = table {
              Nom => table {
                       Sg => base_1+base_2 ;
                       Pl => base_1+base_2+"ներ"
                     } ;
              Dat => table {
                       Sg => base_1+"յ"+base_2+"յի" ;
                       Pl => base_1+base_2+"ների"
                     } ;
              Ablat => table {
                         Sg => base_1+"յ"+base_2+"յից" ;
                         Pl => base_1+base_2+"ներից"
                       } ;
              Instr => table {
                         Sg => base_1+"յ"+base_2+"յով" ;
                         Pl => base_1+base_2+"ներով"
                       } ;
              Loc => table {
                       Sg => nonExist ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"յ"+base_2+"յին" ;
                    Pl => base_1+base_2+"ներին"
                  } ;
        def_nom = table {
                    Sg => base_1+base_2+"ն" ;
                    Pl => base_1+base_2+"ները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+base_2+"ս" ;
                           Pl => base_1+base_2+"ներս"
                         } ;
                  Dat => table {
                           Sg => base_1+"յ"+base_2+"յիս" ;
                           Pl => base_1+base_2+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"յ"+base_2+"յիցս" ;
                             Pl => base_1+base_2+"ներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"յ"+base_2+"յովս" ;
                             Pl => base_1+base_2+"ներովս"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+base_2+"դ" ;
                           Pl => base_1+base_2+"ներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"յ"+base_2+"յիդ" ;
                           Pl => base_1+base_2+"ներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"յ"+base_2+"յիցդ" ;
                             Pl => base_1+base_2+"ներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"յ"+base_2+"յովդ" ;
                             Pl => base_1+base_2+"ներովդ"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkA007"
  } ;

mkA008 : Str -> A ;
mkA008 base =
  case base of {
    base_1+"ու"+base_2@("նչ"|?) => lin A
      { s = table {
              Nom => table {
                       Sg => base_1+"ու"+base_2 ;
                       Pl => base_1+"ու"+base_2+"ներ"
                     } ;
              Dat => table {
                       Sg => base_1+base_2+"ի" ;
                       Pl => base_1+"ու"+base_2+"ների"
                     } ;
              Ablat => table {
                         Sg => base_1+base_2+"ից" ;
                         Pl => base_1+"ու"+base_2+"ներից"
                       } ;
              Instr => table {
                         Sg => base_1+base_2+"ով" ;
                         Pl => base_1+"ու"+base_2+"ներով"
                       } ;
              Loc => table {
                       Sg => base_1+base_2+"ում" ;
                       Pl => base_1+"ու"+base_2+"ներում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+base_2+"ին" ;
                    Pl => base_1+"ու"+base_2+"ներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ու"+base_2+"ը" ;
                    Pl => base_1+"ու"+base_2+"ները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ու"+base_2+"ս" ;
                           Pl => base_1+"ու"+base_2+"ներս"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"իս" ;
                           Pl => base_1+"ու"+base_2+"ներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"իցս" ;
                             Pl => base_1+"ու"+base_2+"ներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ովս" ;
                             Pl => base_1+"ու"+base_2+"ներովս"
                           } ;
                  Loc => table {
                           Sg => base_1+base_2+"ումս" ;
                           Pl => base_1+"ու"+base_2+"ներումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ու"+base_2+"դ" ;
                           Pl => base_1+"ու"+base_2+"ներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"իդ" ;
                           Pl => base_1+"ու"+base_2+"ներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"իցդ" ;
                             Pl => base_1+"ու"+base_2+"ներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ովդ" ;
                             Pl => base_1+"ու"+base_2+"ներովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+base_2+"ումդ" ;
                           Pl => base_1+"ու"+base_2+"ներումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkA008"
  } ;

mkA009 : Str -> A ;
mkA009 base =
  case base of {
    base_1+"ու" => lin A
      { s = table {
              Nom => table {
                       Sg => base_1+"ու" ;
                       Pl => base_1+"ուներ"
                     } ;
              Dat => table {
                       Sg => base_1+"վի" ;
                       Pl => base_1+"ուների"
                     } ;
              Ablat => table {
                         Sg => base_1+"վից" ;
                         Pl => base_1+"ուներից"
                       } ;
              Instr => table {
                         Sg => base_1+"վով" ;
                         Pl => base_1+"ուներով"
                       } ;
              Loc => table {
                       Sg => base_1+"վում" ;
                       Pl => base_1+"ուներում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"վին" ;
                    Pl => base_1+"ուներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ուն" ;
                    Pl => base_1+"ուները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ուս" ;
                           Pl => base_1+"ուներս"
                         } ;
                  Dat => table {
                           Sg => base_1+"վիս" ;
                           Pl => base_1+"ուներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"վիցս" ;
                             Pl => base_1+"ուներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"վովս" ;
                             Pl => base_1+"ուներովս"
                           } ;
                  Loc => table {
                           Sg => base_1+"վումս" ;
                           Pl => base_1+"ուներումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ուդ" ;
                           Pl => base_1+"ուներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"վիդ" ;
                           Pl => base_1+"ուներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"վիցդ" ;
                             Pl => base_1+"ուներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"վովդ" ;
                             Pl => base_1+"ուներովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+"վումդ" ;
                           Pl => base_1+"ուներումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkA009"
  } ;

mkA010 : Str -> A ;
mkA010 base =
  case base of {
    base_1@?+"ի"+base_2 => lin A
      { s = table {
              Nom => table {
                       Sg => base_1+"ի"+base_2 ;
                       Pl => base_1+base_2+"եր"
                     } ;
              Dat => table {
                       Sg => base_1+base_2+"ի" ;
                       Pl => base_1+base_2+"երի"
                     } ;
              Ablat => table {
                         Sg => base_1+base_2+"ից" ;
                         Pl => base_1+base_2+"երից"
                       } ;
              Instr => table {
                         Sg => base_1+base_2+"ով" ;
                         Pl => base_1+base_2+"երով"
                       } ;
              Loc => table {
                       Sg => base_1+base_2+"ում" ;
                       Pl => base_1+base_2+"երում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+base_2+"ին" ;
                    Pl => base_1+base_2+"երին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ի"+base_2+"ը" ;
                    Pl => base_1+base_2+"երը"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ի"+base_2+"ս" ;
                           Pl => base_1+base_2+"երս"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"իս" ;
                           Pl => base_1+base_2+"երիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"իցս" ;
                             Pl => base_1+base_2+"երիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ովս" ;
                             Pl => base_1+base_2+"երովս"
                           } ;
                  Loc => table {
                           Sg => base_1+base_2+"ումս" ;
                           Pl => base_1+base_2+"երումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ի"+base_2+"դ" ;
                           Pl => base_1+base_2+"երդ"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"իդ" ;
                           Pl => base_1+base_2+"երիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"իցդ" ;
                             Pl => base_1+base_2+"երիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ովդ" ;
                             Pl => base_1+base_2+"երովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+base_2+"ումդ" ;
                           Pl => base_1+base_2+"երումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkA010"
  } ;

mkA011 : Str -> A ;
mkA011 base =
  case base of {
    base_1+"ու"+base_2@? => lin A
      { s = table {
              Nom => table {
                       Sg => base_1+"ու"+base_2 ;
                       Pl => base_1+base_2+"եր"
                     } ;
              Dat => table {
                       Sg => base_1+base_2+"ի" ;
                       Pl => base_1+base_2+"երի"
                     } ;
              Ablat => table {
                         Sg => base_1+base_2+"ից" ;
                         Pl => base_1+base_2+"երից"
                       } ;
              Instr => table {
                         Sg => base_1+base_2+"ով" ;
                         Pl => base_1+base_2+"երով"
                       } ;
              Loc => table {
                       Sg => base_1+base_2+"ում" ;
                       Pl => base_1+base_2+"երում"
                     }
            } ;
        def_dat = table {
                    Sg => base_1+base_2+"ին" ;
                    Pl => base_1+base_2+"երին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ու"+base_2+"ը" ;
                    Pl => base_1+base_2+"երը"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"ու"+base_2+"ս" ;
                           Pl => base_1+base_2+"երս"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"իս" ;
                           Pl => base_1+base_2+"երիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"իցս" ;
                             Pl => base_1+base_2+"երիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ովս" ;
                             Pl => base_1+base_2+"երովս"
                           } ;
                  Loc => table {
                           Sg => base_1+base_2+"ումս" ;
                           Pl => base_1+base_2+"երումս"
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"ու"+base_2+"դ" ;
                           Pl => base_1+base_2+"երդ"
                         } ;
                  Dat => table {
                           Sg => base_1+base_2+"իդ" ;
                           Pl => base_1+base_2+"երիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+base_2+"իցդ" ;
                             Pl => base_1+base_2+"երիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+base_2+"ովդ" ;
                             Pl => base_1+base_2+"երովդ"
                           } ;
                  Loc => table {
                           Sg => base_1+base_2+"ումդ" ;
                           Pl => base_1+base_2+"երումդ"
                         }
                }
      };
    _ => error "Can't apply paradigm mkA011"
  } ;

mkA012 : Str -> A ;
mkA012 base =
  case base of {
    base_1+"ի" => lin A
      { s = table {
              Nom => table {
                       Sg => base_1+"ի" ;
                       Pl => base_1+"իներ"
                     } ;
              Dat => table {
                       Sg => base_1+"ի" ;
                       Pl => base_1+"իների"
                     } ;
              Ablat => table {
                         Sg => base_1+"ից" ;
                         Pl => base_1+"իներից"
                       } ;
              Instr => table {
                         Sg => base_1+"ով" ;
                         Pl => base_1+"իներով"
                       } ;
              Loc => table {
                       Sg => nonExist ;
                       Pl => nonExist
                     }
            } ;
        def_dat = table {
                    Sg => base_1+"ին" ;
                    Pl => base_1+"իներին"
                  } ;
        def_nom = table {
                    Sg => base_1+"ին" ;
                    Pl => base_1+"իները"
                  } ;
        poss1 = table {
                  Nom => table {
                           Sg => base_1+"իս" ;
                           Pl => base_1+"իներս"
                         } ;
                  Dat => table {
                           Sg => base_1+"իս" ;
                           Pl => base_1+"իներիս"
                         } ;
                  Ablat => table {
                             Sg => base_1+"իցս" ;
                             Pl => base_1+"իներիցս"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովս" ;
                             Pl => base_1+"իներովս"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                } ;
        poss2 = table {
                  Nom => table {
                           Sg => base_1+"իդ" ;
                           Pl => base_1+"իներդ"
                         } ;
                  Dat => table {
                           Sg => base_1+"իդ" ;
                           Pl => base_1+"իներիդ"
                         } ;
                  Ablat => table {
                             Sg => base_1+"իցդ" ;
                             Pl => base_1+"իներիցդ"
                           } ;
                  Instr => table {
                             Sg => base_1+"ովդ" ;
                             Pl => base_1+"իներովդ"
                           } ;
                  Loc => table {
                           Sg => nonExist ;
                           Pl => nonExist
                         }
                }
      };
    _ => error "Can't apply paradigm mkA012"
  } ;
}