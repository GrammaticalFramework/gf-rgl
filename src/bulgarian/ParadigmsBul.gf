--# -path=.:../abstract:../common:../prelude
--# -coding=utf8

resource ParadigmsBul = MorphoFunsBul ** open
  Predef,
  Prelude,
  MorphoBul,
  CatBul
  in {
  flags coding=utf8 ;

oper
 -- numbers refer to Krustev, Bulg. Morph. in 187 Tables

  mkN001 : Str -> N ;
  mkN001 base = {s = table {
                       NF Sg Indef => base ;
                       NF Sg Def   => base+"а" ;
                       NF Pl Indef => base+"ове" ;
                       NF Pl Def   => base+"овете" ;
                       NFSgDefNom  => base+"ът" ;
                       NFPlCount   => base+"а" ;
                       NFVocative  => base+"е"
                     } ;
                 rel = \\_ => base ; relType = Pref ;
                 g = AMasc NonHuman ;
                 lock_N = <>
                } ;
  mkN001a : Str -> N ;
  mkN001a base = {s = table {
                       NF Sg Indef => base ;
                       NF Sg Def   => base+"а" ;
                       NF Pl Indef => base+"ове" ;
                       NF Pl Def   => base+"овете" ;
                       NFSgDefNom  => base+"ът" ;
                       NFPlCount   => base+"ове" ;
                       NFVocative  => base+"е"
                     } ;
                 rel = \\_ => base ; relType = Pref ;
                 g = AMasc Human ;
                 lock_N = <>
                } ;
  mkN002 : Str -> N ;
  mkN002 base = let v0 = tk 2 base;
                    v1 = last (base);
                    g  = AMasc NonHuman
                in {s = table {
                          NF Sg Indef => v0+"я"+v1 ;
                          NF Sg Def   => v0+"е"+v1+"а" ;
                          NF Pl Indef => v0+"е"+v1+"ове" ;
                          NF Pl Def   => v0+"е"+v1+"овете" ;
                          NFSgDefNom  => v0+"е"+v1+"а" ;
                          NFPlCount   => v0+"я"+v1+"а" ;
                          NFVocative  => v0+"я"+v1
                        } ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   } ;
  mkN002a : Str -> N ;
  mkN002a base = let v0 = tk 2 base;
                     v1 = last (base);
                     g  = AMasc NonHuman
                 in {s = table {
                          NF Sg Indef => v0+"я"+v1 ;
                          NF Sg Def   => v0+"я"+v1+"а" ;
                          NF Pl Indef => v0+"е"+v1+"ове" ;
                          NF Pl Def   => v0+"е"+v1+"овете" ;
                          NFSgDefNom  => v0+"е"+v1+"а" ;
                          NFPlCount   => v0+"я"+v1+"а" ;
                          NFVocative  => v0+"я"+v1
                        } ;
                     rel = \\_ => base ; relType = Pref ;
                     g   = g ;
                     lock_N = <>
                    } ;
  mkN003 : Str -> N ;
  mkN003 base = let v0 = tk 3 base;
                    v1 = last (base);
                    g  = AMasc NonHuman
                in {s = table {
                          NF Sg Indef => v0+"ръ"+v1 ;
                          NF Sg Def   => v0+"ър"+v1+"а" ;
                          NF Pl Indef => v0+"ър"+v1+"ове" ;
                          NF Pl Def   => v0+"ър"+v1+"овете" ;
                          NFSgDefNom  => v0+"ър"+v1+"ът" ;
                          NFPlCount   => v0+"ър"+v1+"ове" ;
                          NFVocative  => v0+"ръ"+v1
                        } ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   } ;
  mkN004 : Str -> N ;
  mkN004 base = let v0 = tk 4 base;
                    g  = AMasc NonHuman
                in {s = mkNoun (v0+"ятър")
                               (v0+"етрове")
                               (v0+"ятъра")
                               (v0+"етре")
                               g;
                    rel = \\_ => base; relType = Pref;
                    g   = g ;
                    lock_N = <>
                   } ;
  mkN005 : Str -> N ;
  mkN005 base = let v0 = base;
                    g  = AMasc NonHuman
                in {s = mkNoun (v0)
                               (v0+"ове")
                               (v0+"а")
                               (v0)
                               g;
                    rel = \\_ => base; relType = Pref;
                    g   = g ;
                    lock_N = <>
                   } ;
  mkN006 : Str -> N ;
  mkN006 base = let v0 = base;
                    g  = AMasc NonHuman
                in {s = mkNoun (v0)
                               (v0+"ове")
                               (v0+"а")
                               (v0)
                               g;
                    rel = \\_ => base; relType = Pref;
                    g   = g ;
                    lock_N = <>
                   } ;
  mkN007 : Str -> N ;
  mkN007 base = {s = table {
                       NF Sg Indef => base ;
                       NF Sg Def   => base+"а" ;
                       NF Pl Indef => base+"и" ;
                       NF Pl Def   => base+"ите" ;
                       NFSgDefNom  => base+"ът" ;
                       NFPlCount   => base+"а" ;
                       NFVocative  => base+"е"
                     } ;
                 rel = \\_ => base; relType = Pref;
                 g   = AMasc NonHuman;
                 lock_N = <>
                } ;
  mkN007b : Str -> N ;
  mkN007b base = let v0 = base;
                     g  = AMasc NonHuman
                 in {s = mkNoun (v0)
                                (v0+"и")
                                (v0+"а")
                                (v0+"о")
                                g ;
                     rel = \\_ => base; relType = Pref;
                     g   = g ;
                     lock_N = <>                     
                    } ;
  mkN007a : Str -> N ;
  mkN007a base = {s = table {
                       NF Sg Indef => base ;
                       NF Sg Def   => base+"а" ;
                       NF Pl Indef => base+"и" ;
                       NF Pl Def   => base+"ите" ;
                       NFSgDefNom  => base+"ът" ;
                       NFPlCount   => base+"и" ;
                       NFVocative  => base+"е"
                     } ;
                  rel = (mkA078 (base+"ски")).s; relType = AdjMod;
                  g   = AMasc Human ;
                  lock_N = <>
                 } ;
  mkN008 : Str -> N ;
  mkN008 base = let v0 = tk 2 base;
                    v1 = last base;
                    g  = AMasc NonHuman
                in {s = mkNoun (v0+"е"+v1)
                               (v0+v1+"и")
                               (v0+"е"+v1+"а")
                               (v0+"е"+v1+"о")
                               g ;
                    rel = \\_ => base; relType = Pref;
                    g   = g ;
                    lock_N = <>
                   } ;
  mkN008b : Str -> N ;
  mkN008b base = let v0 = tk 2 base;
                     v1 = last (base);
                     g  = AMasc NonHuman
                 in {s = mkNoun (v0+"е"+v1)
                                (v0+v1+"и")
                                (v0+"е"+v1+"а")
                                (v0+"е"+v1+"е")
                                g ;
                     rel = \\_ => base ; relType = Pref ;
                     g   = g ;
                     lock_N = <>
                    } ;
  mkN008c : Str -> N ;
  mkN008c base = let v0 = tk 2 base;
                     v1 = last (base);
                     g  = AMasc NonHuman
                 in {s = mkNoun (v0+"е"+v1)
                                (v0+v1+"и")
                                (v0+"е"+v1+"а")
                                (v0+v1+"е")
                                g ;
                     rel = \\_ => base ; relType = Pref ;
                     g   = g ;
                     lock_N = <>
                    } ;
  mkN008a : Str -> N ;
  mkN008a base = let v0 = tk 2 base;
                     g  = AMasc Human
                 in {s = mkNoun (v0+"ец")
                                (v0+"ци")
                                (v0+"ци")
                                (v0+"ецо")
                                g ;
                     rel = (mkA078 (v0+"ски")).s ; relType = AdjMod;
                     g   = g ;
                     lock_N = <>
                    } ;
  mkN009 : Str -> N ;
  mkN009 base = let v0 = tk 2 base;
                    v1 = last (base);
                    g  = AMasc NonHuman
                in {s = mkNoun (v0+"ъ"+v1)
                               (v0+v1+"и")
                               (v0+"ъ"+v1+"а")
                               (v0+v1+"е")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   } ;
  mkN009a : Str -> N ;
  mkN009a base = let v0 = tk 2 base;
                     g  = AMasc NonHuman
                 in {s = mkNoun (v0+"ър")
                                (v0+"рове")
                                (v0+"ъра")
                                (v0+"ре")
                                g ;
                     rel = \\_ => base ; relType = Pref ;
                     g   = g ;
                     lock_N = <>
                    } ;
  mkN010 : Str -> N ;
  mkN010 base = let v0 = tk 2 base;
                    g  = AMasc NonHuman
                in {s = mkNoun (v0+"ър")
                               (v0+"ри")
                               (v0+"ра")
                               (v0+"ре")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN011 : Str -> N ;
  mkN011 base = let v0 = tk 2 base;
                    g  = AMasc NonHuman
                in {s = table {
                          NF Sg Indef => v0+"ъм" ;
                          NF Sg Def   => v0+"ма" ;
                          NF Pl Indef => v0+"ми" ;
                          NF Pl Def   => v0+"мите" ;
                          NFSgDefNom  => v0+"мът" ;
                          NFPlCount   => v0+"ма" ;
                          NFVocative  => v0+"ме"
                        } ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN012 : Str -> N ;
  mkN012 base = let v0 = tk 3 base;
                    g  = AMasc NonHuman
                in {s = mkNoun (v0+"рък")
                               (v0+"ърци")
                               (v0+"ърци")
                               (v0+"ърко")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN013 : Str -> N ;
  mkN013 base = let v0 = tk 2 base;
                    g  = AMasc Human
                in {s = mkNoun (v0+"ец")
                               (v0+"йци")
                               (v0+"йци")
                               (v0+"ецо")
                               g ;
                    rel = (mkA078 (base+"йски")).s; relType = AdjMod;
                    g   = g ;
                    lock_N = <>
                   };
  mkN014 : Str -> N ;
  mkN014 base = let v0 = tk 1 base;
                    g  = AMasc NonHuman
                in {s = mkNoun (v0+"к")
                               (v0+"ци")
                               (v0+"ка")
                               (v0+"к")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN014a : Str -> N ;
  mkN014a base = let v0 = tk 1 base;
                     g  = AMasc Human
                 in {s = mkNoun (v0+"к")
                                (v0+"ци")
                                (v0+"ка")
                                (v0+"ко")
                                g ;
                     rel = (mkA078 (v0+"чески")).s; relType = AdjMod;
                     g   = g ;
                     lock_N = <>
                    };
  mkN015 : Str -> N ;
  mkN015 base = let v0 = tk 1 base;
                    g  = AMasc NonHuman
                in {s = mkNoun (v0+"г")
                               (v0+"зи")
                               (v0+"га")
                               (v0+"зе")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN015a : Str -> N ;
  mkN015a base = let v0 = tk 1 base;
                     g  = AMasc Human
                 in {s = mkNoun (v0+"г")
                                (v0+"зи")
                                (v0+"га")
                                (v0+"зе")
                                g ;
                     rel = \\_ => base ; relType = Pref ;
                     g   = g ;
                     lock_N = <>
                    };
  mkN016 : Str -> N ;
  mkN016 base = let v0 = tk 1 base;
                    g  = AMasc NonHuman
                in {s = mkNoun (v0+"х")
                               (v0+"си")
                               (v0+"ха")
                               (v0+"хо")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN016a : Str -> N ;
  mkN016a base = let v0 = tk 1 base;
                     g  = AMasc NonHuman
                 in {s = mkNoun (v0+"х")
                                (v0+"си")
                                (v0+"ха")
                                (v0+"се")
                                g ;
                     rel = \\_ => base ; relType = Pref ;
                     g   = g ;
                     lock_N = <>
                    };
  mkN017 : Str -> N ;
  mkN017 base = let v0 = tk 1 base;
                    g  = AMasc NonHuman
                in {s = mkNoun (v0+"к")
                               (v0+"ни")
                               (v0+"ка")
                               (v0+"ко")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN018 : Str -> N ;
  mkN018 base = let v0 = tk 2 base;
                    g  = AMasc NonHuman
                in {s = mkNoun (v0+"ин")
                               (v0+"и")
                               (v0+"и")
                               (v0+"ине")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN018a : Str -> N ;
  mkN018a base = let v0 = tk 2 base;
                     v1 = last (base);
                     g  = AMasc NonHuman
                 in {s = mkNoun (v0+"и"+v1)
                                (v0+"и")
                                (v0+"и")
                                (v0+"и"+v1+"о")
                                g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN019 : Str -> N ;
  mkN019 base = let v0 = tk 2 base;
                    g  = AMasc Human
                in {s = mkNoun (v0+"ък")
                               (v0+"ци")
                               (v0+"ци")
                               (v0+"ко")
                               g ;
                    rel = (mkA078 (v0+"ски")).s; relType = AdjMod;
                    g   = g ;
                    lock_N = <>
                   };
  mkN019a : Str -> N ;
  mkN019a base = let v0 = tk 2 base;
                     g  = AMasc NonHuman
                 in {s = mkNoun (v0+"ек")
                                (v0+"йци")
                                (v0+"ека")
                                (v0+"йо")
                                g ;
                     rel = \\_ => base ; relType = Pref ;
                     g   = g ;
                     lock_N = <>
                    };
  mkN020 : Str -> N ;
  mkN020 base = let v0 = tk 3 base;
                    v1 = last (tk 2 base);
                    g  = AMasc Human
                in {s = mkNoun (v0+v1+"ец")
                               (v0+"ъ"+v1+"ци")
                               (v0+"ъ"+v1+"ци")
                               (v0+v1+"ецо")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN021 : Str -> N ;
  mkN021 base = let v0 = tk 3 base;
                    g  = AMasc Human
                in {s = mkNoun (v0+"чин")
                               (v0+"ци")
                               (v0+"ци")
                               (v0+"чино")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN022 : Str -> N ;
  mkN022 base = let v0 = base;
                    g  = AMasc NonHuman
                in {s = mkNoun (v0)
                               (v0+"а")
                               (v0+"а")
                               (v0+"о")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN023 : Str -> N ;
  mkN023 base = let v0 = tk 2 base;
                    g  = AMasc Human
                in {s = mkNoun (v0+"ин")
                               (v0+"а")
                               (v0+"а")
                               (v0+"ине")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN024a : Str -> N ;
  mkN024a base = let v0 = tk 1 base;
                     g  = AMasc Human
                 in {s = mkNoun (v0+"з")
                                (v0+"зе")
                                (v0+"зе")
                                (v0+"же")
                                g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN024 : Str -> N ;
  mkN024 base = let v0 = base;
                    g  = AMasc Human
                in {s = mkNoun (v0)
                               (v0+"е")
                               (v0+"е")
                               (v0+"о")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN025 : Str -> N ;
  mkN025 base = let v0 = base;
                    g  = AMasc Human
                in {s = mkNoun (v0)
                               (v0+"я")
                               (v0+"я")
                               (v0+"е")
                               g ;
                    rel = (mkA078 (base+"ски")).s; relType = AdjMod;
                    g   = g ;
                    lock_N = <>
                   };
  mkN026 : Str -> N ;
  mkN026 base = let v0 = base;
                    g  = AMasc NonHuman
                in {s = mkNoun (v0)
                               (v0+"илища")
                               (v0+"илища")
                               (v0+"е")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN027 : Str -> N ;
  mkN027 base = let v0 = tk 2 base;
                    g  = AMasc NonHuman
                in {s = mkNoun (v0+"ец")
                               (v0+"овце")
                               (v0+"еца")
                               (v0+"о")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN028 : Str -> N ;
  mkN028 base = let v0 = tk 1 base;
                    g  = AMasc NonHuman
                in {s = mkNoun (v0+"й")
                               (v0+"еве")
                               (v0+"я")
                               (v0+"е")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN028a : Str -> N ;
  mkN028a base = let v0 = tk 1 base;
                     g  = AMasc NonHuman
                 in {s = mkNoun (v0+"й")
                                (v0+"йове")
                                (v0+"я")
                                (v0+"йо")
                                g ;
                     rel = \\_ => base ; relType = Pref ;
                     g   = g ;
                     lock_N = <>
                    };
  mkN029 : Str -> N ;
  mkN029 base = let v0 = base;
                    g  = AMasc Human
                in {s = mkNoun (v0)
                               (v0+"ьове")
                               (v0+"ьове")
                               (v0+"ко")
                               g ;
                    rel = (mkA078 (base+"ски")).s; relType = AdjMod;
                    g   = g ;
                    lock_N = <>
                   };
  mkN030 : Str -> N ;
  mkN030 base = let v0 = tk 2 base;
                    g  = AMasc NonHuman
                in {s = mkNoun (v0+"ън")
                               (v0+"ньове")
                               (v0+"ъня")
                               (v0+"е")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN031 : Str -> N ;
  mkN031 base = let v0 = base;
                    g  = AMasc NonHuman
                in {s = mkNoun (v0)
                               (v0+"и")
                               (v0+"я")
                               (v0+"е")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN031a : Str -> N ;
  mkN031a base = let v0 = base;
                     g  = AMasc Human
                 in {s = mkNoun (v0)
                                (v0+"и")
                                (v0+"и")
                                (v0+"ю")
                                g ;
                     rel = (mkA078 (base+"ски")).s ; relType = AdjMod ;
                     g   = g ;
                     lock_N = <>
                    };
  mkN032 : Str -> N ;
  mkN032 base = let v0 = base ;
                    v1 = tk 1 base ;
                    g  = AMasc NonHuman
                in {s = mkNoun (v0)
                               (v1+"и")
                               (v1+"я")
                               (v0+"о")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN032a : Str -> N ;
  mkN032a base = let v0 = tk 1 base;
                     g  = AMasc Human
                 in {s = mkNoun (v0+"й")
                                (v0+"и")
                                (v0+"я")
                                (v0+"ю")
                                g ;
                     rel = (mkA078 (base+"ски")).s ; relType = AdjMod ;
                     g   = g ;
                     lock_N = <>
                    };
  mkN033 : Str -> N ;
  mkN033 base = let v0 = tk 2 base;
                    g  = AMasc NonHuman
                in {s = mkNoun (v0+"ен")
                               (v0+"ни")
                               (v0+"ена")
                               (v0+"ене")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN034 : Str -> N ;
  mkN034 base = let v0 = tk 2 base;
                    g  = AMasc NonHuman
                in {s = mkNoun (v0+"ът")
                               (v0+"ти")
                               (v0+"ътя")
                               (v0+"е")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN035 : Str -> N ;
  mkN035 base = let v0 = base;
                    g  = AMasc NonHuman
                in {s = mkNoun (v0)
                               (v0+"е")
                               (v0+"я")
                               (v0+"е")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN035a : Str -> N ;
  mkN035a base = let v0 = base;
                     g  = AMasc Human
                 in {s = mkNoun (v0)
                                (v0+"е")
                                (v0+"я")
                                (v0+"ю")
                                g ;
                     rel = (mkA078 (base+"ски")).s ; relType = AdjMod ;
                     g   = g ;
                     lock_N = <>                     
                    };
  mkN036 : Str -> N ;
  mkN036 base = let v0 = tk 1 base;
                    g  = AMasc NonHuman
                in {s = mkNoun (v0+"й")
                               (v0+"ища")
                               (v0+"я")
                               (v0+"е")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN037 : Str -> N ;
  mkN037 base = let v0 = base;
                    g  = AMasc NonHuman
                in {s = mkNoun (v0)
                               (v0+"ища")
                               (v0+"я")
                               (v0+"е")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN038 : Str -> N ;
  mkN038 base = let v0 = tk 1 base;
                    g  = AMasc Human
                in {s = mkNoun (v0+"а")
                               (v0+"и")
                               (v0+"и")
                               (v0+"а")
                               g ;
                    rel = (mkAdjective base (base+"та") (base+"та") base (base+"та") base (base+"та") (v0+"и") (v0+"ите")).s ;
                    relType = AdjMod ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN039 : Str -> N ;
  mkN039 base = let v0 = tk 1 base;
                    g  = AMasc Human
                in {s = mkNoun (v0+"я")
                               (v0+"и")
                               (v0+"и")
                               (v0+"йo")
                               g ;
                    rel = (mkA078 (v0+"йски")).s ; relType = AdjMod ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN040 : Str -> N ;
  mkN040 base = let v0 = tk 1 base;
                    g  = AMasc Human
                in {s = mkNoun (v0+"о")
                               (v0+"овци")
                               (v0+"овци")
                               (v0+"о")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN040a : Str -> N ;
  mkN040a base = let v0 = base;
                     g  = AMasc NonHuman
                 in {s = mkNoun (v0)
                                (v0+"и")
                                (v0+"а")
                                (v0+"е")
                                g ;
                     rel = \\_ => base ; relType = Pref ;
                     g   = g ;
                     lock_N = <>
                    };
  mkN041 : Str -> N ;
  mkN041 base = let v0 = tk 1 base;
                    g  = AFem
                in {s = mkNoun (v0+"а")
                               (v0+"и")
                               (v0+"и")
                               (v0+"о")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN041a : Str -> N ;
  mkN041a base = let v0 = tk 1 base;
                     g  = AFem
                 in {s = mkNoun (v0+"а")
                                (v0+"и")
                                (v0+"и")
                                (v0+"о")
                                g ;
                     rel = \\_ => base ; relType = Pref ;
                     g   = g ;
                     lock_N = <>
                    };
  mkN041b : Str -> N ;
  mkN041b base = let v0 = tk 1 base;
                     g  = AFem
                 in {s = mkNoun (v0+"а")
                                (v0+"и")
                                (v0+"и")
                                (v0+"е")
                                g ;
                     rel = \\_ => base ; relType = Pref ;
                     g   = g ;
                     lock_N = <>
                   };
  mkN042 : Str -> N ;
  mkN042 base = let v0 = base;
                    g  = AFem
                in {s = mkNoun (v0)
                               (v0)
                               (v0)
                               (v0)
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                  };
  mkN043 : Str -> N ;
  mkN043 base = let v0 = tk 3 base;
                    v1 = last (tk 1 base);
                    g  = AFem
                in {s = mkNoun (v0+"я"+v1+"а")
                               (v0+"е"+v1+"и")
                               (v0+"е"+v1+"и")
                               (v0+"о")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                  };
  mkN043a : Str -> N ;
  mkN043a base = let v0 = tk 4 base;
                     v1 = last (tk 2 base);
                     g  = AFem
                 in {s = mkNoun (v0+"я"+v1+"ка")
                                (v0+"е"+v1+"ки")
                                (v0+"е"+v1+"ки")
                                (v0+"о")
                                g ;
                     rel = \\_ => base ; relType = Pref ;
                     g   = g ;
                     lock_N = <>
                    };
  mkN044 : Str -> N ;
  mkN044 base = let v0 = tk 1 base;
                    g  = AFem
                in {s = mkNoun (v0+"а")
                               (v0+"е")
                               (v0+"е")
                               (v0+"о")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                  };
  mkN045 : Str -> N ;
  mkN045 base = let v0 = tk 2 base;
                    g  = AFem
                in {s = mkNoun (v0+"ка")
                               (v0+"це")
                               (v0+"це")
                               (v0+"ка")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN046 : Str -> N ;
  mkN046 base = let v0 = tk 2 base;
                    g  = AFem
                in {s = mkNoun (v0+"га")
                               (v0+"зе")
                               (v0+"зе")
                               (v0+"га")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN047 : Str -> N ;
  mkN047 base = let v0 = tk 1 base;
                    g  = AFem
                in {s = mkNoun (v0+"я")
                               (v0+"и")
                               (v0+"и")
                               (v0+"о")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN048 : Str -> N ;
  mkN048 base = let v0 = tk 1 base;
                    g  = AFem
                in {s = mkNoun (v0+"я")
                               (v0+"е")
                               (v0+"е")
                               (v0+"ьо")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN049 : Str -> N ;
  mkN049 base = let v0 = base;
                    g  = AFem
                in {s = mkNoun (v0)
                               (v0+"и")
                               (v0+"и")
                               (v0)
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN050 : Str -> N ;
  mkN050 base = let v0 = tk 2 base;
                    g  = AFem
                in {s = mkNoun (v0+"ен")
                               (v0+"ни")
                               (v0+"ни")
                               (v0+"ен")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN051 : Str -> N ;
  mkN051 base = let v0 = tk 2 base;
                    v1 = last (base);
                    g  = AFem
                in {s = mkNoun (v0+"ъ"+v1)
                               (v0+v1+"и")
                               (v0+v1+"и")
                               (v0+"ъ"+v1)
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN052 : Str -> N ;
  mkN052 base = let v0 = tk 5 base;
                    g  = AFem
                in {s = mkNoun (v0+"ялост")
                               (v0+"ялости")
                               (v0+"ялости")
                               (v0+"ялост")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN052a : Str -> N ;
  mkN052a base = let v0 = tk 6 base;
                     g  = AFem
                 in {s = mkNoun (v0+"ярност")
                                (v0+"ярности")
                                (v0+"ярности")
                                (v0+"ярност")
                                g ;
                     rel = \\_ => base ; relType = Pref ;
                     g   = g ;
                     lock_N = <>
                    };
  mkN053 : Str -> N ;
  mkN053 base = let v0 = tk 3 base;
                    v1 = last (base);
                    g  = AFem
                in {s = mkNoun (v0+"ръ"+v1)
                               (v0+"ър"+v1+"и")
                               (v0+"ър"+v1+"и")
                               (v0+"ръ"+v1)
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN054 : Str -> N ;
  mkN054 base = let v0 = tk 1 base;
                    g  = ANeut
                in {s = mkNoun (v0+"о")
                               (v0+"а")
                               (v0+"а")
                               (v0+"о")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN055 : Str -> N ;
  mkN055 base = let v0 = tk 1 base;
                    g  = ANeut
                in {s = variants {
                          mkNoun (v0+"о")
                                 (v0+"а")
                                 (v0+"а")
                                 (v0+"о") ;
                          mkNoun (v0+"о")
                                 (v0+"ета")
                                 (v0+"ета")
                                 (v0+"о")}
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN056 : Str -> N ;
  mkN056 base = let v0 = tk 1 base;
                    g  = ANeut
                in {s = variants {
                          mkNoun (v0+"о")
                                 (v0+"а")
                                 (v0+"а")
                                 (v0+"о") ;
                          mkNoun (v0+"о")
                                 (v0+"е")
                                 (v0+"а")
                                 (v0+"о")}
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN057 : Str -> N ;
  mkN057 base = let v0 = tk 3 base;
                    v1 = last (tk 1 base);
                    g  = ANeut
                in {s = mkNoun (v0+"я"+v1+"о")
                               (v0+"е"+v1+"а")
                               (v0+"е"+v1+"а")
                               (v0+"я"+v1+"о")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN057a : Str -> N ;
  mkN057a base = let v0 = tk 4 base;
                     g  = ANeut
                 in {s = mkNoun (v0+"ясто")
                                (v0+"еста")
                                (v0+"еста")
                                (v0+"ясто")
                                g ;
                     rel = \\_ => base ; relType = Pref ;
                     g   = g ;
                     lock_N = <>
                    };
  mkN058 : Str -> N ;
  mkN058 base = let v0 = tk 3 base;
                    g  = ANeut
                in {s = mkNoun (v0+"яно")
                               (v0+"ена")
                               (v0+"ена")
                               (v0+"яно")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN059 : Str -> N ;
  mkN059 base = let v0 = tk 1 base;
                    g  = ANeut
                in {s = mkNoun (v0+"о")
                               (v0+"ене")
                               (v0+"ене")
                               (v0+"о")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN060 : Str -> N ;
  mkN060 base = let v0 = tk 1 base;
                    g  = ANeut
                in {s = mkNoun (v0+"о")
                               (v0+"еса")
                               (v0+"еса")
                               (v0+"о")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN061 : Str -> N ;
  mkN061 base = let v0 = tk 1 base;
                    g  = ANeut
                in {s = mkNoun (v0+"о")
                               (v0+"а")
                               (v0+"а")
                               (v0+"о")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN062 : Str -> N ;
  mkN062 base = let v0 = tk 1 base;
                    g  = ANeut
                in {s = mkNoun (v0+"о")
                               (v0+"и")
                               (v0+"и")
                               (v0+"о")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN063 : Str -> N ;
  mkN063 base = let v0 = tk 2 base;
                    g  = ANeut
                in {s = mkNoun (v0+"ко")
                               (v0+"чи")
                               (v0+"чи")
                               (v0+"ко")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN064 : Str -> N ;
  mkN064 base = let v0 = tk 2 base;
                    g  = ANeut
                in {s = mkNoun (v0+"хо")
                               (v0+"ши")
                               (v0+"ши")
                               (v0+"хо")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g;
                    lock_N = <>
                   };
  mkN065 : Str -> N ;
  mkN065 base = let v0 = base;
                    g  = ANeut
                in {s = mkNoun (v0)
                               (v0+"та")
                               (v0+"та")
                               (v0)
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN066 : Str -> N ;
  mkN066 base = let v0 = tk 1 base;
                    g  = ANeut
                in {s = mkNoun (v0+"е")
                               (v0+"а")
                               (v0+"а")
                               (v0+"е")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
 mkN067 : Str -> N ;
  mkN067 base = let v0 = tk 2 base;
                    g  = ANeut
                in {s = mkNoun (v0+"те")
                               (v0+"ца")
                               (v0+"ца")
                               (v0+"те")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN068 : Str -> N ;
  mkN068 base = let v0 = tk 1 base;
                    g  = ANeut
                in {s = mkNoun (v0+"е")
                               (v0+"я")
                               (v0+"я")
                               (v0+"е")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN069 : Str -> N ;
  mkN069 base = let v0 = base;
                    g  = ANeut
                in {s = mkNoun (v0)
                               (v0+"на")
                               (v0+"на")
                               (v0)
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN070 : Str -> N ;
  mkN070 base = let v0 = base;
                    g  = ANeut
                in {s = mkNoun (v0)
                               (v0+"са")
                               (v0+"са")
                               (v0)
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN071 : Str -> N ;
  mkN071 base = let v0 = tk 1 base;
                    g  = ANeut
                in {s = mkNoun (v0+"е")
                               (v0+"ия")
                               (v0+"ия")
                               (v0+"е")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN072 : Str -> N ;
  mkN072 base = let v0 = tk 1 base;
                    g  = ANeut
                in {s = mkNoun (v0+"е")
                               (v0+"я")
                               (v0+"я")
                               (v0+"е")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN073 : Str -> N ;
  mkN073 base = let v0 = base;
                    g  = ANeut
                in {s = mkNoun (v0)
                               (v0+"та")
                               (v0+"та")
                               (v0)
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN074 : Str -> N ;
  mkN074 base = let v0 = base
                in { s = table {
                           NF Sg _     => v0 ;
                           NF Pl Indef => v0 ;
                           NF Pl Def   => v0+"та" ;
                           NFSgDefNom  => v0 ;
                           NFPlCount   => v0 ;
                           NFVocative  => v0
                         } ;
                     rel = \\_ => base ; relType = Pref ;
                     g = ANeut ;
                     lock_N = <>
                   } ;
  mkN075 : Str -> N ;
  mkN075 base = let v0 = base
                in { s = table {
                           NF Sg _     => Prelude.nonExist ;
                           NF Pl Indef => v0 ;
                           NF Pl Def   => v0+"те" ;
                           NFSgDefNom  => Prelude.nonExist ;
                           NFPlCount   => v0 ;
                           NFVocative  => v0
                         } ;
                     rel = \\_ => base ; relType = Pref ;
                     g = ANeut ;
                     lock_N = <>
                   } ;
  mkN076 : Str -> N ;
  mkN076 base = let v0 = tk 1 base;
                    g  = ANeut
                in {s = mkNoun (v0+"о")
                               (v0+"ета")
                               (v0+"ета")
                               (v0+"о")
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkN077 : Str -> N ;
  mkN077 base = let v0 = base;
                    g  = AFem
                in {s = mkNoun (v0)
                               nonExist
                               nonExist
                               (v0)
                               g ;
                    rel = \\_ => base ; relType = Pref ;
                    g   = g ;
                    lock_N = <>
                   };
  mkA076 : Str -> A ;
  mkA076 base = let v0 = base
                in mkAdjective (v0)
                               (v0+"ия")
                               (v0+"ият")
                               (v0+"а")
                               (v0+"ата")
                               (v0+"о")
                               (v0+"ото")
                               (v0+"и")
                               (v0+"ите") ;
  mkA077 : Str -> A ;
  mkA077 base = let v0 = base
                in mkAdjective (v0)
                               (v0+"ия")
                               (v0+"ият")
                               (v0+"а")
                               (v0+"ата")
                               (v0+"е")
                               (v0+"ето")
                               (v0+"и")
                               (v0+"ите") ;
  mkA078 : Str -> A ;
  mkA078 base = let v0 = tk 1 base
                in adjAdv (mkAdjective (v0+"и")
                                       (v0+"ия")
                                       (v0+"ият")
                                       (v0+"а")
                                       (v0+"ата")
                                       (v0+"о")
                                       (v0+"ото")
                                       (v0+"и")
                                       (v0+"ите")) (v0+"и") ;
  mkA079 : Str -> A ;
  mkA079 base = let v0 = tk 2 base
                in mkAdjective (v0+"ен")
                               (v0+"ния")
                               (v0+"ният")
                               (v0+"на")
                               (v0+"ната")
                               (v0+"но")
                               (v0+"ното")
                               (v0+"ни")
                               (v0+"ните") ;
  mkA080 : Str -> A ;
  mkA080 base = let v0 = tk 2 base;
                    v1 = last (base)
                in mkAdjective (v0+"ъ"+v1)
                               (v0+v1+"ия")
                               (v0+v1+"ият")
                               (v0+v1+"а")
                               (v0+v1+"ата")
                               (v0+v1+"о")
                               (v0+v1+"ото")
                               (v0+v1+"и")
                               (v0+v1+"ите") ;
  mkA081 : Str -> A ;
  mkA081 base = let v0 = tk 2 base;
                    v1 = last (base)
                in mkAdjective (v0+"я"+v1)
                               (v0+"е"+v1+"ия")
                               (v0+"е"+v1+"ият")
                               (v0+"я"+v1+"а")
                               (v0+"я"+v1+"ата")
                               (v0+"я"+v1+"о")
                               (v0+"я"+v1+"ото")
                               (v0+"е"+v1+"и")
                               (v0+"е"+v1+"ите") ;
  mkA082 : Str -> A ;
  mkA082 base = let v0 = tk 3 base;
                    v1 = last (base)
                in mkAdjective (v0+"ръ"+v1)
                               (v0+"ър"+v1+"ия")
                               (v0+"ър"+v1+"ият")
                               (v0+"ър"+v1+"а")
                               (v0+"ър"+v1+"ата")
                               (v0+"ър"+v1+"о")
                               (v0+"ър"+v1+"ото")
                               (v0+"ър"+v1+"и")
                               (v0+"ър"+v1+"ите") ;
  mkA082a : Str -> A ;
  mkA082a base = let v0 = tk 5 base
                 in mkAdjective (v0+"ързък")
                                (v0+"ръзкия")
                                (v0+"ръзкият")
                                (v0+"ръзка")
                                (v0+"ръзката")
                                (v0+"ръзко")
                                (v0+"ръзкото")
                                (v0+"ръзки")
                                (v0+"ръзките") ;
  mkA083 : Str -> A ;
  mkA083 base = let v0 = tk 4 base;
                    v1 = last (tk 2 base)
                in mkAdjective (v0+"я"+v1+"ък")
                               (v0+"е"+v1+"кия")
                               (v0+"е"+v1+"кият")
                               (v0+"я"+v1+"ка")
                               (v0+"я"+v1+"ката")
                               (v0+"я"+v1+"ко")
                               (v0+"я"+v1+"кото")
                               (v0+"е"+v1+"ки")
                               (v0+"е"+v1+"ките") ;
  mkA084 : Str -> A ;
  mkA084 base = let v0 = tk 4 base;
                    v1 = last (tk 2 base)
                in mkAdjective (v0+"е"+v1+"ен")
                               (v0+"е"+v1+"ния")
                               (v0+"е"+v1+"ният")
                               (v0+"я"+v1+"на")
                               (v0+"я"+v1+"ната")
                               (v0+"я"+v1+"но")
                               (v0+"я"+v1+"ното")
                               (v0+"е"+v1+"ни")
                               (v0+"е"+v1+"ните") ;
  mkA084a : Str -> A ;
  mkA084a base = let v0 = tk 5 base
                 in mkAdjective (v0+"естен")
                                (v0+"естния")
                                (v0+"естният")
                                (v0+"ястна")
                                (v0+"ястната")
                                (v0+"ястно")
                                (v0+"ястнота")
                                (v0+"естни")
                                (v0+"естните") ;
  mkA085 : Str -> A ;
  mkA085 base = let v0 = tk 2 base
                in mkAdjective (v0+"ен")
                               (v0+"йния")
                               (v0+"йният")
                               (v0+"йна")
                               (v0+"йната")
                               (v0+"йно")
                               (v0+"йното")
                               (v0+"йни")
                               (v0+"йните") ;
  mkA086 : Str -> A ;
  mkA086 base = let v0 = base
                in mkAdjective (v0)
                               (v0+"ия")
                               (v0+"ият")
                               (v0+"я")
                               (v0+"ята")
                               (v0+"ьо")
                               (v0+"ьото")
                               (v0+"и")
                               (v0+"ите") ;
  mkA087 : Str -> A ;
  mkA087 base = let v0 = tk 1 base
                in mkAdjective (v0+"и")
                               (v0+"ия")
                               (v0+"ият")
                               (v0+"а")
                               (v0+"ата")
                               (v0+"е")
                               (v0+"ето")
                               (v0+"и")
                               (v0+"ите") ;
  mkA088 : Str -> A ;
  mkA088 base = let v0 = tk 1 base
                in mkAdjective (v0+"и")
                               (v0+"ия")
                               (v0+"ият")
                               (v0+"я")
                               (v0+"ята")
                               (v0+"е")
                               (v0+"ето")
                               (v0+"и")
                               (v0+"ите") ;
  mkA089a : Str -> A ;
  mkA089a base = let v0 = base
                 in mkAdjective (v0)
                                (v0)
                                (v0)
                                (v0)
                                (v0)
                                (v0)
                                (v0)
                                (v0)
                                (v0) ;
  mkV142 : Str -> VTable ;
  mkV142 base = let v0 = tk 3 base
                in mkVerb (v0+"съм")
                          (v0+"е")
                          (v0+"бях")
                          (v0+"бях")
                          (v0+"бил")
                          (v0+"бил")
                          (v0+"-")
                          (v0+"-")
                          (v0+"бъди")
                          (v0+"бяне") ;
  mkV143 : Str -> VTable ;
  mkV143 base = let v0 = tk 3 base
                in mkVerb (v0+"ъда")
                          (v0+"ъде")
                          (v0+"их")
                          (v0+"ъдех")
                          (v0+"ил")
                          (v0+"ъдел")
                          (v0+"-")
                          (v0+"ъдещ")
                          (v0+"ъди")
                          (v0+"ъдене") ;
  mkV144 : Str -> VTable ;
  mkV144 base = let v0 = tk 1 base
                in mkVerb (v0+"а")
                          (v0+"е")
                          (v0+"ях")
                          (v0+"ях")
                          (v0+"ял")
                          (v0+"ял")
                          (v0+"-")
                          (v0+"-")
                          (v0+"-")
                          (v0+"яне") ;
  mkV145 : Str -> VTable ;
  mkV145 base = let v0 = tk 2 base;
                    v1 = last (tk 1 base)
                in mkVerb (v0+v1+"а")
                          (v0+v1+"е")
                          (v0+v1+"ох")
                          (v0+v1+"ях")
                          (v0+"л")
                          (v0+v1+"ял")
                          (v0+v1+"ен")
                          (v0+v1+"ящ")
                          (v0+v1+"и")
                          (v0+v1+"ене") ;
  mkV145a : Str -> VTable ;
  mkV145a base = let v0 = tk 3 base;
                     v1 = last (tk 2 base)
                 in mkVerb (v0+v1+"са")
                           (v0+v1+"се")
                           (v0+v1+"сох")
                           (v0+v1+"сях")
                           (v0+v1+"съл")
                           (v0+v1+"сял")
                           (v0+v1+"сен")
                           (v0+v1+"сящ")
                           (v0+v1+"си")
                           (v0+v1+"сяне") ;
  mkV145b : Str -> VTable ;
  mkV145b base = let v0 = tk 2 base
                 in mkVerb (v0+"та")
                           (v0+"те")
                           (v0+"тох")
                           (v0+"тях")
                           (v0+"ъл")
                           (v0+"тял")
                           (v0+"-")
                           (v0+"тящ")
                           (v0+"ти")
                           (v0+"тяне") ;
  mkV146 : Str -> VTable ;
  mkV146 base = let v0 = tk 2 base
                in mkVerb (v0+"да")
                          (v0+"де")
                          (v0+"дох")
                          (v0+"дех")
                          (v0+"шъл")
                          (v0+"дел")
                          (v0+"-")
                          (v0+"-")
                          (v0+"ди")
                          (v0+"дене") ;
  mkV146a : Str -> VTable ;
  mkV146a base = let v0 = tk 3 base
                 in mkVerb (v0+"йда")
                           (v0+"йде")
                           (v0+"йдох")
                           (v0+"йдех")
                           (v0+"шъл")
                           (v0+"йдел")
                           (v0+"-")
                           (v0+"-")
                           (v0+"йди")
                           (v0+"йдене") ;
  mkV147 : Str -> VTable ;
  mkV147 base = let v0 = tk 3 base
                in mkVerb (v0+"яза")
                          (v0+"езе")
                          (v0+"язох")
                          (v0+"езех")
                          (v0+"язъл")
                          (v0+"езел")
                          (v0+"язъл")
                          (v0+"изащ")
                          (v0+"ез")
                          (v0+"езене") ;
  mkV148 : Str -> VTable ;
  mkV148 base = let v0 = tk 2 base
                in mkVerb (v0+"ка")
                          (v0+"че")
                          (v0+"кох")
                          (v0+"чех")
                          (v0+"къл")
                          (v0+"чел")
                          (v0+"чен")
                          (v0+"чащ")
                          (v0+"чи")
                          (v0+"чене") ;
  mkV149 : Str -> VTable ;
  mkV149 base = let v0 = tk 3 base
                in mkVerb (v0+"ека")
                          (v0+"ече")
                          (v0+"якох")
                          (v0+"ечех")
                          (v0+"якъл")
                          (v0+"ечел")
                          (v0+"ечен")
                          (v0+"-")
                          (v0+"ечи")
                          (v0+"ечене") ;
  mkV150 : Str -> VTable ;
  mkV150 base = let v0 = tk 1 base
                in mkVerb (v0+"а")
                          (v0+"е")
                          (v0+"ях")
                          (v0+"ях")
                          (v0+"ял")
                          (v0+"ял")
                          (v0+"ян")
                          (v0+"-")
                          (v0+"и")
                          (v0+"яне") ;
  mkV150a : Str -> VTable ;
  mkV150a base = let v0 = tk 1 base
                 in mkVerb (v0+"а")
                           (v0+"е")
                           (v0+"ях")
                           (v0+"ях")
                           (v0+"ял")
                           (v0+"ял")
                           (v0+"-")
                           (v0+"-")
                           (v0+"и")
                           (v0+"яне") ;
  mkV151 : Str -> VTable ;
  mkV151 base = let v0 = tk 1 base
                in mkVerb (v0+"а")
                          (v0+"е")
                          (v0+"ах")
                          (v0+"ях")
                          (v0+"ал")
                          (v0+"ял")
                          (v0+"ан")
                          (v0+"ящ")
                          (v0+"и")
                          (v0+"яне") ;
  mkV152 : Str -> VTable ;
  mkV152 base = let v0 = tk 1 base
                in mkVerb (v0+"а")
                          (v0+"е")
                          (v0+"ах")
                          (v0+"ех")
                          (v0+"ал")
                          (v0+"ел")
                          (v0+"ат")
                          (v0+"-")
                          (v0+"и")
                          (v0+"ене") ;
  mkV152a : Str -> VTable ;
  mkV152a base = let v0 = tk 4 base
                 in mkVerb (v0+"ягна")
                           (v0+"егне")
                           (v0+"ягнах")
                           (v0+"ягнех")
                           (v0+"ягнал")
                           (v0+"ягнел")
                           (v0+"ягнат")
                           (v0+"-")
                           (v0+"егни")
                           (v0+"ягнене") ;
  mkV153 : Str -> VTable ;
  mkV153 base = let v0 = tk 3 base
                in mkVerb (v0+"яна")
                          (v0+"ене")
                          (v0+"янах")
                          (v0+"енех")
                          (v0+"янал")
                          (v0+"енел")
                          (v0+"янат")
                          (v0+"-")
                          (v0+"ени")
                          (v0+"енене") ;
  mkV154 : Str -> VTable ;
  mkV154 base = let v0 = tk 1 base
                in mkVerb (v0+"я")
                          (v0+"е")
                          (v0+"ах")
                          (v0+"ех")
                          (v0+"ал")
                          (v0+"ел")
                          (v0+"ан")
                          (v0+"ещ")
                          (v0+"и")
                          (v0+"ене") ;
  mkV155 : Str -> VTable ;
  mkV155 base = let v0 = tk 2 base
                in mkVerb (v0+"ча")
                          (v0+"че")
                          (v0+"ках")
                          (v0+"чех")
                          (v0+"кал")
                          (v0+"чел")
                          (v0+"кан")
                          (v0+"чещ")
                          (v0+"чи")
                          (v0+"чене") ;
  mkV156 : Str -> VTable ;
  mkV156 base = let v0 = tk 2 base
                in mkVerb (v0+"жа")
                          (v0+"же")
                          (v0+"зах")
                          (v0+"жех")
                          (v0+"зал")
                          (v0+"жел")
                          (v0+"зан")
                          (v0+"-")
                          (v0+"жи")
                          (v0+"жене") ;
  mkV157 : Str -> VTable ;
  mkV157 base = let v0 = tk 3 base
                in mkVerb (v0+"ежа")
                          (v0+"еже")
                          (v0+"язах")
                          (v0+"ежех")
                          (v0+"язал")
                          (v0+"ежел")
                          (v0+"язан")
                          (v0+"ежещ")
                          (v0+"ежи")
                          (v0+"ежене") ;
  mkV158 : Str -> VTable ;
  mkV158 base = let v0 = tk 2 base
                in mkVerb (v0+"жа")
                          (v0+"же")
                          (v0+"гах")
                          (v0+"жех")
                          (v0+"гал")
                          (v0+"жел")
                          (v0+"ган")
                          (v0+"жещ")
                          (v0+"жи")
                          (v0+"жене") ;
  mkV159 : Str -> VTable ;
  mkV159 base = let v0 = tk 2 base
                in mkVerb (v0+"ша")
                          (v0+"ше")
                          (v0+"сах")
                          (v0+"шех")
                          (v0+"сал")
                          (v0+"шел")
                          (v0+"сан")
                          (v0+"шещ")
                          (v0+"ши")
                          (v0+"шене") ;
  mkV160 : Str -> VTable ;
  mkV160 base = let v0 = tk 2 base
                in mkVerb (v0+"ея")
                          (v0+"ее")
                          (v0+"ях")
                          (v0+"еех")
                          (v0+"ял")
                          (v0+"еел")
                          (v0+"ян")
                          (v0+"еещ")
                          (v0+"ей")
                          (v0+"еене") ;
  mkV160a : Str -> VTable ;
  mkV160a base = let v0 = tk 2 base
                 in mkVerb (v0+"ея")
                           (v0+"ее")
                           (v0+"ах")
                           (v0+"еех")
                           (v0+"ал")
                           (v0+"еел")
                           (v0+"ан")
                           (v0+"еещ")
                           (v0+"ей")
                           (v0+"еене") ;
  mkV161 : Str -> VTable ;
  mkV161 base = let v0 = tk 1 base
                in mkVerb (v0+"я")
                          (v0+"е")
                          (v0+"х")
                          (v0+"ех")
                          (v0+"л")
                          (v0+"ел")
                          (v0+"н")
                          (v0+"ещ")
                          (v0+"й")
                          (v0+"ене") ;
  mkV161a : Str -> VTable ;
  mkV161a base = let v0 = tk 1 base
                 in mkVerb (v0+"я")
                           (v0+"е")
                           (v0+"х")
                           (v0+"ех")
                           (v0+"л")
                           (v0+"ел")
                           (v0+"т")
                           (v0+"ещ")
                           (v0+"й")
                           (v0+"ене") ;
  mkV162 : Str -> VTable ;
  mkV162 base = let v0 = tk 1 base
                in mkVerb (v0+"я")
                          (v0+"е")
                          (v0+"ях")
                          (v0+"ех")
                          (v0+"ял")
                          (v0+"ел")
                          (v0+"ян")
                          (v0+"ещ")
                          (v0+"й")
                          (v0+"ене") ;
  mkV163 : Str -> VTable ;
  mkV163 base = let v0 = tk 1 base
                in mkVerb (v0+"я")
                          (v0+"е")
                          (v0+"х")
                          (v0+"ех")
                          (v0+"л")
                          (v0+"ел")
                          (v0+"т")
                          (v0+"ещ")
                          (v0+"й")
                          (v0+"ене") ;
  mkV164 : Str -> VTable ;
  mkV164 base = let v0 = tk 2 base
                in mkVerb (v0+"ея")
                          (v0+"ее")
                          (v0+"ях")
                          (v0+"еех")
                          (v0+"ял")
                          (v0+"еел")
                          (v0+"ят")
                          (v0+"еещ")
                          (v0+"ей")
                          (v0+"еене") ;
  mkV165 : Str -> VTable ;
  mkV165 base = let v0 = tk 1 base
                in mkVerb (v0+"а")
                          (v0+"е")
                          (v0+"ох")
                          (v0+"ех")
                          (v0+"-")
                          (v0+"ел")
                          (v0+"-")
                          (v0+"ещ")
                          (v0+"и")
                          (v0+"ене") ;
  mkV166 : Str -> VTable ;
  mkV166 base = let v0 = tk 2 base
                in mkVerb (v0+"га")
                          (v0+"же")
                          (v0+"жах")
                          (v0+"жех")
                          (v0+"гъл")
                          (v0+"жел")
                          (v0+"-")
                          (v0+"жещ")
                          (v0+"-")
                          (v0+"жене") ;
  mkV167 : Str -> VTable ;
  mkV167 base = let v0 = tk 2 base
                in mkVerb (v0+"ка")
                          (v0+"че")
                          (v0+"ках")
                          (v0+"чех")
                          (v0+"кал")
                          (v0+"чел")
                          (v0+"кан")
                          (v0+"чещ")
                          (v0+"чи")
                          (v0+"чене") ;
  mkV168 : Str -> VTable ;
  mkV168 base = let v0 = tk 1 base
                in mkVerb (v0+"м")
                          (v0+"де")
                          (v0+"дох")
                          (v0+"дях")
                          (v0+"л")
                          (v0+"дял")
                          (v0+"ден")
                          (v0+"-")
                          (v0+"й")
                          (v0+"дяне") ;
  mkV169 : Str -> VTable ;
  mkV169 base = let v0 = tk 1 base
                in mkVerb (v0+"м")
                          (v0+"де")
                          (v0+"дох")
                          (v0+"дях")
                          (v0+"л")
                          (v0+"дял")
                          (v0+"ден")
                          (v0+"дещ")
                          (v0+"ж")
                          (v0+"дяне") ;
  mkV170 : Str -> VTable ;
  mkV170 base = let v0 = tk 3 base
                in mkVerb (v0+"ера")
                          (v0+"ере")
                          (v0+"рах")
                          (v0+"ерях")
                          (v0+"рал")
                          (v0+"ерял")
                          (v0+"ран")
                          (v0+"ерящ")
                          (v0+"ери")
                          (v0+"еряне") ;
  mkV171 : Str -> VTable ;
  mkV171 base = let v0 = tk 2 base
                in mkVerb (v0+"ма")
                          (v0+"ме")
                          (v0+"х")
                          (v0+"мех")
                          (v0+"л")
                          (v0+"мел")
                          (v0+"т")
                          (v0+"-")
                          (v0+"ми")
                          (v0+"мене") ;
  mkV172 : Str -> VTable ;
  mkV172 base = let v0 = tk 4 base
                in mkVerb (v0+"ълна")
                          (v0+"ълне")
                          (v0+"лех")
                          (v0+"ълнех")
                          (v0+"лел")
                          (v0+"ълнел")
                          (v0+"ълнат")
                          (v0+"ълнещ")
                          (v0+"ълни")
                          (v0+"ълнене") ;
  mkV173 : Str -> VTable ;
  mkV173 base = let v0 = tk 1 base
                in mkVerb (v0+"я")
                          (v0+"и")
                          (v0+"их")
                          (v0+"ех")
                          (v0+"ил")
                          (v0+"ел")
                          (v0+"ен")
                          (v0+"ещ")
                          (v0+"и")
                          (v0+"ене") ;
  mkV174 : Str -> VTable ;
  mkV174 base = let v0 = tk 1 base
                in mkVerb (v0+"я")
                          (v0+"и")
                          (v0+"их")
                          (v0+"ях")
                          (v0+"ил")
                          (v0+"ял")
                          (v0+"ен")
                          (v0+"ящ")
                          (v0+"и")
                          (v0+"яне") ;
  mkV175 : Str -> VTable ;
  mkV175 base = let v0 = tk 1 base
                in mkVerb (v0+"я")
                          (v0+"и")
                          (v0+"их")
                          (v0+"ях")
                          (v0+"ил")
                          (v0+"ял")
                          (v0+"ен")
                          (v0+"ящ")
                          (v0+"й")
                          (v0+"ене") ;
  mkV176 : Str -> VTable ;
  mkV176 base = let v0 = tk 1 base
                in mkVerb (v0+"а")
                          (v0+"и")
                          (v0+"их")
                          (v0+"ех")
                          (v0+"ил")
                          (v0+"ел")
                          (v0+"ен")
                          (v0+"ещ")
                          (v0+"и")
                          (v0+"ене") ;
  mkV177 : Str -> VTable ;
  mkV177 base = let v0 = tk 1 base
                in mkVerb (v0+"я")
                          (v0+"и")
                          (v0+"ях")
                          (v0+"ях")
                          (v0+"ял")
                          (v0+"ял")
                          (v0+"ян")
                          (v0+"ящ")
                          (v0+"и")
                          (v0+"ене") ;
  mkV178 : Str -> VTable ;
  mkV178 base = let v0 = tk 1 base
                in mkVerb (v0+"а")
                          (v0+"и")
                          (v0+"ах")
                          (v0+"ех")
                          (v0+"ал")
                          (v0+"ел")
                          (v0+"ителен")
                          (v0+"ащ")
                          (v0+"и")
                          (v0+"ене") ;
  mkV179 : Str -> VTable ;
  mkV179 base = let v0 = tk 4 base
                in mkVerb (v0+"ържа")
                          (v0+"ържи")
                          (v0+"ържах")
                          (v0+"ържех")
                          (v0+"ържал")
                          (v0+"ържел")
                          (v0+"ържан")
                          (v0+"ържащ")
                          (v0+"ръж")
                          (v0+"ържене") ;
  mkV180 : Str -> VTable ;
  mkV180 base = let v0 = tk 1 base
                in mkVerb (v0+"я")
                          (v0+"и")
                          (v0+"ях")
                          (v0+"ях")
                          (v0+"ял")
                          (v0+"ял")
                          (v0+"-")
                          (v0+"ящ")
                          (v0+"й")
                          (v0+"яне") ;
  mkV181 : Str -> VTable ;
  mkV181 base = let v0 = tk 2 base
                in mkVerb (v0+"дя")
                          (v0+"ди")
                          (v0+"дях")
                          (v0+"дех")
                          (v0+"дял")
                          (v0+"дел")
                          (v0+"дян")
                          (v0+"-")
                          (v0+"ж")
                          (v0+"дене") ;
  mkV182 : Str -> VTable ;
  mkV182 base = let v0 = tk 1 base
                in mkVerb (v0+"я")
                          (v0+"и")
                          (v0+"ах")
                          (v0+"ях")
                          (v0+"ал")
                          (v0+"ял")
                          (v0+"ан")
                          (v0+"ящ")
                          (v0+"и")
                          (v0+"ане") ;
  mkV183 : Str -> VTable ;
  mkV183 base = let v0 = tk 3 base
                in mkVerb (v0+"ежа")
                          (v0+"ежи")
                          (v0+"язах")
                          (v0+"ежех")
                          (v0+"язал")
                          (v0+"ежел")
                          (v0+"язан")
                          (v0+"ежещ")
                          (v0+"ежи")
                          (v0+"ежене") ;
  mkV184 : Str -> VTable ;
  mkV184 base = let v0 = tk 3 base
                in mkVerb (v0+"еля")
                          (v0+"ели")
                          (v0+"лях")
                          (v0+"елех")
                          (v0+"лял")
                          (v0+"елел")
                          (v0+"лян")
                          (v0+"елещ")
                          (v0+"ели")
                          (v0+"елене") ;
  mkV185 : Str -> VTable ;
  mkV185 base = let v0 = tk 3 base
                in mkVerb (v0+"оля")
                          (v0+"оли")
                          (v0+"лах")
                          (v0+"олех")
                          (v0+"лал")
                          (v0+"олел")
                          (v0+"лан")
                          (v0+"олещ")
                          (v0+"оли")
                          (v0+"олене") ;
  mkV186 : Str -> VTable ;
  mkV186 base = let v0 = tk 2 base
                in mkVerb (v0+"ам")
                          (v0+"а")
                          (v0+"ах")
                          (v0+"ах")
                          (v0+"ал")
                          (v0+"ал")
                          (v0+"ан")
                          (v0+"ащ")
                          (v0+"ай")
                          (v0+"ане") ;
  mkV187 : Str -> VTable ;
  mkV187 base = let v0 = tk 2 base
                in mkVerb (v0+"ям")
                          (v0+"я")
                          (v0+"ях")
                          (v0+"ях")
                          (v0+"ял")
                          (v0+"ял")
                          (v0+"ян")
                          (v0+"ящ")
                          (v0+"яй")
                          (v0+"яне") ;
  mkV188 : Str -> VTable ;
  mkV188 base = let v0 = tk 2 base;
                    vt = mkVerb base
                                (v0+"ае")
                                (v0+"аех")
                                (v0+"аех")
                                (v0+"ал")
                                (v0+"ал")
                                (v0+"ан")
                                (v0+"ащ")
                                (v0+"ай")
                                (v0+"ане")
                in table {
                     VAorist Sg (P2|P3) => v0+"аеше";
                     vform              => vt ! vform
                   } ;

  adjAdv : A -> Str -> A =
    \a,adv -> a ** {adv = adv} ;
}
