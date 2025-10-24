resource MorphoGla = open CatGla, ResGla, Predef in {

oper

mkA001 : Str -> A ;
mkA001 base =
  case base of {
    base_1+base_2@("l"|"r"|"n"|(?+?)) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2 ;
              ASg (Nom _) Fem => base_1+base_2 ;
              ASg (Dat _) Masc => base_1+base_2 ;
              ASg (Dat _) Fem => base_1+"i"+base_2 ;
              ASg Gen Masc => base_1+"i"+base_2 ;
              ASg Gen Fem => base_1+"i"+base_2+"e" ;
              APl => base_1+base_2
            } ;
        voc = table {
                Masc => base_1+"i"+base_2 ;
                Fem => base_1+base_2
              } ;
        compar = base_1+"i"+base_2+"e"
      };
    _ => error "Can't apply paradigm mkA001"
  } ;

mkA002 : Str -> A ;
mkA002 base =
  case base of {
    base_1@?+base_2+base_3@("r"|"m"|"s"|"rbh"|(?+?)) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+base_3 ;
              ASg (Nom _) Fem => base_1+"h"+base_2+base_3 ;
              ASg (Dat _) Masc => base_1+"h"+base_2+base_3 ;
              ASg (Dat _) Fem => base_1+"h"+base_2+"i"+base_3 ;
              ASg Gen Masc => base_1+"h"+base_2+"i"+base_3 ;
              ASg Gen Fem => base_1+"h"+base_2+"i"+base_3 ;
              APl => base_1+base_2+base_3
            } ;
        voc = table {
                Masc => base_1+"h"+base_2+"i"+base_3 ;
                Fem => base_1+"h"+base_2+"i"+base_3
              } ;
        compar = base_1+base_2+"i"+base_3+"e"
      };
    _ => error "Can't apply paradigm mkA002"
  } ;

mkA003 : Str -> A ;
mkA003 base =
  case base of {
    base_1@?+base_2 => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2 ;
              ASg (Nom _) Fem => base_1+"h"+base_2 ;
              ASg (Dat _) Masc => base_1+base_2 ;
              ASg (Dat _) Fem => base_1+"h"+base_2 ;
              ASg Gen Masc => base_1+"h"+base_2 ;
              ASg Gen Fem => base_1+base_2+"e" ;
              APl => base_1+base_2+"e"
            } ;
        voc = table {
                Masc => base_1+"h"+base_2 ;
                Fem => base_1+"h"+base_2
              } ;
        compar = base_1+base_2+"e"
      };
    _ => error "Can't apply paradigm mkA003"
  } ;

mkA004 : Str -> A ;
mkA004 base =
  case base of {
    base_1@?+base_2+"ea"+base_3@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+"ea"+base_3 ;
              ASg (Nom _) Fem => base_1+"h"+base_2+"ea"+base_3 ;
              ASg (Dat _) Masc => base_1+base_2+"ea"+base_3 ;
              ASg (Dat _) Fem => base_1+"h"+base_2+"i"+base_3 ;
              ASg Gen Masc => base_1+base_2+"i"+base_3 ;
              ASg Gen Fem => base_1+base_2+"i"+base_3+"e" ;
              APl => base_1+base_2+"ea"+base_3
            } ;
        voc = table {
                Masc => base_1+"h"+base_2+"i"+base_3 ;
                Fem => base_1+"h"+base_2+"ea"+base_3
              } ;
        compar = base_1+base_2+base_3+"e"
      };
    _ => error "Can't apply paradigm mkA004"
  } ;

mkA005 : Str -> A ;
mkA005 base =
  case base of {
    base_1+"ea"+base_2@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"ea"+base_2 ;
              ASg (Nom _) Fem => base_1+"ea"+base_2 ;
              ASg (Dat _) Masc => base_1+"ea"+base_2 ; --guessed
              ASg (Dat _) Fem => base_1+"hi"+base_2 ; --guessed
              ASg Gen Masc => base_1+"i"+base_2 ;
              ASg Gen Fem => base_1+"i"+base_2+"e" ;
              APl => base_1+"ea"+base_2
            } ;
        voc = table {
                Masc => base_1+"i"+base_2 ;
                Fem => base_1+"hea"+base_2 --guessed
              } ;
        compar = base_1+"i"+base_2+"e"
      };
    _ => error "Can't apply paradigm mkA005"
  } ;

mkA006 : Str -> A ;
mkA006 base =
  case base of {
    base_1@?+base_2+base_3@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+base_3 ;
              ASg (Nom _) Fem => base_1+"h"+base_2+base_3 ;
              ASg (Dat _) Masc => base_1+base_2+base_3 ;
              ASg (Dat _) Fem => base_1+"h"+base_2+"i"+base_3 ;
              ASg Gen Masc => base_1+base_2+"i"+base_3 ;
              ASg Gen Fem => base_1+base_2+"i"+base_3+"e" ;
              APl => base_1+base_2+base_3
            } ;
        voc = table {
                Masc => base_1+"h"+base_2+"i"+base_3 ;
                Fem => base_1+"h"+base_2+base_3
              } ;
        compar = base_1+base_2+"i"+base_3+"e"
      };
    _ => error "Can't apply paradigm mkA006"
  } ;

mkA007 : Str -> A ;
mkA007 base =
  case base of {
    base_1 => lin A
      { s = table {
              ASg (Nom _) Masc => base_1 ;
              ASg (Nom _) Fem => base_1 ;
              ASg (Dat _) Masc => base_1 ; --guessed
              ASg (Dat _) Fem => base_1+"e" ; --guessed
              ASg Gen Masc => base_1 ;
              ASg Gen Fem => base_1+"e" ;
              APl => base_1+"e"
            } ;
        voc = table {
                Masc => base_1 ; --guessed
                Fem => base_1 --guessed
              } ;
        compar = base_1+"e"
      };
    _ => error "Can't apply paradigm mkA007"
  } ;

mkA008 : Str -> A ;
mkA008 base =
  case base of {
    base_1@?+base_2+"ea"+base_3@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+"ea"+base_3 ;
              ASg (Nom _) Fem => base_1+"h"+base_2+"ea"+base_3 ;
              ASg (Dat _) Masc => base_1+"h"+base_2+"ea"+base_3 ;
              ASg (Dat _) Fem => base_1+"h"+base_2+"i"+base_3 ;
              ASg Gen Masc => base_1+base_2+"i"+base_3 ;
              ASg Gen Fem => base_1+base_2+"i"+base_3+"e" ;
              APl => base_1+base_2+"ea"+base_3
            } ;
        voc = table {
                Masc => base_1+"h"+base_2+"i"+base_3 ;
                Fem => base_1+"h"+base_2+"ea"+base_3
              } ;
        compar = base_1+base_2+"i"+base_3+"e"
      };
    _ => error "Can't apply paradigm mkA008"
  } ;

mkA009 : Str -> A ;
mkA009 base =
  case base of {
    base_1 => lin A
      { s = table {
              ASg (Nom _) Masc => base_1 ;
              ASg (Nom _) Fem => base_1 ;
              ASg (Dat _) Masc => base_1 ; --guessed
              ASg (Dat _) Fem => base_1+"e" ; --guessed
              ASg Gen Masc => base_1 ; --guessed
              ASg Gen Fem => base_1 ; --guessed
              APl => base_1+"a"
            } ;
        voc = table {
                Masc => base_1 ; --guessed
                Fem => base_1 --guessed
              } ;
        compar = base_1
      };
    _ => error "Can't apply paradigm mkA009"
  } ;

mkA010 : Str -> A ;
mkA010 base =
  case base of {
    base_1+"o"+base_2@("rch"|"rb"|"rm"|?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"o"+base_2 ;
              ASg (Nom _) Fem => base_1+"ho"+base_2 ;
              ASg (Dat _) Masc => base_1+"ho"+base_2 ;
              ASg (Dat _) Fem => base_1+"hui"+base_2 ;
              ASg Gen Masc => base_1+"hui"+base_2 ;
              ASg Gen Fem => base_1+"hui"+base_2 ;
              APl => base_1+"o"+base_2
            } ;
        voc = table {
                Masc => base_1+"hui"+base_2 ;
                Fem => base_1+"ho"+base_2
              } ;
        compar = base_1+"ui"+base_2+"e"
      };
    _ => error "Can't apply paradigm mkA010"
  } ;

mkA011 : Str -> A ;
mkA011 base =
  case base of {
    base_1+base_2@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2 ;
              ASg (Nom _) Fem => base_1+base_2 ;
              ASg (Dat _) Masc => base_1+base_2 ;
              ASg (Dat _) Fem => base_1+"i"+base_2 ;
              ASg Gen Masc => base_1+"i"+base_2 ;
              ASg Gen Fem => base_1+"i"+base_2 ;
              APl => base_1+base_2
            } ;
        voc = table {
                Masc => base_1+"i"+base_2 ;
                Fem => base_1+"i"+base_2
              } ;
        compar = base_1+"i"+base_2+"e"
      };
    _ => error "Can't apply paradigm mkA011"
  } ;

mkA012 : Str -> A ;
mkA012 base =
  case base of {
    base_1+"ea"+base_2@? => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"ea"+base_2 ;
              ASg (Nom _) Fem => base_1+"hea"+base_2 ;
              ASg (Dat _) Masc => base_1+"ea"+base_2 ;
              ASg (Dat _) Fem => base_1+"hi"+base_2 ;
              ASg Gen Masc => base_1+"i"+base_2 ;
              ASg Gen Fem => base_1+"i"+base_2+"e" ;
              APl => base_1+"ea"+base_2+"a"
            } ;
        voc = table {
                Masc => base_1+"hi"+base_2 ;
                Fem => base_1+"hea"+base_2
              } ;
        compar = base_1+"i"+base_2+"e"
      };
    _ => error "Can't apply paradigm mkA012"
  } ;

mkA013 : Str -> A ;
mkA013 base =
  case base of {
    base_1+base_2@(?+?+?+?)+base_3@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+base_3 ;
              ASg (Nom _) Fem => base_1+"h"+base_2+base_3 ;
              ASg (Dat _) Masc => base_1+"h"+base_2+base_3 ;
              ASg (Dat _) Fem => base_1+"h"+base_2+"i"+base_3 ;
              ASg Gen Masc => base_1+"h"+base_2+"i"+base_3 ;
              ASg Gen Fem => base_1+"h"+base_2+"i"+base_3 ;
              APl => base_1+"h"+base_2+base_3
            } ;
        voc = table {
                Masc => base_1+"h"+base_2+"i"+base_3 ;
                Fem => base_1+"h"+base_2+"i"+base_3
              } ;
        compar = base_1+base_2+"i"+base_3+"e" --guessed
      };
    _ => error "Can't apply paradigm mkA013"
  } ;

mkA014 : Str -> A ;
mkA014 base =
  case base of {
    base_1@?+base_2+base_3@("r"|"n"|(?+?)) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+base_3 ;
              ASg (Nom _) Fem => base_1+"h"+base_2+base_3 ;
              ASg (Dat _) Masc => base_1+base_2+base_3 ;
              ASg (Dat _) Fem => base_1+"h"+base_2+"i"+base_3 ;
              ASg Gen Masc => base_1+base_2+"i"+base_3 ;
              ASg Gen Fem => base_1+base_2+"i"+base_3+"e" ;
              APl => base_1+base_2+base_3+"a"
            } ;
        voc = table {
                Masc => base_1+"h"+base_2+"i"+base_3 ;
                Fem => base_1+"h"+base_2+base_3
              } ;
        compar = base_1+base_2+"i"+base_3+"e"
      };
    _ => error "Can't apply paradigm mkA014"
  } ;

mkA015 : Str -> A ;
mkA015 base =
  case base of {
    base_1@?+base_2 => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2 ;
              ASg (Nom _) Fem => base_1+"h"+base_2 ;
              ASg (Dat _) Masc => base_1+base_2 ;
              ASg (Dat _) Fem => base_1+"h"+base_2 ;
              ASg Gen Masc => base_1+"h"+base_2 ;
              ASg Gen Fem => base_1+base_2+"a" ;
              APl => base_1+base_2+"a"
            } ;
        voc = table {
                Masc => base_1+"h"+base_2 ;
                Fem => base_1+"h"+base_2
              } ;
        compar = base_1+base_2+"a"
      };
    _ => error "Can't apply paradigm mkA015"
  } ;

mkA016 : Str -> A ;
mkA016 base =
  case base of {
    base_1+"o"+base_2@(?+?)+"a"+base_3@? => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"o"+base_2+"a"+base_3 ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+"ui"+base_2+base_3+"e"
      };
    _ => error "Can't apply paradigm mkA016"
  } ;

mkA017 : Str -> A ;
mkA017 base =
  case base of {
    base_1+base_2@(?+?+?+?+?+?+?+?)+base_3@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+base_3 ;
              ASg (Nom _) Fem => base_1+"h"+base_2+base_3 ;
              ASg (Dat _) Masc => base_1+base_2+base_3 ;
              ASg (Dat _) Fem => base_1+"h"+base_2+"i"+base_3 ;
              ASg Gen Masc => base_1+"h"+base_2+"i"+base_3 ;
              ASg Gen Fem => base_1+base_2+"i"+base_3+"e" ;
              APl => base_1+base_2+base_3
            } ;
        voc = table {
                Masc => base_1+"h"+base_2+"i"+base_3 ;
                Fem => base_1+"h"+base_2+base_3
              } ;
        compar = base_1+base_2+"i"+base_3
      };
    _ => error "Can't apply paradigm mkA017"
  } ;

mkA018 : Str -> A ;
mkA018 base =
  case base of {
    base_1+base_2@?+"ea"+base_3@? => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+"ea"+base_3 ;
              ASg (Nom _) Fem => base_1+"h"+base_2+"ea"+base_3 ;
              ASg (Dat _) Masc => base_1+base_2+"ea"+base_3 ;
              ASg (Dat _) Fem => base_1+"h"+base_2+"i"+base_3 ;
              ASg Gen Masc => base_1+"h"+base_2+"i"+base_3 ;
              ASg Gen Fem => base_1+base_2+"i"+base_3+"e" ;
              APl => base_1+base_2+"ea"+base_3+"a"
            } ;
        voc = table {
                Masc => base_1+"h"+base_2+"i"+base_3 ;
                Fem => base_1+"h"+base_2+"ea"+base_3
              } ;
        compar = base_1+base_2+"i"+base_3+"e"
      };
    _ => error "Can't apply paradigm mkA018"
  } ;

mkA019 : Str -> A ;
mkA019 base =
  case base of {
    base_1+base_2@?+"un" => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+"un" ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+"èin"+base_2
      };
    _ => error "Can't apply paradigm mkA019"
  } ;

mkA020 : Str -> A ;
mkA020 base =
  case base of {
    base_1+base_2@(?+?+?+?+?+?+?+?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2 ;
              ASg (Nom _) Fem => base_1+"h"+base_2 ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => base_1+base_2 ;
              ASg Gen Fem => base_1+base_2+"e" ;
              APl => base_1+base_2+"e"
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+base_2+"e"
      };
    _ => error "Can't apply paradigm mkA020"
  } ;

mkA021 : Str -> A ;
mkA021 base =
  case base of {
    base_1@?+base_2 => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2 ;
              ASg (Nom _) Fem => base_1+"h"+base_2 ;
              ASg (Dat _) Masc => base_1+"h"+base_2 ;
              ASg (Dat _) Fem => base_1+"h"+base_2 ;
              ASg Gen Masc => base_1+"h"+base_2 ;
              ASg Gen Fem => base_1+"h"+base_2 ;
              APl => base_1+base_2
            } ;
        voc = table {
                Masc => base_1+"h"+base_2 ;
                Fem => base_1+"h"+base_2
              } ;
        compar = base_1+base_2+"a" --guessed
      };
    _ => error "Can't apply paradigm mkA021"
  } ;

mkA022 : Str -> A ;
mkA022 base =
  case base of {
    base_1+base_2@(?+?+?+?)+base_3@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+base_3 ;
              ASg (Nom _) Fem => base_1+"h"+base_2+base_3 ;
              ASg (Dat _) Masc => base_1+"h"+base_2+base_3 ;
              ASg (Dat _) Fem => base_1+"h"+base_2+"i"+base_3 ;
              ASg Gen Masc => base_1+"h"+base_2+"i"+base_3 ;
              ASg Gen Fem => base_1+"h"+base_2+"i"+base_3 ;
              APl => base_1+base_2+base_3
            } ;
        voc = table {
                Masc => base_1+"h"+base_2+"i"+base_3 ;
                Fem => base_1+"h"+base_2+base_3
              } ;
        compar = base_1+base_2+"i"+base_3+"e"
      };
    _ => error "Can't apply paradigm mkA022"
  } ;

mkA023 : Str -> A ;
mkA023 base =
  case base of {
    base_1+"_1" => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"_1" ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1
      };
    _ => error "Can't apply paradigm mkA023"
  } ;

mkA024 : Str -> A ;
mkA024 base =
  case base of {
    base_1+base_2@("dh"|"ch"|"rd"|?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2 ;
              ASg (Nom _) Fem => base_1+base_2 ;
              ASg (Dat _) Masc => base_1+base_2 ;
              ASg (Dat _) Fem => base_1+"i"+base_2 ;
              ASg Gen Masc => base_1+"i"+base_2 ;
              ASg Gen Fem => base_1+"i"+base_2+"e" ;
              APl => base_1+base_2+"a"
            } ;
        voc = table {
                Masc => base_1+"i"+base_2 ;
                Fem => base_1+base_2
              } ;
        compar = base_1+"i"+base_2+"e"
      };
    _ => error "Can't apply paradigm mkA024"
  } ;

mkA025 : Str -> A ;
mkA025 base =
  case base of {
    base_1@?+base_2+"a"+base_3@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+"a"+base_3 ;
              ASg (Nom _) Fem => base_1+"h"+base_2+"a"+base_3 ;
              ASg (Dat _) Masc => base_1+base_2+"a"+base_3 ;
              ASg (Dat _) Fem => base_1+"h"+base_2+"i"+base_3 ;
              ASg Gen Masc => base_1+base_2+"i"+base_3 ;
              ASg Gen Fem => base_1+base_2+"i"+base_3+"e" ;
              APl => base_1+base_2+"a"+base_3+"a"
            } ;
        voc = table {
                Masc => base_1+"h"+base_2+"i"+base_3 ;
                Fem => base_1+"h"+base_2+"a"+base_3
              } ;
        compar = base_1+base_2+"i"+base_3+"e"
      };
    _ => error "Can't apply paradigm mkA025"
  } ;

mkA026 : Str -> A ;
mkA026 base =
  case base of {
    base_1 => lin A
      { s = table {
              ASg (Nom _) Masc => base_1 ;
              ASg (Nom _) Fem => base_1 ; --guessed
              ASg (Dat _) Masc => base_1 ; --guessed
              ASg (Dat _) Fem => base_1+"e" ; --guessed
              ASg Gen Masc => base_1 ; --guessed
              ASg Gen Fem => base_1 ; --guessed
              APl => base_1+"a" --guessed
            } ;
        voc = table {
                Masc => base_1 ; --guessed
                Fem => base_1 --guessed
              } ;
        compar = base_1+"a"
      };
    _ => error "Can't apply paradigm mkA026"
  } ;

mkA027 : Str -> A ;
mkA027 base =
  case base of {
    base_1@?+base_2@?+"a"+base_3 => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+"a"+base_3 ;
              ASg (Nom _) Fem => base_1+"h"+base_2+"a"+base_3 ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+"è"+base_2+base_3+"e"
      };
    _ => error "Can't apply paradigm mkA027"
  } ;

mkA028 : Str -> A ;
mkA028 base =
  case base of {
    base_1@?+base_2 => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2 ;
              ASg (Nom _) Fem => base_1+"h"+base_2 ;
              ASg (Dat _) Masc => base_1+base_2 ;
              ASg (Dat _) Fem => base_1+"h"+base_2 ;
              ASg Gen Masc => base_1+"h"+base_2 ;
              ASg Gen Fem => base_1+base_2+"e" ;
              APl => base_1+base_2
            } ;
        voc = table {
                Masc => base_1+"h"+base_2 ;
                Fem => base_1+"h"+base_2
              } ;
        compar = base_1+base_2+"a" --guessed
      };
    _ => error "Can't apply paradigm mkA028"
  } ;

mkA029 : Str -> A ;
mkA029 base =
  case base of {
    base_1+"a"+base_2@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"a"+base_2 ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+"oi"+base_2+"e"
      };
    _ => error "Can't apply paradigm mkA029"
  } ;

mkA030 : Str -> A ;
mkA030 base =
  case base of {
    base_1@(?+?)+"a"+base_2 => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"a"+base_2 ;
              ASg (Nom _) Fem => base_1+"a"+base_2 ;
              ASg (Dat _) Masc => base_1+"a"+base_2 ; --guessed
              ASg (Dat _) Fem => base_1+"i"+base_2 ; --guessed
              ASg Gen Masc => base_1+"i"+base_2 ; --guessed
              ASg Gen Fem => base_1+"i"+base_2+"e" ; --guessed
              APl => base_1+"a"+base_2+"a" --guessed
            } ;
        voc = table {
                Masc => base_1+"i"+base_2 ; --guessed
                Fem => base_1+"a"+base_2 --guessed
              } ;
        compar = base_1+"i"+base_2+"e"
      };
    _ => error "Can't apply paradigm mkA030"
  } ;

mkA031 : Str -> A ;
mkA031 base =
  case base of {
    base_1+base_2@(?+?+?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2 ;
              ASg (Nom _) Fem => base_1+"h"+base_2 ;
              ASg (Dat _) Masc => base_1+"h"+base_2 ;
              ASg (Dat _) Fem => base_1+"h"+base_2 ;
              ASg Gen Masc => base_1+"h"+base_2 ;
              ASg Gen Fem => base_1+base_2+"e" ;
              APl => base_1+base_2
            } ;
        voc = table {
                Masc => base_1+"h"+base_2+"e" ;
                Fem => base_1+"h"+base_2+"e"
              } ;
        compar = base_1+base_2+"a" --guessed
      };
    _ => error "Can't apply paradigm mkA031"
  } ;

mkA032 : Str -> A ;
mkA032 base =
  case base of {
    base_1+base_2@?+base_3@(?+?)+"ai"+base_4@?+"n" => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+base_3+"ai"+base_4+"n" ;
              ASg (Nom _) Fem => base_1+"h"+base_2+base_3+"ai"+base_4+"n" ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+base_2+"i"+base_3+base_4+"e"
      };
    _ => error "Can't apply paradigm mkA032"
  } ;

mkA033 : Str -> A ;
mkA033 base =
  case base of {
    "d"+base_1+"n"+base_2@? => lin A
      { s = table {
              ASg (Nom _) Masc => "d"+base_1+"n"+base_2 ;
              ASg (Nom _) Fem => "dh"+base_1+"n"+base_2 ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = "mi"+base_1+"s"+base_2
      };
    _ => error "Can't apply paradigm mkA033"
  } ;

mkA034 : Str -> A ;
mkA034 base =
  case base of {
    base_1+"o"+base_2@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"o"+base_2 ;
              ASg (Nom _) Fem => base_1+"ho"+base_2 ;
              ASg (Dat _) Masc => base_1+"o"+base_2 ;
              ASg (Dat _) Fem => base_1+"hui"+base_2 ;
              ASg Gen Masc => base_1+"hui"+base_2 ;
              ASg Gen Fem => base_1+"ui"+base_2+"e" ;
              APl => base_1+"o"+base_2+"a"
            } ;
        voc = table {
                Masc => base_1+"hui"+base_2 ;
                Fem => base_1+"ho"+base_2
              } ;
        compar = base_1+"ui"+base_2+"e"
      };
    _ => error "Can't apply paradigm mkA034"
  } ;

mkA035 : Str -> A ;
mkA035 base =
  case base of {
    base_1+"o"+base_2@(?+?+?)+"a" => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"o"+base_2+"a" ;
              ASg (Nom _) Fem => base_1+"ho"+base_2+"a" ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+"ui"+base_2+"e"
      };
    _ => error "Can't apply paradigm mkA035"
  } ;

mkA036 : Str -> A ;
mkA036 base =
  case base of {
    "dr"+base_1+"ch" => lin A
      { s = table {
              ASg (Nom _) Masc => "dr"+base_1+"ch" ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = "mi"+base_1+"sa"
      };
    _ => error "Can't apply paradigm mkA036"
  } ;

mkA037 : Str -> A ;
mkA037 base =
  case base of {
    base_1+base_2@(?+?+?+?+?+?)+"ea"+base_3@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+"ea"+base_3 ;
              ASg (Nom _) Fem => base_1+"h"+base_2+"ea"+base_3 ;
              ASg (Dat _) Masc => base_1+"h"+base_2+"ea"+base_3 ;
              ASg (Dat _) Fem => base_1+"h"+base_2+"i"+base_3 ;
              ASg Gen Masc => base_1+base_2+"i"+base_3 ;
              ASg Gen Fem => base_1+base_2+"i"+base_3 ;
              APl => base_1+base_2+"ea"+base_3
            } ;
        voc = table {
                Masc => base_1+"h"+base_2+"i"+base_3 ;
                Fem => base_1+"h"+base_2+"i"+base_3
              } ;
        compar = base_1+base_2+"i"+base_3+"e"
      };
    _ => error "Can't apply paradigm mkA037"
  } ;

mkA038 : Str -> A ;
mkA038 base =
  case base of {
    base_1@?+base_2+base_3@("r"|(?+?)) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+base_3 ;
              ASg (Nom _) Fem => base_1+"h"+base_2+base_3 ;
              ASg (Dat _) Masc => base_1+"h"+base_2+base_3 ;
              ASg (Dat _) Fem => base_1+"h"+base_2+"i"+base_3 ;
              ASg Gen Masc => base_1+"h"+base_2+"i"+base_3 ;
              ASg Gen Fem => base_1+base_2+"i"+base_3 ;
              APl => base_1+base_2+base_3
            } ;
        voc = table {
                Masc => base_1+"h"+base_2+"i"+base_3 ;
                Fem => base_1+"h"+base_2+"i"+base_3
              } ;
        compar = base_1+"h"+base_2+"i"+base_3+"e"
      };
    _ => error "Can't apply paradigm mkA038"
  } ;

mkA039 : Str -> A ;
mkA039 base =
  case base of {
    base_1+base_2@(?+?+?)+"ic"+base_3@? => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+"ic"+base_3 ;
              ASg (Nom _) Fem => base_1+"h"+base_2+"ic"+base_3 ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+base_2+"g"+base_3+"e"
      };
    _ => error "Can't apply paradigm mkA039"
  } ;

mkA040 : Str -> A ;
mkA040 base =
  case base of {
    base_1+base_2@?+"as" => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+"as" ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+"s"+base_2
      };
    _ => error "Can't apply paradigm mkA040"
  } ;

mkA041 : Str -> A ;
mkA041 base =
  case base of {
    base_1+base_2@?+base_3@?+"a" => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+base_3+"a" ;
              ASg (Nom _) Fem => base_1+"h"+base_2+base_3+"a" ;
              ASg (Dat _) Masc => base_1+base_2+base_3+"a" ;
              ASg (Dat _) Fem => base_1+"h"+base_2+base_3+"a" ;
              ASg Gen Masc => base_1+"h"+base_2+base_3+"a" ;
              ASg Gen Fem => base_1+base_2+base_3+"a" ;
              APl => base_1+base_2+base_3+"a"
            } ;
        voc = table {
                Masc => base_1+"h"+base_2+base_3+"a" ;
                Fem => base_1+"h"+base_2+base_3+"a"
              } ;
        compar = base_1+"h"+base_2+"i"+base_3+"e"
      };
    _ => error "Can't apply paradigm mkA041"
  } ;

mkA042 : Str -> A ;
mkA042 base =
  case base of {
    base_1+base_2@(?+?+?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2 ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => base_1+base_2+"e" ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+"h"+base_2+"e"
      };
    _ => error "Can't apply paradigm mkA042"
  } ;

mkA043 : Str -> A ;
mkA043 base =
  case base of {
    base_1+"ur"+base_2@(?+?)+"d"+base_3@? => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"ur"+base_2+"d"+base_3 ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+"h"+base_2+base_3
      };
    _ => error "Can't apply paradigm mkA043"
  } ;

mkA044 : Str -> A ;
mkA044 base =
  case base of {
    base_1+"ur"+base_2@(?+?)+"t"+base_3@? => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"ur"+base_2+"t"+base_3 ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+"h"+base_2+base_3
      };
    _ => error "Can't apply paradigm mkA044"
  } ;

mkA045 : Str -> A ;
mkA045 base =
  case base of {
    base_1+base_2@("ch"|(?+?+?+?+?+?+?)) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2 ;
              ASg (Nom _) Fem => base_1+"h"+base_2 ;
              ASg (Dat _) Masc => base_1+base_2 ;
              ASg (Dat _) Fem => base_1+base_2 ;
              ASg Gen Masc => base_1+"h"+base_2 ;
              ASg Gen Fem => base_1+"h"+base_2 ;
              APl => base_1+base_2
            } ;
        voc = table {
                Masc => base_1+"h"+base_2 ;
                Fem => base_1+"h"+base_2
              } ;
        compar = base_1+"i"+base_2
      };
    _ => error "Can't apply paradigm mkA045"
  } ;

mkA046 : Str -> A ;
mkA046 base =
  case base of {
    base_1+"ea"+base_2@? => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"ea"+base_2 ;
              ASg (Nom _) Fem => base_1+"hea"+base_2 ;
              ASg (Dat _) Masc => base_1+"ea"+base_2 ;
              ASg (Dat _) Fem => base_1+"hi"+base_2 ;
              ASg Gen Masc => base_1+"hi"+base_2 ;
              ASg Gen Fem => base_1+"i"+base_2+"e" ;
              APl => base_1+"ea"+base_2+"a"
            } ;
        voc = table {
                Masc => base_1+"hi"+base_2 ;
                Fem => base_1+"hea"+base_2
              } ;
        compar = base_1+"i"+base_2+"e"
      };
    _ => error "Can't apply paradigm mkA046"
  } ;

mkA047 : Str -> A ;
mkA047 base =
  case base of {
    base_1+base_2@?+"ur" => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+"ur" ;
              ASg (Nom _) Fem => base_1+"h"+base_2+"ur" ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+"èir"+base_2
      };
    _ => error "Can't apply paradigm mkA047"
  } ;

mkA048 : Str -> A ;
mkA048 base =
  case base of {
    base_1+"eà"+base_2@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"eà"+base_2 ;
              ASg (Nom _) Fem => base_1+"heà"+base_2 ;
              ASg (Dat _) Masc => base_1+"eà"+base_2 ;
              ASg (Dat _) Fem => base_1+"heà"+base_2 ;
              ASg Gen Masc => base_1+"heà"+base_2 ;
              ASg Gen Fem => base_1+"eà"+base_2 ;
              APl => base_1+"eà"+base_2
            } ;
        voc = table {
                Masc => base_1+"heà"+base_2 ;
                Fem => base_1+"heà"+base_2
              } ;
        compar = base_1+"io"+base_2+"a"
      };
    _ => error "Can't apply paradigm mkA048"
  } ;

mkA049 : Str -> A ;
mkA049 base =
  case base of {
    base_1+base_2@(?+?+?+?)+"ea"+base_3@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+"ea"+base_3 ;
              ASg (Nom _) Fem => base_1+"h"+base_2+"ea"+base_3 ;
              ASg (Dat _) Masc => base_1+base_2+"ea"+base_3 ;
              ASg (Dat _) Fem => base_1+"h"+base_2+"i"+base_3 ;
              ASg Gen Masc => base_1+"h"+base_2+"i"+base_3 ;
              ASg Gen Fem => base_1+base_2+"i"+base_3+"e" ;
              APl => base_1+base_2+"ea"+base_3
            } ;
        voc = table {
                Masc => base_1+"h"+base_2+"i"+base_3 ;
                Fem => base_1+"h"+base_2+"ea"+base_3
              } ;
        compar = nonExist
      };
    _ => error "Can't apply paradigm mkA049"
  } ;

mkA050 : Str -> A ;
mkA050 base =
  case base of {
    base_1+base_2@?+"o"+base_3@?+"a"+base_4@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+"o"+base_3+"a"+base_4 ;
              ASg (Nom _) Fem => base_1+"h"+base_2+"o"+base_3+"a"+base_4 ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => base_1+base_2+base_3+"i"+base_4+"e" ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+base_2+base_3+"i"+base_4+"e"
      };
    _ => error "Can't apply paradigm mkA050"
  } ;

mkA051 : Str -> A ;
mkA051 base =
  case base of {
    base_1+base_2@(?+?)+base_3@? => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+base_3 ;
              ASg (Nom _) Fem => base_1+"h"+base_2+base_3 ;
              ASg (Dat _) Masc => base_1+base_2+base_3 ;
              ASg (Dat _) Fem => base_1+"h"+base_2+"i"+base_3 ;
              ASg Gen Masc => base_1+"h"+base_2+"i"+base_3 ;
              ASg Gen Fem => base_1+base_2+"i"+base_3+"e" ;
              APl => base_1+base_2+base_3+"a"
            } ;
        voc = table {
                Masc => base_1+"h"+base_2+"i"+base_3 ;
                Fem => base_1+"h"+base_2+base_3
              } ;
        compar = base_1+base_2+"i"+base_3+"e"
      };
    _ => error "Can't apply paradigm mkA051"
  } ;

mkA052 : Str -> A ;
mkA052 base =
  case base of {
    base_1+"o"+base_2@?+base_3@?+"id" => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"o"+base_2+base_3+"id" ;
              ASg (Nom _) Fem => base_1+"ho"+base_2+base_3+"id" ;
              ASg (Dat _) Masc => base_1+"o"+base_2+base_3+"id" ;
              ASg (Dat _) Fem => base_1+"ho"+base_2+base_3+"id" ;
              ASg Gen Masc => base_1+"ho"+base_2+base_3+"id" ;
              ASg Gen Fem => base_1+"o"+base_2+base_3+"ide" ;
              APl => base_1+"o"+base_2+base_3+"ide"
            } ;
        voc = table {
                Masc => base_1+"ho"+base_2+base_3+"id" ;
                Fem => base_1+"ho"+base_2+base_3+"id"
              } ;
        compar = base_1+base_2+"o"+base_3+"ra"
      };
    _ => error "Can't apply paradigm mkA052"
  } ;

mkA053 : Str -> A ;
mkA053 base =
  case base of {
    base_1+base_2@(?+?)+"a"+base_3@(?+?+?)+base_4@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+"a"+base_3+base_4 ;
              ASg (Nom _) Fem => base_1+"h"+base_2+"a"+base_3+base_4 ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+base_2+base_3+"i"+base_4+"e"
      };
    _ => error "Can't apply paradigm mkA053"
  } ;

mkA054 : Str -> A ;
mkA054 base =
  case base of {
    base_1+"ri"+base_2@?+"n" => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"ri"+base_2+"n" ;
              ASg (Nom _) Fem => base_1+"hri"+base_2+"n" ;
              ASg (Dat _) Masc => base_1+"hri"+base_2+"n" ;
              ASg (Dat _) Fem => base_1+"hri"+base_2+"n" ;
              ASg Gen Masc => base_1+"hri"+base_2+"n" ;
              ASg Gen Fem => base_1+"hri"+base_2+"n" ;
              APl => base_1+"la"+base_2
            } ;
        voc = table {
                Masc => base_1+"hri"+base_2+"n" ;
                Fem => base_1+"hri"+base_2+"n"
              } ;
        compar = nonExist
      };
    _ => error "Can't apply paradigm mkA054"
  } ;

mkA055 : Str -> A ;
mkA055 base =
  case base of {
    base_1+base_2@(?+?+?+?+?)+base_3@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+base_3 ;
              ASg (Nom _) Fem => base_1+"h"+base_2+base_3 ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => base_1+base_2+"i"+base_3 ;
              ASg Gen Fem => nonExist ;
              APl => base_1+"h"+base_2+base_3
            } ;
        voc = table {
                Masc => base_1+"h"+base_2+"i"+base_3 ;
                Fem => base_1+"h"+base_2+base_3
              } ;
        compar = base_1+base_2+"i"+base_3+"e"
      };
    _ => error "Can't apply paradigm mkA055"
  } ;

mkA056 : Str -> A ;
mkA056 base =
  case base of {
    base_1@?+base_2@(?+?)+base_3+"a" => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+base_3+"a" ;
              ASg (Nom _) Fem => base_1+"h"+base_2+base_3+"a" ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+base_2+"i"+base_3+"e"
      };
    _ => error "Can't apply paradigm mkA056"
  } ;

mkA057 : Str -> A ;
mkA057 base =
  case base of {
    base_1+"ea"+base_2@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"ea"+base_2 ;
              ASg (Nom _) Fem => base_1+"ea"+base_2 ;
              ASg (Dat _) Masc => base_1+"ea"+base_2 ;
              ASg (Dat _) Fem => base_1+"i"+base_2 ;
              ASg Gen Masc => base_1+"i"+base_2 ;
              ASg Gen Fem => base_1+"i"+base_2 ;
              APl => base_1+"ea"+base_2
            } ;
        voc = table {
                Masc => base_1+"i"+base_2 ;
                Fem => base_1+"ea"+base_2
              } ;
        compar = base_1+"i"+base_2+"e"
      };
    _ => error "Can't apply paradigm mkA057"
  } ;

mkA058 : Str -> A ;
mkA058 base =
  case base of {
    "ionmh"+base_1+"i"+base_2@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => "ionmh"+base_1+"i"+base_2 ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+base_2+"sa"
      };
    _ => error "Can't apply paradigm mkA058"
  } ;

mkA059 : Str -> A ;
mkA059 base =
  case base of {
    "ionmhui"+base_1 => lin A
      { s = table {
              ASg (Nom _) Masc => "ionmhui"+base_1 ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = "a"+base_1+"sa"
      };
    _ => error "Can't apply paradigm mkA059"
  } ;

mkA060 : Str -> A ;
mkA060 base =
  case base of {
    base_1+"nn" => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"nn" ;
              ASg (Nom _) Fem => base_1+"nn" ;
              ASg (Dat _) Masc => base_1+"nn" ;
              ASg (Dat _) Fem => base_1+"inn" ;
              ASg Gen Masc => base_1+"inn" ;
              ASg Gen Fem => base_1+"inne" ;
              APl => base_1+"nna"
            } ;
        voc = table {
                Masc => base_1+"inn" ;
                Fem => base_1+"nn"
              } ;
        compar = base_1
      };
    _ => error "Can't apply paradigm mkA060"
  } ;

mkA061 : Str -> A ;
mkA061 base =
  case base of {
    "mat"+base_1 => lin A
      { s = table {
              ASg (Nom _) Masc => "mat"+base_1 ;
              ASg (Nom _) Fem => "m"+base_1+"ath" ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => "mait"+base_1 ;
              ASg Gen Fem => "mait"+base_1+"e" ;
              APl => "mat"+base_1+"a"
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = "f"+base_1+"eàrr"
      };
    _ => error "Can't apply paradigm mkA061"
  } ;

mkA062 : Str -> A ;
mkA062 base =
  case base of {
    base_1+"i"+base_2@?+"i"+base_3@? => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"i"+base_2+"i"+base_3 ;
              ASg (Nom _) Fem => base_1+"hi"+base_2+"i"+base_3 ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => base_1+"hi"+base_2+"i"+base_3 ;
              ASg Gen Fem => base_1+"ì"+base_2+base_3+"e" ;
              APl => base_1+"ì"+base_2+base_3+"e"
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+"ì"+base_2+base_3+"e"
      };
    _ => error "Can't apply paradigm mkA062"
  } ;

mkA063 : Str -> A ;
mkA063 base =
  case base of {
    base_1+base_2@(?+?)+"_1" => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+"_1" ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => base_1+"i"+base_2 ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+"i"+base_2+"e"
      };
    _ => error "Can't apply paradigm mkA063"
  } ;

mkA064 : Str -> A ;
mkA064 base =
  case base of {
    base_1+"òr" => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"òr" ;
              ASg (Nom _) Fem => base_1+"hòr" ;
              ASg (Dat _) Masc => base_1+"òr" ;
              ASg (Dat _) Fem => base_1+"hòir" ;
              ASg Gen Masc => base_1+"hòir" ;
              ASg Gen Fem => base_1+"òire" ;
              APl => base_1+"òra"
            } ;
        voc = table {
                Masc => base_1+"hòir" ;
                Fem => base_1+"hòr"
              } ;
        compar = base_1+"otha"
      };
    _ => error "Can't apply paradigm mkA064"
  } ;

mkA065 : Str -> A ;
mkA065 base =
  case base of {
    base_1+base_2@(?+?+?+?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2 ;
              ASg (Nom _) Fem => base_1+"h"+base_2 ;
              ASg (Dat _) Masc => base_1+"h"+base_2 ;
              ASg (Dat _) Fem => base_1+"h"+base_2 ;
              ASg Gen Masc => base_1+"h"+base_2 ;
              ASg Gen Fem => base_1+base_2+"e" ;
              APl => base_1+base_2+"e"
            } ;
        voc = table {
                Masc => base_1+"h"+base_2 ;
                Fem => base_1+"h"+base_2
              } ;
        compar = base_1+base_2+"e"
      };
    _ => error "Can't apply paradigm mkA065"
  } ;

mkA066 : Str -> A ;
mkA066 base =
  case base of {
    base_1+"ór" => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"ór" ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+"otha"
      };
    _ => error "Can't apply paradigm mkA066"
  } ;

mkA067 : Str -> A ;
mkA067 base =
  case base of {
    "neo-"+base_1+"h"+base_2@(?+?+?)+base_3@? => lin A
      { s = table {
              ASg (Nom _) Masc => "neo-"+base_1+"h"+base_2+base_3 ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => base_1+base_2+base_3+"a"
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+base_2+"i"+base_3+"e"
      };
    _ => error "Can't apply paradigm mkA067"
  } ;

mkA068 : Str -> A ;
mkA068 base =
  case base of {
    base_1 => lin A
      { s = table {
              ASg (Nom _) Masc => base_1 ;
              ASg (Nom _) Fem => base_1 ;
              ASg (Dat _) Masc => base_1 ;
              ASg (Dat _) Fem => base_1+"e" ;
              ASg Gen Masc => base_1 ;
              ASg Gen Fem => base_1 ;
              APl => base_1
            } ;
        voc = table {
                Masc => base_1 ;
                Fem => base_1
              } ;
        compar = nonExist
      };
    _ => error "Can't apply paradigm mkA068"
  } ;

mkA069 : Str -> A ;
mkA069 base =
  case base of {
    base_1 => lin A
      { s = table {
              ASg (Nom _) Masc => base_1 ;
              ASg (Nom _) Fem => base_1 ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => base_1 ;
              ASg Gen Fem => base_1+"e" ;
              APl => base_1+"e"
            } ;
        voc = table {
                Masc => base_1 ;
                Fem => base_1
              } ;
        compar = base_1+"e"
      };
    _ => error "Can't apply paradigm mkA069"
  } ;

mkA070 : Str -> A ;
mkA070 base =
  case base of {
    base_1+"lc" => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"lc" ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = "mi"+base_1+"sa"
      };
    _ => error "Can't apply paradigm mkA070"
  } ;

mkA071 : Str -> A ;
mkA071 base =
  case base of {
    base_1+"a"+base_2@(?+?)+base_3@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"a"+base_2+base_3 ;
              ASg (Nom _) Fem => base_1+"a"+base_2+base_3 ;
              ASg (Dat _) Masc => base_1+"a"+base_2+base_3 ;
              ASg (Dat _) Fem => base_1+"a"+base_2+"i"+base_3 ;
              ASg Gen Masc => base_1+"a"+base_2+"i"+base_3 ;
              ASg Gen Fem => base_1+"o"+base_2+"i"+base_3+"e" ;
              APl => base_1+"a"+base_2+base_3
            } ;
        voc = table {
                Masc => base_1+"a"+base_2+"i"+base_3 ;
                Fem => base_1+"a"+base_2+base_3
              } ;
        compar = nonExist
      };
    _ => error "Can't apply paradigm mkA071"
  } ;

mkA072 : Str -> A ;
mkA072 base =
  case base of {
    base_1+base_2@?+"o"+base_3@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+"o"+base_3 ;
              ASg (Nom _) Fem => base_1+"h"+base_2+"o"+base_3 ;
              ASg (Dat _) Masc => base_1+base_2+"o"+base_3 ;
              ASg (Dat _) Fem => base_1+"h"+base_2+"ui"+base_3 ;
              ASg Gen Masc => base_1+base_2+"ui"+base_3 ;
              ASg Gen Fem => base_1+base_2+"ui"+base_3+"e" ;
              APl => base_1+base_2+"o"+base_3+"a"
            } ;
        voc = table {
                Masc => base_1+"h"+base_2+"ui"+base_3 ;
                Fem => base_1+"h"+base_2+"o"+base_3
              } ;
        compar = base_1+base_2+"ui"+base_3+"e"
      };
    _ => error "Can't apply paradigm mkA072"
  } ;

mkA073 : Str -> A ;
mkA073 base =
  case base of {
    base_1+base_2@(?+?+?+?+?+?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2 ;
              ASg (Nom _) Fem => base_1+"h"+base_2 ;
              ASg (Dat _) Masc => base_1+base_2 ;
              ASg (Dat _) Fem => base_1+"h"+base_2 ;
              ASg Gen Masc => base_1+"h"+base_2 ;
              ASg Gen Fem => base_1+"h"+base_2 ;
              APl => base_1+base_2
            } ;
        voc = table {
                Masc => base_1+"h"+base_2 ;
                Fem => base_1+"h"+base_2
              } ;
        compar = base_1+base_2+"e"
      };
    _ => error "Can't apply paradigm mkA073"
  } ;

mkA074 : Str -> A ;
mkA074 base =
  case base of {
    base_1+"a"+base_2@? => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"a"+base_2 ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+base_2+"a"
      };
    _ => error "Can't apply paradigm mkA074"
  } ;

mkA075 : Str -> A ;
mkA075 base =
  case base of {
    base_1 => lin A
      { s = table {
              ASg (Nom _) Masc => base_1 ;
              ASg (Nom _) Fem => base_1 ;
              ASg (Dat _) Masc => base_1 ;
              ASg (Dat _) Fem => base_1 ;
              ASg Gen Masc => base_1 ;
              ASg Gen Fem => base_1 ;
              APl => base_1
            } ;
        voc = table {
                Masc => base_1 ;
                Fem => base_1
              } ;
        compar = base_1+"e"
      };
    _ => error "Can't apply paradigm mkA075"
  } ;

mkA076 : Str -> A ;
mkA076 base =
  case base of {
    base_1+"è"+base_2@(?+?)+"ea"+base_3@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"è"+base_2+"ea"+base_3 ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => base_1+"è"+base_2+"i"+base_3 ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+"é"+base_2+"i"+base_3+"e"
      };
    _ => error "Can't apply paradigm mkA076"
  } ;

mkA077 : Str -> A ;
mkA077 base =
  case base of {
    base_1+base_2@("g"|(?+?)) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2 ;
              ASg (Nom _) Fem => base_1+base_2 ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => base_1+"i"+base_2 ;
              ASg Gen Fem => base_1+"i"+base_2+"e" ;
              APl => base_1+base_2+"a"
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+"i"+base_2+"e"
      };
    _ => error "Can't apply paradigm mkA077"
  } ;

mkA078 : Str -> A ;
mkA078 base =
  case base of {
    base_1+base_2@?+"an" => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+"an" ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+"in"+base_2
      };
    _ => error "Can't apply paradigm mkA078"
  } ;

mkA079 : Str -> A ;
mkA079 base =
  case base of {
    base_1+"a"+base_2@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"a"+base_2 ;
              ASg (Nom _) Fem => base_1+"a"+base_2 ;
              ASg (Dat _) Masc => base_1+"a"+base_2 ;
              ASg (Dat _) Fem => base_1+"i"+base_2 ;
              ASg Gen Masc => base_1+"i"+base_2 ;
              ASg Gen Fem => base_1+"i"+base_2+"e" ;
              APl => base_1+"a"+base_2+"a"
            } ;
        voc = table {
                Masc => base_1+"i"+base_2 ;
                Fem => base_1+"a"+base_2
              } ;
        compar = base_1+"i"+base_2+"e"
      };
    _ => error "Can't apply paradigm mkA079"
  } ;

mkA080 : Str -> A ;
mkA080 base =
  case base of {
    base_1+base_2@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2 ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => base_1+base_2 ;
              ASg Gen Fem => base_1+base_2 ;
              APl => base_1+base_2
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+"i"+base_2+"e"
      };
    _ => error "Can't apply paradigm mkA080"
  } ;

mkA081 : Str -> A ;
mkA081 base =
  case base of {
    base_1+"ea"+base_2@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"ea"+base_2 ;
              ASg (Nom _) Fem => base_1+"ea"+base_2 ;
              ASg (Dat _) Masc => base_1+"ea"+base_2 ;
              ASg (Dat _) Fem => base_1+"i"+base_2 ;
              ASg Gen Masc => base_1+"i"+base_2 ;
              ASg Gen Fem => base_1+"i"+base_2+"e" ;
              APl => base_1+"ea"+base_2
            } ;
        voc = table {
                Masc => base_1+"i"+base_2 ;
                Fem => base_1+"ea"+base_2
              } ;
        compar = nonExist
      };
    _ => error "Can't apply paradigm mkA081"
  } ;

mkA082 : Str -> A ;
mkA082 base =
  case base of {
    base_1 => lin A
      { s = table {
              ASg (Nom _) Masc => base_1 ;
              ASg (Nom _) Fem => base_1 ;
              ASg (Dat _) Masc => base_1 ;
              ASg (Dat _) Fem => base_1 ;
              ASg Gen Masc => base_1 ;
              ASg Gen Fem => base_1+"e" ;
              APl => base_1
            } ;
        voc = table {
                Masc => base_1 ;
                Fem => base_1
              } ;
        compar = nonExist
      };
    _ => error "Can't apply paradigm mkA082"
  } ;

mkA083 : Str -> A ;
mkA083 base =
  case base of {
    base_1+base_2@?+"inn" => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+"inn" ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+"n"+base_2
      };
    _ => error "Can't apply paradigm mkA083"
  } ;

mkA084 : Str -> A ;
mkA084 base =
  case base of {
    base_1@(?+?)+base_2+"a" => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+"a" ;
              ASg (Nom _) Fem => base_1+base_2+"a" ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+"i"+base_2+"e"
      };
    _ => error "Can't apply paradigm mkA084"
  } ;

mkA085 : Str -> A ;
mkA085 base =
  case base of {
    base_1+base_2@?+base_3@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+base_3 ;
              ASg (Nom _) Fem => base_1+"h"+base_2+base_3 ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+base_2+"o"+base_3+"a"
      };
    _ => error "Can't apply paradigm mkA085"
  } ;

mkA086 : Str -> A ;
mkA086 base =
  case base of {
    base_1+base_2@?+"u"+base_3@(?+?) => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+"u"+base_3 ;
              ASg (Nom _) Fem => base_1+"h"+base_2+"u"+base_3 ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+base_2+base_3+"e"
      };
    _ => error "Can't apply paradigm mkA086"
  } ;

mkA087 : Str -> A ;
mkA087 base =
  case base of {
    base_1+base_2@?+base_3@?+"un" => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+base_3+"un" ;
              ASg (Nom _) Fem => base_1+"h"+base_2+base_3+"un" ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+base_2+"èin"+base_3
      };
    _ => error "Can't apply paradigm mkA087"
  } ;

mkA088 : Str -> A ;
mkA088 base =
  case base of {
    base_1 => lin A
      { s = table {
              ASg (Nom _) Masc => base_1 ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => base_1 ;
              ASg Gen Fem => nonExist ;
              APl => base_1+"an"
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = nonExist
      };
    _ => error "Can't apply paradigm mkA088"
  } ;

mkA089 : Str -> A ;
mkA089 base =
  case base of {
    base_1+base_2@?+"ai"+base_3@?+"n" => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+"ai"+base_3+"n" ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+"i"+base_2+base_3+"e"
      };
    _ => error "Can't apply paradigm mkA089"
  } ;

mkA090 : Str -> A ;
mkA090 base =
  case base of {
    base_1+"o"+base_2@?+"a"+base_3@? => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+"o"+base_2+"a"+base_3 ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+base_2+base_3+"e"
      };
    _ => error "Can't apply paradigm mkA090"
  } ;

mkA091 : Str -> A ;
mkA091 base =
  case base of {
    base_1+base_2@?+"al" => lin A
      { s = table {
              ASg (Nom _) Masc => base_1+base_2+"al" ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = base_1+"l"+base_2
      };
    _ => error "Can't apply paradigm mkA091"
  } ;

mkA092 : Str -> A ;
mkA092 base =
  case base of {
    "ù"+base_1+base_2@? => lin A
      { s = table {
              ASg (Nom _) Masc => "ù"+base_1+base_2 ;
              ASg (Nom _) Fem => nonExist ;
              ASg (Dat _) Masc => nonExist ;
              ASg (Dat _) Fem => nonExist ;
              ASg Gen Masc => nonExist ;
              ASg Gen Fem => nonExist ;
              APl => nonExist
            } ;
        voc = table {
                Masc => nonExist ;
                Fem => nonExist
              } ;
        compar = "u"+base_1+"i"+base_2+"e"
      };
    _ => error "Can't apply paradigm mkA092"
  } ;

reg4N' : (_,_,_,_ : Str) -> Gender -> LinN ;
reg4N' nom pl gen pal g =
  lin N
      { s = table {
              Nom _ => table {
                       Indef => table {
                                  Sg => nom ;
                                  Pl => pl
                                } ;
                       Def => table {
                                Sg => nom ;
                                Pl => pl
                              }
                     } ;
              Dat _ => table {
                       Indef => table {
                                  Sg => nom ;
                                  Pl => pl
                                } ;
                       Def => table {
                                Sg => lenite nom ;
                                Pl => pl
                              }
                     } ;
              Gen => table {
                       Indef => table {
                                  Sg => gen ;
                                  Pl => pl
                                } ;
                       Def => table {
                                Sg => case g of {
                                        Fem  => gen ;
                                        Masc => lenite pal
                                      } ;
                                Pl => pl
                              }
                     }
            } ;
        voc = table {
                Sg => lenite pal ;
                Pl => pl
              } ;
        g = g
      };

mkN001 : Str -> LinN ;
mkN001 base = 
  let pal = palatalise base
  in mk5N base base pal (base+"an") pal Masc ;

mkN002 : Str -> LinN ;
mkN002 base =
  let pal = palatalise base
  in mk5N base base pal pal pal Masc ;

mkN004 : Str -> LinN ;
mkN004 base = mk5N base base base (base+"an") (palatalise base) Fem ;

mkN005 : Str -> LinN ;
mkN005 base =
  case base of {
    base_1+base_2@(?+?)+"r" => 
         let pal = palatalise base
         in mk5N base base pal (base_1+"r"+base_2+"chean") pal Masc ;
    _ => error "Can't apply paradigm mkN005"
  } ;

mkN007 : Str -> LinN ;
mkN007 base = mk5N base base (base+"e") (base+"ean") (palatalise base) Fem ;

mkN009 : Str -> LinN ;
mkN009 base = mk5N base base base (base+"an") (palatalise base) Masc ;

mkN010 : Str -> LinN ;
mkN010 base = mk5N base base (base+"e") (base+"ean") (palatalise base) Masc ;

mkN011 : Str -> LinN ;
mkN011 base = 
  case base of {
    base_1@(_+("e"|"è"))+"a"+base_2@("sg"|"mh"|"nd"|"nn"|"bh"|"rt"|"nt"|"rg"|?) => mk5N base base (base_1+"i"+base_2) (base+"an") (base_1+"i"+base_2) Masc ;
    _ => error "Can't apply paradigm mkN011"
  } ;

mkN013 : Str -> LinN ;
mkN013 base = 
  case base of {
    base_1+"ia" => mk5N base base (base_1+"hè") (base+"than") (palatalise base) Masc ;
    _ => error "Can't apply paradigm mkN013"
  } ;

mkN014 : Str -> LinN ;
mkN014 base = 
  case base of {
    base_1+base_2@(?+?) => mk5N base base (base_1+"i"+base_2+"e") base (palatalise base) Masc ;
    _ => error "Can't apply paradigm mkN014"
  } ;

mkN015 : Str -> LinN ;
mkN015 base = 
  case base of {
    base_1+"a"+base_2@("ng"|?) => mk5N base base (base_1+"i"+base_2) (base_1+"i"+base_2) (palatalise base) Masc ;
    _ => error "Can't apply paradigm mkN015"
  } ;

mkN016 : Str -> LinN ;
mkN016 base =
  case base of {
    base_1+"a"+base_2@(?+?) => mk5N base base (base_1+"oi"+base_2) (base_1+"oi"+base_2) (palatalise base) Masc ;
    _ => error "Can't apply paradigm mkN016"
  } ;

mkN017 : Str -> LinN ;
mkN017 base = 
  let pal = palatalise base
  in mk5N base base pal (pal+"ean") pal Masc ;

mkN019 : Str -> LinN ;
mkN019 base = 
  case base of {
    base_1+"a"+base_2@("rc"|"ch"|"ng"|"rg"|"lg"|?) => mk5N base base (base_1+"i"+base_2+"e") (base+"an") (base_1+"i"+base_2) Fem ;
    _ => error "Can't apply paradigm mkN019"
  } ;

mkN020 : Str -> LinN ;
mkN020 base = 
  let pal = palatalise base ;
      len = lenite base
  in mkNoun base pal base pal base pal base pal pal len pal base (lenite pal) (len+"a") Masc ;

mkN021 : Str -> LinN ;
mkN021 base = mk5N base base (base+"a") (base+"achan") (palatalise base) Masc ;

mkN022 : Str -> LinN ;
mkN022 base = reg4N' base (base+"chan") base (palatalise base) Masc ;

mkN023 : Str -> LinN ;
mkN023 base = 
  let pal = palatalise base
  in mk5N base base pal (base+"an") pal Fem ;

mkN024 : Str -> LinN ;
mkN024 base = 
  case base of {
    base_1@?+base_2+"ainn" => mk5N base base (base_1+"i"+base_2+"ne") (base_1+"i"+base_2+"nichean") (palatalise base) Fem ;
    _ => error "Can't apply paradigm mkN024"
  } ;

mkN025 : Str -> LinN ;
mkN025 base = mk5N base base base (base+"ean") (palatalise base) Fem ;

mkN026 : Str -> LinN ;
mkN026 base =
  case base of {
    base_1+base_2@?+"ir" => mk5N base base (base_1+"r"+base_2+"ch")  (base_1+"r"+base_2+"ichean") (palatalise base) Fem ;
    _ => error "Can't apply paradigm mkN026"
  } ;

mkN027 : Str -> LinN ;
mkN027 base = mk5N base base base (base+"annan") (palatalise base) Fem ;

mkN028 : Str -> LinN ;
mkN028 base = mk5N base base base (base+"ean") (palatalise base) Masc ;

mkN029 : Str -> LinN ;
mkN029 base = 
  case base of {
    base_1+"as" => 
         let pal = palatalise base
         in mk5N base base pal (base_1+"an") pal Masc ;
    _ => error "Can't apply paradigm mkN029"
  } ;

mkN030 : Str -> LinN ;
mkN030 base =
  let pal = palatalise base
  in mk5N base base (pal+"e") (base+"an") pal Fem ;

mkN031 : Str -> LinN ;
mkN031 base = 
  case base of {
    "adha" => mk5N base base base "àinean" (palatalise base) Masc ;
    _ => error "Can't apply paradigm mkN031"
  } ;

mkN032 : Str -> LinN ;
mkN032 base = mk5N base base (palatalise base+"e") base (palatalise base) Fem ;

mkN033 : Str -> LinN ;
mkN033 base = mk5N base base (palatalise base) (base+"aichean") (palatalise base) Masc ;

mkN034 : Str -> LinN ;
mkN034 base = mk5N base base (palatalise base) (palatalise base+"an") (palatalise base) Masc ;

mkN035 : Str -> LinN ;
mkN035 base = mk5N base base (base+"aig") (base+"an") (palatalise base) Masc ;

mkN036 : Str -> LinN ;
mkN036 base = mkNoun base (palatalise base+"ean") base (palatalise base+"ean") (palatalise base) (palatalise base+"ean") (palatalise base) (palatalise base+"ean") (palatalise base+"e") base (palatalise base+"e") base (lenite base) (lenite base+"a") Fem ;

mkN037 : Str -> LinN ;
mkN037 base =
  case base of {
    "aghann" => mk5N base base "aighne" (base+"an") (palatalise base) Fem ;
    _ => error "Can't apply paradigm mkN037"
  } ;

mkN038 : Str -> LinN ;
mkN038 base = reg4N' base (base+"an") base (palatalise base) Masc ;

mkN039 : Str -> LinN ;
mkN039 base =
  case base of {
    base_1@(_+"e")+"a"+base_2@? => reg4N' base (base+"an") (base_1+"i"+base_2) (base_1+"i"+base_2) Masc ;
    _ => error "Can't apply paradigm mkN039"
  } ;

mkN040 : Str -> LinN ;
mkN040 base = reg4N' base (base+"ean") (base+"e") base Fem ;

mkN041 : Str -> LinN ;
mkN041 base = mkNoun base (base+"ean") base (base+"ean") base (base+"ean") base (base+"ean") base (base+"ean") (base+"e") (base+"ean") (lenite base) (base+"ean") Fem ;

mkN042 : Str -> LinN ;
mkN042 base = 
  case base of {
    base_1+"ea"+base_2@(?+?) => mk5N base base base (base_1+"i"+base_2) (palatalise base) Masc ;
    _ => error "Can't apply paradigm mkN042"
  } ;

mkN043 : Str -> LinN ;
mkN043 base =
  case base of {
    base_1@(_+("e"|"è"))+"a"+base_2@("bh"|"rt"|"lg"|"mh"|?) => mk5N base base (base_1+"i"+base_2) (base+"an") (palatalise base) Masc ;
    _ => error "Can't apply paradigm mkN043"
  } ;

mkN044 : Str -> LinN ;
mkN044 base =
  case base of {
    base_1+"iach" => mk5N base base (base_1+"eich") (base_1+"iachan") (palatalise base) Masc ;
    _ => error "Can't apply paradigm mkN044"
  } ;

mkN045 : Str -> LinN ;
mkN045 base = mk5N base base (palatalise base) (palatalise base) (palatalise base) Fem ;

mkN046 : Str -> LinN ;
mkN046 base = mk5N base base base (base+"n") (palatalise base) Masc ;

mkN047 : Str -> LinN ;
mkN047 base =
  case base of {
    base_1+"o"+base_2@("n"|"d"|"ll") => mk5N base base (base_1+base_2) (base+"tan") (palatalise base) Masc ;
    _ => error "Can't apply paradigm mkN047"
  } ;

mkN048 : Str -> LinN ;
mkN048 base = mk5N base base base base (palatalise base) Fem ;

mkN049 : Str -> LinN ;
mkN049 base =
  case base of {
    base_1+"i"+base_2@? => mk5N base base (base_1+base_2+"e") (base_1+base_2+"ean") (palatalise base) Fem ;
    _ => error "Can't apply paradigm mkN049"
  } ;

mkN050 : Str -> LinN ;
mkN050 base =
  case base of {
    base_1+"ea"+base_2@(?+?) => mk5N base base (base_1+"io"+base_2+"a") (base+"an") (palatalise base) Fem ;
    _ => error "Can't apply paradigm mkN050"
  } ;

mkN051 : Str -> LinN ;
mkN051 base =
  case base of {
    ("a"|"o")+base_1 => mk5N base base ("ui"+base_1) ("ui"+base_1) (palatalise base) Masc ;
    _ => error "Can't apply paradigm mkN051"
  } ;

mkN052 : Str -> LinN ;
mkN052 base =
  case base of {
    "alp" => mk5N base base "ailp" (base+"a") (palatalise base) Fem ;
    _ => error "Can't apply paradigm mkN052"
  } ;

mkN053 : Str -> LinN ;
mkN053 base =
  case base of {
    "alt" => mk5N base base "uilt" "altan" (palatalise base) Masc ;
    _ => error "Can't apply paradigm mkN053"
  } ;

mkN054 : Str -> LinN ;
mkN054 base = mk5N base base (palatalise base) (palatalise base+"ean") (palatalise base) Fem ;

mkN055 : Str -> LinN ;
mkN055 base = 
  case base of {
    base_1+"i"+base_2@? => mk5N base base (base_1+base_2+"ach") (base+"ean") (palatalise base) Fem ;
    _ => error "Can't apply paradigm mkN055"
  } ;

mkN056 : Str -> LinN ;
mkN056 base =
  case base of {
    base_1+"am" => mk5N base base (base_1+"ma") (base_1+"man") (palatalise base) Masc ;
    _ => error "Can't apply paradigm mkN056"
  } ;

mkN057 : Str -> LinN ;
mkN057 base = mk5N base base base (base+"ichean") (palatalise base) Fem ;

mkN058 : Str -> LinN ;
mkN058 base = 
  case base of {
    base_1+"air" => mkNoun base (base_1+"raichean") base (base_1+"raichean") base (base_1+"raichean") base (base_1+"raichean") (base_1+"ar") (base_1+"raichean") (base_1+"ar") (base_1+"raichean") base (base_1+"raichean") Masc ;
    _ => error "Can't apply paradigm mkN058"
  } ;

mkN059 : Str -> LinN ;
mkN059 base = mk5N base base (base+"a") (base+"an") (palatalise base) Masc ;

mkN060 : Str -> LinN ;
mkN060 base = mkNoun base (palatalise base+"ean") base (palatalise base+"ean") base (palatalise base+"ean") (lenite base) (palatalise base+"ean") (palatalise base) (lenite (palatalise base)+"ean") (lenite (palatalise base)) (palatalise base+"ean") (lenite (palatalise base)) (lenite (palatalise base)+"ean") Masc ;

mkN061 : Str -> LinN ;
mkN061 base = mk5N base base base (base+"ichean") (palatalise base) Masc ;

mkN062 : Str -> LinN ;
mkN062 base = 
  case base of {
    base_1+"e" => mkNoun base (base_1+"tean") base (palatalise base) base (base_1+"tean") (base_1+"he") (palatalise base) base (lenite base) (base_1+"he") base (base_1+"he") (base+"a") Masc ;
    _ => error "Can't apply paradigm mkN062"
  } ;

mkN063 : Str -> LinN ;
mkN063 base = mk5N base base base (base+"rean") (palatalise base) Fem ;

mkN064 : Str -> LinN ;
mkN064 base =
  case base of {
    "baintighearna" => mkNoun base (base+"n") (lenite base) (base+"n") base (base+"n") (lenite base) (base+"n") base (lenite base+"n") base (base+"n") (lenite base) (lenite base+"n") Fem ;
    _ => error "Can't apply paradigm mkN064"
  } ;

mkN067 : Str -> LinN ;
mkN067 base =
  case base of {
    base_1+"a"+base_2@(?+?) => mk5N base base (base_1+"ui"+base_2) (base_1+"ui"+base_2) (palatalise base) Masc ;
    _ => error "Can't apply paradigm mkN067"
  } ;

mkN068 : Str -> LinN ;
mkN068 base = 
  case base of {
    "balla" => mkNoun base (base+"chan") base (base+"chan") base (base+"chan") (lenite base) (base+"chan") base (lenite base+"chan") (lenite base) (base+"chan") (lenite base) (lenite base+"chan") Masc ;
    _ => error "Can't apply paradigm mkN068"
  } ;

mkN069 : Str -> LinN ;
mkN069 base =
  case base of {
    base_1+("a"|"o")+base_2@("sg"|"lt"|"rt"|"rc"|"nn"|?) => mk5N base base (base_1+"ui"+base_2) (base+"an") (palatalise base) Masc ;
    _ => error "Can't apply paradigm mkN069"
  } ;

mkN070 : Str -> LinN ;
mkN070 base = mk5N base base base (base+"thaichean") (palatalise base) Masc ;

mkN071 : Str -> LinN ;
mkN071 base =
  case base of {
    base_1+"nais" => mk5N base base (base_1+"innse") (base_1+"innsean") (palatalise base) Fem ;
    _ => error "Can't apply paradigm mkN071"
  } ;

mkN072 : Str -> LinN ;
mkN072 base =
  case base of {
    base_1+"a"+base_2@("s"|"ch") => mk5N base (base_1+"oi"+base_2) (base_1+"oi"+base_2+"e") (base+"an") (palatalise base) Fem ;
    _ => error "Can't apply paradigm mkN072"
  } ;

mkN073 : Str -> LinN ;
mkN073 base =
  case base of {
    "bea"+base_1 => mkNoun base ("m"+base_1+"athan") (lenite base) ("m"+base_1+"athan") base ("m"+base_1+"athan") (lenite base) ("m"+base_1+"athan") ("m"+base_1+"à") ("ba"+base_1) ("m"+base_1+"à") ("ba"+base_1) (lenite base) ("mh"+base_1+"athan") Fem ;
    _ => error "Can't apply paradigm mkN073"
  } ;

mkN074 : Str -> LinN ;
mkN074 base =
  case base of {
    "beann" => mkNoun base (base+"an") (lenite base) (base+"an") "beinn" (base+"an") (lenite "beinn") (base+"an") "beinn" (lenite base) "beinne" base (lenite base) (lenite base+"a") Fem ;
    _ => error "Can't apply paradigm mkN074"
  } ;

mkN075 : Str -> LinN ;
mkN075 base =
  case base of {
    "beatha" => mkNoun base (base+"nnan") base (base+"nnan") base (base+"nnan") (palatalise base) (base+"nnan") base (lenite base) base base (lenite base) (base+"a") Fem ;
    _ => error "Can't apply paradigm mkN075"
  } ;

mkN076 : Str -> LinN ;
mkN076 base = mkNoun base (base+"ean") (lenite base) (base+"ean") base (base+"ean") (lenite base) (base+"ean") (base+"e") (lenite base+"ean") (base+"e") (base+"ean") (lenite base) (lenite base+"ean") Fem ;

mkN077 : Str -> LinN ;
mkN077 base =
  case base of {
    "beinn" => mkNoun base "beanntan" (lenite base) "beanntan" base "beanntan" (lenite base) "beanntan" (base+"e") "beann" (base+"e") "beann" (lenite base) (lenite "beannta") Fem ;
    _ => error "Can't apply paradigm mkN077"
  } ;

mkN078 : Str -> LinN ;
mkN078 base =
  case base of {
    "beithe" => mkNoun base (base+"an") "bheithe" (base+"an") base (base+"an") "bheithe" (base+"an") base "bheithean" base (base+"an") "bheithe" "bheithean" Fem ;
    _ => error "Can't apply paradigm mkN078"
  } ;

mkN079 : Str -> LinN ;
mkN079 base =
  case base of {
    base_1+"i"+base_2@(?+?)+"i"+base_3@? => mkNoun base (base_1+"i"+base_2+base_3+"ichean") base (palatalise base) base (base_1+"i"+base_2+base_3+"ichean") (lenite base) (palatalise base) (base_1+"a"+base_2+base_3+"ach") (lenite base) (lenite (palatalise base)) (base) (lenite (palatalise base)) (base+"a") Masc ;
    _ => error "Can't apply paradigm mkN079"
  } ;

mkN080 : Str -> LinN ;
mkN080 base =
  case base of {
    base_1+"u"+base_2@? => mkNoun base (base+"an") base (palatalise base) base (base+"an") (lenite base) (palatalise base) (base_1+"òi"+base_2) (base_1+"òi"+base_2) (lenite (palatalise base)) base (lenite (palatalise base)) (base+"a") Masc ;
    _ => error "Can't apply paradigm mkN080"
  } ;

mkN081 : Str -> LinN ;
mkN081 base =
  case base of {
    base_1+"u"+base_2@? => mk5N base base (base_1+"òi"+base_2) (base_1+"òi"+base_2) (palatalise base) Masc ;
    _ => error "Can't apply paradigm mkN081"
  } ;

mkN082 : Str -> LinN ;
mkN082 base = mk5N base base (base+"a") (base+"an") (palatalise base) Fem ;

mkN083 : Str -> LinN ;
mkN083 base =
  case base of {
    base_1+"eà"+base_2@(?+?) => mkNoun base (base+"an") base (palatalise base) base (base+"an") (lenite base) (palatalise base) (base_1+"èi"+base_2) (lenite base) (lenite (palatalise base)) base (lenite (palatalise base)) (base+"a") Masc ;
    _ => error "Can't apply paradigm mkN083"
  } ;

mkN084 : Str -> LinN ;
mkN084 base = mk5N base base base (base+"aichean") (palatalise base) Masc ;

mkN085 : Str -> LinN ;
mkN085 base = mk5N base base base (base+"than") (palatalise base) Masc ;

mkN086 : Str -> LinN ;
mkN086 base =
  case base of {
    base_1+"ia"+base_2@(?+?) => mk5N base base (base_1+"ì"+base_2) (base+"an") (palatalise base) Masc ;
    _ => error "Can't apply paradigm mkN086"
  } ;

mkN088 : Str -> LinN ;
mkN088 base = mk5N base base base (base+"chan") (palatalise base) Masc ;

mkN089 : Str -> LinN ;
mkN089 base =
  case base of {
    base_1@("st"|"cn"|"cr"|?)+"o"+base_2 => mkNoun base (base_1+"ui"+base_2) (base_1+"o"+base_2) (palatalise base) base (base_1+"ui"+base_2) (lenite base) (palatalise base) (base_1+"ui"+base_2) (lenite base) (lenite (palatalise base)) (base_1+"o"+base_2) (lenite (palatalise base)) (base_1+"o"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN089"
  } ;

mkN090 : Str -> LinN ;
mkN090 base =
  case base of {
    base_1@?+base_2 => mkNoun base (base+"an") base (base+"an") base (base+"an") (lenite base) (base+"an") base (lenite base+"an") (lenite base) (base+"an") (lenite base) (lenite base+"an") Masc ;
    _ => error "Can't apply paradigm mkN090"
  } ;

mkN092 : Str -> LinN ;
mkN092 base =
  case base of {
    base_1+"i"+base_2@? => mk5N base base (base_1+"ma") (base_1+base_2+"annan") (palatalise base) Masc ;
    _ => error "Can't apply paradigm mkN092"
  } ;

mkN093 : Str -> LinN ;
mkN093 base =
  case base of {
    base_1+"ea"+base_2@("s"|(?+?)) => mkNoun base (base+"an") base (palatalise base) base (base+"an") (lenite base) (palatalise base) (base_1+"i"+base_2) (base_1+"i"+base_2) (lenite (palatalise base)) base (lenite (palatalise base)) (base+"a") Masc ;
    _ => error "Can't apply paradigm mkN093"
  } ;

mkN094 : Str -> LinN ;
mkN094 base =
  case base of {
    base_1+base_2@?+"ug" => mk5N base base (base_1+"èig"+base_2) (base+"an") (palatalise base) Fem ;
    _ => error "Can't apply paradigm mkN094"
  } ;

mkN095 : Str -> LinN ;
mkN095 base =
  case base of {
    base_1+"ar" => mk5N base base (base_1+"air") (base_1+"ran") (palatalise base) Masc ;
    _ => error "Can't apply paradigm mkN095"
  } ;

mkN096 : Str -> LinN ;
mkN096 base =
  case base of {
    base_1+base_2@?+"o"+base_3@? => mkNoun base (base_1+base_2+"ui"+base_3) base (base_1+base_2+"ui"+base_3) base (base_1+base_2+"ui"+base_3) (base_1+"h"+base_2+"o"+base_3) (base_1+base_2+"ui"+base_3) (base_1+base_2+"ui"+base_3) (base_1+"h"+base_2+"o"+base_3) (base_1+"h"+base_2+"ui"+base_3) base (base_1+"h"+base_2+"ui"+base_3) (base_1+"h"+base_2+"o"+base_3+"a") Masc ;
    _ => error "Can't apply paradigm mkN096"
  } ;

mkN098 : Str -> LinN ;
mkN098 base =
  case base of {
    base_1@?+base_2+base_3@("s"|"c"|"g"|"ch"|"bh"|"th") => mkNoun base (base_1+base_2+base_3+"an") (base_1+"h"+base_2+base_3) (base_1+base_2+base_3+"an") (base_1+base_2+"i"+base_3) (base_1+base_2+base_3+"an") (base_1+"h"+base_2+"i"+base_3) (base_1+base_2+base_3+"an") (base_1+base_2+"i"+base_3+"e") (base_1+"h"+base_2+base_3) (base_1+base_2+"i"+base_3+"e") (base_1+base_2+base_3) (base_1+"h"+base_2+base_3) (base_1+"h"+base_2+base_3+"a") Fem ;
    _ => error "Can't apply paradigm mkN098"
  } ;

mkN100 : Str -> LinN ;
mkN100 base =
  case base of {
    base_1+"inn" => mk5N base base (base_1+"ne") (base_1+"nean") (palatalise base) Fem ;
    _ => error "Can't apply paradigm mkN100"
  } ;

mkN101 : Str -> LinN ;
mkN101 base =
  case base of {
    "brà" => mkNoun base (base+"thntan") "bhrà" (base+"thntan") base (base+"thntan") "bhrà" (base+"thntan") (base+"than") "bhràthntan" (base+"than") (base+"thntan") "bhrà" "bhràthntan" Fem ;
    _ => error "Can't apply paradigm mkN101"
  } ;

mkN102 : Str -> LinN ;
mkN102 base =
  case base of {
    base_1+base_2@(?+?)+"i"+base_3@(?+?) => mkNoun base (base+"eachan") base (base+"eachan") base (base+"eachan") (base_1+"h"+base_2+"i"+base_3) (base+"eachan") (base_1+base_2+base_3+"ad") (base_1+"h"+base_2+"i"+base_3+"eachan") (base_1+"h"+base_2+base_3+"ad") (base+"eachan") (base_1+"h"+base_2+"i"+base_3) (base_1+"h"+base_2+"i"+base_3+"eachan") Masc ;
    _ => error "Can't apply paradigm mkN102"
  } ;

mkN103 : Str -> LinN ;
mkN103 base =
  case base of {
    base_1+base_2@(?+?)+base_3@(?+?)+"ai"+base_4@? => mkNoun (base_1+base_2+base_3+"ai"+base_4) (base_1+base_2+"i"+base_3+base_4+"ean") (base_1+base_2+base_3+"ai"+base_4) (base_1+base_2+"i"+base_3+base_4+"ean") (base_1+base_2+base_3+"ai"+base_4) (base_1+base_2+"i"+base_3+base_4+"ean") (base_1+"h"+base_2+base_3+"ai"+base_4) (base_1+base_2+"i"+base_3+base_4+"ean") (base_1+base_2+base_3+"a"+base_4) (base_1+"h"+base_2+"i"+base_3+base_4+"ean") (base_1+"h"+base_2+base_3+"a"+base_4) (base_1+base_2+"i"+base_3+base_4+"ean") (base_1+"h"+base_2+base_3+"ai"+base_4) (base_1+"h"+base_2+"i"+base_3+base_4+"ean") Masc ;
    _ => error "Can't apply paradigm mkN103"
  } ;

mkN104 : Str -> LinN ;
mkN104 base =
  case base of {
    base_1+"ea"+base_2@(?+?) => mkNoun (base_1+"ea"+base_2) (base_1+"i"+base_2) (base_1+"ea"+base_2) (base_1+"i"+base_2) base (base_1+"i"+base_2) (palatalise base) (base_1+"i"+base_2) (base_1+"i"+base_2) (lenite base) (base_1+"i"+base_2) (base_1+"ea"+base_2) (lenite base) (base_1+"ea"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN104"
  } ;

mkN105 : Str -> LinN ;
mkN105 base =
  case base of {
    base_1+base_2@?+"ù" => mkNoun (base_1+base_2+"ù") (base_1+base_2+"ùthan") (base_1+"h"+base_2+"ù") (base_1+base_2+"ùthan") (base_1+base_2+"oinn") (base_1+base_2+"onnaibh") (base_1+"h"+base_2+"oinn") (base_1+base_2+"onnaibh") (base_1+base_2+"onn") (base_1+"h"+base_2+"onn") (base_1+base_2+"onn") (base_1+base_2+"onn") (base_1+"h"+base_2+"ù") (base_1+"h"+base_2+"ùtha") Fem ;
    _ => error "Can't apply paradigm mkN105"
  } ;

mkN106 : Str -> LinN ;
mkN106 base =
  case base of {
    base_1+"i"+base_2@? => mkNoun (base_1+"i"+base_2) (base_1+base_2+"annan") (base_1+"i"+base_2) (base_1+base_2+"annan") base (base_1+base_2+"annan") (palatalise base) (base_1+base_2+"annan") (base_1+base_2+"a") (lenite base) (base_1+base_2+"a") (base_1+"i"+base_2) (lenite base) (base_1+"i"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN106"
  } ;

mkN107 : Str -> LinN ;
mkN107 base =
  case base of {
    base_1+base_2@? => mkNoun (base_1+base_2) (base_1+"i"+base_2) (base_1+base_2) (palatalise base) base (base_1+"i"+base_2) (lenite base) (palatalise base) (base_1+base_2+"a") (lenite base) (lenite (palatalise base)) (base_1+base_2) (lenite (palatalise base)) (base_1+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN107"
  } ;

mkN108 : Str -> LinN ;
mkN108 base =
  case base of {
    base_1+base_2@(?+?+?+?+?+?) => mkNoun (base_1+base_2) (base_1+"i"+base_2+"ean") (base_1+base_2) (palatalise base) base (base_1+"i"+base_2+"ean") (lenite base) (palatalise base) (base_1+"h"+base_2) (lenite base) (lenite (palatalise base)) (base_1+base_2) (lenite (palatalise base)) (base_1+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN108"
  } ;

mkN109 : Str -> LinN ;
mkN109 base =
  case base of {
    base_1+"ea"+base_2@(?+?) => mkNoun (base_1+"ea"+base_2) (base_1+"i"+base_2+"ean") (base_1+"ea"+base_2) (base_1+"i"+base_2+"ean") base (base_1+"i"+base_2+"ean") (palatalise base) (base_1+"i"+base_2+"ean") (base_1+"i"+base_2) (lenite base) (base_1+"i"+base_2) (base_1+"ea"+base_2) (lenite base) (base_1+"ea"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN109"
  } ;

mkN110 : Str -> LinN ;
mkN110 base =
  case base of {
    base_1+"à"+base_2@(?+?) => mkNoun (base_1+"à"+base_2) (base_1+"a"+base_2+"an") (base_1+"à"+base_2) (palatalise base) base (base_1+"a"+base_2+"an") (lenite base) (palatalise base) (base_1+"a"+base_2+"a") (lenite base) (lenite (palatalise base)) (base_1+"à"+base_2) (lenite (palatalise base)) (base_1+"à"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN110"
  } ;

mkN111 : Str -> LinN ;
mkN111 base =
  case base of {
    base_1+"ò" => mkNoun (base_1+"ò") (base_1+"à") (base_1+"hò") (base_1+"à") (base_1+"ò") (base_1+"à") (base_1+"hoin") (base_1+"à") (base_1+"à") (base_1+"ò") (base_1+"à") (base_1+"ò") (base_1+"hò") (base_1+"hà") Fem ;
    _ => error "Can't apply paradigm mkN111"
  } ;

mkN112 : Str -> LinN ;
mkN112 base =
  case base of {
    base_1+"l" => mkNoun (base_1+"l") (base_1+"gais") (base_1+"l") (palatalise base) base (base_1+"gais") (lenite base) (palatalise base) (base_1+"gais") (lenite base) (lenite (palatalise base)) (base_1+"l") (lenite (palatalise base)) (base_1+"la") Masc ;
    _ => error "Can't apply paradigm mkN112"
  } ;

mkN113 : Str -> LinN ;
mkN113 base =
  case base of {
    base_1+"ò"+base_2@(?+?) => mkNoun (base_1+"ò"+base_2) (base_1+"ùi"+base_2) (base_1+"ò"+base_2) (palatalise base) base (base_1+"ùi"+base_2) (lenite base) (palatalise base) (base_1+"ùi"+base_2) (lenite base) (lenite (palatalise base)) (base_1+"ò"+base_2) (lenite (palatalise base)) (base_1+"ò"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN113"
  } ;

mkN114 : Str -> LinN ;
mkN114 base =
  case base of {
    base_1+"ó" => mkNoun (base_1+"ó") (base_1+"à") (base_1+"ó") (base_1+"à") (base_1+"oin") (base_1+"à") (palatalise base) (base_1+"à") (base_1+"à") (base_1+"ó") (base_1+"à") (base_1+"ó") (base_1+"ó") (base_1+"óa") Fem ;
    _ => error "Can't apply paradigm mkN114"
  } ;

mkN115 : Str -> LinN ;
mkN115 base =
  case base of {
    base_1@?+base_2 => mkNoun (base_1+base_2) (base_1+base_2+"ean") (base_1+"h"+base_2) (base_1+base_2+"ean") (base_1+base_2) (base_1+base_2+"ean") (base_1+"h"+base_2) (base_1+base_2+"ean") (base_1+base_2) (base_1+"h"+base_2+"ean") (base_1+base_2) (base_1+base_2+"ean") (base_1+"h"+base_2) (base_1+"h"+base_2+"ean") Masc ;
    _ => error "Can't apply paradigm mkN115"
  } ;

mkN116 : Str -> LinN ;
mkN116 base = mk5N base base base (base+"annan") (palatalise base) Masc ;

mkN117 : Str -> LinN ;
mkN117 base =
  case base of {
    base_1+"ea"+base_2@(?+?) => mkNoun (base_1+"ea"+base_2) (base_1+"ea"+base_2+"an") (base_1+"ea"+base_2) (base_1+"ea"+base_2+"an") (base_1+"ea"+base_2) (base_1+"ea"+base_2+"an") (palatalise base) (base_1+"ea"+base_2+"an") (base_1+"i"+base_2+"e") (lenite base) (base_1+"i"+base_2+"e") (base_1+"ea"+base_2) (lenite base) (base_1+"ea"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN117"
  } ;

mkN118 : Str -> LinN ;
mkN118 base = mk5N base base base (base+"idhean") (palatalise base) Masc ;

mkN119 : Str -> LinN ;
mkN119 base = mk5N base base (base+"a") (base+"aidhean") (palatalise base) Masc ;

mkN120 : Str -> LinN ;
mkN120 base = mk5N base base (base+"e") base (palatalise base) Masc ;

mkN121 : Str -> LinN ;
mkN121 base = mk5N base base base (base+"nnan") (palatalise base) Masc ;

mkN123 : Str -> LinN ;
mkN123 base =
  case base of {
    base_1+base_2@(?+?+?+?+?+?) => mkNoun (base_1+base_2) (base_1+base_2+"than") (base_1+base_2) (base_1+base_2+"than") (base_1+base_2) (base_1+base_2+"than") (base_1+"h"+base_2) (base_1+base_2+"than") (base_1+base_2) (base_1+"h"+base_2+"than") (base_1+"h"+base_2) (base_1+base_2+"than") (base_1+"h"+base_2) (base_1+"h"+base_2+"than") Masc ;
    _ => error "Can't apply paradigm mkN123"
  } ;

mkN124 : Str -> LinN ;
mkN124 base =
  case base of {
    base_1+base_2@? => mkNoun (base_1+base_2) (base_1+"i"+base_2+"tean") (base_1+base_2) (palatalise base) base (base_1+"i"+base_2+"tean") (lenite base) (palatalise base) (base_1+"i"+base_2) (lenite base) (lenite (palatalise base)) (base_1+base_2) (lenite (palatalise base)) (base_1+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN124"
  } ;

mkN125 : Str -> LinN ;
mkN125 base =
  case base of {
    base_1+base_2@(?+?+?+?) => mkNoun (base_1+base_2) (base_1+base_2+"ich") (base_1+"h"+base_2) (base_1+base_2+"ich") (base_1+base_2) (base_1+base_2+"ibh") (base_1+"h"+base_2) (base_1+base_2+"ibh") (base_1+base_2+"ch") (base_1+base_2+"ch") (base_1+base_2+"ch") (base_1+base_2+"ch") (base_1+"h"+base_2) (base_1+"h"+base_2+"ich") Fem ;
    _ => error "Can't apply paradigm mkN125"
  } ;

mkN126 : Str -> LinN ;
mkN126 base =
  case base of {
    base_1+"a"+base_2@?+base_3@?+"id" => mkNoun (base_1+"a"+base_2+base_3+"id") (base_1+"ài"+base_2+"de"+base_3+"n") (base_1+"a"+base_2+base_3+"id") (palatalise base) base (base_1+"ài"+base_2+"de"+base_3+"n") (lenite base) (palatalise base) (base_1+"a"+base_2+base_3+"id") (lenite base) (lenite (palatalise base)) (base_1+"a"+base_2+base_3+"id") (lenite (palatalise base)) (base_1+"a"+base_2+base_3+"ida") Masc ;
    _ => error "Can't apply paradigm mkN126"
  } ;

mkN127 : Str -> LinN ;
mkN127 base =
  case base of {
    base_1@?+base_2+base_3@("gh"|?) => mkNoun (base_1+base_2+base_3) (base_1+base_2+base_3+"an") (base_1+base_2+base_3) (base_1+base_2+base_3+"an") (base_1+base_2+base_3) (base_1+base_2+base_3+"an") (base_1+"h"+base_2+base_3) (base_1+base_2+base_3+"an") (base_1+base_2+"i"+base_3) (base_1+"h"+base_2+base_3+"an") (base_1+"h"+base_2+"i"+base_3) (base_1+base_2+base_3+"an") (base_1+"h"+base_2+"i"+base_3) (base_1+"h"+base_2+base_3+"an") Masc ;
    _ => error "Can't apply paradigm mkN127"
  } ;

mkN128 : Str -> LinN ;
mkN128 base =
  case base of {
    base_1+"u" => mkNoun (base_1+"u") (base_1+"othan") (base_1+"u") (palatalise base) base (base_1+"othan") (lenite base) (palatalise base) (base_1+"u") (lenite base) (lenite (palatalise base)) (base_1+"u") (lenite (palatalise base)) (base_1+"ua") Masc ;
    _ => error "Can't apply paradigm mkN128"
  } ;

mkN129 : Str -> LinN ;
mkN129 base =
  case base of {
    base_1+base_2@(?+?+?)+base_3@?+"ir" => mkNoun (base_1+base_2+base_3+"ir") (base_1+base_2+"r"+base_3+"ichean") (base_1+"h"+base_2+base_3+"ir") (base_1+base_2+"r"+base_3+"ichean") (base_1+base_2+base_3+"ir") (base_1+base_2+"r"+base_3+"ichean") (base_1+"h"+base_2+base_3+"ir") (base_1+base_2+"r"+base_3+"ichean") (base_1+base_2+"r"+base_3+"ch") (base_1+"h"+base_2+"r"+base_3+"ichean") (base_1+base_2+"r"+base_3+"ch") (base_1+base_2+"r"+base_3+"ichean") (base_1+"h"+base_2+base_3+"ir") (base_1+"h"+base_2+"r"+base_3+"ichean") Fem ;
    _ => error "Can't apply paradigm mkN129"
  } ;

mkN131 : Str -> LinN ;
mkN131 base =
  case base of {
    base_1+base_2@?+"l" => mkNoun (base_1+base_2+"l") (base_1+"l"+base_2+"n") (base_1+base_2+"l") (base_1+"l"+base_2+"n") base (base_1+"l"+base_2+"n") (lenite base) (base_1+"l"+base_2+"n") (base_1+base_2+"il") (lenite base) (lenite (palatalise base)) (base_1+base_2+"l") (lenite (palatalise base)) (base_1+base_2+"la") Masc ;
    _ => error "Can't apply paradigm mkN131"
  } ;

mkN132 : Str -> LinN ;
mkN132 base =
  case base of {
    base_1+"eò"+base_2@? => mkNoun (base_1+"eò"+base_2) (base_1+"iùi"+base_2) (base_1+"eò"+base_2) (palatalise base) base (base_1+"iùi"+base_2) (lenite base) (palatalise base) (base_1+"iùi"+base_2) (lenite base) (lenite (palatalise base)) (base_1+"eò"+base_2) (lenite (palatalise base)) (base_1+"eò"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN132"
  } ;

mkN133 : Str -> LinN ;
mkN133 base =
  case base of {
    base_1+base_2@?+"a"+base_3@("s"|"n"|(?+?)) => mkNoun (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (palatalise base) (base_1+base_2+"a"+base_3+"an") (base_1+"è"+base_2+base_3+"e") (lenite base) (base_1+"è"+base_2+base_3+"e") (base_1+base_2+"a"+base_3) (lenite base) (base_1+base_2+"a"+base_3+"a") Fem ;
    _ => error "Can't apply paradigm mkN133"
  } ;


mkN134 : Str -> LinN ;
mkN134 base =
  case base of {
    base_1@?+base_2@?+"a"+base_3 => mkNoun (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+base_2+"a"+base_3) (palatalise base) (base_1+"è"+base_2+base_3) (base_1+base_2+"a"+base_3+"an") (lenite base) (palatalise base) (base_1+"è"+base_2+base_3) (lenite base) (lenite (palatalise base)) (base_1+base_2+"a"+base_3) (lenite (palatalise base)) (base_1+base_2+"a"+base_3+"a") Masc ;
    _ => error "Can't apply paradigm mkN134"
  } ;

mkN135 : Str -> LinN ;
mkN135 base = mk5N base base (base+"e") (base+"tean") (palatalise base) Masc ;

mkN137 : Str -> LinN ;
mkN137 base =
  case base of {
    base_1+base_2@?+"a"+base_3@? => mkNoun (base_1+base_2+"a"+base_3) (base_1+base_2+"ui"+base_3) (base_1+base_2+"a"+base_3) (base_1+base_2+"ui"+base_3) (base_1+base_2+"a"+base_3) (base_1+base_2+"ui"+base_3) (base_1+"h"+base_2+"a"+base_3) (base_1+base_2+"ui"+base_3) (base_1+base_2+"ui"+base_3) (base_1+"h"+base_2+"a"+base_3) (base_1+"h"+base_2+"ui"+base_3) (base_1+base_2+"a"+base_3) (base_1+"h"+base_2+"ui"+base_3) (base_1+"h"+base_2+"a"+base_3+"a") Masc ;
    _ => error "Can't apply paradigm mkN137"
  } ;

mkN138 : Str -> LinN ;
mkN138 base =
  case base of {
    base_1+"ea"+base_2@?+base_3@? => mkNoun (base_1+"ea"+base_2+base_3) (base_1+base_2+"ea"+base_3) (base_1+"ea"+base_2+base_3) (palatalise base) base (base_1+base_2+"ea"+base_3) (lenite base) (palatalise base) (base_1+"i"+base_2+base_3) (lenite base) (lenite (palatalise base)) (base_1+"ea"+base_2+base_3) (lenite (palatalise base)) (base_1+"ea"+base_2+base_3+"a") Masc ;
    _ => error "Can't apply paradigm mkN138"
  } ;

mkN139 : Str -> LinN ;
mkN139 base =
  case base of {
    base_1+base_2@?+"a"+base_3@(?+?) => mkNoun (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+"h"+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+base_2+"oi"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+"h"+base_2+"oi"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+base_2+"oi"+base_3+"e") (base_1+"h"+base_2+"a"+base_3) (base_1+base_2+"oi"+base_3+"e") (base_1+base_2+"a"+base_3) (base_1+"h"+base_2+"a"+base_3) (base_1+"h"+base_2+"a"+base_3+"a") Fem ;
    _ => error "Can't apply paradigm mkN139"
  } ;

mkN140 : Str -> LinN ;
mkN140 base =
  case base of {
    base_1+base_2@?+"a"+base_3@(?+?) => mkNoun (base_1+base_2+"a"+base_3) (base_1+"è"+base_2+base_3) (base_1+base_2+"a"+base_3) (palatalise base) base (base_1+"è"+base_2+base_3) (lenite base) (palatalise base) (base_1+"è"+base_2+base_3) (lenite base) (lenite (palatalise base)) (base_1+base_2+"a"+base_3) (lenite (palatalise base)) (base_1+base_2+"a"+base_3+"a") Masc ;
    _ => error "Can't apply paradigm mkN140"
  } ;

mkN141 : Str -> LinN ;
mkN141 base =
  case base of {
    base_1+"i"+base_2@(?+?+?)+base_3@?+"inn" => mkNoun (base_1+"i"+base_2+base_3+"inn") (base_1+"e"+base_2+"n"+base_3+"n") (base_1+"i"+base_2+base_3+"inn") (palatalise base) base (base_1+"e"+base_2+"n"+base_3+"n") (lenite base) (palatalise base) (base_1+"e"+base_2+"n"+base_3) (lenite base) (lenite (palatalise base)) (base_1+"i"+base_2+base_3+"inn") (lenite (palatalise base)) (base_1+"i"+base_2+base_3+"inna") Masc ;
    _ => error "Can't apply paradigm mkN141"
  } ;

mkN142 : Str -> LinN ;
mkN142 base =
  case base of {
    base_1@?+base_2+"a"+base_3@("ct"|?) => mkNoun (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+"h"+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+base_2+"i"+base_3) (base_1+"h"+base_2+"a"+base_3+"an") (base_1+"h"+base_2+"i"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+"h"+base_2+"i"+base_3) (base_1+"h"+base_2+"a"+base_3+"an") Masc ;
    _ => error "Can't apply paradigm mkN142"
  } ;

mkN143 : Str -> LinN ;
mkN143 base = mk5N base base base (base+"ithean") (palatalise base) Masc ;

mkN144 : Str -> LinN ;
mkN144 base = mk5N base base (base+"dha") (base+"dhan") (palatalise base) Masc ;

mkN145 : Str -> LinN ;
mkN145 base =
  case base of {
    base_1+"imh" => mk5N base base (base_1+"mha") (base_1+"mhan") (palatalise base) Masc ;
    _ => error "Can't apply paradigm mkN145"
  } ;

mkN147 : Str -> LinN ;
mkN147 base =
  case base of {
    base_1+base_2@(?+?+?+?)+base_3@? => mkNoun (base_1+base_2+base_3) (base_1+base_2+"t"+base_3+"an") (base_1+"h"+base_2+base_3) (base_1+base_2+"t"+base_3+"an") (base_1+base_2+base_3) (base_1+base_2+"t"+base_3+"an") (base_1+"h"+base_2+base_3) (base_1+base_2+"t"+base_3+"an") (base_1+base_2+base_3) (base_1+base_2+"t"+base_3+"an") (base_1+base_2+base_3) (base_1+base_2+"t"+base_3+"an") (base_1+"h"+base_2+base_3) (base_1+base_2+"t"+base_3+"an") Fem ;
    _ => error "Can't apply paradigm mkN147"
  } ;

mkN148 : Str -> LinN ;
mkN148 base =
  case base of {
    base_1+"i"+base_2@(?+?+?+?+?+?+?+?) => mkNoun (base_1+"i"+base_2) (base_1+base_2+"ean") (base_1+"i"+base_2) (palatalise base) base (base_1+base_2+"ean") (lenite base) (palatalise base) (base_1+"i"+base_2) (lenite base) (lenite (palatalise base)) (base_1+"i"+base_2) (lenite (palatalise base)) (base_1+"i"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN148"
  } ;

mkN149 : Str -> LinN ;
mkN149 base =
  case base of {
    base_1+base_2@?+"al" => mkNoun (base_1+base_2+"al") (base_1+"l"+base_2+"an") (base_1+base_2+"al") (base_1+"l"+base_2+"an") base (base_1+"l"+base_2+"an") (palatalise base) (base_1+"l"+base_2+"an") (base_1+"l"+base_2) (lenite base) (base_1+"l"+base_2) (base_1+base_2+"al") (lenite base) (base_1+base_2+"ala") Fem ;
    _ => error "Can't apply paradigm mkN149"
  } ;

mkN150 : Str -> LinN ;
mkN150 base = mk5N base base base (base+"achan") (palatalise base) Masc ;

mkN151 : Str -> LinN ;
mkN151 base =
  case base of {
    base_1+base_2@(?+?) => mkNoun (base_1+base_2) (base_1+"i"+base_2+"ean") (base_1+base_2) (base_1+"i"+base_2+"ean") (base_1+"i"+base_2) (base_1+"i"+base_2+"ean") (base_1+"i"+base_2) (base_1+"i"+base_2+"ean") (base_1+"i"+base_2) (lenite base) (base_1+"i"+base_2+"e") (base_1+base_2) (base_1+base_2) (base_1+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN151"
  } ;

mkN152 : Str -> LinN ;
mkN152 base =
  case base of {
    base_1+base_2@(?+?+?+?+?+?+?+?+?) => mkNoun (base_1+base_2) (base_1+base_2+"ean") (base_1+"h"+base_2) (base_1+base_2+"ean") (base_1+base_2) (base_1+base_2+"ean") (base_1+"h"+base_2) (base_1+base_2+"ean") (base_1+base_2) (base_1+"h"+base_2+"ean") (base_1+base_2+"e") (base_1+base_2+"ean") (base_1+"h"+base_2) (base_1+"h"+base_2+"ean") Fem ;
    _ => error "Can't apply paradigm mkN152"
  } ;

mkN153 : Str -> LinN ;
mkN153 base =
  case base of {
    base_1+base_2@?+"ll" => mkNoun (base_1+base_2+"ll") (base_1+"l"+base_2+"ichean") (base_1+base_2+"ll") (base_1+"l"+base_2+"ichean") base (base_1+"l"+base_2+"ichean") (palatalise base) (base_1+"l"+base_2+"ichean") (base_1+"l"+base_2+"ch") (lenite base) (base_1+"l"+base_2+"ch") (base_1+base_2+"ll") (lenite base) (base_1+base_2+"lla") Fem ;
    _ => error "Can't apply paradigm mkN153"
  } ;

mkN154 : Str -> LinN ;
mkN154 base =
  case base of {
    base_1+base_2@(?+?) => mkNoun (base_1+base_2) (base_1+"i"+base_2) (base_1+base_2) (palatalise base) (base_1+base_2) (base_1+"i"+base_2) (base_1+"h"+base_2) (palatalise base) (base_1+base_2) (lenite base) (base_1+"h"+base_2) (base_1+base_2) (base_1+"h"+base_2) (base_1+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN154"
  } ;

mkN155 : Str -> LinN ;
mkN155 base =
  case base of {
    base_1+"i"+base_2@("l"|(?+?)) => mkNoun (base_1+"i"+base_2) (base_1+base_2+"aichean") (base_1+"i"+base_2) (base_1+base_2+"aichean") base (base_1+base_2+"aichean") (palatalise base) (base_1+base_2+"aichean") (base_1+base_2+"ach") (lenite base) (base_1+base_2+"ach") (base_1+"i"+base_2) (lenite base) (base_1+"i"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN155"
  } ;

mkN156 : Str -> LinN ;
mkN156 base =
  case base of {
    base_1+base_2@(?+?)+"a" => mkNoun (base_1+base_2+"a") (base_1+"i"+base_2+"ean") (base_1+base_2+"a") (palatalise base) base (base_1+"i"+base_2+"ean") (lenite base) (palatalise base) (base_1+"i"+base_2+"e") (lenite base) (lenite (palatalise base)) (base_1+base_2+"a") (lenite (palatalise base)) (base_1+base_2+"aa") Masc ;
    _ => error "Can't apply paradigm mkN156"
  } ;

mkN157 : Str -> LinN ;
mkN157 base =
  case base of {
    base_1+"o"+base_2@(?+?) => mkNoun (base_1+"o"+base_2) (base_1+"o"+base_2+"an") (base_1+"o"+base_2) (base_1+"o"+base_2+"an") (base_1+base_2) (base_1+"o"+base_2+"an") (palatalise base) (base_1+"o"+base_2+"an") (base_1+base_2+"e") (lenite base) (base_1+base_2+"e") (base_1+"o"+base_2) (lenite base) (base_1+"o"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN157"
  } ;

mkN158 : Str -> LinN ;
mkN158 base =
  case base of {
    base_1+base_2@? => mkNoun (base_1+base_2) (base_1+base_2+"tan") (base_1+base_2) (palatalise base) base (base_1+base_2+"tan") (lenite base) (palatalise base) (base_1+"i"+base_2) (lenite base) (lenite (palatalise base)) (base_1+base_2) (lenite (palatalise base)) (base_1+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN158"
  } ;

mkN159 : Str -> LinN ;
mkN159 base = mk5N base base base base (palatalise base) Masc ;

mkN160 : Str -> LinN ;
mkN160 base =
  case base of {
    base_1+"ui"+base_2@? => mkNoun (base_1+"ui"+base_2) (base_1+"o"+base_2+"aichean") (base_1+"hui"+base_2) (base_1+"o"+base_2+"aichean") (base_1+"ui"+base_2) (base_1+"o"+base_2+"aichean") (base_1+"hui"+base_2) (base_1+"o"+base_2+"aichean") (base_1+"o"+base_2+"ach") (base_1+"ho"+base_2+"aichean") (base_1+"o"+base_2+"ach") (base_1+"o"+base_2+"aichean") (base_1+"hui"+base_2) (base_1+"ho"+base_2+"aiche") Fem ;
    _ => error "Can't apply paradigm mkN160"
  } ;

mkN161 : Str -> LinN ;
mkN161 base =
  case base of {
    base_1+"ea"+base_2@?+"l" => mkNoun (base_1+"ea"+base_2+"l") (base_1+base_2+"eachan") (base_1+"ea"+base_2+"l") (base_1+base_2+"eachan") base (base_1+base_2+"eachan") (palatalise base) (base_1+base_2+"eachan") (base_1+"i"+base_2+"l") (lenite base) (base_1+"i"+base_2+"l") (base_1+"ea"+base_2+"l") (lenite base) (base_1+"ea"+base_2+"la") Fem ;
    _ => error "Can't apply paradigm mkN161"
  } ;

mkN163 : Str -> LinN ;
mkN163 base =
  case base of {
    base_1+"s"+base_2@?+"a"+base_3@? => mkNoun (base_1+"s"+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+"s"+base_2+"a"+base_3) (palatalise base) base (base_1+base_2+"a"+base_3+"an") (lenite base) (palatalise base) (base_1+"s"+base_2+"i"+base_3) (lenite base) (lenite (palatalise base)) (base_1+"s"+base_2+"a"+base_3) (lenite (palatalise base)) (base_1+"s"+base_2+"a"+base_3+"a") Masc ;
    _ => error "Can't apply paradigm mkN163"
  } ;

mkN164 : Str -> LinN ;
mkN164 base =
  case base of {
    base_1+base_2@?+base_3@? => mkNoun (base_1+base_2+base_3) (base_1+base_2+base_3+"aichean") (base_1+base_2+base_3) (base_1+base_2+base_3+"aichean") (base_1+base_2+base_3) (base_1+base_2+base_3+"aichean") (base_1+"h"+base_2+base_3) (base_1+base_2+base_3+"aichean") (base_1+base_2+"i"+base_3) (base_1+"h"+base_2+base_3+"aichean") (base_1+"h"+base_2+"i"+base_3) (base_1+base_2+base_3+"aichean") (base_1+"h"+base_2+"i"+base_3) (base_1+"h"+base_2+base_3+"aichean") Masc ;
    _ => error "Can't apply paradigm mkN164"
  } ;

mkN165 : Str -> LinN ;
mkN165 base =
  case base of {
    base_1+"à"+base_2@(?+?) => mkNoun (base_1+"à"+base_2) (base_1+"ùi"+base_2) (base_1+"à"+base_2) (base_1+"ùi"+base_2) (base_1+"à"+base_2) (base_1+"ùi"+base_2) (base_1+"hà"+base_2) (base_1+"ùi"+base_2) (base_1+"ùi"+base_2) (base_1+"hà"+base_2) (base_1+"hùi"+base_2) (base_1+"à"+base_2) (base_1+"hùi"+base_2) (base_1+"hà"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN165"
  } ;

mkN166 : Str -> LinN ;
mkN166 base = mk5N base base (base+"ithe") (base+"ithean") (palatalise base) Masc ;

mkN167 : Str -> LinN ;
mkN167 base =
  case base of {
    base_1+"o"+base_2@(?+?) => mkNoun (base_1+"o"+base_2) (base_1+"o"+base_2+"an") (base_1+"o"+base_2) (base_1+"o"+base_2+"an") (base_1+"o"+base_2) (base_1+"o"+base_2+"an") (palatalise base) (base_1+"o"+base_2+"an") (base_1+base_2+"e") (lenite base) (base_1+base_2+"e") (base_1+"o"+base_2) (lenite base) (base_1+"o"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN167"
  } ;

mkN168 : Str -> LinN ;
mkN168 base =
  case base of {
    base_1+"ò"+base_2@(?+?+?+?)+"a"+base_3@? => mkNoun (base_1+"ò"+base_2+"a"+base_3) (base_1+"ó"+base_2+"a"+base_3+"an") (base_1+"ò"+base_2+"a"+base_3) (palatalise base) base (base_1+"ó"+base_2+"a"+base_3+"an") (lenite base) (palatalise base) (base_1+"ó"+base_2+"i"+base_3) (lenite base) (lenite (palatalise base)) (base_1+"ò"+base_2+"a"+base_3) (lenite (palatalise base)) (base_1+"ò"+base_2+"a"+base_3+"a") Masc ;
    _ => error "Can't apply paradigm mkN168"
  } ;

mkN169 : Str -> LinN ;
mkN169 base =
  case base of {
    base_1+base_2@?+"i"+base_3@? => mkNoun (base_1+base_2+"i"+base_3) (base_1+base_2+base_3+"aichean") (base_1+"h"+base_2+"i"+base_3) (base_1+base_2+base_3+"aichean") (base_1+base_2+"i"+base_3) (base_1+base_2+base_3+"aichean") (base_1+"h"+base_2+"i"+base_3) (base_1+base_2+base_3+"aichean") (base_1+base_2+base_3+"ach") (base_1+base_2+base_3+"aichean") (base_1+base_2+base_3+"ach") (base_1+base_2+base_3+"aichean") (base_1+"h"+base_2+"i"+base_3) (base_1+"h"+base_2+base_3+"aiche") Fem ;
    _ => error "Can't apply paradigm mkN169"
  } ;

mkN170 : Str -> LinN ;
mkN170 base =
  case base of {
    base_1+"h"+base_2@?+"i"+base_3@? => mkNoun (base_1+"h"+base_2+"i"+base_3) (base_1+base_2+base_3+"aichean") (base_1+"h"+base_2+"i"+base_3) (base_1+base_2+base_3+"aichean") base (base_1+base_2+base_3+"aichean") (palatalise base) (base_1+base_2+base_3+"aichean") (base_1+base_2+base_3+"ach") (lenite base) (base_1+base_2+base_3+"ach") (base_1+"h"+base_2+"i"+base_3) (lenite base) (base_1+"h"+base_2+"i"+base_3+"a") Fem ;
    _ => error "Can't apply paradigm mkN170"
  } ;

mkN171 : Str -> LinN ;
mkN171 base =
  case base of {
    base_1+"ù" => mkNoun (base_1+"ù") (base_1+"oin") (base_1+"ù") (base_1+"oin") (base_1+"ù") (base_1+"oin") (base_1+"hù") (base_1+"oin") (base_1+"oin") (base_1+"hon") (base_1+"hoin") (base_1+"on") (base_1+"hoin") (base_1+"hona") Masc ;
    _ => error "Can't apply paradigm mkN171"
  } ;

mkN172 : Str -> LinN ;
mkN172 base =
  case base of {
    base_1+"ea"+base_2@(?+?) => mkNoun (base_1+"ea"+base_2) (base_1+"i"+base_2+"ean") (base_1+"ea"+base_2) (base_1+"i"+base_2+"ean") base (base_1+"i"+base_2+"ean") (palatalise base) (base_1+"i"+base_2+"ean") (base_1+"i"+base_2+"e") (lenite base) (base_1+"i"+base_2+"e") (base_1+"ea"+base_2) (lenite base) (base_1+"ea"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN172"
  } ;

mkN173 : Str -> LinN ;
mkN173 base =
  case base of {
    base_1@?+base_2+base_3@("ch"|?) => mkNoun (base_1+base_2+base_3) (base_1+base_2+"i"+base_3) (base_1+base_2+base_3) (base_1+base_2+"i"+base_3) (base_1+base_2+base_3) (base_1+base_2+"i"+base_3) (base_1+base_2+base_3) (base_1+base_2+"i"+base_3) (base_1+base_2+"i"+base_3) (base_1+"h"+base_2+base_3) (base_1+base_2+"i"+base_3) (base_1+base_2+base_3) (base_1+"h"+base_2+"i"+base_3) (base_1+"h"+base_2+base_3+"a") Masc ;
    _ => error "Can't apply paradigm mkN173"
  } ;

mkN174 : Str -> LinN ;
mkN174 base =
  case base of {
    base_1@?+base_2+base_3@? => mkNoun (base_1+base_2+base_3) (base_1+base_2+base_3+"an") (base_1+base_2+base_3) (base_1+base_2+base_3+"an") (base_1+base_2+base_3) (base_1+base_2+base_3+"an") (base_1+base_2+base_3) (base_1+base_2+base_3+"an") (base_1+base_2+"i"+base_3) (base_1+"h"+base_2+base_3+"an") (base_1+base_2+"i"+base_3) (base_1+base_2+base_3+"an") (base_1+"h"+base_2+"i"+base_3) (base_1+"h"+base_2+base_3+"an") Masc ;
    _ => error "Can't apply paradigm mkN174"
  } ;

mkN175 : Str -> LinN ;
mkN175 base =
  case base of {
    base_1+base_2@?+"och" => mkNoun (base_1+base_2+"och") (base_1+base_2+"ochan") (base_1+base_2+"och") (base_1+base_2+"ochan") base (base_1+base_2+"ochan") (palatalise base) (base_1+base_2+"ochan") (base_1+"igh"+base_2) (lenite base) (base_1+"igh"+base_2) (base_1+base_2+"och") (lenite base) (base_1+base_2+"ocha") Fem ;
    _ => error "Can't apply paradigm mkN175"
  } ;

mkN176 : Str -> LinN ;
mkN176 base =
  case base of {
    base_1+"u"+base_2@? => mkNoun (base_1+"u"+base_2) (base_1+"u"+base_2+"an") (base_1+"u"+base_2) (base_1+"u"+base_2+"an") base (base_1+"u"+base_2+"an") (lenite base) (base_1+"u"+base_2+"an") (base_1+"i"+base_2) (lenite base) (lenite (palatalise base)) (base_1+"u"+base_2) (lenite (palatalise base)) (base_1+"u"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN176"
  } ;

mkN177 : Str -> LinN ;
mkN177 base =
  case base of {
    base_1+"ia" => mkNoun (base_1+"ia") (base_1+"iathan") (base_1+"ia") (base_1+"iathan") (base_1+"ia") (base_1+"iathan") (base_1+"ia") (base_1+"iathan") (base_1+"è") (base_1+"hia") (base_1+"è") (base_1+"ia") (base_1+"hè") (base_1+"hiatha") Masc ;
    _ => error "Can't apply paradigm mkN177"
  } ;

mkN178 : Str -> LinN ;
mkN178 base =
  case base of {
    base_1@?+base_2+"a"+base_3@? => mkNoun (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+base_2+"i"+base_3) (base_1+"h"+base_2+"a"+base_3+"an") (base_1+base_2+"i"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+"h"+base_2+"i"+base_3) (base_1+"h"+base_2+"a"+base_3+"an") Masc ;
    _ => error "Can't apply paradigm mkN178"
  } ;

mkN179 : Str -> LinN ;
mkN179 base =
  case base of {
    base_1+base_2@?+"s" => mkNoun (base_1+base_2+"s") (base_1+"s"+base_2+"n") (base_1+base_2+"s") (palatalise base) base (base_1+"s"+base_2+"n") (lenite base) (palatalise base) (base_1+base_2+"is") (lenite base) (lenite (palatalise base)) (base_1+base_2+"s") (lenite (palatalise base)) (base_1+base_2+"sa") Masc ;
    _ => error "Can't apply paradigm mkN179"
  } ;

mkN180 : Str -> LinN ;
mkN180 base =
  case base of {
    base_1+base_2@?+"st" => mkNoun (base_1+base_2+"st") (base_1+"s"+base_2+"n") (base_1+base_2+"st") (palatalise base) base (base_1+"s"+base_2+"n") (lenite base) (palatalise base) (base_1+base_2+"ist") (lenite base) (lenite (palatalise base)) (base_1+base_2+"st") (lenite (palatalise base)) (base_1+base_2+"sta") Masc ;
    _ => error "Can't apply paradigm mkN180"
  } ;

mkN181 : Str -> LinN ;
mkN181 base =
  case base of {
    base_1+"u"+base_2@? => mkNoun (base_1+"u"+base_2) (base_1+base_2+"an") (base_1+"u"+base_2) (palatalise base) base (base_1+base_2+"an") (lenite base) (palatalise base) (base_1+"ui"+base_2) (lenite base) (lenite (palatalise base)) (base_1+"u"+base_2) (lenite (palatalise base)) (base_1+"u"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN181"
  } ;

mkN182 : Str -> LinN ;
mkN182 base = mk5N base base (base+"a") (base+"annan") (palatalise base) Masc ;

mkN183 : Str -> LinN ;
mkN183 base =
  case base of {
    base_1+base_2@?+"as" => mkNoun (base_1+base_2+"as") (base_1+base_2+"asan") (base_1+base_2+"as") (base_1+base_2+"asan") base (base_1+base_2+"asan") (palatalise base) (base_1+base_2+"asan") (base_1+"is"+base_2) (lenite base) (base_1+"is"+base_2) (base_1+base_2+"as") (lenite base) (base_1+base_2+"asa") Fem ;
    _ => error "Can't apply paradigm mkN183"
  } ;

mkN184 : Str -> LinN ;
mkN184 base =
  case base of {
    base_1+"ui"+base_2@? => mkNoun (base_1+"ui"+base_2) (base_1+"o"+base_2+"annan") (base_1+"ui"+base_2) (palatalise base) base (base_1+"o"+base_2+"annan") (lenite base) (palatalise base) (base_1+"o"+base_2+"a") (lenite base) (lenite (palatalise base)) (base_1+"ui"+base_2) (lenite (palatalise base)) (base_1+"ui"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN184"
  } ;

mkN185 : Str -> LinN ;
mkN185 base =
  case base of {
    base_1+base_2@(?+?+?+?) => mkNoun (base_1+base_2) (base_1+base_2+"ichean") (base_1+base_2) (base_1+base_2+"ichean") (base_1+base_2) (base_1+base_2+"ichean") (base_1+base_2) (base_1+base_2+"ichean") (base_1+base_2) (base_1+"h"+base_2+"ichean") (base_1+base_2) (base_1+base_2+"ichean") (base_1+"h"+base_2) (base_1+"h"+base_2+"ichean") Fem ;
    _ => error "Can't apply paradigm mkN185"
  } ;

mkN186 : Str -> LinN ;
mkN186 base =
  case base of {
    base_1+"à"+base_2@? => mkNoun (base_1+"à"+base_2) (base_1+"a"+base_2+"annan") (base_1+"à"+base_2) (palatalise base) base (base_1+"a"+base_2+"annan") (lenite base) (palatalise base) (base_1+"a"+base_2+"a") (lenite base) (lenite (palatalise base)) (base_1+"à"+base_2) (lenite (palatalise base)) (base_1+"à"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN186"
  } ;

mkN187 : Str -> LinN ;
mkN187 base =
  case base of {
    base_1+"u"+base_2@(?+?+?) => mkNoun (base_1+"u"+base_2) (base_1+"ao"+base_2) (base_1+"u"+base_2) (palatalise base) base (base_1+"ao"+base_2) (lenite base) (palatalise base) (base_1+"u"+base_2) (lenite base) (lenite (palatalise base)) (base_1+"u"+base_2) (lenite (palatalise base)) (base_1+"u"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN187"
  } ;

mkN188 : Str -> LinN ;
mkN188 base =
  case base of {
    base_1+"e" => mkNoun (base_1+"e") (base_1+"eannan") (base_1+"e") (base_1+"eannan") (base_1+"inn") (base_1+"eannan") (palatalise base) (base_1+"eannan") (base_1+"eann") (lenite base) (base_1+"eann") (base_1+"e") (lenite base) (base_1+"ea") Fem ;
    _ => error "Can't apply paradigm mkN188"
  } ;

mkN189 : Str -> LinN ;
mkN189 base =
  case base of {
    base_1+"i"+base_2@? => mkNoun (base_1+"i"+base_2) (base_1+"i"+base_2+"ean") (base_1+"i"+base_2) (base_1+"i"+base_2+"ean") base (base_1+"i"+base_2+"ean") (palatalise base) (base_1+"i"+base_2+"ean") (base_1+"i"+base_2+"e") (base_1+base_2) (base_1+"i"+base_2+"e") (base_1+"i"+base_2) (base_1+base_2) (base_1+"i"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN189"
  } ;

mkN190 : Str -> LinN ;
mkN190 base =
  case base of {
    base_1+base_2@(?+?+?)+"ai"+base_3@(?+?) => mkNoun (base_1+base_2+"ai"+base_3) (base_1+base_2+base_3+"annan") (base_1+base_2+"ai"+base_3) (base_1+base_2+base_3+"annan") (base_1+base_2+"ai"+base_3) (base_1+base_2+base_3+"annan") (base_1+base_2+"ai"+base_3) (base_1+base_2+base_3+"annan") (base_1+base_2+base_3+"a") (base_1+"h"+base_2+base_3+"annan") (base_1+base_2+base_3+"a") (base_1+base_2+base_3+"annan") (base_1+"h"+base_2+"ai"+base_3) (base_1+"h"+base_2+base_3+"annan") Fem ;
    _ => error "Can't apply paradigm mkN190"
  } ;

mkN191 : Str -> LinN ;
mkN191 base =
  case base of {
    base_1+"a"+base_2@("n"|(?+?)) => mkNoun (base_1+"a"+base_2) (base_1+"i"+base_2) (base_1+"a"+base_2) (base_1+"i"+base_2) (base_1+"a"+base_2) (base_1+"i"+base_2) (base_1+"a"+base_2) (base_1+"i"+base_2) (base_1+"i"+base_2) (base_1+"a"+base_2) (base_1+"i"+base_2) (base_1+"a"+base_2) (base_1+"i"+base_2) (base_1+"a"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN191"
  } ;

mkN192 : Str -> LinN ;
mkN192 base = mk5N base base (base+"idh") (base+"chan") (palatalise base) Masc ;

mkN193 : Str -> LinN ;
mkN193 base =
  case base of {
    base_1+base_2@?+"r" => mkNoun (base_1+base_2+"r") (base_1+"r"+base_2+"ichean") (base_1+base_2+"r") (palatalise base) base (base_1+"r"+base_2+"ichean") (lenite base) (palatalise base) (base_1+base_2+"ir") (lenite base) (lenite (palatalise base)) (base_1+base_2+"r") (lenite (palatalise base)) (base_1+base_2+"ra") Masc ;
    _ => error "Can't apply paradigm mkN193"
  } ;

mkN194 : Str -> LinN ;
mkN194 base =
  case base of {
    base_1+base_2@?+"ann" => mkNoun (base_1+base_2+"ann") (nonExist) (base_1+base_2+"ann") (nonExist) base (nonExist) (palatalise base) nonExist (base_1+"n"+base_2) (lenite base) (base_1+"n"+base_2) (base_1+base_2+"ann") (lenite base) (base_1+base_2+"anna") Fem ;
    _ => error "Can't apply paradigm mkN194"
  } ;

mkN195 : Str -> LinN ;
mkN195 base =
  case base of {
    "e"+base_1+"i"+base_2@? => mkNoun ("e"+base_1+"i"+base_2) ("è"+base_1+base_2+"ean") ("e"+base_1+"i"+base_2) ("è"+base_1+base_2+"ean") base ("è"+base_1+base_2+"ean") (palatalise base) ("è"+base_1+base_2+"ean") ("è"+base_1+base_2+"e") (lenite base) ("è"+base_1+base_2+"e") ("e"+base_1+"i"+base_2) (lenite base) ("e"+base_1+"i"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN195"
  } ;

mkN196 : Str -> LinN ;
mkN196 base =
  case base of {
    "eu"+base_1 => mk5N base base ("èi"+base_1) (palatalise base) (palatalise base) Masc ;
    _ => error "Can't apply paradigm mkN196"
  } ;

mkN197 : Str -> LinN ;
mkN197 base =
  case base of {
    "eò" => mkNoun ("eò") ("iach") ("eò") (palatalise base) base ("iach") (lenite base) (palatalise base) ("iach") (lenite base) (lenite (palatalise base)) ("eò") (lenite (palatalise base)) ("eòa") Masc ;
    _ => error "Can't apply paradigm mkN197"
  } ;

mkN199 : Str -> LinN ;
mkN199 base =
  case base of {
    base_1+base_2@? => mkNoun (base_1+base_2) (base_1+"ich"+base_2+"an") (base_1+base_2) (palatalise base) (base_1+base_2) (base_1+"ich"+base_2+"an") (base_1+"h"+base_2) (palatalise base) (base_1+base_2) (lenite base) (base_1+"h"+base_2) (base_1+base_2) (base_1+"h"+base_2) (base_1+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN199"
  } ;

mkN200 : Str -> LinN ;
mkN200 base =
  case base of {
    base_1+base_2@?+"a"+base_3@? => mkNoun (base_1+base_2+"a"+base_3) (base_1+"n"+base_2+"a"+base_3) (base_1+base_2+"a"+base_3) (palatalise base) base (base_1+"n"+base_2+"a"+base_3) (lenite base) (palatalise base) (base_1+base_2+"i"+base_3) (lenite base) (lenite (palatalise base)) (base_1+base_2+"a"+base_3) (lenite (palatalise base)) (base_1+base_2+"a"+base_3+"a") Masc ;
    _ => error "Can't apply paradigm mkN200"
  } ;

mkN201 : Str -> LinN ;
mkN201 base =
  case base of {
    base_1+"i"+base_2@(?+?) => mkNoun (base_1+"i"+base_2) (base+"an") (base_1+"i"+base_2) (base+"an") base (base+"an") (palatalise base) (base+"an") (base_1+base_2) (lenite base) (base_1+base_2) (base_1+"i"+base_2) (lenite base) (base_1+"i"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN201"
  } ;

mkN202 : Str -> LinN ;
mkN202 base =
  case base of {
    base_1+"ea"+base_2@? => mkNoun (base_1+"ea"+base_2) (base_1+"i"+base_2) (base_1+"ea"+base_2) (base_1+"i"+base_2) (base_1+"ea"+base_2) (base_1+"i"+base_2) (base_1+"hea"+base_2) (base_1+"i"+base_2) (base_1+"i"+base_2) (base_1+"hea"+base_2) (base_1+"hi"+base_2) (base_1+"ea"+base_2) (base_1+"hi"+base_2) (base_1+"hea"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN202"
  } ;

mkN203 : Str -> LinN ;
mkN203 base =
  case base of {
    base_1+base_2@(?+?)+"i"+base_3@? => mkNoun (base_1+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+"h"+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+"h"+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+base_3+"a") (base_1+"h"+base_2+"i"+base_3+"ean") (base_1+base_2+base_3+"a") (base_1+base_2+"i"+base_3+"ean") (base_1+"h"+base_2+"i"+base_3) (base_1+"h"+base_2+base_3+"a") Fem ;
    _ => error "Can't apply paradigm mkN203"
  } ;

mkN204 : Str -> LinN ;
mkN204 base =
  case base of {
    base_1+base_2@?+"ill" => mk5N base base (base_1+"l"+base_2) (base_1+"l"+base_2+"n") (palatalise base) Fem ;
    _ => error "Can't apply paradigm mkN204"
  } ;

mkN205 : Str -> LinN ;
mkN205 base =
  case base of {
    base_1+base_2@?+"a"+base_3@(?+?) => mkNoun base (base_1+"è"+base_2+base_3) base (base_1+"è"+base_2+base_3) base (base_1+"è"+base_2+base_3) (lenite base) (base_1+"è"+base_2+base_3) (base_1+"è"+base_2+base_3) (lenite base) (base_1+"hè"+base_2+base_3) base (base_1+"hè"+base_2+base_3) (lenite base+"a") Masc ;
    _ => error "Can't apply paradigm mkN205"
  } ;

mkN206 : Str -> LinN ;
mkN206 base =
  case base of {
    base_1+"i"+base_2@(?+?)+"ea"+base_3@?+"l" => mkNoun (base_1+"i"+base_2+"ea"+base_3+"l") (base_1+"ì"+base_2+base_3+"ean") (base_1+"i"+base_2+"ea"+base_3+"l") (base_1+"ì"+base_2+base_3+"ean") (base_1+"i"+base_2+"i"+base_3+"l") (base_1+"ì"+base_2+base_3+"ean") (palatalise base) (base_1+"ì"+base_2+base_3+"ean") (base_1+"ì"+base_2+base_3+"e") (lenite base) (base_1+"ì"+base_2+base_3+"e") (base_1+"i"+base_2+"ea"+base_3+"l") (lenite base) (base_1+"i"+base_2+"ea"+base_3+"la") Fem ;
    _ => error "Can't apply paradigm mkN206"
  } ;

mkN207 : Str -> LinN ;
mkN207 base =
  case base of {
    base_1+"o"+base_2@(?+?)+"a" => mkNoun base (base_1+"o"+base_2+"an") base (base_1+"o"+base_2+"an") base (base_1+"o"+base_2+"an") (palatalise base) base (base_1+"ui"+base_2+"e") (lenite base) (base_1+"ui"+base_2+"e") base (lenite base) (base+"a") Fem ;
    _ => error "Can't apply paradigm mkN207"
  } ;

mkN208 : Str -> LinN ;
mkN208 base =
  case base of {
    base_1+"ui"+base_2@? => mkNoun (base_1+"ui"+base_2) (base+"an") (base_1+"hui"+base_2) (base+"an") (base_1+"ui"+base_2) (base+"an") (base_1+"hui"+base_2) (base+"an") (base_1+"a"+base_2+"a") (lenite base) (base_1+"a"+base_2+"a") (base_1+"ui"+base_2) (base_1+"hui"+base_2) (base_1+"ui"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN208"
  } ;

mkN209 : Str -> LinN ;
mkN209 base =
  case base of {
    base_1+base_2@(?+?+?)+"ea"+base_3@(?+?) => mkNoun (base_1+base_2+"ea"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+"ea"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+"ea"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+"h"+base_2+"ea"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+"i"+base_3) (base_1+"h"+base_2+"i"+base_3+"ean") (base_1+"h"+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+"h"+base_2+"i"+base_3) (base_1+"h"+base_2+"i"+base_3+"ean") Masc ;
    _ => error "Can't apply paradigm mkN209"
  } ;

mkN210 : Str -> LinN ;
mkN210 base =
  case base of {
    base_1+base_2@? => mkNoun (base_1+base_2) (base_1+"ich"+base_2+"an") (base_1+base_2) (base_1+"ich"+base_2+"an") (base_1+base_2) (base_1+"ich"+base_2+"an") (base_1+"h"+base_2) (base_1+"ich"+base_2+"an") (base_1+base_2) (lenite base) (base_1+base_2) (base_1+base_2) (base_1+"h"+base_2) (base_1+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN210"
  } ;

mkN211 : Str -> LinN ;
mkN211 base = mk5N base base (base+"a") (base+"tan") (palatalise base) Masc ;

mkN212 : Str -> LinN ;
mkN212 base =
  case base of {
    base_1+base_2@(?+?+?+?)+"i"+base_3@? => mkNoun (base_1+base_2+"i"+base_3) (base_1+base_2+base_3+"aichean") (base_1+"h"+base_2+"i"+base_3) (base_1+base_2+base_3+"aichean") (base_1+base_2+"i"+base_3) (base_1+base_2+base_3+"aichean") (base_1+"h"+base_2+"i"+base_3) (base_1+base_2+base_3+"aichean") (base_1+base_2+base_3+"ach") (base_1+"h"+base_2+base_3+"aichean") (base_1+base_2+base_3+"ach") (base_1+base_2+base_3+"aichean") (base_1+"h"+base_2+"i"+base_3) (base_1+"h"+base_2+base_3+"aichean") Masc ;
    _ => error "Can't apply paradigm mkN212"
  } ;

mkN213 : Str -> LinN ;
mkN213 base =
  case base of {
    base_1+"a"+base_2@?+"a"+base_3@(?+?+?) => mkNoun (base_1+"a"+base_2+"a"+base_3) (base_1+"i"+base_2+base_3+"ean") (base_1+"a"+base_2+"a"+base_3) (palatalise base) base (base_1+"i"+base_2+base_3+"ean") (lenite base) (palatalise base) (base_1+"i"+base_2+base_3) (lenite base) (lenite (palatalise base)) (base_1+"a"+base_2+"a"+base_3) (lenite (palatalise base)) (base_1+"a"+base_2+"a"+base_3+"a") Masc ;
    _ => error "Can't apply paradigm mkN213"
  } ;

mkN214 : Str -> LinN ;
mkN214 base =
  case base of {
    base_1+base_2@(?+?+?+?+?)+"ea"+base_3@(?+?) => mkNoun (base_1+base_2+"ea"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+"h"+base_2+"ea"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+"h"+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+"i"+base_3) (base_1+"h"+base_2+"ea"+base_3) (base_1+base_2+"i"+base_3+"e") (base_1+base_2+"ea"+base_3) (base_1+"h"+base_2+"ea"+base_3) (base_1+"h"+base_2+"ea"+base_3+"a") Fem ;
    _ => error "Can't apply paradigm mkN214"
  } ;

mkN216 : Str -> LinN ;
mkN216 base =
  case base of {
    base_1+base_2@(?+?)+"ai"+base_3@?+"n" => mkNoun base (base_1+base_2+base_3+"a") (base_1+base_2+"ai"+base_3+"n") (palatalise base) base (base_1+base_2+base_3+"a") (lenite base) (palatalise base) (base_1+"i"+base_2+base_3+"e") (lenite base) (lenite (palatalise base)) base (lenite (palatalise base)) (base+"a") Masc ;
    _ => error "Can't apply paradigm mkN216"
  } ;

mkN217 : Str -> LinN ;
mkN217 base =
  case base of {
    base_1+base_2@(?+?+?) => mkNoun base (base+"an") base (base+"an") (base+"a") (base+"an") (base_1+"h"+base_2+"a") (base+"an") (base+"a") (base_1+"h"+base_2+"an") (base_1+"h"+base_2+"a") (base+"an") (base_1+"h"+base_2) (base_1+"h"+base_2+"an") Masc ;
    _ => error "Can't apply paradigm mkN217"
  } ;

mkN219 : Str -> LinN ;
mkN219 base =
  case base of {
    base_1+base_2@?+"a"+base_3@? => mkNoun base (base+"an") (base_1+"h"+base_2+"a"+base_3) (base+"an") (base+"a") (base+"an") (base_1+"h"+base_2+"a"+base_3+"a") (base+"an") (base_1+base_2+"i"+base_3) (base_1+"h"+base_2+"a"+base_3+"an") (base+"a") (base+"an") (base_1+"h"+base_2+"a"+base_3) (base_1+"h"+base_2+"a"+base_3+"an") Fem ;
    _ => error "Can't apply paradigm mkN219"
  } ;

mkN220 : Str -> LinN ;
mkN220 base =
  case base of {
    base_1+"ug" => mkNoun base (base+"an") base (base+"an") base (base+"an") (palatalise base) (base+"an") ("gèi"+base_1) (lenite base) ("gèi"+base_1) base (lenite base) (base+"a") Fem ;
    _ => error "Can't apply paradigm mkN220"
  } ;

mkN221 : Str -> LinN ;
mkN221 base =
  case base of {
    base_1+base_2@?+"à"+base_3@(?+?) => mkNoun (base_1+base_2+"à"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+"h"+base_2+"à"+base_3) (base_1+base_2+"à"+base_3+"an") (base_1+base_2+"à"+base_3+"a") (base_1+base_2+"à"+base_3+"an") (base_1+"h"+base_2+"à"+base_3+"a") (base_1+base_2+"à"+base_3+"an") (base_1+base_2+"a"+base_3+"a") (base_1+"h"+base_2+"à"+base_3+"an") (base_1+base_2+"à"+base_3+"a") (base_1+base_2+"à"+base_3+"an") (base_1+"h"+base_2+"à"+base_3) (base_1+"h"+base_2+"à"+base_3+"an") Fem ;
    _ => error "Can't apply paradigm mkN221"
  } ;

mkN222 : Str -> LinN ;
mkN222 base =
  case base of {
    base_1+base_2@(?+?+?+?) => mkNoun (base_1+base_2) (base_1+base_2+"an") (base_1+"h"+base_2) (base_1+base_2+"an") (base_1+base_2+"a") (base_1+base_2+"an") (base_1+"h"+base_2+"a") (base_1+base_2+"an") (base_1+base_2+"a") (base_1+"h"+base_2+"an") (base_1+base_2+"a") (base_1+base_2+"an") (base_1+"h"+base_2) (base_1+"h"+base_2+"an") Fem ;
    _ => error "Can't apply paradigm mkN222"
  } ;

mkN224 : Str -> LinN ;
mkN224 base =
  case base of {
    base_1@?+base_2 => mkNoun (base_1+base_2) (base_1+base_2+"an") (base_1+"h"+base_2) (base_1+base_2+"an") (base_1+base_2) (base_1+base_2+"an") (base_1+"h"+base_2) (base_1+base_2+"an") (base_1+base_2) (base_1+"h"+base_2+"an") (base_1+base_2) (base_1+base_2+"an") (base_1+"h"+base_2) (base_1+"h"+base_2+"an") Masc ;
    _ => error "Can't apply paradigm mkN224"
  } ;

mkN225 : Str -> LinN ;
mkN225 base = mk5N base base base (base+"dhean") (palatalise base) Masc ;

mkN226 : Str -> LinN ;
mkN226 base =
  case base of {
    base_1+"ea"+base_2@(?+?) => mkNoun (base_1+"ea"+base_2) (base_1+"ea"+base_2+"tan") (base_1+"ea"+base_2) (palatalise base) base (base_1+"ea"+base_2+"tan") (lenite base) (palatalise base) (base_1+"i"+base_2+"e") (lenite base) (lenite (palatalise base)) (base_1+"ea"+base_2) (lenite (palatalise base)) (base_1+"ea"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN226"
  } ;

mkN227 : Str -> LinN ;
mkN227 base = mk5N base base base (base+"ithean") (palatalise base) Fem ;

mkN228 : Str -> LinN ;
mkN228 base =
  case base of {
    base_1+base_2@?+base_3@(?+?)+base_4@? => mkNoun (base_1+base_2+base_3+base_4) (base_1+base_2+"i"+base_3+"ne"+base_4+"n") (base_1+base_2+base_3+base_4) (base_1+base_2+"i"+base_3+"ne"+base_4+"n") (base_1+base_2+base_3+base_4) (base_1+base_2+"i"+base_3+"ne"+base_4+"n") (base_1+"h"+base_2+base_3+base_4) (base_1+base_2+"i"+base_3+"ne"+base_4+"n") (base_1+base_2+base_3+base_4+"inn") (base_1+"h"+base_2+"i"+base_3+"ne"+base_4+"n") (base_1+"h"+base_2+base_3+base_4+"inn") (base_1+base_2+"i"+base_3+"ne"+base_4+"n") (base_1+"h"+base_2+base_3+base_4) (base_1+"h"+base_2+"i"+base_3+"ne"+base_4+"n") Masc ;
    _ => error "Can't apply paradigm mkN228"
  } ;

mkN229 : Str -> LinN ;
mkN229 base =
  case base of {
    base_1@(?+?)+base_2+base_3@?+"l" => mkNoun (base_1+base_2+base_3+"l") (base_1+"i"+base_2+"le"+base_3+"n") (base_1+base_2+base_3+"l") (palatalise base) base (base_1+"i"+base_2+"le"+base_3+"n") (lenite base) (palatalise base) (base_1+base_2+base_3+"il") (lenite base) (lenite (palatalise base)) (base_1+base_2+base_3+"l") (lenite (palatalise base)) (base_1+base_2+base_3+"la") Masc ;
    _ => error "Can't apply paradigm mkN229"
  } ;

mkN230 : Str -> LinN ;
mkN230 base = mk5N base base (base+"a") (base+"aichean") (palatalise base) Masc ;

mkN231 : Str -> LinN ;
mkN231 base = mk5N base base (base+"e") (base+"eannan") (palatalise base) Masc ;

mkN232 : Str -> LinN ;
mkN232 base =
  case base of {
    base_1+base_2@(?+?+?+?+?)+"a"+base_3@? => mkNoun (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+"h"+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+base_2+"i"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+"h"+base_2+"i"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+base_2+"i"+base_3+"e") (base_1+"h"+base_2+"a"+base_3) (base_1+base_2+"i"+base_3+"e") (base_1+base_2+"a"+base_3) (base_1+"h"+base_2+"a"+base_3) (base_1+"h"+base_2+"a"+base_3+"a") Fem ;
    _ => error "Can't apply paradigm mkN232"
  } ;

mkN233 : Str -> LinN ;
mkN233 base =
  case base of {
    base_1+base_2@(?+?)+base_3@?+base_4@? => mkNoun (base_1+base_2+base_3+base_4) (base_1+base_2+"i"+base_3+"ne"+base_4+"n") (base_1+"h"+base_2+base_3+base_4) (base_1+base_2+"i"+base_3+"ne"+base_4+"n") (base_1+base_2+base_3+base_4) (base_1+base_2+"i"+base_3+"ne"+base_4+"n") (base_1+"h"+base_2+base_3+base_4) (base_1+base_2+"i"+base_3+"ne"+base_4+"n") (base_1+base_2+base_3+base_4+"inn") (base_1+"h"+base_2+"i"+base_3+"ne"+base_4+"n") (base_1+base_2+base_3+base_4+"inn") (base_1+base_2+"i"+base_3+"ne"+base_4+"n") (base_1+"h"+base_2+base_3+base_4) (base_1+"h"+base_2+"i"+base_3+"ne"+base_4+"n") Fem ;
    _ => error "Can't apply paradigm mkN233"
  } ;

mkN234 : Str -> LinN ;
mkN234 base =
  case base of {
    base_1+base_2@(?+?)+base_3@?+base_4@?+base_5@?+"n" => mkNoun (base_1+base_2+base_3+base_4+base_5+"n") (base_1+base_2+"i"+base_3+"ne"+base_4+base_5) (base_1+"h"+base_2+base_3+base_4+base_5+"n") (base_1+base_2+"i"+base_3+"ne"+base_4+base_5) (base_1+base_2+base_3+base_4+base_5+"n") (base_1+base_2+"i"+base_3+"ne"+base_4+base_5) (base_1+"h"+base_2+base_3+base_4+base_5+"n") (base_1+base_2+"i"+base_3+"ne"+base_4+base_5) (base_1+base_2+base_3+base_4+"i"+base_5+"n") (base_1+"h"+base_2+"i"+base_3+"ne"+base_4+base_5) (base_1+base_2+base_3+base_4+"i"+base_5+"n") (base_1+base_2+"i"+base_3+"ne"+base_4+base_5) (base_1+"h"+base_2+base_3+base_4+base_5+"n") (base_1+"h"+base_2+"i"+base_3+"ne"+base_4+base_5) Fem ;
    _ => error "Can't apply paradigm mkN234"
  } ;

mkN235 : Str -> LinN ;
mkN235 base = mk5N base base (base+"an") (base+"s") (palatalise base) Masc ;

mkN236 : Str -> LinN ;
mkN236 base =
  case base of {
    base_1+"a"+base_2@("dh"|?) => mkNoun (base_1+"a"+base_2) (base_1+"i"+base_2+"ean") (base_1+"a"+base_2) (palatalise base) base (base_1+"i"+base_2+"ean") (lenite base) (palatalise base) (base_1+"i"+base_2) (lenite base) (lenite (palatalise base)) (base_1+"a"+base_2) (lenite (palatalise base)) (base_1+"a"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN236"
  } ;

mkN237 : Str -> LinN ;
mkN237 base =
  case base of {
    base_1+"èa"+base_2@(?+?) => mkNoun (base_1+"èa"+base_2) (base_1+"eòi"+base_2) (base_1+"èa"+base_2) (base_1+"eòi"+base_2) (base_1+"èa"+base_2) (base_1+"eòi"+base_2) (base_1+"hèa"+base_2) (base_1+"eòi"+base_2) (base_1+"eòi"+base_2) (base_1+"hèa"+base_2) (base_1+"heòi"+base_2) (base_1+"èa"+base_2) (base_1+"heòi"+base_2) (base_1+"hèa"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN237"
  } ;

mkN238 : Str -> LinN ;
mkN238 base =
  case base of {
    base_1 => mkNoun (base_1) (base_1+"ean") (base_1) (base_1+"ean") (base_1) (base_1+"ean") (lenite base) (base_1+"ean") (base_1) (base_1+"ean") (base_1) (base_1+"ean") (base_1) (base_1+"ean") Masc ;
    _ => error "Can't apply paradigm mkN238"
  } ;

mkN239 : Str -> LinN ;
mkN239 base =
  case base of {
    base_1+"a"+base_2@(?+?) => mkNoun (base_1+"a"+base_2) (base_1+"a"+base_2+"an") (base_1+"a"+base_2) (base_1+"a"+base_2+"an") base (base_1+"a"+base_2+"an") (palatalise base) (base_1+"a"+base_2+"an") ("è"+base_1+base_2+"e") (lenite base) (nonExist) (base_1+"a"+base_2) (lenite base) (base_1+"a"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN239"
  } ;

mkN240 : Str -> LinN ;
mkN240 base =
  case base of {
    base_1+"a"+base_2@(?+?) => mkNoun (base_1+"a"+base_2) ("è"+base_1+base_2) (base_1+"a"+base_2) ("è"+base_1+base_2) (base_1+"a"+base_2) ("è"+base_1+base_2) (base_1+"a"+base_2) ("è"+base_1+base_2) ("è"+base_1+base_2) (base_1+"a"+base_2) ("è"+base_1+base_2) (base_1+"a"+base_2) ("è"+base_1+base_2) (base_1+"a"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN240"
  } ;

mkN241 : Str -> LinN ;
mkN241 base = mk5N base base (base+"an") (base+"an") (palatalise base) Masc ;

mkN242 : Str -> LinN ;
mkN242 base =
  case base of {
    "in"+base_1+base_2@?+"an" => mkNoun ("in"+base_1+base_2+"an") ("in"+base_1+base_2+"anan") ("in"+base_1+base_2+"an") ("in"+base_1+base_2+"anan") base ("in"+base_1+base_2+"anan") (palatalise base) ("in"+base_1+base_2+"anan") ("ì"+base_1+"n"+base_2) (lenite base) ("ì"+base_1+"n"+base_2) ("in"+base_1+base_2+"an") (lenite base) ("in"+base_1+base_2+"ana") Fem ;
    _ => error "Can't apply paradigm mkN242"
  } ;

mkN243 : Str -> LinN ;
mkN243 base =
  case base of {
    "io"+base_1+"a" => mkNoun ("io"+base_1+"a") ("ì"+base_1+"nean") ("io"+base_1+"a") ("ì"+base_1+"nean") base ("ì"+base_1+"nean") (palatalise base) ("ì"+base_1+"nean") ("i"+base_1+"ne") (lenite base) ("i"+base_1+"ne") ("io"+base_1+"a") (lenite base) ("io"+base_1+"aa") Fem ;
    _ => error "Can't apply paradigm mkN243"
  } ;

mkN244 : Str -> LinN ;
mkN244 base =
  case base of {
    base_1+base_2@(?+?)+"a" => mkNoun (base_1+base_2+"a") (base_1+"i"+base_2+"ean") (base_1+base_2+"a") (base_1+"i"+base_2+"ean") base (base_1+"i"+base_2+"ean") (palatalise base) (base_1+"i"+base_2+"ean") (base_1+"i"+base_2+"e") (lenite base) (base_1+"i"+base_2+"e") (base_1+base_2+"a") (lenite base) (base_1+base_2+"aa") Fem ;
    _ => error "Can't apply paradigm mkN244"
  } ;

mkN245 : Str -> LinN ;
mkN245 base =
  case base of {
    base_1 => mkNoun (base_1) (base_1+"annan") (base_1) (base_1+"annan") (base_1+"a") (base_1+"annan") (base_1+"a") (base_1+"annan") (base_1+"a") (base_1+"annan") (base_1+"a") (base_1+"annan") (base_1) (base_1+"annan") Masc ;
    _ => error "Can't apply paradigm mkN245"
  } ;

mkN246 : Str -> LinN ;
mkN246 base =
  case base of {
    base_1+"t"+base_2@(?+?+?) => mkNoun (base_1+"t"+base_2) (base_1+"d"+base_2+"ean") (base_1+"t"+base_2) (palatalise base) base (base_1+"d"+base_2+"ean") (lenite base) (palatalise base) (nonExist) (lenite base) (lenite (palatalise base)) (base_1+"t"+base_2) (lenite (palatalise base)) (base_1+"t"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN246"
  } ;

mkN247 : Str -> LinN ;
mkN247 base =
  case base of {
    base_1+"a"+base_2@(?+?)+base_3@? => mkNoun (base_1+"a"+base_2+base_3) (base_1+"ài"+base_2+"e"+base_3+"n") (base_1+"a"+base_2+base_3) (palatalise base) base (base_1+"ài"+base_2+"e"+base_3+"n") (lenite base) (palatalise base) (base_1+"a"+base_2+base_3) (lenite base) (lenite (palatalise base)) (base_1+"a"+base_2+base_3) (lenite (palatalise base)) (base_1+"a"+base_2+base_3+"a") Masc ;
    _ => error "Can't apply paradigm mkN247"
  } ;

mkN248 : Str -> LinN ;
mkN248 base =
  case base of {
    base_1+"b"+base_2@?+"idh" => mkNoun (base_1+"b"+base_2+"idh") (base_1+"b"+base_2+"ichean") (base_1+"b"+base_2+"idh") (base_1+"b"+base_2+"ichean") base (base_1+"b"+base_2+"ichean") (palatalise base) (base_1+"b"+base_2+"ichean") (base_1+"p"+base_2) (lenite base) (base_1+"p"+base_2) (base_1+"b"+base_2+"idh") (lenite base) (base_1+"b"+base_2+"idha") Fem ;
    _ => error "Can't apply paradigm mkN248"
  } ;

mkN249 : Str -> LinN ;
mkN249 base =
  case base of {
    base_1+base_2@?+"r" => mkNoun (base_1+base_2+"r") (base_1+"r"+base_2+"ichean") (base_1+base_2+"r") (base_1+"r"+base_2+"ichean") (base_1+base_2+"r") (base_1+"r"+base_2+"ichean") (base_1+base_2+"r") (base_1+"r"+base_2+"ichean") (base_1+base_2+"ir") (base_1+"r"+base_2+"ichean") (base_1+base_2+"ir") (base_1+"r"+base_2+"ichean") (base_1+base_2+"r") (base_1+"r"+base_2+"ichean") Masc ;
    _ => error "Can't apply paradigm mkN249"
  } ;

mkN250 : Str -> LinN ;
mkN250 base =
  case base of {
    base_1+"ea"+base_2@? => mkNoun (base_1+"ea"+base_2) (base_1+"ea"+base_2+"an") (base_1+"ea"+base_2) (base_1+"ea"+base_2+"an") (base_1+"i"+base_2) (base_1+"ea"+base_2+"an") (palatalise base) (base_1+"ea"+base_2+"an") (base_1+"i"+base_2+"e") (lenite base) (base_1+"i"+base_2+"e") (base_1+"ea"+base_2) (lenite base) (base_1+"ea"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN250"
  } ;

mkN251 : Str -> LinN ;
mkN251 base =
  case base of {
    base_1+"a"+base_2@?+"a"+base_3@(?+?) => mkNoun (base_1+"a"+base_2+"a"+base_3) (base_1+"a"+base_2+"a"+base_3+"an") (base_1+"a"+base_2+"a"+base_3) (palatalise base) base (base_1+"a"+base_2+"a"+base_3+"an") (lenite base) (palatalise base) (base_1+"i"+base_2+"i"+base_3) (lenite base) (lenite (palatalise base)) (base_1+"a"+base_2+"a"+base_3) (lenite (palatalise base)) (base_1+"a"+base_2+"a"+base_3+"a") Masc ;
    _ => error "Can't apply paradigm mkN251"
  } ;

mkN252 : Str -> LinN ;
mkN252 base =
  case base of {
    base_1+base_2@?+"thad" => mkNoun (base_1+base_2+"thad") (base_1+"òide"+base_2+"n") (base_1+base_2+"thad") (palatalise base) base (base_1+"òide"+base_2+"n") (lenite base) (palatalise base) (base_1+base_2+"thaid") (lenite base) (lenite (palatalise base)) (base_1+base_2+"thad") (lenite (palatalise base)) (base_1+base_2+"thada") Masc ;
    _ => error "Can't apply paradigm mkN252"
  } ;

mkN253 : Str -> LinN ;
mkN253 base =
  case base of {
    base_1+"o"+base_2@? => mkNoun (base_1+"o"+base_2) (base_1+"o"+base_2+"an") (base_1+"o"+base_2) (palatalise base) base (base_1+"o"+base_2+"an") (lenite base) (palatalise base) (base_1+"ui"+base_2+"e") (lenite base) (lenite (palatalise base)) (base_1+"o"+base_2) (lenite (palatalise base)) (base_1+"o"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN253"
  } ;

mkN254 : Str -> LinN ;
mkN254 base =
  case base of {
    base_1+"i"+base_2@? => mkNoun (base_1+"i"+base_2) (base_1+base_2+"ichean") (base_1+"i"+base_2) (base_1+base_2+"ichean") base (base_1+base_2+"ichean") (palatalise base) (base_1+base_2+"ichean") (base_1+base_2+"each") (lenite base) (base_1+base_2+"each") (base_1+"i"+base_2) (lenite base) (base_1+"i"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN254"
  } ;

mkN255 : Str -> LinN ;
mkN255 base =
  case base of {
    base_1+base_2@?+"a"+base_3@(?+?) => mkNoun (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+"è"+base_2+base_3) (base_1+base_2+"a"+base_3+"an") (palatalise base) (base_1+base_2+"a"+base_3+"an") (base_1+"è"+base_2+base_3) (lenite base) (base_1+"è"+base_2+base_3) (base_1+base_2+"a"+base_3) (lenite base) (base_1+base_2+"a"+base_3+"a") Fem ;
    _ => error "Can't apply paradigm mkN255"
  } ;

mkN256 : Str -> LinN ;
mkN256 base = mk5N base base base (base+"tean") (palatalise base) Masc ;

mkN257 : Str -> LinN ;
mkN257 base =
  case base of {
    base_1+base_2@?+"o"+base_3@(?+?) => mkNoun (base_1+base_2+"o"+base_3) (base_1+base_2+"o"+base_3+"an") (base_1+base_2+"o"+base_3) (base_1+base_2+"o"+base_3+"an") base (base_1+base_2+"o"+base_3+"an") (palatalise base) (base_1+base_2+"o"+base_3+"an") (base_1+"è"+base_2+base_3) (lenite base) (base_1+"è"+base_2+base_3) (base_1+base_2+"o"+base_3) (lenite base) (base_1+base_2+"o"+base_3+"a") Fem ;
    _ => error "Can't apply paradigm mkN257"
  } ;

mkN258 : Str -> LinN ;
mkN258 base =
  case base of {
    base_1+"o"+base_2@? => mkNoun (base_1+"o"+base_2) (base_1+"o"+base_2+"an") (base_1+"o"+base_2) (palatalise base) (base_1+base_2) (base_1+"o"+base_2+"an") (lenite base) (palatalise base) (base_1+base_2+"e") (lenite base) (lenite (palatalise base)) (base_1+"o"+base_2) (lenite (palatalise base)) (base_1+"o"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN258"
  } ;

mkN259 : Str -> LinN ;
mkN259 base =
  case base of {
    base_1+"i"+base_2@(?+?+?)+"a"+base_3@? => mkNoun (base_1+"i"+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+"i"+base_2+"a"+base_3) (palatalise base) base (base_1+base_2+"a"+base_3+"an") (lenite base) (palatalise base) (base_1+"i"+base_2+"i"+base_3) (lenite base) (lenite (palatalise base)) (base_1+"i"+base_2+"a"+base_3) (lenite (palatalise base)) (base_1+"i"+base_2+"a"+base_3+"a") Masc ;
    _ => error "Can't apply paradigm mkN259"
  } ;

mkN260 : Str -> LinN ;
mkN260 base =
  case base of {
    base_1+"o"+base_2@(?+?) => mkNoun (base_1+"o"+base_2) (base_1+"o"+base_2+"an") (base_1+"o"+base_2) (base_1+"o"+base_2+"an") base (base_1+"o"+base_2+"an") (palatalise base) (base_1+"o"+base_2+"an") (base_1+"ui"+base_2+"e") (lenite base) (base_1+"ui"+base_2+"e") (base_1+"o"+base_2) (lenite base) (base_1+"o"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN260"
  } ;

mkN261 : Str -> LinN ;
mkN261 base = mk5N base base (base+"a") (base+"aichean") (palatalise base) Fem ;

mkN262 : Str -> LinN ;
mkN262 base =
  case base of {
    base_1 => mkNoun (base_1) (base_1+"an") (base_1) (base_1+"an") (base_1+"a") (base_1+"an") (base_1+"a") (base_1+"an") (base_1+"a") (base_1+"an") (base_1+"a") (base_1+"an") (base_1) (base_1+"an") Fem ;
    _ => error "Can't apply paradigm mkN262"
  } ;

mkN263 : Str -> LinN ;
mkN263 base =
  case base of {
    base_1+base_2@("mh"|?) => mkNoun (base_1+base_2) (base_1+base_2+"an") (base_1+base_2) (base_1+base_2+"an") (base_1+"i"+base_2) (base_1+base_2+"an") (base_1+"i"+base_2) (base_1+base_2+"an") (base_1+"i"+base_2+"e") (base_1+base_2) (base_1+"i"+base_2+"e") (base_1+base_2) (base_1+base_2) (base_1+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN263"
  } ;

mkN264 : Str -> LinN ;
mkN264 base =
  case base of {
    base_1+base_2@(?+?+?+?+?) => mkNoun (base_1+base_2) (base_1+"i"+base_2+"ean") (base_1+base_2) (base_1+base_2+"ean") (base_1+base_2) (base_1+base_2+"ean") (base_1+base_2) (base_1+base_2+"ean") (base_1+base_2+"e") (base_1+base_2+"ean") (base_1+base_2+"e") (base_1+base_2+"ean") (base_1+base_2) (base_1+base_2+"ean") Fem ;
    _ => error "Can't apply paradigm mkN264"
  } ;

mkN265 : Str -> LinN ;
mkN265 base =
  case base of {
    base_1+"èi"+base_2@? => mkNoun (base_1+"èi"+base_2) (base_1+"eu"+base_2+"an") (base_1+"èi"+base_2) (palatalise base) base (base_1+"eu"+base_2+"an") (lenite base) (palatalise base) (base_1+"eu"+base_2+"a") (lenite base) (lenite (palatalise base)) (base_1+"èi"+base_2) (lenite (palatalise base)) (base_1+"èi"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN265"
  } ;

mkN266 : Str -> LinN ;
mkN266 base =
  case base of {
    base_1+base_2@? => mkNoun (base_1+base_2) (base_1+"t"+base_2+"an") (base_1+base_2) (base_1+"t"+base_2+"an") (base_1+base_2) (base_1+"t"+base_2+"an") (palatalise base) (base_1+"t"+base_2+"an") (base_1+base_2) (lenite base) (base_1+base_2) (base_1+base_2) (base_1+"h"+base_2) (base_1+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN266"
  } ;

mkN267 : Str -> LinN ;
mkN267 base = mk5N base base (base+"idh") (base+"idhean") (palatalise base) Masc ;

mkN268 : Str -> LinN ;
mkN268 base =
  case base of {
    base_1+"a"+base_2@? => mkNoun (base_1+"a"+base_2) (base_1+"i"+base_2) (base_1+"a"+base_2) (base_1+"i"+base_2) (base_1+"a"+base_2) (base_1+"i"+base_2) (base_1+"ha"+base_2) (base_1+"i"+base_2) (base_1+"i"+base_2) (base_1+"ha"+base_2) (base_1+"hi"+base_2) (base_1+"a"+base_2) (base_1+"hi"+base_2) (base_1+"ha"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN268"
  } ;

mkN269 : Str -> LinN ;
mkN269 base =
  case base of {
    base_1+base_2@?+"ir" => mkNoun (base_1+base_2+"ir") (base_1+"r"+base_2+"ichean") (base_1+base_2+"ir") (palatalise base) base (base_1+"r"+base_2+"ichean") (lenite base) (palatalise base) (base_1+base_2+"rach") (lenite base) (lenite (palatalise base)) (base_1+base_2+"ir") (lenite (palatalise base)) (base_1+base_2+"ira") Masc ;
    _ => error "Can't apply paradigm mkN269"
  } ;

mkN270 : Str -> LinN ;
mkN270 base =
  case base of {
    base_1+"da"+base_2@?+base_3@?+"n" => mkNoun (base_1+"da"+base_2+base_3+"n") (base_1+base_2+"d"+base_3+"ean") (base_1+"da"+base_2+base_3+"n") (base_1+base_2+"d"+base_3+"ean") base (base_1+base_2+"d"+base_3+"ean") (palatalise base) (base_1+base_2+"d"+base_3+"ean") (base_1+base_2+"d"+base_3+"e") (lenite base) (base_1+base_2+"d"+base_3+"e") (base_1+"da"+base_2+base_3+"n") (lenite base) (base_1+"da"+base_2+base_3+"na") Fem ;
    _ => error "Can't apply paradigm mkN270"
  } ;

mkN271 : Str -> LinN ;
mkN271 base =
  case base of {
    base_1+base_2@?+"a"+base_3@? => mkNoun (base_1+base_2+"a"+base_3) (base_1+"inn"+base_2+"a"+base_3) (base_1+base_2+"a"+base_3) (base_1+"inn"+base_2+"a"+base_3) base (base_1+"inn"+base_2+"a"+base_3) (palatalise base) (base_1+"inn"+base_2+"a"+base_3) (base_1+base_2+"i"+base_3) (lenite base) (base_1+base_2+"i"+base_3) (base_1+base_2+"a"+base_3) (lenite base) (base_1+base_2+"a"+base_3+"a") Fem ;
    _ => error "Can't apply paradigm mkN271"
  } ;

mkN272 : Str -> LinN ;
mkN272 base =
  case base of {
    base_1+base_2@(?+?+?+?)+"ea"+base_3@(?+?) => mkNoun (base_1+base_2+"ea"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+"h"+base_2+"ea"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+"h"+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+"i"+base_3+"e") (base_1+"h"+base_2+"ea"+base_3) (base_1+base_2+"i"+base_3+"e") (base_1+base_2+"ea"+base_3) (base_1+"h"+base_2+"ea"+base_3) (base_1+"h"+base_2+"ea"+base_3+"a") Fem ;
    _ => error "Can't apply paradigm mkN272"
  } ;

mkN273 : Str -> LinN ;
mkN273 base =
  case base of {
    base_1+base_2@("e"|(?+?+?+?+?+?+?+?+?)) => mkNoun (base_1+base_2) (base_1+"t"+base_2+"an") (base_1+base_2) (palatalise base) (base_1+base_2) (base_1+"t"+base_2+"an") (base_1+"h"+base_2) (palatalise base) (base_1+base_2) (lenite base) (base_1+"h"+base_2) (base_1+base_2) (base_1+"h"+base_2) (base_1+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN273"
  } ;

mkN274 : Str -> LinN ;
mkN274 base =
  case base of {
    base_1+"a"+base_2@("g"|(?+?)) => mkNoun (base_1+"a"+base_2) (base_1+"a"+base_2+"an") (base_1+"a"+base_2) (base_1+"a"+base_2+"an") (base_1+"i"+base_2) (base_1+"a"+base_2+"an") (base_1+"i"+base_2) (base_1+"a"+base_2+"an") (base_1+"i"+base_2+"e") (base_1+"a"+base_2) (base_1+"i"+base_2+"e") (base_1+"a"+base_2) (base_1+"a"+base_2) (base_1+"a"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN274"
  } ;

mkN275 : Str -> LinN ;
mkN275 base =
  case base of {
    base_1+"u"+base_2@? => mkNoun (base_1+"u"+base_2) (base_1+"u"+base_2+"an") (base_1+"u"+base_2) (base_1+"u"+base_2+"an") base (base_1+"u"+base_2+"an") (palatalise base) (base_1+"u"+base_2+"an") (base_1+"òi"+base_2) (base_1+"òi"+base_2) (base_1+"òi"+base_2) (base_1+"u"+base_2) (lenite base) (base_1+"u"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN275"
  } ;

mkN276 : Str -> LinN ;
mkN276 base =
  case base of {
    base_1+"i"+base_2@? => mkNoun (base_1+"i"+base_2) (base_1+"ea"+base_2+"an") (base_1+"i"+base_2) (base_1+"ea"+base_2+"an") base (base_1+"ea"+base_2+"an") (palatalise base) (base_1+"ea"+base_2+"an") (base_1+"ea"+base_2+"ach") (lenite base) (base_1+"ea"+base_2+"ach") (base_1+"i"+base_2) (lenite base) (base_1+"i"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN276"
  } ;

mkN277 : Str -> LinN ;
mkN277 base = mk5N base base (base+"ean") (base+"eachan") (palatalise base) Masc ;

mkN278 : Str -> LinN ;
mkN278 base =
  case base of {
    base_1+"ui"+base_2@? => mkNoun (base_1+"ui"+base_2) (base_1+"a"+base_2+"annan") (base_1+"hui"+base_2) (base_1+"a"+base_2+"annan") (base_1+"ui"+base_2) (base_1+"a"+base_2+"annan") (base_1+"hui"+base_2) (base_1+"a"+base_2+"annan") (base_1+"a"+base_2+"a") (base_1+"ha"+base_2+"annan") (base_1+"a"+base_2+"a") (base_1+"a"+base_2+"annan") (base_1+"hui"+base_2) (base_1+"ha"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN278"
  } ;

mkN279 : Str -> LinN ;
mkN279 base =
  case base of {
    base_1+base_2@(?+?+?)+base_3@?+"ir" => mkNoun (base_1+base_2+base_3+"ir") (base_1+base_2+"r"+base_3+"ichean") (base_1+"h"+base_2+base_3+"ir") (base_1+base_2+"r"+base_3+"ichean") (base_1+base_2+base_3+"ir") (base_1+base_2+"r"+base_3+"ichean") (base_1+"h"+base_2+base_3+"ir") (base_1+base_2+"r"+base_3+"ichean") (base_1+base_2+base_3+"r") (base_1+"h"+base_2+"r"+base_3+"ichean") (base_1+base_2+base_3+"r") (base_1+base_2+"r"+base_3+"ichean") (base_1+"h"+base_2+base_3+"ir") (base_1+"h"+base_2+"r"+base_3+"ichean") Fem ;
    _ => error "Can't apply paradigm mkN279"
  } ;

mkN280 : Str -> LinN ;
mkN280 base =
  case base of {
    base_1+"n"+base_2@(?+?+?+?+?+?) => mkNoun (base_1+"n"+base_2) (base_1+base_2+"ean") (base_1+"n"+base_2) (palatalise base) base (base_1+base_2+"ean") (lenite base) (palatalise base) (base_1+"n"+base_2) (lenite base) (lenite (palatalise base)) (base_1+"n"+base_2) (lenite (palatalise base)) (base_1+"n"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN280"
  } ;

mkN281 : Str -> LinN ;
mkN281 base = mk5N base base (base+"e") (base+"ichean") (palatalise base) Masc ;

mkN282 : Str -> LinN ;
mkN282 base =
  case base of {
    "neach" => mk5N base base base "luchd" (palatalise base) Masc ;
    _ => error "Can't apply paradigm mkN282"
  } ;

mkN283 : Str -> LinN ;
mkN283 base =
  case base of {
    base_1+"ea"+base_2@(?+?) => mkNoun base (base_1+"i"+base_2) (base_1+"ea"+base_2) (palatalise base) base (base_1+"i"+base_2) (lenite base) (palatalise base) (base_1+"èi"+base_2) (lenite base) (lenite (palatalise base)) (base_1+"ea"+base_2) (lenite (palatalise base)) (base_1+"ea"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN283"
  } ;

mkN284 : Str -> LinN ;
mkN284 base = mk5N base base base (base+"thean") (palatalise base) Masc ;

mkN285 : Str -> LinN ;
mkN285 base =
  case base of {
    base_1+"ea"+base_2@? => mkNoun (base_1+"ea"+base_2) (base_1+"ea"+base_2+"an") (base_1+"ea"+base_2) (base_1+"ea"+base_2+"an") (base_1+"i"+base_2+"n") (base_1+"ea"+base_2+"an") (palatalise base) (base_1+"ea"+base_2+"an") (base_1+"i"+base_2+"n") (lenite base) (base_1+"i"+base_2+"n") (base_1+"ea"+base_2) (lenite base) (base_1+"ea"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN285"
  } ;

mkN287 : Str -> LinN ;
mkN287 base =
  case base of {
    base_1+base_2@(?+?)+base_3@?+"id" => mkNoun (base_1+base_2+base_3+"id") (base_1+"i"+base_2+"de"+base_3+"n") (base_1+base_2+base_3+"id") (palatalise base) base (base_1+"i"+base_2+"de"+base_3+"n") (lenite base) (palatalise base) (base_1+base_2+base_3+"d") (lenite base) (lenite (palatalise base)) (base_1+base_2+base_3+"id") (lenite (palatalise base)) (base_1+base_2+base_3+"ida") Masc ;
    _ => error "Can't apply paradigm mkN287"
  } ;

mkN288 : Str -> LinN ;
mkN288 base =
  case base of {
    base_1+"ì" => mkNoun (base_1+"ì") (base_1+"ithean") (base_1+"ì") (palatalise base) base (base_1+"ithean") (lenite base) (palatalise base) (base_1+"ì") (lenite base) (lenite (palatalise base)) (base_1+"ì") (lenite (palatalise base)) (base_1+"ìa") Masc ;
    _ => error "Can't apply paradigm mkN288"
  } ;

mkN289 : Str -> LinN ;
mkN289 base =
  case base of {
    base_1+"ì" => mkNoun (base_1+"ì") (base_1+"itheachan") (base_1+"ì") (palatalise base) base (base_1+"itheachan") (lenite base) (palatalise base) (base_1+"ith") (lenite base) (lenite (palatalise base)) (base_1+"ì") (lenite (palatalise base)) (base_1+"ìa") Masc ;
    _ => error "Can't apply paradigm mkN289"
  } ;

mkN290 : Str -> LinN ;
mkN290 base =
  case base of {
    base_1 => mkNoun (base_1) (base_1+"annan") (base_1) (base_1+"annan") (base_1) (base_1+"annan") (base_1) (base_1+"annan") (base_1) (base_1+"annan") (base_1) (base_1+"annan") (base_1) (base_1+"annan") Fem ;
    _ => error "Can't apply paradigm mkN290"
  } ;

mkN292 : Str -> LinN ;
mkN292 base = mk5N base base (base+"a") (base+"a") (palatalise base) Masc ;

mkN293 : Str -> LinN ;
mkN293 base =
  case base of {
    base_1 => mkNoun (base_1) (base_1+"idhean") (base_1) (base_1+"idhean") base (base_1+"idhean") (palatalise base) (base_1+"idhean") (base_1) (lenite base) (base_1) (base_1) (lenite base) (base_1+"a") Fem ;
    _ => error "Can't apply paradigm mkN293"
  } ;

mkN294 : Str -> LinN ;
mkN294 base =
  case base of {
    base_1 => mkNoun (base_1) (base_1) (base_1) (base_1) (base_1) (base_1) (palatalise base) (base_1) (base_1) (lenite base) (base_1) (base_1) (lenite base) (base_1+"a") Fem ;
    _ => error "Can't apply paradigm mkN294"
  } ;

mkN295 : Str -> LinN ;
mkN295 base =
  case base of {
    base_1+base_2@("i"|"ai")+"r" => mkNoun (base_1+base_2+"r") (base_1+"r"+base_2+"chean") (base_1+base_2+"r") (base_1+"r"+base_2+"chean") base (base_1+"r"+base_2+"chean") (palatalise base) (base_1+"r"+base_2+"chean") (base_1+base_2+"re") (lenite base) (base_1+base_2+"re") (base_1+base_2+"r") (lenite base) (base_1+base_2+"ra") Fem ;
    _ => error "Can't apply paradigm mkN295"
  } ;

mkN296 : Str -> LinN ;
mkN296 base =
  case base of {
    base_1+"ea"+base_2@(?+?) => mkNoun (base_1+"ea"+base_2) (base_1+"ea"+base_2+"ean") (base_1+"ea"+base_2) (palatalise base) base (base_1+"ea"+base_2+"ean") (lenite base) (palatalise base) (base_1+"i"+base_2) (lenite base) (lenite (palatalise base)) (base_1+"ea"+base_2) (lenite (palatalise base)) (base_1+"ea"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN296"
  } ;

mkN297 : Str -> LinN ;
mkN297 base =
  case base of {
    base_1+base_2@?+"ur" => mkNoun (base_1+base_2+"ur") (base_1+base_2+"uran") (base_1+base_2+"ur") (base_1+base_2+"uran") base (base_1+base_2+"uran") (palatalise base) (base_1+base_2+"uran") (base_1+"èir"+base_2) (lenite base) (base_1+"èir"+base_2) (base_1+base_2+"ur") (lenite base) (base_1+base_2+"ura") Fem ;
    _ => error "Can't apply paradigm mkN297"
  } ;

mkN298 : Str -> LinN ;
mkN298 base =
  case base of {
    base_1+base_2@?+"a"+base_3@? => mkNoun (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"tan") (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"tan") base (base_1+base_2+"a"+base_3+"tan") (palatalise base) (base_1+base_2+"a"+base_3+"tan") (base_1+"é"+base_2+base_3) (lenite base) (base_1+"é"+base_2+base_3) (base_1+base_2+"a"+base_3) (lenite base) (base_1+base_2+"a"+base_3+"a") Fem ;
    _ => error "Can't apply paradigm mkN298"
  } ;

mkN299 : Str -> LinN ;
mkN299 base = mk5N base base (base+"a") (base+"nan") (palatalise base) Masc ;

mkN300 : Str -> LinN ;
mkN300 base =
  case base of {
    base_1+"iu"+base_2@(?+?)+"a"+base_3@? => mkNoun (base_1+"iu"+base_2+"a"+base_3) (base_1+"ea"+base_2+base_3+"aichean") (base_1+"hiu"+base_2+"a"+base_3) (base_1+"ea"+base_2+base_3+"aichean") (base_1+"iu"+base_2+"a"+base_3) (base_1+"ea"+base_2+base_3+"aichean") (base_1+"hiu"+base_2+"a"+base_3) (base_1+"ea"+base_2+base_3+"aichean") (base_1+"ea"+base_2+"a"+base_3) (base_1+"hea"+base_2+base_3+"aichean") (base_1+"ea"+base_2+"a"+base_3) (base_1+"ea"+base_2+base_3+"aichean") (base_1+"hiu"+base_2+"a"+base_3) (base_1+"hea"+base_2+base_3+"aichean") Fem ;
    _ => error "Can't apply paradigm mkN300"
  } ;

mkN301 : Str -> LinN ;
mkN301 base =
  case base of {
    base_1+"a" => mkNoun (base_1+"a") (base_1+"othan") (base_1+"a") (palatalise base) base (base_1+"othan") (lenite base) (palatalise base) (base_1+"o") (lenite base) (lenite (palatalise base)) (base_1+"a") (lenite (palatalise base)) (base_1+"aa") Masc ;
    _ => error "Can't apply paradigm mkN301"
  } ;

mkN302 : Str -> LinN ;
mkN302 base =
  case base of {
    base_1+"i"+base_2@? => mkNoun (base_1+"i"+base_2) (base_1+"i"+base_2) (base_1+"i"+base_2) (base_1+"i"+base_2) base (base_1+"i"+base_2) (palatalise base) (base_1+"i"+base_2) (base_1+base_2+"ach") (lenite base) (base_1+base_2+"ach") (base_1+"i"+base_2) (lenite base) (base_1+"i"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN302"
  } ;

mkN303 : Str -> LinN ;
mkN303 base =
  case base of {
    base_1 => mkNoun (base_1) (base_1+"ean") (base_1) (base_1+"ean") (base_1) (base_1+"ean") (base_1) (base_1+"ean") (base_1+"e") (base_1+"ean") (base_1+"e") (base_1+"ean") (base_1) (base_1+"ean") Fem ;
    _ => error "Can't apply paradigm mkN303"
  } ;

mkN304 : Str -> LinN ;
mkN304 base =
  case base of {
    base_1+base_2@?+"ul" => mkNoun (base_1+base_2+"ul") (base_1+base_2+"ultan") (base_1+base_2+"ul") (base_1+base_2+"ultan") base (base_1+base_2+"ultan") (palatalise base) (base_1+base_2+"ultan") (base_1+"èil"+base_2) (lenite base) (base_1+"èil"+base_2) (base_1+base_2+"ul") (lenite base) (base_1+base_2+"ula") Fem ;
    _ => error "Can't apply paradigm mkN304"
  } ;

mkN305 : Str -> LinN ;
mkN305 base =
  case base of {
    base_1+"i"+base_2@? => mkNoun (base_1+"i"+base_2) (base_1+base_2+"ean") (base_1+"i"+base_2) (palatalise base) base (base_1+base_2+"ean") (lenite base) (palatalise base) (base_1+base_2+"e") (lenite base) (lenite (palatalise base)) (base_1+"i"+base_2) (lenite (palatalise base)) (base_1+"i"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN305"
  } ;

mkN306 : Str -> LinN ;
mkN306 base = mk5N base base base (base+"rean") (palatalise base) Masc ;

mkN307 : Str -> LinN ;
mkN307 base =
  case base of {
    base_1+base_2@(?+?+?) => mkNoun (base_1+base_2) (base_1+base_2+"an") (base_1+base_2) (base_1+base_2+"an") (base_1+base_2) (base_1+base_2+"an") (base_1+"i"+base_2) (base_1+base_2+"an") (base_1+"i"+base_2+"e") (lenite base) (base_1+"i"+base_2+"e") (base_1+base_2) (base_1+base_2) (base_1+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN307"
  } ;

mkN308 : Str -> LinN ;
mkN308 base = mk5N base base (base+"a") (base+"e") (palatalise base) Masc ;

mkN309 : Str -> LinN ;
mkN309 base =
  case base of {
    base_1+base_2@?+"ad" => mkNoun (base_1+base_2+"ad") (base_1+"d"+base_2+"an") (base_1+base_2+"ad") (base_1+"d"+base_2+"an") base (base_1+"d"+base_2+"an") (palatalise base) (base_1+"d"+base_2+"an") (base_1+"d"+base_2) (lenite base) (base_1+"d"+base_2) (base_1+base_2+"ad") (lenite base) (base_1+base_2+"ada") Fem ;
    _ => error "Can't apply paradigm mkN309"
  } ;

mkN310 : Str -> LinN ;
mkN310 base =
  case base of {
    base_1@?+base_2+base_3@("th"|?) => mkNoun (base_1+base_2+base_3) (base_1+base_2+base_3+"an") (base_1+base_2+base_3) (base_1+base_2+base_3+"an") (base_1+base_2+base_3) (base_1+base_2+base_3+"an") (base_1+base_2+base_3) (base_1+base_2+base_3+"an") (base_1+base_2+"i"+base_3) (base_1+"h"+base_2+base_3+"an") (base_1+base_2+"i"+base_3) (base_1+base_2+base_3+"an") (base_1+"h"+base_2+"i"+base_3) (base_1+"h"+base_2+base_3+"an") Masc ;
    _ => error "Can't apply paradigm mkN310"
  } ;

mkN311 : Str -> LinN ;
mkN311 base =
  case base of {
    base_1+base_2@? => mkNoun (base_1+base_2) (base_1+"inn"+base_2+"an") (base_1+base_2) (base_1+"inn"+base_2+"an") base (base_1+"inn"+base_2+"an") (palatalise base) (base_1+"inn"+base_2+"an") (base_1+base_2) (lenite base) (base_1+base_2) (base_1+base_2) (lenite base) (base_1+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN311"
  } ;

mkN312 : Str -> LinN ;
mkN312 base = mk5N base base base (base+"innean") (palatalise base) Masc ;

mkN313 : Str -> LinN ;
mkN313 base =
  case base of {
    base_1+base_2@(?+?+?+?)+"i"+base_3@? => mkNoun (base_1+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+base_3) (base_1+"h"+base_2+"i"+base_3+"ean") (base_1+base_2+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+"h"+base_2+"i"+base_3) (base_1+"h"+base_2+"i"+base_3+"ean") Masc ;
    _ => error "Can't apply paradigm mkN313"
  } ;

mkN314 : Str -> LinN ;
mkN314 base =
  case base of {
    base_1+base_2@(?+?+?+?+?+?)+"i"+base_3@? => mkNoun (base_1+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+base_3) (base_1+"h"+base_2+"i"+base_3+"ean") (base_1+base_2+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+"h"+base_2+"i"+base_3) (base_1+"h"+base_2+"i"+base_3+"ean") Fem ;
    _ => error "Can't apply paradigm mkN314"
  } ;

mkN315 : Str -> LinN ;
mkN315 base =
  case base of {
    base_1+"ia"+base_2@? => mkNoun (base_1+"ia"+base_2) (base_1+"ea"+base_2+"an") (base_1+"ia"+base_2) (base_1+"ea"+base_2+"an") (base_1+"ithi"+base_2+"n") (base_1+"ea"+base_2+"an") (palatalise base) (base_1+"ea"+base_2+"an") (base_1+"ei"+base_2+"e") (lenite base) (base_1+"ei"+base_2+"e") (base_1+"ia"+base_2) (lenite base) (base_1+"ia"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN315"
  } ;

mkN316 : Str -> LinN ;
mkN316 base =
  case base of {
    base_1+"ia"+base_2@? => mkNoun (base_1+"ia"+base_2) (base_1+"ia"+base_2+"tan") (base_1+"ia"+base_2) (palatalise base) base (base_1+"ia"+base_2+"tan") (lenite base) (palatalise base) (base_1+"ì"+base_2+"e") (lenite base) (lenite (palatalise base)) (base_1+"ia"+base_2) (lenite (palatalise base)) (base_1+"ia"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN316"
  } ;

mkN317 : Str -> LinN ;
mkN317 base =
  case base of {
    base_1+base_2@? => mkNoun (base_1+base_2) (base_1+base_2+"an") (base_1+base_2) (palatalise base) base (base_1+base_2+"an") (lenite base) (palatalise base) (base_1+"i"+base_2) (lenite base) (base_1+base_2) (palatalise base) (base_1+base_2+"a") (lenite base+"a") Masc ;
    _ => error "Can't apply paradigm mkN317"
  } ;

mkN318 : Str -> LinN ;
mkN318 base =
  case base of {
    base_1+base_2@?+"ar" => mkNoun (base_1+base_2+"ar") (base_1+base_2+"aran") (base_1+base_2+"ar") (palatalise base) base (base_1+base_2+"aran") (lenite base) (palatalise base) (base_1+"ir"+base_2) (lenite base) (base_1+base_2+"ar") (palatalise base) (base_1+base_2+"ara") (lenite base+"a") Masc ;
    _ => error "Can't apply paradigm mkN318"
  } ;

mkN319 : Str -> LinN ;
mkN319 base =
  case base of {
    base_1+"i"+base_2@(?+?)+base_3@?+"ann" => mkNoun (base_1+"i"+base_2+base_3+"ann") (nonExist) (base_1+"i"+base_2+base_3+"ann") (nonExist) base (nonExist) (palatalise base) nonExist (base_1+"ì"+base_2+"n"+base_3) (lenite base) (base_1+"ì"+base_2+"n"+base_3) (base_1+"i"+base_2+base_3+"ann") (lenite base) (base_1+"i"+base_2+base_3+"anna") Fem ;
    _ => error "Can't apply paradigm mkN319"
  } ;

mkN320 : Str -> LinN ;
mkN320 base =
  case base of {
    base_1+"u"+base_2@(?+?)+base_3@?+"l" => mkNoun (base_1+"u"+base_2+base_3+"l") (base_1+"ù"+base_2+"l"+base_3+"ichean") (base_1+"u"+base_2+base_3+"l") (palatalise base) base (base_1+"ù"+base_2+"l"+base_3+"ichean") (lenite base) (palatalise base) (base_1+"u"+base_2+base_3+"il") (lenite base) (lenite (palatalise base)) (base_1+"u"+base_2+base_3+"l") (lenite (palatalise base)) (base_1+"u"+base_2+base_3+"la") Masc ;
    _ => error "Can't apply paradigm mkN320"
  } ;

mkN321 : Str -> LinN ;
mkN321 base =
  case base of {
    base_1+base_2@(?+?)+base_3@? => mkNoun (base_1+base_2+base_3) (base_1+base_2+base_3+"an") (base_1+base_2+base_3) (base_1+base_2+base_3+"an") (base_1+base_2+"i"+base_3) (base_1+base_2+base_3+"an") (base_1+base_2+"i"+base_3) (base_1+base_2+base_3+"an") (base_1+base_2+"i"+base_3) (base_1+"h"+base_2+base_3) (base_1+base_2+"i"+base_3+"e") (base_1+base_2+base_3) (base_1+"h"+base_2+base_3) (base_1+"h"+base_2+base_3+"a") Fem ;
    _ => error "Can't apply paradigm mkN321"
  } ;

mkN322 : Str -> LinN ;
mkN322 base =
  case base of {
    base_1+base_2@?+"a"+base_3@(?+?) => mkNoun (base_1+base_2+"a"+base_3) (base_1+"è"+base_2+base_3+"tean") (base_1+base_2+"a"+base_3) (palatalise base) base (base_1+"è"+base_2+base_3+"tean") (lenite base) (palatalise base) (base_1+"è"+base_2+base_3) (lenite base) (lenite (palatalise base)) (base_1+base_2+"a"+base_3) (lenite (palatalise base)) (base_1+base_2+"a"+base_3+"a") Masc ;
    _ => error "Can't apply paradigm mkN322"
  } ;

mkN323 : Str -> LinN ;
mkN323 base =
  case base of {
    base_1+base_2@?+"a"+base_3@?+"aid" => mkNoun (base_1+base_2+"a"+base_3+"aid") (base_1+"è"+base_2+base_3+"tean") (base_1+base_2+"a"+base_3+"aid") (base_1+"è"+base_2+base_3+"tean") base (base_1+"è"+base_2+base_3+"tean") (palatalise base) (base_1+"è"+base_2+base_3+"tean") (base_1+"è"+base_2+base_3+"te") (lenite base) (base_1+"è"+base_2+base_3+"te") (base_1+base_2+"a"+base_3+"aid") (lenite base) (base_1+base_2+"a"+base_3+"aida") Fem ;
    _ => error "Can't apply paradigm mkN323"
  } ;

mkN324 : Str -> LinN ;
mkN324 base =
  case base of {
    base_1+base_2@(?+?+?+?) => mkNoun (base_1+base_2) (base_1+base_2+"an") (base_1+base_2) (base_1+base_2+"an") (base_1+base_2) (base_1+base_2+"an") (base_1+base_2) (base_1+base_2+"an") (base_1+base_2) (base_1+"h"+base_2+"an") (base_1+base_2) (base_1+base_2+"an") (base_1+"h"+base_2) (base_1+"h"+base_2+"an") Fem ;
    _ => error "Can't apply paradigm mkN324"
  } ;

mkN325 : Str -> LinN ;
mkN325 base =
  case base of {
    base_1+"ua"+base_2@(?+?) => mkNoun (base_1+"ua"+base_2) (base_1+"òi"+base_2) (base_1+"ua"+base_2) (palatalise base) base (base_1+"òi"+base_2) (lenite base) (palatalise base) (base_1+"uai"+base_2) (lenite base) (lenite (palatalise base)) (base_1+"ua"+base_2) (lenite (palatalise base)) (base_1+"ua"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN325"
  } ;

mkN326 : Str -> LinN ;
mkN326 base =
  case base of {
    base_1+"d"+base_2@(?+?)+"nn" => mkNoun (base_1+"d"+base_2+"nn") (base_1+base_2+"dhean") (base_1+"d"+base_2+"nn") (palatalise base) base (base_1+base_2+"dhean") nonExist (palatalise base) (base_1+base_2+"dh") (lenite base) (lenite (palatalise base)) (base_1+"d"+base_2+"nn") (lenite (palatalise base)) (base_1+"d"+base_2+"nna") Masc ;
    _ => error "Can't apply paradigm mkN326"
  } ;

mkN327 : Str -> LinN ;
mkN327 base =
  case base of {
    base_1+"i"+base_2@?+"e"+base_3@(?+?+?+?)+base_4@(?+?) => mkNoun (base_1+"i"+base_2+"e"+base_3+base_4) (base_1+"a"+base_2+base_3+"i"+base_4+"ean") (base_1+"i"+base_2+"e"+base_3+base_4) (palatalise base) base (base_1+"a"+base_2+base_3+"i"+base_4+"ean") nonExist (palatalise base) (base_1+"a"+base_2+base_3+"i"+base_4) (lenite base) (lenite (palatalise base)) (base_1+"i"+base_2+"e"+base_3+base_4) (lenite (palatalise base)) (base_1+"i"+base_2+"e"+base_3+base_4+"a") Masc ;
    _ => error "Can't apply paradigm mkN327"
  } ;

mkN328 : Str -> LinN ;
mkN328 base =
  case base of {
    base_1+base_2@(?+?+?+?+?+?)+"a"+base_3@? => mkNoun (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+base_2+"i"+base_3) (base_1+"h"+base_2+"a"+base_3+"an") (base_1+base_2+"i"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+"h"+base_2+"i"+base_3) (base_1+"h"+base_2+"a"+base_3+"an") Masc ;
    _ => error "Can't apply paradigm mkN328"
  } ;

mkN329 : Str -> LinN ;
mkN329 base =
  case base of {
    base_1+"a"+base_2@(?+?) => mkNoun (base_1+"a"+base_2) (base_1+"a"+base_2+"an") (base_1+"a"+base_2) (base_1+"a"+base_2+"an") (base_1+"a"+base_2) (base_1+"a"+base_2+"an") (base_1+"i"+base_2) (base_1+"a"+base_2+"an") (base_1+"i"+base_2+"e") (lenite base) (base_1+"i"+base_2+"e") (base_1+"a"+base_2) (base_1+"a"+base_2) (base_1+"a"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN329"
  } ;

mkN330 : Str -> LinN ;
mkN330 base =
  case base of {
    base_1+base_2@?+"a"+base_3@? => mkNoun (base_1+base_2+"a"+base_3) (base_1+"è"+base_2+base_3+"ean") (base_1+base_2+"a"+base_3) (base_1+"è"+base_2+base_3+"ean") base (base_1+"è"+base_2+base_3+"ean") (palatalise base) (base_1+"è"+base_2+base_3+"ean") (base_1+"è"+base_2+base_3+"e") (lenite base) (base_1+"è"+base_2+base_3+"e") (base_1+base_2+"a"+base_3) (lenite base) (base_1+base_2+"a"+base_3+"a") Fem ;
    _ => error "Can't apply paradigm mkN330"
  } ;

mkN331 : Str -> LinN ;
mkN331 base =
  case base of {
    base_1+base_2@(?+?)+base_3@? => mkNoun (base_1+base_2+base_3) (base_1+base_2+base_3+"an") (base_1+base_2+base_3) (base_1+base_2+base_3+"an") (base_1+base_2+"i"+base_3) (base_1+base_2+base_3+"an") (base_1+base_2+"i"+base_3) (base_1+base_2+base_3+"an") (base_1+base_2+"i"+base_3+"e") (base_1+"h"+base_2+base_3) (base_1+base_2+"i"+base_3+"e") (base_1+base_2+base_3) (base_1+"h"+base_2+base_3) (base_1+"h"+base_2+base_3+"a") Fem ;
    _ => error "Can't apply paradigm mkN331"
  } ;

mkN332 : Str -> LinN ;
mkN332 base =
  case base of {
    base_1+"i"+base_2@? => mkNoun (base_1+"i"+base_2) (base_1+base_2+"ichean") (base_1+"i"+base_2) (base_1+base_2+"ichean") (base_1+"i"+base_2) (base_1+base_2+"ichean") (base_1+"i"+base_2) (base_1+base_2+"ichean") (base_1+base_2+"each") (base_1+base_2+"ichean") (base_1+base_2+"each") (base_1+base_2+"ichean") (base_1+"i"+base_2) (base_1+base_2+"ichean") Fem ;
    _ => error "Can't apply paradigm mkN332"
  } ;

mkN333 : Str -> LinN ;
mkN333 base = mk5N base base (base+"each") (base+"ean") (palatalise base) Masc ;

mkN334 : Str -> LinN ;
mkN334 base =
  case base of {
    base_1+"d"+base_2@? => mkNoun (base_1+"d"+base_2) (base_1+"g"+base_2+"an") (base_1+"d"+base_2) (base_1+"g"+base_2+"an") base (base_1+"g"+base_2+"an") (palatalise base) (base_1+"g"+base_2+"an") (base_1+"ig"+base_2) (lenite base) (base_1+"ig"+base_2) (base_1+"d"+base_2) (lenite base) (base_1+"d"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN334"
  } ;

mkN337 : Str -> LinN ;
mkN337 base = mk5N base base (base+"ach") (base+"an") (palatalise base) Masc ;

mkN338 : Str -> LinN ;
mkN338 base =
  case base of {
    base_1+base_2@(?+?+?) => mkNoun (base_1+base_2) (base_1+base_2+"ean") (base_1+base_2) (base_1+base_2+"ean") (base_1+base_2) (base_1+base_2+"ean") (base_1+base_2) (base_1+base_2+"ean") (base_1+base_2+"e") (base_1+"h"+base_2+"ean") (base_1+base_2+"e") (base_1+base_2+"ean") (base_1+"h"+base_2) (base_1+"h"+base_2+"ean") Fem ;
    _ => error "Can't apply paradigm mkN338"
  } ;

mkN339 : Str -> LinN ;
mkN339 base =
  case base of {
    base_1+"o"+base_2@? => mkNoun (base_1+"o"+base_2) (base_1+base_2) (base_1+"o"+base_2) (palatalise base) base (base_1+base_2) (lenite base) (palatalise base) (base_1+base_2) (lenite base) (lenite (palatalise base)) (base_1+"o"+base_2) (lenite (palatalise base)) (base_1+"o"+base_2+"a") Masc ;
    _ => error "Can't apply paradigm mkN339"
  } ;

mkN340 : Str -> LinN ;
mkN340 base =
  case base of {
    base_1+base_2@(?+?+?+?+?+?+?) => mkNoun (base_1+base_2) (base_1+base_2+"an") (base_1+base_2) (base_1+base_2+"an") (base_1+base_2) (base_1+base_2+"an") (base_1+base_2) (base_1+base_2+"an") (base_1+base_2) (base_1+"h"+base_2+"an") (base_1+base_2) (base_1+base_2+"an") (base_1+"h"+base_2) (base_1+"h"+base_2+"an") Masc ;
    _ => error "Can't apply paradigm mkN340"
  } ;

mkN341 : Str -> LinN ;
mkN341 base =
  case base of {
    base_1+base_2@?+"i"+base_3@? => mkNoun (base_1+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+base_3+"a") (base_1+base_2+base_3) (base_1+base_2+base_3+"a") (base_1+base_2+"i"+base_3+"ean") (base_1+"h"+base_2+"i"+base_3) (base_1+"h"+base_2+base_3+"a") Fem ;
    _ => error "Can't apply paradigm mkN341"
  } ;

mkN342 : Str -> LinN ;
mkN342 base =
  case base of {
    base_1+"ù"+base_2@(?+?) => mkNoun (base_1+"ù"+base_2) (base_1+"u"+base_2+"eannan") (base_1+"ù"+base_2) (base_1+"u"+base_2+"eannan") base (base_1+"u"+base_2+"eannan") (palatalise base) (base_1+"u"+base_2+"eannan") (base_1+"ù"+base_2+"e") (lenite base) (base_1+"ù"+base_2+"e") (base_1+"ù"+base_2) (lenite base) (base_1+"ù"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN342"
  } ;

mkN343 : Str -> LinN ;
mkN343 base =
  case base of {
    base_1+base_2@(?+?+?+?+?+?) => mkNoun (base_1+base_2) (base_1+base_2+"ean") (base_1+base_2) (base_1+base_2+"ean") (base_1+base_2) (base_1+base_2+"ean") (base_1+base_2) (base_1+base_2+"ean") (base_1+base_2+"e") (base_1+"h"+base_2+"ean") (base_1+base_2+"e") (base_1+base_2+"ean") (base_1+"h"+base_2) (base_1+"h"+base_2+"ean") Fem ;
    _ => error "Can't apply paradigm mkN343"
  } ;

mkN344 : Str -> LinN ;
mkN344 base =
  case base of {
    base_1@?+base_2 => mkNoun (base_1+base_2) (base_1+base_2+"ean") (base_1+base_2) (base_1+base_2+"ean") (base_1+base_2) (base_1+base_2+"ean") (base_1+base_2) (base_1+base_2+"ean") (base_1+base_2+"e") (base_1+"h"+base_2+"ean") (base_1+base_2+"e") (base_1+base_2+"ean") (base_1+"h"+base_2) (base_1+"h"+base_2+"ean") Masc ;
    _ => error "Can't apply paradigm mkN344"
  } ;

mkN345 : Str -> LinN ;
mkN345 base =
  case base of {
    base_1+base_2@(?+?)+"a"+base_3@(?+?) => mkNoun (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+base_2+"a"+base_3) (base_1+base_2+"a"+base_3+"an") (base_1+base_2+base_3+"ainn") (base_1+"h"+base_2+"a"+base_3+"an") (base_1+base_2+base_3+"ainn") (base_1+base_2+"a"+base_3+"an") (base_1+"h"+base_2+"a"+base_3) (base_1+"h"+base_2+"a"+base_3+"an") Masc ;
    _ => error "Can't apply paradigm mkN345"
  } ;

mkN346 : Str -> LinN ;
mkN346 base = mk5N base base (base+"chan") (base+"ichean") (palatalise base) Masc ;

mkN347 : Str -> LinN ;
mkN347 base =
  case base of {
    base_1+base_2@(?+?+?+?)+base_3@?+"ir" => mkNoun base (base_1+base_2+"r"+base_3+"ichean") (base_1+base_2+base_3+"ir") (base_1+base_2+"r"+base_3+"ichean") (base_1+base_2+base_3+"ir") (base_1+base_2+"r"+base_3+"ichean") (base_1+base_2+base_3+"ir") (base_1+base_2+"r"+base_3+"ichean") (base_1+base_2+"r"+base_3+"ch") (base_1+"h"+base_2+"r"+base_3+"ichean") (base_1+base_2+"r"+base_3+"ch") (base_1+base_2+"r"+base_3+"ichean") (base_1+"h"+base_2+base_3+"ir") (base_1+"h"+base_2+"r"+base_3+"ichean") Fem ;
    _ => error "Can't apply paradigm mkN347"
  } ;

mkN348 : Str -> LinN ;
mkN348 base =
  case base of {
    base_1+base_2@(?+?+?+?+?) => mkNoun base (base+"n") base (base+"nnan") base (base+"nnan") base (base+"nnan") base (base_1+"h"+base_2+"nnan") base (base+"nnan") (base_1+"h"+base_2) (base_1+"h"+base_2+"nnan") Fem ;
    _ => error "Can't apply paradigm mkN348"
  } ;

mkN349 : Str -> LinN ;
mkN349 base =
  case base of {
    base_1+"à"+base_2@(?+?) => mkNoun base (base+"an") base (base+"an") base (base+"an") (palatalise base) (base+"an") (base_1+"a"+base_2+"a") (lenite base) (base_1+"a"+base_2+"a") base (lenite base) (base+"a") Fem ;
    _ => error "Can't apply paradigm mkN349"
  } ;

mkN350 : Str -> LinN ;
mkN350 base =
  case base of {
    "tighearna" => mkNoun base (base+"n") base (base+"n") base (base+"n") base (base+"n") base "thighearnan" base (base+"n") (lenite base) (lenite base+"n") Masc ;
    _ => error "Can't apply paradigm mkN350"
  } ;

mkN351 : Str -> LinN ;
mkN351 base = mk5N base base base (base+"en") (palatalise base) Masc ;

mkN352 : Str -> LinN ;
mkN352 base =
  case base of {
    base_1+"ar" => mk5N base base (base_1+"air") (base_1+"rachan") (palatalise base) Masc ;
    _ => error "Can't apply paradigm mkN352"
  } ;

mkN353 : Str -> LinN ;
mkN353 base =
  case base of {
    base_1+"o"+base_2@(?+?)+base_3@?+"s" => mkNoun (base_1+"o"+base_2+base_3+"s") (base_1+"òi"+base_2+"se"+base_3+"n") (base_1+"o"+base_2+base_3+"s") (palatalise base) base (base_1+"òi"+base_2+"se"+base_3+"n") (lenite base) (palatalise base) (base_1+"o"+base_2+base_3+"is") (lenite base) (lenite (palatalise base)) (base_1+"o"+base_2+base_3+"s") (lenite (palatalise base)) (base_1+"o"+base_2+base_3+"sa") Masc ;
    _ => error "Can't apply paradigm mkN353"
  } ;

mkN354 : Str -> LinN ;
mkN354 base =
  case base of {
    base_1+"eu"+base_2@(?+?) => mkNoun (base_1+"eu"+base_2) (base_1+"eu"+base_2+"an") (base_1+"eu"+base_2) (base_1+"eu"+base_2+"an") base (base_1+"eu"+base_2+"an") (palatalise base) (base_1+"eu"+base_2+"an") (base_1+"èi"+base_2) (lenite base) (base_1+"èi"+base_2) (base_1+"eu"+base_2) (lenite base) (base_1+"eu"+base_2+"a") Fem ;
    _ => error "Can't apply paradigm mkN354"
  } ;

mkN356 : Str -> LinN ;
mkN356 base =
  case base of {
    base_1+base_2@(?+?)+"i"+base_3@(?+?) => mkNoun (base_1+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+"i"+base_3) (base_1+base_2+"i"+base_3+"ean") (base_1+base_2+"i"+base_3+"e") (base_1+"h"+base_2+"i"+base_3+"ean") (base_1+base_2+base_3+"ad") (base_1+base_2+"i"+base_3+"ean") (base_1+"h"+base_2+"i"+base_3) (base_1+"h"+base_2+"i"+base_3+"ean") Fem ;
    _ => error "Can't apply paradigm mkN356"
  } ;

mkN357 : Str -> LinN ;
mkN357 base =
  case base of {
    base_1@?+base_2+base_3@("g"|"gh") => mkNoun base (base+"an") base (base+"an") (base_1+base_2+"i"+base_3) (base+"an") (base_1+base_2+"i"+base_3) (base+"an") (base_1+base_2+"i"+base_3+"e") (base_1+"h"+base_2+base_3) (base_1+base_2+"i"+base_3+"e") base (base_1+"h"+base_2+base_3) (base_1+"h"+base_2+base_3+"a") Fem ;
    _ => error "Can't apply paradigm mkN357"
  } ;

mkN358 : Str -> LinN ;
mkN358 base =
  case base of {
    base_1+"i"+base_2@(?+?) => mkNoun base (base_1+base_2+"an") base (base_1+base_2+"an") base (base_1+base_2+"an") (palatalise base) (base_1+base_2+"an") (base_1+base_2+"a") (lenite base) (base_1+base_2+"a") base (lenite base) (base+"a") Fem ;
    _ => error "Can't apply paradigm mkN358"
  } ;

mkN359 : Str -> LinN ;
mkN359 base =
  case base of {
    base_1+"òi"+base_2@? => mkNoun base (base+"an") base (base+"an") base (base+"an") (palatalise base) (base+"an") (base_1+"o"+base_2+"a") (lenite base) (base_1+"o"+base_2+"a") base (lenite base) (base+"a") Fem ;
    _ => error "Can't apply paradigm mkN359"
  } ;

mkN360 : Str -> LinN ;
mkN360 base =
  case base of {
    base_1+base_2@(?+?) => mkNoun base (base_1+"i"+base_2+"ean") base (base_1+"i"+base_2+"ean") (base_1+"i"+base_2) (base_1+"i"+base_2+"ean") (base_1+"i"+base_2) (base_1+"i"+base_2+"ean") (base_1+"i"+base_2+"e") (lenite base) (base_1+"i"+base_2+"e") base base (base+"a") Masc ;
    _ => error "Can't apply paradigm mkN360"
  } ;

mkN361 : Str -> LinN ;
mkN361 base =
  case base of {
    "u"+base_1+base_2@?+"l" => mkNoun base ("ù"+base_1+"l"+base_2+"n") base (palatalise base) base ("ù"+base_1+"l"+base_2+"n") (lenite base) (palatalise base) ("u"+base_1+base_2+"il") (lenite base) (lenite (palatalise base)) base (lenite (palatalise base)) (base+"a") Masc ;
    _ => error "Can't apply paradigm mkN361"
  } ;

mkN362 : Str -> LinN ;
mkN362 base = mk5N base base (palatalise base) (base+"nan") (palatalise base) Masc ;

mkN363 : Str -> LinN ;
mkN363 base = mkNoun base (base+"achan") base (base+"achan") base (base+"achan") base (base+"achan") base (base+"achan") base (base+"achan") base (base+"achan") Masc ;

mkN364 : Str -> LinN ;
mkN364 base =
  case base of {
    "à"+base_1 => mk5N base base ("a"+base_1+"a") ("a"+base_1+"annan") (palatalise base) Masc ;
    _ => error "Can't apply paradigm mkN364"
  } ;

mkN365 : Str -> LinN ;
mkN365 base = mkNoun base (base+"an") base (base+"an") (base+"a") (base+"an") (base+"a") (base+"an") (base+"a") (base+"an") (base+"a") (base+"an") base (base+"an") Fem ;

mkN367 : Str -> LinN ;
mkN367 base =
  case base of {
    "ò"+base_1 => mk5N base base ("ùi"+base_1) ("ùi"+base_1) (palatalise base) Masc ;
    _ => error "Can't apply paradigm mkN367"
  } ;

mkV001 : Str -> V ;
mkV001 base =
  case base of {
    "abai"+base_1 => lin V
      { s = "abai"+base_1 ;
        conditional = table {
                        Sg => "thei"+base_1+"inn" ;
                        Pl => "thei"+base_1+"eadh"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => "ab"+base_1+"am" ;
                               Pl => "ab"+base_1+"amaid"
                             } ;
                       P2 => table {
                               Sg => "abai"+base_1 ;
                               Pl => "ab"+base_1+"aibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => "ab"+base_1+"adh"
                             }
                     } ;
        future = table {
                   Indep => "thei"+base_1 ;
                   Dep => "abai"+base_1
                 } ;
        past = table {
                 Indep => "thui"+base_1+"t" ;
                 Dep => "tui"+base_1+"t"
               } ;
        noun = base_1+"àdh" ;
        participle = base_1+"àite"
      };
    _ => error "Can't apply paradigm mkV001"
  } ;

mkV002 : Str -> V ;
mkV002 base =
  case base of {
    base_1+"i"+base_2@("c"|(?+?)) => lin V
      { s = base_1+"i"+base_2 ;
        conditional = table {
                        Sg => "dh'"+base_1+"i"+base_2+"inn" ;
                        Pl => "dh'"+base_1+"i"+base_2+"eamaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+"i"+base_2+"eam" ;
                               Pl => base_1+"i"+base_2+"eamaid"
                             } ;
                       P2 => table {
                               Sg => base_1+"i"+base_2 ;
                               Pl => base_1+"i"+base_2+"ibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+"i"+base_2+"eadh"
                             }
                     } ;
        future = table {
                   Indep => base_1+"i"+base_2+"idh" ;
                   Dep => base_1+"i"+base_2
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"i"+base_2 ;
                 Dep => "dh'"+base_1+"i"+base_2
               } ;
        noun = base_1+base_2+"adh" ;
        participle = base_1+"i"+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV002"
  } ;

mkV003 : Str -> V ;
mkV003 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => "dh'"+base_1+"ainn" ;
                        Pl => "dh'"+base_1+"amaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+"am" ;
                               Pl => base_1+"amaid"
                             } ;
                       P2 => table {
                               Sg => base_1 ;
                               Pl => base_1+"aibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+"adh"
                             }
                     } ;
        future = table {
                   Indep => base_1+"aidh" ;
                   Dep => base_1
                 } ;
        past = table {
                 Indep => "dh'"+base_1 ;
                 Dep => "dh'"+base_1
               } ;
        noun = base_1+"adh" ;
        participle = base_1+"te"
      };
    _ => error "Can't apply paradigm mkV003"
  } ;

mkV004 : Str -> V ;
mkV004 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1 ;
                 Dep => nonExist
               } ;
        noun = base_1+"adh" ;
        participle = nonExist
      };
    _ => error "Can't apply paradigm mkV004"
  } ;

mkV005 : Str -> V ;
mkV005 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1 ;
                 Dep => nonExist
               } ;
        noun = base_1+"t" ;
        participle = base_1+"te"
      };
    _ => error "Can't apply paradigm mkV005"
  } ;

mkV006 : Str -> V ;
mkV006 base =
  case base of {
    base_1+"i"+base_2@(?+?) => lin V
      { s = base_1+"i"+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"i"+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"i"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+"ea"+base_2+"adh" ;
        participle = base_1+"i"+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV006"
  } ;

mkV007 : Str -> V ;
mkV007 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1 ;
                 Dep => nonExist
               } ;
        noun = base_1+"mhainn" ;
        participle = base_1+"ta"
      };
    _ => error "Can't apply paradigm mkV007"
  } ;

mkV008 : Str -> V ;
mkV008 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1 ;
                 Dep => nonExist
               } ;
        noun = base_1 ;
        participle = base_1+"te"
      };
    _ => error "Can't apply paradigm mkV008"
  } ;

mkV009 : Str -> V ;
mkV009 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1 ;
                 Dep => nonExist
               } ;
        noun = base_1 ;
        participle = base_1+"te"
      };
    _ => error "Can't apply paradigm mkV009"
  } ;

mkV010 : Str -> V ;
mkV010 base =
  case base of {
    base_1+"i"+base_2@("m"|(?+?)) => lin V
      { s = base_1+"i"+base_2 ;
        conditional = table {
                        Sg => "dh'"+base_1+"i"+base_2+"inn" ;
                        Pl => "dh'"+base_1+"i"+base_2+"eamaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+"i"+base_2+"eam" ;
                               Pl => base_1+"i"+base_2+"eamaid"
                             } ;
                       P2 => table {
                               Sg => base_1+"i"+base_2 ;
                               Pl => base_1+"i"+base_2+"ibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+"i"+base_2+"eadh"
                             }
                     } ;
        future = table {
                   Indep => base_1+"i"+base_2+"idh" ;
                   Dep => base_1+"i"+base_2
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"i"+base_2 ;
                 Dep => "dh'"+base_1+"i"+base_2
               } ;
        noun = base_1+base_2 ;
        participle = base_1+"i"+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV010"
  } ;

mkV011 : Str -> V ;
mkV011 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1 ;
                 Dep => nonExist
               } ;
        noun = base_1 ;
        participle = base_1+"hte"
      };
    _ => error "Can't apply paradigm mkV011"
  } ;

mkV012 : Str -> V ;
mkV012 base =
  case base of {
    base_1+base_2@?+"nn" => lin V
      { s = base_1+base_2+"nn" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"n"+base_2+"dh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+base_2+"nn" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"nn" ;
        participle = base_1+base_2+"nnte"
      };
    _ => error "Can't apply paradigm mkV012"
  } ;

mkV013 : Str -> V ;
mkV013 base =
  case base of {
    base_1+base_2@?+"inn" => lin V
      { s = base_1+base_2+"inn" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"n"+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+base_2+"inn" ;
                 Dep => nonExist
               } ;
        noun = base_1+"n"+base_2+"dh" ;
        participle = base_1+base_2+"innte"
      };
    _ => error "Can't apply paradigm mkV013"
  } ;

mkV014 : Str -> V ;
mkV014 base =
  case base of {
    base_1+"i"+base_2@(?+?) => lin V
      { s = base_1+"i"+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"i"+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"i"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+"ea"+base_2+"d" ;
        participle = base_1+"i"+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV014"
  } ;

mkV015 : Str -> V ;
mkV015 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1 ;
                 Dep => nonExist
               } ;
        noun = base_1+"adh" ;
        participle = base_1+"ta"
      };
    _ => error "Can't apply paradigm mkV015"
  } ;

mkV016 : Str -> V ;
mkV016 base =
  case base of {
    base_1+"dhèa"+base_2@? => lin V
      { s = base_1+"dhèa"+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"ì" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"ri"+base_2+"n" ;
                 Dep => nonExist
               } ;
        noun = base_1+"dhèa"+base_2+"amh" ;
        participle = base_1+"dhèa"+base_2+"ta"
      };
    _ => error "Can't apply paradigm mkV016"
  } ;

mkV017 : Str -> V ;
mkV017 base =
  case base of {
    base_1+"a"+base_2@(?+?)+"i"+base_3@(?+?) => lin V
      { s = base_1+"a"+base_2+"i"+base_3 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"a"+base_2+"i"+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"a"+base_2+"i"+base_3 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+base_3+"adh" ;
        participle = base_1+"a"+base_2+"i"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV017"
  } ;

mkV018 : Str -> V ;
mkV018 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1 ;
                 Dep => nonExist
               } ;
        noun = base_1+"ail" ;
        participle = base_1+"te"
      };
    _ => error "Can't apply paradigm mkV018"
  } ;

mkV019 : Str -> V ;
mkV019 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1 ;
                 Dep => nonExist
               } ;
        noun = base_1+"tinn" ;
        participle = base_1+"te"
      };
    _ => error "Can't apply paradigm mkV019"
  } ;

mkV020 : Str -> V ;
mkV020 base =
  case base of {
    base_1+"i"+base_2@? => lin V
      { s = base_1+"i"+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"i"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"e" ;
        participle = base_1+"i"+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV020"
  } ;

mkV021 : Str -> V ;
mkV021 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1 ;
                 Dep => nonExist
               } ;
        noun = base_1 ;
        participle = base_1+"te"
      };
    _ => error "Can't apply paradigm mkV021"
  } ;

mkV022 : Str -> V ;
mkV022 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1 ;
                 Dep => nonExist
               } ;
        noun = base_1+"tainn" ;
        participle = base_1+"te"
      };
    _ => error "Can't apply paradigm mkV022"
  } ;

mkV023 : Str -> V ;
mkV023 base =
  case base of {
    base_1+base_2@(?+?)+"r" => lin V
      { s = base_1+base_2+"r" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"r"+base_2+"dh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+base_2+"r" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"rt" ;
        participle = base_1+base_2+"rte"
      };
    _ => error "Can't apply paradigm mkV023"
  } ;

mkV024 : Str -> V ;
mkV024 base =
  case base of {
    base_1+base_2@? => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+"ad"+base_2 ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV024"
  } ;

mkV025 : Str -> V ;
mkV025 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1 ;
                 Dep => nonExist
               } ;
        noun = base_1+"eam" ;
        participle = base_1+"e"
      };
    _ => error "Can't apply paradigm mkV025"
  } ;

mkV026 : Str -> V ;
mkV026 base =
  case base of {
    base_1@?+base_2 => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => base_1+"h"+base_2+"ainn" ;
                        Pl => base_1+"h"+base_2+"amaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+base_2+"am" ;
                               Pl => base_1+base_2+"amaid"
                             } ;
                       P2 => table {
                               Sg => base_1+base_2 ;
                               Pl => base_1+base_2+"aibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+base_2+"adh"
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"aidh" ;
                   Dep => base_1+"h"+base_2
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => base_1+"h"+base_2
               } ;
        noun = base_1+base_2+"adh" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV026"
  } ;

mkV027 : Str -> V ;
mkV027 base =
  case base of {
    base_1@?+base_2 => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => base_1+"h"+base_2+"inn" ;
                        Pl => base_1+"h"+base_2+"eamaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+base_2+"eam" ;
                               Pl => base_1+base_2+"eamaid"
                             } ;
                       P2 => table {
                               Sg => base_1+base_2 ;
                               Pl => base_1+base_2+"ibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+base_2+"eadh"
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"idh" ;
                   Dep => base_1+"h"+base_2
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => base_1+"h"+base_2
               } ;
        noun = base_1+base_2+"t" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV027"
  } ;

mkV028 : Str -> V ;
mkV028 base =
  case base of {
    base_1+base_2@(?+?+?+?) => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"eadh" ;
        participle = base_1+base_2+"e"
      };
    _ => error "Can't apply paradigm mkV028"
  } ;

mkV029 : Str -> V ;
mkV029 base =
  case base of {
    base_1@?+base_2+"i"+base_3@(?+?) => lin V
      { s = base_1+base_2+"i"+base_3 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"i"+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+"i"+base_3 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+base_3+"d" ;
        participle = base_1+base_2+"i"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV029"
  } ;

mkV030 : Str -> V ;
mkV030 base =
  case base of {
    base_1+base_2@(?+?+?+?+?)+"ic"+base_3@? => lin V
      { s = base_1+base_2+"ic"+base_3 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"ic"+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+"ic"+base_3 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"d"+base_3 ;
        participle = base_1+base_2+"ic"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV030"
  } ;

mkV031 : Str -> V ;
mkV031 base =
  case base of {
    "bei"+base_1 => lin V
      { s = "bei"+base_1 ;
        conditional = table {
                        Sg => "bhei"+base_1+"inn" ;
                        Pl => "bhei"+base_1+"eadh"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => "bei"+base_1+"eam" ;
                               Pl => "bei"+base_1+"eamaid"
                             } ;
                       P2 => table {
                               Sg => "bei"+base_1 ;
                               Pl => "bei"+base_1+"ibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => "bei"+base_1+"eadh"
                             }
                     } ;
        future = table {
                   Indep => "bei"+base_1+"idh" ;
                   Dep => "bhei"+base_1
                 } ;
        past = table {
                 Indep => base_1+"ug" ;
                 Dep => base_1+"ug"
               } ;
        noun = "b"+base_1+"eith" ;
        participle = "bei"+base_1+"te"
      };
    _ => error "Can't apply paradigm mkV031"
  } ;

mkV032 : Str -> V ;
mkV032 base =
  case base of {
    base_1@?+base_2+"i"+base_3@("l"|"g"|(?+?)) => lin V
      { s = base_1+base_2+"i"+base_3 ;
        conditional = table {
                        Sg => base_1+"h"+base_2+"i"+base_3+"inn" ;
                        Pl => base_1+"h"+base_2+"i"+base_3+"eamaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+base_2+"i"+base_3+"eam" ;
                               Pl => base_1+base_2+"i"+base_3+"eamaid"
                             } ;
                       P2 => table {
                               Sg => base_1+base_2+"i"+base_3 ;
                               Pl => base_1+base_2+"i"+base_3+"ibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+base_2+"i"+base_3+"eadh"
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"i"+base_3+"idh" ;
                   Dep => base_1+"h"+base_2+"i"+base_3
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+"i"+base_3 ;
                 Dep => base_1+"h"+base_2+"i"+base_3
               } ;
        noun = base_1+base_2+base_3+"adh" ;
        participle = base_1+base_2+"i"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV032"
  } ;

mkV033 : Str -> V ;
mkV033 base =
  case base of {
    base_1+base_2@?+"à"+base_3@(?+?) => lin V
      { s = base_1+base_2+"à"+base_3 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"a"+base_3+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+"à"+base_3 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"a"+base_3+"adh" ;
        participle = base_1+base_2+"à"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV033"
  } ;

mkV034 : Str -> V ;
mkV034 base =
  case base of {
    base_1@?+base_2 => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => base_1+"h"+base_2+"inn" ;
                        Pl => base_1+"h"+base_2+"eamaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+base_2+"eam" ;
                               Pl => base_1+base_2+"eamaid"
                             } ;
                       P2 => table {
                               Sg => base_1+base_2 ;
                               Pl => base_1+base_2+"ibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+base_2+"eadh"
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"idh" ;
                   Dep => base_1+"h"+base_2
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => base_1+"h"+base_2
               } ;
        noun = base_1+base_2+"eadh" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV034"
  } ;

mkV035 : Str -> V ;
mkV035 base =
  case base of {
    base_1@?+base_2+"i"+base_3@("l"|"r"|(?+?)) => lin V
      { s = base_1+base_2+"i"+base_3 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"i"+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+"i"+base_3 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+base_3 ;
        participle = base_1+base_2+"i"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV035"
  } ;

mkV036 : Str -> V ;
mkV036 base =
  case base of {
    base_1+"ic"+base_2@? => lin V
      { s = base_1+"ic"+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => nonExist ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => nonExist ;
                 Dep => nonExist
               } ;
        noun = base_1+"d"+base_2 ;
        participle = nonExist
      };
    _ => error "Can't apply paradigm mkV036"
  } ;

mkV037 : Str -> V ;
mkV037 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => "dh'"+base_1+"ainn" ; --guessed
                        Pl => "dh'"+base_1+"amaid" --guessed
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+"am" ; --guessed
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P2 => table {
                               Sg => base_1 ; --guessed
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+"adh" --guessed
                             }
                     } ;
        future = table {
                   Indep => base_1+"idh" ; --guessed
                   Dep => base_1 --guessed
                 } ;
        past = table {
                 Indep => base_1 ; --guessed
                 Dep => "dh'"+base_1 --guessed
               } ;
        noun = base_1 ;
        participle = base_1+"te" --guessed
      };
    _ => error "Can't apply paradigm mkV037"
  } ;

mkV038 : Str -> V ;
mkV038 base =
  case base of {
    base_1@?+base_2+"i"+base_3@(?+?) => lin V
      { s = base_1+base_2+"i"+base_3 ;
        conditional = table {
                        Sg => base_1+"h"+base_2+"i"+base_3+"inn" ;
                        Pl => base_1+"h"+base_2+"i"+base_3+"eamaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+base_2+"i"+base_3+"eam" ;
                               Pl => base_1+base_2+"i"+base_3+"eamaid"
                             } ;
                       P2 => table {
                               Sg => base_1+base_2+"i"+base_3 ;
                               Pl => base_1+base_2+"i"+base_3+"ibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+base_2+"i"+base_3+"eadh"
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"i"+base_3+"idh" ;
                   Dep => base_1+base_2+"i"+base_3
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+"i"+base_3 ;
                 Dep => base_1+"h"+base_2+"i"+base_3
               } ;
        noun = base_1+base_2+"ea"+base_3+"adh" ;
        participle = base_1+base_2+"i"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV038"
  } ;

mkV039 : Str -> V ;
mkV039 base =
  case base of {
    base_1@?+base_2 => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"ad" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV039"
  } ;

mkV040 : Str -> V ;
mkV040 base =
  case base of {
    base_1+base_2@(?+?+?+?+?)+"i"+base_3@?+"n" => lin V
      { s = base_1+base_2+"i"+base_3+"n" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+"i"+base_3+"n" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"i"+base_3+"n" ;
        participle = base_1+base_2+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV040"
  } ;

mkV041 : Str -> V ;
mkV041 base =
  case base of {
    base_1@?+base_2 => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2 ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV041"
  } ;

mkV042 : Str -> V ;
mkV042 base =
  case base of {
    base_1@?+base_2 => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => base_1+"h"+base_2+"inn" ; --guessed
                        Pl => base_1+"h"+base_2+"eamaid" --guessed
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+base_2+"eam" ; --guessed
                               Pl => base_1+base_2+"eamaid" --guessed
                             } ;
                       P2 => table {
                               Sg => base_1+base_2 ; --guessed
                               Pl => base_1+base_2+"ibh" --guessed
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+base_2+"eadh" --guessed
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"idh" ;
                   Dep => base_1+"h"+base_2 --guessed
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => base_1+"h"+base_2 --guessed
               } ;
        noun = base_1+base_2+"eadh" ;
        participle = base_1+base_2+"e" --guessed
      };
    _ => error "Can't apply paradigm mkV042"
  } ;

mkV043 : Str -> V ;
mkV043 base =
  case base of {
    base_1@?+base_2 => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2 ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV043"
  } ;

mkV044 : Str -> V ;
mkV044 base =
  case base of {
    base_1+base_2@(?+?)+base_3@?+"inn" => lin V
      { s = base_1+base_2+base_3+"inn" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"n"+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+base_3+"inn" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"n"+base_3+"dh" ;
        participle = base_1+base_2+base_3+"innte"
      };
    _ => error "Can't apply paradigm mkV044"
  } ;

mkV045 : Str -> V ;
mkV045 base =
  case base of {
    base_1+base_2@?+"i"+base_3@?+"i"+base_4@? => lin V
      { s = base_1+base_2+"i"+base_3+"i"+base_4 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"i"+base_3+base_4+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+"i"+base_3+"i"+base_4 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+base_3+"a"+base_4 ;
        participle = base_1+base_2+"i"+base_3+"i"+base_4+"te"
      };
    _ => error "Can't apply paradigm mkV045"
  } ;

mkV046 : Str -> V ;
mkV046 base =
  case base of {
    base_1+base_2@(?+?+?+?) => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"eamh" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV046"
  } ;

mkV047 : Str -> V ;
mkV047 base =
  case base of {
    base_1@?+base_2 => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => base_1+"h"+base_2+"ainn" ;
                        Pl => base_1+"h"+base_2+"amaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+base_2+"am" ;
                               Pl => base_1+base_2+"amaid"
                             } ;
                       P2 => table {
                               Sg => base_1+base_2 ;
                               Pl => base_1+base_2+"aibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+base_2+"adh"
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"aidh" ;
                   Dep => base_1+"h"+base_2
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => base_1+"h"+base_2
               } ;
        noun = base_1+base_2+"tainn" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV047"
  } ;

mkV048 : Str -> V ;
mkV048 base =
  case base of {
    base_1+base_2@(?+?+?+?)+base_3@?+"il" => lin V
      { s = base_1+base_2+base_3+"il" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+base_3+"ilidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+base_3+"il" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"l"+base_3+"dh" ;
        participle = base_1+base_2+base_3+"ilte"
      };
    _ => error "Can't apply paradigm mkV048"
  } ;

mkV049 : Str -> V ;
mkV049 base =
  case base of {
    base_1@?+base_2+base_3@(?+?)+"ai"+base_4@?+"n" => lin V
      { s = base_1+base_2+base_3+"ai"+base_4+"n" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"i"+base_3+base_4+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+base_3+"ai"+base_4+"n" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+base_3+base_4+"adh" ;
        participle = base_1+base_2+base_3+"ai"+base_4+"nte"
      };
    _ => error "Can't apply paradigm mkV049"
  } ;

mkV050 : Str -> V ;
mkV050 base =
  case base of {
    base_1+base_2@(?+?+?+?)+base_3@?+"il" => lin V
      { s = base_1+base_2+base_3+"il" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"l"+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+base_3+"il" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+base_3+"l" ;
        participle = base_1+base_2+base_3+"ilte"
      };
    _ => error "Can't apply paradigm mkV050"
  } ;

mkV051 : Str -> V ;
mkV051 base =
  case base of {
    base_1+base_2@?+"uinn" => lin V
      { s = base_1+base_2+"uinn" ;
        conditional = table {
                        Sg => base_1+"h"+base_2+"uinninn" ;
                        Pl => base_1+"h"+base_2+"uinneadh"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+base_2+"uinneam" ;
                               Pl => base_1+base_2+"uinneamaid"
                             } ;
                       P2 => table {
                               Sg => base_1+base_2+"uinn" ;
                               Pl => base_1+base_2+"uinnibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+base_2+"uinneadh"
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"uinnidh" ;
                   Dep => base_1+"h"+base_2+"uinn"
                 } ;
        past = table {
                 Indep => base_1+"hua"+base_2+"a" ;
                 Dep => base_1+"hua"+base_2+"a"
               } ;
        noun = base_1+base_2+"uinntinn" ;
        participle = base_1+base_2+"uinnte"
      };
    _ => error "Can't apply paradigm mkV051"
  } ;

mkV052 : Str -> V ;
mkV052 base =
  case base of {
    base_1+base_2@(?+?+?)+"i"+base_3@(?+?) => lin V
      { s = base_1+base_2+"i"+base_3 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"i"+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+"i"+base_3 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"ea"+base_3+"d" ;
        participle = base_1+base_2+"i"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV052"
  } ;

mkV053 : Str -> V ;
mkV053 base =
  case base of {
    base_1+base_2@?+"i"+base_3@?+"i"+base_4@?+"n" => lin V
      { s = base_1+base_2+"i"+base_3+"i"+base_4+"n" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"i"+base_3+"i"+base_4+"nidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+"i"+base_3+"i"+base_4+"n" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+base_3+base_4+"adh" ;
        participle = base_1+base_2+"i"+base_3+"i"+base_4+"nte"
      };
    _ => error "Can't apply paradigm mkV053"
  } ;

mkV054 : Str -> V ;
mkV054 base =
  case base of {
    base_1@?+base_2 => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"adh" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV054"
  } ;

mkV055 : Str -> V ;
mkV055 base =
  case base of {
    base_1@?+base_2 => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => base_1+"h"+base_2+"ainn" ;
                        Pl => base_1+"h"+base_2+"amaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+base_2+"am" ;
                               Pl => base_1+base_2+"amaid"
                             } ;
                       P2 => table {
                               Sg => base_1+base_2 ;
                               Pl => base_1+base_2+"aibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+base_2+"adh"
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"aidh" ;
                   Dep => base_1+"h"+base_2
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => base_1+"h"+base_2
               } ;
        noun = base_1+base_2+"adh" ;
        participle = base_1+base_2+"ta"
      };
    _ => error "Can't apply paradigm mkV055"
  } ;

mkV056 : Str -> V ;
mkV056 base =
  case base of {
    base_1@?+base_2 => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"sinn" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV056"
  } ;

mkV057 : Str -> V ;
mkV057 base =
  case base of {
    base_1+base_2@(?+?)+base_3@? => lin V
      { s = base_1+base_2+base_3 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+base_3+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+base_3 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+base_3+"adh" ;
        participle = base_1+base_2+"i"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV057"
  } ;

mkV058 : Str -> V ;
mkV058 base =
  case base of {
    base_1+base_2@?+"i"+base_3@?+base_4@(?+?+?+?) => lin V
      { s = base_1+base_2+"i"+base_3+base_4 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"i"+base_3+"idh"+base_4 ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+"i"+base_3+base_4 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+base_3+base_4 ;
        participle = nonExist
      };
    _ => error "Can't apply paradigm mkV058"
  } ;

mkV059 : Str -> V ;
mkV059 base =
  case base of {
    base_1+base_2@(?+?) => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"ail" ;
        participle = base_1+base_2+"ta"
      };
    _ => error "Can't apply paradigm mkV059"
  } ;

mkV060 : Str -> V ;
mkV060 base =
  case base of {
    base_1@?+base_2 => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"adh" ;
        participle = base_1+base_2+"e"
      };
    _ => error "Can't apply paradigm mkV060"
  } ;

mkV061 : Str -> V ;
mkV061 base =
  case base of {
    base_1+base_2@?+"i"+base_3@?+"ic"+base_4@? => lin V
      { s = base_1+base_2+"i"+base_3+"ic"+base_4 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"i"+base_3+"ic"+base_4+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+"i"+base_3+"ic"+base_4 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+base_3+"ad"+base_4 ;
        participle = base_1+base_2+"i"+base_3+"ic"+base_4+"te"
      };
    _ => error "Can't apply paradigm mkV061"
  } ;

mkV062 : Str -> V ;
mkV062 base =
  case base of {
    base_1+base_2@(?+?+?+?+?+?)+"c"+base_3@? => lin V
      { s = base_1+base_2+"c"+base_3 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"c"+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+"c"+base_3 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"d"+base_3 ;
        participle = base_1+base_2+"c"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV062"
  } ;

mkV063 : Str -> V ;
mkV063 base =
  case base of {
    base_1+"ù"+base_2@? => lin V
      { s = base_1+"ù"+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"u"+base_2+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"hù"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+"u"+base_2+"ail" ;
        participle = base_1+"u"+base_2+"ta"
      };
    _ => error "Can't apply paradigm mkV063"
  } ;

mkV064 : Str -> V ;
mkV064 base =
  case base of {
    base_1+base_2@(?+?+?+?)+base_3@(?+?)+"ic"+base_4@? => lin V
      { s = base_1+base_2+base_3+"ic"+base_4 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"a"+base_3+"ic"+base_4+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+base_3+"ic"+base_4 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+base_3+"d"+base_4 ;
        participle = base_1+base_2+"a"+base_3+"ic"+base_4+"te"
      };
    _ => error "Can't apply paradigm mkV064"
  } ;

mkV065 : Str -> V ;
mkV065 base =
  case base of {
    base_1@?+base_2+"i"+base_3@(?+?) => lin V
      { s = base_1+base_2+"i"+base_3 ;
        conditional = table {
                        Sg => base_1+"h"+base_2+"i"+base_3+"inn" ;
                        Pl => base_1+"h"+base_2+"i"+base_3+"eamaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+base_2+"i"+base_3+"eam" ;
                               Pl => base_1+base_2+"i"+base_3+"eamaid"
                             } ;
                       P2 => table {
                               Sg => base_1+base_2+"i"+base_3 ;
                               Pl => base_1+base_2+"i"+base_3+"ibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+base_2+"i"+base_3+"eadh"
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"i"+base_3+"idh" ;
                   Dep => base_1+base_2+"i"+base_3
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+"i"+base_3 ;
                 Dep => base_1+"h"+base_2+"i"+base_3
               } ;
        noun = base_1+base_2+base_3+"adh" ;
        participle = base_1+base_2+"i"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV065"
  } ;

mkV066 : Str -> V ;
mkV066 base =
  case base of {
    "dèa"+base_1 => lin V
      { s = "dèa"+base_1 ;
        conditional = table {
                        Sg => "dhèa"+base_1+"ainn" ;
                        Pl => "dhèa"+base_1+"adh"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => "dèa"+base_1+"am" ;
                               Pl => "dèa"+base_1+"amaid"
                             } ;
                       P2 => table {
                               Sg => "dèa"+base_1 ;
                               Pl => "dèa"+base_1+"aibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => "dèa"+base_1+"adh"
                             }
                     } ;
        future = table {
                   Indep => base_1+"ì" ;
                   Dep => "dèa"+base_1
                 } ;
        past = table {
                 Indep => "ri"+base_1+"n" ;
                 Dep => "ri"+base_1+"n"
               } ;
        noun = "dèa"+base_1+"amh" ;
        participle = "dèa"+base_1+"ta"
      };
    _ => error "Can't apply paradigm mkV066"
  } ;

mkV067 : Str -> V ;
mkV067 base =
  case base of {
    base_1@?+base_2+base_3@?+"ir" => lin V
      { s = base_1+base_2+base_3+"ir" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"r"+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+base_3+"ir" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"r"+base_3+"dh" ;
        participle = base_1+base_2+base_3+"irte"
      };
    _ => error "Can't apply paradigm mkV067"
  } ;

mkV068 : Str -> V ;
mkV068 base =
  case base of {
    base_1+base_2@(?+?+?+?)+base_3@?+"il" => lin V
      { s = base_1+base_2+base_3+"il" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"l"+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+base_3+"il" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"l"+base_3+"dh" ;
        participle = base_1+base_2+base_3+"ilte"
      };
    _ => error "Can't apply paradigm mkV068"
  } ;

mkV069 : Str -> V ;
mkV069 base =
  case base of {
    base_1+base_2@(?+?)+"ic"+base_3@? => lin V
      { s = base_1+base_2+"ic"+base_3 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"ic"+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+"ic"+base_3 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"ead"+base_3 ;
        participle = base_1+base_2+"ic"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV069"
  } ;

mkV070 : Str -> V ;
mkV070 base =
  case base of {
    base_1+base_2@?+"i"+base_3@(?+?) => lin V
      { s = base_1+base_2+"i"+base_3 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"i"+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+"i"+base_3 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+base_3+"adh" ;
        participle = base_1+base_2+"i"+base_3+"e"
      };
    _ => error "Can't apply paradigm mkV070"
  } ;

mkV071 : Str -> V ;
mkV071 base =
  case base of {
    base_1+base_2@?+"il" => lin V
      { s = base_1+base_2+"il" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"l"+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+base_2+"il" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"l" ;
        participle = base_1+base_2+"ilte"
      };
    _ => error "Can't apply paradigm mkV071"
  } ;

mkV072 : Str -> V ;
mkV072 base =
  case base of {
    base_1+"i"+base_2@(?+?) => lin V
      { s = base_1+"i"+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"i"+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"i"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+"ì"+base_2+"eadh" ;
        participle = base_1+"i"+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV072"
  } ;

mkV073 : Str -> V ;
mkV073 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => "dh'"+base_1+"inn" ;
                        Pl => "dh'"+base_1+"eamaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+"eam" ;
                               Pl => base_1+"eamaid"
                             } ;
                       P2 => table {
                               Sg => base_1 ;
                               Pl => base_1+"ibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+"eadh"
                             }
                     } ;
        future = table {
                   Indep => base_1+"idh" ;
                   Dep => base_1
                 } ;
        past = table {
                 Indep => "dh'"+base_1 ;
                 Dep => "dh'"+base_1
               } ;
        noun = base_1+"eadh" ;
        participle = base_1+"te"
      };
    _ => error "Can't apply paradigm mkV073"
  } ;

mkV074 : Str -> V ;
mkV074 base =
  case base of {
    base_1+"aich" => lin V
      { s = base_1+"aich" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"aichidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"aich" ;
                 Dep => nonExist
               } ;
        noun = base_1+"adh" ;
        participle = base_1+"te"
      };
    _ => error "Can't apply paradigm mkV074"
  } ;

mkV075 : Str -> V ;
mkV075 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1 ;
                 Dep => nonExist
               } ;
        noun = base_1+"sadh" ;
        participle = base_1+"te"
      };
    _ => error "Can't apply paradigm mkV075"
  } ;

mkV076 : Str -> V ;
mkV076 base =
  case base of {
    "fai"+base_1 => lin V
      { s = "fai"+base_1 ;
        conditional = table {
                        Sg => base_1+"hithinn" ;
                        Pl => base_1+"hitheadh"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => "fai"+base_1+"eam" ;
                               Pl => "fai"+base_1+"eamaid"
                             } ;
                       P2 => table {
                               Sg => "fai"+base_1 ;
                               Pl => "fai"+base_1+"ibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => "fai"+base_1+"eadh"
                             }
                     } ;
        future = table {
                   Indep => base_1+"hì" ;
                   Dep => "fhai"+base_1
                 } ;
        past = table {
                 Indep => base_1+"hunnaic" ;
                 Dep => "fha"+base_1+"a"
               } ;
        noun = "fai"+base_1+"inn" ;
        participle = "fai"+base_1+"te"
      };
    _ => error "Can't apply paradigm mkV076"
  } ;

mkV077 : Str -> V ;
mkV077 base =
  case base of {
    "faig"+base_1 => lin V
      { s = "faig"+base_1 ;
        conditional = table {
                        Sg => "g"+base_1+"eibhinn" ;
                        Pl => "g"+base_1+"eibheadh"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => "faig"+base_1+"eam" ;
                               Pl => "faig"+base_1+"eamaid"
                             } ;
                       P2 => table {
                               Sg => "faig"+base_1 ;
                               Pl => "faig"+base_1+"ibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => "faig"+base_1+"eadh"
                             }
                     } ;
        future = table {
                   Indep => "g"+base_1+"eibh" ;
                   Dep => "f"+base_1+"aigh"
                 } ;
        past = table {
                 Indep => "f"+base_1+"uair" ;
                 Dep => "f"+base_1+"uair"
               } ;
        noun = "faig"+base_1+"inn" ;
        participle = "faig"+base_1+"te"
      };
    _ => error "Can't apply paradigm mkV077"
  } ;

mkV078 : Str -> V ;
mkV078 base =
  case base of {
    base_1+base_2@(?+?+?+?+?)+"i"+base_3@(?+?) => lin V
      { s = base_1+base_2+"i"+base_3 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"i"+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2+"i"+base_3 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"ea"+base_3+"d" ;
        participle = base_1+base_2+"i"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV078"
  } ;

mkV079 : Str -> V ;
mkV079 base =
  case base of {
    base_1+base_2@(?+?)+"l"+base_3@? => lin V
      { s = base_1+base_2+"l"+base_3 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"l"+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2+"l"+base_3 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+base_3+"eadh" ;
        participle = base_1+base_2+"l"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV079"
  } ;

mkV080 : Str -> V ;
mkV080 base =
  case base of {
    base_1+base_2@(?+?+?)+"i"+base_3@(?+?) => lin V
      { s = base_1+base_2+"i"+base_3 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"i"+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2+"i"+base_3 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"ea"+base_3+"dainn" ;
        participle = base_1+base_2+"i"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV080"
  } ;

mkV081 : Str -> V ;
mkV081 base =
  case base of {
    base_1+base_2@(?+?+?+?) => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2 ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV081"
  } ;

mkV082 : Str -> V ;
mkV082 base =
  case base of {
    base_1+base_2@(?+?) => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"tail" ;
        participle = nonExist
      };
    _ => error "Can't apply paradigm mkV082"
  } ;

mkV083 : Str -> V ;
mkV083 base =
  case base of {
    "fannaich" => lin V
      { s = "fannaich" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => "fannaichidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'fhannaich" ;
                 Dep => nonExist
               } ;
        noun = "fannachadh" ;
        participle = "te"
      };
    _ => error "Can't apply paradigm mkV083"
  } ;

mkV084 : Str -> V ;
mkV084 base =
  case base of {
    base_1@?+base_2 => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2 ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV084"
  } ;

mkV085 : Str -> V ;
mkV085 base =
  case base of {
    base_1@?+base_2+"i"+base_3@(?+?) => lin V
      { s = base_1+base_2+"i"+base_3 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"i"+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2+"i"+base_3 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+base_3+"adh" ;
        participle = base_1+base_2+"i"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV085"
  } ;

mkV086 : Str -> V ;
mkV086 base =
  case base of {
    base_1@?+base_2 => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"adh" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV086"
  } ;

mkV087 : Str -> V ;
mkV087 base =
  case base of {
    base_1@?+base_2+"i"+base_3@("m"|(?+?)) => lin V
      { s = base_1+base_2+"i"+base_3 ;
        conditional = table {
                        Sg => "dh'"+base_1+"h"+base_2+"i"+base_3+"inn" ;
                        Pl => "dh'"+base_1+"h"+base_2+"i"+base_3+"eamaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+base_2+"i"+base_3+"eam" ;
                               Pl => base_1+base_2+"i"+base_3+"eamaid"
                             } ;
                       P2 => table {
                               Sg => base_1+base_2+"i"+base_3 ;
                               Pl => base_1+base_2+"i"+base_3+"ibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+base_2+"i"+base_3+"eadh"
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"i"+base_3+"idh" ;
                   Dep => base_1+"h"+base_2+"i"+base_3
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2+"i"+base_3 ;
                 Dep => "dh'"+base_1+"h"+base_2+"i"+base_3
               } ;
        noun = base_1+base_2+base_3 ;
        participle = base_1+base_2+"i"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV087"
  } ;

mkV088 : Str -> V ;
mkV088 base =
  case base of {
    base_1@?+base_2 => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => "dh'"+base_1+"h"+base_2+"ainn" ;
                        Pl => "dh'"+base_1+"h"+base_2+"amaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+base_2+"am" ;
                               Pl => base_1+base_2+"amaid"
                             } ;
                       P2 => table {
                               Sg => base_1+base_2 ;
                               Pl => base_1+base_2+"aibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+base_2+"adh"
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"aidh" ;
                   Dep => base_1+"h"+base_2
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2 ;
                 Dep => "dh'"+base_1+"h"+base_2
               } ;
        noun = base_1+base_2+"ail" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV088"
  } ;

mkV089 : Str -> V ;
mkV089 base =
  case base of {
    base_1+base_2@(?+?+?)+base_3@?+"inn" => lin V
      { s = base_1+base_2+base_3+"inn" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+base_3+"innidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2+base_3+"inn" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"n"+base_3+"dh" ;
        participle = base_1+base_2+base_3+"innte"
      };
    _ => error "Can't apply paradigm mkV089"
  } ;

mkV090 : Str -> V ;
mkV090 base =
  case base of {
    base_1+base_2@(?+?+?+?) => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"eamh" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV090"
  } ;

mkV091 : Str -> V ;
mkV091 base =
  case base of {
    base_1+base_2@(?+?+?+?) => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"ainn" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV091"
  } ;

mkV092 : Str -> V ;
mkV092 base =
  case base of {
    base_1+base_2@(?+?+?) => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"e" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV092"
  } ;

mkV093 : Str -> V ;
mkV093 base =
  case base of {
    base_1@?+base_2 => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => "dh'"+base_1+"h"+base_2+"inn" ;
                        Pl => "dh'"+base_1+"h"+base_2+"eamaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+base_2+"eam" ;
                               Pl => base_1+base_2+"eamaid"
                             } ;
                       P2 => table {
                               Sg => base_1+base_2 ;
                               Pl => base_1+base_2+"ibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+base_2+"eadh"
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"idh" ;
                   Dep => base_1+"h"+base_2
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2 ;
                 Dep => "dh'"+base_1+"h"+base_2
               } ;
        noun = base_1+base_2+"eadh" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV093"
  } ;

mkV094 : Str -> V ;
mkV094 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => base_1+"ainn" ;
                        Pl => base_1+"amaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+"am" ;
                               Pl => base_1+"amaid"
                             } ;
                       P2 => table {
                               Sg => base_1 ;
                               Pl => base_1+"aibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+"adh"
                             }
                     } ;
        future = table {
                   Indep => base_1+"aidh" ;
                   Dep => base_1
                 } ;
        past = table {
                 Indep => base_1 ;
                 Dep => base_1
               } ;
        noun = base_1+"adh" ;
        participle = base_1+"te"
      };
    _ => error "Can't apply paradigm mkV094"
  } ;

mkV095 : Str -> V ;
mkV095 base =
  case base of {
    base_1+"o"+base_2@(?+?)+base_3@?+"in" => lin V
      { s = base_1+"o"+base_2+base_3+"in" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"ò"+base_2+"n"+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"ho"+base_2+base_3+"in" ;
                 Dep => nonExist
               } ;
        noun = base_1+"ò"+base_2+"n"+base_3+"dh" ;
        participle = base_1+"o"+base_2+base_3+"inte"
      };
    _ => error "Can't apply paradigm mkV095"
  } ;

mkV096 : Str -> V ;
mkV096 base =
  case base of {
    base_1+base_2@?+"i"+base_3@(?+?+?)+"i"+base_4@(?+?) => lin V
      { s = base_1+base_2+"i"+base_3+"i"+base_4 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+base_3+"i"+base_4+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2+"i"+base_3+"i"+base_4 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"i"+base_3+"ea"+base_4+"adh" ;
        participle = base_1+base_2+"i"+base_3+"i"+base_4+"te"
      };
    _ => error "Can't apply paradigm mkV096"
  } ;

mkV097 : Str -> V ;
mkV097 base =
  case base of {
    base_1+base_2@(?+?+?)+base_3@?+"il" => lin V
      { s = base_1+base_2+base_3+"il" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"l"+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2+base_3+"il" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"l"+base_3+"dh" ;
        participle = base_1+base_2+base_3+"ilte"
      };
    _ => error "Can't apply paradigm mkV097"
  } ;

mkV098 : Str -> V ;
mkV098 base =
  case base of {
    base_1+base_2@(?+?+?+?+?)+"i"+base_3@? => lin V
      { s = base_1+base_2+"i"+base_3 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"i"+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+"i"+base_3 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"a"+base_3+"adh" ;
        participle = base_1+base_2+"i"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV098"
  } ;

mkV099 : Str -> V ;
mkV099 base =
  case base of {
    base_1+base_2@(?+?+?+?+?)+"ei"+base_3@? => lin V
      { s = base_1+base_2+"ei"+base_3 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2+"ei"+base_3 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"ea"+base_3 ;
        participle = base_1+base_2+"ei"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV099"
  } ;

mkV100 : Str -> V ;
mkV100 base =
  case base of {
    base_1+base_2@(?+?+?+?+?)+"i"+base_3@(?+?) => lin V
      { s = base_1+base_2+"i"+base_3 ;
        conditional = table {
                        Sg => "dh'"+base_1+"h"+base_2+"i"+base_3+"inn" ;
                        Pl => "dh'"+base_1+"h"+base_2+"i"+base_3+"eamaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+base_2+"i"+base_3+"eam" ;
                               Pl => base_1+base_2+"i"+base_3+"eamaid"
                             } ;
                       P2 => table {
                               Sg => base_1+base_2+"i"+base_3 ;
                               Pl => base_1+base_2+"i"+base_3+"ibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+base_2+"i"+base_3+"eadh"
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"ni"+base_3+"idh" ;
                   Dep => base_1+"h"+base_2+"i"+base_3
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2+"i"+base_3 ;
                 Dep => "dh'"+base_1+"h"+base_2+"i"+base_3
               } ;
        noun = base_1+base_2+"ea"+base_3+"adh" ;
        participle = base_1+base_2+"i"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV100"
  } ;

mkV101 : Str -> V ;
mkV101 base =
  case base of {
    base_1+base_2@(?+?+?+?)+base_3@?+"il" => lin V
      { s = base_1+base_2+base_3+"il" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+base_3+"ilidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2+base_3+"il" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"l"+base_3+"dh" ;
        participle = base_1+base_2+base_3+"ilte"
      };
    _ => error "Can't apply paradigm mkV101"
  } ;

mkV102 : Str -> V ;
mkV102 base =
  case base of {
    base_1+base_2@?+"i"+base_3@?+"i"+base_4@(?+?) => lin V
      { s = base_1+base_2+"i"+base_3+"i"+base_4 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"i"+base_3+"i"+base_4+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2+"i"+base_3+"i"+base_4 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+base_3+"a"+base_4 ;
        participle = base_1+base_2+"i"+base_3+"i"+base_4+"te"
      };
    _ => error "Can't apply paradigm mkV102"
  } ;

mkV103 : Str -> V ;
mkV103 base =
  case base of {
    base_1+base_2@(?+?+?)+"i"+base_3@(?+?) => lin V
      { s = base_1+base_2+"i"+base_3 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"i"+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2+"i"+base_3 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"ea"+base_3 ;
        participle = base_1+base_2+"i"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV103"
  } ;

mkV104 : Str -> V ;
mkV104 base =
  case base of {
    base_1@?+base_2+"i"+base_3@(?+?) => lin V
      { s = base_1+base_2+"i"+base_3 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"i"+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2+"i"+base_3 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"ea"+base_3+"adh" ;
        participle = base_1+base_2+"i"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV104"
  } ;

mkV105 : Str -> V ;
mkV105 base =
  case base of {
    base_1+base_2@(?+?) => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2 ;
        participle = base_1+base_2+"ta"
      };
    _ => error "Can't apply paradigm mkV105"
  } ;

mkV106 : Str -> V ;
mkV106 base =
  case base of {
    base_1+base_2@(?+?)+base_3@?+"ir" => lin V
      { s = base_1+base_2+base_3+"ir" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+base_3+"iridh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"h"+base_2+base_3+"ir" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"r"+base_3+"dh" ;
        participle = base_1+base_2+base_3+"irte"
      };
    _ => error "Can't apply paradigm mkV106"
  } ;

mkV107 : Str -> V ;
mkV107 base =
  case base of {
    base_1+base_2@(?+?+?) => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"ail" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV107"
  } ;

mkV108 : Str -> V ;
mkV108 base =
  case base of {
    base_1+base_2@?+"à"+base_3@(?+?) => lin V
      { s = base_1+base_2+"à"+base_3 ;
        conditional = table {
                        Sg => base_1+"h"+base_2+"à"+base_3+"inn" ;
                        Pl => base_1+"h"+base_2+"à"+base_3+"eamaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+base_2+"à"+base_3+"eam" ;
                               Pl => base_1+base_2+"à"+base_3+"eamaid"
                             } ;
                       P2 => table {
                               Sg => base_1+base_2+"à"+base_3 ;
                               Pl => base_1+base_2+"à"+base_3+"ibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+base_2+"à"+base_3+"eadh"
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"à"+base_3+"idh" ;
                   Dep => base_1+"h"+base_2+"à"+base_3
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+"à"+base_3 ;
                 Dep => base_1+"h"+base_2+"à"+base_3
               } ;
        noun = base_1+base_2+"a"+base_3+"adh" ;
        participle = base_1+base_2+"à"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV108"
  } ;

mkV109 : Str -> V ;
mkV109 base =
  case base of {
    base_1+base_2@(?+?) => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"tinn" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV109"
  } ;

mkV110 : Str -> V ;
mkV110 base =
  case base of {
    base_1+base_2@(?+?+?)+"i"+base_3@? => lin V
      { s = base_1+base_2+"i"+base_3 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"i"+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+"i"+base_3 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+base_3+"ad" ;
        participle = base_1+base_2+"i"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV110"
  } ;

mkV111 : Str -> V ;
mkV111 base =
  case base of {
    base_1+base_2@(?+?+?+?) => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => base_1+"h"+base_2+"inn" ;
                        Pl => base_1+"h"+base_2+"eamaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+base_2+"eam" ;
                               Pl => base_1+base_2+"eamaid"
                             } ;
                       P2 => table {
                               Sg => base_1+base_2 ;
                               Pl => base_1+base_2+"ibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+base_2+"eadh"
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"idh" ;
                   Dep => base_1+"h"+base_2
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => base_1+"h"+base_2
               } ;
        noun = base_1+base_2+"e" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV111"
  } ;

mkV112 : Str -> V ;
mkV112 base =
  case base of {
    base_1+base_2@(?+?+?) => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"eachdainn" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV112"
  } ;

mkV113 : Str -> V ;
mkV113 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => "dh'"+base_1+"ainn" ;
                        Pl => "dh'"+base_1+"amaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+"am" ;
                               Pl => base_1+"amaid"
                             } ;
                       P2 => table {
                               Sg => base_1 ;
                               Pl => base_1+"aibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+"adh"
                             }
                     } ;
        future = table {
                   Indep => base_1+"aidh" ;
                   Dep => base_1
                 } ;
        past = table {
                 Indep => "dh'"+base_1 ;
                 Dep => "dh'"+base_1
               } ;
        noun = base_1+"aidh" ;
        participle = base_1+"te"
      };
    _ => error "Can't apply paradigm mkV113"
  } ;

mkV114 : Str -> V ;
mkV114 base =
  case base of {
    base_1+base_2@?+"h" => lin V
      { s = base_1+base_2+"h" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"haidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+base_2+"h" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"hadh" ;
        participle = base_1+"dh"+base_2+"e"
      };
    _ => error "Can't apply paradigm mkV114"
  } ;

mkV115 : Str -> V ;
mkV115 base =
  case base of {
    base_1+base_2@?+"ir" => lin V
      { s = base_1+base_2+"ir" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"iridh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+base_2+"ir" ;
                 Dep => nonExist
               } ;
        noun = base_1+"r"+base_2+"dh" ;
        participle = base_1+base_2+"irte"
      };
    _ => error "Can't apply paradigm mkV115"
  } ;

mkV116 : Str -> V ;
mkV116 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => "dh'"+base_1+"inn" ;
                        Pl => "dh'"+base_1+"eamaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+"eam" ;
                               Pl => base_1+"eamaid"
                             } ;
                       P2 => table {
                               Sg => base_1 ;
                               Pl => base_1+"ibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+"eadh"
                             }
                     } ;
        future = table {
                   Indep => base_1+"idh" ;
                   Dep => base_1
                 } ;
        past = table {
                 Indep => "dh'"+base_1 ;
                 Dep => "dh'"+base_1
               } ;
        noun = base_1+"e" ;
        participle = base_1+"te"
      };
    _ => error "Can't apply paradigm mkV116"
  } ;

mkV117 : Str -> V ;
mkV117 base =
  case base of {
    base_1+base_2@?+"ir" => lin V
      { s = base_1+base_2+"ir" ;
        conditional = table {
                        Sg => base_1+"r"+base_2+"inn" ;
                        Pl => base_1+"r"+base_2+"maid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+"r"+base_2+"m" ;
                               Pl => base_1+"r"+base_2+"maid"
                             } ;
                       P2 => table {
                               Sg => base_1+base_2+"ir" ;
                               Pl => base_1+"r"+base_2+"ibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+"r"+base_2+"dh"
                             }
                     } ;
        future = table {
                   Indep => base_1+"r"+base_2+"idh" ;
                   Dep => base_1+base_2+"ir"
                 } ;
        past = table {
                 Indep => base_1+base_2+"ir" ;
                 Dep => base_1+base_2+"ir"
               } ;
        noun = base_1+base_2+"irt" ;
        participle = base_1+base_2+"irte"
      };
    _ => error "Can't apply paradigm mkV117"
  } ;

mkV118 : Str -> V ;
mkV118 base =
  case base of {
    base_1+"i"+base_2@("l"|(?+?)) => lin V
      { s = base_1+"i"+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"i"+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"i"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"adh" ;
        participle = base_1+"i"+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV118"
  } ;

mkV119 : Str -> V ;
mkV119 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1 ;
                 Dep => nonExist
               } ;
        noun = base_1+"e" ;
        participle = base_1+"te"
      };
    _ => error "Can't apply paradigm mkV119"
  } ;

mkV120 : Str -> V ;
mkV120 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1 ;
                 Dep => nonExist
               } ;
        noun = base_1+"tainn" ;
        participle = base_1+"ta"
      };
    _ => error "Can't apply paradigm mkV120"
  } ;

mkV121 : Str -> V ;
mkV121 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1 ;
                 Dep => nonExist
               } ;
        noun = base_1+"eil" ;
        participle = base_1+"te"
      };
    _ => error "Can't apply paradigm mkV121"
  } ;

mkV122 : Str -> V ;
mkV122 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1 ;
                 Dep => nonExist
               } ;
        noun = base_1 ;
        participle = base_1+"te"
      };
    _ => error "Can't apply paradigm mkV122"
  } ;

mkV123 : Str -> V ;
mkV123 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1 ;
                 Dep => nonExist
               } ;
        noun = base_1+"adh" ;
        participle = base_1+"ta"
      };
    _ => error "Can't apply paradigm mkV123"
  } ;

mkV124 : Str -> V ;
mkV124 base =
  case base of {
    base_1+"igh" => lin V
      { s = base_1+"igh" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"ighidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"igh" ;
                 Dep => nonExist
               } ;
        noun = base_1+"ì" ;
        participle = base_1+"ighte"
      };
    _ => error "Can't apply paradigm mkV124"
  } ;

mkV125 : Str -> V ;
mkV125 base =
  case base of {
    base_1+"i"+base_2@(?+?) => lin V
      { s = base_1+"i"+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"i"+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"i"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+"ea"+base_2+"adh" ;
        participle = base_1+"i"+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV125"
  } ;

mkV126 : Str -> V ;
mkV126 base =
  case base of {
    base_1@(?+?+?)+"i"+base_2 => lin V
      { s = base_1+"i"+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"i"+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"i"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2 ;
        participle = base_1+"i"+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV126"
  } ;

mkV127 : Str -> V ;
mkV127 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => base_1+"inn" ;
                        Pl => base_1+"eamaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+"eam" ;
                               Pl => base_1+"eamaid"
                             } ;
                       P2 => table {
                               Sg => base_1 ;
                               Pl => base_1+"ibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+"eadh"
                             }
                     } ;
        future = table {
                   Indep => base_1+"idh" ;
                   Dep => base_1
                 } ;
        past = table {
                 Indep => base_1 ;
                 Dep => base_1
               } ;
        noun = base_1+"eadh" ;
        participle = base_1+"te"
      };
    _ => error "Can't apply paradigm mkV127"
  } ;

mkV128 : Str -> V ;
mkV128 base =
  case base of {
    base_1+base_2@(?+?+?+?)+"i"+base_3@? => lin V
      { s = base_1+base_2+"i"+base_3 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+"i"+base_3 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+base_3+"eadh" ;
        participle = base_1+base_2+"i"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV128"
  } ;

mkV129 : Str -> V ;
mkV129 base =
  case base of {
    base_1+base_2@(?+?+?+?+?+?) => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2 ;
        participle = base_1+base_2+"e"
      };
    _ => error "Can't apply paradigm mkV129"
  } ;

mkV130 : Str -> V ;
mkV130 base =
  case base of {
    base_1+base_2@(?+?+?) => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2 ;
        participle = base_1+base_2+"e"
      };
    _ => error "Can't apply paradigm mkV130"
  } ;

mkV131 : Str -> V ;
mkV131 base =
  case base of {
    base_1+base_2@(?+?+?+?+?+?+?+?+?)+"d" => lin V
      { s = base_1+base_2+"d" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"daidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+"d" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"dadh" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV131"
  } ;

mkV132 : Str -> V ;
mkV132 base =
  case base of {
    base_1+"ì"+base_2@(?+?+?+?+?+?+?) => lin V
      { s = base_1+"ì"+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"i"+base_2+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"hì"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+"ì"+base_2+"adh" ;
        participle = base_1+"ì"+base_2+"e"
      };
    _ => error "Can't apply paradigm mkV132"
  } ;

mkV133 : Str -> V ;
mkV133 base =
  case base of {
    base_1+"ì"+base_2@(?+?+?)+"ò"+base_3@(?+?) => lin V
      { s = base_1+"ì"+base_2+"ò"+base_3 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"i"+base_2+"ò"+base_3+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"hì"+base_2+"ò"+base_3 ;
                 Dep => nonExist
               } ;
        noun = base_1+"ì"+base_2+"ò"+base_3+"adh" ;
        participle = base_1+"ì"+base_2+"o"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV133"
  } ;

mkV134 : Str -> V ;
mkV134 base =
  case base of {
    base_1@?+base_2+"i"+base_3@?+"n" => lin V
      { s = base_1+base_2+"i"+base_3+"n" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+"i"+base_3+"n" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+base_3+"eadh" ;
        participle = base_1+base_2+"i"+base_3+"nte"
      };
    _ => error "Can't apply paradigm mkV134"
  } ;

mkV135 : Str -> V ;
mkV135 base =
  case base of {
    base_1+"dhèa"+base_2@? => lin V
      { s = base_1+"dhèa"+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"ì" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"ri"+base_2+"n" ;
                 Dep => nonExist
               } ;
        noun = base_1+"dhèa"+base_2+"amh" ;
        participle = base_1+"dhèa"+base_2+"ta"
      };
    _ => error "Can't apply paradigm mkV135"
  } ;

mkV136 : Str -> V ;
mkV136 base =
  case base of {
    "n"+base_1+"ulaich" => lin V
      { s = "n"+base_1+"ulaich" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => "n"+base_1+"ulaichidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "n"+base_1+"ulaich" ;
                 Dep => nonExist
               } ;
        noun = "n"+base_1+"ulachadh" ;
        participle = "t"+base_1
      };
    _ => error "Can't apply paradigm mkV136"
  } ;

mkV137 : Str -> V ;
mkV137 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh’"+base_1 ;
                 Dep => nonExist
               } ;
        noun = base_1+"adh" ;
        participle = base_1+"te"
      };
    _ => error "Can't apply paradigm mkV137"
  } ;

mkV138 : Str -> V ;
mkV138 base =
  case base of {
    base_1+base_2@(?+?+?)+"t" => lin V
      { s = base_1+base_2+"t" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"daidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+"t" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"tadh" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV138"
  } ;

mkV139 : Str -> V ;
mkV139 base =
  case base of {
    base_1+base_2@(?+?+?+?)+"ig" => lin V
      { s = base_1+base_2+"ig" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"igidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+"ig" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"eadh" ;
        participle = base_1+base_2+"igte"
      };
    _ => error "Can't apply paradigm mkV139"
  } ;

mkV140 : Str -> V ;
mkV140 base =
  case base of {
    "rach" => lin V
      { s = "rach" ;
        conditional = table {
                        Sg => "rachainn" ;
                        Pl => "rachadh"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => "racham" ;
                               Pl => "rachamaid"
                             } ;
                       P2 => table {
                               Sg => "rach" ;
                               Pl => "rachaibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => "rachadh"
                             }
                     } ;
        future = table {
                   Indep => "thèid" ;
                   Dep => "tèid"
                 } ;
        past = table {
                 Indep => "chaidh" ;
                 Dep => "deach"
               } ;
        noun = "dol" ;
        participle = "rachte"
      };
    _ => error "Can't apply paradigm mkV140"
  } ;

mkV141 : Str -> V ;
mkV141 base =
  case base of {
    base_1+"h" => lin V
      { s = base_1+"h" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"haidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h" ;
                 Dep => nonExist
               } ;
        noun = base_1+"hadh" ;
        participle = base_1+"a"
      };
    _ => error "Can't apply paradigm mkV141"
  } ;

mkV142 : Str -> V ;
mkV142 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1 ;
                 Dep => nonExist
               } ;
        noun = base_1+"t" ;
        participle = base_1+"te"
      };
    _ => error "Can't apply paradigm mkV142"
  } ;

mkV143 : Str -> V ;
mkV143 base =
  case base of {
    base_1+"u"+base_2@(?+?) => lin V
      { s = base_1+"u"+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"u"+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"àin"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+"u"+base_2+"sinn" ;
        participle = base_1+"u"+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV143"
  } ;

mkV144 : Str -> V ;
mkV144 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1 ;
                 Dep => nonExist
               } ;
        noun = base_1+"ail" ;
        participle = base_1+"te"
      };
    _ => error "Can't apply paradigm mkV144"
  } ;

mkV145 : Str -> V ;
mkV145 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1 ;
                 Dep => nonExist
               } ;
        noun = base_1+"adh" ;
        participle = base_1+"a"
      };
    _ => error "Can't apply paradigm mkV145"
  } ;

mkV146 : Str -> V ;
mkV146 base =
  case base of {
    base_1+base_2@(?+?+?+?)+base_3@?+"in" => lin V
      { s = base_1+base_2+base_3+"in" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+base_3+"inidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+base_3+"in" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"n"+base_3+"dh" ;
        participle = base_1+base_2+base_3+"inte"
      };
    _ => error "Can't apply paradigm mkV146"
  } ;

mkV147 : Str -> V ;
mkV147 base =
  case base of {
    base_1+base_2@(?+?+?+?)+base_3@?+"inn" => lin V
      { s = base_1+base_2+base_3+"inn" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+base_3+"innidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+base_3+"inn" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"n"+base_3+"dh" ;
        participle = base_1+base_2+base_3+"innte"
      };
    _ => error "Can't apply paradigm mkV147"
  } ;

mkV148 : Str -> V ;
mkV148 base =
  case base of {
    base_1+base_2@(?+?+?) => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"amh" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV148"
  } ;

mkV149 : Str -> V ;
mkV149 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => "dh'"+base_1+"ainn" ; --guessed
                        Pl => "dh'"+base_1+"amaid" --guessed
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+"am" ; --guessed
                               Pl => base_1+"aibh"
                             } ;
                       P2 => table {
                               Sg => base_1 ; --guessed
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+"adh" --guessed
                             }
                     } ;
        future = table {
                   Indep => base_1+"idh" ; --guessed
                   Dep => base_1 --guessed
                 } ;
        past = table {
                 Indep => "dh'"+base_1 ; --guessed
                 Dep => "dh'"+base_1 --guessed
               } ;
        noun = base_1+"adh" ; --guessed
        participle = base_1+"te" --guessed
      };
    _ => error "Can't apply paradigm mkV149"
  } ;

mkV150 : Str -> V ;
mkV150 base =
  case base of {
    base_1+base_2@?+"g" => lin V
      { s = base_1+base_2+"g" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"gidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+base_2+"g" ;
                 Dep => nonExist
               } ;
        noun = base_1+"ga"+base_2+"l" ;
        participle = base_1+base_2+"gte"
      };
    _ => error "Can't apply paradigm mkV150"
  } ;

mkV151 : Str -> V ;
mkV151 base =
  case base of {
    base_1+"i"+base_2@?+"ic"+base_3@? => lin V
      { s = base_1+"i"+base_2+"ic"+base_3 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"i"+base_2+"ic"+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"i"+base_2+"ic"+base_3 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"ad"+base_3 ;
        participle = base_1+"i"+base_2+"ic"+base_3+"te"
      };
    _ => error "Can't apply paradigm mkV151"
  } ;

mkV152 : Str -> V ;
mkV152 base =
  case base of {
    base_1+"à"+base_2@(?+?) => lin V
      { s = base_1+"à"+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"a"+base_2+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"à"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+"a"+base_2+"adh" ;
        participle = base_1+"a"+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV152"
  } ;

mkV153 : Str -> V ;
mkV153 base =
  case base of {
    base_1+base_2@(?+?+?+?+?) => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"aidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"art" ;
        participle = nonExist
      };
    _ => error "Can't apply paradigm mkV153"
  } ;

mkV154 : Str -> V ;
mkV154 base =
  case base of {
    base_1+base_2@(?+?+?+?) => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => base_1+"h"+base_2+"ainn" ;
                        Pl => base_1+"h"+base_2+"amaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+base_2+"am" ;
                               Pl => base_1+base_2+"amaid"
                             } ;
                       P2 => table {
                               Sg => base_1+base_2 ;
                               Pl => base_1+base_2+"aibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+base_2+"adh"
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"aidh" ;
                   Dep => base_1+"h"+base_2
                 } ;
        past = table {
                 Indep => base_1+base_2 ;
                 Dep => base_1+"h"+base_2
               } ;
        noun = base_1+base_2+"adh" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV154"
  } ;

mkV155 : Str -> V ;
mkV155 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => base_1+"ainn" ;
                        Pl => base_1+"amaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+"am" ;
                               Pl => base_1+"amaid"
                             } ;
                       P2 => table {
                               Sg => base_1 ;
                               Pl => base_1+"aibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+"adh iad"
                             }
                     } ;
        future = table {
                   Indep => base_1 ;
                   Dep => base_1+"aidh"
                 } ;
        past = table {
                 Indep => base_1 ;
                 Dep => base_1
               } ;
        noun = base_1+"adh" ;
        participle = base_1+"te"
      };
    _ => error "Can't apply paradigm mkV155"
  } ;

mkV156 : Str -> V ;
mkV156 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => base_1+"ibh"
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"thidh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1 ;
                 Dep => nonExist
               } ;
        noun = base_1 ;
        participle = base_1+"te"
      };
    _ => error "Can't apply paradigm mkV156"
  } ;

mkV157 : Str -> V ;
mkV157 base =
  case base of {
    base_1+base_2@(?+?)+base_3@?+"ir" => lin V
      { s = base_1+base_2+base_3+"ir" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+base_3+"iridh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+base_3+"ir" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"r"+base_3+"dh" ;
        participle = base_1+base_2+base_3+"irte"
      };
    _ => error "Can't apply paradigm mkV157"
  } ;

mkV158 : Str -> V ;
mkV158 base =
  case base of {
    "tabhair" => lin V
      { s = "tabhair" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => "bheir" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "thug" ;
                 Dep => nonExist
               } ;
        noun = "toirt" ;
        participle = "tugta"
      };
    _ => error "Can't apply paradigm mkV158"
  } ;

mkV159 : Str -> V ;
mkV159 base =
  case base of {
    base_1@?+base_2+base_3@(?+?)+"r" => lin V
      { s = base_1+base_2+base_3+"r" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"r"+base_3+"dh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+base_3+"r" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+base_3+"rt" ;
        participle = base_1+base_2+base_3+"rte"
      };
    _ => error "Can't apply paradigm mkV159"
  } ;

mkV160 : Str -> V ;
mkV160 base =
  case base of {
    base_1+base_2@(?+?+?+?) => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"se" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV160"
  } ;

mkV161 : Str -> V ;
mkV161 base =
  case base of {
    base_1+"a"+base_2@?+"ra"+base_3@?+"ng" => lin V
      { s = base_1+"a"+base_2+"ra"+base_3+"ng" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"ài"+base_2+"n"+base_3+"dh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"ha"+base_2+"ra"+base_3+"ng" ;
                 Dep => nonExist
               } ;
        noun = base_1+"a"+base_2+"ra"+base_3+"ng" ;
        participle = base_1+"a"+base_2+"ra"+base_3+"ngte"
      };
    _ => error "Can't apply paradigm mkV161"
  } ;

mkV162 : Str -> V ;
mkV162 base =
  case base of {
    base_1+base_2@(?+?+?+?) => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => base_1+"h"+base_2+"inn" ;
                        Pl => base_1+"h"+base_2+"eamaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+base_2+"eam" ;
                               Pl => base_1+base_2+"eamaid"
                             } ;
                       P2 => table {
                               Sg => base_1+base_2 ;
                               Pl => base_1+base_2+"ibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+base_2+"eadh"
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"idh" ;
                   Dep => base_1+base_2
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => base_1+"h"+base_2
               } ;
        noun = base_1+base_2+"e" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV162"
  } ;

mkV163 : Str -> V ;
mkV163 base =
  case base of {
    base_1+"h"+base_2@(?+?) => lin V
      { s = base_1+"h"+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"h"+base_2 ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"hàin"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"hinn" ;
        participle = base_1+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV163"
  } ;

mkV164 : Str -> V ;
mkV164 base =
  case base of {
    base_1+"h"+base_2@(?+?+?)+base_3@(?+?+?+?+?+?+?+?+?+?) => lin V
      { s = base_1+"h"+base_2+base_3 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => nonExist ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => nonExist ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"t"+base_3 ;
        participle = nonExist
      };
    _ => error "Can't apply paradigm mkV164"
  } ;

mkV165 : Str -> V ;
mkV165 base =
  case base of {
    "thoir" => lin V
      { s = "thoir" ;
        conditional = table {
                        Sg => "bheirinn" ;
                        Pl => "bheireadh"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => "thoiream" ;
                               Pl => "thoireamaid"
                             } ;
                       P2 => table {
                               Sg => "thoir" ;
                               Pl => "thoiribh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => "thoireadh"
                             }
                     } ;
        future = table {
                   Indep => "bheir" ;
                   Dep => "toir"
                 } ;
        past = table {
                 Indep => "thug" ;
                 Dep => "tug"
               } ;
        noun = "toirt" ;
        participle = "tugta"
      };
    _ => error "Can't apply paradigm mkV165"
  } ;

mkV166 : Str -> V ;
mkV166 base =
  case base of {
    base_1+base_2@(?+?+?) => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"eil" ;
        participle = base_1+base_2+"the"
      };
    _ => error "Can't apply paradigm mkV166"
  } ;

mkV167 : Str -> V ;
mkV167 base =
  case base of {
    base_1+base_2@(?+?+?) => lin V
      { s = base_1+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"eam" ;
        participle = base_1+base_2+"e"
      };
    _ => error "Can't apply paradigm mkV167"
  } ;

mkV168 : Str -> V ;
mkV168 base =
  case base of {
    base_1+base_2@(?+?+?)+base_3@?+"inn" => lin V
      { s = base_1+base_2+base_3+"inn" ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+base_2+"n"+base_3+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => base_1+"h"+base_2+base_3+"inn" ;
                 Dep => nonExist
               } ;
        noun = base_1+base_2+"n"+base_3+"dh" ;
        participle = base_1+base_2+base_3+"inte"
      };
    _ => error "Can't apply paradigm mkV168"
  } ;

mkV169 : Str -> V ;
mkV169 base =
  case base of {
    base_1+"i"+base_2@(?+?) => lin V
      { s = base_1+"i"+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"i"+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"i"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+"a"+base_2 ;
        participle = nonExist
      };
    _ => error "Can't apply paradigm mkV169"
  } ;

mkV170 : Str -> V ;
mkV170 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => "dh'"+base_1+"ainn" ; --guessed
                        Pl => "dh'"+base_1+"amaid" --guessed
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+"am" ; --guessed
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P2 => table {
                               Sg => base_1 ; --guessed
                               Pl => base_1+"aibh" --guessed
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+"adh" --guessed
                             }
                     } ;
        future = table {
                   Indep => base_1+"idh" ; --guessed
                   Dep => base_1 --guessed
                 } ;
        past = table {
                 Indep => "dh'"+base_1 ;
                 Dep => "dh'"+base_1 --guessed
               } ;
        noun = base_1 ;
        participle = base_1+"te" --guessed
      };
    _ => error "Can't apply paradigm mkV170"
  } ;

mkV171 : Str -> V ;
mkV171 base =
  case base of {
    base_1+"i"+base_2@(?+?) => lin V
      { s = base_1+"i"+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"i"+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"i"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+"ea"+base_2 ;
        participle = base_1+"i"+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV171"
  } ;

mkV172 : Str -> V ;
mkV172 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1 ;
                 Dep => nonExist
               } ;
        noun = base_1+"eachd" ;
        participle = base_1+"te"
      };
    _ => error "Can't apply paradigm mkV172"
  } ;

mkV173 : Str -> V ;
mkV173 base =
  case base of {
    base_1+"c"+base_2@? => lin V
      { s = base_1+"c"+base_2 ;
        conditional = table {
                        Sg => nonExist ;
                        Pl => nonExist
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P2 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => nonExist
                             }
                     } ;
        future = table {
                   Indep => base_1+"c"+base_2+"idh" ;
                   Dep => nonExist
                 } ;
        past = table {
                 Indep => "dh'"+base_1+"c"+base_2 ;
                 Dep => nonExist
               } ;
        noun = base_1+"g"+base_2 ;
        participle = base_1+"c"+base_2+"te"
      };
    _ => error "Can't apply paradigm mkV173"
  } ;

mkV174 : Str -> V ;
mkV174 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => "dh'"+base_1+"inn" ;
                        Pl => "dh'"+base_1+"eamaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+"eam" ;
                               Pl => base_1+"eamaid"
                             } ;
                       P2 => table {
                               Sg => base_1 ;
                               Pl => base_1+"ibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+"eadh"
                             }
                     } ;
        future = table {
                   Indep => base_1+"idh" ;
                   Dep => base_1
                 } ;
        past = table {
                 Indep => "dh'"+base_1 ;
                 Dep => "dh'"+base_1
               } ;
        noun = base_1+"eachd" ;
        participle = base_1+"e"
      };
    _ => error "Can't apply paradigm mkV174"
  } ;

mkV175 : Str -> V ;
mkV175 base =
  case base of {
    base_1 => lin V
      { s = base_1 ;
        conditional = table {
                        Sg => "dh'"+base_1+"ainn" ;
                        Pl => "dh'"+base_1+"amaid"
                      } ;
        imperative = table {
                       P1 => table {
                               Sg => base_1+"am" ;
                               Pl => base_1+"amaid"
                             } ;
                       P2 => table {
                               Sg => base_1 ;
                               Pl => base_1+"aibh"
                             } ;
                       P3 => table {
                               Sg => nonExist ;
                               Pl => base_1+"adh"
                             }
                     } ;
        future = table {
                   Indep => base_1+"aidh" ;
                   Dep => base_1
                 } ;
        past = table {
                 Indep => "dh'"+base_1 ;
                 Dep => "dh'"+base_1
               } ;
        noun = base_1 ;
        participle = base_1+"ta"
      };
    _ => error "Can't apply paradigm mkV175"
  } ;
}
