concrete NumeralFao of Numeral = CatFao [Numeral, Digits, Decimal] ** open Prelude, ResFao in {

lincat
  Digit = {
    s,teen,ten : CardOrd => Gender => Case => Str ;
    n : Number ;
  } ;
  Sub10 = {
    s : CardOrd => Gender => Case => Str ;
    hundredth, thousandth, millionth, milliardth : Gender => Case => Str ;
    n : Number
  } ;
  Sub100, Sub1000 = {
    s : CardOrd => Gender => Case => Str ;
    thousandth, millionth, milliardth : Gender => Case => Str ;
    n : Number
  } ;
  Sub1000000, Sub1000000000, Sub1000000000000 =
    {s : CardOrd => Gender => Case => Str ; n : Number} ;
  Dig = {s : Str; n : Number} ;

lin
  num n = n ;

  n2 = {
    s = table {
          NCard =>  table {
                      Masc   => caseList "tveir" "tveir" "tveimum" "tveggja" ;
                      Fem    => caseList "tvær"  "tvær"  "tveimum" "tveggja" ;
                      Neuter => caseList "tvey"  "tvey"  "tveimum" "tveggja"
                    } ;
          NOrd n => table {
                      Masc   => case n of {
                                  Sg => caseList "annar" "annan" "øðrum" "annars" ;
                                  Pl => caseList "aðrir" "aðrar" "øðrum" "annara"
                                } ;
                      Fem    => case n of {
                                  Sg => caseList "onnur" "aðra" "aðrari" "annarar" ;
                                  Pl => caseList "aðrar" "aðrar" "øðrum" "annara"
                                } ;
                      Neuter => case n of {
                                  Sg => caseList "annað" "annað" "øðrum" "annars" ;
                                  Pl => caseList "onnur" "onnur" "øðrum" "annara"
                                }
                    }
        } ;
    teen = table {
             NCard  => \\_,_ => "tólv" ;
             NOrd n => mkWeakOrd "tólvti" ! n
           } ;
    ten  = table {
             NCard  => \\_,_ => "tjúgu" ;
             NOrd n => mkWeakOrd "tjúgundi" ! n
           } ;
    n = Pl
  } ;
  n3 = {
    s = table {
          NCard => table {
                     Masc   => caseList "tríggir"  "tríggjar" "trimum" "tríggja" ;
                     Fem    => caseList "tríggjar" "tríggjar" "trimum" "tríggja" ;
                     Neuter => caseList "trý"      "trý"      "trimum" "tríggja"
                   } ;
          NOrd n => mkOrdForms "triði" "triðja" "triðju" ! n
        } ;
    teen = table {
             NCard  => \\_,_ => "trettan" ;
             NOrd n => mkWeakOrd "trettandi" ! n
           } ;
    ten = table {
             NCard  => \\_,_ => "tríati" ;
             NOrd n => mkWeakOrd "tretivundi" ! n
          } ;
    n = Pl
  } ;
  n4 = mkDigit "fýra" "fjúrtan" "fýrati" "fjórði" "fjúrtandi" "fjørutandi" ;
  n5 = mkDigit "fimm" "fimtan" "fimmti" "fimti" "fimtandi" "fimtandi" ;
  n6 = mkDigit "seks" "sekstan" "seksti" "sætti" "sekstandi" "sekstandi" ;
  n7 = mkDigit "sjey" "seytjan" "sjeyti" "sjeyndi" "seytjandi" "sjútandi" ;
  n8 = mkDigit "átta" "átjan" "áttati" "áttandi" "átjandi" "áttandi" ;
  n9 = mkDigit "níggju" "nítjan" "níti" "níggjundi" "nítjandi" "nítandi" ;

  pot01 = {
    s = table {
          NCard => table {
                     Masc   => caseList "ein"   "ein"  "einum"  "eins" ;
                     Fem    => caseList "einar" "eina" "einari" "einar" ;
                     Neuter => caseList "eitt"  "eitt" "einum"  "eins"
                   } ;
          NOrd n => mkWeakOrd "fyrsti" ! n
        } ;
    hundredth = mkWeakOrd "hundraðandi" ! Sg ;
    thousandth = mkWeakOrd "túsundandi" ! Sg ;
    millionth = mkWeakOrd "milliónandi" ! Sg ;
    milliardth = mkWeakOrd "milliardandi" ! Sg ;
    n = Sg
  } ;
  pot0 d = d ** {
    hundredth = mkCompoundOrd (numBase d) "hundraðandi" ! Pl ;
    thousandth = mkCompoundOrd (numBase d) "túsundandi" ! Pl ;
    millionth = mkCompoundOrd (numBase d) "milliónandi" ! Pl ;
    milliardth = mkCompoundOrd (numBase d) "milliardandi" ! Pl
  } ;

  pot0as1 n = n ;

  pot110 =
    let ten =
          table {
            Neuter => \\_ => "ti" ;
            _      => \\_ => "tíggju"
          } ;
    in mkCardOrd ten
                 (mkWeakOrd "tíggjundi")  ** {
         thousandth = \\g,c => ten ! g ! Nom ++ mkWeakOrd "túsundandi" ! Pl ! g ! Nom ;
         millionth = \\g,c => ten ! g ! Nom ++ mkWeakOrd "milliónandi" ! Pl ! g ! Nom ;
         milliardth = \\g,c => ten ! g ! Nom ++ mkWeakOrd "milliardandi" ! Pl ! g ! Nom
       };

  pot111 =
    mkCardOrd (\\_,_ => "ellivu")
              (mkWeakOrd "ellivti") ** {
      thousandth = \\g,c => "ellivu" ++ mkWeakOrd "túsundandi" ! Pl ! g ! Nom ;
      millionth = \\g,c => "ellivu" ++ mkWeakOrd "milliónandi" ! Pl ! g ! Nom ;
      milliardth = \\g,c => "ellivu" ++ mkWeakOrd "milliardandi" ! Pl ! g ! Nom
    };

  pot1to19 d =
    mkCardOrd (d.teen ! NCard)
              (\\n => d.teen ! NOrd n) ** {
      thousandth = \\g,c => d.teen ! NCard ! g ! Nom ++ mkWeakOrd "túsundandi" ! Pl ! g ! Nom ;
      millionth = \\g,c => d.teen ! NCard ! g ! Nom ++ mkWeakOrd "milliónandi" ! Pl ! g ! Nom ;
      milliardth = \\g,c => d.teen ! NCard ! g ! Nom ++ mkWeakOrd "milliardandi" ! Pl ! g ! Nom
    };

  pot1 d = 
    mkCardOrd (d.ten ! NCard)
              (\\n => d.ten ! NOrd n) ** {
      thousandth = \\g,c => d.ten ! NCard ! g ! Nom ++ mkWeakOrd "túsundandi" ! Pl ! g ! Nom ;
      millionth = \\g,c => d.ten ! NCard ! g ! Nom ++ mkWeakOrd "milliónandi" ! Pl ! g ! Nom ;
      milliardth = \\g,c => d.ten ! NCard ! g ! Nom ++ mkWeakOrd "milliardandi" ! Pl ! g ! Nom
    } ;
  pot1plus d e = {
    s = table {
          NCard  => \\g,c => e.s ! NCard ! g ! c ++ "og" ++ d.ten ! NCard ! g ! Nom ;
          NOrd n => \\g,c => d.ten ! NCard ! g ! c ++ "og" ++ e.s ! NOrd n ! g ! Nom
        } ;
    thousandth = \\g,c => d.ten ! NCard ! g ! Nom ++ "og" ++ e.s ! NCard ! g ! c ++ mkWeakOrd "túsundandi" ! Pl ! g ! Nom ;
    millionth = \\g,c => d.ten ! NCard ! g ! Nom ++ "og" ++ e.s ! NCard ! g ! c ++ mkWeakOrd "milliónandi" ! Pl ! g ! Nom ;
    milliardth = \\g,c => d.ten ! NCard ! g ! Nom ++ "og" ++ e.s ! NCard ! g ! c ++ mkWeakOrd "milliardandi" ! Pl ! g ! Nom ;
    n = Pl
  } ;

  pot1as2 n = n ;

  pot21 =
    mkCardOrd (\\_,_ => "hundrað")
              (mkWeakOrd "hundraðandi") ** {
      thousandth = \\g,c => "hundrað" ++ mkWeakOrd "túsundandi" ! Pl ! g ! Nom ;
      millionth = \\g,c => "hundrað" ++ mkWeakOrd "milliónandi" ! Pl ! g ! Nom ;
      milliardth = \\g,c => "hundrað" ++ mkWeakOrd "milliardandi" ! Pl ! g ! Nom
    } ;

  pot2 d =
    mkCardOrd (\\_,_ => numBase d ++ "hundrað")
              (\\_ => d.hundredth) ** {
      thousandth = \\g,c => numBase d ++ "hundrað" ++ mkWeakOrd "túsundandi" ! Pl ! g ! Nom ;
      millionth = \\g,c => numBase d ++ "hundrað" ++ mkWeakOrd "milliónandi" ! Pl ! g ! Nom ;
      milliardth = \\g,c => numBase d ++ "hundrað" ++ mkWeakOrd "milliardandi" ! Pl ! g ! Nom
    } ;

  pot2plus d e = {
    s = table {
          NCard  => \\g,c => numBase d ++ "hundrað" ++ e.s ! NCard ! g ! c ;
          NOrd n => \\g,c => numBase d ++ "hundrað" ++ "og" ++ e.s ! NOrd n ! g ! c
        } ;
    thousandth = \\g,c => numBase d ++ "hundrað" ++ e.s ! NCard ! g ! c ++ mkWeakOrd "túsundandi" ! Pl ! g ! Nom ;
    millionth = \\g,c => numBase d ++ "hundrað" ++ e.s ! NCard ! g ! c ++ mkWeakOrd "milliónandi" ! Pl ! g ! Nom ;
    milliardth = \\g,c => numBase d ++ "hundrað" ++ e.s ! NCard ! g ! c ++ mkWeakOrd "milliardandi" ! Pl ! g ! Nom ;
    n = Pl
  } ;

  pot2as3 n = n ;

  pot31 = mkCardOrd (\\_,_ => "túsund")
                    (mkWeakOrd "túsundandi") ;

  pot3 n = mkCardOrd (\\_,_ => numBase n ++ "túsund")
                     (\\_ => n.thousandth) ;

  pot3plus n m = {
    s = table {
          NCard    => \\g,c => numBase n ++ "túsund" ++ m.s ! NCard ! g ! c ;
          NOrd num => \\g,c => numBase n ++ "túsund" ++ "og" ++ m.s ! NOrd num ! g ! c
        } ;
    n = Pl
  } ;

  pot3as4 n = n ;

  pot3decimal d = mkCardOrd (\\_,_ => d.s ++ "túsund")
                            (\\_,_,_ => d.s ++ "túsundandi") ;

  pot41 = mkCardOrd (\\_,_ => "millión")
                    (mkWeakOrd "milliónandi") ;

  pot4 n = mkCardOrd (\\_,_ => numBase n ++ "millión")
                     (\\_ => n.millionth) ;

  pot4plus n m = {
    s = table {
          NCard    => \\g,c => numBase n ++ "millión" ++ m.s ! NCard ! g ! c ;
          NOrd num => \\g,c => numBase n ++ "millión" ++ "og" ++ m.s ! NOrd num ! g ! c
        } ;
    n = Pl
  } ;

  pot4as5 n = n ;

  pot4decimal d = mkCardOrd (\\_,_ => d.s ++ "millión")
                            (\\_,_,_ => d.s ++ "milliónandi") ;

  pot51 = mkCardOrd (\\_,_ => "milliard")
                    (mkWeakOrd "milliardandi") ;

  pot5 n = mkCardOrd (\\_,_ => numBase n ++ "milliard")
                     (\\_ => n.milliardth) ;

  pot5plus n m = {
    s = table {
          NCard    => \\g,c => numBase n ++ "milliard" ++ m.s ! NCard ! g ! c ;
          NOrd num => \\g,c => numBase n ++ "milliard" ++ "og" ++ m.s ! NOrd num ! g ! c
        } ;
    n = Pl
  } ;

  pot5decimal d = mkCardOrd (\\_,_ => d.s ++ "milliard")
                            (\\_,_,_ => d.s ++ "milliardandi") ;

  IDig d = d ;
  IIDig d ds = {s = d.s ++ BIND ++ ds.s; n = Pl} ;

  D_0 = {s = "0"; n = Pl} ;
  D_1 = {s = "1"; n = Sg} ;
  D_2 = {s = "2"; n = Pl} ;
  D_3 = {s = "3"; n = Pl} ;
  D_4 = {s = "4"; n = Pl} ;
  D_5 = {s = "5"; n = Pl} ;
  D_6 = {s = "6"; n = Pl} ;
  D_7 = {s = "7"; n = Pl} ;
  D_8 = {s = "8"; n = Pl} ;
  D_9 = {s = "9"; n = Pl} ;

  PosDecimal d = d ** {hasDot = False} ;
  NegDecimal d = {s = "-" ++ BIND ++ d.s; n = Pl; hasDot = False} ;
  IFrac d i = {s = d.s ++ if_then_Str d.hasDot BIND (BIND++"."++BIND) ++ i.s; n = Pl; hasDot = True} ;

oper
  caseList : Str -> Str -> Str -> Str -> Case => Str = \nom,acc,dat,gen ->
    table {Nom => nom ; Acc => acc ; Dat => dat ; Gen => gen} ;

  mkCardOrd :
    (Gender => Case => Str) -> 
    (Number => Gender => Case => Str) ->
    {s : CardOrd => Gender => Case => Str ; n : Number} =
    \card,ord -> {
      s = table {
            NCard  => card ;
            NOrd n => ord ! n
          } ;
      n = Pl
    } ;

  mkDigit :
    Str -> Str -> Str -> Str -> Str -> Str -> Digit =
    \str,teen,ten,unitOrd,teenOrd,tenOrd -> lin Digit {
      s = table {
            NCard  => \\_,_ => str ;
            NOrd n => mkWeakOrd unitOrd ! n
          } ;
      teen = table {
               NCard  => \\_,_ => teen ;
               NOrd n => mkWeakOrd teenOrd ! n
             } ;
      ten  = table {
               NCard  => \\_,_ => ten ;
               NOrd n => mkWeakOrd tenOrd ! n
             } ;
      n = Pl
    } ;

  mkOrdForms : Str -> Str -> Str -> Number => Gender => Case => Str = \masc,obl,pl ->
    table {
      Sg => 
        table {
          Masc => caseList masc obl obl obl ;
          Fem => caseList obl pl pl pl ;
          Neuter => \\_ => obl
        } ;
      Pl => \\_,_ => pl
    } ;

  mkWeakOrd : Str -> Number => Gender => Case => Str = \lemma ->
    let stem : Str = init lemma in
    mkOrdForms lemma (stem + "a") (stem + "u") ;

  mkCompoundOrd : Str -> Str -> Number => Gender => Case => Str = \prefix,lemma ->
    let stem : Str = init lemma in
    mkOrdForms
      (prefix ++ BIND ++ lemma)
      (prefix ++ BIND ++ (stem + "a"))
      (prefix ++ BIND ++ (stem + "u")) ;

  numBase : {s : CardOrd => Gender => Case => Str} -> Str = \n ->
    n.s ! NCard ! Neuter ! Nom ;
}
