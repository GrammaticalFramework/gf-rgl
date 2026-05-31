concrete NumeralFao of Numeral = CatFao [Numeral, Digits, Decimal] ** open Prelude, ResFao in {

lincat
  Digit = {s : Gender => Case => Str ; n : Number ; teen : Str ; ten : Str} ;
  Sub10, Sub100, Sub1000, Sub1000000, Sub1000000000, Sub1000000000000 =
    {s : Gender => Case => Str ; n : Number} ;
  Dig = {s : Str; n : Number} ;

lin
  num n = n ;

  n2 = {s = table {
              Masc   => caseList "tveir" "tveir" "tveimum" "tveggja" ;
              Fem    => caseList "tvær"  "tvær"  "tveimum" "tveggja" ;
              Neuter => caseList "tvey"  "tvey"  "tveimum" "tveggja"
            } ;
        teen = "tólv" ;
        ten = "tjúgu" ;
        n = Pl
       } ;
  n3 = {s = table {
              Masc   => caseList "tríggir"  "tríggjar" "trimum" "tríggja" ;
              Fem    => caseList "tríggjar" "tríggjar" "trimum" "tríggja" ;
              Neuter => caseList "trý"      "trý"      "trimum" "tríggja"
            } ;
        teen = "trettan" ;
        ten = "tríati" ;
        n = Pl
       } ;
  n4 = mkDigit "fýra" "fjúrtan" "fýrati" ;
  n5 = mkDigit "fimm" "fimtan" "fimmti" ;
  n6 = mkDigit "seks" "sekstan" "seksti" ;
  n7 = mkDigit "sjey" "seytjan" "sjeyti" ;
  n8 = mkDigit "átta" "átjan" "áttati" ;
  n9 = mkDigit "níggju" "nítjan" "níti" ;

  pot01 = {
    s = table {
          Masc   => caseList "ein"   "ein"  "einum"  "eins" ;
          Fem    => caseList "einar" "eina" "einari" "einar" ;
          Neuter => caseList "eitt"  "eitt" "einum"  "eins"
        } ;
    n = Sg
  } ;
  pot0 d = {s = d.s ; n = d.n} ;
  pot0as1 n = n ;
  pot110 = {
    s = table {
          Masc   => \\_ => "tíggju" ;
          Fem    => \\_ => "tíggju" ;
          Neuter => \\_ => "ti"
        } ;
    n = Pl
  } ;
  pot111 = mkNum "ellivu" Pl ;
  pot1to19 d = mkNum d.teen Pl ;
  pot1 d = mkNum d.ten Pl ;
  pot1plus d e = {s = \\g,c => e.s ! g ! c ++ "og" ++ d.ten ; n = Pl} ;
  pot1as2 n = n ;
  pot21 = mkNum "hundrað" Pl ;
  pot2 d = mkNum (numBase d ++ "hundrað") Pl ;
  pot2plus d e = {s = \\g,c => numBase d ++ "hundrað" ++ e.s ! g ! c ; n = Pl} ;
  pot2as3 n = n ;
  pot31 = mkNum "túsund" Pl ;
  pot3 n = mkNum (numBase n ++ "túsund") Pl ;
  pot3plus n m = {s = \\g,c => numBase n ++ "túsund" ++ m.s ! g ! c ; n = Pl} ;
  pot3as4 n = n ;
  pot3decimal d = mkNum (d.s ++ "túsund") Pl ;
  pot41 = mkNum "millión" Pl ;
  pot4 n = mkNum (numBase n ++ "millión") Pl ;
  pot4plus n m = {s = \\g,c => numBase n ++ "millión" ++ m.s ! g ! c ; n = Pl} ;
  pot4as5 n = n ;
  pot4decimal d = mkNum (d.s ++ "millión") Pl ;
  pot51 = mkNum "milliard" Pl ;
  pot5 n = mkNum (numBase n ++ "milliard") Pl ;
  pot5plus n m = {s = \\g,c => numBase n ++ "milliard" ++ m.s ! g ! c ; n = Pl} ;
  pot5decimal d = mkNum (d.s ++ "milliard") Pl ;

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

  mkNum : Str -> Number -> {s : Gender => Case => Str ; n : Number} = \str,n -> {
    s = \\_,_ => str ;
    n = n
  } ;

  mkDigit : Str -> Str -> Str -> {s : Gender => Case => Str ; n : Number ; teen : Str ; ten : Str} = \str,teen,ten -> {
    s = \\_,_ => str ;
    teen = teen ;
    ten = ten ;
    n = Pl
  } ;

  numBase : {s : Gender => Case => Str ; n : Number} -> Str = \n ->
    n.s ! Neuter ! Nom ;
}
