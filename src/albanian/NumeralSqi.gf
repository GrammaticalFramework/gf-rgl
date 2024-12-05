concrete NumeralSqi of Numeral = CatSqi ** open ParamX, Prelude in {

oper bind : Str -> Str -> Str = \a -> \b -> a ++ b ;

param DForm = unit | teen | ten ;

oper LinDigit = {s : DForm => Str };
oper LinSub100 = {s : Str } ;

lincat Digit =      LinDigit ;
lincat Sub10 =      LinDigit ;
lincat Sub100 =     LinSub100 ;
lincat Sub1000 =    LinSub100 ;
lincat Sub1000000 = { s : Str } ;

oper mkNum : Str -> LinDigit = \tri -> 
  { s = table {unit => tri ; teen => tri + "mbë" + "dhjetë" ; ten => tri + "dhjetë" }};

lin num x = {s = x.s } ; 

lin n2 = {s = table {unit => "dy" ; teen => "dy" + "mbë" + "dhjetë" ; ten => "njëzet" }};
lin n3 = mkNum "tre" ;
lin n4 = {s = table {unit => "katër" ; teen => "katër" + "mbë" + "dhjetë" ; ten => "dyzet" } };
lin n5 = mkNum "pesë" ;
lin n6 = mkNum "gjashtë";
lin n7 = mkNum "shtatë";
lin n8 = mkNum "tetë";
lin n9 = mkNum "nëntë";

oper mkR : Str -> LinSub100 = \n -> {s = n } ;

lin pot01 = { s = table {_ => "një" }};
lin pot0 d = d ;
lin pot110 = mkR "dhjetë" ;
lin pot111 = mkR ("një" + "mbë" + "dhjetë") ;
lin pot1to19 d = mkR (d.s ! teen) ;
lin pot0as1 n = mkR (n.s ! unit) ;
lin pot1 d = mkR (d.s ! ten) ;
lin pot1plus d e = mkR ((d.s ! ten) ++ "e" ++ (e.s ! unit)) ;
lin pot1as2 n = n ;
lin pot2 d = mkR (bind (d.s ! unit) "qind") ;
lin pot2plus d e = mkR ((bind (d.s ! unit) "qind") ++ "e" ++ e.s) ;
lin pot2as3 n = {s = n.s };
lin pot3 n = {s = n.s ++ "mijë" } ;
lin pot3plus n m = {s = n.s ++ "mijë" ++ m.s} ;


lincat Dig = {s : Str; n : Number} ;

lin IDig d = d ** {tail = T1} ;

    IIDig d i = {
      s = d.s ++ spaceIf i.tail ++ i.s ;
      n = Pl ;
      tail = inc i.tail
    } ;

    D_0 = mkDig "0" Pl ;
    D_1 = mkDig "1" Sg ;
    D_2 = mkDig "2" Pl ;
    D_3 = mkDig "3" Pl ;
    D_4 = mkDig "4" Pl ;
    D_5 = mkDig "5" Pl ;
    D_6 = mkDig "6" Pl ;
    D_7 = mkDig "7" Pl ;
    D_8 = mkDig "8" Pl ;
    D_9 = mkDig "9" Pl ;

lin PosDecimal d = d ** {hasDot=False} ;
    NegDecimal d = {s="-" ++ BIND ++ d.s;  hasDot=False; n = Pl} ;
    IFrac d i = {
        s=d.s ++
          if_then_Str d.hasDot BIND (BIND++","++BIND) ++
          i.s ;
        hasDot=True;
        n = Pl
    } ;

oper
    mkDig : Str -> Number -> Dig = \s,n -> lin Dig {s=s; n=n} ;

    spaceIf : DTail -> Str = \t -> case t of {
      T3 => "" ;
      _  => BIND
      } ;

    inc : DTail -> DTail = \t -> case t of {
      T1 => T2 ;
      T2 => T3 ;
      T3 => T1
      } ;

}
