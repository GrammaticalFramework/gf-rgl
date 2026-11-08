--# -path=.:abstract:common:prelude

concrete NumeralLav of Numeral = CatLav [Numeral,Digits,Decimal] ** open ResLav, ParadigmsLav, Prelude in {

flags coding = utf8 ;

lincat

  -- TODO: formas, kas pieprasa ģenitīvu - tūkstotis grāmatu, trīs simti meiteņu
  Digit = { s : DForm => CardOrd => Gender => Case => Str } ;
  Sub10 = { s : CardOrd => Gender => Case => Str ; num : Number } ;
  Sub100 = { s : CardOrd => Gender => Case => Str ; num : Number } ;
  Sub1000 = { s : CardOrd => Gender => Case => Str ; num : Number } ;
  Sub1000000 = { s : CardOrd => Gender => Case => Str ; num : Number } ;
  Sub1000000000 = { s : CardOrd => Gender => Case => Str ; num : Number } ;
  Sub1000000000000 = { s : CardOrd => Gender => Case => Str ; num : Number } ;

lin

  num x = x ;

  n2 = mkNumReg "divi" "otrais" Pl ;

  n3 =
    let trijs = mkNumSpec "trijs" "trešais" "trīs" "trīs" Pl
    in {
      s = \\f,o,g,c => case <f, o, g, c> of {
        <DUnit, NCard, _, Nom> => "trīs" ;
        <DUnit, NCard, _, Dat> => "trim" ;
        <DUnit, NCard, _, Loc> => "trīs" ;
        _ => trijs.s ! f ! o ! g ! c
      }
    } | {
      s = \\f,o,g,c => case <f, o, g, c> of {
        <DUnit, NCard, _, Nom> => "trīs" ;
        _ => trijs.s ! f ! o ! g ! c
    }
  } ;

  n4 = mkNumReg "četri" "ceturtais" Pl ;
  n5 = mkNumReg "pieci" "piektais" Pl ;
  n6 = mkNumReg "seši" "sestais" Pl ;
  n7 = mkNumReg "septiņi" "septītais" Pl ;
  n8 = mkNumReg "astoņi" "astotais" Pl ;
  n9 = mkNumReg "deviņi" "devītais" Pl ;

  pot01 = { s = viens.s ! DUnit } ** { num = Sg } ;
  pot0 d = { s = d.s ! DUnit } ** { num = Pl } ;
  pot110 = { s = viens.s ! DTen } ** { num = Pl } ;
  pot111 = { s = viens.s ! DTeen } ** { num = Pl } ;
  pot1to19 d = { s = d.s ! DTeen } ** { num = Pl } ;
  pot0as1 n = { s = n.s ; num = n.num } ;
  pot1 d = { s = d.s ! DTen } ** { num = Pl } ;

  pot1plus d e = {
    s = \\o,g,c => d.s ! DTen ! NCard ! Masc ! Nom ++ e.s ! o ! g ! c ;
    num = e.num
  } ;

  pot1as2 n = n ;

  pot21 = {s = \\_,_,_ => "simts" ; num = Pl} ;

  -- FIXME: nav īsti labi, kārtas skaitlim ir jābūt 'trīssimtais' utml
  pot2 d = {
    s = \\o,g,c => d.s ! NCard ! Masc ! Nom ++ simts ! o ! g ! d.num ! c ;
    num = Pl
  } ;

  pot2plus d e = {
    s = \\o,g,c => d.s ! NCard ! Masc ! Nom ++ simts ! NCard ! Masc ! d.num ! Nom ++ e.s ! o ! g ! c ;
    num = e.num
  } ;

  pot2as3 n = n ;

  pot31 = {s = \\_,_,_ => "tūkstotis" ; num = Pl} ;

  pot3 d = {
    s = \\o,g,c => d.s ! NCard ! Masc ! Nom ++ tuukstotis ! o ! g ! d.num ! c ;
    num = Pl
  } ;

  pot3plus d e = {
    s = \\o,g,c => d.s ! NCard ! Masc ! Nom ++ tuukstotis ! NCard ! Masc ! d.num ! Nom ++ e.s ! o ! g ! c ;
    num = e.num
  } ;

  pot3as4 n = n ;

  pot3decimal d = {s = \\_,_,_ => d.s ! NCard ++ "tūkstoši" ; num = Pl} ;

  pot41 = {s = \\_,_,_ => "miljons" ; num = Pl} ;
  pot4 n = {
    s = \\_,_,_ => n.s ! NCard ! Masc ! Nom ++ "miljoni" ; num = Pl
  } ;
  pot4plus n m = {
    s = \\o,g,c => n.s ! NCard ! Masc ! Nom ++ "miljoni" ++ m.s ! o ! g ! c ;
    num = m.num
  } ;
  pot4decimal d = {s = \\_,_,_ => d.s ! NCard ++ "miljoni" ; num = Pl} ;

  pot4as5 n = n ;

  pot51 = {s = \\_,_,_ => "miljards" ; num = Pl} ;
  pot5 n = {
    s = \\_,_,_ => n.s ! NCard ! Masc ! Nom ++ "miljardi" ; num = Pl
  } ;
  pot5plus n m = {
    s = \\o,g,c => n.s ! NCard ! Masc ! Nom ++ "miljardi" ++ m.s ! o ! g ! c ;
    num = m.num
  } ;
  pot5decimal d = {s = \\_,_,_ => d.s ! NCard ++ "miljardi" ; num = Pl} ;

-- Numerals as sequences of digits:

lincat
  Dig = { num : Number ; s : CardOrd => Str } ;

lin
  IDig d = d ;

  IIDig d i = {
    s = \\o => d.s ! NCard ++ BIND ++ i.s ! o ;
    num = Pl ;	-- FIXME: 1 cilvēks, 11 cilvēki, 21 cilvēks, ...
  } ;

  D_0 = mkDig "0" ;
  D_1 = mk2Dig "1" Sg ;
  D_2 = mkDig "2" ;
  D_3 = mkDig "3" ;
  D_4 = mkDig "4" ;
  D_5 = mkDig "5" ;
  D_6 = mkDig "6" ;
  D_7 = mkDig "7" ;
  D_8 = mkDig "8" ;
  D_9 = mkDig "9" ;

  PosDecimal d = d ** {hasDot=False} ;
  NegDecimal d = {
    s = \\o => "-" ++ BIND ++ d.s ! o ;
    num = Pl ;
    hasDot=False
  } ;
  IFrac d i = {
    s = \\o => d.s ! NCard ++
               if_then_Str d.hasDot BIND (BIND++"."++BIND) ++
               i.s ! o;
    num = Pl ;
    hasDot=True
    } ;

oper
  mkDig : Str -> Dig = \c -> mk2Dig c Pl ;

  mk2Dig : Str -> Number -> Dig = \c,n -> lin Dig {
    s = table { NCard => c ; NOrd => c + "." } ;
    num = n
  } ;

}
