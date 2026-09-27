concrete NumeralKaz of Numeral = CatKaz [Numeral,Digits,Decimal] **
  open Prelude in {

  param DValue = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 ;

  lincat
    Dig, Digit = {s : Str; v : DValue} ;
    Sub10, Sub100, Sub1000, Sub1000000,
    Sub1000000000, Sub1000000000000 = {s : Str} ;

  oper
    tens : DValue => Str = table {
      D0 => ""; D1 => "он"; D2 => "жиырма"; D3 => "отыз"; D4 => "қырық";
      D5 => "елу"; D6 => "алпыс"; D7 => "жетпіс"; D8 => "сексен"; D9 => "тоқсан"
      } ;

  lin
    num n = {s=n.s} ;
    n2={s="екі";v=D2}; n3={s="үш";v=D3}; n4={s="төрт";v=D4}; n5={s="бес";v=D5};
    n6={s="алты";v=D6}; n7={s="жеті";v=D7}; n8={s="сегіз";v=D8}; n9={s="тоғыз";v=D9};
    pot01={s="бір"}; pot0 d={s=d.s}; pot0as1 n=n;
    pot110={s="он"}; pot111={s="он бір"}; pot1to19 d={s="он"++d.s};
    pot1 d={s=tens!d.v};
    pot1plus d n={s=(pot1 d).s++n.s}; pot1as2 n=n;
    pot21={s="жүз"}; pot2 n={s=n.s++"жүз"}; pot2plus n m={s=n.s++"жүз"++m.s}; pot2as3 n=n;
    pot31={s="мың"}; pot3 n={s=n.s++"мың"}; pot3plus n m={s=n.s++"мың"++m.s}; pot3as4 n=n;
    pot3decimal d={s=d.s++"мың"}; pot41={s="миллион"}; pot4 n={s=n.s++"миллион"};
    pot4plus n m={s=n.s++"миллион"++m.s}; pot4as5 n=n; pot4decimal d={s=d.s++"миллион"};
    pot51={s="миллиард"}; pot5 n={s=n.s++"миллиард"};
    pot5plus n m={s=n.s++"миллиард"++m.s}; pot5decimal d={s=d.s++"миллиард"};
    D_0={s="0";v=D0};D_1={s="1";v=D1};D_2={s="2";v=D2};D_3={s="3";v=D3};D_4={s="4";v=D4};
    D_5={s="5";v=D5};D_6={s="6";v=D6};D_7={s="7";v=D7};D_8={s="8";v=D8};D_9={s="9";v=D9};
    IDig d={s=d.s}; IIDig d ds={s=d.s ++ BIND ++ ds.s}; PosDecimal ds=ds;
    NegDecimal ds={s="-" ++ BIND ++ ds.s};
    IFrac d x={s=d.s ++ BIND ++ "." ++ BIND ++ x.s};
}
