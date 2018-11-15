--# -path=.:../abstract:../common

concrete SymbolAra of Symbol = CatAra ** open Prelude, ResAra in {


lin
  SymbPN i = {s = \\c => i.s ; g = Masc ; h = NoHum } ; --IL
  IntPN i  = {s = \\c => i.s ; g = Masc ; h = NoHum } ; --IL
  FloatPN i  = {s = \\c => i.s ; g = Masc ; h = NoHum } ; --IL
  NumPN i  = {s = \\c => uttNum i ! Masc ; g = Masc ; h = NoHum } ; --IL
  -- CNIntNP cn i = {
  --   s = \\c => cn2str cn Sg Def c ++ uttNum i ! cn.g ;
  --   a = dummyAgrP3 Sg ;
  --   } ;
  --IL TODO: check out some opers regarding state in ResAra. These are just dummy values.
  CNSymbNP det cn xs =
    let g = cn.g ; n = sizeToNumber det.n  in {
    s = \\c => det.s ! NoHum ! g ! c ++ cn2str cn n Def c ++ xs.s; ----IL word order?? Seems to be nontrivial according to ResAra comments.
    a = dummyAgrP3 n ;
    empty = []
    } ;
  CNNumNP cn i = {
    s = \\c => cn2str cn Sg Def c ++ uttNum i ! cn.g ;
    a = dummyAgrP3 Sg ;
    empty = []
    } ;

  SymbS sy = sy ;


  SymbOrd n = {s = \\_,_,_ => n.s ; n = One ; isNum = False } ;
  SymbNum n = SymbOrd n ** { n = ThreeTen ; isNum = True } ; ----IL

lincat

  Symb, [Symb] = SS ;

lin

  MkSymb s = s ;

  BaseSymb = infixSS "und" ; ----
  ConsSymb = infixSS "," ;

oper

  dummyAgrP3 : Number -> Agr = \n ->
   { pgn = Per3 Masc n ; isPron = False } ;

}
