--# -path=.:../abstract:../common:../prelude
--# -coding=utf8

concrete SymbolBul of Symbol = CatBul ** open Prelude, ResBul in {

flags
  coding = utf8 ;

lin
  SymbPN i = {s = i.s ; g = Neut} ;
  IntPN i  = {s = i.s ; g = Neut} ;
  FloatPN i = {s = i.s ; g = Neut} ;
  NumPN i = {s = i.s ! CFNeut Indef ; g = Neut} ;
  CNIntNP cn i = {
    s  = \\c => cn.s ! NF Sg Indef ++ i.s ;
    gn = gennum cn.g Sg ;
    p  = NounP3 Pos
    } ;
  CNSymbNP det cn xs = {
    s  = \\c => det.s ! False ! cn.g ! RSubj ++ cn.s ! NF (numnnum det.nn) Indef ++ xs.s ; 
    gn = gennum cn.g (numnnum det.nn) ;
    p  = NounP3 Pos
    } ;
  CNNumNP cn i = {
    s  = \\c => (cn.s ! NF Sg Indef ++ i.s ! CFNeut Indef) ;
    gn = gennum cn.g Sg ;
    p  = NounP3 Pos
    } ;

  SymbS sy = sy ; 

  SymbNum sy = {s = \\_ => sy.s; nn = NNum Pl} ;
  SymbOrd sy = {s = \\aform => sy.s ++ "-" ++ 
                               case aform of {
                                 ASg Masc Indef => "ти" ;
                                 ASg Fem  Indef => "та" ;
                                 ASg Neut Indef => "то" ;
                                 ASg Masc Def   => "тия" ;
                                 ASg Fem  Def   => "тата" ;
                                 ASg Neut Def   => "тото" ;
                                 ASgMascDefNom  => "тият" ;
                                 APl Indef      => "ти" ;
                                 APl Def        => "тите"
                               }
                } ;

lincat 

  Symb, [Symb] = SS ;

lin

  MkSymb s = s ;

  BaseSymb = infixSS "и" ;
  ConsSymb = infixSS bindComma ;

}
