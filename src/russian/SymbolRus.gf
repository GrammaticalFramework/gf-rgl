--# -path=.:../abstract:../common:../prelude

concrete SymbolRus of Symbol = CatRus ** open Prelude, ResRus in {
flags coding=utf8;

lin
  -- : Symb -> PN ;                -- x
  SymbPN i = (immutableNounForms i.s Neut Inanimate) ** {g = Neut; a = Inanimate} ;

  -- : Int -> PN ;                 -- 27
  IntPN i = (immutableNounForms i.s Neut Inanimate) ** {g = Neut; a = Inanimate} ;
  -- : Float -> PN ;               -- 3.14159
  FloatPN i = (immutableNounForms i.s Neut Inanimate) ** {g = Neut; a = Inanimate} ;
  -- : Card -> PN ;                -- twelve [as proper name]  -- TODO: implement properly
  NumPN card = (immutableNounForms (card.s ! Neut ! Inanimate ! Nom) Neut Inanimate) ** {g = Neut; a = Inanimate} ;

  -- : CN -> Card -> NP ;          -- level five ; level 5
  CNNumNP cn card = {
    s = \\cas => cn.s ! Sg ! cas ++ card.s ! cn.g ! cn.anim ! cas ;
    pron=False ;
    a = Ag (gennum cn.g (forceMaybeNum cn.mayben Sg)) P3
    } ;

  -- : Det -> CN -> [Symb] -> NP ; -- (the) (2) numbers x and
  CNSymbNP det cn xs = {
    s=\\cas => det.s ! cn.g ! cn.anim ! cas
      ++ cn.s ! animNumSizeNum cn.anim cas det.size ! numSizeCase cas det.size
      ++ xs.s ;
    pron=False ;
    a=Ag (gennum cn.g (forceMaybeNum cn.mayben ((numSizeNumber det.size)))) P3
		} ;

  -- : Symb -> S ;                 -- A
  SymbS symb = {s=\\m=>symb.s};

  -- : Symb -> Card ;              -- n
  SymbNum symb = {s = \\_,_,_ => symb.s ; size = Num5} ;

  -- : Symb -> Ord ;               -- n'th  (i-й "итый")
  SymbOrd symb = ith_forms symb.s ;

lincat
  Symb, [Symb] = SS ;

lin
  MkSymb s = s ;
  BaseSymb = infixSS "и" ;
  ConsSymb = infixSS "," ;
}
