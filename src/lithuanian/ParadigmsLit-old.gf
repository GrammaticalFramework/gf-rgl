--# -path=.:../abstract:../prelude:../common

-- File not used...

-- Adam Slaski, 2011
-- Inari Listenmaa, 2020

   resource ParadigmsLit = open
     CatLit, MorphoLit, ResLit
  in
     {
  flags  coding=utf8;

  oper

    ComplCase : Type ;
    genC : ComplCase ;
    datC : ComplCase ;

    mkA2 : A -> Str -> ComplCase -> A2 ;


--.
-- Definitions hidden from the public API

  ComplCase = ResLit.ComplCase ;
  genC = GenC ;
  datC = DatC ;
  datC = DatC ;

  mkPN : Str -> (Str -> SubstForm => Str) -> GenNum -> PN ;
  mkPN form tab gennum = lin PN
    { nom = (tab form)!SF Sg Nom;
      voc = (tab form)!SF Sg VocL;
      dep = let forms = (tab form) in table {
                  GenC => forms!SF Sg Gen;
                  AccC => forms!SF Sg Acc;
                  DatC => forms!SF Sg Dat;
                  InsC => forms!SF Sg Ins;
                  LocC => forms!SF Sg Loc};
      gn = gennum ;
      p  = P3
    } ;

    mkA2 adj s c = lin A2 (adj ** { c={s=s; c=c} });

}
