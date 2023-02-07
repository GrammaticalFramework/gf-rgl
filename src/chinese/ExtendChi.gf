--# -path=alltenses:../common:../abstract

concrete ExtendChi of Extend = CatChi **
  ExtendFunctor - [ProDrop,ComplDirectVS, ComplDirectVQ]
  with (Grammar=GrammarChi) ** open Prelude, ResChi in {

  lin
    ProDrop pron = pron ** {s = []} ;
    ComplDirectVS vs utt =
      AdvVP (UseV <lin V vs : V>)
            (mkAdv (":" ++ quoted utt.s)) ; -- DEFAULT complement added as Adv in quotes
    ComplDirectVQ vq utt =
      AdvVP (UseV <lin V vq : V>)
            (mkAdv (":" ++ quoted utt.s)) ; -- DEFAULT complement added as Adv in quotes

  oper
    mkAdv : Str -> Adv ;
    mkAdv str = lin Adv {s = str ; advType = ATManner ; hasDe = False} ;
  
lin GivenName, MaleSurname, FemaleSurname, PlSurname = \n -> n ;
lin FullName gn sn = {
       s = gn.s ++ sn.s
    } ;
  
};
