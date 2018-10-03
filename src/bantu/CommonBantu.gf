----1 Auxiliary operations common for Bantu languages
--
-- This module contains operations that are shared by the Bantu
-- languages. The complete set of auxiliary operations needed to
-- implement [Test Test.html] is defined in [ResBantu ResBantu.html],
-- which depends on [DiffBantu DiffBantu.html].

resource CommonBantu = ParamX ** open Prelude in {

  flags optimize=all ;
    coding=utf8 ;

  param
    Case = Nom | Loc ;
    NPCase = NCase Case | NPAcc | NPNomPoss ;
    CardOrd = NCard | NOrd ;

  oper
    ---- Conjunction Agreements (as fas as indep. of Gender) ----
    conjPPerson : Person -> Person -> Person = \p,q ->
    case <p,q> of {
      <_,P1>  | <_,P2>  =>  P1 ;
      <P1,P3>  => P1 ;
      <P2,P3>  => P2 ;
      <P3,P3>  => P3 
     };

    artIndef = "" ;

}
