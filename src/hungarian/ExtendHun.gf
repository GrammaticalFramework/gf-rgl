--# -path=.:../common:../abstract

concrete ExtendHun of Extend = CatHun
  -- ** ExtendFunctor - []
  -- with (Grammar=GrammarHun)
  ** open Prelude, ResHun, NounHun in {

lin
    TPastSimple = {s = []} ** {t = Past} ;   --# notpresent

    CompoundN n1 n2 =
      n2 ** {s = \\nc => n1.s ! SgNom ++ BIND ++ n2.s ! nc} ;

} ;
