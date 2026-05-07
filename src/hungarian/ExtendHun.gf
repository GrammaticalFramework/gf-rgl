--# -path=.:../common:../abstract

concrete ExtendHun of Extend = CatHun
  -- ** ExtendFunctor - []
  -- with (Grammar=GrammarHun)
  ** open Prelude, ResHun, NounHun in {

lin
    TPastSimple = {s = []} ** {t = Past} ;   --# notpresent

    CompoundN n1 n2 =
      n2 ** {s = \\nc => n1.s ! SgNom ++ BIND ++ n2.s ! nc} ;

    UseDAP = DetNP ;
    UseDAPMasc,
    UseDAPFem = \dap -> DetNP dap ** {g = Human} ;

    iFem_Pron = pronTable ! <P1,Sg> ** {g = Human} ;
    theyFem_Pron = pronTable ! <P3,Pl> ** {g = Human} ;
    theyNeutr_Pron = pronTable ! <P3,Pl> ;
    weFem_Pron = pronTable ! <P1,Pl> ** {g = Human} ;
    youFem_Pron = pronTable ! <P2,Sg> ** {g = Human} ;
    youPlFem_Pron = pronTable ! <P2,Pl> ** {g = Human} ;
    youPolFem_Pron = pronTable ! <P2,Sg> ** {g = Human} ;
    youPolPlFem_Pron = pronTable ! <P2,Pl> ** {g = Human} ;
    youPolPl_Pron = pronTable ! <P2,Pl> ** {g = Human} ;

} ;
