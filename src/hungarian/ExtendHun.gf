--# -path=.:../common:../abstract

concrete ExtendHun of Extend = CatHun
  -- ** ExtendFunctor - []
  -- with (Grammar=GrammarHun)
  ** open Prelude, ResHun, NounHun in {

lin
    TPastSimple = {s = []} ** {t = Past} ;   --# notpresent

    CompoundN n1 n2 =
      n2 ** {s = \\nc => n1.s ! SgNom ++ BIND ++ n2.s ! nc} ;

    GenModNP num np cn =
      let det : Determiner = DetQuant DefArt num ;
          pron : Pronoun = pronTable ! np.agr ;
       in emptyNP ** cn ** det ** {
            s = \\_,c =>
              np.s ! NoPoss ! Nom
              ++ np.postmod
              ++ caseFromPossStem cn (DetQuant (PossPron pron) num) c
              ++ cn.compl ! det.n ! c ;
            agr = <P3,det.n> ;
            objdef = Def ;
            } ;

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
