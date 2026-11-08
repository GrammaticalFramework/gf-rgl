concrete VerbSlv of Verb = CatSlv ** open ResSlv, ParamX, ParadigmsSlv, Prelude in {

  lin
    UseV v = 
      { s  = \\p,vform => ne ! p ++ v.s ! vform ;
        s2 = \\a => v.p ;  ----AR: +p particle
        isCop = False ;
        refl = v.refl
      } ;

    ComplVV v vp = {
      s = \\p,vf => ne ! p ++ v.s ! vf;
      s2 = \\a => vp.s ! Pos ! VInf ++ vp.refl ++ vp.s2 ! a;
      isCop=False; refl=[]
    } ;
    ComplVS v s = {
      s = \\p,vf => ne ! p ++ v.s ! vf;
      s2 = \\_ => v.p ++ "da" ++ s.s; isCop=False; refl=v.refl
    } ;
    ComplVQ v q = {
      s = \\p,vf => ne ! p ++ v.s ! vf;
      s2 = \\_ => q.s; isCop=False; refl=[]
    } ;
    ComplVA v ap = {
      s = \\p,vf => ne ! p ++ v.s ! vf;
      s2 = \\a => v.p ++ ap.s ! Indef ! inanimateGender a.g ! Nom ! a.n;
      isCop=False; refl=v.refl
    } ;

    SlashV2a v = 
      { s  = \\p,vform => ne ! p ++ v.s ! vform ;
        s2 = \\a => v.p ;  ----AR: +p particle
        c2 = v.c2 ;
        isCop = False ;
        refl = v.refl
      } ;

    SlashV2V v vp = {
      s=\\p,vf => ne ! p ++ v.s ! vf;
      s2=\\a => v.p ++ vp.s ! Pos ! VInf ++ vp.refl ++ vp.s2 ! a;
      c2=mkPrep [] accusative; isCop=False; refl=v.refl
    } ;
    SlashV2S v s = {
      s=\\p,vf => ne ! p ++ v.s ! vf; s2=\\_ => v.p ++ "da" ++ s.s;
      c2=mkPrep [] accusative; isCop=False; refl=v.refl
    } ;
    SlashV2Q v q = {
      s=\\p,vf => ne ! p ++ v.s ! vf; s2=\\_ => v.p ++ q.s;
      c2=mkPrep [] accusative; isCop=False; refl=v.refl
    } ;
    SlashV2A v ap = {
      s=\\p,vf => ne ! p ++ v.s ! vf;
      s2=\\a => v.p ++ ap.s ! Indef ! inanimateGender a.g ! Acc ! a.n;
      c2=mkPrep [] accusative; isCop=False; refl=v.refl
    } ;

    --Check these V3-slashes AE
    Slash2V3 v np =
      { s  = \\p,vform => ne ! p ++ v.s ! vform ;
        s2 = \\_ => v.p ++ v.c2.s ++ np.s ! v.c2.c ;  
        c2 = v.c3 ;
        isCop = False ;
        refl = v.refl
      } ;

    Slash3V3 v np =
      { s  = \\p,vform => ne ! p ++ v.s ! vform ;
        s2 = \\_ => v.p ++ v.c3.s ++ np.s ! v.c3.c ;  
        c2 = v.c2 ;
        isCop = False ;
        refl = v.refl
      } ;

    ComplSlash vp np =
      insertObj (\\_ => vp.c2.s ++ np.s ! vp.c2.c) vp;


    ReflVP vp = {
      s     =  \\vform => vp.s ! vform ; --? the compiler told me to cut the polarity from this function?
      --s     =  \\p,vform => ne ! p ++ vp.s ! vform ;
      s2    = \\a => vp.s2 ! a ;
      isCop = False ;
      refl = reflexive ! vp.c2.c 
    } ;

    UseComp comp = {
      s     = copula ;
      s2    = comp.s ;
      isCop = True ;
      refl = []  
    } ;

    AdvVP vp adv = insertObj (\\_ => adv.s) vp ;
    ExtAdvVP vp adv = AdvVP vp adv ;
    AdVVP adv vp = vp ** {s2 = \\a => adv.s ++ vp.s2 ! a} ;
    AdvVPSlash vp adv = vp ** {s2 = \\a => vp.s2 ! a ++ adv.s} ;
    AdVVPSlash adv vp = vp ** {s2 = \\a => adv.s ++ vp.s2 ! a} ;
    VPSlashPrep vp prep = vp ** {c2=prep} ;

    CompAP ap = {
      s = \\agr => ap.s ! Indef ! inanimateGender agr.g ! Nom ! agr.n
      } ;
      
    CompAdv adv = {s = \\agr => adv.s} ; ----AR
    CompNP np = {s = \\agr => np.s ! Nom} ; ----AR
    CompCN cn = {s = \\agr => cn.s ! Indef ! Nom ! agr.n} ;
    UseCopula = {s=copula;s2=\\_=>[];isCop=True;refl=[]} ;

}
