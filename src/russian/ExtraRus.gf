-- Seems like this is deprecated - need to use ExtendRus instead

concrete ExtraRus of ExtraRusAbs = CatRus **
  open ResRus, MorphoRus, (P=ParadigmsRus), Prelude, NounRus in {
  flags optimize=all ; coding=utf8 ;
lin
  obj_no_Prep = {s="" ; c=Acc ; neggen=True ; hasPrep=False} ;
  to2_Prep = P.mkPrep v_prep_mod Acc ;
  to_dat_Prep = {s="" ; c=Dat ; neggen=False ; hasPrep=False} ;
  on_to_Prep = P.mkPrep "до" Gen ;
  along_Prep = P.mkPrep "по" Loc ;
  from2_Prep = from2 ;
  about_Prep = P.mkPrep o_prep_pre_mod Pre ;

  have_V3 = P.mkV3 est_ell_V nom_Prep u_Prep;
  have2_V3 = P.mkV3 est_V nom_Prep u_Prep;
  have_not_V3 = P.mkV3 net_V obj_neg_Prep u_Prep;
  be_V3 = P.mkV3 be_ell_V nom_Prep dat_Prep;

oper
  nom_Prep : Prep = lin Prep {s="" ; c=Nom ; neggen=True ; hasPrep=False} ;
  obj_neg_Prep : Prep = lin Prep {s="" ; c=Gen ; neggen=False ; hasPrep=False} ;
  dat_Prep : Prep = lin Prep {s="" ; c=Dat ; neggen=False ; hasPrep=False} ;
  u_Prep : Prep = lin Prep {s="у" ; c=Gen ; neggen=False ; hasPrep=True} ;
  est_V : V = lin V {
    inf="есть";
    infrefl="" ;
    prsg1, prsg2, prsg3, prpl1, prpl2, prpl3="есть";
    fut=BeFuture ;
    psgm="был";
    psgs="бы";
    isg2="";
    isg2refl="" ;
    ipl1="";
    pppss="";
    prtr="";
    ptr="";
    asp=Imperfective;
    refl=NonReflexive;
    tran=Transitive
  } ;
  est_ell_V = est_V ** {prsg1, prsg2, prsg3, prpl1, prpl2, prpl3=""} ;
  be_ell_V = est_ell_V ** {inf=""} ;
  net_V : V = lin V {
    inf="нет";
    infrefl="" ;
    prsg1, prsg2, prsg3, prpl1, prpl2, prpl3="нет";
    fut=BeFuture ;
    psgm="не был";
    psgs="не бы";
    isg2="";
    isg2refl="" ;
    ipl1="";
    pppss="";
    prtr="";
    ptr="";
    asp=Imperfective;
    refl=NonReflexive;
    tran=Transitive
  } ;
}