-- Seems like this is deprecated - need to use ExtendRus instead

concrete ExtraRus of ExtraRusAbs = CatRus **
  open ResRus, MorphoRus, (P=ParadigmsRus), (X = ConstructX), Prelude, Coordination, NounRus in {
  flags optimize=all ; coding=utf8 ;
lin
  nom_Prep = lin Prep {s="" ; c=Nom ; neggen=True ; hasPrep=False} ;
  obj_neg_Prep = lin Prep {s="" ; c=Gen ; neggen=False ; hasPrep=False} ;
  obj_no_Prep = lin Prep {s="" ; c=Acc ; neggen=True ; hasPrep=False} ;
  to_dat_Prep = lin Prep {s="" ; c=Dat ; neggen=False ; hasPrep=False} ;
  ins_Prep = lin Prep {s="" ; c=Ins ; neggen=False ; hasPrep=False} ;
  to2_Prep = P.mkPrep v_prep_mod Acc ;
  u_Prep = lin Prep {s="у" ; c=Gen ; neggen=False ; hasPrep=True} ;
  on_to_Prep = P.mkPrep "до" Gen ;
  on2_Prep = P.on2_Prep ;
  along_Prep = P.mkPrep "по" Loc ;
  from2_Prep = from2 ;
  about_Prep = P.mkPrep o_prep_pre_mod Pre ;
  for2_Prep = P.mkPrep "за" Gen ;

  wherefor_IAdv = ss "зачем" ;
  wherefrom_IAdv = ss "откуда" ;
  whereto_IAdv = ss "куда" ;

  -- : IQuant ;
  what_kind_of_IQuant = (adjFormsAdjective (makeAdjectiveForms "какой" "" "3b" PreferFull)) ** {
    preferShort=PreferFull ;
    g=Neut ;
    c=Nom
    } ;

  -- near deixis
  presently_Adv = P.mkAdv "теперь" ;
  therefore_Adv = P.mkAdv "поэтому" ;
  fromhere_Adv = P.mkAdv "отсюда" ;
  tohere_Adv = P.mkAdv "сюда" ;
  here2_Adv = P.mkAdv "тут" ;
  this_way_Adv = P.mkAdv "так" ;

  -- far deixis
  then_Adv = P.mkAdv "тогда" ;
  that_is_why_Adv = P.mkAdv "потому" ;
  fromthere_Adv = P.mkAdv "оттуда" ;
  tothere_Adv = P.mkAdv "туда" ;
  there2_Adv = P.mkAdv "там" ;
  that_way_Adv = P.mkAdv "этак" ;

  -- universal
  always2_Adv = P.mkAdv ["когда угодно"] ;
  from_everywhere_Adv = P.mkAdv "отовсюду" ;
  to_everywhere_Adv = P.mkAdv "повсюду" ;
  everywhere2_Adv = P.mkAdv "всюду" ;
  that_way_Adv = P.mkAdv "этак" ;
  any_way_Adv = P.mkAdv "всяко" ;

  -- negative
  never_Adv = P.mkAdv "никогда" ;
  from_nowhere_Adv = P.mkAdv "ниоткуда" ;
  to_nowhere_Adv = P.mkAdv "никуда" ;
  nowhere_Adv = P.mkAdv "нигде" ;
  nohow_Adv = P.mkAdv "никак" ;

  -- negative predicatives
  no_need_Adv = P.mkAdv "незачем" ;
  no_time_Adv = P.mkAdv "некогда" ;
  there_is_nowhere_from_Adv = P.mkAdv "неоткуда" ;
  there_is_nowhere_to_Adv = P.mkAdv "некуда" ;
  there_is_nowhere_Adv = P.mkAdv "негде" ;

  -- indefinite
  nechto_NP = lin NP nechto ;  -- "something unknown". Use Nom, Acc only
  anybody_NP = lin NP anybody ;  -- "somenoby unknown". Use Nom only

  somehow_Adv = P.mkAdv "как-нибудь";
  somehow2_Adv = P.mkAdv "как-то";
  somehow3_Adv = P.mkAdv "как-либо";
  somehow4_Adv = P.mkAdv "кое-как";

  somewhere2_Adv = P.mkAdv "где-то";
  somewhere3_Adv = P.mkAdv "где-либо";
  somewhere4_Adv = P.mkAdv "кое-где";

  somewither_Adv = P.mkAdv "куда-нибудь";
  somewither2_Adv = P.mkAdv "куда-то";
  somewither3_Adv = P.mkAdv "куда-либо";
  somewither4_Adv = P.mkAdv "кое-куда";

  from_somewhere_Adv = P.mkAdv "откуда-нибудь";
  from_somewhere2_Adv = P.mkAdv "откуда-то";
  from_somewhere3_Adv = P.mkAdv "откуда-либо";
  from_somewhere4_Adv = P.mkAdv "кое-откуда";

  elsewhen_Adv = P.mkAdv "когда-нибудь";
  elsewhen2_Adv = P.mkAdv "когда-то";
  elsewhen3_Adv = P.mkAdv "когда-либо";
  elsewhen4_Adv = P.mkAdv "кое-когда";

  for_some_reason_Adv = P.mkAdv "зачем-то";
  for_some_reason2_Adv = P.mkAdv "почему-то";
  for_any_reason_Adv = P.mkAdv "почему-либо";

  wherefor_Adv = ss "зачем" ;
  wherefrom_Adv = ss "откуда" ;
  whereto_Adv = ss "куда" ;

  not_AdA = ss "не" ;
  rather_AdA = ss "довольно" ;

  have_V3 = P.mkV3 est_ell_V nom_Prep u_Prep;
  have2_V3 = P.mkV3 est_V nom_Prep u_Prep;
  have_not_V3 = P.mkV3 net_V obj_neg_Prep u_Prep;
  be_V3 = P.mkV3 be_ell_V nom_Prep to_dat_Prep;

  rather_CAdv = X.mkCAdv "скорее" "чем" ;

  kak_Conj = P.mkConj (comma ++ "как") Sg ;

  one_and_half_Num = {s=poltora ; size=Num2_4} ;
  one_hundred_and_fifty_Num = {s=poltorasta ; size=Num5} ;

  oba_Num = {s=oba ; size=Num2_4} ;
  dvoe_Num =      {s=dvoe ; size=Num5} ;
  troe_Num =      {s=troe ; size=Num5} ;
  chetvero_Num =  {s=chetvero ; size=Num5} ;
  pjatero_Num =   {s=pjatero ; size=Num5} ;
  shestero_Num =  {s=shestero ; size=Num5} ;
  semero_Num =    {s=semero ; size=Num5} ;
  vosqmero_Num =  {s=vosqmero ; size=Num5} ;
  devjatero_Num = {s=devjatero ; size=Num5} ;
  desjatero_Num = {s=desjatero ; size=Num5} ;
  stolqko_Num = {s=stolqko ; size=Num5} ;
  skolqko_Num = {s=skolqko ; size=Num5} ;
  neskolqko_Num = {s=neskolqko ; size=Num5} ;

  -- : A -> A -> A ;
  CompoundA a1 a2 = mkCompoundA a1 "-" a2 ;

  -- : Temp -> Pol -> VPSlash -> Adv ;  -- introduce transgressive: ", делая что-то ," = "(was) (not) doing smth, "
  TransgrAsAdv temp pol vps = {
    s=embedInCommas (
      vps.adv ! Ag (GSg Neut) P3
      ++ pol.s
      ++ case temp.t of {Pres => vps.verb.prtr ; _ => vps.verb.ptr }
      ++ verbRefl vps.verb
      ++ case temp.t of {Cond => "бы" ; _ => []}
      ++ vps.dep
      ++ vps.compl1 ! pol.p ! Ag (GSg Neut) P3
      ++ vps.compl2 ! pol.p ! Ag (GSg Neut) P3
      ++ vps.c.s
      )
    } ;

  -- : Temp -> Pol -> VPSlash -> Adv ;  -- transgressive in places, where comma already exists or in the beginning
  TransgrAsAdv1 temp pol vps = {
    s=vps.adv ! Ag (GSg Neut) P3
      ++ pol.s
      ++ case temp.t of {Pres => vps.verb.prtr ; _ => vps.verb.ptr }
      ++ verbRefl vps.verb
      ++ case temp.t of {Cond => "бы" ; _ => []}
      ++ vps.dep
      ++ vps.compl1 ! pol.p ! Ag (GSg Neut) P3
      ++ vps.compl2 ! pol.p ! Ag (GSg Neut) P3
      ++ vps.c.s
      ++ endComma
    } ;

  -- : Pol -> Imp -> Utt ;         -- sleep (impolite, like immediate command)
  UttImpImm pol imp = {s = imp.s ! pol.p ! (GSg Neut)} ; -- reused otherwise unused gender

  -- : NP -> Comp ;            -- (Париж) - столица Франции
  CompNomNP np = {s=\\a=>np.s ! Nom ; adv=[] ; cop=NomCopula} ;

  -- : NP -> Adv -> Cl ;       -- у них есть дети
  ExistsNPAdv np adv = {
    subj=[] ;
    adv=adv.s ;
    verb=copulaFull ;
    dep=[] ;
    compl=table {
      Pos => np.s ! Nom ;
      Neg => np.s ! Gen
      } ;
    a=np.a
    } ;

oper
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
    ppps="явленн";  --*
    pppss="явлен";  --*
    prtr="существуя";  --*
    ptr="существовав";  --*
    asp=Imperfective;
    refltran = Trans ;
    -- refl=NonReflexive;
    -- tran=Transitive
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
    ppps=["не явленн"];  --*
    pppss=["не явлен"];  --*
    prtr=["не существуя"];  --*
    ptr=["не существовав"];  --*
    asp=Imperfective;
    refltran = Trans ;
    -- refl=NonReflexive;
    -- tran=Transitive
  } ;
}
