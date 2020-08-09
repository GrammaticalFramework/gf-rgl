--# -path=.:../abstract:../common:../../prelude

concrete QuestionRus of Question = CatRus ** open ResRus, Prelude in {
flags optimize=all_subs ; coding=utf8 ;

lin
  -- : Cl -> QCl ;            -- does John walk
  QuestCl cl = cl ** {qf=QDir} ;

  -- : IP -> VP -> QCl ;      -- who walks
  QuestVP ip vp = {
    subj=ip.nom ;
    adv=vp.adv ! ip.a ;
    verb=vp.verb ;
    dep=vp.dep ;
    compl=\\p => vp.compl ! p ! ip.a ;  --???
    a=ip.a
    } ;

  -- : IP -> ClSlash -> QCl ; -- whom does John love
  QuestSlash ip cls = cls ** {
    subj=(applyIPronPrep cls.c ip) ++ cls.subj ;
    a=cls.a
    } ;

  -- : IAdv -> Cl -> QCl ;    -- why does John walk
  QuestIAdv iadv cl = cl ** {
    subj=iadv.s ++ cl.subj
    } ;

  -- : IComp -> NP -> QCl ;   -- where is John
  QuestIComp icomp np = {
    subj=icomp.s ! Ag (GSg Neut) P3 ;  --???
    compl=table {
      Pos => np.s ! Nom ;  --???
      Neg => np.s ! Gen    -- TODO: Check!
      };
    adv=icomp.adv ;
    verb=selectCopula icomp.cop ;
    dep=[] ;
    a=np.a
    } ;

  -- : Prep -> IP -> IAdv ;  -- with whom
  PrepIP prep ip = {s=applyIPronPrep prep ip} ;

  -- : IP -> Adv -> IP
  AdvIP ip adv = appendToIP (ip ** {a=ip.a}) adv.s ;

  -- : IAdv -> Adv -> IAdv ;    -- where in Paris
  AdvIAdv = cc2 ;

  -- : IAdv -> IComp ;          -- where (is it)
  CompIAdv iadv = {
    s=\\a=>[] ;
    adv=iadv.s ;
    cop=EllCopula   -- ???
  } ;

  -- : IP -> IComp ;          -- who (is it)
  CompIP ip = {
    s=\\a=>ip.nom ;   -- ???
    adv=[] ;
    cop=EllCopula   -- ???
  } ;

  -- : IDet -> CN -> IP ;       -- which five songs
  IdetCN idet cn = caseTableToRecord (\\cas => idet.s ! cn.g ! cn.anim ! cas
      ++ cn.s ! animNumSizeNum cn.anim cas idet.size ! numSizeCase cas idet.size)
    (Ag (gennum cn.g (forceMaybeNum cn.mayben (numSizeNumber idet.size))) P3) cn.anim ;

  -- : IDet -> IP ;       -- which five
  IdetIP idet = caseTableToRecord (\\cas => idet.s ! idet.g ! Inanimate ! cas)
    (Ag (gennum idet.g (numSizeNumber idet.size)) P3) Inanimate ;

  -- : IQuant -> Num -> IDet ;  -- which (five)
  IdetQuant iq num = {
    s=\\g,a,cas => iq.s ! gennum g (numSizeNumber num.size) ! a ! cas ++ num.s ! iq.g ! a ! cas ;
    size=num.size ;
    g=iq.g ;
    c=iq.c
    } ;

}
