concrete QuestionZul of Question = CatZul ** open ResZul, Prelude, ParamX in {

  flags optimize=all_subs ;

  lin

    QuestCl cl = {
      s = \\p,t => cl.s!p!t ;
      -- potqcl = cl.potcl ;
      qword_pre = [] ;
      qword_post = variants { "na" ; [] } ;
    } ; -- guessing this will work...

  --   QuestVP qp vp =
  --     let cl = mkClause (qp.s ! npNom) (agrP3 qp.n) vp
  --     in {s = \\t,a,b,_ => cl.s ! t ! a ! b ! oDir} ; ----
  --
  --   QuestSlash ip slash =
  --     {s = \\t,a,b,q =>
  --        (mkQuestion (ss (ip.s ! NPAcc)) slash).s ! t ! a ! b ! q ++ slash.c2
  --     } ;
  --     --- changed AR 5/6/2016: uses stranding; pied-piping in ExtraZul

    QuestIAdv iadv cl = qcl_iadv cl iadv ;

    QuestIComp icomp np = {
      s = \\p,t =>
      let
        vform = VFIndic MainCl p t ;
        pre_icomp = case icomp.postIComp of {
          False => (icomp_pref vform np.agr) ++ icomp.s ;
          True => []
        } ;
        post_icomp = case icomp.postIComp of {
          True => (icomp_pref vform np.agr) ++ icomp.s ;
          False => []
        }
      in
          pre_icomp ++ (np.s!NFull) ++ post_icomp ;
      qword_pre = [] ;
      qword_post = []
    } ;
      -- mkQuestion icomp (mkClause (np.s ! npNom) np.a (predAux auxBe)) ;


  --   PrepIP p ip = {s = p.s ++ ip.s ! NPAcc} ;
  --
  --   AdvIP ip adv = {
  --     s = \\c => ip.s ! c ++ adv.s ;
  --     n = ip.n
  --     } ;
  --
  --   IdetCN idet cn = {
  --     s = \\c => idet.s ++ cn.s ! idet.n ! npcase2case c ;
  --     n = idet.n
  --     } ;
  --
  --   IdetIP idet = {
  --     s = \\c => idet.s ;
  --     n = idet.n
  --     } ;
  --
  --   IdetQuant idet num = {
  --     s = idet.s ! num.n ++ num.s ! False ! Nom ;
  --     n = num.n
  --     } ;
  --
  --   AdvIAdv i a = ss (i.s ++ a.s) ;
  --
  --   CompIAdv a = a ;
  --   CompIP p = ss (p.s ! npNom) ;
  --
  -- lincat
  --   QVP = ResZul.VP ;
  -- lin
  --   ComplSlashIP vp np = insertObjPre (\\_ => vp.c2 ++ np.s ! NPAcc) vp ;
  --   AdvQVP vp adv = insertObj (\\_ => adv.s) vp ;
  --   AddAdvQVP vp adv = insertObj (\\_ => adv.s) vp ;
  --
  --   QuestQVP qp vp =
  --     let cl = mkClause (qp.s ! npNom) (agrP3 qp.n) vp
  --     in {s = \\t,a,b,_ => cl.s ! t ! a ! b ! oDir} ; ----

  oper
    -- qcl_iadv : Cl -> CatZul.IAdv -> {s : Polarity => ZTense => DMood => Str ; potqcl : Polarity => DMood => Str ; qword_pre : Str ; qword_post : Str } = \cl,iadv -> {
    --   s = case iadv.postIAdv of {
    --     False => \\p,t,m => cl.s!p!t!m ++ iadv.s ++ cl.advs ;
    --     True => \\p,t,m => cl.s!p!t!m ++ cl.advs
    --   } ;
    --   potqcl = cl.potcl ;
    --   qword_pre = case iadv.postIAdv of {
    --     False => [] ;
    --     True => iadv.s
    --     } ;
    --   qword_post = []
    -- } ;

    qcl_iadv : Cl -> CatZul.IAdv -> {s : Polarity => BasicTense => Str ; qword_pre : Str ; qword_post : Str } = \cl,iadv -> {
      s = \\p,t => cl.s!p!t ;
      qword_pre = case iadv.postIAdv of {
        True => [] ;
        False => iadv.s
        } ;
      qword_post = case iadv.postIAdv of {
        False => [] ;
        True => iadv.s
        } ;
    } ;

}
