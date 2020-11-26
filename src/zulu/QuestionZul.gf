concrete QuestionZul of Question = CatZul ** open ResZul, Prelude, ParamX in {

  flags optimize=all_subs ;

  lin

    QuestCl cl = {
      s = \\p,t,m => cl.s!p!t!m ;
      potqcl = cl.potcl ;
      qword_pre = [] ;
      qword_post = "na"
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
      s = \\p,t,m =>
      let
        -- TODO: deal with auxiliaries properly.
        aux_tense = case t of {
          Absolute bt => bt ;
          Relative b1 b2 => b1
        } ;
        main_tense = case t of {
          Absolute bt => bt ;
          Relative b1 b2 => b2
        } ;
        vform = VFIndic m p main_tense Null ;
        vform_aux = VFIndic m p aux_tense Null ;
        neg1 = icompNeg1 vform ;
        neg2 = icompNeg2 vform ;
        aux = case t of {
          Absolute bt => [] ;
          Relative _ _ => (subjConcLookup!np.agr!ResZul.SC) ++BIND++ "b" ++BIND++ (vtermSuff vform_aux False "e") -- relSubjConc aux_tense np.agr --
        }
      in
          aux ++ neg1 ++ (icompSubjConc vform np.agr) ++ neg2 ++ icomp.s ++ np.s!Full ;
      potqcl = \\p,m =>
      let
        vform = VFPot m p Null ;
        neg = icompNeg2 vform
      in
          (icompSubjConc vform np.agr) ++ neg ++ icomp.s ++ np.s!Full ;
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
    qcl_iadv : Cl -> CatZul.IAdv -> {s : Polarity => ZTense => DMood => Str ; potqcl : Polarity => DMood => Str ; qword_pre : Str ; qword_post : Str } = \cl,iadv -> {
      s = case iadv.postIAdv of {
        False => \\p,t,m => cl.s!p!t!m ++ iadv.s ++ cl.advs ;
        True => \\p,t,m => cl.s!p!t!m ++ cl.advs
      } ;
      potqcl = cl.potcl ;
      qword_pre = case iadv.postIAdv of {
        False => [] ;
        True => iadv.s
        } ;
      qword_post = []
    } ;

}
