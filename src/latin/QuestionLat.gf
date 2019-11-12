concrete QuestionLat of Question = CatLat ** open ResLat, IrregLat, Prelude in {

--  flags optimize=all_subs ;
  --`
  lin
    --   QuestCl : Cl -> QCl ;            -- does John walk
    QuestCl cl = cl ** {
      v = \\t,a,_,ap => cl.v ! t ! a ! VQTrue ! ap ;
      q = ""
      } ;
    
    --  QuestVP     : IP -> VP -> QCl ;      -- who walks
    QuestVP ip vp =
      {
	s = \\_ => "" ;
	adv = "" ;
	neg = \\_,_ => "" ;
	o = \\_ => vp.obj ;
	q = ip.s ! Nom ;
	v = \\t,a,_,ap => vp.s ! VAct (anteriorityToVAnter a) (tenseToVTense t) ip.n P3 ! VQFalse ;
	compl = vp.compl ! Ag Masc ip.n Nom ; -- default gender masculine
	det = { s, sp = \\_ => [] ; n = ip.n } ;
      } ; 
    --   let qcl = mkQuestion { s = ip.s ! Nom } ( mkClause emptyNP vp )
    --   in {s = \\t,a,b,qd => qcl.s ! t ! a ! b ! qd} ;

    --  QuestSlash  : IP -> ClSlash -> QCl ; -- whom does John love
    -- TO FIX
    
    -- QuestSlash ip slash =
    --   mkQuestion (ss ( ip.s ! Acc) ) slash ;

    -- QuestIAdv : 	IAdv -> Cl -> QCl
    QuestIAdv iadv cl = cl ** { q = iadv.s } ;

    -- QuestIComp : IComp -> NP -> QCl ;
    QuestIComp icomp np = 
      {
	s = \\_ => "" ;
	adv = "" ;
	neg = \\_,_ => "" ;
	o = \\_ => combineNounPhrase np ! PronNonDrop ! APostN ! DPreN ! Nom ; -- Should probably not go into the object field
	q = icomp.s ;
	v = \\t,a,_,ap => esseAux.act ! VAct (anteriorityToVAnter a) (tenseToVTense t) np.n P3 ;
	det = { s , sp = \\_=> [] ; n = Sg } ; -- default number singilar
	compl = "" ;
      } ;
--
--
--    PrepIP p ip = {s = p.s ++ ip.s ! Acc} ;
--
--    AdvIP ip adv = {
--      s = \\c => ip.s ! c ++ adv.s ;
--      n = ip.n
--      } ;
-- 
--    IdetCN idet cn = {
--      s = \\c => idet.s ++ cn.s ! idet.n ! c ; 
--      n = idet.n
--      } ;
--
--    IdetIP idet = {
--      s = \\c => idet.s ; 
--      n = idet.n
--      } ;
--
--    IdetQuant idet num = {
--      s = idet.s ! num.n ++ num.s ; 
--      n = num.n
--      } ;
--
    CompIAdv a = a ;
--    CompIP p = ss (p.s ! Nom) ;
--
}
