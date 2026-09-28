concrete QuestionGla of Question = CatGla ** open
  Prelude, ResGla, ParadigmsGla in {

lincat QVP = SS ;

lin
  IdetCN idet cn = {s = idet.s ++ cn.s ! NOM ! Indef ! Pl} ;
  IdetIP idet = idet ;
  IdetQuant iquant num = {s = iquant.s ++ num.s} ;
  QuestSlash ip cls = {s = \\t,a,p => ip.s ++ "a" ++ whClause t a p cls} ;
  QuestCl cl = {s = \\t,a,p => polarClause t a p cl} ;
  QuestVP ip vp = {s = \\t,a,p => ip.s ++ "a" ++ whClause t a p {
    subj = [] ; n = Sg ; pred = vp
    }} ;
  QuestIAdv iadv cls = {s = \\t,a,p => iadv.s ++ whClause t a p cls} ;
  CompIP ip = ip ;
  QuestIComp icomp np = {s = \\t,a,p => icomp.s ++ polarClause t a p {
    subj = linNP np ; n = agrNumber np.a ; pred = questionBiV
    }} ;
  AdvIP ip adv = {s = ip.s ++ adv.s} ;
  PrepIP prep ip = {s = prep.s ! PrepBase ++ ip.s} ;
  AdvIAdv iadv adv = {s = iadv.s ++ adv.s} ;
  CompIAdv iadv = iadv ;
  ComplSlashIP vps ip = {s = vps.s ++ ip.s} ;
  AdvQVP vp iadv = {s = vp.s ++ iadv.s} ;
  AddAdvQVP qvp iadv = {s = qvp.s ++ iadv.s} ;
  QuestQVP ip qvp = {s = \\_,_,_ => ip.s ++ qvp.s} ;

oper
  questionBiV : LinV = {
    s = "bi" ; conditional = \\_ => "bhiodh" ;
    imperative = \\_,_ => "bi" ; future = \\_ => "bidh" ;
    past = \\_ => "bha" ; noun = "bhith" ; participle = "air a bhith" ;
    copular = True ; complement = []
    } ;

  polarClause : GlaTense -> GlaAnteriority -> GlaPolarity -> LinCl -> Str =
    \t,a,p,cl -> case <t,a,p,cl.pred.copular> of {
      <GPres,GSimul,GPos,True> => "a bheil" ++ cl.subj ++ cl.pred.complement ;
      <GPres,GSimul,GPos,False> => "a bheil" ++ cl.subj ++ AG ++ cl.pred.noun ;
      <GPres,GAnter,GPos,_> => "a bheil" ++ cl.subj ++ cl.pred.participle ;
      <GPast,GSimul,GPos,True> => "an robh" ++ cl.subj ++ cl.pred.complement ;
      <GPast,GSimul,GPos,False> => "an do rinn" ++ cl.subj ++ cl.pred.noun ;
      <GPast,GAnter,GPos,_> => "an robh" ++ cl.subj ++ cl.pred.participle ;
      <GFut,_,GPos,True> => "am bi" ++ cl.subj ++ cl.pred.complement ;
      <GFut,_,GPos,False> => "am bi" ++ cl.subj ++ AG ++ cl.pred.noun ;
      <GCond,_,GPos,True> => "am biodh" ++ cl.subj ++ cl.pred.complement ;
      <GCond,_,GPos,False> => "am biodh" ++ cl.subj ++ AG ++ cl.pred.noun ;
      <GPres,GSimul,GNeg,True> => "nach eil" ++ cl.subj ++ cl.pred.complement ;
      <GPres,GSimul,GNeg,False> => "nach eil" ++ cl.subj ++ AG ++ cl.pred.noun ;
      <GPres,GAnter,GNeg,_> => "nach eil" ++ cl.subj ++ cl.pred.participle ;
      <GPast,GSimul,GNeg,True> => "nach robh" ++ cl.subj ++ cl.pred.complement ;
      <GPast,GSimul,GNeg,False> => "nach do rinn" ++ cl.subj ++ cl.pred.noun ;
      <GPast,GAnter,GNeg,_> => "nach robh" ++ cl.subj ++ cl.pred.participle ;
      <GFut,_,GNeg,True> => "nach bi" ++ cl.subj ++ cl.pred.complement ;
      <GFut,_,GNeg,False> => "nach bi" ++ cl.subj ++ AG ++ cl.pred.noun ;
      <GCond,_,GNeg,True> => "nach biodh" ++ cl.subj ++ cl.pred.complement ;
      <GCond,_,GNeg,False> => "nach biodh" ++ cl.subj ++ AG ++ cl.pred.noun
      } ;

  whClause : GlaTense -> GlaAnteriority -> GlaPolarity -> LinCl -> Str =
    \t,a,p,cl -> case <t,a,p,cl.pred.copular> of {
      <GPres,GSimul,GPos,True> => "tha" ++ cl.subj ++ cl.pred.complement ;
      <GPres,GSimul,GPos,False> => "tha" ++ cl.subj ++ AG ++ cl.pred.noun ;
      <GPres,GAnter,GPos,_> => "tha" ++ cl.subj ++ cl.pred.participle ;
      <GPast,GSimul,GPos,True> => "bha" ++ cl.subj ++ cl.pred.complement ;
      <GPast,GSimul,GPos,False> => "rinn" ++ cl.subj ++ cl.pred.noun ;
      <GPast,GAnter,GPos,_> => "bha" ++ cl.subj ++ cl.pred.participle ;
      <GFut,_,GPos,True> => "bhios" ++ cl.subj ++ cl.pred.complement ;
      <GFut,_,GPos,False> => "bhios" ++ cl.subj ++ AG ++ cl.pred.noun ;
      <GCond,_,GPos,True> => "bhiodh" ++ cl.subj ++ cl.pred.complement ;
      <GCond,_,GPos,False> => "bhiodh" ++ cl.subj ++ AG ++ cl.pred.noun ;
      <GPres,GSimul,GNeg,True> => "nach eil" ++ cl.subj ++ cl.pred.complement ;
      <GPres,GSimul,GNeg,False> => "nach eil" ++ cl.subj ++ AG ++ cl.pred.noun ;
      <GPres,GAnter,GNeg,_> => "nach eil" ++ cl.subj ++ cl.pred.participle ;
      <GPast,GSimul,GNeg,True> => "nach robh" ++ cl.subj ++ cl.pred.complement ;
      <GPast,GSimul,GNeg,False> => "nach do rinn" ++ cl.subj ++ cl.pred.noun ;
      <GPast,GAnter,GNeg,_> => "nach robh" ++ cl.subj ++ cl.pred.participle ;
      <GFut,_,GNeg,True> => "nach bi" ++ cl.subj ++ cl.pred.complement ;
      <GFut,_,GNeg,False> => "nach bi" ++ cl.subj ++ AG ++ cl.pred.noun ;
      <GCond,_,GNeg,True> => "nach biodh" ++ cl.subj ++ cl.pred.complement ;
      <GCond,_,GNeg,False> => "nach biodh" ++ cl.subj ++ AG ++ cl.pred.noun
      } ;
}
