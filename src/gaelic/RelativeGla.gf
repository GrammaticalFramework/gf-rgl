concrete RelativeGla of Relative = CatGla ** open ResGla, Prelude in {
lin
  RelCl cl = {s = \\t,a,p => "a" ++ relativeClause t a p cl} ;
  RelVP rp vp = {s = \\t,a,p => rp.s ++ relativeClause t a p {
    subj = [] ; n = Sg ; pred = vp
    }} ;
  RelSlash rp cls = {s = \\t,a,p => rp.s ++ relativeClause t a p cls} ;
  IdRP = {s = "a"} ;
  FunRP prep np rp = {s = prepNP prep np ++ rp.s} ;

oper
  relativeClause : GlaTense -> GlaAnteriority -> GlaPolarity -> LinCl -> Str =
    \t,a,p,cl -> case <t,a,p,cl.pred.copular> of {
      <GPres,GSimul,GPos,True> => "tha" ++ cl.subj ++ cl.pred.complement ;
      <GPres,GSimul,GPos,False> => "tha" ++ cl.subj ++ "a'" ++ cl.pred.noun ;
      <GPres,GAnter,GPos,_> => "tha" ++ cl.subj ++ cl.pred.participle ;
      <GPast,GSimul,GPos,True> => "bha" ++ cl.subj ++ cl.pred.complement ;
      <GPast,GSimul,GPos,False> => "rinn" ++ cl.subj ++ cl.pred.noun ;
      <GPast,GAnter,GPos,_> => "bha" ++ cl.subj ++ cl.pred.participle ;
      <GFut,_,GPos,True> => "bhios" ++ cl.subj ++ cl.pred.complement ;
      <GFut,_,GPos,False> => "bhios" ++ cl.subj ++ "a'" ++ cl.pred.noun ;
      <GCond,_,GPos,True> => "bhiodh" ++ cl.subj ++ cl.pred.complement ;
      <GCond,_,GPos,False> => "bhiodh" ++ cl.subj ++ "a'" ++ cl.pred.noun ;
      <GPres,GSimul,GNeg,True> => "nach eil" ++ cl.subj ++ cl.pred.complement ;
      <GPres,GSimul,GNeg,False> => "nach eil" ++ cl.subj ++ "a'" ++ cl.pred.noun ;
      <GPres,GAnter,GNeg,_> => "nach eil" ++ cl.subj ++ cl.pred.participle ;
      <GPast,GSimul,GNeg,True> => "nach robh" ++ cl.subj ++ cl.pred.complement ;
      <GPast,GSimul,GNeg,False> => "nach do rinn" ++ cl.subj ++ cl.pred.noun ;
      <GPast,GAnter,GNeg,_> => "nach robh" ++ cl.subj ++ cl.pred.participle ;
      <GFut,_,GNeg,True> => "nach bi" ++ cl.subj ++ cl.pred.complement ;
      <GFut,_,GNeg,False> => "nach bi" ++ cl.subj ++ "a'" ++ cl.pred.noun ;
      <GCond,_,GNeg,True> => "nach biodh" ++ cl.subj ++ cl.pred.complement ;
      <GCond,_,GNeg,False> => "nach biodh" ++ cl.subj ++ "a'" ++ cl.pred.noun
      } ;
}
