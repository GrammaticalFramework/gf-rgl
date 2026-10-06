concrete QuestionSco of Question = QuestionEng-[QuestVP,QuestIComp,QuestQVP] ** open ResSco in {

lin QuestVP qp vp =
      let cl = mkClause (qp.s ! npNom) (agrP3 qp.n) vp
      in {s = \\t,a,b,_ => cl.s ! t ! a ! b ! oDir} ;

    QuestIComp icomp np =
      mkQuestion icomp (mkClause (np.s ! npNom) np.a (predAux auxBe)) ;

    QuestQVP qp vp =
      let cl = mkClause (qp.s ! npNom) (agrP3 qp.n) vp
      in {s = \\t,a,b,_ => cl.s ! t ! a ! b ! oDir} ;

}
