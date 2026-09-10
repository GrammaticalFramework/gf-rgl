concrete VerbSco of Verb = VerbEng-[UseComp,UseCopula,PassV2] ** open ResSco in {

lin UseComp comp = insertObj comp.s (predAux auxBe) ;
    UseCopula = predAux auxBe ;
    PassV2 v = insertObj (\\_ => v.s ! VPPart ++ v.p) (predAux auxBe) ;

}
