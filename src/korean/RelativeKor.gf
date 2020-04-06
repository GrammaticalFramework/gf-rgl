concrete RelativeKor of Relative = CatKor ** open
  ResKor, Prelude, (NS=NounKor), (SS=StructuralKor) in {

lin
  --  : Cl -> RCl ;            -- such that John loves her
  RelCl cl = {s = \\t,a,p => cl.s ! t ! a ! p ! Subord} ;

  -- : RP -> VP -> RCl ;
  RelVP rp vp = {
    s = \\t,a,p => vp.adv
                ++ vp.nObj
                ++ vp.s ! VAttr p  -- TODO no tenses yet in the grammar
                ++ rp.s ;
} ;

  -- : RP -> ClSlash -> RCl ; -- whom John loves
  RelSlash rp cls = {
    s = \\t,a,p => cls.s ! t ! a ! p ! Subord
                ++ rp.s ;
} ;

  -- : RP ;
  IdRP = {s = ""} ;

  -- : Prep -> NP -> RP -> RP ;  -- the mother of whom
  --FunRP prep np rp = {} ;

}
