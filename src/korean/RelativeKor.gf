concrete RelativeKor of Relative = CatKor ** open
  ResKor, Prelude, (NS=NounKor), (SS=StructuralKor) in {

lin
  --  : Cl -> RCl ;            -- such that John loves her
  RelCl = relSlash (ss "") ;

  -- : RP -> VP -> RCl ;
  RelVP rp vp = vp ** { -- TODO no tenses yet in the grammar
    s = \\t,a,p,cltyp =>
           rp.s ++ vp.adv ++ vp.nObj ++
           case cltyp of {
             WithConj => vp.s ! VStem p ;
             _        => vp.s ! VAttr p } ;
    } ;

  -- : RP -> ClSlash -> RCl ; -- whom John loves
  RelSlash = relSlash ;


  -- : RP ;
  IdRP = {s = ""} ;

  -- : Prep -> NP -> RP -> RP ;  -- the mother of whom
  --FunRP prep np rp = {} ;

oper
  relSlash : SS -> ClSlash -> ResKor.RClause = \rp,cls -> cls ** {
    s = \\t,a,p,cltyp => rp.s ++
          case cltyp of {
            WithConj => cls.s ! t ! a ! p ! WithConj ;
            _        => cls.s ! t ! a ! p ! Subord } ;
  } ;

}
