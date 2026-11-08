concrete RelativeKor of Relative = CatKor ** open
  ResKor, Prelude, (NS=NounKor), (SS=StructuralKor) in {

lin
  --  : Cl -> RCl ;            -- such that John loves her
  RelCl = relSlash (ss "") ;

  -- : RP -> VP -> RCl ;
  RelVP rp vp = vp ** {
    s = \\t,a,p,cltyp =>
           rp.s ++ vp.nObj ! DeclObj ++ vp.adv ++
           tenseVP t a cltyp p (vp ** {
             nObj = \\_ => [] ; adv = []
           }) ;
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
