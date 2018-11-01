concrete RelativeSom of Relative = CatSom ** open
  ResSom, Prelude, (NS=NounSom), (SS=StructuralSom) in {

{-
lin
  --  : Cl -> RCl ;            -- such that John loves her
  RelCl cl = { } ;

  -- : RP -> VP -> RCl ;
  RelVP rp vp =  ;

  -- : RP -> ClSlash -> RCl ; -- whom John loves
  RelSlash rp cls = ;


  -- : RP ;
  IdRP = { s = "" } ;

  -- : Prep -> NP -> RP -> RP ;  -- the mother of whom
  FunRP prep np rp = { s = applyPost prep whom ++ np.s ! Abs }
   where { num = case np.agr of {
              Sg3 _ => NS.NumSg ;
              _     => NS.NumPl } ;
           whom = NS.DetNP (NS.DetQuant SS.which_IQuant num) } ;



-}

}
