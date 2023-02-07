--# -path=alltenses:../common:../abstract:../romance
concrete ExtendIta of Extend = CatIta ** ExtendRomanceFunctor--  -
  -- [
  --   ]
  -- don't forget to put the names of your own
                       -- definitions here
  with
    (Grammar = GrammarIta), (Syntax = SyntaxIta), (ResRomance = ResIta) **
  open
  GrammarIta,
  ResIta,
  MorphoIta,
  Coordination,
  Prelude,
  ParadigmsIta in {
    -- put your own definitions here

lin GivenName, MaleSurname, FemaleSurname, PlSurname = \n -> n ;
lin FullName gn sn = {
       s = gn.s ++ sn.s ;
       g = gn.g
    } ;

} ;
