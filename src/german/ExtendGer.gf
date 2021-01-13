--# -path=alltenses:../common:../abstract

concrete ExtendGer of Extend =
  CatGer ** ExtendFunctor
   - [InOrderToVP]
  with
    (Grammar = GrammarGer) **
  open
    GrammarGer,
    ResGer,
    Coordination,
    Prelude,
    ParadigmsGer in {

lin

  InOrderToVP vp = {s = "um" ++ useInfVP False vp} ;
  
}