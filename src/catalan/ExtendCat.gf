--# -path=alltenses:../common:../abstract:../romance
concrete ExtendCat of Extend = CatCat ** ExtendRomanceFunctor--  -
  [
      
   BaseVPS, ConsVPS, PredVPS, MkVPS, ConjVPS, RelVPS

     ]
  -- don't forget to put the names of your own
                       -- definitions here
  with
    (Grammar = GrammarCat), (Syntax = SyntaxCat), (ResRomance = ResCat) **
  open
  GrammarCat,
  ResCat,
  MorphoCat,
  Coordination,
  Prelude,
  ParadigmsCat in {
    -- put your own definitions here


  lincat
    VPS = {s : Mood => Agr => Bool => Str} ;
    [VPS] = {s1,s2 : Mood => Agr => Bool => Str} ;

  lin
    BaseVPS x y = twoTable3 Mood Agr Bool x y ;
    ConsVPS = consrTable3 Mood Agr Bool comma ;

  lin
    PredVPS np vpi = {
      s = \\m => (np.s ! Nom).comp ++ vpi.s ! m ! np.a ! np.isNeg
      } ;
    MkVPS tm p vp = {
      s = \\m,agr,isNeg =>
        tm.s ++ p.s ++
        (mkClausePol (orB isNeg vp.isNeg) [] False False agr vp).s
          ! DDir ! tm.t ! tm.a ! p.p ! m
      } ;
    ConjVPS = conjunctDistrTable3 Mood Agr Bool ;

    RelVPS rp vpi = {
      s = \\m, agr => rp.s ! False ! complAgr agr ! Nom ++ vpi
                      .s ! m ! (Ag rp.a.g rp.a.n P3) ! False ;
      c = Nom
      } ;


} ;
