--# -path=alltenses:../common:../abstract:../romance
concrete ExtendIta of Extend = CatIta ** ExtendRomanceFunctor  -
   [
   GenRP,
   PassVPSlash, PassAgentVPSlash,
   BaseVPS, ConsVPS, PredVPS, MkVPS, ConjVPS, RelVPS

   ]
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

lin
    GenRP nu cn = {
      s = \\_b,_aagr,_c => "di cui" ++ num ++ cn.s ! n ;
      a = aagr g n ;
      hasAgr = True
      } where {
        g = cn.g ;
        n = nu.n ;
        num = if_then_Str nu.isNum (nu.s ! g) []
      } ;




lin PassVPSlash vps = passVPSlash vps [] ;
    PassAgentVPSlash vps np = passVPSlash 
      vps (let by = <Grammar.by8agent_Prep : Prep> in by.s ++ (np.s ! by.c).ton) ;

oper
    passVPSlash : VPSlash -> Str -> VP = \vps, agent -> 
      let auxvp = predV auxPassive 
      in
      vps ** {
         s = auxvp.s ;
         agr = auxvp.agr ;
         comp  = \\a => vps.comp ! a ++ (let agr = complAgr a in vps.s.s ! VPart agr.g agr.n) ++ agent ;
        } ;

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

}
