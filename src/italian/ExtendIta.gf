--# -path=alltenses:../common:../abstract:../romance
concrete ExtendIta of Extend = CatIta ** ExtendRomanceFunctor  -
   [
   PassVPSlash, PassAgentVPSlash
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

lin GivenName, MaleSurname, FemaleSurname, PlSurname = \n -> n ;
lin FullName gn sn = {
       s = gn.s ++ sn.s ;
       g = gn.g
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

}
