concrete RelativeUkr of Relative = CatUkr ** open ResUkr, (R = ParamX) in {

oper
  relPron : Gender -> Number -> Case -> Str =
    \g,n,c -> case <n,g,c> of {
      <Pl,_,Nom> => "які" ;
      <Pl,_,Acc> => "яких" ;
      <Pl,_,Dat> => "яким" ;
      <Pl,_,Gen> => "яких" ;
      <Pl,_,Loc> => "яких" ;
      <Pl,_,Instr> => "якими" ;
      <Sg,Masc,Nom> => "який" ;
      <Sg,Masc,Acc> => "якого" ;
      <Sg,Masc,Dat> => "якому" ;
      <Sg,Masc,Gen> => "якого" ;
      <Sg,Masc,Loc> => "якому" ;
      <Sg,Masc,Instr> => "яким" ;
      <Sg,Fem,Nom> => "яка" ;
      <Sg,Fem,Acc> => "яку" ;
      <Sg,Fem,Dat> => "якій" ;
      <Sg,Fem,Gen> => "якої" ;
      <Sg,Fem,Loc> => "якій" ;
      <Sg,Fem,Instr> => "якою" ;
      <Sg,Neuter,Nom> => "яке" ;
      <Sg,Neuter,Acc> => "яке" ;
      <Sg,Neuter,Dat> => "якому" ;
      <Sg,Neuter,Gen> => "якого" ;
      <Sg,Neuter,Loc> => "якому" ;
      <Sg,Neuter,Instr> => "яким"
    } ;

lin
  RelCl cl = {
    s = \\_,_ => "що" ++ cl.s ! R.Pres ! R.Pos
  } ;
  RelVP rp vp = {
    s = \\g,n => rp.s ! g ! n ! Nom ++ vp.s ! R.Pres ! R.Pos ! g ! n ! P3
  } ;
  RelSlash rp cls = {
    s = \\g,n => rp.s ! g ! n ! cls.c.c ++ cls.s ! R.Pres ! R.Pos
  } ;
  IdRP = {s = \\g,n,c => relPron g n c} ;
  FunRP prep np rp = {
    s = \\g,n,c => prepNP prep np ++ rp.s ! g ! n ! c
  } ;
}
