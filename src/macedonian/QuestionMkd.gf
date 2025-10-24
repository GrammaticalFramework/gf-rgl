concrete QuestionMkd of Question = CatMkd ** open Prelude,ResMkd in {
  lin AdvIAdv i a = {s = i.s ++ a.s} ;
  lin AdvIP ip a = {s = ip.s ++ a.s; g = ip.g} ;
  lin CompIAdv i = {s = i.s} ;
  lin CompIP ip = {s = ip.s} ;
  lin IdetCN idet cn = {
        s = idet.s ! cn.g ++ cn.s ! Indef ! idet.n;
        g = genNum cn.g idet.n
      } ;
  lin IdetIP idet = {s = idet.s ! Masc; g = genNum Masc idet.n} ;
  lin IdetQuant i n = {
        s = \\g => i.s ! GSg Masc ++ n.s;
        n = Sg
      } ;
  lin PrepIP p ip = {s : Str = p.s ++ ip.s} ;
  lincat QVP = {s : Str} ;
  lin QuestCl cl = {s = cl.s ! Quest} ;
  lin QuestIAdv i cl = {s = \\t,a,p => i.s ++ cl.s ! Main ! t ! a ! p} ;
  lin QuestIComp i np = {s = \\t,a,p => i.s ++ np.s ! RSubj} ;
  lin QuestSlash ip c = {s = \\t,a,p => ip.s ++ c.s} ;
  lin QuestVP ip vp = {s = mkClause ip.s {g=ip.g; p=P3} vp ! Main} ;
}
