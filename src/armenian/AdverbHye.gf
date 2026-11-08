concrete AdverbHye of Adverb = CatHye ** open Prelude,ResHye in {
  lin PrepNP p np = {s = case p.isPre of {
                           False => np.s ! p.c ++ p.s;
                           True  => p.s ++ np.s ! p.c
                         }} ;
  lin PositAdvAdj a = {s = a.s ! Instr ! Sg} ;
  lin PositAdAAdj a = {s = a.s ! Instr ! Sg} ;
  lin AdAdv ada adv = {s = ada.s ++ adv.s} ;
  lin SubjS subj s = {s = subj.s ++ s.s} ;
}
