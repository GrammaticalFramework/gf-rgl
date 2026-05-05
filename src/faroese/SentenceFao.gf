concrete SentenceFao of Sentence = CatFao ** open Prelude, ResFao, (P = ParamX) in {
  lin PredVP np vp = {Converb = np.s ! Nom ++ vp.Converb;
                      Indicative = \\t,pol =>
                                         let p = persNum np.n np.p
                                         in np.s ! Nom ++ vp.Indicative ! t ! pol ! np.g ! p;
                      Nonfinite = np.s ! Nom ++ vp.Nonfinite;
                      Participle = \\t => np.s ! Nom ++ vp.Participle ! t} ;
  UseCl temp pol cl =
    let
      tense = case temp.t of {
        P.Pres => Pres ;
        P.Past => Past ;
        P.Fut => Pres ;
        P.Cond => Past
      } ;
    in {
      s = temp.s ++ pol.s ++ cl.Indicative ! tense ! pol.p
    } ;
  UseRCl temp pol rcl =
    let
      tense = case temp.t of {
        P.Pres => Pres ;
        P.Past => Past ;
        P.Fut => Pres ;
        P.Cond => Past
      }
    in {
      s = \\g,pn =>
        temp.s ++ pol.s ++ rcl.s ! tense ! pol.p ! g ! pn
    } ;
}
