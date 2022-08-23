concrete PhraseExtZul of PhraseExt = CatZul,CatExtZul ** open ResZul, Prelude, ParamX in {

  lin
    DirectSpeech phr t v np = {
      s = let
        vform = case t.t of {
          PastTense => v.s!R_e ;
          PresTense => v.s!R_a ;
          _ => v.s!R_a ++ "*"
        } ;
      in
        phr.s ++ "ku" ++ BIND ++ vform ++ np.s!NFull ++ t.s
    } ;

    DirectSpeechNP np1 t v np2 = {
      s = let
        vform = case t.t of {
          PastTense => v.s!R_e ;
          PresTense => v.s!R_a ;
          _ => v.s!R_a ++ "*"
        } ;
      in
        np1.s!NFull ++ "ku" ++ BIND ++ vform ++ np2.s!NFull ++ t.s
    } ;

    DirectSpeechAdv adv t v np = {
      s = let
        vform = case t.t of {
          PastTense => v.s!R_e ;
          PresTense => v.s!R_a ;
          _ => v.s!R_a ++ "*"
        } ;
      in
        adv.s ++ "ku" ++ BIND ++ vform ++ np.s!NFull ++ t.s
    } ;

    ExtPhrConj p1 p2 = {
      s = p1.s ++ p2.s
    } ;

}
