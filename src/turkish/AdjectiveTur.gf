concrete AdjectiveTur of Adjective =
  CatTur ** open ResTur, HarmonyTur, ParadigmsTur, Prelude in {

  lin

    PositA  a = {s = a.s; h = a.h} ;

    ComparA a np = {
      s = \\n,c => np.s ! Ablat ++ a.s ! n ! c ;
      h = a.h
    } ;

    UseComparA a = {
      s = \\n,c => "daha" ++ a.s ! n ! c ;
      h = a.h
    } ;

    AdjOrd v = v ** {h = {vow=I_Har; con=SCon Soft}} ;  -- to be fixed

    AdvAP ap adv = {
      s = \\n, c => adv.s ++ ap.s ! n ! c ;
      h = ap.h
    } ;

    AdAP ada ap = {
      s = \\n, c => ada.s ++ ap.s ! n ! c ;
      h = ap.h
    } ;

    UseA2 a = {
      s = a.s ;
      h = a.h
    } ;

    ComplA2 a np = {
      s = \\n, c => np.s ! a.c.c ++ a.c.s ++ a.s ! n ! c ;
      h = a.h
    } ;

    -- TODO: Whether this is correct or not requires further examination.
    ReflA2 a = {
      s =
        let
          kendi : N = mkN "kendi"
        in
          \\n, c => kendi.s ! n ! c ++ a.c.s ++ a.s ! n ! Nom ;
      h = a.h
    } ;

    -- Some examples of using CAdvAP:
    --     Lang> gt -number=2 -depth=1 (CAdvAP ? ? ?) | l -lang=LangTur
    --     > Paris kadar kötü
    --     > o kadar kötü
    CAdvAP cadv ap np = {
      s = \\n, c => np.s ! Nom ++ cadv.s ++ ap.s ! n ! c ;
      h = ap.h
    } ;

    -- TODO: Instead of `++ BIND ++ "si"`, sc.s should be treated as a noun
    -- and it should be inflected to `gen Sg {n = Sg; p = P3}`.
    SentAP ap sc = {
      s = \\n, c => sc.s ++ (ap.s ! n ! c) ;
      h = ap.h
    } ;

}
