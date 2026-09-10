concrete VerbHun of Verb = CatHun **
  open ResHun, AdverbHun, NounHun, Prelude in {


lin

-----
-- VP
  -- : V -> VP
  UseV = ResHun.useV ;

  --  : V2 -> VP ; -- be loved
  -- PassV2 = ResHun.passV2 ;

  -- : VPSlash -> VP ;
  -- ReflVP = ResHun.insertRefl ;

  -- : VV  -> VP -> VP ;
  -- ComplVV vv vp = let vc = vp.vComp in case vv.vvtype of {
  --
  --   } ;

  -- : VS  -> S  -> VP ;
  ComplVS vs s = useV vs ** {
    adv = "hogy" ++ s.s
    } ;

  -- : VQ -> QS -> VP ;
  ComplVQ vq qs = useV vq ** {
    adv = qs.s
    } ;

  -- : VA -> AP -> VP ;  -- they become red
  ComplVA va ap = useV va ** {
    adv = ap.s ! Sg ! Nom ++ ap.compl ! Sg
    } ;

--------
-- Slash

  -- : V2 -> VPSlash
  SlashV2a = ResHun.useVc ;

  -- : V3 -> NP -> VPSlash ; -- give it (to her)
  Slash2V3 v3 np = v3 ** {
    adv = np.s ! NoPoss ! v3.c2 ++ np.postmod ;
    c2 = v3.c3
    } ;

  -- : V3 -> NP -> VPSlash ; -- give (it) to her
  Slash3V3 v3 np = v3 ** {
    adv = np.s ! NoPoss ! v3.c3 ++ np.postmod ;
    c2 = v3.c2
    } ;

  -- : V2S -> S  -> VPSlash ;  -- answer (to him) that it is good
  SlashV2S v2s s = useVc v2s ** {
    adv = "hogy" ++ s.s
    } ;


  -- : V2V -> VP -> VPSlash ;  -- beg (her) to go
  SlashV2V v2v vp = useVc v2v ** {
    adv = infVP vp
    } ;

  -- : V2Q -> QS -> VPSlash ;  -- ask (him) who came
  SlashV2Q v2q qs = useVc v2q ** {
    adv = qs.s
    } ;

  -- : V2A -> AP -> VPSlash ;  -- paint (it) red
  SlashV2A v2a ap = useVc v2a ** {
    adv = ap.s ! Sg ! Nom ++ ap.compl ! Sg
  } ;

  -- : VPSlash -> NP -> VP
  ComplSlash = insertObj ;
{-
  -- : VV  -> VPSlash -> VPSlash ;
                  -- Just like ComplVV except missing subject!
  SlashVV vv vps = ComplVV vv vps ** { missing = vps.missing ;
                                       post = vps.post } ;

  -- : V2V -> NP -> VPSlash -> VPSlash ; -- beg me to buy
  SlashV2VNP v2v np vps =
    ComplVV v2v vps **
      { missing = vps.missing ;
        post = vps.post ;
        iobj = np ** { s = np.s ! Dat } } ;

-}

  -- : Comp -> VP ;
  UseComp comp = comp ;


  -- : VP -> Adv -> VP ;  -- sleep here
  AdvVP = insertAdv ;

  -- : VPSlash -> Adv -> VPSlash ;  -- use (it) here
  AdvVPSlash = insertAdvSlash ;
  -- : VP -> Adv -> VP ;  -- sleep , even though ...
  ExtAdvVP vp adv = vp ** {
    adv = vp.adv ++ bindComma ++ adv.s
    } ;

  -- : AdV -> VP -> VP ;  -- always sleep
  AdVVP adv vp = vp ** {
    s = \\vf => adv.s ++ vp.s ! vf
  } ;

  -- : AdV -> VPSlash -> VPSlash ;  -- always use (it)
  AdVVPSlash adv vps = vps ** {
    s = \\o,vf => adv.s ++ vps.s ! o ! vf
  } ;

  -- : VP -> Prep -> VPSlash ;  -- live in (it)
  VPSlashPrep vp prep = vp ** {
    s = \\_ => vp.s ;
    adv = vp.obj ++ vp.adv ++ prep.s ;
    c2 = prep.c
    } ;

--2 Complements to copula

-- Adjectival phrases, noun phrases, and adverbs can be used.

  -- : AP  -> Comp ;
  CompAP ap = UseCopula ** {
    s = \\vf => case vf of {
                  VPres P3 n => ap.s ! n ! Nom ++ ap.compl ! n ;
                  VPres _  n => ap.s ! n ! Nom ++ copula.s ! vf ++ ap.compl ! n ;
                  VPast _  n => ap.s ! n ! Nom ++ copula.s ! vf ++ ap.compl ! n ;
                  _          => ap.s ! Sg ! Nom ++ copula.s ! vf ++ ap.compl ! Sg
                } ;
    } ;

  -- : CN  -> Comp ;
  CompCN cn = UseCopula ** {
    s = \\vf => case vf of {
                  VPres P3 n => cn.s ! SgNom -- TODO
                             ++ cn.compl ! n ! Nom ;

                  VPres _  n => cn.s ! SgNom -- TODO
                             ++ cn.compl ! n ! Nom
                             ++ copula.s ! vf ;

                  _          => cn.s ! SgNom
                             ++ cn.compl ! Sg ! Nom
                             ++ copula.s ! vf} ;
    adv = cn.postmod ;
    } ;

  -- : NP  -> Comp ;
  CompNP np = UseCopula ** {
    s = \\vf => case vf of {
                  VPres P3 _ => linNP np ;
                  _ => linNP np ++ copula.s ! vf } ;
    } ;

  -- : Adv  -> Comp ;
  CompAdv adv = UseCopula ** {
    s = \\vf => adv.s ++ copula.s ! vf ;
    } ;

  -- : VP -- Copula alone;
  UseCopula = useV copula ;

oper
insertObj : ResHun.VPSlash -> NounPhrase -> VerbPhrase = \vps,np -> vps ** {
  obj = case <vps.sc,vps.c2> of {
              <SCDat,Nom> => [] ;
              _ => np.s ! NoPoss ! vps.c2 } ;

  -- To accommodate application grammars that use AdvCN in place of AdvVP.
  -- (Easy mistake to make, because in English it doesn't affect word order.)
  -- If you want the NP's postmodifiers to go right next to the NP,
  -- put np.postmod right after np.s.
  adv = vps.adv ++ np.postmod ;

  s = \\vf =>
   -- If verb's subject case is Dat and object Nom, verb agrees with obj.
      case <vps.sc,vps.c2> of { -- have_V2 needs its object possessed by the subject
        <SCDat,Nom> =>
          let agr : Person*Number = case vf of {
                VPres p n => <p,n> ;
                VPast p n => <p,n> ;
                _         => <P3,Sg> } ;
              objvf : VForm = case vf of {
                VPast _ _ => VPast np.agr.p1 np.agr.p2 ;
                _         => VPres np.agr.p1 np.agr.p2 } ;
           in np.s ! Poss agr.p1 agr.p2 ! vps.c2
           ++ vps.s ! np.objdef ! objvf ;

        -- Default case: Verb agrees in person and number with subject
        _ => vps.s ! np.objdef ! vf } ;
  } ;

}
