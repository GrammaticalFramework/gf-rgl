incomplete resource NguniSyntax =
  open Grammar,ExtraZulAbs,Backward,TempAbs

in {

oper

  mkUtt = overload {
    mkUtt : S -> Utt
      = UttS ;
    mkUtt : QS -> Utt
      = UttQS ;
    mkUtt : Pol -> Imp -> Utt
      = UttImpSg ;
    mkUtt : Imp -> Utt
      = UttImpSg PPos
  } ;

  mkImp = overload {
    mkImp : VP -> Imp
      = ImpVP ;
  } ;

  mkS = overload {
    mkS : Temp -> Pol -> Cl -> S
      = UseCl ;
    mkS : Pol -> Cl -> S
      = UseCl TPresTemp ;
    mkS : Temp -> Cl -> S
      = \t -> UseCl t PPos ;
    mkS : Cl -> S
      = UseCl TPresTemp PPos
  } ;

  mkQS = overload {
    mkQS : Temp -> Pol -> QCl -> QS
      = UseQCl ;
    mkQS : Pol -> QCl -> QS
      = UseQCl TPresTemp ;
    mkQS : Temp -> QCl -> QS
      = \t -> UseQCl t PPos ;
    mkQS : QCl -> QS
      = UseQCl TPresTemp PPos -- ;
      -- mkQS : Adv -> QS -> QS
      --   = AdvQS ;
  } ;

  potQS = overload {
    potQS : Pol -> QCl -> QS
      = PotQS
  } ;

  positivePol : Pol
    = PPos ;
  negativePol : Pol
    = PNeg ;

  presentTense : Temp
    = TPresTemp ;
  perfectTense : Temp
    = TPerfTemp ;
  pastTense : Temp
    = TPastTemp ;
  futureTense : Temp
    = TFutTemp ;

  mkCl = overload {
    mkCl : NP -> VP -> Cl
      = PredVP ;
    mkCl : NP -> V -> Cl
      = \np,v -> PredVP np (UseV v) ;
    mkCl : NP -> V2 -> NP -> Cl
      = \np,v,obj -> PredVP np (ComplV2 v obj) ;
    -- mkCl : NP -> AP -> Cl
    --   = \np,ap -> PredVP np (UseAP ap) ;
    -- mkCl : NP -> A -> Cl
    --   = \np,a -> PredVP np (UseAP (PositA a)) ;
    mkCl : NP -> Cl
      = PredNP
  } ;

  mkQCl = overload {
    mkQCl : Cl -> QCl
      = QuestCl ;
    -- mkQCl : NP -> IAdv -> QCl
    --   = IAdvQCl ;
    mkQCl : IAdv -> Cl -> QCl
      = QuestIAdv
    -- mkQCl : Adv -> QCl -> QCl
    --   = AdvQCl
  } ;

  mkVP = overload {
    mkVP : V -> VP
      = UseV ;
    mkVP : V2 -> NP -> VP
      = ComplV2 ;
    mkVP : V3 -> NP -> NP -> VP
      = ComplV3 ;
    mkVP : NP -> VP
      = \np -> UseComp (CompNP np) ;
    mkVP : Adv -> VP
      = \adv -> UseComp (CompAdv adv) ;
    mkVP : AP -> VP
      = \ap -> UseComp (CompAP ap) ;
    mkVP : VP -> Adv -> VP
      = AdvVP ;
    mkVP : VA -> AP -> VP
      = ComplVA ;
    mkVP : VS -> S -> VP
      = ComplVS ;
    mkVP : VAux -> VP -> VP
      = ComplVAux
  } ;

  assocVP = overload {
    assocVP : NP -> VP
      = AssocCop
  } ;

  eqVP = overload {
    eqVP : NP -> VP
      = EqCop
  } ;

  -- progVP = overload {
  --   progVP : VP -> VP
  --     = ProgVP
  -- } ;

  mkNP = overload {
    mkNP : Det -> CN -> NP
      = DetCN ;
    mkNP : Det -> N -> NP
      = \det,n -> DetCN det (UseN n) ;
    mkNP : Pron -> NP
      = UsePron ;
    mkNP : CN -> NP
      = \cn -> DetCN (DetQuant IndefArt NumSg) cn ;
    mkNP : N -> NP
      = \n -> DetCN (DetQuant IndefArt NumSg) (UseN n) ;
    -- mkNP : NP -> Conj -> NP -> NP
    --   = \np1,conj,np2 -> ConjNP np1 conj np2 ;
    mkNP : QuantStem -> CN -> NP
      = QuantCN ;
    mkNP : NP -> Adv -> NP
      = AdvNP
  } ;

    i_NP : NP
      = UsePron i_Pron ;
    you_NP : NP
      = UsePron youSg_Pron ;
    -- he_NP : NP
    --   = UsePron he_Pron ;
    -- she_NP : NP
    --   = UsePron she_Pron ;

  mkCN = overload {
    mkCN : N -> CN
      = UseN ;
    mkCN : AP -> CN -> CN
      = AdjCN ;
    mkCN : A -> N -> CN
      = \a,n -> AdjCN (PositA a) (UseN n) ;
    mkCN : A -> CN -> CN
      = \a,cn -> AdjCN (PositA a) cn ;
    mkCN : Adv -> CN -> CN
      = \adv,cn -> AdvCN cn adv ;
    mkCN : CN -> RS -> CN
      = RelCN ;
    mkCN : CN -> NP -> CN
      = PossNP ;
    mkCN : N -> NP -> CN
      = \n,np -> PossNP (UseN n) np
    -- mkCN : CN -> Pron -> CN
    --   = PossPron ;
  } ;

  numCN = overload {
    numCN : CN -> A -> CN
      = NumAdjCN ;
  } ;

  -- descrCN : CN -> NP -> CN = DescrNP ;

  mkAP = overload {
    mkAP : A -> AP
      = PositA ;
    mkAP : AdA -> AP -> AP
      = AdAP
  } ;

  mkAdv = overload {
    mkAdv : NP -> Adv
      = \np -> LocNPAdv np ;
    -- mkAdv : ConjN -> S -> Adv
    --   = \conj,s -> ConjAdv conj s ;
    mkAdv : Subj -> S -> Adv
      = SubjS ;
    mkAdv : A -> Adv
      = PositAdvAdj
  } ;

  instrAdv = overload {
    instrAdv : NP -> Adv
      = \np -> InstrNPAdv np ;
    instrAdv : Adv -> NP -> Adv
      = InstrAdvNPAdv
  } ;

  locAdv = overload {
    locAdv : NP -> Adv
      = \np -> LocNPAdv np ;
    locAdv : Adv -> NP -> Adv
      = \adv,np -> LocAdvNPAdv adv np
  } ;

  kwaAdv = overload {
    kwaAdv : Adv -> NP -> Adv
      = \adv,np -> KwaNPAdv adv np
  } ;

  kuAdv = overload {
    kuAdv : NP -> Adv
      = \np -> KuNPAdv np ;
    -- kuAdv : Adv -> NP -> Adv
    --   = \adv,np -> KuAdvNPAdv adv np
  } ;

  assocAdv = overload {
    assocAdv : NP -> Adv
      = NaNPAdv
  } ;

  relAdv = overload {
    relAdv : Adv -> RS
      = \adv -> RelAdv adv
  } ;

  mkRS = overload {
    mkRS : QuantStem -> RS
      = QuantRS ;
    mkRS : RelStem -> RS
      = RelRS ;
    mkRS : Temp -> Pol -> VP -> RS
      = \t,p,vp -> UseRCl t p (RelVP IdRP vp) ;
    mkRS : VP -> RS
      = \vp -> UseRCl TPresTemp PPos (RelVP IdRP vp)
  } ;

  aPl_Det : Det
      = DetQuant IndefArt NumPl ;

  -- mkSymb : Symb
  --     = MkSymb ;

}
