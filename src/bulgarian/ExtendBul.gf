--# -path=.:../abstract:../common:prelude
concrete ExtendBul of Extend = CatBul ** open Prelude, Predef, ResBul, GrammarBul, MorphoFunsBul in {
flags
  coding=utf8;

lin
  GenModNP num np cn = DetCN (DetQuant DefArt num) (AdvCN cn (PrepNP (mkPrep "на") np)) ;

  AdAdV a adv = {s = a.s ++ adv.s; p = adv.p} ;

  EmptyRelSlash slash = {
      s = \\t,a,p,agr => linPrep slash.c2 ++ whichRP ! agr.gn ++ slash.s ! agr ! t ! a ! p ! Main
      } ;

  CompoundN n1 n2 = 
    let comp : NForm => Str
             = \\nf => case n1.relType of {
                          Pref   => n1.rel ! nform2aform nf n2.g ++ n2.s ! nf ;
                          AdjMod => n1.rel ! nform2aform nf n2.g ++ n2.s ! indefNForm nf ;
                          AdvMod => n2.s ! nf ++ n1.rel ! nform2aform nf n2.g
                        }
    in {
         s   = comp ;
         rel = \\af => "на" ++ comp ! NF Sg Def ;  relType = AdvMod ;
         g   = n2.g
    } ;

  CompoundAP n a = AdvAP (PositA a) (PrepNP (mkPrep "от" Acc) (MassNP (UseN n))) ;

  PositAdVAdj a = {s = a.adv; p = Pos} ;

  PresPartAP vp =
    let ap : AForm => Person => Str
           = \\aform,p => vp.ad.s ++
                          vp.s ! Imperf ! VPresPart aform ++
                          case vp.vtype of {
                            VMedial c => reflClitics ! c;
                            _           => []
                          } ++
                          vp.compl ! {gn=aform2gennum aform; p=p} ;
    in {s = ap; adv = ap ! (ASg Neut Indef) ! P3; isPre = vp.isSimple} ;

  PastPartAP vp =
    let ap : AForm => Person => Str
           = \\aform,p => vp.ad.s ++
                          vp.s ! Perf ! VPassive aform ++
                          vp.compl1 ! {gn=aform2gennum aform; p=p} ++
                          vp.compl2 ! {gn=aform2gennum aform; p=p}
    in {s = ap; adv = ap ! ASg Neut Indef ! P3; isPre = vp.isSimple} ;

  PastPartAgentAP vp np =
    let ap : AForm => Person => Str
           = \\aform,p => vp.ad.s ++
                          vp.s ! Perf ! VPassive aform ++
                          vp.compl1 ! {gn=aform2gennum aform; p=p} ++
                          vp.compl2 ! {gn=aform2gennum aform; p=p} ++
                          "от" ++ np.s ! RObj CPrep
    in {s = ap; adv = ap ! ASg Neut Indef ! P3; isPre = False} ;

  GerundCN vp = {
    s   = \\nform => vp.ad.s ++
                     vp.s ! Imperf ! VNoun nform ++
                     vp.compl ! {gn=GSg Neut; p=P3} ;
    g = ANeut
  } ;

  GerundNP vp = {
    s  = \\_ => daComplex Simul Pos vp ! Imperf ! {gn=GSg Neut; p=P1};
    gn =GSg Neut;
    p  = NounP3 Pos
  } ;

  GerundAdv, ByVP = \vp ->
    {s = vp.ad.s ++
         vp.s ! Imperf ! VGerund ++
         case vp.vtype of {
           VNormal => vp.clitics ;
           VMedial c => vp.clitics++reflClitics ! c ;
           VPhrasal Dat => personalClitics (agrP3 (GSg Masc)) ! Dat++vp.clitics ;
           VPhrasal c   => vp.clitics++personalClitics (agrP3 (GSg Masc)) ! c
         } ++
         vp.compl ! {gn=GSg Neut; p=P3}} ;

  InOrderToVP vp = 
    {s = "за" ++ daComplex Simul Pos vp ! Perf ! {gn=GSg Neut; p=P3}};

  iFem_Pron      = mkPron "аз" "мой" "моя" "моят" "моя" "моята" "мое" "моето" "мои" "моите" (GSg Fem) PronP1 ;
  youFem_Pron    = youSg_Pron ;
  weFem_Pron     = we_Pron ;
  youPlFem_Pron  = youPl_Pron ;
  theyFem_Pron   = they_Pron ;
  youPolFem_Pron = youPol_Pron ;
  youPolPl_Pron  = youPol_Pron ;
  youPolPlFem_Pron = youPol_Pron ;

lin
  PassVPSlash vp = insertObj (\\a => vp.ad.s ++ vp.s ! Perf ! VPassive (aform a.gn Indef (RObj Acc)) ++
                                     vp.compl1 ! a ++ vp.compl2 ! a) Pos (predV verbBe) ;
  ProgrVPSlash vp = vp ** {
      s   = \\_ => vp.s ! Imperf ;
      isSimple = False
      } ;

  PassAgentVPSlash vp np =
    insertObj (\\_ => "от" ++ np.s ! RObj CPrep) Pos
                   (insertObj (\\a => vp.ad.s ++ vp.s ! Perf ! VPassive (aform a.gn Indef (RObj Acc)) ++
                                      vp.compl1 ! a ++ vp.compl2 ! a) Pos (predV verbBe)) ;

  UttVPShort vp = {
    s = let agr = agrP3 (GSg Neut) ;
            clitic = case vp.vtype of {
                       VNormal    => {s=vp.clitics; agr=agr} ;
                       VMedial c  => {s=vp.clitics++reflClitics ! c; agr=agr} ;
                       VPhrasal c => {s=case c of {
                                          Dat => personalClitics agr ! c++vp.clitics;
                                          c   => vp.clitics++personalClitics agr ! c
                                        } ;
                                      agr={gn=GSg Neut; p=P3}
                                     }
                     } ;
        in vp.ad.s ++ clitic.s ++
           vp.s ! Imperf ! VPres (numGenNum clitic.agr.gn) clitic.agr.p ++
           vp.compl ! agr
    } ;

lincat
  VPI   = {s : Agr => Str} ;
  [VPI] = {s : Agr => Ints 4 => Str} ;

lin
  BaseVPI x y = {s  = \\a=>table {4 => y.s!a;    _ => x.s!a}} ;
  ConsVPI x xs = {s  = \\a=>table {4 => xs.s!a!4; t => x.s!a++linCoord bindComma!t++xs.s!a!t}};

  MkVPI vp = {s = daComplex Simul Pos vp ! Perf} ;
  ConjVPI conj vpi = {
      s = \\a =>  linCoord []!conj.sep ++ vpi.s!a!conj.sep ++ conj.s ++ vpi.s!a!4
      } ;
  ComplVPIVV vv vpi = 
      insertObj (\\a => vpi.s ! a) Pos (predV vv) ;

lincat
  VPS   = {s : Agr => Str} ;
  [VPS] = {s : Agr => Ints 4 => Str} ;

lin
  BaseVPS x y  = {s  = \\a=>table {4 => y.s!a;    _ => x.s!a}} ;
  ConsVPS x xs = {s  = \\a=>table {4 => xs.s!a!4; t => x.s!a++linCoord bindComma!t++xs.s!a!t}};

  PredVPS np vps = {s = np.s ! RSubj ++ vps.s ! personAgr np.gn np.p} ;

  MkVPS t p vp = {
    s = \\a => 
          let verb  = vpTenses vp ! t.t ! t.a ! p.p ! a ! False ! Perf ;
              compl = vp.compl ! a
          in t.s ++ p.s ++ verb ++ compl
    } ;
      
  ConjVPS conj vps = {
    s = \\a => linCoord []!conj.sep ++ vps.s!a!conj.sep ++ conj.s ++ vps.s!a!4
    } ;

lincat [Comp] = {s : Agr => Ints 4 => Str} ;
lin BaseComp x y =
      {s = \\agr=>table {4 => y.s!agr; _ => x.s!agr}} ;
    ConsComp x xs =
      {s = \\agr=>table {4 => xs.s!agr!4; t => x.s!agr++linCoord bindComma!t++xs.s!agr!t}} ;
    ConjComp conj ss = {
      s = \\agr => linCoord [] ! conj.sep ++ ss.s!agr!conj.sep ++ conj.s ++ ss.s!agr!4 ;
      p = Pos
      } ;

lincat ListImp = {s : Polarity => GenNum => Ints 4 => Str} ;
lin BaseImp x y =
      {s  = \\p,gn=>table {4 => y.s!p!gn; _ => x.s!p!gn}} ;
    ConsImp x xs =
      {s  = \\p,gn=>table {4 => xs.s!p!gn!4; t => x.s!p!gn++linCoord bindComma!t++xs.s!p!gn!t}} ;
    ConjImp conj ss = {
      s  = \\p,gn => linCoord [] ! conj.sep ++ ss.s!p!gn!conj.sep ++ conj.s ++ ss.s!p!gn!4
      } ;

lin
  ComplBareVS = ComplVS ;
  ComplSlashPartLast = ComplSlash ;

lincat
  RNP = {s : Role => Str; gn : GenNum; isPron : Bool} ;

lin
  ReflRNP slash rnp = {
    s   = slash.s ;
    ad  = slash.ad ;
    clitics = slash.clitics ;
    compl = \\a => slash.compl1 ! a ++ slash.c2.s ++
                   case <rnp.isPron, slash.c2.c> of {
                     <True, Acc | Dat> => [] ;
                     _                 => rnp.s ! RObj slash.c2.c
                   } ++ 
                   slash.compl2 ! agrP3 rnp.gn ;
    vtype = case <rnp.isPron, slash.c2.c> of {
              <True, Acc | Dat> => VMedial slash.c2.c ;
              _                 => slash.vtype
            } ;
    p     = slash.p ;
    isSimple = False
  } ;

  ReflPron =
      { s  = \\role => "себе си";
        gn = GSg Masc;
        isPron = True
      } ;

  ReflPoss num cn =
      { s = \\role => 
                let nf = case num.nn of {
                           NNum Sg => case role of {
				                        RVoc  => NFVocative ;
				                        _     => NF Sg Indef
                                      } ;
                           NNum Pl => NF Pl Indef;
                           NCountable => case cn.g of {
                                           AMasc Human => NF Pl Indef;
                                           _           => NFPlCount
                                         }
                         } ;
                    s = reflPron ! aform (gennum cn.g (numnnum num.nn)) Def (RObj Acc) ++ num.s ! dgenderSpecies cn.g Indef role ++ cn.s ! nf
                in case role of {
                     RObj c => linCase c Pos ++ s;
                     _      => s
                   } ;
        gn = gennum cn.g (numnnum num.nn) ;
        isPron = False
      } ;

  PredetRNP pred rnp = {
    s  = \\c => pred.s ! rnp.gn ++ rnp.s ! c ;
    gn = rnp.gn ;
    isPron = False
  } ;

  AdvRNP np prep rnp = {s = \\role => np.s ! role ++ prep.s ++ rnp.s ! RObj prep.c; gn = np.gn; p = np.p; isPron = False} ;
  AdvRVP vp prep rnp = insertObj (\\a => prep.s ++ rnp.s ! RObj prep.c) Pos vp ;
  AdvRAP ap prep rnp = {
    s = \\aform,p => ap.s ! aform ! p ++ prep.s ++ rnp.s ! RObj prep.c ;
    isPre = False
  } ;

  ReflA2RNP a rnp = {
    s = \\aform,_ => a.s ! aform ++ a.c2.s ++ rnp.s ! RObj a.c2.c ;
    isPre = False
  } ;

  PossPronRNP pron num cn rnp = DetCN (DetQuant (PossPron pron) num) (PossNP cn (lin NP {s = rnp.s; gn = rnp.gn; p=NounP3 Pos})) ;    

lin
  ApposNP np1 np2 = {s = \\role => case role of {
                                     RObj c => linCase c (personPol np1.p) ++ np1.s ! RObj CPrep ;
                                     role   => np1.s ! role
                                   } ++ bindComma ++ np2.s ! role;
                     gn = np1.gn;
                     p  = NounP3 (personPol np1.p)
                    } ;

  DetNPMasc det = {
    s  = \\role => let s = det.s ! False ! (AMasc Human) ! role
                   in case role of {
                        RObj c => linCase c det.p ++ s;
                        _      => s
                      } ;
    gn = gennum (AMasc Human) (numnnum det.nn);
    p  = NounP3 det.p
    } ;

  DetNPFem det = {
    s = \\role => let s = det.s ! False ! AFem ! role
                  in case role of {
                       RObj c => linCase c det.p ++ s;
                       _      => s
                     } ;
    gn = gennum AFem (numnnum det.nn);
    p = NounP3 det.p
    } ;

lin UseDAP dap = {
      s  = \\role => let s = dap.s ! False ! ANeut ! role
                     in case role of {
                          RObj c => linCase c dap.p ++ s;
                          _      => s
                        } ;
      gn = gennum ANeut (numnnum dap.nn);
      p  = NounP3 dap.p
      } ;

    UseDAPMasc dap = {
      s  = \\role => let s = dap.s ! False ! (AMasc Human) ! role
                     in case role of {
                          RObj c => linCase c dap.p ++ s;
                          _      => s
                        } ;
      gn = gennum (AMasc Human) (numnnum dap.nn);
      p  = NounP3 dap.p
      } ;

    UseDAPFem dap = {
      s = \\role => let s = dap.s ! False ! AFem ! role
                    in case role of {
                         RObj c => linCase c dap.p ++ s;
                         _      => s
                       } ;
      gn = gennum AFem (numnnum dap.nn);
      p  = NounP3 dap.p
      } ;

lin UseComp_estar = UseComp ;
    UseComp_ser = UseComp ;

lin ProDrop pro = pro ;

lin AnaphPron np =
      case <np.gn, np.p> of {
        <GSg _, PronP1> => i_Pron ;
        <GSg _, PronP2> => youSg_Pron ;
        <GSg Masc,   _> => he_Pron ;
        <GSg Fem,    _> => she_Pron ;
        <GSg Neutr,  _> => it_Pron ;
        <GPl,   PronP1> => we_Pron ;
        <GPl,   PronP2> => youPl_Pron ;
        <GPl,        _> => they_Pron
      } ;

lin TPastSimple = {s = []} ** {t = VPastSimple} ;  --# notpresent

}

