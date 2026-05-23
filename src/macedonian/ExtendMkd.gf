--# -path=.:../abstract:../common:prelude
concrete ExtendMkd of Extend = CatMkd ** open Predef, ResMkd, ParadigmsMkd, StructuralMkd, GrammarMkd, Prelude in {

lincat
  VPS = {s : Agr => Str} ;
  [VPS] = {s : Agr => Ints 4 => Str} ;
  VPI = {s : Agr => Str} ;
  [VPI] = {s : Agr => Ints 4 => Str} ;
  [Comp] = {s : GenNum => Ints 4 => Str} ;
  [Imp] = {s : Polarity => GenNum => Ints 4 => Str} ;
  RNP = {s : Agr => Role => Str; a : Agr; isPron : Bool} ;
  RNPList = {s : Agr => Ints 4 => Role => Str; a : Agr} ;

lin
  GenModNP num np cn = DetCN (DetQuant DefArt num) (PossNP cn np) ;

  EmptyRelSlash slash = RelSlash IdRP slash ;

  CompoundN n1 n2 = 
    let comp : Species => Number => Str
             = \\sp,n => case n1.relType of {
                           Pref   => n1.rel ! sp ! genNum n2.g n ++ n2.s ! sp ! n ;
                           AdjMod => n1.rel ! sp ! genNum n2.g n ++ n2.s ! Indef ! n ;
                           AdvMod => n2.s ! sp ! n ++ n1.rel ! sp ! genNum n2.g n
                         } ;
        voc : Number => Str
             = \\n => case n1.relType of {
                        Pref   => n1.rel ! Indef ! genNum n2.g n ++ n2.vocative ! n ;
                        AdjMod => n1.rel ! Indef ! genNum n2.g n ++ n2.vocative ! n ;
                        AdvMod => n2.vocative ! n ++ n1.rel ! Indef ! genNum n2.g n
                      }
    in {
         s   = comp ;
         count_form = comp ! Indef ! Pl ;
         vocative = voc ;
         rel = \\sp,n => "на" ++ comp ! sp ! Sg ;  relType = AdvMod ;
         g   = n2.g
    } ;

  CompoundAP n a = AdvAP (PositA a) (PrepNP (mkPrep "од") (MassNP (UseN n))) ;

  PositAdVAdj a = {s = a.adverb} ;

  PresPartAP vp = {
    s = \\_,gn => "што" ++
                  vp.present ! Imperfective ! genNum2num gn ! P3 ++
                  vp.compl ! {g = gn; p = P3} ;
    isPre = False
  } ;

  PastPartAP vps = {
    s = \\_,gn => vps.participle.adjectival ! Perfective ++
                  vps.compl ! {g = gn; p = P3} ;
    isPre = False
  } ;

  PastPartAgentAP vps np = {
    s = \\_,gn => vps.participle.adjectival ! Perfective ++
                  vps.compl ! {g = gn; p = P3} ++
                  "од" ++ np.s ! RPrep ;
    isPre = False
  } ;

  PassVPSlash vps = {
      present = \\_ => auxBe.present ;
      aorist = auxBe.imperfect ;
      imperfect = \\_ => auxBe.imperfect ;
      imperative = \\_ => auxBe.imperative ;
      participle = {aorist = \\_ => auxBe.participle.aorist ;
                    imperfect = \\_ => auxBe.participle.imperfect ;
                    perfect = \\_ => nonExist ;
                    adjectival = \\_ => nonExist ;
                    adverbial = nonExist} ;
      noun_from_verb = nonExist ;
      compl = \\agr => vps.participle.adjectival ! Perfective ++ vps.compl ! agr ;
      vtype = VNormal
    } ;

  PassAgentVPSlash vps np = {
      present = \\_ => auxBe.present ;
      aorist = auxBe.imperfect ;
      imperfect = \\_ => auxBe.imperfect ;
      imperative = \\_ => auxBe.imperative ;
      participle = {aorist = \\_ => auxBe.participle.aorist ;
                    imperfect = \\_ => auxBe.participle.imperfect ;
                    perfect = \\_ => nonExist ;
                    adjectival = \\_ => nonExist ;
                    adverbial = nonExist} ;
      noun_from_verb = nonExist ;
      compl = \\agr => vps.participle.adjectival ! Perfective ++ vps.compl ! agr ++
                       "од" ++ np.s ! RPrep ;
      vtype = VNormal
    } ;

  ProgrVPSlash vps = vps ** {
    present = \\_,n,p => vps.present ! Imperfective ! n ! p ;
    imperfect = \\_,n,p => vps.imperfect ! Imperfective ! n ! p ;
    imperative = \\_,n => vps.imperative ! Imperfective ! n
  } ;

  GerundCN vp =
    let refl : Str = case vp.vtype of {
          VNormal => [] ;
          VMedial Acc => "се" ;
          VMedial Dat => "си"
        }
    in {
      s = \\sp,num => case <sp,num> of {
                        <Indef,Sg> => vp.noun_from_verb ;
                        <Indef,Pl> => vp.noun_from_verb ++ BIND ++ "а" ;
                        <Def Unspecified,Sg> => vp.noun_from_verb ++ BIND ++ "то" ;
                        <Def Unspecified,Pl> => vp.noun_from_verb ++ BIND ++ "та" ;
                        <Def Proximal,Sg> => vp.noun_from_verb ++ BIND ++ "во" ;
                        <Def Proximal,Pl> => vp.noun_from_verb ++ BIND ++ "ва" ;
                        <Def Distal,Sg> => vp.noun_from_verb ++ BIND ++ "но" ;
                        <Def Distal,Pl> => vp.noun_from_verb ++ BIND ++ "на"
                      } ++ refl ++
                     vp.compl ! {g = GSg Neuter; p = P3} ;
      count_form = vp.noun_from_verb ++ BIND ++ "а" ++ refl ++
                   vp.compl ! {g = GSg Neuter; p = P3} ;
      vocative = \\num => vp.noun_from_verb ++ refl ++
                         vp.compl ! {g = GSg Neuter; p = P3} ;
      g = Neuter
    } ;

  GerundNP vp = MassNP (GerundCN vp) ;

  GerundAdv vp =
    let refl : Str = case vp.vtype of {
          VNormal => [] ;
          VMedial Acc => "се" ;
          VMedial Dat => "си"
        }
    in {s = vp.participle.adverbial ++ refl ++
            vp.compl ! {g = GSg Neuter; p = P3}} ;

  ByVP vp = {s = "со" ++ vp.noun_from_verb ++ BIND ++ "то" ++
                 vp.compl ! {g = GSg Neuter; p = P3}} ;
  WithoutVP vp = {s = "без" ++ vp.noun_from_verb ++ BIND ++ "то" ++
                      vp.compl ! {g = GSg Neuter; p = P3}} ;
  InOrderToVP vp = {s = "за да" ++ vp.present ! Perfective ! Sg ! P3 ++
                         vp.compl ! {g = GSg Masc; p = P3}} ;

  ComplBareVS vs s = vs ** {compl = \\_ => s.s} ;
  ComplSlashPartLast = ComplSlash ;

  UttVPShort vp = {
    s = vp.imperative ! Perfective ! Sg ++ vp.compl ! {g = GSg Masc; p = P2}
  } ;

  BaseVPS x y = {s = \\a => table {4 => y.s ! a; _ => x.s ! a}} ;
  ConsVPS x xs = {
    s = \\a => table {
      4 => xs.s ! a ! 4 ;
      i => x.s ! a ++ linCoord "," ! i ++ xs.s ! a ! i
    }
  } ;
  MkVPS t p vp = {
    s = \\agr => t.s ++ p.s ++ mkClause [] agr vp ! Main ! t.t ! t.a ! p.p
  } ;
  ConjVPS conj vps = {
    s = \\agr => linCoord [] ! conj.sep ++
                 vps.s ! agr ! conj.sep ++ conj.s ++ vps.s ! agr ! 4
  } ;
  PredVPS np vps = {s = np.s ! RSubj ++ vps.s ! np.a} ;

  BaseVPI x y = {s = \\a => table {4 => y.s ! a; _ => x.s ! a}} ;
  ConsVPI x xs = {
    s = \\a => table {
      4 => xs.s ! a ! 4 ;
      i => x.s ! a ++ linCoord "," ! i ++ xs.s ! a ! i
    }
  } ;
  MkVPI vp = {
    s = \\agr => "да" ++ vp.present ! Perfective ! genNum2num agr.g ! agr.p ++ vp.compl ! agr
  } ;
  ConjVPI conj vpi = {
    s = \\agr => linCoord [] ! conj.sep ++
                 vpi.s ! agr ! conj.sep ++ conj.s ++ vpi.s ! agr ! 4
  } ;
  ComplVPIVV vv vpi = vv ** {
    compl = \\agr => vpi.s ! agr
  } ;

  BaseComp x y = {
    s = \\gn => table {4 => y.s ! gn; _ => x.s ! gn}
  } ;
  ConsComp x xs = {
    s = \\gn => table {
      4 => xs.s ! gn ! 4 ;
      i => x.s ! gn ++ linCoord "," ! i ++ xs.s ! gn ! i
    }
  } ;
  ConjComp conj comps = {
    s = \\gn => linCoord [] ! conj.sep ++
                comps.s ! gn ! conj.sep ++ conj.s ++ comps.s ! gn ! 4
  } ;

  BaseImp x y = {
    s = \\p,gn => table {4 => y.s ! p ! gn; _ => x.s ! p ! gn}
  } ;
  ConsImp x xs = {
    s = \\p,gn => table {
      4 => xs.s ! p ! gn ! 4 ;
      i => x.s ! p ! gn ++ linCoord "," ! i ++ xs.s ! p ! gn ! i
    }
  } ;
  ConjImp conj imps = {
    s = \\p,gn => linCoord [] ! conj.sep ++
                  imps.s ! p ! gn ! conj.sep ++ conj.s ++ imps.s ! p ! gn ! 4
  } ;

  ReflRNP slash rnp =
    slash ** {
      compl = \\agr =>
        slash.compl ! agr ++
        case <rnp.isPron,slash.c2.c> of {
          <True,Acc> => [] ;
          <True,Dat> => [] ;
          _ => slash.c2.s ++ rnp.s ! agr ! RObj slash.c2.c
        } ;
      vtype = case <rnp.isPron,slash.c2.c> of {
        <True,Acc> => VMedial Acc ;
        <True,Dat> => VMedial Dat ;
        _ => slash.vtype
      }
    } ;

  ReflPron = {
    s = \\_,role => case role of {
      RObj Acc => "себе" ;
      RObj Dat => "себе" ;
      _ => "себе"
    } ;
    a = {g = GSg Masc; p = P3} ;
    isPron = True
  } ;

  ReflPoss num cn =
    let np : NP = DetCN (DetQuant ReflPossPron num) cn
    in {
      s = \\_,role => np.s ! role ;
      a = np.a ;
      isPron = False
    } ;

  PredetRNP pred rnp = rnp ** {
    s = \\agr,role => pred.s ++ rnp.s ! agr ! role ;
    isPron = False
  } ;

  AdvRNP np prep rnp = {
    s = \\agr,role => np.s ! role ++ prep.s ++ rnp.s ! agr ! RObj prep.c ;
    a = np.a ;
    isPron = False
  } ;

  AdvRVP vp prep rnp = vp ** {
    compl = \\agr => vp.compl ! agr ++ prep.s ++ rnp.s ! agr ! RObj prep.c
  } ;

  AdvRAP ap prep rnp = {
    s = \\sp,gn => ap.s ! sp ! gn ++ prep.s ++
                   rnp.s ! {g = gn; p = P3} ! RObj prep.c ;
    isPre = False
  } ;

  ReflA2RNP a2 rnp = {
    s = \\sp,gn => a2.s ! sp ! gn ++ a2.c2.s ++
                   rnp.s ! {g = gn; p = P3} ! RObj a2.c2.c ;
    isPre = False
  } ;

  PossPronRNP pron num cn rnp =
    let det : Det = DetQuant (PossPron pron) num ;
        n : Number = nnum2num num.n
    in {
      s = \\_ => det.s ! cn.g ++ cn.s ! det.sp ! n ++
                 "на" ++ rnp.s ! pron.a ! RObj Acc ;
      vocative = det.s ! cn.g ++ cn.vocative ! n ++
                 "на" ++ rnp.s ! pron.a ! RObj Acc ;
      a = {g = genNum cn.g n; p = P3}
    } ;

  ApposNP np1 np2 = {
    s = \\role => np1.s ! role ++ SOFT_BIND ++ "," ++ np2.s ! role ;
    vocative = np1.vocative ++ SOFT_BIND ++ "," ++ np2.vocative ;
    a = np1.a
  } ;

  Base_rr_RNP x y = {
    s = \\agr => table {4 => y.s ! agr; _ => x.s ! agr} ;
    a = y.a
  } ;
  Base_nr_RNP x y = {
    s = \\agr => table {4 => y.s ! agr; _ => x.s} ;
    a = y.a
  } ;
  Base_rn_RNP x y = {
    s = \\agr => table {4 => y.s; _ => x.s ! agr} ;
    a = y.a
  } ;
  Cons_rr_RNP x xs = {
    s = \\agr => table {
      4 => xs.s ! agr ! 4 ;
      i => \\role => x.s ! agr ! role ++ linCoord "," ! i ++ xs.s ! agr ! i ! role
    } ;
    a = xs.a
  } ;
  Cons_nr_RNP x xs = {
    s = \\agr => table {
      4 => xs.s ! agr ! 4 ;
      i => \\role => x.s ! role ++ linCoord "," ! i ++ xs.s ! agr ! i ! role
    } ;
    a = xs.a
  } ;
  ConjRNP conj xs = {
    s = \\agr,role => linCoord [] ! conj.sep ++
                     xs.s ! agr ! conj.sep ! role ++ conj.s ++
                     xs.s ! agr ! 4 ! role ;
    a = xs.a ;
    isPron = False
  } ;

  ReflPossPron = {
    s = table {
      GSg Masc => "својот" ;
      GSg Fem => "својата" ;
      GSg Neuter => "своето" ;
      GPl => "своите"
    } ;
    sp = Indef
  } ;

  UseDAP dap = {
    s = \\_ => dap.s ! Neuter ;
    vocative = dap.s ! Neuter ;
    a = {g = genNum Neuter (nnum2num dap.n); p = P3}
  } ;
  UseDAPMasc dap = {
    s = \\_ => dap.s ! Masc ;
    vocative = dap.s ! Masc ;
    a = {g = genNum Masc (nnum2num dap.n); p = P3}
  } ;
  UseDAPFem dap = {
    s = \\_ => dap.s ! Fem ;
    vocative = dap.s ! Fem ;
    a = {g = genNum Fem (nnum2num dap.n); p = P3}
  } ;

  UseComp_estar = UseComp ;
  UseComp_ser = UseComp ;
  ProDrop pro = pro ;

  iFem_Pron      = mkPron "јас" "мене" "ме" "мене" "ми" "мене" "мој" "мојот" "моја" "мојата" "мое" "моето" "мои" "моите" "ми" (GSg Fem) P1 ;
  youFem_Pron    = mkPron "ти" "тебе" "те" "тебе" "ти" "тебе" "твој" "твојот" "твоја" "твојата" "твое" "твоето" "твои" "твоите" "ти" (GSg Fem) P2 ;
  weFem_Pron     = we_Pron ;
  youPlFem_Pron  = youPl_Pron ;
  theyFem_Pron   = they_Pron ;
  youPolFem_Pron = mkPron "вие" "вас" "ве" "вам" "ви" "вас" "ваш" "вашиот" "ваша" "вашата" "ваше" "вашето" "ваши" "вашите" "ви" (GSg Fem) P2 ;
  youPolPl_Pron  = youPol_Pron ;
  youPolPlFem_Pron = youPol_Pron ;

lin TPastSimple = {s = []} ** {t = VPastSimple} ;  --# notpresent

}
