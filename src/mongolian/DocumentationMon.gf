--# -path=.:../abstract:../common
concrete DocumentationMon of Documentation = CatMon ** open
  ResMon,
  Prelude,
  HTML in {

flags optimize=noexpand ; coding=utf8 ;

lincat
  Inflection = {t : Str; s1,s2,s3 : Str} ;
  Definition = {s : Str} ;
  Document   = {s : Str} ;
  Tag        = {s : Str} ;

lin InflectionN, InflectionN2, InflectionN3 = \x -> {
      t="n" ;
      s1=heading1 "Noun" ;
      s2=frameTable (
           tr (th ""    ++ th "Sg" ++             th "Pl") ++
           tr (th "Nom" ++ td (x.s ! SF Sg NNom) ++ td (x.s ! SF Pl NNom)) ++
           tr (th "Acc Def" ++ td (x.s ! SF Sg (NAcc Definite)) ++ td (x.s ! SF Pl (NAcc Definite))) ++
           tr (th "Acc Indef" ++ td (x.s ! SF Sg (NAcc Indefinite)) ++ td (x.s ! SF Pl (NAcc Indefinite))) ++
           tr (th "Dat" ++ td (x.s ! SF Sg NDat) ++ td (x.s ! SF Pl NDat)) ++
           tr (th "Gen" ++ td (x.s ! SF Sg NGen) ++ td (x.s ! SF Pl NGen)) ++
           tr (th "Abl" ++ td (x.s ! SF Sg NAbl) ++ td (x.s ! SF Pl NAbl)) ++
           tr (th "Inst" ++td (x.s ! SF Sg NInst) ++td (x.s ! SF Pl NInst)) ++
           tr (th "Com" ++ td (x.s ! SF Sg NCom) ++ td (x.s ! SF Pl NCom)) ++
           tr (th "Dir" ++ td (x.s ! SF Sg NDir) ++ td (x.s ! SF Pl NDir))) ;
      s3=[]
    } ;

lin InflectionPN = \pn -> {
      t="pn" ;
      s1=heading1 "Proper Name" ;
      s2=nameTable pn.s ;
      s3=[]
    } ;

lin InflectionLN = \name -> nameInflection "ln" "Location Name" name ;
lin InflectionGN = \name -> nameInflection "gn" "Given Name" name ;
lin InflectionSN = \name -> nameInflection "sn" "Family Name" name ;

lin InflectionA, InflectionA2 = \adj -> {
      t="a" ;
      s1=heading1 "Adjective" ;
      s2= paragraph (adj.s) ;
      s3=[]
    } ;

lin InflectionAdv, InflectionAdV, InflectionAdA, InflectionAdN = \adv -> {
      t = "adv" ;
      s1= heading1 "Adverb" ;
      s2= paragraph (adv.s) ;
      s3= ""
    } ;

lin InflectionPrep = \prep -> {
      t = "prep" ;
      s1= heading1 "Postposition" ;
      s2= paragraph (prep.s ++
                     case prep.rc of {
                       Nom => "nominative" ;
                       Acc => "accusative" ;
                       Dat => "dative" ;
                       Gen => "genitive" ;
                       Abl => "ablative" ;
                       Inst=> "instrumental" ;
                       Com => "comitative" ;
                       Dir => "directional"
                     }) ;
      s3= ""
    } ;

lin InflectionV = \verb -> {
  t="v" ; s1=heading1 "Verb" ;
  s2=verbTable verb.s ; s3=[]
  } ;
lin InflectionV2 = \verb -> verbInflection verb.s ;
lin InflectionV3 = \verb -> verbInflection verb.s ;
lin InflectionV2V = \verb -> verbInflection verb.s ;
lin InflectionV2S = \verb -> verbInflection verb.s ;
lin InflectionV2Q = \verb -> verbInflection verb.s ;
lin InflectionV2A = \verb -> verbInflection verb.s ;
lin InflectionVS = \verb -> verbInflection verb.s ;
lin InflectionVQ = \verb -> verbInflection verb.s ;
lin InflectionVA = \verb -> verbInflection verb.s ;

lin InflectionVV = \verb -> {
      t="v" ;
      s1=heading1 "Verb" ;
      s2=auxTable verb.s ;
      s3=[]
    } ;

lin InflectionCl = \cl -> {
      t="cl" ;
      s1=heading1 "Clause" ;
      s2=frameTable (
           tr (th "Tense" ++ th "Anteriority" ++ th "Positive" ++ th "Negative") ++
           clauseRow "Present" ClPres Simul cl ++
           clauseRow "Present" ClPres Anter cl ++
           clauseRow "Past perfect" (ClPast Perf) Simul cl ++
           clauseRow "Past perfect" (ClPast Perf) Anter cl ++
           clauseRow "Present perfect" (ClPast PresPerf) Simul cl ++
           clauseRow "Present perfect" (ClPast PresPerf) Anter cl ++
           clauseRow "Indefinite past" (ClPast IndefPast) Simul cl ++
           clauseRow "Indefinite past" (ClPast IndefPast) Anter cl ++
           clauseRow "Future" ClFut Simul cl ++
           clauseRow "Future" ClFut Anter cl) ;
      s3=[]
    } ;

lin
  NoDefinition   t     = {s=t.s};
  MkDefinition   t d   = {s="<p><b>Definition:</b>"++t.s++d.s++"</p>"};
  MkDefinitionEx t d e = {s="<p><b>Definition:</b>"++t.s++d.s++"</p><p><b>Example:</b>"++e.s++"</p>"};

lin
  MkDocument d i e = {s = i.s1 ++ d.s ++ i.s2 ++ paragraph e.s} ;
  MkTag i = {s = i.t} ;

oper
  nameInflection : Str -> Str -> {s : RCase => Str} ->
                   {t : Str; s1,s2,s3 : Str} = \tag,title,name -> {
    t=tag ;
    s1=heading1 title ;
    s2=nameTable name.s ;
    s3=[]
    } ;

  nameTable : (RCase => Str) -> Str = \forms ->
    frameTable (
      tr (th "Case" ++ th "Form") ++
      caseRow "Nominative" Nom forms ++
      caseRow "Accusative" Acc forms ++
      caseRow "Dative-locative" Dat forms ++
      caseRow "Genitive" Gen forms ++
      caseRow "Ablative" Abl forms ++
      caseRow "Instrumental" Inst forms ++
      caseRow "Comitative" Com forms ++
      caseRow "Directional" Dir forms) ;

  caseRow : Str -> RCase -> (RCase => Str) -> Str = \label,c,forms ->
    tr (th label ++ td (forms ! c)) ;

  clauseRow : Str -> ClTense -> Anteriority ->
              {s : ClTense => Anteriority => Polarity => SType => Str} -> Str =
    \label,t,a,cl ->
      tr (th label ++ th (anteriorityName a) ++
          td (cl.s ! t ! a ! Pos ! Main) ++
          td (cl.s ! t ! a ! Neg ! Main)) ;

  anteriorityName : Anteriority -> Str = \a -> case a of {
    Simul => "simultaneous" ;
    Anter => "anterior"
    } ;

  verbInflection : (VerbForm => Str) -> {t : Str; s1,s2,s3 : Str} = \forms -> {
    t="v" ;
    s1=heading1 "Verb" ;
    s2=verbTable forms ;
    s3=[]
    } ;

  verbTable : (VerbForm => Str) -> Str = \forms ->
    heading2 "Indicative" ++
    frameTable (
      tr (th "Voice" ++ th "Aspect" ++ th "Present perfect" ++
          th "Completed past" ++ th "Indefinite past" ++ th "Future") ++
      finiteRows "Active" Act forms ++
      finiteRows "Causative" Caus forms ++
      finiteRows "Passive" Pass forms ++
      finiteRows "Cooperative" Comm forms ++
      finiteRows "Adversative" Advs forms) ++
    verbImperativeTable forms ++
    heading2 "Subordinate forms" ++
    frameTable (
      tr (th "Function" ++ th "Active" ++ th "Causative" ++ th "Passive") ++
      subordinateRow "Conditional" Condl forms ++
      subordinateRow "Concessive" Conc forms ++
      subordinateRow "Immediate succession" ISucc forms ++
      subordinateRow "Logical succession" LSucc forms ++
      subordinateRow "Intention" Intend forms ++
      subordinateRow "Limit" Limit forms ++
      subordinateRow "Progressive" Progr forms ++
      subordinateRow "Succession" Succ forms) ++
    verbCoordinationTable forms ++
    verbParticipleTables forms ;

  finiteRows : Str -> Voice -> (VerbForm => Str) -> Str = \voice,v,forms ->
    finiteRow voice "Simple" v Simpl forms ++
    finiteRow "" "Quick" v Quick forms ++
    finiteRow "" "Collective" v Coll forms ++
    finiteRow "" "Complete" v Compl forms ;

  finiteRow : Str -> Str -> Voice -> Aspect -> (VerbForm => Str) -> Str =
    \voice,aspect,v,a,forms ->
      tr (th voice ++ th aspect ++
          td (forms ! VFORM v a VPresPerf) ++
          td (forms ! VFORM v a VPastComp) ++
          td (forms ! VFORM v a VPastIndef) ++
          td (forms ! VFORM v a VFut)) ;

  subordinateRow : Str -> Subordination -> (VerbForm => Str) -> Str = \label,sub,forms ->
    tr (th label ++
        td (forms ! SVDS ActSub sub) ++
        td (forms ! SVDS CausSub sub) ++
        td (forms ! SVDS PassSub sub)) ;

  auxTable : (VVVerbForm => Str) -> Str = \forms ->
    heading2 "Indicative" ++
    frameTable (
      tr (th "Present perfect" ++ td (forms ! VVTense VPresPerf)) ++
      tr (th "Completed past" ++ td (forms ! VVTense VPastComp)) ++
      tr (th "Indefinite past" ++ td (forms ! VVTense VPastIndef)) ++
      tr (th "Future" ++ td (forms ! VVTense VFut))) ++
    auxImperativeTable forms ++
    heading2 "Subordinate forms" ++
    frameTable (
      auxSubordinateRow "Conditional" Condl forms ++
      auxSubordinateRow "Concessive" Conc forms ++
      auxSubordinateRow "Immediate succession" ISucc forms ++
      auxSubordinateRow "Logical succession" LSucc forms ++
      auxSubordinateRow "Intention" Intend forms ++
      auxSubordinateRow "Limit" Limit forms ++
      auxSubordinateRow "Progressive" Progr forms ++
      auxSubordinateRow "Succession" Succ forms) ++
    auxCoordinationTable forms ++
    auxParticipleTables forms ;

  auxSubordinateRow : Str -> Subordination -> (VVVerbForm => Str) -> Str = \label,sub,forms ->
    tr (th label ++ td (forms ! VVSubordination sub)) ;

  verbImperativeTable : (VerbForm => Str) -> Str = \forms ->
    heading2 "Imperative" ++
    frameTable (
      tr (th "Function" ++ th "Direct" ++ th "Indirect") ++
      verbImperativeRow "Decision" Dec forms ++
      verbImperativeRow "Command" Command forms ++
      verbImperativeRow "Request" Req forms ++
      verbImperativeRow "Demand" Demand forms ++
      verbImperativeRow "Admonition" Admon forms ++
      verbImperativeRow "Appeal" Appeal forms ++
      verbImperativeRow "Permission" Perm forms ++
      verbImperativeRow "Hope" Hope forms ++
      verbImperativeRow "Blessing" Bless forms) ;

  verbImperativeRow : Str -> Imperative -> (VerbForm => Str) -> Str = \label,i,forms ->
    tr (th label ++ td (forms ! VIMP Direct i) ++
        td (forms ! VIMP Indirect i)) ;

  auxImperativeTable : (VVVerbForm => Str) -> Str = \forms ->
    heading2 "Imperative" ++
    frameTable (
      tr (th "Function" ++ th "Direct" ++ th "Indirect") ++
      auxImperativeRow "Decision" Dec forms ++
      auxImperativeRow "Command" Command forms ++
      auxImperativeRow "Request" Req forms ++
      auxImperativeRow "Demand" Demand forms ++
      auxImperativeRow "Admonition" Admon forms ++
      auxImperativeRow "Appeal" Appeal forms ++
      auxImperativeRow "Permission" Perm forms ++
      auxImperativeRow "Hope" Hope forms ++
      auxImperativeRow "Blessing" Bless forms) ;

  auxImperativeRow : Str -> Imperative -> (VVVerbForm => Str) -> Str = \label,i,forms ->
    tr (th label ++ td (forms ! VVImp Direct i) ++
        td (forms ! VVImp Indirect i)) ;

  verbCoordinationTable : (VerbForm => Str) -> Str = \forms ->
    heading2 "Coordinating forms" ++
    frameTable (
      tr (th "Simultaneous" ++ td (forms ! CVDS Simul)) ++
      tr (th "Anterior" ++ td (forms ! CVDS Anter))) ;

  auxCoordinationTable : (VVVerbForm => Str) -> Str = \forms ->
    heading2 "Coordinating forms" ++
    frameTable (
      tr (th "Simultaneous" ++ td (forms ! VVCoordination Simul)) ++
      tr (th "Anterior" ++ td (forms ! VVCoordination Anter))) ;

  verbParticipleTables : (VerbForm => Str) -> Str = \forms ->
    verbParticipleTable "Indefinite present" PPresIndef forms ++
    verbParticipleTable "Progressive present" PPresProgr forms ++
    verbParticipleTable "Past" PPast forms ++
    verbParticipleTable "Future" PFut forms ++
    verbParticipleTable "Agent" PDoer forms ;

  verbParticipleTable : Str -> Participle -> (VerbForm => Str) -> Str = \title,p,forms ->
    heading2 (title ++ " participle") ++
    frameTable (
      tr (th "Case" ++ th "Form") ++
      verbParticipleRow "Nominative" Nom p forms ++
      verbParticipleRow "Accusative" Acc p forms ++
      verbParticipleRow "Dative-locative" Dat p forms ++
      verbParticipleRow "Genitive" Gen p forms ++
      verbParticipleRow "Ablative" Abl p forms ++
      verbParticipleRow "Instrumental" Inst p forms ++
      verbParticipleRow "Comitative" Com p forms ++
      verbParticipleRow "Directional" Dir p forms) ;

  verbParticipleRow : Str -> RCase -> Participle -> (VerbForm => Str) -> Str =
    \label,c,p,forms -> tr (th label ++ td (forms ! VPART p c)) ;

  auxParticipleTables : (VVVerbForm => Str) -> Str = \forms ->
    auxParticipleTable "Indefinite present" PPresIndef forms ++
    auxParticipleTable "Progressive present" PPresProgr forms ++
    auxParticipleTable "Past" PPast forms ++
    auxParticipleTable "Future" PFut forms ++
    auxParticipleTable "Agent" PDoer forms ;

  auxParticipleTable : Str -> Participle -> (VVVerbForm => Str) -> Str = \title,p,forms ->
    heading2 (title ++ " participle") ++
    frameTable (
      tr (th "Case" ++ th "Form") ++
      auxParticipleRow "Nominative" Nom p forms ++
      auxParticipleRow "Accusative" Acc p forms ++
      auxParticipleRow "Dative-locative" Dat p forms ++
      auxParticipleRow "Genitive" Gen p forms ++
      auxParticipleRow "Ablative" Abl p forms ++
      auxParticipleRow "Instrumental" Inst p forms ++
      auxParticipleRow "Comitative" Com p forms ++
      auxParticipleRow "Directional" Dir p forms) ;

  auxParticipleRow : Str -> RCase -> Participle -> (VVVerbForm => Str) -> Str =
    \label,c,p,forms -> tr (th label ++ td (forms ! VVParticiple p c)) ;

}
