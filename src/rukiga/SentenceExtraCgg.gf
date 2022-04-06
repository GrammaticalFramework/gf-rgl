concrete SentenceExtraCgg of SentenceExtra = CatCgg, TenseExtraCgg  ** 
	open Prelude, ResCgg  in {

	

	lin
		UseClExtra temp pol cl = let 
                subj = cl.s;
                vMorphs = mkVerbMorphs;
                clitic = mkSubjClitic cl.subjAgr;
                presSimul =  vMorphs ! VFPres; --this is not delivering the string
                presAnt = vMorphs ! VFPastPart; --this is not delivering the string
                root = cl.root;
                presRestOfVerb = cl.pres;
                pastRestOfVerb = cl.perf; --morphs ! VFPastPart ! RestOfVerb;

                compl = cl.compl

                in
                	case <temp.t, temp.a, Pos> of {
                		 <RemotePast, Performative,Pos>  => case cl.isPresBlank of {
                		 									True => {s = subj ++ clitic ++ "ka" ++ Predef.BIND ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ clitic ++ "ka" ++ Predef.BIND++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                	     <RemotePast, Perfomative,Neg> => case cl.isPerfBlank of {
                	     										True => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ "ra" ++ Predef.BIND++  root ++ Predef.BIND ++ "ire" ++ compl};
                		 										False => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ "ra" ++ Predef.BIND ++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl} 
                	 										};
                	 	 <RemotePast, (Perfect | Resultative),Pos>  => case cl.isPerfBlank of {
                		 									True => {s = subj ++ clitic ++ "kaba" ++ clitic ++ root ++ Predef.BIND ++ "ire" ++ compl};
                		 									False => {s = subj ++ clitic ++ "kaba" ++ clitic ++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                											};
                	     <RemotePast, (Perfect | Resultative),Neg> => case cl.isPerfBlank of {
                	     									True => {s = subj ++ clitic ++ "kaba" ++ clitic ++ "ta" ++ Predef.BIND ++ root ++ Predef.BIND ++ "ire" ++ compl};
                		 									False => {s = subj ++ clitic ++ "kaba" ++ clitic ++ "ta" ++ Predef.BIND ++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                	 										};
                	    <RemotePast, Retrospective, Pos> => case cl.isPerfBlank of {
                	    									True => {s = subj ++ clitic ++ "kaba" ++ clitic ++Predef.BIND ++ "aa" ++ Predef.BIND ++ root ++ Predef.BIND ++ "ire" ++ compl};
                		 									False => {s = subj ++ clitic ++ "kaba" ++ clitic ++Predef.BIND ++ "aa" ++ Predef.BIND ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                											};
                		<RemotePast, Retrospective, Neg> => case cl.isPerfBlank of {
                	    									True => {s = subj ++ clitic ++ "kaba" ++ clitic ++ Predef.BIND++ "ta" ++Predef.BIND ++ "ka" ++ Predef.BIND ++ root ++ Predef.BIND ++ "ire" ++ compl};
                		 									False => {s = subj ++ clitic ++ "kaba" ++ clitic ++ Predef.BIND++ "ta"  ++Predef.BIND ++ "ka" ++ Predef.BIND ++ root ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                											};
                		<RemotePast, Habitual, Pos> => case cl.isPresBlank of{
                											True => {s = subj ++ clitic ++ "kaba" ++ clitic ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ clitic ++ "kaba" ++ clitic ++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<RemotePast, Habitual, Neg> => case cl.isPresBlank of {
                											True => {s = subj ++ clitic ++ "kaba" ++ clitic ++ "ta" ++Predef.BIND ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ clitic ++ "kaba" ++ clitic ++ "ta" ++Predef.BIND ++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<RemotePast, Progressive, Pos> => case cl.isPresBlank of{
                											True => {s = subj ++ clitic ++ "kaba ni" ++ clitic ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ clitic ++ "kaba ni" ++ clitic ++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<RemotePast, Progressive, Neg> => case cl.isPresBlank of{
                											True => {s = subj ++ clitic ++ "kaba" ++ clitic ++ "ta riku" ++Predef.BIND ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ clitic ++ "kaba" ++ clitic ++ "ta riku" ++Predef.BIND ++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<RemotePast, Persistive, Pos> => case cl.isPresBlank of{
                											True => {s = subj ++ clitic ++ "kaba" ++ clitic ++"kyaa" ++Predef.BIND++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ clitic ++ "kaba" ++ clitic ++"kyaa" ++Predef.BIND ++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<RemotePast, Persistive, Neg> => case cl.isPresBlank of{
                											True => {s = subj ++ clitic ++ "kaba" ++ clitic ++ "ta ki" ++Predef.BIND ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ clitic ++ "kaba" ++ clitic ++ "ta ki" ++Predef.BIND ++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<NearPast, Performative,Pos> => case cl.isPerfBlank of {
                											True => {s = subj ++ clitic  ++ root ++ Predef.BIND ++ "ire" ++ compl};
                		 									False => {s = subj ++ clitic ++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                											};
                		<NearPast, Performative,Neg> => case cl.isPerfBlank of {
                											True => {s = subj ++ "ti" ++Predef.BIND ++ clitic ++ root ++ Predef.BIND ++ "ire" ++ compl};
                		 									False => {s = subj ++ "ti" ++ Predef.BIND ++clitic ++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                											};
                		<NearPast, (Perfect |Resultative),Pos> => case cl.isPerfBlank of {
                											True => {s = subj ++ clitic ++ "baire" ++clitic ++ root ++ Predef.BIND ++ "ire" ++ compl};
                		 									False => {s = subj ++ clitic ++ "baire" ++clitic ++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                											};
                		<NearPast, (Perfect |Resultative),Neg> => case cl.isPerfBlank of {
                											True => {s = subj ++ clitic ++ "baire" ++clitic ++ Predef.BIND ++"ta" ++ Predef.BIND++ root ++ Predef.BIND ++ "ire" ++ compl};
                		 									False => {s = subj ++clitic ++ "baire" ++clitic ++ Predef.BIND ++"ta" ++ Predef.BIND++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                											};
                		<NearPast, Retrospective,Pos> => case cl.isPerfBlank of {
                											True => {s = subj ++ clitic ++ "bire" ++clitic ++"aa"++ Predef.BIND++ root ++ Predef.BIND ++ "ire" ++ compl}; --I had already bought
                		 									False => {s = subj ++ clitic ++ "bire" ++clitic ++"aa"++ Predef.BIND++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl} --I had already bought
                											};
                		<NearPast, Retrospective,Neg> => case cl.isPerfBlank of {
                											True => {s = subj ++ clitic ++ "bire" ++clitic ++"ta ka"++ Predef.BIND++ Predef.BIND ++"ta" ++ Predef.BIND++ root ++ Predef.BIND ++ "ire" ++ compl};-- I had not yet bought
                		 									False => {s = subj ++clitic ++ "bire" ++clitic ++"ta ka"++ Predef.BIND++ Predef.BIND ++"ta" ++ Predef.BIND++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl} -- -- I had not yet bought
                											};

                		<(NearPast | MemorialPres|ExpPres|NearFut), Habitual,Pos>  => case cl.isPresBlank of {
                		 									True => {s = subj ++ clitic ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ clitic ++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                	    <(NearPast | MemorialPres|ExpPres|NearFut), Habitual,Neg> => case cl.isPresBlank of {
                	     										True => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++  root ++ Predef.BIND ++ "a" ++ compl};
                		 										False => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl} 
                	 										};
                	 	<NearPast, Progressive, Pos> => case cl.isPresBlank of{
                											True => {s = subj ++ clitic ++ "beire ni" ++ clitic ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ clitic ++ "beire ni" ++ clitic ++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<NearPast, Progressive, Neg> => case cl.isPresBlank of{
                											True => {s = subj ++ clitic ++ "beire" ++ clitic ++ "ta riku" ++Predef.BIND ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ clitic ++ "beire" ++ clitic ++ "ta riku" ++Predef.BIND ++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<NearPast, Persistive, Pos> => case cl.isPresBlank of{
                											True => {s = subj ++ clitic ++ "bire" ++ clitic ++ "kyaa" ++ Predef.BIND ++root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ clitic ++ "bire" ++ clitic ++ "kyaa" ++ Predef.BIND ++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<NearPast, Persistive, Neg> => case cl.isPresBlank of{
                											True => {s = subj ++ clitic ++ "bire" ++ clitic ++ "ta ki" ++Predef.BIND ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ clitic ++ "bire" ++ clitic ++ "ta ki" ++Predef.BIND ++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<MemorialPres, Performative, Pos> => case cl.isPresBlank of {
                		 									True => {s = subj ++ clitic ++ "aa" ++ Predef.BIND ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ clitic ++ "aa" ++ Predef.BIND++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<MemorialPres, Performative, Neg> => case cl.isPresBlank of {
                		 									True => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ "aa" ++ Predef.BIND ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ "aa" ++ Predef.BIND++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<MemorialPres, (Perfect | Resultative), Pos> => case cl.isPerfBlank of {
                											True => {s = subj ++ clitic ++ "aba" ++ clitic ++ root ++ Predef.BIND ++ "ire" ++ compl};
                		 									False => {s = subj ++ clitic ++ "aba" ++ clitic ++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                											};
                		<MemorialPres, (Perfect | Resultative), Neg> => case cl.isPerfBlank of {
                		 									True => {s = subj ++ clitic ++ "aba" ++ clitic ++ "ta" ++ Predef.BIND ++ root ++ Predef.BIND ++ "ire" ++ compl};
                		 									False => {s = subj ++ clitic ++ "aba" ++ clitic ++ "ta" ++ Predef.BIND ++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                											};
                		<MemorialPres, Restrospective, Pos> => case cl.isPerfBlank of {
                											True => {s = subj ++ clitic ++ "aaba" ++ clitic ++"aa" ++ Predef.BIND ++ root ++ Predef.BIND ++ "ire" ++ compl};
                		 									False => {s = subj ++ clitic ++ "aaba" ++ clitic ++"aa" ++ Predef.BIND ++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                											};
                		<MemorialPres, Restrospective, Neg> => case cl.isPerfBlank of {
                		 									True => {s = subj ++ clitic ++ "aaba" ++ clitic ++ "ta ka" ++ Predef.BIND ++ root ++ Predef.BIND ++ "ire" ++ compl};
                		 									False => {s = subj ++ clitic ++ "aaba" ++ clitic ++ "ta ka" ++ Predef.BIND ++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                											};
                		<MemorialPres, Progressive, Pos> => case cl.isPresBlank of{
                											True => {s = subj ++ clitic ++ "aaba ni" ++ clitic ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ clitic ++ "aaba ni" ++ clitic ++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<MemorialPres, Progressive, Neg> => case cl.isPresBlank of{
                											True => {s = subj ++ clitic ++ "aaba" ++ clitic ++ "ta riku"++ Predef.BIND ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ clitic ++ "aaba" ++ clitic ++ "ta riku"++ Predef.BIND++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<MemorialPres, Persistive, Pos> => case cl.isPresBlank of{
                											True => {s = subj ++ clitic ++ "aaba" ++ clitic ++ "kaa"++ Predef.BIND ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ clitic ++ "aaba" ++ clitic ++ "kaa"++ Predef.BIND++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<MemorialPres, Persistive, Neg> => case cl.isPresBlank of{
                											True => {s = subj ++ clitic ++ "aaba" ++ clitic ++ "ta ki"++ Predef.BIND ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ clitic ++ "aaba" ++ clitic ++ "ta ki"++ Predef.BIND++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<ExpPres, Performative,(Pos|Neg)> =>{s="ImpossibleInLanguage"}; -- with GF could allow one to throw Error messages that can be caught
                		<ExpPres, (Perfect |Resultative), Pos> => case cl.isPerfBlank of {
                											True => {s = subj ++ clitic ++ root ++ Predef.BIND ++ "ire" ++ compl};
                		 									False => {s = subj ++ clitic ++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                											};
                		<ExpPres, (Perfect |Resultative), Neg> => case cl.isPerfBlank of {
                											True => {s = subj ++ clitic ++"ti"++Predef.BIND ++ root ++ Predef.BIND ++ "ire" ++ compl};
                		 									False => {s = subj ++ clitic ++"ti"++Predef.BIND ++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                											};
                		<ExpPres, Retrospective, Pos>     => case cl.isPerfBlank of {
                		 									True => {s = subj ++ clitic ++ "naa" ++ Predef.BIND ++ root ++ Predef.BIND ++ "ire" ++ compl};
                		 									False => {s = subj ++ clitic ++ "naa" ++ Predef.BIND++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                											};
                		<ExpPres, Retrospective, Neg>     => case cl.isPerfBlank of {
                		 									True => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ "ka" ++ Predef.BIND ++ root ++ Predef.BIND ++ "ire" ++ compl};
                		 									False => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ "ka" ++ Predef.BIND++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                											};
                		<ExpPres, Progressive, Pos>     => case cl.isPresBlank of {
                		 									True => {s = subj ++ "ni" ++ Predef.BIND ++clitic ++  root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ "ni" ++ Predef.BIND ++ clitic ++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<ExpPres, Progressive, Neg>     => case cl.isPerfBlank of {
                		 									True => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ "ri ku" ++ Predef.BIND ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ "ri ku" ++ Predef.BIND++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<ExpPres, Persistive, Pos>     => case cl.isPerfBlank of {
                		 									True => {s = subj ++ clitic ++ "kaa" ++ Predef.BIND ++  root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj  ++ clitic ++ "kaa" ++ Predef.BIND++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<ExpPres, Persistive, Neg>     => case cl.isPerfBlank of {
                		 									True => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ "ri ku" ++ Predef.BIND ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ "ri ku" ++ Predef.BIND++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<NearFut, Performative, Pos> => case cl.isPresBlank of {
                											True => {s = subj ++ "ni"  ++ Predef.BIND ++clitic ++"za ku" ++ Predef.BIND ++  root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj  ++ "ni"  ++ Predef.BIND ++clitic ++"za ku" ++ Predef.BIND++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		-- Uses the subjunctive e.g a + e = e
                		<NearFut, Performative, Neg>     => case cl.isPerfBlank of {
                		 									True => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ "raa" ++ Predef.BIND ++ root ++ Predef.BIND ++ "e" ++ compl};
                		 									False => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ "raa" ++ Predef.BIND++ root  ++ Predef.BIND ++ presRestOfVerb ++Predef.BIND ++"e" ++ compl} -- my own way of performing the subjuctive i.e a+e =e
                											};
                		<NearFut, (Perfect | Resultative), Pos> => case cl.isPerfBlank of {
                		 									True => {s = subj ++ clitic ++ "raaba" ++ clitic ++  root ++ Predef.BIND ++ "ire" ++ compl};
                		 									False => {s = subj  ++ clitic ++ "raaba" ++ clitic ++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                											};
                		<NearFut, (Perfect | Resultative), Neg> => case cl.isPerfBlank of {
                		 									True => {s = subj ++ clitic ++ "raaba" ++ clitic ++ "ta" ++ Predef.BIND ++  root ++ Predef.BIND ++ "ire" ++ compl};
                		 									False => {s = subj  ++ clitic ++ "raaba" ++ clitic ++ "ta" ++ Predef.BIND ++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                											};
                		<NearFut, Retrospective, Pos> =>case cl.isPerfBlank of { 
                											True => {s = subj ++ "ni"  ++ Predef.BIND ++clitic ++"za kuba" ++ Predef.BIND ++ clitic ++ "aa" ++Predef.BIND ++ root ++ Predef.BIND ++ "ire" ++ compl};
                		 									False => {s = subj  ++ "ni"  ++ Predef.BIND ++clitic ++"za kuba" ++ Predef.BIND ++ clitic ++ "aa" ++Predef.BIND ++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                											};
                		<NearFut, Retrospective, Neg> =>case cl.isPerfBlank of { 
                											True => {s = subj ++ "ni"  ++ Predef.BIND ++clitic ++"za kuba" ++ Predef.BIND ++ clitic ++ "taka" ++Predef.BIND ++ root ++ Predef.BIND ++ "ire" ++ compl};
                		 									False => {s = subj  ++ "ni"  ++ Predef.BIND ++clitic ++"za kuba" ++ Predef.BIND ++ clitic ++ "taka" ++Predef.BIND ++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                											};
                		<NearFut, Progressive, Pos> => case cl.isPresBlank of {
                											True => {s = subj ++ "ni"  ++ Predef.BIND ++clitic ++"za kuba ni" ++ Predef.BIND ++ clitic ++  root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj  ++ "ni"  ++ Predef.BIND ++clitic ++"za kuba ni" ++ Predef.BIND ++ clitic ++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<NearFut, Progressive, Neg> => case cl.isPresBlank of {
                											True => {s = subj ++ "ni"  ++ Predef.BIND ++clitic ++"za kuba"  ++ clitic ++ "tariku" ++ Predef.BIND ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj  ++ "ni"  ++ Predef.BIND ++clitic ++"za kuba"  ++ clitic ++ "tariku" ++ Predef.BIND ++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<NearFut, Persistive, Pos> => case cl.isPresBlank of {
                											True => {s = subj ++ "ni"  ++ Predef.BIND ++clitic ++"za kuba" ++ clitic  ++ "kyaa" ++ Predef.BIND ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj  ++ "ni"  ++ Predef.BIND ++clitic ++"za kuba" ++ clitic  ++ "kyaa" ++ Predef.BIND++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<NearFut, Persistive, Neg> => case cl.isPresBlank of {
                											True => {s = subj ++ "ni"  ++ Predef.BIND ++clitic ++"za kuba"  ++ clitic ++ "taki" ++ Predef.BIND ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj  ++ "ni"  ++ Predef.BIND ++clitic ++"za kuba" ++ clitic ++ "taki" ++ Predef.BIND ++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<RemoteFut, Performative, Pos> => case cl.isPresBlank of {
                		 									True => {s = subj ++ clitic ++ "rya" ++ Predef.BIND ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ clitic ++ "rya" ++ Predef.BIND++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<RemoteFut, Performative, Neg> => case cl.isPresBlank of {
                		 									True => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ "rya" ++ Predef.BIND ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ "ti" ++ Predef.BIND ++ clitic ++ "rya" ++ Predef.BIND++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<RemoteFut, (Perfect | Resultative), Pos> => case cl.isPerfBlank of {
                		 									True => {s = subj ++ clitic ++ "ryaba" ++ clitic ++ Predef.BIND ++ root ++ Predef.BIND ++ "ire" ++ compl};
                		 									False => {s = subj ++ clitic ++ "ryaba" ++ clitic ++ Predef.BIND++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                											};
                		<RemoteFut, (Perfect | Resultative), Neg> => case cl.isPerfBlank of {
                		 									True => {s = subj ++ clitic ++ "ryaba" ++ clitic ++ "ta" ++ Predef.BIND ++ root ++ Predef.BIND ++ "ire" ++ compl};
                		 									False => {s = subj ++ clitic ++ "ryaba" ++ clitic ++ "ta" ++ Predef.BIND++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                											};
                		<RemoteFut, Restrospective, Pos> => case cl.isPerfBlank of {
                		 									True => {s = subj ++ clitic ++ "ryaba" ++ clitic ++ "aa" ++ Predef.BIND ++ root ++ Predef.BIND ++ "ire" ++ compl};
                		 									False => {s = subj ++ clitic ++ "ryaba" ++ clitic ++ "aa"++ Predef.BIND++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                											};
                		<RemoteFut, Restrospective, Neg> => case cl.isPerfBlank of {
                		 									True => {s = subj ++ clitic ++ "ryaba" ++ clitic ++ "taka" ++ Predef.BIND ++ root ++ Predef.BIND ++ "ire" ++ compl};
                		 									False => {s = subj ++ clitic ++ "ryaba" ++ clitic ++ "taka" ++ Predef.BIND++ root  ++ Predef.BIND ++ pastRestOfVerb ++ compl}
                											};
                		<RemoteFut, Habitual, Pos> =>case cl.isPresBlank of {
                		 									True => {s = subj ++ clitic ++ "raa" ++ Predef.BIND ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ clitic ++ "raa" ++ Predef.BIND++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<RemoteFut, Habitual, Neg> =>{s="Non Existent. Can someone suggest?"};
                		<RemoteFut, Progressive, Pos> =>case cl.isPresBlank of {
                		 									True => {s = subj ++ clitic ++ "riba ni" ++ Predef.BIND ++ clitic ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ clitic ++ "riba ni" ++ Predef.BIND ++ clitic ++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<RemoteFut, Progressive, Neg> =>case cl.isPresBlank of {
                		 									True => {s = subj ++ clitic ++ "riba"  ++ clitic ++ "ta riku" ++ Predef.BIND++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ clitic ++ "riba" ++ clitic ++ "ta riku" ++ Predef.BIND ++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<RemoteFut, Persistive, Pos> =>case cl.isPresBlank of {
                		 									True => {s = subj ++ clitic ++ "riba" ++ Predef.BIND ++ clitic ++"kya" ++ Predef.BIND ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ clitic ++ "riba" ++ Predef.BIND ++ clitic ++"kya" ++ Predef.BIND ++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											};
                		<RemoteFut, Persistive, Neg> =>case cl.isPresBlank of {
                		 									True => {s = subj ++ clitic ++ "riba" ++ Predef.BIND ++ clitic ++"taki" ++ Predef.BIND ++ root ++ Predef.BIND ++ "a" ++ compl};
                		 									False => {s = subj ++ clitic ++ "riba" ++ Predef.BIND ++ clitic ++"taki" ++ Predef.BIND ++ root  ++ Predef.BIND ++ presRestOfVerb ++ compl}
                											}

                };

}