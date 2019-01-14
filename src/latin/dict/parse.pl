use strict;
use warnings;
use Data::Dumper;
use feature ":5.10";

my %genders = (F => "feminine" , M => "masculine" , N => "neuter" , C => "COMMON" ,  X => "UNKNOWN" ) ;
my %cases = (ABL => "Abl" , ACC => "Acc", DAT => "Dat", GEN => "Gen", NOM => "Nom" ) ; 

my $wordform = "[\\w()/.\\s-]+" ;
my $comment = "(\\[.*)" ;
my $gender = "(F|M|N|C|X)";
my $vtype = "(TRANS|INTRANS|DEP|SEMIDEP|IMPERS)";
my $case = "(ABL|ACC|DAT|GEN|NOM|VOC|LOC)" ;
my $number = "(S|P)" ;
my $declension = "\\((1st|2nd|3rd|4th|5th)\\)";
my $interjection = "#($wordform)\\s+INTERJ\\s+$comment";
my $adverb = "#(($wordform,\\s+)*$wordform)\\s+ADV\\s+$comment";
my $adjective = "#(($wordform,\\s+)*$wordform)\\s+$gender\\s+ADJ\\s+$comment";
my $adjective2 = "#(($wordform,\\s+)*$wordform)\\s+ADJ\\s+$comment";
my $adjective3 = "#(($wordform,\\s+)*$wordform)\\s+ADJ\\s+[1-3]\\s+[1-3]\\s+$case\\s+$number\\s+$gender\\s+POS\\s+$comment";
my $conjunction = "#($wordform)\\s+CONJ\\s+$comment";
my $noun = "#(($wordform,\\s+)*$wordform)\\s+N\\s+$gender\\s+$comment";
my $noun2 = "#(($wordform,\\s+)*$wordform)\\s+N\\s+$declension\\s+$gender\\s+$comment";
my $noun3 = "#(($wordform,\\s+)*$wordform)\\s+N\\s+[0-9]\\s+[0-9]\\s+$case\\s+$number\\s+$gender\\s+$comment";
my $prep = "#($wordform)\\s+PREP\\s+$case\\s+$comment";
my $verb = "#(($wordform,\\s+)*$wordform)\\s+V\\s+$comment";
my $verb2 = "#(($wordform,\\s+)*$wordform)\\s+V\\s+$declension\\s+$comment";
my $verb3 = "#(($wordform,\\s+)*$wordform)\\s+V\\s+$vtype\\s+$comment";
my $verb4 = "#(($wordform,\\s+)*$wordform)\\s+V\\s+$declension\\s+$vtype\\s+$comment";
my $verb5 = "#(($wordform,\\s+)*$wordform)\\s+V\\s+$declension\\s+$case\\s+$comment";

my $absheader = "--# -path=.:..\n" .
    "abstract DictLatAbs = Cat,Structural-[here_Adv],Lexicon-[leg_N,man_N,do_V2] ** {\n" .
    "-- extracted from http://archives.nd.edu/whitaker/dictpage.htm\n" . 
    "  fun";
my $concheader = "--# -path=.:..\n" . 
    "concrete DictLat of DictLatAbs = CatLat,StructuralLat-[here_Adv],LexiconLat-[leg_N,man_N,do_V2] ** open Prelude, ParadigmsLat, ResLat, ExtraLat in {\n" .
    "-- extracted from http://archives.nd.edu/whitaker/dictpage.htm\n" .
    "  lin";
my $footer = "}";

open(my $absfh, ">", "DictLatAbs.gf") or die "Can't open > DictLatAbs.gf: $!";
open(my $concfh, ">", "DictLat.gf") or die "Can't open > DictLatAbs.gf: $!";

my %conclines ;
my %abslines ;

my @blacklist = ["multi_N","multae_N","alisalius_A","alius_A","semibos_A","semivir_A"];

print $absfh "$absheader\n";
print $concfh "$concheader\n";

while (my $l = <>) {
    chomp($l);
    my @wfs;
    my $cat;
    my $absname;
    if ($l =~ /$interjection/)
    {
    	# print "INTERJ Word: $1 -- Comment: $2\n";
    	my $wf = $1 ;
    	my $comment = $2 ;
    	$wf =~ s/\s$//g;
    	$cat = "Interj" ;
    	$absname = $wf."_Interj" ;
    	push @wfs, $wf;
    	$conclines{$absname} = "    $absname = ss \"$wf\" ; -- $comment \n" ;
    }
    elsif ($l =~ /$adverb/)
    {
    	# print "ADV Word: $1 -- Comment: $3\n";
    	my $wf = $1 ; # all word forms in one string
    	my $comment = $3;
    	@wfs = split /\s*,\s+/, $1; # word forms split into a list
    	map { s/\s$// } @wfs;
    	$cat = "Adv" ;
    	$absname = $wfs[0]."_Adv" ;
    	if (@wfs == 1)
    	{
    	    $conclines{$absname} = "    $absname = mkAdv \"".$wfs[0]."\" ; -- $comment\n" ;
    	}
	elsif (@wfs == 2)
	{
	    $conclines{$absname} = "    $absname =mkAdv \"".$wfs[0]."\" \"".$wfs[1]."\" ; -- $comment\n" ;
	}
	elsif (@wfs == 3)
	{
	    $conclines{$absname} = "    $absname =mkAdv \"".$wfs[0]."\" \"".$wfs[1]."\" \"".$wfs[2]."\" ; -- $comment\n" ;
	}
    }
    elsif ($l =~ /$adjective/)
    {
    	my $wf = $1 ; # all word forms in one string
    	my $comment = $3;
    	@wfs = split /\s*,\s+/, $1; # word forms split into a list
    	map { s/[\s.]$// } @wfs;
    	$cat = "A" ;
    	$absname = $wfs[0]."_A" ;
    	$conclines{$absname} = "    $absname = mkA \"".$wfs[0]."\" ; -- $comment\n" ;
    	# print "A1 Word: $1 -- Comment: $3\n"; # these adjectives should always work
    }
    elsif ($l =~ /$adjective2/)
    {
    	# print "A2 Word: $1 -- Comment: $3\n";
    	my $wf = $1 ; # all word forms in one string
    	my $comment = $3;
    	@wfs = split /\s*[,-]\s*/, $1; # word forms split into a list
    	map { s/[\s.]$// } @wfs;
    	# Word forms contain variations -> Ignore
    	#print $absfh "    ".$wfs[0]."_A : A ;\n" ;
    	$cat = "A" ;
    	$absname = $wfs[0]."_A" ;
	if ($absname ~~ @blacklist) {
	    $conclines{$absname} = "-- BLACKLISTED $absname : A2 $wf -- blacklisted -- $comment\n" ;
	}
    	# Wordform contains variants -> to be fixed
    	elsif ($wf =~ /[(\/\(\))]|(\s-)/ )
    	{
	    if ($wfs[1] eq "(gen.)")
	    {
		if (@wfs == 3)
		{
		    $conclines{$absname} = "    $absname = mkA \"".$wfs[0]."\" \"".$wfs[2]."\"; -- $comment\n" ;
		}
		else 
		{
		    $conclines{$absname} = "-- TODO $absname : A2 $wf -- $comment\n" ;
		}
		
	    }
	    elsif ($wfs[1] =~ /\(gen\.\)/)
	    {
		$wfs[1] =~ s/ \(gen\.\)//g;
		$conclines{$absname} = "    $absname = mkA \"".$wfs[0]."\" \"".$wfs[1]."\"; -- $comment\n" ;
	    }
	    else
	    {
		$conclines{$absname} = "-- SLASHSTUFF $absname : A2 $wf -- $comment\n" ;
	    }
    	}
    	# Abbreviations -> Ignore
    	elsif ($wf =~ /abbr?\./) {
    	    $conclines{$absname} = "-- IGNORED $absname : A2 $wf -- $comment\n" ;
    	}
    	# Undefined -> Ignore
    	elsif ($wf =~ /undecl/) {
    	    $conclines{$absname} = "-- IGNORED $absname : A2 $wf -- $comment\n" ;
    	}
    	# Two word forms, just use both
    	elsif (@wfs == 2)
    	{
    	    $conclines{$absname} = "    $absname = mkA \"".$wfs[0]."\" \"".$wfs[1]."\"; -- $comment\n" ;
    	}
    	# Three word forms -> regular adjective?!?
    	elsif (@wfs == 3)
    	{
	    $conclines{$absname} = "    $absname = mkA \"".$wfs[0]."\" \"".$wfs[1]."\" \"".$wfs[2]."\" ; -- $comment\n" ;
    	}
    	# Too many word forms
    	else
    	{
    	    $conclines{$absname} = "-- TOOMUCH $absname : A2 $wf -- $comment\n" ;
    	    # print "A2 Word: ".$wfs[0]." -- $wf -- Comment: $comment\n" ;
    	}
    }
    elsif ($l =~ /$adjective3/)
    {
	# Overly specific forms to be ignored
	$cat = "" ;
    }
    elsif ($l =~ /$conjunction/)
    {
    	# print "CONJ Word: $1 -- Comment: $2\n";
    	my $wf = $1 ;
    	my $comment = $2;
    	$wf =~ s/\s$//g;
    	$cat = "Conj" ;
    	$absname = $wf."_Conj" ;
    	push @wfs, $wf;
    	# We don't get enough information for our grammar from the lexicon
    	$conclines{$absname} = "-- $absname : Conj $wf -- $comment\n" ;
	
    }
    elsif ($l =~ /$noun/)
    {
    	my $wf = $1 ;
    	my $gender = $3;
    	my $comment = $4;
    	@wfs = split /\s*,\s+/, $1; # word forms split into a list
    	$wfs[0] =~ s/[\.\s]//g; # remove punctuation from word form
    	$cat = "N" ;
    	$absname = $wfs[0]."_N" ;
    	# Blacklisted, not handled yet
	if ($absname ~~ @blacklist) {
	    $conclines{$absname} = "-- BLACKLISTED $absname : N1 $wf -- Gender: $gender -- Comment: $comment\n" ;
	}
	# strange special characters -> ingnored
    	elsif ($wf =~ /[\/\(\)]/ )
    	{
    	    $conclines{$absname} = "-- TODO $absname : N1 $wf -- $gender -- $comment\n" ;
    	}
    	# Wordforms contain "undecl" -> Ignored
    	elsif ($wf =~ /undecl/ )
    	{
    	    $conclines{$absname} = "-- IGNORED $absname : N1 $wf -- $gender -- $comment\n" ;
    	}
    	# Wordforms contains abbreviation -> Ignored
    	elsif ($wf =~ /abb(r?)\./)
    	{
    	    $conclines{$absname} = "-- IGNORED $absname : N1 $wf -- $gender -- $comment\n" ;
    	}
    	elsif(scalar(@wfs) == 1)
    	{
    	    $absname = $wfs[0]."_".$gender."_N" ;
    	    $conclines{$absname} = "    $absname = mkN \"".$wfs[0]."\" ; -- $comment\n" ;
    	}
    	elsif(scalar(@wfs) == 2)
    	{
    	    my $g = $genders{$gender};
    	    if ($gender eq "C")
    	    {
    		$absname = $wfs[0]."_F_N" ;
    		$abslines{$absname} = $cat ;
    		$conclines{$absname} = "    $absname = mkN \"".$wfs[0]."\" \"".$wfs[1]."\" feminine ; -- $comment\n" ;
    		$absname = $wfs[0]."_M_N" ;
    		$conclines{$absname} = "    $absname = mkN \"".$wfs[0]."\" \"".$wfs[1]."\" masculine ; -- $comment\n" ;
    	    }
    	    # Unknown gender
    	    elsif ($gender eq "X")
    	    {
    		$conclines{$absname} = "-- $absname : N1 $wf -- Gender: $gender -- Comment: $comment\n" ;
    	    }
    	    else
    	    {
    		$absname = $wfs[0]."_".$gender."_N" ;
    		$conclines{$absname} = "    $absname = mkN \"".$wfs[0]."\" \"".$wfs[1]."\" $g ; -- $comment\n" ;
    	    }
    	}
    	else
    	{
    	    $absname = $wfs[0]."_N";
    	    $conclines{$absname} = "-- $absname : N1 $wf -- Gender: $gender -- Comment: $comment\n" ;
    	    # print "N1 Word: ".$wfs[0]." -- ".scalar(@wfs)." -- Gender: $gender -- Comment: $comment\n" ;
    	}
	    
    }
    elsif ($l =~ /$noun2/)
    {
    	my $wf = $1 ;
    	my $gender = $4;
    	my $declension = $3;
    	my $comment = $5;
     	$cat = "N" ;
     	@wfs = split /\s*,\s+/, $1; # word forms split into a list
    	$absname = $wfs[0]."_".$gender."_N" ;
	# Blacklisted
	if ($absname ~~ @blacklist) {
	    $conclines{$absname} = "-- BLACKLISTED $absname : N1 $wf -- Gender: $gender -- Comment: $comment\n" ;
	}
	elsif ($declension eq "1st" || $declension eq "2nd")
    	{
    	    $conclines{$absname} = "    $absname = mkN \"".$wfs[0]."\" ; -- $comment\n" ;
    	}
    	elsif (scalar(@wfs) == 2)
    	{
    	    my $g = $genders{$gender};
    	    if ($gender eq "C")
    	    {
    		$absname = $wfs[0]."_F_N" ;
    		$abslines{$absname} = $cat ;
    		$conclines{$absname} = "    $absname = mkN \"".$wfs[0]."\" \"".$wfs[1]."\" feminine ; -- $comment\n" ;
    		$absname = $wfs[0]."_M_N" ;
    		$conclines{$absname} = "    $absname = mkN \"".$wfs[0]."\" \"".$wfs[1]."\" masculine ; -- $comment\n" ;
    	    }
    	    # Unknown gender
    	    elsif ($gender eq "X")
    	    {
    		$conclines{$absname} = "-- $absname : N1 $wf -- Gender: $gender -- Comment: $comment\n" ;
    	    }
    	    else
    	    {
    		$absname = $wfs[0]."_".$gender."_N" ;
    		$conclines{$absname} = "    $absname = mkN \"".$wfs[0]."\" \"".$wfs[1]."\" $g ; -- $comment\n" ;
    	    }
    	}
    	else
    	{
    	    $conclines{$absname} = "-- $absname : N2 $wf -- Gender: $gender -- Declension: $declension -- Comment: $comment\n";
    	    # print "N2 Word: ".$wfs[0]." -- Gender: $gender -- Declension: $declension -- Comment: $comment\n";
    	}
    }
    elsif ($l =~ /$noun3/)
    {
    	# Overly specific
	$cat = "" ;
    }
    # Has to be after nouns to avoid problems with N ADJ
    elsif ($l =~ /$prep/)
    {
    	my $wf = $1 ;
    	my $case = $2 ;
    	my $comment = $3 ; 
    	$wf =~ s/\s//g;
    	push @wfs, $wf;
    	# print "PREP Word: $wf -- Case: $case -- Comment: $comment\n";
    	my $c = $cases{$case};
    	$cat = "Prep" ;
    	$absname =  $wfs[0]."_".$c."_Prep" ;
    	$conclines{$absname} = "    $absname = mkPrep \"".$wfs[0]."\" $c ; -- $comment\n" ;
    }
    elsif ($l =~ /$verb/)
    {
    	my $wf = $1 ;
    	my $comment = $3 ; 
    	$cat = "V" ;
    	@wfs = split /\s*,\s+/, $wf ;
    	$absname = $wfs[0]."_V";
	# Undefined -> Ignore
	if ($absname ~~ @blacklist) {
	    $conclines{$absname} = "-- BLACKLISTED $absname : V1 $wf -- Comment: $comment\n" ;
	}
	# strange special characters -> ingnored
    	elsif ($wf =~ /[(\/\(\))]|(\s-)/ )
    	{
    	    $conclines{$absname} = "-- TODO $absname : V1 $wf -- $comment\n" ;
    	}
	elsif ($wf =~ /undecl/) {
    	    $conclines{$absname} = "-- IGNORED $absname : V1 $wf -- $comment\n" ;
    	}
	elsif ($wf =~ /additional/ && $wf =~ /forms/) {
    	    $conclines{$absname} = "-- IGNORED $absname : V1 $wf -- $comment\n" ;
    	}
	elsif (@wfs == 4)
	{
	    $conclines{$absname} = "    $absname = mkV \"".$wfs[1]."\" \"".$wfs[0]."\"  \"".$wfs[2]."\"  \"".$wfs[3]."\" ; -- Comment: $comment\n" ;
	}
	else
	{
	    print "V1 Word: ".$wfs[0]." -- Comment: $comment\n";
	}
    }
    elsif ($l =~ /$verb2/)
    {
     	my $wf = $1 ;
	my $declension = $3 ;
     	my $comment = $4 ; 
	$cat = "V" ;
     	@wfs = split /\s*,\s+/, $1;
	$absname = $wfs[0]."_V";
	if ($absname ~~ @blacklist) {
	    $conclines{$absname} = "-- BLACKLISTED $absname : V2 $wf -- Declension: $declension -- Comment: $comment\n" ;
	}
	# strange special characters -> ingnored
    	elsif ($wf =~ /[(\/\(\))]|(\s-)/ )
    	{
    	    $conclines{$absname} = "-- TODO $absname : V2 $wf -- Declension: $declension -- Comment: $comment\n" ;
    	}
	elsif ($wf =~ /undecl/) {
    	    $conclines{$absname} = "-- IGNORED $absname : V2 $wf -- Declension: $declension -- Comment: $comment\n" ;
    	}
     	if ($declension eq "1st" || $declension eq "2nd")
     	{
     	    $conclines{$absname} = "    $absname = mkV \"".$wfs[1]."\" ; -- $comment\n" ;
     	}
     	elsif (scalar(@wfs) == 4)
     	{ 
     	    if (!$wf =~ /-/)
     	    {
     		$conclines{$absname} = "    $absname = mkV2 (mkV \"".$wfs[1]."\" \"".$wfs[0]."\" \"".$wfs[2]."\" \"".$wfs[3]."\") ; -- $comment\n" ;
     	    }
     	    elsif ($wfs[2] eq "-" && $wfs[3] eq "-") 
     	    {
     		$conclines{$absname} = "    $absname = mkV \"".$wfs[1]."\" \"".$wfs[0]."\" nonExist nonExist ; -- $comment\n" ;
     	    }
    	    elsif ($wfs[3] eq "-") 
    	    {
    		$conclines{$absname} = "    $absname = mkV2 (mkV \"".$wfs[1]."\" \"".$wfs[0]."\" \"".$wfs[2]."\" nonExist ; -- $comment\n" ;
    	    }
     	}
     	# print "V2 Word: ".$wfs[0]." -- Declension: $declension -- Comment: $comment\n";
    }
    elsif ($l =~ /$verb3/)
    {
    	my $wf = $1 ;
    	my $type = $3 ;
    	my $comment = $4 ; 
    	$cat = "V" ;
    	@wfs = split /\s*,\s+/, $1 ;
    	if ($type eq "TRANS")
    	{
    	    $cat = "V2" ;
    	}
    	elsif ($type eq "IMPERS")
    	{
    	    $cat= "V0" ;
    	}
	else
    	{
    	    $cat= "V" ;
    	}
    	@wfs = split /\s*,\s+/, $1;
    	$absname = $wfs[0]."_".$cat ;
    	if ($wf =~ /undecl/)
    	{
    	    $conclines{$absname} = "-- IGNORED $absname : V3 $wf -- Type: $type -- Comment: $comment\n" ;
    	}
    	else
    	{
	    if ($cat eq "V")
	    {
		$conclines{$absname} = "    $absname = mkV \"".$wfs[1]."\" ; -- $comment\n" ;
	    }
	    elsif ($cat eq "V0")
	    {
		$conclines{$absname} = "    $absname = mkV0 \"".$wfs[0]."\" ; -- $comment\n" ;
	    }
	    else
	    {
		$conclines{$absname} = "    $absname = mkV2 (mkV \"".$wfs[1]."\") ; -- $comment\n" ;
	    }
    	    # print "V3 Word: ".$wfs[0]." -- VType: $type -- Comment: $comment\n";
    	}
	$cat = "V" if $cat eq "V0";
    }
    elsif ($l =~ /$verb4/)
    {
    	my $wf = $1 ;
    	my $declension = $3 ;
    	my $type = $4 ;
    	my $comment = $5 ;
    	if ($type eq "TRANS")
    	{
    	    $cat = "V2" ;
    	}
	elsif ($type eq "IMPERS")
    	{
    	    $cat= "V0" ;
    	}
    	else
    	{
    	    $cat= "V" ;
    	}
    	@wfs = split /\s*,\s+/, $1;
    	$absname = $wfs[0]."_".$cat ;
    	if ($declension eq "1st" || $declension eq "2nd")
    	{
	    if ($cat eq "V")
	    {
		$conclines{$absname} = "    $absname = mkV \"".$wfs[1]."\" ; -- $comment\n" ;
	    }
	    elsif ($cat eq "V0")
	    {
		$conclines{$absname} = "    $absname = mkV0 \"".$wfs[0]."\" ; -- $comment\n" ;
	    }
	    else
	    {
		$conclines{$absname} = "    $absname = mkV2 (mkV \"".$wfs[1]."\") ; -- $comment\n" ;
	    }
    	}
     	elsif (scalar(@wfs) == 4)
     	{
     	    if (!$wf =~ /-/)
     	    {
     		if ($cat eq "V")
		{
		    $conclines{$absname} = "    $absname = mkV \"".$wfs[1]."\" \"".$wfs[0]."\" \"".$wfs[2]."\" \"".$wfs[3]."\"; -- $comment\n" ;
		}
		elsif ($cat eq "V0")
    		{
    		    $conclines{$absname} = "    $absname = mkV0 \"".$wfs[0]."\"; -- $comment\n" ;
    		}
    		else
    		{
    		    $conclines{$absname} = "    $absname = mkV2 (mkV \"".$wfs[1]."\" \"".$wfs[0]."\" \"".$wfs[2]."\" \"".$wfs[3]."\") ; -- $comment\n" ;
    		}
     	    }	
     	}
	$cat = "V" if $cat eq "V0";
    }
    elsif ($l =~ /$verb5/)
    {
    	my $wf = $1 ;
    	my $declension = $3 ;
    	my $case = $4 ;
    	my $comment = $5 ; 
    	$cat = "V2" ;
    	@wfs = split /\s*,\s+/, $1;
    	$absname = $wfs[0]."_".$cat ;
	my $c = $cases{$case};
	if ($declension eq "1st" || $declension eq "2nd")
     	{
     	    $conclines{$absname} = "    $absname = mkV2 (mkV \"".$wfs[1]."\") ".$c."_Prep ; -- $comment\n" ;
     	}
	else
	{
	    $conclines{$absname} = "-- $absname : V4 $wf -- Declension: $declension -- Case: $case -- Comment: $comment\n" ;
	}
    	#print "V5 Word: ".$wfs[0]." -- Declension: $declension -- Case: $case -- Comment: $comment\n";
    }
    else
    {
	$cat = "" ;
	print "MISSING: $l\n";
    }
    if ($cat ne "") {
    	# print $absfh "    ".$wfs[0]."_".$cat." : $cat ;\n" ;
    	$abslines{$absname} = $cat;
    }
}

foreach my $k (sort (keys %abslines))
{
    print $absfh "    $k : ".$abslines{$k}." ;\n" ;
}

foreach my $k (sort (keys %conclines))
{
    given ($k) {
	# when (@blacklist) {
	#     print $concfh "-- ".$conclines{$k};
	# }
	# default {
	    print $concfh $conclines{$k};
	# }
    }	
}
print $absfh "$footer\n";
print $concfh "$footer\n";
close($absfh);
close($concfh);
