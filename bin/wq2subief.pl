#!/usr/bin/perl
#---------------------------------------------------------------------- 
# NOM 
#	wq2subief = transformation d'un fichier "Qualite d'eau" en un systeme
#	            de fichiers pour SUBIEF (dictionnaire, sources FORTRAN ...)
#                   Version compatible : SUBIEF version V3P2
# 
# SYNTAXE
#	wq2subief -h|H (aide)
#	wq2subief [-f] fichier [projet]
#
#---------------------------------------------------------------------- 
#  
# OPTION(S) 
#	-h|H	aide courte ou longue. 
#	-f	forcage: on efface l'ancien projet s'il existe.
#	-a	anglais.
#
# DEPENDANCE
#	commandes UNIX trap,sed,$AWK,sort,grep,comm
#	commandes locales $BIN/extract $BIN/txt2fort
#  
# AUTEURS : J. Gailhard (ENV)     Mail : joel@che31aa.der.edf.fr
#           C. Moulin   (LNH)     Tel  : 30.87.83.81
#                                 Mail : christophe.moulin@der.edf.fr
# Cree   : 22/02/95			
#                                              
#----------------------------------------------------------------------

use Getopt::Std;
use File::Basename;
use File::Copy;
use File::Path;
use Time::Local;


$hostType=`gethosttype`;
if($hostType eq "win") {
    $tmp=$ENV{"TEMP"};
    $ps="\\";  #separateur dans un chemin windows (ps=path separator)
}
else {
    $tmp="/tmp";
    $ps="/";  #separateur dans un chemin unix (ps=path separator)
}

#$ERR=2;  #pas utilise
#$ENV{"PROJECT"};
$BIN=`getproject`;
$BIN="$BIN$ps"."subief2d$ps"."share$ps".""."$hostType$ps"."";
$SEP="[ 	]*=[ 	]*";
$LNG=1;

sub sortie {
    unlink ("$tmp$ps"."xxfile1.$$");
    unlink ("$tmp$ps"."xxfile2.$$");
    unlink ("$tmp$ps"."xxfile3.$$");
    unlink ("$tmp$ps"."xxfile4.$$");
    unlink ("$tmp$ps"."xxfile5.$$");
    unlink ("$tmp$ps"."xxfile6.$$");
    exit;
}

sub ecrilng {
    if($LNG eq 1) {
	printf "$_[0]";
    }
    else {
	printf "$_[1]";
    }
}

sub evallng {
    if($LNG eq 1) {
	return $_[0];
    }
    else {
	return $_[1];
    }
}

sub usage {
    printf "Not yet implemented\n"; #caution
    sortie();
}

sub copyValidLines {
#$_[0]   :in-The file to be seded befor copy.
#$_[1]   :in-The regular expression.
#$_[2]   :in/out-The seded output.
#Definition : Copies file keeping only the lines which match reg exp.
#           Ment to work like : cat file1 | sed -e "/^\[/ !d" > $file2.
#Return     : None

    open(SRC,"<$_[0]");
    $regexp="$_[1]";
    open(DST,">$_[2]");

    while(eof(SRC) == 0) {
	$ligne=<SRC>;
	if($ligne =~ $regexp) {
	    printf DST $ligne;
	}
    }
    close(SRC);
    close(DST);
}

sub copyAndSubstitute {
#$_[0]   :in-The source file.
#$_[1]   :in-The regexp to search for.
#$_[2]   :in-The regexp to replace by.
#$_[3]   :in/out-The destination file.
#Definition : Copies file replacing regexp1 by regexp2.
#           Ment to work like : cat file1 | sed -e "s/regexp1/regexp2/" > file2
#Return     : None.

    open(SRC,"<$_[0]");
    $regexp1="$_[1]";
    $regexp2="$_[2]";
    open(DST,">$_[3]");

    while(eof(SRC) == 0) {
	$ligne=<SRC>;
	$ligne=~s/$regexp1/$regexp2/;
	printf DST $ligne;
    }
    close(SRC);
    close(DST);
}

sub copyAndConcatenBackslash {
#$_[0]   :in-The source file.
#$_[1]   :in/out-The destination file.
#Definition : Copies file concatenating lines seperated by \
#           Ment to work like : cat file1 | sed -e "/\\/ { N; s/\\\n//; }" > file2
#Return     : None.

    open(SRC,"<$_[0]");
    open(DST,">$_[1]");

    while(eof(SRC) == 0) {
	$ligne=<SRC>;
	while($ligne =~ /\\/) {
	    $ligne2=<SRC>;
	    $ligne=~s/\\\n//;
	    $ligne="$ligne$ligne2";
	}
	printf DST $ligne;
    }
    close(SRC);
    close(DST);
    close(TMP);
}

sub extractAndSort {
#$_[0]   : in-The source file.
#$_[1]   : in-The opening extract area character
#$_[2]   : in-The closing extract area character
#$_[3]   : in/out-The destination file.
#Definition : Extracts between c1 and c2 from file and sorts the
#           extracted elements by alphabetical order.
#           Ment to work like : extract file1 c1 c2 | sort > file2
#Return     : None.

    $src=$_[0];
    $c1=$_[1];
    $c2=$_[2];
    open(DST,">$_[3]");

    system("$BIN$ps"."extract $src $c1 $c2 > $src.tmp1");

    open(FILE,"<$src.tmp1");
    @tmpBuffer=<FILE>;
    close(FILE);
    unlink("$src.tmp1");

    @tmpBuffer=sort(@tmpBuffer);
    printf DST "@tmpBuffer";
    close(DST);
}

sub commEmu {
#$_[0]   : in-The first source file.
#$_[1]   : in-The second source file.
#Definition : Compares the two files line by line and displays and creates a list
#           of lines present in file 1 and not in file 2 except spaces and tabs.
#Return     : The list of lines in file1 and not in file2.

    open(FILE1,$_[0]);
    open(FILE2,$_[1]);

    @file1=<FILE1>;
    @file2=<FILE2>;

    close(FILE1);
    close(FILE2);

    $lostList="";
    for($i=0;$i<@file1;$i=$i+1) {
	$found=0;
	for($j=0;$j<@file2;$j=$j+1) {
	    if($file1[$i] eq $file2[$j]) {
		$found=1;
	    }
	}
	if($found==0&&(!$file1[$i]=~/^[ \t]*\n/)) {
	    $lostList="$lostList".$file1[$i];
	}
    }

    return $lostList;
}

sub extractBetweenRegexps {
#$_[0]   : in-The first source file.
#$_[1]   : in-The second source file.
#$_[2]   : in-First colon display.
#Definition : For each line of the file it substitutes regexp1 by nothing and 
#           and does the same with regexp2 on the result.
#Return     : The list of extracted elements.
    $file=$_[0];
    $regexp1=$_[1];
    $regexp2=$_[2];
    open(FILE,$file);
    @vars=<FILE>;
    close(FILE);
    @result=();
    for($i=0;$i<@vars;$i++) {
	$line=$vars[$i];
	$var=$line;
	$var=~s/$regexp1//;
	if($var ne $line) {
	    $var=~s/$regexp2//;
        }
        if($var ne $line) {
	    @result=(@result,$var);
        }
    }
    return @result;
}

sub sortU {
#$_[0]   : in-The list to sort.
#Definition : Sorts the list and keeps only one occurence of the same string.
#Return     : The sorted and cleaned list.
    @list=(@_);
    @list=sort(@list);
    @listOut=();
    $j=0;
    for($i=0;$i<@list;$i++) {
	if(($j==0)||($list[$i] ne $listOut[$j-1])) {
	    $listOut[$j]=$list[$i];
	    $j++;
	}
    }
    return @listOut;
}

sub suppressListFromList {
#$_[0]   : in-The length of the first array.
#$_[1]   : in-The elements of the first array.
#$_[2]   : in-The length of the second array.
#$_[3]   : in-The elements of the second array.
#Definition : It takes out any element of array 1 that appears in array 2
#Return     : The new array.
    $offs=1;
    @list1=();
    @list2=();
    for($i=0;$i<$_[$offs-1];$i++) {
	@list1=(@list1,$_[$i+$offs]);
    }
    $offs=$i+2;
    for($i=0;$i<$_[$offs-1];$i++) {
	@list2=(@list2,$_[$i+$offs]);
    }

    @listOut=();
    for($i=0;$i<@list1;$i++) {
	$found=0;
	for($j=0;$j<@list2;$j++) {
	    if($list1[$i] eq $list2[$j]) {
		$found=1;
	    }
	}
	if($found == 0) {
	    @listOut=(@listOut,$list1[$i]);
	}
    }
    return @listOut;
}

sub suppressLinesNotStartingBy {
#$_[0]   : in-The input file.
#$_[1]   : in-The output file.
#$_[2]   : in-The list of starting strings.
#Definition : Copies all input lines starting with at least one element of list to output.
#Return     : None.
    ($input,$output,$c1,$c2,@list)=(@_);
    open(SRC,$input);
    open(DST,">$output");
 
    while(eof(SRC) == 0) {
	$line=<SRC>;
	$lineCmp=$line;
	$lineCmp=~s/^$c1//;
	$lineCmp=~s/$c2.*//;
	for($i=0;$i<@list;$i++) {
	    if($lineCmp eq $list[$i]) {
		printf DST $line;
	    }
	}
    }
    close(SRC);
    close(DST);
}

sub alaligne {
#$_[0]   : in-The input array.
#Definition : Takes each element of the input array and formats it out to a string, the first line
#           of the string has a maximum length of len1 and all other lines start at len2.
#Return     : the output string.
    ($len1,$len2,@liste)=(@_);

    $liste="";
    $len=0;
    for($i=0;$i<@liste;$i++) {
	$txt=$liste[$i];
	chomp($txt);
	$liste="$liste$txt ";
	$len+=length($txt)+1;
	if($len > $len1) {
	    $tmptxt=sprintf("\n");
	    for($j=0;$j<$len2;$j++) {
		$tmptxt="$tmptxt ";
	    }
	    $liste="$liste$tmptxt";
	    $len=0;
	}
    }
    return $liste;
}

#----------------------------------------------------------------------

$appel=basename("$0");

$forcage=FALSE;
$opt_h=$opt_H=$opt_f=$opt_a="";
getopts("hHaf"); # || "pasoption";	#caution
usage if $opt_h || $opt_H;
if ($opt_f){
    $forcage=TRUE;
}
if ($opt_a){
    $LNG=2;
}

#----------------------------------------------------------------------

if($ARGV[0] eq "") {
    usage;
    sortie();
}

$modele=basename($ARGV[0],".wq");
$fichier=dirname($ARGV[0]).$ps.basename($ARGV[0],".wq").".wq";

if($ARGV[1]) {
    $projet=dirname($ARGV[1]).$ps.basename($ARGV[1],".sub").".sub";
}
else {
    $projet=dirname($ARGV[0]).$ps.basename($ARGV[0],".wq").".sub";
}

$fvar="$tmp$ps"."xxfile1.$$";
$fparm="$tmp$ps"."xxfile2.$$";
$fequa="$tmp$ps"."xxfile3.$$";
$liste1="$tmp$ps"."xxfile4.$$";
$liste2="$tmp$ps"."xxfile5.$$";
#$sfile="$tmp$ps"."xxfile6.$$"; #pas utilise

if(not -f $fichier) {
    if($LNG eq 1) {
	printf "$appel: le fichier '$fichier' n'existe pas\n";
    }
    else {
	printf "$appel: the file '$fichier' does not exist\n";
    }
    sortie();
}

if((-d $projet)) {
    if($forcage ne TRUE) {
	if($LNG eq 1) {
	    printf "$appel: le repertoire '$projet' existe deja\n";
	}
	else {
	    printf "$appel: the directory '$projet' already exists\n";
	}
	sortie();
    }
    else {
	unlink <$projet\\*.*>;
	rmdir($projet);
    }
}
mkdir($projet,0777);

printf "\n";

$txt=evallng("Fichier de Qualite d'eau","Water Quality File");
printf("%-50s : %s\n","$appel> $txt",$fichier);
$txt=evallng("RÅpertoire de QualitÅ d'eau","Water Quality Directory");
printf("%-50s : %s\n","$appel> $txt",$projet);

#----------------------------------------------------------------------
# Identification des trois parties du fichier d'origine :
#   ü <fvar>  : variables d'etat
#   ü <fparm> : parametres du calcul
#   ü <fequa> : equations d'evolution
#----------------------------------------------------------------------

# <fvar>  : lignes commencant par le caractere '[' 
# <fparm> : lignes commencant par le caractere '{'
# <fequa> : lignes commencant pas le caractere ' '

copyValidLines("$fichier","^\\[","$fvar"); #caution
copyValidLines("$fichier","^{","$fparm"); #caution

copyValidLines("$fichier","^ ","$fequa.tmp1");
copyAndSubstitute("$fequa.tmp1","^ *","","$fequa.tmp2");
copyAndSubstitute("$fequa.tmp2","[ \t]*\$","","$fequa.tmp3");

# concatenation de 2 lignes du fichier <fequa> separees par le caractere '\' en une seule

copyAndConcatenBackslash("$fequa.tmp3","$fequa"); #caution

unlink("$fequa.tmp1","$fequa.tmp2","$fequa.tmp3");

#----------------------------------------------------------------------
# Extraction des listes de variables et de parametres et tests de coherence
#----------------------------------------------------------------------

# <liste1> : expressions entre crochets dans <fvar> , triees par ordre alphabetique
# <liste2> : expressions entre crochets dans <fequa>, triees par ordre alphabetique
# {liste3} : expressions presentes dans <fequa> et non dans <fvar> 

extractAndSort($fvar,'[',']',$liste1);
extractAndSort($fequa,'[',']',$liste2);

$lostList=commEmu($liste2,$liste1);
if($lostList ne "") {
    printf "$appel: ***ERREUR*** Les parametres suivants ne sont pas definis :\n";
    printf "$appel: ***ERREUR***\n";
    printf "$lostList";
    sortie();
}

# <liste1> : expressions entre accolades dans <fparm>, triees par ordre alphabetique
# <liste2> : expressions entre accolades dans <fequa>, triees par ordre alphabetique
# {liste3} : expressions presentes dans <fequa> et non dans <fparm> 

extractAndSort($fparm,'{','}',$liste1);
extractAndSort($fequa,'{','}',$liste2);

$lostList=commEmu($liste2,$liste1);
if($lostList ne "") {
    printf "$appel: ***ERREUR*** Les parametres suivants ne sont pas definis :\n";
    printf "$appel: ***ERREUR***\n";
    printf "$lostList";
    sortie();
}

#----------------------------------------------------------------------
# Creation des diverses listes utiles
#   ü {liste_var}  : liste des variables d'etat
#   ü {liste_varc} : liste des variables calculees
#   ü {liste_forc} : liste des fonctions de forcage (fichiers de donnees)
#   ü {liste_fort} : liste des parametres internes (sources FORTRAN)
#   ü {liste_dico} : liste des parametres externes (dictionnaire)
#----------------------------------------------------------------------

# {liste_var} : premiere expression entre crochet dans les lignes de <fequa>
# commencant par les caracteres 'd/dt['

@liste_var=extractBetweenRegexps($fequa,"^d\\/dt\\[","\\].*"); #caution (.* and $)
$n_var=@liste_var; #caution

# {liste_varc} : premiere expression entre crochet dans les lignes de <fequa>
# commencant par les caracteres '[' 

@liste_varc=extractBetweenRegexps($fequa,"^\\[","\\].*"); #caution (.* and $)
$n_varc=@liste_varc; #caution

# creation du nouveau fichier <fvar> ordonne selon l'ordre {liste_var} {liste_varc}

unlink($liste1);
@tmpList=(@liste_var,@liste_varc);


for($i=0;$i<@tmpList;$i++) {
    $line=$tmpList[$i];
    chomp($line);
    copyValidLines($fvar,"^\\[$line\\]","$fvar.tmp1");
    open(SRC,"$fvar.tmp1");
    open(DST,">>$liste1");
    printf DST <SRC>;
    close(DST);
    close(SRC);
}

unlink("$fvar.tmp1");

copy ($liste1,$fvar);
unlink($liste1);

# {liste_forc} : premiere expression entre accolade dans les lignes de <fparm>
# dont la valeur par defaut est du type (f)
@liste_forc=();
open(SRC,$fparm);
while(eof(SRC) == 0 ) {
    $line=<SRC>;
    @fields=split($SEP,$line);
    $fields[0]=~s/[{}]/""/;
    if($fields[2]&&$fields[2]=~/\(f\)/) {
	$tmpLine=$fields[2];
	@liste_forc=(@liste_forc,$tmpLine);
    }
}
$n_forc=@liste_forc;

# {liste_fort} : premiere expression entre accolade dans les lignes de <fequa>,
# valeurs uniques (possibilite de declaration multiple au sein d'un bloc IF THEN ELSE)

@liste_fort=extractBetweenRegexps($fequa,"^{","}.*");
@liste_fort=sortU(@liste_fort);
$n_fort=@liste_fort; #caution

# {liste_dico} : premiere expression entre accolade dans les lignes de <fparm>
# hormis les expressions comprises dans {liste_fort} et dans {liste_forc}
# <sfile> : fichier script sed du type (/^{<expr>}/ d)
@liste_dico=extractBetweenRegexps($fparm,"^{","}.*");
@liste_dico=suppressListFromList(@liste_dico+0,@liste_dico,@liste_fort+0,@liste_fort);
@liste_dico=suppressListFromList(@liste_dico+0,@liste_dico,@liste_forc+0,@liste_forc);
$n_dico=@liste_dico;

$format="%-50s : %s\n";

$txt=evallng("Nombre de variables d'etat","Number of state variables");
printf("$format","$appel> $txt",$n_var);
$txt=evallng("Nombre de variables calculees","Number of computed variables");
printf("$format","$appel> $txt",$n_varc);
$txt=evallng("Nombre de fonctions de forcage","Number of forcing functions");
printf("$format","$appel> $txt",$n_forc);
$txt=evallng("Nombre de parametres internes","Number of internal parameters");
printf("$format","$appel> $txt",$n_fort);
$txt=evallng("Nombre de parametres externes","Number of external parameters");
printf("$format","$appel> $txt",$n_dico);

#----------------------------------------------------------------------
# Creation du dictionnaire et du fichier cas
#----------------------------------------------------------------------

$txt=evallng("Creation du dictionnaire 'dicowq' ...","creating the 'dicowq' dictionnary ...");
printf("%s\n","$appel> $txt");
$txt=evallng("Creation du fichier cas  'caswq'  ...","creating the 'caswq' steering file ...");
printf("%s\n","$appel> $txt");

# <sfile> : fichier script sed du type (/^{<expr>}/ p)
suppressLinesNotStartingBy($fparm,"$fparm.tmp1",'{','}',@liste_dico);

open(DICO,">$projet$ps"."dicowq");
open(CAS,">$projet$ps"."caswq");
open(PARM,"<$fparm.tmp1");
$APOS="'";
$DBLE="''";
if ( $LNG == 1 ) {
    printf DICO ("/ MOTS-CLES SPECIFIQUES AU MODELE %s%s%s\n",$APOS,$modele,$APOS);
    printf CAS ("/ MOTS-CLES MINIMUM DU MODELE %s%s%s\n",$APOS,$modele,$APOS);
}
else {
    printf DICO ("/ SPECIFIC KEYWORDS OF MODEL %s%s%s\n",$APOS,$modele,$APOS);
    printf CAS ("/ MINIMUM KEYWORDS OF MODEL %s%s%s\n\n",$APOS,$modele,$APOS);
}

$i=0;
while(eof(PARM) == 0) {
    $line=<PARM>;
    @fields=split($SEP,$line);
    $fields[0]=~s/[{}]//g;
    $param=$fields[0];
    @fieldsTmp=split(/ *\(.*\)/,$fields[1]);
    $nom=$fieldsTmp[0];
    @fieldsTmp=split("$nom *",$fields[1]);
    if(!$fieldsTmp[1]) {
	$fieldsTmp[1]="(.)";
    }
    $unite=$fieldsTmp[1];
    $nom=~s/[ \t]*\n//;
    $unite=~s/[ \t]*\n//;

    $nomquote=$nom;
    $nom=~s/$APOS/$DBLE/;
    if($fields[2]) {
	$defaut=$fields[2];
    }
    else {
	$defaut="";
    }

    printf DICO ("\n");
    printf DICO ("NOM = %s%s%s\n",$APOS,$nom,$APOS);
    printf DICO ("AIDE = %s%s %s%s\n",$APOS,$param,$unite,$APOS);
    printf DICO ("TYPE = REEL\n");
    printf DICO ("INDEX = %2d\n",++$i);
    if ( $defaut ne "" ) {
	printf DICO ("DEFAUT = %s",$defaut);
    }
    else {
	printf CAS ("%s = \n",$nomquote);
    }
}

close(PARM);
close(DICO);
close(CAS);
unlink("$fparm.tmp1");

#----------------------------------------------------------------------
# Creation du fichier lecdon.h
#----------------------------------------------------------------------

$txt=evallng("Creation du source FORTRAN 'lecdon.h' ...","creating the 'lecdon.h' FORTRAN source ...");
printf ("%s\n","$appel> $txt");
unlink("$projet$ps"."lecdon.h");

# {n_var} variables d'etat
# <sfile> : fichier script sed du type (/^[<expr>]/ p)
if(@liste_var != 0) {
    suppressLinesNotStartingBy($fvar,"$fvar.tmp1",'\[','\]',@liste_var);
    
    open(DST,">$fvar.tmp2");
    open(SRC,"<$fvar.tmp1");
 
    if ( $LNG == 1 ) { 
	printf DST ("C Variables d%setat :\n",$APOS);
	printf DST ("C NOMV(I)  : nom generique de la variable d%setat [I]\n",$APOS);
	printf DST ("C UNITV(I) : unite de la variable d%setat [I]\n",$APOS);
	printf DST ("C CONVT(I) : convection de la variable d%setat [I]\n",$APOS);
	printf DST ("C DIFFT(I) : diffusion de la variable d%setat [I]\n",$APOS);
    }
    else { 
	printf DST ("C State variables :\n",$APOS);
	printf DST ("C NOMV(I)  : name of the state variable [I]\n",$APOS);
	printf DST ("C UNITV(I) : unit of the state variable [I]\n",$APOS);
	printf DST ("C CONVT(I) : advection of the state variable [I]\n",$APOS);
	printf DST ("C DIFFT(I) : dispersion of the state variable [I]\n",$APOS);
    }
    printf DST ("\n");

    $i=0;
    while(eof(SRC) == 0) {
	$i++;
	$line=<SRC>;
	@fields=split($SEP,$line);

	$variable=$fields[0];
	@fieldsTmp=split(/ *\(.*\)/,$fields[1]);
	$nom=$fieldsTmp[0];
	@fieldsTmp=split("$nom *",$fields[1]);
	if(!$fieldsTmp[1]) {
	    $fieldsTmp[1]=".";
	}
	$unite=$fieldsTmp[1];
	$unite=~s/[ \t]*\n|[()]//g;
	printf DST ("NOMV (%2d) = %s%s%s\n",$i,$APOS,$variable,$APOS);
	printf DST ("UNITV(%2d) = %s%s%s\n",$i,$APOS,$unite,$APOS);
	if ( $fields[2]&&$fields[2]=~/!C/ ) {
	    printf DST ("CONVT(%2d) = .FALSE.\n",$i);
	}
	if ( $fields[2]&&$fields[2]=~/!D/ ) {
	    printf DST ("DIFFT(%2d) = .FALSE.\n",$i);
	}
    }
    printf DST ("NTRAC = %2d\n\n",$i);

    close(SRC);
    close(DST);
    system("$BIN$ps"."txt2fort $fvar.tmp2 >> $projet$ps"."lecdon.h");
    unlink("$fvar.tmp2");
    unlink("$fvar.tmp1");
}

# {n_varc} variables calculees
# <sfile> : fichier script sed du type (/^[<expr>]/ p)
if(@liste_varc != 0) {
    suppressLinesNotStartingBy($fvar,"$fvar.tmp1",'\[','\]',@liste_varc);
    
    open(DST,">$fvar.tmp2");
    open(SRC,"<$fvar.tmp1");
 
    if ( $LNG == 1 ) { 
	printf DST ("C Variables calculees :\n");
	printf DST ("C NOMC(I)  : nom generique de la variable calculee [I]\n");
	printf DST ("C UNITC(I) : unite de la variable calculee [I]\n");
    }
    else { 
	printf DST ("C Computed variables :\n");
	printf DST ("C NOMC(I)  : name of the computed variable [I]\n");
	printf DST ("C UNITC(I) : unit of the computed variable [I]\n");
    }
    printf DST ("\n");

    $i=0;
    while(eof(SRC) == 0) {
	$i++;
	$line=<SRC>;
	@fields=split($SEP,$line);

	$variable=$fields[0];
	@fieldsTmp=split(/ *\(.*\)/,$fields[1]);
	$nom=$fieldsTmp[0];
	@fieldsTmp=split("$nom *",$fields[1]);
	if(!$fieldsTmp[1]) {
	    $fieldsTmp[1]=".";
	}
	$unite=$fieldsTmp[1];
	$unite=~s/[ \t]*\n|[()]//g;
	printf DST ("NOMC (%2d) = %s%s%s\n",$i,$APOS,$variable,$APOS);
	printf DST ("UNITC(%2d) = %s%s%s\n",$i,$APOS,$unite,$APOS);
    }
    printf DST ("NCALC = %2d\n\n",$i);

    close(SRC);
    close(DST);
    system("$BIN$ps"."txt2fort $fvar.tmp2 >> $projet$ps"."lecdon.h");
    unlink("$fvar.tmp2");
    unlink("$fvar.tmp1");
}

# {n_forc} fonctions de forcage
# <sfile> : fichier script sed du type (/^{<expr>}/ p)
if(@liste_forc != 0) {
    suppressLinesNotStartingBy($fvar,"$fvar.tmp1",'\[','\]',@liste_varc);
    suppressLinesNotStartingBy($fparm,"$fparm.tmp1",'\[','\]',@liste_forc);

    open(TMPADD,">>$fparm.tmp1");
    open(TMP,"$fvar.tmp1");
    printf TMPADD <TMP>;
    close(TMP);
    close(TMPADD);
    unlink("$fvar.tmp1");

    open(DST,">$fvar.tmp2");
    open(SRC,"<$fparm.tmp1");
 
    if ( $LNG == 1 ) { 
	printf DST ("C Fonctions de forcage :\n");
	printf DST ("C NOMF(I)  : nom generique de la fonction de forcage [I]\n");
	printf DST ("C UNITF(I) : unite de la fonction de forcage [I]\n");
    }
    else {
	printf DST ("C Forcing functions :\n");
	printf DST ("C NOMF(I)  : name of the Forcing function [I]\n");
	printf DST ("C UNITF(I) : unit of the Forcing function [I]\n");
    }
    printf DST ("\n");

    $i=0;
    while(eof(SRC) == 0) {
	$i++;
	$line=<SRC>;
	@fields=split($SEP,$line);

	$variable=$fields[0];
	@fieldsTmp=split(/ *\(.*\)/,$fields[1]);
	$nom=$fieldsTmp[0];
	@fieldsTmp=split("$nom *",$fields[1]);
	if(!$fieldsTmp[1]) {
	    $fieldsTmp[1]=".";
	}
	$unite=$fieldsTmp[1];
	$unite=~s/[ \t]*\n|[()]//g;
	printf DST ("NOMF (%2d) = %s%s%s\n",$i,$APOS,$variable,$APOS);
	printf DST ("UNITF(%2d) = %s%s%s\n",$i,$APOS,$unite,$APOS);
    }
    printf DST ("NFORC = %2d\n\n",$i);

    close(SRC);
    close(DST);
    system("$BIN$ps"."txt2fort $fvar.tmp2 >> $projet$ps"."lecdon.h");
    unlink("$fvar.tmp2");
    unlink("$fvar.tmp1");
}

# DICO : Parametres externes (dictionnaire)
if(@liste_dico != 0) {
    
    open(DST,">$fvar.tmp1");

    printf DST ("C Parametres :\n");

    for($n=0;$n<@liste_dico;$n++) {
	$var[$n]=$liste_dico[$n];
	chomp($var[$n]);
    }

    for($i=0;$i<$n;$i++) {
	printf DST ("C DICO(%2d) : parametre {%s}\n",$i+1,$var[$i]);
    }
    printf DST "\n";

    for($i=0;$i<$n;$i++) {
	printf DST ("DICO(%2d) = MOTREA(%2d)\n",$i+1,$i+1);
    }
    printf DST "\n";

    close(DST);
    system("$BIN$ps"."txt2fort $fvar.tmp1 >> $projet$ps"."lecdon.h");
    unlink("$fvar.tmp1");
}

# <sfile> : fichier script sed du type (s/wVAR/XWC(*)/)
# <sfile> : fichier script sed du type (s/dVAR/VITCD(*)/)
# <sfile> : fichier script sed du type (s/dVAR/VITCE(*)/)

if(@liste_dico != 0) {
    open(DST,">$fvar.tmp1");
    for($i=0;$i<@liste_dico;$i++) {
	$line=$liste_dico[$i];
	for($j=0;$j<@liste_var;$j++) {
	    $line2=$liste_var[$j];
	    $line2="w$line2";
	    if($line eq $line2) {
		$jp=$j+1;
		$c=sprintf ("%2d",$jp);
		$line="XWC  ($c)\n";
	    }
	}
	for($j=0;$j<@liste_var;$j++) {
	    $line2=$liste_var[$j];
	    $line2="d$line2";
	    if($line eq $line2) {
		$jp=$j+1;
		$c=sprintf ("%2d",$jp);
		$line="VITCD($c)\n";
	    }
	}
	for($j=0;$j<@liste_var;$j++) {
	    $line2=$liste_var[$j];
	    $line2="e$line2";
	    if($line eq $line2) {
		$jp=$j+1;
		$c=sprintf ("%2d",$jp);
		$line="VITCE($c)\n";
	    }
	}
	printf DST $line;
    }
    close(DST);
    
    open(SRC,"$fvar.tmp1");
    open(DST,">$fvar.tmp2");
    if ( $LNG == 1 ) {  
	printf DST ("C Parametres hydrodynamiques : \n");
	printf DST ("C XWC  (I) : vitesse de chute de la variable [I]\n");
	printf DST ("C VITCD(I) : vitesse critique de depot de la variable [I]\n");
	printf DST ("C VITCE(I) : vitesse critique d%serosion de la variable [I]\n",$APOS);
    } 
    else {
	printf DST ("C Hydrodynamic parameters : \n");
	printf DST ("C XWC  (I) : settling velocity of State variable [I]\n");
	printf DST ("C VITCD(I) : critical velocity for settling of Variable [I]\n");
	printf DST ("C VITCE(I) : critical velocity for erosion of Variable [I]\n",$APOS);
    }
    printf DST ("\n");

    $i=0;
    while(eof(SRC) == 0) {
	$i++;
	$line=<SRC>;
	chomp($line);
	if($line=~/XWC  \([ 0-9]*\)/) {
	    printf DST ("%s = DICO(%2d)\n",$line,$i);
	}
	if($line=~/VITCD\([ 0-9]*\)/) {
	    printf DST ("%s = DICO(%2d)\n",$line,$i);
	}
	if($line=~/VITCE\([ 0-9]*\)/) {
	    printf DST ("%s = DICO(%2d)\n",$line,$i);
	}
    }

    close(DST);
    close(SRC);
    system("$BIN$ps"."txt2fort $fvar.tmp2 >> $projet$ps"."lecdon.h");
    unlink("$fvar.tmp1");
    unlink("$fvar.tmp2");
}

#----------------------------------------------------------------------
# Creation du fichier chimie.h
#----------------------------------------------------------------------

$txt=evallng("Creation du source FORTRAN 'chimie.h' ...","creating the 'chimie.h' FORTRAN source ...");
printf ("%s\n","$appel> $txt");

unlink("$projet$ps"."chimie.h");

open(SOURCE,">$projet$ps"."chimie.h");


# CS : Variable d'etat, DCHIM : variation due aux echanges
# <sfile> : fichier script sed du type (s/[<expr>]/CS(I,*)/g)
if(@liste_var != 0) {
    for($i=0;$i<@liste_var;$i++) {
	$line=$liste_var[$i];
	chomp($line);
	printf SOURCE ("C CS(*,%2d) : variable [%s]\n",$i+1,$line);
    }
    printf SOURCE ("\n");
}

# TWQ : fonctions de forcage
# <sfile> : fichier script sed du type (s/{<expr>}/TWQ(I,*)/g)
if(@liste_forc != 0) {
    for($i=0;$i<@liste_forc;$i++) {
	$line=$liste_forc[$i];
	chomp($line);
	if($LNG==1) {
	    printf SOURCE ("C TWQ(*,%2d) : fonction de forcage {%s}\n",$i+1,$line);
	}
	else {
	    printf SOURCE ("C TWQ(*,%2d) : forcing function {%s}\n",$i+1,$line);
	}
    }
    printf SOURCE ("\n");
}

# TWQ : variables calculees
# <sfile> : fichier script sed du type (s/c[<expr>]/TWQ(I,*)/g)
if(@liste_varc != 0) {
    for($i=0;$i<@liste_varc;$i++) {
	$line=$liste_varc[$i];
	chomp($line);
	if($LNG==1) {
	    printf SOURCE ("C TWQ(*,%2d) : variable calculee [%s]\n",$i+1,$line);
	}
	else {
	    printf SOURCE ("C TWQ(*,%2d) : computed variable [%s]\n",$i+1,$line);
	}
    }
    printf SOURCE ("\n");
}

# DICO : Parametres externes (dictionnaire)
# <sfile> : fichier script sed du type (s/{<expr>}/DICO(*)/g)
if(@liste_dico != 0) {
    for($i=0;$i<@liste_dico;$i++) {
	$line=$liste_dico[$i];
	chomp($line);
	printf SOURCE ("C DICO(%2d) : parametre {%s}\n",$i+1,$line);
    }
    printf SOURCE ("\n");
}

# LOCAL : Parametres internes (variables locales a la SUBROUTINE)
# <sfile> : fichier script sed du type (s/<expr>/LOCAL(*)/g)
if(@liste_fort != 0) {
    for($i=0;$i<@liste_fort;$i++) {
	$line=$liste_fort[$i];
	chomp($line);
	printf SOURCE ("C LOCAL(%2d) : parametre {%s}\n",$i+1,$line);
    }
    printf SOURCE ("\n");
}


open(FEQUA,"<$fequa");
open(SRCTMP,">$fequa.tmp1");
while(eof(FEQUA) == 0) {
    $line=<FEQUA>;
    $line=~s/\[H\]/H(I)/g;

    for($i=0;$i<@liste_var;$i++) {
	$txt=$liste_var[$i];
	chomp($txt);
	$c=sprintf("%2d",$i+1);
	$line=~s@d\/dt\[$txt\]@DCHEQ($c)@g;
	$line=~s/\[$txt\]/CSEQ($c)/g;
    }

    for($i=0;$i<@liste_forc;$i++) {
	$txt=$liste_forc[$i];
	chomp($txt);
	$c=sprintf("%2d",$i+1);
	$line=~s/{$txt}/TWQ(I,$c)/g;
    }

    for($i=0;$i<@liste_varc;$i++) {
	$txt=$liste_varc[$i];
	chomp($txt);
	$c=sprintf("%2d",$i+1);
	$line=~s/\[$txt\]/TWQEQ($c)/g;
    }

    for($i=0;$i<@liste_dico;$i++) {
	$txt=$liste_dico[$i];
	chomp($txt);
	$c=sprintf("%2d",$i+1);
	$line=~s/{$txt}/DICO($c)/g;
    }

    for($i=0;$i<@liste_fort;$i++) {
	$txt=$liste_fort[$i];
	chomp($txt);
	$c=sprintf("%2d",$i+1);
	$line=~s/{$txt}/LOCAL($c)/g;
    }


    printf SRCTMP $line;
}

close(SRCTMP);
close(FEQUA);
close(SOURCE);

system("$BIN$ps"."txt2fort $fequa.tmp1 >> $projet$ps"."chimie.h");

unlink("$fequa.tmp1");

$liste_var=alaligne(30,43,@liste_var);
$liste_varc=alaligne(30,43,@liste_varc);
$liste_forc=alaligne(30,43,@liste_forc);
$liste_fort=alaligne(30,43,@liste_fort);
$liste_dico=alaligne(30,43,@liste_dico);

printf "\n";
$format="%s %-25s : %s\n";

$txt=evallng ("Variables d'Etat","State Variables");
printf ("$format","$appel>","$txt","$liste_var");
$txt=evallng ("Variables calculÅes","Computed Variables");
printf ("$format","$appel>","$txt","$liste_varc");
$txt=evallng ("Fonctions de forµage","Forcing functions");
printf ("$format","$appel>","$txt","$liste_forc");
$txt=evallng ("ParamÉtres internes","Internal parameters");
printf ("$format","$appel>","$txt","$liste_fort");
$txt=evallng ("ParamÉtres externes","External parameters");
printf ("$format","$appel>","$txt","$liste_dico");
printf "\n";


sortie();

__END__
