#!perl
#line 8
#----------------------------------------------------------------------#
#                                                                      #
#   ROLE   : lancement d'un logiciel figurant sous ce repertoire       #
#                                                                      #
#   ARGUMENTS : [-c] : calcul sur CRAY (sur station par defaut)        #
#               [-f] : calcul sur CRAY avec flowtrace                  #
#               [-b] : Execution differee (l'heure doit alors etre le  #
#                                         premier argument)            #
#                                                                      #
#            [file1] : nom du fichier des parametres ('cas' par defaut)#
#            [file2] : nom du fichier de sortie (facultatif)           #
#                                                                      #
#----------------------------------------------------------------------#
#

use Getopt::Std;
use File::Basename;
use File::Copy;

$PERLPATH=`getperlpath`;

if($ENV{"OS"} eq "Windows_NT") {	#hrModif
    #Cas du systeme Windows NT
    $dcOS="WINNT";
    $dirlib="win";
    $ps="\\";
    $libExt="lib";
    ($BASE, $TMP) = split(/\n/, `CD`);      # On enleve le NewLine a la fin
    $FC           = "df.exe /nologo /warn:nofileopt /convert:big_endian";
    $OBJS="source.obj";
    $linkage="link.exe/MACHINE:IX86 /subsystem:console /incremental:no /out:a.exe";
    $callPerl="perl";
    $entetePerl="\@rem = '\n$PERLPATH\\perl %0\ngoto endofperl\n\@rem ';\n";
    $entetePerl2="\@rem = '\n$PERLPATH\\perl delete_cas$$.bat\ngoto endofperl\n\@rem ';\n";
    $fileToDelete="";
    $listJobs="at";
    $atFormat="at %s %s";
}
else {
    #Cas du systeme UNIX
    $dcOS="UNIX";
    $dirlib=`version -s`;
    $dirlib=~s/\n//;
    $ps="/";
    $libExt="a";
    ($BASE, $TMP) = split(/\n/, `pwd`);      # On enleve le NewLine a la fin
    $FC           = "f77 ";
    $OBJS="source.o";
    $linkage="$FC -o a.exe";
    $callPerl="";
    $entetePerl="#!$PERLPATH/perl\n";
    $entetePerl2="#!$PERLPATH/perl\n";
    $fileToDelete=".bat";
    $listJobs="at -l";
    $atFormat="echo %s | at %s";
}

######################################################
#   /  Routines                /
######################################################

sub usage {
  $EXECUTABLE = basename($0,".bat");
  # Erase evidence of previous errors (if any), so exit status is simple.
  $! = 0;
  die <<EOF;
Usage: $EXECUTABLE [-D] [-s|b|n|d xx:xx] [cas]
       $EXECUTABLE -h|H
EOF
}

sub heure_plusun() {
  $min += 1;
  if ($min eq 60) {
    $min = 0;
    $hour += 1;
    if ($hour eq 24) {
      $hour = 0; }
  }
}


######################################################
#   /  Parametres configurables /
######################################################
$cc="";
$batch="";
$DEBUG="";
$FLTR="";
$LOGI=$0;
#$FC           = "df.exe /nologo /warn:nofileopt /convert:big_endian";

######################################################
#   / Traitement des options /
######################################################

getopts("bcpfDG:") || pasoption;
if ($opt_b) {
  $batch="o";
}
if ($opt_D) {
  $DEBUG="";
  printf "WARNING : Mode DEBUG non implémenté sous WindowsNT\n";
}
if ($opt_G) {
  $GPROF="";
  printf "WARNING : Mode PROFILER non implémenté sous WindowsNT\n";
}
if ($opt_c) {
  $TYPE=cray; $cc="-c";
}
if ($opt_f) {
  $TYPE=cray; $cc="-f"; $FLTR="o";
}

#$RESS1=$ENV{"PROJECT"};
$RESS1=`getproject`;
$RESS1="$RESS1$ps"."tomawac";

if ($batch eq "o") {  
  $heure = $ARGV[0];
  open(F1,  ">work$$.bat");
  printf F1 "$LOGI $cc @ARGV[1] @ARGV[2]";
  close(F1);
  if($heure="now") {
    ($sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst) = localtime(time);
    heure_plusun();    
  }
  else {
    ($hour, $tmp) = split("\:", $heure);      
    ($tmp, $min) = split("\:", $heure);
  } 

  printf "\n\n";
  system("at $hour:$min d:$RESS1$ps"."work$$.bat");
  system("at");
  
  exit;
}

$PARA=@ARGV[0];
$LIST=@ARGV[1];
$LOGI =~ tr/a-z/A-Z/;

if($TYPE eq "") {
  $TYPE="hp";
}
if($PARA eq "") {
  $PARA="cas";
}

chdir(dirname($PARA));
#($HPWD, $TMP) = split(/\n/, `CD`);
$HPWD=$BASE;
$WORK="work$$";

#export RESS1 HPWD WORK FLTR LOGI DEBUG GPROF
mkdir($WORK,0777);
copy(basename($PARA),"$WORK$ps"."para");
copy("$RESS1$ps"."dico","$WORK$ps");
chdir($WORK);
`$RESS1$ps""lecfic.exe`;

#   / CREATION DE L'ENTETE PERL EN ENTETE DU FICHIER $tomawac.bat
#   / AJOUT DU FICHIER UNIX EN ENTETE DU FICHIER $tomawac.bat
#   / AJOUT $GENERIQUE.hp DANS $tomawac.bat

open(F,">tomawac2.bat");
open(F2,"<data");
open(F3,"<$RESS1$ps"."exec.bat");

#---- Entete Perl

print F $entetePerl;
$RESS1_DB=$RESS1;$RESS1_DB=~s/\\/\\\\/g;
$HPWD_DB=$HPWD;$HPWD_DB=~s/\\/\\\\/g;
$WORK_DB=$WORK;$WORK_DB=~s/\\/\\\\/g;
$FLTR_DB=$FLTR;$FLTR_DB=~s/\\/\\\\/g;
$LOGI_DB=$LOGI;$LOGI_DB=~s/\\/\\\\/g;
$DEBUG_DB=$DEBUG;$DEBUG_DB=~s/\\/\\\\/g;
$GPROF_DB=$GPROF;$GPROF_DB=~s/\\/\\\\/g;
$ps_DB=$ps;$ps_DB=~s/\\/\\\\/g;

printf F "\$OBJS    = \"$OBJS\";\n";
printf F "\$RESS1    = \"$RESS1_DB\";\n";
printf F "\$HPWD    = \"$HPWD_DB\";\n";
printf F "\$WORK    = \"$WORK_DB\";\n";
printf F "\$FLTR    = \"$FLTR_DB\";\n";
printf F "\$LOGI    = \"$LOGI_DB\";\n";
printf F "\$DEBUG    = \"$DEBUG_DB\";\n";
printf F "\$GPROF    = \"$GPROF_DB\";\n";
printf F "\$linkage    = \"$linkage\";\n";
printf F "\$FC        = \"$FC\";\n";
printf F "\$ps    = \"$ps_DB\";\n";
printf F "\$libExt    = \"$libExt\";\n";
printf F "\$dirlib    = \"$dirlib\";\n";

#------ ajout du fichier unix
while(eof(F2) == 0) {                           # Copie du fichier unix
  $LIGNE = <F2>;                                #   avec traduction shell=>perl
  ($LIGNE, $TMP) = split(/\n/, $LIGNE);         # On enleve le NewLine a la fin
  $LIGNE =~ s/ //g ;					# On enlève tous les blancs
  ($LIGNE1, $LIGNE2) = split(/=/, $LIGNE);      # On recupere MOT + VALEUR
  ($TMP, $TMP3, $TMP2) = split(/"/, $LIGNE2);   # On enleve les guillemets
  if ($TMP3 ne "") {$LIGNE2 = $TMP3; }          # Cas chaine vide ""

    if ($LIGNE1) {
        # Passer VERSION et BIBLIO en minuscules
        if ( $LIGNE1 =~ /VERSION/ ) {$LIGNE2 =~ tr/A-Z/a-z/;}
        if ( $LIGNE1 =~ /BIBLIO/ )  {$LIGNE2 =~ tr/A-Z/a-z/;}
	printf F "\$$LIGNE1 = \"$LIGNE2\";\n";
    }
                    }
#------ ajout du fichier $GENERIQUE.hp
while(eof(F3) == 0) {                           # Copie du fichier $GENERIQUE.hp
  $LIGNE = <F3>;
  printf F "$LIGNE"; }

#------ ajout de la fin du fichier (perl)
printf F "__END__\n";                           # Entete de fin Perl
printf F ":endofperl\n";

close(F);
close(F2);
close(F3);
chmod(0777,"tomawac2.bat");

if($LIST ne "") {
 
}
else {
  system("tomawac2.bat");
  #nettoyage du repertoire.
  chdir($WORK);
  rmdir("core");
  opendir(CURDIR,".");
  foreach $var (readdir(CURDIR))
  {
      chmod(0777,$var);
      unlink $var;
  }
  rmdir("core");
  chdir("..");
  rmdir("$WORK");
}

printf "\n";
exit;

__END__
:endofperl

