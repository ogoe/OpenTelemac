#------------------------Systeme TELEMAC V5P2----------cray_fuji.pm---
#
# Module de traitements spécifiques pour Cray/Fujitsu
#
# Code Perl inclus dans le lanceur principal (runtel) dans le
# cas d'une execution sur unede ces deux plateformes.
#
#----------------------------------------------------------------------
#
# CREATION puis LANCEMENT du CNTL
#
printf "-------------------------- \n";

$script=join "", $GENERIQUE,".cntl";  # .cntl POUR WINDOWS
open (F, ">$script") or die "## Error : Open the $script file is impossible\n";

if ( $CRAY eq "oui" ) {
  printf "LANCEMENT SUR CRAY \n";
  $filename2="${PROJECT}/cfgdef-unicos";
}
if ( $FUJI eq "oui" ) {
  printf "LANCEMENT SUR FUJITSU \n";
  $filename2="${PROJECT}/cfgdef-fuji";
}

printf "-------------------------- \n";

open (F2, "<$filename2") or die "## Error : Open the $filename2 file is impossible\n";
while($LIGNE = <F2>) {                            # Copie du fichier de donnees par defaut
    ($LIGNE, $TMP) = split(/\n/, $LIGNE);         # On enleve le NewLine a la fin
    if ($LIGNE=~/SYSTEME_CIBLE/i) {
	($TMP, $SYSTEME_CIBLE) = split(/=/, $LIGNE); }
    if ($LIGNE=~/NAME_CIBLE/i) {
	($TMP, $NOM_CIBLE) = split(/=/, $LIGNE); }
    if ($LIGNE=~/USR_CIBLE/i) {
	($TMP, $USR_CIBLE) = split(/=/, $LIGNE); }
    if ($LIGNE=~/FC_CIBLE/i) {
	($TMP, $FC_CIBLE) = split(/=/, $LIGNE); }
    if ($LIGNE=~/FCFLAG_CIBLE/i) {
	($TMP, $FCFLAG_CIBLE) = split(/=/, $LIGNE); }
    if ($LIGNE=~/FC_LKCIBLE/i) {
	($TMP, $FC_LKCIBLE) = split(/=/, $LIGNE); }
    if ($LIGNE=~/FC_FLAG_LKCIBLE/i) {
	($TMP, $FC_FLAG_LKCIBLE) = split(/=/, $LIGNE); }
    if ($LIGNE=~/FC_LIBCIBLE/i) {
	($TMP, $FC_LIBCIBLE) = split(/=/, $LIGNE); }
}
close (F2)      or die "## Error : Close the $filename2 file is impossible\n";

$PASS_CIBLE="";
$MEMO_CIBLE="";
$TIME_CIBLE="";
$MAIN="";

$filename2="unix";
open (F2, "<$filename2") or die "## Error : Open the $filename2 file is impossible\n";
while($LIGNE = <F2>) {                            # Copie du fichier unix
    ($LIGNE, $TMP) = split(/\n/, $LIGNE);         # On enleve le NewLine a la fin
    $LIGNE =~ s/ //g ;                            # On enlève tous les blancs
    if ($LIGNE=~/NOMCRAY/i) {
	($TMP, $NOM_CIBLE) = split(/=/, $LIGNE); }
    if ($LIGNE=~/USERCRAY/i) {
	($TMP, $USR_CIBLE) = split(/=/, $LIGNE); }
    if ($LIGNE=~/PASSWORD/i) {
	($TMP, $PASS_CIBLE) = split(/=/, $LIGNE); }
    if ($LIGNE=~/MEMOIRE/i) {
	($TMP, $MEMO_CIBLE) = split(/=/, $LIGNE); }
    if ($LIGNE=~/CPU/i) {
	($TMP, $TIME_CIBLE) = split(/=/, $LIGNE); }
    if ($LIGNE=~/FORTRAN/i) {
	($TMP, $MAIN) = split(/=/, $LIGNE); }
}

#printf F " # MAIN = $MAIN \n";

close (F2)      or die "## Error : Close the $filename2 file is impossible\n";

 if ($LNG eq "1" ) {
   if ($NOM_CIBLE  eq "") { die "## 'NOM DU CRAY' non defini dans le fichier parametre\n"; }
   if ($USR_CIBLE  eq "") { die "## 'USER CRAY' non defini dans le fichier parametre\n"; }
   if ($PASS_CIBLE eq "") { die "## 'MOT DE PASSE CRAY' non defini dans le fichier parametre\n"; }
   if ($MEMO_CIBLE eq "") { die "## 'PLACE MEMOIRE CRAY' non definie dans le fichier parametre\n";}
   if ($TIME_CIBLE eq "") { die "## 'TEMPS MACHINE CRAY' non defini dans le fichier parametre\n"; }
 }
 else {
   if ($NOM_CIBLE  eq "") { die "## 'CRAY NAME' not defined in steering file\n"; }
   if ($USR_CIBLE  eq "") { die "## 'USER CRAY' not defined in steering file\n"; }
   if ($PASS_CIBLE eq "") { die "## 'PASSWORD' not defined in steering file\n"; }
   if ($MEMO_CIBLE eq "") { die "## 'MEMORY SPACE' not defined in steering file\n"; }
   if ($TIME_CIBLE eq "") { die "## 'CPU TIME' not defined in steering file\n"; }
 }

($STHP, $TMP) = split(/\n/, `uname -n`);      # On enleve le NewLine a la fin


if ($CRAY eq "oui") {
  printf F "# USER=$USR_CIBLE PW=$PASS_CIBLE\n";   # User et Passwd
  printf F "# QSUB-r $USR_CIBLE\n";
  printf F "# QSUB-lT $TIME_CIBLE\n";
  printf F "# QSUB-lM $MEMO_CIBLE\n";
  printf F "# QSUB-o out_$REP \n";
  printf F "# QSUB-ro \n";
  printf F "# QSUB-eo \n";
# printf F "newacct n \n";
}
if ($FUJI eq "oui") {
  printf F "# @\$-x \n";
  printf F "# @\$-r $USR_CIBLE \n";
  printf F "# @\$-lT $TIME_CIBLE \n";
  printf F "# @\$-lM $MEMO_CIBLE \n";
  printf F "# @\$-o out_$REP \n";
  printf F "# @\$-ro \n";
  printf F "# @\$-eo \n";
}
printf F "\n";
printf F "RESS=\"$PROJECT\"\n";               # repertoire PROJET
printf F "HPWD=\"$BASE\"\n";                  # repertoire de lancement des calculs
printf F "WORK=\"$REP\"\n";                   # repertoire temporaire
printf F "FLTR=\"\"\n";                       # ????
printf F "NCPUS=\"\"\n";                      # ????
printf F "STHP=\"$STHP\"\n";                  # adresse IP de la station ou nom
printf F "USHP=\"$user_name\"\n";             # user name

printf F "set -x \n";
printf F "ja \n";
printf F "err=\"\" \n";
#
# Exclusivement pour le VPP
# printf F "[ -d \$HOME/workdir ] || { ln -s /work/$USR_CIBLE workdir ; } \n" ;
#
printf F "cd \$HOME/workdir \n";
printf F "mkdir \${USHP}-\${WORK} || { ";
if ($LNG eq "1" ) {
  printf F " echo \"*** CREATION DU REPERTOIRE TEMPORAIRE IMPOSSIBLE ***\"; ";}
else {
  printf F " echo \"*** OPENING THE TEMPORARY DIRECTORY NOT ALLOWED ***\"; ";}
printf F " err=\"oui\"; } \n";
printf F "cd \${USHP}-\${WORK} \n";

printf F "\n";
printf F "#--------------------------- \n";
printf F "# Acquisition des fichiers   \n";
printf F "#--------------------------- \n";
printf F "\n";
printf F "[ -z \"\$err\" ] \&\& { \\ \n" ;
printf F "rcp ${user_name}\@${STHP}:${REPLANCE}/${REP}/CONFIG . || { ";
if ($LNG eq "1" ) {
  printf F "echo \"*** FICHIER \'CONFIG\' INTROUVABLE ***\"; "; }
else {
  printf F "echo \"*** FILE \'CONFIG\' NOT FOUND ***\"; "; }
printf F "err=\"oui\"; } }\n";
printf F "\n";
printf F "[ -z \"\$err\" ] \&\& { \\ \n" ;
printf F "rcp ${user_name}\@${STHP}:${REPLANCE}/${REP}/FORT.2 . || { ";
if ($LNG eq "1" ) {
  printf F "echo \"*** FICHIER \'FORT.2\' INTROUVABLE ***\"; "; }
else {
  printf F "echo \"*** FILE \'FORT.2\' NOT FOUND ***\"; "; }
printf F "err=\"oui\"; } }\n";
printf F "[ -z \"\$err\" ] \&\& { \\ \n" ;
printf F "rcp ${user_name}\@${STHP}:${REPLANCE}/${REP}/FORT.3 . || { ";
if ($LNG eq "1" ) {
  printf F "echo \"*** FICHIER \'FORT.3\' INTROUVABLE ***\"; "; }
else {
  printf F "echo \"*** FILE \'FORT.3\' NOT FOUND ***\"; "; }
printf F "err=\"oui\"; } }\n";
$FIC_IN="CONFIG FORT.2 FORT.3";
printf F "\n";
if ($MAIN ne "DEFAUT") {
   printf F "[ -z \"\$err\" ] \&\& { \\ \n" ;
   printf F "rcp ${user_name}\@${STHP}:${REPLANCE}/${REP}/source.f . || { ";
   if ($LNG eq "1" ) {
     printf F "echo \"*** FICHIER \'source.f\' INTROUVABLE ***\"; "; }
   else {
     printf F "echo \"*** FILE \'source.f\' NOT FOUND ***\"; "; }
   printf F "err=\"oui\"; } }\n";
   $FIC_IN=join "", $FIC_IN," ","source.f";
}
open (F2, "<$filename2") or die "## Error : Open the $filename2 file is impossible\n";
while($LIGNE = <F2>) {                            # Copie du fichier unix
    ($LIGNE, $TMP) = split(/\n/, $LIGNE);         # On enleve le NewLine a la fin
    if ($LIGNE=~/FDESC/i) {
      $LIGNE =~ s/^.*,"// ;
      $LIGNE =~ s/".*$// ;
      ($varnam,$ficnamcod,$oblig,$fictyp,$ficmod,$modepar) = split (/;/,$LIGNE);
      if ( $ficmod =~ /LIT/ ) {
        $LIGNE2 = <F2>;
        ($LIGNE2, $TMP) = split(/\n/, $LIGNE2);
        $LIGNE2 =~ s/^.*="// ;
        $LIGNE2 =~ s/".*$// ;
        if ( $LIGNE2 ne "") {
          printf F "[ -z \"\$err\" ] \&\& { \\ \n" ;
          printf F "rcp ${user_name}\@${STHP}:${REPLANCE}/${REP}/${ficnamcod} . || { ";
          if ($LNG eq "1" ) {
            printf F "echo \"*** FICHIER \'${ficnamcod}\' INTROUVABLE ***\"; "; }
          else {
            printf F "echo \"*** FILE \'${ficnamcod}\' NOT FOUND ***\"; "; }
          printf F "err=\"oui\"; } }\n";
          $FIC_IN=join "", $FIC_IN," ",${ficnamcod} ;
        }
      }
    }
}
close (F2)      or die "## Error : Close the $filename2 file is impossible\n";

if ($CRAY eq "oui" ) {
  printf F "\n";
  printf F "#--------------------------- \n";
  printf F "# Assignation des fichiers   \n";
  printf F "#      binaires IEEE         \n";
  printf F "#     CRAY uniquement        \n";
  printf F "#--------------------------- \n";
  printf F "\n";
  open (F2, "<$filename2") or die "## Error : Open the $filename2 file is impossible\n";
  while($LIGNE = <F2>) {                            # Copie du fichier unix
      ($LIGNE, $TMP) = split(/\n/, $LIGNE);         # On enleve le NewLine a la fin
      if ($LIGNE=~/FDESC/i) {
        $LIGNE =~ s/^.*,"// ;
        $LIGNE =~ s/".*$// ;
        ($varnam,$ficnamcod,$oblig,$fictyp,$ficmod,$modepar) = split (/;/,$LIGNE);
        if ( $fictyp =~ /BIN/ ) {
          $LIGNE2 = <F2>;
          ($LIGNE2, $TMP) = split(/\n/, $LIGNE2);
          $LIGNE2 =~ s/^.*="// ;
          $LIGNE2 =~ s/".*$// ;
          if ( $LIGNE2 ne "") {
            ($TMP, $UNIT) = split(/\./, $ficnamcod);
            printf F "[ -z \"\$err\" ] \&\& { \\ \n" ;
            printf F "assign -F f77 -N ieee u:${UNIT} || { ";
            if ($LNG eq "1" ) {
              printf F "echo \"*** ERREUR DE TRAITEMENT DU BINAIRE \'${ficnamcod}\' ***\"; "; }
            else {
              printf F "echo \"*** PROCESSING ERROR WITH BINARY FILE \'${ficnamcod}\' ***\"; "; }
            printf F "err=\"oui\"; } }\n";
          }
        }
      }
  }
close (F2)      or die "## Error : Close the $filename2 file is impossible\n";
}
printf F "\n";
printf F "#--------------------------- \n";
printf F "# Acquisition des librairies \n";
printf F "#      et des modules        \n";
printf F "#--------------------------- \n";
printf F "\n";
if ($MAIN ne "DEFAUT") {
  printf F "[ -z \"\$err\" ] \&\& { \\ \n" ;
  printf F "rcp ${user_name}\@${STHP}:${PROJECT}/include/${SYSTELVERS}/${SYSTEME_CIBLE}/*.mod . || { ";
  if ($LNG eq "1" ) {
    printf F "echo \"*** ERREUR DE CHARGEMENT DES MODULES ***\"; "; }
  else {
    printf F "echo \"*** MODULE LOADING ERROR ***\"; "; }
  printf F "err=\"oui\"; } }\n";
  printf F "\n";
  $BIBLI="";
  open (F2, "<$filename2") or die "## Error : Open the $filename2 file is impossible\n";
  while($LIGNE = <F2>) {                            # Copie du fichier unix
      ($LIGNE, $TMP) = split(/\n/, $LIGNE);         # On enleve le NewLine a la fin
      if ($LIGNE=~/CODLIBS/i) {
        $LIGNE =~ s/^.*,"// ;
        $LIGNE =~ s/".*$// ;
        $LIGNE =~ s/\|/\//g ;
        $LIGNE =~ s/VVV/$SYSTELVERS/g ;
        $LIGNE =~ s/MMM/$MODE/g ;
        $LIGNE =~ s/LLL/$libExt/g ;
        $LIGNE =~ s/PPP/$SYSTEME_CIBLE/g ;
        printf F "[ -z \"\$err\" ] \&\& { \\ \n" ;
        printf F "rcp ${user_name}\@${STHP}:${PROJECT}/${LIGNE} . || { ";
        $LIGNE = basename($LIGNE);
        $BIBLI=join "", $BIBLI, " ", $LIGNE;
        if ($LNG eq "1" ) {
          printf F "echo \"*** LIBRAIRIE \'${LIGNE}\' INTROUVABLE ***\"; "; }
        else {
          printf F "echo \"*** LIBRARY \'${LIGNE}\' NOT FOUND ***\"; "; }
        printf F "err=\"oui\"; } }\n";
      }
  }
  close (F2)      or die "## Error : Close the $filename2 file is impossible\n";
}
else {
  open (F2, "<$filename2") or die "## Error : Open the $filename2 file is impossible\n";
  while($LIGNE = <F2>) {                            # Copie du fichier unix
      ($LIGNE, $TMP) = split(/\n/, $LIGNE);         # On enleve le NewLine a la fin
      if ($LIGNE=~/EXEDEF/i) {
        $LIGNE =~ s/^.*="// ;
        $LIGNE =~ s/".*$// ;
        $LIGNE =~ s/\|/\//g ;
        $LIGNE =~ s/VVV/$SYSTELVERS/g ;
        $LIGNE =~ s/MMM/$MODE/g ;
        $LIGNE =~ s/LLL/$libExt/g ;
        $LIGNE =~ s/PPP/$SYSTEME_CIBLE/g ;
        printf F "[ -z \"\$err\" ] \&\& { \\ \n" ;
        printf F "rcp ${user_name}\@${STHP}:${PROJECT}/${LIGNE} a.out || { ";
        $EXEC = basename($LIGNE);
        if ($LNG eq "1" ) {
          printf F "echo \"*** EXECUTABLE \'${EXEC}\' INTROUVABLE ***\"; "; }
        else {
          printf F "echo \"*** BINARY FILE \'${EXEC}\' NOT FOUND ***\"; "; }
        printf F "err=\"oui\"; } }\n";
      }
  }
  close (F2)      or die "## Error : Close the $filename2 file is impossible\n";
}
printf F "\n";
printf F "#--------------------------- \n";
printf F "# Compilation                \n";
printf F "#--------------------------- \n";
printf F "\n";
if ($MAIN ne "DEFAUT") {
  $OBJS="source.o";
  printf F "OBJS=\"$OBJS\" \n";
  printf F "\n";
  printf F "[ -z \"\$err\" ] \&\& { \\ \n" ;
# printf F "/opt/ctl/bin/f90 -c -enim -dpq -- source.f || { ";
  printf F "$FC_CIBLE $FCFLAG_CIBLE source.f || { ";
  if ($LNG eq "1" ) {
    printf F "echo \"*** ERREUR A LA COMPILATION DE \'source.f\' ***\" ; "; }
  else {
    printf F "echo \"*** COMPILATION ERROR IN \'source.f\' ***\" ; "; }
  printf F "err=\"oui\"; } }\n";
}
else {
printf F "# -> sans objet \n";
}
printf F "\n";
printf F "#--------------------------- \n";
printf F "# Edition de liens           \n";
printf F "#--------------------------- \n";
printf F "\n";
if ($MAIN ne "DEFAUT") {
# $LIB1="/usr/lib/sysz%%generale";
# $LIB2="/usr/lib/sysz%%imsl";
# $LIBS=join "", $LIB1, " ", $LIB2;
  printf F "BIBLI=\"$BIBLI\" \n";
# printf F "LIBS=\"$LIBS\" \n";
  printf F "LIBS=\"$FC_LIBCIBLE\" \n";
  printf F "\n";
  printf F "[ -z \"\$err\" ] \&\& { \\ \n" ;
# printf F "/opt/ctl/bin/f90 \$OBJS \$BIBLI \$LIBS || { ";
  printf F "$FC_LKCIBLE $FC_FLAG_LKCIBLE \$OBJS \$BIBLI \$LIBS || { ";
  if ($LNG eq "1" ) {
    printf F "echo \"*** ERREUR A L\'EDITION DE LIEN ***\" ; "; }
  else {
    printf F "echo \"*** LINKING ERROR  ***\" ; "; }
  printf F "err=\"oui\"; } }\n";
  printf F "\n";
}
else {
printf F "# -> sans objet \n";
}
printf F "#--------------------------- \n";
printf F "# Execution                  \n";
printf F "#--------------------------- \n";
printf F "\n";
#printf F "[ -z \"\$err\" ] \&\& { \\ \n" ;
#printf F "mkdir core || { ";
#if ($LNG eq "1" ) {
#  printf F "echo \"*** CREATION DU REPERTOIRE \'core\' IMPOSSIBLE ***\" ; "; }
#else {
#  printf F "echo \"*** OPENING \'core\' DIRECTORY NOT ALLOWED  ***\" ; "; }
#printf F "err=\"oui\"; } }\n";
#printf F "\n";

#printf F "[ -z \"\$err\" ] \&\& { \\ \n" ;
#printf F "a.out || { ";
#if ($LNG eq "1" ) {
#  printf F "echo \"*** EXECUTION IMPOSSIBLE ***\" ; "; }
#else {
#  printf F "echo \"*** RUNNING ERROR  ***\" ; "; }
#printf F "err=\"oui\"; } }\n";

# on ne se preoccupe pas ici du code de retour de 'a.out'
printf F "[ -z \"\$err\" ] \&\&  a.out  \n" ;

printf F "\n";
printf F "#--------------------------- \n";
printf F "# Recuperation des fichiers  \n";
printf F "#--------------------------- \n";
printf F "\n";
printf F "# -> Suppression des fichiers a ne pas recuperer  \n";
printf F "\n";

# Modification A. Bas (26/06/2000) Probleme pour supprimer ce reppertoire sur le fuji.

#printf F "[ -z \"\$err\" ] \&\& { \\ \n" ;
#printf F "rmdir core || { ";
#if ($LNG eq "1" ) {
#  printf F "echo \"*** SUPPRESSION DU REPERTOIRE \'core\' IMPOSSIBLE ***\" ; "; }
#else {
#  printf F "echo \"*** REMOVING DIRECTORY \'core\' NOT ALLOWED  ***\" ; "; }
#printf F "err=\"oui\"; } }\n";
#printf F "\n";
foreach $FIC (split ' ', $FIC_IN) {
printf F "[ -z \"\$err\" ] \&\& { \\ \n" ;
printf F "rm -f $FIC || { ";
  if ($LNG eq "1" ) {
    printf F "echo \"*** SUPPRESSION DU FICHIER \'${FIC}\' IMPOSSIBLE ***\" ; "; }
  else {
    printf F "echo \"*** REMOVING FILE \'${FIC}\' NOT ALLOWED  ***\" ; "; }
  printf F "err=\"oui\"; } }\n";
  printf F "\n";
}
if ($MAIN ne "DEFAUT") {
  printf F "[ -z \"\$err\" ] \&\& { \\ \n" ;
  printf F "rm -f source.o || { ";
  if ($LNG eq "1" ) {
    printf F "echo \"*** SUPPRESSION DU FICHIER \'source.o\' IMPOSSIBLE ***\" ; "; }
  else {
    printf F "echo \"*** REMOVING FILE \'source.o\' NOT ALLOWED  ***\" ; "; }
  printf F "err=\"oui\"; } }\n";
  printf F "\n";
  foreach $FIC (split ' ', $BIBLI) {
    printf F "[ -z \"\$err\" ] \&\& { \\ \n" ;
    printf F "rm -f $FIC || { ";
    if ($LNG eq "1" ) {
      printf F "echo \"*** SUPPRESSION DE LA LIBRAIRIE \'${FIC}\' IMPOSSIBLE ***\" ; "; }
    else {
      printf F "echo \"*** REMOVING LIBRARY \'${FIC}\' NOT ALLOWED  ***\" ; "; }
    printf F "err=\"oui\"; } }\n";
    printf F "\n";
  }
  printf F "\n";
  printf F "for i in \`ls -1 *.mod\` \n";
  printf F "do \n";
  printf F "[ -z \"\$err\" ] \&\& { \\ \n" ;
  printf F "rm -f \$i || { ";
  if ($LNG eq "1" ) {
    printf F "echo \"*** SUPPRESSION DU MODULE \'\$i\' IMPOSSIBLE ***\" ; "; }
  else {
    printf F "echo \"*** REMOVING MODULE \'\$i\' NOT ALLOWED  ***\" ; "; }
  printf F "err=\"oui\"; } }\n";
  printf F "done \n";
  printf F "\n";
}
printf F "[ -z \"\$err\" ] \&\& { \\ \n" ;
printf F "rm -f a.out || { ";
if ($LNG eq "1" ) {
  printf F "echo \"*** SUPPRESSION DE L\'EXECUTABLE \'a.out\' IMPOSSIBLE ***\" ; "; }
else {
  printf F "echo \"*** REMOVING PROGRAM \'a.out\' NOT ALLOWED  ***\" ; "; }
printf F "err=\"oui\"; } }\n";
printf F "\n";
printf F "# -> Recuperation des fichiers du calcul \n";
printf F "\n";
printf F "for i in \`ls -1\` \n";
printf F "do \n";
printf F "[ -z \"\$err\" ] \&\& { \\ \n" ;
printf F "rcp \$i ${user_name}\@${STHP}:${REPLANCE}/${REP}/. || { ";
if ($LNG eq "1" ) {
  printf F "echo \"*** RECUPERATION DU FICHIER \'\$i\' IMPOSSIBLE ***\" ; "; }
else {
  printf F "echo \"*** LOADING FILE \'\$i\' NOT ALLOWED  ***\" ; "; }
printf F "err=\"oui\"; } }\n";
printf F "done \n";
printf F "\n";
printf F "#--------------------------- \n";
printf F "# Recuperation du fichier    \n";
printf F "#    d\'execution            \n";
printf F "#--------------------------- \n";
printf F "\n";
printf F "cd \$HOME/workdir \n";
printf F "\n";
printf F "[ -z \"\$err\" ] \&\& { \\ \n" ;
printf F "rm -rf \${USHP}-\${WORK} || { ";
if ($LNG eq "1" ) {
  printf F " echo \"*** SUPPRESSION DU REPERTOIRE TEMPORAIRE IMPOSSIBLE ***\"; ";}
else {
  printf F " echo \"*** REMOVING THE TEMPORARY DIRECTORY NOT ALLOWED ***\"; ";}
printf F " err=\"oui\"; } }\n";
printf F "\n";
printf F "cd \n";
printf F "[ -z \"\$err\" ] \&\& { \\ \n" ;
printf F "rm -f ${script}_${WORKING} || { ";
if ($LNG eq "1" ) {
  printf F " echo \"*** SUPPRESSION DU SCRIPT \'${script}_${WORKING}\' IMPOSSIBLE ***\"; ";}
else {
  printf F " echo \"*** REMOVING SCRIPT FILE \'${script}_${WORKING}\' NOT ALLOWED ***\"; ";}
printf F " err=\"oui\"; } }\n";
printf F "\n";

printf F "ja -csfl \n";
printf F "\n";
printf F "rcp out_$REP ${user_name}\@${STHP}:${REPLANCE}/${REP}/LISTING && rm -f out_$REP \n";
printf F "cat /dev/null > FIN_$REP \n";
printf F "rcp FIN_$REP ${user_name}\@${STHP}:${REPLANCE}/${REP}/FIN_CALCUL && rm -f FIN_$REP \n";
printf F "\n";

close (F) or die "## Error : Close the $script file is impossible\n";
chmod(0755,"$script");

# Fin du module 'cray_fujitsu'
