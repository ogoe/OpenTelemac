#!perl
#
#jaj this script is strongly modified for BAW Karlsruhe purposes 
#version date: Wed Mar 19 17:07:07 MET 2003
#
#------------------------Systeme TELEMAC V5P2--------------runcode.pl--
#
# Lanceur generique de niveau 2 des codes sur Systeme TELEMAC.
#
# Ce script est exploite par le lanceur de niveau 1 "runtel.pl"
# pour construire le script de conduite d'une execution d'un code.
#
# AUTEUR : DeltaCAD - 1999
#
# MODIFICATIONS
#
#          date    : 16/09/1999
#          objet   : Changement des noms des fichiers "sortie" et "error"
#
#          date    : 16/09/1999
#          objet   : Suppresion de l'ouverture d'un repertoire 'core'
#
#          date    : 17/09/1999
#          objet   : Restitution des fichiers resultats Multiples
#
#          date    : 29/09/1999
#          objet   : Report des modifications d'Alain DUTOYA (SIMOLUG)
#
#          date    : 18/01/2000
#          objet   : Restitution des fichiers sans recopie
#
#          date    : 25/01/2000
#          objet   : Correction d'un bug ABsoft (Delta-Cad)
#
#          date    : 08/03/2000
#          objet   : Modification des traitements generant le fichier CONFIG
#
#          date    : 10/05/2000
#          objet   : Appel de la librairie CALCIUM
#
#          date    : 26/06/2000
#          objet   : On utilise les chemins complets pour le fichier
#                    de parametres
#
#          date    : 22/06/2000
#          objet   : On renomme les anciens fichiers de resultats : *.old
#                    (fonction rename).
#
#          date    : Juillet 2000 - DeltaCAD
#          objet   : Introduction de la protection sous WindowsNT
#
#          date    : Juillet 2001 - DeltaCAD
#          objet   : Remplacement de PVM par MPI
#
#          date    : Septembre 2001 - DeltaCAD
#          objet   : Amelioration de la gestion des versions
#                    Localisation des modules fortran90 dans les branches des codes
#
#          date    : Mai 2008- Pascal Vezolle (IBM)
#          object  : Modication des functions sub list_extens_file_names, restihpextens, acquihpextens
#                    pour un grand nombre de processors (maximun: 99999)
#
#          date    : 15 Juin 2009- Chi-Tuan Pham (EDF)
#          object  : modifications for coupling with Delwaq in parallel mode.
#
#          date    : Avril 2010 - BAW Karlsruhe (jaj) et Sinetics (C. Denis)
#          objet   : Prise en compte des sections de controles en // (jaj)
#                  : Ajout des cibles pour Mumps (C. Denis)
#
#------------------------Systeme TELEMAC V5P2--------------------------
#
use File::Copy;
use File::Path;
use File::Basename;
use Benchmark;

#############################################################################
#    LIBRAIRIES LIEES AU CODE LANCE
#############################################################################

# Decomposition de VERSION :
# codes T2D, BIEF, DAMOCLES, PARAVOID

@VERS = split(/,/, $VERSION); $v0=$VERS[0];                 # 1 meme version pour tous
if ( $VERS[1] eq "" ) { @VERS=($v0,$v0,$v0,$v0,$v0,$v0,$v0,$v0,$v0)};

$BIBLI="";
$LIBCALCIUM = "";
$i=0;
foreach (@CODLIBS)
  {
    $param = SubstParamPath ($_, $VERS[$i]);
    $param = join "", $PROJECT,$ps,$param;
    $BIBLI="$BIBLI $param";
    $INCDIR=$INCDIR." $cmdInc".dirname($param);
    $i++;
  }
#
#---- Specificite pour le parallelisme
#
$MPI_NPROC=0;
$MPI_ROOT  = join "",$PROJECT,$ps,"mpi",$ps,$dirlib;

if ($NCSIZE < 1 && $CALCIUM ne "OUI" )
  {    #scalaire
  $LIBPARALL  = "";
  $LIBMUMPS=$libmumpsseq;
  $LIB_SYS = "";
# PLG INGEROP  if($ENV{"OS"} eq "Windows_NT")  {$LIB_SYS = "advapi32.lib netapi32.lib";}
  $LIB_CALCIUM = "";
  }
else { #parallele
  $INCDIR     = $INCDIR." $cmdInc$MPI_ROOT$ps"."include";
  $LIBPARALL  = $libsmpi;
  $LIBMUMPS=$libmumpspar;

  if ($CALCIUM eq "OUI")   #ajout libs du coupleur Calcium
  {
    my $PVM_ARCH  = $ENV{"PVM_ARCH"};
    my $ACCC_ROOT = $ENV{"ACCC_ROOT"};
    $LIBCALCIUM = "$ACCC_ROOT"."$ps"."$PVM_ARCH"."$ps"."lib"."$ps"."libFcpl.a";
  }

  #Remplacer PARAVOID par PARALLEL
  $BIBLI =~ s/paravoid/parallel/g;
  #Trouver le path du repertoire parallel pour PARTEL/GRETEL
  $PathParall=$BIBLI;
  $BIBLI =~  m/.*\s+(.*parallel).*/i; $PathParall =$1;
  $PathParall =~  s/parallel$//i;
  #Cas WindowsNT
  if($ENV{"OS"} eq "Windows_NT")     # Librairies pour WindowsNT
    {
     $LIB_SYS = " $LIB_SYS "
#PLG INGEROP     $LIB_SYS = " $LIB_SYS wsock32.lib kernel32.lib libc.lib gdi32.lib winspool.lib".
#PLG INGEROP                " comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib".
#PLG INGEROP                " netapi32.lib uuid.lib oldnames.lib dfconsol.lib";
    }

} #($NCSIZE < 2 && $CALCIUM ne "OUI" )

#Bilan des librairies a considerer : BIBLI
# special lib added
$BIBLI="$BIBLI  $LIB_SYS $LIBSPECIAL $LIBCALCIUM $LIBPARALL  $LIBMUMPS " ;

#############################################################################
#    EXECUTABLE PAR DEFAUT
#############################################################################
#jaj

if ($NCSIZE < 1 ) {
  $RUNEXEDEF=SubstParamPath($EXEDEF,$VERS[$0]);
} else {
  $RUNEXEDEF=SubstParamPath($EXEDEFPARA,$VERS[$0]);
}

$RUNEXEDEF=join "", $PROJECT,$ps,$RUNEXEDEF;
$EXEFILE=join "","out",$WORKING,"_$dirlib",".exe";

##########################################################
#    / ECRIRE UNE LIGNE DE "_"
##########################################################

sub genereligne {
  print "______________________________________________________________________________\n";
}

#####################################################################################
#    / ECRIRE SUR ECRAN EN FRANCAIS OU EN ANGLAIS EN TESTANT LA VARIABLE LNG
#####################################################################################

sub ecrire {
  if ($LNG eq "1") {
    print "@_[0] @_[2]\n"; }
  else {
    print "@_[1] @_[2]\n"; }
}

######################################################################################
#    / ECRIRE SUR FICHIER EN FRANCAIS OU EN ANGLAIS EN TESTANT LA VARIABLE LNG
######################################################################################

sub copie {
  if (@_[3] eq "") {
    if ($LNG eq "1") {
      print "*** @_[0] @_[2] ***\n"; }
    else {
      print "*** @_[1] @_[2] ***\n"; }
  }
  else {
    open(F, ">@_[3]");
    if ($LNG eq "1") {
      print F "*** @_[0] @_[2] ***\n"; }
    else {
      print F "*** @_[1] @_[2] ***\n"; }
    close(F);
  }
}
#####################################################################################
#    / ECRIRE (TITRE : VALEUR) SUR ECRAN EN FRANCAIS/ANGLAIS FORMATTE
#####################################################################################

sub ecrire_FicTitrVal {
  if ($LNG eq "1") { $ligaff="$_[0]"; } else { $ligaff="$_[1]"; }
  $ligaff="$ligaff".
  "                                                                           ";
  substr($ligaff,49,1)=":";                                 # ':' en colonne 40
  substr($ligaff,51,25)=basename($_[2]);
  print " - $ligaff\n";
}

######################################################################################
#    Substitution VERSION, MODE, DIRLIB, ... dans un PATH
######################################################################################
#
#--- Sub SubstModeVersion
#
#   VVV = version
#   MMM = mode (debug ou non)
#   PPP = plateforme
#   LLL = extension d'une librairie ("a" ou "lib")
#
#    ex : "telemac2d|tel2d_VVV|telemac2dMMMVVV.LLL"
#
sub SubstParamPath                 # (path, num_version)
{
my $param=$_[0];
$param =~ s/\|/$ps/g;              # placer le separateur de path
$param =~ s/VVV/$_[1]/g;           # placer le numero de version
$param =~ s/MMM/$MODE/g;           # placer le mode (debug/profile)
$param =~ s/LLL/$libExt/g;         # extension d'une librairie
$param =~ s/PPP/$dirlib/g;         # nom dir pour la plateforme
return $param;
}

######################################################################################
#    Gestion de l'affichage et le calcul de la duree d'un calcul
######################################################################################

sub heure_plusun() {
  $min += 1;
  if ($min eq 60) {
    $min = 0;
    $hour += 1;
    if ($hour eq 24) { $hour = 0; }
  }
}

#------ Affichage de la duree du job

sub time_affich {
    my($tr) = @_;
    my @t = @$tr;

# $real, $user, $system, $children_user, $children_system
    my ($dtr, $dtpu, $dtps, $dtcu, $dtcs, $dtn) = @t;

    $hh=int($dtr/3600);
    $mm=int($dtr%3600/60);
    $ss=$dtr%60;
    $sys=$dtps+$dtpu;

  if ($LNG eq "1") {      # Francais
    $stmp =  sprintf " Duree du calcul : $dtr secondes ( $hh:$mm:$ss )";
    $stmp .= sprintf " (systeme=$sys sec)";
                   }
  else {                  # Anglais
    $stmp =  sprintf " Duration of job : $dtr seconds ( $hh:$mm:$ss )";
    $stmp .= sprintf " (system=$sys sec)";
       }

    $stmp;
}

##########################################################
#    / FONCTIONS D'ACQUISITIONS DES FICHIERS
##########################################################

sub erreuracqui {
  $FIL1 = @_[0];

  ecrire("ERREUR A L'ACQUISITION DU FICHIER",
         "ERROR : ALLOCATION FILE", $FIL1);
#jaj
#  if ($sortie eq "listing") {

    open(F, ">>$REPLANCE$ps"."$PARA$WORKING"."_error.log");
    copie("ERREUR A L'ACQUISITION DU FICHIER",
          "ERROR : ALLOCATION FILE", $FIL1,
          "$REPLANCE$ps"."$PARA$WORKING"."_error.log"); 
    close(F);
# }
  exit;
}

sub erreuracquiextens {
  $FIL1 = @_[0];

  ecrire("ERREUR A L'ACQUISITION (parallele) DU FICHIER",
         "ERROR : PARALLEL ALLOCATION FILE", $FIL1);
#jaj
#  if ($sortie eq "listing") {

    open(F, ">>$REPLANCE$ps"."$PARA$WORKING"."_error.log");
    copie("ERREUR A L'ACQUISITION (parallele) DU FICHIER",
          "ERROR : PARALLEL ALLOCATION FILE", $FIL1,
          "$REPLANCE$ps"."$PARA$WORKING"."_error.log"); 
    close(F);
#}
  exit;
}


sub acquihp {
  $FIL1 = @_[0];
  $FIL2 = @_[1];
#jaj
#  copy($FIL1, "$REPLANCE$ps"."$REP$ps"."$FIL2") || erreuracqui($FIL1);
  copy($FIL1, "$REP$ps"."$FIL2") || erreuracqui($FIL1);
}

sub acquihpextens {
  $FIL1 = @_[0];
  $FIL2 = @_[1];
  $zero1 = "";
  $zero2 = "";
  $compteur = 0;
  $ncsize = $NCSIZE - 1;
  if ($ncsize eq 0) { $ncsize = ""; }
  if ($ncsize < 10) {
    $zero1 = "0000"; }
  elsif ($ncsize < 100) { 
    $zero1 = "000"; }
  elsif ($ncsize < 1000) { 
    $zero1 = "00"; }
  elsif ($ncsize < 10000) { 
    $zero1 = "0"; }
  else {
          $zero1 = ""; }

  while ($compteur <= $ncsize) {
    if ($compteur < 10) {
      $zero2 = "0000"; }
    elsif ($compteur < 100) { 
      $zero2 = "000"; }
    elsif ($compteur < 1000) { 
      $zero2 = "00"; }
    elsif ($compteur < 10000) { 
      $zero2 = "0"; }
    else {
      $zero2 = ""; }
#jaj
#    copy("$FIL1$zero1$ncsize-$zero2$compteur",
#         "$REPLANCE$ps"."$REP$ps"."$FIL2$zero1$ncsize-$zero2$compteur")
#         || erreuracquiextens($FIL1);

    copy("$FIL1$zero1$ncsize-$zero2$compteur",
         "$REP$ps"."$FIL2$zero1$ncsize-$zero2$compteur")
         || erreuracquiextens($FIL1);
    $compteur = $compteur + 1;
                                }   #while
} #acquihpextens

#jaj number of parameters changed, added secFIC
sub acquihpPARAL    # (ficnam, ficnamcode, modepar, conlimFIC, autopar, secFIC, secFICname)
#_______________________________________________________________________
#
#  Acquisition des fichiers en mode PARALLEL  ($NCSIZE > 1)
#_______________________________________________________________________
#
{#acquihpPARAL
 my $FIL1 = @_[0];      #nom utilisateur du fichier
 my $FIL2 = @_[1];      #nom (de base) du fichier pour le code apres acquisition
 my $modepar =@_[2];    #type d'acquisition parallèle : SCAL, SELAFIN ou PARAL
 my $conlimFIC =@_[3];  #nom du fichier des CONDITIONS LIMITES (pour PARTEL)
 my $autopar =@_[4];    #mode parallelisme automatique ou non
 my $secFIC=@_[5];      #the sections input file workdir name #jaj
 my $secFICname=@_[6];  #the sections input file real name #jaj
#
 my @lfic=();
#
#---- Le fichier CONDITIONS LIMITES sera automatiquement partitionne
#     dans les runs de PARTEL sur les autres fichiers (dont geometrie).
#jaj (this is not true if partel fails)

  if ( ($modepar =~ /CONLIM/) && ($autopar eq "1") )
       { ecrire("    (partitionne pour $NCSIZE processeurs)",
                "    (split for $NCSIZE processors)",        " ");
         return;
       }
#
#---- Les fichiers "SCAL"
#
  if (    ($modepar =~ /SCAL/) 
       || ($modepar =~ /CAS/) 
       || ($modepar =~ /DICO/)  )
   {
   	  acquihp ( $FIL1, $FIL2);
   	  return;
   }
#
#---- Les fichiers "SELAFIN" sont a decouper avec PARTEL en
#      mode "autopar"
#     Sinon, les utiliser comme pré-découpés avec les fonctions
#     "extens"
  if ( ($modepar =~ /SELAFIN/)  )
   {
   	  if ($autopar eq "0")
   	    { acquihpextens ( $FIL1, $FIL2);
   	      return;
   	    }
   	  else
   	    { acquihp   ( $FIL1, $FIL2);
   	      chdir($REP);
   	      $rcp = RunPartel ($FIL2, $conlimFIC, $NCSIZE, $secFIC, $secFICname); #jaj
              if ( $rcp != 0 )  
                {
                  open(F, ">>$REPLANCE$ps"."$PARA$WORKING"."_error.log");
                  printf F "Partel error: \n";
                  printf F "Partitioning into $NCSIZE subdomains with files: \n";
                  printf F "$FIL2 \n";
                  printf F "$conlimFIC \n";
                  printf F "$secFIC \n";   #jaj
                  printf F "returns with error code $rcp \n";
                  printf F "Probably corrupt input files. \n";
                  close(F);
                  $FILEP = "$REPLANCE".$ps."partel.log";
                  if (-e $FILEP )
                    {
                      if (-e $FILEP.".old")
                      {
                        unlink ($FILEP.".old");
                      }
                      rename ($FILEP, $FILEP.".old");
                    }
                  copy ("partel.log", $FILEP);
                  exit 1;   # generate error exit 
                }
              chdir($REPLANCE);
              #copy ("partel.log", "$REPLANCE".$ps."partel.log" );
              ecrire("    (partitionne pour $NCSIZE processeurs)",
                 "    (split for $NCSIZE processors)",    " ");
              return;
            }
   } #modepar=/SELAFIN/

#
#---- Les fichiers "PARAL" sont a gerer avec des suffixes pour
#     chaque processeur
  if  ($modepar =~ /PARAL/)
   {
      @lfic=list_extens_file_names($FIL1, $NCSIZE);
#
#------- En manuel, copier les fichiers qui doivent pré-exister
#
   	  if ($autopar eq "0")    
   	    { foreach (@lfic)
   	        {
   	           if (! -e "$_")     # Erreur : le fichier n'existe pas.
   	             { ecrire("Erreur : le fichier $_ n'existe pas",
	  	                  "Error : File $_ not found");
	  	           erreuracquiextens($_);
	  	         }
	  	       else               # copie du fichier sous le nom du code
	  	         {
	               my $f2=$_; $f2=~s/$FIL1/$FIL2/;
                    #jaj
                    #   copy("$_","$REPLANCE$ps$REP$ps$f2") || erreuracquiextens($_);
	               copy("$_","$REP$ps$f2") || erreuracquiextens($_);
	  	         }
   	        } #foreach lfic
   	      return;
   	    }
#
#------- En automatique,
#        - copier les fichiers avec extension s'ils existent
#        - dupliquer le fichier vers des copies avec extensions sinon
#
   	  else # autopar=1:
   	    { printf "\n"; foreach (@lfic)
   	        {
   	           my $f2=$_; $f2=~s/$FIL1/$FIL2/;
   	           if (-e "$_")     # le fichier existe -> copie
	  	         {
   #jaj
	               # copy("$_","$REPLANCE$ps$REP$ps$f2") || erreuracquiextens($_);
                       copy("$_","$REP$ps$f2") || erreuracquiextens($_);
	               print "COPY $_  $REP$ps$f2\n";
	  	         }
	  	       else             # copie du fichier sans extentsion avec extension
	  	         {
	               # copy("$FIL1","$REPLANCE$ps$REP$ps$f2") || erreuracquiextens($_);
	               copy("$FIL1","$REP$ps$f2") || erreuracquiextens($_);
	               print "COPY $FIL1  $REP$ps$f2\n";
	  	         }
   	        } #foreach lfic
   	      return;
        }
   } #modepar=/PARAL/


} #acquihpPARAL

##########################################################
#    / FONCTIONS DE RESTITUTION DES FICHIERS
##########################################################

sub list_extens_file_names    # (ficnam, nbproc)
#_______________________________________________________________________
#
#  Retourne une liste des fichiers avec les suffixes du paralllisme
#_______________________________________________________________________
#
{#list_extens_file_names
  my $fic= @_[0];
  my $nbp= @_[1];
#
  my @lisfic=();
  my $zero1 = ""; my $zero2 = "";
  my $compteur = 0;
  $nbp = $nbp - 1;

  if ($nbp < 10) {
    $zero1 = "0000"; }
  elsif ($nbp < 100) { 
    $zero1 = "000"; }
  elsif ($nbp < 1000) { 
    $zero1 = "00"; }
  elsif ($nbp < 10000) { 
    $zero1 = "0"; }
  else {
          $zero1 = ""; }

  while ($compteur <= $nbp) {
    if ($compteur < 10) {
      $zero2 = "0000"; }
    elsif ($compteur < 100) { 
      $zero2 = "000"; }
    elsif ($compteur < 1000) { 
      $zero2 = "00"; }
    elsif ($compteur < 10000) { 
      $zero2 = "0"; }
    else {
      $zero2 = ""; }
#
    push (@lisfic, "$fic$zero1$nbp-$zero2$compteur");
    $compteur = $compteur + 1;
  }
return @lisfic;
}#list_extens_file_names


sub restihpextens {
  $FIL1 = @_[0];
  $FIL2 = @_[1];
  $zero1 = "";
  $zero2 = "";
  $compteur = 0;
  $ncsize = $NCSIZE - 1;

  if ($ncsize < 10) {
    $zero1 = "0000"; }
  elsif ($ncsize < 100) { 
    $zero1 = "000"; }
  elsif ($ncsize < 1000) { 
    $zero1 = "00"; }
  elsif ($ncsize < 10000) { 
    $zero1 = "0"; }
  else {
          $zero1 = ""; }

  while ($compteur <= $ncsize) {
    if ($compteur < 10) {
      $zero2 = "0000"; }
    elsif ($compteur < 100) { 
      $zero2 = "000"; }
    elsif ($compteur < 1000) { 
      $zero2 = "00"; }
    elsif ($compteur < 10000) { 
      $zero2 = "0"; }
    else {
      $zero2 = ""; }
#
# Modif 18/01/00 : renommer les fichiers plutot que les copier
#
#   copy("$REPLANCE$ps"."$REP$ps"."$FIL1$zero1$ncsize-$zero2$compteur",
#        "$FIL2$zero1$ncsize-$zero2$compteur") || erreurresti($FIL2);
#
#jaj
#    rename("$REPLANCE$ps"."$REP$ps"."$FIL1$zero1$ncsize-$zero2$compteur",
#         "$FIL2$zero1$ncsize-$zero2$compteur") || erreurresti($FIL2);
    copy("$REP$ps"."$FIL1$zero1$ncsize-$zero2$compteur",
         "$FIL2$zero1$ncsize-$zero2$compteur") || erreurresti($FIL2);

    $compteur = $compteur + 1;
  }
}

sub erreurresti {
  $FIL2 = @_[0];
  ecrire("ERREUR A LA RESTITUTION DU FICHIER", "ERROR : RESTITUTION FILE", $FIL2);
#jaj
#  if ($sortie eq "listing") {

   open(F, ">>$REPLANCE$ps"."$PARA$WORKING"."_error.log");
   copie("ERREUR A LA RESTITUTION DU FICHIER", "ERROR : RESTITUTION FILE", $FIL2,
          "$REPLANCE$ps"."$PARA$WORKING"."_error.log"); 
   close(F);

  if ($sortie eq "listing") {
   unlink("$REPLANCE$ps"."$PARA$WORKING"."_sortie.log");
  }
  exit;
}

sub restihp {
 $FIL1 = @_[0];
 $FIL2 = @_[1];
# modif juillet 2007 RN : copie en cas de fichiers decoupes : 
# reprendre le nom de base, restituer les fichiers avec les
# extensions.
#
# Modif 18/01/00 : renommer les fichiers plutot que les copier
#
# rename("$BASE$ps"."$REPLANCE$ps"."$REP$ps"."$FIL1", $FIL2) || erreurresti($FIL1);
#
# Modif 22/06/00 A. Bas (Steria) : on renome l ancien fichier par nom.old
#
print "dans restihp";
if (-e $FIL2 )
        {
        if (-e $FIL2.".old")
                {
                unlink ($FIL2.".old");
                }
        rename ($FIL2, $FIL2.".old");
        }
#jaj
#rename("$REPLANCE$ps"."$REP$ps"."$FIL1", $FIL2) || erreurresti($FIL1);
copy("$REP$ps"."$FIL1", $FIL2) || erreurresti($FIL1);
}

sub restihpmulti
{
 $FIL1 = @_[0];
 $FIL2 = @_[1];

#jaj
# $DIR="$REPLANCE$ps"."$REP";
 $DIR="$REP";
 opendir (HDIR,$DIR);

# 

 foreach (grep {/$FIL1/} readdir (HDIR))
 {
   $NEW = $_;
   $NEW =~ s/$FIL1/$FIL2/;
   printf ("    -  $NEW \n");

   #jaj rename("$DIR$ps"."$_", "$NEW") || erreurresti($_);
    if (-e $NEW )
        {
        if (-e $NEW.".old")
                {
                unlink ($NEW.".old");
                }
        rename ($NEW, $NEW.".old");
        }
    copy ("$DIR$ps"."$_", "$NEW") || erreurresti($_);
 }
}

sub restihpPARAL    # (ficnamcode, ficnam, ficGEOM, modepar, autopar)
#_______________________________________________________________________
#
#  Restitution des fichiers en mode PARALLEL  ($NCSIZE > 1)
#_______________________________________________________________________
#
{#restihpPARAL
 my $FIL1 = @_[0];      #nom (de base) du fichier pour le code apres acquisition
 my $FIL2 = @_[1];      #nom utilisateur du fichier
 my $ficGEOM = @_[2];   #nom du fichier GEOMETRIE du code
 my $modepar =@_[3];    #type de gestion parallèle : SCAL, SELAFIN ou PARAL
 my $autopar =@_[4];    #mode parallelisme automatique ou non 

#
#---- Les fichiers "SCAL"
#
  if  ($modepar =~ /SCAL/)
   {
   	 # RN - more general : restihp ( $FIL1, $FIL2);
   	 restihpmulti ( $FIL1, $FIL2);
   	 return;
   }
#
#---- Les fichiers "SELAFIN" sont a regrouper avec GRETEL en
#      mode "autopar"
#     Sinon, les retourner avec les fonctions "extens"
  if  ($modepar =~ /SELAFIN/)
   {
   	  if ($autopar eq "0")
   	    { restihpextens ( $FIL1, $FIL2);
   	      return;
   	    }
   	  else
   	    { chdir("$REP");  #jaj hihi...
   	      $rcg = RunGretel ($ficGEOM, $FIL1, $NCSIZE);
              if ( $rcg != 0 )  
                {
                  open(F, ">>$REPLANCE$ps"."$PARA$WORKING"."_error.log");
                  printf F "Gretel error: \n";
                  printf F "Merging $NCSIZE partitions into file $FIL1 \n";
                  printf F "returns with error code $rcg \n";
                  printf F "Probably corrupt result files. \n";
                  close(F);
                  $FILEG = "$REPLANCE".$ps."gretel.log";
                  if (-e $FILEG )
                    {
                      if (-e $FILEG.".old")
                      {
                        unlink ($FILEG.".old");
                      }
                      rename ($FILEG, $FILEG.".old");
                    }
                  copy ("gretel.log", $FILEG );
                  # save the files (ok?)
                  chdir("$REPLANCE");
                  restihpextens ( $FIL1, $FIL2);
                  exit 1;  # generate error exit (questionable for gretel?)
                }
              # copy ("gretel.log", "$REPLANCE".$ps."gretel.log" );
   	      # chdir("..");
              chdir("$REPLANCE");
              # restihp   ("gretel.log", "gretel.log");
   	      restihp   ( $FIL1, $FIL2);
          ecrire("    (regroupe pour $NCSIZE processeurs)",
                 "    (merged from $NCSIZE processors)",    " ");
          return;
        }
   } #modepar=/SELAFIN/
#
#---- Les fichiers "DELWAQPTS" sont a regrouper avec GREDELPTS en
#      mode "autopar"
#     Sinon, les retourner avec les fonctions "extens"
  if  ($modepar =~ /DELWAQPTS/)
   {
   	  if ($autopar eq "0")
   	    { restihpextens ( $FIL1, $FIL2);
   	      return;
   	    }
   	  else
   	    { chdir("$REP");  #jaj hihi...
  	      $rcg = RunGredelpts ($ficGEOM, $FIL1, $NCSIZE);
              if ( $rcg != 0 )  
                {
                  open(F, ">>$REPLANCE$ps"."$PARA$WORKING"."_error.log");
                  printf F "Gredelpts error: \n";
                  printf F "Merging $NCSIZE partitions into file $FIL1 \n";
                  printf F "returns with error code $rcg \n";
                  printf F "Probably corrupt result files. \n";
                  close(F);
                  $FILEG = "$REPLANCE".$ps."gredelpts.log";
                  if (-e $FILEG )
                    {
                      if (-e $FILEG.".old")
                      {
                        unlink ($FILEG.".old");
                      }
                      rename ($FILEG, $FILEG.".old");
                    }
                  copy ("gredelpts.log", $FILEG );
                  chdir("$REPLANCE");
                  restihpextens ( $FIL1, $FIL2);
                  exit 1;  # generate error exit (questionable for gredelpts?)
                }
              chdir("$REPLANCE");
   	      restihp   ( $FIL1, $FIL2);
          ecrire("    (regroupe pour $NCSIZE processeurs)",
                 "    (merged from $NCSIZE processors)",    " ");
          return;
        }
   } #modepar=/DELWAQPTS/
#
#---- Les fichiers "DELWAQSEG" sont a regrouper avec GREDELSEG en
#      mode "autopar"
#     Sinon, les retourner avec les fonctions "extens"
  if  ($modepar =~ /DELWAQSEG/)
   {
   	  if ($autopar eq "0")
   	    { restihpextens ( $FIL1, $FIL2);
   	      return;
   	    }
   	  else
   	    { chdir("$REP");  #jaj hihi...
    	      $rcg = RunGredelseg ($ficGEOM, $FIL1, $NCSIZE);
              if ( $rcg != 0 )  
                {
                  open(F, ">>$REPLANCE$ps"."$PARA$WORKING"."_error.log");
                  printf F "Gredelseg error: \n";
                  printf F "Merging $NCSIZE partitions into file $FIL1 \n";
                  printf F "returns with error code $rcg \n";
                  printf F "Probably corrupt result files. \n";
                  close(F);
                  $FILEG = "$REPLANCE".$ps."gredelseg.log";
                  if (-e $FILEG )
                    {
                      if (-e $FILEG.".old")
                      {
                        unlink ($FILEG.".old");
                      }
                      rename ($FILEG, $FILEG.".old");
                    }
                  copy ("gredelseg.log", $FILEG );
                  chdir("$REPLANCE");
                  restihpextens ( $FIL1, $FIL2);
                  exit 1;  # generate error exit (questionable for gredelseg?)
                }
              chdir("$REPLANCE");
   	      restihp   ( $FIL1, $FIL2);
          ecrire("    (regroupe pour $NCSIZE processeurs)",
                 "    (merged from $NCSIZE processors)",    " ");
          return;
        }
   } #modepar=/DELWAQSEG/
#
#---- Les fichiers "DELWAQMET" sont a regrouper avec GREDELMET en
#      mode "autopar"
#     Sinon, les retourner avec les fonctions "extens"
  if  ($modepar =~ /DELWAQMET/)
   {
   	  if ($autopar eq "0")
   	    { restihpextens ( $FIL1, $FIL2);
   	      return;
   	    }
   	  else
   	    { chdir("$REP");  #jaj hihi...
  	      $rcg = RunGredelmet ($ficGEOM, $FIL1, $NCSIZE);
              if ( $rcg != 0 )  
                {
                  open(F, ">>$REPLANCE$ps"."$PARA$WORKING"."_error.log");
                  printf F "Gredelmet error: \n";
                  printf F "Merging $NCSIZE partitions into file $FIL1 \n";
                  printf F "returns with error code $rcg \n";
                  printf F "Probably corrupt result files. \n";
                  close(F);
                  $FILEG = "$REPLANCE".$ps."gredelmet.log";
                  if (-e $FILEG )
                    {
                      if (-e $FILEG.".old")
                      {
                        unlink ($FILEG.".old");
                      }
                      rename ($FILEG, $FILEG.".old");
                    }
                  copy ("gredelmet.log", $FILEG );
                  chdir("$REPLANCE");
                  restihpextens ( $FIL1, $FIL2);
                  exit 1;  # generate error exit (questionable for gredelmet?)
                }
              chdir("$REPLANCE");
   	      restihp   ( $FIL1, $FIL2);
          ecrire("    (regroupe pour $NCSIZE processeurs)",
                 "    (merged from $NCSIZE processors)",    " ");
          return;
        }
   } #modepar=/DELWAQMET/
#
#---- Les fichiers "DELWAQHYD" sont a regrouper avec GREDELHYD en
#      mode "autopar"
#     Sinon, les retourner avec les fonctions "extens"
  if  ($modepar =~ /DELWAQHYD/)
   {
   	  if ($autopar eq "0")
   	    { restihpextens ( $FIL1, $FIL2);
   	      return;
   	    }
   	  else
   	    { chdir("$REP");  #jaj hihi...
  	      $rcg = RunGredelhyd ($ficGEOM, $FIL1, $NCSIZE);
              if ( $rcg != 0 )  
                {
                  open(F, ">>$REPLANCE$ps"."$PARA$WORKING"."_error.log");
                  printf F "Gredelhyd error: \n";
                  printf F "Merging $NCSIZE partitions into file $FIL1 \n";
                  printf F "returns with error code $rcg \n";
                  printf F "Probably corrupt result files. \n";
                  close(F);
                  $FILEG = "$REPLANCE".$ps."gredelhyd.log";
                  if (-e $FILEG )
                    {
                      if (-e $FILEG.".old")
                      {
                        unlink ($FILEG.".old");
                      }
                      rename ($FILEG, $FILEG.".old");
                    }
                  copy ("gredelhyd.log", $FILEG );
                  chdir("$REPLANCE");
                  restihpextens ( $FIL1, $FIL2);
                  exit 1;  # generate error exit (questionable for gredelhyd?)
                }
              chdir("$REPLANCE");
   	      restihp   ( $FIL1, $FIL2);
          ecrire("    (regroupe pour $NCSIZE processeurs)",
                 "    (merged from $NCSIZE processors)",    " ");
          return;
        }
   } #modepar=/DELWAQHYD/
#
#---- Les fichiers "PARAL" sont a gerer avec des suffixes pour
#     chaque processeur
  if  ($modepar =~ /PARAL/)
   {
      @lfic=list_extens_file_names($FIL1, $NCSIZE);
#
#------- Restituer les fichiers issus du calcul et qui sont specifiques
#        a chaque processeur avec des extensions numeriques
#
  	  foreach (@lfic)
   	        {
               my $f2=$_; $f2=~s/$FIL1/$FIL2/;
               # RN : restihp  ("$_",$f2);
               restihpmulti  ("$_",$f2);
   	        }
   	  return;
    }

} #restihpPARAL



sub RunPartel       # (geo, cli, NCSIZE, sec, secname); #jaj added sec, secname)
#_______________________________________________________________________
#
#  Partitionnement automatique avec PARTEL
#_______________________________________________________________________
#
{#RunPartel

 #Arguments de PARTEL dans "partel.par"
  open(FPAR,">partel.par") or die "File \'partel.par\' cannot be opened!";
 #PARTEL parameters (METIS_PartMeshDual method always choosen [=1])
  #print FPAR "@_[0]\n@_[1]\n@_[2]\n1\n0\n\n";           #jaj without sec
  if (@_[4] eq "") {$ifsec=0;} else {$ifsec=1;}          #jaj
  print FPAR "@_[0]\n@_[1]\n@_[2]\n1\n$ifsec\n@_[3]\n";  #jaj
  close(FPAR) or die "File \'partel.par\' cannot be closed!";
# partel outputs redirected to a file
  $command=join "",$PathParall,"partel < partel.par >> partel.log";
# this line will redirect partel outputs to screen or listing instead of partel.log
# $command=join "",$PathParall,"partel < partel.par ";
  $ret = system ("$command");
  return $ret;
}#RunPartel (end)

sub RunGretel       # (geo, res, NCSIZE);
#_______________________________________________________________________
#
#  Consolidation automatique avec GRETEL
#_______________________________________________________________________
#
{#RunGretel

 #Arguments de GRETEL dans "partel.par"
  open(FPAR,">gretel.par") or die "File \'gretel.par\' cannot be opened!";
  print FPAR "@_[0]\n@_[1]\n@_[2]\n";
  close(FPAR) or die "File \'gretel.par\' cannot be closed!";
 #Lancement GRETEL, append outputs
  $command=join "",$PathParall,"gretel < gretel.par >> gretel.log";
  $ret = system ("$command");
  return $ret;
}#RunGretel (end)

sub RunGredelpts       # (geo, res, NCSIZE);
#_______________________________________________________________________
#
#  Consolidation automatique avec GREDELPTS
#_______________________________________________________________________
#
{#RunGredelpts

 #Arguments de GREDELPTS dans "partel.par"
  open(FPAR,">gredelpts.par") or die "File \'gredelpts.par\' cannot be opened!";
  print FPAR "@_[0]\n@_[1]\n@_[2]\n";
  close(FPAR) or die "File \'gredelpts.par\' cannot be closed!";
 #Lancement GREDELPTS, append outputs
  $command=join "",$PathParall,"gredelpts < gredelpts.par >> gredelpts.log";
  $ret = system ("$command");
  return $ret;
}#RunGredelpts (end)

sub RunGredelseg       # (geo, res, NCSIZE);
#_______________________________________________________________________
#
#  Consolidation automatique avec GREDELSEG
#_______________________________________________________________________
#
{#RunGredelseg

 #Arguments de GREDELSEG dans "partel.par"
  open(FPAR,">gredelseg.par") or die "File \'gredelseg.par\' cannot be opened!";
  print FPAR "@_[0]\n@_[1]\n@_[2]\n";
  close(FPAR) or die "File \'gredelseg.par\' cannot be closed!";
 #Lancement GREDELSEG, append outputs
  $command=join "",$PathParall,"gredelseg < gredelseg.par >> gredelseg.log";
  $ret = system ("$command");
  return $ret;
}#RunGredelseg (end)

sub RunGredelmet       # (geo, res, NCSIZE);
#_______________________________________________________________________
#
#  Consolidation automatique avec GREDELMET
#_______________________________________________________________________
#
{#RunGredelmet

 #Arguments de GREDELMET dans "partel.par"
  open(FPAR,">gredelmet.par") or die "File \'gredelmet.par\' cannot be opened!";
  print FPAR "@_[0]\n@_[1]\n@_[2]\n";
  close(FPAR) or die "File \'gredelmet.par\' cannot be closed!";
 #Lancement GREDELMET, append outputs
  $command=join "",$PathParall,"gredelmet < gredelmet.par >> gredelmet.log";
  $ret = system ("$command");
  return $ret;
}#RunGredelmet (end)

sub RunGredelhyd       # (geo, res, NCSIZE);
#_______________________________________________________________________
#
#  Consolidation automatique avec GREDELHYD
#_______________________________________________________________________
#
{#RunGredelhyd

 #Arguments de GREDELHYD dans "partel.par"
  open(FPAR,">gredelhyd.par") or die "File \'gredelhyd.par\' cannot be opened!";
  print FPAR "@_[0]\n@_[1]\n@_[2]\n";
  close(FPAR) or die "File \'gredelhyd.par\' cannot be closed!";
 #Lancement GREDELHYD, append outputs
  $command=join "",$PathParall,"gredelhyd < gredelhyd.par >> gredelhyd.log";
  $ret = system ("$command");
  return $ret;
}#RunGredelhyd (end)

############################################
#    Machine MPI : gestion du fichier de
#    configuration de la machine MPI
#      (avec MPICH)
############################################

sub mpi_config {

#---- Ouverture fichier 'mpi_telemac.conf'

$ret = stat($MPICONF);
if ($ret == 0)
  {
   ecrire ("ERREUR : Fichier inexistant :","ERROR : File not found :", $MPICONF);
   return 1;
  }
open (F, "<$MPICONF") or die "## Error : Open the $MPICONF file is impossible\n";

#--- Preparation fichier de run "mpirun.txt"

open (F2, ">mpirun.txt") or die "## Error : Open the 'mpirun.txt' file is impossible\n";

if($ENV{"OS"} eq "Windows_NT")                #MPICH.NT specific
  {
    my $s1="$REPLANCE";
#- Traitement de l'option UNCPATH
    if ( ($UNCPATH eq "oui") && ($TM_UNCPATH ne "") )
       { $s1 =~s/^[A-Za-z]:/$TM_UNCPATH/; }

    print F2 "exe $s1$ps$REP$ps$EXEFILE\n";
    print F2 "hosts\n";
  }

#---- Liste des hosts + NbProc

my $ideb=0;
while ($LIGNE=<F>)
{
  $FIRST_CAR = substr( $LIGNE, 0,1);
  if ( $FIRST_CAR ne '#' )                    # sauter les commentaires
   {
    ($LIGNE, $TMP)  = split(/\n/,$LIGNE);     # On enleve le NewLine a la fin
    $LIGNE =~ s/ *$//;                        # On enleve les espaces terminaux

#----- Nombre de processeurs

    if ($ideb == 0)
      { $MPI_NPROC=$LIGNE; $ideb++;}
    else
#----- Traitement de l'info (Host, NbProc)   
      {
        my ($hostmpi, $nbprochost)=($LIGNE=~/^\s*(\S*)\s*(\S*)\s*$/);
        if($ENV{"OS"} eq "Windows_NT")                
                { print F2 "$hostmpi $nbprochost\n";} #MPICH.NT specific
        else    { for ($k=0;$k<$nbprochost;$k++)
                     { print F2 "$hostmpi\n";}       #MPICH.Unix
                }
      }
#------ lignes suivantes

   } # if '#'
} # while <F>

close(F)  or die "## Error : Close the $MPICONF file is impossible\n";
close(F2) or die "## Error : Close the 'mpirun.txt' file is impossible\n";

#Conserver le fichier "mpirun.txt" qui a ete utilise
#jaj
#copy("mpirun.txt","..$ps" );
copy("mpirun.txt","$REPLANCE$ps" );

return 0;

} #fin_mpi_config

############################################
#    /      PROGRAMME PRINCIPAL
############################################
# Fichier de gestion d'erreur : doit disparaitre si run OK
open (FIC, ">err$WORKING.err"); close (FIC);

#jaj the sequence of actions is changed: 
#    (1) copy files required for compilation 
#    (2) compile & link (and break if required)
#    (3) copy all files required for execution
#    (4) in parallel case, do partitioning
#    (5) execute


if( $COMPILE_LINK eq "1" ) # copy only the fortran file 
  { 
    genereligne();
    ecrire("*** ONLY COMPILATION AND LINKING REQUIRED ***", 
           "*** ONLY COMPILATION AND LINKING REQUIRED ***");
    print "\n";
  }

$Directory_tmp=join "", $REPLANCE;
chdir($Directory_tmp) or die "## Error : chdir \"$Directory_tmp\" impossible :$!";

# le fichier FORTRAN est traite a part pour compilation
   @FicFortToCompile=();
   foreach $FORTFIC (@FORTRANS)
   {
   ($FORTRAN,$FORAC)=split (/;/,$FORTFIC);
    if ($FORTRAN ne "DEFAUT")
      { acquihp($FORTRAN, $FORAC);        #mot clef "FICHIER FORTRAN"
        ecrire_FicTitrVal("FICHIER FORTRAN","FORTRAN FILE", $FORTRAN);
        push (@FicFortToCompile, $FORAC);
      }
#On ajoute les fichiers Fortran inclus (FORTINC)
      foreach $FORTFIC (@FORTINC)
      {
      ($FORTR,$FORAC)=split (/;/,$FORTFIC);
      acquihp($FORTR, $FORAC);        #mot clef "FICHIER FORTRAN"
      ecrire_FicTitrVal("FICHIER FORTRAN INCLU","FORTRAN INCLUDE FILE", $FORTR);
      }
   }#foreach FORTFIC
   $FORTRAN=@FORTRANS[0];                #pour le nom de l'exe local
   ($FORTRAN,$TMP)=split (/;/,$FORTRAN); #sur la base du premier FORTRAN

# now do all the compilation and link actions  

$Directory_tmp=join "",$REP;
chdir($Directory_tmp) or die "## Error : chdir \"$Directory_tmp\" impossible :$!";

# PAS DE GENERATION LOCALE DE L'EXECUTABLE POUR LE CRAY OU LE FUJITSU

if ($CRAY ne "oui" && $FUJI ne "oui") {

############################################################################
#   CREATION EVENTUELLE DE L'EXECUTABLE
#   OU UTILISATION DE L'EXECUTABLE LOCAL si valide
############################################################################

genereligne();
$MPSFX = ($NCSIZE > 0) ? '_MP' : '';

#---------- Executable par defaut/local

if ($FORTRAN ne "DEFAUT")                        # Executable par defaut ?
{
#--- Y a-t-il un exe "local" valide ?
  $uselocal=0;
# $localexe=basename($FORTRAN,".f")."_$dirlib"."$MPSFX".".exe";
  $localexe=basename($FORTRAN,".f")."_$dirlib"."$MPSFX"."_$VERS[$0]".".exe";
  if (  ( -e "$REPLANCE".$ps."$localexe" )
     && ( -e "$REPLANCE".$ps."$FORTRAN" )    )           # exe "local" existe ?
   {
    ($z,$z,$z,$z,$z,$z,$z,$z,$z,$m1time,$z,$z,$z) = stat("$REPLANCE".$ps."$localexe");
    $uselocal=1;

#--- un fichier Fortran ou Fortran_inclus plus recent ?
    my @lisf=@FORTRANS; push(@lisf,@FORTINC);
    foreach $FORTFIC (@lisf)
      {
      ($FORTR,$FORAC)=split (/;/,$FORTFIC);
       if (-e "$REPLANCE"."$ps$FORTR")
          {
           ($z,$z,$z,$z,$z,$z,$z,$z,$z,$m2time,$z,$z,$z) = 
            stat("$REPLANCE"."$ps$FORTR");
            if ( $m1time < $m2time )    #existe plus recent ->  recompiler
               { $uselocal=0;}
          }
      }

   } #localexe+FORTRAN

  if ( $uselocal==1 )                          # on utilise l'exe "local"
  {
  ecrire ("*** EXECUTABLE LOCAL ***\n", "*** LOCAL EXECUTABLE ***\n");
  print "   $localexe\n\n";

  copy("$REPLANCE".$ps."$localexe","$EXEFILE" ) ||
         ecrire("EXECUTABLE LOCAL INEXISTANT", "LOCAL EXECUTABLE NOT FOUND");
          # can it happen there's no executable at theis stage?
#jaj chmod(0755,"$EXEFILE");
  chmod(0744,"$EXEFILE");
  }

#---------- Executable a construire : compilation + link

  else   # Pas d'executable utilisable -> Compilation+Link
  {

#---- Compilation
  ecrire("*** COMPILATION ***", "*** COMPILATION ***");
  printf "\n";
  printf " $compile $INCDIR @FicFortToCompile \n";
  $TMP  = `$compile $INCDIR @FicFortToCompile`;
  
# Traitement des erreurs de compilation
  $errcomp=0;
  @FicToLink=();
  foreach $f (@FicFortToCompile)
    { $fobj=$f;
# Fortran files may be with .f90 or .f extension
      $fobj=~s/\.f90/\.$objExt/g;
      $fobj=~s/\.f/\.$objExt/g;
      push (@FicToLink,$fobj);
      if (not(-e $fobj)) {$errcomp=1;}
    }
  if (($TMP=~/error|erreur/i) || ($errcomp!=0)) # tests erreurs
  { 
    ecrire("ERREUR A LA COMPILATION", "ERROR : COMPILATION");
    #jaj chdir("..");
    chdir ("$REPLANCE");
    open(F, ">>$REPLANCE$ps"."$PARA$WORKING"."_error.log");
    copie("ERREUR A LA COMPILATION",
          "ERROR : COMPILATION", $FIL1,
          "$REPLANCE$ps"."$PARA$WORKING"."_error.log"); 
    printf F "$TMP\n";
    printf "$TMP\n";
    close(F);
    exit;
  } 

#---- Link
genereligne();
ecrire("*** BIBLIOTHEQUES ***\n", "*** LIBRARIES ***\n");

$tmp=$BIBLI; $tmp =~ s/^\s//;
@MSGBIB=split (/ /,$tmp);
foreach (@MSGBIB)
  { if ( "$_" ne "" ) { printf " - $_\n";}
  }
printf "\n";

ecrire("*** EDITION DE LIENS ***", "*** LINKING ***");
print "\n";

if ($NCSIZE > 0 )   #cas parallèle : MPIlink
  {
    $dolink=$lkmpi;
    $dolink=~s/<EXE>/$EXEFILE/;
#   version Uwe Merkel, ne marche pas sur LInux ????
#   $dolink=~s/<EXE>/.\$EXEFILE/;
    $dolink=~s/<OBJS>/@FicToLink/;
    $dolink=~s/<LIBS>/$BIBLI/;
  }
 else               #cas scalaire
  { $dolink =  "$linkage";
    $dolink =~ s/a\.exe/$EXEFILE/;       # nom de l'exe = "out####.exe"
    $dolink = "$dolink @FicToLink $BIBLI";
  }

#Lancement LINK
#print "SA-dolink = $dolink\n";
$TMP  = `$dolink`;
  print $TMP; $TMP=`echo $dolink >lk`; 

#----------------------------------------
# BUG ABsoft sur option /out:   inoperante
# L'exe porte le nom "source.exe"
if ( ($dirlib eq "win_ab") && ( ! -e "$EXEFILE" ) && ( -e "source.exe" ))
       {  copy ("source.exe", "$EXEFILE"); }
#----------------------------------------

#Verif disponibilite EXE et validite du link
if  ( ( ! -x "$EXEFILE" ) || ($TMP=~/error|erreur/i) )
  {
    ecrire("ERREUR A L'EDITION DE LIEN", "ERROR : DURING LINKING");
    print "$TMP\n";
    #jaj chdir("..");
    chdir ("$REPLANCE");
    open(F, ">>$REPLANCE$ps"."$PARA$WORKING"."_error.log");
    copie("ERREUR A L'EDITION DE LIEN",
          "ERROR : DURING LINKING", $FIL1,
          "$REPLANCE$ps"."$PARA$WORKING"."_error.log"); 
    printf F "$TMP\n";
    close(F);
    exit;
  } 

  #jaj chmod(0755,"$EXEFILE");
   chmod(0744,"$EXEFILE");

#jaj orig
#copy ("$EXEFILE", "..$ps"."$localexe");  # le garder pour le reutiliser
copy ("$EXEFILE", "$REPLANCE".$ps."$localexe");  # le garder pour le reutiliser

  }   # if uselocal
}     # if FORTRAN <> DEFAUT

#############################################################################
#    EXECUTABLE PAR DEFAUT
#############################################################################
else                  #if ($FORTRAN ne "DEFAUT")
{
#jaj:
  if ($NCSIZE < 1 ) {
    ecrire("*** EXECUTABLE PAR DEFAUT ***\n", "*** DEFAULT EXECUTABLE ***\n");
  } else {
    ecrire("*** EXECUTABLE PARALLEL PAR DEFAUT ***\n", "*** DEFAULT PARALLEL EXECUTABLE ***\n");
  }
  print "   $RUNEXEDEF\n";
  copy("$RUNEXEDEF","$EXEFILE" ) || 
      ecrire("EXECUTABLE PAR DEFAUT INEXISTANT", "DEFAULT EXECUTABLE NOT FOUND");

# no action taken? 
#    {
#    ecrire("EXECUTABLE PAR DEFAUT INEXISTANT", "DEFAULT EXECUTABLE NOT FOUND");
#    open(F, ">>$REPLANCE$ps"."$PARA$WORKING"."_error.log");
#    copie("ERREUR : EXECUTABLE PAR DEFAUT INEXISTANT",
#          "ERROR : DEFAULT EXECUTABLE NOT FOUND", $FIL1,
#          "$REPLANCE$ps"."$PARA$WORKING"."_error.log"); 
#    printf F "$TMP\n";
#    close(F);
#    exit;
#    }

  chmod(0755,"$EXEFILE");
  }

#jaj compilation and linking and/ are OK so far,
#    if not, the script exited previously 
#    remove the file indicating an error

  if ( -e "err"."$WORKING.err" ) { unlink "err"."$WORKING.err"; }

#jaj according to Pallas consultants

if( $COMPILE_LINK eq "1" ) 
  { 
    ecrire("*** COMPILATION AND LINKING FINISHED ***", 
           "*** COMPILATION AND LINKING FINISHED ***");
    exit; 
  }

#jaj ========================================
# copying all files required for execution
#============================================

$Directory_replance=join "",$REPLANCE;
chdir($Directory_replance) or die "## Error : chdir \"$Directory_replance\" impossible :$!";


############################################
#    /      ACQUISITION DES FICHIERS
#
#    fichiers avec marqueur "LIT"
#
############################################

genereligne();
ecrire("*** ACQUISITION DES FICHIERS ***", "*** ALLOCATION OF USER FILES ***");
print "\n";

  $Directory_tmp=join "", $REPLANCE;
  chdir($Directory_tmp) or die "## Error : chdir \"$Directory_tmp\" impossible :$!";

#sa1 # le fichier CAS est traite a part et toujours nomme FORT.3
#sa1   if ($CAS ne "")
#sa1     { acquihp($CAS, "FORT.3");              #mot clef "FICHIER DES PARAMETRES"
#sa1       ecrire_FicTitrVal("FICHIER CAS","STEERING FILE", $CAS);
#sa1     }
#sa1   else
#sa1     { acquihp($PARA, "FORT.3");             #argument ligne de commande
#sa1       ecrire_FicTitrVal("FICHIER CAS","STEERING FILE", $PARA);
#sa1     }
#sa1 
# jaj this has been done before 
### le fichier FORTRAN est traite a part et toujours nomme "source.f"
##  if ($FORTRAN ne "DEFAUT")
##    { acquihp($FORTRAN, "source.f");        #mot clef "FICHIER FORTRAN"
##      ecrire_FicTitrVal("FICHIER FORTRAN","FORTRAN FILE", $FORTRAN);
##    }

#sa7 # le dictionnaire est traite a part et toujours nomme FORT.2
#sa7   acquihp($DICO, "FORT.2");
#sa7 

# les fichiers CONDITIONS LIMITES pour le parallelisme doivent etre "acqui" par
# avance pour pouvoir utiliser PARTEL
  if ( ( $NCSIZE > 1) && (scalar(@conlimDSC) >=1 )  )
    {
     foreach $clDSC (@conlimDSC)
        { my @v=split(";", $clDSC);
          my $f=$v[0]; 
          $conlimF=$v[1]; push (@clF, $conlimF);
          if ($AUTOPAR eq "1")     {acquihp( $$f, $conlimF);}
                              else {acquihpextens( $$f, $conlimF);}
        }
    }

#jaj similar procedure for sections input file
  if ( ( $NCSIZE > 1) && (scalar(@sectionDSC) >=1 )  )
    {
     foreach $seDSC (@sectionDSC)
        { my @v=split(";", $seDSC);
          my $f=$v[0]; 
          $sectionF=$v[1]; push (@seF, $sectionF);
          push (@seF2, $$f);
          if ($$f ne "")
          {
            if ($AUTOPAR eq "1") {acquihp( $$f, $sectionF);}
                            else {acquihpextens( $$f, $sectionF);}
          }
        }
    }

# Autres fichiers decrits dans le dictionnaire du code :
$i=0; 
$iclCOD=0; $conlimF=@clF[$iclCOD];
$iseCOD=0; $sectionF=@seF[$iseCOD]; #jaj
           $sectionFname=@seF2[$iseCOD]; #jaj

foreach (@FDESC)
{
    ($varnam,$ficnamcod,$oblig,$fictyp,$ficmod,$modepar) = split (/;/,$_);

#detecter le changement de code (couplage) et changer de fichier C.L. et sections #jaj
    if ($varnam eq "NEWCODE") 
       {$iclCOD++; 
        $conlimF=@clF[$iclCOD]; 
        $sectionF=@seF[$iclCOD];  #jaj
        $sectionFname=@seF2[$iclCOD]; #jaj
        next;
       }
    
#seulement les fichiers LUS (marqueur "LIT")
    if ( ($$varnam ne "") && ( $ficmod =~ /LIT/ ) )
    {
#acquisition
      ecrire_FicTitrVal($FLNG1[$i],$FLNG2[$i], $$varnam);
#
      if ( $NCSIZE > 1)
        { acquihpPARAL  ( $$varnam, "$ficnamcod", $modepar, $conlimF, $AUTOPAR, $sectionF, $sectionFname);} #jaj
      else
        { acquihp       ( $$varnam, "$ficnamcod");}

    }
    $i++;
}

#jaj orig
#$Directory_tmp=join "", $REPLANCE,$ps,$REP;
$Directory_tmp=join "",$REP;
chdir($Directory_tmp) or die "## Error : chdir \"$Directory_tmp\" impossible :$!";


#######################################
#  Fichier CONFIG
#    - 1/2 = LNG
#    - 6
#######################################

open(F, ">CONFIG");
printf F "$LNG\n";
printf F "6\n";
close(F);

#######################################
#  CAS PARALLELE :
#     - verification machine MPI
#     - partitionnement automatique
#######################################

if ($NCSIZE > 0)
{
  genereligne();
  ecrire("*** MACHINE MPI ***", "*** MPI MACHINE ***");
#
  $ret = mpi_config();
  if ( $ret != 0)
     { ecrire("ERREUR : machine MPI mal configuree.","ERROR : wrongly configured MPI machine.", " ");
       ecrire("Verifiez le contenu du fichier  $MPICONF .","Please check $MPICONF file.", " ");
       exit 1;
     }
#
   if ( $NCSIZE > $MPI_NPROC )
     { ecrire("ERREUR : Nombre de processeurs insuffisant in parameter file: ", 
              "ERROR  : wrong number of processors in parameter file: ", 
              "$NCSIZE is larger than $MPI_NPROC");
       ecrire("         Please check files: $MPICONF, $PARA", 
              "         Please check files: $MPICONF, $PARA", " ");
       exit 1;
     }

   ecrire(" Machine MPI ok (avec $NCSIZE processeurs).", 
          " MPI machine ok (with $NCSIZE processors).", " ");


}       #if NCSIZE>0


} #FIN DE GENERATION LOCALE : ($CRAY ne "oui" && $FUJI ne "oui")

#######################################
#    /      EXECUTION
#######################################


genereligne();
ecrire("*** EXECUTION ***", "*** RUNNING ***");
print "\n";

#Cas de la commande de lancement si MPI
if ($NCSIZE > 0 )
{
  $cmd_runmpi=$runmpi;
  $cmd_runmpi=~s/<EXE>/$EXEFILE/;
# version Uwe Merkel, ne marche pas sur Linux ???  
# $cmd_runmpi=~s/<EXE>/.\$EXEFILE/;
  $cmd_runmpi=~s/<N>/$NCSIZE/;
  
  ecrire(" lancement MPI : $cmd_runmpi",
         " MPI launcher  : $cmd_runmpi");
 
}

  if ($lancement eq "interactif") {
    if ($sortie eq "listing") {                            #========== ecran + listing
      $filename="$REPLANCE$ps"."$PARA$WORKING"."_sortie.log";
      open (F, ">$filename") or die "## Erreur : Impossible d'ouvrir le fichier : $filename\n";
      close (F)              or die "## Erreur : Impossible de fermer le fichier : $filename\n";
      $t0 = new Benchmark;
      open(F, ">>$filename") or die "## Erreur : Impossible d'ouvrir le fichier : $filename\n";;
      if ( $MODE  eq  "p" )
        { open (JBO, "$runprofile $EXEFILE | ");}          #lance profiling
      else
        {
         if ($NCSIZE > 0 )
              { $cmd_lin=$cmd_runmpi; }                    #parallele MPI
         else { $cmd_lin="$cmd_tim $EXEFILE"; }            #scalaire
   #Lancer:
         open (JBO, "$cmd_lin |");                         #lancement
        }
   #Consommer l'output:
      while (<JBO>)  {print $_; print F $_;}               #output ecran+fichier
   #Bilan CPU
      $t1 = new Benchmark;
      $td = timediff($t1, $t0);
      close (JBO);
      $msgtim=time_affich($td);
      printf   "$msgtim\n";
      printf F "$msgtim\n";
      close(F)  or die "## Erreur : Impossible de fermer le fichier : $filename\n";

         } #if ($sortie eq "listing")
    else {                                                 #============= mode ecran seul :

#run avec lecture output sous NT :
#(pas de bufferisation de la part de WNT)
if($ENV{"OS"} eq "Windows_NT")
{
      $t0 = new Benchmark;
      if ( $MODE  eq  "p" )
        { open (JBO, "$runprofile $EXEFILE |"); }          #profiling DVF
      else
        {
         if ($NCSIZE > 0 )
              { $cmd_lin=$cmd_runmpi; }                    #parallele MPI
         else { $cmd_lin="$cmd_tim $EXEFILE"; }            #scalaire
   #Lancer:
         open (JBO, "$cmd_lin |");                         #lancement
        }
   #Consommer l'output:
      while (<JBO>) { print   $_; }                        #consommer l'output
      close (JBO);
}#($ENV{"OS"} eq "Windows_NT")
else
#Unix interactif ecran pour eviter la bufferisation
{
#########moulinec
#######      $command=join "",$cmd_tim," $EXEFILE";               #mode Normal
      $command=join "",$cmd_tim," ./","$EXEFILE";               #mode Normal
#########moulinec
      if ($DEBUG eq "debug")
#########moulinec
           { $command=join "",$rundebug," ./","$EXEFILE";}      #mode Debug
####       { $command=join "",$rundebug," $EXEFILE";}      #mode Debug
#########moulinec
      if ( $MODE eq "p"  )
#########moulinec
           { $command=join "",$runprofile," ./","$EXEFILE";}    #mode Profile
####       { $command=join "",$runprofile," $EXEFILE";}    #mode Profile
#########moulinec
      if ($NCSIZE > 0 )
           { #jaj 
             #$Directory_tmp=join "", $REPLANCE,$ps,$REP;
             $Directory_tmp=join "", $REP;
           $command=join "","cd $Directory_tmp; ",$cmd_runmpi;
#	   exit;
           }                       #mode MPI
   #Lancer:
      $t0 = new Benchmark;
      system("$command") == 0 or die "## Erreur : Fin anormale : $command :$?\n";
}#($ENV{"OS"} eq "Windows_NT")

   #Bilan CPU (NT/Unix)
      $t1 = new Benchmark;
      $td = timediff($t1, $t0);
      $msgtim=time_affich($td);
      print "$msgtim\n";
          }
  }
  else  #if ($lancement eq "interactif")
       {                          #============= mode BATCH
    if ($CRAY eq "oui" || $FUJI eq "oui" ) {
      $command=join "", "remsh ", $NOM_CIBLE," -l ", $USR_CIBLE," \"", $com_CIBLE, " \"";
      system("$command") == 0 or die "## Erreur : Fin anormale : $command :$?\n";

      # On se met ici en attente de l'apparition du fichier 'FIN_CALCUL' (vide)
      # transfere par la machine CIBLE a la fin du calcul.
      # Le test est realise toutes les 10 minutes pour ne pas charger le reseau.

      $fic_FIN="FIN_CALCUL";
      while (! -e $fic_FIN) { sleep 600 ; }

#Le fichier de sortie n est pas executable 09/06/00 A. Bas

     chmod (0666, LISTING);
     rename(LISTING, "$REPLANCE$ps"."$PARA$WORKING"."_sortie.log");
     chmod (0666, "$REPLANCE$ps"."$PARA$WORKING"."_sortie.log");
    }
    else
    {
      $filename="$REPLANCE$ps"."$PARA$WORKING"."_sortie.log";
      open(F, ">$filename") or die "## Erreur : Impossible d' ouvrir le fichier : $filename\n";
      $t0 = new Benchmark;
      open (JBO, "$cmd_tim $EXEFILE |");                      #run avec output direct
      select F; $| =1;                                        #dans fichier
      while (<JBO>) {
        print  $_;
         }
      close (JBO);
      $t1 = new Benchmark;
      $td = timediff($t1, $t0);
      $msgtim=time_affich($td);
      printf "$msgtim\n";
      select STDOUT; $| =1;
      close(F) or die "## Erreur : Impossible de fermer le fichier : $filename\n";
    }
  }#if ($lancement eq "interactif")

############################################
#    /      RESTITUTION DES FICHIERS
#
#    fichiers avec marqueur "ECR"
#
############################################

#jaj oh, mamma mia...
#chdir("..");

chdir($REPLANCE);

genereligne();
ecrire("*** RESTITUTION DES FICHIERS ***",
       "*** FILES DELIVERY ***");
print "\n";

$i=0;
# foreach (@FDESC)
# {
#     ($varnam,$ficnamcod,$oblig,$fictyp,$ficmod,$modepar) = split (/;/,$_);
#  
# #seulement les fichiers ECRITS (marqueur "ECR")
#     if ( ($$varnam ne "") && ( $ficmod =~ /ECR/ ) )
#     {
# #restitution
#      ecrire_FicTitrVal($FLNG1[$i],$FLNG2[$i], $$varnam);
#  
#      if ($modepar =~ /MULTI/)
#           { restihpmulti  ( "$ficnamcod", "$$varnam");}
#      else
#           { restihp       ( "$ficnamcod", "$$varnam");}
#     } #if ECR
#     $i++;
# }

#Initialisation du nom du fichier CONLIM (premier code)
$iclCOD=0; $geoDSC=@selgeomDSC[$iclCOD];
my @v=split(";", $geoDSC); $geomF=$v[1];

foreach (@FDESC)
{
    ($varnam,$ficnamcod,$oblig,$fictyp,$ficmod,$modepar) = split (/;/,$_);

#Marqueur de changement de code (couplage) : chager fichier GEOM nouveau code
    if ($varnam eq "NEWCODE")
      { $iclCOD=0; $geoDSC=@selgeomDSC[$iclCOD];
        my @v=split(";", $geoDSC); $geomF=$v[1];
        next; 
      }

#seulement les fichiers ECRITS (marqueur "ECR")
    if ( ($$varnam ne "") && ( $ficmod =~ /ECR/ ) )
    {
#restitution
      ecrire_FicTitrVal($FLNG1[$i],$FLNG2[$i], $$varnam);

      if ( $NCSIZE > 1)
        { restihpPARAL  ( "$ficnamcod", $$varnam, $geomF, $modepar, $AUTOPAR); }
      else
        {
	# RN : always use multi ...
#          if ($modepar =~ /MULTI/)
#                { restihpmulti  ( "$ficnamcod", "$$varnam");}
#          else  { restihp       ( "$ficnamcod", "$$varnam");}
             restihpmulti  ( "$ficnamcod", "$$varnam");
        }
    }
    $i++;
}


#######################################
#  CAS PARALLELE :
#   - recup des fichiers log
#######################################

chdir ("$REP");

if ($NCSIZE > 1)
{
#Recuperer les fichiers LOG
  opendir(TMPDIR,"$REP");
  # foreach ( grep {/^p.*\.log/} readdir(TMPDIR))
  foreach ( grep {/.log/} readdir(TMPDIR))
   {
     $FILR = "$REPLANCE".$ps."$_";
     if (-e $FILR )
        {
        if (-e $FILR.".old")
                {
                unlink ($FILR.".old");
                }
        rename ($FILR, $FILR.".old");
        #  copy ($FILR, $FILR.".old");
        }
    #jaj 
    # rename ("$_", "..$ps$_" );
    # strange: rename does not work between different filesystems!!!
    # rename ("$_", "$REPLANCE".$ps."$_" );
    # copy instead
    copy ("$_", "$REPLANCE".$ps."$_" );
   }
  closedir (PIDIR);

}   #if NCSIZE


print "\n";

##########################################################################
#    Fin
##########################################################################
#jaj - not necessary ?
#########unlink "$REP$ps"."err"."$WORKING.err";        # pas d'erreur
chdir("$REPLANCE");
exit;

