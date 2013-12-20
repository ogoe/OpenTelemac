#!perl
#line 8
#------------------------Systeme TELEMAC V5P3-------------runtel.pl----
#
#	Procedures de lancement simplifie d'un code
#           du systeme TELEMAC
#
# SYNTAXE
#	runtel.pl nom_code [-D] [-C|-F] [-s|b|n|d heure] [cas]
#	runtel.pl -h|H                                             (aide)
#----------------------------------------------------------------------
# DESCRIPTION
#	Cette procedure permet de lancer un calcul 'telemac2d' ou 'stbtel'
#	dont toutes les caracteristiques d'execution (physiques et
#	informatiques) sont precisees dans un fichier de parametres
#	(appele 'cas' par defaut).
#
#	A chaque lancement les operations suivantes sont realisees :
#       1) options par defaut
#       2) parametres configurables
#       3) Traitement des options
#          initialisation des variables a partir du nom du generique
#       4) creation du repertoire temporaire
#       5) copie du dictionnaire reduit et du fichier cas dans ce repertoire
#       6) execution de damocle dans ce repertoire
#       7) test sur les mots cles imperatifs concernant les fichiers
#       8) test sur l'existance des fichiers
#       9) copie du fichier unix en entete du fichier $generique.exec
#      10) recherche de la version du code lance
#      11) copie du dictionnaire de la version utilisee
#      12) copie $generique.hp dans $generique.exec
#      13) lancement de $generique.exec :
#               mode interactif - lancement immediat sans sortie listing redirige
#		dans un fichier
#               mode interactif - lancement immediat avec une sortie listing
#               redirige dans un fichier
#               mode batch - lancement immediat en rendant la connexion
#               creation dans le repertoire de lancement d'un fichier
#               delete_$PARA$WORKING  pour effacer le calcul en cours
#               mode nuit - lancement a 20h00 en rendant la connexion
#               creation dans le repertoire de lancement d'un fichier
#               delete_$PARA$WORKING pour effacer le calcul lance en batch
#
#      14) Allocation des fichiers sur des canaux predefinis (FORT.xx)
#	   dans le repertoire de travail apres contrÃÂle de leur existence.
#      15) Compilation du programme principal fournis par l'utilisateur
#	   et edition de liens avec les bibliotheques ad-hoc.
#      16) Lancement de TELEMAC
#      17) Restitution des fichiers resultats aux endroits precises par
#          l'utilisateur dans son fichier 'cas'.
#          Si le nom du fichier resultat existe deja la procedure le renom
#          avec la terminaison ".old"
#
# OPTIONS
#	-h|H	  aide courte ou longue.
#       [ ]       mode interactif sans fichier listing du calcul resultats
#                 seulement a l'ecran
#       -s        mode interactif avec fichier listing du calcul
#	-D	  mode de compilation : Debogueur
#	-b	  lancement en B-atch (depart immediat) on peut se deconnecter
#	-n	  lancement de N-uit (depart a 20 H) on peut se deconnecter
#	-d heure  lancement avec depart D-iffere a 'xx:xx', on peut se deconnecter
#		  l'heure est precise sous la forme de 2 chiffres pour l'heure,
#		  du signe ':' et de 2 chiffres pour les minutes (d'autres
#		  formes compatibles avec la commande 'at' peuvent etre utilisees,
#		  pour cela se reporter au manuel de cette commande).
#       -C        lancement sur CRAY depuis la station locale
#       -F        lancement sur FUJITSU depuis la station locale
#                 Les donnees de connexion sont precisees dans le fichier parametre.
#
# ENVIRONNEMENT
#   Le fonctionnement de cette procedure sous-entend que l'utilisateur a declare
#   dans son environnement la variable PROJECT, cette variable servant a indiquer
#   le chemin du repertoire 'bin' dans lequel est place tous les
#   fichiers utiles.
#   De plus, il faut initialiser les variables suivantes a 1 pour la langue
#   francais ou 2  pour la langue anglaise :
#                               - LNGTEL    -----> pour TELEMAC2D
#                               - LNGTEL3D  -----> pour TELAMC3D
#                               - LNGSTB    -----> pour STBTEL
#                               - LNGSUB    -----> pour SUBIEF
#                               - LNGTSE    -----> pour TSEF
#                               - LNGSPA    -----> pour SPARTACUS2D
#    La variable LC_TIME doit etre initialise a "". Ceci concerne la commande at
#
# FICHIERS
#
#   Dans les explications qui suivent le mot $GENERIQUE vaut pour le nom
#   du code utilise : 'telemac2d', 'stbtel',...
#
#   ---------------------------------------------------------------------
#   Repertoire	cas_$WORKING_tmp
#   Fichiers	cas
#   		  cas_$WORKING_sortie.txt
#                 delete_cas_$WORKING.
#                 cas_$WORKING_error.txt
#   ---------------------------------------------------------------------
#	Pour chaque cas traite cette procedure cree un repertoire temporaire
#	qui contient tout ce qui est necessaire a l'execution du code.
#	Si le fichier de parametres utilise s'appelle 'cas' ce repertoire
#	s'appelle 'cas_$WORKING_tmp' et est situe dans le meme repertoire
#       que le fichier 'cas'. Les impressions du code sont placees dans le fichier
#       'cas_$WORKING_sortie.txt' dans le meme repertoire que le fichier 'cas'.
#       Si une erreur intervient le fichier  cas_$WORKING_error.txt apparait
#       et contient l'explication de l'arret du calcul.
#
#   ---------------------------------------------------------------------
#   Fichier	'$PROJECT/$CODE/share/$GENERIQUE.dico'
#   ---------------------------------------------------------------------
#	Le dictionnaire des mots cles du code au standard DAMOCLE.
#
#   ---------------------------------------------------------------------
#   Fichier	'$PROJECT/$CODE/$RACINE$VERSION$DICO/share/$GENERIQUE.hp'
#   ---------------------------------------------------------------------
#                pour cette nouvelle version contient toutes les allocations
#                des fichiers fortran.
#                C'est ce fichier qui est ensuite execute.
#
#   ---------------------------------------------------------------------
#   Repertoire	cas_$WORKING_tmp
#
#   Fichier	cas_$WORKING_tmp/TESTFIC
#               contient les mots-cles concernant
#               les fichiers obligatoires qui ne sont contenus dans le fichier cas
#               Ce fichier est genere par damocle
#
#   Fichier	cas_$WORKING_tmp/TESTOBL
#               contient les noms des fichiers obligatoires. Ce fichier est genere
#               par damocle
#   ---------------------------------------------------------------------
#
#   ---------------------------------------------------------------------
#   LES VARIABLES UTILISEES
#   /
#   / PROJECT   = racine du projet - variable d'environnement
#   / GENERIQUE = nom du code lance
#   / DICO      = dictionnaire reduit ou complet pour determiner la version
#   / CHEMIN    = chemin de la version du code utilise sans le numero
#   /             de le version
#   / RACINE    = racine de l'arborescence du code lance
#   / PARA       = nom du fichier parametres recupere dans la variables $1
#   / REP       = repertoire temporaire pour l'execution
#   / VERSBAN   = numero de la version du code pour utiliser le banner
#   / VERSDICO  = numero de la version du code pour utiliser la version
#   /             correspondante du dictionnaire
#   / LANCEMENT = type de lancement (immediat-en batch immediat-en batch nuit)
#   / LOGIN     = nom de la connexion
#   / BASE      = repertoire de lancement
#   / WORKING   =  numero du process
#   / DEBUGGER  =  xdb
#   / DEBUG     = pas de debugger
#   / FC        = compilateur f77
#   / MODE      = mode de compilation des bibliotheques
#   /
#   /  OPTIONS PAR DEFAUT /
#   /
#
# DEPENDANCES
#	Perl >= 5.2
#
# REMARQUES
#       Le meme fichier 'cas' peut etre lance plusieurs fois simultanement.
#	Attention au fichier resultat, s'il a le meme nom le fichier existant
#       sera renomme avec la terminaison .old lors de la restitution du calcul
#
#
# AUTEUR : didier Rouge  : 29/08/94
#          DeltaCAD      : 1998
#
# MODIFICATIONS
#
#          date    : 06/09/1999
#          objet   : Changement des noms des fichiers "sortie" et "error"
#
#          date    : 06/09/1999
#          objet   : Lancement du logiciel ESTEL-2D
#
#          date    : 13/09/1999
#          objet   : Lancement du logiciel POSTEL-3D
#
#          date    : 16/09/1999
#          objet   : Suppression de la destruction du repertoire 'core'
#
#          date    : 27/09/1999
#          objet   : Repport des modifications d'Alain DUTOYA (SIMULOG)
#
#          date    : 04/10/1999
#          objet   : Edition de liens normale avec l'option "LK_OPT_NORMAL"
#                    Cette option permet d'ajouter l'option "-s" (strip) pour
#                    supprimer la table des symboles (version non-debug
#                    uniquement ) et ainsi gagner de la place en memoire
#                    et du temps calcul.
#
#          date    : 26/11/1999
#          objet   : Correction d'un bug au lancement sur Windows NT
#
#          date    : 29/02/2000
#          objet   : Mise en place des procedures de lancement des calculs sur CRAY
#                    L'option "-C" est reservee a cet effet.
#
#          date    : 27/03/2000
#          objet   : Mise en place des procedures de lancement des calculs sur FUJITSU
#                    L'option "-F" est reservee a cet effet.
#
#          date    : 12/04/2000
#          objet   : Lancement du logiciel ESTEL-3D
#
#          date    : 10/05/2000
#          objet   : Appel de la librairie CALCIUM
#
#          date    : 26/06/2000 A. Bas (Steria)
#          objet   : On ne cree plus le repertoire core sur le fuji car ca ne passe pas
#                  : On utilise les chemins complets des fichiers de paramÃÂtres (REPLANCE)
#
#          date    : Juillet 2000 - DeltaCAD
#          objet   : Introduction de la protection sous WindowsNT
#
#          date    : Septembre 2001 - DeltaCAD
#          objet   : AmÃ©lioration de la gestion des versions
#                    Suppression des branches "share" (executables damo)
#                    Exploitation du dictionnaire et fichier cas par modules Perl
#
#          date    : Mars 2003 - BAW Karlsruhe (jaj)
#          objet   : Nouvelles fonctionnalites :
#                     - localisation du repertoire temporaire
#                     - option de suppression ou non du rÃ©pertoire temporaire aprÃ¨s calcul
#                     - compilation/link prÃ©alable Ã  l'acquisition des fichiers et
#                       option de compilation/link seul "-cl"
#                     - amÃ©lioration de la dÃ©tection et gestion des erreurs
#                     - mot clef LK_LIB_SPECIAL pour des librairies spÃ©cifiques
#                     - fichier global "mpi_telemac.conf"
#
#          date    : Juin 2003 - DeltaCAD
#          objet   : Couplage des codes TELEMAC2D et SISYPHE
#
#          date    : Avril 2010 - BAW Karlsruhe (jaj) et SINETICS (C. Denis)
#          objet   : Prise en compte des sections de contrÃ´les en // (jaj)
#                  : Ajout des cibles pour Mumps (C. Denis)
#
#------------------------Systeme TELEMAC V5P3--------------------------

#-- Modules perls
use File::Basename;
use File::Copy;
use File::Path;
use Time::Local;

#---- INITIALISATIONS vitales
BEGIN
{
#Separateur Unix/NT
  if($ENV{"OS"} eq "Windows_NT") {$ps="\\";}   #Windows NT
                            else {$ps="/"; }   #Unix/Linux

#Localiser les modules dans le rÃ©pertoire 'bin'
  $project=`getproject`;  unshift (@INC, "$project$ps"."bin");

#Verification des modules
  if ( ! -e "$project$ps"."bin$ps"."tm_info.pm") 
       {die "FATAL : Module <tm_info> inaccessible.\n";}
  eval (require 'tm_info.pm'); 
  die "FATAL : Module <tm_info> incorrect ($@)"  if  ($@);
  if ( ! -e "$project$ps"."bin$ps"."tm_casdico.pm") 
       {die "FATAL : Module <tm_casdico> inaccessible.\n";}
  eval (require 'tm_casdico.pm'); 
  die "FATAL : Module <tm_casdico> incorrect ($@)"  if  ($@);
  
}#BEGIN

######################################################
#   /  Routines                /
######################################################

#-------------
sub CatchError
#-------------
#Definition : Capture de l'erreur ou non lors d'un lancement d'une commande
#Return     : code de retour ( true=executon ok, sinon False).
{
    my ($Submit,$FileStdout,$FileStderr,$Lret,$rc,$tmp);

    $Submit=$_[0];
    $FileStdout=$_[1];
    $FileStderr=$_[2];

    if ( $FileStdout ) {
      unlink $FileStdout;
      $Submit=join "", $Submit," 1> $FileStdout";
    }
    if ( $FileStderr ) {
      unlink $FileStderr;
      $Submit=join "", $Submit," 2> $FileStderr";
    }

    $rc=(0xffff & system($Submit));
    ($tmp=($rc!=0)) ? ($Lret="False") : ($Lret="True");

    if ( -s $FileStderr ) { $Lret="False"; }
    return $Lret;
}

#---------------
sub getSystelIni
#---------------
#Definition : Determine le systel.ini valide (soit par defaut dans
#            systel/config/systel.ini soit si elle existe
#            la variable d'environnement SYSTELCFG qui contient
#            le chemine complet du fichier)
#Return     : Le chemin complet du systel.ini valide.
{
    my ($systelIni,$ps);

#Separateur Unix/NT
if($ENV{"OS"} eq "Windows_NT") {$ps="\\";}   #Windows NT
	                      else {$ps="/";}    #Unix/Linux

    #On cherche la presence de la variable systelcfg, sinon on prend
    #le fichier de config par defaut (configuration de l'install).
    if($ENV{"SYSTELCFG"})
    {
	$systelIni=$ENV{"SYSTELCFG"}.$ps."systel.ini";
	$custSYSTEL=1;
    }
    else
    {
	#Recupere le premier argument envoye par le shell pour determiner
	#le chemin racine du projet et le nom du repertoire d'ou est lancer
	#install pour connaitre la plateforme.
	$systelIni=dirname($0);
	$systelIni=dirname($systelIni);
	$systelIni="$systelIni$ps"."config$ps"."systel.ini";
    }
    return $systelIni;
}

#------------
sub clearLine
#------------
#$_[0]      : La ligne a nettoyer
#Definition : Enleve le commentaire, les espaces et tab et le new line.
#Return     : La ligne nettoyee.
{
    $_[0]=~s/#.*//;         #Enleve un eventuel commentaire
    $_[0]=~s/;.*//;

    chomp($_[0]);           #Enleve le \n a la fin.
    return $_[0];
}

#-------------------
sub fillHashFromBloc
#-------------------
#$_[0]      : Le chemin complet du fichier a lire.
#$_[1]      : Le nom du bloc a considerer.
#$_[2]      : La "hashe table" ou seront stockees comme cles
#             En cas d'erreur, une entree de de type est retournÃ©e :
#              ("H_ERROR","Error msg")
#--------------------
#
{
    my ($val,$var,$line,$systelIni,$blocName,%hash);
    ($systelIni,$blocName,%hash)=@_;

    if ( ! open(FILE,"<$systelIni") )
      { $hash{"H_ERROR"}="Unable to open configuration file '$systelIni'.";
      	return %hash; }

    #Cherche le bloc $blocName.
    while($line=<FILE>)
    {
	$line=clearLine($line);
	if($line=~/\[$blocName\]/) { last; }
    }

    #On quitte si c'est la fin du fichier.
    if(eof(FILE))
      { $hash{"H_ERROR"}="bloc [$blocName] not found in configuration file '$systelIni'.";
      	return %hash;
      }

    #Maintenant on lit tous le bloc $blocName en stockant chaque variable
    #dans la "hashe table". On s'arrete au bloc suivant.
    while($line=<FILE>)
    {
	$line=clearLine($line);
	if($line=~/\[.+\]/) { last;}   #On sort si on trouve un autre bloc.

	if($line=~/.+=.*/)
	{
#Le bloc est bien de la forme "variable=valeur", donc on traite.

	    my $int2lin=$line;
            $int2lin=~s/#.*//;         #Enleve un eventuel commentaire.
            $int2lin=~s/;.*//;
            chomp($_[0]);              #Enleve le \n a la fin.

	    $_ = $int2lin;             #Decomposer (mot_clef, valeur)
	    ($var, $val) = /^\s*(.*?)\s*=\s*(.*).*$/;


	    $var=~s/ \t//g;            #Eliminer spaces + tabs

	    if($val=~/".+"/)
	    {
		$val=~s/[ \t]*"//;
		$val=~s/"[ \t]*//;
	    }
	    else
	    {
		$val=~s/[ \t"]*//g;
	    }
#Traduire les marqueurs : <TELEMAC_HOME>, <DIRLIB>
        $val =~ s/<TELEMAC_HOME>/$hash{"PROJECT"}/g;
# PLG INGEROP        $val =~ s/<DIRLIB>/$hash{"HOSTTYPE"}/g;
        $val =~ s/<DIRLIB>/$hash{"DIRLIB"}/g;

	    $hash{$var}=$val;
	}
    }
    if ( ! close(FILE) )
      { $hash{"H_ERROR"}="Unable to close configuration file '$systelIni'.";
      	return %hash; }
    return %hash;
}

#--------
sub usage {
#--------
    my ($EXECUTABLE, @Information);
    $EXECUTABLE = $GENERIQUE;
    # Erase evidence of previous errors (if any), so exit status is simple.
    $! = 0;

    $Information[0]="  ==> OPTIONS  :\n";
    $Information[1]=" --------------\n";

    if ( $LNG eq "1" )
    {
      $Information[2]="     -h|H     : Aide courte ou longue.\n";
      $Information[3]="     [ ]      : Mode interactif sans fichier listing du calcul,\n";
      $Information[4]="                  Resultats seulement a l'ecran.\n";
      $Information[5]="     -s       : Mode interactif avec fichier listing du calcul.\n";
      $Information[6]="     -D       : Mode de compilation : Debogueur.\n";
      $Information[7]="     -b       : Lancement en B-atch (depart immediat) on peut se deconnecter.\n";
      $Information[8]="     -n       : Lancement de N-uit (depart a 20H) on peut se deconnecter.\n";
      $Information[9]="     -d heure : Lancement avec depart D-iffere a 'xx:xx', on peut se deconnecter\n";
      $Information[10]="                l'heure est precise sous la forme de 2 chiffres pour l'heure,\n";
      $Information[11]="                du signe ':' et de 2 chiffres pour les minutes (d'autres\n";
      $Information[12]="                formes compatibles avec la commande 'at' peuvent etre utilisees,\n";
      $Information[13]="                pour cela se reporter au manuel de cette commande).\n\n";
      $Information[14]="   -noautopar : Pas de parallelisme automatique (partitionnement).\n";
      $Information[15]="     -t       : Do not delete working directory after a normal run.\n";
      $Information[16]="     -cl      : Compile and link an executable only, do not run.\n";
      $Information[17]="     cas      : Parameter file name.\n";
      $Information[18]="    /dir      : Full path to the /root of the working directory.\n";
    }
    else
    {
      $Information[2]="     -h|H     : Short or long help.\n";
      $Information[3]="     [ ]      : Interactive mode without computation listing file,\n";
      $Information[4]="                 Results only on the screen.\n";
      $Information[5]="     -s       : Interactive mode with computation listing file.\n";
      $Information[6]="     -D       : Compilation and execution using a Debugger.\n";
      $Information[7]="     -b       : Lauching in B-atch (immediate startup), we can logout.\n";
      $Information[8]="     -n       : Launching during the N-ight (startup at 20 H), we can logout.\n";
      $Information[9]="     -d heure : Lauching wit deferred startup at 'xx:xx', we can logout.\n";
      $Information[10]="                The time must be given with : 2 numbers for the heure variable,\n";
      $Information[11]="                the ':' sign and then 2 numbers for the minuts (another\n";
      $Information[12]="                compatible forms with the 'at' command can be used as well,\n";
      $Information[13]="                you can find more informations in the user manual).\n\n";
      $Information[14]="   -noautopar : Automatic parallel computation disabled (partitioning).\n";
      $Information[15]="     -t       : Do not delete working directory after a normal run.\n";
      $Information[16]="     -cl      : Compile and link an executable only, do not run.\n";
      $Information[17]="     cas      : Nom du fichier de donnees.\n";
      $Information[18]="    /dir      : Full path to the /root of the working directory.\n";
    }

    die <<EOF;
Usage: $EXECUTABLE [-D] [-s|b|n|d xx:xx] [cas] [/dir]
       $EXECUTABLE -h|H

@Information
EOF
}

#---------
sub ecrire {
#---------
    if(!$_[2]) { $_[2]=""; }

    printf "________________________________________________________\n";
    if ($LNG eq "1") {
	printf "$_[0]     $_[2]\n"; }
    else {
	printf "$_[1]     $_[2]\n"; }
    printf "________________________________________________________\n";
}


#---------
sub entete {                 # Routine originale : utilisation de banner et
#---------
    if ($LNG eq "1") {       #  de paste pour y ajouter le fichier escape
	printf "\n*** $_[0] ***\n\n"; }
    else {
	printf "\n*** $_[1] ***\n\n"; }
}

#------------
sub suitvalid {
#------------
    my ($SUITE, $VALIDATION, $PRECEDENT);
    $SUITE      = $_[0];
    $VALIDATION = $_[1];
    $PRECEDENT  = $_[2];
    if ($SUITE eq "T") {
	print "\n";
	ecrire ("IL EXISTE UNE SUITE DE CALCUL",
		"YOU START FROM A PREVIOUS COMPUTATION FILE");
	print "\n";
	ecrire ("NOM DU FICHIER DU CALCUL PRECEDENT = ",
		"PREVIOUS COMPUTATION FILE", $PRECEDENT);
	print "\n"; }

    if ($VALIDATION eq "T") {         # CORRECTION ($VALIDA)   $DCSA$ ÃÂ  verif
	print "\n";
	ecrire ("IL EXISTE UNE VALIDATION POUR VOTRE CALCUL", "VALIDATION IS ASKED");
	print "\n";
	ecrire ("LE NOM DU FICHIER DU CALCUL PRECEDENT = ", "PREVIOUS COMPUTATION FILE", $PRECEDENT) }
}

#-----------------
sub heure_plusun() {
#-----------------
    $min += 1;
    if ($min eq 60) {
	$min = 0;
	$hour += 1;
	if ($hour eq 24) { $hour = 0; }
    }
}

#-------------
sub ecrire_variable_fichier
#-------------
#Definition : Ecriture d'un variable et de sa valeur 
#             dans un fichier Perl
{
  $nbArg = $#_;
  if ($nbArg!=2-1 && $nbArg!=3-1)
    { die "## ERREUR : appel incorrect a <ecrire_variable_fichier>";}

#Initialisations/recup arguments
  $nomVariable = @_[1];
  $fhandle = @_[0];
  if ($nbArg==2-1)
      { $valeurVar=$$nomVariable; }    #la valeur est dans la variable
  elsif ($nbArg==3-1)
      { $valeurVar=@_[2];  }           #la valeur est en dernier argument

#Sous Windows, doubler les "\" 
  if($ENV{"OS"} eq "Windows_NT") { $valeurVar =~ s%\\%\\\\%g;}

#Ecriture de la ligne Perl
  print $fhandle "\$$nomVariable = \"$valeurVar\";\n";
#Fin
  return 0;
} #End ecrire_variable_fichier

#-------------
sub desc_fichiers_acqui_resti
#-------------
#--- Ecriture des descriptions des fichiers a acquerir/restituer
#        ("LISTE DES FICHIERS"/"LIST OF FILES" les ÃÂ©numÃÂ¨re)
#  - FH        : handle du fichier
#  - %motsdic  : mots clefs Ã  considÃ©rer (reference)
#  - i0        : numero du premier fichier
#  - DICO      : DICTIONNAIRE
#  - CAS       : CAS
{
#Initialisations/recup arguments
  my $FH = @_[0];
  my $refMots= @_[1];
  my $refi0 = @_[2];
  my $mDICO = @_[3];
  my $mCAS  = @_[4];

 if($ENV{"OS"} eq "Windows_NT") 
   { $mDICO =~ s%\\%\\\\%g;          # doubler les \ sous Windows
     $mCAS  =~ s%\\%\\\\%g;
   }
#
#--- Ecriture des descriptions des fichiers a acquerir/restituer
#        ("LISTE DES FICHIERS"/"LIST OF FILES" les Ã©numÃ¨re)
#
my @vals   = tm_casdico::recup_valeur_mot($refMots, "LISTE DES FICHIERS");   #fr
my @valsgb = tm_casdico::recup_valeur_mot($refMots, "LIST OF FILES");        #gb
if (scalar(@vals) == 0)
  {  ecrire("ERREUR : Mot clef du dictionnaire 'LISTE DES FICHIERS' non defini",
            "ERROR : undefined dictionary parameter 'LISTE DES FICHIERS'");
     return 1;}
if (scalar(@valsgb) == 0)
  {  ecrire("ERREUR : Mot clef du dictionnaire 'LIST OF FILES' non defini",
            "ERROR : undefined dictionary parameter 'LIST OF FILES'");
     return 1;}

for($i=0;$i<@vals;$i++)
   {
    @valsd = tm_casdico::recup_valeur_mot($refMots,$vals[$i],"SUBMIT"); 
    my $ii=$$refi0;

    #@valsd[0]=~s/INUTILE;//;                    #eliminer INUTILE
    my @ltmp=split(";",@valsd[0]); shift @ltmp; @valsd[0]=join ";",@ltmp;
    @valsd[0]="F$ii".";".@valsd[0];
    @vals2 = tm_casdico::recup_valeur_mot($refMots,$vals[$i]); 
    @vals2[0]=~s/^\s*$//;
        
#Cas d'un fichier de type FORTRAN : le repÃÂ©rer pour compilation avec son nom
#d'acquisition (valable pour plusieurs FORTRANs)
    if (@valsd[0]=~/;FORTRAN/) 
      {my @items=split (/;/,@valsd[0]);
       my  $fn=join ";",@vals2[0],@items[1];
       printf $FH "push(\@FORTRANS,\"$fn\");\n";
       next;
      }
#Cas d'un fichier de type FORTINC : le repÃÂ©rer pour l'acquÃÂ©rir en mÃÂªme
#temps que les Fortrans
    if (@valsd[0]=~/;FORTINC/) 
      {my @items=split (/;/,@valsd[0]);
       my  $fn=join ";",@vals2[0],@items[1];
       printf $FH "push(\@FORTINC,\"$fn\");\n";
       next;
      }

#Cas d'un fichier de type CONLIM : le repÃÂ©rer pour PARTEL
    if (@valsd[0]=~/;CONLIM/) 
     {printf $FH "push(\@conlimDSC,\"@valsd[0]\");\n"; }

#Cas d'un fichier de type SECTION : le repÃÂ©rer pour PARTEL #### !jaj
    if (@valsd[0]=~/;SECTION/) 
     {printf $FH "push(\@sectionDSC,\"@valsd[0]\");\n"; }
    
#Cas d'un fichier de type SELAFIN-GEOM : le repÃÂ©rer pour GRETEL
    if (@valsd[0]=~/;SELAFIN-GEOM/) 
     {printf $FH "push(\@selgeomDSC,\"@valsd[0]\");\n"; }
    
#Cas d'un fichier de type CAS : surcharger la valeur par celle eventuellement
#donnee sur la ligne de commande
    if (@valsd[0]=~/CAS/)  
        { if (@vals2[0] == "") {@vals2[0]=$mCAS;}    }

#Cas d'un fichier de type DICO : valeur dans $mDICO
    if (@valsd[0]=~/;DICO/)   { @vals2[0]=$mDICO; }

#Ecriture des donnÃÂ©es du fichier pour son acquisition/restitution par 'runcode'   
    printf $FH "\@FLNG1=(\@FLNG1,\"@vals[$i]\"".");\n"; 
    printf $FH "\@FLNG2=(\@FLNG2,\"@valsgb[$i]\"".");\n"; 
    printf $FH "\@FDESC=(\@FDESC,\"@valsd[0]\"".");\n"; 
    printf $FH "\$F$ii"."=\"@vals2[0]\"".";\n"; 

    $$refi0++;

   }#for
   
#Fin
  return 0;
} #End desc_fichiers_acqui_resti

#-------------
sub get_code_params
#  - HASH
#  - nom du code
#  - chemin   
#  - racine   
#  - lng      
#  - versdef  
#-------------
#Definition : Retourne les caractÃÂ©ristiques d'un code 
{
#Initialisations/recup arguments
  my $refH  = @_[0];
  my $NOMCOD= @_[1];
  my $refCH = @_[2];
  my $refRAC= @_[3];
  my $refLNG= @_[4];
  my $refVER= @_[5];

#Traitement
if ($NOMCOD eq "telemac3d") {    $$refCH    = "TELEMAC3D$ps"."TEL3D_";
                                 $$refRAC   = "TELEMAC3D";
                                 $$refLNG   = $$refH{"LNGTEL3D"};
                                 $$refVER   = $$refH{"VERSTEL3D"};          }
if ($NOMCOD eq "telemac2d") {    $$refCH    = "TELEMAC2D$ps"."TEL2D_";
                                 $$refRAC   = "TELEMAC2D";
                                 $$refLNG   = $$refH{"LNGTEL"};
                                 $$refVER   = $$refH{"VERSTEL"};            }
if ($NOMCOD eq "stbtel")    {    $$refCH    = "STBTEL$ps"."STBTEL_";
                                 $$refRAC   = "STBTEL";
                                 $$refLNG   = $$refH{"LNGSTB"};
                                 $$refVER   = $$refH{"VERSSTB"};            }
if ($NOMCOD eq "postel3d")  {    $$refCH    = "POSTEL3D$ps"."POSTEL3D_";
                                 $$refRAC   = "POSTEL3D";
                                 $$refLNG   = $$refH{"LNGPOSTE"};
                                 $$refVER   = $$refH{"VERSPOSTE"};          }
if ($NOMCOD eq "artemis")   {    $$refCH    = "ARTEMIS$ps"."ARTE_";
                                 $$refRAC   = "ARTEMIS";
                                 $$refLNG   = $$refH{"LNGARTE"};
                                 $$refVER   = $$refH{"VERSARTE"};           }
if ($NOMCOD eq "sisyphe")   {    $$refCH    = "SISYPHE$ps"."SISYPHE_";
                                 $$refRAC   = "SISYPHE";
                                 $$refLNG   = $$refH{"LNGSISY"};
                                 $$refVER   = $$refH{"VERSSISY"};           }
if ($NOMCOD eq "cowadis")   {    $$refCH    = "COWADIS$ps"."COWA_";
                                 $$refRAC   = "COWADIS";
                                 $$refLNG   = $$refH{"LNGCOWA"};
                                 $$refVER   = $$refH{"VERSCOWA"};           }
if ($NOMCOD eq "estel2d")   {    $$refCH    = "ESTEL2D$ps"."ESTEL2D_";
                                 $$refRAC   = "ESTEL2D";
                                 $$refLNG   = $$refH{"LNGESTEL2"};
                                 $$refVER   = $$refH{"VERSESTEL2"};          }
if ($NOMCOD eq "estel3d")   {    $$refCH    = "ESTEL3D$ps"."ESTEL3D_";
                                 $$refRAC   = "ESTEL3D";
                                 $$refLNG   = $$refH{"LNGESTEL3"};
                                 $$refVER   = $$refH{"VERSESTEL3"};          }
if ($NOMCOD eq "tomawac")   {    $$refCH    = "TOMAWAC$ps"."TOMA_";
                                 $$refRAC   = "TOMAWAC";
                                 $$refLNG   = $$refH{"LNGTOMA"};
                                 $$refVER   = $$refH{"VERSTOMA"};           }
if ($NOMCOD eq "spartacus2d")   {    $$refCH    = "SPARTACUS2D$ps"."SPARTACUS2D_";
                                 $$refRAC   = "SPARTACUS2D";
                                 $$refLNG   = $$refH{"LNGSPA"};
                                 $$refVER   = $$refH{"VERSSPA"};           }
$$refCH=~tr/A-Z/a-z/;
#Fin
  return 0;
} #End get_code_params


#-------------
sub get_mots
#-------------
#--- Acquisition des mots clÃÂ© ÃÂ  partir des fichiers
#    cas et dico
#E:
#  - NOMCOD  : nom du code
#  - PROJECT : chemin du repertoire racine
#  - CHEMIN  : chemin vers l'arborescence du code
#  - VERSDEF : version par defaut du code
#  - CAS     : fichier cas
#S:
#  - DICO    : fichier dictionaire
#  - motEtude: hash des mots de l'etude
#  - VERS    : version du code a considerer
#
{
#Initialisations/recup arguments
  my $mNOMCOD = @_[0];
  my $mPROJ   = @_[1];
  my $mCHEM   = @_[2];
  my $mVDEF   = @_[3];
  my $mCAS    = @_[4];
  my $refDICO = @_[5];
  my $refVER  = @_[6];
  
  my %motsMods, %Mots;
#
#-- Lecture du dictionnaire du code dans sa version par defaut
#   et du fichier CAS
$$refDICO="$mPROJ$ps$mCHEM$mVDEF$ps"."lib$ps$mNOMCOD$mVDEF".".dico";
%Mots = tm_casdico::lecture_fichier_dictionnaire($$refDICO);
if (keys(%Mots) == 0 )
  {  ecrire("ERREUR : le dictionnaire n'existe pas :",
            "ERROR : unable to find the dictionary :",
            "\n   '$$refDICO'"); 
     printf "Returning exit status 1 \n";
     jajbanner("...stopping.");
     exit 1;}
#print "SAcas CAS=$mCAS   ".__FILE__."  ".__LINE__." \n";  
%motsMods = tm_casdico::lecture_fichier_cas($mCAS, \%Mots); 
if (keys(%motsMods)==0)
  {  ecrire("ERREUR : Erreur dans la lecture du fichier cas :",
            "ERROR : Error during read of steering file :",
            "\n   '$mCAS'"); 
     printf "Returning exit status 1 \n";
     jajbanner("...stopping.");
     exit 1;}
tm_casdico::modifier_mots(\%Mots, \%motsMods); 

#-- Relecture du dictionnaire du code dans la version specifique
#   indiquee par le mot clef NUMERO DE VERSION

my @vals = tm_casdico::recup_valeur_mot(\%Mots, "NUMERO DE VERSION"); 
if (scalar(@vals) == 0)
  {  ecrire("ERREUR : Mot clef du dictionnaire 'NUMERO DE VERSION' non defini",
            "ERROR : undefined dictionary parameter 'NUMERO DE VERSION'");
     printf "Returning exit status 1 \n";
     jajbanner("...stopping.");
     exit 1;}
@vals[0]=~tr/A-Z/a-z/;
@VERS = split(/,/, @vals[0]);
$$refVER=$mVDEF;

if($mVDEF ne $VERS[0])
  {
  entete("Exploitation de la version specifique $VERS[0]",
         "Using specific version $VERS[0]", " ");
  $$refDICO="$mPROJ$ps$mCHEM$VERS[0]$ps"."lib$ps$mNOMCOD$VERS[0]".".dico";

  %Mots = tm_casdico::lecture_fichier_dictionnaire($$refDICO); 
  if (keys(%Mots)==0)
     {  ecrire("ERREUR : le dictionnaire n'existe pas :",
               "ERROR : unable to find the dictionary :",
               "\n   '$$refDICO'"); 
        printf "Returning exit status 1 \n";
        jajbanner("...stopping.");
        exit 1;}
  %motsMods = tm_casdico::lecture_fichier_cas($mCAS, \%Mots); 
  if (keys(%motsMods)==0)
     {  ecrire("ERREUR : Erreur dans la lecture du fichier cas :",
               "ERROR : Error during read of steering file :",
               "\n   '$CAS'"); 
        printf "Returning exit status 1 \n";
        jajbanner("...stopping.");
        exit 1;}
  tm_casdico::modifier_mots(\%Mots, \%motsMods); 

  $$refVER=$VERS[0];

  } #if VDEF
   
#Fin
  return %Mots;
} #End get_mots



#jaj
#-------------
sub jajbanner
#-------------
{
  #return; # ...ehm, we love banners indicating which versions WE use, don't WE?
  printf "\n";
  printf "=========================================================\n";
  printf " Telemac System 5.6 to 6.2 - Perl scripts version 6.2    \n";
  printf "=========================================================\n";
  printf "$_[0]\n";
  printf "\n";
}

######################################################
#   /  PROGRAMME PRINCIPAL               /
######################################################

#
#-- initialisation de variables
#
$blksize=$rdev=$gid=$isdst=$yday=$ficmode=$dev=$atime=$ctime=$mtime=$nlink=$uid=$ino=$blocks="";
#$soumission="";
$mon=$year=$wday=$sec=$mday="";
$sortie       = "nul";
$lancement    = "interactif";
$GENERIQUE    = basename($ARGV[0],"");
#PLG
$GENERIQUE1   = $GENERIQUE;
shift(@ARGV);
#$WORKING      = $$;
#UHM take PBS-JOBID if available
$WORKING      = $ENV{"JOBID"};
if ( $WORKING eq "" )                      #UHM take PARENT PID
       { $WORKING      = $$; }
$DEBUG        = "pasdebug";
$LNG          = "2";
$SUITE        = "";
$VALIDATION   = "";
$PRECEDENT    = "";
$PROJECT      = $ENV{"PROJECT"};
$MODE         = "";
$FFLAGS       = " ";
$MPICONF       = "mpi_telemac.conf";

@FORTRANS=(); @FORTINC=();
@conlimDSC=(); @selgeomDSC=(); @sectionDSC=();
$i0fic=1; #numero du premier fichier acqui/resti

$CRAY         = "";
$FUJI         = "";
$CALCIUM      = "";

$TM_UNCPATH   = $ENV{"TM_UNCPATH"};
$AUTOPAR      = "1";   #par defaut, mode parallelisme automatique actif (partel)
$DELWORKDIR   = "1";   #default - remove the work directory after a run
$COMPILE_LINK = "0";   #default: compile, link AND run

######################################################
#   / Traitement initiaux /
#       - lecture du fichier de configuration
#       - traitement des options de lancement
######################################################
#
# Lecture du bon fichier systel.ini.
#-----------------------------------
$custSYSTEL=0;
my $systelIni=getSystelIni();
%hash=();
%hash=fillHashFromBloc($systelIni,"GENERAL",%hash);
if ( defined $hprot{"H_ERROR"} )                      #Erreur
       { print "ERROR : $hprot{\"H_ERROR\"}\n"; exit; }
%hash=fillHashFromBloc($systelIni,"PERL",%hash);
if ( defined $hprot{"H_ERROR"} )                      #Erreur
       { print "ERROR : $hprot{\"H_ERROR\"}\n"; exit; }
%hash=fillHashFromBloc($systelIni,$hash{"HOSTTYPE"},%hash);
if ( defined $hprot{"H_ERROR"} )                      #Erreur
       { print "ERROR : $hprot{\"H_ERROR\"}\n"; exit; }

# Traitement des options de la ligne de commande.
#-----------------------------------------------

$compile=$hash{"FC_NAM"}.$hash{"FC_OPT_COMPIL"}.$hash{"FC_OPT_OTHERS"};
$linkage=$hash{"LK_NAM"}.$hash{"LK_OPT_NORMAL"}.$hash{"LK_OPT_OTHERS"}.$hash{"LK_OPT_OUTNAME"}."a.exe";

$f=0;
$counter=0;
for($i=0;$i<@ARGV;$i++)
{
    $opt=$ARGV[$i];
    $f=0;
    if($opt eq "-h" || $opt eq "-H") { $f++;
	usage();
    }
    if($opt eq "-s") { $f++; $counter++;
	$lancement="interactif";
	$sortie="listing";
    }
    if($opt eq "-D") { $f++;  $counter++;
        $compile=$hash{"FC_NAM"}.$hash{"FC_OPT_DEBUG"}.$hash{"FC_OPT_OTHERS"};
        $linkage=$hash{"LK_NAM"}.$hash{"LK_OPT_DEBUG"}.$hash{"LK_OPT_OTHERS"}.$hash{"LK_OPT_OUTNAME"}."a.exe";
	$MODE="d";
	$DEBUG="debug";
	$sortie="nul";
	$lancement="interactif";
    }
    if($opt eq "-g") { $f++;  $counter++;
        $compile=$hash{"FC_NAM"}.$hash{"FC_OPT_PROFILE"}.$hash{"FC_OPT_OTHERS"};
        $linkage=$hash{"LK_NAM"}.$hash{"LK_OPT_PROFILE"}.$hash{"LK_OPT_OTHERS"}.$hash{"LK_OPT_OUTNAME"}."a.exe";
	$MODE="p";
	$DEBUG="";
	$sortie="nul";
	$lancement="interactif";
    }
    if($opt eq "-b") { $f++;  $counter++;
	$lancement="batch";
	$sortie="listing";
    }
    if($opt eq "-n") { $f++;  $counter++;
	$lancement="nuit";
	$sortie="listing";
    }
    if($opt eq "-d") { $f++;  $counter++;
	$lancement="diff";
	$sortie="listing";
    }
    if($opt=~/[0-9]+\:[0-9]+/) { $f++;  $counter++;
	$heure=$opt;
    }

    if($opt eq "-C") { $f++; $counter++;
	$lancement="batch";
	$sortie="listing";
	$CRAY="oui";
    }

    if($opt eq "-F") { $f++; $counter++;
        $lancement="batch";
	$sortie="listing";
	$FUJI="oui";
    }

    if($opt eq "-ca") { $f++;  $counter++;
	$CALCIUM="oui";
    }

    if($opt eq "-noautopar") { $f++;  $counter++;
	$AUTOPAR="0";
    }

    if($opt eq "-t") { $f++;  $counter++;
	$DELWORKDIR="0";
    }

#jaj according to Pallas consultants

    if($opt eq "-cl") { $f++; $counter++;
        $COMPILE_LINK="1";
    }

    if($opt eq "-uncpath") { $f++;  $counter++;
	$UNCPATH="oui";
	if ($TM_UNCPATH eq "")
	  {	ecrire("Erreur : la variable TM_UNCPATH n'est pas definie.",
	  	       "Error : TM_UNCPATH environment variable not defined.");
	  	exit;
	  }
    }

#jaj we use now two names, cas-file, work-directory

    if($f==0&&$i<@ARGV-2) {
	printf "\nUnknown switch in arguments : $opt\n";
	usage();
    }
}

#Determine le nom du fichier des parametres.
#jaj--v
$PARA = "parameter_file_not_set";
$WORKPATH = "workpath_not_set";

$noarg=@ARGV-$counter;

#jaj perl problem to be solved. 
# if only one parameter is used it is assumed to be the parameter file name, 
# by default cas, if someone uses teh work dir name as the only parameter
# nasty errors can occure - how to distinguish them? 
# (easy to buld in: requirement that there must be two arguments)

if ($noarg==2) {
  if($f==0) { $PARA = $ARGV[@ARGV-2]; $WORKPATH = $ARGV[@ARGV-1];}
} elsif ($noarg==1) {
  if($f==0) { $PARA = $ARGV[@ARGV-1]; $WORKPATH = "";}
} elsif ($noarg==0) {
  if($f==0) { $PARA = ""; $WORKPATH = "";}
} else {
 printf "\nIncorrect number of parameters: $noarg\n";
         usage();
}

# this is strange
if ($PARA eq "") { $PARA = "cas"; }  # Valeur par defaut 

$REPLANCE  = dirname($PARA);
$PARA      = basename($PARA);
$REP       = "$PARA$WORKING"."_tmp";
# PLG INGEROP $dirlib    =$hash{"HOSTTYPE"};
$dirlib    =$hash{"DIRLIB"};
$PROJECT   =$hash{"PROJECT"};
$PERLPATH  =$hash{"PERLPATH"};
$libExt    =$hash{"LIB_OPT_LIBEXT"};
$objExt    =$hash{"FC_OPT_OBJEXT"};
$cmdInc    =$hash{"FC_OPT_INCLUDE"};
$runprofile=$hash{"RUN_PROFILE"};
$rundebug  =$hash{"RUN_DEBUG"};
$runmpi    =$hash{"RUN_MPI"};
$libsmpi   =$hash{"LIBS_MPI"};
$libmumpsseq  =$hash{"LIBMUMPSSEQ"};
$libmumpspar  =$hash{"LIBMUMPSPAR"};
$lkmpi     =$hash{"LK_MPI"};

#Parametrage en fonction du syteme (UNIX ou NT)
if($ENV{"OS"} eq "Windows_NT")
  {
#---- Cas du systeme Windows NT
    $ps="\\";
    ($BASE, $TMP) = split(/\n/, `CD`);      # On enleve le NewLine a la fin
# Test-26/06/2000
    if ($REPLANCE eq ".")  { $REPLANCE=$BASE;}
    $entetePerl="\@rem = '\n\@$PERLPATH\\perl %0\n\@goto endofperl\n\@rem ';\n";
    $entetePerl2="\@rem = '\n\@$PERLPATH\\perl delete_$PARA$WORKING.bat\n\@goto endofperl\n\@rem ';\n";
#RBR
    $fileToDelete=".bat";
    $listJobs="at";
    $atFormat="at %s $REPLANCE$ps"."$REP$ps"."$GENERIQUE.bat";
    $cmd_tim="";
    $exe_ext=".exe";
# RBR 09-07-2004 : $REP changed V5P3 with the V5P4!
#                  So for delleted the TMP, we muste have this line.
    $REP1="$PARA$WORKING"."_tmp";
  }
else 
  {
#---- Cas du systeme UNIX
    $ps="/";
    ($BASE, $TMP) = split(/\n/, `pwd`);      # On enleve le NewLine a la fin
# Test-26/06/2000
    if ($REPLANCE eq ".") {$REPLANCE=$BASE;}
    $entetePerl="#!$PERLPATH/perl\n";
    $entetePerl2="#!$PERLPATH/perl\n";
#RBR
    $fileToDelete="";
    $listJobs="at -l";
    $atFormat="echo $REPLANCE$ps"."$REP$ps"."$GENERIQUE.bat | at %s";
    $cmd_tim="time";
    $exe_ext="";
#recuperation du username pour tuer job actif : $LOGIN, $LOGUSER
    $user_name=$ENV{"LOGIN"};
    if ( $username eq "" )  {$user_name=$ENV{"LOGNAME"}; }
# RBR 09-07-2004 : $REP changed V5P3 with the V5P4!
#                  So for delleted the TMP, we muste have this line.
#RBR    $REP1="$PARA$WORKING"."_tmp";
  }

#jaj set a default for the work directory
#    or set the name given in the parameter line

if ($WORKPATH eq "") { $WORKPATH = $REPLANCE; }     # Valeur par defaut 
$REP = "$WORKPATH$ps$REP";

#jaj
jajbanner("starting...");

printf "HOSTTYPE         : $dirlib \n";
printf "PROJECT          : $PROJECT \n";
printf "BASE DIRECTORY   : $BASE \n";
printf "LAUNCH DIRECTORY : $REPLANCE \n";
printf "WORK DIRECTORY   : $REP \n";
printf "PARAMETER FILE   : $PARA \n\n";

#####################################################################
# INITIALISATION DES VARIABLES A PARTIR DU NOM DU GENERIQUE /
# et controles
#####################################################################

get_code_params (\%hash,$GENERIQUE,\$CHEMIN,\$RACINE,\$LNG,\$VERSDEF);

$RACINEBAN=$RACINE; $RACINE=~tr/A-Z/a-z/;
$VERSBAN=$VERSDEF;

if ($LNG ne "1" && $LNG ne "2")
    { die "## ERREUR : variable de la langue pour $GENERIQUE non initialisee\n".
                      "language variable not initialized\n"; }

# Affichage du nom du fichier de configuration
if ($custSYSTEL == 1)
  { entete("Fichier de configuration SPECIFIQUE : \n    $systelIni",
           "Using CUSTOM configuration file : \n    $systelIni", " ");
  }
else
  { entete("Fichier de configuration par defaut : \n    $systelIni",
           "Using default configuration file : \n    $systelIni", " ");
  } 

#Verification de l'existence du fichier des parametres
$st = stat("$REPLANCE"."$ps"."$PARA");
if ($st == 0)
  {
    ecrire("Le fichier des parametres n'existe pas",
           "The steering file does not exist");
    usage();
  }

######################################################
#   /  DEBUT DES TACHES DE GESTION DE FICHIERS
######################################################

printf "\n";

$CAS ="$REPLANCE"."$ps"."$PARA";
%motsEtude=get_mots ($GENERIQUE,$PROJECT,$CHEMIN,$VERSDEF,$CAS,\$DICO,\$VERS);

#-- Creation du repertoire de calcul

#jaj
#orig 
#chdir($REPLANCE)  or die "## Error : chdir $REPLANCE impossible : $!\n";
#mkdir($REP, 0755) or die "## Error : mkdir $REP impossible : $!\n";
#chdir($REP)       or die "## Error : chdir $REP impossible : $!\n";

mkdir($REP, 0755) or die "## Error : mkdir $REP impossible : $!\n";
chdir($REP)       or die "## Error : chdir $REP impossible : $!\n";

#-- Creation du fichier PARAL pour le parallelisme
#jaj  (strange, these files are created/copied even when NCSIZE eq "0") 
#
@vals = tm_casdico::recup_valeur_mot(\%motsEtude, "PROCESSEURS PARALLELES"); 
$NCSIZE=@vals[0]; 
if ($NCSIZE eq "")  {$NCSIZE=0;}          #Le mot clef n'existe pas

#jaj orig:
#$Directory_tmp=join "",$REPLANCE,$ps,$REP,$ps;
$Directory_tmp =join "",$REP,$ps;
$filename=join "", $Directory_tmp,"PARAL";
$NCAR = length ($Directory_tmp);

open (F, ">$filename") or die "## Error : Open the $filename file is impossible\n";
printf F "$NCSIZE\n";

#- Traitement de l'option UNCPATH dans le fichier PARALL
#jaj orig
#$Directory_tmp=join "",$REPLANCE,$ps,$REP,$ps;
$Directory_tmp=join "",$REP,$ps;
if ( ($UNCPATH eq "oui") && ($TM_UNCPATH ne "") )
  { $Directory_tmp =~s/^[A-Za-z]:/$TM_UNCPATH/; }

$NCAR = length ($Directory_tmp);
printf F "$NCAR\n";
printf F "$Directory_tmp\n";
close (F)  or die "## Error : Close the $filename file is impossible\n";

#
# Fichier 'mpi_telemac.conf' si parallelisme
#
#jaj
# if there's a specific file in the launch directory, use it.
# if not, take the default one for the given Telemac installation

#if ($NCSIZE > 0) { copy("..$ps$MPICONF", ".$ps"); }

if ($NCSIZE > 0) { 
  $LOCMPICONF = "$REPLANCE"."$ps$MPICONF"; 
  $GLOMPICONF = "$PROJECT"."$ps"."install"."$ps"."$dirlib"."$ps"."$MPICONF";
  #printf "$LOCMPICONF \n";
  #printf "$GLOMPICONF \n";
  if (-e "$LOCMPICONF" ) { 
    copy("$LOCMPICONF", ".$ps");
    entete("Fichier de configuration MPI SPECIFIQUE : \n    $LOCMPICONF",
           "Using CUSTOM MPI configuration file : \n    $LOCMPICONF", " ");
  } elsif ( -e "$GLOMPICONF" ) { 
    copy("$GLOMPICONF", ".$ps"); 
    entete("Fichier de configuration MPI GLOBALE : \n    $GLOMPICONF",
           "Using GLOBAL MPI configuration file : \n    $GLOMPICONF", " ");
  } else {
    ecrire("ERREUR : Fichier de configuration MPI n'existe pas :",
           "ERROR : MPI configuration file does not exist :",
            "\n   '$LOCMPICONF' \n   '$GLOMPICONF'"); 
    printf "Returning exit status 1 \n";
    jajbanner("...stopping.");
    exit 1;
  }
}

# S'agit-il d'un calcul sur CRAY ou FUJITSU
# Si oui, executer le code du module "cray_fuji"
if ($CRAY eq "oui" || $FUJI eq "oui") { require "cray_fuji.pl"; }

#jaj additional lib defined in systel.ini
$LIBSPECIAL=$hash{"LK_LIB_SPECIAL"};

######################################################
#   / CONSTRUCTION DU FICHIER $GENERIQUE.bat
######################################################
 
#---- Creation du fichier

$filename="$GENERIQUE".".bat";  # .BAT POUR WINDOWS
open (F, ">$filename") or die "## Error : Open the $filename file is impossible\n";

#---- Entete Perl
print F $entetePerl;

#---- Ecriture des variables

ecrire_variable_fichier(F,"objExt");    
ecrire_variable_fichier(F,"libExt");    
ecrire_variable_fichier(F,"cmdInc");    
ecrire_variable_fichier(F,"linkage");   
ecrire_variable_fichier(F,"runprofile");
ecrire_variable_fichier(F,"rundebug");  
ecrire_variable_fichier(F,"runmpi");    
ecrire_variable_fichier(F,"libsmpi");   
ecrire_variable_fichier(F,"lkmpi");     
ecrire_variable_fichier(F,"libmumpsseq");  
ecrire_variable_fichier(F,"libmumpspar");   
ecrire_variable_fichier(F,"ps");        
#SAinutile ecrire_variable_fichier(F,"BASE");      
#SA ecrire_variable_fichier(F,"DICO");      
ecrire_variable_fichier(F,"NCSIZE");    
ecrire_variable_fichier(F,"PARA");      
ecrire_variable_fichier(F,"PROJECT");   
ecrire_variable_fichier(F,"REP");       
ecrire_variable_fichier(F,"REPLANCE");  
ecrire_variable_fichier(F,"WORKING");   
ecrire_variable_fichier(F,"MODE");      
ecrire_variable_fichier(F,"FFLAGS");    
ecrire_variable_fichier(F,"DEBUG");     
ecrire_variable_fichier(F,"LNG");       
ecrire_variable_fichier(F,"lancement"); 
ecrire_variable_fichier(F,"sortie");    
ecrire_variable_fichier(F,"compile");   
ecrire_variable_fichier(F,"MPICONF");   
ecrire_variable_fichier(F,"dirlib");    
ecrire_variable_fichier(F,"cmd_tim");   
ecrire_variable_fichier(F,"exe_ext");
ecrire_variable_fichier(F,"AUTOPAR");
#jaj
ecrire_variable_fichier(F,"LIBSPECIAL");
ecrire_variable_fichier(F,"COMPILE_LINK");
printf F "# si lancement sur CRAY \n";
ecrire_variable_fichier(F,"CRAY");      
ecrire_variable_fichier(F,"FUJI");      

if ($CRAY eq "oui" || $FUJI eq "oui")
  {
   ecrire_variable_fichier (F,"NOM_CIBLE");      
   ecrire_variable_fichier (F,"USR_CIBLE");      
# Modification du chemin complet 27/06/2000 A. Bas (Steria)
#jaj there could be an error in the next line (?)
   printf F "\$com_CIBLE=\" ( rcp ${user_name}\\\@${STHP}:${REPLANCE}/${REP}/${script} ${script}_${WORKING} && qsub ${script}_${WORKING} ) \";\n";
   ecrire_variable_fichier (F,"CALCIUM");      
}


#--- FICHIER FORTRAN
#sa2 @vals = tm_casdico::recup_valeur_mot(\%motsEtude, "FICHIER FORTRAN"); 
#sa2 ecrire_variable_fichier(F,"FORTRAN", @vals[0]);



#sa1 
#sa1 #--- FICHIER DES PARAMETRES
#sa1 @vals = tm_casdico::recup_valeur_mot(\%motsEtude, "FICHIER DES PARAMETRES");
#sa1 if ( @vals[0] =~ /\s*/ ) { @vals[0]="";}       # accepter des "blancs" dans le dico 
#sa1 ecrire_variable_fichier(F,"CAS", @vals[0]);
#
#--- NUMERO DE VERSION
@vals = tm_casdico::recup_valeur_mot(\%motsEtude, "NUMERO DE VERSION"); 
if (scalar(@vals) == 0)
  {  ecrire("ERREUR : Mot clef du dictionnaire 'NUMERO DE VERSION' non defini",
            "ERROR : undefined dictionary parameter 'NUMERO DE VERSION'");
     exit 1;}
$vs=@vals[0];          #traitement specifique pour accepter la forme
if (@vals > 1)         #         V5P2;V5P2;V5P1;V5P1
  { for ($i=1;$i<@vals;$i++)  { $vs=$vs.",@vals[i]";}
  }
$vs=~tr/A-Z/a-z/;
ecrire_variable_fichier(F,"VERSION", $vs);

#
#--- Description des fichiers a acquerir/restituer
#
$ier=desc_fichiers_acqui_resti (F, \%motsEtude, \$i0fic, $DICO, $CAS);
if ($ier != 0)
  {  ecrire("ERREUR : Probleme dans l'analyse des fichiers",
            "ERROR : problem during file analysis");
     exit 1;}

#
#--- Cas du couplage TELEMAC2D/SISYPHE :
#      - lecture du DICO de SISYPHE + fichier CAS
#      - description des fichiers de SISYPHE
#
if ($GENERIQUE eq "telemac2d" || $GENERIQUE eq "telemac3d")
  { 
#print "COUPLAGE T2D OU T3D /SISYPHE ! \n";
#PLG
     @vals = tm_casdico::recup_valeur_mot(\%motsEtude,"FICHIER DES PARAMETRES DE SISYPHE"); 
     if ((scalar(@vals) != 0) && (@vals[0] ne "") )
       { 
       if ($GENERIQUE eq "telemac2d") {$GENERIQUE1 = "TEL2DSIS"};
       if ($GENERIQUE eq "telemac3d") {$GENERIQUE1 = "TEL3DSIS"};
#      printf "COUPLAGE T2D OU T3D /SISYPHE $GENERIQUE1! \n";
         $sCAS ="$REPLANCE"."$ps"."@vals[0]";
         get_code_params (\%hash,"sisyphe",\$sCHEMIN,
                          \$sRACINE,\$sLNG,\$sVERSDEF);

         %smotsEtude=get_mots ("sisyphe",$PROJECT,$sCHEMIN,$sVERSDEF,
                               $sCAS,\$sDICO,\$sVERS);
# marquer le changement de code
         printf F "\@FDESC=(\@FDESC,\"NEWCODE;.;.;.;.;.\");\n"; 
         $ier=desc_fichiers_acqui_resti (F, \%smotsEtude, \$i0fic, $sDICO, $sCAS);
         if ($ier != 0)
           {  ecrire("ERREUR : Probleme dans l'analyse des fichiers SISYPHE",
                     "ERROR : problem during SISYPHE files analysis");
              exit 1;
           }
       }
#print "COUPLAGE T2D /TOMAWAC ! \n";
#PLG
     @vals = tm_casdico::recup_valeur_mot(\%motsEtude,"FICHIER DES PARAMETRES DE TOMAWAC"); 
     if ((scalar(@vals) != 0) && (@vals[0] ne "") )
       { 
       if ($GENERIQUE eq "telemac2d") {$GENERIQUE1 = "TEL2DTOM"};
       if ($GENERIQUE eq "telemac3d") {$GENERIQUE1 = "TEL3DTOM"};
#       printf "COUPLAGE T2D OU T3D /TOMAWAC $GENERIQUE1! \n";
         $sCAS ="$REPLANCE"."$ps"."@vals[0]";
         get_code_params (\%hash,"tomawac",\$sCHEMIN,
                          \$sRACINE,\$sLNG,\$sVERSDEF);
  
         %smotsEtude=get_mots ("tomawac",$PROJECT,$sCHEMIN,$sVERSDEF,
                               $sCAS,\$sDICO,\$sVERS);
# marquer le changement de code
         printf F "\@FDESC=(\@FDESC,\"NEWCODE;.;.;.;.;.\");\n"; 
         $ier=desc_fichiers_acqui_resti (F, \%smotsEtude, \$i0fic, $sDICO, $sCAS);
         if ($ier != 0)
           {  ecrire("ERREUR : Probleme dans l'analyse des fichiers TOMAWAC",
                     "ERROR : problem during TOMAWAC files analysis");
              exit 1;
           }
       }
  }


#
#--- Cas du couplage ESTEL3D/TELEMAC2D :
#      - lecture du DICO de TELEMAC2D + fichier CAS
#      - description des fichiers de TELEMAC2D
#
if ($GENERIQUE eq "estel3d")
  { 
     @vals = tm_casdico::recup_valeur_mot(\%motsEtude, "FICHIER DES PARAMETRES DE TELEMAC2D"); 
     if ((scalar(@vals) != 0) && (@vals[0] ne "") )
       { #print "COUPLAGE ESTEL3D/TELEMAC2D ! \n";

         $sCAS ="$REPLANCE"."$ps"."@vals[0]";
         get_code_params (\%hash,"telemac2d",\$sCHEMIN,
                          \$sRACINE,\$sLNG,\$sVERSDEF);

         %smotsEtude=get_mots ("telemac2d",$PROJECT,$sCHEMIN,$sVERSDEF,
                               $sCAS,\$sDICO,\$sVERS);
# marquer le changement de code
         printf F "\@FDESC=(\@FDESC,\"NEWCODE;.;.;.;.;.\");\n"; 
         $ier=desc_fichiers_acqui_resti (F, \%smotsEtude, \$i0fic, $sDICO, $sCAS);
         if ($ier != 0)
           {  ecrire("ERREUR : Probleme dans l'analyse des fichiers TELEMAC2D",
                     "ERROR : problem during TELEMAC2D files analysis");
              exit 1;
           }
       }
  }

#--- DESCRIPTION DES LIBRAIRIES
@vals = tm_casdico::recup_valeur_mot(\%motsEtude, "DESCRIPTION DES LIBRAIRIES"); 
if (scalar(@vals) == 0)
  {  ecrire("ERREUR : Mot clef du dictionnaire 'DESCRIPTION DES LIBRAIRIES' non defini",
            "ERROR : undefined dictionary parameter 'DESCRIPTION OF LIBRARIES'");
     exit 1;}
for($i=0;$i<@vals;$i++)
{
    printf F "\@CODLIBS=(\@CODLIBS,\"@vals[$i]\"".");\n"; 
}
#--- EXECUTABLE PAR DEFAUT
@vals = tm_casdico::recup_valeur_mot(\%motsEtude, "EXECUTABLE PAR DEFAUT"); 
if (scalar(@vals) == 0)
  {  ecrire("ERREUR : Mot clef du dictionnaire 'EXECUTABLE PAR DEFAUT' non defini",
            "ERROR : undefined dictionary parameter 'DEFAULT EXECUTABLE'");
     exit 1;}
ecrire_variable_fichier(F,"EXEDEF", @vals[0]);

#jaj
#--- EXECUTABLE PARALLELE PAR DEFAUT
@vals = tm_casdico::recup_valeur_mot(\%motsEtude, "EXECUTABLE PARALLELE PAR DEFAUT");
if (scalar(@vals) == 0)
  {  ecrire("ERREUR : Mot clef du dictionnaire 'EXECUTABLE PARALLELE PAR DEFAUT' non defini",
            "ERROR : undefined dictionary parameter 'DEFAULT PARALLEL EXECUTABLE'");
     exit 1;}
ecrire_variable_fichier(F,"EXEDEFPARA", @vals[0]);

#----------------------------------
#------ ajout du fichier runcode.pl
#----------------------------------
$filename3=join "", $PROJECT,$ps,"bin",$ps,"runcode.pl";
open (F3, "<$filename3") or die "## Error : Open the $filename3 file is impossible\n";
while($LIGNE = <F3>) 
  {                                             #Lecture et ajout des lignes du
    printf F "$LIGNE";                          #fichier "runcode.pl"
  }
close (F3)  or die "## Error : Close the $filename3 file is impossible\n";

#----------------------------------------
#------ ajout de la fin du fichier (perl)
#----------------------------------------
printf F "__END__\n";                           # Entete de fin Perl
printf F ":endofperl\n";
close (F) or die "## Error : Close the $filename file is impossible\n";
#jaj chmod(0755,"$filename");
chmod(0744,"$filename");

#-----------------------------------------------------
#------Construction du fichier PERL de nettoyage.
#      Ce fichier sera execute a la fin de ce lanceur.
#-----------------------------------------------------
#jaj---v

# two files, one for kill & delete, 
# second for checkpoint and stop gracefully

#$dfilename=join "", "..",$ps,"delete_",$PARA,$WORKING,".bat";
#$sfilename=join "", "..",$ps,"signal_",$PARA,$WORKING,".bat";
#RBR $dfilename=join "", $REPLANCE,$ps,"delete_",$PARA,$WORKING,".bat";
#RBR $sfilename=join "", $REPLANCE,$ps,"signal_",$PARA,$WORKING,".bat";
$dfilename=join "", $REPLANCE,$ps,"delete_",$PARA,$WORKING,$fileToDelete;
$sfilename=join "", $REPLANCE,$ps,"signal_",$PARA,$WORKING,$fileToDelete;

#--------------------------------------------------
# write a file to kill the program
#--------------------------------------------------

open (F,">$dfilename") or die "## Error : Open the $dfilename file is impossible\n";
printf F $entetePerl2;

#Sous Unix, tuer un job actif avant :
$REPLANCE_DB=$REPLANCE;$REPLANCE_DB=~s/\\/\\\\/g;
printf F "if(\$ENV{\"OS\"} ne \"Windows_NT\") \n";
printf F "  { \n";

# for a SGI machine with MPI the KILL signal must be sent to mpirun's PID
# the following SIGTERM and SIGKILL to all processes... 

if ($NCSIZE > 0) { 
  printf F "    \$pid=`ps -fu $user_name | grep 'out$WORKING' | grep -v grep | grep mpirun`; \n";
  } else {
  printf F "    \$pid=`ps -fu $user_name | grep 'out$WORKING' | grep -v grep | grep -v time`; \n";
}
printf F "    (\$tmp1, \$pid, \$tmp) = split (\" \", \$pid); \n";
printf F "    if ( \$pid ne \"\" ) \n";
printf F "      { \$ret=`kill -2 \$pid`; \n";
printf F "        print \"job  \$pid  interrupted\\n\"; \n";
printf F "        print \"waiting 3 seconds\\n\"; \n";
printf F "        system(sleep 3); \n";                  
printf F "      } \n";

if ($NCSIZE > 0) { 
  printf F "    \$pid=`ps -fu $user_name | grep 'out$WORKING' | grep -v grep`; \n";
  } else {
  printf F "    \$pid=`ps -fu $user_name | grep 'out$WORKING' | grep -v grep | grep -v time`; \n";
}
printf F "    (\$tmp1, \$pid, \$tmp) = split (\" \", \$pid); \n";
printf F "    if ( \$pid ne \"\" ) \n";
printf F "      { \$ret=`kill -15 \$pid`; \n";
printf F "        print \"job  \$pid  terminated\\n\"; \n";
printf F "      } \n";

if ($NCSIZE > 0) { 
  printf F "    \$pid=`ps -fu $user_name | grep 'out$WORKING' | grep -v grep`; \n";
  } else {
  printf F "    \$pid=`ps -fu $user_name | grep 'out$WORKING' | grep -v grep | grep -v time`; \n";
}
printf F "    (\$tmp1, \$pid, \$tmp) = split (\" \", \$pid); \n";
printf F "    if ( \$pid ne \"\" ) \n";
printf F "      { \$ret=`kill -9 \$pid`; \n";
printf F "        print \"job  \$pid  killed\\n\"; \n";
printf F "      } \n";
# RBR 09-07/2004 Modif pour gÃÂ©stion UNIX et WIN
#RBR printf F "  } \n";

printf F "if ( chdir(\"$REP\") )\n";
#RBRprintf F "if ( chdir(\"$REPLANCE_DB$ps$ps$REP1\") )\n";
printf F "{\n";
printf F "opendir(CURDIR,\".\");\n";
printf F "foreach \$var (readdir(CURDIR))\n";
printf F "{\n";
printf F "    chmod(0755,\$var);\n";
printf F "    unlink \$var;\n";
printf F "}\n";
printf F "      }\n";

printf F "chdir(\"$REPLANCE\");\n";
printf F "rmdir(\"$REP\");\n";

printf F "    unlink \"$dfilename\";\n";
printf F "    unlink \"$sfilename\";\n";

printf F "  }\n";
printf F "if(\$ENV{\"OS\"} eq \"Windows_NT\")\n";
printf F "  {\n";

printf F "    if ( chdir(\"$REPLANCE_DB$ps$ps$REP1\") )\n";
printf F "      {\n";
printf F "        opendir(CURDIR,\".\");\n";
printf F "        foreach \$var (readdir(CURDIR))\n";
printf F "          {\n";
printf F "            unlink \$var;\n";
printf F "          }\n";
printf F "        closedir(CURDIR);\n";
printf F "      }\n";

printf F "    chdir(\"..\");\n";
printf F "    rmdir(\"$REP1\") or die \"Error : '$REP1' not cleaned\";\n";

printf F "  }\n";

#printf F "rmdir(\"core\");\n";
#RBRprintf F "chdir(\"$REPLANCE\");\n";
#RBRprintf F "chdir(\"$REPLANCE_DB\") or die \"Error : no chdir\";\n";
#RBRprintf F "rmdir(\"$REP1\");\n";
#RBRprintf F "rmdir(\"$REP1\") or die \"Error : '$REP1' not cleaned\";\n";
#Message de destruction du repertoire de calcul
#RBRif ($LNG == 2 )
#RBR   { printf F "print \"Working directory '$REP1'  cleaned.\\n\"; \n"; }
#RBRelse
#RBR   { printf F "print \"Repertoire de calcul '$REP1' detruit.\\n\"; \n"; }
#RBRprintf F "}\n";

#destruction acceptee ici sous Unix
#RBRprintf F "if(\$ENV{\"OS\"} ne \"Windows_NT\") \n";
#RBRprintf F "  { unlink \"$dfilename\"; \n";
#RBRprintf F "    unlink \"$sfilename\"; } \n";

printf F "__END__\n";
printf F ":endofperl\n";

close (F) or die "## Error : Close the $dfilename file is impossible\n";
#jaj chmod(0755,"$dfilename");
chmod(0744,"$dfilename");

#--------------------------------------------------
# write a file to signal the program
#--------------------------------------------------
# this is a Unix-only feature
# presently (February 2003) it works in parallel for sgi only (?)

if($ENV{"OS"} ne "Windows_NT") {
open (F,">$sfilename") or die "## Error : Open the $sfilename file is impossible\n";
printf F $entetePerl2;

# a hack for IRIX 6.5.19f with MPT 1.4 - mpirun does not propagete SIGUSR1
# to all child processes. Therefore a workaround with sending this signal
# to all processes separately (but mpirun). 

$send_to_mpi = 0;

if ( $send_to_mpi != 0 ) {

# for a SGI machine with MPI the USR1 signal must be sent to mpirun's PID
if ($NCSIZE > 0) { 
  printf F "    \$pid=`ps -fu $user_name | grep 'out$WORKING' | grep -v grep | grep -v time | grep mpirun`; \n";
  } else {
  printf F "    \$pid=`ps -fu $user_name | grep 'out$WORKING' | grep -v grep | grep -v time`; \n";
}
printf F "(\$tmp1, \$pid, \$tmp) = split (\" \", \$pid); \n";
printf F "if ( \$pid ne \"\" ) \n";
printf F "  { \$ret=`kill -USR1 \$pid`; \n";
printf F "    print \"Telemac System job with PID  \$pid  signalled using USR1\\n\"; \n";
printf F "  } \n";
###printf F "unlink \"$sfilename\"; \n";
###printf F "unlink \"$dfilename\"; \n";
printf F "__END__\n";
printf F ":endofperl\n";
close (F) or die "## Error : Close the $sfilename file is impossible\n";
#jaj chmod(0755,"$sfilename");
chmod(0744,"$sfilename");

} else {   

# for a machine where USR1 must be sent to all processes
# do not send to mpirun (?)
if ($NCSIZE > 0) { 
  printf F "    \$processes=`ps -fu $user_name | grep 'out$WORKING' | grep -v grep | grep -v time | grep -v mpirun`; \n";
  } else {
  printf F "    \$processes=`ps -fu $user_name | grep 'out$WORKING' | grep -v grep | grep -v time`; \n";
}
printf F "\$line = \"\"; \n";
printf F "while (\$processes =~ /(.*)/gc) { \n";
printf F "    \$pid = \$1; \n";
printf F "    (\$tmp1, \$pid, \$tmp) = split (\" \", \$pid); \n";
printf F "    \$line = \"\$line\".\" \$pid\"; \n";
printf F "  } \n";

printf F "if ( \$line ne \"\" ) \n";
printf F "  { \$ret=`kill -USR1 \$line`; \n";
printf F "    print \"Telemac System job with PIDS: \\n\"; \n";  
printf F "    print \"\$line \\n\"; \n";  
printf F "    print \"signalled using USR1\\n\"; \n";
printf F "  } \n";
###printf F "unlink \"$sfilename\"; \n";
###printf F "unlink \"$dfilename\"; \n";
printf F "__END__\n";
printf F ":endofperl\n";
close (F) or die "## Error : Close the $sfilename file is impossible\n";
#jaj chmod(0755,"$sfilename");
chmod(0744,"$sfilename");

}}
#jaj----^ 


#   /--------------------------------/
#   / TEST SUR LE MODE DE LANCEMENT  /
#   /--------------------------------/

#   / MODE INTERACTIF - LANCEMENT IMMEDIAT  /

if ($lancement eq "interactif" ) {
    entete("$RACINEBAN sur STATION", "$RACINEBAN ON STATION");
    entete("Lancement en interactif", "Interactive mode");
    entete("VERSION $VERSBAN", "RELEASE $VERSBAN");
    ecrire("Fichier des parametres   : ", "Steering file   : ", $PARA);

    if ($GENERIQUE eq "telemac2d" || $GENERIQUE eq "telemac3d") {
	suitvalid("$SUITE", "$VALIDATION", "$PRECEDENT"); }
    if ($sortie eq "listing") {
	ecrire("Sortie listing  : ","Output file        : ", "$PARA$WORKING"."_sortie.txt"); }
    sleep(1);
    print "\n";

    ecrire("Starting execution: $GENERIQUE.bat", 
           "Starting execution: $GENERIQUE.bat");
#jaj orig
# $command="$REPLANCE"."$ps"."$REP"."$ps"."$GENERIQUE".".bat";
 $command="$REP"."$ps"."$GENERIQUE".".bat";
#jaj
  $errorexe = "0";
  $Lret=CatchError($command,"","");
  if ( $Lret eq "False" ) {
    print "## Error : System command failed for $command :$?\n";
    $errorexe = "1";
    ####exit 1;    
  }
#jaj
    ecrire("Execution terminee: $GENERIQUE.bat", 
           "Execution finished: $GENERIQUE.bat");

#jaj modified - it exists if compilation / linking errors occured
# Traitement d'erreur :

$errcomplink = "0";
if ( -e "err$WORKING.err" )
  {
    $errcomplink = "1";
    printf "Compilation/linking/file errors detected.\n";
    printf "Please see messages in: stdout above, stderr,\n";
    $errfiletxt = "$REPLANCE$ps"."$PARA$WORKING"."_error.log"; 
    printf "       and/or file $errfiletxt\n";  
  } else {
    printf "No compilation/linking/file errors detected.\n";
  }

if ( $errorexe eq "1" ) 
  {
    printf "Execution errors detected.\n";
    printf "Please see messages in stdout above or study stderr output.\n";
  } else {
    printf "No execution errors detected.\n";
  }

#Nettoyage du repertoire de travail (SI pas mode PROFILE)
#jaj orig
#  chdir("..") or die "## Error : chdir \"..\" impossible : $!\n";
  chdir("$REPLANCE") or die "## Error : chdir $REPLANCE impossible : $!\n";

  if ( ($errcomplink ne "1") && ($errorexe ne "1") &&
       ($MODE ne "p") && ($DELWORKDIR ne "0") ) {
########moulinec
    $command=join "", "delete_",$PARA,$WORKING,$fileToDelete;
# Don't work on Windows. On linux, add a "." to your PATH
####    $command=join "","./", "delete_",$PARA,$WORKING,$fileToDelete;
########moulinec
    $Lret=CatchError($command,"","");
    if ( $Lret eq "False" ) {
      print "## Error : System command failed for $command :$?\n";
      exit 1; } 
  } else {
    $command=join "", "delete_",$PARA,$WORKING,$fileToDelete;
    printf "\nWorking directory: $REP\n";
    printf "can be manually deleted with: $REPLANCE$ps$command\n\n";
  }

#ici pour NT car "delete_" ne peut se detruire lui  meme
    if($ENV{"OS"} eq "Windows_NT") { unlink("delete_$PARA$WORKING.bat"); }

  if ( ($errcomplink ne "1") && ($errorexe ne "1") ) 
    {
       printf "Returning exit status 0 \n";
       jajbanner("...stopping.");
       exit 0;
    } else {
       printf "Returning exit status 1 \n";
       jajbanner("...stopping.");
       exit 1;
    }
}

#jaj I do not touch this part of the code

#   /-----------------------------------------------------------------------------/
#   / MODES BATCH - NUIT - DIFF                                                   /
#   / LANCEMENT IMMEDIAT EN RENDANT LA CONNEXION                                  /
#   / CREATION DANS LE REPERTOIRE DE LANCEMENT D'UN FICHIER delete_$PARA$WORKING  /
#   / POUR EFFACER UN CALCUL EN COURS                                             /
#   /-----------------------------------------------------------------------------/

entete("$RACINEBAN sur STATION", "$RACINEBAN ON STATION");
entete("Lancement en batch", "operating in batch mode");
entete("VERSION $VERSBAN", "RELEASE $VERSBAN");
ecrire("Fichier des parametres   : ", "Steering file   : ", $PARA);
if ($GENERIQUE eq "telemac2d" || $GENERIQUE eq "telemac3d") {
    suitvalid("$SUITE", "$VALIDATION", "$PRECEDENT"); }
sleep(2);
print "\n";

ecrire("Lancement de l'executable", "Running in batch mode");
ecrire("Sortie listing  : ", "Output file        : ", "$PARA$WORKING"."_sortie.txt");
ecrire("Sortie erreur   : ", "Error file : ", "$PARA$WORKING"."_error.txt");

# heure de lancement "now" si batch
if ($lancement eq "batch") {
    ($sec, $min, $hour, $mday, $mon, $year, $wday, $yday, $isdst) = localtime(time);
    heure_plusun();
}
else {
    if ($lancement eq "nuit") {
	$hour = "20";
	$min  = "00";
    }
    else {
	($hour, $tmp) = split("\:", $heure);
	($tmp, $min)  = split("\:", $heure);
    }
}

#----------
# lancement
#----------

print "\n\n";

$txt=sprintf ("$atFormat","$hour:$min");

$Lret=CatchError($txt,"","");
if ( $Lret eq "False" ) {
  print "## Error : System command failed for $txt :$?\n";
  exit 1;
}

$Lret=CatchError($listJobs,"","");
if ( $Lret eq "False" ) {
  print "## Error : System command failed for $listJobs :$?\n";
  exit 1;
}

#jaj
#chdir("..") or "## Error : chdir \"..\" impossible : $!\n";
chdir("$REPLANCE") or "## Error : chdir \"$REPLANCE\" impossible : $!\n";
printf "\n";

#----eof---
