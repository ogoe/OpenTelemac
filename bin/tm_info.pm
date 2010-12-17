package info;

#                     -                   Package impression des informations #
###############################################################################
# DeltaCAD                                                              2001  #
###############################################################################
# Ce package pour Perl 5 permet d'afficher sur STDOUT les informations en 
# fonction du niveau d'information requis
#
# niveau 0      : interdit
#        1      : minimum
#        2 a  9 : bavard    (-v ->  2)
#       10 a 99 : debug     (-d -> 10, -debug -> 99)
#
# Ces actions sont:
# init()         : initialisation...
# info()         : affiche l'info sur STDOUT,
# erreur()       : affiche un message d'erreur,
# erreurFatale() : affiche un message d'erreur et quitte le programme,
# debug()        : affiche un message de debug ,
# afficherAoH()  : affiche un tableau de type "Array of Hashes",
# ti()           : affiche un point sans retour a la ligne.
#
###############################################################################
#
# Packages generiques utilises:
#
use strict;
#

#  if ( -f $fileName ) { print $fileName." oui\n"; return(1); }
#  else                { print $fileName." non\n"; return(0); }

my $info=0;
# limite inf. affichage des messages de debug
my $db0=10;
# limite inf. affichage du nom de fichier et numero de ligne dans les messages
my $dbg=13;
# position du caractere sur la ligne en cours...
my $posCar=0;
# AutoFlush des output!!!
$|=1;

sub init
#------------------------------------------------------------------------------
# initialisation de l'affichage des informations
#------------------------------------------------------------------------------
# Argument : niveau = niveau d'affichage des messages pour l'execution en
#                     cours,
#                  <=  0      : interdit,
#                   =  1      : minimum,
#                   =  2 a  9 : bavard,
#                   = 10 a 99 : debug.
#
# Retour   : info   = niveau enregistre pour l'execution en cours.
#------------------------------------------------------------------------------
{ my ($niveau)=@_;
  # print "niveau debug = ".$niveau."\n";
  if ( $niveau <= 0 )
  { erreur("appel de ".__PACKAGE__."::init avec niveau = \"$niveau\" <= 0 !!",
           __FILE__,__LINE__);
    return($info);
  }
  if ( $info )
  { if ( $info != $niveau )
    { if ( $niveau == 2 && $info < $niveau )
      { info($info, "niveau d'information -> \"verbose\""); }
      else
      { info($info, "modification du niveau d'information $info -> $niveau"); }
      $info = $niveau;
      return ($info);
    }
  }
  # Valeur du niveau d'information pour le programme
  $info = $niveau;
  # rediriger STDERR vers STDOUT !
  open (SVGERR, ">&STDERR");
  open (STDERR, ">&STDOUT");
  # AutoFlush des output!!!
  $|=1;
  # message d'info (seult si info >= $dbg=13)
  info($dbg,"info initialise ($info)",__FILE__,__LINE__);
  return ($info);
}
#------------------------------------------------------------------------------

sub info
#------------------------------------------------------------------------------
# affichage du message si le niveau d'info est sufisant
#------------------------------------------------------------------------------
# Arguments : niveau = niveau d'impression du message, le message n'est affiche
#                      que si ce niveau est superieur au niveau de l'execution
#                      en cours.
#             texte  = texte du message.
#            [file]  = nom du fichier                     (pas necessaire).
#            [line]  = numero de la ligne dans le fichier (pas necessaire).
#            [indice]= indice de boucle par exemple       (pas necessaire).
#------------------------------------------------------------------------------
{ my ($niveau,$texte,$file,$line,$indice)=@_;
  # afficher le message si le niveau du message est inferieur au 
  # niveau  d'affichage pour l'execution en cours.
  if ( $niveau <= $info )
  { # terminer la ligne en cours le cas echeant
    if ( $posCar )
    { print "\n"; $posCar = 0; }
    # affichage du fichier et du numero de ligne si ces infos sont donnees et
    # si le niveau d'affichage pour l'execution en cours est superieur a $dbg.
    if ( $info >= $dbg )
    { # info debug...
      if ( defined $file )
      { print "debug - fichier ".$file;
        if ( defined $line )
        { print ", ligne ".$line;
          if ( defined $indice )
          {  print ", indice ".$indice; }
        }
        print " :\n";
      }
    }
    print $texte."\n";
  }
  return;
}
#------------------------------------------------------------------------------

sub ti
#------------------------------------------------------------------------------
# affichage d'un point sans carriage return (pour faire patienter)
#------------------------------------------------------------------------------
# Arguments : niveau = niveau d'impression du message, le message n'est affiche
#                      que si ce niveau est superieur au niveau de l'execution
#                      en cours.
#------------------------------------------------------------------------------
{ my ($niveau)=@_;
  if ( $niveau <= $info )
  { print "."; $posCar++; }
  return;
}
#------------------------------------------------------------------------------

sub erreur
#------------------------------------------------------------------------------
# affichage d'un message d'erreur
#------------------------------------------------------------------------------
# Arguments : texte  = texte du message.
#            [file]  = nom du fichier                     (pas necessaire).
#            [line]  = numero de la ligne dans le fichier (pas necessaire).
#------------------------------------------------------------------------------
{ my ($texte,$file,$line,$indice)=@_;
  $texte = "ERREUR : ".$texte;
  info($info,$texte,$file,$line,$indice);
  return;
}
#------------------------------------------------------------------------------

sub erreurFatale
#------------------------------------------------------------------------------
# affichage d'un message d'erreur puis abandon de l'execution...
#------------------------------------------------------------------------------
{ my ($texte,$file,$line,$indice)=@_;
  # affichage du message (+ fichier et ligne si $info >= $dbg)
  info($info,"ERREUR : ".$texte,$file,$line,$indice);
  # affichage du message d'arret, sans fichier et ligne.
  info(1,"Execution de \"".envi::get('NOM_APPLI')."\" abandonnee!!");
  exit 0;
}
#------------------------------------------------------------------------------

sub debug
#------------------------------------------------------------------------------
# affichage d'un message de debug
# (affichage simple si $info >= $db0, avec fichier et ligne si $info >= $dbg)
#------------------------------------------------------------------------------
{ my ($texte,$file,$line,$indice)=@_;
  if ( $info >= $db0 )
  { info($info,$texte,$file,$line,$indice); }
  return;
}
#------------------------------------------------------------------------------

sub afficherAoH
#------------------------------------------------------------------------------
# affichage d'un tableau de type "Array of Hashes"
#
# type 'SIMPLE' = Les "Hashes" ont tous les memes clefs que l'on peut extraire 
#                 du premier item...
#     'ORDONNE' = le premier item de l'"Array" est la liste ordonnee des
#                 clefs des items suivants qui sont des "Hashes".
#      <autres> = pas encore developpe
#------------------------------------------------------------------------------
{ my ($niveau,$AoHRef,$type,$file,$line,$indice)=@_;
  if ( $info >= $niveau )
  { # acces a l'"Array"
    my @AoH = @$AoHRef;
    my $nbItems = scalar(@AoH);
    my $nbLgns  = scalar(@AoH);
    my $numItem = 0;

    # extraire la liste des clefs du premier item
    my @clefs = ();
    if ( $type eq 'SIMPLE' )
    { # les clefs sont les clefs du premier item qui est un "Hash"
      my $itemRef = $AoH[0];
      my %item = %$itemRef;
      @clefs = keys(%item);
      # ce premier item est quand meme valable, $numItem & $nbLgns pas modifies
    }
    elsif ( $type eq 'ORDONNE' )
    { # les clefs sont les valeurs du premier item qui est un "Array"
      my $clefsRef = $AoH[0];
      @clefs = @$clefsRef;
      # ce premier item doit ensuite etre ignore !
      $numItem++;
      $nbLgns--;
    }

    # variables utiles
    my $largCol = 17;
    my $longLgn = 0;

    # nombre de colonnes
    my $nbColonnes = scalar(@clefs);
    info::info($niveau+2, "Affichage du tableau $type de $nbLgns lignes de ".
                          "$nbColonnes colonnes:");
    my $ligneEncadr = "";
    for (my $col=0; $col<$nbColonnes; $col++)
    { $ligneEncadr=$ligneEncadr."----------------|" }
    info::info($niveau,$ligneEncadr);

    # affichage des clefs (cadrees a gauche)
    my $ligneClefs = "";
    $longLgn = 0;
    foreach my $clef (@clefs)
    { $ligneClefs=$ligneClefs.$clef;
      $longLgn+=$largCol;
      while ( length($ligneClefs) < $longLgn )
      { $ligneClefs=$ligneClefs." "; }
    }
    info::info($niveau,$ligneClefs);
    info::info($niveau,$ligneEncadr);

    # boucle sur les items
    while ( $numItem < $nbItems )
    { # extraire l'item
      my $itemRef = $AoH[$numItem];
      my %item    = %$itemRef;
      # affichage des valeurs (cadrees a droite)
      my $ligneVals = "";
      $longLgn = -1;
      foreach my $clef (@clefs)
      { my $val = $item{$clef};
        $longLgn+=$largCol;
        my $longAvant = $longLgn - length($val) - 1;
        while ( length($ligneVals) < $longAvant )
        { $ligneVals=$ligneVals." "; }
        $ligneVals=$ligneVals." "; 
        $ligneVals=$ligneVals.$val; 
      }
      info::info($niveau,$ligneVals);
      $numItem++;
    }
    info::info($niveau,$ligneEncadr);
  }

  return;
}

1;
