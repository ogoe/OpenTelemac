
package tm_casdico;

##########################Systeme TELEMAC######################################
#
# Package perl "tm_litdico".
#
# Fonctions de lecture d'un dictionnaire au format DAMOCLES
# et de lecture d'un fichier cas.
#
#
#                                                      DeltaCAD - aout 2001
###############################################################################
#
#
use tm_info;
use strict;

###############################################################################
# CONFIGURATION

# ... pour le fichier dictionnaire
# liste des parametres pour chacun des mots clefs qui doivent etre recuperes.
my @LST_MOTS_CLEFS_A_RECUPERER=qw(NOM
                                  NOM1
                                  TYPE
                                  TAILLE
                                  SUBMIT
                                  DEFAUT
                                  DEFAUT1);

# chaine de caractere definissant un nouveau mot clef dans le dictionnaire.
my @LST_DEF_MOTS_CLEFS=qw(NOM NOM1);

my %LST_PARAM_STOCKES = qw(TYPE 0
                           TAILLE 1
                           SUBMIT 2
                           DEFAUT 3
                           DEFAUT1 3
                           reference 4
                           langue 5
                           flag_valdef 6);
#my $NB_PARAM_STOCKES = %LST_PARAM_STOCKES-1;

# mots clefs specifiques a une seule langue
my @LST_MOTSCLEFS_LANG0 = qw(NOM  DEFAUT); #langue par defaut
my @LST_MOTSCLEFS_LANG1 = qw(NOM1 DEFAUT1);# langue numero 1

#
#
###############################################################################



###############################################################################
#PUBLIC
#Role  : Renvoyer, pour un mot donne appartenant au dictionnaire des mots clefs
#        la valeur d'une de ces caracteristiques. Celle ci est renvoyee dans
#        un tableau car elle peut contenir plusieurs valeurs.
#Entree: 4
#       - reference vers une structure dictionnaire
#       - nom du mot du dictionnaire dont on cherche une des caracteristiques
#       - nom du parametre (une des caracteristiques de ce mot)
#       - scalaire "0" ou "1" :
#           si le mot est defini dans les 2 langues avec le meme nom (exemple
#           <VALIDATION>) alors si
#           0 la valeur en francais est prise en compte
#           1 la valeur en anglais est prise en compte sinon
#Retour: un tableau de valeurs
#        retourne rien si le mot n'existe pas
###############################################################################
sub recup_valeur_mot
{
  # Nb d'arguments passes
  my $nbArg = $#_;
  
  if ($nbArg<1 || $nbArg>3)
  { # si moins de 2 arg ou plus de 4
    info::info(20,"Le nombre d'arguments passes a la fonction est incorrect <$nbArg> (".__FILE__." ".__LINE__.")");
    return;
  }
  my $refDictionnaire = @_[0];
  my $motDico         = @_[1];
  my ($parametre,$langue);
  if ($nbArg==1)
  {
    $parametre  = "DEFAUT";
    $langue     = "0";
  }
  elsif ($nbArg==3 && @_[2] eq "")
  { # donc 4 parametres dont le 3eme qui est vide
    $parametre= "DEFAUT";
    $langue   = @_[3];
  }
  else
  { #au moins 3 parametres donc
    if (@_[2] eq ""){
      $parametre = "DEFAUT" }
    else{
      $parametre = @_[2]; }
            
    if ($nbArg==3){
      $langue   = @_[3]; }
    else{
      $langue   = "0"; }
  }
  #info::info(20,"<@_>, <$motDico> <$parametre> <$langue>.\n");
  
  
  #$motDicoComplet = construire_nom_de_stockage($motDico,"0");
  #unless ( exists($refDictionnaire->{$motDicoComplet}) )
  #{
    #$motDicoComplet = construire_nom_de_stockage($motDico,"1");
    #$otherLang="0";
    #unless ( exists($refDictionnaire->{$motDicoComplet}) )
    #{
      #info::erreur("Mot <$motDico> inexistant dans le dictionnaire (".__FILE__." ".__LINE__.")");
      #return;
    #}
  #}

  # determiner le nom de stockage dans la table en fonction de la langue souhaite
  # tout d'abord
  my $motDicoComplet = construire_nom_de_stockage($motDico,
                                                  $langue);
  info::info(20,"Mot clef <$motDico> => nom de stockage dans la table <$motDicoComplet> (".__FILE__." ".__LINE__.")");
  unless ( exists($refDictionnaire->{$motDicoComplet}) )
  {
    #si on arrive la c'est que ce mot n'a pas cette synthaxe dans cette langue pe etre
    info::info(20,"Mot clef <$motDico> non trouve dans la langue <$langue> (".__FILE__." ".__LINE__.")");

    my $langue2=();
    if ($langue == "0") { $langue2="1"; }
    else              { $langue2="0"; }
    # on cherche si il n'existe pas dans l'autre langue
    $motDicoComplet = construire_nom_de_stockage($motDico,
                                                 $langue2);
    info::info(20,"Mot clef <$motDico> => nom de stockage dans la table <$motDicoComplet> (".__FILE__." ".__LINE__.")");
    unless ( exists($refDictionnaire->{$motDicoComplet}) )
    {
      info::info(10,"Mot clef <$motDico> non trouve dans la langue <$langue2> (".__FILE__." ".__LINE__.")");
      return;
    }
  }

  # demander l'endroit ou est stocke ce parametre dans le tableau
  my $rang= donne_rang_stockage($parametre);

  # recuperer la reference du tableau contenant les parametres pour ce mot du dico
  my $ref_liste_valeurs = $$refDictionnaire{$motDicoComplet};
  unless ($ref_liste_valeurs != () )
  { #liste de valeur nulle, vide donc on renvoie rien !
    info::info(20,"Mot clef <$motDico> => aucune valeur trouvee (".__FILE__." ".__LINE__.")");
    return;
  }

  # on recupere la valeur souhaite si elle existe
  my $valeur= @$ref_liste_valeurs[$rang];
  info::info(20,"Mot clef <$motDico>, valeur stockee pour <$parametre> => <$valeur> (".__FILE__." ".__LINE__.")");
  
  # decomposer en une liste de valeurs exploitables
  my @liste = decomposer_valeur($valeur);
  my $nbEle= $#liste;
  info::info(20,"dans recup_valeur_mot(), <$nbEle> valeur(s) renvoyee(s) => <@liste> (".__FILE__." ".__LINE__.")");
    
  return @liste
}


###############################################################################
#PRIVEE
#Role  : decomposer une chaine du type <'a';b';'c'> en <a>,<b> et <c>
#Entree:  1
#       la chaine a decomposer
#Retour: un tableau
#
###############################################################################
sub decomposer_valeur
{
    my $valeur =@_[0];
    my @liste  =();
    
    $_=$valeur;
    # Il est possible que la valeur soit en fait composee de plusieurs valeurs
    # exemple : <'a';b';'c'> => 3 valeurs <a>,<b>, <c>
    #           <11;12;13>   => 3 valeurs <11>,<12>,<13>
    #           <'NOMBRE MAXI D''ITERATIONS POUR L''EQUATION'> => 1 valeur <'NOMBRE MAXI D''ITERATIONS POUR L''EQUATION'>
    #           <toto d''ici> => valeur <toto d''ici>
    #           <' ';'titi';'toto'>

    #if ( m%(^['"])
    #         \s*
    #         \1
    #         ;? # eventuellement peut avoir 
    #      %x )
    #{
      # traitement du cas 1 (identifie le 22/10/01 NL)
      # qui est <' '> ou <''>
      # donc on renvoie une chaine vide
      #push(@liste, "");
      #$_ = &';
    #}
    
    # Traitement du cas 1
    #     cas avec des valeurs entre guillemets
    #while ( m|
    #       (['"])          # Qq chose qui commence par " ou '
    #                        # dont on va prendre le contenu
    #       (.*?             #  qui peut contenir des ''
    #         ('')
    #         .*?
    #       )*               #  cette suite se repetant 0 à n fois
    #       \1              # qui se termine par le " ou le ' de (1)
    #       ;?              # avec un pt virgule delimitant la valeur suivante eventuelle          
    #     |x )
    #print "** <$_>\n";
    while ( m|^
            (['"])          # Qq chose qui commence par " ou '
            (               # dont on va prendre le contenu
             (.*?(''))?      #  qui peut contenir <''>
             .*?
            )*              #  cette suite se repetant 0 à n fois
            \1              # qui se termine par le " ou le ' de (1)
            ;?              # avec un pt virgule delimitant la valeur suivante eventuelle
          |x )
    {
      my $temp = $&;
      $_ = $';
      s/^\s*//;
      
      if ( $temp =~ m|^(['"])\s*\1| )
      {
        # si la chaine recuperee est une chaine vide
        # on place une chaine vide et hop on continue...
        push(@liste, "");
      }
      else
      {
        # enlever les guillemets autour de ce qu'on a recupere
        $temp =~ s/^\s*['"]//;
        $temp =~ s/['"]?
                       \s*;?\s*$
                  //x;
        
        $temp =~ s/''/'/g;
        push(@liste, $temp);
      }
    }
    
    # Traitement du cas 3
    if ($#liste ==-1)
    {
      @liste = split(/;/, $_);
    }
        
    # Traitement du dernier cas : une seule valeur
    if ($#liste ==-1)
    {
      @liste = $valeur;
    }
    return @liste;
}  
  
###############################################################################
#PUBLIC
#Role  : Renvoyer, pour un mot donne appartenant au dictionnaire des mots clefs
#        la liste de toutes ces caracteristiques.
#Entree: 2
#       - reference vers une structure dictionnaire
#       - nom du mot du dictionnaire dont on cherche les caracteristiques
#         (clef d'entree dans la table de hash)
#Retour: une liste
#
###############################################################################
sub recup_valeurs_mot_dictionnaire
{
  # recuperer les arguments fournis a la fonction
  my ($refDictionnaire,$motDico)= @_;

  if (! exists( $refDictionnaire->{$motDico}) )
  {
    # ce mot n'existe pas dans ce dictionnaire
    info::info(10,"mot $motDico inexistant dans le dictionnaire $refDictionnaire (".__FILE__." ".__LINE__.")");
    return;
  }
  else
  {
    # recuperer la reference de la liste de parametre pour ce mot du dico
    my $ref_liste_valeurs = $$refDictionnaire{$motDico};

    unless ($ref_liste_valeurs != () )
    { #liste de valeur nulle, vide donc on renvoie une liste vide !
      info::info(20,"Mot clef <$motDico> => aucune valeur trouvee (".__FILE__." ".__LINE__.")");
      return;
    }
    info::info(20,"Mot clef <$motDico> valeur(s) stockee(s) => <@$ref_liste_valeurs> (".__FILE__." ".__LINE__.")");
    return @$ref_liste_valeurs;
  }
}

###############################################################################
#PRIVEE
#Role   : Sert a definir un nom qui sera utilise comme clef d'entree dans une
#         table de hash, celui ci contiendra une partie permettant de connaitre
#         la langue dans lequel il a ete defini.
#         (cf retrouver_nommot_dapres_nom_de_stockage() pour l'operation inverse)
#
#         On utilise ceci car il est possible d'avoir dans le dictionnaire pour
#         un mot le meme nom dans 2 langues differentes (exemple <DESTINATION>)
#         Le stockage des 2 n'est donc pas possible dans une table de hash car
#         chaque clef doit porter un nom different.
#Entree : <nom du mot> (E)
#         <argument a ajouter> 
#             0 si francais
#             1 si anglais
#Retour : <nouveau nom>
###############################################################################
sub construire_nom_de_stockage
{
  my ($motCourant, $idLang)    = @_;
  
  my $delim=qq(|LANG);  
  
  $motCourant = $motCourant.$delim.$idLang;
  return $motCourant;
}

###############################################################################
#PRIVEE
#Role   : Permet de retrouver le nom d'un mot d'apres la clef qui a servie
#         d'entree dans la table de hash.
#         (cf construire_nom_de_stockage() pour construire cette clef)
#Entree : 1
#         <Nom du mot> (E)
#Retour : Rien si probleme, ou tableau de 2 valeurs:
#           <nouveau nom>
#           <numero de la langue de ce mot>
###############################################################################
sub retrouver_nommot_dapres_nom_de_stockage
{
  $_            = @_[0];
  my @tabVal;

  unless ( m/\|LANG(\d)/)
  {    
    return;
  }
  @tabVal[1]=$1;
  
  s/\|LANG\d//;
  
  @tabVal[0]=$_;
  return @tabVal;
}


###############################################################################
#PRIVEE
#Role   : Tester si la ligne donnee en entree, qui provient d'un fichier cas ou
#         d'un fichier dictionnaire, est complete ou non
#Entree : 1
#         Ligne lue
#Retour : 1 si oui
###############################################################################
sub is_ligne_fichier_casdico_complete
{
    $_= @_[0];
    
    # On verifie que la ligne contient bien le nom de la caracteristique
    # ainsi que sa valeur car celle-ci pourrait etre definie sur plusieurs lignes
    #ex: <DEFAUT = 
    #       1900;1;
    #       1;>
    #ex: <DEFAUT =
    #       'ceci est une definition de 
    #       defaut'
    if ( m/([:=;]\s*$)/ )
    {   #1er cas: ligne finie par un <:>, un <=> ou un <;>
        info::info(20,"CAS1 ligne non terminee <$_>");
        return 0;
    }
    elsif ( m/
              (^\s*['"])        # 1 ligne commencant par ' ou "
              |                 # OU
              ([:=]\s*["'])     # 1 ligne commencant par 'ou" apres le =
             /x )
    {   #2eme cas: definition de la valeur qui commence par un 'ou"
        # ex: <SUBMIT = 'lib;for;void;telemac2dV3P1.a damoV3P1.a utileV3P1.a
        #      biefV3P1.a hpV3P1.a'>
        
        # on travaille sur le reste de la chaine
        $_=$';
        
        my $nb=1;
        while ( m/['"]/ ){
          $nb++;
          $_=$';
        }
        my $t=$nb&1;
        
        if ( $nb==0 || ($nb&1)==1)
        { 
          info::info(20,"CAS2 ligne non terminee <$_> ($nb)");
          return 0;
        }
    }

    return 1;
}


###############################################################################
#PRIVEE
#Role   : Effectuer un filtrage sur une ligne lue dans un fichier cas ou un 
#         fichier dictionnaire (enleve les lignes vides, les commentaires ...)
#         qui est la suite d'une ligne complete donnee en argument (avec cette
#         suite est pe etre complete ou pas encore)
#Entree : 2
#         <Valeur de la ligne complete courante >
#         <Ligne lue>
#Retour : <nouvelle ligne complete>
###############################################################################
sub filtrer_ligne_fichier_casdico
{
  if ($#_+1!=2)
  {
    info::info(1,"Appel incorrect pour filtrer_ligne_fichier_casdico()");
    return @_[0];
  }
  my ($ligneComplete, $ligne) = @_;


  chomp ($ligne);   #enlever le CR/LF en fin de ligne si il est present

  #Filtrage
  $ligne =~ s|^\s*&+.*||;     #on enleve les lignes commencants par un "&"
    
  $ligne =~ s|^\s*/+.*||;     #on enleve les lignes COMPLETES de commentaires
     
  # Ca c'est pour eviter le bug du perl 5.005_02 qui fait que ca rame grave
  # avec <            coef=1 /comm>
  # (NL le 5/11/01)
  $ligne =~ s|\s*||;
  # ca c'est pour <GEOMETRY FILE :                 geo /FORT.1> (NL le 5/11/01)
  $ligne =~ s|\s*([:=])\s*|\1|;
  
  $ligne =~ s/\s+$//;     #plus de blancs finaux
  
  
  # on concatene les deux strings
  $_ = $ligneComplete.$ligne;
  
  
  # on enleve aussi les commentaires en fin de ligne
  #$ligne =~ s|/.*$||; avec ca on loupe <FICHIER =  '../ex/toto'> 5/11/01
  # autre exemple a prendre en compte
  #   <MOTCLEF =  'exe//toto.exe' 'toto.exe' '/toto2/' /commentaire>
  #   <            coef=1 /comm>
  if ( m|/| )
  {
    #print "1) <$ligne>\n";
    # il est possible que cette ligne contienne un commentaire donc
    s|(          # p1 => partie utile
        (\s*
          ('.*?')?  # Tout ce qui est entre ' '
                    # ou
          (".*?")?  # tout ce qui est entre " "
          \s*
          [;,-.]?   # plus suivi d'un separateur eventuellement
        )+         # Tout ca de 1 à n fois repete!
      
      )          # Fin de p1
             # ICI commence la partie a virer !
      \s*
      (/.*)?$    # partie en commentaire si elle existe (dou le ?)
     |\1|x;
    
    #print "2)\n";
  }
  
  my $l=$_;
  $l=~s|\s+$||;     #enlever les blancs finaux encore
  
  return $l;
}

###############################################################################
#PUBLIC
#Role   : Lecture d'un fichier dictionnaire qui contient la definition de mots
#Entree : <Nom du fichier dictionnaire>
#Retour : <structure dictionnaire>
###############################################################################
sub lecture_fichier_dictionnaire
{
  my $filedico    =$_[0];
  my %tab_hash    =();
  my ($motCourant,$ligneComplete)  =();

  # ouverture du fichier
  unless (open (FDICO, $filedico) ){
    info::info(10,"Ouverture impossible du fichier dictionnaire.");
    return;
  }
 
  # lire ligne a ligne le fichier et effectuer un traitement
  info::info(10,"INFO => Lecture du fichier dictionnaire $filedico");
  while (<FDICO>)
  {
        
    #Filtrer
    $ligneComplete = filtrer_ligne_fichier_casdico($ligneComplete, $_);
    #on passe a la ligne suivante si plus rien après nettoyage ...
    next unless length($ligneComplete);
    
    next if (! is_ligne_fichier_casdico_complete($ligneComplete) );

    
    info::info(20,"Ligne complete à traiter : <$ligneComplete> (".__FILE__." ".__LINE__.")");
    
    # dissocier le nom du mot de vocabulaire de sa valeur
    my ($motClefTrouve, $valeur) = traiter_ligne_fichier_dictionnaire($ligneComplete);

    # Tester si le mot clef est "interessant"
    unless (is_inside($motClefTrouve,@LST_MOTS_CLEFS_A_RECUPERER))
    {
      $ligneComplete = ();
      next; # non donc on passe a la ligne suivante!
    }
        
    # sinon
    if ( is_inside($motClefTrouve, @LST_DEF_MOTS_CLEFS) )
    { # definition d'un nouveau mot du dictionnaire

      # enlever les '' ou "" entourant la valeur du mot si ils existent
      $valeur =~  s|(['"])(.*)\1|$2|;
      #enlever le double '' (exemple: <MASSE VOLUMIQUE DE L''EAU> )
      $valeur =~  s/''/'/g;      

      if ( is_inside($motClefTrouve,@LST_MOTSCLEFS_LANG1) )
      {
        # ici on definit le mot du dictionnaire courant dans une autre langue

        # enlever les '' ou "" entourant la valeur du mot si ils existent
        $valeur =~  s|(['"])(.*)\1|$2|;
        #enlever le double '' (exemple: <MASSE VOLUMIQUE DE L''EAU> )
        $valeur =~  s/''/'/g;
                    
        #ajouter un nouveau mot clef dans la table de hash
        # qui sera pour l'instant la copie de celui dans la langue d'origine
        my $motCourant2 = $valeur;
        
        # pour connaitre la langue en lisant la clef de la table de hash
        my $motCourantComplet2= construire_nom_de_stockage($motCourant2,"1");
        my $motCourantComplet = construire_nom_de_stockage($motCourant, "0");
        
        info::info(10,"Entree cree dans la table : $motCourantComplet2 (" .__FILE__." ".__LINE__.")");

        my @copieParam = recup_valeurs_mot_dictionnaire(\%tab_hash, $motCourantComplet);
        $tab_hash{$motCourantComplet2} = \@copieParam;

        my @tab = $tab_hash{$motCourantComplet2};
        definir_langue_mot(\@tab,"anglais");
        $tab_hash{$motCourantComplet2} = \@tab;
              
        
        ajouter_reference_vers_motclef(\%tab_hash, $motCourantComplet, $motCourantComplet2);                        
        ajouter_reference_vers_motclef(\%tab_hash, $motCourantComplet2, $motCourantComplet);
      }
      else{        
        $motCourant = $valeur;
        # pour connaitre la langue en lisant la clef de la table de hash
        my $motCourantComplet= construire_nom_de_stockage($motCourant,"0");

        info::info(10,"Entree cree dans la table : $motCourantComplet (".__FILE__." ".__LINE__.")");

        my @tab;
        definir_langue_mot(\@tab,"francais"); #ca ca marche!        
        #definir_langue_mot(\($tab_hash{$motCourantComplet}),"francais"); #ca non !!!
        
        # la valeur dans la table de hash pour cette entree est un pointeur
        # vers un tableau contenant les valeurs.
        $tab_hash{$motCourantComplet} = \@tab;
      }
    }
    #info::erreur("Mot clef attendu non trouve, probleme dans le fichier dictionnaire (".__FILE__." ".__LINE__.")");
    else
    { 
      # On complete un mot deja defini dans le dictionnaire en lui ajoutant une
      # caracteristique.
      #
        # Petite verif ...
        if ( is_inside($motClefTrouve, @LST_DEF_MOTS_CLEFS) )
        {
          if ( $motClefTrouve eq "NOM" )
          { # on ne devrait pas trouver ce mot clef ici, erreur donc
            info::erreur("Le mot $motCourant existe deja dans ce dictionnaire.\n (".__FILE__." ".__LINE__.")");
            return;
          }
        }
        
        # On recupere d'abord la traduction de ce mot de vocabulaire qui en theorie devrait exister
        my @lst = recup_valeur_mot(\%tab_hash, $motCourant, "reference","0");
        
        my $motCourantComplet2 = @lst[0];        
        unless ( $motCourantComplet2 )
        {
            info::info(20,"Valeur renvoyee par recup_valeur_mot() <@lst> (".__FILE__." ".__LINE__.")");
            info::erreur("Le mot $motCourant n'a pas de traduction dans le dictionnaire (".__FILE__." ".__LINE__.")");
            return;
        }
        # determiner le nom de stockage dans la table de Hash pour ce mot
        my $motCourantComplet = construire_nom_de_stockage($motCourant,"0");
        if (! is_inside($motClefTrouve,@LST_MOTSCLEFS_LANG1) )
        { 
          # Ajout d'un parametre
          info::info(10,"->ajout de $valeur pour le parametre $motClefTrouve du mot $motCourantComplet (".__FILE__." ".__LINE__.")");
          completer_mot_du_dictionnaire(\%tab_hash, $motCourantComplet,
                                        $motClefTrouve, $valeur);

          #ajouter aussi ce parametre dans le mot traduisant ce mot du dico
          # uniquement si ce mot est commun aux deux langues
          if (! is_inside($motClefTrouve, @LST_MOTSCLEFS_LANG0)){
              info::info(10,"->ajout de $valeur pour le parametre $motClefTrouve du mot $motCourantComplet2 (".__FILE__." ".__LINE__.")");
              completer_mot_du_dictionnaire(\%tab_hash, $motCourantComplet2,
                                            $motClefTrouve, $valeur);
          }
        }
        else{
          # on definit un des parametres de ce mot mais dans l'autre langue

          info::info(10,"->ajout de $valeur pour le parametre $motClefTrouve du mot $motCourantComplet2 (".__FILE__." ".__LINE__.")");
          if ($motCourantComplet2 ne () ){
            completer_mot_du_dictionnaire(\%tab_hash, $motCourantComplet2,
                                          $motClefTrouve, $valeur);}
          else{
            info::erreur("Une traduction du mot $motCourant n'a pas ete trouvee (".__FILE__." ".__LINE__.")");}

        }

    };
    $ligneComplete = ();
    # on arrive directement ici si la ligne courante est continuee sur celle suivante
    
  }#while

  # Indiquer que les valeurs associees a chaque mot de ce dictionnaire sont
  # pour l'instant toutes des valeurs par defauts.
  while ( my ($key,$refliste) = each(%tab_hash) )
  {
    set_val_pardefaut_mot(\%tab_hash, $key, 1);
  }
  info::info(10,"INFO => Fin de lecture du fichier dictionnaire.");

  unless ( close(FDICO) ) {info::erreur("can't close $filedico");}

  return %tab_hash;
}

###############################################################################
#PRIVEE
#Role  : indiquer pour un mot du dictionnaire si la valeur pour laquelle il
#        est defini actuellement est celle donnee par defaut (donc celle qui
#        normalement lue dans un fichier dictionnaire).
#Entree: 3
#  reference vers la structure dictionnaire
#  Nom du mot du dictionnaire (nom d'entree pour la table de hash)
#  valeur a mettre (1 si valeur du dictionnaire, 0 sinon)
#Retour: aucun
###############################################################################
sub set_val_pardefaut_mot
{
  my ($refDictionnaire, $motDico, $val) = @_;

  unless ( exists($$refDictionnaire{$motDico}) )
  {
    info::erreur("Mot <$motDico> inexsistant.(".__FILE__." ".__LINE__.")");
    return;
  }

  # determiner le rang de stockage
  my $rang = donne_rang_stockage("flag_valdef");

  # recuperer la liste associee a ce mot
  my $listeTempo = $refDictionnaire->{$motDico};

  # on place la valeur, au bon rang!
  $listeTempo->[$rang] = $val;

  return; #terminé !
}
###############################################################################
#PUBLIC
#Role  : Demander pour un mot du dictionnaire si la valeur pour laquelle il
#        est defini actuellement est celle donnee par defaut (donc celle qui
#        normalement lue dans un fichier dictionnaire) ou une valeur trouvee
#        dans un fichier cas.
#Entree: 2
# reference vers la structure dictionnaire
# Nom du mot du dictionnaire
#Retour: 1 si la valeur definie est celle par defaut, 0 sinon
###############################################################################
sub is_val_pardefaut_mot_dictionnaire
{
  my ($refDictionnaire, $motDico) = @_;

  unless ( exists($$refDictionnaire{$motDico}) )
  {
    #info::erreur("Mot $motDico inexsistant dans ce dictionnaire (".__FILE__." ".__LINE__.")");
    info::info(10,"Mot <$motDico> inexsistant dans ce dictionnaire (".__FILE__." ".__LINE__.")");
    return;
  }
  # determiner le rang de stockage
  my $rang = donne_rang_stockage("flag_valdef");

  if ( $refDictionnaire->{$motDico}[$rang] eq 1 ){
    return 1;}
  else{
    return 0;}
}

###############################################################################
#Role  : Traiter une ligne d'un fichier dictionnaire donnee en argument, cad
#        extraction du nom d'un mot clef de sa valeur separee par un ":" ou
#       un "=".
#Entree: une ligne complete lue dans un fichier cas
#Retour: une liste de 2 termes.
#        le nom du mot
#        la valeur
###############################################################################
sub traiter_ligne_fichier_dictionnaire
{
  my $ligne     = @_[0];

  # Si la ligne est composee de cette facon :
  # 'valeur 1';'valeur2';'valeur3'
  # on va supprimer les ; qui ne servent donc a rien 13/9/01
  #$ligne =~ s|\s*((['"]).*?\2)\s*;\s*|\1 |g ;
  # En fin de compte je les laisse (17/9/01) et la fonction qui
  # sera charge de renvoyer les valeurs les enlevera!
  
  # Rq : les valeurs peuvent etre renvoyees entre '' ou "", ils
  #      ne sont pas enleves ici!

  my ($var, $valeur) = split (/\s*[=:]\s*/, $ligne , 2);
  
  # Pour la valeur, il faut remplacer les <'   '> par des <''>, il faut
  # enlever ces blancs inutiles.
  $valeur =~ s|(['"])
                \s+
               \1
              |\1\1|gx;
  
  #enlever les blancs superflus en debut et fin
  $valeur =~ s/^\s*//; $valeur =~ s/\s*$//;
  $var    =~ s/^\s*//; $var    =~ s/\s*$//;
    
  return ($var, $valeur);
}

###############################################################################
#PRIVEE
#Role  : Definir ou redefinir une caracteristique d'un mot  dans un
#        dictionnaire(structure table de hash de tableaux). Cette caracteristique
#        peut etre la valeur(DEFAUT), le type(TYPE), ...
#Entree: 4
#       - reference vers une structure dictionnaire
#       - nom du mot clef a completer (clef d'entree de la table de Hash)
#       - nom du parametre rajoute ou redefini pour ce motclef
#       - valeur de ce parametre
#Retour: aucun
#
###############################################################################
sub completer_mot_du_dictionnaire
{
  # prise en compte des arguments
  my ($refDictionnaire, $motDico, $nomParam, $valeur) = @_;

  # on recupere la table de hash "pointee" par refDictionnaire
  # CEST PAS POSSIBLE CA my %dictionnaire = %$refDictionnaire;

  # Determiner le rang de stockage dans le tableau
  my $rang=donne_rang_stockage($nomParam);

  unless ( exists($$refDictionnaire{$motDico}) )
  {
    info::info(10,"Mot <$motDico> inexsistant dans ce dictionnaire (".__FILE__." ".__LINE__.")");
    return;
  }

  # recuperer la reference vers le tableau contenant les differentes valeurs
  # pour ce mot clef
  my $liste = $$refDictionnaire{$motDico};

  #ajout simple a la fin du tableau
  #push(@$liste, $valeur);
  # ajout de la caracteristique a l'endroit prevu
  @$liste[$rang]= $valeur;

  # et on remets dans la table de Hash la reference vers ce tableau
  # auquel on vient d'ajouter une valeur.
  $$refDictionnaire{$motDico}=\@$liste;
  
  info::info(20,"Mot <$motDico> nouvelle valeur pour <$nomParam> => <$valeur> (".__FILE__." ".__LINE__.")");

  return;
}

###############################################################################
#PRIVE
#Role  : Modifier la valeur d'un mot d'un dictionnaire (structure table de hash
#        de tableaux). La valeur ainsi affectee sera consideree comme n'etant
#        pas une valeur par defaut.
#Entree: 3
#       - reference vers une structure dictionnaire
#       - nom du mot clef a completer
#       - nouvelle valeur
#Retour: 0 si mot clef modifie
#
###############################################################################
sub modifier_mot_dictionnaire
{
  # prise en compte des arguments
  my ($refDictionnaire, $motDico, $newValeur) = @_;

  my $motDicoComplet = construire_nom_de_stockage($motDico,"0");
  my $otherLang="1";
  info::info(20,"Mot clef <$motDico> langue <0> => nom de stockage dans la table <$motDicoComplet> (".__FILE__." ".__LINE__.")");
  
  unless ( exists($$refDictionnaire{$motDicoComplet}) )
  {
    info::info(10,"Mot <$motDico> langue <0> inexsistant dans ce dictionnaire (".__FILE__." ".__LINE__.")");
    
    $motDicoComplet = construire_nom_de_stockage($motDico,"1");
    $otherLang="0";
    
    info::info(20,"Mot clef <$motDico> langue <1> => nom de stockage dans la table <$motDicoComplet> (".__FILE__." ".__LINE__.")");
    
    unless ( exists($$refDictionnaire{$motDicoComplet}) )
    {
      info::info(10,"Mot <$motDico> langue <1> inexsistant dans ce dictionnaire (".__FILE__." ".__LINE__.")");
      return 1;
    }
  }

  # Determiner le rang de stockage dans le tableau de la valeur
  my $rang=donne_rang_stockage("DEFAUT");

  # recuperer la reference vers le tableau contenant les differentes valeurs
  # pour ce mot clef
  my $refListe = $refDictionnaire->{$motDicoComplet};
  #changer la valeur
  $refListe->[$rang] =$newValeur;
  
  info::info(20,"Mot clef <$motDicoComplet> nouvelle valeur => <$newValeur> (".__FILE__." ".__LINE__.")");
  
  # indiquer que l'on a change cette valeur (plus de val par defaut donc)  
  set_val_pardefaut_mot($refDictionnaire, $motDicoComplet, 0);

  
  # Maintenant on va changer la valeur dans la traduction de ce mot clef ...
  #
  # recuperer le nom identifiant ce mot dans l'autre langue
  my @lst = recup_valeur_mot($refDictionnaire, $motDico,
                             "reference", $otherLang);
  my $motDicoDansAutreLangue = @lst[0];
  unless ( exists($$refDictionnaire{$motDicoComplet}) )
  {
    info::info(20,"Mot <$motDicoComplet> inexsistant dans ce dictionnaire (".__FILE__." ".__LINE__.")");
    info::info(10,"Impossible de modifier la traduction de <$motDico> (".__FILE__." ".__LINE__.")");
    return 1;
  }

  #
  # traduire la valeur si c'est un booleen tout d'abord
  my $newValeurOtherLang = traduire_valeur($newValeur);
            
  my $refListe = $refDictionnaire->{$motDicoDansAutreLangue};
  $refListe->[$rang] =$newValeurOtherLang;
  
  info::info(20,"Mot clef <$motDicoDansAutreLangue> nouvelle valeur => <$newValeurOtherLang> (".__FILE__." ".__LINE__.")");
  
  set_val_pardefaut_mot($refDictionnaire, $motDicoDansAutreLangue, 0);

  return 0;
}

###############################################################################
#PRIVE
#Role  : traduire une valeur d'une langue vers une autre (ne traduit que des 
#        booleens !!)
#Entree: 1
#       - chaine a traduire
#Retour: chaine traduite
#
###############################################################################
sub traduire_valeur
{
    my $str    = @_[0];

    #my @tabStr = decomposer_valeur($str);    
    $_ = $str;
    if ( m|^\s*OUI\s*$| )
    {
      s|OUI|YES|;
    }
    elsif ( m|^\s*YES\s*$| )
    {
      s|YES|OUI|;
    }
    elsif ( m|^\s*NON\s*$| )
    {
      s|NON|NO|;
    }
    else
    {
      if ( m|^\s*NO\s*$| )
      {
        s|NO|NON|;
      }
      else
      {
        # pas de traduction possible 
        info::info(20,"  Valeur <$str> non traduite (".__FILE__." ".__LINE__.")");
      }
    }

    $str = $_;    
          
    return $str;
}

###############################################################################
#PRIVEE
#Role  : Ajouter pour une clef de la structure "hash of arrays" le nom d'une
#        autre clef deja existante, dans le tableau des valeurs.
#        (Ceci est utilisé pour savoir quelle est le nom d'un mot de vocabulaire
#        pour une autre langue)
#Entree: 3
#         reference vers la structure dictionnaire
#         nom de la clef
#         nom de la clef qui sera referencee
#Retour: aucun
###############################################################################
sub ajouter_reference_vers_motclef
{
  # prise en compte des arguments
  my ($refDictionnaire, $mot, $mot2) = @_;

  my $refTab = $$refDictionnaire{$mot};

  # determiner le rang de stockage
  my $rang = donne_rang_stockage("reference");
  
  $refTab->[$rang] = $mot2; # on place la valeur, au bon rang!
  
  info::info(20,"La clef <$mot> reference la clef <$mot2> (".__FILE__." ".__LINE__.")");
  
  return;
}

###############################################################################
#PRIVEE
#Role  : Definir la langue d'un mot
#Entree: 2
#        Reference vers le tableau contenant les parametres pour le mot
#        Nom de la langue
#Retour: Aucun
###############################################################################
sub definir_langue_mot
{
  #recuperer les arguments
  my ($refListe, $lang) = @_;

  my $rang = donne_rang_stockage("langue");

  $refListe->[$rang]= $lang;
  
  #info::info(20,"Le mot est defini dans la langue <$lang> (".__FILE__." ".__LINE__.")");
  return;
}

###############################################################################
#PUBLIC
#Role  : Renvoyer le rang de stockage pour le parametre d'un mot de vocabulaire
#        d'un dictionnaire
#Entree: nom du parametre caracterisant un mot clef d'un dictionnaire
#Retour: rang
###############################################################################
sub donne_rang_stockage
{
  #recuperer les arguments
  my $nomParam = $_[0];
  my $rang;

  if (! exists($LST_PARAM_STOCKES{$nomParam}) )
  {
    info::erreur("Nom de parametre incorrect <$nomParam> (".__FILE__." ".__LINE__.")");
    return;
  }
  else{
    return( $LST_PARAM_STOCKES{$nomParam} );
  }
}


###############################################################################
#Role  : Tester si un scalaire se trouve dans une liste
#Entree:  - valeur a chercher
#         - liste des valeurs
#Retour: 1 si oui
#        0 si non
###############################################################################
sub is_inside
{
  my ($val, @liste)=@_;
  my $longueur = @liste;
  while (@liste)
  {
    my $last_value=pop(@liste);
    if ( $last_value eq $val )
    {
      return 1;
    }
  }
  return 0;
}

###############################################################################
#Role  : visualiser les valeurs d'une table de hash
#Entree: <la table a visualiser>
#Retour: aucun
###############################################################################
sub visu_table_hash
{
  my (%tab_hash)=@_;

  my $i=keys(%tab_hash);
  print "\nNombre d'elements contenus : $i\n";
  $i =0;
  while ( my ($key,$value) = each(%tab_hash))
  {
      print "Entree $i => $key / $value\n";
    $i++;
  }
  return;
}

###############################################################################
#Role  : visualiser le contenu d'une structure 'dictionnaire' de type
#        "hash of arrays"
#Entree: <le dictionnaire a visualiser>
#Retour: aucun
###############################################################################
sub visualiser_mots
{
  my (%dico)=@_;
   
  while ( my ($key,$refliste) = each(%dico) )
  {
    print "Mot clef          : $key\n";
    #print "ICI : <$refliste>\n";
    my $nbElements= @$refliste;
    
    my @tabRetour = retrouver_nommot_dapres_nom_de_stockage($key);
    my $motDicoOriginal = $tabRetour[0];
    my $langue= $tabRetour[1];

    #print "ici <@tabRetour> <$motDicoOriginal> <$langue>\n";
    
    my $param;
    foreach $param (keys(%LST_PARAM_STOCKES))
    {
      if (! is_inside($param,@LST_MOTSCLEFS_LANG1) )
      {
        my @liste = recup_valeur_mot(\%dico, $motDicoOriginal, 
                                     $param,$langue);
        my $nbEle = @liste;
        if ($nbEle>1)
        {
            for(my $i=1;$i<=$nbEle;$i++)
            {
              print "caract. $param numero $i : <@liste[$i-1]>\n";
            }
        }
        else{
              print "caract. $param : <@liste>\n";
        }
      }
    }
  }
  return;
}

###############################################################################
#PUBLIC
#Role   : Modifier les mots d'une structure dictionnaire dont les nouvelles
#         valeurs sont donnees en entree, dans une table de Hash (couple nom du
#         mot clef->valeur associee)
#Entree :
#         Reference vers une structure hash of arrays (E/S)
#         Reference vers une table de hash (E)
#
#Retour : nombre de mots modifies
###############################################################################
sub modifier_mots
{
  #prendre en compte les arguments
  my ($refDico,$refnewMotsClefs) = @_;
  my $i=0;
  
  while( my ($nomMotClef,$valeur) = each(%$refnewMotsClefs) )
  {
    # Modifier ce mot dans la structure dictionnaire
    if ( modifier_mot_dictionnaire($refDico,$nomMotClef,$valeur)!=0 )
    {
      info::info(10,"Mot clef <$nomMotClef> non trouve dans le dictionnaire (".__FILE__." ".__LINE__.")");
    }
    else
    {
      info::info(20,"Mot clef <$nomMotClef> modifie dans le dictionnaire (".__FILE__." ".__LINE__.")");
    }
    
    # Visu
    #
    my $nomCompletStock = construire_nom_de_stockage($nomMotClef, "0");
    # recuperer la ou les nouvelles valeurs de ce mot de vocabulaire
    my @valeur          = recup_valeur_mot($refDico, $nomMotClef, "DEFAUT", "0");
    
    # recuperer la traduction de ce mot de vocabulaire
    my @nomCompletLang1 = recup_valeur_mot($refDico, $nomMotClef, "reference", "0");
    # recuperer ce mot d'apres le mot servant d'entree dans la table de hash  
    my @nomMotClefLang1 = retrouver_nommot_dapres_nom_de_stockage(@nomCompletLang1[0]);
    # recuperer les valeurs associees
    my @valeur2         = recup_valeur_mot($refDico, @nomMotClefLang1[0], "DEFAUT", "0");
    #
    # Visualisation    
    info::info(10,"MOT CLEF <$nomMotClef> nouvelle VALEUR <@valeur>");
    info::info(10,"MOT CLEF <@nomMotClefLang1[0]> nouvelle VALEUR <@valeur2>");
    info::info(10,"----------------------------------------------");

    $i++;
  }
  info::info(10,"=> Nombre de mots clefs modifies => $i");

  return $i;
}

###############################################################################
#PUBLIC
#Role   : Lecture d'un fichier cas et verification de celui ci
#         A FAIRE eventuellement ...
#           verif synthaxe, verif nb de valeurs, type, ...
#Entree : 2
#         <Nom du fichier cas> (E)
#         <reference vers une Hash of arrays (structure contenant les mots
#          clefs du dictionnaire)> (E)
#Retour : Une table de hash (couple nom du mot clef -> valeur associee)
#         vide si probleme
###############################################################################
sub lecture_fichier_cas
{
  my ($filecas, $refDico) = @_;

  my %listeMots=();
  my ($ligneComplete, $isLigneComplete) =();

  # ouverture du fichier
  unless( open (FDICO, $filecas) )
  {
    info::info(10,"Ouverture impossible du fichier cas.");
    return;
  }
    
    
  # lire ligne a ligne le fichier et effectuer un traitement
  info::info(10,"INFO => Lecture du fichier cas $filecas");
  while (<FDICO>)
  {      
      #Filtrer et concatener si on travaille sur une declaration sur plusieurs lignes
      $ligneComplete = filtrer_ligne_fichier_casdico($ligneComplete, $_);
      #on passe a la ligne suivante si plus rien après nettoyage ...
      next unless length($ligneComplete);
    
      
      # On verifie que la ligne contient bien la valeur du mot clef, car celle-ci
      # pourrait etre definie sur la ligne suivante
      if (! is_ligne_fichier_casdico_complete($ligneComplete) ){
        goto finTraitement; }
      
      info::info(20,"Ligne complete à traiter : <$ligneComplete> (".__FILE__." ".__LINE__.")");
      
      # Dissocier les noms des mots clefs de leurs valeurs
      my %lstMotsPourUneLigneComplete = traiter_ligne_fichier_cas($ligneComplete);
      if (! (my $nbEle=%lstMotsPourUneLigneComplete) )
      {
        info::erreur("Mauvaise syntaxe dans le fichier cas, ligne <$ligneComplete>");
        info::info(10,"         Ligne ".__LINE__." dans le fichier ".__FILE__);
      }

      while( my ($nomMotClef,$valeur) = each(%lstMotsPourUneLigneComplete) )
      {
        # verifier tout d'abord l'existence de ce mot dans le dictionnaire
        # contenant la liste de tous les mots clefs existants et definis
        my @tab = recup_valeur_mot($refDico,$nomMotClef,"langue","0");
        if ($#tab<0){
          # reessayer dans l'autre langue cette fois
          @tab = recup_valeur_mot($refDico,$nomMotClef,"langue","1");
        }
        if ($#tab<0)
        {
          # on arrete tout de suite
          info::erreur("Le mot <$nomMotClef> n'existe pas dans le dictionnaire");
          info::info(10,"Ligne ".__LINE__." dans le fichier ".__FILE__);
          return;
        }


        # ok, on ajoute tous ces mots a la liste globale ...
        $listeMots{$nomMotClef}= $valeur;

        # visu en mode DEBUG
        info::info(10,"MOT CLEF <$nomMotClef> VALEUR <$valeur>");
      }
            
      # initialisation pour la prochaine fois
      $ligneComplete = ();

    finTraitement:    
  }#while

  unless (close(FDICO)) { info::erreur("can't close $filecas");}
  info::info(10,"INFO => Fin de lecture du fichier cas $filecas (".__FILE__."  line ".__LINE__.")");
  
  return %listeMots;
}

###############################################################################
#PRIVEE
#Role  : traiter une ligne d'un fichier cas. Celle ci a une syntaxe du type
#        TITRE = 'xxxx '
#        ou encore NOM DU CRAY : cray1  USER CRAY : cray_user
#Entree: 1
#       - une ligne lue dans un fichier cas
#Retour: une table de hash (couple nom du mot clef, valeur)
#
###############################################################################
sub traiter_ligne_fichier_cas
{
  my $ligne     = @_[0];
  my %lstSortie = ();

  my (@tabVar, @tabVal, $restant) = ();

  ### MEMO ###
  # \D => any single nondigit character
  # \w => [a-zA-Z0-9_] word char
  # \S => tous sauf des espaces (^\s)
  # RQ1 : soit l'expression <'TELEMAC 2D : GOUTTE D''EAU DANS UN BASSIN$'>
  # avec m/'.*'/ on obtient <'TELEMAC 2D : GOUTTE D''EAU DANS UN BASSIN$'>
  # avec m/'.*?'/ on obtient <'TELEMAC 2D : GOUTTE D'> car le ? match le
  # pls petit nombre de caracteres possibles.
  ############
  info::info(30,"\n");  

  $_ = $ligne;    #initialisation
  do{
     my ($nomMotClef, $valeur) =();
     
    #__________________________________________________________________________
    # PREREQUIS: $_ doit contenir une chaine du type :
    # <nomVar1 = valV1 nomVar2=valV2 ... nomVARn = valVn >
    #
    #
    # => Separation en 2 parties (nom de la 1ere variable et le reste)
    #
    #
    # Exemples a prende en compte:
      # <MAXIMUM D'ITERATIONS POUR LE SOLVEUR : 100>
      # <PRODUIT MATRICE-VECTEUR : 1> avec le <->
   
    m/^\s*   # une expression
      (.*?)  # qui pourrait etre entouree par des blancs
      \s*
      [:=]   # delimité par ce 1er separateur trouve
     /x;
    #__________________________________________________________________________
    $nomMotClef= $1;
    $_=$'; # le reste de la ligne ...
    #print __LINE__."> <$nomMotClef> <$_>\n";
    s/^\s*//;
    my $isCas1=0;
    
    info::info(30,"Mot clef trouve <$nomMotClef> Reste de la ligne a traiter <$_> (".__FILE__." ".__LINE__.")");
    
    #__________________________________________________________________________
    # => Trouver la valeur affectee a ce mot clef
    #
    #if ( m/
    #      ^\s*        # Debut de ligne suivi d'espaces eventuellement
    #       (('.*?')+) # expression entre guillemets ''
    #       |          # ou
    #       (".*?")    # expression entre guillemets ""
    #     /x)
              
    # Cas 1 : valeur entre guillemets
    #  exemples <'toto'> ou <"toto"> ou <'toto d''ici'>
    #           ou <'m';'day'> (cas identifie le 29/10/01)
    while ( m|
            (['"])          # Qq chose qui commence par " ou '
            (               # dont on va prendre le contenu
             (.*?(''))?      #  qui peut contenir <''>
             .*?
            )*              #  cette suite se repetant 0 à n fois
            \1              # qui se termine par le " ou le ' de (1)
            ;?              # avec un pt virgule delimitant la valeur suivante eventuelle
          |x )
    { # expression reguliere inspiree de decomposer_valeur()
      $isCas1=1;
      
      my $recup = $&;
      $restant  = $';
      info::info(30,"\t Recup <$recup> Restant <$restant> (".__FILE__." ".__LINE__.")");
      
      if ( $recup =~ m|^(['"])\s*\1| )
      {
        # si la chaine recuperee n'est une chaine vide
        
        # pour ne pas rentrer dans la cas2
        #if ($valeur eq "") { $valeur=" ";}        
      }
      else{
        
        $recup =~ s/''/'/g;
        # enlever les blancs superflus
        $recup =~ s/^\s*//;
        $recup =~ s/\s*$//;
    
        $valeur = $valeur.$recup;
      }

      $_ = $restant;
    }

    # on rentre dans le cas2 ssi on a pas fait le cas1
    if (! $isCas1 )
    {
      # Cas 2 : pas de guillemets delimitant de valeur donc
      #         ca va etre plus compliqué ...
      # Exemples a prendre en compte : <????>
      #                                <0.05 / 1.2 h>
      #Hypotheses:.Un ensemble de 'blanc' ne peut etre precede que par un caractere 
      #            particulier dont la liste est <,;:-_/+%>.
      #           .Des blancs peuvent etre precedes d'un caractere autre que <,;:-_/+%>
      #            SI ET SEULEMENT SI le caractere qui suivant est <,;:-_/+%>.
      #           .L'expression doit commencer par un caractere(\w) ou un '?' 
      #            ou  '.' ou '-' (ex: <-10> ou <???> )
      if (m%
            ([?.\-]|\w)
            (
              (\s*[,;:\-_/+\%]\s*)?# listes de caracteres pouvant etre suivie ou 
                                   # precedee par un ensemble de blancs.
              \??                  # caracteres NE pouvant PAS etre suivi par un(ou des) espace(s)
              \w?
              \.?
            )*
         %x)
      {
        $valeur = $&;
        $restant = $';
      }
      else{
          # si on arrive la c'est soit qu'il ya une erreur dans le fichier cas
          # soit que l'on est en presence d'un cas non pris en compte ...
          info::erreur("Impossible de traiter la ligne <$ligne> (".__FILE__." line ".__LINE__." )");
          return;
      }
    }
    #enlever les blancs superflus
    $valeur =~ s/^\s*//;
    $valeur =~ s/\s*$//;
    $restant =~ s/^\s*//;
    $restant =~ s/\s*$//;
    #__________________________________________________________________________

   

    # ajouter ce mot clef et sa valeur a la liste
    $lstSortie{$nomMotClef} = $valeur;

    # initialisation pour le prochain passage dans la boucle
    $_ = $restant;
  }while( $restant );


  return %lstSortie;
}

###############################################################################
#Role  : Determiner si un mot clef existe dans une structure de type hash. Il
#        faut donner le nom du mot clef, le vrai nom de stockage (nom du mot
#        clef+suffixe) sera trouve par la fonction
#       un "=".
#Entree: 2
#        nom du mot clef (E)
#        reference vers une structure de type hash (E)
#Retour: 1 si il existe
#        0 sinon
###############################################################################
sub existe_mot_clef
{
  # Nb d'arguments passes
  my $nbArg = $#_;
  
  if ($nbArg!=2-1)
  {
    info::info(20,"Le nombre d'arguments passes a la fonction est incorrect <$nbArg> (".__FILE__." ".__LINE__.")");
    return 0;
  }
  my $motClef = @_[0];
  my $refHash = @_[1];
  
  my $numLang="0";
  
  # construire le nom complet de ce mot clef
  my $motClefComplet = construire_nom_de_stockage($motClef, $numLang);
  if ( exists($refHash->{$motClefComplet}) )
  {
    return 1;
  }
  
  # sinon
  $numLang="1";
  # construire le nom complet de ce mot clef
  $motClefComplet = construire_nom_de_stockage($motClef, $numLang);
  if ( exists($refHash->{$motClefComplet}) )
  {
    return 1;
  }

  return 0;
}

1;