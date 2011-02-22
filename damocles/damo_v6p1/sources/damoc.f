C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief    MAIN ROUTINE OF THE DAMOCLES LIBRARY
!>             CALLED BY THE DAMOCLES EXECUTABLE (DAMOCLE.F)
!>             CALLED BY THE LNH COMPUTATIONAL CODES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note      PORTABILITY : IBM,CRAY,HP,SUN

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ADRESS, DEFATT, DEFCAR, DEFINT, DEFLOG, DEFREA, DIMENS, DOC, GESTD, INDIC, LLNG, LLU, MOTATT, MOTCAR, MOTCLE, MOTINT, MOTLOG, MOTREA, NBLANG, NFICDA, NFICMO, NMAX, RETRY, SIZE, TROUVE, USRATT, USRCAR, USRINT, USRLOG, USRREA, UTINDX
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> DCINFO : LNG, LU<hr>
!> DCRARE : ERREUR, RETOUR<hr>
!> DCMLIG : NLIGN, LONGLI<hr>
!> DCCHIE : NFIC<hr>
!> DCNGE : INDX, NTYP, ITAI, LONGU, NMOT, DEFLU<hr>
!> DCNGEC : PARAM
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ADD, ADDES, ADSRC, AIDLNG, ARRET, DEPLAC, DYNAM, EXECMD, I, I2, ICOL, ILONG, ITYP, IVAL, J, JCOL, K, L1, LANGUE, LCAR, LIGNE, LONIGN, LONPRO, LUIGN, MOTIGN, MOTPRO, NBMOT, NIGN, NMAXR, NULCAR, NULINT, NULLOG, NULREA, NUMERO, NVAL, OFFSET, ORDRE, PARAM2, PTVIRG, QUOTE, TYPE, TYPE2, TYPIGN, VUCMD, VUCMD0, VUMOT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> AIDELU(), CARLU(), CLASSE(), CMD(), DICO(), INFLU(), INTLU(), LOGLU(), LONGLU(), NEXT(), PREV(), PREVAL(), REALU()
!>   </td></tr>
!>     <tr><th> Unknown(s)
!>    </th><td> DOCAIDLNG
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DAMOCLE(), DAMOCLES()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>  <tr>
!>    <td><center> 5.9                                    </center></td>
!>    <td> 10/11/2008                                              </td>
!>    <td> J-M HERVOUET (LNH); A. YESSAYAN; L. LEGUE               </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 14/12/1993                                              </td>
!>    <td> O. QUIQUEMPOIX (LNH) 30.87.78.70                        </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ADRESS
!></td><td><--</td><td>TABLEAU DES ADRESSES DES MOTS CLES
!>    </td></tr>
!>          <tr><td>DEFATT
!></td><td><--</td><td>TABLEAU DES SUBMITS PAR DEFAUT
!>    </td></tr>
!>          <tr><td>DEFCAR
!></td><td><--</td><td>TABLEAU DES VALEURS CARACTERES PAR DEFAUT
!>    </td></tr>
!>          <tr><td>DEFINT
!></td><td><--</td><td>TABLEAU DES VALEURS ENTIERES PAR DEFAUT
!>    </td></tr>
!>          <tr><td>DEFLOG
!></td><td><--</td><td>TABLEAU DES VALEURS LOGIQUES PAR DEFAUT
!>    </td></tr>
!>          <tr><td>DEFLU
!></td><td><-></td><td>NOMBRE DE VALEURS LUES POUR LE MOT CLE
!>    </td></tr>
!>          <tr><td>DEFREA
!></td><td><--</td><td>TABLEAU DES VALEURS REELLES PAR DEFAUT
!>    </td></tr>
!>          <tr><td>DIMENS
!></td><td><--</td><td>TABLEAU DES DIMENSIONS DES MOTS CLES
!>    </td></tr>
!>          <tr><td>DOC
!></td><td>--></td><td>LOGIQUE DE DOCUMENTATION DE LA SORTIE
!>                  = VRAI : IMPRIME L'AIDE (FICHIER RESULTAT)
!>                  = FAUX : N'IMPRIME PAS L'AIDE
!>    </td></tr>
!>          <tr><td>ERREUR
!></td><td><-></td><td>SORT AVEC LA VALEUR .TRUE. EN CAS D'ERREUR
!>    </td></tr>
!>          <tr><td>GESTD
!></td><td>--></td><td>LOGIQUE D'APPEL PAR LE GESTIONNAIRE D'ETUDES
!>    </td></tr>
!>          <tr><td>INDIC
!></td><td><--</td><td>TABLEAU D'INDICATEURS D'ETAT DES MOTS CLES
!>                  = 0 : PAS DE SUBMIT & NON TABLEAU
!>                  = 1 : PAS DE SUBMIT & TABLEAU
!>                  = 2 : AVEC   SUBMIT & NON TABLEAU
!>                  = 3 : AVEC   SUBMIT & TABLEAU
!>    </td></tr>
!>          <tr><td>INDX
!></td><td><-></td><td>INDEX DU MOT CLE EN COURS
!>    </td></tr>
!>          <tr><td>ITAI
!></td><td><-></td><td>TAILLE DU MOT CLE EN COURS
!>    </td></tr>
!>          <tr><td>LLNG
!></td><td>--></td><td>NUMERO DE LA LANGUE DE DECODAGE
!>    </td></tr>
!>          <tr><td>LLU
!></td><td>--></td><td>NUMERO DE L'UNITE LOGIQUE DES SORTIES
!>    </td></tr>
!>          <tr><td>LNG
!></td><td>--></td><td>NUMERO DE LA LANGUE DE DECODAGE
!>    </td></tr>
!>          <tr><td>LONGLI
!></td><td>--></td><td>LONGUEUR DES LIGNES
!>    </td></tr>
!>          <tr><td>LONGU
!></td><td><-></td><td>LONGUEUR DU MOT CLE EN COURS
!>    </td></tr>
!>          <tr><td>LU
!></td><td>--></td><td>NUMERO DE L'UNITE LOGIQUE DES SORTIES
!>    </td></tr>
!>          <tr><td>MOTATT
!></td><td><--</td><td>TABLEAU DES SUBMITS
!>    </td></tr>
!>          <tr><td>MOTCAR
!></td><td><--</td><td>TABLEAU DES VALEURS CARACTERES
!>    </td></tr>
!>          <tr><td>MOTCLE
!></td><td><--</td><td>TABLEAU DES MOTS CLES ACTIFS
!>    </td></tr>
!>          <tr><td>MOTINT
!></td><td><--</td><td>TABLEAU DES VALEURS ENTIERES
!>    </td></tr>
!>          <tr><td>MOTLOG
!></td><td><--</td><td>TABLEAU DES VALEURS LOGIQUES
!>    </td></tr>
!>          <tr><td>MOTREA
!></td><td><--</td><td>TABLEAU DES VALEURS REELLES
!>    </td></tr>
!>          <tr><td>NBLANG
!></td><td>--></td><td>NOMBRE DE LANGUES CONNUES
!>    </td></tr>
!>          <tr><td>NFIC
!></td><td>--></td><td>NUMERO DE CANAL DU FICHIER EN COURS DE LECT.
!>    </td></tr>
!>          <tr><td>NFICDA
!></td><td>--></td><td>NUMERO DE CANAL DU FICHIER DES DONNEES
!>    </td></tr>
!>          <tr><td>NFICMO
!></td><td>--></td><td>NUMERO DE CANAL DU FICHIER DES MOTS-CLES
!>    </td></tr>
!>          <tr><td>NLIGN
!></td><td><-></td><td>NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU
!>    </td></tr>
!>          <tr><td>NMAX
!></td><td>--></td><td>TAILLE MAXIMALE AUTORISEE POUR LES TABLEAUX
!>    </td></tr>
!>          <tr><td>NMOT
!></td><td><-></td><td>TABLEAU DU NOMBRE DE MOTS CLES PAR TYPE
!>    </td></tr>
!>          <tr><td>NTYP
!></td><td><-></td><td>TYPE DU MOT CLE EN COURS
!>    </td></tr>
!>          <tr><td>PARAM
!></td><td><-></td><td>NOM DU MOT CLE EN COURS
!>    </td></tr>
!>          <tr><td>RETOUR
!></td><td><-></td><td>SORT AVEC LA VALEUR .TRUE. EN CAS DE FIN DE
!>                  FIN DE FICHIER OU D'ERREUR DE LECTURE.
!>    </td></tr>
!>          <tr><td>RETRY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SIZE
!></td><td><--</td><td>TABLEAU DES LONGUEURS DES MOTS CLES
!>    </td></tr>
!>          <tr><td>TROUVE
!></td><td><--</td><td>INDICATEUR D'ETAT DES MOTS CLES
!>                  = 0 : AUCUNE VALEUR TROUVEE
!>                  = 1 : VALEUR PAR DEFAUT TROUVEE
!>                  = 2 : VALEUR TROUVEE (FICHIER DE DONNEES)
!>                  = 3 : AUCUNE VALEUR TROUVEE (OPTIONNELLE)
!>                  = 5 : TABLEAU DE MOTS A SUBMIT COMPACTE
!>                  = 6 : MOT CLE A SUBMIT FORCE NON AFFECTE
!>                  = 7 : MOT CLE A SUBMIT FORCE AFFECTE (DICO)
!>                  = 8 : MOT CLE A SUBMIT FORCE AFFECTE (CAS)
!>                  = 9 : FICHIER DICO : SUBMIT + VALEUR LANCEUR
!>                  =10 : FICHIER CAS  : SUBMIT + VALEUR LANCEUR
!>    </td></tr>
!>          <tr><td>USRATT
!></td><td><--</td><td>TABLEAU DES SUBMITS A USAGE LOCAL
!>    </td></tr>
!>          <tr><td>USRCAR
!></td><td><--</td><td>TABLEAU DES VALEURS CARACTERES A USAGE LOCAL
!>    </td></tr>
!>          <tr><td>USRINT
!></td><td><--</td><td>TABLEAU DES VALEURS ENTIERES A USAGE LOCAL
!>    </td></tr>
!>          <tr><td>USRLOG
!></td><td><--</td><td>TABLEAU DES VALEURS LOGIQUES A USAGE LOCAL
!>    </td></tr>
!>          <tr><td>USRREA
!></td><td><--</td><td>TABLEAU DES VALEURS REELLES A USAGE LOCAL
!>    </td></tr>
!>          <tr><td>UTINDX
!></td><td><--</td><td>TABLEAU DE LOGIQUES D'UTILISATION DES INDEX
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DAMOC
     &( ADRESS , DIMENS , NMAX   , DOC    , LLNG   , LLU    ,
     &  MOTINT , MOTREA , MOTLOG , MOTCAR , MOTATT ,
     &  DEFINT , DEFREA , DEFLOG , DEFCAR , DEFATT ,
     &  USRINT , USRREA , USRLOG , USRCAR , USRATT ,
     &  MOTCLE , SIZE   , TROUVE , UTINDX , NFICMO , NFICDA ,
     &  INDIC  , GESTD  , NBLANG , RETRY )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ADRESS         |<--| TABLEAU DES ADRESSES DES MOTS CLES
C| DEFATT         |<--| TABLEAU DES SUBMITS PAR DEFAUT
C| DEFCAR         |<--| TABLEAU DES VALEURS CARACTERES PAR DEFAUT
C| DEFINT         |<--| TABLEAU DES VALEURS ENTIERES PAR DEFAUT
C| DEFLOG         |<--| TABLEAU DES VALEURS LOGIQUES PAR DEFAUT
C| DEFLU          |<->| NOMBRE DE VALEURS LUES POUR LE MOT CLE
C| DEFREA         |<--| TABLEAU DES VALEURS REELLES PAR DEFAUT
C| DIMENS         |<--| TABLEAU DES DIMENSIONS DES MOTS CLES
C| DOC            |-->| LOGIQUE DE DOCUMENTATION DE LA SORTIE
C|                |   | = VRAI : IMPRIME L'AIDE (FICHIER RESULTAT)
C|                |   | = FAUX : N'IMPRIME PAS L'AIDE
C| ERREUR         |<->| SORT AVEC LA VALEUR .TRUE. EN CAS D'ERREUR
C| GESTD          |-->| LOGIQUE D'APPEL PAR LE GESTIONNAIRE D'ETUDES
C| INDIC          |<--| TABLEAU D'INDICATEURS D'ETAT DES MOTS CLES
C|                |   | = 0 : PAS DE SUBMIT & NON TABLEAU
C|                |   | = 1 : PAS DE SUBMIT & TABLEAU
C|                |   | = 2 : AVEC   SUBMIT & NON TABLEAU
C|                |   | = 3 : AVEC   SUBMIT & TABLEAU
C| INDX           |<->| INDEX DU MOT CLE EN COURS
C| ITAI           |<->| TAILLE DU MOT CLE EN COURS
C| LLNG           |-->| NUMERO DE LA LANGUE DE DECODAGE
C| LLU            |-->| NUMERO DE L'UNITE LOGIQUE DES SORTIES
C| LNG            |-->| NUMERO DE LA LANGUE DE DECODAGE
C| LONGLI         |-->| LONGUEUR DES LIGNES
C| LONGU          |<->| LONGUEUR DU MOT CLE EN COURS
C| LU             |-->| NUMERO DE L'UNITE LOGIQUE DES SORTIES
C| MOTATT         |<--| TABLEAU DES SUBMITS
C| MOTCAR         |<--| TABLEAU DES VALEURS CARACTERES
C| MOTCLE         |<--| TABLEAU DES MOTS CLES ACTIFS
C| MOTINT         |<--| TABLEAU DES VALEURS ENTIERES
C| MOTLOG         |<--| TABLEAU DES VALEURS LOGIQUES
C| MOTREA         |<--| TABLEAU DES VALEURS REELLES
C| NBLANG         |-->| NOMBRE DE LANGUES CONNUES
C| NFIC           |-->| NUMERO DE CANAL DU FICHIER EN COURS DE LECT.
C| NFICDA         |-->| NUMERO DE CANAL DU FICHIER DES DONNEES
C| NFICMO         |-->| NUMERO DE CANAL DU FICHIER DES MOTS-CLES
C| NLIGN          |<->| NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU
C| NMAX           |-->| TAILLE MAXIMALE AUTORISEE POUR LES TABLEAUX
C| NMOT           |<->| TABLEAU DU NOMBRE DE MOTS CLES PAR TYPE
C| NTYP           |<->| TYPE DU MOT CLE EN COURS
C| PARAM          |<->| NOM DU MOT CLE EN COURS
C| RETOUR         |<->| SORT AVEC LA VALEUR .TRUE. EN CAS DE FIN DE
C|                |   | FIN DE FICHIER OU D'ERREUR DE LECTURE.
C| RETRY          |---| 
C| SIZE           |<--| TABLEAU DES LONGUEURS DES MOTS CLES
C| TROUVE         |<--| INDICATEUR D'ETAT DES MOTS CLES
C|                |   | = 0 : AUCUNE VALEUR TROUVEE
C|                |   | = 1 : VALEUR PAR DEFAUT TROUVEE
C|                |   | = 2 : VALEUR TROUVEE (FICHIER DE DONNEES)
C|                |   | = 3 : AUCUNE VALEUR TROUVEE (OPTIONNELLE)
C|                |   | = 5 : TABLEAU DE MOTS A SUBMIT COMPACTE
C|                |   | = 6 : MOT CLE A SUBMIT FORCE NON AFFECTE
C|                |   | = 7 : MOT CLE A SUBMIT FORCE AFFECTE (DICO)
C|                |   | = 8 : MOT CLE A SUBMIT FORCE AFFECTE (CAS)
C|                |   | = 9 : FICHIER DICO : SUBMIT + VALEUR LANCEUR
C|                |   | =10 : FICHIER CAS  : SUBMIT + VALEUR LANCEUR
C| USRATT         |<--| TABLEAU DES SUBMITS A USAGE LOCAL
C| USRCAR         |<--| TABLEAU DES VALEURS CARACTERES A USAGE LOCAL
C| USRINT         |<--| TABLEAU DES VALEURS ENTIERES A USAGE LOCAL
C| USRLOG         |<--| TABLEAU DES VALEURS LOGIQUES A USAGE LOCAL
C| USRREA         |<--| TABLEAU DES VALEURS REELLES A USAGE LOCAL
C| UTINDX         |<--| TABLEAU DE LOGIQUES D'UTILISATION DES INDEX
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER          NMAX,LLNG,LLU,NFICMO,NFICDA,NBLANG,RETRY
      INTEGER          MOTINT(*),DEFINT(*),USRINT(*)
      INTEGER          SIZE(4,*),ADRESS(4,*),DIMENS(4,*)
      INTEGER          INDIC(4,*),TROUVE(4,*)
      LOGICAL          MOTLOG(*),DEFLOG(*),USRLOG(*),UTINDX(4,*),DOC
      CHARACTER*72     MOTCLE(4,*)
      CHARACTER*144    MOTATT(4,*),DEFATT(*),USRATT(*)
      CHARACTER*144    MOTCAR(*),DEFCAR(*),USRCAR(*)
      DOUBLE PRECISION MOTREA(*),DEFREA(*),USRREA(*)
C
      INTEGER            INTLU,NEXT,PREV,PREVAL,LONGLU
      LOGICAL            LOGLU
      CHARACTER(LEN=144) CARLU,PARAM2
      DOUBLE PRECISION   REALU
C
      INTEGER          LNG,LU
      INTEGER          INDX,NTYP,ITAI,LONGU,NMOT(4),DEFLU
      INTEGER          NLIGN,LONGLI
      INTEGER          NFIC
      LOGICAL          ERREUR , RETOUR
      CHARACTER(LEN=72)     PARAM
C
C-----------------------------------------------------------------------
C
      INTEGER          I,K,IVAL,LCAR,ICOL,JCOL,ILONG,ITYP,NUMERO,I2
      INTEGER          DEPLAC,ADD,J,OFFSET(4),NBMOT
      INTEGER          TYPIGN(100),LONIGN(100),NMAXR(4),ORDRE
      INTEGER          ADSRC,ADDES,NULINT,NVAL,NIGN,L1,LONPRO(15)
      LOGICAL          DYNAM,LANGUE,NULLOG,LUIGN,AIDLNG,VUMOT
      LOGICAL          ARRET,VUCMD(5),VUCMD0(5),EXECMD,GESTD
      CHARACTER*1      PTVIRG,QUOTE
      CHARACTER*9      MOTPRO(15),TYPE
      CHARACTER*72     MOTIGN(100),LIGNE
      CHARACTER*144    NULCAR,TYPE2
      DOUBLE PRECISION NULREA
C
C-----------------------------------------------------------------------
C
      COMMON / DCINFO / LNG,LU
      COMMON / DCRARE / ERREUR , RETOUR
      COMMON / DCMLIG / NLIGN , LONGLI
      COMMON / DCCHIE / NFIC
      COMMON / DCNGE  / INDX,NTYP,ITAI,LONGU,NMOT,DEFLU
      COMMON / DCNGEC / PARAM
C
      EXTERNAL CARLU,INTLU,LOGLU,REALU,NEXT,PREV,PREVAL,LONGLU
C
C-----------------------------------------------------------------------
C
      DATA MOTPRO /'NOM','TYPE','INDEX','TAILLE','DEFAUT','AIDE',
     & 'CHOIX','RUBRIQUE','NIVEAU','MNEMO','COMPOSE','COMPORT',
     & 'CONTROLE','APPARENCE','SUBMIT'/
C     LENGTH OF THE PROTECTED WORDS
      DATA LONPRO /3,4,5,6,6,4,5,8,6,5,7,7,8,9,6/
C
C***********************************************************************
C                                    RCS AND SCCS MARKING
C
C***********************************************************************
C
C     TYPE NUMBERING :    1 : INTEGER
C                         2 : REAL
C                         3 : LOGICAL
C                         4 : CHARACTER
C
C     MOTPRO(I)   : IEME MOT RESERVE POUR LE PROGRAMME (NOM,TYPE,...)
C     MOTCLE(I,J) : NOM DU JIEME MOT-CLE DE TYPE I
C     DIMENS(I,J) : DIMENSION DU JIEME MOT-CLE DE TYPE I
C     ADRESS(I,J) : ADRESSE DU JIEME MOT-CLE DE TYPE I DANS LE TABLEAU
C     MOTINT(I)   : IEME RESULTAT ENTIER
C     MOTREA(I)   : IEME RESULTAT REEL
C     MOTLOG(I)   : IEME RESULTAT LOGIQUE
C     MOTCAR(I)   : IEME RESULTAT CARACTERE
C     MOTATT(I,J) : JEME SUBMIT DU TYPE I
C     SIZE(I,J)   : LONGUEUR DU JIEME MOT-CLE DE TYPE I
C     TROUVE(I,J) : CONCERNE LE JIEME MOT-CLE DE TYPE I
C     INDIC(I,J)  : CONCERNE LE JIEME MOT-CLE DE TYPE I
C     LUIGN       : INDIQUE SI C'EST UN MOT POUR EDAMOX SEULEMENT
C     MOTIGN(I)   : IEME MOT LU DANS LE FICHIER CAS DONNE PAR EDAMOX
C                   ET LU COMME IGNORE DANS LE DICTIONNAIRE
C     DYNAM       : LOGIQUE POUR LE DYNAMIQUE (.TRUE. SI MODE DYNAMIQUE)
C     VUCMD(I)    : TABLEAU DE LOGIQUES (MEMORISATION DES COMMANDES)
C                   I=1->&LIS;I=2->&ETA;I=3->&IND;I=4->&STO;I=5->&FIN
C     EXECMD      : LOGIQUE D'ACTIVATION DES COMMANDES MEMORISEES
C     NMAXR(I)    : INDEX MAXIMUM REELLEMENT UTILISE POUR LE TYPE I
C
C-----------------------------------------------------------------------
C
C INITIALISES :
C
      LU      = LLU
      LNG     = LLNG
      ARRET   = .FALSE.
      ERREUR  = .FALSE.
      RETOUR  = .FALSE.
      DYNAM   = .FALSE.
      EXECMD  = .FALSE.
      AIDLNG  = .FALSE.
      VUMOT   = .FALSE.
      LONGLI  = 72
      NFIC    = NFICMO
      PTVIRG  = ';'
      QUOTE   = ''''
C     TABUL   = CHAR(9)
      NBMOT   = 0
      NIGN    = 0
      ORDRE   = 0
      PARAM  = ' '
      LONGU  = 0
      NTYP   = -100
      INDX   =  123456
      ITAI   = -100
      DEFLU  = 0
C
      DO 2 K=1, 5
       VUCMD(K) = .FALSE.
       VUCMD0(K) = .FALSE.
2     CONTINUE
C
      DO 3 K=1,100
       MOTIGN(K)= ' '
       TYPIGN(K)=1
       LONIGN(K)=0
3     CONTINUE
C
      DO 5 K=1, 4
       NMOT(K) = 0
       NMAXR(K) = 0
       OFFSET(K) = 1
       DO 6 I=1,NMAX
         ADRESS(K,I)  = 0
         DIMENS(K,I)  = 1
         TROUVE(K,I)  = 0
         SIZE(K,I)    = 0
         UTINDX(K,I)  = .FALSE.
         MOTINT(I)    = 0
         MOTREA(I)    = 0.
         MOTLOG(I)    = .FALSE.
         MOTCAR(I)    = ' '
         MOTATT(K,I)  = ' '
         DEFINT(I)    = 0
         DEFREA(I)    = 0.
         DEFLOG(I)    = .FALSE.
         DEFCAR(I)    = ' '
         DEFATT(I)    = ' '
         USRINT(I)    = 0
         USRREA(I)    = 0.
         USRLOG(I)    = .FALSE.
         USRCAR(I)    = ' '
         USRATT(I)    = ' '
         MOTCLE(K,I)  = ' '
         INDIC(K,I)   = 0
6      CONTINUE
5      CONTINUE
C
C CHECKS THE LANGUAGE
C
      IF(LNG.LT.1.OR.LNG.GT.NBLANG) THEN
        WRITE(LU,*) ' '
        WRITE(LU,*) ' CHOIX DE LA LANGUE = ',LNG,' INVALIDE.'
        WRITE(LU,*) ' ARRET DE DAMOCLES'
        WRITE(LU,*) ' '
        STOP 'ERREUR DAMOCLES 1'
      ENDIF
C
C 99 : NEW FILE   100 : NEW KEYWORD
C
  99  CONTINUE
C
      ICOL   = LONGLI
      NLIGN = 0
C
C SEEKS THE FIRST NON-WHITE CHARACTER (IGNORES COMMENTED LINES) :
C
      ICOL = NEXT(ICOL+1,LIGNE)
C
100   CONTINUE
C
C IF REACHED THE END OF FILE :
C
      IF(RETOUR) GO TO 900
C
C LOCATES THE COMMANDS STARTING WITH &
C
      IF ( LIGNE(ICOL:ICOL).EQ.'&' ) THEN
           CALL CMD (ICOL,LIGNE,ADRESS,DIMENS,TROUVE,MOTCLE,NMOT,
     &          MOTINT,MOTREA,MOTLOG,MOTCAR,MOTATT,INDIC,SIZE,
     &          UTINDX,DYNAM,VUCMD,EXECMD,NFICDA,NMAXR)
C
C     IF FOUND &FIN, ENDS AFTER COMPACTING :
           IF (VUCMD(5)) GO TO 900
C     IF FOUND &STO, ENDS FILE READING :
           IF (VUCMD(4)) GO TO 1000
C
           ICOL = NEXT(ICOL+1,LIGNE)
           IF(RETOUR) GO TO 900
      ELSE
C
              I2 = PREVAL(ICOL+1,LIGNE,'=',':','=')
C             CASE WHERE '=' IS ON THE FOLLOWING LINE
              IF(I2.GT.LONGLI) I2=LONGLI
              JCOL = PREV  (I2,LIGNE)
              ILONG = JCOL - ICOL + 1
C
              LUIGN = .FALSE.
              IF (NFIC.EQ.NFICMO.AND.INDX.LE.0) LUIGN = .TRUE.
C
              CALL DICO(ITYP,NUMERO,ILONG,LIGNE(ICOL:JCOL),
     &             MOTCLE,NMOT,MOTPRO,LONPRO,SIZE,UTINDX,LANGUE,
     &             AIDLNG,MOTIGN,NIGN,LUIGN,TYPIGN,LONIGN,NFICDA,
     &             NBLANG,NMAXR)
C
              IF(ERREUR) THEN
                             WRITE(LU,*)
                             WRITE(LU,*)'************************'
                IF(LNG.EQ.1) WRITE(LU,*)'* ARRET DE DAMOCLES    *'
                IF(LNG.EQ.2) WRITE(LU,*)'* DAMOCLES STOPPED     *'
                             WRITE(LU,*)'************************'
                GO TO 900
              ENDIF
C
C STOPS IF THE WORD IS UNKNOWN
              IF(ITYP.EQ.0) THEN
                ARRET=.TRUE.
                GOTO 1300
              ENDIF
C
              ICOL = PREVAL(ICOL+1,LIGNE,'=',':','=')
C             CASE WHERE '=' IS ON THE FOLLOWING LINE
              IF(ICOL.GT.LONGLI) THEN
                ICOL  = NEXT(LONGLI,LIGNE)
                IF(RETOUR) GO TO 900
              ENDIF
C
C 1) READS AND ASSIGNS A VALUE:
C
           IF(ITYP.LE.4) THEN
C
C A PRIORI THE NUMBER OF VALUES TO READ IS DIMENS(ITYP,NUMERO)
C BUT MISSING OR ADDITIONAL VALUES ARE TOLERATED
C
C WHEN THIS IS IDENTIFIED THE VARIOUS ARRAYS ARE UPDATED
C
C
           IF (.NOT.(LUIGN)) THEN
            NTYP = ITYP
            INDX = NUMERO
            ITAI = DIMENS(NTYP,INDX)
            ADD  = ADRESS(NTYP,INDX)
C           PARAM = MOTCLE(NTYP,INDX)
C           LONGU = LONGLU(PARAM)
            IVAL = 1
            IF (TROUVE(NTYP,INDX).EQ.2.OR.TROUVE(NTYP,INDX).EQ.8) THEN
             WRITE(LU,*) ' '
             IF(LNG.EQ.1) THEN
               WRITE(LU,*) 'LE MOT CLE : ',MOTCLE(NTYP,INDX)(1:ILONG)
               WRITE(LU,*) 'EST CITE AU MOINS 2 FOIS, SEULE LA',
     &                     ' DERNIERE VALEUR EST CONSERVEE...'
             ELSEIF(LNG.EQ.2) THEN
               WRITE(LU,*) 'THE KEY-WORD: ',MOTCLE(NTYP,INDX)(1:ILONG)
               WRITE(LU,*) 'APPPEARS AT LEAST TWICE , THE LAST',
     &                     ' VALUE WILL BE KEPT...'
             ENDIF
             WRITE(LU,*) ' '
            ENDIF
           ENDIF
 10        CONTINUE
           IF (.NOT.(LUIGN)) THEN
             IF     (NTYP.EQ.1) THEN
                     DEFINT(IVAL) = INTLU(ICOL,LIGNE)
             ELSEIF (NTYP.EQ.2) THEN
                     DEFREA(IVAL) = REALU(ICOL,LIGNE)
             ELSEIF (NTYP.EQ.3) THEN
                     DEFLOG(IVAL) = LOGLU(ICOL,LIGNE)
             ELSEIF (NTYP.EQ.4) THEN
                     DEFCAR(IVAL) = CARLU(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,
     &                                    SIZE,MOTIGN,LONIGN,NMAXR,
     &                                    NFICDA,LEN(DEFCAR(IVAL)))
             ENDIF
C
C            JMH CORRECTION 01/03/05
C            SUBMIT FOR A CHARACTER ARRAY
C            THERE IS ONLY ONE; WHEREAS IT WAS PREVIOUSLY ASSUMED
C            THAT THERE WERE AS MANY AS CHARACTER STRINGS ?
C            DEFATT(IVAL) = MOTATT(NTYP,ADD+IVAL-1)
             DEFATT(IVAL) = MOTATT(NTYP,ADD)
C
C CASE OF THE OPTIONAL EMPTY SUBMIT: REMAINS OPTIONAL
             IF (ITAI.LE.1.AND.INDIC(NTYP,INDX).GE.2.AND.
     &           TROUVE(NTYP,INDX).EQ.3) THEN
                 L1 = LONGLU(DEFCAR(IVAL))
                 IF (L1.GT.0) TROUVE(NTYP,INDX)=2
C
             ELSEIF(TROUVE(NTYP,INDX).LT.6) THEN
                    TROUVE(NTYP,INDX)=2
C
             ELSEIF (TROUVE(NTYP,INDX).EQ.6.OR.
     &              TROUVE(NTYP,INDX).EQ.7) THEN
                    TROUVE(NTYP,INDX)=8
             ENDIF
C
            ELSE
             NTYP = ITYP
             IF     (NTYP .EQ. 1) THEN
                     NULINT = INTLU(ICOL,LIGNE)
             ELSEIF (NTYP .EQ. 2) THEN
                     NULREA = REALU(ICOL,LIGNE)
             ELSEIF (NTYP .EQ. 3) THEN
                     NULLOG = LOGLU(ICOL,LIGNE)
             ELSEIF (NTYP .EQ. 4) THEN
                     NULCAR = CARLU(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,
     &                              SIZE,MOTIGN,LONIGN,NMAXR,NFICDA,
     &                              LEN(NULCAR))
             ENDIF
           ENDIF
C
           ICOL = NEXT(ICOL+1,LIGNE)
           IF(ICOL.LE.LONGLI) THEN
             IF(LIGNE(ICOL:ICOL).EQ.PTVIRG) THEN
               IVAL = IVAL + 1
               GO TO 10
             ENDIF
           ENDIF
C
           IF (LUIGN) GO TO 100
C
C ALL THE VALUES FOR A KEYWORD HAVE BEEN READ
C
C PARTICULAR CASE: KEYWORDS WITH A SUBMIT
C PREVENTS DYNAMIC ALLOCATION WHEN VALUES ARE GREATER THAN SIZE (SEE SUBMIT)
C OR FOR KEYWORDS NOT ASSOCIATED WITH ARRAYS
C
           IF (INDIC(NTYP,INDX).NE.1.AND.IVAL.GT.ITAI) IVAL = ITAI
C
C
C IF THE KEYWORD &DYN IS NOT IN THE STEERING FILE AND IF THERE ARE
C MORE VALUES THAN THE PARAMETER TAILLE THEN TRUNCATES TO ITAI;
C ELSE (&DYN IN THE STEERING FILE) READS ALL THE VALUES OF THE STEERING FILE
C
C IS IT REALLY NECESSARY TO SET BACK TO DYNAMIC MODE EVERY TIME ???
C IS MODIFYING DIMENS NOT ENOUGH ?
C IF LECDON IS WELL WRITTEN : IT DOES NOT MATTER IF THERE ARE HOLES
C IN THE ARRAYS SENT ... THINK ABOUT THIS
C
C
           IF (.NOT.(DYNAM)) THEN
             DO 400 I=1 , MIN(IVAL,ITAI)
               IF     (NTYP.EQ.1) THEN
                      MOTINT(ADD+I-1) = DEFINT(I)
               ELSEIF (NTYP.EQ.2) THEN
                      MOTREA(ADD+I-1) = DEFREA(I)
               ELSEIF (NTYP.EQ.3)THEN
                       MOTLOG(ADD+I-1) = DEFLOG(I)
               ELSEIF (NTYP.EQ.4)THEN
                       MOTCAR(ADD+I-1) = DEFCAR(I)
               ENDIF
 400         CONTINUE
           ELSE
              DO 410 I=1 ,NMAXR(NTYP)
                IF (UTINDX(NTYP,I)) THEN
                  IF(ADRESS(NTYP,I) .NE. ADD) THEN
                    IF (ADRESS(NTYP,I) .LT. ADD) DEPLAC = 0
                    IF (ADRESS(NTYP,I) .GT. ADD) DEPLAC = IVAL - ITAI
                    DO 420 J=1 , DIMENS(NTYP,I)
                      ADSRC = ADRESS(NTYP,I)+J-1
                      ADDES = ADRESS(NTYP,I)+J-1+DEPLAC
                      IF (ADDES.GT. NMAX) GO TO 1515
                      IF     (NTYP.EQ.1) THEN
                              USRINT(ADDES) = MOTINT(ADSRC)
                      ELSEIF (NTYP.EQ.2) THEN
                              USRREA(ADDES) = MOTREA(ADSRC)
                      ELSEIF (NTYP.EQ.3) THEN
                              USRLOG(ADDES) = MOTLOG(ADSRC)
                      ELSEIF (NTYP.EQ.4) THEN
                              USRCAR(ADDES) = MOTCAR(ADSRC)
                      ENDIF
                      USRATT(ADDES) = MOTATT(NTYP,ADSRC)
 420                CONTINUE
                    IF (ADRESS(NTYP,I) .GT. ADD) THEN
                      ADRESS(NTYP,I) = ADRESS(NTYP,I) + DEPLAC
                      IF (ADRESS(NTYP,I) .GT. NMAX) GO TO 1515
                    ENDIF
C
                  ELSE IF (ADRESS(NTYP,I) .EQ. ADD) THEN
                    DO 430 J=1 ,IVAL
                      IF     (NTYP.EQ.1) THEN
                             USRINT(ADD+J-1) = DEFINT(J)
                      ELSEIF (NTYP.EQ.2) THEN
                             USRREA(ADD+J-1) = DEFREA(J)
                      ELSEIF (NTYP.EQ.3) THEN
                             USRLOG(ADD+J-1) = DEFLOG(J)
                      ELSEIF (NTYP.EQ.4)THEN
                             USRCAR(ADD+J-1) = DEFCAR(J)
                      ENDIF
                      USRATT(ADD+J-1) = DEFATT(J)
 430                CONTINUE
                    DIMENS(NTYP,I) = IVAL
                  ENDIF
               ENDIF
 410         CONTINUE
C SORTS IN FINAL ARRAYS
             DO 440 I=1 ,NMAXR(NTYP)
               IF (UTINDX(NTYP,I)) THEN
                 ADSRC = ADRESS(NTYP,I)
                 DO 450 J=1 ,DIMENS(NTYP,I)
                  IF     (NTYP.EQ.1) THEN
                         MOTINT(ADSRC+J-1)=USRINT(ADSRC+J-1)
                  ELSEIF (NTYP.EQ.2) THEN
                         MOTREA(ADSRC+J-1)=USRREA(ADSRC+J-1)
                  ELSEIF (NTYP.EQ.3) THEN
                         MOTLOG(ADSRC+J-1)=USRLOG(ADSRC+J-1)
                  ELSEIF (NTYP.EQ.4)  THEN
                         MOTCAR(ADSRC+J-1)=USRCAR(ADSRC+J-1)
                  ENDIF
                  MOTATT(NTYP,ADSRC+J-1) = USRATT(ADSRC+J-1)
 450             CONTINUE
               ENDIF
 440         CONTINUE
           ENDIF
C
C
C          ICOL = NEXT(ICOL,LIGNE)
C
C ENDIF DU IF(ITYP.LE.4) ...
           ENDIF
C
C
C 2) RESERVED KEYWORDS:
C
C    RESERVED KEYWORDS CURRENTLY ARE:
C
C           'NOM'       :NUMERO = 1  (DE TYPE CARACTERE)
C           'TYPE'      :NUMERO = 2  (DE TYPE CARACTERE)
C           'INDEX'     :NUMERO = 3  (DE TYPE ENTIER)
C           'TAILLE'    :NUMERO = 4  (DE TYPE ENTIER)
C           'DEFAUT'    :NUMERO = 5  (DE TYPE VARIABLE)
C           'AIDE'      :NUMERO = 6  (DE TYPE CARACTERE)
C           'CHOIX'     :NUMERO = 7  (DE TYPE VARIABLE)
C           'RUBRIQUE'  :NUMERO = 8  (DE TYPE CARACTERE)
C           'NIVEAU'    :NUMERO = 9  (DE TYPE ENTIER)
C           'MNEMO'     :NUMERO = 10 (DE TYPE CARACTERE)
C           'COMPOSE'   :NUMERO = 11 (DE TYPE CARACTERE)
C           'COMPORT'   :NUMERO = 12 (DE TYPE CARACTERE)
C           'CONTROLE'  :NUMERO = 13 (DE TYPE ENTIER)
C           'APPARENCE' :NUMERO = 14 (DE TYPE CARACTERE)
C           'SUBMIT'    :NUMERO = 15 (DE TYPE CARACTERE)
C
       IF(ITYP.EQ.5) THEN
C
C    NAME
C
          IF(NUMERO.EQ.1) THEN
C
C SHOULD NOT COUNT THE SAME WORD IN SEVERAL LANGUAGES SEVERAL TIMES
C COUNTED ONLY ONCE IN FIRST FOUND LANGUAGE
C
             IF (.NOT.(VUMOT)) NBMOT = NBMOT + 1
C
             ORDRE = 1
C
C COMING FROM THE PRECEDING WORD, SORTS IT BEFORE READING THE FOLLOWING
C SINCE ALL THE INFORMATION ON THE PRECEDING WORD IS AVAILABLE
C
             IF (NBMOT.GT.1 .AND. (.NOT.(VUMOT)) ) THEN
                 IF (INDX.GT.NMAXR(NTYP)) NMAXR(NTYP)=INDX
                 CALL CLASSE(DIMENS,SIZE,MOTCLE,UTINDX,NMAX,
     &                       OFFSET,ADRESS,INDIC,LUIGN,
     &                       MOTINT,MOTREA,MOTLOG,MOTCAR,MOTATT ,
     &                       DEFCAR,DEFINT,DEFLOG,DEFREA,DEFATT )
             ENDIF
C
C SIGNALS THAT THIS NEW KEYWORD WAS ALREADY ENCOUNTERED IN ANOTHER LANGUAGE
             IF (.NOT.(VUMOT)) VUMOT=.TRUE.
C
C            NAME OF THE KEYWORD
             IF (LANGUE) THEN
               PARAM2= CARLU(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,SIZE,MOTIGN,
     &                       LONIGN,NMAXR,NFICDA,LEN(PARAM))
               LONGU = LCAR
               PARAM=PARAM2(1:MIN(72,LONGU))
             ELSE
C READS THE NAME OF A NON-REQUESTED LANGUAGE (NOT USED)
               NULCAR = CARLU(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,SIZE,MOTIGN,
     &                        LONIGN,NMAXR,NFICDA,LEN(NULCAR))
             ENDIF
C
             ICOL = NEXT(ICOL+1,LIGNE)
C
C    TYPE
C
          ELSE IF(NUMERO.EQ.2) THEN
                  VUMOT = .FALSE.
                  IF (ORDRE.NE.1) GOTO 1500
                  ORDRE=2
                  TYPE2= CARLU(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,SIZE,MOTIGN,
     &                         LONIGN,NMAXR,NFICDA,LEN(TYPE))
                  TYPE=TYPE2(1:MIN(LCAR,9))
                  IF(TYPE(1:6).EQ.'ENTIER'
     &           .OR.TYPE(1:8).EQ.'INTEGER') THEN
                    NTYP = 1
                  ELSEIF(TYPE(1:4).EQ.'REEL'
     &           .OR.TYPE(1:4).EQ.'REAL') THEN
                    NTYP = 2
                  ELSEIF(TYPE(1:7).EQ.'LOGIQUE'
     &           .OR.TYPE(1:7).EQ.'LOGICAL') THEN
                    NTYP = 3
                  ELSEIF(TYPE(1:9).EQ.'CARACTERE'
     &           .OR.TYPE(1:6).EQ.'STRING') THEN
C    *           .OR.TYPE(1:4).EQ.'FILE'
C    *           .OR.TYPE(1:7).EQ.'FICHIER') THEN
                    NTYP = 4
                  ELSE
C                 ERROR: UNKNOWN TYPE
                  IF(LNG.EQ.1) WRITE (LU,1002) LIGNE
                  IF(LNG.EQ.2) WRITE (LU,1003) LIGNE
1002              FORMAT(1X,A72,/,1X,'TYPE INCONNU SUR CETTE LIGNE')
1003              FORMAT(1X,A72,/,1X,'UNKNOWN TYPE ON THIS LINE')
                  STOP 'ERREUR DAMOCLES 2'
                  ENDIF
                  ICOL = NEXT(ICOL+1,LIGNE)
C
C    INDEX
C
          ELSE IF(NUMERO.EQ.3) THEN
                  IF (ORDRE.NE.2) GOTO 1500
                  ORDRE=3
                  INDX = INTLU(ICOL,LIGNE)
                  ICOL = NEXT(ICOL+1,LIGNE)
C
C CASE INDEX=-1 : WORD FOR EDAMOX CONSTRUCTION, TO KEEP
C
                  IF (INDX.EQ.-1) THEN
                    NIGN = NIGN + 1
                    IF (NIGN.GT.100) THEN
                      IF (LNG.EQ.1) THEN
                        WRITE(LU,*) 'TROP DE MOTS RESERVES POUR EDAMOX',
     &                              ' (100 AU MAXIMUM)'
                      ELSEIF (LNG.EQ.2) THEN
                        WRITE(LU,*) 'TOO MANY WORDS FOR EDAMOX',
     &                              ' (MAX=100)'
                      ENDIF
                      ERREUR = .TRUE.
                      GO TO 900
                    ENDIF
                    MOTIGN(NIGN)=PARAM(1:LONGU)
                    LONIGN(NIGN)=LONGU
                    TYPIGN(NIGN)=NTYP
                  ENDIF
C
C    SIZE
C
          ELSE IF(NUMERO.EQ.4) THEN
                  IF (ORDRE.NE.3) GOTO 1500
                  ORDRE=4
                  ITAI = INTLU(ICOL,LIGNE)
                  ICOL = NEXT(ICOL+1,LIGNE)
C
C    DEFAULT VALUE
C    FOR ARRAYS, IT IS NOT NECESSARY TO SET ALL VALUES
C
          ELSE IF(NUMERO.EQ.5) THEN
C
C
          IF (ORDRE.LT.3.OR.ORDRE.GT.6) GOTO 1500
          ORDRE=6
          IF (LANGUE) THEN
             DEFLU = 1
             IF (NTYP.NE.4) TROUVE(NTYP,INDX) = 1
C
C200          ICOL = NEXT(ICOL+1,LIGNE) -1
200          CONTINUE
C
             IF (NTYP .EQ. 1) THEN
                DEFINT(DEFLU) = INTLU(ICOL,LIGNE)
             ELSE IF (NTYP .EQ. 2) THEN
                DEFREA(DEFLU) = REALU(ICOL,LIGNE)
             ELSE IF (NTYP .EQ. 3) THEN
                DEFLOG(DEFLU) = LOGLU(ICOL,LIGNE)
             ELSE IF (NTYP .EQ. 4) THEN
                DEFCAR(DEFLU) = CARLU(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,
     &                                SIZE,MOTIGN,LONIGN,NMAXR,NFICDA,
     &                                LEN(DEFCAR(DEFLU)))
                L1 = LONGLU(DEFCAR(DEFLU))
                IF (ITAI.LE.1.AND.INDIC(NTYP,INDX).GE.2) THEN
                   IF (TROUVE(NTYP,INDX).LE.3) THEN
                       IF (L1.GT.0) TROUVE(NTYP,INDX)=1
                   ELSEIF(TROUVE(NTYP,INDX).EQ.6) THEN
C                      IF (L1.GT.0) TROUVE(NTYP,INDX)=7
                       TROUVE(NTYP,INDX)=7
                   ENDIF
                ELSE
                  TROUVE(NTYP,INDX)=1
                ENDIF
             ENDIF
C
             ICOL = NEXT(ICOL+1,LIGNE)
C
             IF(LIGNE(ICOL:ICOL).EQ.PTVIRG) THEN
                DEFLU = DEFLU + 1
                GO TO 200
             ELSE
                ICOL=ICOL-1
             ENDIF
C
C
C
          ELSE
C
C READS THE DEFAULT OF A NON-REQUESTED LANGUAGE (NOT USED)
C
C210          ICOL = NEXT(ICOL+1,LIGNE) -1
 210         CONTINUE
C
             IF (NTYP .EQ. 1) THEN
                NULINT = INTLU(ICOL,LIGNE)
             ELSE IF (NTYP .EQ. 2) THEN
                NULREA = REALU(ICOL,LIGNE)
             ELSE IF (NTYP .EQ. 3) THEN
                NULLOG = LOGLU(ICOL,LIGNE)
             ELSE IF (NTYP .EQ. 4) THEN
                NULCAR = CARLU(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,SIZE,MOTIGN,
     &                         LONIGN,NMAXR,NFICDA,LEN(NULCAR))
             ENDIF
C
             ICOL = NEXT(ICOL+1,LIGNE)
C
             IF (LIGNE(ICOL:ICOL) .EQ. PTVIRG) THEN
                GO TO 210
             ELSE
C               DEFLU=DEFLU
                ICOL=ICOL-1
             ENDIF
          ENDIF
C
          ICOL = NEXT(ICOL+1,LIGNE)
C
C    HELP
C
          ELSE IF(NUMERO.EQ.6) THEN
C
          IF(AIDLNG.AND.DOC) THEN
            WRITE(LU,511)
511         FORMAT(1X,72('-'))
            WRITE(LU,*) PARAM(1:LONGU)
            WRITE(LU,511)
          ENDIF
          CALL AIDELU(ICOL,LIGNE,DOC.AND.AIDLNG)
          AIDLNG = .FALSE.
C
C
C    'CHOIX' 'RUBRIQUE' 'NIVEAU' 'MNEMO' 'COMPOSE' 'COMPORT' 'CONTROLE' 'APPARENCE'
C    NUMBER 7 TO 14 INCLUDED
C
          ELSE IF((NUMERO .GE. 7) .AND. (NUMERO .LE. 14)) THEN
            CALL AIDELU(ICOL,LIGNE,.FALSE.)
C
C    DEFINES A SUBMIT TYPE
          ELSE IF (NUMERO .EQ. 15) THEN
            IF (ORDRE.NE.3.AND.ORDRE.NE.4) GOTO 1500
            ORDRE=5
            IF (.NOT.(LUIGN)) INDIC(NTYP,INDX)=INDIC(NTYP,INDX)+2
            ICOL = NEXT(ICOL+1,LIGNE) -1
            CALL INFLU(ICOL,LIGNE,DEFATT,TROUVE,LUIGN,MOTCLE,SIZE,
     &                 MOTIGN,LONIGN,NMAXR,NFICDA,GESTD)
            DO 890 I=1,DEFLU
               DEFINT(I)    = 0
               DEFREA(I)    = 0.
               DEFLOG(I)    = .FALSE.
               DEFCAR(I)    = ' '
 890        CONTINUE
            IF (ERREUR) GO TO 900
            ICOL = NEXT(ICOL,LIGNE)
          ENDIF
C
          ENDIF
C
      ENDIF
C
      GO TO 100
900   CONTINUE
      IF(ERREUR) THEN
         WRITE(LU,*)' '
         IF(NFIC.EQ.NFICMO) THEN
            WRITE(LU,*)'-------------------------------'
            IF(LNG.EQ.1) THEN
            WRITE(LU,*)'- ERREUR DANS LE DICTIONNAIRE -'
            ENDIF
            IF(LNG.EQ.2) THEN
            WRITE(LU,*)'- ERROR IN THE DICTIONARY     -'
            ENDIF
            WRITE(LU,*)'-------------------------------'
            STOP 'ERREUR DAMOCLES 3'
         ELSE
            WRITE(LU,*)'-----------------------------------------'
            IF(LNG.EQ.1) THEN
            WRITE(LU,*)'- ERREUR DANS LE FICHIER DES PARAMETRES -'
            ENDIF
            IF(LNG.EQ.2) THEN
            WRITE(LU,*)'- ERROR IN THE STEERING FILE            -'
            ENDIF
            WRITE(LU,*)'-----------------------------------------'
            RETRY=RETRY+1
         ENDIF
         IF(RETRY.LE.1) THEN
           RETURN
         ELSE
           STOP 'ERREUR DAMOCLES 3'
         ENDIF
      ENDIF
C
      IF(NFIC.EQ.NFICMO) THEN
                IF (INDX.GT.NMAXR(NTYP)) NMAXR(NTYP)=INDX
                CALL CLASSE(DIMENS,SIZE,MOTCLE,UTINDX,NMAX,
     &                      OFFSET,ADRESS,INDIC,LUIGN,
     &                      MOTINT,MOTREA,MOTLOG,MOTCAR,MOTATT ,
     &                      DEFCAR,DEFINT,DEFLOG,DEFREA,DEFATT )
      ENDIF
      IF(NFICMO.EQ.NFICDA.OR.NFIC.EQ.NFICDA) THEN
C            TRUE END: 2 FILES READ OR 2 FILES IN 1 READ
             GO TO 1000
      ELSE
C            FALSE END: REMAINS A FILE
             NFIC = NFICDA
             RETOUR = .FALSE.
             GO TO 99
      ENDIF
C
1515  CONTINUE
      WRITE(LU,*)'*********************************************'
      IF(LNG.EQ.1) THEN
        WRITE(LU,*)'ADRESSE SUPERIEURE A NMAX = ',NMAX
        WRITE(LU,*)'TROP DE VALEURS DE TYPE : ',NTYP,' DECLAREES.'
        WRITE(LU,*)'ARRET DE DAMOCLES AU MOT-CLE D''INDEX : ',INDX
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*)'ADRESS GREATER THAN NMAX = ',NMAX
        WRITE(LU,*)'TOO MANY VALUES OF TYPE : ',NTYP,' DECLARED.'
        WRITE(LU,*)'STOP OF DAMOCLES AT KEY-WORD NUMBER: ',INDX
      ENDIF
      WRITE(LU,*)'*********************************************'
      STOP 'ERREUR DAMOCLES 4'
C
1000  CONTINUE
C
C COMPACTS WHITE CHARS - REDISTRIBUTES - TESTS THE RESULTS
C
      DO 1195 K=1,NMAXR(4)
       IF (UTINDX(4,K).AND.INDIC(4,K).GE.2.AND.
     &     TROUVE(4,K).LT.3.AND.TROUVE(4,K).GT.0) THEN
         ADD = ADRESS(4,K)
         PARAM = MOTCLE(4,K)
         LONGU = SIZE(4,K)
         NVAL = DIMENS(4,K)
         I=0
 1180    CONTINUE
         I=I+1
 1185    CONTINUE
C IF IT IS A WHITE CHAR (LENGTH=0):
         IF (LONGLU(MOTCAR(ADD+I-1)).EQ.0) THEN
           DO 1190 J=I,NVAL-1
             MOTCAR(ADD+J-1)=MOTCAR(ADD+J)
C
C SUBMITS DO NOT FOLLOW IF THIS LINE IS COMMENTED OUT
C OTHERWISE PB EXPERIENCED WITH STBTEL
C            MOTATT(4,ADD+J-1)=MOTATT(4,ADD+J)
C
 1190      CONTINUE
           NVAL = NVAL-1
           IF (I.LE.NVAL) GO TO 1185
         ENDIF
         IF (I.LT.NVAL) GO TO 1180
C
C CASE OF EMPTY ALLOCATIONS FOR NON ARRAYS
C
         IF (NVAL.EQ.0.AND.INDIC(4,K).EQ.2) THEN
           IF (LNG.EQ.1) THEN
             WRITE(LU,*) 'AFFECTATION VIDE NON ADMISE POUR LE ',
     &                   'MOT CLE : ', PARAM(1:LONGU)
           ELSEIF (LNG.EQ.2) THEN
             WRITE(LU,*) 'EMPTY ALLOCATION NOT ALLOWED FOR ',
     &                   'THE KEY WORD : ', PARAM(1:LONGU)
           ENDIF
           WRITE(LU,*)
           ARRET = .TRUE.
           GO TO 1300
         ENDIF
C
C HAS COMPACTED ARRAYS TO DIMENSION NVAL (CAN BE = 0)
         IF (NVAL.LT.DIMENS(4,K)) THEN
           DIMENS(4,K) = NVAL
           TROUVE(4,K) = 5
         ENDIF
       ENDIF
C
C CASE OF SUBMIT ARRAYS NEVER AFFECTED -> DIMENSION = 0
       IF (UTINDX(4,K).AND.INDIC(4,K).EQ.3.AND.TROUVE(4,K).EQ.0.
     &     AND.DIMENS(4,K).GT.1) THEN
          DIMENS(4,K) = 0
          TROUVE(4,K) = 3
       ENDIF
C
 1195 CONTINUE
C
C CARRIES OUT THE COMMANDS RECORDED BEFORE THE END
      EXECMD = .TRUE.
C     TO AVOID TESTS ON LINE IN CMD
      LIGNE = 'NUL'
      DO K = 1,5
        VUCMD0(K) = VUCMD(K)
        VUCMD(K)  = .FALSE.
      ENDDO
C
      DO K=1,5
        VUCMD(K)=VUCMD0(K)
        IF (VUCMD(K).AND.(.NOT.(ERREUR))) THEN
          CALL CMD (ICOL,LIGNE,ADRESS,DIMENS,TROUVE,MOTCLE,NMOT,
     &          MOTINT,MOTREA,MOTLOG,MOTCAR,MOTATT,INDIC,SIZE,
     &          UTINDX,DYNAM,VUCMD,EXECMD,NFICDA,NMAXR)
          VUCMD(K) = .FALSE.
        ENDIF
      ENDDO
C
C  LOOKS FOR REQUIRED KEYWORDS THAT HAVE NOT BEEN READ:
C
      WRITE(LU,*) ' '
C
      DO 1200  K = 1 , 4
      DO 1201 INDX = 1 , NMAXR(K)
        IF (UTINDX(K,INDX)) THEN
          IF (TROUVE(K,INDX).EQ.0) THEN
C
C IF NO DEFAULT VALUE AND NOTHING IN STEERING FILE, DIMENS = 0 FOR ARRAYS
            IF (DIMENS(K,INDX).NE.1) THEN
              IF (DYNAM) DIMENS(K,INDX) = 0
            ELSE
              WRITE(LU,*)'----------------------------------------'
              ARRET= .TRUE.
      IF(LNG.EQ.1) WRITE(LU,1101) MOTCLE(K,INDX)(1:SIZE(K,INDX))
      IF(LNG.EQ.2) WRITE(LU,1102) MOTCLE(K,INDX)(1:SIZE(K,INDX))
 1101         FORMAT(1X,'ATTENTION, LE MOT-CLE :',1X,A,/,1X,
     &        'N''A PAS RECU DE VALEUR')
 1102         FORMAT(1X,'BEWARE, THE KEY-WORD:',1X,A,/,1X,
     &        'HAS BEEN GIVEN NO VALUE')
            ENDIF
          ENDIF
        ENDIF
1201  CONTINUE
1200  CONTINUE
C
1300  CONTINUE
      IF(ARRET) THEN
        WRITE(LU,*)  ' '
        IF(LNG.EQ.1) WRITE(LU,*) 'INTERRUPTION DU SOUS-PROGRAMME DAMOC'
        IF(LNG.EQ.2) WRITE(LU,*) 'DAMOC IS STOPPED'
        STOP 'ERREUR DAMOCLES 5'
      ENDIF
C
      RETURN
C
C TREATS ERRORS OF DECLARATION ORDER IN THE DICTIONARY
C
1500  ERREUR=.TRUE.
      WRITE(LU,'(/,1X,A72,/)') LIGNE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'A LA LIGNE ',NLIGN,
     &               ', ORDRE DE DEFINITION OBLIGATOIRE NON RESPECTE'
        WRITE(LU,*)
        WRITE(LU,*) 'L''ORDRE ATTENDU EST LE SUIVANT :'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'AT LINE ',NLIGN,', PRIORITY ORDER NOT RESPECTED'
        WRITE(LU,*)
        WRITE(LU,*) 'EXPECTED ORDER IS :'
      ENDIF
      WRITE(LU,*) 'NOM, TYPE, INDEX, (TAILLE), (SUBMIT), (DEFAUT)'
      GOTO 900
C
C-----------------------------------------------------------------------
C
      END
C
C#######################################################################
C