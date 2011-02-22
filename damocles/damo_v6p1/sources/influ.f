C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief    DECODES THE SUBMIT FIELD FROM COLUMN ICOL+1 OF THE
!>             CURRENT LINE. TESTS THE PRESENCE OF THE 4 FIELDS.
!>             RECOGNISES CHAMP2.
!>             MOVES THE POINTER ICOL TO THE LAST DECODED CHARACTER.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note      PORTABILITY : IBM,CRAY,HP,SUN

!>  @warning  IF THE 1ST FIELD IS NOT KNOWN, THERE ARE NO CHECKS OTHER
!>            THAN ON THE 2ND FIELD, WHICH CHARACTERISES THE BEHAVIOUR
!>            OF THE KEYWORD FOR DAMOCLES. THIS EXTENDS THE
!>            COMPATIBILITY OF DAMOCLES WITHOUT DIRECT MODIFICATIONS
!>            TO THE FORTRAN

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DEFATT, GESTD, ICOL, LIGNE, LONIGN, LUIGN, MOTCLE, MOTIGN, NFICDA, NMAXR, SIZE, TROUVE
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> DCINFO : LNG, LU<hr>
!> DCNGE : INDX, NTYP, ITAI, LONGU, NMOT, DEFLU<hr>
!> DCNGEC : PARAM<hr>
!> DCRARE : ERREUR, RETOUR<hr>
!> DCMLIG : NLIGN, LONGLI
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ANALYS, CHAMP, FIELD, FIELD0, GECHP1, GUILLT, I, ICOLA, II, JCOLA, LCAR, LGA, LGMOTG, MESERR, MOTCH1, NBCHP1, NULATT, PTVIRG, QUOTE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CARLU(), LONGLU(), MAJUS(), NEXT(), PRECAR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DAMOC()

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
!>    <td><center> 5.8                                    </center></td>
!>    <td> 14/01/2008                                              </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
!>    <td> CORRECTION: IN MOTCH1(1:LCAR), LCAR SHOULD NOT EXCEED 10</td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td> 16/08/1994                                              </td>
!>    <td> O. QUIQUEMPOIX (LNH) 30.87.78.70                        </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DEFATT
!></td><td><--</td><td>TABLEAU DES SUBMITS PAR DEFAUT
!>    </td></tr>
!>          <tr><td>DEFLU
!></td><td>--></td><td>NOMBRE DE VALEURS LUES POUR LE MOT CLE
!>    </td></tr>
!>          <tr><td>ERREUR
!></td><td><-></td><td>SORT AVEC LA VALEUR .TRUE. EN CAS D'ERREUR
!>    </td></tr>
!>          <tr><td>GESTD
!></td><td>--></td><td>LOGIQUE D'APPEL PAR LE GESTIONNAIRE D'ETUDES
!>    </td></tr>
!>          <tr><td>ICOL
!></td><td><-></td><td>POSITION COURANTE DU POINTEUR DANS LA LIGNE
!>    </td></tr>
!>          <tr><td>INDX
!></td><td>--></td><td>INDEX DU MOT CLE EN COURS
!>    </td></tr>
!>          <tr><td>ITAI
!></td><td>--></td><td>TAILLE DU MOT CLE EN COURS
!>    </td></tr>
!>          <tr><td>LIGNE
!></td><td><-></td><td>LIGNE EN COURS DE DECODAGE
!>    </td></tr>
!>          <tr><td>LNG
!></td><td>--></td><td>NUMERO DE LA LANGUE DE DECODAGE
!>    </td></tr>
!>          <tr><td>LONGLI
!></td><td>--></td><td>LONGUEUR DES LIGNES
!>    </td></tr>
!>          <tr><td>LONGU
!></td><td>--></td><td>LONGUEUR DU MOT CLE EN COURS
!>    </td></tr>
!>          <tr><td>LONIGN
!></td><td>--></td><td>TABLEAU DES LONGUEURS DES MOTS EDAMOX
!>    </td></tr>
!>          <tr><td>LU
!></td><td>--></td><td>NUMERO DE L'UNITE LOGIQUE DES SORTIES
!>    </td></tr>
!>          <tr><td>LUIGN
!></td><td>--></td><td>LOGIQUE POUR LES MOTS A NE PAS CLASSER
!>    </td></tr>
!>          <tr><td>MOTCLE
!></td><td>--></td><td>TABLEAU DES MOTS CLES ACTIFS
!>    </td></tr>
!>          <tr><td>MOTIGN
!></td><td>--></td><td>TABLEAU DES MOTS CLES DUS A EDAMOX A IGNORER
!>    </td></tr>
!>          <tr><td>NFICDA
!></td><td>--></td><td>NUMERO DE CANAL DU FICHIER DES DONNEES
!>    </td></tr>
!>          <tr><td>NLIGN
!></td><td><-></td><td>NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU
!>    </td></tr>
!>          <tr><td>NMAXR
!></td><td>--></td><td>TABLEAU DES INDEX MAXIMUM REELS PAR TYPES
!>    </td></tr>
!>          <tr><td>NMOT
!></td><td>--></td><td>TABLEAU DU NOMBRE DE MOTS CLES PAR TYPE
!>    </td></tr>
!>          <tr><td>NTYP
!></td><td>--></td><td>TYPE DU MOT CLE EN COURS
!>    </td></tr>
!>          <tr><td>PARAM
!></td><td>--></td><td>NOM DU MOT CLE EN COURS
!>    </td></tr>
!>          <tr><td>RETOUR
!></td><td><-></td><td>SORT AVEC LA VALEUR .TRUE. EN CAS DE FIN DE
!>                  FIN DE FICHIER OU D'ERREUR DE LECTURE.
!>    </td></tr>
!>          <tr><td>SIZE
!></td><td>--></td><td>TABLEAU DES LONGUEURS DES MOTS CLES
!>    </td></tr>
!>          <tr><td>TROUVE
!></td><td><-></td><td>INDICATEUR D'ETAT DES MOTS CLES
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
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE INFLU
     &( ICOL   , LIGNE  , DEFATT , TROUVE , LUIGN , MOTCLE , SIZE,
     &  MOTIGN , LONIGN , NMAXR  , NFICDA , GESTD )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DEFATT         |<--| TABLEAU DES SUBMITS PAR DEFAUT
C| DEFLU          |-->| NOMBRE DE VALEURS LUES POUR LE MOT CLE
C| ERREUR         |<->| SORT AVEC LA VALEUR .TRUE. EN CAS D'ERREUR
C| GESTD          |-->| LOGIQUE D'APPEL PAR LE GESTIONNAIRE D'ETUDES
C| ICOL           |<->| POSITION COURANTE DU POINTEUR DANS LA LIGNE
C| INDX           |-->| INDEX DU MOT CLE EN COURS
C| ITAI           |-->| TAILLE DU MOT CLE EN COURS
C| LIGNE          |<->| LIGNE EN COURS DE DECODAGE
C| LNG            |-->| NUMERO DE LA LANGUE DE DECODAGE
C| LONGLI         |-->| LONGUEUR DES LIGNES
C| LONGU          |-->| LONGUEUR DU MOT CLE EN COURS
C| LONIGN         |-->| TABLEAU DES LONGUEURS DES MOTS EDAMOX
C| LU             |-->| NUMERO DE L'UNITE LOGIQUE DES SORTIES
C| LUIGN          |-->| LOGIQUE POUR LES MOTS A NE PAS CLASSER
C| MOTCLE         |-->| TABLEAU DES MOTS CLES ACTIFS
C| MOTIGN         |-->| TABLEAU DES MOTS CLES DUS A EDAMOX A IGNORER
C| NFICDA         |-->| NUMERO DE CANAL DU FICHIER DES DONNEES
C| NLIGN          |<->| NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU
C| NMAXR          |-->| TABLEAU DES INDEX MAXIMUM REELS PAR TYPES
C| NMOT           |-->| TABLEAU DU NOMBRE DE MOTS CLES PAR TYPE
C| NTYP           |-->| TYPE DU MOT CLE EN COURS
C| PARAM          |-->| NOM DU MOT CLE EN COURS
C| RETOUR         |<->| SORT AVEC LA VALEUR .TRUE. EN CAS DE FIN DE
C|                |   | FIN DE FICHIER OU D'ERREUR DE LECTURE.
C| SIZE           |-->| TABLEAU DES LONGUEURS DES MOTS CLES
C| TROUVE         |<->| INDICATEUR D'ETAT DES MOTS CLES
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
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      EXTERNAL NEXT,PRECAR,CARLU,LONGLU
C
      INTEGER       TROUVE(4,*),ICOL,NMAXR(4),NFICDA,SIZE(4,*)
      INTEGER       LONIGN(100)
      LOGICAL       LUIGN,GESTD
      CHARACTER*72  MOTIGN(100),MOTCLE(4,*)
      CHARACTER*144 DEFATT(*)
      CHARACTER*(*) LIGNE
C
      INTEGER       NEXT,PRECAR,LONGLU
      CHARACTER*144 CARLU
C
      INTEGER       LNG,LU
      INTEGER       INDX,NTYP,ITAI,LONGU,NMOT(4),DEFLU
      INTEGER       NLIGN,LONGLI
      LOGICAL       ERREUR,RETOUR
      CHARACTER*72  PARAM
C
C-----------------------------------------------------------------------
C
      INTEGER       NBCHP1
      PARAMETER (NBCHP1=12)
C
      INTEGER       I,LCAR,ICOLA,JCOLA,CHAMP(4),LGA,II
      INTEGER       LGMOTG(NBCHP1),GECHP1(NBCHP1)
      CHARACTER*1   PTVIRG,QUOTE,GUILLT
      CHARACTER*72  MESERR(2*NBCHP1)
      CHARACTER*10  MOTCH1(NBCHP1)
      CHARACTER*144 NULATT,ANALYS,FIELD,FIELD0
C
C-----------------------------------------------------------------------
C
      COMMON / DCINFO / LNG,LU
      COMMON / DCNGE  / INDX,NTYP,ITAI,LONGU,NMOT,DEFLU
      COMMON / DCNGEC / PARAM
      COMMON / DCRARE / ERREUR,RETOUR
      COMMON / DCMLIG / NLIGN,LONGLI
C
C-----------------------------------------------------------------------
C
C ******************* DATABASE FOR THE SUBROUTINE **********************
C
C DEFINITION OF FIELDS 1
      DATA MOTCH1 /'IN','OUT','CAS','DIC','QSUB','LIB','FORTRAN',
     &            'DIROUT','USER','ACCT','PRE','POST'/
C LENGTHS OF THE STRINGS FOR FIELDS 1 DEFINED ABOVE
      DATA LGMOTG /2,3,3,3,4,3,7,6,4,4,3,4/
C CHANGE TO 'NUL;FOR' IF GESTD=.TRUE. ? : 1-YES, 0-NO
      DATA GECHP1 /1,1,0,0,0,0,1,1,0,0,0,0/
C NUMBER OF THE FIELDS TO BE GIVEN TO THESE WORDS --> ERROR MESSAGES
C      DATA NOCHMP /1,2,3,4,5,6,7,8,9,10,11,12/
C ERROR MESSAGES ASSOCIATED WITH THE FIELD NUMBERS
      DATA MESERR /
     & 'PAS D''ALLOCATION DE FICHIER D''ENTREE !!',
     & 'NO ALLOCATION FOR INPUT FILE !!',
     & 'PAS D''ALLOCATION DE FICHIER DE SORTIE !!',
     & 'NO ALLOCATION FOR OUTPUT FILE !!',
     & 'PAS D''ALLOCATION POUR LE FICHIER CAS !!',
     & 'NO ALLOCATION FOR THE STEERING FILE !!',
     & 'PAS D''ALLOCATION POUR LE DICTIONNAIRE !!',
     & 'NO ALLOCATION FOR THE DICTIONARY !!',
     & 'PAS DE COMMANDE CRAY !!','NO INSTRUCTION FOR CRAY !!',
     & 'PAS DE LIBRAIRIE !!', 'NO LIBRARY !!',
     & 'PAS DE VALEUR POUR LE REPERTOIRE FORTRAN !!',
     & 'NO VALUE FOR THE FORTRAN DIRECTORY !!',
     & 'PAS DE VALEUR POUR LE REPERTOIRE DE SORTIE !!',
     & 'NO VALUE FOR THE OUTPUT DIRECTORY !!',
     & 'PAS DE COMMANDE CRAY !!','NO INSTRUCTION FOR CRAY !!',
     & 'PAS DE COMMANDE CRAY !!','NO INSTRUCTION FOR CRAY !!',
     & 'PAS DE COMMANDE PRE !!','NO INSTRUCTION FOR PRE !!',
     & 'PAS DE COMMANDE POST !!','NO INSTRUCTION FOR POST !!'
     & /
C
C***********************************************************************
C                                    RCS AND SCCS MARKING
C
C***********************************************************************
C
C  INITIALISES
C
      PTVIRG = ';'
      QUOTE  = ''''
      GUILLT = '"'
      DEFLU  = 0
C
100   DEFLU = DEFLU +1
      IF(.NOT.(LUIGN)) THEN
        DEFATT(DEFLU)=CARLU(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,SIZE,MOTIGN,
     &                      LONIGN,NMAXR,NFICDA,LEN(DEFATT(DEFLU)))
      ELSE
        NULATT = CARLU(LCAR,ICOL,LIGNE,QUOTE,MOTCLE,SIZE,MOTIGN,
     &                 LONIGN,NMAXR,NFICDA,LEN(NULATT))
      ENDIF
C
      ICOL = NEXT(ICOL+1,LIGNE)
C
      IF (LIGNE(ICOL:ICOL) .EQ. PTVIRG) GO TO 100
C
C NO ANALYSIS IF TO BE IGNORED ...
      IF (LUIGN) GO TO 1300
C
      IF (DEFLU .LT. ITAI) THEN
         ERREUR = .TRUE.
         IF(LNG.EQ.1) THEN
           WRITE(LU,*)'POUR LE MOT CLE : ',PARAM(1:LONGU)
           WRITE(LU,*)'PAS ASSEZ DE VALEURS DEFINIES POUR SUBMIT...'
         ELSEIF(LNG.EQ.2) THEN
           WRITE(LU,*)'FOR THE KEY-WORD : ', PARAM(1:LONGU)
           WRITE(LU,*)'NOT ENOUGH DATAS DEFINED FOR SUBMIT...'
         ENDIF
         WRITE(LU,*)' '
         GO TO 1300
      ENDIF
C
C  EXAMINES THE SUBMIT FIELDS
C
      DO 1140 I = 1 , DEFLU
 200     ICOLA = 0
         ANALYS = DEFATT(I)
C
C   *** FIELD 1 ***
C
         LGA = MAX(LONGLU(ANALYS),1)
         IF (ANALYS(ICOLA+1:ICOLA+1).EQ.';') THEN
           LCAR = 0
         ELSE
           JCOLA = PRECAR(ICOLA+1,ANALYS,';',';',';')
           LCAR = LONGLU(ANALYS(ICOLA+1:JCOLA-1))
           IF (LCAR.GT.0) THEN
             FIELD0 = CARLU(LCAR,ICOLA,ANALYS,GUILLT,MOTCLE,SIZE,MOTIGN,
     &                      LONIGN,NMAXR,NFICDA,LEN(FIELD0))
             LCAR = LONGLU(FIELD0(1:LCAR))
          ENDIF
         ENDIF
         IF (LCAR.LE.0) THEN
           IF (LNG.EQ.1) THEN
              WRITE(LU,*)'POUR LE MOT CLE : ',PARAM(1:LONGU)
              WRITE(LU,*)'SUBMIT INCORRECT : ',ANALYS(1:LGA)
              WRITE(LU,*)'PAS DE PREMIER CHAMP !!'
           ELSEIF (LNG.EQ.2) THEN
              WRITE(LU,*)'FOR THE KEY-WORD : ', PARAM(1:LONGU)
              WRITE(LU,*)'INVALID SUBMIT : ',ANALYS(1:LGA)
              WRITE(LU,*)'NO FIRST FIELD !!'
           ENDIF
           ERREUR = .TRUE.
           GO TO 1300
         ENDIF
         IF (ERREUR) GO TO 1300
         FIELD = FIELD0
         CALL MAJUS(FIELD)
C
         CHAMP(1)=100
         DO 300 II=1,NBCHP1
         IF (LCAR.EQ.LGMOTG(II).AND.
     &       FIELD(1:MIN(LCAR,10)).EQ.MOTCH1(II)(1:MIN(LCAR,10))) THEN
            IF (GESTD.AND.GECHP1(II).EQ.1) THEN
              DEFATT(I) = 'NUL;FOR'//DEFATT(I)(JCOLA+4:MAX(LGA,JCOLA+4))
              GO TO 200
            ELSE
C             CHAMP(1)=NOCHMP(II)
              CHAMP(1)=II
              GOTO 400
            ENDIF
         ENDIF
300      CONTINUE
C
C   *** FIELD 2 ***
C
400      ICOLA = JCOLA
         IF (ICOLA.GE.LONGLI) THEN
           LCAR = 0
         ELSEIF (ANALYS(ICOLA+1:ICOLA+1).EQ.';') THEN
           LCAR = 0
         ELSE
           JCOLA = PRECAR(ICOLA+1,ANALYS,';',';',';')
           LCAR = LONGLU(ANALYS(ICOLA+1:JCOLA-1))
           IF (LCAR.GT.0) THEN
             FIELD0 = CARLU(LCAR,ICOLA,ANALYS,GUILLT,MOTCLE,SIZE,MOTIGN,
     &                      LONIGN,NMAXR,NFICDA,LEN(FIELD0))
             LCAR = LONGLU(FIELD0(1:LCAR))
           ENDIF
         ENDIF
         IF (LCAR.LE.0) THEN
           IF (LNG.EQ.1) THEN
              WRITE(LU,*)'POUR LE MOT CLE : ',PARAM(1:LONGU)
              WRITE(LU,*)'SUBMIT INCORRECT : ',ANALYS(1:LGA)
              WRITE(LU,*)'PAS DE DEUXIEME CHAMP !!'
           ELSEIF (LNG.EQ.2) THEN
              WRITE(LU,*)'FOR THE KEY-WORD : ', PARAM(1:LONGU)
              WRITE(LU,*)'INVALID SUBMIT : ',ANALYS(1:LGA)
              WRITE(LU,*)'NO SECOND FIELD !! '
           ENDIF
           ERREUR = .TRUE.
           GO TO 1300
         ENDIF
C
         IF (ERREUR) GO TO 1300
         FIELD = FIELD0
         CALL MAJUS(FIELD)
C
C  NOTE JMH 13/11/2001 : SUPPRESSION OF AN OBSOLETE CONTROL
C  THE 2ND SUBMIT FIELD CAN BE DIFFERENT FROM FOR...
C  (CASE OF SUBIEF)
C
         IF (FIELD(1:3).EQ.'OPT') THEN
            CHAMP(2) = 1
         ELSEIF (FIELD(1:3).EQ.'REQ') THEN
            CHAMP(2) = 2
C        ELSEIF (FIELD(1:3).EQ.'FOR') THEN
         ELSE
            CHAMP(2) = 3
C        ELSE
C           ERREUR = .TRUE.
C           IF (LNG.EQ.1) THEN
C             WRITE(LU,*)'POUR LE MOT CLE : ',PARAM(1:LONGU)
C             WRITE(LU,*)'SUBMIT INCORRECT : ',ANALYS(1:LGA)
C             WRITE(LU,*)'DEUXIEME CHAMP INCONNU : ',FIELD0(1:LCAR)
C           ELSEIF (LNG.EQ.2) THEN
C             WRITE(LU,*)'FOR THE KEY-WORD : ', PARAM(1:LONGU)
C             WRITE(LU,*)'INVALID SUBMIT : ',ANALYS(1:LGA)
C             WRITE(LU,*)'SECOND FIELD UNKNOWN : ',FIELD0(1:LCAR)
C           ENDIF
C           GO TO 1300
         ENDIF
C
C ASSIGNS THE INITIAL VALUE TO TROUVE ACCORDING TO CHAMP(1)
         IF (ITAI.LE.1.AND.I.LE.MAX(ITAI,1)) THEN
          IF (CHAMP(2) .EQ. 1) TROUVE(NTYP,INDX)=3
          IF (CHAMP(2) .EQ. 3) TROUVE(NTYP,INDX)=6
C         IF (CHAMP(1) .EQ. 3) TROUVE(NTYP,INDX)=10
          IF (CHAMP(1) .EQ. 4) TROUVE(NTYP,INDX)=9
        ENDIF
C
C IF THE 1ST FIELD IS NOT KNOWN, IGNORES THE REST
C TO BE COMPATIBLE WITH EVOLUTIONS OF THE LAUNCHER
        IF (CHAMP(1).EQ.100) GO TO 1140
C
C   *** FIELD 3 ***
C
        ICOLA = JCOLA
        IF (JCOLA.GE.LONGLI) THEN
           IF (LNG.EQ.1) THEN
              WRITE(LU,*)'POUR LE MOT CLE : ',PARAM(1:LONGU)
              WRITE(LU,*)'SUBMIT INCORRECT : ',ANALYS(1:LGA)
              WRITE(LU,*)'PAS DE TROISIEME CHAMP !!'
           ELSEIF (LNG.EQ.2) THEN
              WRITE(LU,*)'FOR THE KEY-WORD : ', PARAM(1:LONGU)
              WRITE(LU,*)'INVALID SUBMIT : ',ANALYS(1:LGA)
              WRITE(LU,*)'NO THIRD FIELD !! '
           ENDIF
           ERREUR = .TRUE.
           GO TO 1300
        ENDIF
        JCOLA = PRECAR(ICOLA+1,ANALYS,';',';',';')
C
C   *** FIELD 4 ***
C
        ICOLA = JCOLA
        IF (ICOLA.GE.LONGLI) THEN
          LCAR = 0
        ELSEIF (ANALYS(ICOLA+1:ICOLA+1).EQ.';') THEN
           LCAR = 0
        ELSE
           JCOLA = PRECAR(ICOLA+1,ANALYS,';',';',';')
           LCAR = LONGLU(ANALYS(ICOLA+1:JCOLA-1))
        ENDIF
        IF (LCAR.LE.0) THEN
             IF (LNG.EQ.1) THEN
              WRITE(LU,*)'POUR LE MOT CLE : ',PARAM(1:LONGU)
              WRITE(LU,*)'SUBMIT INCORRECT : ',ANALYS(1:LGA)
            ELSEIF (LNG.EQ.2) THEN
              WRITE(LU,*)'FOR THE KEY-WORD : ', PARAM(1:LONGU)
              WRITE(LU,*)'INVALID SUBMIT : ',ANALYS(1:LGA)
            ENDIF
            ERREUR = .TRUE.
C
C WRITES THE CORRESPONDING ERROR MESSAGE
            WRITE(LU,*) MESERR(2*(CHAMP(1)-1)+LNG)
            GO TO 1300
        ENDIF
1140  CONTINUE
C
C-----------------------------------------------------------------------
C
1300  CONTINUE
      RETURN
      END

C
C#######################################################################
C