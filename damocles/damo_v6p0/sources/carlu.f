C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief    DECODES A CHARACTER STRING, FROM COLUMN ICOL+1 OF THE
!>             CURRENT LINE (MAXIMUM OF LGA CHARACTERS).
!>             IF THE STRING IS NOT COMPLETE, GOES TO THE NEXT LINE
!>             IF NEED BE.
!>             MOVES THE POINTER ICOL TO THE LAST DECODED CHARACTER
!>             OR TO ICOL=0 IF THE NEXT LINE WAS READ WITH NO REASON.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note      PORTABILITY : IBM,CRAY,HP,SUN

!>  @warning  FOLLOWS THE FORTRAN CONVENTION : '' IS READ AS
!>            ' WHEN WITHIN A CHARACTER STRING
!>
!>  @warning  STRINGS WITHOUT ' OR " CANNOT CONTAIN SEPARATOR
!>            CHARACTERS

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> EXTREM, ICOL, LCAR, LGVAR, LIGNE, LONIGN, MOTCLE, MOTIGN, NFICDA, NMAXR, SIZE
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> DCINFO : LNG, LU<hr>
!> DCRARE : ERREUR, RETOUR<hr>
!> DCMLIG : NLIGN, LONGLI<hr>
!> DCCHIE : NFIC
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> COTE, I, ICOL2, IDEB, IFIN, ITYP, K, LGLU, LIGNE2, LIGNED, LISUIV, LONPRO, LUCOTE, LUFIC, MOTPRO, NCAR, NLIGN2, QCAS, QUOTE, TABUL
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> LONGLU(), NEXT(), PRECAR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DAMOC(), INFLU()

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
!>    <td><center> 5.1                                    </center></td>
!>    <td> 16/08/1994                                              </td>
!>    <td> J.M. HERVOUET (LNH); A. YESSAYAN                        </td>
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
!>          <tr><td>ERREUR
!></td><td><--</td><td>SORT AVEC LA VALEUR .TRUE. EN CAS D'ERREUR
!>    </td></tr>
!>          <tr><td>EXTREM
!></td><td>--></td><td>SEPARATEUR DE CHAINE = ' OU "
!>    </td></tr>
!>          <tr><td>ICOL
!></td><td><-></td><td>POSITION COURANTE DU POINTEUR DANS LA LIGNE
!>    </td></tr>
!>          <tr><td>LCAR
!></td><td><--</td><td>LONGUEUR DE LA CHAINE DE CARACTERES
!>    </td></tr>
!>          <tr><td>LGVAR
!></td><td>--></td><td>LONGUEUR MAXIMUM DE LA CHAINE A LIRE
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
!>          <tr><td>LONIGN
!></td><td>--></td><td>TABLEAU DES LONGUEURS DES MOTS EDAMOX
!>    </td></tr>
!>          <tr><td>LU
!></td><td>--></td><td>NUMERO DE L'UNITE LOGIQUE DES SORTIES
!>    </td></tr>
!>          <tr><td>MOTCLE
!></td><td>--></td><td>TABLEAU DES MOTS CLES ACTIFS
!>    </td></tr>
!>          <tr><td>MOTIGN
!></td><td>--></td><td>TABLEAU DES MOTS CLES DUS A EDAMOX A IGNORER
!>    </td></tr>
!>          <tr><td>NFIC
!></td><td>--></td><td>NUMERO DE CANAL DU FICHIER EN COURS DE LECT.
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
!>          <tr><td>RETOUR
!></td><td><--</td><td>SORT AVEC LA VALEUR .TRUE. EN CAS DE FIN DE
!>                  FIN DE FICHIER OU D'ERREUR DE LECTURE.
!>    </td></tr>
!>          <tr><td>SIZE
!></td><td>--></td><td>TABLEAU DES LONGUEURS DES MOTS CLES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                   CHARACTER*144 FUNCTION CARLU
     &( LCAR   , ICOL  , LIGNE  , EXTREM , MOTCLE , SIZE , MOTIGN ,
     &  LONIGN , NMAXR , NFICDA , LGVAR  )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ERREUR         |<--| SORT AVEC LA VALEUR .TRUE. EN CAS D'ERREUR
C| EXTREM         |-->| SEPARATEUR DE CHAINE = ' OU "
C| ICOL           |<->| POSITION COURANTE DU POINTEUR DANS LA LIGNE
C| LCAR           |<--| LONGUEUR DE LA CHAINE DE CARACTERES
C| LGVAR          |-->| LONGUEUR MAXIMUM DE LA CHAINE A LIRE
C| LIGNE          |<->| LIGNE EN COURS DE DECODAGE
C| LNG            |-->| NUMERO DE LA LANGUE DE DECODAGE
C| LONGLI         |-->| LONGUEUR DES LIGNES
C| LONIGN         |-->| TABLEAU DES LONGUEURS DES MOTS EDAMOX
C| LU             |-->| NUMERO DE L'UNITE LOGIQUE DES SORTIES
C| MOTCLE         |-->| TABLEAU DES MOTS CLES ACTIFS
C| MOTIGN         |-->| TABLEAU DES MOTS CLES DUS A EDAMOX A IGNORER
C| NFIC           |-->| NUMERO DE CANAL DU FICHIER EN COURS DE LECT.
C| NFICDA         |-->| NUMERO DE CANAL DU FICHIER DES DONNEES
C| NLIGN          |<->| NUMERO DE LA LIGNE TRAITEE DANS LE FICHIER LU
C| NMAXR          |-->| TABLEAU DES INDEX MAXIMUM REELS PAR TYPES
C| RETOUR         |<--| SORT AVEC LA VALEUR .TRUE. EN CAS DE FIN DE
C|                |   | FIN DE FICHIER OU D'ERREUR DE LECTURE.
C| SIZE           |-->| TABLEAU DES LONGUEURS DES MOTS CLES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C
      INTEGER       LCAR,ICOL,NMAXR(4),NFICDA,LGVAR,SIZE(4,*)
      INTEGER       LONIGN(100)
      CHARACTER(LEN=*) LIGNE
      CHARACTER*1   EXTREM
      CHARACTER*72  MOTIGN(100),MOTCLE(4,*)
C
      INTEGER  NEXT,PRECAR,LONGLU
      EXTERNAL NEXT,PRECAR,LONGLU
C
      INTEGER       LNG,LU
      INTEGER       NLIGN,LONGLI
      INTEGER       NFIC
      LOGICAL       ERREUR , RETOUR
C
C-----------------------------------------------------------------------
C
      INTEGER       I,IDEB,IFIN,NCAR,ICOL2,NLIGN2,ITYP,K,LGLU,LONPRO(15)
      INTEGER       QCAS
      LOGICAL       COTE,LISUIV,LUFIC,LUCOTE
      CHARACTER*1   QUOTE,TABUL
      CHARACTER*9   MOTPRO(15)
      CHARACTER*72  LIGNE2
      CHARACTER*144 LIGNED
C
C-----------------------------------------------------------------------
C
      COMMON / DCINFO / LNG,LU
      COMMON / DCRARE / ERREUR,RETOUR
      COMMON / DCMLIG / NLIGN,LONGLI
      COMMON / DCCHIE / NFIC
C
      INTRINSIC CHAR
C
C-----------------------------------------------------------------------
C
      DATA MOTPRO/'NOM','TYPE','INDEX','TAILLE','DEFAUT','AIDE',
     & 'CHOIX','RUBRIQUE','NIVEAU','MNEMO','COMPOSE','COMPORT',
     & 'CONTROLE','APPARENCE','SUBMIT'/
C LENGTH OF PROTECTED WORDS
      DATA LONPRO /3,4,5,6,6,4,5,8,6,5,7,7,8,9,6/
C
C***********************************************************************
C                                    RCS AND SCCS MARKING
C
C***********************************************************************
C
      COTE   = .FALSE.
      LISUIV = .FALSE.
      LUFIC  = .FALSE.
      LUCOTE = .FALSE.
      LCAR   = 1
      CARLU  = ' '
      QUOTE  = ''''
      TABUL  = CHAR(9)
      NLIGN2 = NLIGN
      ICOL2  = ICOL
C     LIGNE2 = LIGNE
      LIGNE2 = LIGNE(1:MIN(72,LEN(LIGNE)))
      LIGNED = ' '
      LGLU   = 0
      QCAS   = 0
C
      ICOL   = NEXT( ICOL+1 , LIGNE )
C
C        //// FINDS THE ENDS OF THE STRING ////
C
C    NOTE: THE STRING CAN BE BETWEEN QUOTES OR WITHOUT QUOTES
C          IT CANNOT CONTAIN WHITE CHARACTERS IF THERE ARE
C          NO QUOTES
C
      IF ( LIGNE(ICOL:ICOL).NE.EXTREM ) THEN
           IDEB = ICOL
C                 PRECAR : SAME ROLE AS PREVAL, EXCEPT IT DOES NOT
C                          SKIP COMMENTED LINES
           ICOL = PRECAR ( ICOL+1 , LIGNE , ' ' , ';' , TABUL) - 1
           IFIN = ICOL
           LIGNED = LIGNE(IDEB:IFIN)
           LGLU = IFIN-IDEB+1
C
C STEERING FILE : GOES TO THE NEXT, WHEN GETS TO THE END OF A LINE
C
290        IF (IFIN.GE.LONGLI) THEN
             LISUIV = .TRUE.
             LUFIC = .TRUE.
             READ(NFIC,END=900,ERR=998,FMT='(A)') LIGNE2
             ICOL2 = 0
             IF (LIGNE2(1:1).EQ.'&'.OR.
     &           LIGNE2(1:1).EQ.'='.OR.LIGNE2(1:1).EQ.':'.OR.
     &           LIGNE2(1:1).EQ.';'.OR.LIGNE2(1:1).EQ.'/' ) THEN
                LISUIV = .FALSE.
                GO TO 96
             ENDIF
C
C CHECKS IF IT'S A KNOWN KEYWORD FOR THE STEERING FILE
C
            IF (NFIC.EQ.NFICDA) THEN
             DO 300 ITYP = 1,4
              DO 310 I=1,NMAXR(ITYP)
C                K=LONGLU(MOTCLE(ITYP,I))
                 K=SIZE(ITYP,I)
                 IF (K.GT.0.AND.LIGNE2(1:K).EQ.MOTCLE(ITYP,I)(1:K)) THEN
                    LISUIV = .FALSE.
                    GO TO 96
                 ENDIF
 310          CONTINUE
 300         CONTINUE
             DO 320 I=1,100
C               K = LONGLU(MOTIGN(I))
                K = LONIGN(I)
                IF(K.GT.0.AND.LIGNE2(1:K).EQ.MOTIGN(I)(1:K)) THEN
                  LISUIV = .FALSE.
                  GO TO 96
                ENDIF
 320         CONTINUE
            ELSE
             DO 330 I=1,15
C               K = LONGLU(MOTPRO(I))
                K = LONPRO(I)
                IF(K.GT.0.AND.LIGNE2(1:K).EQ.MOTPRO(I)(1:K)) THEN
                  LISUIV = .FALSE.
                  GO TO 96
                ENDIF
 330         CONTINUE
            ENDIF
C
C GETS TO THIS POINT IF/WHEN HAS TO READ THE NEXT LINE
C
        ICOL2 =PRECAR (1 , LIGNE2 , ' ' , TABUL ,' ') - 1
C
        LGLU = LGLU + ICOL2
C
        IF (LGLU.GT.LGVAR) THEN
             ERREUR = .TRUE.
             IF (LONGLU(LIGNED).GT.0) THEN
               LIGNED = LIGNED(1:LONGLU(LIGNED))//LIGNE2(1:ICOL2)
             ELSE
               LIGNED = LIGNE2(1:ICOL2)
             ENDIF
             IF (LGLU.GT.0) WRITE(LU,'(1X,A)') LIGNED(1:LGLU)
             WRITE(LU,*) ' '
             IF (LNG.EQ.1) THEN
               WRITE(LU,'(1X,A6,I4,1X,A27)') 'LIGNE: ',NLIGN,
     &                'ERREUR : CHAINE TROP LONGUE'
             ELSEIF (LNG.EQ.2) THEN
               WRITE(LU,'(1X,A5,I4,1X,A23)') 'LINE: ',NLIGN,
     &                'ERROR : STRING TOO LONG'
             ENDIF
             ICOL = ICOL -1
             GO TO 1000
         ELSE
C NEEDS TO READ ANOTHER LINE - SIMULATES A SHIFT OF LINE
             LISUIV = .FALSE.
             LIGNE = LIGNE2
             IF (LONGLU(LIGNED).GT.0) THEN
               LIGNED = LIGNED(1:LONGLU(LIGNED))//LIGNE2(1:LONGLI)
             ELSE
              LIGNED = LIGNE2(1:LONGLI)
             ENDIF
             NLIGN = NLIGN2
             ICOL = ICOL2
             IFIN = LONGLI+1
             GO TO 290
        ENDIF
  96    IF (LISUIV) THEN
            IF (LONGLU(LIGNED).GT.0) THEN
               LIGNED = LIGNED(1:LONGLU(LIGNED))//LIGNE2(1:ICOL2)
             ELSE
              LIGNED = LIGNE2(1:LONGLI)
             ENDIF
            IFIN = LGLU+ICOL2
            IDEB = 1
        ENDIF
       ENDIF
C
           GO TO 901
 900       CONTINUE
           RETOUR = .TRUE.
 901       CONTINUE
           DO 90 I = 1 , LGLU
             IF (LIGNED(I:I).EQ.QUOTE.OR.LIGNED(I:I).EQ.'&'.OR.
     &          LIGNED(I:I).EQ.'='.OR.LIGNED(I:I).EQ.':'.OR.
     &          LIGNED(I:I).EQ.'/') THEN
                IF (NLIGN2.NE.NLIGN.AND.(.NOT.(LUFIC)))
     &                 WRITE(LU,'(1X,A)') LIGNE2(1:LONGLI)
                IF (LGLU.GT.0) WRITE(LU,'(1X,A)') LIGNED(1:LGLU)
              IF(LNG.EQ.1) THEN
                WRITE(LU,'(1X,A6,I4,A45,A)') 'LIGNE: ',NLIGN,
     &         ' ERREUR : CARACTERE INTERDIT DANS UNE CHAINE ',
     &         'SANS APOSTROPHES'
              ENDIF
              IF(LNG.EQ.2) THEN
                  WRITE(LU,'(1X,A5,I4,A)') 'LINE: ',NLIGN,
     &         ' ERROR: UNEXPECTED CHARACTER IN A STRING WITHOUT QUOTES'
              ENDIF
              ERREUR = .TRUE.
              GO TO 1000
            ENDIF
90         CONTINUE
C
      ELSE
C
C CASE WHERE THERE ARE QUOTES
C
           IDEB = ICOL + 1
C
C THE 1ST QUOTE IS IN LAST POSITION (QCAS=4 OR QCAS=5)
           IF (ICOL.EQ.LONGLI) QCAS=45
C
 100       ICOL   = PRECAR ( ICOL+1 , LIGNE , EXTREM , EXTREM , EXTREM )
           IF (ICOL.EQ.LONGLI) ICOL = LONGLI+1
C
C CASE WHERE DOUBLE QUOTES CAN BE FOUND IN THE 1ST LINE EXCEPT IN COLUMN 72
C
           IF(ICOL.LT.LONGLI) THEN
           IF(LIGNE(ICOL+1:ICOL+1).EQ.EXTREM.AND.EXTREM.EQ.QUOTE) THEN
              ICOL = ICOL + 1
C THE QUOTE IN 72 IS THE 2ND QUOTE OF A DOUBLE QUOTE (QCAS=3)
              IF (ICOL.EQ.LONGLI) QCAS=3
              COTE = .TRUE.
              GO TO 100
           ENDIF
           ENDIF
C
           LGLU = MAX(0,ICOL-IDEB)
           IF (LGLU.GT.0) LIGNED = LIGNE(IDEB:ICOL-1)
C
C HAS NOT FOUND THE END, OR A QUOTE WAS FOUND IN COLUMN 72
C
           IF (ICOL.GT.LONGLI) THEN
390             LISUIV = .TRUE.
                LUFIC = .TRUE.
                READ(NFIC,END=905,ERR=998,FMT='(A)') LIGNE2
C
C CASE WHERE THE PRECEDING LINE ENDS WITH A QUOTE
C
                IF (LIGNE(LONGLI:LONGLI).EQ.QUOTE) THEN
C THE QUOTE IN COLUMN 72 STARTS A STRING, OR IS THE 2ND OF A DOUBLE QUOTE
                  IF (QCAS.EQ.45.OR.QCAS.EQ.3) THEN
                      QCAS=0
                  ELSEIF (LIGNE2(1:1).EQ.QUOTE) THEN
                    COTE = .TRUE.
                    LUCOTE = .TRUE.
                    QCAS=0
                 ELSE
                    LGLU=LGLU-1
                    IF (LGLU.GT.0) LIGNED = LIGNED(1:LGLU)
                    LISUIV = .FALSE.
                    QCAS=0
                    GO TO 920
                  ENDIF
                ENDIF
C
                ICOL2 = 0
                IF (LIGNE2(1:1).EQ.QUOTE.AND.LUCOTE) THEN
                   LUCOTE = .FALSE.
                   ICOL2=1
                ENDIF
 110            ICOL2 =PRECAR (ICOL2+1,LIGNE2,EXTREM,EXTREM,EXTREM)
                IF(ICOL2.LT.LONGLI) THEN
                IF(LIGNE2(ICOL2+1:ICOL2+1).EQ.
     &             EXTREM.AND.EXTREM.EQ.QUOTE) THEN
C                   ICOL2 = PRECAR(ICOL2+1,LIGNE2,EXTREM,EXTREM,EXTREM)
                    ICOL2=ICOL2+1
                    COTE=.TRUE.
                    IF (ICOL2.EQ.LONGLI) QCAS=3
                   GO TO 110
                ENDIF
                ENDIF
                IF (ICOL2.EQ.LONGLI) ICOL2=ICOL2+1
                IF (LGLU.GT.0) THEN
                  LIGNED = LIGNED(1:LGLU)//LIGNE2(1:ICOL2-1)
                ELSE
                  LIGNED = LIGNE2(1:ICOL2-1)
                ENDIF
                LGLU = LGLU + ICOL2-1
C
                IF (LGLU.GT.LGVAR) GO TO 910
C
C GOES TO NEXT LINE IF NOT COMPLETE, OR IF HAS FOUND A QUOTE IN 72
C
                IF (ICOL2.GE.LONGLI) THEN
                  LISUIV = .FALSE.
                  LIGNE = LIGNE2
                  NLIGN = NLIGN2
                  ICOL = ICOL2
                  IFIN = ICOL2
                  GO TO 390
                ENDIF
C HERE IT'S OK
                GO TO 920
C
 905            CONTINUE
                RETOUR = .TRUE.
C
 910            CONTINUE
                WRITE(LU,'(1X,A)') LIGNED(1:MAX(1,LGLU))
                WRITE(LU,*)
                IF(LNG.EQ.1) THEN
                WRITE(LU,'(1X,A6,I4,A)') 'LIGNE: ',NLIGN,
     &         ' ERREUR : COTE MANQUANTE EN FIN DE CHAINE DE CARACTERES'
                WRITE(LU,*)'OU CHAINE TROP LONGUE ... '
                ENDIF
                IF(LNG.EQ.2) THEN
                WRITE(LU,'(1X,A5,I4,A)') 'LINE: ',NLIGN,
     &         ' ERROR: QUOTE MISSING AT THE END OF THE STRING'
                WRITE(LU,*)'OR STRING TOO LONG ... '
                ENDIF
                ERREUR = .TRUE.
                ICOL = LONGLI
                GO TO 1000
C
            ENDIF
           IFIN   = ICOL - 1
      ENDIF
C
 920  CONTINUE
      IF ( LGLU.NE.0  ) THEN
           LCAR = MIN(LGLU,LGVAR)
           CARLU = LIGNED(1:LGLU)
      ENDIF
C
C  CHANGES DOUBLE QUOTES WITH SIMPLE QUOTES
C
      IF(COTE) THEN
         NCAR = LCAR
         I = 1
 200     CONTINUE
         IF(I.GT.NCAR) THEN
            LCAR = NCAR
            GO TO 1000
         ENDIF
         IF(CARLU(I:I).EQ.QUOTE.AND.CARLU(I+1:I+1).EQ.QUOTE) THEN
            CARLU(I+1:LCAR) = CARLU(I+2:LCAR)//' '
            NCAR = NCAR - 1
         ENDIF
         I = I + 1
         GO TO 200
      ENDIF
C
1000  CONTINUE
C
      IF (LUFIC) THEN
        NLIGN = NLIGN + 1
        LIGNE = LIGNE2
        IF (LISUIV) THEN
          ICOL = ICOL2
        ELSE
          ICOL = 0
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
C
998   CONTINUE
      IF(LNG.EQ.1) WRITE(6,999) NFIC,NLIGN+1
      IF(LNG.EQ.2) WRITE(6,1999) NFIC,NLIGN+1
999   FORMAT(1X,'UNITE LOGIQUE ',1I2,'   ERREUR LIGNE ',1I6)
1999  FORMAT(1X,'LOGICAL UNIT ',1I2,'   ERROR LINE ',1I6)
      RETOUR = .TRUE.
      RETURN
C
      END

C
C#######################################################################
C