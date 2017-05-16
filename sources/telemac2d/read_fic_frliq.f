!                    *************************
                     SUBROUTINE READ_FIC_FRLIQ
!                    *************************
!
     &( Q , WHAT , AT , NFIC , LISTIN , FOUND )
!
!***********************************************************************
! TELEMAC2D   V7P2
!***********************************************************************
!
!brief    READS AND INTERPOLATES VALUES FROM THE LIQUID BOUNDARY FILE.
!
!history  J-M HERVOUET (LNHE)
!+        10/08/2009
!+        V6P0
!+
!
!history  J-M HERVOUET (LNHE)
!+        28/06/2010
!+        V6P0
!+   SIZE OF LIGN PARAMETERIZED (SEE SIZELIGN)
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  C. COULET (ARTELIA GROUP)
!+        08/11/2011
!+        V6P2
!+   Modification size WHAT and CHOIX_RFF due to modification of TRACER
!
!history  U.H. Merkel (BAW)
!+        17/07/2012
!+        V6P2
!+   NAG: MAXVAL intrinsic! -> MAXVALUE_RFF
!
!history  J-M HERVOUET (LNHE)
!+        13/12/2012
!+        V6P3
!+   Now works with tabs as well as spaces as delimiters.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        15/07/2016
!+        V7P2
!+   Now controlling that the time of a new line is greater than the
!+   time of the previous line.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME_RFF IN SECONDS
!| LISTIN         |-->| IF YES, PRINTS INFORMATION
!| NFIC           |-->| LOGICAL UNIT OF FILE
!| Q              |<--| VARIABLE READ AND INTERPOLATED
!| FOUND          |<--| IF FALSE: VARIABLE NOT FOUND
!| WHAT           |-->| VARIABLE TO LOOK FOR IN 9 CHARACTERS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TELEMAC2D, ONLY : INFIC_RFF,TIME_RFF,CHOIX_RFF,
     &                                   IL1_RFF,IL2_RFF,TL1_RFF,
     &                                   TL2_RFF,NVALUE_RFF,
     &                                   LASTWHAT_RFF,LASTAT_RFF,
     &                                   NLIG_RFF,MAXVALUE_RFF,DEJA_RFF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=9), INTENT(IN)       :: WHAT
      DOUBLE PRECISION, INTENT(IN)       :: AT
      DOUBLE PRECISION, INTENT(INOUT)    :: Q
      INTEGER         , INTENT(IN)       :: NFIC
      LOGICAL         , INTENT(IN)       :: LISTIN
      LOGICAL         , INTENT(OUT)      :: FOUND
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!     MAXIMUM NUMBER OF CHARACTERS PER LIGN (MAY BE CHANGED)
!
      INTEGER, PARAMETER :: SIZELIGN = 3000
!
      INTEGER IVALUE,ILIG,OK,J,IWHAT,IDEB,IFIN
      DOUBLE PRECISION TETA
      DOUBLE PRECISION, PARAMETER :: TOL = 1.D-3
!
      CHARACTER(LEN=SIZELIGN) :: LIGNE
!
      INTRINSIC ABS,CHAR
!
!-----------------------------------------------------------------------
!
!     1) (AT FIRST CALL)
!        READS THE LIQUID BOUNDARY FILE
!        INITIALISES CURRENT LINES AND INTERVAL OF TIME_RFF
!
      IF(.NOT.DEJA_RFF) THEN
        REWIND(NFIC)
!       SKIPS COMMENTS
1       READ(NFIC,FMT='(A)',ERR=10) LIGNE
        GO TO 20
10      CONTINUE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'ERREUR DE LECTURE DANS LE'
          WRITE(LU,*) 'FICHIER DES FRONTIERES LIQUIDES'
          WRITE(LU,*) 'SANS DOUTE UN PROBLEME DE FORMAT DE FICHIER'
          WRITE(LU,*) 'RETOURS CHARRIOTS WINDOWS SUR UNIX OU LINUX'
          WRITE(LU,*) 'LIGNE FAUTIVE :'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'READ ERROR IN THE'
          WRITE(LU,*) 'LIQUID BOUNDARIES FILE'
          WRITE(LU,*) 'PROBABLY A PROBLEM OF FORMAT'
          WRITE(LU,*) 'ANY WINDOWS CARRIAGE RETURNS ON UNIX OR LINUX'
          WRITE(LU,*) 'GUILTY LINE:'
        ENDIF
        WRITE(LU,*) LIGNE
        CALL PLANTE(1)
        STOP
20      CONTINUE
        IF(LIGNE(1:1).EQ.'#') GO TO 1
!
!       FINDS OUT WHAT AND HOW MANY VALUES ARE GIVEN IN THE FILE
!
        NVALUE_RFF = -1
        IFIN = 1
40      IDEB = IFIN
!
!       IDENTIFIES FIRST CHARACTER OF NAME
!
!       SKIPPING SPACES AND TABS
50      IF((LIGNE(IDEB:IDEB).EQ.' '.OR.LIGNE(IDEB:IDEB).EQ.CHAR(9))
     &     .AND.IDEB.LT.SIZELIGN) THEN
          IDEB=IDEB+1
          GO TO 50
        ENDIF
!       IDENTIFIES LAST CHARACTER OF NAME
        IFIN = IDEB
60      IF(LIGNE(IFIN:IFIN).NE.' '.AND.LIGNE(IFIN:IFIN).NE.CHAR(9)
     &     .AND.IFIN.LT.SIZELIGN) THEN
          IFIN=IFIN+1
          GO TO 60
        ENDIF
!
        IF(IDEB.EQ.IFIN) GO TO 4
!
        NVALUE_RFF = NVALUE_RFF + 1
        IF(NVALUE_RFF.EQ.0) THEN
          IF(LIGNE(IDEB:IFIN-1).NE.'T') THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'LA PREMIERE VARIABLE DOIT ETRE LE TEMPS T'
            WRITE(LU,*) 'DANS LE FICHIER DES FRONTIERES LIQUIDES'
            WRITE(LU,*) 'AUTRE CAUSE POSSIBLE :'
            WRITE(LU,*) 'IL Y A DES TABULATIONS DANS LE FICHIER'
            WRITE(LU,*) 'CHANGER LES TABULATIONS EN ESPACES'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'FIRST VALUE MUST BE TIME_RFF, DENOTED T'
            WRITE(LU,*) 'IN FILE OF LIQUID BOUNDARIES'
            WRITE(LU,*) 'OTHER POSSIBLE CAUSE:'
            WRITE(LU,*) 'THERE ARE TABS IN THE FILE'
            WRITE(LU,*) 'CHANGE TABS INTO SPACES'
          ENDIF
          CALL PLANTE(1)
          STOP
          ENDIF
        ELSEIF(NVALUE_RFF.LE.MAXVALUE_RFF) THEN
          CHOIX_RFF(NVALUE_RFF)='         '
          CHOIX_RFF(NVALUE_RFF)(1:IFIN-IDEB+1)=LIGNE(IDEB:IFIN-1)
        ELSE
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'AUGMENTER MAXVALUE_RFF DANS READ_FIC_FRLIQ'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'INCREASE MAXVALUE_RFF IN READ_FIC_FRLIQ'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(IFIN.LT.SIZELIGN) GO TO 40
!
!       SKIPS THE LINE WITH UNITS OR NAMES
4       READ(NFIC,FMT='(A)',ERR=10) LIGNE
        IF(LIGNE(1:1).EQ.'#') GO TO 4
!
!       COUNTS LINES OF DATA
        NLIG_RFF = 0
998     READ(NFIC,*,END=1000,ERR=999) LIGNE
        IF(LIGNE(1:1).NE.'#') NLIG_RFF=NLIG_RFF+1
        GO TO 998
999     CONTINUE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'ERREUR DE LECTURE SUR LE FICHIER DES FRONTIERES'
          WRITE(LU,*) 'LIQUIDES A LA LIGNE DE DONNEES : ',NLIG_RFF
          WRITE(LU,*) '(COMMENTAIRES NON COMPTES)'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'READING ERROR ON THE LIQUID BOUNDARIES FILE'
          WRITE(LU,*) 'AT LINE OF DATA : ',NLIG_RFF
          WRITE(LU,*) '(COMMENTS EXCLUDED)'
        ENDIF
        CALL PLANTE(1)
        STOP
1000    CONTINUE
!
!       DYNAMICALLY ALLOCATES TIME_RFF AND INFIC_RFF
!
        ALLOCATE(TIME_RFF(NLIG_RFF),STAT=OK)
        IF(OK.NE.0) WRITE(LU,*) 'MEMORY ALLOCATION ERROR FOR TIME_RFF'
        ALLOCATE(INFIC_RFF(NVALUE_RFF,NLIG_RFF),STAT=OK)
        IF(OK.NE.0) WRITE(LU,*) 'MEMORY ALLOCATION ERROR FOR INFIC_RFF'
!
!       FINAL READ OF TIME_RFF AND INFIC_RFF
!
        REWIND(NFIC)
!       SKIPS COMMENTS AND FIRST TWO MANDATORY LINES
2       READ(NFIC,FMT='(A)') LIGNE
        IF(LIGNE(1:1).EQ.'#') GO TO 2
        READ(NFIC,FMT='(A)') LIGNE
!
        DO ILIG=1,NLIG_RFF
3         READ(NFIC,FMT='(A)') LIGNE
          IF(LIGNE(1:1).EQ.'#') THEN
            GO TO 3
          ELSE
            BACKSPACE(NFIC)
!           COMPILERS SKIP SPACES AS WELL AS TABS
            READ(NFIC,*) TIME_RFF(ILIG),
     &                (INFIC_RFF(IVALUE,ILIG),IVALUE=1,NVALUE_RFF)
          ENDIF
        ENDDO
!
        CLOSE(NFIC)
        DEJA_RFF = .TRUE.
!
        IL1_RFF = 1
        IL2_RFF = 2
        TL1_RFF = TIME_RFF(1)
        TL2_RFF = TIME_RFF(2)
!
        LASTWHAT_RFF = '         '
        LASTAT_RFF = 0.D0
!
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LE FICHIER DES FRONTIERES LIQUIDES CONTIENT'
          WRITE(LU,*) NLIG_RFF,' LIGNES AVEC :'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'THE LIQUID BOUNDARIES FILE CONTAINS'
          WRITE(LU,*) NLIG_RFF,' LINES WITH:'
        ENDIF
        WRITE(LU,*) (CHOIX_RFF(IVALUE),IVALUE=1,NVALUE_RFF)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     2) INTERPOLATES THE DATA TO GET THE CORRECT TIME_RFF
!
!     2.A) FINDS THE ADDRESS IN THE ARRAY OF STORED DATA
!
!     2.B) INTERPOLATES DATA FROM THE ARRAY INFIC_RFF
!
!-----------------------------------------------------------------------
!
!
!     WHICH VARIABLE ?
      IWHAT = 0
      DO J=1,NVALUE_RFF
        IF(WHAT.EQ.CHOIX_RFF(J)) IWHAT=J
      ENDDO
      IF(IWHAT.EQ.0) THEN
        FOUND=.FALSE.
        RETURN
      ENDIF
!
70    IF(AT.GE.TL1_RFF-TOL.AND.AT.LE.TL2_RFF+TOL) THEN
        IF(TL2_RFF.GT.TL1_RFF) THEN
          TETA = (AT-TL1_RFF)/(TL2_RFF-TL1_RFF)
        ELSE
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'ERREUR, LE TEMPS ',TL2_RFF
            WRITE(LU,*) 'EST INFERIEUR OU EGAL A CELUI'
            WRITE(LU,*) 'DE LA LIGNE PRECEDENTE : ',TL1_RFF
            WRITE(LU,*) 'DANS LE FICHIER DES FRONTIERES LIQUIDES'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'ERROR, TIME ',TL2_RFF
            WRITE(LU,*) 'IS LESS OR EQUAL TO THE TIME'
            WRITE(LU,*) 'OF THE PREVIOUS LINE: ',TL1_RFF
            WRITE(LU,*) 'IN THE FILE OF LIQUID BOUNDARIES'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSE
        DO J=1,NLIG_RFF-1
          IF(AT.GE.TIME_RFF(J)-TOL.AND.AT.LE.TIME_RFF(J+1)+TOL) THEN
            TL1_RFF=TIME_RFF(J)
            TL2_RFF=TIME_RFF(J+1)
            IL1_RFF=J
            IL2_RFF=J+1
            GO TO 70
          ENDIF
        ENDDO
        IL1_RFF=IL2_RFF
        IL2_RFF=IL2_RFF+1
        IF(IL2_RFF.GT.NLIG_RFF) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'T=',AT,' HORS LIMITES'
            WRITE(LU,*) 'DU FICHIER DES FRONTIERES LIQUIDES'
            WRITE(LU,*) 'NOMBRE DE LIGNES : ',NLIG_RFF
            WRITE(LU,*) 'SUR CERTAINS COMPILATEURS'
            WRITE(LU,*) 'IL FAUT AJOUTER UNE LIGNE VIDE'
            WRITE(LU,*) 'A LA FIN DU FICHIER'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'T=',AT,' OUT OF RANGE'
            WRITE(LU,*) 'OF THE FILE OF LIQUID BOUNDARIES'
            WRITE(LU,*) 'NUMBER OF LINES : ',NLIG_RFF
            WRITE(LU,*) 'SOME COMPILERS REQUIRE AN'
            WRITE(LU,*) 'EMPTY LINE AT THE END OF THE FILE'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
        TL1_RFF=TIME_RFF(IL1_RFF)
        TL2_RFF=TIME_RFF(IL2_RFF)
        GO TO 70
      ENDIF
!
      Q = (1.D0-TETA)*INFIC_RFF(IWHAT,IL1_RFF)
     &  +       TETA *INFIC_RFF(IWHAT,IL2_RFF)
!
      FOUND=.TRUE.
!
!     PRINTS ONLY IF NEW TIME_RFF OR NEW VALUE IS ASKED
!
      IF(LISTIN) THEN
        IF(ABS(AT-LASTAT_RFF).GT.TOL.OR.LASTWHAT_RFF.NE.WHAT) THEN
          IF(LNG.EQ.1) WRITE(LU,*) 'FRONTIERE LIQUIDE : ',WHAT,'=',Q
          IF(LNG.EQ.2) WRITE(LU,*) 'LIQUID BOUNDARY: ',WHAT,'=',Q
        ENDIF
      ENDIF
      LASTAT_RFF=AT
      LASTWHAT_RFF=WHAT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
