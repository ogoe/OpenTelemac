!                    **********************
                     SUBROUTINE BIEF_VALIDA
!                    **********************
!
     &(VARREF,TEXTREF,UREF,REFFORMAT,VARRES,TEXTRES,URES,RESFORMAT,
     & MAXTAB,NP,IT,MAXIT,ACOMPARER)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    VALIDATES THE RESULTS AGAINST AN ANALYTICAL SOLUTION
!+                OR AGAINST RESULTS IN THE COMPUTATION REFERENCE FILE.
!
!note     THE LAST TIMESTEP ONLY IS COMPARED.
!note  EXCEPT FOR THE BOTTOM, ASSUMES THAT THE REFERENCE
!+         FILE DOES HOLD THE VARIABLES TO BE COMPARED.
!
!warning  THIS SUBROUTINE MUST BE MODIFIED ACCORDING TO EACH
!+            PARTICULAR CASE
!
!history  J-M HERVOUET (LNHE)
!+        05/08/2009
!+        V6P0
!+
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ACOMPARER      |-->| INDICATES WHICH VARIABLE TO COMPARE
!| IT             |-->| TIME STEP NUMBER
!| MAXIT          |-->| MAXIMUM NUMBER OF ITERATIONS
!| MAXTAB         |-->| TOTAL NUMBER OF VARIABLES
!| NP             |-->| NUMBER OF POINTS TO BE CHECKED
!| REFFORMAT      |-->| FORMAT OF REFERENCE FILE
!| RESFORMAT      |-->| FORMAT OF RESULTS FILE
!| TEXTREF        |-->| NAMES & UNITS OF VARIABLES IN THE REFERENCE FILE
!| TEXTRES        |-->| NAMES & UNITS OF VARIABLES IN THE RESULTS FILE
!| UREF           |-->| LOGICAL UNIT OF REFERENCE FILE
!| URES           |-->| LOGICAL UNIT OF RESULTS FILE
!| VARREF         |-->| BLOCK OF VARIABLES IN REFERENCE FILE 
!| VARRES         |-->| BLOCK OF VARIABLES IN RESULTS FILE 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF    !, EX_VALIDA => VALIDA
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NP,MAXTAB,IT,MAXIT,URES,UREF
      INTEGER, INTENT(IN) :: ACOMPARER(MAXTAB)
!
      CHARACTER(LEN=32), INTENT(IN) :: TEXTREF(MAXTAB),TEXTRES(MAXTAB)
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VARREF,VARRES
      CHARACTER(LEN=*), INTENT(IN)  :: REFFORMAT,RESFORMAT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IVAR,I,IREF,IRES,IERMAX
!
      DOUBLE PRECISION TIMEREF,TIMERES,ERMAX,HIST(1),ERR
!
      DOUBLE PRECISION P_DMAX
      EXTERNAL         P_DMAX
!
!-----------------------------------------------------------------------
!
      INTEGER FINDREF(500),FINDRES(500)
      IF(MAXTAB.GT.500) THEN
        WRITE(LU,*) 'WRONG SIZE OF FINDREF AND FINDRES IN BIEF_VALIDA'
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(IT.EQ.MAXIT) THEN
!
!  CALLS SUITE TO READ THE REFERENCE FILE
!
      IF(LNG.EQ.1) WRITE(LU,10)
      IF(LNG.EQ.2) WRITE(LU,11)
      CALL BIEF_SUITE(VARREF,VARREF,IREF,UREF,REFFORMAT,HIST,0,NP,
     &                TIMEREF,TEXTREF,TEXTREF,0,FINDREF,ACOMPARER,
     &                .TRUE.,.TRUE.,MAXTAB)
!
!  CALLS SUITE TO READ THE RESULTS FILE
!
      IF(LNG.EQ.1) WRITE(LU,12)
      IF(LNG.EQ.2) WRITE(LU,13)
      CALL BIEF_SUITE(VARRES,VARRES,IRES,URES,RESFORMAT,HIST,0,NP,
     &                TIMERES,TEXTRES,TEXTRES,0,FINDRES,ACOMPARER,
     &                .TRUE.,.TRUE.,MAXTAB)
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) WRITE(LU,14)
      IF(LNG.EQ.2) WRITE(LU,15)
!
      IF(ABS(TIMERES-TIMEREF).GT.1.D-4) THEN
        IF(LNG.EQ.1) WRITE(LU,16)
        IF(LNG.EQ.2) WRITE(LU,17)
      ENDIF
      IF(IRES.NE.IREF) THEN
        IF(LNG.EQ.1) WRITE(LU,18)
        IF(LNG.EQ.2) WRITE(LU,19)
      ENDIF
!
!-----------------------------------------------------------------------
!
!     LOOP ON THE VARIABLES TO COMPARE
!
      DO IVAR=1,MAXTAB
!
        IF(ACOMPARER(IVAR).EQ.1) THEN
!
!       COMPARES THE VARIABLE IVAR
!
          IF(FINDREF(IVAR).EQ.1.AND.FINDRES(IVAR).EQ.1) THEN
!
            ERMAX = 0.D0
            IERMAX = 1
            DO I = 1 , NP
              ERR=ABS(VARREF%ADR(IVAR)%P%R(I)-VARRES%ADR(IVAR)%P%R(I))
              IF(ERR.GT.ERMAX) THEN
                ERMAX=ERR
                IERMAX=I
              ENDIF
            ENDDO
!
            IF(NCSIZE.GT.1) ERMAX=P_DMAX(ERMAX)
            IF(LNG.EQ.1) WRITE(LU,60) TEXTRES(IVAR)(1:16),ERMAX
            IF(LNG.EQ.2) WRITE(LU,61) TEXTRES(IVAR)(1:16),ERMAX
!
          ELSEIF(FINDREF(IVAR).EQ.1) THEN
!
            IF(LNG.EQ.1) WRITE(LU,70) TEXTRES(IVAR)(1:16)
            IF(LNG.EQ.2) WRITE(LU,71) TEXTRES(IVAR)(1:16)
!
          ENDIF
!
        ENDIF
!
      ENDDO
!
      IF(LNG.EQ.1) WRITE(LU,50)
      IF(LNG.EQ.2) WRITE(LU,51)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
10    FORMAT(1X,////,1X,80('='),/,
     &       25X,' PROCEDURE DE VALIDATION ',/,
     &       1X,80('-'),//,
     &       1X,' 1) RELECTURE DU FICHIER DE REFERENCE :',/,
     &       1X,' --------------------------------------',/)
11    FORMAT(1X,////,1X,80('='),/,
     &       25X,' VALIDATION PROCEDURE ',/,
     &       1X,80('-'),//,
     &       1X,' 1) READING THE REFERENCE FILE :',/,
     &       1X,' ------------------------------',/)
12    FORMAT(1X,///,
     &       1X,' 2) RELECTURE DU FICHIER DE RESULTATS :',/,
     &       1X,' --------------------------------------',/)
13    FORMAT(1X,///,
     &       1X,' 2) READING THE RESULTS FILE :',/,
     &       1X,' --------------------------------',/)
14    FORMAT(1X,///,
     &       1X,' 3) COMPARAISON :',/,
     &       1X,' ----------------',/)
15    FORMAT(1X,///,
     &       1X,' 3) COMPARISON:',/,
     &       1X,' --------------',/)
16    FORMAT(1X,///,
     &       1X,' ATTENTION : TEMPS DIFFERENTS',/,
     &       1X,' ----------------------------',/)
17    FORMAT(1X,///,
     &       1X,' BEWARE: TIMES ARE DIFFERENT',/,
     &       1X,' ---------------------------',/)
18    FORMAT(1X,///,
     &       1X,' ATTENTION : NUMEROS D''ENREGISTREMENT DIFFERENTS',/,
     &       1X,' ------------------------------------------------',/)
19    FORMAT(1X,///,
     &       1X,' BEWARE: RECORD NUMBERS ARE DIFFERENT',/,
     &       1X,' ------------------------------------',/)
!
50    FORMAT(1X,80('-'),/,23X,'FIN DU COMPTE-RENDU DE VALIDATION',/,
     &       1X,80('='),////)
51    FORMAT(1X,80('-'),/,23X,'END OF VALIDATION REPORT',/,
     &       1X,80('='),////)
!
60    FORMAT(1X,'VARIABLE : ',A16,'  DIFFERENCE : ',G16.7,/)
61    FORMAT(1X,'VARIABLE: ' ,A16,'  DIFFERENCE: ',G16.7,/)
!
70    FORMAT(1X,'VARIABLE : ',A16,'  NON TROUVEE',/)
71    FORMAT(1X,'VARIABLE: ' ,A16,'  NOT FOUND'  ,/)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
