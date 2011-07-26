!                    ***************
                     PROGRAM DIFFSEL
!                    ***************
!
!
!***********************************************************************
! PARALLEL   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPARES 2 FILES SELAFIN.
!+                PRINTS THE MAXIMUM DEVIATION IF THE FILES ARE DIFFERENT.
!
!history  S. AUNAY
!+        **/08/1998
!+
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER NVAR,I,K,NELEM1,NPOIN1,NDP1,II1,II2,J
      INTEGER ITAB(100000),JTAB(100000),IT1(10),IT2(10)
      INTEGER NELEM2,NPOIN2,NDP2
!
      REAL  XTAB(100000),YTAB(100000)
      REAL R1, R2, EPSILON, EPSREF, EPSMAX,R
!
      CHARACTER*72 TITRE,TIT1,TIT2
      INTEGER I1,I2,J1,J2
!
      CHARACTER*32 C1_32,C2_32
      CHARACTER*40 FICNAM1, FICNAM2
!
!------ NAME OF THE 2 FILES TO COMPARE (SELAFIN)
!
      PRINT*,'NOM DU FICHIER SELAFIN 1'
      READ(5,5000) FICNAM1
5000  FORMAT(A)
!
      PRINT*,'NOM DU FICHIER SELAFIN 2'
      READ(5,5000) FICNAM2
      EPSREF = 1.E-10
      EPSMAX = 0.
      PRINT*,'                    FICHIERS SELAFIN : '
      PRINT*,'      - ', FICNAM1
      PRINT*,'      - ', FICNAM2
      PRINT*,' '
!
!------ EXPLOITATION OF THE FILES:
!
      OPEN (UNIT=2,FILE=FICNAM2,FORM='UNFORMATTED',
     &      STATUS='OLD',ERR=8010)
      OPEN (UNIT=1,FILE=FICNAM1,FORM='UNFORMATTED',
     &      STATUS='OLD',ERR=8000)
!
!------ #1 : TITLE
!
        READ (1) TIT1
        READ (2) TIT2
        IF ( TIT1 .EQ. TIT2 ) THEN
        PRINT *, ' #1 ... OK'
        ELSE
        STOP ' #2 ... ERREUR : DIFFERENT'
        ENDIF
!
!------ #2 : NBV_1 AND NBV_2
!
      READ (1) I1, I2
      READ (2) J1, J2
        IF (I1 .NE. J1) PRINT*, ' #2 ... ERREUR I1=',I1,
     &                                   ',  J1=',J1
        IF (I2 .NE. J2) PRINT*, ' #2 ... ERREUR I2=',I2,
     &                                   ',  J2=',J2
!
        PRINT *, ' #2 ... OK'
!
!------ #3 : NAMES AND UNITS
!
      NVAR = I1 + I2
      DO 100 I=1, NVAR
      READ(1) C1_32
      READ(2) C2_32
!
        IF (C1_32 .NE. C2_32) THEN
          PRINT*, ' #3 ... ERREUR C1_32=',C1_32,
     &                      ', C2_32=',C2_32
          STOP
        ENDIF
!
100   CONTINUE
        PRINT *, ' #3 ... OK,   NVAR=',NVAR
!
!------ #4 : 1,0,0,0,0,0,0,0,0,0,0
!
      READ(1)  ( IT1(K), K=1,10)
      READ(2)  ( IT2(K), K=1,10)
        DO 20 K=1, 10
        IF (IT1(K) .NE.IT2(K)) THEN
          PRINT*, ' #4 ... ERREUR IT1=',IT1(K),
     &                     ',   IT2=',IT2(K)
          STOP
        ENDIF
20      CONTINUE
!
      IF (IT1(10) .EQ. 1) THEN
      READ(1)  ( IT1(K), K=1,6)
      READ(2)  ( IT2(K), K=1,6)
        DO 30 K=1, 10
        IF (IT1(K) .NE. IT2(K)) THEN
          PRINT*, ' #4 ... ERREUR IT1=',IT1(K),
     &                     ',   IT2=',IT2(K)
          STOP
        ENDIF
30      CONTINUE
      ENDIF
      PRINT *, ' #4 ... OK'
!
!------ #5 : NELEM, NPOIN, NDP, 1
!
      READ (1) NELEM1, NPOIN1, NDP1, II1
      READ (2) NELEM2, NPOIN2, NDP2, II2
        IF ( NELEM1 .NE. NELEM2 )
     &     STOP ' #5 ... ERREUR : NELEM1<>NELEM2'
        IF ( NPOIN1 .NE. NPOIN2 )
     &     STOP ' #5 ... ERREUR : NPOIN1<>NPOIN2'
        IF ( NDP1 .NE. NDP2 )
     &     STOP ' #5 ... ERREUR : NDP1<>NDP2'
        IF ( II1 .NE. II2 )
     &     STOP ' #5 ... ERREUR : II1<>II2'
        PRINT *,' #5 ... OK,  NELEM=',NELEM1,',  NPOINT=',NPOIN1
!
!------ #6 : IKLE
!
      READ (1) (ITAB(I), I=1, NELEM1*NDP1)
      READ (2) (JTAB(I), I=1, NELEM1*NDP1)
        EPSILON=0.
        DO 50 K=1, NELEM1*NDP1
        IF (FLOAT(ABS(ITAB(K)-JTAB(K))) .GT. EPSMAX)
     &      EPSMAX=ABS(ITAB(K)-JTAB(K))
        IF (FLOAT(ABS(ITAB(K)-JTAB(K))) .GT. EPSILON)
     &          EPSILON = FLOAT(ABS(ITAB(K)-JTAB(K)) )
50        CONTINUE
        IF (EPSILON .GT. EPSREF) THEN
          PRINT*, ' #6 ... ERREUR : EPSILON = ', EPSILON
        ENDIF
        PRINT *, ' #6 ... OK'
!
!------ #7 : IPOBO
!
      READ (1) (ITAB(I), I=1, NPOIN1)
      READ (2) (JTAB(I), I=1, NPOIN1)
        EPSILON=0.
        DO 51 K=1, NPOIN1
        IF (ABS(ITAB(K)-JTAB(K)) .GT. EPSMAX)
     &      EPSMAX=ABS(ITAB(K)-JTAB(K))
        IF (ABS(ITAB(K)-JTAB(K)) .GT. EPSILON)
     &          EPSILON = ABS(ITAB(K)-JTAB(K) )
51      CONTINUE
        IF (EPSILON .GT. EPSREF) THEN
          PRINT*, ' #7 ... ERREUR IPOBO : EPSILON = ', EPSILON
        ENDIF
        PRINT *, ' #7 ... OK'
!
!------ #8 : X
!
!8/11: PB CRASHES HERE (" INVALID REAL")
!
      READ (1) (XTAB(I), I=1, NPOIN1)
      READ (2) (YTAB(I), I=1, NPOIN1)
        EPSILON=0.
        DO 52 K=1, NPOIN1
        IF (ABS(XTAB(K)-YTAB(K)) .GT. EPSMAX)
     &      EPSMAX=ABS(XTAB(K)-YTAB(K))
        IF (ABS(XTAB(K)-YTAB(K)) .GT. EPSILON)
     &          EPSILON = ABS(XTAB(K)-YTAB(K) )
52        CONTINUE
        IF (EPSILON .GT. EPSREF) THEN
          PRINT*, ' #8 ... ERREUR : EPSILON = ', EPSILON
          STOP
        ENDIF
!
        PRINT *, ' #8 ... OK'
!
!------ #9 : Y
!
      READ (1) (XTAB(I), I=1, NPOIN1)
      READ (2) (YTAB(I), I=1, NPOIN1)
        EPSILON=0.
        DO 200 K=1, NPOIN1
        IF (ABS(XTAB(K)-YTAB(K)) .GT. EPSMAX)
     &      EPSMAX=ABS(XTAB(K)-YTAB(K))
        IF (ABS(XTAB(K)-YTAB(K)) .GT. EPSILON)
     &          EPSILON = ABS(XTAB(K)-YTAB(K) )
200        CONTINUE
        IF (EPSILON .GT. EPSREF) THEN
          PRINT*, ' #9 ... ERREUR : EPSILON = ', EPSILON
          STOP
        ENDIF
        PRINT *, ' #9 ... OK'
!
!------ #10 : T
!
800   CONTINUE
!
       READ(1, END=9999) R1
       READ(2) R2
         IF (ABS(R1-R2) .GT. EPSREF) THEN
           PRINT*, '# 10 ... ERREUR : EPSILON = ', ABS(R1-R2), ',
     &           T1=', R1, ', T2=',R2
           STOP
         ENDIF
         PRINT*, '#10 ... T1=T2=', R1
!
!------ #9 : NVAR VECTORS
!
      IF ( NVAR .LT. 1) GOTO 800
      DO 850 J=1,NVAR
        READ (1) (XTAB(I), I=1, NPOIN1)
        READ (2) (YTAB(I), I=1, NPOIN1)
        EPSILON=0.
        DO 54 K=1, NPOIN1
          IF (ABS(XTAB(K)-YTAB(K)) .GT. EPSMAX)
     &      EPSMAX=ABS(XTAB(K)-YTAB(K))
          IF (ABS(XTAB(K)-YTAB(K)) .GT. EPSILON)
     &    EPSILON = ABS(XTAB(K)-YTAB(K) )
54          CONTINUE
          IF (EPSILON .GT. EPSREF) THEN
            PRINT*, ' #11 ... ERREUR : EPSILON = ', EPSILON, ', T = ', R1
          ENDIF
850   CONTINUE
!
      GOTO 800
!
!------ ERRORS
!
8000  STOP 'ERREUR OUVERTURE FICHIER 1'
8010  STOP 'ERREUR OUVERTURE FICHIER 2'
!
!------- END : CLOSES THE FILES
!
9999  CONTINUE
      PRINT*,' '
      PRINT*, '   -> EPSILON MAX GLOBAL : ', EPSMAX
      CLOSE (1)
      CLOSE (2)
      END
