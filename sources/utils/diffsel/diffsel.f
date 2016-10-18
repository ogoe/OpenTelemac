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
      REAL R1, R2, CHOUIA, EPSREF, EPSMAX
!
      CHARACTER(LEN=72) TIT1,TIT2
      INTEGER I1,I2,J1,J2
!
      CHARACTER(LEN=32) C1_32,C2_32
      CHARACTER(LEN=40) FICNAM1, FICNAM2
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
          WRITE(*,*) ' #2 ... ERREUR : DIFFERENT'
          CALL PLANTE(1)
          STOP
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
      DO I=1, NVAR
      READ(1) C1_32
      READ(2) C2_32
!
        IF (C1_32 .NE. C2_32) THEN
          PRINT*, ' #3 ... ERREUR C1_32=',C1_32,
     &                      ', C2_32=',C2_32
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDDO ! I
        PRINT *, ' #3 ... OK,   NVAR=',NVAR
!
!------ #4 : 1,0,0,0,0,0,0,0,0,0,0
!
      READ(1)  ( IT1(K), K=1,10)
      READ(2)  ( IT2(K), K=1,10)
        DO K=1, 10
        IF (IT1(K) .NE.IT2(K)) THEN
          PRINT*, ' #4 ... ERREUR IT1=',IT1(K),
     &                     ',   IT2=',IT2(K)
          CALL PLANTE(1)
          STOP
        ENDIF
        ENDDO ! K
!
      IF (IT1(10) .EQ. 1) THEN
      READ(1)  ( IT1(K), K=1,6)
      READ(2)  ( IT2(K), K=1,6)
        DO K=1, 10
        IF (IT1(K) .NE. IT2(K)) THEN
          PRINT*, ' #4 ... ERREUR IT1=',IT1(K),
     &                     ',   IT2=',IT2(K)
          CALL PLANTE(1)
          STOP
        ENDIF
        ENDDO ! K
      ENDIF
      PRINT *, ' #4 ... OK'
!
!------ #5 : NELEM, NPOIN, NDP, 1
!
      READ (1) NELEM1, NPOIN1, NDP1, II1
      READ (2) NELEM2, NPOIN2, NDP2, II2
        IF ( NELEM1 .NE. NELEM2 )
     &    WRITE(*,*) ' #5 ... ERREUR : NELEM1<>NELEM2'
          CALL PLANTE(1)
          STOP
        IF ( NPOIN1 .NE. NPOIN2 )
     &    WRITE(*,*) ' #5 ... ERREUR : NPOIN1<>NPOIN2'
          CALL PLANTE(1)
          STOP
        IF ( NDP1 .NE. NDP2 )
     &    WRITE(*,*) ' #5 ... ERREUR : NDP1<>NDP2'
          CALL PLANTE(1)
          STOP
        IF ( II1 .NE. II2 )
     &    WRITE(*,*) ' #5 ... ERREUR : II1<>II2'
          CALL PLANTE(1)
          STOP
        PRINT *,' #5 ... OK,  NELEM=',NELEM1,',  NPOINT=',NPOIN1
!
!------ #6 : IKLE
!
      READ (1) (ITAB(I), I=1, NELEM1*NDP1)
      READ (2) (JTAB(I), I=1, NELEM1*NDP1)
        CHOUIA=0.
        DO K=1, NELEM1*NDP1
        IF (FLOAT(ABS(ITAB(K)-JTAB(K))) .GT. EPSMAX)
     &      EPSMAX=ABS(ITAB(K)-JTAB(K))
        IF (FLOAT(ABS(ITAB(K)-JTAB(K))) .GT. CHOUIA)
     &          CHOUIA = FLOAT(ABS(ITAB(K)-JTAB(K)) )
        ENDDO ! K
        IF (CHOUIA .GT. EPSREF) THEN
          PRINT*, ' #6 ... ERREUR : CHOUIA = ', CHOUIA
        ENDIF
        PRINT *, ' #6 ... OK'
!
!------ #7 : IPOBO
!
      READ (1) (ITAB(I), I=1, NPOIN1)
      READ (2) (JTAB(I), I=1, NPOIN1)
        CHOUIA=0.
        DO K=1, NPOIN1
        IF (ABS(ITAB(K)-JTAB(K)) .GT. EPSMAX)
     &      EPSMAX=ABS(ITAB(K)-JTAB(K))
        IF (ABS(ITAB(K)-JTAB(K)) .GT. CHOUIA)
     &          CHOUIA = ABS(ITAB(K)-JTAB(K) )
        ENDDO ! K
        IF (CHOUIA .GT. EPSREF) THEN
          PRINT*, ' #7 ... ERREUR IPOBO : CHOUIA = ', CHOUIA
        ENDIF
        PRINT *, ' #7 ... OK'
!
!------ #8 : X
!
!8/11: PB CRASHES HERE (" INVALID REAL")
!
      READ (1) (XTAB(I), I=1, NPOIN1)
      READ (2) (YTAB(I), I=1, NPOIN1)
        CHOUIA=0.
        DO K=1, NPOIN1
        IF (ABS(XTAB(K)-YTAB(K)) .GT. EPSMAX)
     &      EPSMAX=ABS(XTAB(K)-YTAB(K))
        IF (ABS(XTAB(K)-YTAB(K)) .GT. CHOUIA)
     &          CHOUIA = ABS(XTAB(K)-YTAB(K) )
        ENDDO ! K
        IF (CHOUIA .GT. EPSREF) THEN
          PRINT*, ' #8 ... ERREUR : CHOUIA = ', CHOUIA
          CALL PLANTE(1)
          STOP
        ENDIF
!
        PRINT *, ' #8 ... OK'
!
!------ #9 : Y
!
      READ (1) (XTAB(I), I=1, NPOIN1)
      READ (2) (YTAB(I), I=1, NPOIN1)
        CHOUIA=0.
        DO K=1, NPOIN1
        IF (ABS(XTAB(K)-YTAB(K)) .GT. EPSMAX)
     &      EPSMAX=ABS(XTAB(K)-YTAB(K))
        IF (ABS(XTAB(K)-YTAB(K)) .GT. CHOUIA)
     &          CHOUIA = ABS(XTAB(K)-YTAB(K) )
        ENDDO ! K
        IF (CHOUIA .GT. EPSREF) THEN
          PRINT*, ' #9 ... ERREUR : CHOUIA = ', CHOUIA
          CALL PLANTE(1)
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
        PRINT*, '# 10 ... ERREUR : CHOUIA = ', ABS(R1-R2), ',
     &        T1=', R1, ', T2=',R2
        CALL PLANTE(1)
        STOP
      ENDIF
      PRINT*, '#10 ... T1=T2=', R1
!
!------ #9 : NVAR VECTORS
!
      IF ( NVAR .LT. 1) GOTO 800
      DO J=1,NVAR
        READ (1) (XTAB(I), I=1, NPOIN1)
        READ (2) (YTAB(I), I=1, NPOIN1)
        CHOUIA=0.
        DO K=1, NPOIN1
          IF (ABS(XTAB(K)-YTAB(K)) .GT. EPSMAX)
     &      EPSMAX=ABS(XTAB(K)-YTAB(K))
          IF (ABS(XTAB(K)-YTAB(K)) .GT. CHOUIA)
     &    CHOUIA = ABS(XTAB(K)-YTAB(K) )
        ENDDO ! K
        IF (CHOUIA .GT. EPSREF) THEN
          PRINT*, ' #11 ... ERREUR : CHOUIA = ', CHOUIA, ', T = ', R1
        ENDIF
      ENDDO ! J
!
      GOTO 800
!
!------ ERRORS
!
8000  WRITE(*,*) 'ERREUR OUVERTURE FICHIER 1'
      CALL PLANTE(1)
      STOP
8010  WRITE(*,*) 'ERREUR OUVERTURE FICHIER 2'
      CALL PLANTE(1)
      STOP
!
!------- END : CLOSES THE FILES
!
9999  CONTINUE
      PRINT*,' '
      PRINT*, '   -> CHOUIA MAX GLOBAL : ', EPSMAX
      CLOSE (1)
      CLOSE (2)
      END PROGRAM DIFFSEL
