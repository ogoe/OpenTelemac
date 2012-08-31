!                    *****************
                     SUBROUTINE FSPRD2
!                    *****************
!
     &( FRA   , DIREC , NPLAN , SPRED1, TETA1 , SPRED2, TETA2 , XLAMDA,
     &  DEUPI )
!
!***********************************************************************
! TOMAWAC   V6P1                                   15/06/2011
!***********************************************************************
!
!brief    COMPUTES THE BIMODAL DIRECTIONAL SPREADING FUNCTION
!+                FOR A RANGE OF DIRECTIONS.
!code
!+           EXP -0.5((T-T0)/S)**2  WHERE T IN (T0-PI/2;T0+PI/2)
!
!history  M. BENOIT
!+        10/01/96
!+        V1P0
!+   CREATED
!
!history  M. BENOIT
!+        07/11/96
!+        V1P2
!+   MODIFIED
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
!history  G.MATTAROLO (EDF - LNHE)
!+        15/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DEUPI          |-->| 2.PI
!| DIREC          |-->| DISCRETIZED DIRECTION
!| FRA            |<--| DIRECTIONAL SPREADING FUNCTION VALUES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| SPRED1         |-->| DIRECTIONAL SPREAD 1
!| SPRED2         |-->| DIRECTIONAL SPREAD 1
!| TETA1          |-->| MAIN DIRECTION 1
!| TETA2          |-->| MAIN DIRECTION 2
!| XLAMDA         |-->| WEIGHTING FACTOR FOR FRA
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER  NPLAN
      DOUBLE PRECISION SPRED1, TETA1 , SPRED2, TETA2 , XLAMDA, DEUPI
      DOUBLE PRECISION FRA(NPLAN)    , DIREC(NPLAN)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  JP
      DOUBLE PRECISION DELT1 , DELT2 , FTH   , FRA1  , FRA2  , ARGUM
      DOUBLE PRECISION PI    , C1    , C2
!
!
      PI=DEUPI/2.D0
      IF (SPRED1.GT.1.D-4) THEN
        DELT1 = 1.D0/(SPRED1*SQRT(DEUPI))
        C1    = -0.5/(SPRED1*SPRED1)
      ELSE
        DELT1 = 0.D0
        C1    = 0.D0
      ENDIF
      IF (SPRED2.GT.1.D-4) THEN
        DELT2 = 1.D0/(SPRED2*SQRT(DEUPI))
        C2    = -0.5/(SPRED2*SPRED2)
      ELSE
        DELT2 = 0.D0
        C2    = 0.D0
      ENDIF
!
      DO 110 JP=1,NPLAN
        FTH = DIREC(JP)
!
        ARGUM = FTH-TETA1
   50   CONTINUE
        IF (ARGUM.LT.-PI) THEN
          ARGUM=ARGUM+DEUPI
          GOTO 50
        ENDIF
   60   CONTINUE
        IF (ARGUM.GT.PI) THEN
          ARGUM=ARGUM-DEUPI
          GOTO 60
        ENDIF
        FRA1=DELT1*EXP(MAX(-10.D0,C1*ARGUM*ARGUM))
!
        ARGUM = FTH-TETA2
   70   CONTINUE
        IF (ARGUM.LT.-PI) THEN
          ARGUM=ARGUM+DEUPI
          GOTO 70
        ENDIF
   80   CONTINUE
        IF (ARGUM.GT.PI) THEN
          ARGUM=ARGUM-DEUPI
          GOTO 80
        ENDIF
        FRA2=DELT2*EXP(MAX(-10.D0,C2*ARGUM*ARGUM))
!
        FRA(JP)=XLAMDA*FRA1+(1.D0-XLAMDA)*FRA2
        IF (FRA(JP).LT.1.D-10) FRA(JP)=0.D0
  110 CONTINUE
!
      RETURN
      END
