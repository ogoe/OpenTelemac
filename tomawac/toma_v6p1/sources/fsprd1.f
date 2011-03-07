!                    *****************
                     SUBROUTINE FSPRD1
!                    *****************
!
     &( FRA   , DIREC , NPLAN , SPRED1, TETA1 , SPRED2, TETA2 , XLAMDA,
     &  DEUPI )
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE BIMODAL DIRECTIONAL SPREADING FUNCTION
!+                FOR A RANGE OF DIRECTIONS.
!code
!+              2S
!+           COS  (T-T0)    WHERE T IN (T0-PI/2;T0+PI/2)
!
!history  M. BENOIT
!+        15/11/95
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| DEUPI          |-->| 2.PI
!| DIREC          |---|
!| FRA            |---|
!| NPLAN          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
!| SPRED1         |-->| ETALEMENT DIRECTIONNEL 1 POUR FRA
!| SPRED2         |-->| ETALEMENT DIRECTIONNEL 2 POUR FRA
!| TETA1          |-->| DIRECTION PRINCIPALE 1 POUR FRA
!| TETA2          |-->| DIRECTION PRINCIPALE 2 POUR FRA
!| XLAMDA         |-->| FACTEUR DE PONDERATION POUR LA FRA
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
!
!.....EXTERNAL FUNCTIONS
!     """""""""""""""""
      DOUBLE PRECISION DELFRA
      EXTERNAL         DELFRA
!
!
      DELT1 = 1.D0/DELFRA(SPRED1,DEUPI)
      DELT2 = 1.D0/DELFRA(SPRED2,DEUPI)
!
      DO 110 JP=1,NPLAN
        FTH = DIREC(JP)
!
        ARGUM = COS(FTH-TETA1)
        IF (ARGUM.GT.0.D0) THEN
          FRA1=DELT1*ARGUM**(2.D0*SPRED1)
        ELSE
          FRA1=0.D0
        ENDIF
!
        ARGUM = COS(FTH-TETA2)
        IF (ARGUM.GT.0.D0) THEN
          FRA2=DELT2*ARGUM**(2.D0*SPRED2)
        ELSE
          FRA2=0.D0
        ENDIF
!
        FRA(JP)=XLAMDA*FRA1+(1.D0-XLAMDA)*FRA2
        IF (FRA(JP).LT.1.D-9) FRA(JP)=0.D0
  110 CONTINUE
!
      RETURN
      END