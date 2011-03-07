!                    *****************
                     SUBROUTINE FSPRD3
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
!+           COS  ((T-T0)/2.)    (MITSUYASU)
!
!history  M. BENOIT
!+        07/11/96
!+        V1P2
!+   CREATED 
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
      DOUBLE PRECISION ARGMI1, ARGMI2
!
!.....EXTERNAL FUNCTIONS
!     """""""""""""""""
      DOUBLE PRECISION DELFRA
      EXTERNAL         DELFRA
!
!
      DELT1 = 0.5D0/DELFRA(SPRED1,DEUPI)
      DELT2 = 0.5D0/DELFRA(SPRED2,DEUPI)
      IF (SPRED1.GT.1.D-1) THEN
        ARGMI1=10.D0**(-4.D0/SPRED1)
      ELSE
        ARGMI1=0.D0
      ENDIF
      IF (SPRED2.GT.1.D-1) THEN
        ARGMI2=10.D0**(-4.D0/SPRED2)
      ELSE
        ARGMI2=0.D0
      ENDIF
!
      DO 110 JP=1,NPLAN
        FTH = DIREC(JP)
!
        ARGUM = ABS(COS(0.5D0*(FTH-TETA1)))
        IF (ARGUM.GT.ARGMI1) THEN
          FRA1=DELT1*ARGUM**(2.D0*SPRED1)
        ELSE
          FRA1=0.D0
        ENDIF
!
        ARGUM = ABS(COS(0.5D0*(FTH-TETA2)))
        IF (ARGUM.GT.ARGMI2) THEN
          FRA2=DELT2*ARGUM**(2.D0*SPRED2)
        ELSE
          FRA2=0.D0
        ENDIF
!
        FRA(JP)=XLAMDA*FRA1+(1.D0-XLAMDA)*FRA2
        IF (FRA(JP).LT.1.D-10) FRA(JP)=0.D0
  110 CONTINUE
!
      RETURN
      END