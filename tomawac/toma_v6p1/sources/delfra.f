!                    ***************
                     FUNCTION DELFRA
!                    ***************
!
     &( SS    , DEUPI )
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE COEFFICIENT THAT NORMALISES THE DIRECTIONAL
!+                SPREADING FUNCTION IN COS **2.S (TETA-TETA0).
!code
!+                               GAMMA( SS + 0.5)
!+        DELFRA(SS) = SQRT(PI)  ----------------
!+                               GAMMA( SS + 1. )
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
!| SS             |-->| EXPOSANT DE LA FONCTION DE REPARTITION ANG.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      DOUBLE PRECISION DELFRA, SS    , DEUPI
!
!.....EXTERNAL FUNCTIONS
!     """"""""""""""""""
      DOUBLE PRECISION GAMMLN
      EXTERNAL         GAMMLN
!
!
      DELFRA=SQRT(DEUPI/2.D0)
     &      *EXP(GAMMLN(SS+0.5D0,DEUPI)-GAMMLN(SS+1.D0,DEUPI))
!
      RETURN
      END