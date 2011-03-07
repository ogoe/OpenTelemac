!                    *****************
                     SUBROUTINE ANAVEN
!                    *****************
!
     &( UV    , VV    , X     , Y     , NPOIN2, AT    , DDC   , VX_CTE,
     &  VY_CTE)
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    SPECIFIES AN ANALYTICAL WIND
!+               (CAN BE VARIABLE IN TIME).
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER; THIS IS MERELY AN EXAMPLE
!code
!+      DO IP=1,NPOIN2
!+        UV(IP)=VX_CTE
!+        VV(IP)=VY_CTE
!+      ENDDO
!
!history  M. BENOIT (LNH)
!+        07/06/95
!+        V1P0
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
!| AT             |-->| TEMPS DU CALCUL
!| DDC            |-->| DATE DE DEBUT DU CALCUL
!| NPOIN2         |-->| NOMBRE DE POINTS 2D
!| UV,VV          |<--| COMPOSANTES DU CHAMP DE VENT INITIAL
!| VX_CTE         |---|
!| VY_CTE         |---|
!| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE 2D
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER  NPOIN2
      DOUBLE PRECISION AT    , DDC   , VX_CTE, VY_CTE
      DOUBLE PRECISION X (NPOIN2)    , Y (NPOIN2)
      DOUBLE PRECISION UV(NPOIN2)    , VV(NPOIN2)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  IP
!
!
      DO IP=1,NPOIN2
        UV(IP)=VX_CTE
        VV(IP)=VY_CTE
      ENDDO
!
      RETURN
      END