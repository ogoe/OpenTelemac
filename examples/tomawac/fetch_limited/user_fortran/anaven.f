!                    *****************
                     SUBROUTINE ANAVEN
!                    *****************
!
     &( UV    , VV    , X     , Y     , NPOIN2, AT    , DDC   , VX_CTE,
     &  VY_CTE)
!
!***********************************************************************
! TOMAWAC   V6P1                                   08/06/2011
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
!history  G.MATTAROLO (EDF - LNHE)
!+        08/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| DDC            |-->| DATE OF COMPUTATION BEGINNING
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| UV             |<--| WIND VELOCITY ALONG X AT THE MESH POINTS
!| VV             |<--| WIND VELOCITY ALONG Y AT THE MESH POINTS
!| VX_CTE         |-->| WIND ALONG X (CONSTANT VALUE IN STEERING FILE)
!| VY_CTE         |-->| WIND ALONG Y (CONSTANT VALUE IN STEERING FILE)
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
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
      DOUBLE PRECISION UCONST, VCONST
!
      UCONST= 20.D0
      VCONST= 0.D0
!
      DO IP=1,NPOIN2
        UV(IP)=UCONST
        VV(IP)=VCONST
      ENDDO
!
      RETURN
      END

