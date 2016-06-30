!                    *****************
                     SUBROUTINE ANACOS
!                    *****************
!
     &( UC    , VC    , X     , Y     , NPOIN2 )
!
!***********************************************************************
! TOMAWAC   V6P1                                   09/06/2011
!***********************************************************************
!
!brief    SPECIFIES A ! STATIONARY ! ANALYTICAL CURRENT.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER; THIS IS MERELY AN EXAMPLE
!code
!+      UCONST=0.D0
!+      VCONST=0.D0
!+
!+      DO IP=1,NPOIN2
!+        UC(IP)=UCONST
!+        VC(IP)=VCONST
!+    ENDDO ! IP
!
!history
!+        07/06/2001
!+        V5P2
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
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| UC             |<--| CURRENT VELOCITY ALONG X AT THE MESH POINTS
!| VC             |<--| CURRENT VELOCITY ALONG Y AT THE MESH POINTS
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_TOMAWAC, EX_ANACOS => ANACOS
      IMPLICIT NONE
!
!
!.....VARIABLES IN ARGUMENT
!     """"""""""""""""""""
      INTEGER, INTENT(IN)             ::  NPOIN2
      DOUBLE PRECISION, INTENT(IN)    ::  X (NPOIN2) , Y (NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) ::  UC(NPOIN2) , VC(NPOIN2)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER  IP
      DOUBLE PRECISION UCONST, VCONST
!
!
      UCONST=0.D0
      VCONST=0.D0
!
      DO IP=1,NPOIN2
        UC(IP)=UCONST
        VC(IP)=VCONST
      ENDDO ! IP
!
      RETURN
      END
