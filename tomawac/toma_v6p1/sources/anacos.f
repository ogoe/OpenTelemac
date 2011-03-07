!                    *****************
                     SUBROUTINE ANACOS
!                    *****************
!
     &( UC    , VC    , X     , Y     , NPOIN2 )
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    SPECIFIES A ! STATIONARY ! ANALYTICAL CURRENT.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER; THIS IS MERELY AN EXAMPLE
!code
!+      UCONST=0.D0
!+      VCONST=0.D0
!+
!+      DO 100 IP=1,NPOIN2
!+        UC(IP)=UCONST
!+        VC(IP)=VCONST
!+  100 CONTINUE
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPOIN2         |-->| NOMBRE DE POINTS 2D
!| UC,VC          |<--| COMPOSANTES DU CHAMP DE COURANT
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
      DOUBLE PRECISION X (NPOIN2) , Y (NPOIN2)
      DOUBLE PRECISION UC(NPOIN2) , VC(NPOIN2)
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
      DO 100 IP=1,NPOIN2
        UC(IP)=UCONST
        VC(IP)=VCONST
  100 CONTINUE
!
      RETURN
      END