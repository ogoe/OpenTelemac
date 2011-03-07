!                    *****************
                     SUBROUTINE ANAMAR
!                    *****************
!
     &( UC  , VC  , ZM  , ZM1 , ZM2 , DZHDT , X  , Y  , NPOIN2 ,
     &  AT  , DDC , LT  )
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    SPECIFIES AN ANALYTICAL TIDE :
!+                WATER LEVEL AND CURRENT SPEED ARE VARIABLE IN TIME.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER; THIS IS MERELY AN EXAMPLE
!code
!+      UCONST=0.D0
!+      VCONST=0.D0
!+
!+      DO 100 IP=1,NPOIN2
!+        UC(IP)   = UCONST
!+        VC(IP)   = VCONST
!+        ZM(IP)   = 0.D0
!+        DZHDT(IP)= 0.D0
!+  100 CONTINUE
!
!history
!+
!+        V5P0
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
!| DZHDT          |<--| VARIATION TEMPORELLE DE LA HAUTEUR DE MAREE
!| LT             |---|
!| NPOIN2         |-->| NOMBRE DE POINTS 2D
!| UC,VC          |<--| COMPOSANTES DU CHAMP DE COURANT DE LA MAREE
!| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE 2D
!| ZM             |---|
!| ZM1            |<--| HAUTEUR DE LA MAREE PAR RAPPORT A ZREPOS A T
!| ZM2            |<--| HAUTEUR DE LA MAREE PAR RAPPORT A ZREPOS A T2
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
      INTEGER  LT
      DOUBLE PRECISION AT    , DDC
      DOUBLE PRECISION X (NPOIN2), Y (NPOIN2), ZM1(NPOIN2), ZM2(NPOIN2)
      DOUBLE PRECISION UC(NPOIN2), VC(NPOIN2), DZHDT(NPOIN2),ZM(NPOIN2)
!
!.....LOCAL VARIABLES
!     """""""""""""""""
      INTEGER          IP, I, J
      DOUBLE PRECISION UCONST, VCONST
!
!-----------------------------------------------------------------------
!     EXAMPLE 1
!-----------------------------------------------------------------------
!
      UCONST=0.D0
      VCONST=0.D0
!
      DO 100 IP=1,NPOIN2
        UC(IP)   = UCONST
        VC(IP)   = VCONST
        ZM(IP)   = 0.D0
        DZHDT(IP)= 0.D0
  100 CONTINUE
!
!
      RETURN
      END