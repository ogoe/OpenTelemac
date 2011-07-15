!                    *****************
                     SUBROUTINE ANAMAR
!                    *****************
!
     &( UC  , VC  , ZM  , ZM1 , ZM2 , DZHDT , X  , Y  , NPOIN2 ,
     &  AT  , DDC , LT  )
!
!***********************************************************************
! TOMAWAC   V6P1                                   08/06/2011
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
!history  G.MATTAROLO (EDF - LNHE)
!+        08/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| COMPUTATION TIME
!| DDC            |-->| DATE OF COMPUTATION BEGINNING
!| DZHDT          |<--| VARIATION TEMPORELLE DE LA HAUTEUR DE MAREE
!| LT             |<--| NUMBER OF THE TIME STEP CURRENTLY SOLVED
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| UC             |-->| CURRENT VELOCITY ALONG X AT THE MESH POINTS
!| VC             |-->| CURRENT VELOCITY ALONG Y AT THE MESH POINTS
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| ZM             |<--| TIDAL HEIGTH AT TIME AT, AT THE MESH POINTS
!| ZM1            |-->| TIDAL HEIGTH AT TIME TM1, AT THE MESH POINTS
!| ZM2            |-->| TIDAL HEIGTH AT TIME TM2, AT THE MESH POINTS
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
