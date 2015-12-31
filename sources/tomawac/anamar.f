!                    *****************
                     SUBROUTINE ANAMAR
!                    *****************
!
     &( UC  , VC  , ZM  , ZM1 , ZM2 , DZHDT , X  , Y  , NPOIN2 ,
     &  AT  , DDC , LT  )
!
!***********************************************************************
! TOMAWAC   V6P3                                   08/06/2011
!***********************************************************************
!
!brief    SPECIFIES AN ANALYTICAL TIDE :
!+                WATER LEVEL AND CURRENT SPEED ARE VARIABLE IN TIME.
!
!warning  User subroutine; Must be coded by the user; Here only an example
!+        Now depth is requested, unlike free surface elevation in
!+        versions up to 6.2  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!code
!+      UCONST=0.D0
!+      VCONST=0.D0
!+
!+      DO IP=1,NPOIN2
!+        UC(IP)   = UCONST
!+        VC(IP)   = VCONST
!+        ZM(IP)   = 0.D0
!+        DZHDT(IP)= 0.D0
!+      ENDDO ! IP
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
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        23/01/20103
!+        V6P3
!+   Now depth is requested, not elevation above z0 !!!!!!!!!!!!
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
!| ZM             |<--| DEPTH AT TIME AT, AT THE MESH POINTS
!| ZM1            |-->| DEPTH AT TIME TM1, AT THE MESH POINTS
!| ZM2            |-->| DEPTH AT TIME TM2, AT THE MESH POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_TOMAWAC, EX_ANAMAR => ANAMAR
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN2,LT
      DOUBLE PRECISION, INTENT(IN)    :: AT,DDC
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN2),Y(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: ZM1(NPOIN2),ZM2(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: UC(NPOIN2),VC(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: DZHDT(NPOIN2),ZM(NPOIN2)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          IP
      DOUBLE PRECISION UCONST, VCONST
!
!-----------------------------------------------------------------------
!     EXAMPLE 1
!-----------------------------------------------------------------------
!
      UCONST=0.D0
      VCONST=0.D0
!
      DO IP=1,NPOIN2
        UC(IP)   = UCONST
        VC(IP)   = VCONST
        ZM(IP)   = 3.D0
        DZHDT(IP)= 0.D0
      ENDDO ! IP
!
!-----------------------------------------------------------------------
!
      RETURN
      END
