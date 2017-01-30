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
      IMPLICIT NONE
!
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
!
      DO IP=1,NPOIN2
        IF (X(IP).GE.(1.56D0).AND.X(IP).LE.(2.78D0)) THEN
          UC(IP)=-0.13*1/(1-(0.3D0/1.22D0)*(X(IP)-1.56D0)/0.75D0)
        ELSEIF (X(IP).GT.(2.78D0).AND.X(IP).LE.(5.22D0)) THEN
          UC(IP)=-0.22D0
        ELSEIF (X(IP).GT.(5.22D0).AND.X(IP).LE.(6.44D0)) THEN
          UC(IP)=-0.22*1/(1+(0.3D0/1.22D0)*(X(IP)-5.22D0)/0.45D0)
        ELSE
          UC(IP)=-0.13D0
        ENDIF
        VC(IP)=0.D0
        WRITE(LU,*) IP,UC(IP),VC(IP),'IP,UC,VC'
      ENDDO
!
!
      RETURN
      END

