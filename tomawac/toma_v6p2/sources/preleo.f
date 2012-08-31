!                    *****************
                     SUBROUTINE PRELEO
!                    *****************
!
     &(XLEO,YLEO,NLEO,X,Y,NPOIN2,NOLEO)
!
!***********************************************************************
! TOMAWAC   V6P1                                   22/06/2011
!***********************************************************************
!
!brief    SELECTS THE COMPUTATION NODES CLOSEST
!+                TO THE REQUESTED OUTPUT POINTS.
!
!history  F. MARCOS (LNH)
!+        01/02/95
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
!+        22/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NLEO           |-->| NUMBER OF SPECTRUM PRINTOUT POINTS
!| NOLEO          |<--| NUMBERS OF THE SPECTRUM PRINTOUT POINTS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XLEO           |-->| ABSCISSAE OF SPECTRUM PRINTOUT POINTS
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| YLEO           |-->| ORDINATES OF SPECTRUM PRINTOUT POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER I,ILEO,NLEO,NPOIN2
!
      DOUBLE PRECISION X(NPOIN2)  , Y(NPOIN2)
      DOUBLE PRECISION XLEO(NLEO)  , YLEO(NLEO)
      DOUBLE PRECISION DIST,DIST2
!
      INTEGER NOLEO(NLEO)
!
!-----------------------------------------------------------------------
!
      DO 10 ILEO=1,NLEO
        DIST=1.D99
        DO 20 I=1,NPOIN2
         DIST2=(XLEO(ILEO)-X(I))**2+(YLEO(ILEO)-Y(I))**2
         IF (DIST2.LT.DIST) THEN
             DIST=DIST2
             NOLEO(ILEO)=I
         ENDIF
20      CONTINUE
10    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
