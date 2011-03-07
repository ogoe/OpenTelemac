!                    *****************
                     SUBROUTINE PRELEO
!                    *****************
!
     &(XLEO,YLEO,NLEO,X,Y,NPOIN2,NOLEO)
!
!***********************************************************************
! TOMAWAC   V6P0                                   21/08/2010
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NLEO           |-->| NOMBRE DE POINTS DE SORTIE
!| NOLEO          |<--| TABLEAU DES NUMERO DES POINTS CHOISIS
!| NPOIN2         |-->| NOMBRE DE POINTS 2D
!| X              |-->| ABSCISSES DES POINTS
!| XLEO           |-->| TABLEAU DES ABSCISSES DES POINTS DE SORTIE
!| Y              |-->| ORDONNEES DES POINTS
!| YLEO           |-->| TABLEAU DES ORDONNEES DES POINTS DE SORTIE
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