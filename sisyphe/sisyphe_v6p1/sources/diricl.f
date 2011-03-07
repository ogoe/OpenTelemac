!                    *****************
                     SUBROUTINE DIRICL
!                    *****************
!
     &( ZF1 , ZF , EBOR , LIEBOR , NBOR , NPOIN  , NPTFR  , KENT )
!
!***********************************************************************
! SISYPHE   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    DETERMINES THE BOUNDARY CONDITIONS ON E
!+                FOR DIRICHLET POINTS.
!
!history  C.LE NORMANT
!+        **/10/97
!+        V5P1
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
!| EBOR           |<->| EVOLUTION AUX POINTS DE BORD
!| KENT           |-->| TYPE DE CONDITION LIMITE
!| LIEBOR         |<->| TYPES DE CONDITIONS AUX LIMITES SUR E
!| NBOR           |-->| TABLEAU DES NUMEROS GLOBAUX DES POINTS
!|                |   | DE BORD
!| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES
!| ZF             |-->| COTE DU FOND
!| ZF1            |<->| COTE DU FOND
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN):: KENT,NPOIN,NPTFR
      INTEGER, INTENT(IN):: NBOR(NPTFR)
      INTEGER, INTENT(IN):: LIEBOR(NPTFR)
!
      DOUBLE PRECISION, INTENT(IN)::  ZF(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT):: ZF1(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: EBOR(NPTFR)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K, N
!
!-----------------------------------------------------------------------
!
      DO 10 K=1,NPTFR
!
        N = NBOR(K)
!
        IF (LIEBOR(K).EQ.KENT) THEN
          ZF1(N)   = EBOR(K)+ZF(N)
        ENDIF
!
10    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE DIRICL