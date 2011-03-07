!                    *****************
                     SUBROUTINE DIFFCL
!                    *****************
!
     &(LITBOR,TTILD,TBOR,NBOR,ICONV,NPOIN,NPTFR)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES THE VALUE OF THE TRACER FOR BOUNDARY
!+                CONDITIONS OF TYPE DIRICHLET, IN THE DIFFUSION STEP.
!
!history  J-M HERVOUET (LNHE)
!+        09/10/2009
!+        V6P0
!+   MOVED FROM TELEMAC-2D TO ALLOW CALL BY SISYPHE 
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
!| ICONV          |---| 
!| LITBOR         |-->| TYPES DE CONDITIONS AUX LIMITES DU TRACEUR.
!| NBOR           |-->| ADRESSES DES POINTS FRONTIERES.
!|                |   | CONDITIONS AUX LIMITES (PHYSIQUE) .
!| NPOIN          |-->| NOMBRE DE POINTS TOTAL .
!| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES .
!| TBOR           |<->| CONDITIONS AUX LIMITES SUR T.
!| TTILD          |-->| TRACEUR.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TELEMAC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,NPTFR,ICONV
      INTEGER, INTENT(IN)             :: NBOR(NPTFR)
      INTEGER, INTENT(IN)             :: LITBOR(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: TTILD(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: TBOR(NPTFR)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K
!
!----------------------------------------------------------------------
!
!  INITIALISES TBOR (SEE CONSTRUCTION OF LIMTRA IN DIFFIN)
!
      IF(ICONV.EQ.ADV_CAR) THEN
!
      DO 1 K=1,NPTFR
!
!  IMPOSES THE RESULT OF ADVECTION AT FREE EXITS
!                      WITH THE METHOD OF CHARACTERISTICS
!
        IF(LITBOR(K).EQ.KSORT) TBOR(K) = TTILD(NBOR(K))
!
1     CONTINUE
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END