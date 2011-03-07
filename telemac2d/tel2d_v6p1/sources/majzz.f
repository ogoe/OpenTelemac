!                       ****************
                        SUBROUTINE MAJZZ
!                       ****************
!
     *(W,FLUX,AIRS,DT,NPOIN,ZF,CF,EPS,DDMIN,KFROT,SMH,
     * HN,QU,QV)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    INTEGRATION EN TEMPS
!
!note     JMH: DDMIN NON UTILISE
!
!history  N. GOUTAL
!+        24/11/97
!+        V5P4
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AIRE           |---|
!| FLUX           |-->| FLUX DE ROE
!| AIRS           |-->| TABLEAU DES AIRES DES CELLULES
!| H              |<->| ENTHALPIE
!| DT             |-->| PAS DE TEMPS
!| 3              |-->| DIMENSION DU SYSTEME
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN,KFROT
      DOUBLE PRECISION, INTENT(INOUT) :: W(3,NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FLUX(NPOIN,3),DT,EPS,DDMIN
      DOUBLE PRECISION, INTENT(IN)    :: AIRS(NPOIN),ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: CF(NPOIN),SMH(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: HN(NPOIN),QU(NPOIN),QV(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
      DOUBLE PRECISION FACT
      DOUBLE PRECISION, PARAMETER :: G = 9.81D0
!
!     UPDATING
!
      DO 10 I = 1 , NPOIN
         FACT=DT/AIRS(I)
         W(1,I) = W(1,I) + FACT *( FLUX (I,1)+SMH(I))
         W(2,I) = W(2,I) + FACT *  FLUX (I,2)
         W(3,I) = W(3,I) + FACT *  FLUX (I,3)
10    CONTINUE
!
!     FRICTION
!
      IF (KFROT.NE.0) CALL FRICTION(NPOIN,G,DT,W,HN,QU,QV,CF)
!
!-----------------------------------------------------------------------
!  
      RETURN                                                            
      END
