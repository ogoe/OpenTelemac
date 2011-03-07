!                    ***********************
                     SUBROUTINE TOBW_SISYPHE
!                    ***********************
!
     &(TOBW ,CF, FW, UW,TW,HN,NPOIN,XMVE)
!
!***********************************************************************
! SISYPHE   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE WAVE FRICTION STRESS. THE FRICTION
!+                COEFFICIENT IS COMPUTED USING SWART FORMULATION (1976).
!
!history  C. VILLARET (LNHE)
!+        01/10/2003
!+        V5P4
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
!| CF             |-->| COEFFICIENT DE FROTTEMENT QUADRATIQUE (COURAN
!| FW             |<--| COEFFICIENT DE FROTTEMENT quadratique (houle)
!| HN             |-->| HAUTEUR D'EAU AU TEMPS N
!| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE 2D
!| TOBW           |<--| CONTRAINTE TOTALE AU FOND
!| TW             |-->| PERIODEE DE LA HOULE
!| UW             |-->| VITESSE ORBITALE DE LA HOULE
!| XMVE           |-->| MASSE VOLUMIQUE DE L'EAU
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN
!
      DOUBLE PRECISION, INTENT(IN)    :: CF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: UW(NPOIN),TW(NPOIN),HN(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: XMVE
      DOUBLE PRECISION, INTENT(INOUT) :: TOBW(NPOIN),FW(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
      DOUBLE PRECISION KS,AUX
      DOUBLE PRECISION PI,AW,KARMAN
      PARAMETER (PI=3.141592653589793D0)
      PARAMETER (KARMAN=0.4D0)
!
!-----------------------------------------------------------------------
!
      DO  I=1,NPOIN
!       KS : NIKURADSE COEFFICIENT (TOTAL FRICTION)
        AUX=1.D0+KARMAN*SQRT(2.D0/MAX(CF(I),1.D-10))
        KS=30.D0*MAX(HN(I),1.D-8)*EXP(-AUX)
        AW= UW(I)*TW(I) / (2.D0*PI)
        IF(AW/KS.GT.1.59D0) THEN
          FW(I)=EXP( -6.D0 + 5.2D0 * (AW/KS)**(-0.19D0) )
        ELSE
          FW(I)=0.3D0
        ENDIF
        TOBW(I)=0.5D0 * XMVE * FW(I) * UW(I)*UW(I)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE TOBW_SISYPHE