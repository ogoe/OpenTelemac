!                    *****************
                     SUBROUTINE KEPINI
!                    *****************
!
     &(AK,EP,U,V,Z,ZF,NPOIN2,NPLAN,DNUVIH,DNUVIV,KARMAN,CMU,KMIN,EMIN)
!
!***********************************************************************
! TELEMAC3D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES K AND EPSILON.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+        
!+   FORTRAN95 VERSION 
!
!history  V. BOYER UMIST
!+        
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
!| AK             |<--| ENERGIE TURBULENTE
!| CMU            |-->| CONSTANTE DU MODELE K-EPSILON
!| DNUVIH         |-->| COEFFICIENT DE DIFFUSION HORIZONTALE
!| DNUVIV         |-->| COEFFICIENT DE DIFFUSION VERTICALE
!| EMIN           |-->| EPSILON MINIMUM EN CAS DE CLIPPING
!| EP             |<--| DISSIPATION TURBULENTE
!| KARMAN         |-->| CONSTANTE DE KARMAN
!| KMIN           |-->| K MINIMUM EN CAS DE CLIPPING
!| NPLAN          |-->| NOMBRE DE PLANS  DU MAILLAGE 3D
!| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
!| U,V            |-->| COMPOSANTES DE LA VITESSE
!| Z              |-->| COTES DES POINTS DU MAILLAGE 3D REEL
!| ZF             |---| 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG, LU
      COMMON/INFO/ LNG, LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: NPOIN2,NPLAN
      DOUBLE PRECISION, INTENT(INOUT):: AK(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT):: EP(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN)   :: U(NPOIN2,NPLAN), V(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN)   :: Z(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN)   :: ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)   :: KARMAN, DNUVIH, DNUVIV
      DOUBLE PRECISION, INTENT(IN)   :: CMU
      DOUBLE PRECISION, INTENT(IN)   :: KMIN, EMIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN2,IPLAN,N2,IT
      DOUBLE PRECISION C,DIST
!
      INTRINSIC LOG, SQRT, MAX
!
      DOUBLE PRECISION, PARAMETER :: FICTIFEPS = 2.D0
!
!-----------------------------------------------------------------------
!
!     A THEORY BY VINCENT BOYER MODIFIED BY MARTIN FERRAND
!
!     DO IPOIN2 = 1,NPOIN2
!       DO IPLAN = 1,NPLAN
!
!         ARBITRARY COMPUTATION OF K EXPRESSED AS A PERCENTAGE OF SPEED
!
!         AK(IPOIN2,IPLAN) = 1.D-3*U(IPOIN2,IPLAN)**2
!         AK(IPOIN2,IPLAN) = MAX(AK(IPOIN2,IPLAN),KMIN)
!
!         COMPUTATION OF EPSILON
!
!         EP INITIALISED ACCORDING TO UETOIL**3/KAPPA/Y
!         WHERE UETOIL IS CALCULATED FROM THE VALUE OF K AT THE WALL
!
!         IF(IPLAN.EQ.1) THEN
!           DIST = (Z(IPOIN2,2)-ZF(IPOIN2))/FICTIFEPS
!         ELSE
!           DIST = Z(IPOIN2,IPLAN)-ZF(IPOIN2)
!         ENDIF
!         EP(IPOIN2,IPLAN)=CMU**0.75*SQRT(AK(IPOIN2,1)**3)/KARMAN/DIST
!         EP(IPOIN2,IPLAN)=MAX(EP(IPOIN2,IPLAN),EMIN)
!       ENDDO
!     ENDDO
!
!-----------------------------------------------------------------------
!
!     HERE: NO INITIAL TURBULENCE
!
      DO IPOIN2 = 1,NPOIN2
        DO IPLAN = 1,NPLAN
          AK(IPOIN2,IPLAN) = KMIN
          EP(IPOIN2,IPLAN) = EMIN
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END