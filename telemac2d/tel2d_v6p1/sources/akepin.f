!                    *****************
                     SUBROUTINE AKEPIN
!                    *****************
!
     &(AK,EP,U,V,H,NPOIN,KFROT,CMU,C2,ESTAR,SCHMIT,KMIN,EMIN,CF)
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES K AND EPSILON.
!
!note     FRICTION LAWS :
!+
!note     KFROT = 0:  NO FRICTION   (NOT CODED)
!note     KFROT = 1:  LINEAR LAW      (NOT CODED)
!note     KFROT = 2:  LAW OF CHEZY
!note     KFROT = 3:  LAW OF STRICKLER
!note     KFROT = 4:  LAW OF MANNING
!note     KFROT = 5:  LAW OF NIKURADSE
!
!history  J-M HERVOUET (LNHE)
!+        27/11/1992
!+        
!+   
!
!history  L. VAN HAREN (LNH)
!+        30/05/1994
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AK             |<--| ENERGIE TURBULENTE
!| C2             |-->| CONSTANTE DU MODELE K-EPSILON
!| CF             |---| 
!| CMU            |-->| CONSTANTE DU MODELE K-EPSILON
!| EMIN           |-->| EPSILON MINIMUM EN CAS DE CLIPPING
!| EP             |<--| DISSIPATION TURBULENTE
!| ESTAR          |-->| CONSTANTE DU MODELE K-EPSILON
!| H              |-->| HAUTEUR D'EAU
!| KFROT          |-->| CORRESPOND AU MOT CLE: "LOI DE FROTTEMENT SUR
!|                |   | LE FOND"  (1:CHEZY 2:LINEAIRE 3:STRICKLER).
!| KMIN           |-->| K MINIMUM EN CAS DE CLIPPING
!| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE.
!| SCHMIT         |-->| NOMBRE DE SCHMITT
!| U,V            |-->| COMPOSANTES DE LA VITESSE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN,KFROT
      DOUBLE PRECISION, INTENT(INOUT) :: AK(NPOIN),EP(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: KMIN,EMIN,CMU,C2,ESTAR,SCHMIT
      DOUBLE PRECISION, INTENT(IN) :: U(NPOIN),V(NPOIN),H(NPOIN),CF(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K
!
      DOUBLE PRECISION TIERS,HAUT,USTAR,CEPS
!
      INTRINSIC SQRT,MAX
!
!-----------------------------------------------------------------------
!
      TIERS = 1.D0/3.D0
!
!  INITIALISATION OF K AND EPSILON
!
!     *******************
      IF(KFROT.EQ.0) THEN
!     *******************
!
        IF(LNG.EQ.1) WRITE(LU,100)
        IF(LNG.EQ.2) WRITE(LU,101)
100     FORMAT(1X,'AKEPIN N''EST PAS PREVU SANS FROTTEMENT SUR LE FOND')
101     FORMAT(1X,'AKEPIN IS NOT PROVIDED WITHOUT BOTTOM FRICTION')
        CALL PLANTE(1)
        STOP
!
!     ****
      ELSE
!     ****
!
        DO 20 K=1,NPOIN
           HAUT  = MAX(H(K),1.D-4)
           USTAR = SQRT( 0.5D0 * CF(K) * ( U(K)**2 + V(K)**2 ) )
           CEPS  = C2*SQRT(CMU)/SQRT(ESTAR*SCHMIT)/(0.5D0*CF(K))**0.75D0
           AK(K) = C2*USTAR**2/(0.5D0*CF(K)*CEPS)
           EP(K) = MAX( USTAR**3/(HAUT*SQRT(0.5D0*CF(K))) , EMIN )
20      CONTINUE
!
!     *****
      ENDIF
!     *****
!
!-----------------------------------------------------------------------
!
      RETURN
      END