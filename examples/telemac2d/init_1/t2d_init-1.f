!                       *****************
                        SUBROUTINE AKEPIN
!                       *****************
!
     &(AK,EP,U,V,H,NPOIN,KFROT,CMU,C2,ESTAR,SCHMIT,KMIN,EMIN,CF)
!
!***********************************************************************
!  TELEMAC 2D VERSION 5.2    27/11/92    J-M HERVOUET (LNH) 30 87 80 18
!                            30/05/94    L. VAN HAREN (LNH) 30 87 84 14
!***********************************************************************
!
!     FONCTION  : INITIALISATION DE K ET EPSILON
!
!     LOIS DE FROTTEMENT :
!
!     KFROT = 0 :  PAS DE FROTTEMENT   (NON PREVU)
!     KFROT = 1 :  LOIS LINEAIRE       (NON PREVU)
!     KFROT = 2 :  LOIS DE CHEZY
!     KFROT = 3 :  LOIS DE STRICKLER
!     KFROT = 4 :  LOIS DE MANNING
!     KFROT = 5 :  LOIS DE NIKURADSE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |      AK        |<---| ENERGIE TURBULENTE                           |
! |      EP        |<---| DISSIPATION TURBULENTE                       |
! |      U,V       | -->| COMPOSANTES DE LA VITESSE                    |
! |       H        | -->| HAUTEUR D'EAU                                |
! |     NPOIN      | -->| NOMBRE DE POINTS DU MAILLAGE.                |
! |     KFROT      | -->| CORRESPOND AU MOT CLE: "LOI DE FROTTEMENT SUR|
! |                |    | LE FOND"  (1:CHEZY 2:LINEAIRE 3:STRICKLER).  |
! |     FFON       | -->| COEFFICIENT DE FROTTEMENT.                   |
! |     CHESTR     | -->| TABLEAU DES COEFFICIENTS DE FROTTEMENT SUR LE|
! |                |    | FOND.
! |     GRAV       | -->| ACCELERATION DE LA PESANTEUR.                |
! |     KARMAN     | -- | CONSTANTE DE KARMAN                          |
! |     CMU        | -->| CONSTANTE DU MODELE K-EPSILON                |
! |     C1         | -- | CONSTANTE DU MODELE K-EPSILON                |
! |     C2         | -->| CONSTANTE DU MODELE K-EPSILON                |
! |     SIGMAK     | -- | CONSTANTE DU MODELE K-EPSILON                |
! |     SIGMAE     | -- | CONSTANTE DU MODELE K-EPSILON                |
! |     ESTAR      | -->| CONSTANTE DU MODELE K-EPSILON                |
! |     SCHMIT     | -->| NOMBRE DE SCHMITT                            |
! |     KMIN       | -->| K MINIMUM EN CAS DE CLIPPING                 |
! |     KMAX       | -- | K MAXIMUM EN CAS DE CLIPPING                 |
! |     EMIN       | -->| EPSILON MINIMUM EN CAS DE CLIPPING           |
! |     EMAX       | -->| EPSILON MAXIMUM EN CAS DE CLIPPING           |
! |________________|____|______________________________________________|
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!
!-----------------------------------------------------------------------
!
! APPELE PAR : TELMAC
!
! SOUS-PROGRAMME APPELE : OV
!
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      INTEGER NPOIN,KFROT,K
!
      DOUBLE PRECISION KMIN,EMIN
      DOUBLE PRECISION CMU,C2,ESTAR,SCHMIT
      DOUBLE PRECISION AK(NPOIN),EP(NPOIN),U(NPOIN),V(NPOIN)
      DOUBLE PRECISION H(NPOIN),TIERS,HAUT,CF(*),USTAR,CEPS
!
      INTRINSIC SQRT,MAX
!
!-----------------------------------------------------------------------
!
      TIERS = 1.D0/3.D0
!
!  INITIALISATION DE K ET EPSILON
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
        DO K=1,NPOIN
           HAUT  = MAX(H(K),1.D-4)
           USTAR = SQRT( 0.5D0 * CF(K) * ( U(K)**2 + V(K)**2 ) )
           CEPS  = C2*SQRT(CMU)/SQRT(ESTAR*SCHMIT)/(0.5D0*CF(K))**0.75D0
           AK(K) = C2*USTAR**2/(0.5D0*CF(K)*CEPS)
           EP(K) = MAX( USTAR**3/(HAUT*SQRT(0.5D0*CF(K))) , EMIN )
        ENDDO
!
!     *****
      ENDIF
!     *****
!
!-----------------------------------------------------------------------
!
      RETURN
      END
