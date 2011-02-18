C                       *****************
                        SUBROUTINE AKEPIN
C                       *****************
C
     *(AK,EP,U,V,H,NPOIN,KFROT,CMU,C2,ESTAR,SCHMIT,KMIN,EMIN,CF)
C
C***********************************************************************
C  TELEMAC 2D VERSION 5.2    27/11/92    J-M HERVOUET (LNH) 30 87 80 18
C                            30/05/94    L. VAN HAREN (LNH) 30 87 84 14
C***********************************************************************
C
C     FONCTION  : INITIALISATION DE K ET EPSILON
C
C     LOIS DE FROTTEMENT :
C
C     KFROT = 0 :  PAS DE FROTTEMENT   (NON PREVU)
C     KFROT = 1 :  LOIS LINEAIRE       (NON PREVU)
C     KFROT = 2 :  LOIS DE CHEZY
C     KFROT = 3 :  LOIS DE STRICKLER
C     KFROT = 4 :  LOIS DE MANNING
C     KFROT = 5 :  LOIS DE NIKURADSE
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________.
C |      NOM       |MODE|                   ROLE                       |
C |________________|____|______________________________________________|
C |      AK        |<---| ENERGIE TURBULENTE                           |
C |      EP        |<---| DISSIPATION TURBULENTE                       |
C |      U,V       | -->| COMPOSANTES DE LA VITESSE                    |
C |       H        | -->| HAUTEUR D'EAU                                |
C |     NPOIN      | -->| NOMBRE DE POINTS DU MAILLAGE.                |
C |     KFROT      | -->| CORRESPOND AU MOT CLE: "LOI DE FROTTEMENT SUR|
C |                |    | LE FOND"  (1:CHEZY 2:LINEAIRE 3:STRICKLER).  |
C |     FFON       | -->| COEFFICIENT DE FROTTEMENT.                   |
C |     CHESTR     | -->| TABLEAU DES COEFFICIENTS DE FROTTEMENT SUR LE|
C |                |    | FOND.
C |     GRAV       | -->| ACCELERATION DE LA PESANTEUR.                |
C |     KARMAN     | -- | CONSTANTE DE KARMAN                          |
C |     CMU        | -->| CONSTANTE DU MODELE K-EPSILON                |
C |     C1         | -- | CONSTANTE DU MODELE K-EPSILON                |
C |     C2         | -->| CONSTANTE DU MODELE K-EPSILON                |
C |     SIGMAK     | -- | CONSTANTE DU MODELE K-EPSILON                |
C |     SIGMAE     | -- | CONSTANTE DU MODELE K-EPSILON                |
C |     ESTAR      | -->| CONSTANTE DU MODELE K-EPSILON                |
C |     SCHMIT     | -->| NOMBRE DE SCHMITT                            |
C |     KMIN       | -->| K MINIMUM EN CAS DE CLIPPING                 |
C |     KMAX       | -- | K MAXIMUM EN CAS DE CLIPPING                 |
C |     EMIN       | -->| EPSILON MINIMUM EN CAS DE CLIPPING           |
C |     EMAX       | -->| EPSILON MAXIMUM EN CAS DE CLIPPING           |
C |________________|____|______________________________________________|
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C
C-----------------------------------------------------------------------
C
C APPELE PAR : TELMAC
C
C SOUS-PROGRAMME APPELE : OV
C
C***********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NPOIN,KFROT,K
C
      DOUBLE PRECISION KMIN,EMIN
      DOUBLE PRECISION CMU,C2,ESTAR,SCHMIT
      DOUBLE PRECISION AK(NPOIN),EP(NPOIN),U(NPOIN),V(NPOIN)
      DOUBLE PRECISION H(NPOIN),TIERS,HAUT,CF(*),USTAR,CEPS
C
      INTRINSIC SQRT,MAX
C
C-----------------------------------------------------------------------
C
      TIERS = 1.D0/3.D0
C
C  INITIALISATION DE K ET EPSILON
C
C     *******************
      IF(KFROT.EQ.0) THEN
C     *******************
C
        IF(LNG.EQ.1) WRITE(LU,100)
        IF(LNG.EQ.2) WRITE(LU,101)
100     FORMAT(1X,'AKEPIN N''EST PAS PREVU SANS FROTTEMENT SUR LE FOND')
101     FORMAT(1X,'AKEPIN IS NOT PROVIDED WITHOUT BOTTOM FRICTION')
        CALL PLANTE(1)
        STOP
C
C     ****
      ELSE
C     ****
C
        DO 20 K=1,NPOIN
           HAUT  = MAX(H(K),1.D-4)
           USTAR = SQRT( 0.5D0 * CF(K) * ( U(K)**2 + V(K)**2 ) )
           CEPS  = C2*SQRT(CMU)/SQRT(ESTAR*SCHMIT)/(0.5D0*CF(K))**0.75D0
           AK(K) = C2*USTAR**2/(0.5D0*CF(K)*CEPS)
           EP(K) = MAX( USTAR**3/(HAUT*SQRT(0.5D0*CF(K))) , EMIN )
20      CONTINUE
C
C     *****
      ENDIF
C     *****
C
C-----------------------------------------------------------------------
C
      RETURN
      END
