!                       *****************
                        SUBROUTINE ART_CORFON
!                       *****************
!
!***********************************************************************
! PROGICIEL : TELEMAC 5.0          01/03/90    J-M HERVOUET
!***********************************************************************
!
!  USER SUBROUTINE ART_CORFON
!
!  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
!
!
!-----------------------------------------------------------------------
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|_______________________________________________
! |      ZF        |<-->| FOND A MODIFIER.
! |      X,Y,(Z)   | -->| COORDONNEES DU MAILLAGE (Z N'EST PAS EMPLOYE).
! |      A         |<-- | MATRICE
! |      T1,2      | -->| TABLEAUX DE TRAVAIL (DIMENSION NPOIN)
! |      W1        | -->| TABLEAU DE TRAVAIL (DIMENSION 3 * NELEM)
! |      NPOIN     | -->| NOMBRE DE POINTS DU MAILLAGE.
! |      PRIVE     | -->| TABLEAU PRIVE POUR L'UTILISATEUR.
! |      LISFON    | -->| NOMBRE DE LISSAGES DU FOND.
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
!
! PROGRAMME APPELANT :
! PROGRAMMES APPELES : RIEN EN STANDARD
!
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_ARTEMIS
!
      IMPLICIT NONE
!
      INTEGER I,II
!
      DOUBLE PRECISION PI,BID
!
      PARAMETER( PI = 3.1415926535897932384626433D0)
!
!
!     ----------------------------------------
!     VOS NOUVELLES DECLARATIONS DE VARIABLES :
!     ----------------------------------------
!
! JCB :
      DOUBLE PRECISION R0,R1,R2
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
!
!-----------------------------------------------------------------------
!
!  LISSAGES EVENTUELS DU FOND
      DO  I = 1,NPOIN
        ZF%R(I) = 0.D0
      ENDDO
!
!      IF(LISFON.GT.0) THEN
!
!        MAS=.TRUE.
!        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
!     *              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
!
!      ENDIF
!
! RAYON DU CYLINDRE
!      R0 = 1.D50
!      DO I=1,NPOIN
!         IF ( R0.GT.SQRT( X(I)**2 + Y(I)**2 ) ) THEN
!            R0 = SQRT( X(I)**2 + Y(I)**2 )
!         ENDIF
!       ENDDO
!
!     RAYON DE L'ETENDUE DU FOND PARABOLIQUE
      R0 = 39850.D0
!      WRITE(LU,*) 'R0=',R0
      R1 = 3.D0*R0
!
!-----------------------------------------------------------------------
!
      DO II = 1 , NPOIN
!
        R2 = X(II)**2 + Y(II)**2
        IF (R2.LE.R1**2) ZF%R(II) = 4000.D0*(1.D0-R2/R1**2)
!
      ENDDO
!
!
      RETURN
      END

