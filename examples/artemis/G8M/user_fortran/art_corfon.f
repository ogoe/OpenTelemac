!                       *****************
                        SUBROUTINE ART_CORFON
!                       *****************
!
!***********************************************************************
! PROGICIEL : TELEMAC 5.0          01/03/90    J-M HERVOUET
!***********************************************************************
!
!  USER SUBROUTINE CORFON
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
!
!     ----------------------------------------
!     VOS NOUVELLES DECLARATIONS DE VARIABLES :
!     ----------------------------------------
!
! JCB :
      INTEGER KK
      DOUBLE PRECISION ECHEL
! JCB
!
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
!
!-----------------------------------------------------------------------
!
!  LISSAGES EVENTUELS DU FOND
!
      IF(LISFON.GT.0) THEN
!
        MAS=.TRUE.
        CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
!
      ENDIF
!
      DO  I = 1,NPOIN
        ZF%R(I) = 0.D0
      ENDDO
!-----------------------------------------------------------------------
!
! EXEMPLE :
!
!      DO 10 I = 1,NPOIN
!        ZF%R(I) = -1.D0 -0.02D0*Y(I)
!        IF (Y(I).GE.700.D0) THEN
!           ZF%R(I) = -15.D0
!        ENDIF
!10    CONTINUE
!
!-----------------------------------------------------------------------
! VOTRE MODIFICATION DES FONDS :
!-----------------------------------------------------------------------
!
! JCB :
!
      ECHEL=1.D0
      DO KK=1,NPOIN
        IF((Y(KK)/ECHEL).GE. 7.12D0)THEN
          ZF%R(KK)=-0.33D0*ECHEL
        ELSEIF ((Y(KK)/ECHEL) .LE. -9.13D0)THEN
          ZF%R(KK)=-0.0044*ECHEL
!       ELSEIF ((Y(KK)/ECHEL) .LE. -8D0)THEN
!         ZF%R(KK)=-0.027*ECHEL
        ELSE
          ZF%R(KK)=-0.02D0*((Y(KK)/ECHEL)+9.35D0)*ECHEL
        ENDIF
      ENDDO
!-------------------------------------------------------
!
      RETURN
      END

