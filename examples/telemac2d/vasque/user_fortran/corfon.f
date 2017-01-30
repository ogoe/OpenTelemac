!                       *****************
                        SUBROUTINE CORFON
!                       *****************
!
!***********************************************************************
! TELEMAC 2D VERSION 5.1          01/03/90    J-M HERVOUET
!***********************************************************************
!
!  USER SUBROUTINE CORFON
!
!  FUNCTION  : MODIFICATION OF THE BOTTOM TOPOGRAPHY
!
!
!-----------------------------------------------------------------------
!  ARGUMENTS USED IN THE EXAMPLE
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
      USE DECLARATIONS_TELEMAC2D
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
      INTEGER IM,JM,I,J,POS_LOC
      DOUBLE PRECISION EIKON
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
      IM = 47
      JM = 10
!
!  VARIANTE FOND EN PENTE RECTILIGNE + CUVETTE
!
      DO I=1,IM
      DO J=1,JM
!       PENTE RECTILIGNE
        POS_LOC = GLOBAL_TO_LOCAL_POINT(I+(J-1)*IM,MESH)
!
!       NOTE JMH: THIS IS VERY HEAVY, THERE SHOULD BE A
!                 FORMULA FUNCTION OF X.
!
        IF(POS_LOC.GT.0) THEN
          ZF%R(POS_LOC)=-0.6D0+0.46D0*FLOAT(I-1)/FLOAT(IM-1)
!         BOSSE GAUSSIENNE
          IF(I.GT.9.AND.I.LT.29) THEN
            EIKON = -(I-19)**2/20.D0
            ZF%R(POS_LOC) = ZF%R(POS_LOC) + 0.1D0*EXP(EIKON)
          ENDIF
        ENDIF
      ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

