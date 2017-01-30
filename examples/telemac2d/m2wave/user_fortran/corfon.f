!                       *****************
                        SUBROUTINE CORFON
!                       *****************
!
!***********************************************************************
! PROGICIEL : TELEMAC-2D 5.0          01/03/90    J-M HERVOUET
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
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      INTEGER I,J
      DOUBLE PRECISION ZM
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     REMARQUE JMH : 5023 POINTS DANS LE FICHIER MAIS NPOIN = 5007 ?????
      DOUBLE PRECISION ZZM(5023)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL MAS
      INTEGER ID
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
!-----------------------------------------------------------------------
!
!  CALCUL DU DEMI-MARNAGE EN CHAQUE POINT DU MAILLAGE
!
      ID = T2D_FILES(T2DFO2)%LU
      REWIND ID
!
!  LECTURE DES DEMI-MARNAGES DONNES PAR LE FICHIER
!  INITIALISATIONS DES TABLEAUX ET DES VARIABLES
!  CALCUL DE LA COTE DU FOND AU POINT I
!
      IF(NCSIZE.GT.1) THEN
!       NOMBRE DE POINTS DU MAILLAGE NON DECOUPE (5023 DANS FICHIER)
        DO I=1,5023
          READ(ID,*) J,ZZM(I)
          IF(I.NE.J) STOP 'PROBLEME DANS FICHIER 27'
        ENDDO
        DO I=1,NPOIN
          ZF%R(I)=ZF%R(I)-ZZM(MESH%KNOLG%I(I))*12.D0/7.D0
        ENDDO
      ELSE
        DO I=1,NPOIN
          READ(ID,*) J,ZM
          ZF%R(I)=ZF%R(I)-ZM*12.D0/7.D0
        ENDDO
      ENDIF
!
! ON LISSE 5 FOIS LE FOND (PB DE PENTE DU TALUS)
!
      MAS = .TRUE.
      LISFON = 5
      CALL FILTER(ZF,MAS,T1,T2,AM1,'MATMAS          ',
     &              1.D0,T1,T1,T1,T1,T1,T1,MESH,MSK,MASKEL,LISFON)
!
!-----------------------------------------------------------------------
!
      RETURN
      END

