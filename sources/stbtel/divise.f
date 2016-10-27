!                       *****************
                        SUBROUTINE DIVISE
!                       *****************
!
     &(X,Y,XEL,YEL,IKLE,NCOLOR,NPOIN,NELEM,NELMAX,NSOM2,SOM2,INDICP,
     & INDICE,CORR,LEVEL)
!
!***********************************************************************
! PROGICIEL : STBTEL  V5.2                 J-M JANIN   (LNH) 30 87 72 84
! ORIGINE   : TELEMAC
!***********************************************************************
!
!     FONCTION  :  DIVISION PAR 4 DE TOUTES LES MAILLES
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   X,Y          |<-->| COORDONNEES DU MAILLAGE .
! |   IKLE         |<-->| NUMEROS GLOBAUX DES NOEUDS DE CHAQUE ELEMENT
! |   NCOLOR       |<-->| TABLEAU DES COULEURS DES POINTS DU MAILLAGE
! |   NPOIN        |<-->| NOMBRE TOTAL DE NOEUDS DU MAILLAGE
! |   NELEM        |<-->| NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
! |   NELMAX       | -->| DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!----------------------------------------------------------------------
! APPELE PAR : STBTEL
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
      DOUBLE PRECISION, INTENT(INOUT) :: X(*), Y(*)
      INTEGER, INTENT(IN)    :: NELMAX, NSOM2
      DOUBLE PRECISION, INTENT(INOUT) :: XEL(NELMAX,3), YEL(NELMAX,3)
      DOUBLE PRECISION, INTENT(IN) :: SOM2(10,2)
      INTEGER, INTENT(INOUT) :: IKLE(NELMAX,*), NCOLOR(*)
      INTEGER, INTENT(INOUT) :: INDICP(*), INDICE(*)
      INTEGER, INTENT(INOUT) :: NPOIN, NELEM
      INTEGER, INTENT(INOUT), OPTIONAL :: CORR(NELMAX,*)
      INTEGER, INTENT(IN), OPTIONAL :: LEVEL
      INTEGER IELEM , IPOIN , ISOM
      INTEGER NO1 , NO2 , NO3 , NP1 , NP2 , NP3 , NE1 , NE2 , NE3
!
      DOUBLE PRECISION  DX , DY
!
!=======================================================================
!      RECHERCHE DES ELEMENTS A DIVISER PAR 4 OU PAR 2
!=======================================================================
!
      DO IPOIN = 1,NPOIN
        INDICP(IPOIN) = 1
      ENDDO
!
      IF (NSOM2.GE.3) THEN
!
        DO ISOM = 1,NSOM2
!
          DX = SOM2(ISOM+1,1) - SOM2(ISOM,1)
          DY = SOM2(ISOM+1,2) - SOM2(ISOM,2)
!
          DO IPOIN = 1,NPOIN
            IF (DX*(Y(IPOIN)-SOM2(ISOM,2)).LT.
     &          DY*(X(IPOIN)-SOM2(ISOM,1))) INDICP(IPOIN) = 0
          ENDDO
!
        ENDDO
!
      ENDIF
!
      DO IELEM = 1,NELEM
        INDICE(IELEM) = INDICP(IKLE(IELEM,1))
     &              + 2*INDICP(IKLE(IELEM,2))
     &              + 4*INDICP(IKLE(IELEM,3))
      ENDDO
!
!=======================================================================
!      DIVISION DES ELEMENTS PAR 4 OU PAR 2
!=======================================================================
!
      IPOIN = 1
!
      DO IELEM = 1,NELEM
!
        IF (INDICE(IELEM).EQ.7) THEN
!
          NO1 = IKLE(IELEM,1)
          NO2 = IKLE(IELEM,2)
          NO3 = IKLE(IELEM,3)
!
          NP1 = NPOIN + IPOIN
          NP2 = NP1   + 1
          NP3 = NP2   + 1
!
          NE1 = NELEM + IPOIN
          NE2 = NE1   + 1
          NE3 = NE2   + 1
!
          IPOIN = IPOIN + 3
!
          X(NP1) = 0.5D0 * ( X(NO1) + X(NO2) )
          X(NP2) = 0.5D0 * ( X(NO2) + X(NO3) )
          X(NP3) = 0.5D0 * ( X(NO3) + X(NO1) )
!
          Y(NP1) = 0.5D0 * ( Y(NO1) + Y(NO2) )
          Y(NP2) = 0.5D0 * ( Y(NO2) + Y(NO3) )
          Y(NP3) = 0.5D0 * ( Y(NO3) + Y(NO1) )
!
          NCOLOR(NP1) = NCOLOR(NO1)
          NCOLOR(NP2) = NCOLOR(NO2)
          NCOLOR(NP3) = NCOLOR(NO3)
!
          IKLE(IELEM,2) = NP1
          IKLE(IELEM,3) = NP3
!
          IKLE(  NE1,1) = NP1
          IKLE(  NE1,2) = NO2
          IKLE(  NE1,3) = NP2
!
          IKLE(  NE2,1) = NP3
          IKLE(  NE2,2) = NP2
          IKLE(  NE2,3) = NO3
!
          IKLE(  NE3,1) = NP2
          IKLE(  NE3,2) = NP3
          IKLE(  NE3,3) = NP1
!
          XEL(IELEM,1) = X(NO1)
          XEL(IELEM,2) = X(NP1)
          XEL(IELEM,3) = X(NP3)
!
          XEL(  NE1,1) = X(NP1)
          XEL(  NE1,2) = X(NO2)
          XEL(  NE1,3) = X(NP2)
!
          XEL(  NE2,1) = X(NP3)
          XEL(  NE2,2) = X(NP2)
          XEL(  NE2,3) = X(NO3)
!
          XEL(  NE3,1) = X(NP2)
          XEL(  NE3,2) = X(NP3)
          XEL(  NE3,3) = X(NP1)
!
          YEL(IELEM,1) = Y(NO1)
          YEL(IELEM,2) = Y(NP1)
          YEL(IELEM,3) = Y(NP3)
!
          YEL(  NE1,1) = Y(NP1)
          YEL(  NE1,2) = Y(NO2)
          YEL(  NE1,3) = Y(NP2)
!
          YEL(  NE2,1) = Y(NP3)
          YEL(  NE2,2) = Y(NP2)
          YEL(  NE2,3) = Y(NO3)
!
          YEL(  NE3,1) = Y(NP2)
          YEL(  NE3,2) = Y(NP3)
          YEL(  NE3,3) = Y(NP1)
!
          IF(PRESENT(CORR)) THEN
            CORR(IELEM,LEVEL) = IELEM
            CORR(NE1  ,LEVEL) = IELEM
            CORR(NE2  ,LEVEL) = IELEM
            CORR(NE3  ,LEVEL) = IELEM
          ENDIF
!
        ELSEIF (INDICE(IELEM).EQ.3.OR.
     &          INDICE(IELEM).EQ.5.OR.
     &          INDICE(IELEM).EQ.6) THEN
!
          IF (INDICE(IELEM).EQ.3) THEN
            NO1 = IKLE(IELEM,1)
            NO2 = IKLE(IELEM,2)
            NO3 = IKLE(IELEM,3)
          ELSEIF (INDICE(IELEM).EQ.5) THEN
            NO1 = IKLE(IELEM,3)
            NO2 = IKLE(IELEM,1)
            NO3 = IKLE(IELEM,2)
          ELSE
            NO1 = IKLE(IELEM,2)
            NO2 = IKLE(IELEM,3)
            NO3 = IKLE(IELEM,1)
          ENDIF
!
          NP1 = NPOIN + IPOIN
!
          NE1 = NELEM + IPOIN
!
          IPOIN = IPOIN + 1
!
          X(NP1) = 0.5D0 * ( X(NO1) + X(NO2) )
!
          Y(NP1) = 0.5D0 * ( Y(NO1) + Y(NO2) )
!
          NCOLOR(NP1) = NCOLOR(NO1)
!
          IKLE(IELEM,1) = NO1
          IKLE(IELEM,2) = NP1
          IKLE(IELEM,3) = NO3
!
          IKLE(  NE1,1) = NO2
          IKLE(  NE1,2) = NO3
          IKLE(  NE1,3) = NP1
!
        ENDIF
!
      ENDDO !IELEM
!
      NPOIN = NPOIN + IPOIN - 1
      NELEM = NELEM + IPOIN - 1
!
!=======================================================================
!  SORTIE LISTING
!=======================================================================
!
      IF (LNG.EQ.1) WRITE(LU,40) NPOIN,NELEM
      IF (LNG.EQ.2) WRITE(LU,50) NPOIN,NELEM
40    FORMAT(//,1X,'DIVISION PAR 4 DES ELEMENTS',
     &        /,1X,'---------------------------',/,
     &        /,1X,'NOUVEAU NOMBRE DE POINTS   :',I9,
     &        /,1X,'NOUVEAU NOMBRE D''ELEMENTS  :',I9)
50    FORMAT(//,1X,'CUTTING ELEMENTS BY 4',
     &        /,1X,'---------------------',/,
     &        /,1X,'NEW NUMBER OF POINTS   : ',I9,
     &        /,1X,'NEW NUMBER OF ELEMENTS : ',I9)
!
      RETURN
      END SUBROUTINE
