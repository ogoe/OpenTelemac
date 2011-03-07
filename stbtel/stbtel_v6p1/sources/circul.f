!                       *****************
                        SUBROUTINE CIRCUL
!                       *****************
!
     &(IKLE,ITEST1 ,IELEM,I1,I2,I3,X,Y)
!
!***********************************************************************
!  STBTEL VERSION 5.2   16/08/89    J.C. GALLAND   (LNH)
!***********************************************************************
!
!    FONCTION : CALCUL DE L'AIRE FORMEE PAR LES TROIS POINTS I1,I2,I3
!               ET PERMUTATION DES POINTS I2 ET I3 LORSQU'ELLE EST
!               NEGATIVE.
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |    IKLE        |<-->| NUMEROS GLOBAUX DES POINTS DE CHAQUE ELEMENT |
! |    ITEST1      | -->| COMPTEUR                                     |
! |    IELEM       | -->| NUMERO DE L'ELEMENT COURANT                  |
! |    I1,I2,I3    | -->| PERMUTATION DES IKLE                         |
! |    X,Y         | -->| COORDONNEES DES POINTS DU MAILLAGE           |
! |________________|____|______________________________________________
! | COMMON:        |    |
! |  GEO:          |    |
! |    MESH        | -->| TYPE DES ELEMENTS DU MAILLAGE
! |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
! |    NPOIN       | -->| NOMBRE TOTAL DE NOEUDS DU MAILLAGE
! |    NELEM       | -->| NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
! |    NPMAX       | -->| DIMENSION EFFECTIVE DES TABLEAUX X ET Y
! |                |    | (NPMAX = NPOIN + 0.1*NELEM)
! |    NELMAX      | -->| DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
! |                |    | LES ELEMENTS (NELMAX = NELEM + 0.2*NELEM)
! |________________|____|______________________________________________|
!  MODE: -->(DONNEE NON MODIFIEE),<--(RESULTAT),<-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! APPELE PAR : LECSTB
! APPEL DE :
!***********************************************************************
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER IELEM , NELEM , NPOIN , MESH , NDP , NELMAX , NPMAX
      INTEGER IKLE(NELMAX,4) , I1 , I2 , I3 , ITEST1 , I
!
      DOUBLE PRECISION X2 , X3 , Y2 , Y3 , X(*) , Y(*)
      DOUBLE PRECISION AIRE
!
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
!
         X2 = X(IKLE(IELEM,I2))-X(IKLE(IELEM,I1))
         X3 = X(IKLE(IELEM,I3))-X(IKLE(IELEM,I1))
!
         Y2 = Y(IKLE(IELEM,I2))-Y(IKLE(IELEM,I1))
         Y3 = Y(IKLE(IELEM,I3))-Y(IKLE(IELEM,I1))
!
         AIRE = X2*Y3 - X3*Y2
!
         IF (AIRE.LT.0.D0) THEN
            ITEST1 = ITEST1 + 1
            I = IKLE(IELEM,I2)
!
            IKLE(IELEM,I2) = IKLE(IELEM,I3)
            IKLE(IELEM,I3) = I
         ENDIF
!
         RETURN
         END