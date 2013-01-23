!                       *****************
                        SUBROUTINE DEPARR
!                       *****************
!
     &(IKLE,NDEPAR,LGVEC)
!
!***********************************************************************
! PROGICIEL: STBTEL V5.2          28/08/89  J-M HERVOUET (LNH) 3071 8018
!***********************************************************************
!
! FONCTION : DETECTION DES DEPENDANCES ARRIERES
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |  IKLE          | -->|NUMEROS GLOBAUX DES NOEUDS DE CHAQUE ELEMENT  |
! |  NDEPAR        | -->|NOMBRE DE DEPENDANCES ARRIERES                |
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
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! APPELE PAR : STBTEL
! APPEL DE : -
!***********************************************************************
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER LGVEC , MESH , NDP , NDEPAR , IELEM , NELEM , K
      INTEGER NPMAX , NPOIN , NELMAX
      INTEGER I1 , I2 , I3 , J1 , J2 , J3 , IEL1
      INTEGER IKLE(NELMAX,4)
!
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
!
!=======================================================================
!
      NDEPAR = 0
      DO 20 IELEM = 1,NELEM
         I1 = IKLE(IELEM,1)
         I2 = IKLE(IELEM,2)
         I3 = IKLE(IELEM,3)
         DO 30 K = 2,LGVEC
            IEL1 = MOD(NELEM+IELEM-K,NELEM) + 1
            J1 = IKLE(IEL1,1)
            J2 = IKLE(IEL1,2)
            J3 = IKLE(IEL1,3)
            IF (I1.EQ.J1.OR.I1.EQ.J2.OR.I1.EQ.J3.OR.
     &          I2.EQ.J1.OR.I2.EQ.J2.OR.I2.EQ.J3.OR.
     &          I3.EQ.J1.OR.I3.EQ.J2.OR.I3.EQ.J3) NDEPAR = NDEPAR + 1
30       CONTINUE
20    CONTINUE
!
!=======================================================================
!
      RETURN
      END