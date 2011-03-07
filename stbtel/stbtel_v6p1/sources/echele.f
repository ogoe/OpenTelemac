!                       *****************
                        SUBROUTINE ECHELE
!                       *****************
!
     &(IKLE, IEL1 , IEL2 )
!
!***********************************************************************
! PROGICIEL: STBTEL V5.2          28/08/89  J-M HERVOUET (LNH) 3071 8018
!***********************************************************************
!
! FONCTION : ECHANGE DES NUMEROS DE 2 ELEMENTS
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
! |      NOM       |MODE|                   ROLE                       |
! |________________|____|______________________________________________|
! |  IKLE          |<-->|VECTEUR ASSEMBLE                              |
! |  IEL1, IEL2    | -->|NUMEROS DES NOEUDS A PERMUTER                 |
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
! APPELE PAR : SHUFLE
!***********************************************************************
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER MESH , NDP , I , IEL1 , IEL2 , NPMAX , NELEM
      INTEGER NPOIN , NELMAX
      INTEGER IKLE(NELMAX,4) , STO(4)
!
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
!
!=======================================================================
!
      DO 10 I = 1 , NDP
        STO(I) = IKLE(IEL1,I)
        IKLE(IEL1,I) = IKLE(IEL2,I)
        IKLE(IEL2,I) = STO(I)
 10   CONTINUE
!
!=======================================================================
!
      RETURN
      END