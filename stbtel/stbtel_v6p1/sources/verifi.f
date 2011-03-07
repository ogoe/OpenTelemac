!                       *****************
                        SUBROUTINE VERIFI
!                       *****************
!
     &( X , Y ,IKLE , NCOLOR , TRAV1 , EPSI )
!
!***********************************************************************
! PROGICIEL : STBTEL V5.2         09/08/89    J-C GALLAND  (LNH)
!***********************************************************************
!
!     FONCTION  :  ELIMINATION DES TROUS DANS LA NUMEROTATION DES NOEUDS
!                  ET RE-ORIENTATION DES ELEMENTS DU MAILLAGE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   X,Y          |<-->| COORDONNEES DU MAILLAGE .
! |   IKLE         |<-->| NUMEROS GLOBAUX DES NOEUDS DE CHAQUE ELEMENT
! |   NCOLOR       |<-->| TABLEAU DES COULEURS DES POINTS DU MAILLAGE
! |   TRAV1,2      |<-->| TABLEAUX DE TRAVAIL
! |   EPSI         | -->| DISTANCE MINIMALE ENTRE 2 NOEUDS DU MAILLAGE
! |________________|____|______________________________________________
! | COMMON:        |    |
! |  GEO:          |    |
! |    MESH        | -->| TYPE DES ELEMENTS DU MAILLAGE
! |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
! |    NPOIN       |<-->| NOMBRE TOTAL DE NOEUDS DU MAILLAGE
! |    NELEM       |<-->| NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
! |    NPMAX       | -->| DIMENSION EFFECTIVE DES TABLEAUX X ET Y
! |                |    | (NPMAX = NPOIN + 0.1*NELEM)
! |    NELMAX      | -->| DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
! |                |    | LES ELEMENTS (NELMAX = NELEM + 0.2*NELEM)
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!----------------------------------------------------------------------
! APPELE PAR : STBTEL
! APPEL DE : REMAIL, CIRCUL
!***********************************************************************
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER MESH , NDP , NELEM , NPOIN , NELMAX , NPMAX
      INTEGER IKLE(NELMAX,4) , NCOLOR(*)
      INTEGER ITEST , ITEST1 , IELEM
      INTEGER TRAV1(*)
!
      DOUBLE PRECISION X(*) , Y(*) , EPSI
!
! COMMON
!
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
!
!=======================================================================
! ON VERIFIE QUE TOUS LES POINTS SONT DISTINCTS
!=======================================================================
!
      CALL REMAIL (IKLE,NCOLOR,TRAV1,X,Y,EPSI)
!
!=======================================================================
! ON VERIFIE QUE TOUS LES ELEMENTS SONT CORRECTEMENT ORIENTES
!=======================================================================
!
      ITEST = 0
!
! CAS DES QUADRANGLES
!
      IF (MESH.EQ.2) THEN
!
         DO 70 IELEM=1,NELEM
!
            ITEST1 = 0
            CALL CIRCUL (IKLE,ITEST1,IELEM,1,2,3,X,Y)
            CALL CIRCUL (IKLE,ITEST1,IELEM,2,3,4,X,Y)
            CALL CIRCUL (IKLE,ITEST1,IELEM,3,4,1,X,Y)
            CALL CIRCUL (IKLE,ITEST1,IELEM,4,1,2,X,Y)
            IF (ITEST1.GT.0) ITEST = ITEST + 1
!
 70      CONTINUE
!
! CAS DES TRIANGLES
!
      ELSE IF (MESH.EQ.3) THEN
!
         DO 80 IELEM=1,NELEM
!
            ITEST1 = 0
            CALL CIRCUL (IKLE,ITEST1,IELEM,1,2,3,X,Y)
            IF (ITEST1.GT.0) ITEST = ITEST + 1
!
 80      CONTINUE
!
      ELSE
         IF (LNG.EQ.1) WRITE(LU,90) MESH
         IF (LNG.EQ.2) WRITE(LU,3090) MESH
 90      FORMAT(/,' LECSTB TYPE DE MAILLAGE NON PREVU , MESH = ',I4)
 3090    FORMAT(/,' LECSTB TYPE OF MESH NOT AVAILABLE , MESH = ',I4)
      ENDIF
!
      IF (LNG.EQ.1) WRITE(LU,100) ITEST
      IF (LNG.EQ.2) WRITE(LU,3100) ITEST
 100  FORMAT(1X,'NOMBRE D''ELEMENTS MAL ORIENTES : ',I5)
 3100 FORMAT(1X,'NUMBER OF ELEMENTS BADLY ORIENTED : ',I5)
!
      RETURN
      END