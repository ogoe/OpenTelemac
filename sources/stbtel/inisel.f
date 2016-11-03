!                       *****************
                        SUBROUTINE INISEL
!                       *****************
!
     &(NPOIN1,TYPELE,STD,NSFOND,FUSION,IHAUT,NGEO,NFO1)
!
!***********************************************************************
! PROGICIEL : STBTEL  V5.2           11/02/93    J.M. JANIN
!***********************************************************************
!
!   FONCTION  : RECHERCHE LES NOMBRES TOTAUX DE NOEUDS ET D'ELEMENTS DU
!               MAILLAGE DANS LE FICHIER D'ENTREE SELAFIN
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! | NPOIN1         |<-- | NOMBRE REEL DE POINTS DU MAILLAGE
! |                |    | (NPOIN REPRESENTE L'INDICE MAX DES NOEUDS CAR
! |                |    | SUPERTAB LAISSE DES TROUS DANS LA NUMEROTATION
! | TYPELE         |<-- | TYPE D'ELEMENTS
! | IHAUT          |<-- | NUMERO DE LA VARIABLE HAUTEUR D'EAU
! |________________|____|______________________________________________
! | COMMON:        |    |
! |  GEO:          |    |
! |    MESH        |<-- | TYPE DES ELEMENTS DU MAILLAGE
! |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
! |    NPOIN       |<-- | NOMBRE TOTAL DE NOEUDS DU MAILLAGE
! |    NELEM       |<-- | NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
! |    NPMAX       | -->| DIMENSION EFFECTIVE DES TABLEAUX X ET Y
! |                |    | (NPMAX = NPOIN + 0.1*NELEM)
! |    NELMAX      | -->| DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
! |                |    | LES ELEMENTS (NELMAX = NELEM + 0.2*NELEM)
! |  FICH:         |    |
! |    NRES        |--> | NUMERO DU CANAL DU FICHIER DE SERAFIN
! |    NGEO        |--> | NUMERO DU CANAL DU FICHIER MAILLEUR
! |    NLIM        |--> | NUMERO DU CANAL DU FICHIER DYNAM DE TELEMAC
! |    NFO1        |--> | NUMERO DU CANAL DU FICHIER TRIANGLE TRIGRID
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! APPELE PAR : HOMERE
! APPEL DE : -
!***********************************************************************
!
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_STBTEL, ONLY: NELEM,MESH,NDP,NPOIN,NELMAX,NPMAX
      USE INTERFACE_STBTEL, EX_INISEL => INISEL
      IMPLICIT NONE
!
      INTEGER, INTENT(INOUT) :: NPOIN1,NSFOND
      CHARACTER(LEN=11), INTENT(INOUT) :: TYPELE
      CHARACTER(LEN=3), INTENT(IN) :: STD
      LOGICAL, INTENT(IN) :: FUSION
      INTEGER, INTENT(INOUT) :: IHAUT
      INTEGER, INTENT(IN) :: NGEO , NFO1
!
      DOUBLE PRECISION XBID(1)
!
      REAL WBID(1)
!
      INTEGER IBID(1) , NVAR , I , IB(10) , ISTAT
!
!
      CHARACTER(LEN=72) CBID
      CHARACTER(LEN=32) NOMVAR
!
! COMMON
!
!
!
!=======================================================================
! INITIALISATION
!=======================================================================
!
      REWIND NGEO
!
!=======================================================================
! LECTURE SEQUENTIELLE DU FICHIER ET RECHERCHE ENREGISTREMENT 5
!=======================================================================
!
      CALL LIT(XBID,WBID,IBID,CBID,72,'CH',NGEO,STD,ISTAT)
      CALL LIT(XBID,WBID,IB  ,CBID, 2,'I ',NGEO,STD,ISTAT)
      NVAR = IB(1) + IB(2)
      IHAUT = 0
      DO I=1,NVAR
        CALL LIT(XBID,WBID,IBID,NOMVAR,32,'CH',NGEO,STD,ISTAT)
        IF( NOMVAR(1:16) .EQ. 'FOND            ' .OR.
     &      NOMVAR(1:16) .EQ. 'BOTTOM          ' ) NSFOND = I
        IF( NOMVAR(1:16) .EQ. 'HAUTEUR D''EAU   ' .OR.
     &      NOMVAR(1:16) .EQ. 'WATER DEPTH     ' ) IHAUT = I
      ENDDO
      CALL LIT(XBID,WBID,IB  ,CBID,10,'I ',NGEO,STD,ISTAT)
!
!     CORRECTION JMH 07/03/2001
!
!     CAS OU IL Y A UNE DATE DANS LE FICHIER
      IF(IB(10).EQ.1) THEN
        CALL LIT(XBID,WBID,IB  ,CBID,6,'I ',NGEO,STD,ISTAT)
      ENDIF
!
!     FIN CORRECTION JMH 07/03/2001
!
      CALL LIT(XBID,WBID,IB  ,CBID, 4,'I ',NGEO,STD,ISTAT)
!
!=======================================================================
! AFFECTATION DES VALEURS LUES AUX VARIABLES CONCERNEES
!=======================================================================
!
      NELEM = IB(1)
      NPOIN = IB(2)
      NDP   = IB(3)
      NPOIN1= NPOIN
!
!=======================================================================
! LECTURE SEQUENTIELLE DU SECOND FICHIER EN CAS DE FUSION
!=======================================================================
!
      IF (FUSION) THEN
!
        REWIND NFO1
!
        CALL LIT(XBID,WBID,IBID,CBID,72,'CH',NFO1,STD,ISTAT)
        CALL LIT(XBID,WBID,IB  ,CBID, 2,'I ',NFO1,STD,ISTAT)
        NVAR = IB(1) + IB(2)
        DO I=1,NVAR
          CALL LIT(XBID,WBID,IBID,NOMVAR,32,'CH',NFO1,STD,ISTAT)
        ENDDO
        CALL LIT(XBID,WBID,IB  ,CBID,10,'I ',NFO1,STD,ISTAT)
        CALL LIT(XBID,WBID,IB  ,CBID, 4,'I ',NFO1,STD,ISTAT)
!
        NELEM = NELEM + IB(1)
        NPOIN = NPOIN + IB(2)
!
        IF (NDP.NE.IB(3)) THEN
          IF (LNG.EQ.1) WRITE(LU,130)
          IF (LNG.EQ.2) WRITE(LU,3130)
 130      FORMAT(' INISEL : TYPES DE MAILLAGE HETEROGENES')
 3130     FORMAT(' INISEL : TYPES OF MESH INHOMOGENEOUS')
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDIF
!
!=======================================================================
! MISE DES VALEURS DE MESH AU STANDARD TELEMAC
!=======================================================================
!
      IF (NDP.EQ.4) THEN
        MESH = 2
        TYPELE = 'QUADRANGLES'
      ELSEIF (NDP.EQ.3) THEN
        MESH = 3
        TYPELE = 'TRIANGLES  '
      ELSE
        IF (LNG.EQ.1) WRITE(LU,140) MESH
        IF (LNG.EQ.2) WRITE(LU,3140) MESH
 140    FORMAT(' INISEL : TYPE DE MAILLAGE NON PREVU DANS TELEMAC,
     &           MESH = ',I4)
 3140   FORMAT(' INISEL : TYPE OF MESH NOT AVAILABLE IN TELEMAC,
     &           MESH = ',I4)
        CALL PLANTE(1)
        STOP
      ENDIF
!
      RETURN
      END
