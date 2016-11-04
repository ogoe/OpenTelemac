!                       *****************
                        SUBROUTINE ECRSEL
!                       *****************
!
     &(VAINIT,IKINIT,NPINIT,NEINIT,SHP,ELT,NPOIN,NPOIN1,NPMAX,W,
     & X,ZF,NSFOND,NCOLOR,COLOR,VAR,NVARIN,NVAROU,NVAR2,STD,FUSION,
     & NRES,NGEO,NFO1,MAILLE)
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
      USE INTERFACE_STBTEL, EX_ECRSEL => ECRSEL
      IMPLICIT NONE
!
      INTEGER, INTENT(IN) :: NPINIT,NEINIT,NPOIN,NPMAX
      INTEGER, INTENT(INOUT) :: NPOIN1,NVAR2
      DOUBLE PRECISION, INTENT(INOUT) :: VAINIT(NPINIT)
      DOUBLE PRECISION, INTENT(IN) :: SHP(NPMAX,3)
      INTEGER, INTENT(IN) :: IKINIT(NEINIT,3),ELT(NPOIN)
      REAL, INTENT(INOUT) :: W(*)
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: ZF(NPOIN)
      INTEGER, INTENT(IN) :: NSFOND
      INTEGER, INTENT(IN) :: NCOLOR(NPOIN)
      LOGICAL, INTENT(IN) :: COLOR,FUSION
      DOUBLE PRECISION, INTENT(INOUT) :: VAR(NPOIN)
      INTEGER, INTENT(IN) :: NVARIN,NVAROU
      CHARACTER(LEN=3), INTENT(IN) :: STD
      INTEGER, INTENT(IN) :: NGEO,NRES,NFO1
      CHARACTER(LEN=9), INTENT(IN) :: MAILLE
!
      DOUBLE PRECISION XBID(1)
      INTEGER IBID(1)
      CHARACTER(LEN=72) CBID
!
      INTEGER NPOIN2
      INTEGER IVAR,IPOIN,ISTAT,I
!
      DOUBLE PRECISION A(2)
!
      LOGICAL BIEF_EOF,OK2
      EXTERNAL BIEF_EOF
!
!=======================================================================
!
      OK2 = FUSION
      NPOIN2 = NPINIT - NPOIN1
!
!  ECRITURE DU TEMPS
!
10    CONTINUE
!
      A(1) = 0.D0
      IF(NVARIN.GT.0) THEN
        IF (BIEF_EOF(NGEO)) GOTO 40
        IF (OK2) THEN
          IF (BIEF_EOF(NFO1)) OK2 = .FALSE.
          IF (OK2) CALL LIT(A,W,IBID,CBID,1,'R4',NFO1,STD,ISTAT)
        ENDIF
        CALL LIT(A,W,IBID,CBID,1,'R4',NGEO,STD,ISTAT)
      ENDIF
!
      CALL ECRI2(A,IBID,CBID,1,'R4',NRES,STD,ISTAT)
!
!=======================================================================
!
!  ECRITURE DES VARIABLES
!
      IF (NVARIN.GT.0) THEN
        DO IVAR = 1,NVARIN
!
!         ZF DEJA LU AVEC ADCIRC
          IF(MAILLE.NE.'ADCIRC') THEN
            CALL LIT(VAINIT,W,IBID,CBID,NPOIN1,'R4',NGEO,STD,ISTAT)
          ENDIF
!
          IF (FUSION) THEN
            IF (OK2.AND.IVAR.LE.NVAR2) THEN
              CALL LIT(VAINIT(NPOIN1+1),W,IBID,CBID,NPOIN2,'R4',
     &                 NFO1,STD,ISTAT)
            ELSE
              DO I = NPOIN1+1,NPINIT
                VAINIT(I) = 0.D0
              ENDDO
            ENDIF
          ENDIF
!
          IF (IVAR.EQ.NSFOND) THEN
            CALL ECRI2(ZF,IBID,CBID,NPOIN,'R4',NRES,STD,ISTAT)
          ELSE
            DO IPOIN = 1,NPOIN
              VAR(IPOIN) = VAINIT(IKINIT(ELT(IPOIN),1))*SHP(IPOIN,1)
     &                   + VAINIT(IKINIT(ELT(IPOIN),2))*SHP(IPOIN,2)
     &                   + VAINIT(IKINIT(ELT(IPOIN),3))*SHP(IPOIN,3)
            ENDDO
            CALL ECRI2(VAR,IBID,CBID,NPOIN,'R4',NRES,STD,ISTAT)
          ENDIF
        ENDDO
!
        IF (OK2) THEN
          IF (NVARIN.LT.NVAR2) THEN
            DO IVAR = NVARIN+1,NVAR2
              CALL LIT(VAINIT,W,IBID,CBID,2,'R4',NFO1,STD,ISTAT)
            ENDDO
          ENDIF
        ENDIF
!
      ENDIF
!
      IF(NSFOND.EQ.NVARIN+1.OR.MAILLE.EQ.'ADCIRC') THEN
        CALL ECRI2(ZF,IBID,CBID,NPOIN,'R4',NRES,STD,ISTAT)
      ENDIF
      IF(COLOR) THEN
        CALL ECRI2(XBID,NCOLOR,CBID,NPOIN,'I ',NRES,STD,ISTAT)
      ENDIF
      IF(NVAROU.EQ.0) THEN
        CALL ECRI2(X,IBID,CBID,NPOIN,'R4',NRES,STD,ISTAT)
      ENDIF
!
      IF (NVARIN.GT.0) GOTO 10
!
40    CONTINUE
!
!=======================================================================
!
      RETURN
      END
