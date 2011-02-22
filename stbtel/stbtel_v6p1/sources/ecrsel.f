C                       *****************
                        SUBROUTINE ECRSEL
C                       *****************
C
     *(VAINIT,IKINIT,NPINIT,NEINIT,SHP,ELT,NPOIN,NPOIN1,NPMAX,W,
     * X,ZF,NSFOND,NCOLOR,COLOR,VAR,NVARIN,NVAROU,NVAR2,STD,FUSION,
     * NRES,NGEO,NFO1,MAILLE)
C
C***********************************************************************
C PROGICIEL : STBTEL  V5.2           11/02/93    J.M. JANIN
C***********************************************************************
C
C   FONCTION  : RECHERCHE LES NOMBRES TOTAUX DE NOEUDS ET D'ELEMENTS DU
C               MAILLAGE DANS LE FICHIER D'ENTREE SELAFIN
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C | NPOIN1         |<-- | NOMBRE REEL DE POINTS DU MAILLAGE
C |                |    | (NPOIN REPRESENTE L'INDICE MAX DES NOEUDS CAR
C |                |    | SUPERTAB LAISSE DES TROUS DANS LA NUMEROTATION
C | TYPELE         |<-- | TYPE D'ELEMENTS
C |________________|____|______________________________________________
C | COMMON:        |    |
C |  GEO:          |    |
C |    MESH        |<-- | TYPE DES ELEMENTS DU MAILLAGE
C |    NDP         | -->| NOMBRE DE NOEUDS PAR ELEMENTS
C |    NPOIN       |<-- | NOMBRE TOTAL DE NOEUDS DU MAILLAGE
C |    NELEM       |<-- | NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
C |    NPMAX       | -->| DIMENSION EFFECTIVE DES TABLEAUX X ET Y
C |                |    | (NPMAX = NPOIN + 0.1*NELEM)
C |    NELMAX      | -->| DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
C |                |    | LES ELEMENTS (NELMAX = NELEM + 0.2*NELEM)
C |  FICH:         |    |
C |    NRES        |--> | NUMERO DU CANAL DU FICHIER DE SERAFIN
C |    NGEO        |--> | NUMERO DU CANAL DU FICHIER MAILLEUR
C |    NLIM        |--> | NUMERO DU CANAL DU FICHIER DYNAM DE TELEMAC
C |    NFO1        |--> | NUMERO DU CANAL DU FICHIER TRIANGLE TRIGRID
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C APPELE PAR : HOMERE
C APPEL DE : -
C***********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      DOUBLE PRECISION XBID(1)
      REAL W(*)
      INTEGER IBID(1)
      CHARACTER*72 CBID
      CHARACTER*9 MAILLE
C
      INTEGER NEINIT,NPOIN
      INTEGER IKINIT(NEINIT,3),ELT(NPOIN),NCOLOR(NPOIN)
      INTEGER NPINIT,NPOIN1,NPOIN2,NPMAX
      INTEGER NSFOND,NVARIN,NVAROU,NVAR2
      INTEGER NRES,NGEO,NFO1,IVAR,IPOIN,ISTAT,I 
C
      DOUBLE PRECISION VAINIT(NPINIT),SHP(NPMAX,3)
      DOUBLE PRECISION X(NPOIN),ZF(NPOIN),VAR(NPOIN),A(2)
C
      CHARACTER*3  STD
C
      LOGICAL COLOR,EOF,FUSION,OK2
      EXTERNAL EOF
C
C=======================================================================
C
      OK2 = FUSION
      NPOIN2 = NPINIT - NPOIN1
C
C  ECRITURE DU TEMPS
C
10    CONTINUE
C
      A(1) = 0.D0
      IF(NVARIN.GT.0) THEN
         IF (EOF(NGEO)) GOTO 40
         IF (OK2) THEN
            IF (EOF(NFO1)) OK2 = .FALSE.
            IF (OK2) CALL LIT(A,W,IBID,CBID,1,'R4',NFO1,STD,ISTAT)
         ENDIF
         CALL LIT(A,W,IBID,CBID,1,'R4',NGEO,STD,ISTAT)
      ENDIF
C
      CALL ECRI2(A,IBID,CBID,1,'R4',NRES,STD,ISTAT)
C
C=======================================================================
C
C  ECRITURE DES VARIABLES
C
      IF (NVARIN.GT.0) THEN
         DO 20 IVAR = 1,NVARIN
C
C           ZF DEJA LU AVEC ADCIRC
            IF(MAILLE.NE.'ADCIRC') THEN
             CALL LIT(VAINIT,W,IBID,CBID,NPOIN1,'R4',NGEO,STD,ISTAT)
            ENDIF
C
            IF (FUSION) THEN
               IF (OK2.AND.IVAR.LE.NVAR2) THEN
                  CALL LIT(VAINIT(NPOIN1+1),W,IBID,CBID,NPOIN2,'R4',
     *                     NFO1,STD,ISTAT)
               ELSE
                  DO 22 I = NPOIN1+1,NPINIT
                     VAINIT(I) = 0.D0
22                CONTINUE
               ENDIF
            ENDIF

            IF (IVAR.EQ.NSFOND) THEN
               CALL ECRI2(ZF,IBID,CBID,NPOIN,'R4',NRES,STD,ISTAT)
            ELSE
               DO 30 IPOIN = 1,NPOIN
                  VAR(IPOIN) = VAINIT(IKINIT(ELT(IPOIN),1))*SHP(IPOIN,1)
     *                       + VAINIT(IKINIT(ELT(IPOIN),2))*SHP(IPOIN,2)
     *                       + VAINIT(IKINIT(ELT(IPOIN),3))*SHP(IPOIN,3)
30             CONTINUE
               CALL ECRI2(VAR,IBID,CBID,NPOIN,'R4',NRES,STD,ISTAT)
            ENDIF
20       CONTINUE
C
         IF (OK2) THEN
            IF (NVARIN.LT.NVAR2) THEN
               DO 32 IVAR = NVARIN+1,NVAR2
                  CALL LIT(VAINIT,W,IBID,CBID,2,'R4',NFO1,STD,ISTAT)
32             CONTINUE
            ENDIF
         ENDIF
C
      ENDIF
C
      IF(NSFOND.EQ.NVARIN+1.OR.MAILLE.EQ.'ADCIRC') THEN
          CALL ECRI2(ZF,IBID,CBID,NPOIN,'R4',NRES,STD,ISTAT)
      ENDIF
      IF(COLOR) THEN
        CALL ECRI2(XBID,NCOLOR,CBID,NPOIN,'I ',NRES,STD,ISTAT)
      ENDIF
      IF(NVAROU.EQ.0) THEN
        CALL ECRI2(X,IBID,CBID,NPOIN,'R4',NRES,STD,ISTAT)
      ENDIF
C
      IF (NVARIN.GT.0) GOTO 10
C
40    CONTINUE
C
C=======================================================================
C
      RETURN
      END
