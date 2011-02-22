C                       *****************
                        SUBROUTINE ECRRES
C                       *****************
C
     *(VAINIT,IKINIT,NPINIT,NEINIT,SHP,ELT,NPOIN,NPOIN1,NPMAX,W,
     * X,ZF,NSFOND,NCOLOR,COLOR,VAR,NVARIN,NVAROU,STD,NDP,IKLES,
     * STOTOT,TPSFIN,NGEO,NRES)
C
C***********************************************************************
C PROGICIEL : STBTEL  V5.2           11/02/93    J.M. JANIN
C                               26/02/99    P. LANG (SOGREAH)
C***********************************************************************
C
C   FONCTION  : FIN D'ECRITURE DU FICHIER RESULTAT DANS LE CAS DE
C               L'OPTION D'ELIMINATION DES ELEMENTS SECS 
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C | VAINIT         |--->| TABLEAU DB PPREC SEVANT A LIRE LES VARIABLES
C | IKINIT         |--->| TABLEAU IKLE DU FICHIER INITIAL
C | NPINIT         |--->| NOMBRE DE POINTS DU FICHIER INITIAL
C | NEINIT         |--->| NOMBRE D'ELEMENTS DU FICHIER INITIAL
C | SHP            |--->| MATRICE D'INTERPOLATION DES POINTS SUR LE MAILLAGE INITIAL
C | ELT            |--->| TAB INDIQUANT L'ELEMENT INITIAL AUQUEL LE POINT APPARTIENT
C | NPOIN          |--->| NOMBRE DE POINTS ACTUEL
C | NPOIN1         |--->| NOMBRE DE POINTS INITIAL
C |    NPMAX       | -->| DIMENSION EFFECTIVE DES TABLEAUX X ET Y
C |                |    | (NPMAX = NPOIN + 0.1*NELEM)
C | NEINIT         |--->| NOMBRE D'ELEMENTS DU FICHIER INITIAL
C | NEINIT         |--->| NOMBRE D'ELEMENTS DU FICHIER INITIAL
C | NEINIT         |--->| NOMBRE D'ELEMENTS DU FICHIER INITIAL
C | NEINIT         |--->| NOMBRE D'ELEMENTS DU FICHIER INITIAL
C | NEINIT         |--->| NOMBRE D'ELEMENTS DU FICHIER INITIAL
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
C
      INTEGER   NDP  
 
      INTEGER NEINIT,NPOIN
      INTEGER IKINIT(NEINIT,3),ELT(NPOIN),NCOLOR(NPOIN)
      INTEGER NPINIT,NPOIN1,NPMAX
      INTEGER NSFOND,NVARIN,NVAROU,IB(10)
      INTEGER NGEO,NRES,IVAR,IPOIN,ISTAT 
C
      DOUBLE PRECISION VAINIT(NPINIT),SHP(NPMAX,3), TPSFIN(1)
      DOUBLE PRECISION X(NPOIN),ZF(NPOIN),VAR(NPOIN),A(2)
C
      LOGICAL STOTOT
      INTEGER IKLES(NDP,NEINIT)
      CHARACTER*3  STD
      CHARACTER*32 NOMVAR
C
      INTEGER I
      LOGICAL COLOR,EOF 
      EXTERNAL EOF
C
C=======================================================================
C
      REWIND NGEO
      CALL LIT(XBID,W ,IBID,CBID,72,'CH',NGEO,STD,ISTAT)
      CALL LIT(XBID,W ,IB  ,CBID, 2,'I ',NGEO,STD,ISTAT)
      DO  5 I=1,NVARIN
         CALL LIT(XBID,W ,IBID,NOMVAR,32,'CH',NGEO,STD,ISTAT)
 5    CONTINUE
      CALL LIT(XBID,W ,IB  ,CBID,10,'I ',NGEO,STD,ISTAT)
      IF (IB(10).EQ.1) THEN
       CALL LIT(XBID,W ,IB  ,CBID, 6,'I ',NGEO,STD,ISTAT)
      ENDIF
      CALL LIT(XBID,W ,IB  ,CBID, 4,'I ',NGEO,STD,ISTAT)
      CALL LIT(XBID,W,IKLES,CBID,NEINIT*NDP,'I ',NGEO,STD,ISTAT)
      CALL LIT(XBID,W,IB,CBID, 1,'I ',NGEO,STD,ISTAT)
      CALL LIT(X   ,W,IBID,CBID,NPINIT,'R4',NGEO,STD,ISTAT)
      CALL LIT(X   ,W,IBID,CBID,NPINIT,'R4',NGEO,STD,ISTAT)

C  ECRITURE DU TEMPS
C
10    CONTINUE
C
      A(1) = 0.D0
      IF (NVARIN.GT.0) THEN
         IF (EOF(NGEO)) GOTO 40
         CALL LIT(A,W,IBID,CBID,1,'R4',NGEO,STD,ISTAT)
      ENDIF
      IF (STOTOT.OR.A(1).EQ.TPSFIN(1)) THEN
C
        IF (LNG.EQ.1) WRITE (LU,9000) A(1)
        IF (LNG.EQ.2) WRITE (LU,9001) A(1)
        IF (STD(1:3).EQ.'IBM') THEN
           A(2) = 0.D0
           CALL ECRI2(A,IBID,CBID,2,'R4',NRES,STD,ISTAT)
        ELSE
           CALL ECRI2(A,IBID,CBID,1,'R4',NRES,STD,ISTAT)
        ENDIF
      ENDIF
C
C=======================================================================
C
C  ECRITURE DES VARIABLES
C
      IF(NVARIN.GT.0) THEN
C
         DO 20 IVAR = 1,NVARIN
            CALL LIT(VAINIT,W,IBID,CBID,NPOIN1,'R4',NGEO,STD,ISTAT)
C
            IF (STOTOT.OR.A(1).EQ.TPSFIN(1)) THEN
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
            ENDIF
20       CONTINUE
C
      ENDIF
C
      IF (STOTOT.OR.A(1).EQ.TPSFIN(1)) THEN
        IF (NSFOND.EQ.NVARIN+1) THEN
          CALL ECRI2(ZF,IBID,CBID,NPOIN,'R4',NRES,STD,ISTAT)
        ENDIF
        IF (COLOR)
     *    CALL ECRI2(XBID,NCOLOR,CBID,NPOIN,'I ',NRES,STD,ISTAT)
        IF (NVAROU.EQ.0)
     *    CALL ECRI2(X,IBID,CBID,NPOIN,'R4',NRES,STD,ISTAT)
      ENDIF
C
      IF (NVARIN.GT.0) GOTO 10
C
40    CONTINUE
C
C=======================================================================
C
 9000 FORMAT (1X,'************************************************',/,
     *        1X,'  ROUTINE ECRRES - TEMPS ECRIT DANS LE FICHIER ',/,
     *        1X,'  DE SORTIE : ',F8.1,' SEC.',/,
     *        1X,'************************************************')
 9001 FORMAT (1X,'************************************************',/,
     *        1X,'  ROUTINE ECRRES - TIME STORED IN THE OUTPUT ',/,
     *        1X,'  FILE : ',F8.1,' SEC.',/,
     *        1X,'************************************************')
      RETURN
      END
