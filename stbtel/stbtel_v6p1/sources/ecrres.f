!                       *****************
                        SUBROUTINE ECRRES
!                       *****************
!
     &(VAINIT,IKINIT,NPINIT,NEINIT,SHP,ELT,NPOIN,NPOIN1,NPMAX,W,
     & X,ZF,NSFOND,NCOLOR,COLOR,VAR,NVARIN,NVAROU,STD,NDP,IKLES,
     & STOTOT,TPSFIN,NGEO,NRES)
!
!***********************************************************************
! PROGICIEL : STBTEL  V5.2           11/02/93    J.M. JANIN
!                               26/02/99    P. LANG (SOGREAH)
!***********************************************************************
!
!   FONCTION  : FIN D'ECRITURE DU FICHIER RESULTAT DANS LE CAS DE
!               L'OPTION D'ELIMINATION DES ELEMENTS SECS
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! | VAINIT         |--->| TABLEAU DB PPREC SEVANT A LIRE LES VARIABLES
! | IKINIT         |--->| TABLEAU IKLE DU FICHIER INITIAL
! | NPINIT         |--->| NOMBRE DE POINTS DU FICHIER INITIAL
! | NEINIT         |--->| NOMBRE D'ELEMENTS DU FICHIER INITIAL
! | SHP            |--->| MATRICE D'INTERPOLATION DES POINTS SUR LE MAILLAGE INITIAL
! | ELT            |--->| TAB INDIQUANT L'ELEMENT INITIAL AUQUEL LE POINT APPARTIENT
! | NPOIN          |--->| NOMBRE DE POINTS ACTUEL
! | NPOIN1         |--->| NOMBRE DE POINTS INITIAL
! |    NPMAX       | -->| DIMENSION EFFECTIVE DES TABLEAUX X ET Y
! |                |    | (NPMAX = NPOIN + 0.1*NELEM)
! | NEINIT         |--->| NOMBRE D'ELEMENTS DU FICHIER INITIAL
! | NEINIT         |--->| NOMBRE D'ELEMENTS DU FICHIER INITIAL
! | NEINIT         |--->| NOMBRE D'ELEMENTS DU FICHIER INITIAL
! | NEINIT         |--->| NOMBRE D'ELEMENTS DU FICHIER INITIAL
! | NEINIT         |--->| NOMBRE D'ELEMENTS DU FICHIER INITIAL
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
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! APPELE PAR : HOMERE
! APPEL DE : -
!***********************************************************************
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      DOUBLE PRECISION XBID(1)
      REAL W(*)
      INTEGER IBID(1)
      CHARACTER*72 CBID
!
      INTEGER   NDP
!
      INTEGER NEINIT,NPOIN
      INTEGER IKINIT(NEINIT,3),ELT(NPOIN),NCOLOR(NPOIN)
      INTEGER NPINIT,NPOIN1,NPMAX
      INTEGER NSFOND,NVARIN,NVAROU,IB(10)
      INTEGER NGEO,NRES,IVAR,IPOIN,ISTAT
!
      DOUBLE PRECISION VAINIT(NPINIT),SHP(NPMAX,3), TPSFIN(1)
      DOUBLE PRECISION X(NPOIN),ZF(NPOIN),VAR(NPOIN),A(2)
!
      LOGICAL STOTOT
      INTEGER IKLES(NDP,NEINIT)
      CHARACTER*3  STD
      CHARACTER*32 NOMVAR
!
      INTEGER I
      LOGICAL COLOR,EOF
      EXTERNAL EOF
!
!=======================================================================
!
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
!
!  ECRITURE DU TEMPS
!
10    CONTINUE
!
      A(1) = 0.D0
      IF (NVARIN.GT.0) THEN
         IF (EOF(NGEO)) GOTO 40
         CALL LIT(A,W,IBID,CBID,1,'R4',NGEO,STD,ISTAT)
      ENDIF
      IF (STOTOT.OR.A(1).EQ.TPSFIN(1)) THEN
!
        IF (LNG.EQ.1) WRITE (LU,9000) A(1)
        IF (LNG.EQ.2) WRITE (LU,9001) A(1)
        IF (STD(1:3).EQ.'IBM') THEN
           A(2) = 0.D0
           CALL ECRI2(A,IBID,CBID,2,'R4',NRES,STD,ISTAT)
        ELSE
           CALL ECRI2(A,IBID,CBID,1,'R4',NRES,STD,ISTAT)
        ENDIF
      ENDIF
!
!=======================================================================
!
!  ECRITURE DES VARIABLES
!
      IF(NVARIN.GT.0) THEN
!
         DO 20 IVAR = 1,NVARIN
            CALL LIT(VAINIT,W,IBID,CBID,NPOIN1,'R4',NGEO,STD,ISTAT)
!
            IF (STOTOT.OR.A(1).EQ.TPSFIN(1)) THEN
              IF (IVAR.EQ.NSFOND) THEN
               CALL ECRI2(ZF,IBID,CBID,NPOIN,'R4',NRES,STD,ISTAT)
              ELSE
               DO 30 IPOIN = 1,NPOIN
                  VAR(IPOIN) = VAINIT(IKINIT(ELT(IPOIN),1))*SHP(IPOIN,1)
     &                       + VAINIT(IKINIT(ELT(IPOIN),2))*SHP(IPOIN,2)
     &                       + VAINIT(IKINIT(ELT(IPOIN),3))*SHP(IPOIN,3)
30             CONTINUE
               CALL ECRI2(VAR,IBID,CBID,NPOIN,'R4',NRES,STD,ISTAT)
              ENDIF
            ENDIF
20       CONTINUE
!
      ENDIF
!
      IF (STOTOT.OR.A(1).EQ.TPSFIN(1)) THEN
        IF (NSFOND.EQ.NVARIN+1) THEN
          CALL ECRI2(ZF,IBID,CBID,NPOIN,'R4',NRES,STD,ISTAT)
        ENDIF
        IF (COLOR)
     &    CALL ECRI2(XBID,NCOLOR,CBID,NPOIN,'I ',NRES,STD,ISTAT)
        IF (NVAROU.EQ.0)
     &    CALL ECRI2(X,IBID,CBID,NPOIN,'R4',NRES,STD,ISTAT)
      ENDIF
!
      IF (NVARIN.GT.0) GOTO 10
!
40    CONTINUE
!
!=======================================================================
!
 9000 FORMAT (1X,'************************************************',/,
     &        1X,'  ROUTINE ECRRES - TEMPS ECRIT DANS LE FICHIER ',/,
     &        1X,'  DE SORTIE : ',F8.1,' SEC.',/,
     &        1X,'************************************************')
 9001 FORMAT (1X,'************************************************',/,
     &        1X,'  ROUTINE ECRRES - TIME STORED IN THE OUTPUT ',/,
     &        1X,'  FILE : ',F8.1,' SEC.',/,
     &        1X,'************************************************')
      RETURN
      END