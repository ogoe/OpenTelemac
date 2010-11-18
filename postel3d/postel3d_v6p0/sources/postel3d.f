C                       *******************
                        SUBROUTINE POSTEL3D
C                       *******************
C
C***********************************************************************
C POSTEL3D VERSION 5.1   01/09/99   T. DENOT (LNH) 01 30 87 74 89
C FORTRAN90
C***********************************************************************
C
C       PPPP    OOO    SSSS  TTTTT  EEEEE  L         3333   DDDD
C       P   P  O   O  S        T    E      L             3  D   D
C       PPPP   O   O   SSS     T    EEEE   L     ---  333   D   D
C       P      O   O      S    T    E      L             3  D   D
C       P       OOO   SSSS     T    EEEEE  LLLLL     3333   DDDD
C
C-----------------------------------------------------------------------
C
C SOUS-PROGRAMME APPELE PAR : HOMERE_POSTEL3D
C SOUS-PROGRAMME APPELES : LIT, PRE2DH, PRE2DV, LECR3D, COUPEH, COUPEV
C
C-----------------------------------------------------------------------
C                    DECLARATION DES TYPES ET DIMENSIONS
C-----------------------------------------------------------------------
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_POSTEL3D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C TABLEAUX DE REELS
C
      DOUBLE PRECISION SHP(IM,3,NC2DV)
C
C TABLEAUX D'ENTIERS
C
      INTEGER IKLES(3,NELEM2) , PLINF(NPOIN2)
      INTEGER IPOBO(NPOIN2)
      INTEGER INDIC(IM,JM,NC2DV) , ELEM(IM,NC2DV)
      INTEGER N,NPRE,NRES,NCOU
C
C VARIABLES LOCALES
C
      INTEGER I,J , K ,  ISTAT , IMSEG(49,9)
      DOUBLE PRECISION AT
C
C VARIABLES BIDON POUR LIT
C
      DOUBLE PRECISION XB(2)
      REAL, ALLOCATABLE :: RB(:)
      DOUBLE PRECISION, ALLOCATABLE :: VAR(:)
      DOUBLE PRECISION, ALLOCATABLE :: SHZ(:)
      INTEGER IB(2), ERR
      CHARACTER(LEN=1) CB
C
C***********************************************************************
C allocate a (simple) REAL vector

      ALLOCATE(RB(NPOIN3),STAT=ERR)
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LECSUI : ALLOCATION DE RB DEFECTUEUSE'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'LECSUI : WRONG ALLOCATION OF RB'
        ENDIF
        STOP
      ENDIF
C
      ALLOCATE(VAR(NPOIN2),STAT=ERR)
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LECSUI : ALLOCATION DE VAR DEFECTUEUSE'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'LECSUI : WRONG ALLOCATION OF VAR'
        ENDIF
        STOP
      ENDIF
C
      ALLOCATE(SHZ(NPOIN2),STAT=ERR)
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LECSUI : ALLOCATION DE SHZ DEFECTUEUSE'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'LECSUI : WRONG ALLOCATION OF SHZ'
        ENDIF
        STOP
      ENDIF
C
C***********************************************************************
C
C     LECTURE DES DONNEES RELATIVES AU MAILLAGE
C     DANS LE FICHIER DE RESULTATS 3D
C
      NPRE = POS_FILES(POSPRE)%LU
      NRES = POS_FILES(POSHOR)%LU
      NCOU = POS_FILES(POSVER)%LU
C
      REWIND NPRE
C
C     TITRE
C
      READ(NPRE)
      CALL LIT(XB,RB,IB,CB,2, 'I',NPRE,BINPRE,ISTAT)
      NVA3 = IB(1)+IB(2)
C
C     LEC/ECR 3 : NOMS ET UNITES DES VARIABLES
C
      IF(NVA3.GE.1) THEN
        DO I=1,NVA3
          CALL LIT(XB,RB,IB,TEXTLU(I),32,'CH',NPRE,BINPRE,ISTAT)
        ENDDO
      ENDIF
C
      READ(NPRE)
      READ(NPRE)
      READ(NPRE)
      READ(NPRE)
C
      CALL LIT(X,RB,IB,CB,NPOIN3,'R4',NPRE,BINPRE,ISTAT)
      CALL LIT(Y,RB,IB,CB,NPOIN3,'R4',NPRE,BINPRE,ISTAT)
C
c *****************
c fin de l'en-tete
c *****************
C
C INVERSION DE IKLE3 EN IKLES
C
      DO K = 1,NELEM2
         IKLES(1,K) = IKLE2%I(K)
         IKLES(2,K) = IKLE2%I(K+NELEM2)
         IKLES(3,K) = IKLE2%I(K+2*NELEM2)
      ENDDO
C
C PREPARATION DES DONNEES POUR LES COUPES HORIZONTALES
C
      IF(NC2DH.GE.1) THEN
         DO 70 K = 1,NPOIN2
            IPOBO(K) = 0
70       CONTINUE
         CALL PRE2DH (X,Y,IKLES,IPOBO,NPOIN2,NELEM2,NC2DH,NRES,
     *      TITCAS,NVAR,NTRAC,NTRPA,BINCOU,nva3,textlu)
      ENDIF
C
C PREPARATION DES DONNEES POUR LES COUPES VERTICALES
C
      IF(NC2DV.GE.1) CALL PRE2DV(X,Y,SHP,NSEG,IMSEG,X2DV,Y2DV,
     *   IKLES,INDIC,ELEM,NPOIN2,NELEM2,IM,JM,NC2DV)
C
C-----------------------------------------------------------------------
C
C LECTURE ECRITURE DES RESULTATS RELATIFS AU IEME ENREGISTREMENT
C
      NCOU2 = NCOU 
      N=0
C
      DO 90 K = 1,NENRE
         IF (K.GE.NUPRSO.AND.MOD(K-NUPRSO,PESOGR).EQ.0) THEN
C
C LA ON SAIT QUE CET ENREGISTREMENT EST A TRANSCRIRE
C
           CALL LECR3D(AT,Z,U%R,V%R,W%R,NPOIN3,NPOIN2,NPLAN,
     *                 NPRE,BINPRE,RB,NVA3,TAB,VARSUB)
C
           IF(NC2DH.GE.1) CALL COUPEH (AT,Z,U%R,V%R,W%R,
     *        HREF,NPLREF,PLINF,NC2DH,NPOIN2,NPLAN,NRES,BINCOU,
     *        VAR,SHZ,NVA3,TAB)
C
           IF (NC2DV.GE.1) THEN 
           N=N+1
           CALL COUPEV(AT,Z,U%R,V%R,W%R,SHP,
     *        IMSEG,X2DV,Y2DV,DISTOR,IKLES,INDIC,ELEM,NC2DV,NPOIN2,
     *        NELEM2,NCOU2,BINCOU,IM,JM,NVAR,TITCAS,nva3,tab,textlu,
     *        N)
           ENDIF
C
           NCOU2 = NCOU2 + NC2DV
         ELSE
C
C LA ON SAIT QUE CET ENREGISTREMENT N'EST PAS A TRANSCRIRE
C
           DO 100 J = 1,NVA3+1
              READ(NPRE)
100        CONTINUE
         ENDIF
90    CONTINUE
C
      do i=1,nc2dh
      close(nres+i-1)
      enddo
c
      DEALLOCATE (RB)
      DEALLOCATE (VAR)
      DEALLOCATE (SHZ)
C
C-----------------------------------------------------------------------
C
101   FORMAT('LE NUMERO DU PREMIER ENREGISTREMENT POUR LES COUPES',/,
     *       'EST SUPERIEUR AU NOMBRE D''ENREGISTREMENTS')
102   FORMAT('THE NUMBER OF THE FIRST RECORD FOR CROSS SECTIONS',/,
     *       'IS HIGHER THAN THE NUMBER OF RECORDS')
C
C-----------------------------------------------------------------------
C
      RETURN
      END
