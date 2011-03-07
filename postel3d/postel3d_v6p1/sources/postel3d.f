!                       *******************
                        SUBROUTINE POSTEL3D
!                       *******************
!
!***********************************************************************
! POSTEL3D VERSION 5.1   01/09/99   T. DENOT (LNH) 01 30 87 74 89
! FORTRAN90
!***********************************************************************
!
!       PPPP    OOO    SSSS  TTTTT  EEEEE  L         3333   DDDD
!       P   P  O   O  S        T    E      L             3  D   D
!       PPPP   O   O   SSS     T    EEEE   L     ---  333   D   D
!       P      O   O      S    T    E      L             3  D   D
!       P       OOO   SSSS     T    EEEEE  LLLLL     3333   DDDD
!
!-----------------------------------------------------------------------
!
! SOUS-PROGRAMME APPELE PAR : HOMERE_POSTEL3D
! SOUS-PROGRAMME APPELES : LIT, PRE2DH, PRE2DV, LECR3D, COUPEH, COUPEV
!
!-----------------------------------------------------------------------
!                    DECLARATION DES TYPES ET DIMENSIONS
!-----------------------------------------------------------------------
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_POSTEL3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
! TABLEAUX DE REELS
!
      DOUBLE PRECISION SHP(IM,3,NC2DV)
!
! TABLEAUX D'ENTIERS
!
      INTEGER IKLES(3,NELEM2) , PLINF(NPOIN2)
      INTEGER IPOBO(NPOIN2)
      INTEGER INDIC(IM,JM,NC2DV) , ELEM(IM,NC2DV)
      INTEGER N,NPRE,NRES,NCOU
!
! VARIABLES LOCALES
!
      INTEGER I,J , K ,  ISTAT , IMSEG(49,9)
      DOUBLE PRECISION AT
!
! VARIABLES BIDON POUR LIT
!
      DOUBLE PRECISION XB(2)
      REAL, ALLOCATABLE :: RB(:)
      DOUBLE PRECISION, ALLOCATABLE :: VAR(:)
      DOUBLE PRECISION, ALLOCATABLE :: SHZ(:)
      INTEGER IB(2), ERR
      CHARACTER(LEN=1) CB
!
!***********************************************************************
! allocate a (simple) REAL vector
!
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
!
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
!
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
!
!***********************************************************************
!
!     LECTURE DES DONNEES RELATIVES AU MAILLAGE
!     DANS LE FICHIER DE RESULTATS 3D
!
      NPRE = POS_FILES(POSPRE)%LU
      NRES = POS_FILES(POSHOR)%LU
      NCOU = POS_FILES(POSVER)%LU
!
      REWIND NPRE
!
!     TITRE
!
      READ(NPRE)
      CALL LIT(XB,RB,IB,CB,2, 'I',NPRE,BINPRE,ISTAT)
      NVA3 = IB(1)+IB(2)
!
!     LEC/ECR 3 : NOMS ET UNITES DES VARIABLES
!
      IF(NVA3.GE.1) THEN
        DO I=1,NVA3
          CALL LIT(XB,RB,IB,TEXTLU(I),32,'CH',NPRE,BINPRE,ISTAT)
        ENDDO
      ENDIF
!
      READ(NPRE)
      READ(NPRE)
      READ(NPRE)
      READ(NPRE)
!
      CALL LIT(X,RB,IB,CB,NPOIN3,'R4',NPRE,BINPRE,ISTAT)
      CALL LIT(Y,RB,IB,CB,NPOIN3,'R4',NPRE,BINPRE,ISTAT)
!
! *****************
! fin de l'en-tete
! *****************
!
! INVERSION DE IKLE3 EN IKLES
!
      DO K = 1,NELEM2
         IKLES(1,K) = IKLE2%I(K)
         IKLES(2,K) = IKLE2%I(K+NELEM2)
         IKLES(3,K) = IKLE2%I(K+2*NELEM2)
      ENDDO
!
! PREPARATION DES DONNEES POUR LES COUPES HORIZONTALES
!
      IF(NC2DH.GE.1) THEN
         DO 70 K = 1,NPOIN2
            IPOBO(K) = 0
70       CONTINUE
         CALL PRE2DH (X,Y,IKLES,IPOBO,NPOIN2,NELEM2,NC2DH,NRES,
     &      TITCAS,NVAR,NTRAC,NTRPA,BINCOU,nva3,textlu)
      ENDIF
!
! PREPARATION DES DONNEES POUR LES COUPES VERTICALES
!
      IF(NC2DV.GE.1) CALL PRE2DV(X,Y,SHP,NSEG,IMSEG,X2DV,Y2DV,
     &   IKLES,INDIC,ELEM,NPOIN2,NELEM2,IM,JM,NC2DV)
!
!-----------------------------------------------------------------------
!
! LECTURE ECRITURE DES RESULTATS RELATIFS AU IEME ENREGISTREMENT
!
      NCOU2 = NCOU
      N=0
!
      DO 90 K = 1,NENRE
         IF (K.GE.NUPRSO.AND.MOD(K-NUPRSO,PESOGR).EQ.0) THEN
!
! LA ON SAIT QUE CET ENREGISTREMENT EST A TRANSCRIRE
!
           CALL LECR3D(AT,Z,U%R,V%R,W%R,NPOIN3,NPOIN2,NPLAN,
     &                 NPRE,BINPRE,RB,NVA3,TAB,VARSUB)
!
           IF(NC2DH.GE.1) CALL COUPEH (AT,Z,U%R,V%R,W%R,
     &        HREF,NPLREF,PLINF,NC2DH,NPOIN2,NPLAN,NRES,BINCOU,
     &        VAR,SHZ,NVA3,TAB)
!
           IF (NC2DV.GE.1) THEN
           N=N+1
           CALL COUPEV(AT,Z,U%R,V%R,W%R,SHP,
     &        IMSEG,X2DV,Y2DV,DISTOR,IKLES,INDIC,ELEM,NC2DV,NPOIN2,
     &        NELEM2,NCOU2,BINCOU,IM,JM,NVAR,TITCAS,nva3,tab,textlu,
     &        N)
           ENDIF
!
           NCOU2 = NCOU2 + NC2DV
         ELSE
!
! LA ON SAIT QUE CET ENREGISTREMENT N'EST PAS A TRANSCRIRE
!
           DO 100 J = 1,NVA3+1
              READ(NPRE)
100        CONTINUE
         ENDIF
90    CONTINUE
!
      do i=1,nc2dh
      close(nres+i-1)
      enddo
!
      DEALLOCATE (RB)
      DEALLOCATE (VAR)
      DEALLOCATE (SHZ)
!
!-----------------------------------------------------------------------
!
101   FORMAT('LE NUMERO DU PREMIER ENREGISTREMENT POUR LES COUPES',/,
     &       'EST SUPERIEUR AU NOMBRE D''ENREGISTREMENTS')
102   FORMAT('THE NUMBER OF THE FIRST RECORD FOR CROSS SECTIONS',/,
     &       'IS HIGHER THAN THE NUMBER OF RECORDS')
!
!-----------------------------------------------------------------------
!
      RETURN
      END