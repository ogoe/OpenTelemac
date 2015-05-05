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
      USE INTERFACE_HERMES
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
      INTEGER N,NPRE,NHOR,NVER
!
! VARIABLES LOCALES
!
      INTEGER I, K ,  ISTAT , IMSEG(49,9)
      DOUBLE PRECISION AT
!
! VARIABLES BIDON POUR LIT
!
      REAL, ALLOCATABLE :: RB(:)
      DOUBLE PRECISION, ALLOCATABLE :: VAR(:)
      DOUBLE PRECISION, ALLOCATABLE :: SHZ(:)
      INTEGER ERR, IERR
      CHARACTER(LEN=8) PRE_FMT, VER_FMT, HOR_FMT
      CHARACTER(LEN=16),ALLOCATABLE :: VAR_NAME(:), VAR_UNIT(:)
!
!***********************************************************************
! allocate a (simple) REAL vector
!
      ALLOCATE(RB(NPOIN3),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR,'POSTEL3D:RB')
!
      ALLOCATE(VAR(NPOIN2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR,'POSTEL3D:VAR')
!
      ALLOCATE(SHZ(NPOIN2),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR,'POSTEL3D:SHZ')
!
!***********************************************************************
!
!     LECTURE DES DONNEES RELATIVES AU MAILLAGE
!     DANS LE FICHIER DE RESULTATS 3D
!
      NPRE = POS_FILES(POSPRE)%LU
      PRE_FMT = POS_FILES(POSPRE)%FMT
      NHOR = POS_FILES(POSHOR)%LU
      HOR_FMT = POS_FILES(POSHOR)%FMT
      NVER = POS_FILES(POSVER)%LU
      VER_FMT = POS_FILES(POSVER)%FMT
!
!
      CALL GET_DATA_NVAR(PRE_FMT,NPRE,NVA3,IERR)
      CALL CHECK_CALL(IERR, 'POSTEL3D:GET_DATA_NVAR')
!
!     LEC/ECR 3 : NOMS ET UNITES DES VARIABLES
!
      IF(NVA3.GE.1) THEN
        ALLOCATE(VAR_NAME(NVA3),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'POSTEL3D:VAR_NAME')
        ALLOCATE(VAR_UNIT(NVA3),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'POSTEL3D:VAR_UNIT')
        CALL GET_DATA_VAR_LIST(PRE_FMT,NPRE,NVA3,VAR_NAME,VAR_UNIT,IERR)
        CALL CHECK_CALL(IERR, 'POSTEL3D:GET_DATA_VAR_LIST')
        DO I=1,NVA3
          TEXTLU(I)(1:16) = VAR_NAME(I)
          TEXTLU(I)(17:32) = VAR_UNIT(I)
        ENDDO
        DEALLOCATE(VAR_NAME,VAR_UNIT)
      ENDIF
!
!
      CALL GET_MESH_COORD(PRE_FMT,NPRE,1,2,NPOIN3,X,IERR)
      CALL CHECK_CALL(IERR, 'POSTEL3D:GET_MESH_COORD:X')
      CALL GET_MESH_COORD(PRE_FMT,NPRE,2,2,NPOIN3,Y,IERR)
      CALL CHECK_CALL(IERR, 'POSTEL3D:GET_MESH_COORD:X')
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
      ! Cancelling the opening done in bief_open_file as multiple files will be reopen
      CALL CLOSE_MESH(HOR_FMT,NHOR,IERR)
      CALL CHECK_CALL(IERR,'POSTEL3D:CLOSE_MESH')
      CALL CLOSE_MESH(VER_FMT,NVER,IERR)
      CALL CHECK_CALL(IERR,'POSTEL3D:CLOSE_MESH')
!
! PREPARATION DES DONNEES POUR LES COUPES HORIZONTALES
!
      IF(NC2DH.GE.1) THEN
        DO K = 1,NPOIN2
           IPOBO(K) = 0
        ENDDO
        CALL PRE2DH (X,Y,IKLES,IPOBO,NPOIN2,NELEM2,NC2DH,NHOR,
     &     TITCAS,NVAR,NTRAC,NTRPA,HOR_FMT,NVA3,TEXTLU)
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
      N=0
!
      DO K = 0,NENRE-1
        IF (K.GE.NUPRSO.AND.MOD(K-NUPRSO,PESOGR).EQ.0) THEN
!
! LA ON SAIT QUE CET ENREGISTREMENT EST A TRANSCRIRE
!
          CALL LECR3D(K,AT,Z,U%R,V%R,W%R,NPOIN3,NPOIN2,NPLAN,
     &                NPRE,PRE_FMT,RB,NVA3,TAB,VARSUB)
!
          IF(NC2DH.GE.1) CALL COUPEH (K,AT,Z,U%R,V%R,W%R,
     &       HREF,NPLREF,PLINF,NC2DH,NPOIN2,NPLAN,NHOR,VER_FMT,
     &       VAR,SHZ,NVA3,TAB,TEXTLU)
!
          IF (NC2DV.GE.1) THEN
          N=N+1
          CALL COUPEV(AT,Z,U%R,V%R,W%R,SHP,
     &       IMSEG,X2DV,Y2DV,DISTOR,IKLES,INDIC,ELEM,NC2DV,NPOIN2,
     &       NELEM2,NVER,VER_FMT,IM,JM,NVAR,TITCAS,NVA3,TAB,TEXTLU,
     &       N)
          ENDIF
        ENDIF
      ENDDO
!
      IF (NC2DH.GE.2) THEN
        DO I=2,NC2DH
          CALL CLOSE_MESH(HOR_FMT,NHOR+I-1,IERR)
          CALL CHECK_CALL(IERR,'POSTEL3D:CLOSE_MESH')
        ENDDO
      ENDIF
      CALL OPEN_MESH(VER_FMT,'POSVER',NVER,'WRITE    ',IERR)
!
      DEALLOCATE (RB)
      DEALLOCATE (VAR)
      DEALLOCATE (SHZ)
!
!-----------------------------------------------------------------------
!
!101   FORMAT('LE NUMERO DU PREMIER ENREGISTREMENT POUR LES COUPES',/,
!     &       'EST SUPERIEUR AU NOMBRE D''ENREGISTREMENTS')
!102   FORMAT('THE NUMBER OF THE FIRST RECORD FOR CROSS SECTIONS',/,
!     &       'IS HIGHER THAN THE NUMBER OF RECORDS')
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
