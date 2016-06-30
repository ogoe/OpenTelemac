!                       ********************
                        SUBROUTINE LECBREACH
!                       ********************
!
     &(IFIC)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   03/08/2012
!***********************************************************************
!
!brief    READ THE BREACHES DATA FILE, ALLOCATE THE DEDICATED ARRAY
!+        AND IDENTIFY THE NODES
!
!
!history  P. CHASSE (CETMEF) / C.COULET (ARTELIA)
!+        03/08/2012
!+        V6P2
!+        Creation
!
!history  Y.B. TADESSE (TUHH, INSTITUTE OF RIVER AND COASTAL ENGINEERING)
!+        14/02/2014
!+        V6P3R2
!+   Addition of later breach growth option
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IFIC           |-->| LOGICAL UNIT OF BREACHES DATA FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN)    :: IFIC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N, M, NUM, NBL, ISTAT
      INTEGER ITMP(NPOIN)
!      DOUBLE PRECISION LEMPRISE
      DOUBLE PRECISION X1, X2, Y1, Y2, DX, DY
      DOUBLE PRECISION U1, U2, V1, V2, DELS, MIDDIS
      DOUBLE PRECISION, DIMENSION (:), ALLOCATABLE :: XL, YL, XP, YP
      DOUBLE PRECISION, DIMENSION (:), ALLOCATABLE :: DS
!
      CHARACTER(LEN=6) :: NOM, NOMX, NOMY, NOMDS
      CHARACTER*1, PARAMETER :: CHIFFRE(0:9) =
     &           (/'0','1','2','3','4','5','6','7','8','9'/)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      READ(IFIC,*,END=900) ! COMMENT LINE
      READ(IFIC,*,ERR=999) NBRECH
!
!     ALLOCATION OF SPECIFIC ARRAYS
!
      IF(NBRECH.GT.0) THEN
        CALL BIEF_ALLVEC(2,OPTNBR,'OPTNBR',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(2,OPTERO,'OPTERO',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(1,TDECBR,'TDECBR',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(1,DURBR ,'DURBR ',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(1,ZFINBR,'ZFINBR',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(1,ZDECBR,'ZDECBR',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(1,ZCRBR ,'ZCRBR ',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(1,POLWDT,'POLWDT',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(2,NUMPSD,'NUMPSD',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(2,NBNDBR,'NBNDBR',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(2,NPONBR,'NPONBR',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(1,CURBRW,'CURBRW',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(1,FINBRW,'FINBRW',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(1,INIBRW,'INIBRW',NBRECH,1,0,MESH)
      ELSE
        CALL BIEF_ALLVEC(2,OPTNBR,'OPTNBR',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,OPTERO,'OPTERO',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,TDECBR,'TDECBR',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,DURBR ,'DURBR ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,ZFINBR,'ZFINBR',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,ZDECBR,'ZDECBR',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,ZCRBR ,'ZCRBR ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,POLWDT,'POLWDT',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,NUMPSD,'NUMPSD',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,NBNDBR,'NBNDBR',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,NPONBR,'NPONBR',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,CURBRW,'CURBRW',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,FINBRW,'FINBRW',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,INIBRW,'INIBRW',0,1,0,MESH)
      ENDIF
      CALL ALLBLO(INDBR ,'INDBR ')
      CALL ALLBLO(DKAXCR,'DKAXCR')
      CALL ALLBLO(DKAYCR,'DKAYCR')
      CALL ALLBLO(PONDSB,'PONDSB')
!
!
      DO N = 1, NBRECH
        READ(IFIC,*,END=900) ! COMMENT LINE
        READ(IFIC,*,END=900) ! COMMENT LINE
        READ(IFIC,*,ERR=998) POLWDT%R(N)
        READ(IFIC,*,END=900) ! COMMENT LINE
        READ(IFIC,*,ERR=997) OPTNBR%I(N)
        READ(IFIC,*,END=900) ! COMMENT LINE
        IF(OPTNBR%I(N).EQ.1) THEN
          READ(IFIC,*,ERR=996) TDECBR%R(N)
          READ(IFIC,*,END=900) ! COMMENT LINE
        ELSE
          TDECBR%R(N) = -9999.D0
        ENDIF
        READ(IFIC,*,ERR=995) DURBR%R(N)
        READ(IFIC,*,END=900) ! COMMENT LINE
        READ(IFIC,*,ERR=810) OPTERO%I(N)
        READ(IFIC,*,END=900) ! COMMENT LINE
        READ(IFIC,*,ERR=994) ZFINBR%R(N)
        READ(IFIC,*,END=900) ! COMMENT LINE
        IF(OPTNBR%I(N).EQ.3) THEN
          READ(IFIC,*,ERR=993) NUMPSD%I(N)
          READ(IFIC,*,END=900) ! COMMENT LINE
          IF(NCSIZE.GT.1) THEN
            NUM = NUMPSD%I(N)
            NUMPSD%I(N) = 0
            DO M=1,MESH%NPOIN
              IF(NUM.EQ.MESH%KNOLG%I(M)) THEN
                NUMPSD%I(N) = M
              ENDIF
            ENDDO
          ENDIF
        ENDIF
        IF(OPTNBR%I(N).NE.1) THEN
          READ(IFIC,*,ERR=992) ZDECBR%R(N)
          READ(IFIC,*,END=900) ! COMMENT LINE
        ENDIF
!
        READ(IFIC,*,ERR=991) NBL
        READ(IFIC,*,END=900) ! COMMENT LINE
!
!       ALLOCATION OF LOCAL VARIABLE TO READ BREACH DEFINITION
        ISTAT = 0
        ALLOCATE(XL(NBL), STAT=ISTAT)
        IF(ISTAT.NE.0) THEN
          IF(LNG.EQ.1) WRITE(LU,10) NOM,ISTAT
          IF(LNG.EQ.2) WRITE(LU,20) NOM,ISTAT
          CALL PLANTE(1)
          STOP
        ENDIF
        ALLOCATE(YL(NBL), STAT=ISTAT)
        IF(ISTAT.NE.0) THEN
          IF(LNG.EQ.1) WRITE(LU,10) NOM,ISTAT
          IF(LNG.EQ.2) WRITE(LU,20) NOM,ISTAT
          CALL PLANTE(1)
          STOP
        ENDIF
        ALLOCATE(DS(NBL-1), STAT=ISTAT)
        IF(ISTAT.NE.0) THEN
          IF(LNG.EQ.1) WRITE(LU,10) NOM,ISTAT
          IF(LNG.EQ.2) WRITE(LU,20) NOM,ISTAT
          CALL PLANTE(1)
          STOP
        ENDIF
!
10      FORMAT(1X,'ERREUR A L''ALLOCATION DU VECTEUR : ',A6,/,1X,
     &            'CODE D''ERREUR : ',1I6)
20      FORMAT(1X,'ERROR DURING ALLOCATION OF VECTOR: ',A6,/,1X,
     &            'ERROR CODE: ',1I6)
!
        DO M = 1, NBL
           READ(IFIC,*,ERR=990) XL(M), YL(M)
        ENDDO
!       SEARCH MESH POINTS INSIDE THE BREACH DOMAIN
        ISTAT = 0
        ALLOCATE(XP(2*NBL), STAT=ISTAT)
        IF(ISTAT.NE.0) THEN
          IF(LNG.EQ.1) WRITE(LU,10) NOM,ISTAT
          IF(LNG.EQ.2) WRITE(LU,20) NOM,ISTAT
          CALL PLANTE(1)
          STOP
        ENDIF
        ALLOCATE(YP(2*NBL), STAT=ISTAT)
        IF(ISTAT.NE.0) THEN
          IF(LNG.EQ.1) WRITE(LU,10) NOM,ISTAT
          IF(LNG.EQ.2) WRITE(LU,20) NOM,ISTAT
          CALL PLANTE(1)
          STOP
        ENDIF
!
        X1 = XL(1)
        Y1 = YL(1)
        X2 = XL(2)
        Y2 = YL(2)
        DX = X2 - X1
        DY = Y2 - Y1
        DELS=SQRT(DX*DX+DY*DY)
        IF(DELS.GT.0.D0) THEN
          U1 = DX/DELS
          U2 = DY/DELS
        ELSE
          IF(LNG.EQ.1) WRITE(LU,*)
     &      'PROBLEME DANS LA DEFINITION DE LA BRECHE :',N
          IF(LNG.EQ.2) WRITE(LU,*)
     &      'PROBLEM IN DEFINITION OF BREACH :',N
          CALL PLANTE(1)
        ENDIF
        V1 = -U2
        V2 = U1
        XP(1)     = X1 + V1*POLWDT%R(N)/2.D0
        YP(1)     = Y1 + V2*POLWDT%R(N)/2.D0
        XP(2*NBL) = X1 - V1*POLWDT%R(N)/2.D0
        YP(2*NBL) = Y1 - V2*POLWDT%R(N)/2.D0
!
        DO M = 2,NBL
           X2 = XL(M)
           Y2 = YL(M)
           DX = X2 - X1
           DY = Y2 - Y1
           DS(M-1)=SQRT(DX*DX+DY*DY)
           IF(DS(M-1).GT.0.D0) THEN
             U1 = DX/DS(M-1)
             U2 = DY/DS(M-1)
           ELSE
             IF(LNG.EQ.1) WRITE(LU,*)
     &         'PROBLEME DANS LA DEFINITION DE LA BRECHE :',
     &                     N
             IF(LNG.EQ.2) WRITE(LU,*)
     &         'PROBLEM IN DEFINITION OF BREACH :',N
             CALL PLANTE(1)
           ENDIF
           V1 = -U2
           V2 = U1
           XP(M)         = X2 + V1*POLWDT%R(N)/2.D0
           YP(M)         = Y2 + V2*POLWDT%R(N)/2.D0
           XP(2*NBL-M+1) = X2 - V1*POLWDT%R(N)/2.D0
           YP(2*NBL-M+1) = Y2 - V2*POLWDT%R(N)/2.D0
           X1=X2
           Y1=Y2
        ENDDO
!
        NBNDBR%I(N) = 0
        DO M = 1, NPOIN
           IF(INPOLY(MESH%X%R(M), MESH%Y%R(M), XP, YP, 2*NBL)) THEN
             NBNDBR%I(N) = NBNDBR%I(N)+1
             ITMP(NBNDBR%I(N)) = M
           ENDIF
        ENDDO
!
        IF(N.LE.INDBR%MAXBLOCK) THEN
          NOM='NBR   '
          IF(N.LT.10) THEN
            NOM(4:4) = CHIFFRE(N)
          ELSEIF(N.LT.100) THEN
            NOM(4:4) = CHIFFRE(N/10)
            NOM(5:5) = CHIFFRE(N-10*(N/10))
          ELSEIF(N.LT.1000) THEN
            NOM(4:4) = CHIFFRE(N/100)
            NOM(5:5) = CHIFFRE((N-100*(N/100))/10)
            NOM(6:6) = CHIFFRE((N-100*(N/100))-10*((N-100*(N/100))/10))
          ELSE
            IF(LNG.EQ.1)
     &        WRITE(LU,*)
     &          'PLUS DE 999 BRECHES DEMANDEES DANS LECBREACH'
            IF(LNG.EQ.2)
     &        WRITE(LU,*)
     &          'MORE THAN 999 BREACHS ASKED IN LECBREACH'
            CALL PLANTE(1)
            STOP
          ENDIF
          ALLOCATE(INDBR%ADR(N)%P)
          CALL BIEF_ALLVEC(2,INDBR%ADR(N)%P,NOM,NBNDBR%I(N),1,0,MESH)
          INDBR%ADR(N)%P%FATHER = INDBR%NAME
        ELSE
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'LECBREACH :'
            WRITE(LU,*) 'PLUS DE ',INDBR%MAXBLOCK,' (',N,')'
            WRITE(LU,*) 'VECTEURS DEMANDES,'
            WRITE(LU,*) 'CHANGER MAXBLOCK DANS ALLBLO.'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'LECBREACH:'
            WRITE(LU,*) 'MORE THAN ',INDBR%MAXBLOCK,'(',N,')'
            WRITE(LU,*) 'VECTORS TO BE ALLOCATED'
            WRITE(LU,*) 'CHANGE MAXBLOCK IN ALLBLO.'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
!
        DO M=1, NBNDBR%I(N)
           INDBR%ADR(N)%P%I(M) = ITMP(M)
        ENDDO

        IF (OPTERO%I(N).EQ.2) THEN
          IF(N.LE.DKAXCR%MAXBLOCK) THEN
            NOMX='XCB   '
            NOMY='YCB   '
            NOMDS='PDS   '
            IF(N.LT.10) THEN
              NOMX(4:4) = CHIFFRE(N)
              NOMY(4:4) = CHIFFRE(N)
              NOMDS(4:4) = CHIFFRE(N)
            ELSEIF(N.LT.100) THEN
              NOMX(4:4) = CHIFFRE(N/10)
              NOMX(5:5) = CHIFFRE(N-10*(N/10))
              NOMY(4:4) = CHIFFRE(N/10)
              NOMY(5:5) = CHIFFRE(N-10*(N/10))
              NOMDS(4:4) = CHIFFRE(N/10)
              NOMDS(5:5) = CHIFFRE(N-10*(N/10))
            ELSEIF(N.LT.1000) THEN
              NOMX(4:4) = CHIFFRE(N/100)
              NOMX(5:5) = CHIFFRE((N-100*(N/100))/10)
              NOMX(6:6) =
     &          CHIFFRE((N-100*(N/100))-10*((N-100*(N/100))/10))
              NOMY(4:4) = CHIFFRE(N/100)
              NOMY(5:5) = CHIFFRE((N-100*(N/100))/10)
              NOMY(6:6) =
     &          CHIFFRE((N-100*(N/100))-10*((N-100*(N/100))/10))
              NOMDS(4:4) = CHIFFRE(N/100)
              NOMDS(5:5) = CHIFFRE((N-100*(N/100))/10)
              NOMDS(6:6) =
     &          CHIFFRE((N-100*(N/100))-10*((N-100*(N/100))/10))
            ELSE
              IF(LNG.EQ.1)
     &          WRITE(LU,*)
     &            'PLUS DE 999 BRECHES DEMANDEES DANS LECBREACH'
              IF(LNG.EQ.2)
     &          WRITE(LU,*)
     &            'MORE THAN 999 BREACHS ASKED IN LECBREACH'
              CALL PLANTE(1)
              STOP
            ENDIF
            ALLOCATE(DKAXCR%ADR(N)%P,DKAYCR%ADR(N)%P,PONDSB%ADR(N)%P)
            CALL BIEF_ALLVEC(1,DKAXCR%ADR(N)%P,NOMX,NBL,1,0,MESH)
            CALL BIEF_ALLVEC(1,DKAYCR%ADR(N)%P,NOMY,NBL,1,0,MESH)
            CALL BIEF_ALLVEC(1,PONDSB%ADR(N)%P,NOMDS,NBL-1,1,0,MESH)
            DKAXCR%ADR(N)%P%FATHER = DKAXCR%NAME
            DKAYCR%ADR(N)%P%FATHER = DKAYCR%NAME
            PONDSB%ADR(N)%P%FATHER = PONDSB%NAME
          ELSE
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'LECBREACH :'
              WRITE(LU,*) 'PLUS DE ',DKAXCR%MAXBLOCK,' (',N,')'
              WRITE(LU,*) 'VECTEURS DEMANDES,'
              WRITE(LU,*) 'CHANGER MAXBLOCK DANS ALLBLO.'
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'LECBREACH:'
              WRITE(LU,*) 'MORE THAN ',DKAXCR%MAXBLOCK,'(',N,')'
              WRITE(LU,*) 'VECTORS TO BE ALLOCATED'
              WRITE(LU,*) 'CHANGE MAXBLOCK IN ALLBLO.'
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
          FINBRW%R(N)=0.D0
          DO M=1,NBL
            DKAXCR%ADR(N)%P%R(M)=XL(M)
            DKAYCR%ADR(N)%P%R(M)=YL(M)
            IF (M .GT.1) THEN
              PONDSB%ADR(N)%P%R(M-1)=DS(M-1)
              FINBRW%R(N)=FINBRW%R(N)+DS(M-1)
            ENDIF
          ENDDO
          NPONBR%I(N)=NBL
          MIDDIS=0.D0
          CURBRW%R(N)=0.D0
          INIBRW%R(N)=FINBRW%R(N)/10.D0
          FINDMIDNODE: DO M=2,NBL
            MIDDIS=MIDDIS+DS(M-1)
            IF (MIDDIS .GE. (FINBRW%R(N)/2.D0)) THEN
              IF (M .GT. 2) THEN
                INIBRW%R(N)=MAX(DS(M-1)+DS(M),INIBRW%R(N))
                CURBRW%R(N)=INIBRW%R(N)
              ELSE
                IF(LNG.EQ.1) WRITE(LU,*)
     &            'AUGMENTATION DES POINTS DE LA BRECHE :',N
                IF(LNG.EQ.2) WRITE(LU,*)
     &            'INCREASE NUMBER OF POINTS OF BREACH :',N
                GO TO 2000
              ENDIF
              EXIT FINDMIDNODE
            ENDIF
          ENDDO FINDMIDNODE
        END IF
        IF ((OPTERO%I(N).NE.1) .AND. (OPTERO%I(N).NE.2)) THEN
          GO TO 800
        END IF
        DEALLOCATE(XL)
        DEALLOCATE(YL)
        DEALLOCATE(XP)
        DEALLOCATE(YP)
        DEALLOCATE(DS)
!
      ENDDO
!
      INDBR%N = NBRECH
      DKAXCR%N = NBRECH
      DKAYCR%N = NBRECH
      PONDSB%N = NBRECH
      GOTO 1000
!
!-----------------------------------------------------------------------
!     MESSAGES D'ERREURS
!-----------------------------------------------------------------------
!
810   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'BRECHE : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES BRECHES'
        WRITE(LU,*) '         POUR LA BRECHE ',N
        WRITE(LU,*) '         OPTION DE EROSION ILLISIBLE'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'BRECHE : READ ERROR ON THE'
        WRITE(LU,*) '         BREACHES DATA FILE'
        WRITE(LU,*) '         FOR THE BREACH ',N
        WRITE(LU,*) '         EROSION OPTION CANNOT BE READ'
      ENDIF
      GO TO 2000
!
800   CONTINUE
      IF (LNG.EQ.1) THEN
        WRITE (LU,*) 'BRECHE : INDISPONIBLE EROSION OPTION'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'BREACH : UNAVAILABLE EROSION OPTION'
      ENDIF
      GO TO 2000
!
999   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'BRECHE : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES BRECHES'
        WRITE(LU,*) '         2EME LIGNE DU FICHIER NON CONFORME.'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'BRECHE : READ ERROR ON THE'
        WRITE(LU,*) '         BREACHES DATA FILE'
        WRITE(LU,*) '         AT LINE 2'
      ENDIF
      GO TO 2000
!
998   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'BRECHE : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES BRECHES'
        WRITE(LU,*) '         POUR LA BRECHE ',N
        WRITE(LU,*) '       VIOLATION POLYGONE LARGEUR NE PEUT PAS LIRE'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'BRECHE : READ ERROR ON THE'
        WRITE(LU,*) '         BREACHES DATA FILE'
        WRITE(LU,*) '         FOR THE BREACH ',N
        WRITE(LU,*) '         BREACH POLYGON WIDTH CANNOT BE READ'
      ENDIF
      GO TO 2000
!
997   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'BRECHE : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES BRECHES'
        WRITE(LU,*) '         POUR LA BRECHE ',N
        WRITE(LU,*) '         OPTION ILLISIBLE'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'BRECHE : READ ERROR ON THE'
        WRITE(LU,*) '         BREACHES DATA FILE'
        WRITE(LU,*) '         FOR THE BREACH ',N
        WRITE(LU,*) '         OPTION CANNOT BE READ'
      ENDIF
      GO TO 2000
!
996   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'BRECHE : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES BRECHES'
        WRITE(LU,*) '         POUR  LA BRECHE ',N
        WRITE(LU,*) '         LE TEMPS DE DECLENCHEMENT EST ILLISIBLE'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'BRECHE : READ ERROR ON THE'
        WRITE(LU,*) '         BREACHES DATA FILE'
        WRITE(LU,*) '         FOR THE BREACH ',N
        WRITE(LU,*) '         THE STARTING TIME CANNOT BE READ'
      ENDIF
      GO TO 2000
!
995   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'BRECHE : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES BRECHES'
        WRITE(LU,*) '         POUR  LA BRECHE ',N
        WRITE(LU,*) '         LA DUREE DE FORMATION EST ILLISIBLE'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'BRECHE : READ ERROR ON THE'
        WRITE(LU,*) '         BREACHES DATA FILE'
        WRITE(LU,*) '         FOR THE BREACH ',N
        WRITE(LU,*) '         THE OPENNING DURATION CANNOT BE READ'
      ENDIF
      GO TO 2000
!
994   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'BRECHE : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES BRECHES'
        WRITE(LU,*) '         POUR LA BRECHE ',N
        WRITE(LU,*) '         LA COTE FINALE EST ILLISIBLE'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'BRECHE : READ ERROR ON THE'
        WRITE(LU,*) '         BREACHES DATA FILE'
        WRITE(LU,*) '         FOR THE BREACH ',N
        WRITE(LU,*) '         THE FINAL LEVEL CANNOT BE READ'
      ENDIF
      GO TO 2000
!
993   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'BRECHE : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES BRECHES'
        WRITE(LU,*) '         POUR LA BRECHE ',N
        WRITE(LU,*) '         LE NUMERO DU POINT DE SONDE EST ILLISIBLE'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'BRECHE : READ ERROR ON THE'
        WRITE(LU,*) '         BREACHES DATA FILE'
        WRITE(LU,*) '         FOR THE BREACH ',N
        WRITE(LU,*) '         THE NUMBER OF TEST POINT CANNOT BE READ'
      ENDIF
      GO TO 2000
!
992   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'BRECHE : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES BRECHES'
        WRITE(LU,*) '         POUR LA BRECHE ',N
        WRITE(LU,*) '         LE NIVEAU DE DECLENCHEMENT EST ILLISIBLE'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'BRECHE : READ ERROR ON THE'
        WRITE(LU,*) '         BREACHES DATA FILE'
        WRITE(LU,*) '         FOR THE BREACH ',N
        WRITE(LU,*) '         THE STARTING LEVEL CANNOT BE READ'
      ENDIF
      GO TO 2000
!
991   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'BRECHE : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES BRECHES'
        WRITE(LU,*) '         POUR LA BRECHE ',N
        WRITE(LU,*) '         LE NOMBRE DE POINTS DE LA LIGNE EST'
        WRITE(LU,*) '         ILLISIBLE'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'BRECHE : READ ERROR ON THE'
        WRITE(LU,*) '         BREACHES DATA FILE'
        WRITE(LU,*) '         FOR THE BREACH ',N
        WRITE(LU,*) '         THE POINT NUMBER OF LINE CANNOT BE READ'
      ENDIF
      GO TO 2000
!
990   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'BRECHE : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES BRECHES'
        WRITE(LU,*) '         POUR LA BRECHE ',N
        WRITE(LU,*) '         LES COORDONNEES DU POINT ',M
        WRITE(LU,*) '         SONT ILLISIBLE'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'BRECHE : READ ERROR ON THE'
        WRITE(LU,*) '         BREACHES DATA FILE'
        WRITE(LU,*) '         FOR THE BREACH ',N
        WRITE(LU,*) '         THE COORDINATE OF POINT ',M
        WRITE(LU,*) '         CANNOT BE READ'
      ENDIF
      GO TO 2000
!
900   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'BRECHE : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES BRECHES'
        WRITE(LU,*) '         FIN DE FICHIER PREMATUREE'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'BRECHE : READ ERROR ON THE'
        WRITE(LU,*) '         BREACHES DATA FILE'
        WRITE(LU,*) '         UNEXPECTED END OF FILE'
      ENDIF
!
2000  CONTINUE
!
      CALL PLANTE(1)
      STOP
!
1000  CONTINUE
      RETURN
      END

