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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IFIC           |-->| LOGICAL UNIT OF BREACHES DATA FILE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN)    :: IFIC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N, M, Num, Nbl, ISTAT
      INTEGER Itmp(NPOIN)
      DOUBLE PRECISION LEMPRISE
      DOUBLE PRECISION X1, X2, Y1, Y2, DX, DY
      DOUBLE PRECISION U1, U2, V1, V2, DS
      DOUBLE PRECISION, DIMENSION (:), ALLOCATABLE :: Xl, Yl, Xp, Yp
!
      CHARACTER(LEN=6) :: NOM
      CHARACTER*1 CHIFFRE(0:9)
      DATA CHIFFRE/'0','1','2','3','4','5','6','7','8','9'/
      SAVE CHIFFRE
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      READ(IFIC,*,END=900) ! COMMENT LINE
      READ(IFIC,*,ERR=999) NBRECH
!
!     Allocation of specific arrays
!
      IF(NBRECH.GT.0) THEN
        CALL BIEF_ALLVEC(2,OPTNBR,'OPTNBR',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(1,TDECBR,'TDECBR',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(1,DURBR ,'DURBR ',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(1,ZFINBR,'ZFINBR',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(1,ZDECBR,'ZDECBR',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(1,ZCRBR ,'ZCRBR ',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(2,NUMPSD,'NUMPSD',NBRECH,1,0,MESH)
        CALL BIEF_ALLVEC(2,NBNDBR,'NBNDBR',NBRECH,1,0,MESH)
      ELSE
        CALL BIEF_ALLVEC(2,OPTNBR,'OPTNBR',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,TDECBR,'TDECBR',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,DURBR ,'DURBR ',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,ZFINBR,'ZFINBR',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,ZDECBR,'ZDECBR',0,1,0,MESH)
        CALL BIEF_ALLVEC(1,ZCRBR ,'ZCRBR ',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,NUMPSD,'NUMPSD',0,1,0,MESH)
        CALL BIEF_ALLVEC(2,NBNDBR,'NBNDBR',0,1,0,MESH)
      ENDIF
      CALL ALLBLO(INDBR ,'INDBR ')
!
      READ(IFIC,*,END=900) ! COMMENT LINE
      READ(IFIC,*,ERR=998) LEMPRISE
!
      DO N = 1, NBRECH
        READ(IFIC,*,END=900) ! COMMENT LINE
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
        READ(IFIC,*,ERR=994) ZFINBR%R(N)
        READ(IFIC,*,END=900) ! COMMENT LINE
        IF(OPTNBR%I(N).EQ.3) THEN
          READ(IFIC,*,ERR=993) NUMPSD%I(N)
          READ(IFIC,*,END=900) ! COMMENT LINE
          IF(NCSIZE.GT.1) THEN 
            Num = NUMPSD%I(N)
            NUMPSD%I(N) = 0
            DO M=1,MESH%NPOIN 
              IF(Num.EQ.MESH%KNOLG%I(M)) THEN 
                NUMPSD%I(N) = M 
              ENDIF  
            ENDDO  
          ENDIF 
        ENDIF
        IF(OPTNBR%I(N).NE.1) THEN
          READ(IFIC,*,ERR=992) ZDECBR%R(N)
          READ(IFIC,*,END=900) ! COMMENT LINE
        ENDIF
        READ(IFIC,*,ERR=991) Nbl
        READ(IFIC,*,END=900) ! COMMENT LINE
!
!       ALLOCATION OF LOCAL VARIABLE TO READ BREACH DEFINITION
        ISTAT = 0
        ALLOCATE(Xl(Nbl), STAT=ISTAT)
        IF(ISTAT.NE.0) THEN
          IF(LNG.EQ.1) WRITE(LU,10) NOM,ISTAT
          IF(LNG.EQ.2) WRITE(LU,20) NOM,ISTAT
          STOP
        ENDIF
        ALLOCATE(Yl(Nbl), STAT=ISTAT)
        IF(ISTAT.NE.0) THEN
          IF(LNG.EQ.1) WRITE(LU,10) NOM,ISTAT
          IF(LNG.EQ.2) WRITE(LU,20) NOM,ISTAT
          STOP
        ENDIF
!
10      FORMAT(1X,'ERREUR A L''ALLOCATION DU VECTEUR : ',A6,/,1X,
     &            'CODE D''ERREUR : ',1I6)
20      FORMAT(1X,'ERROR DURING ALLOCATION OF VECTOR: ',A6,/,1X,
     &            'ERROR CODE: ',1I6)
!
        DO M = 1, Nbl
           READ(IFIC,*,ERR=990) Xl(M), Yl(M)
        ENDDO
!       SEARCH MESH POINTS INSIDE THE BREACH DOMAIN
        ISTAT = 0
        ALLOCATE(Xp(2*Nbl), STAT=ISTAT)
        IF(ISTAT.NE.0) THEN
          IF(LNG.EQ.1) WRITE(LU,10) NOM,ISTAT
          IF(LNG.EQ.2) WRITE(LU,20) NOM,ISTAT
          STOP
        ENDIF
        ALLOCATE(Yp(2*Nbl), STAT=ISTAT)
        IF(ISTAT.NE.0) THEN
          IF(LNG.EQ.1) WRITE(LU,10) NOM,ISTAT
          IF(LNG.EQ.2) WRITE(LU,20) NOM,ISTAT
          STOP
        ENDIF
!
        X1 = Xl(1)
        Y1 = Yl(1)
        X2 = Xl(2)
        Y2 = Yl(2)
        DX = X2 - X1
        DY = Y2 - Y1
        DS=DSQRT(DX*DX+DY*DY)
        IF(DS.GT.0.D0) THEN
          U1 = DX/DS
          U2 = DY/DS
        ELSE
          IF(LNG.EQ.1)
     &      WRITE(LU,*) 'PROBLEME DANS LA DEFINITION DE LA BRECHE :',N
          IF(LNG.EQ.2)
     &      WRITE(LU,*) 'PROBLEM IN DEFINITION OF BREACH :',N
          CALL PLANTE(1)
        ENDIF
        V1 = -U2
        V2 = U1
        Xp(1)     = X1 + V1*LEMPRISE/2.0
        Yp(1)     = Y1 + V2*LEMPRISE/2.0
        Xp(2*Nbl) = X1 - V1*LEMPRISE/2.0
        Yp(2*Nbl) = Y1 - V2*LEMPRISE/2.0
!
        DO M = 2,Nbl
           X2 = Xl(M)
           Y2 = Yl(M)
           DX = X2 - X1
           DY = Y2 - Y1
           DS=DSQRT(DX*DX+DY*DY)
           IF(DS.GT.0.D0) THEN
             U1 = DX/DS
             U2 = DY/DS
           ELSE
             IF(LNG.EQ.1)
     &         WRITE(LU,*) 'PROBLEME DANS LA DEFINITION DE LA BRECHE :',
     &                     N
             IF(LNG.EQ.2)
     &         WRITE(LU,*) 'PROBLEM IN DEFINITION OF BREACH :',N
             CALL PLANTE(1)
           ENDIF
           V1 = -U2
           V2 = U1
           Xp(M)         = X2 + V1*LEMPRISE/2.0
           Yp(M)         = Y2 + V2*LEMPRISE/2.0
           Xp(2*Nbl-M+1) = X2 - V1*LEMPRISE/2.0
           Yp(2*Nbl-M+1) = Y2 - V2*LEMPRISE/2.0
           X1=X2
           Y1=Y2
        ENDDO
!
        NBNDBR%I(N) = 0
        DO M = 1, NPOIN
           IF(INPOLY(MESH%X%R(M), MESH%Y%R(M), Xp, Yp, 2*Nbl)) THEN
             NBNDBR%I(N) = NBNDBR%I(N)+1
             Itmp(NBNDBR%I(N)) = M
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
            STOP 'MORE THAN 999 BREACHS ASKED IN LECBREACH'
          ENDIF
          ALLOCATE(INDBR%ADR(N)%P)
          CALL BIEF_ALLVEC(2,INDBR%ADR(N)%P,NOM,NBNDBR%I(N),1,0,MESH)
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
          STOP
        ENDIF
        DO M=1, NBNDBR%I(N)
           INDBR%ADR(N)%P%I(M) = Itmp(M)
        ENDDO
!
        DEALLOCATE(Xl)
        DEALLOCATE(Yl)
        DEALLOCATE(Xp)
        DEALLOCATE(Yp)
!
      ENDDO
C
      INDBR%N = NBRECH
      GOTO 1000
C
C-----------------------------------------------------------------------
C     MESSAGES D'ERREURS
C-----------------------------------------------------------------------
C
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
C
998   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'BRECHE : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES BRECHES'
        WRITE(LU,*) '         4EME LIGNE DU FICHIER NON CONFORME.'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'BRECHE : READ ERROR ON THE'
        WRITE(LU,*) '         BREACHES DATA FILE'
        WRITE(LU,*) '         AT LINE 4'
      ENDIF
      GO TO 2000
C
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
C
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
C
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
C
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
C
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
C
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
C
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
C
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
C
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
C
2000  CONTINUE
C
      CALL PLANTE(1)
C
1000  CONTINUE
      RETURN
      END                  
 
