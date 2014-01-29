!                    *****************
                     SUBROUTINE LECSNG
!                    *****************
!
     &(IOPTAN,IFIC)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    READS THE DATA DEFINING SINGULARITIES
!+                FROM FORMATTED FILE 1.
!
!history  V. GUINOT (LHF)
!+        19/04/1996
!+
!+
!
!history  J.-M. HERVOUET (LNH)
!+        03/10/1996
!+        V5P2
!+   MODIFIED
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  J.-M. HERVOUET (LNH)
!+        04/08/2011
!+        V6P2
!+   Numdig in file is now the global number of points
!+   it is read here as such, and then changed into the former
!+   definition, boundary points numbers. This allows a treatment
!+   in parallel.
!
!history  C.COULET (ARTELIA)
!+        30/03/2012
!+        V6P2
!+   Separation of weirs and culvert file
!
!history  C.COULET / A.REBAI / E.DAVID (ARTELIA)
!+        21/03/2013
!+        V6P3
!+   Modification for new treatment of weirs and dynamic allocation
!
!history  J.-M. HERVOUET (LNH)
!+        05/08/2013
!+        V6P3
!+   Setting QP0 to 0 must be protected by a test on TYPSEUIL
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IFIC           |-->| LOGICAL UNIT OF FORMATED DATA FILE 1
!| IOPTAN         |<--| OPTION FOR TANGENTIAL VELOCITIES
!| TYPSEUIL       |<--| OPTION FOR TYPE OF WEIRS
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
      INTEGER, INTENT(IN)    :: IFIC
      INTEGER, INTENT(INOUT) :: IOPTAN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N,I,IPTFR,NNWEIRS
      DOUBLE PRECISION XDIG1,XDIG2,YDIG1,YDIG2
!
      CHARACTER(LEN=6) :: NOM
      CHARACTER*1 CHIFFRE(0:9)
      DATA CHIFFRE/'0','1','2','3','4','5','6','7','8','9'/
      SAVE CHIFFRE
!
!-----------------------------------------------------------------------
!
      MAXNPS=0
      READ(IFIC,*,END=900) ! COMMENT LINE
      READ(IFIC,*,ERR=998) NNWEIRS,IOPTAN
!
!     COHERENCE WITH THE STEERING FILE
!
      IF(NNWEIRS.NE.NWEIRS) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LECSNG : NOMBRE DE SEUILS : ',NNWEIRS
          WRITE(LU,*) '         DIFFERENT DE LA VALEUR DONNEE DANS LE'
          WRITE(LU,*) '         FICHIER DES PARAMETRES :',NWEIRS
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*) 'LECSNG : NUMBER OF WEIRS:',NNWEIRS
          WRITE(LU,*) '         DIFFERENT FROM THE ONE GIVEN IN THE'
          WRITE(LU,*) '         PARAMETER FILE: ',NWEIRS
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(LNG.EQ.1) THEN
        WRITE(LU,*)'LECSNG : NOMBRE DE DIGUES :',NWEIRS
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*)'LECSNG : NUMBER OF WEIRS :',NWEIRS
      ENDIF
!
!     ALLOCATIONS OF BLOCKS
!
!     GENERAL
      IF(NWEIRS.GT.0) THEN
        CALL BIEF_ALLVEC(2,NPSING,'NPSING',NWEIRS,1,0,MESH)
      ELSE
        CALL BIEF_ALLVEC(2,NPSING,'NPSING',0,1,0,MESH)
      ENDIF
      CALL ALLBLO(NDGA1 ,'NDGA1 ')
      CALL ALLBLO(NDGB1 ,'NDGB1 ')
      CALL ALLBLO(ZDIG  ,'ZDIG  ')
!     SPECIFIC
      IF(TYPSEUIL.EQ.1) THEN
        CALL ALLBLO(PHIDIG,'PHIDIG')
      ELSEIF(TYPSEUIL.EQ.2) THEN
        CALL ALLBLO(WDIG  ,'WDIG  ')
        CALL ALLBLO(NDGA2 ,'NDGA2 ')
        CALL ALLBLO(NDGB2 ,'NDGB2 ')
        CALL ALLBLO(QWA   ,'QWA   ')
        CALL ALLBLO(QWB   ,'QWB   ')
        CALL ALLBLO(UWEIRA,'UWEIRA')
        CALL ALLBLO(UWEIRB,'UWEIRB')
        CALL ALLBLO(VWEIRA,'VWEIRA')
        CALL ALLBLO(VWEIRB,'VWEIRB')
        CALL ALLBLO(QP0   ,'QP0   ')
      ELSE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*)'LECSNG : TYPE DE SEUIL NON PROGRAMME '
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*)'LECSNG : TYPE OF WEIRS NOT IMPLEMENTED'
        ENDIF
      ENDIF
!
      DO N=1,NWEIRS
        READ(IFIC,*,END=900)
        READ(IFIC,*,END=900)
        READ(IFIC,*,ERR=997) NPSING%I(N)
        MAXNPS = MAX(MAXNPS,NPSING%I(N))
!
!     ALLOCATIONS IN EACH BLOCK
!
      IF(N.LE.NDGA1%MAXBLOCK) THEN
        NOM='      '
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
          STOP 'MORE THAN 999 WEIRS ASKED IN LECSNG'
        ENDIF
!       GEENRAL
        NOM(1:3) = 'NA1'
        ALLOCATE(NDGA1%ADR(N)%P)
        CALL BIEF_ALLVEC(2,NDGA1%ADR(N)%P,NOM,NPSING%I(N),1,0,MESH)
        NOM(1:3) = 'NB1'
        ALLOCATE(NDGB1%ADR(N)%P)
        CALL BIEF_ALLVEC(2,NDGB1%ADR(N)%P,NOM,NPSING%I(N),1,0,MESH)
        NOM(1:3) = 'ZDG'
        ALLOCATE(ZDIG%ADR(N)%P)
        CALL BIEF_ALLVEC(1,ZDIG%ADR(N)%P,NOM,NPSING%I(N),1,0,MESH)
!       SPECIFIC
        IF(TYPSEUIL.EQ.1) THEN
          NOM(1:3) = 'PDG'
          ALLOCATE(PHIDIG%ADR(N)%P)
          CALL BIEF_ALLVEC(1,PHIDIG%ADR(N)%P,NOM,NPSING%I(N),1,0,MESH)
        ELSEIF(TYPSEUIL.EQ.2) THEN
          NOM(1:3) = 'NA2'
          ALLOCATE(NDGA2%ADR(N)%P)
          CALL BIEF_ALLVEC(2,NDGA2%ADR(N)%P,NOM,NPSING%I(N),1,0,MESH)
          NOM(1:3) = 'NB2'
          ALLOCATE(NDGB2%ADR(N)%P)
          CALL BIEF_ALLVEC(2,NDGB2%ADR(N)%P,NOM,NPSING%I(N),1,0,MESH)
          NOM(1:3) = 'QWA'
          ALLOCATE(QWA%ADR(N)%P)
          CALL BIEF_ALLVEC(1,QWA%ADR(N)%P,NOM,NPSING%I(N),1,0,MESH)
          NOM(1:3) = 'QWB'
          ALLOCATE(QWB%ADR(N)%P)
          CALL BIEF_ALLVEC(1,QWB%ADR(N)%P,NOM,NPSING%I(N),1,0,MESH)
          NOM(1:3) = 'UWA'
          ALLOCATE(UWEIRA%ADR(N)%P)
          CALL BIEF_ALLVEC(1,UWEIRA%ADR(N)%P,NOM,NPSING%I(N),1,0,MESH)
          NOM(1:3) = 'UWB'
          ALLOCATE(UWEIRB%ADR(N)%P)
          CALL BIEF_ALLVEC(1,UWEIRB%ADR(N)%P,NOM,NPSING%I(N),1,0,MESH)
          NOM(1:3) = 'VWA'
          ALLOCATE(VWEIRA%ADR(N)%P)
          CALL BIEF_ALLVEC(1,VWEIRA%ADR(N)%P,NOM,NPSING%I(N),1,0,MESH)
          NOM(1:3) = 'VWB'
          ALLOCATE(VWEIRB%ADR(N)%P)
          CALL BIEF_ALLVEC(1,VWEIRB%ADR(N)%P,NOM,NPSING%I(N),1,0,MESH)
          NOM(1:3) = 'WDG'
          ALLOCATE(WDIG%ADR(N)%P)
          CALL BIEF_ALLVEC(1,WDIG%ADR(N)%P,NOM,NPSING%I(N)-1,1,0,MESH)
          NOM(1:3) = 'QP0'
!         JMH 05/08/2013: IF NOT INCREMENTED, QP0%N WILL REMAIN 0
          QP0%N=QP0%N+1
          ALLOCATE(QP0%ADR(N)%P)
          CALL BIEF_ALLVEC(1,QP0%ADR(N)%P,NOM,NPSING%I(N)-1,1,0,MESH)
        ELSE
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)'LECSNG : TYPE DE SEUIL NON PROGRAMME '
          ELSEIF(LNG.EQ.2) THEN
            WRITE(LU,*)'LECSNG : TYPE OF WEIRS NOT IMPLEMENTED'
          ENDIF
        ENDIF
      ELSE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LECSNG :'
          WRITE(LU,*) 'PLUS DE ',NDGA1%MAXBLOCK,' (',N,')'
          WRITE(LU,*) 'VECTEURS DEMANDES,'
          WRITE(LU,*) 'CHANGER MAXBLOCK DANS ALLBLO.'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'LECSNG:'
          WRITE(LU,*) 'MORE THAN ',NDGA1%MAXBLOCK,'(',N,')'
          WRITE(LU,*) 'VECTORS TO BE ALLOCATED'
          WRITE(LU,*) 'CHANGE MAXBLOCK IN ALLBLO.'
        ENDIF
        STOP
      ENDIF
!
      IF(TYPSEUIL.EQ.1) THEN
        READ(IFIC,*,END=900)
        READ(IFIC,*,ERR=996) (NDGA1%ADR(N)%P%I(I),I=1,NPSING%I(N))
        READ(IFIC,*,END=900)
        READ(IFIC,*,ERR=994) (NDGB1%ADR(N)%P%I(I),I=1,NPSING%I(N))
        READ(IFIC,*,END=900)
        READ(IFIC,*,ERR=992) (ZDIG%ADR(N)%P%R(I),I=1,NPSING%I(N))
        READ(IFIC,*,END=900)
        READ(IFIC,*,ERR=991) (PHIDIG%ADR(N)%P%R(I),I=1,NPSING%I(N))
      ELSEIF(TYPSEUIL.EQ.2) THEN
        DO I=1,NPSING%I(N)
          READ(IFIC,*,ERR=990) XDIG2,YDIG2,ZDIG%ADR(N)%P%R(I),
     &                         NDGA1%ADR(N)%P%I(I),NDGA2%ADR(N)%P%I(I),
     &                         NDGB1%ADR(N)%P%I(I),NDGB2%ADR(N)%P%I(I)
          IF(I.GT.1) THEN
            WDIG%ADR(N)%P%R(I-1)=DSQRT((XDIG2-XDIG1)**2+
     &                                 (YDIG2-YDIG1)**2)
          ENDIF  
          XDIG1=XDIG2
          YDIG1=YDIG2
        ENDDO
      ELSE
         IF(LNG.EQ.1) THEN
           WRITE(LU,*)'LECSNG : TYPE DE SEUIL NON PROGRAMME '
         ELSEIF(LNG.EQ.2) THEN
           WRITE(LU,*)'LECSNG : TYPE OF WEIRS NOT IMPLEMENTED'
         ENDIF
      ENDIF
      ENDDO ! N
!
!     RETRIEVING BOUNDARY POINTS NUMBERS 
!     WITH MINUS SIGN TO TRACE POINTS WHICH ARE NOT IN THE DOMAIN
!     SEE STEP 2.
!
!     1) SIDES 1 AND 2
!
      IF(TYPSEUIL.EQ.1) THEN
        IF(NCSIZE.GT.1) THEN
!
          DO N=1,NWEIRS
            DO I=1,NPSING%I(N)
              DO IPTFR=1,NPTFR
                IF(NDGA1%ADR(N)%P%I(I).EQ.
     &             MESH%KNOLG%I(MESH%NBOR%I(IPTFR))) THEN
                  NDGA1%ADR(N)%P%I(I)=-IPTFR
                  EXIT
                ENDIF 
              ENDDO
              DO IPTFR=1,NPTFR
                IF(NDGB1%ADR(N)%P%I(I).EQ.
     &             MESH%KNOLG%I(MESH%NBOR%I(IPTFR))) THEN
                  NDGB1%ADR(N)%P%I(I)=-IPTFR
                  EXIT
                ENDIF 
              ENDDO
            ENDDO
          ENDDO
!
        ELSE
!
          DO N=1,NWEIRS
            DO I=1,NPSING%I(N)
              DO IPTFR=1,NPTFR
                IF(NDGA1%ADR(N)%P%I(I).EQ.MESH%NBOR%I(IPTFR)) THEN
                  NDGA1%ADR(N)%P%I(I)=-IPTFR
                  EXIT
                ENDIF 
              ENDDO
              DO IPTFR=1,NPTFR
                IF(NDGB1%ADR(N)%P%I(I).EQ.MESH%NBOR%I(IPTFR)) THEN
                  NDGB1%ADR(N)%P%I(I)=-IPTFR
                  EXIT
                ENDIF 
              ENDDO
            ENDDO
          ENDDO
!
        ENDIF
      ELSEIF(TYPSEUIL.EQ.2) THEN
        IF(NCSIZE.GT.1) THEN
!
          DO N=1,NWEIRS
            DO I=1,NPSING%I(N)
              DO IPTFR=1,NPOIN
                IF(NDGA1%ADR(N)%P%I(I).EQ.
     &             MESH%KNOLG%I(IPTFR)) THEN
                  NDGA1%ADR(N)%P%I(I)=-IPTFR
                  EXIT
                ENDIF 
              ENDDO
              DO IPTFR=1,NPOIN
                IF(NDGA2%ADR(N)%P%I(I).EQ.
     &             MESH%KNOLG%I(IPTFR)) THEN
                  NDGA2%ADR(N)%P%I(I)=-IPTFR
                  EXIT
                ENDIF 
              ENDDO
              DO IPTFR=1,NPOIN
                IF(NDGB1%ADR(N)%P%I(I).EQ.
     &             MESH%KNOLG%I(IPTFR)) THEN
                  NDGB1%ADR(N)%P%I(I)=-IPTFR
                  EXIT
                ENDIF 
              ENDDO
              DO IPTFR=1,NPOIN
                IF(NDGB2%ADR(N)%P%I(I).EQ.
     &             MESH%KNOLG%I(IPTFR)) THEN
                  NDGB2%ADR(N)%P%I(I)=-IPTFR
                  EXIT
                ENDIF 
              ENDDO
            ENDDO
          ENDDO
!
        ELSE
!
          DO N=1,NWEIRS
            DO I=1,NPSING%I(N)
              DO IPTFR=1,NPOIN
                IF(NDGA1%ADR(N)%P%I(I).EQ.IPTFR) THEN
                  NDGA1%ADR(N)%P%I(I)=-IPTFR
                  EXIT
                ENDIF 
              ENDDO
              DO IPTFR=1,NPOIN
                IF(NDGA2%ADR(N)%P%I(I).EQ.IPTFR) THEN
                  NDGA2%ADR(N)%P%I(I)=-IPTFR
                  EXIT
                ENDIF 
              ENDDO
              DO IPTFR=1,NPOIN
                IF(NDGB1%ADR(N)%P%I(I).EQ.IPTFR) THEN
                  NDGB1%ADR(N)%P%I(I)=-IPTFR
                  EXIT
                ENDIF 
              ENDDO
              DO IPTFR=1,NPOIN
                IF(NDGB2%ADR(N)%P%I(I).EQ.IPTFR) THEN
                  NDGB2%ADR(N)%P%I(I)=-IPTFR
                  EXIT
                ENDIF 
              ENDDO
            ENDDO
          ENDDO
!
        ENDIF
      ELSE
         IF(LNG.EQ.1) THEN
           WRITE(LU,*)'LECSNG : TYPE DE SEUIL NON PROGRAMME '
         ELSEIF(LNG.EQ.2) THEN
           WRITE(LU,*)'LECSNG : TYPE OF WEIRS NOT IMPLEMENTED'
         ENDIF
      ENDIF
!
!     2) NOW PUTTING A POSITIVE NUMBER IF POINT IN DOMAIN
!                    AND ZERO IF POINT NOT IN DOMAIN (PARALLELISM)
!
      DO N=1,NWEIRS
        DO I=1,NPSING%I(N)
          NDGA1%ADR(N)%P%I(I)=MAX(-NDGA1%ADR(N)%P%I(I),0)
          NDGB1%ADR(N)%P%I(I)=MAX(-NDGB1%ADR(N)%P%I(I),0)
          IF(TYPSEUIL.EQ.2) THEN
            NDGA2%ADR(N)%P%I(I)=MAX(-NDGA2%ADR(N)%P%I(I),0)
            NDGB2%ADR(N)%P%I(I)=MAX(-NDGB2%ADR(N)%P%I(I),0)
          ENDIF
        ENDDO
      ENDDO
!
      GO TO 1000
!
!-----------------------------------------------------------------------
!     ERROR MESSAGES
!-----------------------------------------------------------------------
!
998   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSNG : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES SEUILS'
        WRITE(LU,*) '         2EME LIGNE DU FICHIER NON CONFORME.'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSNG : READ ERROR ON THE'
        WRITE(LU,*) '         WEIRS DATA FILE'
        WRITE(LU,*) '         AT LINE 2'
      ENDIF
      GO TO 2000
!
997   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSNG : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES SEUILS'
        WRITE(LU,*) '         POUR LA SINGULARITE ',N
        WRITE(LU,*) '         NOMBRE DE POINTS ILLISIBLE'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSNG : READ ERROR ON THE'
        WRITE(LU,*) '         WEIRS DATA FILE'
        WRITE(LU,*) '         FOR SINGULARITY NUMBER ',N
        WRITE(LU,*) '         THE NUMBER OF POINTS CANNOT BE READ'
      ENDIF
      GO TO 2000
!
996   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSNG : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES SEUILS'
        WRITE(LU,*) '         POUR LA SINGULARITE ',N
        WRITE(LU,*) '         NUMDIGS DES POINTS ILLISIBLE'
        WRITE(LU,*) '         POUR LE COTE 1'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSNG : READ ERROR ON THE'
        WRITE(LU,*) '         WEIRS DATA FILE'
        WRITE(LU,*) '         FOR SINGULARITY NUMBER ',N
        WRITE(LU,*) '         THE NUMBER OF THE POINTS CANNOT BE READ'
        WRITE(LU,*) '         FOR SIDE NUMBER 1'
      ENDIF
      GO TO 2000
!
994   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSNG : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES SEUILS'
        WRITE(LU,*) '         POUR LA SINGULARITE ',N
        WRITE(LU,*) '         NUMDIGS DES POINTS ILLISIBLE'
        WRITE(LU,*) '         POUR LE COTE 2'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSNG : READ ERROR ON THE'
        WRITE(LU,*) '         WEIRS DATA FILE'
        WRITE(LU,*) '         FOR SINGULARITY NUMBER ',N
        WRITE(LU,*) '         THE NUMBER OF THE POINTS CANNOT BE READ'
        WRITE(LU,*) '         FOR SIDE NUMBER 2'
      ENDIF
      GO TO 2000
!
992   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSNG : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES SEUILS'
        WRITE(LU,*) '         POUR LA SINGULARITE ',N
        WRITE(LU,*) '         COTES SUR LA DIGUE ILLISIBLES'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSNG : READ ERROR ON THE'
        WRITE(LU,*) '         WEIRS DATA FILE'
        WRITE(LU,*) '         FOR SINGULARITY NUMBER ',N
        WRITE(LU,*) '         ELEVATIONS ON THE WEIR CANNOT BE READ'
      ENDIF
      GO TO 1000
!
991   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSNG : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES SEUILS'
        WRITE(LU,*) '         POUR LA SINGULARITE ',N
        WRITE(LU,*) '         COEFFICIENTS DE DEBIT ILLISIBLES'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSNG : READ ERROR ON THE'
        WRITE(LU,*) '         WEIRS DATA FILE'
        WRITE(LU,*) '         FOR SINGULARITY NUMBER ',N
        WRITE(LU,*) '         DISCHARGE COEFFICIENTS CANNOT BE READ'
      ENDIF
      GO TO 2000
!
990   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSNG : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES SEUILS'
        WRITE(LU,*) '         POUR LA SINGULARITE ',N
        WRITE(LU,*) '         DESCRIPTION DU SEUIL ILLISIBLE'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSNG : READ ERROR ON THE'
        WRITE(LU,*) '         WEIRS DATA FILE'
        WRITE(LU,*) '         FOR SINGULARITY NUMBER ',N
        WRITE(LU,*) '         WEIR DESCRIPTION CANNOT BE READ'
      ENDIF
      GO TO 2000
!
900   CONTINUE
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'LECSNG : ERREUR DE LECTURE SUR LE'
        WRITE(LU,*) '         FICHIER DE DONNEES DES SEUILS'
        WRITE(LU,*) '         FIN DE FICHIER PREMATUREE'
      ELSEIF(LNG.EQ.2) THEN
        WRITE(LU,*) 'LECSNG : READ ERROR ON THE'
        WRITE(LU,*) '         WEIRS DATA FILE'
        WRITE(LU,*) '         UNEXPECTED END OF FILE'
      ENDIF
!
2000  CONTINUE
!
      NWEIRS = 0
!
1000  CONTINUE
!
      IF(NWEIRS.EQ.0) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*)
          WRITE(LU,*)'LECSNG : ERREUR DE LECTURE'
          WRITE(LU,*)'         AUCUNE SINGULARITE NE SERA'
          WRITE(LU,*)'         PRISE EN COMPTE.'
          WRITE(LU,*)
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*)
          WRITE(LU,*)'LECSNG : READ ERROR'
          WRITE(LU,*)'         NO SINGULARITY WILL BE TAKEN'
          WRITE(LU,*)'         INTO ACCOUNT'
          WRITE(LU,*)
        ENDIF
      ENDIF
!
      IF(TYPSEUIL.EQ.2) THEN
        DO N=1, NWEIRS
          CALL OS('X=0     ',X=QP0%ADR(N)%P)
        ENDDO
        CALL ALLBLO(TWEIRA,'TWEIRA')
        CALL ALLBLO(TWEIRB,'TWEIRA')
        IF(NTRAC.GT.0) THEN
          CALL BIEF_ALLVEC_IN_BLOCK(TWEIRA,NTRAC,1,'TWEIRA',
     &                              NWEIRS,MAXNPS,0,MESH)
          CALL BIEF_ALLVEC_IN_BLOCK(TWEIRB,NTRAC,1,'TWEIRB',
     &                              NWEIRS,MAXNPS,0,MESH)
        ELSE
          CALL BIEF_ALLVEC_IN_BLOCK(TWEIRA,1    ,1,'TWEIRA',
     &                              NWEIRS,MAXNPS,0,MESH)
          CALL BIEF_ALLVEC_IN_BLOCK(TWEIRB,1    ,1,'TWEIRB',
     &                              NWEIRS,MAXNPS,0,MESH)
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
