C                       *****************
                        SUBROUTINE INIFAS
C                       *****************
C    
     *(TYPELE,NGEO)
C
C***********************************************************************
C PROGICIEL : STBTEL V5.2      09.07.1996  P. CHAILLET  (LHF) - FASTTABS
C***********************************************************************
C
C     FONCTION  : INITIALISATION DES INFORMATIONS DANS LE CAS DE FASTTAB
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C !      NOM       |MODE!                   ROLE
C !________________|____!______________________________________________     
C ! TYPELE         !<-- ! TYPE D'ELEMENT DU MAILLAGE (ICI TRIANGLES)   
C !________________!____!______________________________________________
C ! COMMON:        !    !
C !  GEO:          !    !
C !    MESH        !<-- ! TYPE DES ELEMENTS DU MAILLAGE
C !    NDP         !<-- ! NOMBRE DE NOEUDS PAR ELEMENTS ( TOUJOURS 3 ) 
C !    NPOIN       !<-- ! NOMBRE TOTAL DE NOEUDS DU MAILLAGE
C !    NELEM       !<-- ! NOMBRE TOTAL D'ELEMENTS DU MAILLAGE
C !    NPMAX       ! -->! DIMENSION EFFECTIVE DES TABLEAUX X ET Y
C !                !    ! (NPMAX = NPOIN + 100)
C !    NELMAX      ! -->! DIMENSION EFFECTIVE DES TABLEAUX CONCERNANT
C !                !    ! LES ELEMENTS (NELMAX = NELEM + 200)
C !  FICH:         !    !
C !    NRES        ! -->! NUMERO DU CANAL DU FICHIER DE SERAFIN
C !    NGEO       ! -->! NUMERO DU CANAL DU FICHIER MAILLEUR
C !    NLIM      ! -->! NUMERO DU CANAL DU FICHIER DYNAM DE TELEMAC
C ¸    NFO1      ¸ -->! NUMERO DU CANAL DU FICHIER TRIANGLE DE TRIGRID
C !                !    !
C !  INFO:         !    !
C !    LNG         !--> ! LANGUE UTILISEE
C !    LU          !--> ! CANAL DE SORTIE DES MESSAGES
C !________________!____!______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C APPELE PAR : HOMERE
C APPEL DE : 
C***********************************************************************
C     
      IMPLICIT NONE
C
      INTEGER       MESH, NDP, NPOIN, NELEM, NPMAX, NELMAX
      INTEGER       NGEO
      CHARACTER*(*) TYPELE
      CHARACTER*80  LIGNE
      INTEGER       LNG,LU
      INTEGER       IE,J
      INTEGER       ELMLOC(8)
C
C COMMON
C
      COMMON/GEO/MESH, NDP, NPOIN, NELEM, NPMAX, NELMAX
      COMMON/INFO/LNG,LU
C
C - INITIALISATION
C
      REWIND (NGEO)
      NPOIN = 0
      NELEM = 0
C
C  - BOUCLE DE LECTURE DU FICHIER DE MAILLAGE
C  - TANT QUE NON FIN DE FICHIER
C
 1    CONTINUE
        READ (NGEO, '(A80)', END=9000, ERR=8000) LIGNE
C
C - LA LIGNE COMMENCE PAR "GNN" - DEINITION D'UN POINT
C
        IF (LIGNE(1:3).EQ.'GNN') THEN
          NPOIN = NPOIN + 1
        ENDIF
C
C - LA LIGNE COMMENCE PAR "GE" - DEINITION D'UN ELEMENT
C
        IF (LIGNE(1:2).EQ.'GE') THEN
          NELEM = NELEM + 1
          READ(LIGNE(4:80),*,ERR=8000,END=9000) IE,
     +     (ELMLOC(J),J=1,8)
          IF (ELMLOC(8).NE.0.OR.
     +         (ELMLOC(4).NE.0.AND.ELMLOC(6).EQ.0) ) THEN
C
C - ON RAJOUTERA DES ELEMENTS
C
            NELEM = NELEM + 1
          ENDIF
        ENDIF
      GOTO 1
C
 9000 CONTINUE
      TYPELE = 'TRIANGLES  '
      NDP = 3
      MESH = 3 
C
      RETURN
C
C - TRAITEMENT ERREUR DE FICHIER
C
 8000 CONTINUE
      IF (LNG.EQ.1) WRITE (LU,4000)
      IF (LNG.EQ.2) WRITE (LU,4001)
 4000 FORMAT (//,1X,'***************************************'
     +        ,/,1X,'SOUS-PROGRAMME INIFAS : ERREUR DANS LA'
     +        ,/,1X,'LECTURE DU FICHIER DE MAILLAGE FASTTABS.'
     +        ,/,1X,'***************************************')
 4001 FORMAT (//,1X,'****************************'
     +        ,/,1X,'SUBROUTINE INIFAS :'
     +        ,/,1X,'ERROR READING FASTTABS FILE.'
     +        ,/,1X,'****************************')
      STOP
      END
