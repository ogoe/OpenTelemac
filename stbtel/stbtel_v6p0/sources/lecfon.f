C                       *****************
                        SUBROUTINE LECFON
C                       *****************
C
     *( XRELV , YRELV , ZRELV , NBAT , NFOND , NBFOND ,  NP ,
     *  NPT , FONTRI , CORTRI , MAILLE, NGEO )
C
C***********************************************************************
C PROGICIEL : STBTEL V5.2         25/03/92    J-C GALLAND  (LNH)
C                                 09/11/94    P. LANG / LHF (TRIGRID)
C                                  07/96    P. CHAILLET / LHF (FASTTABS)
C***********************************************************************
C
C FONCTION : LECTURE DES FICHIERS DE BATHYMETRIE
C
C----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |    XRELV,YRELV | -->|  COORDONNEES DES POINTS DE BATHY
C |    ZRELV       | -->|  COTES DES POINTS DE BATHY
C |    NBAT        | -->|  NOMBRE DE POINTS DE BATHY
C |    NFOND       | -->|  CANAUX DES FICHIERS DES FONDS
C |    NBFOND      | -->|  NOMBRE DE FICHIERS FONDS DONNES PAR
C |                |    |  L'UTILISATEUR (5 MAXI)
C |    FOND        | -->|  NOM DES FICHIERS DES FONDS
C |    NP          | -->|  NOMBRES DE POINTS LUS PAR LECFON DANS LES
C |                |    |  FICHIERS DES FONDS
C |    NPT         | -->|  NOMBRE TOTAL DE POINTS DE BATHYMETRIE
C |    FONTRI      | -->|  INDICATEUR DE LECTURE DES FONDS DANS TRIGRID
C |    CORTRI      | -->|  VALEUR DE LA CORRECTION DES FONDS DE TRIGRID
C |    MAILLE      | -->| NOM DU MAILLEUR UTILISE
C |________________|____|______________________________________________
C | COMMON :       |    |
C |                |    |
C |  FICH:         |    |
C |    NRES        | -->|  NUMERO DU CANAL DU FICHIER GEOMETRIE
C |    NGEO        | -->|  NUMERO DU CANAL DU FICHIER UNIVERSEL
C |    NLIM        | -->|  NUMERO DU CANAL DU FICHIER DYNAM
C |    NFO1        | -->|  NUMERO DU CANAL DU FICHIER TRIANGLE TRIGRID
C |________________|____|______________________________________________

C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C----------------------------------------------------------------------
C
C APPELE PAR : INTERP
C APPEL DE : -
C
C**********************************************************************
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER I , NPT , NBAT
      INTEGER NFOND(*) , NP(5) , NBFOND
      INTEGER NGEO , IDUMMY , ITRI
C
      DOUBLE PRECISION XRELV(*) , YRELV(*) , ZRELV(*)
      DOUBLE PRECISION CORTRI
C
C     REELS DECLARES SIMPLES PRECISION POUR LECTURE FICHIER SINUSX
C
      REAL   XSP , YSP , ZSP
C
      CHARACTER*1  C
C
C Ajout PCt - 11/07/96
      CHARACTER*9  MAILLE
      CHARACTER*80 LIGNE
C
      LOGICAL FONTRI
C
      INTRINSIC DBLE
C      
C=======================================================================
C  INITIALISATION
C=======================================================================
C
      DO 10 I=1,NBAT
         XRELV(I)=0.D0
         YRELV(I)=0.D0
         ZRELV(I)=0.D0
10    CONTINUE
C
C=======================================================================
C LECTURE DES FICHIERS FOND
C=======================================================================
C
      NP(1) = 0
      NP(2) = 0
      NP(3) = 0
      NP(4) = 0
      NP(5) = 0
      NPT   = 0
C
C DANS LE CAS DU MAILLEUR TRIGRID, SI FONTRI=VRAI ON LIT LA BATHY 
C DIRECTEMENT DANS LE FICHIER UNIVERSEL, SINON ON EFFECTUE LE TRAITEMENT
C NORMAL.
C
C Modification PCt le 11/07/96
C ajout du cas FASTTABS
C
      IF (FONTRI) THEN
        IF (MAILLE.EQ.'TRIGRID') THEN
          IF (LNG.EQ.1) WRITE (LU,1040)
          IF (LNG.EQ.2) WRITE (LU,4040)
          REWIND (NGEO)
          READ (NGEO,'(//)')
1         CONTINUE
            READ (NGEO,*,END=9000,ERR=9000) IDUMMY,XSP,YSP,ITRI,ZSP
            NPT = NPT + 1
            XRELV(NPT) = DBLE(XSP)
            YRELV(NPT) = DBLE(YSP)
            ZRELV(NPT) = DBLE(-ZSP) + CORTRI
            GOTO 1
9000      CONTINUE
          NP(1) = NPT
          IF (LNG.EQ.1) WRITE (LU,1050) NPT
          IF (LNG.EQ.2) WRITE (LU,4050) NPT
        ELSEIF (MAILLE.EQ.'FASTTABS') THEN
C
C Ajout PCt - FASTTABS - le 11/07/1996
C
          IF (LNG.EQ.1) WRITE (LU,1060)
          IF (LNG.EQ.2) WRITE (LU,4070)
          REWIND (NGEO)
2         CONTINUE
            READ (NGEO,'(A)',END=9010,ERR=8000) LIGNE
            IF (LIGNE(1:3).EQ.'GNN') THEN
              READ(LIGNE(4:80),*,ERR=8000,END=8000) IDUMMY,XSP,YSP,ZSP
              NPT = NPT + 1
              XRELV(NPT) = DBLE(XSP)
              YRELV(NPT) = DBLE(YSP)
              ZRELV(NPT) = DBLE(ZSP)
            ENDIF
            GOTO 2
9010      CONTINUE
        ENDIF
C temporaire
      ELSE
C                          
        DO 20 I = 1,NBFOND
C
           REWIND NFOND(I)
30         READ(NFOND(I),1000,END=40) C
           IF (C(1:1).NE.'C'.AND.C(1:1).NE.'B') THEN
               BACKSPACE ( UNIT = NFOND(I) )
              NP(I)=NP(I)+1
              NPT  =NPT +1
              IF (NPT.GT.NBAT.AND.LNG.EQ.1) THEN
                 WRITE(LU,1020) NBAT
                 STOP
              ENDIF
              IF (NPT.GT.NBAT.AND.LNG.EQ.2) THEN
                 WRITE(LU,4020) NBAT
                 STOP
              ENDIF
C
C LECTURE FICHIER SINUSX SIMPLE PRECISION PUIS -> DOUBLE PRECISION
C
              READ (NFOND(I),*) XSP,YSP,ZSP
              XRELV(NPT) = DBLE(XSP)
              YRELV(NPT) = DBLE(YSP)
              ZRELV(NPT) = DBLE(ZSP)
C
           ENDIF
           GOTO 30
40         CONTINUE
           IF (NP(I).EQ.0.AND.LNG.EQ.1) THEN
              WRITE(LU,1030) I
              STOP
           ENDIF
           IF (NP(I).EQ.0.AND.LNG.EQ.2) THEN
              WRITE(LU,4030) I
              STOP
           ENDIF
C
20      CONTINUE
      ENDIF
C
C Ajout PCt - FASTTABS - le 11/07/1996
C
      RETURN
 8000 CONTINUE
      IF (LNG.EQ.1) WRITE (LU,4000)
      IF (LNG.EQ.2) WRITE (LU,4001)
 4000 FORMAT (//,1X,'***************************************'
     +        ,/,1X,'SOUS-PROGRAMME LECFON : ERREUR DANS LA'
     +        ,/,1X,'LECTURE DU FICHIER DE MAILLAGE FASTTABS.'
     +        ,/,1X,'***************************************')
 4001 FORMAT (//,1X,'****************************'
     +        ,/,1X,'SUBROUTINE LECFON :'
     +        ,/,1X,'ERROR READING FASTTABS FILE.'
     +        ,/,1X,'****************************')
      STOP
C
C-----------------------------------------------------------------------
C
1000  FORMAT(A1)
1020  FORMAT(/,'****************************************************',/,
     *         'LE NOMBRE DE POINTS DE BATHYMETRIE EST   ',           /,
     *         'SUPERIEUR A :',                                   1I6,/,
     *         'MODIFIER LE PARAMETRE SUIVANT DU FICHIER CAS : '     ,/,
     *         'NOMBRE MAXIMUM DE POINTS DE BATHYMETRIE'             ,/,
     *         '****************************************************')
4020  FORMAT(/,'****************************************************',/,
     *         'THE NUMBER OF BATHYMETRY POINTS IS     ',/,
     *         'GREATER THAN :',                                  1I6,/,
     *         'CHANGE THE FOLLOWING PARAMETER ',/,
     *         'IN THE STEERING FILE : ',/,
     *         'NUMBER OF BATHYMETRY POINTS '             ,/,
     *         '****************************************************')
1030  FORMAT(/,'********************************',/,
     *         'LE FICHIER FOND ',I1,' EST VIDE |',/,
     *         '********************************',/)
4030  FORMAT(/,'******************************************',/,
     *         'THE BOTTOM TOPOGRAPHY FILE ',I1,' IS EMPTY|',/,
     *         '******************************************',/)
1040  FORMAT(/,'**********************************************',/,
     *         'SOUS-PROGRAMME LECFON',/,
     *         'LA BATHYMETRIE EST LUE DANS LE FICHIER TRIGRID',/
     *         '**********************************************',/)
4040  FORMAT(/,'****************************************',/,
     *         'SUBROUTINE LECFON',/,
     *         'READING BATHYMETRY IN TRIGRID MESH FILE',/
     *         '****************************************',/)     
1050  FORMAT(/,'**********************************************',/,
     *         'SOUS-PROGRAMME LECFON',/,
     *         'NOMBRE DE POINTS LUS DANS LE FICHIER TRIGRID : ',
     *         I5,/
     *         '**********************************************',/)
4050  FORMAT(/,'****************************************',/,
     *         'SUBROUTINE LECFON',/,
     *         'NUMBER OF BATHYMETRIC POINTS IN TRIGRID FILE : ',
     *         I5,/
     *         '****************************************',/)
1060  FORMAT(/,'**********************************************',/,
     *         'SOUS-PROGRAMME LECFON',/,
     *         'LA BATHYMETRIE EST LUE DANS LE FICHIER FASTTABS',/
     *         '**********************************************',/)
4060  FORMAT(/,'****************************************',/,
     *         'SUBROUTINE LECFON',/,
     *         'READING BATHYMETRY IN FASTTABS MESH FILE',/
     *         '****************************************',/)
1070  FORMAT(/,'**********************************************',/,
     *         'SOUS-PROGRAMME LECFON',/,
     *         'NOMBRE DE POINTS LUS DANS LE FICHIER FASTTABS : ',
     *         I5,/
     *         '**********************************************',/)
4070  FORMAT(/,'****************************************',/,
     *         'SUBROUTINE LECFON',/,
     *         'NUMBER OF BATHYMETRIC POINTS IN FASTTABS FILE : ',
     *         I5,/
     *         '****************************************',/)
C
      END
