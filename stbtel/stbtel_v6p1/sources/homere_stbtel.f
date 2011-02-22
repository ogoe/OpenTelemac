C                       *********************
                        PROGRAM HOMERE_STBTEL
C                       *********************
C
C***********************************************************************
C  STBTEL VERSION 5.9     19/02/2009   J-M HERVOUET (LNH) 01 30 87 80 18
C
C***********************************************************************
C
C     FONCTIONS:
C     ==========
C
C 1)  ACQUISITION DE TOUTES LES DONNEES NECESSAIRES
C     AU CALCUL DES POINTEURS: FICHIER CAS + PARTIELLEMENT LA GEOMETRIE
C
C 2)  APPEL DU SOUS-PROGRAMME STBTEL.
C
C
C-----------------------------------------------------------------------
C
C SOUS-PROGRAMMES APPELES : LECDON , POINT , STBTEL
C
C**********************************************************************
C
C     USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_STBTEL
C
      IMPLICIT NONE
      INTEGER     LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER TDEB,TFIN
C
      CHARACTER(LEN=24), PARAMETER :: CODE='STBTEL                  '
C
      INTEGER  TIME_IN_SECONDS
      EXTERNAL TIME_IN_SECONDS
C
      INTEGER NPOIN1,NELMAX,NPMAX,NPOIN,NELEM,MESH,NDP 
C 
      INTEGER NSFOND ,NFOND(5) 
C   
      CHARACTER*11 TYPELE 
      CHARACTER*80 TITRE
      CHARACTER*6  PRECIS 
C   
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX 
C
C======================================================================
C
      INTEGER NCSIZE,NCAR,IPID
      CHARACTER*250 FORTXY(50)
      CHARACTER(LEN=250) PATH
      NGEO=1
      NCLE=2
      NCAS=3
      NLIM=7
      NRES=8
      NSOU=11
      NIMP=12
      NFRC=20
      NFON=23
      NFO1=26
      NFO2=27
      NOMGEO=' '
      NOMFO1=' '
      NOMFO2=' '
      NOMFON=' '
      NOMIMP=' '
      NOMSOU=' '
      NOMFRC=' '
      NOMFOR=' '
      NOMCAS=' '
      NOMLIM=' '
      NOMRES=' '
      CALL P_INIT(PATH,NCAR,IPID,NCSIZE)
      CALL READ_CONFIG(LNG,LU,PATH,NCAR)
      FORTXY(NGEO) ='FORT.1'
      FORTXY(NCLE) ='FORT.2'
      FORTXY(NCAS) ='FORT.3'
      FORTXY(NLIM) ='FORT.7'
      FORTXY(NRES) ='FORT.8'
      FORTXY(NSOU) ='FORT.11'
      FORTXY(NIMP) ='FORT.12'
      FORTXY(NFRC) ='FORT.20'
      FORTXY(NFON) ='FORT.23'
      FORTXY(NFO1) ='FORT.26'
      FORTXY(NFO2) ='FORT.27'
C
      TDEB = TIME_IN_SECONDS()
C
C     ENTETE SUR LISTING
C
      IF(LNG.EQ.1) WRITE(LU,100)
      IF(LNG.EQ.2) WRITE(LU,101)
      WRITE(LU,102)
100   FORMAT(/////,1X,'LISTING DE STBTEL ',78('-'))
101   FORMAT(/////,1X,'LISTING OF STBTEL ',78('-'))
102   FORMAT(/////,
     *14X,'   SSSSS  TTTTT  BBBB   TTTTT  EEEEE  L    ',/,
     *14X,'   S        T    B   B    T    E      L    ',/,
     *14X,'   SSSSS    T    BBBB     T    EEEE   L    ',/,
     *14X,'       S    T    B   B    T    E      L    ',/,
     *14X,'   SSSSS    T    BBBB     T    EEEEE  LLLLL',//,
     *14X,'            VERSION 6.0  FORTRAN 90                 ',/////)
C 
C======================================================================= 
C LECTURE DU FICHIER CAS 
C======================================================================= 
C 
      OPEN(NCLE , FILE=FORTXY(NCLE) , FORM='FORMATTED'  ,ACTION='READ')
      OPEN(NCAS , FILE=FORTXY(NCAS) , FORM='FORMATTED'  ,ACTION='READ')
      CALL LECDON_STBTEL 
C
C     LE FICHIER UNIVERSEL EST DE TYPE BINAIRE OU FORMATE
C
      IF(MAILLE.EQ.'SIMAIL'.OR.MAILLE.EQ.'SELAFIN') THEN 
        OPEN(NGEO,FILE='FORT.1' , FORM='UNFORMATTED') 
      ELSE 
        OPEN(NGEO,FILE='FORT.1' , FORM='FORMATTED') 
      ENDIF 
C 
C======================================================================= 
C OUVERTURE DES FICHIERS 
C======================================================================= 
C
      IF(NOMLIM(1:1).NE.' ') THEN
        OPEN(NLIM , FILE=FORTXY(NLIM),FORM='FORMATTED',ACTION='WRITE')
      ENDIF
C
      IF(NOMRES(1:1).NE.' ') THEN
        OPEN(NRES,FILE=FORTXY(NRES),
     *       FORM='UNFORMATTED',ACTION='READWRITE')
      ENDIF
C
      IF(NOMSOU(1:1).NE.' ') THEN
        OPEN(NSOU,FILE=FORTXY(NSOU),FORM='FORMATTED',ACTION='READWRITE')
      ENDIF
C
      IF(NOMIMP(1:1).NE.' ') THEN
        OPEN(NIMP,FILE=FORTXY(NIMP),FORM='FORMATTED',ACTION='READ')
      ENDIF
C
      IF(NOMFRC(1:1).NE.' ') THEN
        OPEN(NFRC,FILE=FORTXY(NFRC),FORM='FORMATTED',ACTION='READ')
      ENDIF
C
      IF(NOMFON(1:1).NE.' ') THEN
        OPEN(NFON,FILE=FORTXY(NFON),FORM='FORMATTED',ACTION='READ')
      ENDIF
C
      IF(NOMFO1(1:1).NE.' ') THEN
        OPEN(NFO1,FILE=FORTXY(NFO1),FORM='FORMATTED',ACTION='READ')
      ENDIF
C
      IF(NOMFO2(1:1).NE.' ') THEN
        OPEN(NFO2,FILE=FORTXY(NFO2),FORM='FORMATTED',ACTION='READ')
      ENDIF
C
C
C
      IF(NOMFO1(1:1).NE.' '.AND.MAILLE.EQ.'SELAFIN') THEN  
C       POUR SELAFIN : NFO1 EST BINAIRE
        CLOSE(NFO1)
        OPEN (NFO1, FILE='FORT.26',FORM='UNFORMATTED',ACTION='READ')
      ENDIF 
C
C CANAUX DU FICHIER FOND1 et SUIVANTS
C
      IF(NBFOND.NE.0) THEN 
        NFOND(1) = 23 
        NFOND(2) = 27 
        NFOND(3) = 12 
        NFOND(4) = 11 
        NFOND(5) = 20 
      ENDIF 
C 
C======================================================================= 
C INITIALISATION : RECHERCHE DES NOMBRES DE POINTS , D'ELEMENTS ET 
C                  DU TYPE DES ELEMENTS 
C======================================================================= 
C 
C INITIALISATION DE LA LONGUEUR DU TABLEAU NOP5 A 1
C 
      INOP5 = 1 
      NSFOND = 0 
C 
      IF (MAILLE.EQ.'SELAFIN') THEN 
         CALL INISEL (NPOIN1,TYPELE,STD,NSFOND,FUSION,IHAUT,
     *                NGEO , NFO1) 
      ELSEIF (MAILLE.EQ.'TRIGRID') THEN 
         CALL INITRI (NPOIN1,TYPELE,NGEO,NFO1)   
      ELSEIF (MAILLE.EQ.'FASTTABS') THEN 
         CALL INIFAS (TYPELE,NGEO)  
      ELSEIF (MAILLE.EQ.'SIMAIL') THEN 
         CALL INISIM (NPOIN1,TYPELE,INOP5,NGEO) 
      ELSEIF (MAILLE.EQ.'ADCIRC') THEN
         CALL INIADC (NPOIN1,TYPELE,NSFOND,IHAUT,NGEO,TITRE) 
      ELSE 
         CALL INISTB (NPOIN1,TYPELE,MAILLE,PRECIS,NGEO,
     *                NSEC2,NSEC11,NSEC12) 
      ENDIF
C 
C======================================================================= 
C DEFINITION DES POINTEURS 
C======================================================================= 
C 
      CALL POINT_STBTEL    
C 
C======================================================================= 
C APPEL DU PROGRAMME GENERAL 
C======================================================================= 
C 
      CALL STBTEL(NPOIN1,TYPELE,NFOND,PRECIS,NSFOND,TITRE)
C 
C======================================================================= 
C FERMETURE DES FICHIERS 
C======================================================================= 
C
      CLOSE(NGEO)
      CLOSE(NCLE)
      CLOSE(NCAS)
      IF(NOMLIM(1:1).NE.' ') CLOSE(NLIM)
      IF(NOMRES(1:1).NE.' ') CLOSE(NRES)
      IF(NOMSOU(1:1).NE.' ') CLOSE(NSOU)
      IF(NOMIMP(1:1).NE.' ') CLOSE(NIMP)
      IF(NOMFRC(1:1).NE.' ') CLOSE(NFRC)
      IF(NOMFON(1:1).NE.' ') CLOSE(NFON)
      IF(NOMFO1(1:1).NE.' ') CLOSE(NFO1)
      IF(NOMFO2(1:1).NE.' ') CLOSE(NFO2)
C
C-----------------------------------------------------------------------
C
      IF(LNG.EQ.1) WRITE(LU,10)
      IF(LNG.EQ.2) WRITE(LU,11)
10    FORMAT(1X,///,1X,'FIN NORMALE DU PROGRAMME',///)
11    FORMAT(1X,///,1X,'CORRECT END OF RUN',///)
C
C-----------------------------------------------------------------------
C
      TFIN = TIME_IN_SECONDS()
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'DUREE DU CALCUL : ',TFIN-TDEB,' SECONDES'
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'COMPUTER TIME: ',TFIN-TDEB,' SECONDS'
      ENDIF
C
C-----------------------------------------------------------------------
C
      STOP
      END
