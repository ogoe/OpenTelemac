!                       *********************
                        PROGRAM HOMERE_STBTEL
!                       *********************
!
!***********************************************************************
!  STBTEL VERSION 5.9     19/02/2009   J-M HERVOUET (LNH) 01 30 87 80 18
!
!***********************************************************************
!
!     FONCTIONS:
!     ==========
!
! 1)  ACQUISITION DE TOUTES LES DONNEES NECESSAIRES
!     AU CALCUL DES POINTEURS: FICHIER CAS + PARTIELLEMENT LA GEOMETRIE
!
! 2)  APPEL DU SOUS-PROGRAMME STBTEL.
!
!
!-----------------------------------------------------------------------
!
! SOUS-PROGRAMMES APPELES : LECDON , POINT , STBTEL
!
!**********************************************************************
!
!     USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_STBTEL
!
      IMPLICIT NONE
      INTEGER     LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER TDEB,TFIN
!
      CHARACTER(LEN=24), PARAMETER :: CODE='STBTEL                  '
!
      INTEGER  TIME_IN_SECONDS
      EXTERNAL TIME_IN_SECONDS
!
      INTEGER NPOIN1,NELMAX,NPMAX,NPOIN,NELEM,MESH,NDP
!
      INTEGER NSFOND ,NFOND(5)
!
      CHARACTER*11 TYPELE
      CHARACTER*80 TITRE
      CHARACTER*6  PRECIS
!
      COMMON/GEO/ MESH , NDP , NPOIN , NELEM , NPMAX , NELMAX
!
!======================================================================
!
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
!
      TDEB = TIME_IN_SECONDS()
!
!     ENTETE SUR LISTING
!
      IF(LNG.EQ.1) WRITE(LU,100)
      IF(LNG.EQ.2) WRITE(LU,101)
      WRITE(LU,102)
100   FORMAT(/////,1X,'LISTING DE STBTEL ',78('-'))
101   FORMAT(/////,1X,'LISTING OF STBTEL ',78('-'))
102   FORMAT(/////,
     &14X,'   SSSSS  TTTTT  BBBB   TTTTT  EEEEE  L    ',/,
     &14X,'   S        T    B   B    T    E      L    ',/,
     &14X,'   SSSSS    T    BBBB     T    EEEE   L    ',/,
     &14X,'       S    T    B   B    T    E      L    ',/,
     &14X,'   SSSSS    T    BBBB     T    EEEEE  LLLLL',//,
     &14X,'            VERSION 6.0  FORTRAN 90                 ',/////)
!
!=======================================================================
! LECTURE DU FICHIER CAS
!=======================================================================
!
      OPEN(NCLE , FILE=FORTXY(NCLE) , FORM='FORMATTED'  ,ACTION='READ')
      OPEN(NCAS , FILE=FORTXY(NCAS) , FORM='FORMATTED'  ,ACTION='READ')
      CALL LECDON_STBTEL
!
!     LE FICHIER UNIVERSEL EST DE TYPE BINAIRE OU FORMATE
!
      IF(MAILLE.EQ.'SIMAIL'.OR.MAILLE.EQ.'SELAFIN') THEN
        OPEN(NGEO,FILE='FORT.1' , FORM='UNFORMATTED')
      ELSE
        OPEN(NGEO,FILE='FORT.1' , FORM='FORMATTED')
      ENDIF
!
!=======================================================================
! OUVERTURE DES FICHIERS
!=======================================================================
!
      IF(NOMLIM(1:1).NE.' ') THEN
        OPEN(NLIM , FILE=FORTXY(NLIM),FORM='FORMATTED',ACTION='WRITE')
      ENDIF
!
      IF(NOMRES(1:1).NE.' ') THEN
        OPEN(NRES,FILE=FORTXY(NRES),
     &       FORM='UNFORMATTED',ACTION='READWRITE')
      ENDIF
!
      IF(NOMSOU(1:1).NE.' ') THEN
        OPEN(NSOU,FILE=FORTXY(NSOU),FORM='FORMATTED',ACTION='READWRITE')
      ENDIF
!
      IF(NOMIMP(1:1).NE.' ') THEN
        OPEN(NIMP,FILE=FORTXY(NIMP),FORM='FORMATTED',ACTION='READ')
      ENDIF
!
      IF(NOMFRC(1:1).NE.' ') THEN
        OPEN(NFRC,FILE=FORTXY(NFRC),FORM='FORMATTED',ACTION='READ')
      ENDIF
!
      IF(NOMFON(1:1).NE.' ') THEN
        OPEN(NFON,FILE=FORTXY(NFON),FORM='FORMATTED',ACTION='READ')
      ENDIF
!
      IF(NOMFO1(1:1).NE.' ') THEN
        OPEN(NFO1,FILE=FORTXY(NFO1),FORM='FORMATTED',ACTION='READ')
      ENDIF
!
      IF(NOMFO2(1:1).NE.' ') THEN
        OPEN(NFO2,FILE=FORTXY(NFO2),FORM='FORMATTED',ACTION='READ')
      ENDIF
!
!
!
      IF(NOMFO1(1:1).NE.' '.AND.MAILLE.EQ.'SELAFIN') THEN
!       POUR SELAFIN : NFO1 EST BINAIRE
        CLOSE(NFO1)
        OPEN (NFO1, FILE='FORT.26',FORM='UNFORMATTED',ACTION='READ')
      ENDIF
!
! CANAUX DU FICHIER FOND1 et SUIVANTS
!
      IF(NBFOND.NE.0) THEN
        NFOND(1) = 23
        NFOND(2) = 27
        NFOND(3) = 12
        NFOND(4) = 11
        NFOND(5) = 20
      ENDIF
!
!=======================================================================
! INITIALISATION : RECHERCHE DES NOMBRES DE POINTS , D'ELEMENTS ET
!                  DU TYPE DES ELEMENTS
!=======================================================================
!
! INITIALISATION DE LA LONGUEUR DU TABLEAU NOP5 A 1
!
      INOP5 = 1
      NSFOND = 0
!
      IF (MAILLE.EQ.'SELAFIN') THEN
         CALL INISEL (NPOIN1,TYPELE,STD,NSFOND,FUSION,IHAUT,
     &                NGEO , NFO1)
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
     &                NSEC2,NSEC11,NSEC12)
      ENDIF
!
!=======================================================================
! DEFINITION DES POINTEURS
!=======================================================================
!
      CALL POINT_STBTEL
!
!=======================================================================
! APPEL DU PROGRAMME GENERAL
!=======================================================================
!
      CALL STBTEL(NPOIN1,TYPELE,NFOND,PRECIS,NSFOND,TITRE)
!
!=======================================================================
! FERMETURE DES FICHIERS
!=======================================================================
!
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
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) WRITE(LU,10)
      IF(LNG.EQ.2) WRITE(LU,11)
10    FORMAT(1X,///,1X,'FIN NORMALE DU PROGRAMME',///)
11    FORMAT(1X,///,1X,'CORRECT END OF RUN',///)
!
!-----------------------------------------------------------------------
!
      TFIN = TIME_IN_SECONDS()
      IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'DUREE DU CALCUL : ',TFIN-TDEB,' SECONDES'
      ENDIF
      IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'COMPUTER TIME: ',TFIN-TDEB,' SECONDS'
      ENDIF
!
!-----------------------------------------------------------------------
!
      STOP
      END