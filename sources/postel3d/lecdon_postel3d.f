!                       **************************
                        SUBROUTINE LECDON_POSTEL3D
!                       **************************
!
     &(MOTCAR,FILE_DESC,PATH,NCAR)
!
!***********************************************************************
! POSTEL3D VERSION 6.0   01/09/99   T. DENOT (LNH) 01 30 87 74 89
! FORTRAN90
!***********************************************************************
!
! SOUS-PROGRAMME APPELE PAR : HOMERE_POSTEL3D
! SOUS-PROGRAMME APPELES : DAMOC , LIT
!
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!**********************************************************************
!
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_POSTEL3D
      USE INTERFACE_HERMES
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)               :: NCAR
      CHARACTER(LEN=250), INTENT(IN)    :: PATH
      CHARACTER(LEN=144), INTENT(INOUT) :: FILE_DESC(4,MAXKEYWORD)
      CHARACTER(LEN=144), INTENT(INOUT) :: MOTCAR(MAXKEYWORD)
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=250) NOM_CAS,NOM_DIC
      CHARACTER(LEN=24), PARAMETER :: CODE='POSTEL3D                '
!
!
! DECLARATION DES VARIABLES LUES DANS NPRE
!
      INTEGER ADRESS(4,MAXKEYWORD),DIMENS(4,MAXKEYWORD)
      DOUBLE PRECISION   MOTREA(MAXKEYWORD)
      INTEGER            MOTINT(MAXKEYWORD)
      LOGICAL            MOTLOG(MAXKEYWORD)
!
      CHARACTER*72     MOTCLE(4,MAXKEYWORD,2)
      INTEGER          TROUVE(4,MAXKEYWORD)
      INTEGER      J,K
      LOGICAL DOC
!
      INTEGER  ERR
      CHARACTER(LEN=8) FFORMAT
      CHARACTER(LEN=80) TITLE
      CHARACTER(LEN=16),ALLOCATABLE :: VAR_NAME(:), VAR_UNIT(:)
      INTEGER FID
!
!***********************************************************************
!
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!
! LECTURE DU FICHIER CAS
!
      IF (LNG.EQ.1) WRITE(LU,21)
      IF (LNG.EQ.2) WRITE(LU,22)
!
      DO K=1,MAXKEYWORD
!
!    UN FICHIER NON DONNE PAR DAMOCLES SERA RECONNU PAR UN BLANC
!    (IL N'EST PAS SUR QUE TOUS LES COMPILATEURS INITIALISENT AINSI)
!
        MOTCAR(K)(1:1)=' '
!
        DIMENS(1,K) = 0
        DIMENS(2,K) = 0
        DIMENS(3,K) = 0
        DIMENS(4,K) = 0
!
      ENDDO
!
!     IMPRESSION DE LA DOC
      DOC = .FALSE.
!
!-----------------------------------------------------------------------
!     OUVERTURE DES FICHIERS DICTIONNAIRE ET CAS
!-----------------------------------------------------------------------
!
      NOM_DIC='POSDICO'
      NOM_CAS='POSCAS'
      OPEN(2,FILE=NOM_DIC,FORM='FORMATTED',ACTION='READ')
      OPEN(3,FILE=NOM_CAS,FORM='FORMATTED',ACTION='READ')
!
      CALL DAMOCLE( ADRESS , DIMENS , MAXKEYWORD, DOC  , LNG    , LU ,
     &              MOTINT , MOTREA , MOTLOG , MOTCAR  , MOTCLE ,
     &              TROUVE , 2      , 3      , .FALSE. , FILE_DESC )
!
!-----------------------------------------------------------------------
!     FERMETURE DES FICHIERS DICTIONNAIRE ET CAS
!-----------------------------------------------------------------------
!
      CLOSE(2)
      CLOSE(3)
!
!     DECRYPTAGE DES CHAINES SUBMIT
!
      CALL READ_SUBMIT(POS_FILES,100,CODE,FILE_DESC,MAXKEYWORD)
!
!-----------------------------------------------------------------------
!
!     RETRIEVING FILES NUMBERS IN POSTEL-3D FORTRAN PARAMETERS
!     AT THIS LEVEL LOGICAL UNITS ARE EQUAL TO THE FILE NUMBER
!
      DO J=1,100
        IF(POS_FILES(J)%TELNAME.EQ.'POSPRE') THEN
!         POSPRE=POS_FILES(J)%LU  (IS EQUIVALENT)
          POSPRE=J
        ELSEIF(POS_FILES(J)%TELNAME.EQ.'POSHOR') THEN
          POSHOR=J
        ELSEIF(POS_FILES(J)%TELNAME.EQ.'POSVER') THEN
          POSVER=J
        ELSEIF(POS_FILES(J)%TELNAME.EQ.'POSGEO') THEN
          POSGEO=J
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
!     MOTS CLES LIES A TOUTES LES COUPES
!
      NUPRSO = MAX(MOTINT(ADRESS(1,3)),1)
      PESOGR = MAX(MOTINT(ADRESS(1,4)),1)
!
!     MOTS CLES LIES DE TYPE CARACTERE
!
      BINPRE = MOTCAR(ADRESS(4,14))(1:3)
      BINCOU = MOTCAR(ADRESS(4,15))(1:3)
      BINGEO = MOTCAR(ADRESS(4,17))(1:3)
!
!     FORMATS EN DUR
!
      POS_FILES(POSPRE)%FMT=MOTCAR( ADRESS(4, 19) )(1:8)
      POS_FILES(POSHOR)%FMT=MOTCAR( ADRESS(4, 20) )(1:8)
      POS_FILES(POSVER)%FMT=MOTCAR( ADRESS(4, 21) )(1:8)
      POS_FILES(POSGEO)%FMT=MOTCAR( ADRESS(4, 18) )(1:8)
!
      POS_FILES(POSPRE)%NAME=MOTCAR( ADRESS(4, 3) )
      POS_FILES(POSHOR)%NAME=MOTCAR( ADRESS(4, 4) )
      POS_FILES(POSVER)%NAME=MOTCAR( ADRESS(4, 5) )
      POS_FILES(POSGEO)%NAME=MOTCAR( ADRESS(4,16) )
!
!-----------------------------------------------------------------------
!
! LECTURE PARTIELLE DU FICHIER DE RESULTATS 3D
! CERTAINES DONNEES (NOMBRE DE POINTS,...) SONT INDISPENSABLES POUR
! CONSTRUIRE LES POINTEURS + COMPTAGE DU NOMBRE D'ENREGISTREMENTS
!
      FFORMAT = POS_FILES(POSPRE)%FMT
      FID = POS_FILES(POSPRE)%LU
      CALL OPEN_MESH(FFORMAT,POS_FILES(POSPRE)%TELNAME,FID,'READ     ',
     &               ERR)
      CALL CHECK_CALL(ERR,'LECDON_POSTEL3D:OPEN_MESH')
!
      ! Reading the title
      CALL GET_MESH_TITLE(FFORMAT,FID,TITLE,ERR)
      CALL CHECK_CALL(ERR,'LECDON_POSTEL3D:GET_MESH_TITLE')
      TITCAS = TITLE(1:72)

      ! Get the number of variables
      CALL GET_DATA_NVAR(FFORMAT,FID,NVA3,ERR)
      CALL CHECK_CALL(ERR,'LECDON_POSTEL3D:GET_DATA_NVAR')

      ! Get the Name and Unit of the variables
      ALLOCATE(VAR_NAME(NVA3),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR,'LECDON_POSTEL3D:VAR_NAME')
      ALLOCATE(VAR_UNIT(NVA3),STAT=ERR)
      CALL CHECK_ALLOCATE(ERR,'LECDON_POSTEL3D:VAR_UNIT')
      CALL GET_DATA_VAR_LIST(FFORMAT,FID,NVA3,VAR_NAME,VAR_UNIT,ERR)
      CALL CHECK_CALL(ERR,'LECDON_POSTEL3D:GET_DATA_NVAR')
      DO K=1,NVA3
        TEXTLU(K)(1:16) = VAR_NAME(K)
        TEXTLU(K)(17:32) = VAR_UNIT(K)
      ENDDO
      DEALLOCATE(VAR_NAME)
      DEALLOCATE(VAR_UNIT)

      ! Get the number of planes
      CALL GET_MESH_NPLAN(FFORMAT,FID,NPLAN,ERR)
      CALL CHECK_CALL(ERR,'LECDON_POSTEL3D:GET_MESH_NPLAN')

      ! Get the number of planes
      CALL GET_DATA_NTIMESTEP(FFORMAT,FID,NENRE,ERR)
      CALL CHECK_CALL(ERR,'LECDON_POSTEL3D:GET_DATA_TIMESTEP')

      CALL CLOSE_MESH(FFORMAT,FID,ERR)
      CALL CHECK_CALL(ERR,'LECDON_POSTEL3D:CLOSE_MESH')

!
!
!-----------------------------------------------------------------------
!
! MOTS CLES LIES AUX COUPES HORIZONTALES
!
      NC2DH = MIN(MAX(MOTINT(ADRESS(1,1)),0),9)
!
      IF(NC2DH.GE.1) THEN
        DO K=1,NC2DH
          NPLREF(K) = K-1
          IF (K.LE.DIMENS(1,5)) NPLREF(K) = MOTINT(ADRESS(1,5)+K-1)
!th un controle que l'on peut pour l'instant enlever
!th (on ne connait pas nplan actuellement
!th          NPLREF(K) = MIN(MAX(NPLREF(K),0),NPLAN)
          HREF(K) = 0.D0
          IF (K.LE.DIMENS(2,1)) HREF(K) = MOTREA(ADRESS(2,1)+K-1)
        ENDDO
      ENDIF
!
! MOTS CLES LIES AUX COUPES VERTICALES
!
      NC2DV = MIN(MAX(MOTINT(ADRESS(1,2)),0),9)
!
      IM = MOTINT(ADRESS(1,6))
      JM = NPLAN
!
      IF(NC2DV.GE.1) THEN
        DO K=1,NC2DV
          NSEG(K) = MIN(DIMENS(2,2*K),DIMENS(2,2*K+1)) - 1
          IF (NSEG(K).LT.1) THEN
            IF (LNG.EQ.1) WRITE(LU,91) K
            IF (LNG.EQ.2) WRITE(LU,92) K
            CALL PLANTE(0)
          ENDIF
          DO J=0,NSEG(K)
            X2DV(J+1,K) = MOTREA(ADRESS(2,2*K  )+J)
            Y2DV(J+1,K) = MOTREA(ADRESS(2,2*K+1)+J)
          ENDDO
          DISTOR(K) = 1.D0
          IF (K.LE.DIMENS(2,20)) DISTOR(K) = MOTREA(ADRESS(2,20)+K-1)
          IM = MAX(IM,NSEG(K)+1)
        ENDDO !K
      ENDIF
!
! ARRET EN CAS DE DEMANDE DE COUPES NULLE
!
      IF (NC2DH+NC2DV.EQ.0) THEN
        IF (LNG.EQ.1) WRITE(LU,101)
        IF (LNG.EQ.2) WRITE(LU,102)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
21    FORMAT(/,19X,'********************************************',/,
     &         19X,'*         LECTURE DES PARAMETRES           *',/,
     &         19X,'*           APPEL DE DAMOCLES              *',/,
     &         19X,'*     VERIFICATION DES DONNEES LUES        *',/,
     &         19X,'*           SUR LE FICHIER CAS             *',/,
     &         19X,'********************************************',/)
22    FORMAT(/,19X,'********************************************',/,
     &         19X,'*       READING OF THE PARAMETERS          *',/,
     &         19X,'*           CALLING DAMOCLES               *',/,
     &         19X,'*          CHECKING READ DATA              *',/,
     &         19X,'*         ON THE STEERING FILE             *',/,
     &         19X,'********************************************',/)
!
!-----------------------------------------------------------------------
!
91    FORMAT('LA COUPE VERTICALE',I2,' EST MAL DEFINIE :',/,
     &       'IL FAUT AU MOINS 2 ABSCISSES ET 2 ORDONNEES')
92    FORMAT('VERTICAL CROSS SECTION',I2,' IS NOT WELL DEFINED :',/,
     &       'YOU NEED AT LEAST 2 ABSCISSAE AND 2 ORDONATES')
!
101   FORMAT('VOUS N''AVEZ DEMANDE AUCUNE COUPE HORIZONTALE',/,
     &       'NI AUCUNE COUPE VERTICALE, POSTEL3D N''A RIEN A FAIRE')
102   FORMAT('YOU HAVE ASKED NO HORIZONTAL CROSS SECTION AND',/,
     &       'NO VERTICAL CROSS SECTION, POSTEL3D HAS NOTHING TO DO')
!
!111   FORMAT(' NOMBRE D''ENREGISTREMENTS    : ',I8,///,
!     &       ' MAILLAGE 2D',/,
!     &       ' -----------',//,
!     &       ' NOMBRE DE POINTS 2D         : ',I8,/,
!     &       ' NOMBRE D''ELEMENTS 2D        : ',I8,/,
!     &       ' NOMBRE DE POINTS DE BORD 2D : ',I8,///,
!     &       ' MAILLAGE 3D',/,
!     &       ' -----------',//,
!     &       ' NOMBRE DE POINTS 3D         : ',I8,/,
!     &       ' NOMBRE D''ELEMENTS 3D        : ',I8,/,
!     &       ' NOMBRE DE PLANS             : ',I8,//)
!112   FORMAT(' NUMBER OF RECORDS           : ',I8,///,
!     &       ' 2D MESH',/,
!     &       ' -------',//,
!     &       ' NUMBER OF 2D NODES          : ',I8,/,
!     &       ' NUMBER OF 2D ELEMENTS       : ',I8,/,
!     &       ' NUMBER OF 2D BOUNDARY NODES : ',I8,///,
!     &       ' 3D MESH',/,
!     &       ' -------',//,
!     &       ' NUMBER OF 3D NODES          : ',I8,/,
!     &       ' NUMBER OF 3D ELEMENTS       : ',I8,/,
!     &       ' NUMBER OF LEVELS            : ',I8,//)
!
!
      RETURN
      END SUBROUTINE
