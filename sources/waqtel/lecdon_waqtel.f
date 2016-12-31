!                    ************************
                     SUBROUTINE LECDON_WAQTEL
!                    ************************
!
     & (FILE_DESC,PATH,NCAR,CODE)
!
!***********************************************************************
! WAQTEL   V7P2
!***********************************************************************
!
!brief    READS THE STEERING FILE THROUGH A DAMOCLES CALL.
!
!history  RIADH ATA (EDF R&D LNHE)
!+        07/21/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CODE           |-->| NAME OF CALLING PROGRAMME
!| FILE_DESC      |-->| STORES THE FILES 'SUBMIT' ATTRIBUTES
!|                |   | IN DICTIONARIES. IT IS FILLED BY DAMOCLES.
!| NCAR           |-->| LENGTH OF PATH
!| PATH           |-->| NAME OF CURRENT DIRECTORY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_WAQTEL
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
! ARGUMENTS
      CHARACTER(LEN=24), INTENT(IN)     :: CODE
      CHARACTER(LEN=144), INTENT(INOUT) :: FILE_DESC(4,MAXKEYWORD)
      INTEGER, INTENT(IN)               :: NCAR
      CHARACTER(LEN=250), INTENT(IN)    :: PATH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!-----------------------------------------------------------------------
!
      CHARACTER(LEN=8) ::      MNEMO(MAXWQVAR)
      INTEGER          K,I
!
      CHARACTER(LEN=250) :: NOM_CAS
      CHARACTER(LEN=250) :: NOM_DIC
!-----------------------------------------------------------------------
!
! ARRAYS USED IN THE DAMOCLES CALL
!
      INTEGER            ADRESS(4,MAXKEYWORD),DIMEN(4,MAXKEYWORD)
      DOUBLE PRECISION   MOTREA(MAXKEYWORD)
      INTEGER            MOTINT(MAXKEYWORD)
      LOGICAL            MOTLOG(MAXKEYWORD)
      CHARACTER(LEN=144) MOTCAR(MAXKEYWORD)
      CHARACTER(LEN=72)  MOTCLE(4,MAXKEYWORD,2)
      INTEGER            TROUVE(4,MAXKEYWORD)
      LOGICAL            DOC
      INTEGER :: ID_DICO, ID_CAS
!
! END OF DECLARATIONS FOR DAMOCLES CALL
!
!***********************************************************************
!
      IF (LNG.EQ.1) WRITE(LU,1)
      IF (LNG.EQ.2) WRITE(LU,2)
1     FORMAT(1X,/,19X, '********************************************',/,
     &            19X, '*     SOUS-PROGRAMME LECDON_WAQTEL         *',/,
     &            19X, '*           APPEL DE DAMOCLES              *',/,
     &            19X, '*     VERIFICATION DES DONNEES LUES        *',/,
     &            19X, '*           SUR LE FICHIER CAS             *',/,
     &            19X, '********************************************',/)
2     FORMAT(1X,/,19X, '********************************************',/,
     &            19X, '*        SUBROUTINE LECDON_WAQTEL          *',/,
     &            19X, '*           CALL OF DAMOCLES               *',/,
     &            19X, '*        VERIFICATION OF READ DATA         *',/,
     &            19X, '*            ON STEERING FILE              *',/,
     &            19X, '********************************************',/)
!
!-----------------------------------------------------------------------
!
! INITIALISES THE VARIABLES FOR DAMOCLES CALL :
!
      DO K=1,MAXKEYWORD
!       A FILENAME NOT GIVEN BY DAMOCLES WILL BE RECOGNIZED AS A WHITE SPACE
!       (IT MAY BE THAT NOT ALL COMPILERS WILL INITIALISE LIKE THAT)
        MOTCAR(K)(1:1)=' '
!
        DIMEN(1,K) = 0
        DIMEN(2,K) = 0
        DIMEN(3,K) = 0
        DIMEN(4,K) = 0
      ENDDO
!
!     WRITES OUT INFO
      DOC = .FALSE.
!
!-----------------------------------------------------------------------
!     OPENS DICTIONNARY AND STEERING FILES
!-----------------------------------------------------------------------
!
      IF(NCAR.GT.0) THEN
!
        NOM_DIC=PATH(1:NCAR)//'WAQDICO'
        NOM_CAS=PATH(1:NCAR)//'WAQCAS'
!
      ELSE
!
        NOM_DIC='WAQDICO'
        NOM_CAS='WAQCAS'
!
      ENDIF
!
      CALL GET_FREE_ID(ID_DICO)
      OPEN(ID_DICO,FILE=NOM_DIC,FORM='FORMATTED',ACTION='READ')
      CALL GET_FREE_ID(ID_CAS)
      OPEN(ID_CAS,FILE=NOM_CAS,FORM='FORMATTED',ACTION='READ')
!
      CALL DAMOCLE
     &( ADRESS, DIMEN , MAXKEYWORD  , DOC    , LNG   , LU    , MOTINT,
     &  MOTREA, MOTLOG, MOTCAR, MOTCLE , TROUVE, ID_DICO, ID_CAS,
     &  .FALSE.,FILE_DESC)
!-----------------------------------------------------------------------
!     CLOSES DICTIONNARY AND STEERING FILES
!-----------------------------------------------------------------------
!
      CLOSE(ID_DICO)
      CLOSE(ID_CAS)
!
!     DECODES 'SUBMIT' CHAINS
!
      CALL READ_SUBMIT(WAQ_FILES,MAXLU_WAQ,CODE,FILE_DESC,300)
!
!-----------------------------------------------------------------------
!
!     RETRIEVES FILE NUMBERS FROM WAQTEL FORTRAN PARAMETERS
!     AT THIS LEVEL LOGICAL UNITS ARE EQUAL TO THE FILE NUMBER
!
      DO I=1,MAXLU_WAQ
        IF    (WAQ_FILES(I)%TELNAME.EQ.'WAQGEO') THEN
          WAQGEO=I
        ELSEIF(WAQ_FILES(I)%TELNAME.EQ.'WAQCLI') THEN
          WAQCLI=I
        ELSEIF(WAQ_FILES(I)%TELNAME.EQ.'WAQHYD') THEN
          WAQHYD=I
        ELSEIF(WAQ_FILES(I)%TELNAME.EQ.'WAQREF') THEN
          WAQREF=I
        ELSEIF(WAQ_FILES(I)%TELNAME.EQ.'WAQRES') THEN
          WAQRES=I
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
!     ASSIGNS THE STEERING FILE VALUES TO THE PARAMETER FORTRAN NAME
!
!-----------------------------------------------------------------------
!*******************************
!     INTEGER KEYWORDS         *
!*******************************
!
!     PRINTOUT WAQ PERIOD
      LEOPRD    = MOTINT( ADRESS(1,  1) )
!     WAQTEL PROCESS
!      WAQPROCESS = MOTINT( ADRESS(1,  2) )
!     K2 FORMULA
      FORMK2    = MOTINT( ADRESS(1,  3) )
!     RS FORMULA
      FORMRS    = MOTINT( ADRESS(1,  4) )
!     CS FORMULA
      FORMCS    = MOTINT( ADRESS(1,  5) )
!     MODEL OF EXCHANGE WITH ATMOSPHERE
      ATMOSEXCH = MOTINT( ADRESS(1,  9) )
!     BRIGHTNESS OF THE SKY
      ISKYTYPE  = MOTINT( ADRESS(1,  10) )
!     DEBUG KEYWORD
      DEBUG     = MOTINT( ADRESS(1,  11) )
!     RAY EXTINCTION METHOD
      MEXTINC   = MOTINT( ADRESS(1,  12) )
!     FORMULA OF ATMOSPHERIC RADIATION (GLM)
      IRAY_ATM  = MOTINT( ADRESS(1,  13) )
!
!*******************************
!     REAL KEYWORDS            *
!*******************************
!
      RO0    = MOTREA( ADRESS(2,  2) )
!      GRAV   = MOTREA( ADRESS(2,  5) )
      VCE    = MOTREA( ADRESS(2,  8) )
      LDISP  = MOTREA( ADRESS(2, 11) )
      TDISP  = MOTREA( ADRESS(2, 12) )
      K120   = MOTREA( ADRESS(2, 21) )
      K520   = MOTREA( ADRESS(2, 22) )
      O2PHOTO= MOTREA( ADRESS(2, 25) )
      O2NITRI= MOTREA( ADRESS(2, 26) )
      DEMBEN = MOTREA( ADRESS(2, 29) )
      K22    = MOTREA( ADRESS(2, 31) )
      RSW    = MOTREA( ADRESS(2, 32) )
      O2SATU = MOTREA( ADRESS(2, 33) )
      ABRS(1)= MOTREA( ADRESS(2, 34) )
      ABRS(2)= MOTREA( ADRESS(2, 34)+1)
      WPOR   = MOTREA( ADRESS(2, 36) )
      WNOR   = MOTREA( ADRESS(2, 39) )
      CMAX   = MOTREA( ADRESS(2, 42) )
      ZSD    = MOTREA( ADRESS(2, 45) )
!
      KPE    = MOTREA( ADRESS(2, 51) )
      IK     = MOTREA( ADRESS(2, 54) )
      KP     = MOTREA( ADRESS(2, 57) )
      KN     = MOTREA( ADRESS(2, 60) )
      CTOXIC = MOTREA( ADRESS(2, 63) )
      TRESPIR= MOTREA( ADRESS(2, 66) )
      PROPHOC= MOTREA( ADRESS(2, 69) )
      DTP    = MOTREA( ADRESS(2, 71) )
!     CONVERSION OF DTP (FROM PERCENTAGE TO [])
      DTP    = DTP/100.D0
      IF(DTP.GT.1.D0)THEN
        IF(LNG.EQ.1)THEN
          WRITE(LU,*)'POURCENTAGE DE PHOSPHORE ASSIMILABLE '
          WRITE(LU,*)'DANS LE PHYTOPLANCTON MORT SUPERIEUR A 100 %'
        ELSEIF(LNG.EQ.2)THEN
          WRITE(LU,*)'PERCENTAGE OF PHYSPHORUS ASSIMILABLE '
          WRITE(LU,*)'IN DEAD PHYTOPLANKTON GREATER THAN 100 %'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
      K320   = MOTREA( ADRESS(2, 73) )
      PRONITC= MOTREA( ADRESS(2, 75) )
      PERNITS= MOTREA( ADRESS(2, 77) )
!     CONVERSION OF PERNITS (FROM PERCENTAGE TO [])
      PERNITS=PERNITS/100.D0
      IF(PERNITS.GT.1.D0)THEN
        IF(LNG.EQ.1)THEN
          WRITE(LU,*)'POURCENTAGE D''AZOTE '
          WRITE(LU,*)'ASSIMILABLE DANS LE PHYTO MORT SUPERIEUR A 100 %'
        ELSEIF(LNG.EQ.2)THEN
          WRITE(LU,*)'PERCENTAGE OF ASSIMILABLE NITROGEN '
          WRITE(LU,*)'IN DEAD PHYTOPLANKTON GREATER THAN 100 %'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
      K360   = MOTREA( ADRESS(2, 79) )
      CMORALG= MOTREA( ADRESS(2, 81) )
      WLOR   = MOTREA( ADRESS(2, 85) )
      K1     = MOTREA( ADRESS(2, 90) )
      K44    = MOTREA( ADRESS(2, 93) )
      PHOTO  = MOTREA( ADRESS(2, 95) )
      RESP   = MOTREA( ADRESS(2, 97) )
      WATTEMP= MOTREA( ADRESS(2, 99) )
      ERO    = MOTREA( ADRESS(2,104) )
      TAUR   = MOTREA( ADRESS(2,106) )
      TAUS   = MOTREA( ADRESS(2,109) )
      VITCHU = MOTREA( ADRESS(2,111) )
      CCSEDIM= MOTREA( ADRESS(2,113) )
      CDISTRIB=MOTREA( ADRESS(2,115) )
      KDESORP= MOTREA( ADRESS(2,117) )
      CP_EAU = MOTREA( ADRESS(2,119) )
      CP_AIR = MOTREA( ADRESS(2,121) )
      CFAER  = MOTREA( ADRESS(2,125) )
      COEF_K = MOTREA( ADRESS(2,127) )
      EMA    = MOTREA( ADRESS(2,129) )
      EMI_EAU= MOTREA( ADRESS(2,131) )
      I0     = MOTREA( ADRESS(2,133) )
      C_ATMOS= MOTREA( ADRESS(2,135) )
      EVAPORATION= MOTREA( ADRESS(2,137) )
!
!*******************************
!     LOGICAL KEYWORDS         *
!*******************************
!
      WQBILMAS= MOTLOG( ADRESS(3,  1) )
      WQVALID = MOTLOG( ADRESS(3,  3) )
!
!*******************************
!     STRING KEYWORDS          *
!*******************************
!
      TITWAQCAS = MOTCAR( ADRESS(4, 2) ) (1:72)
!
! FILES IN THE STEERING FILE
!
      WAQ_FILES(WAQRES)%NAME=MOTCAR( ADRESS(4,6 ) )
      WAQ_FILES(WAQRES)%FMT=MOTCAR( ADRESS(4,60) )(1:8)
      WAQ_FILES(WAQGEO)%NAME=MOTCAR( ADRESS(4,8 ) )
      WAQ_FILES(WAQGEO)%FMT=MOTCAR( ADRESS(4,61) )(1:8)
      WAQ_FILES(WAQREF)%NAME=MOTCAR( ADRESS(4,12) )
      WAQ_FILES(WAQREF)%FMT=MOTCAR( ADRESS(4,63) )(1:8)
      WAQ_FILES(WAQHYD)%NAME=MOTCAR( ADRESS(4,14) )
      WAQ_FILES(WAQHYD)%FMT=MOTCAR( ADRESS(4,62) )(1:8)
      WAQ_FILES(WAQCLI)%NAME=MOTCAR( ADRESS(4,10 ) )
!
!
!-----------------------------------------------------------------------
!  NAME OF THE VARIABLES FOR THE RESULTS AND GEOMETRY FILES:
!-----------------------------------------------------------------------
!
! LOGICAL ARRAY FOR OUTPUT
!
!      CALL NOMVAR_WAQTEL(TEXTE,TEXTPR,MNEMO,MAXWQVAR)
!
!    ARRAY OF LOGICALS FOR OUTPUTS
!
!      CALL SORTIE(SORT2D , MNEMO , MAXWQVAR , SORLEO )
!
      DO K=1,MAXWQVAR
        SORIMP(K)=.FALSE.
      ENDDO
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
