!                    *************************
                     SUBROUTINE LECDON_SISYPHE
!                    *************************
!
     &(MOTCAR,FILE_DESC,PATH,NCAR,CODE)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    READS THE STEERING FILE BY CALL TO DAMOCLES.
!
!history  M. GONZALES DE LINARES
!+        2002
!+
!+
!
!history  C.VILLARET
!+        **/10/2003
!+
!+   * READS KFROT, HOULE
!
!history  F. HUVELIN
!+        **/12/2003
!+
!+   * INITIALISES F90 TO FDM IF NOT IN THE STEERING FILE
!
!history  CV
!+        **/03/06
!+
!+   ADDED NEW KEYWROD: TASS
!
!history  JMH
!+        11/04/2008
!+
!+   DEBUG IS A KEYWORD: DEBUGGER
!
!history  CV+JMH
!+        29/07/2008
!+
!+   READS CBOR_CLASSE
!
!history  JMH
!+        17/10/2008
!+
!+   CHECKS NCSIZE (FOR CONSISTENCY WITH TELEMAC-2D WHEN COUPLING)
!
!history  JMH
!+        23/12/2008
!+
!+   KEYWORDS FOR COUPLING WITH DREDGESIM
!
!history  BD+JMH
!+        09/04/2009
!+
!+   MED FORMAT
!
!history   J.-M. HERVOUET; C.VILLARET
!+        03/11/2009
!+        V6P0
!+
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
!history  C.VILLARET; U. MERKEL, R. KOPMAN
!+        20/03/2011
!+        V6P1
!+
!
!history  C.VILLARET (EDF-LNHE), P.TASSI (EDF-LNHE)
!+        19/07/2011
!+        V6P1
!+  Name of variables   
!+   
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FILE_DESC      |<--| STORES STRINGS 'SUBMIT' OF DICTIONARY
!| MOTCAR         |<--| VALUES OF KEY-WORDS OF TYPE CHARACTER
!| NCAR           |-->| NUMBER OF LETTERS IN STRING PATH
!| PATH           |-->| FULL PATH TO CODE DICTIONARY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!

      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)               :: NCAR
      CHARACTER(LEN=24), INTENT(IN)     :: CODE
      CHARACTER(LEN=250), INTENT(IN)    :: PATH
C                                                 NMAX
      CHARACTER*144, INTENT(INOUT)      :: MOTCAR(300)
C                                                      NMAX
      CHARACTER(LEN=144), INTENT(INOUT) :: FILE_DESC(4,300)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, PARAMETER :: NMAX = 300
C
      INTEGER            :: I,K,ERR
      INTEGER            :: MOTINT(NMAX)
      INTEGER            :: TROUVE(4,NMAX)
      INTEGER            :: ADRESS(4,NMAX)
      INTEGER            :: DIMENS(4,NMAX)
      DOUBLE PRECISION   :: SUMAVAI
      DOUBLE PRECISION   :: MOTREA(NMAX)
      LOGICAL            :: DOC,EFFPEN
      LOGICAL            :: MOTLOG(NMAX)
      CHARACTER(LEN=250) :: NOM_CAS
      CHARACTER(LEN=250) :: NOM_DIC
      CHARACTER*72       :: MOTCLE(4,NMAX,2)
C
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
C
      SUMAVAI = 0

C INITIALISES THE VARIABLES FOR DAMOCLES CALL :
C
      DO K = 1, NMAX
C       A FILENAME NOT GIVEN BY DAMOCLES WILL BE RECOGNIZED AS A WHITE SPACE
C       (IT MAY BE THAT NOT ALL COMPILERS WILL INITIALISE LIKE THAT)
        MOTCAR(K)(1:1)=' '
C
        DIMENS(1,K) = 0
        DIMENS(2,K) = 0
        DIMENS(3,K) = 0
        DIMENS(4,K) = 0
      ENDDO
C
C     WRITES OUT INFO
      DOC = .FALSE.
C
C-----------------------------------------------------------------------
C     OPENS DICTIONNARY AND STEERING FILES
C-----------------------------------------------------------------------
C
      IF(NCAR.GT.0) THEN
C
        NOM_DIC=PATH(1:NCAR)//'SISDICO'
        NOM_CAS=PATH(1:NCAR)//'SISCAS'
C
      ELSE
C
        NOM_DIC='SISDICO'
        NOM_CAS='SISCAS'
C
      ENDIF
C
      OPEN(2,FILE=NOM_DIC,FORM='FORMATTED',ACTION='READ')
      OPEN(3,FILE=NOM_CAS,FORM='FORMATTED',ACTION='READ')
C
C-----------------------------------------------------------------------
C     CALLS DAMOCLES
C-----------------------------------------------------------------------
C
      CALL DAMOCLE( ADRESS , DIMENS  , NMAX   , DOC    , LNG , LU  ,
     &               MOTINT , MOTREA  , MOTLOG , MOTCAR ,
     &               MOTCLE , TROUVE , 2 , 3 ,.FALSE., FILE_DESC )
C
C     DECODES 'SUBMIT' CHAINS
C
      CALL READ_SUBMIT(SIS_FILES,MAXLU_SIS,CODE,FILE_DESC,300)
C
C-----------------------------------------------------------------------
C
C     RETRIEVES FILES NUMBERS IN TELEMAC-2D FORTRAN PARAMETERS
C     AT THIS LEVEL LOGICAL UNITS ARE EQUAL TO THE FILE NUMBER
C
      DO I=1,MAXLU_SIS
        IF(SIS_FILES(I)%TELNAME.EQ.'SISHYD') THEN
          SISHYD=I
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SISGEO') THEN
          SISGEO=I
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SISCLI') THEN
          SISCLI=I
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SISPRE') THEN
          SISPRE=I
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SISRES') THEN
          SISRES=I
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SISREF') THEN
          SISREF=I
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SISCOU') THEN
          SISCOU=I
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SISFON') THEN
          SISFON=I
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SISMAF') THEN
          SISMAF=I
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SISSEC') THEN
          SISSEC=I
        ELSEIF(SIS_FILES(I)%TELNAME.EQ.'SISSEO') THEN
          SISSEO=I
        ENDIF
      ENDDO
C
C-----------------------------------------------------------------------
C
C   ASSIGNS THE STEERING FILE VALUES TO THE PARAMETER FORTRAN NAME
C
C-----------------------------------------------------------------------
C
C ################# !
C INTEGER KEYWORDS  !
C ################# !
C OPTION OF MATRIX ASSEMBLY IS HARD-CODED
C ---------------------------------------------
C
      OPTASS = 1
      PRODUC = 1

      ! DISCRETISES THE VARIABLES
      ! ----------------------------
      IELMT     = 11 ! SEDIMENTOLOGICAL VARIABLES
      IELMH_SIS = 11 ! VARIABLES ASSOCIATED WITH WATER DEPTH
      IELMU_SIS = 11 ! VARIABLES ASSOCIATED WITH VELOCITIES

      ! FOR NOW PRINTOUTS START AT ZERO
      ! -----------------------------------------------
      PTINIG = 0
      PTINIL = 0

      ! NON-EQUILIBIRUM BEDLOAD
      ! ------------------------
      LOADMETH = 0

C     ICM           = MOTINT( ADRESS(1,  1) )
      ICF           = MOTINT( ADRESS(1,  2) )
      NPAS          = MOTINT( ADRESS(1,  3) )
      NMAREE        = MOTINT( ADRESS(1,  4) )
C     N1            = MOTINT( ADRESS(1,  5) )      NCOUCH_TASS = MOTINT( ADRESS(1,45)   )

      LEOPR         = MOTINT( ADRESS(1,  6) )
      LISPR         = MOTINT( ADRESS(1,  7) )
C     STDGEO IS NOT USED, DELETE FROM DECLARATIONS
      STDGEO        = MOTINT( ADRESS(1,  8) )
C     LOGDES IS NOT USED, DELETE FROM DECLARATIONS
      LOGDES        = MOTINT( ADRESS(1,  9) )
C     LOGPRE IS NOT USED, DELETE FROM DECLARATIONS
      LOGPRE        = MOTINT( ADRESS(1, 10) )
      OPTBAN        = MOTINT( ADRESS(1, 11) )
      LVMAC         = MOTINT( ADRESS(1, 12) )
      HYDRO         = MOTINT( ADRESS(1, 13) )
      NSOUS         = MOTINT( ADRESS(1, 14) )
!
      MARDAT(1)     = MOTINT( ADRESS(1, 15) )
      MARDAT(2)     = MOTINT( ADRESS(1, 15) + 1 )
      MARDAT(3)     = MOTINT( ADRESS(1, 15) + 2 )
      MARTIM(1)     = MOTINT( ADRESS(1, 16) )
      MARTIM(2)     = MOTINT( ADRESS(1, 16) + 1 )
      MARTIM(3)     = MOTINT( ADRESS(1, 16) + 2 )
!
      SLVSED%SLV    = MOTINT( ADRESS(1, 17) )
      SLVSED%KRYLOV = MOTINT( ADRESS(1, 18) )
      SLVSED%PRECON = MOTINT( ADRESS(1, 19) )
      SLVSED%NITMAX = MOTINT( ADRESS(1, 20) )
      CHOIX         = MOTINT( ADRESS(1, 21) )
      DIRFLU        = MOTINT( ADRESS(1, 22) )
      NPRIV         = MOTINT( ADRESS(1, 23) )
C
C     NCSIZE        = MOTINT( ADRESS(1, 24) )
C     NUMBER OF PROCESSORS (ALREADY GIVEN IN INIT_FILES2;
C     MUST BE THE SAME, BUT WHEN USING COUPLED MODELS IT CAN
C     WRONGLY BE DIFFERENT)
      IF(NCSIZE.NE.MOTINT(ADRESS(1,24))) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'NOMBRE DE PROCESSEURS PARALLELES DIFFERENT :'
          WRITE(LU,*) 'DEJA DECLARE (CAS DE COUPLAGE ?) :',NCSIZE
          WRITE(LU,*) 'SISYPHE :',MOTINT(ADRESS(1,24))
          WRITE(LU,*) 'LA VALEUR ',NCSIZE,' EST GARDEE'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'DIFFERENT NUMBER OF PARALLEL PROCESSORS:'
          WRITE(LU,*) 'DECLARED BEFORE (CASE OF COUPLING ?):',NCSIZE
          WRITE(LU,*) 'SISYPHE :',MOTINT(ADRESS(1,24))
          WRITE(LU,*) 'VALUE ',NCSIZE,' IS KEPT'
        ENDIF
      ENDIF
      RESOL         = MOTINT( ADRESS(1, 25) )
      SLVTRA%SLV    = MOTINT( ADRESS(1, 26) )
      SLVTRA%KRYLOV = MOTINT( ADRESS(1, 27) )
      SLVTRA%PRECON = MOTINT( ADRESS(1, 28) )
      SLVTRA%NITMAX = MOTINT( ADRESS(1, 29) )
      OPTDIF        = MOTINT( ADRESS(1, 31) )
      OPTSUP        = MOTINT( ADRESS(1, 32) )
      PRODUC        = MOTINT( ADRESS(1, 33) )
      OPTASS        = MOTINT( ADRESS(1, 34) )
      OPDTRA        = MOTINT( ADRESS(1, 35) )
      DEPER         = MOTINT( ADRESS(1, 36) )
      KFROT         = MOTINT( ADRESS(1, 37) )
      NCONDIS       = MOTINT( ADRESS(1, 38) )
      SLOPEFF       = MOTINT( ADRESS(1, 39) )
      DEVIA         = MOTINT( ADRESS(1, 40) )
      NOMBLAY       = MOTINT( ADRESS(1,251) )
      NSICLA        = MOTINT( ADRESS(1,252) )
      HIDFAC        = MOTINT( ADRESS(1,253) )
      ICQ           = MOTINT( ADRESS(1, 41) )
C     CONTROL SECTIONS
      NCP=DIMENS(1,42)
      ALLOCATE(CTRLSC(NCP),STAT=ERR)
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) WRITE(LU,1039) ERR
        IF(LNG.EQ.2) WRITE(LU,2039) ERR
1039    FORMAT(1X,'LECDON_SISYPHE :',/,1X,
     &            'ERREUR A L''ALLOCATION DE CTRLSC : ',/,1X,
     &            'CODE D''ERREUR : ',1I6)
2039    FORMAT(1X,'LECDON_SISYPHE:',/,1X,
     &            'ERROR DURING ALLOCATION OF CTRLSC: ',/,1X,
     &            'ERROR CODE: ',1I6)
      ENDIF
      IF(NCP.GE.1) THEN
        DO K=1,NCP
          CTRLSC(K) = MOTINT( ADRESS(1,42) + K-1 )
        ENDDO
      ENDIF
C     COORDINATES OF THE ORIGIN
      I_ORIG = MOTINT( ADRESS(1,43)   )
      J_ORIG = MOTINT( ADRESS(1,43)+1 )
      DEBUG  = MOTINT( ADRESS(1,44)   )
C 
      ICR  =   MOTINT(ADRESS(1,46)   )
C 
      IKS  =   MOTINT(ADRESS(1,47)   )


!
! ############### !
C REAL KEYWORDS   !
! ############### !
!
      ! NON-EQUILIBIRUM BEDLOAD
      ! ------------------------
      LS0         = 1.D0
!
      RC          = MOTREA( ADRESS(2,  1) )
      XMVE        = MOTREA( ADRESS(2,  2) )
      XMVS        = MOTREA( ADRESS(2,  3) )
      DO K=1,NSICLA
        FDM(K)   = MOTREA( ADRESS(2,  4) + K-1 )
      ENDDO
C     IF OLD NAME OF KEYWORD 28 HAS BEEN FOUND
      IF(TROUVE(2,28).EQ.2) THEN
        DO K=1,NSICLA
          FDM(K) = MOTREA( ADRESS(2,28) + K-1 )
        ENDDO
      ENDIF
      XKV         = MOTREA( ADRESS(2,  5) )
CV
CV      AC          = MOTREA( ADRESS(2,  6) )
      DO K=1,MAX(DIMENS(2,6),NSICLA)
         AC(K)   = MOTREA( ADRESS(2, 6) + K-1 )
      ENDDO
      IF(DIMENS(2,6).LT.NSICLA) THEN
        DO K=DIMENS(2,6)+1,NSICLA
          AC(K) = MOTREA( ADRESS(2, 6)+DIMENS(2,6)-1 )
        ENDDO
      ENDIF
CV
      SFON        = MOTREA( ADRESS(2,  7) )
      GRAV        = MOTREA( ADRESS(2,  8) )
      ZERO        = MOTREA( ADRESS(2,  9) )
      SLVSED%ZERO = ZERO
      VCE         = MOTREA( ADRESS(2, 10) )
      HMIN        = MOTREA( ADRESS(2, 11) )
      DELT        = MOTREA( ADRESS(2, 12) )
      TPREC       = MOTREA( ADRESS(2, 13) )
      PMAREE      = MOTREA( ADRESS(2, 14) )
      TETA        = MOTREA( ADRESS(2, 15) )
      BETA        = MOTREA( ADRESS(2, 16) )
      SLVSED%EPS  = MOTREA( ADRESS(2, 17) )
      TETA_SUSP   = MOTREA( ADRESS(2, 18) )
      XKX         = MOTREA( ADRESS(2, 19) )
      XKY         = MOTREA( ADRESS(2, 20) )
      SLVTRA%EPS  = MOTREA( ADRESS(2, 21) )
      DO K=1,NSICLA
         XWC(K)   = MOTREA( ADRESS(2, 22) + K-1 )
      ENDDO
CV
      IF(DIMENS(2,22).LT.NSICLA) THEN
        DO K=DIMENS(2,22)+1,NSICLA
          XWC(K) = MOTREA( ADRESS(2, 22)+DIMENS(2,22)-1 )
        ENDDO
      ENDIF

CV
      CRIT_CFD    = MOTREA( ADRESS(2, 23) )
      KSPRATIO    = MOTREA( ADRESS(2, 24) )
      PHISED      = MOTREA( ADRESS(2, 25) )
      BETA2       = MOTREA( ADRESS(2, 26) )
      BIJK        = MOTREA( ADRESS(2, 27) )
C
C     INITIAL CONCENTRATIONS
C++++++++++++++++++++++++++++++++     
      DO K=1,NSICLA
        CS0(K)=MOTREA( ADRESS(2,30) + K-1 )
      ENDDO
      DO K=1,10*MAXFRO
        CBOR_CLASSE(K)=0.D0
      ENDDO
      IF(DIMENS(2,31).GT.0) THEN
        DO K=1,DIMENS(2,31)
          CBOR_CLASSE(K)=MOTREA( ADRESS(2,31) + K-1 )
        ENDDO
      ENDIF
C 
C       COHESIVE SEDIMENT 
C +++++++++++++++++++++++++++++++
C 
      NCOUCH_TASS = MOTINT( ADRESS(1,45)   )

C
      IF(DIMENS(2,32).GT.0) THEN
        DO K=1,DIMENS(2,32)
          CONC_VASE(K)=MOTREA( ADRESS(2,32) + K-1 )
        ENDDO
      ENDIF
C
C OBSOLETE KEY WORD
C      CSF_VASE    = MOTREA( ADRESS(2, 29) )

        CSF_VASE = CONC_VASE(1)/XMVS
C 
      IF(DIMENS(2,34).GT.0) THEN
        DO K=1,DIMENS(2,34)
          TOCE_VASE(K)=MOTREA( ADRESS(2,34) + K-1 )
        ENDDO
      ENDIF
C
C      KRONE AND PARTHENIADES EROSION AND DEPOSITION LAW
C
C OBSOLETE KEY WORD
C      VITCE= MOTREA( ADRESS(2,35))
C     
      VITCE = SQRT(TOCE_VASE(1)/XMVE)
      VITCD= MOTREA( ADRESS(2,36))
      PARTHENIADES = MOTREA( ADRESS(2,37))
C
C CONVERTED TO  M/S
C
       PARTHENIADES = PARTHENIADES/XMVS
C
C CONSOLIDATION MODEL
C
      TASS = MOTLOG(ADRESS(3,23))
      ITASS  =   MOTINT(ADRESS(1,48)   )
!
! MULTILAYER MODEL (WALTHER, 2008)
! ITASS = 1
      IF(DIMENS(2,33).GT.0) THEN
        DO K=1,DIMENS(2,33)
          TRANS_MASS(K)=MOTREA( ADRESS(2,33) + K-1 )
        ENDDO
      ENDIF
C
C V6P1
C THIEBOT MULTI LAYER MODEL 
C ITASS=2
      CONC_GEL=MOTREA( ADRESS(2,38))
      COEF_N= MOTREA( ADRESS(2,39))
C
C       HIDING EXPOSURE MULTI GRAIN MODEL
C +++++++++++++++++++++++++++++++++++++++++++++++++++++++
      DO K=1,NSICLA
         HIDI(K)  = MOTREA( ADRESS(2,253) + K-1 )
         IF (TROUVE(2,255).EQ.1) THEN
           FD90(K)= FDM(K)
         ELSE
           FD90(K)= MOTREA( ADRESS(2,255) + K-1 )
         ENDIF
         AVA0(K)  = MOTREA( ADRESS(2,258) + K-1 )
      ENDDO
      ELAY0       = MOTREA( ADRESS(2,259) )
C
C UM: MPM-Factor
      MPM         = MOTREA( ADRESS(2,260) )
C UM: ALPHA-Factor
      ALPHA       = MOTREA( ADRESS(2,261) )	  
C UM: MOFAC-Factor
      MOFAC       = MOTREA( ADRESS(2,262) )	  

      ! ################## !
      ! LOGICAL KEYWORDS !
      ! ################## !
C INDEX 99 IS ALREADY USED FOR KEYWORD 'LIST OF FILES'
C INDEX 54 IS ALREADY USED FOR KEYWORD 'DESCRIPTION OF LIBRARIES'
C INDEX 57 IS ALREADY USED FOR KEYWORD 'DEFAULT EXECUTABLE'
      ! SPHERICAL EQUATIONS HARD-CODED
      ! ----------------------------------
      SPHERI       = .FALSE.


      ! COMPUTATION OF FALL VELOCITIES
      ! ------------------------------------------
      CALWC = .FALSE.
      ! IF TROUVE: VELOCITIES ARE USER-DEFINED
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      IF (TROUVE(2, 22).EQ.2) CALWC = .TRUE.
C CV
      ! SHIELDS PARAMETER
      ! ------------------------------------------
      CALAC = .FALSE.
      ! IF TROUVE
      ! ~~~~~~~~~~~~~
      IF (TROUVE(2, 6).EQ.2) CALAC = .TRUE.


      BILMA        = MOTLOG( ADRESS(3,  1) )
      PERMA        = MOTLOG( ADRESS(3,  2) )
      BANDEC       = MOTLOG( ADRESS(3,  3) )
      VALID        = MOTLOG( ADRESS(3,  4) )
C     DTVAR        = MOTLOG( ADRESS(3,  5) )
      LUMPI        = MOTLOG( ADRESS(3,  6) )
      SUSP         = MOTLOG( ADRESS(3,  7) )
      CHARR        = MOTLOG( ADRESS(3,  8) )
      HOULE        = MOTLOG( ADRESS(3, 10) )
      CONST_ALAYER = MOTLOG( ADRESS(3, 11) )
      LCONDIS      = MOTLOG( ADRESS(3, 12) )
      LGRAFED      = MOTLOG( ADRESS(3, 13) )
C     USED TO CHECK SIS_FILES(SISPRE)%NAME
      DEBU         = MOTLOG( ADRESS(3, 14) )
      IMP_INFLOW_C = MOTLOG( ADRESS(3, 15) )
      SECCURRENT   = MOTLOG( ADRESS(3, 16) )
      IF(CODE(1:9).EQ.'TELEMAC3D') SECCURRENT = .FALSE.
      UNIT         = MOTLOG( ADRESS(3, 17) )
      VF           = MOTLOG( ADRESS(3,253) )
      CORR_CONV    = MOTLOG( ADRESS(3, 18) )
      DO K=1,NSICLA
        SEDCO(K)   = .FALSE.
      ENDDO
      IF(DIMENS(3,19).GT.0) THEN
        DO K=1,DIMENS(3,19)
          SEDCO(K) = MOTLOG( ADRESS(3,19) + K-1 )
        ENDDO
      ENDIF
      SLIDE    = MOTLOG( ADRESS(3, 20) )
      DIFT     = MOTLOG( ADRESS(3, 21) )
      EFFPEN   = MOTLOG( ADRESS(3, 22) )
      IF(.NOT.EFFPEN) THEN
        SLOPEFF=0
        DEVIA=0
      ENDIF
C 
      MIXTE=MOTLOG(ADRESS(3,24))
C COUPLING WITH DREDGESIM
      DREDGESIM=MOTLOG(ADRESS(3,25))
C V6P1
      KSPRED   =MOTLOG(ADRESS(3,26))
!
! ################################### !
C CHARACTER STRING KEYWORDS           !
! ################################### !
!
      TITCA            = MOTCAR( ADRESS(4, 1) )(1:72)
      SORTIS           = MOTCAR( ADRESS(4, 2) )(1:72)
      VARIM            = MOTCAR( ADRESS(4, 3) )(1:72)
      SIS_FILES(SISGEO)%NAME=MOTCAR( ADRESS(4,6) )
      SIS_FILES(SISCLI)%NAME=MOTCAR( ADRESS(4,9) )
      SIS_FILES(SISHYD)%NAME=MOTCAR( ADRESS(4,29) )
      SIS_FILES(SISPRE)%NAME=MOTCAR( ADRESS(4,11) )
      SIS_FILES(SISRES)%NAME=MOTCAR( ADRESS(4,12) )
      SIS_FILES(SISFON)%NAME=MOTCAR( ADRESS(4,16) )
      SIS_FILES(SISRES)%FMT = MOTCAR( ADRESS(4,31) )(1:8)
      CALL MAJUS(SIS_FILES(SISRES)%FMT)
C     RESULT FILE FORMAT FOR PREVIOUS SEDIMENTOLOGICAL
C     COMPUTATION...
      SIS_FILES(SISPRE)%FMT = MOTCAR( ADRESS(4,34) )(1:8)
      CALL MAJUS(SIS_FILES(SISPRE)%FMT)
C     REFERENCE FILE FORMAT
      SIS_FILES(SISREF)%FMT = MOTCAR( ADRESS(4,33) )(1:8)
      CALL MAJUS(SIS_FILES(22)%FMT)
C     HYDRODYNAMIC FILE FORMAT
      SIS_FILES(SISHYD)%FMT = MOTCAR( ADRESS(4,32) )(1:8)
      CALL MAJUS(SIS_FILES(SISHYD)%FMT)
C     WAVE FILE FORMAT (COUPLING WITH TOMAWAC)
      SIS_FILES(SISCOU)%FMT = MOTCAR( ADRESS(4,35) )(1:8)
      CALL MAJUS(SIS_FILES(SISCOU)%FMT)
      BINGEOSIS        = MOTCAR( ADRESS(4,18) )(1:3)
      BINHYDSIS        = MOTCAR( ADRESS(4,19) )(1:3)
      BINPRESIS        = MOTCAR( ADRESS(4,20) )(1:3)
      BINRESSIS        = MOTCAR( ADRESS(4,21) )(1:3)
      SIS_FILES(SISREF)%NAME=MOTCAR( ADRESS(4,22) )
      BINREFSIS        = MOTCAR( ADRESS(4,23) )(1:3)
C     DREDGESIM STEERING FILE
      SIS_FILES(SISMAF)%NAME = MOTCAR( ADRESS(4,27) )
C     ******           = MOTCAR( ADRESS(4,28) )
C     WAVE FILE
      SIS_FILES(SISCOU)%NAME=MOTCAR( ADRESS(4,30) )
C !JAJ ####
      SIS_FILES(SISSEC)%NAME=MOTCAR( ADRESS(4,36) )
      SIS_FILES(SISSEO)%NAME=MOTCAR( ADRESS(4,37) )
C
      IF(LNG.EQ.1) WRITE(LU,101)
      IF(LNG.EQ.2) WRITE(LU,102)
101   FORMAT(1X,/,19X, '********************************************',/,
     &            19X, '*               LECDON:                    *',/,
     &            19X, '*        APRES APPEL DE DAMOCLES           *',/,
     &            19X, '*     VERIFICATION DES DONNEES LUES        *',/,
     &            19X, '*     SUR LE FICHIER DES PARAMETRES        *',/,
     &            19X, '********************************************',/)
102   FORMAT(1X,/,19X, '********************************************',/,
     &            19X, '*               LECDON:                    *',/,
     &            19X, '*        AFTER CALLING DAMOCLES            *',/,
     &            19X, '*        CHECKING OF DATA  READ            *',/,
     &            19X, '*         IN THE STEERING FILE             *',/,
     &            19X, '********************************************',/)
C
C-----------------------------------------------------------------------
C
C LOGICALS FOR OUTPUT VARIABLES
C-----------------------------------------------------------------------
C
C     CV ... IF CANNOT FIND ANY BETTER (MOVED HERE BEFORE CALL TO NOMVAR)
      IF(TASS) NOMBLAY=NCOUCH_TASS
C
C  NPRIV MOFIFIED FOR OUTPUT OF USER-BUILT VARIABLES
CV augmentation des index
      CALL NOMVAR_SISYPHE(TEXTE,TEXTPR,MNEMO,NSICLA,UNIT)
      CALL SORTIE(SORTIS , MNEMO , MAXVAR , SORLEO )
      CALL SORTIE(VARIM  , MNEMO , MAXVAR , SORIMP )
      DO I = 1, 4
         IF ((NPRIV.LT.I).AND.
     &       (SORLEO(I+27+(NOMBLAY+4)*NSICLA+NOMBLAY).OR.
     &        SORIMP(I+27+(NOMBLAY+4)*NSICLA+NOMBLAY))) THEN
            NPRIV=MAX(NPRIV,I)
         ENDIF
      ENDDO
C
C-----------------------------------------------------------------------
C
C     CANCELS OUTPUT OF VARIABLES WHICH ARE NOT BUILT IN THIS CASE
C
      IF(.NOT.SUSP) THEN
CV 7/09/2006 MIGHT WANT TO OUTPUT THE SUSPENDED COMPONENT IN BIJKER
C        SORIMP(24+4*NSICLA)=.FALSE.
C        SORIMP(25+4*NSICLA)=.FALSE.
C        SORIMP(26+4*NSICLA)=.FALSE.
      ENDIF
CV 2010: augmentation des index +1      
      IF(.NOT.CHARR) THEN
        SORLEO(22+(NOMBLAY+2)*NSICLA)=.FALSE.
        SORLEO(23+(NOMBLAY+2)*NSICLA)=.FALSE.
        SORLEO(24+(NOMBLAY+2)*NSICLA)=.FALSE.
        SORIMP(22+(NOMBLAY+2)*NSICLA)=.FALSE.
        SORIMP(23+(NOMBLAY+2)*NSICLA)=.FALSE.
        SORIMP(24+(NOMBLAY+2)*NSICLA)=.FALSE.
      ENDIF
C
C-----------------------------------------------------------------------
C
C CHECKS TETA VALUE
C
      IF( TETA.LT.0.D0.OR.TETA.GT.1.D0) THEN
          IF (LNG.EQ.1) WRITE(LU,50)
          IF (LNG.EQ.2) WRITE(LU,51)
50      FORMAT(/,1X,'VALEUR DE TETA INCORRECTE !            ',/
     &          ,1X,'TETA DOIT ETRE COMPRIS ENTRE 0 ET 1    ')
51      FORMAT(/,1X,'BAD VALUE FOR TETA !                   ',/
     &          ,1X,'TETA MUST BE WITHIN 0 AND 1            ')
        CALL PLANTE(1)
        STOP
      ENDIF
C
C     INITIALISES MSK (MASKING VARIABLE)
C     FOR NOW MASKING IS ONLY DONE FOR ONE 'OPTION FOR THE TREATMENT
C     OF TIDAL FLATS'. SHOULD BE OFFERED AS AN OPTION FOR THE USER TO
C     CREATE ISLANDS IN THE FUTURE
      MSK = .FALSE.
      IF (.NOT.BANDEC) OPTBAN=0
      IF (OPTBAN.EQ.2) MSK = .TRUE.
C
C-----------------------------------------------------------------------
C
C     CHECKS WHETHER THERE IS A VALIDATION FILE
C
      IF (VALID.AND.SIS_FILES(SISREF)%NAME.EQ.' ') THEN
          VALID=.FALSE.
        IF (LNG.EQ.1) WRITE(LU,70)
        IF (LNG.EQ.2) WRITE(LU,71)
        WRITE(LU,*)
70      FORMAT(/,1X,'VALIDATION IMPOSSIBLE  :      ',/
     &          ,1X,'PAS DE FICHIER DE VALIDATION !        ')
71      FORMAT(/,1X,'VALIDATION IS NOT POSSIBLE :  ',/
     &          ,1X,'NO VALIDATION FILE  !                 ')
      ENDIF
C
CMGDL
C     CHECKS THE NUMBER OF
      IF(NSICLA.GT.10) THEN
      IF (LNG.EQ.1) WRITE(LU,80)
        IF (LNG.EQ.2) WRITE(LU,81)
        WRITE(LU,*)
80      FORMAT(/,1X,'LE NOMBRE MAXIMUM DE CLASSES DE SEDIMENTS EST 10')
81      FORMAT(/,1X,'THE MAXIMUM NUMBER OF SEDIMENT CLASSES IS 10')
        CALL PLANTE(1)
        STOP
      ENDIF
C     CHECKS THE SUM OF INITIAL AVAI
      DO I=1,NSICLA
      SUMAVAI = SUMAVAI + AVA0(I)
      ENDDO
      IF(ABS(SUMAVAI-1).GE.1.D-8) THEN
      IF (LNG.EQ.1) WRITE(LU,90)
        IF (LNG.EQ.2) WRITE(LU,91)
        WRITE(LU,*)
90      FORMAT(/,1X,'LA SOMME DES FRACTIONS DE SEDIMENTS ',/
     &          ,1X,'EST DIFFERENTE DE 1 !        ')
91      FORMAT(/,1X,'SUM OF SEDIMENT FRACTIONS IS NOT 1  ')
        CALL PLANTE(1)
        STOP
      ENDIF
C
C     WARNING FOR THE CHOICE OF RIGID BED METHOD
C
      IF(CHOIX.GT.0.AND.CHOIX.LT.4.AND.VF) THEN
      IF(LNG.EQ.1) WRITE(LU,200)
        IF (LNG.EQ.2) WRITE(LU,201)
        WRITE(LU,*)
200     FORMAT(/,1X,'CALCUL EN VOLUMES FINIS : ',/
     &          ,1X,'LA METHODE 4 POUR LES FONDS NON ERODABLES SERA UTIL
     &ISEE ')
201     FORMAT(/,1X,'FINITE VOLUMES CHOSEN: ',/
     &          ,1X,'METHOD 4 FOR RIGID BED WILL BE USED ')
C       ADDED BY JMH 12/07/2007
        CHOIX=4
      ENDIF
      IF (CHOIX.EQ.4.AND..NOT.VF) THEN
        IF(LNG.EQ.1) WRITE(LU,300)
        IF(LNG.EQ.2) WRITE(LU,301)
        WRITE(LU,*)
300     FORMAT(/,1X,'CALCUL EN ELEMENTS FINIS : ',/
     &          ,1X,'LA METHODE 4 NE PEUT ETRE UTILISEE, METHODE 3 UTILI
     &SEE A LA PLACE')
301     FORMAT(/,1X,'FINITE ELEMENTS CHOSEN: ',/
     &          ,1X,'METHOD 4 FOR RIGID BED CAN NOT BE USED, METHOD 3 US
     &ED INSTEAD')
C       ADDED BY JMH 12/07/2007
        CHOIX=3
      ENDIF
C
C     CHECKS THAT THE BEDLOAD TRANSPORT FORMULATION AND THE HIDING
C     FACTOR FORMULATION CAN GO TOGETHER
C
      IF ((HIDFAC.EQ.3.AND.ICF.NE.6).OR.
     &    (HIDFAC.EQ.1.AND.ICF.NE.1).OR.
     &    (HIDFAC.EQ.2.AND.ICF.NE.1)) THEN
      IF (LNG.EQ.1) WRITE(LU,110)
        IF (LNG.EQ.2) WRITE(LU,111)
        WRITE(LU,*)
110     FORMAT(/,1X,'MAUVAIS ASSOCIATION ENTRE LA FORMULE DE TRANSPORT E
     &T LE HIDING FACTOR ')
111     FORMAT(/,1X,'CHOICE OF TRANSPORT FORMULA AND HIDING FACTOR FORMU
     &LATION NOT ALLOWED ')
        CALL PLANTE(1)
        STOP
      ENDIF
C
C     WITHOUT AND WITH COUPLING, SOME CORRECTIONS
C
      IF(CODE(1:7).EQ.'TELEMAC'.AND.
     &   SIS_FILES(SISHYD)%NAME(1:1).NE.' ') THEN
        SIS_FILES(SISHYD)%NAME(1:1)=' '
        IF(LNG.EQ.1) WRITE(LU,112)
112     FORMAT(/,1X,'COUPLAGE : FICHIER HYDRODYNAMIQUE IGNORE')
        IF(LNG.EQ.1) WRITE(LU,113)
113     FORMAT(/,1X,'COUPLING: HYDRODYNAMIC FILE IGNORED')
      ENDIF
C
C     COMPUTATION CONTINUED
C
      IF(DEBU) THEN
        IF(SIS_FILES(SISPRE)%NAME(1:1).EQ.' ') THEN
          IF(LNG.EQ.1) WRITE(LU,312)
312       FORMAT(/,1X,'SUITE DE CALCUL :',/,
     &    1X,'FICHIER PRECEDENT SEDIMENTOLOGIQUE ABSENT')
          IF(LNG.EQ.2) WRITE(LU,313)
313       FORMAT(/,1X,'COMPUTATION CONTINUED:',/,
     &             1X,'PREVIOUS SEDIMENTOLOGICAL FILE MISSING')
          CALL PLANTE(1)
          STOP
        ENDIF
      ELSE
        IF(SIS_FILES(SISPRE)%NAME(1:1).NE.' ') THEN
          SIS_FILES(SISPRE)%NAME(1:1)=' '
          IF(LNG.EQ.1) WRITE(LU,212)
212       FORMAT(/,1X,'PAS DE SUITE DE CALCUL :',/,
     &             1X,'FICHIER PRECEDENT SEDIMENTOLOGIQUE IGNORE')
          IF(LNG.EQ.2) WRITE(LU,213)
213       FORMAT(/,1X,'NO COMPUTATION CONTINUED:',/,
     &             1X,'PREVIOUS SEDIMENTOLOGICAL FILE IGNORED')
        ENDIF
      ENDIF
C
C METHODS NOT CODED UP FOR SUSPENSION
C -------------------------------------------
C
C     JMH ON 09/10/2009 : NEW PARAMETERISATION AND NEW SCHEMES
C
      IF(SUSP) THEN
      IF(RESOL.NE.ADV_CAR   .AND.RESOL.NE.ADV_SUP   .AND.
     &   RESOL.NE.ADV_PSI_NC.AND.RESOL.NE.ADV_NSC_NC.AND.
     &   RESOL.NE.ADV_LPO   .AND.RESOL.NE.ADV_NSC   .AND.
     &   RESOL.NE.ADV_PSI   .AND.RESOL.NE.ADV_LPO_TF.AND.
     &   RESOL.NE.ADV_NSC_TF                              ) THEN
         IF (LNG.EQ.1) WRITE(LU,302) RESOL 
         IF (LNG.EQ.2) WRITE(LU,303) RESOL
302      FORMAT(1X,'METHODE DE RESOLUTION NON PROGRAMMEE : ',1I6)
303      FORMAT(1X,'RESOLVING METHOD NOT IMPLEMENTED : ',1I6)
         IF(RESOL.EQ.8) THEN
           IF(LNG.EQ.2) THEN
           WRITE(LU,*) 'LE SCHEMA 8 AVANT VERSION 6.0 EST DEVENU LE 4'
           ENDIF
           IF(LNG.EQ.2) THEN
           WRITE(LU,*) 'NUMBER 8 PRIOR TO VERSION 6.0 IS NOW NUMBER 4'
           ENDIF
         ENDIF
         CALL PLANTE(1)
         STOP
      ENDIF
      ENDIF
CC
C CV 27/01/2005
C
      IF(.NOT.HOULE) SIS_FILES(SISCOU)%NAME(1:1)=' '
      IF(HOULE) THEN
        IF(ICF.NE.4.AND.ICF.NE.5.AND.ICF.NE.8.AND.ICF.NE.9) THEN
          IF(LNG.EQ.1) WRITE(LU,1303) ICF
          IF(LNG.EQ.2) WRITE(LU,1304) ICF
1303      FORMAT(' LA FORMULE DE TRANSPORT',1I3,1X,
     &       'NE PREND PAS EN COMPTE LA HOULE,',/,1X,
     &       'ESSAYER 4, 5, 8 OU 9')
1304      FORMAT(' TRANSPORT FORMULA',1I3,1X,
     &       'DOES NOT TAKE WAVES INTO ACCOUNT,',/,1X,
     &       'TRY 4, 5, 8 OR 9')
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
C BEDLOAD AND SUSPENDED TRANSPORT COUPLING
! ---------------------------------
!
      IF((ICF==30.OR.ICF==3.OR.ICF==9).AND.SUSP.AND.CHARR) THEN
        IF(LNG.EQ.1) WRITE(LU,1301) ICF
        IF(LNG.EQ.2) WRITE(LU,1302) ICF
        CALL PLANTE(1)
        STOP
      ENDIF
1301  FORMAT('POUR LA FORMULE :',1I3,/,1X,
     &       'LE TERME DE TRANSPORT EN SUSPENSION EST CALCULE'
     &      ,' LORS DU CHARRIAGE ET LORS DE LA SUSPENSION')
1302  FORMAT('FOR THE FORMULA',1I3,/,1X,
     &       'THE SUSPENSION TERM IS CALCULATED TWICE,'
     &      ,' WITH TOTAL LOAD FORMULA AND SUSPENSION ')
!
C REFERENCE CONCENTRATION
!
C MODIFICATION CV 31/12      IF(ICQ.EQ.2.AND.(PERCOU.NE.1.OR..NOT.CHARR)) THEN
!
      IF(ICQ.EQ.2.AND.(PERCOU.GT.1.OR..NOT.CHARR)) THEN
        IF(LNG == 1) WRITE(LU,1401) ICQ
        IF(LNG == 2) WRITE(LU,1402) ICQ
1401  FORMAT('POUR LA METHODE DE BIJKER: ICQ=',1I3,/,1X,
     &       'LE CHARRIAGE DOIT ETRE CALCULE A CHAQUE PAS DE TEMPS
     &       , CHOISIR  : PERCOU = 1 ET',/,1X,
     &       'CHARRIAGE=OUI')
1402  FORMAT('FOR THE BIJKER REFERENCE CONCENTRATION',1I3,/,1X,
     &       'BEDLOAD MUST BE COMPUTED, CHOOSE:',/,1X,
     &       'BEDLOAD = YES')
        CALL PLANTE(1)
        STOP
      ENDIF
!
C     CHECKS CONSISTENCY OF BEDLOAD LAWS
!
C     SOULSBY SLOPE EFFECT : REQUIRES A THRESHOLD FORMULA
!
      IF(SLOPEFF.EQ.2) THEN
C       CHECK FOR ICF=6
C       IF(ICF.NE.1.AND.ICF.NE.6) THEN
        IF(ICF.NE.1) THEN
        IF(LNG == 1) WRITE(LU,1403) ICF
        IF(LNG == 2) WRITE(LU,1404) ICF
1403    FORMAT('LA LOI DE TRANSPORT SOLIDE, ICI ICF=',1I3,/,1X,
     &         'DOIT ETRE UNE FORMULE A SEUIL',/,1X,
     &         'SI FORMULE POUR EFFET DE PENTE=2 (SOULSBY)')
1404    FORMAT('BED-LOAD TRANSPORT FORMULA, HERE ICF=',1I3,/,1X,
     &         'MUST HAVE A THRESHOLD',/,1X,
     &         'IF FORMULA FOR SLOPE EFFECT=2 (SOULSBY)')
        ENDIF
      ENDIF
C
C V6P0 : COHERENCE IF CONSOLIDATION MODEL IS USED
C VITCE AND CSF_VASE STEM FROM THE FIRST LAYER OF THE MULTI-LAYER MODEL
C +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Mots clés supprimés vitesse critique d'erosion et concentration du lit
C CV : si première couche est  vide cela n'est pas correct

C
      IF(MIXTE) THEN
C
C       FILLS VOIDS WITH MUD: 
C CV: vérifier que la concentration en cohésif est non nulle
C
        CSF_SABLE= 1.D0
      ELSE
        CSF_SABLE= (1.D0-XKV)
      ENDIF
C
      IF((.NOT.MIXTE).AND.SEDCO(1)) THEN
        CHARR=.FALSE.
        !SUSP=.TRUE. (In general, but not necessary)
      ENDIF
C
      IF(NOMBLAY.GT.NLAYMAX) THEN
        WRITE (LU,*) 'NUMBER OF BED LOAD MODEL LAYERS LARGER THAN '
        WRITE (LU,*) 'THE MAXIMUM PROGRAMMED VALUE OF ', NLAYMAX
        CALL PLANTE(1)
        STOP
      ENDIF
C      IF(NOMBLAY.LT.2) THEN
C        WRITE (LU,*) 'BEWARE: NUMBER OF BED LOAD MODEL LAYERS'
C        WRITE (LU,*) '======= LOWER THAN THE DEFAULT VALUE OF 2'
C      ENDIF
C
C----------------------------------------------------------------
C
C  V6P1: FOR THE BED FRICTION PREDICTOR USE LAW OF FRICTION 5 (NIKURADSE)
C
      IF(KSPRED) KFROT=5
C
C

      RETURN
      END
C
C#######################################################################
C
C#######################################################################
C
