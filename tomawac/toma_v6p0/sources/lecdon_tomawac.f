C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       READS THE STEERING FILE THROUGH A DAMOCLES CALL.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TOMAWAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CODE, FILE_DESC, NCAR, PATH
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TOMAWAC :<br>
!> @link DECLARATIONS_TOMAWAC::ALFABJ ALFABJ@endlink, 
!> @link DECLARATIONS_TOMAWAC::ALFARO ALFARO@endlink, 
!> @link DECLARATIONS_TOMAWAC::ALFLTA ALFLTA@endlink, 
!> @link DECLARATIONS_TOMAWAC::ALPHA ALPHA@endlink, 
!> @link DECLARATIONS_TOMAWAC::ALPHIL ALPHIL@endlink, 
!> @link DECLARATIONS_TOMAWAC::APHILL APHILL@endlink, 
!> @link DECLARATIONS_TOMAWAC::BDISPB BDISPB@endlink, 
!> @link DECLARATIONS_TOMAWAC::BDSSPB BDSSPB@endlink, 
!> @link DECLARATIONS_TOMAWAC::BETAIH BETAIH@endlink, 
!> @link DECLARATIONS_TOMAWAC::BETAM BETAM@endlink, 
!> @link DECLARATIONS_TOMAWAC::BINBI1 BINBI1@endlink, 
!> @link DECLARATIONS_TOMAWAC::BINCOU BINCOU@endlink, 
!> @link DECLARATIONS_TOMAWAC::BINGEO BINGEO@endlink, 
!> @link DECLARATIONS_TOMAWAC::BINLEO BINLEO@endlink, 
!> @link DECLARATIONS_TOMAWAC::BINMAR BINMAR@endlink, 
!> @link DECLARATIONS_TOMAWAC::BINPRE BINPRE@endlink, 
!> @link DECLARATIONS_TOMAWAC::BINRBI BINRBI@endlink, 
!> @link DECLARATIONS_TOMAWAC::BINRES BINRES@endlink, 
!> @link DECLARATIONS_TOMAWAC::BINVEN BINVEN@endlink, 
!> @link DECLARATIONS_TOMAWAC::BORETG BORETG@endlink, 
!> @link DECLARATIONS_TOMAWAC::CDRAG CDRAG@endlink, 
!> @link DECLARATIONS_TOMAWAC::CFROT1 CFROT1@endlink, 
!> @link DECLARATIONS_TOMAWAC::CIMPLI CIMPLI@endlink, 
!> @link DECLARATIONS_TOMAWAC::CMOUT1 CMOUT1@endlink, 
!> @link DECLARATIONS_TOMAWAC::CMOUT2 CMOUT2@endlink, 
!> @link DECLARATIONS_TOMAWAC::COEFHS COEFHS@endlink, 
!> @link DECLARATIONS_TOMAWAC::COURAN COURAN@endlink, 
!> @link DECLARATIONS_TOMAWAC::COUSTA COUSTA@endlink, 
!> @link DECLARATIONS_TOMAWAC::DDC DDC@endlink, 
!> @link DECLARATIONS_TOMAWAC::DEBUG DEBUG@endlink, 
!> @link DECLARATIONS_TOMAWAC::DECAL DECAL@endlink, 
!> @link DECLARATIONS_TOMAWAC::DONTEL DONTEL@endlink, 
!> @link DECLARATIONS_TOMAWAC::DT DT@endlink, 
!> @link DECLARATIONS_TOMAWAC::E2FMIN E2FMIN@endlink, 
!> @link DECLARATIONS_TOMAWAC::EM2SIH EM2SIH@endlink, 
!> @link DECLARATIONS_TOMAWAC::EQUA EQUA@endlink, 
!> @link DECLARATIONS_TOMAWAC::F1 F1@endlink, 
!> @link DECLARATIONS_TOMAWAC::FETCH FETCH@endlink, 
!> @link DECLARATIONS_TOMAWAC::FETCHL FETCHL@endlink, 
!> @link DECLARATIONS_TOMAWAC::FPIC FPIC@endlink, 
!> @link DECLARATIONS_TOMAWAC::FPICL FPICL@endlink, 
!> @link DECLARATIONS_TOMAWAC::FPMAXL FPMAXL@endlink, 
!> @link DECLARATIONS_TOMAWAC::FRABI FRABI@endlink, 
!> @link DECLARATIONS_TOMAWAC::FRABL FRABL@endlink, 
!> @link DECLARATIONS_TOMAWAC::FREMAX FREMAX@endlink, 
!> @link DECLARATIONS_TOMAWAC::GAM2RO GAM2RO@endlink, 
!> @link DECLARATIONS_TOMAWAC::GAMARO GAMARO@endlink, 
!> @link DECLARATIONS_TOMAWAC::GAMATG GAMATG@endlink, 
!> @link DECLARATIONS_TOMAWAC::GAMBJ1 GAMBJ1@endlink, 
!> @link DECLARATIONS_TOMAWAC::GAMBJ2 GAMBJ2@endlink, 
!> @link DECLARATIONS_TOMAWAC::GAMMA GAMMA@endlink, 
!> @link DECLARATIONS_TOMAWAC::GAMMAL GAMMAL@endlink, 
!> @link DECLARATIONS_TOMAWAC::GLOB GLOB@endlink, 
!> @link DECLARATIONS_TOMAWAC::GRADEB GRADEB@endlink, 
!> @link DECLARATIONS_TOMAWAC::GRAPRD GRAPRD@endlink, 
!> @link DECLARATIONS_TOMAWAC::HM0 HM0@endlink, 
!> @link DECLARATIONS_TOMAWAC::HM0L HM0L@endlink, 
!> @link DECLARATIONS_TOMAWAC::IDHMA IDHMA@endlink, 
!> @link DECLARATIONS_TOMAWAC::IDISRO IDISRO@endlink, 
!> @link DECLARATIONS_TOMAWAC::IDTEL IDTEL@endlink, 
!> @link DECLARATIONS_TOMAWAC::IEXPRO IEXPRO@endlink, 
!> @link DECLARATIONS_TOMAWAC::IFRBJ IFRBJ@endlink, 
!> @link DECLARATIONS_TOMAWAC::IFRIH IFRIH@endlink, 
!> @link DECLARATIONS_TOMAWAC::IFRRO IFRRO@endlink, 
!> @link DECLARATIONS_TOMAWAC::IFRTG IFRTG@endlink, 
!> @link DECLARATIONS_TOMAWAC::IHMBJ IHMBJ@endlink, 
!> @link DECLARATIONS_TOMAWAC::INDIC INDIC@endlink, 
!> @link DECLARATIONS_TOMAWAC::INDIM INDIM@endlink, 
!> @link DECLARATIONS_TOMAWAC::INDIV INDIV@endlink, 
!> @link DECLARATIONS_TOMAWAC::INISPE INISPE@endlink, 
!> @link DECLARATIONS_TOMAWAC::IQBBJ IQBBJ@endlink, 
!> @link DECLARATIONS_TOMAWAC::IWHTG IWHTG@endlink, 
!> @link DECLARATIONS_TOMAWAC::I_ORIG I_ORIG@endlink, 
!> @link DECLARATIONS_TOMAWAC::J_ORIG J_ORIG@endlink, 
!> @link DECLARATIONS_TOMAWAC::KSPB KSPB@endlink, 
!> @link DECLARATIONS_TOMAWAC::LAM LAM@endlink, 
!> @link DECLARATIONS_TOMAWAC::LIMIT LIMIT@endlink, 
!> @link DECLARATIONS_TOMAWAC::LIMSPE LIMSPE@endlink, 
!> @link DECLARATIONS_TOMAWAC::LISFON LISFON@endlink, 
!> @link DECLARATIONS_TOMAWAC::LISPRD LISPRD@endlink, 
!> @link DECLARATIONS_TOMAWAC::LVMAC LVMAC@endlink, 
!> @link DECLARATIONS_TOMAWAC::MAREE MAREE@endlink, 
!> @link DECLARATIONS_TOMAWAC::MAXLU_WAC MAXLU_WAC@endlink, 
!> @link DECLARATIONS_TOMAWAC::MAXVAR MAXVAR@endlink, 
!> @link DECLARATIONS_TOMAWAC::NDTBRK NDTBRK@endlink, 
!> @link DECLARATIONS_TOMAWAC::NF NF@endlink, 
!> @link DECLARATIONS_TOMAWAC::NIT NIT@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPLAN NPLAN@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPLEO NPLEO@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPRIV NPRIV@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPTT NPTT@endlink, 
!> @link DECLARATIONS_TOMAWAC::NSITS NSITS@endlink, 
!> @link DECLARATIONS_TOMAWAC::PROINF PROINF@endlink, 
!> @link DECLARATIONS_TOMAWAC::PROMIN PROMIN@endlink, 
!> @link DECLARATIONS_TOMAWAC::PROP PROP@endlink, 
!> @link DECLARATIONS_TOMAWAC::RAISF RAISF@endlink, 
!> @link DECLARATIONS_TOMAWAC::RFMLTA RFMLTA@endlink, 
!> @link DECLARATIONS_TOMAWAC::ROAIR ROAIR@endlink, 
!> @link DECLARATIONS_TOMAWAC::ROEAU ROEAU@endlink, 
!> @link DECLARATIONS_TOMAWAC::SBREK SBREK@endlink, 
!> @link DECLARATIONS_TOMAWAC::SFROT SFROT@endlink, 
!> @link DECLARATIONS_TOMAWAC::SIGMAA SIGMAA@endlink, 
!> @link DECLARATIONS_TOMAWAC::SIGMAB SIGMAB@endlink, 
!> @link DECLARATIONS_TOMAWAC::SIGMAL SIGMAL@endlink, 
!> @link DECLARATIONS_TOMAWAC::SIGMBL SIGMBL@endlink, 
!> @link DECLARATIONS_TOMAWAC::SMOUT SMOUT@endlink, 
!> @link DECLARATIONS_TOMAWAC::SORIMP SORIMP@endlink, 
!> @link DECLARATIONS_TOMAWAC::SORLEO SORLEO@endlink, 
!> @link DECLARATIONS_TOMAWAC::SORT2D SORT2D@endlink, 
!> @link DECLARATIONS_TOMAWAC::SPEULI SPEULI@endlink, 
!> @link DECLARATIONS_TOMAWAC::SPHE SPHE@endlink, 
!> @link DECLARATIONS_TOMAWAC::SPRE1L SPRE1L@endlink, 
!> @link DECLARATIONS_TOMAWAC::SPRE2L SPRE2L@endlink, 
!> @link DECLARATIONS_TOMAWAC::SPRED1 SPRED1@endlink, 
!> @link DECLARATIONS_TOMAWAC::SPRED2 SPRED2@endlink, 
!> @link DECLARATIONS_TOMAWAC::STDGEO STDGEO@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRIA STRIA@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRIF STRIF@endlink, 
!> @link DECLARATIONS_TOMAWAC::SUIT SUIT@endlink, 
!> @link DECLARATIONS_TOMAWAC::SVENT SVENT@endlink, 
!> @link DECLARATIONS_TOMAWAC::TAILF TAILF@endlink, 
!> @link DECLARATIONS_TOMAWAC::TETA1 TETA1@endlink, 
!> @link DECLARATIONS_TOMAWAC::TETA1L TETA1L@endlink, 
!> @link DECLARATIONS_TOMAWAC::TETA2 TETA2@endlink, 
!> @link DECLARATIONS_TOMAWAC::TETA2L TETA2L@endlink, 
!> @link DECLARATIONS_TOMAWAC::TEXTE TEXTE@endlink, 
!> @link DECLARATIONS_TOMAWAC::TEXTPR TEXTPR@endlink, 
!> @link DECLARATIONS_TOMAWAC::TITCAS TITCAS@endlink, 
!> @link DECLARATIONS_TOMAWAC::TRIGO TRIGO@endlink, 
!> @link DECLARATIONS_TOMAWAC::TSOU TSOU@endlink, 
!> @link DECLARATIONS_TOMAWAC::VALID VALID@endlink, 
!> @link DECLARATIONS_TOMAWAC::VENSTA VENSTA@endlink, 
!> @link DECLARATIONS_TOMAWAC::VENT VENT@endlink, 
!> @link DECLARATIONS_TOMAWAC::VERS VERS@endlink, 
!> @link DECLARATIONS_TOMAWAC::VX_CTE VX_CTE@endlink, 
!> @link DECLARATIONS_TOMAWAC::VY_CTE VY_CTE@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACBI1 WACBI1@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACCAS WACCAS@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACCLI WACCLI@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACCOB WACCOB@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACCOF WACCOF@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACFO1 WACFO1@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACFON WACFON@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACGEO WACGEO@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACLEO WACLEO@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACMAB WACMAB@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACMAF WACMAF@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACPAR WACPAR@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACPRE WACPRE@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACRBI WACRBI@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACREF WACREF@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACRES WACRES@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACVEB WACVEB@endlink, 
!> @link DECLARATIONS_TOMAWAC::WACVEF WACVEF@endlink, 
!> @link DECLARATIONS_TOMAWAC::WAC_FILES WAC_FILES@endlink, 
!> @link DECLARATIONS_TOMAWAC::XDTBRK XDTBRK@endlink, 
!> @link DECLARATIONS_TOMAWAC::XKAPPA XKAPPA@endlink, 
!> @link DECLARATIONS_TOMAWAC::XLAMD XLAMD@endlink, 
!> @link DECLARATIONS_TOMAWAC::XLAMDA XLAMDA@endlink, 
!> @link DECLARATIONS_TOMAWAC::XLAMDL XLAMDL@endlink, 
!> @link DECLARATIONS_TOMAWAC::XLEO XLEO@endlink, 
!> @link DECLARATIONS_TOMAWAC::YLEO YLEO@endlink, 
!> @link DECLARATIONS_TOMAWAC::ZREPOS ZREPOS@endlink, 
!> @link DECLARATIONS_TOMAWAC::ZVENT ZVENT@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ADRESS, DEGRAD, DIMEN, DOC, I, K, MNEMO, MOTCAR, MOTCLE, MOTINT, MOTLOG, MOTREA, NMAX, NOM_CAS, NOM_DIC, PIS2, TROUVE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> DAMOCLE(), MAJUS(), NOMVAR_TOMAWAC(), READ_SUBMIT(), SORTIE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>HOMERE_TOMAWAC()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center> 6.0                                       </center>
!> </td><td> 06/12/2004
!> </td><td> MICHEL BENOIT (EDF R&D LNHE)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CODE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FILE_DESC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NCAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PATH
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE LECDON_TOMAWAC
     & (FILE_DESC,PATH,NCAR,CODE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CODE           |---| 
C| FILE_DESC      |---| 
C| NCAR           |---| 
C| PATH           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C DEGRAD = CONVERSION FACTOR FROM DEGREES TO RADIANS
      DOUBLE PRECISION DEGRAD, PIS2
      PARAMETER(DEGRAD=0.01745329252D0, PIS2=1.570796327D0)
      CHARACTER*8      MNEMO(MAXVAR)
      INTEGER          K
C
C-----------------------------------------------------------------------
C
C ARRAYS USED IN THE DAMOCLES CALL
C
      INTEGER, PARAMETER :: NMAX = 300
C
      INTEGER          ADRESS(4,NMAX),DIMEN(4,NMAX)
      DOUBLE PRECISION MOTREA(NMAX)
      INTEGER          MOTINT(NMAX)
      LOGICAL          MOTLOG(NMAX)
      CHARACTER*144    MOTCAR(NMAX)
      CHARACTER*72     MOTCLE(4,NMAX,2)
      INTEGER          TROUVE(4,NMAX)
      LOGICAL          DOC
      CHARACTER(LEN=250) :: NOM_CAS
      CHARACTER(LEN=250) :: NOM_DIC
C ARGUMENTS
      CHARACTER(LEN=24), INTENT(IN)     :: CODE
      CHARACTER(LEN=144), INTENT(INOUT) :: FILE_DESC(4,NMAX)
      INTEGER, INTENT(IN)               :: NCAR
      CHARACTER(LEN=250), INTENT(IN)    :: PATH
      INTEGER :: I
C
C END OF DECLARATIONS FOR DAMOCLES CALL
C
C
C***********************************************************************
C
      IF (LNG.EQ.1) WRITE(LU,1)
      IF (LNG.EQ.2) WRITE(LU,2)
1     FORMAT(1X,/,19X, '********************************************',/,
     &            19X, '*     SOUS-PROGRAMME LECDON_TOMAWAC        *',/,
     &            19X, '*           APPEL DE DAMOCLES              *',/,
     &            19X, '*     VERIFICATION DES DONNEES LUES        *',/,
     &            19X, '*           SUR LE FICHIER CAS             *',/,
     &            19X, '********************************************',/)
2     FORMAT(1X,/,19X, '********************************************',/,
     &            19X, '*        SUBROUTINE LECDON_TOMAWAC         *',/,
     &            19X, '*           CALL OF DAMOCLES               *',/,
     &            19X, '*        VERIFICATION OF READ DATA         *',/,
     &            19X, '*            ON STEERING FILE              *',/,
     &            19X, '********************************************',/)
C
C-----------------------------------------------------------------------
C
C INITIALISES THE VARIABLES FOR DAMOCLES CALL :
C
      DO K=1,NMAX
C       A FILENAME NOT GIVEN BY DAMOCLES WILL BE RECOGNIZED AS A WHITE SPACE
C       (IT MAY BE THAT NOT ALL COMPILERS WILL INITIALISE LIKE THAT)
        MOTCAR(K)(1:1)=' '
C
        DIMEN(1,K) = 0
        DIMEN(2,K) = 0
        DIMEN(3,K) = 0
        DIMEN(4,K) = 0
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
        NOM_DIC=PATH(1:NCAR)//'WACDICO'
        NOM_CAS=PATH(1:NCAR)//'WACCAS'
C
      ELSE
C
        NOM_DIC='WACDICO'
        NOM_CAS='WACCAS'
C
      ENDIF
C
      OPEN(2,FILE=NOM_DIC,FORM='FORMATTED',ACTION='READ')
      OPEN(3,FILE=NOM_CAS,FORM='FORMATTED',ACTION='READ')
C
      CALL DAMOCLE
     &( ADRESS, DIMEN , NMAX  , DOC    , LNG   , LU    , MOTINT,
     &  MOTREA, MOTLOG, MOTCAR, MOTCLE , TROUVE, 2  , 3  ,
     &  .FALSE.,FILE_DESC)
C
C     DECODES 'SUBMIT' CHAINS
C
      CALL READ_SUBMIT(WAC_FILES,MAXLU_WAC,CODE,FILE_DESC,300)
C
C-----------------------------------------------------------------------
C
C     RETRIEVES FILE NUMBERS FROM TOMAWAC FORTRAN PARAMETERS
C     AT THIS LEVEL LOGICAL UNITS ARE EQUAL TO THE FILE NUMBER
C
      DO I=1,MAXLU_WAC
        IF(WAC_FILES(I)%TELNAME.EQ.'WACGEO') THEN
          WACGEO=I
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACCAS') THEN
          WACCAS=I
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACCLI') THEN
          WACCLI=I
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACFON') THEN
          WACFON=I
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACRES') THEN
          WACRES=I
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACREF') THEN
          WACREF=I
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACLEO') THEN
          WACLEO=I
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACPRE') THEN
          WACPRE=I
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACRBI') THEN
          WACRBI=I
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACCOB') THEN
          WACCOB=I
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACCOF') THEN
          WACCOF=I
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACBI1') THEN
          WACBI1=I
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACFO1') THEN
          WACFO1=I
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACVEB') THEN
          WACVEB=I
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACVEF') THEN
          WACVEF=I
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACPAR') THEN
          WACPAR=I
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACMAB') THEN
          WACMAB=I
        ELSEIF(WAC_FILES(I)%TELNAME.EQ.'WACMAF') THEN
          WACMAF=I
        ENDIF
      ENDDO
C
C-----------------------------------------------------------------------
C
C   ASSIGNS THE STEERING FILE VALUES TO THE PARAMETER FORTRAN NAME
C
C-----------------------------------------------------------------------
C
C INTEGER KEYWORDS
C
      GRAPRD = MOTINT( ADRESS(1,  1) )
      LISPRD = MOTINT( ADRESS(1,  2) )
      NIT    = MOTINT( ADRESS(1,  3) )
      NPLAN  = MOTINT( ADRESS(1,  4) )
      NF     = MOTINT( ADRESS(1,  5) )
      GRADEB = MOTINT( ADRESS(1,  6) )
      LISFON = MOTINT( ADRESS(1,  7) )
      SVENT  = MOTINT( ADRESS(1,  8) )
      SMOUT  = MOTINT( ADRESS(1,  9) )
      SFROT  = MOTINT( ADRESS(1, 10) )
      STRIF  = MOTINT( ADRESS(1, 11) )
      INDIC  = MOTINT( ADRESS(1, 12) )
      INDIV  = MOTINT( ADRESS(1, 13) )
      NSITS  = MOTINT( ADRESS(1, 14) )
      INISPE = MOTINT( ADRESS(1, 15) )
      IDTEL  = MOTINT( ADRESS(1, 16) )
      NPTT   = MOTINT( ADRESS(1, 17) )
      LVMAC  = MOTINT( ADRESS(1, 18) )
      SBREK  = MOTINT( ADRESS(1, 19) )
      IQBBJ  = MOTINT( ADRESS(1, 20) )
      IHMBJ  = MOTINT( ADRESS(1, 21) )
      IFRBJ  = MOTINT( ADRESS(1, 22) )
      IWHTG  = MOTINT( ADRESS(1, 23) )
      IFRTG  = MOTINT( ADRESS(1, 24) )
      IDISRO = MOTINT( ADRESS(1, 25) )
      IEXPRO = MOTINT( ADRESS(1, 26) )
      IFRRO  = MOTINT( ADRESS(1, 27) )
      IFRIH  = MOTINT( ADRESS(1, 28) )
      NDTBRK = MOTINT( ADRESS(1, 29) )
      LIMIT  = MOTINT( ADRESS(1, 30) )
C
C         NPRIV     = MOTINT( ADRESS(1, 32?) ) 'ADD TO DICO FILE'
C
      STRIA  = MOTINT( ADRESS(1, 32) )
      LIMSPE = MOTINT( ADRESS(1, 33) )
      LAM    = MOTINT( ADRESS(1, 34) )
      INDIM  = MOTINT( ADRESS(1, 35) )
      IDHMA  = MOTINT( ADRESS(1, 36) )
      FRABI  = MOTINT( ADRESS(1, 37) )
      NPRIV  = MOTINT( ADRESS(1, 38) )
      FRABL  = MOTINT( ADRESS(1, 39) )
C     COORDINATES OF THE ORIGIN IN (X, Y)
      I_ORIG = MOTINT( ADRESS(1, 40) )
      J_ORIG = MOTINT( ADRESS(1, 40)+1 )
C     DEBUG KEYWORD
      DEBUG  = MOTINT( ADRESS(1, 41) )
C
C     GEOMETRY FILE STANDARD
      STDGEO = 3
C
C REAL KEYWORDS
C
      DT     = MOTREA( ADRESS(2,  1) )
      F1     = MOTREA( ADRESS(2,  2) )
      RAISF  = MOTREA( ADRESS(2,  3) )
      NPLEO  = DIMEN(2,4)
      DO K=1,DIMEN(2,4)
        XLEO(K)= MOTREA( ADRESS(2,  4) + K-1)
      ENDDO
      DO K=1,DIMEN(2,5)
        YLEO(K)= MOTREA( ADRESS(2,  5) + K-1)
      ENDDO
      DDC    = MOTREA( ADRESS(2,  6) )
      CFROT1 = MOTREA( ADRESS(2,  7) )
      CMOUT1 = MOTREA( ADRESS(2,  8) )
      CMOUT2 = MOTREA( ADRESS(2,  9) )
      ROAIR  = MOTREA( ADRESS(2, 10) )
      ROEAU  = MOTREA( ADRESS(2, 11) )
      BETAM  = MOTREA( ADRESS(2, 12) )
      XKAPPA = MOTREA( ADRESS(2, 13) )
      ALPHA  = MOTREA( ADRESS(2, 14) )
      DECAL  = MOTREA( ADRESS(2, 15) )
      ZVENT  = MOTREA( ADRESS(2, 16) )
      CDRAG  = MOTREA( ADRESS(2, 17) )
      HM0    = MOTREA( ADRESS(2, 18) )
      FPIC   = MOTREA( ADRESS(2, 19) )
      GAMMA  = MOTREA( ADRESS(2, 20) )
      SIGMAA = MOTREA( ADRESS(2, 21) )
      SIGMAB = MOTREA( ADRESS(2, 22) )
      ALPHIL = MOTREA( ADRESS(2, 23) )
      FETCH  = MOTREA( ADRESS(2, 24) )
      FREMAX = MOTREA( ADRESS(2, 25) )
      TETA1  = MOTREA( ADRESS(2, 26) )*DEGRAD
      SPRED1 = MOTREA( ADRESS(2, 27) )
      TETA2  = MOTREA( ADRESS(2, 28) )*DEGRAD
      SPRED2 = MOTREA( ADRESS(2, 29) )
      XLAMDA = MOTREA( ADRESS(2, 30) )
      TAILF  = MOTREA( ADRESS(2, 31) )
      E2FMIN = MOTREA( ADRESS(2, 32) )
      ALFABJ = MOTREA( ADRESS(2, 33) )
      GAMBJ1 = MOTREA( ADRESS(2, 34) )
      GAMBJ2 = MOTREA( ADRESS(2, 35) )
      BORETG = MOTREA( ADRESS(2, 36) )
      GAMATG = MOTREA( ADRESS(2, 37) )
      ALFARO = MOTREA( ADRESS(2, 38) )
      GAMARO = MOTREA( ADRESS(2, 39) )
      GAM2RO = MOTREA( ADRESS(2, 40) )
      BETAIH = MOTREA( ADRESS(2, 41) )
      EM2SIH = MOTREA( ADRESS(2, 42) )
      COEFHS = MOTREA( ADRESS(2, 43) )
      XDTBRK = MOTREA( ADRESS(2, 44) )
      XLAMD  = MOTREA( ADRESS(2, 45) )
      ZREPOS = MOTREA( ADRESS(2, 46) )
      ALFLTA = MOTREA( ADRESS(2, 47) )
      RFMLTA = MOTREA( ADRESS(2, 48) )
      KSPB   = MOTREA( ADRESS(2, 49) )
      BDISPB = MOTREA( ADRESS(2, 50) )*DEGRAD
      BDSSPB = MOTREA( ADRESS(2, 51) )*DEGRAD
      HM0L   = MOTREA( ADRESS(2, 52) )
      FPICL  = MOTREA( ADRESS(2, 53) )
      SIGMAL = MOTREA( ADRESS(2, 54) )
      SIGMBL = MOTREA( ADRESS(2, 55) )
      APHILL = MOTREA( ADRESS(2, 56) )
      FETCHL = MOTREA( ADRESS(2, 57) )
      FPMAXL = MOTREA( ADRESS(2, 58) )
      TETA1L = MOTREA( ADRESS(2, 59) )*DEGRAD
      SPRE1L = MOTREA( ADRESS(2, 60) )
      TETA2L = MOTREA( ADRESS(2, 61) )*DEGRAD
      SPRE2L = MOTREA( ADRESS(2, 62) )
      XLAMDL = MOTREA( ADRESS(2, 63) )
      GAMMAL = MOTREA( ADRESS(2, 64) )
      PROMIN = MOTREA( ADRESS(2, 65) )
      VX_CTE = MOTREA( ADRESS(2, 66) )
      VY_CTE = MOTREA( ADRESS(2, 67) )
      CIMPLI = MOTREA( ADRESS(2, 68) )
C
C LOGICAL KEYWORDS
C
      TSOU   = MOTLOG( ADRESS(3,  1) )
      SPHE   = MOTLOG( ADRESS(3,  2) )
      GLOB   = MOTLOG( ADRESS(3,  3) )
      SUIT   = MOTLOG( ADRESS(3,  4) )
      PROINF = MOTLOG( ADRESS(3,  5) )
      COUSTA = MOTLOG( ADRESS(3,  6) )
      VENT   = MOTLOG( ADRESS(3,  7) )
      DONTEL = MOTLOG( ADRESS(3,  8) )
      PROP   = MOTLOG( ADRESS(3,  9) )
      VENSTA = MOTLOG( ADRESS(3, 10) )
      VALID  = MOTLOG( ADRESS(3, 11) )
      MAREE  = MOTLOG( ADRESS(3, 12) )
      TRIGO  = MOTLOG( ADRESS(3, 13) )
      SPEULI = MOTLOG( ADRESS(3, 14) )
C
C STRING KEYWORDS
C
      TITCAS = MOTCAR( ADRESS(4, 1) ) (1:72)
      SORT2D = MOTCAR( ADRESS(4, 2) ) (1:72)
C
C FILES IN THE STEERING FILE
C
      WAC_FILES(WACGEO)%NAME=MOTCAR( ADRESS(4,3) )
C     NOMFOR = MOTCAR( ADRESS(4, 4) )
C     NOMCAS = MOTCAR( ADRESS(4, 5) )
      WAC_FILES(WACCLI)%NAME=MOTCAR( ADRESS(4,6) )
      WAC_FILES(WACFON)%NAME=MOTCAR( ADRESS(4,7) )
      WAC_FILES(WACRES)%NAME=MOTCAR( ADRESS(4,8) )
      WAC_FILES(WACLEO)%NAME=MOTCAR( ADRESS(4,9) )
      WAC_FILES(WACPRE)%NAME=MOTCAR( ADRESS(4,10) )
      WAC_FILES(WACRBI)%NAME=MOTCAR( ADRESS(4,11) )
      WAC_FILES(WACCOB)%NAME=MOTCAR( ADRESS(4,12) )
      WAC_FILES(WACCOF)%NAME=MOTCAR( ADRESS(4,13) )
      WAC_FILES(WACBI1)%NAME=MOTCAR( ADRESS(4,14) )
      WAC_FILES(WACFO1)%NAME=MOTCAR( ADRESS(4,15) )
      BINGEO = MOTCAR( ADRESS(4,16) )(1:3)
      BINRES = MOTCAR( ADRESS(4,17) )(1:3)
      BINLEO = MOTCAR( ADRESS(4,18) )(1:3)
      BINCOU = MOTCAR( ADRESS(4,19) )(1:3)
      BINRBI = MOTCAR( ADRESS(4,20) )(1:3)
      BINPRE = MOTCAR( ADRESS(4,21) )(1:3)
      VERS   = MOTCAR( ADRESS(4,22) )(1:4)
C
C     FROM 23 TO 28 : FOR CRAY, NOT USEFUL HERE
C
      BINVEN = MOTCAR( ADRESS(4,29) )(1:3)
      BINBI1 = MOTCAR( ADRESS(4,30) )(1:3)
      WAC_FILES(WACVEB)%NAME=MOTCAR( ADRESS(4,31) )
      WAC_FILES(WACVEF)%NAME=MOTCAR( ADRESS(4,32) )
      WAC_FILES(WACPAR)%NAME=MOTCAR( ADRESS(4,33) )
      WAC_FILES(WACREF)%NAME=MOTCAR( ADRESS(4,34) )
      WAC_FILES(WACMAB)%NAME=MOTCAR( ADRESS(4,35) )
      WAC_FILES(WACMAF)%NAME=MOTCAR( ADRESS(4,36) )
      BINMAR = MOTCAR( ADRESS(4,37) )(1:3)
      EQUA   = 'TOMAWAC-COWADIS'
!BD_INCKA FILE FORMATS
C     RESULTS FILE FORMAT
      WAC_FILES(WACRES)%FMT = MOTCAR( ADRESS(4,40) )(1:8)
      CALL MAJUS(WAC_FILES(WACRES)%FMT)
C     INITIAL RESULTS FILE FORMAT (< PREVIOUS COMPUTATION)
C     SEDIMENT...
      WAC_FILES(WACPRE)%FMT = MOTCAR( ADRESS(4,41) )(1:8)
      CALL MAJUS(WAC_FILES(WACPRE)%FMT)
C     REFERENCE FILE FORMAT
      WAC_FILES(WACREF)%FMT = MOTCAR( ADRESS(4,42) )(1:8)
      CALL MAJUS(WAC_FILES(WACREF)%FMT)
C     BINARY FILE 1 FORMAT
      WAC_FILES(WACBI1)%FMT = MOTCAR( ADRESS(4,43) )(1:8)
      CALL MAJUS(WAC_FILES(WACBI1)%FMT)
C     SPECTRAL FILE FORMAT
      WAC_FILES(WACLEO)%FMT = MOTCAR( ADRESS(4,44) )(1:8)
      CALL MAJUS(WAC_FILES(WACLEO)%FMT)
C
C  CORRECTS OR COMPUTES OTHER PARAMETERS FROM THOSE THAT
C  HAVE JUST BEEN READ
C
      IF(COUSTA.OR.MAREE) THEN
        COURAN=.TRUE.
      ELSE
        COURAN=.FALSE.
      ENDIF
      IF(.NOT.VENT.AND.SVENT.NE.0) THEN
         IF(LNG.EQ.1)
     &         WRITE(LU,*)
     &      'INCOHERENCE DES MOTS CLES DU VENT => PAS DE VENT'
         IF(LNG.EQ.2)
     &         WRITE(LU,*)
     &      'INCOMPATIBILITY OF KEY-WORDS CONCERNING WIND => NO
     & WIND'
        SVENT=0
      ENDIF
      IF(TRIGO) THEN
        TETA1  = PIS2-TETA1
        TETA2  = PIS2-TETA2
        TETA1L = PIS2-TETA1L
        TETA2L = PIS2-TETA2L
        BDISPB = PIS2-BDISPB
        BDSSPB = PIS2-BDSSPB
      ENDIF
      IF(LIMIT.LT.0.OR.LIMIT.GT.2) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'INCOHERENCE DE L OPTION DE LIMITEUR'
          WRITE(LU,*) 'VALEUR LUE = ',LIMIT
          WRITE(LU,*) 'ON PREND LA VALEUR PAR DEFAUT LIMIT=1'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'INCOMPATIBILITY OF LIMITER OPTION'
          WRITE(LU,*) 'VALUE READ = ',LIMIT
          WRITE(LU,*) 'WE TAKE THE DEFAULT VALUE LIMIT=1'
        ENDIF
        LIMIT=1
      ENDIF
      IF(CIMPLI.LT.0.OR.CIMPLI.GT.1) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'INCOHERENCE DU COEFFICIENT D IMPLICITATION'
          WRITE(LU,*) 'VALEUR LUE = ',CIMPLI
          WRITE(LU,*) 'ON PREND LA VALEUR PAR DEFAUT CIMPLI=0.5'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'INCOMPATIBILITY OF IMPLICITATION COEFFICIENT'
          WRITE(LU,*) 'VALUE READ = ',CIMPLI
          WRITE(LU,*) 'WE TAKE THE DEFAULT VALUE CIMPLI=0.5'
        ENDIF
        CIMPLI=0.5D0
      ENDIF
C
C
C-----------------------------------------------------------------------
C  NAME OF THE VARIABLES FOR THE RESULTS AND GEOMETRY FILES:
C-----------------------------------------------------------------------
C
C LOGICAL ARRAY FOR OUTPUT
C
      CALL NOMVAR_TOMAWAC(TEXTE,TEXTPR,MNEMO,MAXVAR)
C
C$DC$ BUG : ARRAYS MNEMO AND SORLEO OF SIZE MAXVAR
C             MUCH LESS THAN 100 !
      CALL SORTIE(SORT2D , MNEMO , MAXVAR , SORLEO )
C
C.....IF NO WIND, THERE SHOULD BE NO INFORMATION WRITTEN ABOUT WINDS
      IF (.NOT.VENT) THEN
        SORLEO( 9)=.FALSE.
        SORLEO(10)=.FALSE.
      ENDIF
C
C.....IF INFINITE DEPTH, THE RADIATION STRESSES ARE NOT COMPUTED
      IF (PROINF) THEN
        IF (SORLEO(11) .OR. SORLEO(12) .OR. SORLEO(13) .OR.
     &      SORLEO(14) .OR. SORLEO(15) ) THEN
           IF (LNG.EQ.1) THEN
             WRITE(LU,*) '******************************************'
             WRITE(LU,*) ' LE CALCUL DES CONTRAINTES DE RADIATION ET'
             WRITE(LU,*) '  DES FORCES MOTRICES NE S''EFFECTUE PAS'
             WRITE(LU,*) '      PAS EN PROFONDEUR INFINIE          '
             WRITE(LU,*) '******************************************'
           ELSE
             WRITE(LU,*) '*****************************************'
             WRITE(LU,*) '   RADIATION STRESSES ARE NOT COMPUTED  '
             WRITE(LU,*) '       OVER INFINITE WATER DEPTHS       '
             WRITE(LU,*) '******************************************'
           ENDIF
           DO K=11,15
             SORLEO(K) = .FALSE.
           ENDDO
        ENDIF
      ENDIF
C
      DO K=1,MAXVAR
        SORIMP(K)=.FALSE.
      ENDDO
C
C
 1001 FORMAT('*** INCOMPATIBILITE DES MOTS CLES ***
     &                ARRET DU PROGRAMME')
 1002 FORMAT('*** INCOMPATIBILITY OF THE KEY WORDS ***
     &                   PROGRAM STOP')
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C