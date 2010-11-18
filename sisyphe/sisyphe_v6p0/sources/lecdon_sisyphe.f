C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       READS THE STEERING FILE BY CALL TO DAMOCLES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>DECLARATIONS_SISYPHE, DECLARATIONS_TELEMAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CODE, FILE_DESC, MOTCAR, NCAR, PATH
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::ADV_CAR ADV_CAR@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_LPO ADV_LPO@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_LPO_TF ADV_LPO_TF@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_NSC ADV_NSC@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_NSC_NC ADV_NSC_NC@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_NSC_TF ADV_NSC_TF@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_PSI ADV_PSI@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_PSI_NC ADV_PSI_NC@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_SUP ADV_SUP@endlink<hr>
!> DECLARATIONS_SISYPHE :<br>
!> @link DECLARATIONS_SISYPHE::AC AC@endlink, 
!> @link DECLARATIONS_SISYPHE::AVA0 AVA0@endlink, 
!> @link DECLARATIONS_SISYPHE::BANDEC BANDEC@endlink, 
!> @link DECLARATIONS_SISYPHE::BETA BETA@endlink, 
!> @link DECLARATIONS_SISYPHE::BETA2 BETA2@endlink, 
!> @link DECLARATIONS_SISYPHE::BIJK BIJK@endlink, 
!> @link DECLARATIONS_SISYPHE::BILMA BILMA@endlink, 
!> @link DECLARATIONS_SISYPHE::BINGEOSIS BINGEOSIS@endlink, 
!> @link DECLARATIONS_SISYPHE::BINHYDSIS BINHYDSIS@endlink, 
!> @link DECLARATIONS_SISYPHE::BINPRESIS BINPRESIS@endlink, 
!> @link DECLARATIONS_SISYPHE::BINREFSIS BINREFSIS@endlink, 
!> @link DECLARATIONS_SISYPHE::BINRESSIS BINRESSIS@endlink, 
!> @link DECLARATIONS_SISYPHE::CALAC CALAC@endlink, 
!> @link DECLARATIONS_SISYPHE::CALWC CALWC@endlink, 
!> @link DECLARATIONS_SISYPHE::CBOR_CLASSE CBOR_CLASSE@endlink, 
!> @link DECLARATIONS_SISYPHE::CHARR CHARR@endlink, 
!> @link DECLARATIONS_SISYPHE::CHOIX CHOIX@endlink, 
!> @link DECLARATIONS_SISYPHE::CONC_VASE CONC_VASE@endlink, 
!> @link DECLARATIONS_SISYPHE::CONST_ALAYER CONST_ALAYER@endlink, 
!> @link DECLARATIONS_SISYPHE::CORR_CONV CORR_CONV@endlink, 
!> @link DECLARATIONS_SISYPHE::CRIT_CFD CRIT_CFD@endlink, 
!> @link DECLARATIONS_SISYPHE::CS0 CS0@endlink, 
!> @link DECLARATIONS_SISYPHE::CSF_SABLE CSF_SABLE@endlink, 
!> @link DECLARATIONS_SISYPHE::CSF_VASE CSF_VASE@endlink, 
!> @link DECLARATIONS_SISYPHE::CTRLSC CTRLSC@endlink, 
!> @link DECLARATIONS_SISYPHE::DEBU DEBU@endlink, 
!> @link DECLARATIONS_SISYPHE::DEBUG DEBUG@endlink, 
!> @link DECLARATIONS_SISYPHE::DELT DELT@endlink, 
!> @link DECLARATIONS_SISYPHE::DEPER DEPER@endlink, 
!> @link DECLARATIONS_SISYPHE::DEVIA DEVIA@endlink, 
!> @link DECLARATIONS_SISYPHE::DIFT DIFT@endlink, 
!> @link DECLARATIONS_SISYPHE::DREDGESIM DREDGESIM@endlink, 
!> @link DECLARATIONS_SISYPHE::ELAY0 ELAY0@endlink, 
!> @link DECLARATIONS_SISYPHE::FD90 FD90@endlink, 
!> @link DECLARATIONS_SISYPHE::FDM FDM@endlink, 
!> @link DECLARATIONS_SISYPHE::GRAV GRAV@endlink, 
!> @link DECLARATIONS_SISYPHE::HIDFAC HIDFAC@endlink, 
!> @link DECLARATIONS_SISYPHE::HIDI HIDI@endlink, 
!> @link DECLARATIONS_SISYPHE::HMIN HMIN@endlink, 
!> @link DECLARATIONS_SISYPHE::HOULE HOULE@endlink, 
!> @link DECLARATIONS_SISYPHE::HYDRO HYDRO@endlink, 
!> @link DECLARATIONS_SISYPHE::ICF ICF@endlink, 
!> @link DECLARATIONS_SISYPHE::ICQ ICQ@endlink, 
!> @link DECLARATIONS_SISYPHE::ICR ICR@endlink, 
!> @link DECLARATIONS_SISYPHE::IELMH_SIS IELMH_SIS@endlink, 
!> @link DECLARATIONS_SISYPHE::IELMT IELMT@endlink, 
!> @link DECLARATIONS_SISYPHE::IELMU_SIS IELMU_SIS@endlink, 
!> @link DECLARATIONS_SISYPHE::IMP_INFLOW_C IMP_INFLOW_C@endlink, 
!> @link DECLARATIONS_SISYPHE::I_ORIG I_ORIG@endlink, 
!> @link DECLARATIONS_SISYPHE::J_ORIG J_ORIG@endlink, 
!> @link DECLARATIONS_SISYPHE::KFROT KFROT@endlink, 
!> @link DECLARATIONS_SISYPHE::KSPRATIO KSPRATIO@endlink, 
!> @link DECLARATIONS_SISYPHE::LCONDIS LCONDIS@endlink, 
!> @link DECLARATIONS_SISYPHE::LEOPR LEOPR@endlink, 
!> @link DECLARATIONS_SISYPHE::LGRAFED LGRAFED@endlink, 
!> @link DECLARATIONS_SISYPHE::LISPR LISPR@endlink, 
!> @link DECLARATIONS_SISYPHE::LOADMETH LOADMETH@endlink, 
!> @link DECLARATIONS_SISYPHE::LOGDES LOGDES@endlink, 
!> @link DECLARATIONS_SISYPHE::LOGPRE LOGPRE@endlink, 
!> @link DECLARATIONS_SISYPHE::LS0 LS0@endlink, 
!> @link DECLARATIONS_SISYPHE::LUMPI LUMPI@endlink, 
!> @link DECLARATIONS_SISYPHE::LVMAC LVMAC@endlink, 
!> @link DECLARATIONS_SISYPHE::MARDAT MARDAT@endlink, 
!> @link DECLARATIONS_SISYPHE::MARTIM MARTIM@endlink, 
!> @link DECLARATIONS_SISYPHE::MAXFRO MAXFRO@endlink, 
!> @link DECLARATIONS_SISYPHE::MAXLU_SIS MAXLU_SIS@endlink, 
!> @link DECLARATIONS_SISYPHE::MAXVAR MAXVAR@endlink, 
!> @link DECLARATIONS_SISYPHE::MIXTE MIXTE@endlink, 
!> @link DECLARATIONS_SISYPHE::MNEMO MNEMO@endlink, 
!> @link DECLARATIONS_SISYPHE::MSK MSK@endlink, 
!> @link DECLARATIONS_SISYPHE::NCONDIS NCONDIS@endlink, 
!> @link DECLARATIONS_SISYPHE::NCOUCH_TASS NCOUCH_TASS@endlink, 
!> @link DECLARATIONS_SISYPHE::NCP NCP@endlink, 
!> @link DECLARATIONS_SISYPHE::NLAYMAX NLAYMAX@endlink, 
!> @link DECLARATIONS_SISYPHE::NMAREE NMAREE@endlink, 
!> @link DECLARATIONS_SISYPHE::NOMBLAY NOMBLAY@endlink, 
!> @link DECLARATIONS_SISYPHE::NPAS NPAS@endlink, 
!> @link DECLARATIONS_SISYPHE::NPRIV NPRIV@endlink, 
!> @link DECLARATIONS_SISYPHE::NSICLA NSICLA@endlink, 
!> @link DECLARATIONS_SISYPHE::NSOUS NSOUS@endlink, 
!> @link DECLARATIONS_SISYPHE::OPDTRA OPDTRA@endlink, 
!> @link DECLARATIONS_SISYPHE::OPTASS OPTASS@endlink, 
!> @link DECLARATIONS_SISYPHE::OPTBAN OPTBAN@endlink, 
!> @link DECLARATIONS_SISYPHE::OPTDIF OPTDIF@endlink, 
!> @link DECLARATIONS_SISYPHE::OPTSUP OPTSUP@endlink, 
!> @link DECLARATIONS_SISYPHE::PARTHENIADES PARTHENIADES@endlink, 
!> @link DECLARATIONS_SISYPHE::PERCOU PERCOU@endlink, 
!> @link DECLARATIONS_SISYPHE::PERMA PERMA@endlink, 
!> @link DECLARATIONS_SISYPHE::PHISED PHISED@endlink, 
!> @link DECLARATIONS_SISYPHE::PMAREE PMAREE@endlink, 
!> @link DECLARATIONS_SISYPHE::PRODUC PRODUC@endlink, 
!> @link DECLARATIONS_SISYPHE::PTINIG PTINIG@endlink, 
!> @link DECLARATIONS_SISYPHE::PTINIL PTINIL@endlink, 
!> @link DECLARATIONS_SISYPHE::RC RC@endlink, 
!> @link DECLARATIONS_SISYPHE::RESOL RESOL@endlink, 
!> @link DECLARATIONS_SISYPHE::SECCURRENT SECCURRENT@endlink, 
!> @link DECLARATIONS_SISYPHE::SEDCO SEDCO@endlink, 
!> @link DECLARATIONS_SISYPHE::SFON SFON@endlink, 
!> @link DECLARATIONS_SISYPHE::SISCLI SISCLI@endlink, 
!> @link DECLARATIONS_SISYPHE::SISCOU SISCOU@endlink, 
!> @link DECLARATIONS_SISYPHE::SISFON SISFON@endlink, 
!> @link DECLARATIONS_SISYPHE::SISGEO SISGEO@endlink, 
!> @link DECLARATIONS_SISYPHE::SISHYD SISHYD@endlink, 
!> @link DECLARATIONS_SISYPHE::SISMAF SISMAF@endlink, 
!> @link DECLARATIONS_SISYPHE::SISPRE SISPRE@endlink, 
!> @link DECLARATIONS_SISYPHE::SISREF SISREF@endlink, 
!> @link DECLARATIONS_SISYPHE::SISRES SISRES@endlink, 
!> @link DECLARATIONS_SISYPHE::SISSEC SISSEC@endlink, 
!> @link DECLARATIONS_SISYPHE::SISSEO SISSEO@endlink, 
!> @link DECLARATIONS_SISYPHE::SIS_FILES SIS_FILES@endlink, 
!> @link DECLARATIONS_SISYPHE::SLIDE SLIDE@endlink, 
!> @link DECLARATIONS_SISYPHE::SLOPEFF SLOPEFF@endlink, 
!> @link DECLARATIONS_SISYPHE::SLVSED SLVSED@endlink, 
!> @link DECLARATIONS_SISYPHE::SLVTRA SLVTRA@endlink, 
!> @link DECLARATIONS_SISYPHE::SORIMP SORIMP@endlink, 
!> @link DECLARATIONS_SISYPHE::SORLEO SORLEO@endlink, 
!> @link DECLARATIONS_SISYPHE::SORTIS SORTIS@endlink, 
!> @link DECLARATIONS_SISYPHE::SPHERI SPHERI@endlink, 
!> @link DECLARATIONS_SISYPHE::STDGEO STDGEO@endlink, 
!> @link DECLARATIONS_SISYPHE::SUSP SUSP@endlink, 
!> @link DECLARATIONS_SISYPHE::TASS TASS@endlink, 
!> @link DECLARATIONS_SISYPHE::TETA TETA@endlink, 
!> @link DECLARATIONS_SISYPHE::TETA_SUSP TETA_SUSP@endlink, 
!> @link DECLARATIONS_SISYPHE::TEXTE TEXTE@endlink, 
!> @link DECLARATIONS_SISYPHE::TEXTPR TEXTPR@endlink, 
!> @link DECLARATIONS_SISYPHE::TITCA TITCA@endlink, 
!> @link DECLARATIONS_SISYPHE::TOCE_VASE TOCE_VASE@endlink, 
!> @link DECLARATIONS_SISYPHE::TPREC TPREC@endlink, 
!> @link DECLARATIONS_SISYPHE::TRANS_MASS TRANS_MASS@endlink, 
!> @link DECLARATIONS_SISYPHE::UNIT UNIT@endlink, 
!> @link DECLARATIONS_SISYPHE::VALID VALID@endlink, 
!> @link DECLARATIONS_SISYPHE::VARIM VARIM@endlink, 
!> @link DECLARATIONS_SISYPHE::VCE VCE@endlink, 
!> @link DECLARATIONS_SISYPHE::VF VF@endlink, 
!> @link DECLARATIONS_SISYPHE::VITCD VITCD@endlink, 
!> @link DECLARATIONS_SISYPHE::VITCE VITCE@endlink, 
!> @link DECLARATIONS_SISYPHE::XKV XKV@endlink, 
!> @link DECLARATIONS_SISYPHE::XKX XKX@endlink, 
!> @link DECLARATIONS_SISYPHE::XKY XKY@endlink, 
!> @link DECLARATIONS_SISYPHE::XMVE XMVE@endlink, 
!> @link DECLARATIONS_SISYPHE::XMVS XMVS@endlink, 
!> @link DECLARATIONS_SISYPHE::XWC XWC@endlink, 
!> @link DECLARATIONS_SISYPHE::ZERO ZERO@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ADRESS, DIMENS, DOC, EFFPEN, ERR, I, K, MOTCLE, MOTINT, MOTLOG, MOTREA, NMAX, NOM_CAS, NOM_DIC, SUMAVAI, TROUVE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> DAMOCLE(), MAJUS(), NOMVAR_SISYPHE(), PLANTE(), READ_SUBMIT(), SORTIE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>HOMERE_SISYPHE(), HOMERE_TELEMAC2D(), HOMERE_TELEMAC3D()

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
!> </td><td> 03/11/2009
!> </td><td> E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 09/04/2009
!> </td><td> BD+JMH
!> </td><td> MED FORMAT
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 23/12/2008
!> </td><td> JMH
!> </td><td> KEYWORDS FOR COUPLING WITH DREDGESIM
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 17/10/2008
!> </td><td> JMH
!> </td><td> CHECKS NCSIZE (FOR CONSISTENCY WITH TELEMAC-2D WHEN COUPLING)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 29/07/2008
!> </td><td> CV+JMH
!> </td><td> READS CBOR_CLASSE
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 11/04/2008
!> </td><td> JMH
!> </td><td> DEBUG IS A KEYWORD: DEBUGGER
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/06
!> </td><td> CV
!> </td><td> ADDED NEW KEYWROD: TASS
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/12/2003
!> </td><td> F. HUVELIN
!> </td><td> * INITIALISES F90 TO FDM IF NOT IN THE STEERING FILE
!>          (SEE TEXT : CFH DEC 2003 DEBUT MOD. F90)
!> <br>      * LOGICAL CALWC BECOMES AN INTERNAL VARIABLE
!> <br>      IF CALWC = T, FALL VELOCITIES ARE SPECIFIED BY THE USER
!> <br>      IF CALWC = F, SISYPHE COMPUTES THE FALL VELOCITIES
!> <br>     (SEE TEXT : CFH DEC 2003 DEBUT MOD. CALWC)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/10/2003
!> </td><td> C.VILLARET
!> </td><td> * READS KFROT, HOULE
!> <br>      * CSF = CONCENTRATION AT THE BED (% VOLUME)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 2002
!> </td><td> M. GONZALES DE LINARES
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
!>          <tr><td>MOTCAR
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
                        SUBROUTINE LECDON_SISYPHE
     &(MOTCAR,FILE_DESC,PATH,NCAR,CODE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CODE           |---| 
C| FILE_DESC      |---| 
C| MOTCAR         |---| 
C| NCAR           |---| 
C| PATH           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
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
C     N1            = MOTINT( ADRESS(1,  5) )
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
C     ******        = MOTINT( ADRESS(1, 22) )
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
      NCOUCH_TASS = MOTINT( ADRESS(1,45)   )
C CV V6P0
      ICR  =   MOTINT(ADRESS(1,46)   )
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
      CSF_VASE    = MOTREA( ADRESS(2, 29) )
C
C     INITIAL CONCENTRATIONS
C
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
      IF(DIMENS(2,32).GT.0) THEN
        DO K=1,DIMENS(2,32)
          CONC_VASE(K)=MOTREA( ADRESS(2,32) + K-1 )
        ENDDO
      ENDIF
      IF(DIMENS(2,33).GT.0) THEN
        DO K=1,DIMENS(2,33)
          TRANS_MASS(K)=MOTREA( ADRESS(2,33) + K-1 )
        ENDDO
      ENDIF
      IF(DIMENS(2,34).GT.0) THEN
        DO K=1,DIMENS(2,34)
          TOCE_VASE(K)=MOTREA( ADRESS(2,34) + K-1 )
        ENDDO
      ENDIF
C
CV V6P0: 20/07/2009
C
      VITCE= MOTREA( ADRESS(2,35))
C IF MULTI-LAYER CONSOLIDATION MODEL: USE THE VALUE FOR THE FIRST LAYER
C SEE END
      VITCD= MOTREA( ADRESS(2,36))
      PARTHENIADES = MOTREA( ADRESS(2,37))
C
C CONVERTS TO  M/S
C
       PARTHENIADES = PARTHENIADES/XMVS
C
C END MODIFICATION CV 20/07
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
      UNIT         = MOTLOG( ADRESS(3, 17) )
C     NOEQUBED     = MOTLOG( ADRESS(3,252) )
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
C CV 06/06/2008
      TASS = MOTLOG(ADRESS(3,23))
      MIXTE=MOTLOG(ADRESS(3,24))
C COUPLING WITH DREDGESIM
      DREDGESIM=MOTLOG(ADRESS(3,25))
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
C
C  NPRIV MOFIFIED FOR OUTPUT OF USER-BUILT VARIABLES
      CALL NOMVAR_SISYPHE(TEXTE,TEXTPR,MNEMO,NSICLA,UNIT)
      CALL SORTIE(SORTIS , MNEMO , MAXVAR , SORLEO )
      CALL SORTIE(VARIM  , MNEMO , MAXVAR , SORIMP )
      DO I = 1, 4
         IF ((NPRIV.LT.I).AND.
     &       (SORLEO(I+26+(NOMBLAY+4)*NSICLA+NOMBLAY).OR.
     &        SORIMP(I+26+(NOMBLAY+4)*NSICLA+NOMBLAY))) THEN
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
      IF(.NOT.CHARR) THEN
        SORLEO(21+(NOMBLAY+2)*NSICLA)=.FALSE.
        SORLEO(22+(NOMBLAY+2)*NSICLA)=.FALSE.
        SORLEO(23+(NOMBLAY+2)*NSICLA)=.FALSE.
        SORIMP(21+(NOMBLAY+2)*NSICLA)=.FALSE.
        SORIMP(22+(NOMBLAY+2)*NSICLA)=.FALSE.
        SORIMP(23+(NOMBLAY+2)*NSICLA)=.FALSE.
      ENDIF
C
C-----------------------------------------------------------------------
C
C     CV ... IF CANNOT FIND ANY BETTER (MOVED HERE BY JMH, IT WAS AT THE END)
      IF(TASS) NOMBLAY=NCOUCH_TASS
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
C
      IF(TASS) THEN
        VITCE = SQRT(TOCE_VASE(1)/XMVE)
        CSF_VASE = CONC_VASE(1)/XMVS
      ENDIF
C
      IF(MIXTE) THEN
C       FILLS VOIDS WITH MUD
        CSF_SABLE= 1.D0
      ELSE
C       VOID INDICES TAKEN INTO ACCOUNT
        CSF_SABLE= 1.D0/XKV
      ENDIF
C
      IF((.NOT.MIXTE).AND.SEDCO(1)) THEN
        CHARR=.FALSE.
        ! SUSP=.TRUE.
      ENDIF
C
      IF(NOMBLAY.GT.NLAYMAX) THEN
        WRITE (LU,*) 'NUMBER OF BED LOAD MODEL LAYERS LARGER THAN '
        WRITE (LU,*) 'THE MAXIMUM PROGRAMMED VALUE OF ', NLAYMAX
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(NOMBLAY.LT.2) THEN
        WRITE (LU,*) 'BEWARE: NUMBER OF BED LOAD MODEL LAYERS'
        WRITE (LU,*) '======= LOWER THAN THE DEFAULT VALUE OF 2'
      ENDIF
C
C----------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C