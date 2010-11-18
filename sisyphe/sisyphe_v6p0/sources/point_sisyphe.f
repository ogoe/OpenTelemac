C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ALLOCATES STRUCTURES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_SISYPHE :<br>
!> @link DECLARATIONS_SISYPHE::ACLADM ACLADM@endlink, 
!> @link DECLARATIONS_SISYPHE::AFBOR AFBOR@endlink, 
!> @link DECLARATIONS_SISYPHE::AM1_S AM1_S@endlink, 
!> @link DECLARATIONS_SISYPHE::AM2_S AM2_S@endlink, 
!> @link DECLARATIONS_SISYPHE::AVAI AVAI@endlink, 
!> @link DECLARATIONS_SISYPHE::AVAIL AVAIL@endlink, 
!> @link DECLARATIONS_SISYPHE::BFBOR BFBOR@endlink, 
!> @link DECLARATIONS_SISYPHE::BOUNDARY_COLOUR BOUNDARY_COLOUR@endlink, 
!> @link DECLARATIONS_SISYPHE::BREACH BREACH@endlink, 
!> @link DECLARATIONS_SISYPHE::CALFA CALFA@endlink, 
!> @link DECLARATIONS_SISYPHE::CBOR CBOR@endlink, 
!> @link DECLARATIONS_SISYPHE::CF CF@endlink, 
!> @link DECLARATIONS_SISYPHE::CHESTR CHESTR@endlink, 
!> @link DECLARATIONS_SISYPHE::CLT CLT@endlink, 
!> @link DECLARATIONS_SISYPHE::CLU CLU@endlink, 
!> @link DECLARATIONS_SISYPHE::CLV CLV@endlink, 
!> @link DECLARATIONS_SISYPHE::COEFPN COEFPN@endlink, 
!> @link DECLARATIONS_SISYPHE::CS CS@endlink, 
!> @link DECLARATIONS_SISYPHE::CST CST@endlink, 
!> @link DECLARATIONS_SISYPHE::CSTAEQ CSTAEQ@endlink, 
!> @link DECLARATIONS_SISYPHE::CTILD CTILD@endlink, 
!> @link DECLARATIONS_SISYPHE::DEL_QU DEL_QU@endlink, 
!> @link DECLARATIONS_SISYPHE::DEL_QV DEL_QV@endlink, 
!> @link DECLARATIONS_SISYPHE::DEL_Z DEL_Z@endlink, 
!> @link DECLARATIONS_SISYPHE::DISP DISP@endlink, 
!> @link DECLARATIONS_SISYPHE::DISP_C DISP_C@endlink, 
!> @link DECLARATIONS_SISYPHE::DZF_GF DZF_GF@endlink, 
!> @link DECLARATIONS_SISYPHE::E E@endlink, 
!> @link DECLARATIONS_SISYPHE::EBOR EBOR@endlink, 
!> @link DECLARATIONS_SISYPHE::ECPL ECPL@endlink, 
!> @link DECLARATIONS_SISYPHE::ELAY ELAY@endlink, 
!> @link DECLARATIONS_SISYPHE::EMAX EMAX@endlink, 
!> @link DECLARATIONS_SISYPHE::EQUA EQUA@endlink, 
!> @link DECLARATIONS_SISYPHE::ES ES@endlink, 
!> @link DECLARATIONS_SISYPHE::ESOMT ESOMT@endlink, 
!> @link DECLARATIONS_SISYPHE::ESTRAT ESTRAT@endlink, 
!> @link DECLARATIONS_SISYPHE::FLBOR FLBOR@endlink, 
!> @link DECLARATIONS_SISYPHE::FLBORTRA FLBORTRA@endlink, 
!> @link DECLARATIONS_SISYPHE::FLBOR_SIS FLBOR_SIS@endlink, 
!> @link DECLARATIONS_SISYPHE::FLUDP FLUDP@endlink, 
!> @link DECLARATIONS_SISYPHE::FLUDPT FLUDPT@endlink, 
!> @link DECLARATIONS_SISYPHE::FLUER FLUER@endlink, 
!> @link DECLARATIONS_SISYPHE::FLUERT FLUERT@endlink, 
!> @link DECLARATIONS_SISYPHE::FLUER_VASE FLUER_VASE@endlink, 
!> @link DECLARATIONS_SISYPHE::FW FW@endlink, 
!> @link DECLARATIONS_SISYPHE::HCLIP HCLIP@endlink, 
!> @link DECLARATIONS_SISYPHE::HCPL HCPL@endlink, 
!> @link DECLARATIONS_SISYPHE::HIDING HIDING@endlink, 
!> @link DECLARATIONS_SISYPHE::HN HN@endlink, 
!> @link DECLARATIONS_SISYPHE::HPROP HPROP@endlink, 
!> @link DECLARATIONS_SISYPHE::HW HW@endlink, 
!> @link DECLARATIONS_SISYPHE::IELMH_SIS IELMH_SIS@endlink, 
!> @link DECLARATIONS_SISYPHE::IELMT IELMT@endlink, 
!> @link DECLARATIONS_SISYPHE::IFAMAS IFAMAS@endlink, 
!> @link DECLARATIONS_SISYPHE::INDIC INDIC@endlink, 
!> @link DECLARATIONS_SISYPHE::IT1 IT1@endlink, 
!> @link DECLARATIONS_SISYPHE::IT2 IT2@endlink, 
!> @link DECLARATIONS_SISYPHE::IT3 IT3@endlink, 
!> @link DECLARATIONS_SISYPHE::IT4 IT4@endlink, 
!> @link DECLARATIONS_SISYPHE::KS KS@endlink, 
!> @link DECLARATIONS_SISYPHE::KSP KSP@endlink, 
!> @link DECLARATIONS_SISYPHE::KSR KSR@endlink, 
!> @link DECLARATIONS_SISYPHE::KX KX@endlink, 
!> @link DECLARATIONS_SISYPHE::KY KY@endlink, 
!> @link DECLARATIONS_SISYPHE::KZ KZ@endlink, 
!> @link DECLARATIONS_SISYPHE::LAYTHI LAYTHI@endlink, 
!> @link DECLARATIONS_SISYPHE::LICBOR LICBOR@endlink, 
!> @link DECLARATIONS_SISYPHE::LIEBOR LIEBOR@endlink, 
!> @link DECLARATIONS_SISYPHE::LIHBOR LIHBOR@endlink, 
!> @link DECLARATIONS_SISYPHE::LIMDIF LIMDIF@endlink, 
!> @link DECLARATIONS_SISYPHE::LIMPRO LIMPRO@endlink, 
!> @link DECLARATIONS_SISYPHE::LIMTEC LIMTEC@endlink, 
!> @link DECLARATIONS_SISYPHE::LIQBOR LIQBOR@endlink, 
!> @link DECLARATIONS_SISYPHE::MASK MASK@endlink, 
!> @link DECLARATIONS_SISYPHE::MASKB MASKB@endlink, 
!> @link DECLARATIONS_SISYPHE::MASKEL MASKEL@endlink, 
!> @link DECLARATIONS_SISYPHE::MASKPT MASKPT@endlink, 
!> @link DECLARATIONS_SISYPHE::MASKTR MASKTR@endlink, 
!> @link DECLARATIONS_SISYPHE::MBOR MBOR@endlink, 
!> @link DECLARATIONS_SISYPHE::MESH MESH@endlink, 
!> @link DECLARATIONS_SISYPHE::MSK MSK@endlink, 
!> @link DECLARATIONS_SISYPHE::MSKTMP MSKTMP@endlink, 
!> @link DECLARATIONS_SISYPHE::MS_SABLE MS_SABLE@endlink, 
!> @link DECLARATIONS_SISYPHE::MS_VASE MS_VASE@endlink, 
!> @link DECLARATIONS_SISYPHE::MU MU@endlink, 
!> @link DECLARATIONS_SISYPHE::NCP NCP@endlink, 
!> @link DECLARATIONS_SISYPHE::NLAYER NLAYER@endlink, 
!> @link DECLARATIONS_SISYPHE::NOMBLAY NOMBLAY@endlink, 
!> @link DECLARATIONS_SISYPHE::NPRIV NPRIV@endlink, 
!> @link DECLARATIONS_SISYPHE::NSICLA NSICLA@endlink, 
!> @link DECLARATIONS_SISYPHE::NUMLIQ NUMLIQ@endlink, 
!> @link DECLARATIONS_SISYPHE::NVARCL NVARCL@endlink, 
!> @link DECLARATIONS_SISYPHE::OPTASS OPTASS@endlink, 
!> @link DECLARATIONS_SISYPHE::PRIVE PRIVE@endlink, 
!> @link DECLARATIONS_SISYPHE::PRODUC PRODUC@endlink, 
!> @link DECLARATIONS_SISYPHE::QBOR QBOR@endlink, 
!> @link DECLARATIONS_SISYPHE::QS QS@endlink, 
!> @link DECLARATIONS_SISYPHE::QSCL QSCL@endlink, 
!> @link DECLARATIONS_SISYPHE::QSCLX QSCLX@endlink, 
!> @link DECLARATIONS_SISYPHE::QSCLXC QSCLXC@endlink, 
!> @link DECLARATIONS_SISYPHE::QSCLXS QSCLXS@endlink, 
!> @link DECLARATIONS_SISYPHE::QSCLY QSCLY@endlink, 
!> @link DECLARATIONS_SISYPHE::QSCLYC QSCLYC@endlink, 
!> @link DECLARATIONS_SISYPHE::QSCLYS QSCLYS@endlink, 
!> @link DECLARATIONS_SISYPHE::QSCL_C QSCL_C@endlink, 
!> @link DECLARATIONS_SISYPHE::QSCL_S QSCL_S@endlink, 
!> @link DECLARATIONS_SISYPHE::QSX QSX@endlink, 
!> @link DECLARATIONS_SISYPHE::QSXC QSXC@endlink, 
!> @link DECLARATIONS_SISYPHE::QSXS QSXS@endlink, 
!> @link DECLARATIONS_SISYPHE::QSY QSY@endlink, 
!> @link DECLARATIONS_SISYPHE::QSYC QSYC@endlink, 
!> @link DECLARATIONS_SISYPHE::QSYS QSYS@endlink, 
!> @link DECLARATIONS_SISYPHE::QS_C QS_C@endlink, 
!> @link DECLARATIONS_SISYPHE::QS_S QS_S@endlink, 
!> @link DECLARATIONS_SISYPHE::QU QU@endlink, 
!> @link DECLARATIONS_SISYPHE::QV QV@endlink, 
!> @link DECLARATIONS_SISYPHE::S S@endlink, 
!> @link DECLARATIONS_SISYPHE::SALFA SALFA@endlink, 
!> @link DECLARATIONS_SISYPHE::SEDCO SEDCO@endlink, 
!> @link DECLARATIONS_SISYPHE::SISSEC SISSEC@endlink, 
!> @link DECLARATIONS_SISYPHE::SIS_FILES SIS_FILES@endlink, 
!> @link DECLARATIONS_SISYPHE::SLVSED SLVSED@endlink, 
!> @link DECLARATIONS_SISYPHE::SLVTRA SLVTRA@endlink, 
!> @link DECLARATIONS_SISYPHE::SORLEO SORLEO@endlink, 
!> @link DECLARATIONS_SISYPHE::SPHERI SPHERI@endlink, 
!> @link DECLARATIONS_SISYPHE::TB TB@endlink, 
!> @link DECLARATIONS_SISYPHE::TE1 TE1@endlink, 
!> @link DECLARATIONS_SISYPHE::TE2 TE2@endlink, 
!> @link DECLARATIONS_SISYPHE::TE3 TE3@endlink, 
!> @link DECLARATIONS_SISYPHE::THETAW THETAW@endlink, 
!> @link DECLARATIONS_SISYPHE::TOB TOB@endlink, 
!> @link DECLARATIONS_SISYPHE::TOBW TOBW@endlink, 
!> @link DECLARATIONS_SISYPHE::TOCE_MIXTE TOCE_MIXTE@endlink, 
!> @link DECLARATIONS_SISYPHE::TW TW@endlink, 
!> @link DECLARATIONS_SISYPHE::U2D U2D@endlink, 
!> @link DECLARATIONS_SISYPHE::UCONV UCONV@endlink, 
!> @link DECLARATIONS_SISYPHE::UNLADM UNLADM@endlink, 
!> @link DECLARATIONS_SISYPHE::UNORM UNORM@endlink, 
!> @link DECLARATIONS_SISYPHE::UNSV2D UNSV2D@endlink, 
!> @link DECLARATIONS_SISYPHE::UW UW@endlink, 
!> @link DECLARATIONS_SISYPHE::V2D V2D@endlink, 
!> @link DECLARATIONS_SISYPHE::V2DPAR V2DPAR@endlink, 
!> @link DECLARATIONS_SISYPHE::VARCL VARCL@endlink, 
!> @link DECLARATIONS_SISYPHE::VARSOR VARSOR@endlink, 
!> @link DECLARATIONS_SISYPHE::VCONV VCONV@endlink, 
!> @link DECLARATIONS_SISYPHE::VF VF@endlink, 
!> @link DECLARATIONS_SISYPHE::VOLU2D VOLU2D@endlink, 
!> @link DECLARATIONS_SISYPHE::W1 W1@endlink, 
!> @link DECLARATIONS_SISYPHE::Z Z@endlink, 
!> @link DECLARATIONS_SISYPHE::ZF ZF@endlink, 
!> @link DECLARATIONS_SISYPHE::ZFCL ZFCL@endlink, 
!> @link DECLARATIONS_SISYPHE::ZFCL_C ZFCL_C@endlink, 
!> @link DECLARATIONS_SISYPHE::ZFCL_S ZFCL_S@endlink, 
!> @link DECLARATIONS_SISYPHE::ZF_C ZF_C@endlink, 
!> @link DECLARATIONS_SISYPHE::ZF_S ZF_S@endlink, 
!> @link DECLARATIONS_SISYPHE::ZR ZR@endlink, 
!> @link DECLARATIONS_SISYPHE::ZREF ZREF@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CFG, CFGBOR, I, IELBT, IELM0, IELM0_SUB, IELM1, K, NTR
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> DIM, IKLE, LV, MXELVS, MXPTVS, NELEM, NELMAX, NPMAX, NPOIN, NPTFR, NPTFRX, T1, T10, T11, T12, T13, T14, T2, T3, T4, T5, T6, T7, T8, T9, TYPELM, X, Y
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ADDBLO(), ALLBLO(), ALLMAT(), ALLVEC(), ALLVEC_IN_BLOCK(), ALMESH(), IELBOR(), NBFEL(), OS(), Q(), READ_SECTIONS_SISYPHE()
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
!> </td><td> 19/08/2010
!> </td><td> JMH
!> </td><td> SEE MS_VASE (FOR MIXED SEDIMENTS)
!> </td></tr>
!>      <tr>
!>      <td><center> 6.0                                       </center>
!> </td><td> 18/09/2009
!> </td><td> JMH
!> </td><td> SEE AVAI AND LAYTHI
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 16/09/2009
!> </td><td> JMH
!> </td><td> AVAIL(NPOIN,10,NSICLA)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 16/06/2008
!> </td><td> JMH
!> </td><td> ADDED BOUNDARY_COLOUR
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 10/06/2002
!> </td><td> C. MACHET
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 11/09/1995
!> </td><td> C. LENORMANT; J.-M. HERVOUET
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE POINT_SISYPHE
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_SISYPHE
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU


      ! 2/ LOCAL VARIABLES
      ! ------------------
      INTEGER :: I,K,NTR,IELM0,IELM1,IELBT,IELM0_SUB
      INTEGER :: CFG(2),CFGBOR(2)

C-----------------------------------------------------------------------
C
C-----------------------------------------------------------------------

      IF (LNG == 1) WRITE(LU,11)
      IF (LNG == 2) WRITE(LU,12)

      ! ************************************** !
      ! I - DISCRETISATION AND TYPE OF STORAGE !
      ! ************************************** !
      ! IELMT, IELMH_SIS AND IELMU_SIS HARD-CODED IN LECDON
      IELM0     = 10
      IELM1     = 11
      IELBT     = IELBOR(IELMT,1)
      IELM0_SUB = 10*(IELMT/10)

      CFG(1)    = OPTASS
      CFG(2)    = PRODUC
      CFGBOR(1) = 1 ! CFG IMPOSED FOR BOUNDARY MATRICES
      CFGBOR(2) = 1 ! CFG IMPOSED FOR BOUNDARY MATRICES

      IF(VF) EQUA(1:15)='SAINT-VENANT VF'

      ! ******************************************* !
      ! II - ALLOCATES THE MESH STRUCTURE           !
      ! ******************************************* !
      CALL ALMESH(MESH,'MESH_S',IELMT,SPHERI,CFG,
     &            SIS_FILES(SISGEO)%LU,EQUA)

      IKLE  => MESH%IKLE
      X     => MESH%X%R
      Y     => MESH%Y%R
      NELEM => MESH%NELEM
      NELMAX=> MESH%NELMAX
      NPTFR => MESH%NPTFR
      NPTFRX=> MESH%NPTFRX
      DIM   => MESH%DIM
      TYPELM=> MESH%TYPELM
      NPOIN => MESH%NPOIN
      NPMAX => MESH%NPMAX
      MXPTVS=> MESH%MXPTVS
      MXELVS=> MESH%MXELVS
      LV    => MESH%LV


      ! ******************** !
      ! III - REAL ARRAYS    !
      ! ******************** !
      CALL ALLVEC(1, S     , 'S     ', 0    , 1, 1) ! VOID STRUCTURE
!
      CALL ALLVEC(1, E     , 'E     ', IELMT, 1, 2) ! RESULT
      CALL ALLVEC(1, Z     , 'Z     ', IELMT, 1, 2) ! RESULT
      CALL ALLVEC(1, DEL_Z , 'DEL_Z ', IELMT, 1, 2) ! INCREMENT OF Z IF HYDRO
      CALL ALLVEC(1, ZF_C  , 'ZF_C  ', IELMT, 1, 2) ! VARIABLES E SUMMED UP
      CALL ALLVEC(1, ZF_S  , 'ZF_S  ', IELMT, 1, 2) ! VARIABLES E SUMMED U
      CALL ALLVEC(1, ESOMT , 'ESOMT ', IELMT, 1, 2) ! VARIABLES E SUMMED U
      CALL ALLVEC(1, EMAX  , 'EMAX  ', IELMT, 1, 2) ! VARIABLES E SUMMED U
      CALL ALLVEC(1, Q     , 'Q     ', IELMT, 1, 2) ! FLOWRATE
      CALL ALLVEC(1, QU    , 'QU    ', IELMT, 1, 2) ! X FLOWRATE
      CALL ALLVEC(1, QV    , 'QV    ', IELMT, 1, 2) ! Y FLOWRATE
      CALL ALLVEC(1, DEL_QU, 'DEL_QU', IELMT, 1, 2) ! INCREMENT OF QU IF HYDRO
      CALL ALLVEC(1, DEL_QV, 'DEL_QV', IELMT, 1, 2) ! INCREMENT OF QV IF HYDRO
      CALL ALLVEC(1, U2D   , 'U2D   ', IELMT, 1, 2) ! X VELOCITY
      CALL ALLVEC(1, V2D   , 'V2D   ', IELMT, 1, 2) ! Y VELOCITY
      CALL ALLVEC(1, QS    , 'QS    ', IELMT, 1, 2) ! TRANSPORT RATE
      CALL ALLVEC(1, QSX   , 'QSX   ', IELMT, 1, 2) ! X TRANSPORT RATE
      CALL ALLVEC(1, QSY   , 'QSY   ', IELMT, 1, 2) ! Y TRANSPORT RATE
      CALL ALLVEC(1, QS_C  , 'QS_C  ', IELMT, 1, 2) ! BEDLOAD RATE
      CALL ALLVEC(1, QSXC  , 'QSXC  ', IELMT, 1, 2) ! X BEDLOAD RATE
      CALL ALLVEC(1, QSYC  , 'QSYC  ', IELMT, 1, 2) ! Y BEDLOAD RATE
      CALL ALLVEC(1, QS_S  , 'QS_S  ', IELMT, 1, 2) ! SUSPENSION RATE
      CALL ALLVEC(1, QSXS  , 'QSXS  ', IELMT, 1, 2) ! X SUSPENSION RATE
      CALL ALLVEC(1, QSYS  , 'QSYS  ', IELMT, 1, 2) ! Y SUSPENSION RATE
      CALL ALLVEC(1, HIDING, 'HIDING', IELMT, 1, 2) ! HIDING FACTOR
      CALL ALLVEC(1, ZF    , 'ZF    ', IELMT, 1, 2) ! BED ELEVATIONS
      CALL ALLVEC(1, ZR    , 'ZR    ', IELMT, 1, 2) ! NON-ERODABLE BED ELEVATIONS
      CALL ALLVEC(1, ZREF  , 'ZREF  ', IELMT, 1, 2) ! REFERENCE ELEVATION
      CALL ALLVEC(1, CHESTR, 'CHESTR', IELMT, 1, 2) ! FRICTION COEFFICIENT
      CALL ALLVEC(1, COEFPN, 'COEFPN', IELMT, 1, 2) ! SLOPE EFFECT
      CALL ALLVEC(1, CALFA , 'CALFA ', IELMT, 1, 2)
      CALL ALLVEC(1, SALFA , 'SALFA ', IELMT, 1, 2)
      CALL ALLVEC(1, CF    , 'CF    ', IELMT, 1, 2) ! ADIMENSIONAL FRICTION
      CALL ALLVEC(1, TOB   , 'TOB   ', IELMT, 1, 2) ! TOTAL FRICTION
      CALL ALLVEC(1, TOBW  , 'TOBW  ', IELMT, 1, 2) ! WAVE VARIABLE
      CALL ALLVEC(1, MU    , 'MU    ', IELMT, 1, 2) ! SKIN FRICTION
      CALL ALLVEC(1, KSP   , 'KSP   ', IELMT, 1, 2) ! SKIN ROUGHNESS
      CALL ALLVEC(1, KS    , 'KS    ', IELMT, 1, 2) ! TOTAL ROUGHNESS
      CALL ALLVEC(1, KSR   , 'KSR   ', IELMT, 1, 2) ! RIPPLE INDUCED ROUGHNESS
      CALL ALLVEC(1, THETAW, 'THETAW', IELMT, 1, 2) ! WAVE VARIABLE
      CALL ALLVEC(1, FW    , 'FW    ', IELMT, 1, 2) ! WAVE VARIABLE
      CALL ALLVEC(1, UW    , 'UW    ', IELMT, 1, 2) ! WAVE VARIABLE
      CALL ALLVEC(1, HW    , 'HW    ', IELMT, 1, 2)
      CALL ALLVEC(1, TW    , 'TW    ', IELMT, 1, 2)
      CALL ALLVEC(1, DZF_GF, 'DZF_GF', IELMT, 1, 2) ! BED LEVEL CHANGE FOR GRAIN-FEEDING
      CALL ALLVEC(1, ACLADM, 'ACLADM', IELMT, 1, 2) ! MEAN DIAMETER IN ACTIVE LAYER
      CALL ALLVEC(1, UNLADM, 'UNLADM', IELMT, 1, 2) ! MEAN DIAMETER IN 2ND LAYER
      CALL ALLVEC(1, HCPL  , 'HCPL  ', IELMT, 1, 2) ! WATER DEPTH SAVED FOR CONSTANT FLOW DISCHARGE
      CALL ALLVEC(1, ECPL  , 'ECPL  ', IELMT, 1, 2) ! EVOLUTION SAVED FOR CONSTANT FLOW DISCHARGE
      CALL ALLVEC(1, ELAY  , 'ELAY  ', IELMT, 1, 2) ! ACTIVE LAYER THICKNESS
      CALL ALLVEC(1, ESTRAT, 'ESTRAT', IELMT, 1, 2) ! 2ND LAYER THICKNESS
      CALL ALLVEC(1, KX    , 'KX    ', IELMT, 1, 1)
      CALL ALLVEC(1, KY    , 'KY    ', IELMT, 1, 1)
      CALL ALLVEC(1, KZ    , 'KZ    ', IELMT, 1, 1)
      CALL ALLVEC(1, UCONV , 'UCONV ', IELMT, 1, 1)
      CALL ALLVEC(1, VCONV , 'VCONV ', IELMT, 1, 1)
      CALL ALLVEC(1, UNORM , 'UNORM ', IELMT, 1, 2)
      CALL ALLVEC(1, DISP  , 'DISP  ', IELMT, 3, 1)
      CALL ALLVEC(1, DISP_C, 'DISP_C', IELMT, 3, 1)
      CALL ALLVEC(1, MASKB , 'MASKB ', IELM0, 1, 2)
      CALL ALLVEC(1, MASK  , 'MASK  ', IELBT, 1, 2)
      CALL ALLVEC(1, AFBOR , 'AFBOR ', IELBT, 1, 1)
      CALL ALLVEC(1, BFBOR , 'BFBOR ', IELBT, 1, 1)
      CALL ALLVEC(1, FLBOR , 'FLBOR ', IELBT, 1, 1)
C     BOUNDARY FLUX FOR CALL TO CVDFTR
      CALL ALLVEC(1, FLBOR_SIS , 'FLBORS', IELBT, 1, 1)
      CALL ALLVEC(1, FLBORTRA  , 'FLBTRA', IELBT, 1, 1)
      CALL ALLVEC(1, CSTAEQ, 'CSTAEQ', IELMT, 1, 2)
      CALL ALLVEC(1, HN    , 'HN    ', IELMH_SIS, 1, 2) ! WATER DEPTH
      CALL ALLVEC(1, HCLIP , 'HCLIP ', IELMH_SIS, 1, 2) ! CLIPPING WATER DEPTH
      CALL ALLVEC(1, HPROP , 'HPROP ', IELMH_SIS, 1, 1)
      CALL ALLVEC(1, VOLU2D, 'VOLU2D', IELMH_SIS, 1, 1)
      CALL ALLVEC(1, V2DPAR, 'V2DPAR', IELMH_SIS, 1, 1)
      CALL ALLVEC(1, UNSV2D, 'UNSV2D', IELMH_SIS, 1, 1)
!
      IF(MSK) THEN
        CALL ALLVEC(1,MASKEL,'MASKEL', IELM0 , 1 , 2 )
        CALL ALLVEC(1,MSKTMP,'MSKTMP', IELM0 , 1 , 2 )
        CALL ALLVEC(1,MASKPT,'MASKPT', IELMT , 1 , 2 )
      ELSE
        CALL ALLVEC(1,MASKEL,'MASKEL', 0 , 1 , 0 )
        CALL ALLVEC(1,MSKTMP,'MSKTMP', 0 , 1 , 0 )
        CALL ALLVEC(1,MASKPT,'MASKPT', 0 , 1 , 0 )
      ENDIF
!
C     FOR MIXED SEDIMENTS
!
      IF(SEDCO(1).OR.SEDCO(2)) THEN
!      IF(MIXTE.OR.TASS) THEN
        CALL ALLVEC(1,FLUER_VASE,'FRMIXT',IELMT,1,2)
        CALL ALLVEC(1,TOCE_MIXTE ,'TCMIXT',IELMT,10,2)
        CALL ALLVEC(1,MS_SABLE   ,'MSSABL',IELMT,10,2)
        CALL ALLVEC(1,MS_VASE    ,'MSVASE',IELMT,10,2)
      ELSE
        CALL ALLVEC(1,FLUER_VASE,'FRMIXT',0,1,0)
        CALL ALLVEC(1,TOCE_MIXTE ,'TCMIXT',0,1,0)
        CALL ALLVEC(1,MS_SABLE   ,'MSSABL',0,1,0)
        CALL ALLVEC(1,MS_VASE    ,'MSVASE',0,1,0)
      ENDIF
!
      ! *********************** !
      ! IV - INTEGER ARRAYS     ! (_IMP_)
      ! *********************** !
      CALL ALLVEC(2, LIEBOR, 'LIEBOR', IELBOR(IELM1,1), 1, 1)
      CALL ALLVEC(2, LIQBOR, 'LIQBOR', IELBOR(IELM1,1), 1, 1)
      CALL ALLVEC(2, LIMTEC, 'LIMTEC', IELBOR(IELM1,1), 1, 1)
      CALL ALLVEC(2, NUMLIQ, 'NUMLIQ', IELBOR(IELM1,1), 1, 1)
      CALL ALLVEC(2, CLT   , 'CLT   ', IELBOR(IELMT,1), 1, 1)
      CALL ALLVEC(2, CLU   , 'CLU   ', IELBOR(IELMT,1), 1, 1)
      CALL ALLVEC(2, CLV   , 'CLV   ', IELBOR(IELMT,1), 1, 1)
      CALL ALLVEC(2, LIMDIF, 'LIMDIF', IELBOR(IELMT,1), 1, 1)
      CALL ALLVEC(2, LICBOR, 'LICBOR', IELBOR(IELMT,1), 1, 1)
      CALL ALLVEC(2, LIHBOR, 'LIHBOR', IELBOR(IELMT,1), 1, 1)
      CALL ALLVEC(2, BOUNDARY_COLOUR,
     &                       'BNDCOL', IELBOR(IELMT,1), 1, 1)
      CALL ALLVEC(2, LIMPRO, 'LIMPRO', IELBOR(IELMT,1), 6, 1)
      CALL ALLVEC(2, INDIC , 'INDIC ', IELM1          , 1, 1)
      CALL ALLVEC(2, IT1   , 'IT1   ', IELM1          , 1, 2)
      CALL ALLVEC(2, IT2   , 'IT2   ', IELM1          , 1, 2)
      CALL ALLVEC(2, IT3   , 'IT3   ', IELM1          , 1, 2)
      CALL ALLVEC(2, IT4   , 'IT4   ', IELM1          , 1, 2)
      CALL ALLVEC(2, NLAYER, 'NLAYE ', IELMT          , 1, 2) ! NUMBER OF LAYERS

      IF(VF) THEN
        CALL ALLVEC(2,BREACH,'BREACH',IELM1,1,2)
      ELSE
        CALL ALLVEC(2,BREACH,'BREACH',0,1,0)
      ENDIF

      IF(MSK) THEN
        CALL ALLVEC(2,IFAMAS,'IFAMAS',IELM0,NBFEL(IELM0),1)
      ELSE
        CALL ALLVEC(2,IFAMAS,'IFAMAS',0,1,0)
      ENDIF

      ! ******************* !
      ! V - BLOCK OF ARRAYS !
      ! ******************* !
      ALLOCATE(AVAIL(NPOIN,10,NSICLA)) ! FRACTION OF EACH CLASS FOR EACH LAYER
      ALLOCATE(ES(NPOIN,10))           ! THICKNESS OF EACH CLASS ???

      !================================================================!
      CALL ALLBLO(MASKTR, 'MASKTR') ! MASK OF THE BOUNDARY CONDITIONS
      CALL ALLBLO(EBOR  , 'EBOR  ') ! BOUNDARY CONDITIONS
      CALL ALLBLO(QBOR  , 'QBOR  ') ! BOUNDARY CONDITIONS
      CALL ALLBLO(AVAI  , 'AVAI  ') ! FRACTION OF EACH CLASS FOR THE TWO FIRST LAYERS
      CALL ALLBLO(LAYTHI, 'LAYTHI') ! LAYER THICKNESSES
      !================================================================!
      CALL ALLBLO(QSCL  , 'QSCL  ') ! TRANSPORT RATE FOR EACH CLASS
      CALL ALLBLO(QSCLX , 'QSCLX ') ! TRANSPORT RATE FOR EACH CLASS ALONG X
      CALL ALLBLO(QSCLY , 'QSCLY ') ! TRANSPORT RATE FOR EACH CLASS ALONG Y
      CALL ALLBLO(QSCL_C, 'QSCL_C') ! BEDLOAD TRANSPORT RATE FOR EACH CLASS
      CALL ALLBLO(QSCLXC, 'QSCLXC') ! BEDLOAD TRANSPORT RATE FOR EACH CLASS ALONG X
      CALL ALLBLO(QSCLYC, 'QSCLYC') ! BEDLOAD TRANSPORT RATE FOR EACH CLASS ALONG Y
      CALL ALLBLO(ZFCL  , 'ZFCL  ') ! EVOLUTION FOR EACH CLASS
      CALL ALLBLO(ZFCL_C, 'ZFCL_C') ! EVOLUTION FOR EACH CLASS DUE TO BEDLOAD TRANSPORT
      !================================================================!
      CALL ALLBLO(CBOR  , 'CBOR  ') ! BOUNDARY CONDITIONS
      CALL ALLBLO(QSCL_S, 'QSCL_S') ! SUSPENDED TRANSPORT RATE FOR EACH CLASS
      CALL ALLBLO(QSCLXS, 'QSCLXS') ! SUSPENDED TRANSPORT RATE FOR EACH CLASS ALONG X
      CALL ALLBLO(QSCLYS, 'QSCLYS') ! SUSPENDED TRANSPORT RATE FOR EACH CLASS ALONG Y
      CALL ALLBLO(ZFCL_S, 'ZFCL_S') ! EVOLUTION FOR EACH CLASS DUE TO SUSPENDED TRANSPORT
      CALL ALLBLO(FLUDP , 'FLUDP ') ! DEPOSITION FLUX
      CALL ALLBLO(FLUDPT, 'FLUDPT') ! DEPOSITION FLUX FOR IMPLICITATION
      CALL ALLBLO(FLUER , 'FLUER ') ! EROSION FLUX
      CALL ALLBLO(FLUERT, 'FLUERT') ! EROSION FLUX FOR IMPLICITATION
      CALL ALLBLO(CS    , 'CS    ') ! CONCENTRATION AT TIME N
      CALL ALLBLO(CTILD , 'CTILD ') ! CONCENTRATION AT TIME N+1/2 (=> ADVECTION STEP)
      CALL ALLBLO(CST   , 'CST   ') ! CONCENTRATION AT TIME N+1   (=> RESULT)
      !================================================================!
      !================================================================!
      CALL ALLVEC_IN_BLOCK(MASKTR, 4       , 1, 'MSKTR ', IELBT, 1, 2)
      CALL ALLVEC_IN_BLOCK(EBOR  , NSICLA  , 1, 'EBOR  ', IELBT, 1, 2)
      CALL ALLVEC_IN_BLOCK(QBOR  , NSICLA  , 1, 'QBOR  ', IELBT, 1, 2)
!
C     JMH 18/09/09 AVAI ALLOCATED WITH SIZE 0 AND POINTING TO
C                  RELEVANT SECTIONS OF AVAIL
C     CALL ALLVEC_IN_BLOCK(AVAI,NOMBLAY*NSICLA,1,'AVAI  ',IELMT,1,2)
      CALL ALLVEC_IN_BLOCK(AVAI,NOMBLAY*NSICLA,1,'AVAI  ',    0,1,0)
      DO I=1,NSICLA
        DO K=1,NOMBLAY
          AVAI%ADR(K+(I-1)*NOMBLAY)%P%R=>AVAIL(1:NPOIN,K,I)
          AVAI%ADR(K+(I-1)*NOMBLAY)%P%MAXDIM1=NPOIN
          AVAI%ADR(K+(I-1)*NOMBLAY)%P%DIM1=NPOIN
        ENDDO
      ENDDO
C     LAYTHI ALLOCATED WITH SIZE 0 AND POINTING TO RELEVANT SECTIONS OF ES
C     CALL ALLVEC_IN_BLOCK(LAYTHI,NOMBLAY, 1, 'LAYTHI', IELMT, 1, 2)
      CALL ALLVEC_IN_BLOCK(LAYTHI,NOMBLAY, 1, 'LAYTHI',     0, 1, 0)
      DO K=1,NOMBLAY
        LAYTHI%ADR(K)%P%R=>ES(1:NPOIN,K)
        LAYTHI%ADR(K)%P%MAXDIM1=NPOIN
        LAYTHI%ADR(K)%P%DIM1=NPOIN
      ENDDO
!
      !================================================================!
      CALL ALLVEC_IN_BLOCK(QSCL  , NSICLA  , 1, 'QSCL  ', IELMT, 1, 2)
      CALL ALLVEC_IN_BLOCK(QSCLX , NSICLA  , 1, 'QSCLX ', IELMT, 1, 2)
      CALL ALLVEC_IN_BLOCK(QSCLY , NSICLA  , 1, 'QSCLY ', IELMT, 1, 2)
      CALL ALLVEC_IN_BLOCK(QSCL_C, NSICLA  , 1, 'QSCL_C', IELMT, 1, 2)
      CALL ALLVEC_IN_BLOCK(QSCLXC, NSICLA  , 1, 'QSCLXC', IELMT, 1, 2)
      CALL ALLVEC_IN_BLOCK(QSCLYC, NSICLA  , 1, 'QSCLYC', IELMT, 1, 2)
      CALL ALLVEC_IN_BLOCK(ZFCL  , NSICLA  , 1, 'ZFCL  ', IELMT, 1, 2)
      CALL ALLVEC_IN_BLOCK(ZFCL_C, NSICLA  , 1, 'ZFCL_C', IELMT, 1, 2)
      !================================================================!
      CALL ALLVEC_IN_BLOCK(CBOR  , NSICLA  , 1, 'CBOR  ', IELBT, 1, 2)
      CALL ALLVEC_IN_BLOCK(QSCL_S, NSICLA  , 1, 'QSCL_S', IELMT, 1, 2)
      CALL ALLVEC_IN_BLOCK(QSCLXS, NSICLA  , 1, 'QSCLXS', IELMT, 1, 2)
      CALL ALLVEC_IN_BLOCK(QSCLYS, NSICLA  , 1, 'QSCLYS', IELMT, 1, 2)
      CALL ALLVEC_IN_BLOCK(ZFCL_S, NSICLA  , 1, 'ZFCL_S', IELMT, 1, 2)
      CALL ALLVEC_IN_BLOCK(FLUDP , NSICLA  , 1, 'FLUDP ', IELMT, 1, 2)
      CALL ALLVEC_IN_BLOCK(FLUDPT, NSICLA  , 1, 'FLUDPT', IELMT, 1, 2)
      CALL ALLVEC_IN_BLOCK(FLUER , NSICLA  , 1, 'FLUER ', IELMT, 1, 2)
      CALL ALLVEC_IN_BLOCK(FLUERT, NSICLA  , 1, 'FLUERT', IELMT, 1, 2)
      CALL ALLVEC_IN_BLOCK(CS    , NSICLA  , 1, 'CS    ', IELMT, 1, 2)
      CALL ALLVEC_IN_BLOCK(CTILD , NSICLA  , 1, 'CTILD ', IELMT, 1, 2)
      CALL ALLVEC_IN_BLOCK(CST   , NSICLA  , 1, 'CST   ', IELMT, 1, 2)
      !================================================================!


      ! ************* !
      ! VI - MATRICES !
      ! ************* !


      !================================================================!
      CALL ALLMAT(AM1_S, 'AM1_S ', IELMT, IELMT, CFG   , 'Q', 'Q') ! SUSPENSION WORK MATRIX
      CALL ALLMAT(AM2_S, 'AM2_S ', IELMT, IELMT, CFG   , 'Q', 'Q') ! SUSPENSION WORK MATRIX
      CALL ALLMAT(MBOR , 'MBOR  ', IELBT, IELBT, CFGBOR, 'Q', 'Q') ! SUSPENSION BOUNDRAY MATRIX
      !================================================================!


      ! ****************** !
      ! VII - OTHER ARRAYS !
      ! ****************** !
!
C     NTR SHOULD AT LEAST BE THE NUMBER OF VARIABLES IN VARSOR THAT WILL BE READ IN
C     VALIDA. HERE UP TO THE LAYER THICKNESSES
      NTR   = 26+(NOMBLAY+4)*NSICLA+NOMBLAY+NPRIV
      IF(SLVSED%SLV == 7) NTR = MAX(NTR,2+2*SLVSED%KRYLOV)
      IF(SLVTRA%SLV == 7) NTR = MAX(NTR,2+2*SLVTRA%KRYLOV)
      IF(3*(SLVSED%PRECON/3) == SLVSED%PRECON) NTR = NTR + 2 ! IF PRECOND. BLOC-DIAG (+2 DIAG)
      IF(3*(SLVTRA%PRECON/3) == SLVTRA%PRECON) NTR = NTR + 2 ! IF PRECOND. BLOC-DIAG (+2 DIAG)
!
C     W1 NO LONGER USED (IS SENT TO CVDFTR BUT CVDFTR DOES NOTHING WITH IT)
      CALL ALLVEC(1, W1 , 'W1    ', IELM0    , 1    , 1) ! WORK ARRAY
      CALL ALLVEC(1, TE1, 'TE1   ', IELM0_SUB, 1    , 1) ! WORK ARRAY BY ELEMENT
      CALL ALLVEC(1, TE2, 'TE2   ', IELM0_SUB, 1    , 1) ! WORK ARRAY BY ELEMENT
      CALL ALLVEC(1, TE3, 'TE3   ', IELM0_SUB, 1    , 1) ! WORK ARRAY BY ELEMENT
!
      CALL ALLBLO(VARCL, 'VARCL ') ! CLANDESTINE VARIABLES
      CALL ALLBLO(PRIVE, 'PRIVE ') ! USER ARRAY
      CALL ALLBLO(TB   , 'TB    ') ! WORKING ARRAY
!
      CALL ALLVEC_IN_BLOCK(TB   , NTR   , 1, 'T     ', IELMT, 1, 2)
      CALL ALLVEC_IN_BLOCK(VARCL, NVARCL, 1, 'CL    ', IELMT, 1, 2)
      IF(NPRIV.GT.0) THEN
        CALL ALLVEC_IN_BLOCK(PRIVE,MAX(NPRIV,4),1,'PRIV  ',IELMT,1, 2)
      ELSE
        CALL ALLVEC_IN_BLOCK(PRIVE,4           ,1,'PRIV  ',    0,1, 0)
      ENDIF
C     TO AVOID WRITING NON-INITIALISED ARRAYS TO FILE
      CALL OS('X=0     ',X=PRIVE)
!
      ! ************ !
      ! VIII - ALIAS !
      ! ************ !
!
      T1   => TB%ADR( 1)%P ! WORK ARRAY
      T2   => TB%ADR( 2)%P ! WORK ARRAY
      T3   => TB%ADR( 3)%P ! WORK ARRAY
      T4   => TB%ADR( 4)%P ! WORK ARRAY
      T5   => TB%ADR( 5)%P ! WORK ARRAY
      T6   => TB%ADR( 6)%P ! WORK ARRAY
      T7   => TB%ADR( 7)%P ! WORK ARRAY
      T8   => TB%ADR( 8)%P ! WORK ARRAY
      T9   => TB%ADR( 9)%P ! WORK ARRAY
      T10  => TB%ADR(10)%P ! WORK ARRAY
      T11  => TB%ADR(11)%P ! WORK ARRAY
      T12  => TB%ADR(12)%P ! WORK ARRAY
      T13  => TB%ADR(13)%P ! WORK ARRAY
      T14  => TB%ADR(14)%P ! WORK ARRAY
!
      ! ****************************************************************** !
      ! IX - ALLOCATES A BLOCK CONNECTING A VARIABLE NAME TO ITS ARRAY     !
      ! ****************************************************************** !
!
      CALL ALLBLO(VARSOR, 'VARSOR')
      CALL ADDBLO(VARSOR, U2D    )            ! 01
      CALL ADDBLO(VARSOR, V2D    )            ! 02
      CALL ADDBLO(VARSOR, HN    )             ! 03
      CALL ADDBLO(VARSOR, Z     )             ! 04
      CALL ADDBLO(VARSOR, ZF    )             ! 05
      CALL ADDBLO(VARSOR, Q     )             ! 06
      CALL ADDBLO(VARSOR, QU    )             ! 07
      CALL ADDBLO(VARSOR, QV    )             ! 08
      CALL ADDBLO(VARSOR, ZR    )             ! 09
      CALL ADDBLO(VARSOR, CHESTR)             ! 10
      CALL ADDBLO(VARSOR, TOB   )             ! 11
      CALL ADDBLO(VARSOR, HW    )             ! 12
      CALL ADDBLO(VARSOR, TW    )             ! 13
      CALL ADDBLO(VARSOR, THETAW)             ! 14
      CALL ADDBLO(VARSOR, QS    )             ! 15
      CALL ADDBLO(VARSOR, QSX   )             ! 16
      CALL ADDBLO(VARSOR, QSY   )             ! 17
      CALL ADDBLO(VARSOR, ESOMT )             ! 18
      CALL ADDBLO(VARSOR, KS)                 ! 19
      CALL ADDBLO(VARSOR, MU)                 ! 20
C
C     AVAI: FROM 21 TO 20+NOMBLAY*NSICLA
C
      DO I = 1,NOMBLAY*NSICLA
        CALL ADDBLO(VARSOR, AVAI%ADR(I)%P)
      ENDDO
C
C     QSCL: FROM 21+NOMBLAY*NSICLA TO 20+(NOMBLAY+1)*NSICLA
C
      DO I = 1, NSICLA
        CALL ADDBLO(VARSOR, QSCL%ADR(I)%P)
      ENDDO
C
C     CS: FROM 21+(NOMBLAY+1)*NSICLA TO 20+(NOMBLAY+2)*NSICLA
C
      DO I=1,NSICLA
        CALL ADDBLO(VARSOR, CS%ADR(I)%P)
      ENDDO
      CALL ADDBLO(VARSOR,QS_C)               ! 21+(NOMBLAY+2)*NSICLA
      CALL ADDBLO(VARSOR,QSXC)               ! 22+(NOMBLAY+2)*NSICLA
      CALL ADDBLO(VARSOR,QSYC)               ! 23+(NOMBLAY+2)*NSICLA
      CALL ADDBLO(VARSOR,QS_S)               ! 24+(NOMBLAY+2)*NSICLA
      CALL ADDBLO(VARSOR,QSXS)               ! 25+(NOMBLAY+2)*NSICLA
      CALL ADDBLO(VARSOR,QSYS)               ! 26+(NOMBLAY+2)*NSICLA
C
C     QSCL_C: FROM 27+(NOMBLAY+2)*NSICLA TO 26+(NOMBLAY+3)*NSICLA
C
      DO I=1,NSICLA
        CALL ADDBLO(VARSOR,QSCL_C%ADR(I)%P)
      ENDDO
C
C     QSCL_S: FROM 27+(NOMBLAY+3)*NSICLA TO 26+(NOMBLAY+4)*NSICLA
C
      DO I=1,NSICLA
        CALL ADDBLO(VARSOR,QSCL_S%ADR(I)%P)
      ENDDO
C
C     LAYTHI: FROM 27+(NOMBLAY+4)*NSICLA TO 26+(NOMBLAY+4)*NSICLA+NOMBLAY
C
      DO I=1,NOMBLAY
        CALL ADDBLO(VARSOR,LAYTHI%ADR(I)%P) ! 26+(NOMBLAY+4)*NSICLA+NOMBLAY
      ENDDO
C
C     PRIVE: FROM 27+(NOMBLAY+4)*NSICLA+NOMBLAY TO
C                 26+(NOMBLAY+4)*NSICLA+MAX(4,NPRIV)+NOMBLAY
C
      DO I=1,MAX(4,NPRIV)
        CALL ADDBLO(VARSOR,PRIVE%ADR(I)%P)
      ENDDO
!
      IF(VARCL%N.GT.0) THEN
        DO I=1,VARCL%N
          CALL ADDBLO(VARSOR,VARCL%ADR(I)%P)
          SORLEO(26+MAX(4,NPRIV)+NSICLA*(NOMBLAY+4)+NOMBLAY+I)=.TRUE.
        ENDDO
      ENDIF
!
!
!-----------------------------------------------------------------------
C !JAJ #### IF REQUIRED, HERE WE CAN READ THE INPUT SECTIONS FILE
C      AND MODIFY NCP AND CTRLSC(1:NCP) ACCORDINGLY IN READ_SECTIONS
!
      IF(TRIM(SIS_FILES(SISSEC)%NAME).NE.'') THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*)
     &   'POINT_SISYPHE: SECTIONS DEFINIES PAR FICHIER'
        ELSEIF(LNG.EQ.2) THEN
          WRITE(LU,*)
     &   'POINT_SISYPHE: SECTIONS DEFINED IN THE SECTIONS INPUT FILE'
        ENDIF
        CALL READ_SECTIONS_SISYPHE
      ELSE ! THE PREVIOUS WAY OF DOING THINGS
        IF(NCP.NE.0) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)
     &      'POINT_SISYPHE: SECTIONS DEFINED IN THE PARAMETER FILE'
          ELSEIF(LNG.EQ.2) THEN
            IF(NCP.NE.0) WRITE(LU,*)
     &      'POINT_SISYPHE: SECTIONS DEFINED IN THE PARAMETER FILE'
          ENDIF
        ENDIF
      ENDIF
!
      IF(LNG == 1) WRITE(LU,21)
      IF(LNG == 2) WRITE(LU,22)
!
11    FORMAT(1X,///,21X,'*******************************',/,
     &21X,              '* ALLOCATION DE LA MEMOIRE    *',/,
     &21X,              '*******************************',/)
21    FORMAT(1X,///,21X,'****************************************',/,
     &21X,              '* FIN DE L''ALLOCATION DE LA MEMOIRE  : *',/,
     &21X,              '****************************************',/)

12    FORMAT(1X,///,21X,'*******************************',/,
     &21X,              '*     MEMORY ORGANISATION     *',/,
     &21X,              '*******************************',/)
22    FORMAT(1X,///,21X,'*************************************',/,
     &21X,              '*    END OF MEMORY ORGANIZATION:    *',/,
     &21X,              '*************************************',/)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C