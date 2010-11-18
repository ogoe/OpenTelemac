C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES THE EVOLUTION EQUATION OF THE VARIANCE DENSITY
!>               (DEPENDENT ON FREQUENCY AND DIRECTION), BASED ON THE
!>                CONSERVATION OF THE ACTION DENSITY IN THE GENERAL CASE).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TOMAWAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TOMAWAC :<br>
!> @link DECLARATIONS_TOMAWAC::ALFABJ ALFABJ@endlink, 
!> @link DECLARATIONS_TOMAWAC::ALFARO ALFARO@endlink, 
!> @link DECLARATIONS_TOMAWAC::ALFLTA ALFLTA@endlink, 
!> @link DECLARATIONS_TOMAWAC::ALIRE ALIRE@endlink, 
!> @link DECLARATIONS_TOMAWAC::ALPHA ALPHA@endlink, 
!> @link DECLARATIONS_TOMAWAC::APHILL APHILL@endlink, 
!> @link DECLARATIONS_TOMAWAC::BDISPB BDISPB@endlink, 
!> @link DECLARATIONS_TOMAWAC::BDSSPB BDSSPB@endlink, 
!> @link DECLARATIONS_TOMAWAC::BETAIH BETAIH@endlink, 
!> @link DECLARATIONS_TOMAWAC::BETAM BETAM@endlink, 
!> @link DECLARATIONS_TOMAWAC::BINBI1 BINBI1@endlink, 
!> @link DECLARATIONS_TOMAWAC::BINPRE BINPRE@endlink, 
!> @link DECLARATIONS_TOMAWAC::BINRBI BINRBI@endlink, 
!> @link DECLARATIONS_TOMAWAC::BINVEN BINVEN@endlink, 
!> @link DECLARATIONS_TOMAWAC::BORETG BORETG@endlink, 
!> @link DECLARATIONS_TOMAWAC::BOUNDARY_COLOUR BOUNDARY_COLOUR@endlink, 
!> @link DECLARATIONS_TOMAWAC::BST1 BST1@endlink, 
!> @link DECLARATIONS_TOMAWAC::CDRAG CDRAG@endlink, 
!> @link DECLARATIONS_TOMAWAC::CFROT1 CFROT1@endlink, 
!> @link DECLARATIONS_TOMAWAC::CIMPLI CIMPLI@endlink, 
!> @link DECLARATIONS_TOMAWAC::CMOUT1 CMOUT1@endlink, 
!> @link DECLARATIONS_TOMAWAC::CMOUT2 CMOUT2@endlink, 
!> @link DECLARATIONS_TOMAWAC::COEFHS COEFHS@endlink, 
!> @link DECLARATIONS_TOMAWAC::COEFNL COEFNL@endlink, 
!> @link DECLARATIONS_TOMAWAC::COURAN COURAN@endlink, 
!> @link DECLARATIONS_TOMAWAC::COUSTA COUSTA@endlink, 
!> @link DECLARATIONS_TOMAWAC::DDC DDC@endlink, 
!> @link DECLARATIONS_TOMAWAC::DEBUG DEBUG@endlink, 
!> @link DECLARATIONS_TOMAWAC::DECAL DECAL@endlink, 
!> @link DECLARATIONS_TOMAWAC::DEPTH DEPTH@endlink, 
!> @link DECLARATIONS_TOMAWAC::DF_LIM DF_LIM@endlink, 
!> @link DECLARATIONS_TOMAWAC::DONTEL DONTEL@endlink, 
!> @link DECLARATIONS_TOMAWAC::DT DT@endlink, 
!> @link DECLARATIONS_TOMAWAC::EM2SIH EM2SIH@endlink, 
!> @link DECLARATIONS_TOMAWAC::EQUA EQUA@endlink, 
!> @link DECLARATIONS_TOMAWAC::F1 F1@endlink, 
!> @link DECLARATIONS_TOMAWAC::FETCHL FETCHL@endlink, 
!> @link DECLARATIONS_TOMAWAC::FPICL FPICL@endlink, 
!> @link DECLARATIONS_TOMAWAC::FPMAXL FPMAXL@endlink, 
!> @link DECLARATIONS_TOMAWAC::FRABL FRABL@endlink, 
!> @link DECLARATIONS_TOMAWAC::GAM2RO GAM2RO@endlink, 
!> @link DECLARATIONS_TOMAWAC::GAMARO GAMARO@endlink, 
!> @link DECLARATIONS_TOMAWAC::GAMATG GAMATG@endlink, 
!> @link DECLARATIONS_TOMAWAC::GAMBJ1 GAMBJ1@endlink, 
!> @link DECLARATIONS_TOMAWAC::GAMBJ2 GAMBJ2@endlink, 
!> @link DECLARATIONS_TOMAWAC::GAMMAL GAMMAL@endlink, 
!> @link DECLARATIONS_TOMAWAC::GLOB GLOB@endlink, 
!> @link DECLARATIONS_TOMAWAC::GRADEB GRADEB@endlink, 
!> @link DECLARATIONS_TOMAWAC::GRAPRD GRAPRD@endlink, 
!> @link DECLARATIONS_TOMAWAC::GRAVIT GRAVIT@endlink, 
!> @link DECLARATIONS_TOMAWAC::HIST HIST@endlink, 
!> @link DECLARATIONS_TOMAWAC::HM0L HM0L@endlink, 
!> @link DECLARATIONS_TOMAWAC::IANGNL IANGNL@endlink, 
!> @link DECLARATIONS_TOMAWAC::IDISRO IDISRO@endlink, 
!> @link DECLARATIONS_TOMAWAC::IELM2 IELM2@endlink, 
!> @link DECLARATIONS_TOMAWAC::IEXPRO IEXPRO@endlink, 
!> @link DECLARATIONS_TOMAWAC::IFRBJ IFRBJ@endlink, 
!> @link DECLARATIONS_TOMAWAC::IFRIH IFRIH@endlink, 
!> @link DECLARATIONS_TOMAWAC::IFRRO IFRRO@endlink, 
!> @link DECLARATIONS_TOMAWAC::IFRTG IFRTG@endlink, 
!> @link DECLARATIONS_TOMAWAC::IHMBJ IHMBJ@endlink, 
!> @link DECLARATIONS_TOMAWAC::INDIV INDIV@endlink, 
!> @link DECLARATIONS_TOMAWAC::IQBBJ IQBBJ@endlink, 
!> @link DECLARATIONS_TOMAWAC::IWHTG IWHTG@endlink, 
!> @link DECLARATIONS_TOMAWAC::I_ORIG I_ORIG@endlink, 
!> @link DECLARATIONS_TOMAWAC::J_ORIG J_ORIG@endlink, 
!> @link DECLARATIONS_TOMAWAC::KSPB KSPB@endlink, 
!> @link DECLARATIONS_TOMAWAC::LAM LAM@endlink, 
!> @link DECLARATIONS_TOMAWAC::LIMIT LIMIT@endlink, 
!> @link DECLARATIONS_TOMAWAC::LIMSPE LIMSPE@endlink, 
!> @link DECLARATIONS_TOMAWAC::LISPRD LISPRD@endlink, 
!> @link DECLARATIONS_TOMAWAC::LVMAC LVMAC@endlink, 
!> @link DECLARATIONS_TOMAWAC::MAREE MAREE@endlink, 
!> @link DECLARATIONS_TOMAWAC::MAXVAR MAXVAR@endlink, 
!> @link DECLARATIONS_TOMAWAC::MESH MESH@endlink, 
!> @link DECLARATIONS_TOMAWAC::NBOR NBOR@endlink, 
!> @link DECLARATIONS_TOMAWAC::NDTBRK NDTBRK@endlink, 
!> @link DECLARATIONS_TOMAWAC::NELEM2 NELEM2@endlink, 
!> @link DECLARATIONS_TOMAWAC::NF NF@endlink, 
!> @link DECLARATIONS_TOMAWAC::NIT NIT@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPLAN NPLAN@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPLEO NPLEO@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPOIN2 NPOIN2@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPOIN3 NPOIN3@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPRIV NPRIV@endlink, 
!> @link DECLARATIONS_TOMAWAC::NPTFR NPTFR@endlink, 
!> @link DECLARATIONS_TOMAWAC::NSITS NSITS@endlink, 
!> @link DECLARATIONS_TOMAWAC::PROINF PROINF@endlink, 
!> @link DECLARATIONS_TOMAWAC::PROMIN PROMIN@endlink, 
!> @link DECLARATIONS_TOMAWAC::PROP PROP@endlink, 
!> @link DECLARATIONS_TOMAWAC::RAISF RAISF@endlink, 
!> @link DECLARATIONS_TOMAWAC::RFMLTA RFMLTA@endlink, 
!> @link DECLARATIONS_TOMAWAC::ROAIR ROAIR@endlink, 
!> @link DECLARATIONS_TOMAWAC::ROEAU ROEAU@endlink, 
!> @link DECLARATIONS_TOMAWAC::SB SB@endlink, 
!> @link DECLARATIONS_TOMAWAC::SBETA SBETA@endlink, 
!> @link DECLARATIONS_TOMAWAC::SBREK SBREK@endlink, 
!> @link DECLARATIONS_TOMAWAC::SCF SCF@endlink, 
!> @link DECLARATIONS_TOMAWAC::SCG SCG@endlink, 
!> @link DECLARATIONS_TOMAWAC::SCOSF SCOSF@endlink, 
!> @link DECLARATIONS_TOMAWAC::SCOSTE SCOSTE@endlink, 
!> @link DECLARATIONS_TOMAWAC::SCT SCT@endlink, 
!> @link DECLARATIONS_TOMAWAC::SCX SCX@endlink, 
!> @link DECLARATIONS_TOMAWAC::SCY SCY@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDEPTH SDEPTH@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDFR SDFR@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDUX SDUX@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDUY SDUY@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDVX SDVX@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDVY SDVY@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDZHDT SDZHDT@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDZX SDZX@endlink, 
!> @link DECLARATIONS_TOMAWAC::SDZY SDZY@endlink, 
!> @link DECLARATIONS_TOMAWAC::SELT SELT@endlink, 
!> @link DECLARATIONS_TOMAWAC::SETA SETA@endlink, 
!> @link DECLARATIONS_TOMAWAC::SETAP1 SETAP1@endlink, 
!> @link DECLARATIONS_TOMAWAC::SF SF@endlink, 
!> @link DECLARATIONS_TOMAWAC::SFBOR SFBOR@endlink, 
!> @link DECLARATIONS_TOMAWAC::SFR SFR@endlink, 
!> @link DECLARATIONS_TOMAWAC::SFRE SFRE@endlink, 
!> @link DECLARATIONS_TOMAWAC::SFROT SFROT@endlink, 
!> @link DECLARATIONS_TOMAWAC::SIBOR SIBOR@endlink, 
!> @link DECLARATIONS_TOMAWAC::SIGMAL SIGMAL@endlink, 
!> @link DECLARATIONS_TOMAWAC::SIGMBL SIGMBL@endlink, 
!> @link DECLARATIONS_TOMAWAC::SITR01 SITR01@endlink, 
!> @link DECLARATIONS_TOMAWAC::SITR11 SITR11@endlink, 
!> @link DECLARATIONS_TOMAWAC::SITR12 SITR12@endlink, 
!> @link DECLARATIONS_TOMAWAC::SITR13 SITR13@endlink, 
!> @link DECLARATIONS_TOMAWAC::SITR31 SITR31@endlink, 
!> @link DECLARATIONS_TOMAWAC::SITR32 SITR32@endlink, 
!> @link DECLARATIONS_TOMAWAC::SITR33 SITR33@endlink, 
!> @link DECLARATIONS_TOMAWAC::SLIFBR SLIFBR@endlink, 
!> @link DECLARATIONS_TOMAWAC::SMOUT SMOUT@endlink, 
!> @link DECLARATIONS_TOMAWAC::SORIMP SORIMP@endlink, 
!> @link DECLARATIONS_TOMAWAC::SORLEO SORLEO@endlink, 
!> @link DECLARATIONS_TOMAWAC::SPEULI SPEULI@endlink, 
!> @link DECLARATIONS_TOMAWAC::SPHE SPHE@endlink, 
!> @link DECLARATIONS_TOMAWAC::SPRE1L SPRE1L@endlink, 
!> @link DECLARATIONS_TOMAWAC::SPRE2L SPRE2L@endlink, 
!> @link DECLARATIONS_TOMAWAC::SPRIVE SPRIVE@endlink, 
!> @link DECLARATIONS_TOMAWAC::SSHF SSHF@endlink, 
!> @link DECLARATIONS_TOMAWAC::SSHP1 SSHP1@endlink, 
!> @link DECLARATIONS_TOMAWAC::SSHP2 SSHP2@endlink, 
!> @link DECLARATIONS_TOMAWAC::SSHP3 SSHP3@endlink, 
!> @link DECLARATIONS_TOMAWAC::SSHZ SSHZ@endlink, 
!> @link DECLARATIONS_TOMAWAC::SSINTE SSINTE@endlink, 
!> @link DECLARATIONS_TOMAWAC::ST1 ST1@endlink, 
!> @link DECLARATIONS_TOMAWAC::ST2 ST2@endlink, 
!> @link DECLARATIONS_TOMAWAC::ST3 ST3@endlink, 
!> @link DECLARATIONS_TOMAWAC::ST4 ST4@endlink, 
!> @link DECLARATIONS_TOMAWAC::STDGEO STDGEO@endlink, 
!> @link DECLARATIONS_TOMAWAC::STETA STETA@endlink, 
!> @link DECLARATIONS_TOMAWAC::STGF STGF@endlink, 
!> @link DECLARATIONS_TOMAWAC::STNEW STNEW@endlink, 
!> @link DECLARATIONS_TOMAWAC::STOLD STOLD@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA01 STRA01@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA02 STRA02@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA31 STRA31@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA32 STRA32@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA33 STRA33@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA34 STRA34@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA35 STRA35@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA36 STRA36@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA37 STRA37@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA38 STRA38@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA39 STRA39@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA41 STRA41@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA42 STRA42@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA43 STRA43@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRA44 STRA44@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRIA STRIA@endlink, 
!> @link DECLARATIONS_TOMAWAC::STRIF STRIF@endlink, 
!> @link DECLARATIONS_TOMAWAC::STSDER STSDER@endlink, 
!> @link DECLARATIONS_TOMAWAC::STSTOT STSTOT@endlink, 
!> @link DECLARATIONS_TOMAWAC::SUC SUC@endlink, 
!> @link DECLARATIONS_TOMAWAC::SUC1 SUC1@endlink, 
!> @link DECLARATIONS_TOMAWAC::SUC2 SUC2@endlink, 
!> @link DECLARATIONS_TOMAWAC::SUIT SUIT@endlink, 
!> @link DECLARATIONS_TOMAWAC::SUV SUV@endlink, 
!> @link DECLARATIONS_TOMAWAC::SUV1 SUV1@endlink, 
!> @link DECLARATIONS_TOMAWAC::SUV2 SUV2@endlink, 
!> @link DECLARATIONS_TOMAWAC::SVC SVC@endlink, 
!> @link DECLARATIONS_TOMAWAC::SVC1 SVC1@endlink, 
!> @link DECLARATIONS_TOMAWAC::SVC2 SVC2@endlink, 
!> @link DECLARATIONS_TOMAWAC::SVENT SVENT@endlink, 
!> @link DECLARATIONS_TOMAWAC::SVV SVV@endlink, 
!> @link DECLARATIONS_TOMAWAC::SVV1 SVV1@endlink, 
!> @link DECLARATIONS_TOMAWAC::SVV2 SVV2@endlink, 
!> @link DECLARATIONS_TOMAWAC::SXK SXK@endlink, 
!> @link DECLARATIONS_TOMAWAC::SXRELV SXRELV@endlink, 
!> @link DECLARATIONS_TOMAWAC::SYRELV SYRELV@endlink, 
!> @link DECLARATIONS_TOMAWAC::SZF SZF@endlink, 
!> @link DECLARATIONS_TOMAWAC::TAILF TAILF@endlink, 
!> @link DECLARATIONS_TOMAWAC::TETA1L TETA1L@endlink, 
!> @link DECLARATIONS_TOMAWAC::TETA2L TETA2L@endlink, 
!> @link DECLARATIONS_TOMAWAC::TEXTE TEXTE@endlink, 
!> @link DECLARATIONS_TOMAWAC::TITCAS TITCAS@endlink, 
!> @link DECLARATIONS_TOMAWAC::TSOU TSOU@endlink, 
!> @link DECLARATIONS_TOMAWAC::VARSOR VARSOR@endlink, 
!> @link DECLARATIONS_TOMAWAC::VENSTA VENSTA@endlink, 
!> @link DECLARATIONS_TOMAWAC::VENT VENT@endlink, 
!> @link DECLARATIONS_TOMAWAC::VX_CTE VX_CTE@endlink, 
!> @link DECLARATIONS_TOMAWAC::VY_CTE VY_CTE@endlink, 
!> @link DECLARATIONS_TOMAWAC::WAC_FILES WAC_FILES@endlink, 
!> @link DECLARATIONS_TOMAWAC::XDTBRK XDTBRK@endlink, 
!> @link DECLARATIONS_TOMAWAC::XKAPPA XKAPPA@endlink, 
!> @link DECLARATIONS_TOMAWAC::XLAMD XLAMD@endlink, 
!> @link DECLARATIONS_TOMAWAC::XLAMDL XLAMDL@endlink, 
!> @link DECLARATIONS_TOMAWAC::XLEO XLEO@endlink, 
!> @link DECLARATIONS_TOMAWAC::YLEO YLEO@endlink, 
!> @link DECLARATIONS_TOMAWAC::ZF ZF@endlink, 
!> @link DECLARATIONS_TOMAWAC::ZM1 ZM1@endlink, 
!> @link DECLARATIONS_TOMAWAC::ZM2 ZM2@endlink, 
!> @link DECLARATIONS_TOMAWAC::ZREPOS ZREPOS@endlink, 
!> @link DECLARATIONS_TOMAWAC::ZVENT ZVENT@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::KENT KENT@endlink, 
!> @link DECLARATIONS_TELEMAC::KLOG KLOG@endlink, 
!> @link DECLARATIONS_TELEMAC::KSORT KSORT@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ADC, AT, C, DATE, DEBRES, DEUPI, DTSI, HDC, I1, I2, IBID, IMPRES, IP, ISLEO, JDC, LAMBD0, LT, LT1, MDC, NBD, NOLEO, NPC, NPM, NPTL, NPV, NRK, NVARCL, NVCOU, NVHMA, QINDI, TC1, TC2, TIME, TM1, TM2, TV1, TV2, VITMIN, VITVEN, Z
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> BIEF_DESIMP(), BIEF_VALIDA(), CONDIW(), CORFON(), CORMAR(), CREATE_DATASET(), DUMP2D(), ECRETE(), ECRSPE(), FONSTR(), IMPR(), INBIEF(), INIPHY(), INITAB(), LECLIM_TOMAWAC(), LECSUI(), LIMWAC(), OV(), PLANTE(), PRELEO_MPI(), PRENL1(), PREPRO(), PREQT2(), PROPA(), SEMIMP(), SOR3D(), TRANSF(), USTAR1(), USTAR2(), VARTEL(), WRITE_MESH()
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
!>      <td><center> 5.2                                       </center>
!> </td><td> 12/06/2001
!> </td><td> OPTIMER (02 98 44 24 51)
!> </td><td> TOMAWAC/COWADIS MERGE.
!> <br>      M. BENOIT IS THE LEAD FOR EDF/LNHE
!> </td></tr>
!>      <tr>
!>      <td><center> 5.1                                       </center>
!> </td><td> 25/08/2000
!> </td><td> OPTIMER (02 98 44 24 51)
!> </td><td> CONVERTED TO FORTRAN 90.
!> <br>      D. VIOLEAU IS THE LEAD FOR EDF/LNHE
!> </td></tr>
!>      <tr>
!>      <td><center> 1.2                                       </center>
!> </td><td> 07/11/96
!> </td><td> M. BENOIT
!> </td><td> MODIFIED
!> </td></tr>
!>      <tr>
!>      <td><center> 1.0                                       </center>
!> </td><td> 01/02/95
!> </td><td> F. MARCOS
!> </td><td> CREATED
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
                          SUBROUTINE WAC
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TOMAWAC
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
C     LOCAL VARIABLES
C
      INTEGER          NVARCL, LT , LT1 , NRK   , NPV   , NPC , NPM,
     &                 NOLEO(99)
      LOGICAL          ISLEO(99)
      INTEGER          DATE(3)       , TIME(3)
      INTEGER          NPTL  , IBID, IP
C
C     TV1 TIME CORRESPONDING TO WIND 1
C     TV2 TIME CORRESPONDING TO WIND 2
C
      DOUBLE PRECISION LAMBD0, C     , Z(1)  , DEUPI , DTSI,
     &                 AT    , TV1   , TV2   , TC1 , TC2 , TM1 , TM2
      DOUBLE PRECISION VITVEN, VITMIN
      INTEGER, ALLOCATABLE :: QINDI(:)
      INTEGER          NBD , ADC , MDC , JDC , HDC, I1, I2, NVHMA,NVCOU
      LOGICAL          IMPRES, DEBRES
C
C
C
C=====C
C  1  C INITIALISES THE LOCAL VARIABLES
C=====C======================================
C
C.....1.2 PHYSICAL PARAMETERS
C     """""""""""""""""""""""""
      GRAVIT=9.806D0
      DEUPI =2.D0*3.14159265358979D0
C
C.....1.3 GENERAL COMPUTATION PARAMETERS
C     """""""""""""""""""""""""""""""""
      NVARCL=0
      NPTL  =NPOIN3*NF
C
C.....1.4 INITIALISES THE ARRAYS DATE AND TIME
C     """"""""""""""""""""""""""""""""""""""""""""
      ADC=INT(DDC*1.D-8)
      MDC=INT(DDC*1.D-6)
      JDC=INT(DDC*1.D-4)
      HDC=INT(DDC*1.D-2)
      DATE(1)=ADC
      DATE(2)=MDC-100*ADC
      DATE(3)=JDC-100*MDC
      TIME(1)=HDC-100*JDC
      TIME(2)=INT(DDC-100.D0*HDC)
      TIME(3)=0
C
C=====C
C  2  C READS THE BOUNDARY CONDITIONS AND INDICES OF BOUNDARY NODES
C=====C================================================================
C
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LECLIM'
      CALL LECLIM_TOMAWAC
     &(SLIFBR%I,SFBOR%R,NPTFR,NBOR,STDGEO,WAC_FILES(WACCLI)%LU,
!BD_INCKA MODIFICATION FOR PARALLEL MODE
C    *  MESH%ISEG%I,MESH%XSEG%R,MESH%YSEG%R,MESH%NACHB%I )
     &  MESH%ISEG%I,MESH%XSEG%R,MESH%YSEG%R,MESH%NACHB%I ,MESH,
     &  BOUNDARY_COLOUR%I)
      IF(DEBUG.GT.0) WRITE(LU,*) 'SORTIE DE LECLIM'
!BD_INCKA END OF MODIFICATION FOR PARALLEL MODE
C
C-----------------------------------------------------------------------
C
C COMPLEMENTS THE DATA STRUCTURE FOR BIEF
C
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE INBIEF'
      CALL INBIEF(SLIFBR%I,KLOG,SITR31,SITR32,SITR33,
     &            LVMAC,IELM2,LAMBD0,SPHE,MESH,ST1,ST2,1,1,EQUA)
      IF(DEBUG.GT.0) WRITE(LU,*) 'SORTIE DE INBIEF'
C
C-----------------------------------------------------------------------
C
C READS THE BOTTOM ELEVATION (ZF) FROM THE GEOMRTY FILE
C
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE FONSTR'
      CALL FONSTR (ST1,SZF,ST2,ST3,WAC_FILES(WACGEO)%LU,
     &             WAC_FILES(WACFON)%LU,WAC_FILES(WACFON)%NAME,MESH,
     &             1.D0,.TRUE.)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE FONSTR'
C
C CORRECTS THE BOTTOM ELEVATION IF NEED BE (OR COMPUTES IT IF HAS NOT
C BEEN DONE IN FONSTR)
C IN STANDARD MODE, CORFON DOES NOT DO ANYTHING
C (BEWARE, TAKE TOMAWAC CORFON)
C
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CORFON'
      CALL CORFON
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CORFON'
C
C.....COMPUTES THE WATER DEPTH (ARRAY 'DEPTH')
C
      DO IP=1,NPOIN2
        DEPTH(IP)=ZREPOS-ZF(IP)
        IF(DEPTH(IP).LT.PROMIN) DEPTH(IP)=0.9D0*PROMIN
      ENDDO
C
C-----------------------------------------------------------------------
C
C PREPARES GRAPHICAL OUTPUTS
C
        ! CREATES DATA FILE USING A GIVEN FILE FORMAT : FORMAT_RES.
        ! THE DATA ARE CREATED IN THE FILE: NRES, AND ARE
        ! CHARACTERISED BY A TITLE AND NAME OF OUTPUT VARIABLES
        ! CONTAINED IN THE FILE.
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CREATE_DATASET'
        CALL CREATE_DATASET(WAC_FILES(WACRES)%FMT, ! RESULTS FILE FORMAT
     &                      WAC_FILES(WACRES)%LU,  ! LU FOR RESULTS FILE
     &                      TITCAS,     ! TITLE
     &                      MAXVAR,     ! MAX NUMBER OF OUTPUT VARIABLES
     &                      TEXTE,      ! NAMES OF OUTPUT VARIABLES
     &                      SORLEO)     ! PRINT TO FILE OR NOT
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CREATE_DATASET'
        ! WRITES THE MESH IN THE OUTPUT FILE :
        ! IN PARALLEL, REQUIRES NCSIZE AND NPTIR.
        ! THE REST OF THE INFORMATION IS IN MESH.
        ! ALSO WRITES : START DATE/TIME AND COORDINATES OF THE
        ! ORIGIN.
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE WRITE_MESH'
        CALL WRITE_MESH(WAC_FILES(WACRES)%FMT, ! RESULTS FILE FORMAT
     &                  WAC_FILES(WACRES)%LU,  ! LU FOR RESULTS FILE
     &                  MESH,          ! CHARACTERISES MESH
     &                  1,             ! NUMBER OF PLANES /NA/
     &                  DATE,          ! START DATE
     &                  TIME,          ! START TIME
     &                  I_ORIG,J_ORIG) ! COORDINATES OF THE ORIGIN.
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE WRITE_MESH'
C
C-----------------------------------------------------------------------
C
C INITIAL CONDITIONS
C
C
C
C=====C INITIALISES THE DISCRETISATION VECTORS, THE CURRENT,
C  2  C THE WIND AND THE VARIANCE SPECTRUM
C=====C===========================================================
      LT=0
      DTSI=DT/NSITS
C
      IF (SUIT) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LECSUI'
        CALL LECSUI
     &( SF%R    , NPLAN   , NF      , STETA%R, SFR%R  ,
     &  NELEM2  , NPOIN2  , AT      , SUC%R  , SVC%R  ,
     &  SUC1%R  , SVC1%R  , SUC2%R  , SVC2%R , SUV%R  ,
     &  SVV%R   , SUV1%R  , SVV1%R  , SUV2%R , SVV2%R ,
     &  VENT    , TV1     , TV2     , COUSTA ,
     &  WAC_FILES(WACPRE)%LU ,
     &  BINPRE  , STRA02%R, SDEPTH%R, TC1 , TC2 , ZM1 , ZM2 ,
     &  SDZHDT%R, TM1     , TM2     , MAREE  )
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE LECSUI'
      ELSE
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CONDIW'
        CALL CONDIW
     &( AT, LT , DEUPI , TC1 , TC2 , NPC, TV1, TV2, NPV, TM1, TM2, NPM,
     &  NVHMA  , NVCOU )
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CONDIW'
      ENDIF
C
C
C
C=====C
C  3  C USES A TELEMAC VARIABLE (OPTIONAL)
C=====C=================================================
      IF (DONTEL) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VARTEL'
        CALL VARTEL
     &( STRA31%R, MESH%X%R, MESH%Y%R, SDEPTH%R,
     &  SUC%R   , SVC%R   , ZREPOS     , STRA32%R,
     &  SF%R    , NPLAN      , NF         , NPOIN2     )
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VARTEL'
      ENDIF
C
      DO IP=1,NPOIN2
         IF(DEPTH(IP).LE.0.D0) THEN
           IF(LNG.EQ.1) THEN
             WRITE(LU,*) ''
             WRITE(LU,*) '*************************'
             WRITE(LU,*) ' ! PROFONDEUR NEGATIVE ! '
             WRITE(LU,*) '   ARRET DU PROGRAMME    '
             WRITE(LU,*) '*************************'
             CALL PLANTE(0)
           ELSE
             WRITE(LU,*) ''
             WRITE(LU,*) '**************************'
             WRITE(LU,*) ' ! NEGATIVE WATER DEPTH ! '
             WRITE(LU,*) '   END OF THE COMPUTATION '
             WRITE(LU,*) '**************************'
             CALL PLANTE(0)
           ENDIF
         ENDIF
      ENDDO
C
C
C
C=====C
C  4  C PREPARES FOR NON-LINEAR INTERACTION COMPUTATIONS
C=====C=======================================================
      IF(STRIF.EQ.1) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PRENL1'
        CALL PRENL1( IANGNL, COEFNL, NPLAN , NF , RAISF , XLAMD )
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PRENL1'
      ENDIF
C
      ALLOCATE(QINDI(NPLAN))
      IF (STRIA.EQ.2) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PREQT2'
        CALL PREQT2(STETA%R,NPLAN,BDISPB,BDSSPB,NBD,QINDI)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PREQT2'
      ENDIF
C
C=====C INITIALISES THE WAVE STRESS, THEN COMPUTES THE FRICTION
C  5  C VELOCITY U*, THE ROUGHNESS Z0 AND DIRECTIONS
C=====C==============================================================
C
C.....5.1 INTITIALISES THE INITIAL WAVE STRESS
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""
      CALL OV ( 'X=C     ' ,STRA41%R, STRA32%R , STRA33%R,
     &                      0.D0 , NPOIN2 )
C
C.....5.2 COMPUTES U* AND Z0 USING THE CHOSEN METHOD
C     """""""""""""""""""""""""""""""""""""""""""""""""""
      IF (VENT) THEN
        IF (SVENT.EQ.1) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE USTAR1'
          CALL USTAR1
     &( STRA42%R , STRA44%R , STRA41%R , SUV%R , SVV%R ,
     &  CDRAG    , ALPHA    , XKAPPA   , ZVENT , GRAVIT,
     &  NPOIN2   )
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE USTAR1'
        ELSEIF (SVENT.EQ.2) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE USTAR2'
          CALL USTAR2(STRA42%R,SUV%R,SVV%R,NPOIN2)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE USTAR2'
        ELSE
          IF (LNG.EQ.1) THEN
            WRITE(LU,*)
     &      'PB DANS WAC : VENT PRESENT, MAIS SVENT NON CORRECT'
          ELSE
            WRITE(LU,*)
     &      'PB IN WAC : WIND PRESENT, BUT SVENT NOT CORRECT'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
C
C.....5.3 COMPUTES THE WIND DIRECTION
C     """"""""""""""""""""""""""""""""""
      VITMIN=1.D-3
      IF (VENT) THEN
        DO IP=1,NPOIN2
          VITVEN=SQRT(SUV%R(IP)**2+SVV%R(IP)**2)
          IF (VITVEN.GT.VITMIN) THEN
            STRA43%R(IP)=ATAN2(SUV%R(IP),SVV%R(IP))
          ELSE
            STRA43%R(IP)=0.D0
          ENDIF
        ENDDO
      ENDIF
C
C
C
C=====C
C  6  C INITIALISES SOME USEFUL ARRAYS
C=====C============================================
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE INITAB'
      CALL INITAB( SIBOR%I, MESH%IFABOR%I, NELEM2)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE INITAB'
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE IMPR'
      CALL IMPR(LISPRD,LT,AT,LT,3)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE IMPR'
C
C
C
C=====C
C  7  C APPLIES BOUNDARY CONDITIONS AT FIRST TIME STEP
C=====C============================================================
      I1=NF+1
      I2=NF+NPLAN
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LIMWAC'
      CALL LIMWAC
     &(SF%R    , SFBOR%R , SLIFBR%I , NPTFR  , NPLAN , NF    ,
     & STETA%R , SFR%R   , NPOIN2   , NBOR   , AT    , LT    ,
     & DDC     , LIMSPE  , FPMAXL   , FETCHL , SIGMAL, SIGMBL,
     & GAMMAL  , FPICL   , HM0L     , APHILL , TETA1L, SPRE1L,
     & TETA2L  , SPRE2L  , XLAMDL   , MESH%X%R   ,
     & MESH%Y%R, KENT    , KSORT    , WAC_FILES(WACFO1)%LU  ,
     & WAC_FILES(WACBI1)%LU         , BINBI1 ,
     & SUV%R   , SVV%R   , SPEULI   , VENT  , VENSTA, GRAVIT ,
     & DEUPI   , SPRIVE%R, NPRIV    , STRA31%R , STRA32%R,
     & DEPTH   , FRABL   ,BOUNDARY_COLOUR%I)
       IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE LIMWAC'
C
C
C
C=====C COMPUTES THE WAVE NUMBER (XK), THE GROUP VELOCITY (CG) AND
C  8  C THE CONVERSION FACTOR (B) TO GO FROM (KX, KY) TO (FR, TETA)
C=====C=================================================================
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE INIPHY'
      CALL INIPHY
     &( SXK%R   , SCG%R , SB%R , SDEPTH%R , SFR%R ,
     &  SCOSF%R , NPOIN2   , NF      , PROINF      , SPHE     )
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE INIPHY'
C
C=====C
C  8B C INITIALISES (SETS TO 0) THE SPECTRUM FOR ALL THE NODES WHERE PROF
C=====C=======================================================
C
      IF(.NOT.PROINF) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE ECRETE'
        CALL ECRETE(SF%R,SDEPTH%R,NPOIN2,NPLAN,NF,PROMIN)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE ECRETE'
      ENDIF
C
C
C=====C
C  9  C GRAPHICAL OUTPUT (OPTIONAL) FOR INITIAL TIME STEP
C=====C===================================================
C
C.....9.1 OUTPUT POINTS FOR THE DIRECTIONAL SPECTRUM
C     """""""""""""""""""""""""""""""""""""""""""""""""""""""
C
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PRELEO_MPI'
      CALL PRELEO_MPI
     &(XLEO,YLEO,NPLEO,MESH%X%R,MESH%Y%R,MESH%IKLE%I,MESH%SURDET%R,
     & NPOIN2,NELEM2,NOLEO,ISLEO)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PRELEO_MPI'
C
C.....9.2 CHECKS WHETHER PRINTOUTS ARE REQUIRED OR NOT
C     """"""""""""""""""""""""""""""""""""""""""
      IMPRES=.FALSE.
      DEBRES=.FALSE.
      IF (LT.EQ.GRADEB) THEN
        IMPRES=.TRUE.
        DEBRES=.TRUE.
      ENDIF
C
      IF (IMPRES) THEN
C
C.....9.3 WRITES (OPTIONAL) THE VARIABLES ON THE 2D MESH
C     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
C
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE DUMP2D'
        CALL DUMP2D(LT,DEUPI,SF%R,NPTL)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE DUMP2D'
C
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE BIEF_DESIMP'
        CALL BIEF_DESIMP(WAC_FILES(WACRES)%FMT,VARSOR,
     &            HIST,0,NPOIN2,WAC_FILES(WACRES)%LU,'STD',AT,
     &            LT,GRAPRD,GRAPRD,
     &            SORLEO,SORIMP,MAXVAR,TEXTE,GRADEB,GRADEB)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BIEF_DESIMP'
C
C.....9.4 WRITES (OPTIONAL) THE DIRECTIONAL SPECTRUM
C     """""""""""""""""""""""""""""""""""""""""""""""""""""""
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE ECRSPE'
        CALL ECRSPE
     &( SF%R    , STRA01%R , STETA%R, NPLAN ,
     &  SFR%R   , NF  , NF   , NPOIN2      , AT ,LT ,
     &  STRA01%R, SITR01%I, NOLEO , NPLEO , WAC_FILES(WACLEO)%LU ,
     &  WAC_FILES(WACLEO)%FMT, DEBRES , TITCAS , DATE , TIME ,
     &  ISLEO , MESH%KNOLG%I )
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE ECRSPE'
C
      ENDIF
C
C
C=====C
C  10 C PREPARES FOR PROPAGATION COMPUTATION (CHARACTERISTICS METHOD)
C=====C===============================================================
      IF (PROP) THEN
        CALL IMPR(LISPRD,LT,AT,LT,1)
        CALL IMPR(LISPRD,LT,AT,LT,2)
        NRK=3
C
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PREPRO'
        CALL PREPRO
     & ( SCX%R    , SCY%R     , SCT%R    , SCF%R   , DT    ,
     &   NRK      , MESH%X%R  , MESH%Y%R , STETA%R ,
     &   SCOSTE%R , SSINTE%R  , SFR%R    , MESH%IKLE%I     ,
     &   SIBOR%I  , SETAP1%I  , STRA01%R , SSHP1%R ,
     &   SSHP2%R  , SSHP3%R   , SSHZ%R   , SSHF%R  ,
     &   SELT%I   , SETA%I    , SFRE%I   , SDEPTH%R,
     &   SDZHDT%R , SDZX%R    , SDZY%R   , SUC%R   ,
     &   SVC%R    , SDUX%R    , SDUY%R   , SDVX%R  ,
     &   SDVY%R   , SXK%R     , SCG%R    , SCOSF%R ,
     &   STGF%R   , SITR01%I  , NPOIN3   , NPOIN2  , NELEM2,
     &   NPLAN    , NF        , MESH%SURDET%R, COURAN,
     &   SPHE     , PROINF   , PROMIN,MESH)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PREPRO'
      ENDIF
C
C=====C
C  11 C MAIN LOOP IN TIME
C=====C============================
      DO 10 LT=1,NIT
C
C.....11.1 ASSIGNS THE END DATE OF THE CURRENT TIME STEP
C     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      AT=AT+DT
      CALL IMPR(LISPRD,LT,AT,LT,3)
C
C.....11.2 APPLIES THE BOUNDARY CONDITIONS
C     """"""""""""""""""""""""""""""""""""""""""""
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LIMWAC'
      CALL LIMWAC
     &(SF%R    , SFBOR%R , SLIFBR%I , NPTFR  , NPLAN , NF    ,
     & STETA%R , SFR%R   , NPOIN2      , NBOR   , AT    , LT    ,
     & DDC        , LIMSPE     , FPMAXL      , FETCHL , SIGMAL, SIGMBL,
     & GAMMAL     , FPICL      , HM0L        , APHILL , TETA1L, SPRE1L,
     & TETA2L     , SPRE2L     , XLAMDL      , MESH%X%R   ,
     & MESH%Y%R   , KENT       , KSORT       , WAC_FILES(WACFO1)%LU ,
     & WAC_FILES(WACBI1)%LU    , BINBI1 ,
     & SUV%R   , SVV%R   , SPEULI      , VENT  , VENSTA, GRAVIT ,
     & DEUPI      , SPRIVE%R, NPRIV      , STRA31%R , STRA32%R,
     & DEPTH      , FRABL      ,BOUNDARY_COLOUR%I)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE LIMWAC'
C
C.....11.2B SETS THE SPECTRUM TO 0 FOR THE NODES WHERE PROF
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      IF (.NOT.PROINF) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE ECRETE'
        CALL ECRETE( SF%R    , SDEPTH%R, NPOIN2, NPLAN , NF , PROMIN)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE ECRETE'
      ENDIF
C
      IF (MAREE) THEN
       LT1=MAX((LT/LAM)*LAM,2)
       IF (LT.EQ.LT1) THEN
        DO IP=1,NPOIN2
          DEPTH(IP)=ZREPOS-ZF(IP)
        ENDDO
C
C......11.3 UPDATES THE BATHYMETRY AND CURRENTS
C      """"""""""""""""""""""""""""""""""""""""
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CORMAR'
        CALL CORMAR
     &( AT    , LT    , TC1   , TC2   , TV1   , TV2   , TM1   , TM2   ,
     &  NPC   , NPM   , NVHMA , NVCOU )
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CORMAR'
        DO IP=1,NPOIN2
          IF (DEPTH(IP).LT.PROMIN) DEPTH(IP)=0.9D0*PROMIN
        ENDDO
C
C......11.4 PREPARES THE PROPAGATION (CHARACTERISTICS METHOD)
C      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE INIPHY'
        CALL INIPHY
     & ( SXK%R  , SCG%R , SB%R , SDEPTH%R , SFR%R  ,
     &   SCOSF%R, NPOIN2   , NF      , PROINF      , SPHE      )
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE INIPHY'
C
        IF (PROP) THEN
         CALL IMPR(LISPRD,LT,AT,LT,1)
         CALL IMPR(LISPRD,LT,AT,LT,2)
         NRK=3
         IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PREPRO'
         CALL PREPRO
     & ( SCX%R    , SCY%R     , SCT%R    , SCF%R   , DT    ,
     &   NRK      , MESH%X%R  , MESH%Y%R , STETA%R ,
     &   SCOSTE%R , SSINTE%R  , SFR%R    , MESH%IKLE%I     ,
     &   SIBOR%I  , SETAP1%I  , STRA01%R , SSHP1%R ,
     &   SSHP2%R  , SSHP3%R   , SSHZ%R   , SSHF%R  ,
     &   SELT%I   , SETA%I    , SFRE%I   , SDEPTH%R,
     &   SDZHDT%R , SDZX%R    , SDZY%R   , SUC%R   ,
     &   SVC%R    , SDUX%R    , SDUY%R   , SDVX%R  ,
     &   SDVY%R   , SXK%R     , SCG%R    , SCOSF%R ,
     &   STGF%R   , SITR01%I  , NPOIN3   , NPOIN2  , NELEM2,
     &   NPLAN    , NF        , MESH%SURDET%R, COURAN,
     &   SPHE     , PROINF    , PROMIN   , MESH)
         IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PREPRO'
        ENDIF
       ENDIF
      ENDIF
C
C.....11.3 PROPAGATION (INTERPOLATION AT THE FOOT OF THE CHARACTERISTICS)
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      IF (PROP) THEN
        CALL IMPR(LISPRD,LT,AT,LT,5)
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PROPA'
        CALL PROPA
     &( SF%R       , SB%R    , SSHP1%R, SSHP2%R, SSHP3%R,
     &  SSHZ%R     , SSHF%R  , SELT%I , SETA%I , SFRE%I ,
     &  MESH%IKLE%I, SETAP1%I, NPOIN3    , NPOIN2    , NELEM2,
     &  NPLAN         , NF   , COURAN    ,STRA01%R   , STRA02%R )
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PROPA'
       ENDIF

C
C.....11.4 INTEGRATES THE SOURCE TERMS
C     """"""""""""""""""""""""""""""""""""
      IF (TSOU) THEN
        CALL IMPR(LISPRD,LT,AT,NSITS,4)
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SEMIMP'
        CALL SEMIMP(SF%R,SXK%R,SFR%R,SDFR%R,SDEPTH%R,SUV%R,SVV%R ,
     &  MESH%X%R      , MESH%Y%R      ,
     &  WAC_FILES(WACVEB)%LU  , WAC_FILES(WACVEF)%LU  , NBOR  , NPTFR ,
     &  DDC   , TV1   , TV2   , NPV   , SXRELV%R      , SYRELV%R      ,
     &  SUV1%R, SVV1%R, SUV2%R, SVV2%R, STETA%R       , SSINTE%R      ,
     &  SCOSTE%R      , INDIV , TAILF , RAISF , GRAVIT, CFROT1, CMOUT1,
     &  CMOUT2, AT    , DTSI  , ROAIR , ROEAU , XKAPPA, BETAM , DECAL ,
     &  CDRAG , ALPHA , ZVENT , NF    , NPLAN , NPOIN2, IANGNL, COEFNL,
     &  F1    , NSITS , SMOUT , SFROT , SVENT , STRIF , VENT  , VENSTA,
     &  VX_CTE, VY_CTE, SBREK , ALFABJ, GAMBJ1, GAMBJ2, IQBBJ , IHMBJ ,
     &  IFRBJ , BORETG, GAMATG, IWHTG , IFRTG , ALFARO, GAMARO, GAM2RO,
     &  IDISRO, IEXPRO, IFRRO , BETAIH, EM2SIH, IFRIH , COEFHS, XDTBRK,
     &  NDTBRK, STRIA , ALFLTA, RFMLTA, KSPB  , BDISPB, BDSSPB, PROINF,
     &  DF_LIM, LIMIT , CIMPLI,
     &  WAC_FILES(WACVEB)%NAME, WAC_FILES(WACVEF)%NAME, BINVEN, NBD   ,
     &  QINDI,STRA41%R,STRA42%R,STRA43%R,STRA44%R,STSTOT%R,
     &  STSDER%R      , STOLD%R       , STNEW%R       , STRA31%R      ,
     &  STRA32%R      , STRA33%R      , STRA34%R      , STRA35%R      ,
     &  STRA36%R      , STRA37%R      , STRA38%R      , STRA39%R      ,
     &  ST1%R , ST2%R , ST3%R , ST4%R , STRA01%R      , SBETA%R  )
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SEMIMP'
      ENDIF
C
C.....11.5 CONVERTS TO ABSOLUTE FREQUENCY (OPTIONAL)
C     """""""""""""""""""""""""""""""""""""""""""""
      IF (COURAN) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TRANSF'
        CALL TRANSF
     &( STRA02%R      , SF%R  , SFR%R , SDFR%R, SCOSTE%R      ,
     &  SSINTE%R      , SUC%R , SVC%R , SXK%R , SITR11%I      ,
     &  SITR12%I      , SITR13%I      , STRA31%R      , STRA32%R      ,
     &  NPOIN2, NPLAN , NF    , RAISF , LT    , GRADEB, GRAPRD)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TRANSF'
      ELSE
        CALL OV ( 'X=Y     ' ,STRA02%R, SF%R , Z , C , NPOIN3*NF )
      ENDIF
C
C.....11.6 CHECKS WHETHER PRINTOUTS ARE REQUIRED OR NOT
C     """""""""""""""""""""""""""""""""""""""""""
      IMPRES=.FALSE.
      DEBRES=.FALSE.
      IF ((LT.GE.GRADEB).AND.(MOD(LT-GRADEB,GRAPRD).EQ.0)) IMPRES=.TRUE.
      IF (LT.EQ.GRADEB) DEBRES=.TRUE.
C
      IF (IMPRES) THEN
C
C.....11.7 WRITES (OPTIONAL) THE VARIABLES ON THE 2D MESH
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
         IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE DUMP2D'
         CALL DUMP2D(LT, DEUPI, STRA02%R, NPTL )
         IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE DUMP2D'
C
         IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE BIEF_DESIMP'
         CALL BIEF_DESIMP(WAC_FILES(WACRES)%FMT,VARSOR,
     &            HIST,0,NPOIN2,WAC_FILES(WACRES)%LU,'STD',AT,
     &            LT,GRAPRD,GRAPRD,
     &            SORLEO,SORIMP,MAXVAR,TEXTE,GRADEB,GRADEB)
         IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BIEF_DESIMP'
C
C.....11.8 WRITES (OPTIONAL) THE DIRECTIONAL SPECTRUM
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""
         IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE ECRSPE'
         CALL ECRSPE
     &( STRA02%R, STRA01%R , STETA%R, NPLAN ,
     &  SFR%R   , NF  , NF   , NPOIN2      , AT ,LT ,
     &  STRA01%R, SITR01%I, NOLEO  , NPLEO, WAC_FILES(WACLEO)%LU ,
     &  WAC_FILES(WACLEO)%FMT , DEBRES, TITCAS, DATE  , TIME ,
     &  ISLEO , MESH%KNOLG%I )
         IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE ECRSPE'
C
      ENDIF
C
   10 CONTINUE
C
C=====C
C  12 C WRITES RESULTS (OPTIONAL) AT THE END OF COMPUTATION
C=====C=====================================================
C
      IF (GLOB)  THEN
        CALL IMPR(LISPRD,NIT,AT,NIT,6)
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SOR3D'
        CALL SOR3D
     &( SF%R  , NPLAN  , NF       , STETA%R   , SFR%R ,
     &  NELEM2, NPOIN2 , AT       , SUC%R     , SVC%R ,
     &  SUV%R , SVV%R  , SDEPTH%R , VENT      , COURAN   ,
     &  MAREE , TITCAS , WAC_FILES(WACRBI)%LU , BINRBI    )
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SOR3D'
      ENDIF
C
C----------------------------------------------------------------------
C
C  VALIDATES THE RESULTS AGAINST A REFERENCE FILE
C
      IF(VALID) CALL BIEF_VALIDA(BST1,TEXTE,
     &                   WAC_FILES(WACREF)%LU,WAC_FILES(WACREF)%FMT,
     &                   VARSOR,TEXTE,
     &                   WAC_FILES(WACRES)%LU,WAC_FILES(WACRES)%FMT,
     &                   MAXVAR,NPOIN2,NIT,NIT,ALIRE)
C
C----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C