C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ALLOCATES THE STRUCTURES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC2D :<br>
!> @link DECLARATIONS_TELEMAC2D::A23 A23@endlink, 
!> @link DECLARATIONS_TELEMAC2D::A32 A32@endlink, 
!> @link DECLARATIONS_TELEMAC2D::AK AK@endlink, 
!> @link DECLARATIONS_TELEMAC2D::AKN AKN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::AKTILD AKTILD@endlink, 
!> @link DECLARATIONS_TELEMAC2D::AM1 AM1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::AM2 AM2@endlink, 
!> @link DECLARATIONS_TELEMAC2D::AM3 AM3@endlink, 
!> @link DECLARATIONS_TELEMAC2D::AMPL AMPL@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ATBOR ATBOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::AUBOR AUBOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::BM1 BM1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::BM1S BM1S@endlink, 
!> @link DECLARATIONS_TELEMAC2D::BM2 BM2@endlink, 
!> @link DECLARATIONS_TELEMAC2D::BM2S BM2S@endlink, 
!> @link DECLARATIONS_TELEMAC2D::BOUNDARY_COLOUR BOUNDARY_COLOUR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::BTBOR BTBOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::C0 C0@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CF CF@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CFBOR CFBOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CHBORD CHBORD@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CHESTR CHESTR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CLH CLH@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CLU CLU@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CLV CLV@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CM1 CM1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CM2 CM2@endlink, 
!> @link DECLARATIONS_TELEMAC2D::COMFLU COMFLU@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CONVV CONVV@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CORCON CORCON@endlink, 
!> @link DECLARATIONS_TELEMAC2D::COTOND COTOND@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CV1 CV1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CV1S CV1S@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CV2 CV2@endlink, 
!> @link DECLARATIONS_TELEMAC2D::CV3 CV3@endlink, 
!> @link DECLARATIONS_TELEMAC2D::DEBFLO DEBFLO@endlink, 
!> @link DECLARATIONS_TELEMAC2D::DEBLAG DEBLAG@endlink, 
!> @link DECLARATIONS_TELEMAC2D::DEFZON DEFZON@endlink, 
!> @link DECLARATIONS_TELEMAC2D::DH DH@endlink, 
!> @link DECLARATIONS_TELEMAC2D::DHN DHN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::DIRBOR DIRBOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::DM1 DM1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::DSZ DSZ@endlink, 
!> @link DECLARATIONS_TELEMAC2D::DU DU@endlink, 
!> @link DECLARATIONS_TELEMAC2D::DV DV@endlink, 
!> @link DECLARATIONS_TELEMAC2D::EBOR EBOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ELTFLO ELTFLO@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ELTLAG ELTLAG@endlink, 
!> @link DECLARATIONS_TELEMAC2D::EP EP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::EPN EPN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::EPTILD EPTILD@endlink, 
!> @link DECLARATIONS_TELEMAC2D::EQUA EQUA@endlink, 
!> @link DECLARATIONS_TELEMAC2D::F F@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FINFLO FINFLO@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FINLAG FINLAG@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FLBOR FLBOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FLBORTRA FLBORTRA@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FLODEL FLODEL@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FLUHBOR FLUHBOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FLUHBTEMP FLUHBTEMP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FLULIM FLULIM@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FLUXT FLUXT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FLUXTEMP FLUXTEMP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FN FN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FNCAR FNCAR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FRICTB FRICTB@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FRTAB FRTAB@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FTILD FTILD@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FU FU@endlink, 
!> @link DECLARATIONS_TELEMAC2D::FV FV@endlink, 
!> @link DECLARATIONS_TELEMAC2D::H H@endlink, 
!> @link DECLARATIONS_TELEMAC2D::H0 H0@endlink, 
!> @link DECLARATIONS_TELEMAC2D::HBOR HBOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::HC HC@endlink, 
!> @link DECLARATIONS_TELEMAC2D::HCSTOK HCSTOK@endlink, 
!> @link DECLARATIONS_TELEMAC2D::HN HN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::HPROP HPROP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::HSTOK HSTOK@endlink, 
!> @link DECLARATIONS_TELEMAC2D::HT HT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::HTILD HTILD@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ICONVF ICONVF@endlink, 
!> @link DECLARATIONS_TELEMAC2D::IELM0 IELM0@endlink, 
!> @link DECLARATIONS_TELEMAC2D::IELM1 IELM1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::IELME IELME@endlink, 
!> @link DECLARATIONS_TELEMAC2D::IELMH IELMH@endlink, 
!> @link DECLARATIONS_TELEMAC2D::IELMK IELMK@endlink, 
!> @link DECLARATIONS_TELEMAC2D::IELMT IELMT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::IELMU IELMU@endlink, 
!> @link DECLARATIONS_TELEMAC2D::IFAMAS IFAMAS@endlink, 
!> @link DECLARATIONS_TELEMAC2D::IKLFLO IKLFLO@endlink, 
!> @link DECLARATIONS_TELEMAC2D::IORDRH IORDRH@endlink, 
!> @link DECLARATIONS_TELEMAC2D::IORDRU IORDRU@endlink, 
!> @link DECLARATIONS_TELEMAC2D::IT1 IT1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::IT2 IT2@endlink, 
!> @link DECLARATIONS_TELEMAC2D::IT3 IT3@endlink, 
!> @link DECLARATIONS_TELEMAC2D::IT4 IT4@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ITURB ITURB@endlink, 
!> @link DECLARATIONS_TELEMAC2D::I_ORIG I_ORIG@endlink, 
!> @link DECLARATIONS_TELEMAC2D::J_ORIG J_ORIG@endlink, 
!> @link DECLARATIONS_TELEMAC2D::KBOR KBOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::KFROPT KFROPT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::KFRO_B KFRO_B@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LIHBOR LIHBOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LIMKEP LIMKEP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LIMPRO LIMPRO@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LIMTRA LIMTRA@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LINDDP LINDDP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LINDNER LINDNER@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LINDSP LINDSP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LISTIN LISTIN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LITBOR LITBOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LIUBOR LIUBOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LIVBOR LIVBOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LOGFR LOGFR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MASK MASK@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MASKEL MASKEL@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MASKPT MASKPT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MASKTR MASKTR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MAT MAT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MAXV MAXV@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MAXVAR MAXVAR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MAXZ MAXZ@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MBOR MBOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MESH MESH@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MSK MSK@endlink, 
!> @link DECLARATIONS_TELEMAC2D::MSKSEC MSKSEC@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NCP NCP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NDEFMA NDEFMA@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NDEF_B NDEF_B@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NFLOT NFLOT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NITFLO NITFLO@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NKFROT NKFROT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NLAG NLAG@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NPERIAF NPERIAF@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NPRIV NPRIV@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NTRAC NTRAC@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NUMDIG NUMDIG@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NUMLIQ NUMLIQ@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NVARCL NVARCL@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NWEIRS NWEIRS@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NZONMX NZONMX@endlink, 
!> @link DECLARATIONS_TELEMAC2D::OPDTRA OPDTRA@endlink, 
!> @link DECLARATIONS_TELEMAC2D::OPDVIT OPDVIT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::OPTASS OPTASS@endlink, 
!> @link DECLARATIONS_TELEMAC2D::OPTBAN OPTBAN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::PATMOS PATMOS@endlink, 
!> @link DECLARATIONS_TELEMAC2D::PHAS PHAS@endlink, 
!> @link DECLARATIONS_TELEMAC2D::PHIDIG PHIDIG@endlink, 
!> @link DECLARATIONS_TELEMAC2D::PRIVE PRIVE@endlink, 
!> @link DECLARATIONS_TELEMAC2D::PRODUC PRODUC@endlink, 
!> @link DECLARATIONS_TELEMAC2D::QU QU@endlink, 
!> @link DECLARATIONS_TELEMAC2D::QV QV@endlink, 
!> @link DECLARATIONS_TELEMAC2D::RHS RHS@endlink, 
!> @link DECLARATIONS_TELEMAC2D::RO RO@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ROVAR ROVAR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::S S@endlink, 
!> @link DECLARATIONS_TELEMAC2D::SECMOU SECMOU@endlink, 
!> @link DECLARATIONS_TELEMAC2D::SHPFLO SHPFLO@endlink, 
!> @link DECLARATIONS_TELEMAC2D::SHPLAG SHPLAG@endlink, 
!> @link DECLARATIONS_TELEMAC2D::SLVK SLVK@endlink, 
!> @link DECLARATIONS_TELEMAC2D::SLVPRO SLVPRO@endlink, 
!> @link DECLARATIONS_TELEMAC2D::SLVTRA SLVTRA@endlink, 
!> @link DECLARATIONS_TELEMAC2D::SMH SMH@endlink, 
!> @link DECLARATIONS_TELEMAC2D::SMTR SMTR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::SOLSYS SOLSYS@endlink, 
!> @link DECLARATIONS_TELEMAC2D::SORIMP SORIMP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::SORLEO SORLEO@endlink, 
!> @link DECLARATIONS_TELEMAC2D::SPHERI SPHERI@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T T@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T2DSEC T2DSEC@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T2D_FILES T2D_FILES@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TB TB@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TBOR TBOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TE1 TE1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TE2 TE2@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TE3 TE3@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TE4 TE4@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TE5 TE5@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TEXP TEXP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TEXTE TEXTE@endlink, 
!> @link DECLARATIONS_TELEMAC2D::THOMFR THOMFR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TIMP TIMP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TM1 TM1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TMAXV TMAXV@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TMAXZ TMAXZ@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TN TN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TSCEXP TSCEXP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TTILD TTILD@endlink, 
!> @link DECLARATIONS_TELEMAC2D::U U@endlink, 
!> @link DECLARATIONS_TELEMAC2D::UBOR UBOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::UCONV UCONV@endlink, 
!> @link DECLARATIONS_TELEMAC2D::UDEL UDEL@endlink, 
!> @link DECLARATIONS_TELEMAC2D::UN UN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::UNK UNK@endlink, 
!> @link DECLARATIONS_TELEMAC2D::UNSV2D UNSV2D@endlink, 
!> @link DECLARATIONS_TELEMAC2D::UTILD UTILD@endlink, 
!> @link DECLARATIONS_TELEMAC2D::V V@endlink, 
!> @link DECLARATIONS_TELEMAC2D::V2DPAR V2DPAR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VARCL VARCL@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VARCLA VARCLA@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VARSOR VARSOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VBOR VBOR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VCONV VCONV@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VDEL VDEL@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VENT VENT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VISC VISC@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VISCT VISCT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VISC_S VISC_S@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VN VN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VOLU2D VOLU2D@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VTILD VTILD@endlink, 
!> @link DECLARATIONS_TELEMAC2D::W W@endlink, 
!> @link DECLARATIONS_TELEMAC2D::W1 W1@endlink, 
!> @link DECLARATIONS_TELEMAC2D::WINDX WINDX@endlink, 
!> @link DECLARATIONS_TELEMAC2D::WINDY WINDY@endlink, 
!> @link DECLARATIONS_TELEMAC2D::XFLOT XFLOT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::XLAG XLAG@endlink, 
!> @link DECLARATIONS_TELEMAC2D::YFLOT YFLOT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::YLAG YLAG@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ZCONV ZCONV@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ZDIG ZDIG@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ZF ZF@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ZFE ZFE@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ZFLATS ZFLATS@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ZONE ZONE@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::ADV_CAR ADV_CAR@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_NSC_NC ADV_NSC_NC@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_SUP ADV_SUP@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CFG, CFGBOR, ERR, I, I3, I4, IELB1, IELBE, IELBH, IELBK, IELBT, IELBU, IELBX, IELMC1, IELMC2, IELMHT, IELMUT, IELMX, ITRAC, J, MEMW1, NTR, NTRKE, NTRT, TYP
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> DIM, IKLE, LV, MXELVS, MXPTVS, NELEM, NELMAX, NPMAX, NPOIN, NPTFR, NPTFRX, PRIVE1, PRIVE2, PRIVE3, PRIVE4, T1, T10, T11, T12, T2, T3, T4, T5, T6, T7, T8, T9, TYPELM, X, Y
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ADDBLO(), ALLBLO(), ALLMAT(), ALLVEC(), ALLVEC_IN_BLOCK(), ALMESH(), IELBOR(), NBFEL(), NBMPTS(), NBPEL(), OS(), PLANTE(), READ_SECTIONS_TELEMAC2D()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>HOMERE_TELEMAC2D()

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
!> </td><td> 24/03/2010
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 26/11/2009
!> </td><td>
!> </td><td> SPECIFIC ADVECTION IF EQUA='SAINT-VENANT VF', NO
!>           OTHER METHOD USED (CHARACTERISTICS, ETC.)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 02/04/2009
!> </td><td>
!> </td><td> T2D_FILES(T2DGEO)%LU REPLACES NGEO
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 02/10/2008
!> </td><td>
!> </td><td> NTR=22
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 11/07/2008
!> </td><td>
!> </td><td> SIZE FOR LIMPRO
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
                        SUBROUTINE POINT_TELEMAC2D
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER MEMW1,NTR,NTRT,NTRKE,I,J,I3,I4,ITRAC
      INTEGER IELMX,IELMC1,IELMC2,IELMUT,IELMHT
      INTEGER IELBU,IELBH,IELBT,IELBK,IELBE,IELB1
      INTEGER IELBX,CFG(2),CFGBOR(2),ERR
C
      CHARACTER*1 TYP
C
      INTRINSIC MAX
C
C-----------------------------------------------------------------------
C
      IF(LISTIN) THEN
         IF(LNG.EQ.1) WRITE(LU,20)
         IF(LNG.EQ.2) WRITE(LU,21)
      ENDIF
20    FORMAT(1X,///,26X,'*****************************',/,
     &26X,              '* ALLOCATION DE LA MEMOIRE  *',/,
     &26X,              '*****************************',/)
21    FORMAT(1X,///,26X,'*****************************',/,
     &26X,              '*    MEMORY ORGANIZATION    *',/,
     &26X,              '*****************************',/)
C
C-----------------------------------------------------------------------
C
C     TYPES OF DISCRETISATIONS
C
      IELM0 = 10*(IELMH/10)
      IELM1 = IELM0 + 1
C
      IELB1 = IELBOR(IELM1,1)
      IELBU = IELBOR(IELMU,1)
      IELBH = IELBOR(IELMH,1)
      IELBT = IELBOR(IELMT,1)
      IELBK = IELBOR(IELMK,1)
      IELBE = IELBOR(IELME,1)
C
      IELMX=MAX(IELMU,IELMH,IELMT,IELMK,IELME)
C
C TYPE OF STORAGE AND PRODUCT MATRIX X VECTOR
C
      CFG(1) = OPTASS
      CFG(2) = PRODUC
C     CFG FOR THE BOUNDARY MATRICES
      CFGBOR(1) = 1
      CFGBOR(2) = 1
C
C=======================================================================
C
C     ALLOCATES THE MESH STRUCTURE
C
      CALL ALMESH(MESH,'MESH  ',IELMX,SPHERI,CFG,T2D_FILES(T2DGEO)%LU,
     &            EQUA,I3=I3,I4=I4)
C     IF COORDINATES OF ORIGIN ARE IN GEOMETRY FILE AND NOT IN STEERING
C     FILE, THE VALUES OF GEOMETRY FILE ARE TAKEN
      IF(I3.NE.0.AND.I_ORIG.EQ.0) I_ORIG=I3
      IF(I4.NE.0.AND.J_ORIG.EQ.0) J_ORIG=I4
C
C     ALIAS FOR CERTAIN COMPONENTS OF MESH
C
      IKLE  => MESH%IKLE
      X     => MESH%X%R
      Y     => MESH%Y%R
C
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
C
C=======================================================================
C
C                     **********************
C                     *   REAL ARRAYS      *
C                     **********************
C
C-----------------------------------------------------------------------
C
      ALLOCATE(W(NPOIN),STAT=ERR)
      IF(ERR.NE.0) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'POINT_TELEMAC2D : MAUVAISE ALLOCATION DE W'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'POINT_TELEMAC2D: WRONG ALLOCATION OF W'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C                       ******************
C                       *   STRUCTURES   *
C                       ******************
C
C-----------------------------------------------------------------------
C
C  ALLOCATES AN EMPTY STRUCTURE
C
      CALL ALLVEC(1,S,'S     ',0,1,1)
C
C  ARRAYS CONTAINING THE VARIABLES WHICH WILL BE OUTPUT TO THE RESULT FILE:
C
      CALL ALLVEC(1,U,'U     ',IELMU,1,1)
      CALL ALLVEC(1,V,'V     ',IELMU,1,1)
      CALL ALLVEC(1,H,'H     ',IELMH,1,1)
C
C  ARRAYS CONTAINING THE ADVECTED VARIABLES U, V, T, K AND EPSILON
C
      CALL ALLVEC(1,UTILD,'UTILD ',IELMU,1,2)
      CALL ALLVEC(1,VTILD,'VTILD ',IELMU,1,2)
      CALL ALLVEC(1,HTILD,'HTILD ',IELMH,1,2)
C
C  ARRAYS CONTAINING THE VARIABLES U, V, H STORED AT TIME N
C
      CALL ALLVEC(1,UN,'UN    ', IELMU,1,1 )
      CALL ALLVEC(1,VN,'VN    ', IELMU,1,1 )
      CALL ALLVEC(1,HN,'HN    ', IELMH,1,1 )
C
C  ARRAYS STORING THE RELATIVE CHANGES
C
      CALL ALLVEC(1,DH  ,'DH    ' , IELMH ,1,2 )
      IF(IORDRU.EQ.2) THEN
        CALL ALLVEC(1,DU  ,'DU    ' , IELMU , 1,2 )
        CALL ALLVEC(1,DV  ,'DV    ' , IELMU , 1,2 )
      ELSE
        CALL ALLVEC(1,DU  ,'DU    ' , 0     , 1,0 )
        CALL ALLVEC(1,DV  ,'DV    ' , 0     , 1,0 )
      ENDIF
      IF(IORDRH.EQ.2) THEN
        CALL ALLVEC(1,DHN ,'DHN   ' , IELMH , 1,2 )
      ELSE
        CALL ALLVEC(1,DHN ,'DHN   ' , 0     , 1,0 )
      ENDIF
C
C  BLOCK OF THE UNKNOWNS IN PROPAG
C
      CALL ALLBLO(UNK,'UNK   ')
      CALL ADDBLO(UNK,DH)
      CALL ADDBLO(UNK, U)
      CALL ADDBLO(UNK, V)
C
C  BOUNDARY CONDITIONS ARRAYS (BOUNDARY ARRAYS)
C  FOR UBOR AND VBOR, SIZE 2 TO ALLOW VELOCITIES
C  OR FLOWRATES IMPOSED BY FUNCTION
C
      CALL ALLVEC(1,UBOR    ,'UBOR  ',IELBU,2,1)
      CALL ALLVEC(1,VBOR    ,'VBOR  ',IELBU,2,1)
      CALL ALLVEC(1,HBOR    ,'HBOR  ',IELBH,1,1)
      CALL ALLVEC(1,AUBOR   ,'AUBOR ',IELBU,1,1)
      CALL ALLVEC(1,FLBOR   ,'FLBOR ',IELBH,1,1)
      CALL ALLVEC(1,FLBORTRA,'FLBTRA',IELBT,1,1)
C
C  BLOCK OF DIRICHLET CONDITIONS TO PREPARE CALL TO DIRICH
C
      CALL ALLBLO(DIRBOR,'DIRBOR')
      CALL ADDBLO(DIRBOR,HBOR)
      CALL ADDBLO(DIRBOR,UBOR)
      CALL ADDBLO(DIRBOR,VBOR)
C
C BOTTOM ELEVATION ARRAY:
C
      CALL ALLVEC(1,ZF,'ZF    ',IELMH,1,1)
C
C BOTTOM ELEVATION ARRAY BY ELEMENT (TIDAL FLATS)
C
      IF(MSK) THEN
        CALL ALLVEC(1,ZFE,'ZFE   ',IELM0,1,1)
      ELSE
        CALL ALLVEC(1,ZFE,'ZFE   ',    0,1,0)
      ENDIF
C
C VISCOSITY : FOR NOW IN P1
C             BUT SIZE 2 TO CATER FOR ELDER'S MODEL
C
      IF(ITURB.EQ.2) THEN
        CALL ALLVEC(1,VISC ,'VISC  ',IELM1,3,1)
      ELSE
        CALL ALLVEC(1,VISC ,'VISC  ',IELM1,1,1)
      ENDIF
C     BACKUP ARRAY FOR VISCOSITY
      IF(OPDVIT.EQ.2.OR.(NTRAC.GT.0.AND.OPDTRA.EQ.2)) THEN
        IF(ITURB.EQ.2) THEN
          CALL ALLVEC(1,VISC_S,'VISC_S',IELM1,3,1)
        ELSE
          CALL ALLVEC(1,VISC_S,'VISC_S',IELM1,1,1)
        ENDIF
      ENDIF
C
C  FRICTION COEFFICIENT
C
      CALL ALLVEC(1,CHESTR,'CHESTR',IELMU,1,1)
C
C  ARRAYS FOR ATMOSPHERIC AND INCIDENT WAVE CONDITIONS
C
      CALL ALLVEC(1,C0    ,'C0    ',IELBH,1,1)
      CALL ALLVEC(1,COTOND,'COTOND',IELBH,1,1)
      CALL ALLVEC(1,PATMOS,'PATMOS',IELMH,1,1)
      IF(ROVAR) THEN
        CALL ALLVEC(1,RO,'RO    ',IELMH,1,1)
      ELSE
        CALL ALLVEC(1,RO,'RO    ',    0,1,0)
      ENDIF
C     WIND GIVEN IN P1
      IF(VENT) THEN
        CALL ALLVEC(1,WINDX,'WINDX ',IELM1,1,1)
        CALL ALLVEC(1,WINDY,'WINDY ',IELM1,1,1)
      ELSE
        CALL ALLVEC(1,WINDX,'WINDX ',    0,1,0)
        CALL ALLVEC(1,WINDY,'WINDY ',    0,1,0)
      ENDIF
C
C  SOURCE TERM ARRAYS
C
      CALL ALLVEC(1,FU,'FU    ',IELMU,1,2)
      CALL ALLVEC(1,FV,'FV    ',IELMU,1,2)
C
C  POINTERS FOR THE MATRICES
C
      IELMHT = IELMH
C     AM1 USED FOR THE TRACERS
      IF(NTRAC.GT.0) IELMHT = MAX(IELMHT,IELMT)
      CALL ALLMAT(AM1,'AM1   ',IELMHT,IELMHT,CFG,'Q','Q')
C
      TYP='Q'
      IF(ICONVF(1).NE.ADV_SUP    .AND.
     &   ICONVF(1).NE.ADV_NSC_NC .AND.
     &   3*(SLVPRO%PRECON/3).NE.SLVPRO%PRECON) TYP = 'S'
C
      IF(OPDVIT.EQ.2) TYP='Q'
C
      IELMUT = IELMU
C     AM2 AND AM3 USED FOR THE TRACERS
      IF(NTRAC.GT.0) THEN
        IELMUT = MAX(IELMU,IELMT)
        TYP='Q'
      ENDIF
C     AM2 AND AM3 MODIFIED FOR BOUSSINESQ
      IF(EQUA(1:10).EQ.'BOUSSINESQ') THEN
        TYP='Q'
      ENDIF
      CALL ALLMAT(AM2,'AM2   ',IELMUT,IELMUT,CFG,'Q',TYP)
      CALL ALLMAT(AM3,'AM3   ',IELMUT,IELMUT,CFG,'Q',TYP)
C
C  BM1 AND BM2:
C
      CALL ALLMAT(BM1,'BM1   ',IELMH,IELMU,CFG,'Q','Q')
      CALL ALLMAT(BM2,'BM2   ',IELMH,IELMU,CFG,'Q','Q')
C
C  STORES CV1, BM1 AND BM2 FOR CORRECTION FOR CONTINUITY
C
      IF(CORCON.AND.SOLSYS.EQ.1) THEN
        CALL ALLMAT(BM1S,'BM1S  ',IELMH,IELMU,CFG,'Q','Q')
        CALL ALLMAT(BM2S,'BM2S  ',IELMH,IELMU,CFG,'Q','Q')
        CALL ALLVEC(1,CV1S,'CV1S  ',IELMX,1,2)
      ELSE
        CALL ALLMAT(BM1S,'BM1S  ',IELMH,IELMU,CFG,'0','0')
        CALL ALLMAT(BM2S,'BM2S  ',IELMH,IELMU,CFG,'0','0')
        CALL ALLVEC(1,CV1S,'CV1S  ',0,1,0)
      ENDIF
C
C  CM1 AND CM2:
C
      IELMC1 = IELMH
      IELMC2 = IELMU
C     CM2 USED FOR U IN SOME CASES
      IF(ICONVF(1).EQ.ADV_SUP.OR.ICONVF(1).EQ.ADV_NSC_NC) THEN
        IELMC1 = MAX(IELMC1,IELMU)
      ENDIF
      IF(EQUA(1:10).EQ.'BOUSSINESQ') IELMC1 = MAX(IELMC1,IELMU)
C
      CALL ALLMAT(CM1,'CM1   ',IELMC1,IELMC2,CFG,'Q','Q')
      CALL ALLMAT(CM2,'CM2   ',IELMC1,IELMC2,CFG,'Q','Q')
      CALL ALLMAT(TM1,'TM1   ',IELMU ,IELMU ,CFG,'Q','Q')
C
C  BOUNDARY MATRIX
C
      IELBX = MAX(IELBU,IELBH,IELBT,IELBK,IELBE)
      CALL ALLMAT(MBOR,'MBOR  ',IELBX,IELBX,CFGBOR,'Q','Q')
C
C  MATRICES A23 AND A32 USED FOR DIAGONAL-BLOCK PRECONDITIONING
C  OR FOR THE BOUSSINESQ EQUATIONS
C
      TYP = '0'
      IF(3*(SLVPRO%PRECON/3).EQ.SLVPRO%PRECON) TYP = 'Q'
      IF(EQUA(1:10).EQ.'BOUSSINESQ') TYP = 'Q'
      CALL ALLMAT(A23,'A23   ',IELMU,IELMU,CFG,TYP,TYP)
      CALL ALLMAT(A32,'A32   ',IELMU,IELMU,CFG,TYP,TYP)
C
C BLOCK OF THE MATRICES IN PROPAG
C
      CALL ALLBLO(MAT,'MAT   ')
      CALL ADDBLO(MAT,AM1)
      CALL ADDBLO(MAT,BM1)
      CALL ADDBLO(MAT,BM2)
      CALL ADDBLO(MAT,CM1)
      CALL ADDBLO(MAT,AM2)
      CALL ADDBLO(MAT,A23)
      CALL ADDBLO(MAT,CM2)
      CALL ADDBLO(MAT,A32)
      CALL ADDBLO(MAT,AM3)
C
C WORKING ARRAY W1 (SIZE TO BE CHECKED)
C
C     NECESSARY MEMORY FOR W1 IN VALIDA
      MEMW1 = 9*NPOIN
C     FINITE VOLUMES
      IF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
        MEMW1 = MAX(MEMW1,9*NPOIN+3*NPTFR,2*MXPTVS*NPOIN)
      ENDIF
C     THIS MEMORY SPACE IS RESERVED IN THE FORM OF ONE
C     ARRAY P0 OF SIZE 2
      MEMW1 = MAX(3,1+MEMW1/NBMPTS(IELM0))
      CALL ALLVEC(1,W1,'W1    ',IELM0,MEMW1,1)
C
C_______________________________________________________________________
C
C  POINTERS FOR THE SECOND MEMBERS OF THE PROPAGATION STEP
C
C_______________________________________________________________________
C
      CALL ALLVEC(1,CV1,'CV1   ',IELMX,1,2)
      CALL ALLVEC(1,CV2,'CV2   ',IELMU,1,2)
      CALL ALLVEC(1,CV3,'CV3   ',IELMU,1,2)
C
C  BLOCK OF THE SECOND MEMBERS IN PROPAG
C
      CALL ALLBLO(RHS,'RHS   ')
      CALL ADDBLO(RHS,CV1)
      CALL ADDBLO(RHS,CV2)
      CALL ADDBLO(RHS,CV3)
C_______________________________________________________________________
C
C  POINTERS FOR THE SOURCE TERMS OF THE PROPAGATION STEP
C
C_______________________________________________________________________
C
      CALL ALLVEC(1,SMH,'SMH   ',IELMX,1,2)
C_______________________________________________________________________
C
C  POINTERS FOR ADVECTION AND PROPAGATION FIELDS
C_______________________________________________________________________
C
      CALL ALLVEC(1,UCONV,'UCONV ',IELMU,1,1)
      CALL ALLVEC(1,VCONV,'VCONV ',IELMU,1,1)
      CALL ALLVEC(1,HPROP,'HPROP ',IELMH,1,1)
C_______________________________________________________________________
C
C  POINTERS FOR INTEGRAL OF THE BASES, IN PARALLEL, AND REVERSE
C_______________________________________________________________________
C
      CALL ALLVEC(1,VOLU2D,'VOLU2D',IELMH,1,1)
      CALL ALLVEC(1,V2DPAR,'V2DPAR',IELMH,1,1)
      CALL ALLVEC(1,UNSV2D,'UNSV2D',IELMH,1,1)
C_______________________________________________________________________
C
C  POINTERS USED FOR LAGRANGIAN DRIFTS
C_______________________________________________________________________
C
      CALL ALLVEC(1,XLAG  ,'XLAG  ',NPOIN*NLAG             ,1,0)
      CALL ALLVEC(1,YLAG  ,'YLAG  ',NPOIN*NLAG             ,1,0)
      CALL ALLVEC(1,SHPLAG,'SHPLAG',NPOIN*NBPEL(IELM1)*NLAG,1,0)
C
C-----------------------------------------------------------------------
C
C  POINTERS FOR WORKING ARRAYS:
C
C-----------------------------------------------------------------------
C
C  NUMBER OF ARRAYS TO BE ALLOCATED : NTR
C           21 : POUR CGSTAB =3 X 7, 22 POUR CVDFTR (APPEL DE CVTRVF)
      NTR = 22
      IF(SLVPRO%SLV.EQ.7) NTR = MAX(NTR,6+6*SLVPRO%KRYLOV)
C     6 ADDITIONAL DIAGONALS TO STORE IN BLOCK-DIAGONAL PRECONDITIONING
      IF(3*(SLVPRO%PRECON/3).EQ.SLVPRO%PRECON) NTR = NTR + 6
C
C  MAXIMUM USEFUL SIZE
C
      NTRT=0
      IF(NTRAC.GT.0) THEN
C       NTRT = 7
C       BECAUSE OF THE POSITION OF TRACERS IN VARSOR (WILL BE
C       THE SAME IN TB, USED BY VALIDA)
        NTRT = 31+NTRAC
        IF(SLVTRA%SLV.EQ.7) NTRT = MAX(2+2*SLVTRA%KRYLOV,NTRT)
        NTR = MAX(NTR,NTRT)
      ENDIF
      NTRKE=0
      IF(ITURB.EQ.3) THEN
        NTRKE=7
        IF(SLVK%SLV.EQ.7) NTRKE = MAX(NTRKE,2+2*SLVK%KRYLOV)
        NTR  = MAX(NTR,NTRKE)
      ENDIF
C
C  ALLOCATES NTR WORKING ARRAYS (SIZE: THE MAXIMUM NUMBER OF
C                                      DEGREES OF FREEDOM)
C
C     TB WILL CONTAIN ARRAYS T1,T2,...
C
      CALL ALLBLO(TB ,'TB    ')
C
      CALL ALLVEC_IN_BLOCK(TB,NTR,1,'TB    ',IELMX,1,2)
C
C     ALIAS FOR THE FIRST 12 WORKING ARRAYS OF THE BLOCK: TB
C
      T1 =>TB%ADR( 1)%P
      T2 =>TB%ADR( 2)%P
      T3 =>TB%ADR( 3)%P
      T4 =>TB%ADR( 4)%P
      T5 =>TB%ADR( 5)%P
      T6 =>TB%ADR( 6)%P
      T7 =>TB%ADR( 7)%P
      T8 =>TB%ADR( 8)%P
      T9 =>TB%ADR( 9)%P
      T10=>TB%ADR(10)%P
      T11=>TB%ADR(11)%P
      T12=>TB%ADR(12)%P
C
C  ALLOCATES WORKING ARRAYS (SIZE: THE MAXIMUM NUMBER OF ELEMENTS)
C
C
      CALL ALLVEC(1,TE1,'TE1   ',IELM0,1,1)
      CALL ALLVEC(1,TE2,'TE2   ',IELM0,1,1)
      CALL ALLVEC(1,TE3,'TE3   ',IELM0,1,1)
      IF(OPTBAN.EQ.1.OR.OPTBAN.EQ.3) THEN
C       PIECE-WISE LINEAR FREE SURFACE
        CALL ALLVEC(1,ZFLATS, 'ZFLATS',IELM0,3,1)
      ELSE
        CALL ALLVEC(1,ZFLATS, 'ZFLATS',    0,1,1)
      ENDIF
      IF(OPTBAN.EQ.3) THEN
        CALL ALLVEC(1,TE4,'TE4   ',IELM0,1,1)
        CALL ALLVEC(1,TE5,'TE5   ',IELM0,1,1)
      ELSE
        CALL ALLVEC(1,TE4,'TE4   ',    0,1,0)
        CALL ALLVEC(1,TE5,'TE5   ',    0,1,0)
      ENDIF
C
C-----------------------------------------------------------------------
C !JAJ #### IF REQUIRED, WE READ HERE THE INPUT SECTIONS FILE
C      AND MODIFY NCP AND CTRLSC(1:NCP) ACCORDINGLY IN READ_SECTIONS
C
      IF (TRIM(T2D_FILES(T2DSEC)%NAME)/='') THEN
        WRITE(LU,*)
     &   'POINT_TELEMAC2D: SECTIONS DEFINED IN THE SECTIONS INPUT FILE'
        CALL READ_SECTIONS_TELEMAC2D
      ELSE ! THE EARLIER WAY OF DOING THINGS
        IF (NCP.NE.0) WRITE(LU,*)
     &   'POINT_TELEMAC2D: SECTIONS DEFINED IN THE PARAMETER FILE'
      ENDIF
C
C     BLOCK OF MASKS FOR THE COMPUTATION OF FLUXES ACCROSS SECTIONS
C     ONLY WITH COMPATIBLE FLUXES
C
      CALL ALLBLO(MSKSEC,'MSKSEC')
      IF(NCP.GT.1.AND.COMFLU) THEN
        CALL ALLVEC_IN_BLOCK(MSKSEC,NCP/2,1,'MSKS  ',IELM0,1,1)
      ENDIF
C
C-----------------------------------------------------------------------
C
C POINTERS OF THE MASKS
C
C     BLOCK OF THE MASKS FOR BOUNDARY CONDITIONS
C     (PROPAGATION)
C
      CALL ALLBLO(MASK,'MASK  ')
      CALL ALLVEC_IN_BLOCK(MASK,11,1,'MASK  ',IELBH,1,2)
C
      IF(MSK) THEN
        CALL ALLVEC(1,MASKEL,'MASKEL',IELM0,1,1)
        CALL ALLVEC(1,MASKPT,'MASKPT',IELMX,1,1)
      ELSE
        CALL ALLVEC(1,MASKEL,'MASKEL',    0,1,0)
        CALL ALLVEC(1,MASKPT,'MASKPT',    0,1,0)
      ENDIF
C
C  ADDITIONAL ARRAYS IF THERE ARE TRACERS
C
      CALL ALLBLO(T      ,'T     ')
      CALL ALLBLO(TTILD  ,'TTILD ')
      CALL ALLBLO(TN     ,'TN    ')
      CALL ALLBLO(TEXP   ,'TEXP  ')
      CALL ALLBLO(TIMP   ,'TIMP  ')
      CALL ALLBLO(TSCEXP ,'TSCEXP')
      CALL ALLBLO(VISCT  ,'VISCT ')
      CALL ALLBLO(MASKTR ,'MASKTR')
      CALL ALLBLO(TBOR   ,'TBOR  ')
      CALL ALLBLO(ATBOR  ,'ATBOR ')
      CALL ALLBLO(BTBOR  ,'BTBOR ')
      CALL ALLBLO(LITBOR ,'LITBOR')
      IF(NTRAC.GT.0) THEN
        CALL ALLVEC_IN_BLOCK(T     ,NTRAC,1,'T     ',IELMT,1,1)
        CALL ALLVEC_IN_BLOCK(TTILD ,NTRAC,1,'TTILD ',IELMT,1,1)
        CALL ALLVEC_IN_BLOCK(TN    ,NTRAC,1,'TN    ',IELMT,1,1)
        CALL ALLVEC_IN_BLOCK(TEXP  ,NTRAC,1,'TEXP  ',IELMT,1,1)
        CALL ALLVEC_IN_BLOCK(TIMP  ,NTRAC,1,'TIMP  ',IELMT,1,1)
        CALL ALLVEC_IN_BLOCK(TSCEXP,NTRAC,1,'TSCEXP',IELMT,1,1)
        IF(ITURB.EQ.2) THEN
          CALL ALLVEC_IN_BLOCK(VISCT,NTRAC,1,'VISCT ',IELMT,3,1)
        ELSE
          CALL ALLVEC_IN_BLOCK(VISCT,NTRAC,1,'VISCT ',IELMT,1,1)
        ENDIF
        CALL ALLVEC_IN_BLOCK(MASKTR,4,1,'MSKTR ',IELBH,1,2)
        IF(THOMFR) THEN
C         SECOND DIMENSION USED AS A WORKING ARRAY
C         IN THOMPS
          CALL ALLVEC_IN_BLOCK(TBOR,NTRAC,1,'TBOR  ',IELBT,2,1)
        ELSE
          CALL ALLVEC_IN_BLOCK(TBOR,NTRAC,1,'TBOR  ',IELBT,1,1)
        ENDIF
        CALL ALLVEC_IN_BLOCK(ATBOR  ,NTRAC,1,'ATBOR ',IELBT,1,1)
        CALL ALLVEC_IN_BLOCK(BTBOR  ,NTRAC,1,'BTBOR ',IELBT,1,1)
        CALL ALLVEC_IN_BLOCK(LITBOR ,NTRAC,2,'LITBOR',IELBT,1,1)
      ELSE
C       AT LEAST ONE ELEMENT IN BLOCKS, NOT NTRAC
        CALL ALLVEC_IN_BLOCK(T     ,1,1,'T     ',0,1,0)
        CALL ALLVEC_IN_BLOCK(TTILD ,1,1,'TTILD ',0,1,0)
        CALL ALLVEC_IN_BLOCK(TN    ,1,1,'TN    ',0,1,0)
        CALL ALLVEC_IN_BLOCK(TEXP  ,1,1,'TEXP  ',0,1,0)
        CALL ALLVEC_IN_BLOCK(TIMP  ,1,1,'TIMP  ',0,1,0)
        CALL ALLVEC_IN_BLOCK(TSCEXP,1,1,'TSCEXP',0,1,0)
        IF(ITURB.EQ.2) THEN
          CALL ALLVEC_IN_BLOCK(VISCT,1,1,'VISCT ',0,3,0)
        ELSE
          CALL ALLVEC_IN_BLOCK(VISCT,1,1,'VISCT ',0,1,0)
        ENDIF
        CALL ALLVEC_IN_BLOCK(MASKTR,4,1,'MSKTR ',0,1,0)
        CALL ALLVEC_IN_BLOCK(TBOR   ,1,1,'TBOR  ',0,1,0)
        CALL ALLVEC_IN_BLOCK(ATBOR  ,1,1,'ATBOR ',0,1,0)
        CALL ALLVEC_IN_BLOCK(BTBOR  ,1,1,'BTBOR ',0,1,0)
        CALL ALLVEC_IN_BLOCK(LITBOR ,1,2,'LITBOR',0,1,0)
      ENDIF
C
C-----------------------------------------------------------------------
C
C  FRICTION COEFFICIENT CF
C
      CALL ALLVEC(1,CF    ,'CF    ',IELMU,1,1)
C
C  DATA FOR FRICTION SET PER ZONE
C
C  FRICTION LAW USED
C
      CALL ALLVEC(2,NKFROT,'NKFROT',IELMU,1,1)
C
C  CHESTR ON THE BOUNDARY
C
      CALL ALLVEC(1,CHBORD,'CHBORD',IELBT,1,1)
C
      IF(FRICTB) THEN
         ALLOCATE(FRTAB%ADR(NZONMX))
         DO I=1,NZONMX
            ALLOCATE(FRTAB%ADR(I)%P)
         ENDDO
         CALL ALLVEC(2,KFROPT,'KFROPT',IELMU,1,1)
         CALL ALLVEC(1,NDEFMA,'NDEFMA',IELMU,1,1)
         IF(LINDNER) THEN
           CALL ALLVEC(1,LINDDP,'LINDDP',IELMU,1,1)
           CALL ALLVEC(1,LINDSP,'LINDSP',IELMU,1,1)
         ELSE
           CALL ALLVEC(1,LINDDP,'LINDDP',0,1,0)
           CALL ALLVEC(1,LINDSP,'LINDSP',0,1,0)
         ENDIF
         CALL ALLVEC(1,NDEF_B,'NDEF_B',IELBT,1,1)
         CALL ALLVEC(1,KFRO_B,'KFRO_B',IELBT,1,1)
      ELSE
         CALL ALLVEC(2,KFROPT,'KFROPT',0,1,0)
         CALL ALLVEC(1,NDEFMA,'NDEFMA',0,1,0)
         CALL ALLVEC(1,LINDDP,'LINDDP',0,1,0)
         CALL ALLVEC(1,LINDSP,'LINDSP',0,1,0)
         CALL ALLVEC(1,NDEF_B,'NDEF_B',0,1,0)
         CALL ALLVEC(1,KFRO_B,'KFRO_B',0,1,0)
      ENDIF
C
C  END OF DATA FOR FRICTION SET PER ZONE
C
C  ADDITIONAL ARRAY IF THE K-EPSILON MODEL IS USED
C
      IF(ITURB.EQ.3) THEN
        CALL ALLVEC(1,AK     ,'AK    ',IELMK,1,1)
        CALL ALLVEC(1,EP     ,'EP    ',IELME,1,1)
        CALL ALLVEC(1,AKN    ,'AKN   ',IELMK,1,1)
        CALL ALLVEC(1,EPN    ,'EPN   ',IELME,1,1)
        CALL ALLVEC(1,AKTILD ,'AKTILD',IELMK,1,1)
        CALL ALLVEC(1,EPTILD ,'EPTILD',IELME,1,1)
        CALL ALLVEC(1,KBOR   ,'KBOR  ',IELBK,1,1)
        CALL ALLVEC(1,EBOR   ,'EBOR  ',IELBE,1,1)
        CALL ALLVEC(1,CFBOR  ,'CFBOR ',IELB1,1,1)
      ELSE
        CALL ALLVEC(1,AK     ,'AK    ',0,1,0)
        CALL ALLVEC(1,EP     ,'EP    ',0,1,0)
        CALL ALLVEC(1,AKN    ,'AKN   ',0,1,0)
        CALL ALLVEC(1,EPN    ,'EPN   ',0,1,0)
        CALL ALLVEC(1,AKTILD ,'AKTILD',0,1,0)
        CALL ALLVEC(1,EPTILD ,'EPTILD',0,1,0)
        CALL ALLVEC(1,KBOR   ,'KBOR  ',0,1,0)
        CALL ALLVEC(1,EBOR   ,'EBOR  ',0,1,0)
        CALL ALLVEC(1,CFBOR  ,'CFBOR ',0,1,0)
      ENDIF
C
      CALL ALLVEC(1,UDEL   ,'UDEL  ',    IELMU,1,1)
      CALL ALLVEC(1,VDEL   ,'VDEL  ',    IELMU,1,1)
      CALL ALLVEC(1,DM1    ,'DM1   ',    IELMU,1,2)
      CALL ALLVEC(1,ZCONV  ,'ZCONV ',       10,3,1)
      CALL ALLVEC(1,FLODEL ,'FLODEL',MESH%NSEG,1,0)
      CALL ALLVEC(1,FLULIM ,'FLULIM',MESH%NSEG,1,0)
C
C-----------------------------------------------------------------------
C
C ALLOCATES THE BLOCKS
C
C     FUNCTIONS TO ADVECT BY CHARACTERISTICS
C
      CALL ALLBLO(FN    , 'FN    ')
      CALL ALLBLO(F     , 'F     ')
      CALL ALLBLO(FTILD , 'FTILD ')
      CALL ALLBLO(FNCAR , 'FNCAR ')
C
      CALL ADDBLO(FN,UN)
      CALL ADDBLO(FN,VN)
      CALL ADDBLO(FN,HN)
      CALL ADDBLO(F ,U )
      CALL ADDBLO(F ,V )
      CALL ADDBLO(F ,H )
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          CALL ADDBLO(FN   ,TN%ADR(ITRAC)%P   )
          CALL ADDBLO(F    ,T%ADR(ITRAC)%P    )
        ENDDO
      ENDIF
      IF(ITURB.EQ.3) THEN
        CALL ADDBLO(FN    ,AKN)
        CALL ADDBLO(FN    ,EPN)
        CALL ADDBLO(F     ,AK )
        CALL ADDBLO(F     ,EP )
      ENDIF
C
C-----------------------------------------------------------------------
C
C     WITH FINITE VOLUMES OR KINETIC SCHEMES ADVECTION IS DONE
C     IN VOLFIN
C
      IF(EQUA(1:15).NE.'SAINT-VENANT VF') THEN
        IF(CONVV(1).AND.ICONVF(1).EQ.ADV_CAR) THEN
          CALL ADDBLO(FTILD,UTILD)
          CALL ADDBLO(FTILD,VTILD)
          CALL ADDBLO(FNCAR,UN   )
          CALL ADDBLO(FNCAR,VN   )
        ENDIF
        IF(CONVV(3).AND.NTRAC.GT.0.AND.ICONVF(3).EQ.ADV_CAR) THEN
          DO ITRAC=1,NTRAC
            CALL ADDBLO(FTILD,TTILD%ADR(ITRAC)%P)
            CALL ADDBLO(FNCAR,TN%ADR(ITRAC)%P)
          ENDDO
        ENDIF
      ENDIF
C
      IF(CONVV(4).AND.ITURB.EQ.3.AND.ICONVF(4).EQ.ADV_CAR) THEN
        CALL ADDBLO(FTILD,AKTILD)
        CALL ADDBLO(FTILD,EPTILD)
        CALL ADDBLO(FNCAR,AKN   )
        CALL ADDBLO(FNCAR,EPN   )
      ENDIF
C
C_______________________________________________________________________
C
C  ARRAYS USED TO FOLLOW THE DRIFTS
C
C_______________________________________________________________________
C
      IF(NFLOT.NE.0) THEN
        CALL ALLVEC(1,XFLOT ,'XFLOT ',NITFLO*NFLOT      ,1,0)
        CALL ALLVEC(1,YFLOT ,'YFLOT ',NITFLO*NFLOT      ,1,0)
        CALL ALLVEC(1,SHPFLO,'SHPFLO',NBPEL(IELM1)*NFLOT,1,0)
      ELSE
        CALL ALLVEC(1,XFLOT ,'XFLOT ',0,1,0)
        CALL ALLVEC(1,YFLOT ,'YFLOT ',0,1,0)
        CALL ALLVEC(1,SHPFLO,'SHPFLO',0,1,0)
      ENDIF
C
C_______________________________________________________________________
C
C  ARRAYS USED FOR WEIRS
C
C-----------------------------------------------------------------------
C
      IF(NWEIRS.NE.0) THEN
C       IN FACT ARRAYS (NWEIRS,NPSMAX) OR NPOIN>NWEIRS*NPSMAX
        CALL ALLVEC(1,ZDIG  ,'ZDIG  ',IELM1,1,1)
        CALL ALLVEC(1,PHIDIG,'PHIDIG',IELM1,1,1)
      ELSE
        CALL ALLVEC(1,ZDIG  ,'ZDIG  ',0,1,0)
        CALL ALLVEC(1,PHIDIG,'PHIDIG',0,1,0)
      ENDIF
C
C-----------------------------------------------------------------------
C
C  ARRAYS AT THE USER'S DISPOSAL
C
      CALL ALLBLO(PRIVE ,'PRIVE ')
C
      IF(NPRIV.GT.0) THEN
C       THESE ARRAYS MUST EXIST BUT CAN BE EMPTY
        CALL ALLVEC_IN_BLOCK(PRIVE,NPRIV,1,'PRIV  ',IELMX,1,2)
      ENDIF
C     AT LEAST 4 ARRAYS ARE REQUIRED BUT THEY CAN BE EMPTY
      IF(NPRIV.LT.4) THEN
        CALL ALLVEC_IN_BLOCK(PRIVE,4-NPRIV,1,'PRIV  ',    0,1,2)
      ENDIF
C
C     ALIAS FOR THE FIRST 4 'PRIVE' ARRAYS
C
      PRIVE1 => PRIVE%ADR(1)%P%R
      PRIVE2 => PRIVE%ADR(2)%P%R
      PRIVE3 => PRIVE%ADR(3)%P%R
      PRIVE4 => PRIVE%ADR(4)%P%R
C
C  BLOCK OF THE CLANDESTINE VARIABLES
C
      CALL ALLBLO(VARCL,'VARCL ')
      CALL ALLVEC_IN_BLOCK(VARCL,NVARCL,1,'CL    ',IELMX,1,2)
C
C     INITIALISES AT 0
C
C
      IF(NVARCL.GT.0) THEN
        DO I=1,NVARCL
          CALL OS('X=C     ',VARCL%ADR(I)%P,VARCL%ADR(I)%P,
     &                       VARCL%ADR(I)%P,0.D0)
        ENDDO
      ENDIF
C
C_______________________________________________________________________
C
C                         * INTEGER ARRAYS *
C_______________________________________________________________________
C
      IF(MSK) THEN
        CALL ALLVEC(2,IFAMAS,'IFAMAS',IELM0,NBFEL(IELM0),1)
      ELSE
        CALL ALLVEC(2,IFAMAS,'IFAMAS',0,1,0)
      ENDIF
      CALL ALLVEC(2,LIUBOR,'LIUBOR',IELBU,1,1)
      CALL ALLVEC(2,LIVBOR,'LIVBOR',IELBU,1,1)
      CALL ALLVEC(2,LIHBOR,'LIHBOR',IELBH,1,1)
C     CLU, CLV AND CLH ARE WORKING ARRAYS IN PROPIN
      CALL ALLVEC(2,CLU            ,'CLU   ',IELBU,1,1)
      CALL ALLVEC(2,CLV            ,'CLV   ',IELBU,1,1)
      CALL ALLVEC(2,CLH            ,'CLH   ',IELBH,1,1)
      CALL ALLVEC(2,BOUNDARY_COLOUR,'BNDCOL',IELB1,1,1)
C
      CALL ALLVEC(2,NUMLIQ,'NUMLIQ',IELB1,1,1)
      IF(ITURB.EQ.3) THEN
        CALL ALLVEC(2,LIMKEP,'LIMKEP',IELB1,2,1)
      ELSE
        CALL ALLVEC(2,LIMKEP,'LIMKEP',    0,2,0)
      ENDIF
      CALL ALLVEC(2,LIMPRO,'LIMPRO',MAX(IELBH,IELBU),6,1)
      CALL ALLVEC(2,LIMTRA,'LIMTRA',IELBT,1,1)
      CALL ALLVEC(2,SECMOU,'SECMOU',IELM0,1,1)
C
C     INTEGER WORKING ARRAY (MINIMUM SIZE NELEM)
C
      IF(IELMX.GT.11) THEN
        CALL ALLVEC(2,IT1,'IT1   ',IELMX,1,2)
        CALL ALLVEC(2,IT2,'IT2   ',IELMX,1,2)
        CALL ALLVEC(2,IT3,'IT3   ',IELMX,1,2)
        CALL ALLVEC(2,IT4,'IT4   ',IELMX,1,2)
      ELSE
        CALL ALLVEC(2,IT1,'IT1   ',   10,1,2)
        CALL ALLVEC(2,IT2,'IT2   ',   10,1,2)
        CALL ALLVEC(2,IT3,'IT3   ',   10,1,2)
        CALL ALLVEC(2,IT4,'IT4   ',   10,1,2)
      ENDIF
C
C_______________________________________________________________________
C
C  ARRAYS USED TO FOLLOW THE DRIFTS
C
C_______________________________________________________________________
C
C     IF THERE ARE NO DRIFTS, NO TEST ON NFLOT
C     IF NFLOT IS 0, THE VECTORS WILL HAVE NO SIZE
      CALL ALLVEC(2,DEBFLO,'DEBFLO',NFLOT         ,1,0)
      CALL ALLVEC(2,FINFLO,'FINFLO',NFLOT         ,1,0)
      CALL ALLVEC(2,ELTFLO,'ELTFLO',NFLOT         ,1,0)
      CALL ALLVEC(2,IKLFLO,'IKLFLO',NFLOT*NITFLO*3,1,0)
C
C_______________________________________________________________________
C
C  ARRAYS USED FOR LAGRANGIAN DRIFTS
C
C-----------------------------------------------------------------------
C
      IF(NLAG.NE.0) THEN
        CALL ALLVEC(2,DEBLAG,'DEBLAG',NLAG      ,1,0)
        CALL ALLVEC(2,FINLAG,'FINLAG',NLAG      ,1,0)
        CALL ALLVEC(2,ELTLAG,'ELTLAG',NLAG*NPOIN,1,0)
      ELSE
        CALL ALLVEC(2,DEBLAG,'DEBLAG',0         ,1,0)
        CALL ALLVEC(2,FINLAG,'FINLAG',0         ,1,0)
        CALL ALLVEC(2,ELTLAG,'ELTLAG',0         ,1,0)
      ENDIF
C
C_______________________________________________________________________
C
C  ARRAYS USED FOR WEIRS
C
C-----------------------------------------------------------------------
C
C     NUMDIG (2, NWEIRS, NPSMAX) IN ACTUAL FACT
C     NPOIN IS GREATER THAN NWEIRS * NPSMAX, WHICH ARE BOTH
C     READ IN THE SINGULARITY FILES
      IF(NWEIRS.NE.0) THEN
        CALL ALLVEC(2,NUMDIG,'NUMDIG',2*NPOIN,1,0)
      ELSE
        CALL ALLVEC(2,NUMDIG,'NUMDIG',    0  ,1,0)
      ENDIF
C
C_______________________________________________________________________
C
C  ARRAYS USED FOR THE ZONE NUMBERS
C
C-----------------------------------------------------------------------
C
      IF(DEFZON) THEN
        CALL ALLVEC(2,ZONE,'ZONE  ',IELM1,1,1)
      ELSE
        CALL ALLVEC(2,ZONE,'ZONE  ',0    ,1,0)
      ENDIF
C
C_______________________________________________________________________
C
C  ARRAYS NOT COMMON TO ALL TYPES OF SOLVED EQUATIONS
C_______________________________________________________________________
C
      CALL ALLBLO(SMTR     ,'SMTR  ')
      CALL ALLBLO(FLUXT    ,'FLUXT ')
      CALL ALLBLO(FLUXTEMP ,'FLUXTE')
      CALL ALLBLO(FLUHBTEMP,'FLUHBT')
      CALL ALLBLO(FLUHBOR  ,'FLUHB ')
      CALL ALLBLO(HT       ,'HT    ')
      IF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
        CALL ALLVEC(1,QU       ,'QU    ',IELM1    ,1        ,1)
        CALL ALLVEC(1,QV       ,'QV    ',IELM1    ,1        ,1)
        CALL ALLVEC(1,HSTOK    ,'HSTOK ',IELM1    ,1        ,1)
        CALL ALLVEC(1,HCSTOK   ,'HCSTOK',2        ,MESH%NSEG,0)
        CALL ALLVEC(1,SMTR     ,'SMTR  ',IELM1    ,1        ,1)
        CALL ALLVEC(2,LOGFR    ,'LOGFR ',IELM1    ,1        ,1)
        CALL ALLVEC(1,HC       ,'HC    ',2        ,MESH%NSEG,0)
        CALL ALLVEC(1,DSZ      ,'DSZ   ',2        ,MESH%NSEG,0)
        IF(NTRAC.GT.0) THEN
          CALL ALLVEC_IN_BLOCK(FLUXT    ,NTRAC,1,'FLUXT ',MESH%NSEG,1,0)
          CALL ALLVEC_IN_BLOCK(FLUXTEMP ,NTRAC,1,'FLUXTE',MESH%NSEG,1,0)
          CALL ALLVEC_IN_BLOCK(FLUHBTEMP,NTRAC,1,'FLUHBT',IELBH    ,1,1)
          CALL ALLVEC_IN_BLOCK(FLUHBOR  ,NTRAC,1,'FLUHB ',IELBH    ,1,1)
          CALL ALLVEC_IN_BLOCK(HT       ,NTRAC,1,'HT    ',IELM1    ,1,1)
          CALL ALLVEC_IN_BLOCK(SMTR     ,NTRAC,1,'SMTR  ',IELM1    ,1,1)
        ELSE
          CALL ALLVEC_IN_BLOCK(FLUXT    ,1,1,'FLUXT ',0,1,0)
          CALL ALLVEC_IN_BLOCK(FLUXTEMP ,1,1,'FLUXTE',0,1,0)
          CALL ALLVEC_IN_BLOCK(FLUHBTEMP,1,1,'FLUHBT',0,1,1)
          CALL ALLVEC_IN_BLOCK(FLUHBOR  ,1,1,'FLUHB ',0,1,1)
          CALL ALLVEC_IN_BLOCK(HT       ,1,1,'HT    ',0,1,1)
          CALL ALLVEC_IN_BLOCK(SMTR     ,1,1,'SMTR  ',0,1,1)
        ENDIF
      ELSE
        CALL ALLVEC(1,QU       ,'QU    ',0 , 1,0)
        CALL ALLVEC(1,QV       ,'QV    ',0 , 1,0)
        CALL ALLVEC(1,HSTOK    ,'HSTOK ',0 , 1,0)
        CALL ALLVEC(1,HCSTOK   ,'HCSTOK',0 , 1,0)
        CALL ALLVEC(2,LOGFR    ,'LOGFR ',0 , 1,0)
        CALL ALLVEC(1,HC       ,'HC    ',0 , 1,0)
        CALL ALLVEC(1,DSZ      ,'DSZ   ',0 , 1,0)
        CALL ALLVEC_IN_BLOCK(FLUXT    ,1,1,'FLUXT ',0,1,0)
        CALL ALLVEC_IN_BLOCK(FLUXTEMP ,1,1,'FLUXTE',0,1,0)
        CALL ALLVEC_IN_BLOCK(FLUHBTEMP,1,1,'FLUHBT',0,1,1)
        CALL ALLVEC_IN_BLOCK(FLUHBOR  ,1,1,'FLUHB ',0,1,1)
        CALL ALLVEC_IN_BLOCK(HT       ,1,1,'HT    ',0,1,1)
        CALL ALLVEC_IN_BLOCK(SMTR     ,1,1,'SMTR  ',0,1,1)
      ENDIF
C
      IF(EQUA(1:10).EQ.'BOUSSINESQ') THEN
        CALL ALLVEC(1,H0  ,'H0    ',IELMH,1,1 )
      ELSE
        CALL ALLVEC(1,H0  ,'H0    ',0 , 1,0 )
      ENDIF
C
C-----------------------------------------------------------------------
C
C    FOR MAX FREE SURFACE ELEVATION, MAX SPEEDS
C    AND CORRESPONDING TIMES
C
      IF(SORLEO(27).OR.SORIMP(27)) THEN
        CALL ALLVEC(1,MAXZ,'MAXZ  ',IELM1,1,1 )
      ELSE
        CALL ALLVEC(1,MAXZ,'MAXZ  ',0    ,1,0 )
      ENDIF
      IF(SORLEO(28).OR.SORIMP(28)) THEN
        CALL ALLVEC(1,TMAXZ,'TMAXZ ',IELM1,1,1 )
      ELSE
        CALL ALLVEC(1,TMAXZ,'TMAXZ ',0    ,1,0 )
      ENDIF
      IF(SORLEO(29).OR.SORIMP(29)) THEN
        CALL ALLVEC(1,MAXV,'MAXV  ',IELM1,1,1 )
      ELSE
        CALL ALLVEC(1,MAXV,'MAXV  ',0    ,1,0 )
      ENDIF
      IF(SORLEO(30).OR.SORIMP(30)) THEN
        CALL ALLVEC(1,TMAXV,'TMAXV ',IELM1,1,1 )
      ELSE
        CALL ALLVEC(1,TMAXV,'TMAXV ',0    ,1,0 )
      ENDIF
C
C    FOR FOURIER ANALYSES
C
      CALL ALLBLO(AMPL,'AMPL  ')
      CALL ALLBLO(PHAS,'PHAS  ')
      IF(NPERIAF.GT.0) THEN
        CALL ALLVEC_IN_BLOCK(AMPL,NPERIAF,1,'AMPL  ',IELM1,1,2)
        CALL ALLVEC_IN_BLOCK(PHAS,NPERIAF,1,'PHAS  ',IELM1,1,2)
      ENDIF
C
C-----------------------------------------------------------------------
C
C COMPUTES THE BLOCK WHICH CONNECTS A VARIABLE NAME
C TO ITS ARRAY
C
      CALL ALLBLO(VARSOR ,'VARSOR')
C 01
      CALL ADDBLO(VARSOR,U)
C 02
      CALL ADDBLO(VARSOR,V)
C 03
      CALL ADDBLO(VARSOR,FU)
C 04
      CALL ADDBLO(VARSOR,H)
C 05
      CALL ADDBLO(VARSOR,FV)
C 06
      CALL ADDBLO(VARSOR,ZF)
C 07
      CALL ADDBLO(VARSOR,T2)
C 08
      CALL ADDBLO(VARSOR,T3)
C 09  OLD TRACER
C     REPEATED HERE BUT NOT USED; MOVED ELSEWHERE
      CALL ADDBLO(VARSOR,T%ADR(1)%P)
C 10
      CALL ADDBLO(VARSOR,AK)
C 11
      CALL ADDBLO(VARSOR,EP)
C 12
      CALL ADDBLO(VARSOR,VISC)
C 13
      CALL ADDBLO(VARSOR,T4)
C 14
      CALL ADDBLO(VARSOR,T5)
C 15
      CALL ADDBLO(VARSOR,T6)
C 16
      CALL ADDBLO(VARSOR,WINDX)
C 17
      CALL ADDBLO(VARSOR,WINDY)
C 18
      CALL ADDBLO(VARSOR,PATMOS)
C 19
      CALL ADDBLO(VARSOR,CHESTR)
C 20
      CALL ADDBLO(VARSOR,T7)
C 21
      CALL ADDBLO(VARSOR,T8)
C 22
      CALL ADDBLO(VARSOR,T9)
C 23
      CALL ADDBLO(VARSOR,PRIVE%ADR(1)%P)
C 24
      CALL ADDBLO(VARSOR,PRIVE%ADR(2)%P)
C 25
      CALL ADDBLO(VARSOR,PRIVE%ADR(3)%P)
C 26
      CALL ADDBLO(VARSOR,PRIVE%ADR(4)%P)
C 27
      CALL ADDBLO(VARSOR,MAXZ)
C 28
      CALL ADDBLO(VARSOR,TMAXZ)
C 29
      CALL ADDBLO(VARSOR,MAXV)
C 30
      CALL ADDBLO(VARSOR,TMAXV)
C 31  FRICTION VELOCITY
      CALL ADDBLO(VARSOR,T7)
C
C     TRACERS
C
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          CALL ADDBLO(VARSOR,T%ADR(ITRAC)%P)
        ENDDO
      ENDIF
C
C     FOURIER ANALYSIS
C
      IF(NPERIAF.GT.0) THEN
        DO I=1,NPERIAF
C         OUTPUT VARIABLES (TO BE CHECKED)
          SORLEO(32+NTRAC+2*(I-1))=.TRUE.
          SORLEO(33+NTRAC+2*(I-1))=.TRUE.
C         END OF OUTPUT VARIABLES (TO BE CHECKED)
          CALL ADDBLO(VARSOR,AMPL%ADR(I)%P)
          CALL ADDBLO(VARSOR,PHAS%ADR(I)%P)
        ENDDO
      ENDIF
C
C     OTHER POSSIBLE VARIABLES ADDED BY USER
C
      J=32+NTRAC+2*NPERIAF
900   CONTINUE
      IF(SORLEO(J).OR.SORIMP(J)) THEN
        IF(NPRIV.LT.J-27-NTRAC-2*NPERIAF) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'POINT : AUGMENTER LE NOMBRE'
            WRITE(LU,*) '        DE TABLEAUX PRIVES'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'POINT : NUMBER OF PRIVATE ARRAYS'
            WRITE(LU,*) '        TOO SMALL'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
        CALL ADDBLO(VARSOR,PRIVE%ADR(J-27-NTRAC-2*NPERIAF)%P)
        J=J+1
        IF(J.LE.MAXVAR) GO TO 900
      ENDIF
C
C     CLANDESTINE VARIABLES
C
      IF(VARCL%N.NE.0) THEN
        DO I=1,VARCL%N
          CALL ADDBLO(VARSOR,VARCL%ADR(I)%P)
          SORLEO(J+I-1)=.TRUE.
          TEXTE(J+I-1)=VARCLA(I)
        ENDDO
      ENDIF
C
C=======================================================================
C
C WRITES OUT TO LISTING :
C
      IF(LISTIN) THEN
         IF(LNG.EQ.1) WRITE(LU,22)
         IF(LNG.EQ.2) WRITE(LU,23)
      ENDIF
22    FORMAT(1X,///,21X,'****************************************',/,
     &21X,              '* FIN DE L''ALLOCATION DE LA MEMOIRE  : *',/,
     &21X,              '****************************************',/)
23    FORMAT(1X,///,21X,'*************************************',/,
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