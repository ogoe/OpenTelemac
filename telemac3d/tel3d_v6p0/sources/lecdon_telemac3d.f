C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       READS THE STEERING FILE USING DAMOCLES.
!><br>            SETS SOME DEFAULT VALUES.
!><br>            CHECKS SOME INCONSISTENCIES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> FILE_DESC, MOTCAR, NCAR, PATH
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_TELEMAC3D :<br>
!> @link DECLARATIONS_TELEMAC3D::AC AC@endlink, 
!> @link DECLARATIONS_TELEMAC3D::AGGLOD AGGLOD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::AGGLOH AGGLOH@endlink, 
!> @link DECLARATIONS_TELEMAC3D::AGGLOU AGGLOU@endlink, 
!> @link DECLARATIONS_TELEMAC3D::ATMOS ATMOS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BANDEC BANDEC@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BETAC BETAC@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BILMAS BILMAS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BINGEO BINGEO@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BINHYD BINHYD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BINPRE BINPRE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BINRES BINRES@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BIRSED BIRSED@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BISUIS BISUIS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::BYPASS BYPASS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::CDTINI CDTINI@endlink, 
!> @link DECLARATIONS_TELEMAC3D::CFDEP CFDEP@endlink, 
!> @link DECLARATIONS_TELEMAC3D::CFMAX CFMAX@endlink, 
!> @link DECLARATIONS_TELEMAC3D::CLDYN CLDYN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::CLIPH CLIPH@endlink, 
!> @link DECLARATIONS_TELEMAC3D::CONPRO CONPRO@endlink, 
!> @link DECLARATIONS_TELEMAC3D::CONSOL CONSOL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::CONV CONV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::CORIOL CORIOL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::COTIMP COTIMP@endlink, 
!> @link DECLARATIONS_TELEMAC3D::COTINI COTINI@endlink, 
!> @link DECLARATIONS_TELEMAC3D::COTINT COTINT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::COUROU COUROU@endlink, 
!> @link DECLARATIONS_TELEMAC3D::D50 D50@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DAMPING DAMPING@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DEBIMP DEBIMP@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DEBU DEBU@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DEBUG DEBUG@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DELTA DELTA@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DENLAW DENLAW@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DIF DIF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DIFF_DEL DIFF_DEL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DIRFLU DIRFLU@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DNUTAH DNUTAH@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DNUTAV DNUTAV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DNUVIH DNUVIH@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DNUVIV DNUVIV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DPWAVEQ DPWAVEQ@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DT DT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DTC DTC@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DUREE DUREE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::ELEMENT ELEMENT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::EPAI0 EPAI0@endlink, 
!> @link DECLARATIONS_TELEMAC3D::FAIR FAIR@endlink, 
!> @link DECLARATIONS_TELEMAC3D::FCOR FCOR@endlink, 
!> @link DECLARATIONS_TELEMAC3D::FLOPRD FLOPRD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::FUAIR FUAIR@endlink, 
!> @link DECLARATIONS_TELEMAC3D::FVAIR FVAIR@endlink, 
!> @link DECLARATIONS_TELEMAC3D::GIBSON GIBSON@endlink, 
!> @link DECLARATIONS_TELEMAC3D::GRADEB GRADEB@endlink, 
!> @link DECLARATIONS_TELEMAC3D::GRAPRD GRAPRD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::GRAV GRAV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::HAULIN HAULIN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::HAUTIN HAUTIN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::HMIN HMIN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::HWIND HWIND@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IELMH IELMH@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IELMU IELMU@endlink, 
!> @link DECLARATIONS_TELEMAC3D::INCHYD INCHYD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IND_S IND_S@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IND_T IND_T@endlink, 
!> @link DECLARATIONS_TELEMAC3D::INFMAS INFMAS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IORDRH IORDRH@endlink, 
!> @link DECLARATIONS_TELEMAC3D::ITURBH ITURBH@endlink, 
!> @link DECLARATIONS_TELEMAC3D::ITURBV ITURBV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::I_ORIG I_ORIG@endlink, 
!> @link DECLARATIONS_TELEMAC3D::J_ORIG J_ORIG@endlink, 
!> @link DECLARATIONS_TELEMAC3D::KFROT KFROT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::KFROTL KFROTL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::KSPRATIO KSPRATIO@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LATIT LATIT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LISDEB LISDEB@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LISFON LISFON@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LISPRD LISPRD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LISRUF LISRUF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LISRUL LISRUL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LISTIN LISTIN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LONGIT LONGIT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::LVMAC LVMAC@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MARDAT MARDAT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MARTIM MARTIM@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MAXFRO MAXFRO@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MAXLU_T3D MAXLU_T3D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MAXTRA MAXTRA@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MAXVA3 MAXVA3@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MAXVAR MAXVAR@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MIXING MIXING@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MPART MPART@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MSK MSK@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MSKUSE MSKUSE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NAMETRAC NAMETRAC@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NCOTE NCOTE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NCOUCH NCOUCH@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NDEBIT NDEBIT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NFLOT NFLOT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NIT NIT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NITFLO NITFLO@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NONHYD NONHYD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NORD NORD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPFMAX NPFMAX@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPLAN NPLAN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPLINT NPLINT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPRIV NPRIV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPTH NPTH@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NREJEU NREJEU@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NSCE NSCE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NSOUSI NSOUSI@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NTRAC NTRAC@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NTRACER NTRACER@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NVIT NVIT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::OPDVIT OPDVIT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::OPTASS OPTASS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::OPTASS2D OPTASS2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::OPTBAN OPTBAN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::OPTDIF OPTDIF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::OPTSOU OPTSOU@endlink, 
!> @link DECLARATIONS_TELEMAC3D::OPTSUP OPTSUP@endlink, 
!> @link DECLARATIONS_TELEMAC3D::OPT_HNEG OPT_HNEG@endlink, 
!> @link DECLARATIONS_TELEMAC3D::PHILAT PHILAT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::PRODUC PRODUC@endlink, 
!> @link DECLARATIONS_TELEMAC3D::PROFVEL PROFVEL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::PROLIN PROLIN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::PROP PROP@endlink, 
!> @link DECLARATIONS_TELEMAC3D::PROTYP PROTYP@endlink, 
!> @link DECLARATIONS_TELEMAC3D::QSCE QSCE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::RAIN RAIN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::RAIN_MMPD RAIN_MMPD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::RAZTIM RAZTIM@endlink, 
!> @link DECLARATIONS_TELEMAC3D::RHO0 RHO0@endlink, 
!> @link DECLARATIONS_TELEMAC3D::RHOS RHOS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::RUGOF0 RUGOF0@endlink, 
!> @link DECLARATIONS_TELEMAC3D::RUGOL0 RUGOL0@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SALI_DEL SALI_DEL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SCHCH SCHCH@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SCHCKE SCHCKE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SCHCTA SCHCTA@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SCHCVI SCHCVI@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SCHDKE SCHDKE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SCHDTA SCHDTA@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SCHDVI SCHDVI@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SEDCO SEDCO@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SEDI SEDI@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SIGMAG SIGMAG@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SLVDKE SLVDKE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SLVDSE SLVDSE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SLVDTA SLVDTA@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SLVDVI SLVDVI@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SLVPOI SLVPOI@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SLVPRJ SLVPRJ@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SLVPRO SLVPRO@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SLVW SLVW@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SORG2D SORG2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SORG3D SORG3D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SORIMP SORIMP@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SORT2D SORT2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SORT3D SORT3D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SPHERI SPHERI@endlink, 
!> @link DECLARATIONS_TELEMAC3D::STA_DIS_CURVES STA_DIS_CURVES@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SUIT2 SUIT2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T0AC T0AC@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DBI1 T3DBI1@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DBI2 T3DBI2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DCLI T3DCLI@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DDL1 T3DDL1@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DDL2 T3DDL2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DDL3 T3DDL3@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DDL4 T3DDL4@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DDL5 T3DDL5@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DDL6 T3DDL6@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DDL7 T3DDL7@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DDL8 T3DDL8@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DDL9 T3DDL9@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DFO1 T3DFO1@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DFO2 T3DFO2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DFON T3DFON@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DGEO T3DGEO@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DHYD T3DHYD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DIMP T3DIMP@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DL10 T3DL10@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DL11 T3DL11@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DLEO T3DLEO@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DPAR T3DPAR@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DPRE T3DPRE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DRBI T3DRBI@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DREF T3DREF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DRES T3DRES@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DRFO T3DRFO@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DSCO T3DSCO@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DSED T3DSED@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DSUS T3DSUS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3DVEF T3DVEF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3D_FILES T3D_FILES@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TAIR TAIR@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TASCE TASCE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TASSE TASSE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TEMP_DEL TEMP_DEL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TETAD TETAD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TETADI TETADI@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TETAH TETAH@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TETAU TETAU@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TETAZCOMP TETAZCOMP@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TEXT3 TEXT3@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TEXTE TEXTE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TEXTP3 TEXTP3@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TEXTPR TEXTPR@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TITCAS TITCAS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TOCD TOCD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TOCE TOCE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TRAC0 TRAC0@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TRACER TRACER@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TRANSF TRANSF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TRBAKE TRBAKE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TRBATA TRBATA@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TRBAVI TRBAVI@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TREST TREST@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TURBA TURBA@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TURBB TURBB@endlink, 
!> @link DECLARATIONS_TELEMAC3D::TURBWC TURBWC@endlink, 
!> @link DECLARATIONS_TELEMAC3D::USCE USCE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VALID VALID@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VARSUB VARSUB@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VELO_DEL VELO_DEL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VELPROBOT VELPROBOT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VELPROLAT VELPROLAT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VENT VENT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VERPROTRA VERPROTRA@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VERPROVEL VERPROVEL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VITIMP VITIMP@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VSCE VSCE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::WAQPRD WAQPRD@endlink, 
!> @link DECLARATIONS_TELEMAC3D::WCHU0 WCHU0@endlink, 
!> @link DECLARATIONS_TELEMAC3D::XSCE XSCE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::YSCE YSCE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::ZSCE ZSCE@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::ADV_CAR ADV_CAR@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_LPO ADV_LPO@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_LPO_TF ADV_LPO_TF@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_NSC ADV_NSC@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_NSC_TF ADV_NSC_TF@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_PSI ADV_PSI@endlink, 
!> @link DECLARATIONS_TELEMAC::ADV_SUP ADV_SUP@endlink, 
!> @link DECLARATIONS_TELEMAC::COUPLING COUPLING@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ADRESS, CODE1, CONVEC, DIFFUS, DIMEN, DOC, I, I_IN_2_LETTERS, J, K, MNEM3, MNEMO, MOTCLE, MOTINT, MOTLOG, MOTREA, NMAX, NOM_CAS, NOM_DIC, NTRTOT, TROUVE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> DAMOCLE(), INCLUS(), MAJUS(), NOMVAR_2D_IN_3D(), NOMVAR_TELEMAC3D(), PLANTE(), READ_SUBMIT(), SORTIE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>HOMERE_TELEMAC3D()

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
!> </td><td> 22/06/2009
!> </td><td> JMH
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/1999
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
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
                  SUBROUTINE LECDON_TELEMAC3D
     &(MOTCAR,FILE_DESC,PATH,NCAR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| FILE_DESC      |---| 
C| MOTCAR         |---| 
C| NCAR           |---| 
C| PATH           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C                                                 NMAX BELOW
      CHARACTER(LEN=144), INTENT(INOUT) :: MOTCAR(300)
      CHARACTER(LEN=144), INTENT(INOUT) :: FILE_DESC(4,300)
      CHARACTER(LEN=250), INTENT(IN) :: PATH
      INTEGER, INTENT(IN) :: NCAR
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL CONVEC, DIFFUS
      INTEGER I,J,K,NTRTOT
!
      CHARACTER(LEN=8) MNEMO(MAXVAR)
      CHARACTER(LEN=8) MNEM3(MAXVA3)
!
      CHARACTER(LEN=250) NOM_CAS,NOM_DIC
      CHARACTER(LEN=24), PARAMETER :: CODE1='TELEMAC3D               '
!
!-----------------------------------------------------------------------
C DECLARATIONS NEEDED TO CALL DAMOCLES
C PARAMETER NMAX - MAX 300 STEERING WORDS ALLOWED
!
      INTEGER, PARAMETER :: NMAX = 300
!
      INTEGER ADRESS(4,NMAX),DIMEN(4,NMAX)
      DOUBLE PRECISION      MOTREA(NMAX)
      INTEGER               MOTINT(NMAX)
      LOGICAL               MOTLOG(NMAX)
      CHARACTER(LEN=72)     MOTCLE(4,NMAX,2)
      INTEGER TROUVE(4,NMAX)
      LOGICAL DOC
!
C END OF VARIABLES FOR DAMOCLES
!-----------------------------------------------------------------------
!
      INTRINSIC MAX,MOD
!
!-----------------------------------------------------------------------
!
      CHARACTER(LEN=2) I_IN_2_LETTERS(32)
      DATA I_IN_2_LETTERS /'1 ','2 ','3 ','4 ','5 ','6 ','7 ','8 ','9 ',
     &                     '10','11','12','13','14','15','16','17','18',
     &                     '19','20','21','22','23','24','25','26','27',
     &                     '28','29','30','31','32'/
!
!-----------------------------------------------------------------------
!
      IF (LNG.EQ.1) WRITE(LU,101)
      IF (LNG.EQ.2) WRITE(LU,102)
!
101   FORMAT(1X,/,19X, '********************************************',/,
     &            19X, '*               LECDON:                    *',/,
     &            19X, '*        AVANT APPEL DE DAMOCLES           *',/,
     &            19X, '********************************************',/)
102   FORMAT(1X,/,19X, '********************************************',/,
     &            19X, '*               LECDON:                    *',/,
     &            19X, '*        BEFORE CALLING DAMOCLES           *',/,
     &            19X, '********************************************',/)
!
!-----------------------------------------------------------------------
C INITIALISATIONS TO CALL DAMOCLES:
C STRINGS MUST BE EQUAL TO ' ' (ONE BLANK CHARACTER)
!
      DO K=1,NMAX
        MOTCAR(K)(1:1)=' '
        DIMEN(1,K) = 0
        DIMEN(2,K) = 0
        DIMEN(3,K) = 0
        DIMEN(4,K) = 0
      ENDDO
!
C DO NOT PRINT THE DICTIONARY OUT
!
      DOC = .FALSE.
!
!
!-----------------------------------------------------------------------
C     OPENS THE DICTIONARY AND STEERING FILES
!-----------------------------------------------------------------------
!
      IF(NCAR.GT.0) THEN
!
        NOM_DIC=PATH(1:NCAR)//'T3DDICO'
        NOM_CAS=PATH(1:NCAR)//'T3DCAS'
!
      ELSE
!
        NOM_DIC='T3DDICO'
        NOM_CAS='T3DCAS'
!
      ENDIF
!
      OPEN(2,FILE=NOM_DIC,FORM='FORMATTED',ACTION='READ')
      OPEN(3,FILE=NOM_CAS,FORM='FORMATTED',ACTION='READ')
!
!-----------------------------------------------------------------------
!
      CALL DAMOCLE( ADRESS , DIMEN  , NMAX   , DOC     , LNG    , LU ,
     &              MOTINT , MOTREA , MOTLOG , MOTCAR  , MOTCLE ,
     &              TROUVE , 2      , 3      , .FALSE. , FILE_DESC )
!
!-----------------------------------------------------------------------
C     CLOSES THE DICTIONARY AND STEERING FILES
!-----------------------------------------------------------------------
!
      CLOSE(2)
      CLOSE(3)
C
C     DECODES THE SUBMIT STRINGS
C
      CALL READ_SUBMIT(T3D_FILES,MAXLU_T3D,CODE1,FILE_DESC,300)
C
C-----------------------------------------------------------------------
C
C     RETRIEVES FILES NUMBERS IN TELEMAC-3D FORTRAN PARAMETERS
C     AT THIS LEVEL LOGICAL UNITS ARE EQUAL TO THE FILE NUMBER
C
      DO I=1,MAXLU_T3D
        IF(T3D_FILES(I)%TELNAME.EQ.'T3DGEO') THEN
C         T3DGEO=T3D_FILES(I)%LU  (IS EQUIVALENT)
          T3DGEO=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DCLI') THEN
          T3DCLI=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DPRE') THEN
          T3DPRE=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DRES') THEN
          T3DRES=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DFON') THEN
          T3DFON=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DSCO') THEN
          T3DSCO=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DHYD') THEN
          T3DHYD=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DFO1') THEN
          T3DFO1=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DFO2') THEN
          T3DFO2=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DBI1') THEN
          T3DBI1=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DBI2') THEN
          T3DBI2=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DSED') THEN
          T3DSED=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DSUS') THEN
          T3DSUS=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DREF') THEN
          T3DREF=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DLEO') THEN
          T3DLEO=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DIMP') THEN
          T3DIMP=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DDL1') THEN
          T3DDL1=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DDL2') THEN
          T3DDL2=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DDL3') THEN
          T3DDL3=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DDL4') THEN
          T3DDL4=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DDL5') THEN
          T3DDL5=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DDL6') THEN
          T3DDL6=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DDL7') THEN
          T3DDL7=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DDL8') THEN
          T3DDL8=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DDL9') THEN
          T3DDL9=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DL10') THEN
          T3DL10=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DL11') THEN
          T3DL11=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DPAR') THEN
          T3DPAR=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DVEF') THEN
          T3DVEF=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DRBI') THEN
          T3DRBI=I
        ELSEIF(T3D_FILES(I)%TELNAME.EQ.'T3DRFO') THEN
          T3DRFO=I
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
C GETS VALUES ACCORDING TO THE KEYWORDS
!
C INTEGER KEYWORDS
!
        NIT       = MAX(MOTINT(ADRESS(1, 1)),0)
        NPLAN     = MAX(MOTINT(ADRESS(1, 2)),2)
        NTRAC     = MAX(MOTINT(ADRESS(1, 3)),0)
        MIXING    = MAX(MOTINT(ADRESS(1, 4)),0)
        NFLOT     = MAX(MOTINT(ADRESS(1, 5)),0)
        FLOPRD    = MAX(MOTINT(ADRESS(1, 6)),1)
        GRAPRD    = MAX(MOTINT(ADRESS(1, 7)),1)
        LISPRD    = MAX(MOTINT(ADRESS(1, 8)),1)
        GRADEB    = MAX(MOTINT(ADRESS(1, 9)),0)
        LISDEB    = MAX(MOTINT(ADRESS(1,10)),0)
        LISFON    = MAX(MOTINT(ADRESS(1,11)),0)
        NSOUSI    = MAX(MOTINT(ADRESS(1,12)),1)
        NPLINT    = MAX(MIN(MOTINT(ADRESS(1,13)),NPLAN-1),1)
        ITURBV    = MOTINT(ADRESS(1,14))
        LISRUF    = MOTINT(ADRESS(1,15))
        LISRUL    = MOTINT(ADRESS(1,16))
        IORDRH    = MOTINT(ADRESS(1,17))
!
        SCHCVI    = MOTINT(ADRESS(1,18))
        SCHCTA    = MOTINT(ADRESS(1,19))
        DAMPING   = MOTINT(ADRESS(1,20))
        SCHCKE    = MOTINT(ADRESS(1,21))
        SCHCH     = MOTINT(ADRESS(1,22))
        SCHDVI    = MOTINT(ADRESS(1,23))
        SCHDTA    = MOTINT(ADRESS(1,24))
        KFROTL    = MOTINT(ADRESS(1,25))
        SCHDKE    = MOTINT(ADRESS(1,26))
!
        SLVDVI%PRECON    =     MOTINT(ADRESS(1,27))
        SLVDTA%PRECON    =     MOTINT(ADRESS(1,28))
        SLVDKE%PRECON    =     MOTINT(ADRESS(1,30))
        SLVPRO%PRECON    =     MOTINT(ADRESS(1,31))
        SLVW%PRECON      =     MOTINT(ADRESS(1,32))
!
        SLVDVI%SLV =     MOTINT(ADRESS(1,33))
        SLVDTA%SLV =     MOTINT(ADRESS(1,34))
        SLVDKE%SLV =     MOTINT(ADRESS(1,36))
        SLVPRO%SLV =     MOTINT(ADRESS(1,37))
        SLVW%SLV   =     MOTINT(ADRESS(1,38))
!
        SLVDVI%NITMAX    = MOTINT(ADRESS(1,39))
        SLVDTA%NITMAX    = MOTINT(ADRESS(1,40))
        SLVDKE%NITMAX    = MOTINT(ADRESS(1,42))
        SLVPRO%NITMAX    = MOTINT(ADRESS(1,43))
        SLVW%NITMAX      = MOTINT(ADRESS(1,44))
!
        NCSIZE    =     MOTINT(ADRESS(1,45))
!
        TRBAVI    =     MOTINT(ADRESS(1,46))
        TRBATA    =     MOTINT(ADRESS(1,47))
        TRBAKE    =     MOTINT(ADRESS(1,49))
!
C NEW ONES 60,61 (50++ FOR SEDIMENT)
!
        LVMAC     =     MOTINT(ADRESS(1,61))
        NPRIV     =     MOTINT(ADRESS(1,62))
C       R3D2D     =     MOTINT(ADRESS(1,63))
C JMH 29/09/99:
        KFROT     =     MOTINT(ADRESS(1,64))
!
C NON-HYDROSTATIC
!
        SLVPOI%PRECON = MOTINT( ADRESS(1, 71) )
        SLVPOI%SLV    = MOTINT( ADRESS(1, 72) )
        SLVPOI%NITMAX = MOTINT( ADRESS(1, 73) )
C       ????          = MOTINT( ADRESS(1, 74) )
C       ??????        = MOTINT( ADRESS(1, 75) )
C       ??????        = MOTINT( ADRESS(1, 76) )
C       ??????        = MOTINT( ADRESS(1, 77) )
        SLVPRJ%PRECON = MOTINT( ADRESS(1, 78) )
        SLVPRJ%SLV    = MOTINT( ADRESS(1, 79) )
        SLVPRJ%NITMAX = MOTINT( ADRESS(1, 80) )
C       ??????        = MOTINT( ADRESS(1, 81) )
C       SMOVEL        = MOTINT( ADRESS(1, 82) )
C       ??????        = MOTINT( ADRESS(1, 83) )
C       ??????        = MOTINT( ADRESS(1, 84) )
        ITURBH        = MOTINT( ADRESS(1, 85) )
        PROTYP        = MOTINT( ADRESS(1, 86) )
        OPTASS        = MOTINT( ADRESS(1,87) )
        OPTASS2D      = OPTASS
C       OPTT2D        = MOTINT( ADRESS(1,88)     )
C       SOLSYS        = MOTINT( ADRESS(1,88) + 1 )
        DENLAW        = MOTINT( ADRESS(1,89) )
        OPTBAN        = MOTINT( ADRESS(1,90) )
        MARDAT(1)     = MOTINT( ADRESS(1,91) )
        MARDAT(2)     = MOTINT( ADRESS(1,91) + 1 )
        MARDAT(3)     = MOTINT( ADRESS(1,91) + 2 )
        MARTIM(1)     = MOTINT( ADRESS(1,92) )
        MARTIM(2)     = MOTINT( ADRESS(1,92) + 1 )
        MARTIM(3)     = MOTINT( ADRESS(1,92) + 2 )
        OPTDIF        = MOTINT( ADRESS(1,93) )
C       HYDSTEP       = MOTINT( ADRESS(1,94) )
        DO K=1,MAXFRO
          PROFVEL(K)=1
          DIRFLU(K)=1
          VERPROVEL(K)=1
          STA_DIS_CURVES(K)=0
        ENDDO
        DO K=1,MAXFRO*MAXTRA
          VERPROTRA(K)=1
        ENDDO
        IF(DIMEN(1,95).GT.0) THEN
          DO K=1,DIMEN(1,95)
            PROFVEL(K) = MOTINT( ADRESS(1,95) + K-1 )
          ENDDO
        ENDIF
        TRANSF= MOTINT( ADRESS(1,96) )
        IF(DIMEN(1,97).GT.0) THEN
          DO K=1,DIMEN(1,97)
            DIRFLU(K) = MOTINT( ADRESS(1,97) + K-1 )
          ENDDO
        ENDIF
        IF(DIMEN(1,98).GT.0) THEN
          DO K=1,DIMEN(1,98)
            VERPROVEL(K) = MOTINT( ADRESS(1,98) + K-1 )
          ENDDO
        ENDIF
        IF(DIMEN(1,99).GT.0) THEN
          DO K=1,DIMEN(1,99)
            VERPROTRA(K) = MOTINT( ADRESS(1,99) + K-1 )
          ENDDO
        ENDIF
        OPTSUP(1)=1
        IF(DIMEN(1,100).GT.0) THEN
          DO K=1,MAX(DIMEN(1,100),4)
            OPTSUP(K) = MOTINT( ADRESS(1,100) + K-1 )
          ENDDO
        ENDIF
C       SO FAR THE SUPG MATRIX IS THE SAME FOR ALL ADVECTIONS
C       SEE IN PRECON
C       OPTSUP(2)=OPTSUP(1)  (THIS IS DEPTH, NO LONGER USED)
        OPTSUP(3)=OPTSUP(1)
        OPTSUP(4)=OPTSUP(1)
        WAQPRD=MOTINT( ADRESS(1,101) )
C       KEYWORD: ORIGIN COORDINATES
        I_ORIG = MOTINT( ADRESS(1,102)   )
        J_ORIG = MOTINT( ADRESS(1,102)+1 )
C       KEYWORD: STAGE-DISCHARGE CURVES
        IF(DIMEN(1,103).NE.0) THEN
          DO K=1,DIMEN(1,103)
           STA_DIS_CURVES(K) = MOTINT( ADRESS(1,103) + K-1 )
          ENDDO
        ENDIF
C       KEYWORD: DEBUGGER
        DEBUG = MOTINT( ADRESS(1,104) )
C       KEYWORD: RECORD IN THE WAVE DRIVEN CURRENTS FILE
        NPTH = MOTINT( ADRESS(1,105) )
C       KEYWORD: TREATMENT OF NEGATIVE DEPTHS
        OPT_HNEG = MOTINT( ADRESS(1,106) )
!
C REAL KEYWORDS
!
        DT        = MOTREA(ADRESS(2, 1))
        GRAV      = MOTREA(ADRESS(2, 2))
        FCOR      = MOTREA(ADRESS(2, 3))
        FAIR      = MOTREA(ADRESS(2, 4))
        FUAIR     = MOTREA(ADRESS(2, 5))
        FVAIR     = MOTREA(ADRESS(2, 6))
        TAIR      = MOTREA(ADRESS(2, 7))
!
        IF(NTRAC.GT.0) THEN
         IF(TROUVE(2,8).EQ.2.AND.DIMEN(2,8).EQ.NTRAC) THEN
           DO I=1,NTRAC
             BETAC(I)  = MOTREA(ADRESS(2, 8)+I-1)
           ENDDO
         ELSEIF(DENLAW.EQ.4) THEN
          IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'AVEC LOI DE DENSITE = 4 DONNER LE MOT-CLE'
          WRITE(LU,*)
     &    'COEFFICIENT DE DILATATION BETA POUR LES TRACEURS'
          WRITE(LU,*) 'POUR TOUS LES TRACEURS'
          ENDIF
          IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'WITH DENSITY LAW = 4 GIVE THE KEY-WORD'
          WRITE(LU,*) 'BETA EXPANSION COEFFICIENT FOR TRACERS'
          WRITE(LU,*) 'FOR ALL TRACERS'
          ENDIF
          CALL PLANTE(1)
          STOP
         ENDIF
         IF(TROUVE(2,9).EQ.2.AND.DIMEN(2,9).GE.NTRAC) THEN
           DO I=1,NTRAC
             T0AC(I)  = MOTREA(ADRESS(2, 9)+I-1)
           ENDDO
         ELSEIF(DENLAW.EQ.4) THEN
          IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'AVEC LOI DE DENSITE = 4 DONNER LE MOT-CLE'
          WRITE(LU,*) 'VALEUR DE REFERENCE DES TRACEURS'
          WRITE(LU,*) 'POUR TOUS LES TRACEURS'
          ENDIF
          IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'WITH DENSITY LAW = 4 GIVE THE KEY-WORD'
          WRITE(LU,*) 'STANDARD VALUE FOR TRACERS'
          WRITE(LU,*) 'FOR ALL TRACERS'
          ENDIF
          STOP
         ENDIF
        ENDIF
!
        RHO0      = MOTREA(ADRESS(2,10))
        RUGOF0    = MOTREA(ADRESS(2,11))
        RUGOL0    = MOTREA(ADRESS(2,12))
!
C (...THE SAME ZERO FOR ALL SOLVERS...)
!
        SLVDVI%ZERO = MOTREA(ADRESS(2,13))
        SLVDTA%ZERO = MOTREA(ADRESS(2,13))
        SLVDKE%ZERO = MOTREA(ADRESS(2,13))
        SLVPRO%ZERO = MOTREA(ADRESS(2,13))
        SLVW%ZERO   = MOTREA(ADRESS(2,13))
        SLVPOI%ZERO = MOTREA(ADRESS(2,13))
        SLVPRJ%ZERO = MOTREA(ADRESS(2,13))
!
        HMIN      = MOTREA(ADRESS(2,14))
        HAULIN    = MOTREA(ADRESS(2,15))
        DNUVIH    = MOTREA(ADRESS(2,16))
        DNUVIV    = MOTREA(ADRESS(2,17))
        DNUTAH    = MOTREA(ADRESS(2,18))
        DNUTAV    = MOTREA(ADRESS(2,19))
!
        IF(NTRAC.GT.0) THEN
          IF(TROUVE(2,20).EQ.2.AND.DIMEN(2,20).GE.NTRAC) THEN
            DO I=1,NTRAC
              TRAC0(I) = MOTREA(ADRESS(2,20)+I-1)
            ENDDO
          ELSE
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'DONNER LE MOT-CLE'
              WRITE(LU,*) 'VALEURS INITIALES DES TRACEURS'
              WRITE(LU,*) 'POUR TOUS LES TRACEURS'
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'GIVE THE KEY-WORD'
              WRITE(LU,*) 'INITIAL VALUES OF TRACERS'
              WRITE(LU,*) 'FOR ALL TRACERS'
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
!
C       = MOTREA(ADRESS(2,21))
!
        SLVDVI%EPS    = MOTREA(ADRESS(2,22))
        SLVDTA%EPS    = MOTREA(ADRESS(2,23))
        SLVDKE%EPS    = MOTREA(ADRESS(2,25))
        SLVPRO%EPS    = MOTREA(ADRESS(2,26))
        SLVW%EPS      = MOTREA(ADRESS(2,27))
!
        TETAH     = MOTREA(ADRESS(2,28))
        TETAU     = MOTREA(ADRESS(2,29))
        AGGLOH    = MOTREA(ADRESS(2,30))
        AGGLOU    = MOTREA(ADRESS(2,31))
        COTINT    = MOTREA(ADRESS(2,32))
!
        NDEBIT=DIMEN(2,33)
        DO I=1,NDEBIT
         DEBIMP(I) = MOTREA(ADRESS(2,33)+I-1)
        ENDDO
!
        NCOTE=DIMEN(2,34)
        DO I=1,NCOTE
         COTIMP(I) = MOTREA(ADRESS(2,34)+I-1)
        ENDDO
!
        NVIT=DIMEN(2,35)
        DO I=1,NVIT
         VITIMP(I) = MOTREA(ADRESS(2,35)+I-1)
        ENDDO
!
C NON-HYDROSTATIC
!
        SLVPOI%EPS = MOTREA( ADRESS(2, 71) )
C       ??????     = MOTREA( ADRESS(2, 72) )
        SLVPRJ%EPS = MOTREA( ADRESS(2, 73) )
C       ?????      = MOTREA( ADRESS(2, 74) )
        PHILAT     = MOTREA( ADRESS(2, 75) )
        DELTA      = MOTREA( ADRESS(2, 76) )
C SOGREAH ADDITIONS
        LATIT      = MOTREA( ADRESS(2, 77) )
        LONGIT     = MOTREA( ADRESS(2, 78) )
        NORD       = MOTREA( ADRESS(2, 79) )
        NSCE=DIMEN(2,80)
        DO I=1,NSCE
          XSCE(I) = MOTREA(ADRESS(2,80)+I-1)
          YSCE(I) = MOTREA(ADRESS(2,81)+I-1)
          ZSCE(I) = MOTREA(ADRESS(2,82)+I-1)
          QSCE(I) = MOTREA(ADRESS(2,83)+I-1)
        ENDDO
!
        IF(NTRAC.GT.0) THEN
         DO I=1,NTRAC
           DO J=1,NSCE
             TASCE(J,I) = MOTREA(ADRESS(2,84)+((J-1)*NTRAC)+I-1)
           ENDDO
         ENDDO
         NTRACER=DIMEN(2,85)
         IF(NTRACER.GT.0) THEN
C          TRACER WILL BE TRACER(NFRLIQ,NTRAC) BUT NFRLIQ UNKNOWN
           DO I=1,NTRACER
             TRACER(I)=MOTREA(ADRESS(2,85)+I-1)
           ENDDO
         ENDIF
        ENDIF
!
        NREJEU = DIMEN(2,86)
        IF(NREJEU.EQ.NSCE) THEN
          DO I=1,NSCE
            USCE(I) = MOTREA(ADRESS(2,86)+I-1)
          ENDDO
        ELSEIF(NREJEU.EQ.0) THEN
          DO I=1,NSCE
            USCE(I) = 0.D0
          ENDDO
        ELSE
          IF(LNG.EQ.1) WRITE(LU,*)
     &      'MAUVAIS NOMBRE DE VITESSES DES SOURCES SELON X'
          IF(LNG.EQ.2) WRITE(LU,*)
     &      'WRONG NUMBER OF VELOCITIES OF SOURCES ALONG X'
          STOP
        ENDIF
!
        IF(DIMEN(2,87).EQ.NSCE) THEN
          DO I=1,NSCE
            VSCE(I) = MOTREA(ADRESS(2,87)+I-1)
          ENDDO
        ELSEIF(DIMEN(2,87).EQ.0) THEN
          DO I=1,NSCE
            VSCE(I) = 0.D0
          ENDDO
        ELSE
          IF(LNG.EQ.1) WRITE(LU,*)
     &      'MAUVAIS NOMBRE DE VITESSES DES SOURCES SELON Y'
          IF(LNG.EQ.2) WRITE(LU,*)
     &      'WRONG NUMBER OF VELOCITIES OF SOURCES ALONG Y'
        ENDIF
!
C END OF SOGREAH ADDITIONS
!
        COTINI = MOTREA( ADRESS(2, 88) )
        HAUTIN = MOTREA( ADRESS(2, 89) )
        TETADI = MOTREA( ADRESS(2, 90) )
        D50    = MOTREA( ADRESS(2, 91) )
C       RELEASE 5.5 : MASS-LUMPING FOR DIFFUSION
        AGGLOD = MOTREA( ADRESS(2, 92) )
C       RELEASE 5.7 :
        DUREE  = MOTREA( ADRESS(2, 93) )
        NIT=MAX(NIT,INT(DUREE/DT+0.5))
        TETAZCOMP = MOTREA( ADRESS(2, 94) )
        RAIN_MMPD = MOTREA( ADRESS(2, 95) )
        KSPRATIO  = MOTREA( ADRESS(2, 96) )
        AC        = MOTREA( ADRESS(2, 97) )
        HWIND     = MOTREA( ADRESS(2, 98) )
!
C LOGICAL KEYWORDS
!
        DEBU      = .NOT.MOTLOG(ADRESS(3,1))
        CONVEC    = MOTLOG(ADRESS(3, 2))
        DIFFUS    = MOTLOG(ADRESS(3, 3))
        PROP      = MOTLOG(ADRESS(3, 4))
        CORIOL    = MOTLOG(ADRESS(3, 5))
        VENT      = MOTLOG(ADRESS(3, 6))
        ATMOS     = MOTLOG(ADRESS(3, 7))
        RAZTIM    = MOTLOG(ADRESS(3, 8))
        SEDI      = MOTLOG(ADRESS(3, 9))
        IF(SEDI.AND.NTRAC.EQ.0) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)
            WRITE(LU,*) 'AVEC SEDIMENT IL FAUT AU MOINS UN TRACEUR'
            WRITE(LU,*)
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*)
            WRITE(LU,*) 'WITH SEDIMENT AT LEAST ONE TRACER NEEDED'
            WRITE(LU,*)
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
        MSKUSE    = MOTLOG(ADRESS(3,10))
        BANDEC    = MOTLOG(ADRESS(3,11))
        PROLIN    = MOTLOG(ADRESS(3,12))
        BILMAS    = MOTLOG(ADRESS(3,13))
        INFMAS    = MOTLOG(ADRESS(3,14))
        RAIN      = MOTLOG(ADRESS(3,15))
        INCHYD    = MOTLOG(ADRESS(3,16))
C       QUABUB    = MOTLOG(ADRESS(3,17))
        VARSUB    = MOTLOG(ADRESS(3,18))
        VALID     = MOTLOG(ADRESS(3,19))
        LISTIN    = MOTLOG(ADRESS(3,61))
!
C NON-HYDROSTATIC
!
        CLDYN     = MOTLOG( ADRESS(3,70) )
        NONHYD    = MOTLOG( ADRESS(3,71) )
        DPWAVEQ   = MOTLOG( ADRESS(3,72) )
!
C SEDIMENT CONSOLIDATION
!
        CONSOL    = MOTLOG( ADRESS(3,73) )
        CONPRO    = MOTLOG( ADRESS(3,74) )
!
        SUIT2     = MOTLOG( ADRESS(3,75) )
C COHESIVE SEDIMENT
        SEDCO     = MOTLOG( ADRESS(3,76) )
C SALINITY AND TEMPERATURE OUTPUT FOR DELWAQ
        SALI_DEL  = MOTLOG( ADRESS(3,77) )
        TEMP_DEL  = MOTLOG( ADRESS(3,78) )
        VELO_DEL  = MOTLOG( ADRESS(3,79) )
        DIFF_DEL  = MOTLOG( ADRESS(3,80) )
C WAVE DRIVEN CURRENTS
        COUROU    = MOTLOG( ADRESS(3,81) )
C BYPASS VOID VOLUMES
        BYPASS    = MOTLOG( ADRESS(3,82) )
C VELOCITY PROJECTED ON SOLID LATERAL BOUNDARIES
        VELPROLAT = MOTLOG( ADRESS(3,83) )
C VELOCITY PROJECTED ON BOTTOM
        VELPROBOT = MOTLOG( ADRESS(3,84) )
!
C CHARACTER KEYWORDS (APPROPRIATELY TRUNCATED!)
!
        TITCAS    = MOTCAR(ADRESS(4, 1))(1:72)
        SORT3D    = MOTCAR(ADRESS(4, 2))(1:72)
        CALL MAJUS(SORT3D)
        SORT2D    = MOTCAR(ADRESS(4, 3))(1:72)
        CALL MAJUS(SORT2D)
!
C FILENAMES FROM THE STEERING FILE
!
C       4 TO 5 : READ AND USED BY LAUNCHING PROCEDURE
        T3D_FILES(T3DGEO)%NAME=MOTCAR( ADRESS(4, 6) )
C       NOMFOR                =MOTCAR( ADRESS(4, 7) )
C       NOMCAS                =MOTCAR( ADRESS(4, 8) )
        T3D_FILES(T3DCLI)%NAME=MOTCAR( ADRESS(4, 9) )
        T3D_FILES(T3DPRE)%NAME=MOTCAR( ADRESS(4,10) )
        T3D_FILES(T3DRES)%NAME=MOTCAR( ADRESS(4,11) )
        T3D_FILES(T3DFON)%NAME=MOTCAR( ADRESS(4,16) )
        T3D_FILES(T3DSCO)%NAME=MOTCAR( ADRESS(4,17) )
C       2D RESULTS FILE
        T3D_FILES(T3DHYD)%NAME=MOTCAR( ADRESS(4,18) )
        T3D_FILES(T3DFO1)%NAME=MOTCAR( ADRESS(4,19) )
        T3D_FILES(T3DFO2)%NAME=MOTCAR( ADRESS(4,20) )
        T3D_FILES(T3DBI1)%NAME=MOTCAR( ADRESS(4,21) )
        T3D_FILES(T3DBI2)%NAME=MOTCAR( ADRESS(4,22) )
        T3D_FILES(T3DREF)%NAME=MOTCAR( ADRESS(4,55) )
C       RESULTS FILE FOR SUBIEF-3D
        T3D_FILES(T3DLEO)%NAME=MOTCAR( ADRESS(4,57) )
C       LIQUID BOUNDARY FILE
        T3D_FILES(T3DIMP)%NAME=MOTCAR( ADRESS(4,58) )
!
C       INITIAL CONDITIONS
        CDTINI    = MOTCAR(ADRESS(4,59))(1:72)
!
        BINGEO    = MOTCAR(ADRESS(4,24))(1:3)
        CALL MAJUS(BINGEO)
        BINRES    = MOTCAR(ADRESS(4,25))(1:3)
        CALL MAJUS(BINRES)
        BINPRE    = MOTCAR(ADRESS(4,26))(1:3)
        CALL MAJUS(BINPRE)
        BINHYD    = MOTCAR(ADRESS(4,27))(1:3)
        CALL MAJUS(BINHYD)
C       FORMAT OF THE GEOMETRY FILE
        T3D_FILES(T3DGEO)%FMT = MOTCAR( ADRESS(4,29) )(1:8)
        CALL MAJUS(T3D_FILES(T3DGEO)%FMT)
C       FORMAT OF THE 3D RESULTS FILE
        T3D_FILES(T3DRES)%FMT = MOTCAR( ADRESS(4,30) )(1:8)
        CALL MAJUS(T3D_FILES(T3DRES)%FMT)
C       FORMAT OF THE 2D RESULTS FILE
        T3D_FILES(T3DHYD)%FMT = MOTCAR( ADRESS(4,30) )(1:8)
        CALL MAJUS(T3D_FILES(T3DHYD)%FMT)
C       FORMAT OF THE PREVIOUS COMPUTATION RESULTS FILE
        T3D_FILES(T3DPRE)%FMT = MOTCAR( ADRESS(4,31) )(1:8)
        CALL MAJUS(T3D_FILES(T3DPRE)%FMT)
!
C       INITIALISES AND READS THE NAMES OF TRACERS
!
        IF(NTRAC.GT.0) THEN
          DO I=1,NTRAC
            IF(LNG.EQ.1) THEN
             NAMETRAC(I) =  'TRACEUR '//I_IN_2_LETTERS(I)//'      '
     &                     // '??              '
            ELSEIF(LNG.EQ.2) THEN
             NAMETRAC(I) =  'TRACER '//I_IN_2_LETTERS(I)//'       '
     &                     // '??              '
            ENDIF
          ENDDO
        ENDIF
        NTRTOT=DIMEN(4,56)
        IF(NTRTOT.GT.0.AND.NTRAC.GT.0) THEN
          DO I=1,NTRTOT
            NAMETRAC(I) = MOTCAR(ADRESS(4,56)+I-1)(1:32)
          ENDDO
        ENDIF
!
        ELEMENT = MOTCAR(ADRESS(4,60))(1:72)
!
C       61: SISYPHE STEERING FILE, NOT READ HERE
!
C       COUPLING IN BIEF DECLARATIONS
        COUPLING = MOTCAR(ADRESS(4,62))(1:74)
C       63-71 : DELWAQ FILES
        T3D_FILES(T3DDL1)%NAME=MOTCAR( ADRESS(4,63) )
        T3D_FILES(T3DDL2)%NAME=MOTCAR( ADRESS(4,64) )
        T3D_FILES(T3DDL3)%NAME=MOTCAR( ADRESS(4,65) )
        T3D_FILES(T3DDL5)%NAME=MOTCAR( ADRESS(4,66) )
        T3D_FILES(T3DDL6)%NAME=MOTCAR( ADRESS(4,67) )
        T3D_FILES(T3DDL7)%NAME=MOTCAR( ADRESS(4,68) )
        T3D_FILES(T3DL11)%NAME=MOTCAR( ADRESS(4,69) )
        T3D_FILES(T3DDL4)%NAME=MOTCAR( ADRESS(4,70) )
        T3D_FILES(T3DDL8)%NAME=MOTCAR( ADRESS(4,71) )
!
        T3D_FILES(T3DDL9)%NAME=MOTCAR( ADRESS(4,76) )
        T3D_FILES(T3DL10)%NAME=MOTCAR( ADRESS(4,77) )
C       STAGE-DISCHARGE CURVES FILE
        T3D_FILES(T3DPAR)%NAME=MOTCAR( ADRESS(4,72) )
C       SOURCES FILE (MUST BE ALSO NOMVEF IN TELEMAC-2D)
        T3D_FILES(T3DVEF)%NAME=MOTCAR( ADRESS(4,73) )
C       BINARY RESULTS FILE
        T3D_FILES(T3DRBI)%NAME=MOTCAR( ADRESS(4,74) )
C       FORMATTED RESULTS FILE
        T3D_FILES(T3DRFO)%NAME=MOTCAR( ADRESS(4,75) )
!
C       76 AND 77: SEE IN DELWAQ FILES ABOVE
!
C       FORMAT OF THE REFERENCE FILE
        T3D_FILES(T3DREF)%FMT = MOTCAR( ADRESS(4,78) )(1:8)
        CALL MAJUS(T3D_FILES(T3DREF)%FMT)
C       FORMAT OF THE BINARY DATA FILE 1
        T3D_FILES(T3DBI1)%FMT = MOTCAR( ADRESS(4,79) )(1:8)
        CALL MAJUS(T3D_FILES(T3DBI1)%FMT)
!
!-----------------------------------------------------------------------
C SEDIMENT - EX-LECSED.F
!
C INTEGERS
!
        NPFMAX        = MOTINT(ADRESS(1,51))
        SLVDSE%SLV    = MOTINT(ADRESS(1,52))
        SLVDSE%PRECON = MOTINT(ADRESS(1,53))
!
C REALS
!
        RHOS      = MOTREA(ADRESS(2,51))
        TOCD      = MOTREA(ADRESS(2,52))
        CFDEP     = MOTREA(ADRESS(2,53))
        EPAI0     = MOTREA(ADRESS(2,54))
        DTC       = MOTREA(ADRESS(2,55))
        CFMAX     = MOTREA(ADRESS(2,56))
        MPART     = MOTREA(ADRESS(2,57))
        TOCE      = MOTREA(ADRESS(2,58))
        TURBA     = MOTREA(ADRESS(2,59))
        TURBB     = MOTREA(ADRESS(2,60))
        WCHU0     = MOTREA(ADRESS(2,61))
!
        NCOUCH    = DIMEN(2,62)
        IF(NCOUCH.NE.0) THEN
          DO K = 1, NCOUCH
            TREST(K) = MOTREA(ADRESS(2,62)+K-1)
          ENDDO
        ENDIF
!
C LOGICAL
!
        TASSE     = MOTLOG(ADRESS(3,51))
        GIBSON    = MOTLOG(ADRESS(3,52))
        TURBWC    = MOTLOG(ADRESS(3,53))
!
C CHARACTERS
!
        T3D_FILES(T3DSED)%NAME=MOTCAR( ADRESS(4,51) )
        T3D_FILES(T3DSUS)%NAME=MOTCAR( ADRESS(4,52) )
!
        BIRSED    = MOTCAR(ADRESS(4,53))(1:3)
        CALL MAJUS(BIRSED)
        BISUIS    = MOTCAR(ADRESS(4,54))(1:3)
        CALL MAJUS(BISUIS)
!
!----------------------------------------------------------------------
C SETS LACKING SOLVER CONFIGURATION PARAMETERS FOR SEDIMENT
!
        SLVDSE%ZERO   = SLVDTA%ZERO
        SLVDSE%EPS    = SLVDTA%EPS
        SLVDSE%NITMAX = SLVDTA%NITMAX
!
!-----------------------------------------------------------------------
C INFORMS THE USER THAT NO LISTING WILL APPEAR
!
      IF(LISTIN) THEN
         IF(LNG.EQ.1) WRITE(LU,103)
         IF(LNG.EQ.2) WRITE(LU,104)
      ELSE
         IF(LNG.EQ.1) WRITE(LU,*) '*** PAS DE LISTING DEMANDE ***'
         IF(LNG.EQ.2) WRITE(LU,*) '*** NO LISTING REQUIRED ***'
      ENDIF
103   FORMAT(1X,/,19X, '********************************************',/,
     &            19X, '*               LECDON:                    *',/,
     &            19X, '*        APRES APPEL DE DAMOCLES           *',/,
     &            19X, '*     VERIFICATION DES DONNEES LUES        *',/,
     &            19X, '*     SUR LE FICHIER DES PARAMETRES        *',/,
     &            19X, '********************************************',/)
104   FORMAT(1X,/,19X, '********************************************',/,
     &            19X, '*               LECDON:                    *',/,
     &            19X, '*        AFTER CALLING DAMOCLES            *',/,
     &            19X, '*        CHECKING OF DATA  READ            *',/,
     &            19X, '*         IN THE STEERING FILE             *',/,
     &            19X, '********************************************',/)
!
!-----------------------------------------------------------------------
C DEDUCES OTHER PARAMETERS
!-----------------------------------------------------------------------
C SEDIMENT CONCENTRATION IS THE LAST ACTIVE TRACER BY DEFINITION
!
C SEDIMENT CONCENTRATION IS ONE OF ACTIVE TRACERS
C TA(1,NTRAC) --> SEDIMENT CONCENTRATION IF SEDIMENTOLOGY
!
      IF (SEDI) THEN
        IF (LNG.EQ.1) WRITE(LU,111)
        IF (LNG.EQ.2) WRITE(LU,112)
111   FORMAT(/,'ATTENTION: LA CONCENTRATION EN SEDIMENT EST LE DERNIER',
     &       /,'=========  TRACEUR DU TABLEAU DES TRACEURS ACTIFS')
112   FORMAT(/,'ATTENTION: THE SEDIMENT CONCENTRATION IS THE LAST',
     &       /,'=========  TRACER OF THE ARRAY OF ACTIVE TRACERS')
!
      ENDIF
!
!-----------------------------------------------------------------------
C SPECIAL TREATMENT IF PARALLELISM
!
      IF(NCSIZE.GT.1.AND.BANDEC.AND.OPTBAN.EQ.2) THEN
         OPTBAN=1
         IF (LNG.EQ.1) WRITE(LU,121)
         IF (LNG.EQ.2) WRITE(LU,122)
121      FORMAT(/,'ATTENTION: VOUS AVEZ CHOISI LE MODE PARALLELE,',
     &          /,'=========  L''OPTION DE TRAITEMENT DES BANCS',
     &          /,'           DECOUVRANTS EST MISE A 1')
!
122       FORMAT(/,'ATTENTION: YOU HAVE CHOSEN PARALLEL MODE,',
     &           /,'=========  THE TIDAL FLATS TREATMENT IS SET TO 1')
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(OPTBAN.EQ.1.AND.OPT_HNEG.EQ.2) THEN
        IF(ABS(AGGLOH-1.D0).GT.0.01D0) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'TRAITEMENT DES HAUTEURS NEGATIVES=2'
            WRITE(LU,*) 'MASS-LUMPING POUR LA HAUTEUR DOIT VALOIR 1.'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'TREATMENT OF NEGATIVE DEPTHS=2'
            WRITE(LU,*) 'MASS-LUMPING FOR DEPTH MUST BE EQUAL TO 1.'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
C GENERALISED SIGMA TRANSFORMATION REQUIRES SPECIAL TREATMENT
C OF ADVECTION TERMS
!
      SIGMAG=.TRUE.
      IF(TRANSF.EQ.1.OR.TRANSF.EQ.2) SIGMAG=.FALSE.
!
!-----------------------------------------------------------------------
C I M P O R T A N T
C TELEMAC2D PARAMETERS ARE SET HERE FOR CALLING IT FROM TELEMAC3D
!
      IELMH = 11
!
      PROP=.FALSE.
!
      IELMU = IELMH
!
      PRODUC = 1
!
      IF(.NOT.BANDEC) THEN
        OPTBAN = 0
        OPT_HNEG = 0
      ENDIF
      IF(OPTBAN.EQ.2) THEN
        MSK  = .TRUE.
C       WITH A NEGATIVE HMIN, MASKBD WILL FAIL
        HMIN = MAX(HMIN,0.D0)
      ELSEIF(MSKUSE) THEN
        MSK  = .TRUE.
      ELSE
C       NOTE JMH : MASKING BY THE USER DOES NOT APPEAR TO HAVE
C                  BEEN ENVISAGED
        MSK = .FALSE.
      ENDIF
!
C     TO KEEP DHN FREE FOR CALLING CONTIN
!
      IF(IORDRH.EQ.2.AND.NONHYD) IORDRH=1
!
C     SUPG OPTIONS
!
      IF(SCHCH.EQ.5) THEN
        OPTSUP(2) = 0
      ELSE
       IF(LNG.EQ.1) THEN
         WRITE(LU,*) 'SCHEMA POUR LA CONVECTION DE LA HAUTEUR'
         WRITE(LU,*) 'EST MAINTENANT OBLIGATOIREMENT 5'
       ENDIF
       IF(LNG.EQ.2) THEN
         WRITE(LU,*) 'SCHEME FOR ADVECTION OF DEPTH'
         WRITE(LU,*) 'MUST NOW ALWAYS BE EQUAL TO 5'
       ENDIF
       CALL PLANTE(1)
       STOP
      ENDIF
!
C     OPTION FOR DIFFUSION OF VELOCITY
!
      OPDVIT = 1
C     OPTSOU : TREATMENT OF SOURCES, NORMAL (1) OR DIRAC (2)
C     HERE 2 BECAUSE IT WILL AVOID AN INTEGRATION OF SMH IN PROPAG
      OPTSOU = 2
      TETAD  = 1.D0
      SPHERI = .FALSE.
      CLIPH  = .TRUE.
!
!-----------------------------------------------------------------------
C SETS VARIABLES WHEN THE ADVECTION STEP IS NOT REQUIRED
!
      IF(.NOT.CONVEC) THEN
        SCHCVI = 0
        SCHCTA = 0
        SCHCKE = 0
      ENDIF
      IF(NTRAC.EQ.0) SCHCTA = 0
      IF(ITURBV.NE.3.AND.ITURBV.NE.7) SCHCKE = 0
!
!-----------------------------------------------------------------------
!
C     IF K-E IS SELECTED FOR VERTICAL TURBULENCE
C     K-E IS MANDATORY FOR HORIZONTAL TURBULENCE (AND REVERSE)
!
      IF(ITURBV.EQ.3.AND.ITURBH.NE.3) THEN
        ITURBH = 3
        IF (LNG.EQ.1) WRITE(LU,*)
     &   'LECDON: MODELE DE TURBULENCE HORIZONTAL FORCE A 3'
        IF (LNG.EQ.2) WRITE(LU,*)
     &   'LECDON: HORIZONTAL TURBULENCE MODEL SET TO 3'
      ENDIF
      IF(ITURBH.EQ.3.AND.ITURBV.NE.3) THEN
        ITURBV = 3
        IF (LNG.EQ.1) WRITE(LU,*)
     &   'LECDON: MODELE DE TURBULENCE VERTICAL FORCE A 3'
        IF (LNG.EQ.2) WRITE(LU,*)
     &   'LECDON: VERTICAL TURBULENCE MODEL SET TO 3'
      ENDIF
!
C     IF K-W IS SELECTED FOR VERTICAL TURBULENCE
C     K-W IS MANDATORY FOR HORIZONTAL TURBULENCE (AND REVERSE)
!
      IF(ITURBV.EQ.7.AND.ITURBH.NE.7) THEN
        ITURBH = 7
        IF (LNG.EQ.1) WRITE(LU,*)
     &   'LECDON: MODELE DE TURBULENCE HORIZONTAL FORCE A 7'
        IF (LNG.EQ.2) WRITE(LU,*)
     &   'LECDON: HORIZONTAL TURBULENCE MODEL SET TO 7'
      ENDIF
      IF(ITURBH.EQ.7.AND.ITURBV.NE.7) THEN
        ITURBV = 7
        IF (LNG.EQ.1) WRITE(LU,*)
     &   'LECDON: MODELE DE TURBULENCE VERTICAL FORCE A 7'
        IF (LNG.EQ.2) WRITE(LU,*)
     &   'LECDON: VERTICAL TURBULENCE MODEL SET TO 7'
      ENDIF
!
C     IF SMAGORINSKI IS SELECTED FOR VERTICAL TURBULENCE
C     IT IS MANDATORY FOR HORIZONTAL TURBULENCE
C     AND NOT REVERSE !!!!!!!!!!!
!
      IF(ITURBV.EQ.4.AND.ITURBH.NE.4) THEN
        ITURBH = 4
        IF (LNG.EQ.1) WRITE(LU,*)
     &   'LECDON: MODELE DE TURBULENCE HORIZONTAL FORCE A 4'
        IF (LNG.EQ.2) WRITE(LU,*)
     &   'LECDON: HORIZONTAL TURBULENCE MODEL SET TO 4'
      ENDIF
!
      IF(ITURBH.NE.1.AND.ITURBH.NE.3.AND.
     &   ITURBH.NE.4.AND.ITURBH.NE.7) THEN
        IF (LNG.EQ.1) WRITE(LU,*)
     &  'LECDON: MODELE DE TURBULENCE HORIZONTAL INCONNU : ',ITURBH
        IF (LNG.EQ.2) WRITE(LU,*)
     &  'LECDON: UNKNOWN HORIZONTAL TURBULENCE MODEL: ',ITURBH
        CALL PLANTE(1)
        STOP
      ENDIF
!
      IF(ITURBV.NE.1.AND.ITURBV.NE.2.AND.ITURBV.NE.3.AND.
     &   ITURBV.NE.4.AND.ITURBV.NE.7) THEN
        IF (LNG.EQ.1) WRITE(LU,*)
     &  'LECDON: MODELE DE TURBULENCE VERTICAL INCONNU : ',ITURBV
        IF (LNG.EQ.2) WRITE(LU,*)
     &  'LECDON: UNKNOWN VERTICAL TURBULENCE MODEL: ',ITURBV
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
C
C BUILDS ARRAY CONV INDICATING IF THERE ARE VARIABLES TO BE TREATED
C FOR EACH ADVECTION SCHEME
!
C     SIZE OF CONV GIVEN IN DECLARATIONS_TELEMAC3D
      DO I=0,15
       CONV(I) = .FALSE.
      ENDDO
!
      IF(SCHCVI.EQ.8.OR.SCHCTA.EQ.8.OR.SCHCKE.EQ.8) THEN
        WRITE(LU,*) 'ADVECTION SCHEME 8 IS NOW NUMBER ',ADV_LPO
      ENDIF
      IF(SCHCVI.EQ.9.OR.SCHCTA.EQ.9.OR.SCHCKE.EQ.9) THEN
        WRITE(LU,*) 'ADVECTION SCHEME 9 IS NOW NUMBER ',ADV_LPO_TF
      ENDIF
!
      IF(  SCHCVI.NE.0      .AND.SCHCVI.NE.ADV_CAR.AND.SCHCVI.NE.ADV_SUP
     &.AND.SCHCVI.NE.ADV_LPO.AND.SCHCVI.NE.ADV_NSC.AND.SCHCVI.NE.ADV_PSI
     &.AND.SCHCVI.NE.ADV_LPO_TF.AND.SCHCVI.NE.ADV_NSC_TF) THEN
         IF(LNG.EQ.1) WRITE(LU,*)
     &  'LECDON: SCHEMA POUR LA CONVECTION DES VITESSES INCONNU : ',
     &                                                            SCHCVI
        IF (LNG.EQ.2) WRITE(LU,*)
     &  'LECDON: SCHEME FOR ADVECTION OF VELOCITIES UNKNOWN: ',   SCHCVI
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(  SCHCTA.NE.0      .AND.SCHCTA.NE.ADV_CAR.AND.SCHCTA.NE.ADV_SUP
     &.AND.SCHCTA.NE.ADV_LPO.AND.SCHCTA.NE.ADV_NSC.AND.SCHCTA.NE.ADV_PSI
     &.AND.SCHCTA.NE.ADV_LPO_TF.AND.SCHCTA.NE.ADV_NSC_TF) THEN
         IF(LNG.EQ.1) WRITE(LU,*)
     &  'LECDON: SCHEMA POUR LA CONVECTION DES TRACEURS INCONNU : ',
     &                                                            SCHCTA
        IF (LNG.EQ.2) WRITE(LU,*)
     &  'LECDON: SCHEME FOR ADVECTION OF TRACERS UNKNOWN: ',      SCHCTA
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(  SCHCKE.NE.0      .AND.SCHCKE.NE.ADV_CAR.AND.SCHCKE.NE.ADV_SUP
     &.AND.SCHCKE.NE.ADV_LPO.AND.SCHCKE.NE.ADV_NSC.AND.SCHCKE.NE.ADV_PSI
     &.AND.SCHCKE.NE.ADV_LPO_TF.AND.SCHCKE.NE.ADV_NSC_TF) THEN
         IF(LNG.EQ.1) WRITE(LU,*)
     &  'LECDON: SCHEMA DE CONVECTION DU K-EPSILON INCONNU : ',SCHCKE
        IF (LNG.EQ.2) WRITE(LU,*)
     &  'LECDON: SCHEME FOR ADVECTION OF K-EPSILON UNKNOWN: ', SCHCKE
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(OPTASS.NE.3) THEN
        IF(SCHCVI.NE.0.AND.(SCHCVI.EQ.ADV_LPO.OR.
     &                      SCHCVI.EQ.ADV_LPO_TF.OR.
     &                      SCHCVI.EQ.ADV_NSC_TF)) THEN
          IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LECDON :'
          WRITE(LU,*) 'SCHEMA POUR LA CONVECTION DES VITESSES : ',SCHCVI
          WRITE(LU,*) 'STOCKAGE DES MATRICES = 3 OBLIGATOIRE'
          ENDIF
          IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'LECDON:'
          WRITE(LU,*) 'SCHEME FOR ADVECTION OF VELOCITIES: ',SCHCVI
          WRITE(LU,*) 'MATRIX STORAGE = 3 MANDATORY'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(SCHCTA.NE.0.AND.(SCHCTA.EQ.ADV_LPO.OR.
     &                      SCHCTA.EQ.ADV_LPO_TF.OR.
     &                      SCHCTA.EQ.ADV_NSC_TF)) THEN
          IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LECDON :'
          WRITE(LU,*) 'SCHEMA POUR LA CONVECTION DES TRACEURS : ',SCHCTA
          WRITE(LU,*) 'STOCKAGE DES MATRICES = 3 OBLIGATOIRE'
          ENDIF
          IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'LECDON:'
          WRITE(LU,*) 'SCHEME FOR ADVECTION OF TRACERS: ',SCHCTA
          WRITE(LU,*) 'MATRIX STORAGE = 3 MANDATORY'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(SCHCKE.NE.0.AND.(SCHCKE.EQ.ADV_LPO.OR.
     &                      SCHCKE.EQ.ADV_LPO_TF.OR.
     &                      SCHCKE.EQ.ADV_NSC_TF)) THEN
          IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LECDON :'
          WRITE(LU,*) 'SCHEMA POUR LA CONVECTION DU K-EPSILON : ',SCHCKE
          WRITE(LU,*) 'STOCKAGE DES MATRICES = 3 OBLIGATOIRE'
          ENDIF
          IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'LECDON:'
          WRITE(LU,*) 'SCHEME FOR ADVECTION OF K-EPSILON: ',SCHCKE
          WRITE(LU,*) 'MATRIX STORAGE = 3 MANDATORY'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDIF
      CONV(SCHCVI) = .TRUE.
      CONV(SCHCTA) = .TRUE.
      CONV(SCHCKE) = .TRUE.
!
!-----------------------------------------------------------------------
C SETS VARIABLES WHEN THE DIFFUSION STEP IS NOT REQUIRED
C Q? WHAT HAPPENS WHEN SUPG ADVECTION IS REQUIRED, BUT NO DIFFUSION
!
      IF(.NOT.DIFFUS) THEN
        SCHDVI = 0
        SCHDTA = 0
        SCHDKE = 0
      ENDIF
      IF(NTRAC.EQ.0) SCHDTA = 0
      IF(ITURBV.NE.3.AND.ITURBV.NE.7) SCHDKE = 0
!
!-----------------------------------------------------------------------
C
C BUILDS ARRAY DIF INDICATING IF THERE ARE VARIABLES TO BE TREATED
C FOR EACH DIFFUSION SCHEME
!
      DIF(0) = .FALSE.
      DIF(1) = .FALSE.
      DIF(2) = .FALSE.
!
      DIF(SCHDVI) = .TRUE.
      DIF(SCHDTA) = .TRUE.
      DIF(SCHDKE) = .TRUE.
!
!-----------------------------------------------------------------------
C KRYLOV SPACE DIMENSION (VALID FOR GMRES, %SLV=7)
!
      SLVDVI%KRYLOV = MOTINT(ADRESS(1,74))
      SLVDTA%KRYLOV = MOTINT(ADRESS(1,75))
      SLVDKE%KRYLOV = MOTINT(ADRESS(1,76))
      SLVPRO%KRYLOV = MOTINT(ADRESS(1,77))
C     NO LONGER USED
      SLVW%KRYLOV   = 3
      SLVDSE%KRYLOV = MOTINT(ADRESS(1,84))
      SLVPOI%KRYLOV = MOTINT(ADRESS(1,81))
      SLVPRJ%KRYLOV = MOTINT(ADRESS(1,83))
!
!-----------------------------------------------------------------------
C  NAMES OF THE VARIABLES FOR THE RESULTS AND GEOMETRY FILES:
!-----------------------------------------------------------------------
!
C TABLE OF LOGICAL FOR VARIABLES OUTPUT
!
C     FOR 2D
!
      CALL NOMVAR_2D_IN_3D(TEXTE,TEXTPR,MNEMO,NTRAC,NAMETRAC)
      CALL SORTIE(SORT2D , MNEMO , MAXVAR , SORG2D )
C     OUTPUTS WHICH ARE NOT RELEVANT OR NOT PROGRAMMED
      SORG2D( 9) = .FALSE.
      SORG2D(10) = .FALSE.
      SORG2D(11) = .FALSE.
      SORG2D(12) = .FALSE.
      SORG2D(20) = .FALSE.
      SORG2D(21) = .FALSE.
      SORG2D(22) = .FALSE.
      IF(.NOT.SEDI) THEN
        SORG2D(23) = .FALSE.
        SORG2D(24) = .FALSE.
        SORG2D(25) = .FALSE.
        SORG2D(26) = .FALSE.
        SORG2D(32) = .FALSE.
        SORG2D(33) = .FALSE.
        SORG2D(34) = .FALSE.
      ENDIF
!
C     DIFFERENT HERE FROM TELEMAC-2D
C     CALL SORTIE(VARIMP , MNEMO , MAXVAR , SORIMP )
C     SORIMP NOT USED SO FAR
      DO I=1,MAXVAR
         SORIMP(I) = .FALSE.
      ENDDO
!
C     FOR 3D
!
      CALL NOMVAR_TELEMAC3D(TEXT3,TEXTP3,MNEM3,NTRAC,NAMETRAC)
      CALL SORTIE(SORT3D , MNEM3 , MAXVA3 , SORG3D )
!
!-----------------------------------------------------------------------
C NUMBER OF FLOAT OUTPUTS
!
      NITFLO = (NIT-1)/FLOPRD + 1
!
!-----------------------------------------------------------------------
!
C     NO K AND NO E IF NOT K-EPSILON OR K-OMEGA MODELS
!
      IF(ITURBV.NE.3.AND.ITURBV.NE.7) THEN
        SORG3D(8)=.FALSE.
        SORG3D(9)=.FALSE.
      ENDIF
!
!-----------------------------------------------------------------------
!
C     NO RICHARDSON IF NO MIXING LENGTH AND NO K-EPSILON
!
      IF(ITURBV.NE.2.AND.ITURBV.NE.3) THEN
        SORG3D(10)=.FALSE.
      ENDIF
!
!-----------------------------------------------------------------------
!
C     NO DYNAMIC NOR HYDROSTATIC PRESSURE WITHOUT NON-HYDROSTATIC OPTION
!
      IF(.NOT.NONHYD) THEN
        SORG3D(12)=.FALSE.
        SORG3D(13)=.FALSE.
      ENDIF
!
!-----------------------------------------------------------------------
!
C  CHECKS THE TRACERS USED IN THE DENSITY LAW
!
      IF(DENLAW.NE.0.AND.NTRAC.EQ.0) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LECDON: AVEC LOI DE DENSITE # 0'
          WRITE(LU,*) '        IL FAUT AU MOINS UN TRACEUR'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'LECDON: WITH DENSITY LAW # 0'
          WRITE(LU,*) '        AT LEAST 1 TRACER IS REQUIRED'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
C  CHECKS THE MAX TRACER NUMBER
!
      IF(NTRAC.GT.MAXTRA) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LECDON: DEPASSEMENT NOMBRE TRACEURS MAXI'
          WRITE(LU,*) '        MODIFIER BETAC DIM'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'LECDON: TRACERS NUMBER BIGGER THAN MAX NUMBER'
          WRITE(LU,*) '        MODIFIY BETAC DIM'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
C  LOOKS FOR TEMPERATURE AND SALINITY IN THE TRACERS
!
      IND_T=0
      IND_S=0
      IF(NTRAC.GE.1) THEN
        DO I=1,NTRAC
          IF(NAMETRAC(I)(1:11).EQ.'TEMPERATURE') IND_T = I
          IF(NAMETRAC(I)(1: 7).EQ.'SALINIT')     IND_S = I
        ENDDO
      ENDIF
!
C  CHECKS THE EXISTENCE OF RELEVANT TRACERS FOR DELWAQ
!
      IF(IND_T.EQ.0.AND.TEMP_DEL) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'POUR DELWAQ IL MANQUE LA TEMPERATURE'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'TEMPERATURE MISSING FOR DELWAQ'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
      IF(IND_S.EQ.0.AND.SALI_DEL) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'POUR DELWAQ IL MANQUE LA SALINITE'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'SALINITY MISSING FOR DELWAQ'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
C  CHECKS THE EXISTENCE OF RELEVANT TRACERS FOR THE DENSITY LAW
!
      IF( (DENLAW.EQ.1.AND. IND_T.EQ.0                ) .OR.
     &    (DENLAW.EQ.2.AND.               IND_S.EQ.0  ) .OR.
     &    (DENLAW.EQ.3.AND.(IND_T.EQ.0.OR.IND_S.EQ.0) )      ) THEN
!
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LECDON: AVEC LOI DE DENSITE = ',DENLAW
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'LECDON: WITH DENSITY LAW = ',DENLAW
        ENDIF
        IF(IND_T.EQ.0.AND.(DENLAW.EQ.1.OR.DENLAW.EQ.3)) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) '        UN TRACEUR DOIT ETRE LA TEMPERATURE'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) '        ONE TRACER MUST BE THE TEMPERATURE'
          ENDIF
        ENDIF
        IF(IND_S.EQ.0.AND.(DENLAW.EQ.2.OR.DENLAW.EQ.3)) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) '        UN TRACEUR DOIT ETRE LA SALINITE'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) '        ONE TRACER MUST BE THE SALINITY'
          ENDIF
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
C
C REACTS IF A PREVIOUS COMPUTATION FILE IS REQUIRED, BUT NOT PROVIDED
!
CBD_INCKA NOW USES T3DPRE
      IF (T3DPRE/=0) THEN
C      IF(.NOT.DEBU.AND.NOMPRE(1:1).EQ.' ') THEN
       IF(.NOT.DEBU.AND.T3D_FILES(T3DPRE)%NAME(1:1).EQ.' ') THEN
        IF(LISTIN) THEN
          IF(LNG.EQ.1) WRITE(LU,151)
          IF(LNG.EQ.2) WRITE(LU,152)
        ENDIF
151     FORMAT(1X,'LECDON : UNE SUITE DE CALCUL EST DEMANDEE',/,10X,
     &  'IL FAUT DONNER UN FICHIER DE RESULTATS DU CALCUL PRECEDENT',/)
152     FORMAT(1X,'LECDON : A CONTINUED COMPUTATION REQUIRED, ',/,
     &         1X,' A PREVIOUS RESULTS FILE IS NECESSARY',/)
        CALL PLANTE(1)
        STOP
       ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
C  CROUT TYPE PRECONDITIONING WITH GMRES: NOT ON VECTOR MACHINES
!
161     FORMAT(1X,'LECDON: MOT-CLES SOLVEURS ET PRECONDITIONNEMENTS :',
     &            /,1X,'SUR MACHINE VECTORIELLE,',/,1X,
     &            'NE PAS UTILISER GMRES + CROUT',///)
162     FORMAT(1X,'LECDON: KEY-WORDS SOLVERS AND PRECONDITONING:',/,1X,
     &            'ON VECTOR MACHINES,',/,1X,
     &            'DO NOT USE GMRES + CROUT',///)
      IF(LVMAC.NE.1) THEN
!
        IF(SLVDVI%SLV.EQ.7.AND.SLVDVI%PRECON.NE.0) THEN
          IF(MOD(SLVDVI%PRECON, 7).EQ.0.OR.
     &       MOD(SLVDVI%PRECON,11).EQ.0.OR.
     &       MOD(SLVDVI%PRECON,13).EQ.0    ) THEN
            IF(LNG.EQ.1) WRITE(LU,161)
            IF(LNG.EQ.2) WRITE(LU,162)
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
        IF(SLVPRO%SLV.EQ.7.AND.SLVPRO%PRECON.NE.0) THEN
          IF(MOD(SLVPRO%PRECON, 7).EQ.0.OR.
     &       MOD(SLVPRO%PRECON,11).EQ.0.OR.
     &       MOD(SLVPRO%PRECON,13).EQ.0    ) THEN
            IF(LNG.EQ.1) WRITE(LU,161)
            IF(LNG.EQ.2) WRITE(LU,162)
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
        IF(SLVDTA%SLV.EQ.7.AND.SLVDTA%PRECON.NE.0) THEN
          IF(MOD(SLVDTA%PRECON, 7).EQ.0.OR.
     &       MOD(SLVDTA%PRECON,11).EQ.0.OR.
     &       MOD(SLVDTA%PRECON,13).EQ.0    ) THEN
            IF(LNG.EQ.1) WRITE(LU,161)
            IF(LNG.EQ.2) WRITE(LU,162)
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
        IF(SLVDKE%SLV.EQ.7.AND.SLVDKE%PRECON.NE.0) THEN
          IF(MOD(SLVDKE%PRECON, 7).EQ.0.OR.
     &       MOD(SLVDKE%PRECON,11).EQ.0.OR.
     &       MOD(SLVDKE%PRECON,13).EQ.0    ) THEN
            IF(LNG.EQ.1) WRITE(LU,161)
            IF(LNG.EQ.2) WRITE(LU,162)
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
        IF(SLVW%SLV.EQ.7.AND.SLVW%PRECON.NE.0) THEN
          IF(MOD(SLVW%PRECON, 7).EQ.0.OR.
     &       MOD(SLVW%PRECON,11).EQ.0.OR.
     &       MOD(SLVW%PRECON,13).EQ.0    ) THEN
            IF(LNG.EQ.1) WRITE(LU,161)
            IF(LNG.EQ.2) WRITE(LU,162)
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
!
      ENDIF
!
C  CROUT-TYPE PRECONDITIONING AND PARALLELISM :
!
171   FORMAT(1X,'AVEC PARALLELISME',/,1X,
     &          'PRECONDITIONNEMENT DE TYPE CROUT DEGRADE',///)
172   FORMAT(1X,'WITH PARALLELISM = 2',/,1X,
     &          'CROUT-TYPE PRECONDITIONNING DOWNGRADED',///)
      IF(NCSIZE.GT.1) THEN
        IF(SLVDVI%PRECON.NE.0) THEN
          IF(MOD(SLVDVI%PRECON, 7).EQ.0.OR.
     &       MOD(SLVDVI%PRECON,11).EQ.0.OR.
     &       MOD(SLVDVI%PRECON,13).EQ.0    ) THEN
            IF(LNG.EQ.1) WRITE(LU,171)
            IF(LNG.EQ.2) WRITE(LU,172)
          ENDIF
        ENDIF
        IF(SLVPRO%PRECON.NE.0) THEN
          IF(MOD(SLVPRO%PRECON, 7).EQ.0.OR.
     &       MOD(SLVPRO%PRECON,11).EQ.0.OR.
     &       MOD(SLVPRO%PRECON,13).EQ.0    ) THEN
            IF(LNG.EQ.1) WRITE(LU,171)
            IF(LNG.EQ.2) WRITE(LU,172)
          ENDIF
        ENDIF
        IF(SLVDTA%PRECON.NE.0) THEN
          IF(MOD(SLVDTA%PRECON, 7).EQ.0.OR.
     &       MOD(SLVDTA%PRECON,11).EQ.0.OR.
     &       MOD(SLVDTA%PRECON,13).EQ.0    ) THEN
            IF(LNG.EQ.1) WRITE(LU,171)
            IF(LNG.EQ.2) WRITE(LU,172)
          ENDIF
        ENDIF
        IF(SLVDKE%PRECON.NE.0) THEN
          IF(MOD(SLVDKE%PRECON, 7).EQ.0.OR.
     &       MOD(SLVDKE%PRECON,11).EQ.0.OR.
     &       MOD(SLVDKE%PRECON,13).EQ.0    ) THEN
            IF(LNG.EQ.1) WRITE(LU,171)
            IF(LNG.EQ.2) WRITE(LU,172)
          ENDIF
        ENDIF
        IF(SLVW%PRECON.NE.0) THEN
          IF(MOD(SLVW%PRECON, 7).EQ.0.OR.
     &       MOD(SLVW%PRECON,11).EQ.0.OR.
     &       MOD(SLVW%PRECON,13).EQ.0    ) THEN
            IF(LNG.EQ.1) WRITE(LU,171)
            IF(LNG.EQ.2) WRITE(LU,172)
          ENDIF
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(.NOT.DIFFUS.AND.KFROT.NE.0) THEN
!
          IF(LNG.EQ.1) WRITE(LU,173)
          IF(LNG.EQ.2) WRITE(LU,174)
 173      FORMAT(///,1X,'ATTENTION !  SANS ETAPE DE DIFFUSION',/,1X,
     &                    'LE FROTTEMENT N''EST PAS PRIS EN COMPTE',///)
 174      FORMAT(///,1X,'BEWARE ! WITHOUT DIFFUSION STEP',/,1X,
     &                    'FRICTION IS NOT TAKEN INTO ACCOUNT',///)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(LISTIN) THEN
        IF(LNG.EQ.1) WRITE(LU,200) TITCAS
        IF(LNG.EQ.2) WRITE(LU,201) TITCAS
 200    FORMAT(/1X,'SORTIE DE LECDON. TITRE DE L''ETUDE :',/,1X,A72,/)
 201    FORMAT(/1X,'EXITING LECDON. NAME OF THE STUDY:',/,1X,A72,/)
      ENDIF
!
!-----------------------------------------------------------------------
!
C BUOYANCY IS NOW IMPLICIT IN DIFFUSION JUST MAKE SURE YOU HAVE IT NOW
!
      IF(NTRAC.NE.0.AND..NOT.DIFFUS) THEN
!
          IF(LNG.EQ.1) WRITE(LU,177)
          IF(LNG.EQ.2) WRITE(LU,178)
 177      FORMAT(///,1X,'ATTENTION ! DIFFUSION OBLIGATOIRE AVEC',/,1X,
     &          'TRACEUR ACTIFS',///)
 178      FORMAT(///,1X,'BEWARE ! DIFFUSION STEP MANDATORY',/,1X,
     &         'WITH A BUOYANCY EFFECT',///)
          DIFFUS=.TRUE.
!
      ENDIF
!
!-----------------------------------------------------------------------
!
C EDGE-BASED STORAGE MANDATORY FOR DELWAQ
!
      IF(INCLUS(COUPLING,'DELWAQ').AND.OPTASS.NE.3) THEN
        IF(LNG.EQ.1) WRITE(LU,2024)
        IF(LNG.EQ.2) WRITE(LU,2025)
2024    FORMAT(1X,'AVEC COUPLAGE DELWAQ, STOCKAGE PAR SEGMENT',/,1X,
     &            'OBLIGATOIRE',///)
2025    FORMAT(1X,'WITH COUPLING WITH DELWAQ, EDGE-BASED STORAGE',/,1X,
     &            'IS MANDATORY',///)
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C