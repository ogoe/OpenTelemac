C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MAIN SUBROUTINE FOR THE SUSPENDED-LOAD TRANSPORT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AC, ACLADM, AFBOR, AM1_S, AM2_S, AVAIL, BFBOR, BILMA, CBOR, CF, CHARR, CLT, CODE, CONC_VASE, CORR_CONV, CS, CSF_SABLE, CSF_VASE, CST, CSTAEQ, CTILD, DEBUG, DIFT, DISP, DISP_C, DM1, DTS, ELAY, ENTET, ENTETS, ES, FLBORTRA, FLBOR_SIS, FLBOR_TEL, FLUDP, FLUDPT, FLUER, FLUER_VASE, GRAV, HMIN, HN, HN_TEL, HPROP, ICQ, IELMT, IFAMAS, IMP_INFLOW_C, ISOUS, IT1, IT2, IT3, IT4, KARMAN, KDDL, KDIR, KENT, KINC, KLOG, KNEU, KSORT, KSP, KSR, KX, KY, KZ, LICBOR, LIMDIF, LT, MASDEP, MASDEPT, MASED0, MASFIN, MASINI, MASKEL, MASKPT, MASKTR, MASSOU, MASTCP, MASTEN, MASTOU, MBOR, MESH, MIXTE, MSK, MS_SABLE, MS_VASE, MU, NCOUCH_TASS, NFRLIQ, NIT, NPOIN, NPTFR, NSICLA, NSOUS, NUMLIQ, OPDTRA, OPTBAN, OPTDIF, OPTSUP, PARTHENIADES, PASS, QSCLXS, QSCLYS, QSCL_S, QSXS, QSYS, QS_C, QS_S, RESOL, S, SEDCO, SLVTRA, SOLSYS, T1, T10, T11, T12, T2, T3, T4, T5, T6, T7, T8, T9, TASS, TB, TE1, TE2, TE3, TETA_SUSP, TOB, TOCE_MIXTE, TOCE_VASE, U2D, UCONV, UCONV_TEL, UNSV2D, V2D, V2DPAR, VCONV, VCONV_TEL, VISC_TEL, VITCD, VITCE, VOLU2D, W1, XKX, XKY, XMVE, XMVS, XWC, ZCONV, ZERO, ZF, ZFCL_S, ZF_S, ZREF
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, P_DSUM
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SUSPENSION_MAIN
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> DOTS(), OS(), SUSPENSION_COMPUTATION(), SUSPENSION_DISPERSION(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SISYPHE()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 25/06/2008
!> </td><td> JMH
!> </td><td> CALL TO DIFFIN MOVED IN SUSPENSION_COMPUTATION
!> <br>      ARGUMENTS FOR SUSPENSION_COMPUTATION: 2 ARGUMENTS
!>           RELATED TO MUD HAVE BEEN REMOVED, LICBOR ADDED AT THE END
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 22/12/2004
!> </td><td> F. HUVELIN
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ACLADM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AFBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AM1_S
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AM2_S
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AVAIL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BFBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BILMA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CHARR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CLT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CODE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CONC_VASE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CORR_CONV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CSF_SABLE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CSF_VASE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CST
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CSTAEQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CTILD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEBUG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DIFT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DISP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DISP_C
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DM1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DTS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ELAY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ENTET
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ENTETS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLBORTRA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLBOR_SIS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLBOR_TEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUDP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUDPT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUER
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUER_VASE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HMIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HN_TEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HPROP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ICQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELMT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IFAMAS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IMP_INFLOW_C
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ISOUS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IT1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IT2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IT3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IT4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KDDL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KDIR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KENT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KINC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KLOG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KNEU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KSORT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KSP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KSR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KZ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LICBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIMDIF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASDEP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASDEPT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASED0
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASFIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASINI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKPT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKTR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASSOU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASTCP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASTEN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASTOU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MIXTE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MS_SABLE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MS_VASE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NCOUCH_TASS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NFRLIQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NIT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NSICLA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NSOUS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NUMLIQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPDTRA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPTBAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPTDIF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPTSUP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PARTHENIADES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PASS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSCLXS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSCLYS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSCL_S
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSXS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSYS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QS_C
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QS_S
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>RESOL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>S
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SEDCO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SLVTRA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SOLSYS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T10
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T11
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T12
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T5
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T6
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T7
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T8
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T9
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TASS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TE1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TE2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TE3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TETA_SUSP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOCE_MIXTE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOCE_VASE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UCONV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UCONV_TEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UNSV2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V2DPAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VCONV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VCONV_TEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VISC_TEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VITCD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VITCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VOLU2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XKX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XKY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMVE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMVS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XWC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZCONV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZERO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZFCL_S
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZF_S
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZREF
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE SUSPENSION_MAIN
     &(SLVTRA,HN,HN_TEL,MU,TOB,ACLADM,KSP,KSR,
     & CF,VOLU2D,V2DPAR,UNSV2D,AFBOR,
     & BFBOR,ZF,LICBOR,IFAMAS,MASKEL,MASKPT,U2D,V2D,NSICLA,NPOIN,
     & NPTFR,IELMT,OPTDIF,RESOL,LT,NIT,OPTBAN,OPTSUP,OPDTRA,
     & KENT,KSORT,KLOG,KINC,KNEU,KDIR,KDDL,ISOUS,NSOUS,DEBUG,
     & DTS,CSF_VASE,CSF_SABLE,ZERO,GRAV,XKX,XKY,KARMAN,
     & XMVE,XMVS,HMIN,XWC,VITCD,VITCE,PARTHENIADES,ENTET,BILMA,MSK,
     & CHARR,IMP_INFLOW_C,MESH,ZF_S,CS,CST,CTILD,CBOR,DISP,
     & IT1,IT2,IT3,IT4,TB,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,W1,
     & TE1,CLT,TE2,TE3,S,AM1_S,AM2_S,MBOR,ELAY,LIMDIF,
     & MASKTR, TETA_SUSP, AC, MASED0, MASINI, MASTEN,
     & MASTOU, ES,AVAIL,  ENTETS, PASS, ZFCL_S,
     & HPROP, FLUDPT, FLUDP, FLUER, DISP_C, KX, KY,
     & KZ, UCONV, VCONV,QSXS, QSYS, QSCLXS, QSCLYS, QSCL_S,
     & QS_S,QS_C,CSTAEQ,ICQ,MASTCP,MASFIN,MASDEPT,MASDEP,MASSOU,
     & CORR_CONV,ZREF,SEDCO,VISC_TEL,CODE,
     & DIFT,DM1,UCONV_TEL,VCONV_TEL,ZCONV,SOLSYS,FLBOR_TEL,FLBOR_SIS,
     & FLBORTRA,NUMLIQ,NFRLIQ,MIXTE,NCOUCH_TASS,CONC_VASE,
     & TOCE_VASE,FLUER_VASE,TOCE_MIXTE,MS_SABLE,MS_VASE,TASS)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AC             |---| 
C| ACLADM         |---| 
C| AFBOR          |---| 
C| AM1_S          |---| 
C| AM2_S          |---| 
C| AVAIL          |---| 
C| BFBOR          |---| 
C| BILMA          |---| 
C| CBOR           |---| 
C| CF             |---| 
C| CHARR          |---| 
C| CLT            |---| 
C| CODE           |---| 
C| CONC_VASE      |---| 
C| CORR_CONV      |---| 
C| CS             |---| 
C| CSF_SABLE      |---| 
C| CSF_VASE       |---| 
C| CST            |---| 
C| CSTAEQ         |---| 
C| CTILD          |---| 
C| DEBUG          |---| 
C| DIFT           |---| 
C| DISP           |---| 
C| DISP_C         |---| 
C| DM1            |---| 
C| DTS            |---| 
C| ELAY           |---| 
C| ENTET          |---| 
C| ENTETS         |---| 
C| ES             |---| 
C| FLBORTRA       |---| 
C| FLBOR_SIS      |---| 
C| FLBOR_TEL      |---| 
C| FLUDP          |---| 
C| FLUDPT         |---| 
C| FLUER          |---| 
C| FLUER_VASE     |---| 
C| GRAV           |---| 
C| HMIN           |---| 
C| HN             |---| 
C| HN_TEL         |---| 
C| HPROP          |---| 
C| ICQ            |---| 
C| IELMT          |---| 
C| IFAMAS         |---| 
C| IMP_INFLOW_C   |---| 
C| ISOUS          |---| 
C| IT1            |---| 
C| IT2            |---| 
C| IT3            |---| 
C| IT4            |---| 
C| KARMAN         |---| 
C| KDDL           |---| 
C| KDIR           |---| 
C| KENT           |---| 
C| KINC           |---| 
C| KLOG           |---| 
C| KNEU           |---| 
C| KSORT          |---| 
C| KSP            |---| 
C| KSR            |---| 
C| KX             |---| 
C| KY             |---| 
C| KZ             |---| 
C| LICBOR         |---| 
C| LIMDIF         |---| 
C| LT             |---| 
C| MASDEP         |---| 
C| MASDEPT        |---| 
C| MASED0         |---| 
C| MASFIN         |---| 
C| MASINI         |---| 
C| MASKEL         |---| 
C| MASKPT         |---| 
C| MASKTR         |---| 
C| MASSOU         |---| 
C| MASTCP         |---| 
C| MASTEN         |---| 
C| MASTOU         |---| 
C| MBOR           |---| 
C| MESH           |---| 
C| MIXTE          |---| 
C| MSK            |---| 
C| MS_SABLE       |---| 
C| MS_VASE        |---| 
C| MU             |---| 
C| NCOUCH_TASS    |---| 
C| NFRLIQ         |---| 
C| NIT            |---| 
C| NPOIN          |---| 
C| NPTFR          |---| 
C| NSICLA         |---| 
C| NSOUS          |---| 
C| NUMLIQ         |---| 
C| OPDTRA         |---| 
C| OPTBAN         |---| 
C| OPTDIF         |---| 
C| OPTSUP         |---| 
C| PARTHENIADES   |---| 
C| PASS           |---| 
C| QSCLXS         |---| 
C| QSCLYS         |---| 
C| QSCL_S         |---| 
C| QSXS           |---| 
C| QSYS           |---| 
C| QS_C           |---| 
C| QS_S           |---| 
C| RESOL          |---| 
C| S             |---| 
C| SEDCO          |---| 
C| SLVTRA         |---| 
C| SOLSYS         |---| 
C| T1             |---| 
C| T10            |---| 
C| T11            |---| 
C| T12            |---| 
C| T2             |---| 
C| T3             |---| 
C| T4             |---| 
C| T5             |---| 
C| T6             |---| 
C| T7             |---| 
C| T8             |---| 
C| T9             |---| 
C| TASS           |---| 
C| TB             |---| 
C| TE1            |---| 
C| TE2            |---| 
C| TE3            |---| 
C| TETA_SUSP      |---| 
C| TOB            |---| 
C| TOCE_MIXTE     |---| 
C| TOCE_VASE      |---| 
C| U2D            |---| 
C| UCONV          |---| 
C| UCONV_TEL      |---| 
C| UNSV2D         |---| 
C| V2D            |---| 
C| V2DPAR         |---| 
C| VCONV          |---| 
C| VCONV_TEL      |---| 
C| VISC_TEL       |---| 
C| VITCD          |---| 
C| VITCE          |---| 
C| VOLU2D         |---| 
C| W1             |---| 
C| XKX            |---| 
C| XKY            |---| 
C| XMVE           |---| 
C| XMVS           |---| 
C| XWC            |---| 
C| ZCONV          |---| 
C| ZERO           |---| 
C| ZF             |---| 
C| ZFCL_S         |---| 
C| ZF_S           |---| 
C| ZREF           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE,EX_SUSPENSION_MAIN => SUSPENSION_MAIN
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE (SLVCFG),    INTENT(INOUT) :: SLVTRA
      TYPE (BIEF_OBJ),  INTENT(IN)    :: HN,HN_TEL,MU,TOB,ACLADM
      TYPE (BIEF_OBJ),  INTENT(IN)    :: KSP,KSR
      TYPE (BIEF_OBJ),  INTENT(IN)    :: CF,VOLU2D,AFBOR,BFBOR,ZF
      TYPE (BIEF_OBJ),  INTENT(IN)    :: V2DPAR,UNSV2D
      TYPE (BIEF_OBJ),  INTENT(IN)    :: LICBOR, IFAMAS, MASKEL, MASKPT
      TYPE (BIEF_OBJ),  INTENT(IN)    :: U2D, V2D,DM1,ZCONV,FLBOR_TEL
      INTEGER,          INTENT(IN)    :: NSICLA, NPOIN, NPTFR, IELMT
      INTEGER,          INTENT(IN)    :: OPTDIF, RESOL,LT, NIT
      INTEGER,          INTENT(IN)    :: OPTBAN,OPTSUP,OPDTRA,NFRLIQ
      INTEGER,          INTENT(IN)    :: KENT, KSORT, KLOG, KINC, KNEU
      INTEGER,          INTENT(IN)    :: KDIR,KDDL,ISOUS,NSOUS
      INTEGER,          INTENT(IN)    :: DEBUG,SOLSYS,NCOUCH_TASS
      INTEGER,          INTENT(IN)    :: NUMLIQ(NFRLIQ)
      DOUBLE PRECISION, INTENT(IN)    :: DTS,CSF_VASE,CSF_SABLE
      DOUBLE PRECISION, INTENT(IN)    :: ZERO,GRAV
      DOUBLE PRECISION, INTENT(IN)    :: XKX,XKY,KARMAN
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, XMVS, HMIN, XWC(NSICLA)
      DOUBLE PRECISION, INTENT(IN)    :: VITCD, VITCE
      DOUBLE PRECISION, INTENT(IN)    :: PARTHENIADES
      LOGICAL,          INTENT(IN)    :: ENTET, BILMA, MSK, CHARR
      LOGICAL,          INTENT(IN)    :: IMP_INFLOW_C
      LOGICAL,          INTENT(IN)    :: SEDCO(NSICLA),MIXTE,TASS
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: ZF_S,CS,CST,CTILD,CBOR
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: DISP,IT1,IT2,IT3,IT4,TB
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: T1,T2,T3,T4,T5,T6,T7,T8
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: T9,T10,T11,T12,W1,TE1,CLT
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: TE2,TE3,S,AM1_S,AM2_S,MBOR
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: ELAY, LIMDIF,FLBORTRA
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: MASKTR
      DOUBLE PRECISION, INTENT(INOUT) :: TETA_SUSP, AC(NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: MASED0(NSICLA), MASINI(NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: MASTEN(NSICLA), MASTOU(NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,10)
      DOUBLE PRECISION, INTENT(INOUT) :: TOCE_VASE(10),CONC_VASE(10)
      DOUBLE PRECISION, INTENT(INOUT) :: AVAIL(NPOIN,10,NSICLA)
      LOGICAL,          INTENT(INOUT) :: ENTETS, PASS
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: ZFCL_S,HPROP,ZREF
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUDPT,FLUDP,FLUER
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: DISP_C,KX,KY,KZ,UCONV
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: VCONV,FLBOR_SIS
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: QSXS,QSYS,QSCLXS,QSCLYS
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: QSCL_S,QS_S,CSTAEQ
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUER_VASE,TOCE_MIXTE
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: MS_SABLE,MS_VASE
      TYPE (BIEF_OBJ),  INTENT(IN)    :: QS_C,VISC_TEL
      TYPE (BIEF_OBJ),  INTENT(IN)    :: UCONV_TEL,VCONV_TEL
      DOUBLE PRECISION, INTENT(OUT)   :: MASTCP(NSICLA),MASFIN(NSICLA)
      DOUBLE PRECISION, INTENT(OUT)   :: MASDEPT(NSICLA),MASDEP(NSICLA)
      DOUBLE PRECISION, INTENT(OUT)   :: MASSOU
      INTEGER, INTENT(IN)             :: ICQ
      LOGICAL, INTENT (IN)            :: CORR_CONV,DIFT
      CHARACTER(LEN=24), INTENT(IN)   :: CODE
!

      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER :: I
!
       DOUBLE PRECISION, EXTERNAL :: P_DSUM
!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!
!

      IF(PASS) THEN
      ! *************************  !
      ! III - INITIAL MASS-BALANCE !
      ! *************************  !
!
      IF(BILMA) THEN
         DO I = 1,NSICLA
!
            CALL VECTOR(T1, '=', 'MASVEC          ', IELMT, 1.D0,
     &                  CS%ADR(I)%P, T1, T1, T1, T1, T1, MESH, MSK,
     &                  MASKEL)
!
            MASED0(I) = DOTS(T1,HN)
            IF(NCSIZE.GT.1) MASED0(I)=P_DSUM(MASED0(I))
            MASINI(I) = MASED0(I)
            MASTEN(I) = 0.D0
            MASTOU(I) = 0.D0
            MASTCP(I) = 0.D0
            IF(LNG.EQ.1) WRITE(LU,1) I, MASED0(I)
            IF(LNG.EQ.2) WRITE(LU,2) I, MASED0(I)
         ENDDO
      ENDIF
!
      !----------------------------------------------------------------!
001   FORMAT(1X,'QUANTITE INITIALE EN SUSPENSION POUR LA CLASSE ',
     &       I2,' : ', G16.7, ' M3')
      !----------------------------------------------------------------!
002   FORMAT(1X,'INITIAL QUANTITY IN SUSPENSION FOR CLASS ',
     &       I2,' : ', G16.7, ' M3')
      !----------------------------------------------------------------!
C     END OF IF(PASS)
      ENDIF
      PASS = .FALSE.

      ! ********************************* !
      ! V - COMPUTES THE DISPERSION       !
      ! ********************************* !
      IF (DEBUG > 0) WRITE(LU,*) 'SUSPENSION_DISPERSION'
      CALL SUSPENSION_DISPERSION
     &     (TOB,XMVE,HN,OPTDIF,NPOIN,XKX,XKY,T1,T2,T3,KX,KY,KZ,DISP,
     &      U2D,V2D,VISC_TEL,CODE)
      IF (DEBUG > 0) WRITE(LU,*) 'END_SUSPENSION_DISPERSION'

      ! ************************************************ !
      ! VI  - COMPUTES THE CONCENTRATION AND EVOLUTION   !
      ! ************************************************ !
       CALL OS('X=Y     ', X=HPROP, Y=HN)
C      CALL OS('X=+(Y,C)', X=HCLIP, Y=HN, C=HMIN)
       DO I = 1, NSICLA
         CALL OS('X=0     ', X=ZFCL_S%ADR(I)%P)

         IF(DEBUG > 0) WRITE(LU,*)
     &                'SUSPENSION_COMPUTATION : ',I,'/',NSICLA
         CALL SUSPENSION_COMPUTATION(SLVTRA,HN,HN_TEL,UCONV,
     & VCONV,CF,MU,TOB,ACLADM,KSP,KSR,ELAY,AVAIL(1:NPOIN,1,I),
     & AFBOR,BFBOR,LIMDIF,
     & CLT,MASKEL,MASKTR,MASKPT,IFAMAS,NPOIN,IELMT,NPTFR,I,LT,NIT,
     & RESOL,OPTBAN,KENT,KDDL,KDIR,KSORT,KLOG,KINC,KNEU,OPTSUP,
     & OPDTRA,DEBUG,CSF_VASE, CSF_SABLE, TETA_SUSP,DTS,MASED0(I),ZERO,
     & XWC(I),KARMAN,XMVE,XMVS,GRAV,HMIN,VITCD,VITCE,
     & PARTHENIADES,ENTETS,ENTET,BILMA,
     & MSK,CHARR,IMP_INFLOW_C,MESH,ZF,CS%ADR(I)%P,
     & CST%ADR(I)%P,CTILD%ADR(I)%P,CBOR%ADR(I)%P,DISP,IT1,IT2,
     & IT3,IT4,TB,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,
     & W1,TE1,TE2,TE3,S,AM1_S,AM2_S,MBOR,MASTEN(I),MASTOU(I),
     & MASINI(I),AC(I),ZFCL_S%ADR(I)%P,FLUDPT%ADR(I)%P,
     & FLUDP%ADR(I)%P,FLUER%ADR(I)%P,HPROP,DISP_C,CSTAEQ,
     & MASFIN(I),MASDEPT(I),MASDEP(I),MASSOU,QS_C,ICQ,ZREF,
     & CORR_CONV,U2D,V2D,SEDCO(I),DIFT,DM1,ZCONV,UCONV_TEL,
     & VCONV_TEL,SOLSYS,FLBOR_TEL,FLBOR_SIS,FLBORTRA,CODE,VOLU2D,
     & V2DPAR,UNSV2D,NUMLIQ,NFRLIQ,LICBOR,MIXTE,AVAIL,NSICLA,ES,
     & NCOUCH_TASS,CONC_VASE,TOCE_VASE,
     & FLUER_VASE,TOCE_MIXTE,MS_SABLE,MS_VASE,TASS)
         IF (DEBUG > 0) WRITE(LU,*) 'END_SUSPENSION_COMPUTATION'
!
C        CV MODIFICATIONS : 03/2006
C        TAKES INTO ACCOUNT ADVECTION VELOCITY
         CALL OS('X=YZ    ', X=T1, Y=UCONV, Z=HN)
         CALL OS('X=YZ    ', X=T2, Y=VCONV, Z=HN)
C        JMH MODIFICATION : 25/06/2008 WHAT WAS THIS ?
C        CALL OS('X=ABS(Y)', X=T1, Y=T1)
C        CALL OS('X=ABS(Y)', X=T2, Y=T2)
         CALL OS('X=Y     ', X=CS%ADR(I)%P, Y=CST%ADR(I)%P)
         CALL OS('X=YZ    ', X=QSCLXS%ADR(I)%P, Y=CS%ADR(I)%P, Z=T1)
         CALL OS('X=YZ    ', X=QSCLYS%ADR(I)%P, Y=CS%ADR(I)%P, Z=T2)
!
      ENDDO
!
      ! *********************************************************** !
      ! VII  - UPDATES EVOLUTION, CONCENTRATION AND TRANSPORT RATE  !
      ! *********************************************************** !
!
      IF (DEBUG > 0) WRITE(LU,*) 'UPDATING_DATA'
!
      CALL OS('X=0     ', X=QSXS)
      CALL OS('X=0     ', X=QSYS)
      CALL OS('X=0     ', X=ZF_S)
!
      DO I = 1, NSICLA
        CALL OS('X=X+Y   ', X=ZF_S, Y=ZFCL_S%ADR(I)%P)
        CALL OS('X=X+Y   ', X=QSXS, Y=QSCLXS%ADR(I)%P)
        CALL OS('X=X+Y   ', X=QSYS, Y=QSCLYS%ADR(I)%P)
      ENDDO
      CALL OS('X=N(Y,Z)', X=QSCL_S, Y=QSCLXS, Z=QSCLYS)
      CALL OS('X=N(Y,Z)', X=QS_S, Y=QSXS, Z=QSYS)
      IF (DEBUG > 0) WRITE(LU,*) 'END_UPDATING_DATA'

!======================================================================!
!======================================================================!

      RETURN
      END
C
C#######################################################################
C