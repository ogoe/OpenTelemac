C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MAIN SUBROUTINE FOR THE COMPUTATION OF THE
!>                CONCENTRATION AND THE ELEVATION SOLVING THE EQUATION :
!>  @code
!>      D(ZF)
!>      ----  + DIV(QS) + (E-D)ZA = 0
!>      DT
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  IF COUPLING, DIV(QS) ALREADY COMPUTED
!>         ELSE,        DIV(QS) = 0

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AC, ACLADM, AFBOR, AM1_S, AM2_S, AVA, AVAIL, BFBOR, BILMA, CBOR, CF, CHARR, CLT, CODE, CONC_VASE, CORR_CONV, CS, CSF_SABLE, CSF_VASE, CST, CSTAEQ, CTILD, DEBUG, DIFT, DISP, DISP_C, DM1, DT, ELAY, ENTET, ENTETS, ES, FLBORTRA, FLBOR_SIS, FLBOR_TEL, FLUDP, FLUDPT, FLUER, FLUER_VASE, GRAV, HMIN, HN, HN_TEL, HPROP, ICQ, IELMT, IFAMAS, IMP_INFLOW_C, IT1, IT2, IT3, IT4, ITRA, KARMAN, KDDL, KDIR, KENT, KINC, KLOG, KNEU, KSORT, KSP, KSR, LICBOR, LIMDIF, LT, MASDEP, MASDEPT, MASED0, MASFIN, MASINI, MASKEL, MASKPT, MASKTR, MASSOU, MASTEN, MASTOU, MBOR, MESH, MIXTE, MSK, MS_SABLE, MS_VASE, MU, NCOUCH_TASS, NFRLIQ, NIT, NPOIN, NPTFR, NSICLA, NUMLIQ, OPDTRA, OPTBAN, OPTSUP, PARTHENIADES, QS_C, RESOL, S, SEDCO, SLVTRA, SOLSYS, T1, T10, T11, T12, T2, T3, T4, T5, T6, T7, T8, T9, TASS, TB, TE1, TE2, TE3, TETA_SUSP, TOB, TOCE_MIXTE, TOCE_VASE, U2D, UCONV, UCONV_TEL, UNSV2D, V2D, V2DPAR, VCONV, VCONV_TEL, VITCD, VITCE, VOLU2D, W1, XMVE, XMVS, XWC, ZCONV, ZERO, ZF, ZFCL_S, ZREF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AGGLOT, BID, CSF, HOLD, I, K, OPTVF, RESOL_MOD, SAVE_UCONV, SAVE_VCONV, SOLSYS_SIS, TETAH, YASMI2
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SUSPENSION_COMPUTATION, HOLD, SAVE_UCONV, SAVE_VCONV
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CHARAC(), CPSTVC(), CVDFTR(), DIFFIN(), IELBOR(), OS(), OSBD(), SUSPENSION_BILAN(), SUSPENSION_CONV(), SUSPENSION_DEPOT(), SUSPENSION_EROSION(), SUSPENSION_EROSION_COH(), SUSPENSION_EVOL(), SUSPENSION_FLUX_MIXTE(), SUSPENSION_LISTING(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SUSPENSION_MAIN()

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
!> </td><td> 05/04/2010
!> </td><td>
!> </td><td> CSTAEQ TAKES INTO ACCOUNT THE % OF LAYER QQ OR THE SELECTED FORMULATION
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 16/09/2009
!> </td><td>
!> </td><td> AVAIL(NPOIN,10,NSICLA)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 31/07/2008
!> </td><td>
!> </td><td> CALLS SUSPENSION_FLUX SPLIT IN 2 : DEPOSITION + EROSION
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 25/06/2008
!> </td><td>
!> </td><td> CALLS DIFFIN (USED TO BE IN SUSPENSION_MAIN)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 12/06/2008
!> </td><td>
!> </td><td> SECTIONS "TREATING SMALL DEPTHS" AND
!>          "LIMITATION OF FLUER..." SWAPPED ROUND
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 09/06/2008
!> </td><td>
!> </td><td> NEW SUSPENSION_BILAN WITH FLBORTRA GIVEN BY CVDFTR
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 28/05/2008
!> </td><td>
!> </td><td> NEW SUSPENSION_BILAN WITH FLUXES THROUGH BOUNDARIES
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 09/05/2008
!> </td><td>
!> </td><td> FLUDP REMOVED FROM SUSPENSION_FLUX, SUSPENSION_NERBED DELETED
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 05/05/2008
!> </td><td>
!> </td><td> ADAPTED FOR FINITE VOLUME ADVECTION
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 10/11/2010
!> </td><td> JMH:
!> </td><td> ENTET CHANGED INTO ENTETS IN THE CALL TO CVDFTR
!> </td><td> LISTING ONLY FOR THE LAST SUB-ITERATION. ENTET NO LONGER USED
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
!>          <tr><td>AVA
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
!>          <tr><td>DT
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
!>          <tr><td>ITRA
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
!>          <tr><td>NUMLIQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPDTRA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPTBAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>OPTSUP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PARTHENIADES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QS_C
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
!>          <tr><td>ZREF
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE SUSPENSION_COMPUTATION
     &  (SLVTRA, HN,HN_TEL,UCONV, VCONV,  CF, MU,TOB,ACLADM, KSP,KSR,KS,
     &   ELAY, AVA, AFBOR, BFBOR, LIMDIF, CLT, MASKEL, MASKTR,
     &   MASKPT, IFAMAS, NPOIN, IELMT, NPTFR, ITRA, LT, NIT, RESOL,
     &   OPTBAN, KENT,KDDL,KDIR,KSORT,KLOG,KINC,KNEU,
     &   OPTSUP, OPDTRA, DEBUG, CSF_VASE,CSF_SABLE,
     &   TETA_SUSP, DT, MASED0, ZERO, XWC, KARMAN, XMVE, XMVS, GRAV,
     &   HMIN, VITCD, VITCE,PARTHENIADES, ENTETS,
     &   ENTET,BILMA,MSK,CHARR,IMP_INFLOW_C,MESH,ZF,CS,
     &   CST,CTILD,CBOR,DISP,IT1,IT2,IT3,IT4,TB,T1,T2,T3,
     &   T4, T5, T6, T7, T8, T9, T10, T11, T12, W1, TE1, TE2, TE3, S,
     &   AM1_S, AM2_S, MBOR,MASTEN, MASTOU, MASINI, AC,
     &   ZFCL_S, FLUDPT, FLUDP, FLUER, HPROP, DISP_C, CSTAEQ,
     &   MASFIN, MASDEPT, MASDEP, MASSOU,QS_C,ICQ,ZREF,
     &   CORR_CONV,U2D,V2D,SEDCO,DIFT,
     &   DM1,ZCONV,UCONV_TEL,VCONV_TEL,SOLSYS,FLBOR_TEL,FLBOR_SIS,
     &   FLBORTRA,CODE,
     &   VOLU2D,V2DPAR,UNSV2D,NUMLIQ,NFRLIQ,LICBOR,MIXTE,AVAIL,NSICLA,
     &   ES,NCOUCH_TASS,CONC_VASE,TOCE_VASE,
     &   FLUER_VASE,TOCE_MIXTE,MS_SABLE,MS_VASE,TASS)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AC             |---| 
C| ACLADM         |---| 
C| AFBOR          |---| 
C| AM1_S          |---| 
C| AM2_S          |---| 
C| AVA            |---| 
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
C| DT             |---| 
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
C| IT1            |---| 
C| IT2            |---| 
C| IT3            |---| 
C| IT4            |---| 
C| ITRA           |---| 
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
C  KS             |---| 
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
C| NUMLIQ         |---| 
C| OPDTRA         |---| 
C| OPTBAN         |---| 
C| OPTSUP         |---| 
C| PARTHENIADES   |---| 
C| QS_C           |---| 
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
C| VITCD          |---| 
C| VITCE          |---| 
C| VOLU2D         |---| 
C| W1             |---| 
C| XMVE           |---| 
C| XMVS           |---| 
C| XWC            |---| 
C| ZCONV          |---| 
C| ZERO           |---| 
C| ZF             |---| 
C| ZFCL_S         |---| 
C| ZREF           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE,
     &    EX_SUSPENSION_COMPUTATION => SUSPENSION_COMPUTATION
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE (SLVCFG),    INTENT(INOUT) :: SLVTRA
      TYPE (BIEF_OBJ),  INTENT(IN)    :: ZF,VOLU2D,V2DPAR,UNSV2D
      TYPE (BIEF_OBJ),  INTENT(IN), TARGET    :: HN,HN_TEL
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: UCONV,VCONV
      TYPE (BIEF_OBJ),  INTENT(IN)    :: MU,KSP,KSR,KS
      TYPE (BIEF_OBJ),  INTENT(IN)    :: CF,TOB,ACLADM,ELAY,LICBOR
      TYPE (BIEF_OBJ),  INTENT(IN)    :: AFBOR,BFBOR
      TYPE (BIEF_OBJ),  INTENT(IN)    :: MASKEL,MASKPT,IFAMAS
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: MASKTR,LIMDIF,CLT
      INTEGER,          INTENT(IN)    :: NPOIN,IELMT,NPTFR,ITRA,LT
      INTEGER,          INTENT(IN)    :: NIT,RESOL,OPTBAN,KENT,KDDL
      INTEGER,          INTENT(IN)    :: KDIR,OPTSUP,OPDTRA,SOLSYS
      INTEGER,          INTENT(IN)    :: KSORT,KLOG,KINC,KNEU
      INTEGER,          INTENT(IN)    :: NFRLIQ,NSICLA,NCOUCH_TASS
      INTEGER,          INTENT(IN)    :: DEBUG
      INTEGER,          INTENT(IN)    :: NUMLIQ(NFRLIQ)
      DOUBLE PRECISION, INTENT(IN)    :: CSF_VASE, TETA_SUSP, DT, MASED0
      DOUBLE PRECISION, INTENT(IN)    :: ZERO, XWC, CSF_SABLE,AVA(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN, XMVE, XMVS, GRAV, HMIN
      DOUBLE PRECISION, INTENT(IN)    :: VITCD,VITCE,PARTHENIADES
      LOGICAL,          INTENT(IN)    :: ENTETS,ENTET,BILMA,MSK,SEDCO
      LOGICAL,          INTENT(IN)    :: CHARR, IMP_INFLOW_C,CORR_CONV
      LOGICAL,          INTENT(IN)    :: DIFT,MIXTE, TASS
      TYPE (BIEF_MESH), INTENT(INOUT) :: MESH
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: CS,CST,CTILD,CBOR,FLBOR_SIS
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: DISP,IT1,IT2,IT3,IT4,TB
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: T2, T3, T4, T5, T6, T7, T8
      TYPE (BIEF_OBJ),  INTENT(INOUT), TARGET :: T1
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: T9, T10, T11, T12, W1, TE1
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: TE2, TE3, S, AM1_S, AM2_S
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: MBOR,ZREF
      DOUBLE PRECISION, INTENT(INOUT) :: MASTEN, MASTOU, MASINI, AC
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: ZFCL_S
      TYPE (BIEF_OBJ),  INTENT(IN)    :: UCONV_TEL,VCONV_TEL
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUDPT,FLUDP,FLUER,FLBORTRA
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: HPROP, DISP_C, CSTAEQ
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUER_VASE,TOCE_MIXTE
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: MS_SABLE,MS_VASE
      DOUBLE PRECISION, INTENT(INOUT) :: MASFIN, MASDEPT, MASDEP
      DOUBLE PRECISION, INTENT(INOUT) :: MASSOU,AVAIL(NPOIN,10,NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,10)
      DOUBLE PRECISION, INTENT(INOUT) :: CONC_VASE(10),TOCE_VASE(10)
      TYPE (BIEF_OBJ),  INTENT(IN)    :: QS_C,U2D,V2D,DM1,ZCONV
      TYPE (BIEF_OBJ),  INTENT(IN)    :: FLBOR_TEL
      INTEGER,          INTENT(IN)    :: ICQ
      CHARACTER(LEN=24), INTENT(IN)   :: CODE
C
!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
C 3/ LOCAL VARIABLES
! ------------------
!
      INTEGER          :: I,K,SOLSYS_SIS,OPTVF,BID(1),RESOL_MOD
      DOUBLE PRECISION :: TETAH,AGGLOT
      DOUBLE PRECISION :: CSF
      LOGICAL YASMI2
      TYPE (BIEF_OBJ),  POINTER :: HOLD
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVE_UCONV,SAVE_VCONV
      SAVE_UCONV=>UCONV%R
      SAVE_VCONV=>VCONV%R
!
!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
C     JMH 18/04/2008 :
C
C     TAKES DETAILS OF THE CONTINUITY EQUATION INTO ACCOUNT
C     IN TELEMAC-2D OR 3D, WITH SOLSYS=2, DM1 AND ZCONV ARE USED BUT
C     ARE NOT YET BUILT WHEN SISYPHE IS CALLED AT THE FIRST TIME STEP
C     WHAT'S MORE, UCONV_TEL AND VCONV_TEL ARE THEN DIFFERENT FROM
C     U2D AND V2D
!
      SOLSYS_SIS=1
      IF(LT.NE.1.AND.CODE(1:7).EQ.'TELEMAC') SOLSYS_SIS=SOLSYS
C     SEE FURTHER DOWN A MODIFICATION TO SOLSYS_SIS : IF(CORR_CONV)...
!
!
!
C THREE OPTIONS : ICQ=1: FREDSOE REFERENCE CONC. ZREF = 2.D50
C               ICQ=2: BIJKER METHOD ZREF = MAX(KSP,KS)
!               ICQ= 3: VAN RIJN ZREF= 0.5 KS
!
C     COMPUTES THE REFERENCE ELEVATION  -->  ZREF
!
        IF(ICQ.EQ.1) CALL OS('X=Y     ', X=ZREF, Y=KSP)
        IF(ICQ.EQ.2) CALL OS('X=Y     ', X=ZREF, Y=KSR)
! Nicolas Huybrechts Oct 2010  
        IF(ICQ.EQ.3) CALL OS('X=CY     ', X=ZREF, Y=KS,C=0.5D0)    
!       fin modif NH
!
C     COMPUTES THE ADVECTION VELOCITIES
C     TAKING INTO ACCOUNT THE VERTICAL PROFILE OF CONCENTRATIONS AND VELOCITIES
!
C     OPTVF : TENS                  0 : NORMAL
C                                   1 : ADVECTION FIELD DOES NOT SATISFY
C                                       CONTINUITY
!
C     OPTVF : UNITS                 0 : CONSTANT = 0
C                                   1 : CHI-TUAN CONSTANT
C                                   2 : LEO POSTMA CONSTANT
C                                   SEE CVTRVF IN BIEF AND
C                                   V5.7 RELEASE NOTES
!
!
      IF(CORR_CONV.AND.(.NOT.SEDCO)) THEN
!
C        TEMPORARILY USES U2D AND V2D HERE
C        EVEN THOUGH SOLSYS_TEL=2 BECAUSE SHOULD ALSO CORRECT DM1
         SOLSYS_SIS=1
         CALL CPSTVC(U2D,T12)
         CALL SUSPENSION_CONV(TOB,XMVE, KSR,NPOIN,ZREF,U2D,V2D,HN,HMIN,
     &                        UCONV,VCONV,KARMAN,ZERO,XWC,T1,T12)
C        ADVECTION FORM WHICH ACCEPTS AN ADVECTION FIELD
C        THAT DOES NOT SATISFY CONTINUITY + CHI-TUAN CONSTANT
         OPTVF=11
!
      ELSE
!
C       POINTERS ARE USED TO AVOID COPY
!
        IF(SOLSYS_SIS.EQ.1) THEN
          UCONV%R=>U2D%R
          VCONV%R=>V2D%R
        ELSE
C         HERE UCONV_TEL WAS PASSED ON
          UCONV%R=>UCONV_TEL%R
          VCONV%R=>VCONV_TEL%R
        ENDIF
C        ADVECTION FORM THAT REQUIRES AN ADVECTION FIELD
C        THAT SATISFIES CONTINUITY + CHI-TUAN CONSTANT
         OPTVF=1
!
      ENDIF
!
C     ADVECTION WITH CHARACTERISTICS
!
      IF(RESOL == 1) THEN
         IF (DEBUG > 0) WRITE(LU,*) 'CHARAC'
         CALL CHARAC(CS,CTILD,1,UCONV,VCONV,S,S,DT,IFAMAS,
     &          IELMT, NPOIN,1,1,MSK,MASKEL,AM1_S%X,AM1_S%D,
     &               TB,IT1%I,IT2%I,IT3%I,IT4%I,
     &               MESH,MESH%NELEM,MESH%NELMAX,MESH%IKLE,
     &               MESH%SURDET)
         IF (DEBUG > 0) WRITE(LU,*) 'END_CHARAC'
      ENDIF
!
C     FLUX COMPUTATION AT TIME N (CALLED FOR THE FIRST CLASS
C                                     WHICH MUST BE THE SAND)
!
C DEPOSITION IS TREATED THE SAME WAY FOR MIXES OR SEDIMENTS ALONE
C!---> T2: RATION BETWEEN BOTTOM CONCENTRATION AND AVERAGE
C          CONCENTRATION
!

      IF (DEBUG > 0) WRITE(LU,*) 'SUSPENSION_DEPOT'
      CALL SUSPENSION_DEPOT(TOB, HN,ACLADM,NPOIN, HMIN,XWC,
     &  VITCD, ZERO,KARMAN,XMVE,T1,T2,ZREF,FLUDPT,DEBUG,SEDCO)
!
! +++++++++++++++++++++++++++++
C EROSION IS TREATED DIFFERENTLY : TASS PASSED IN ARGUMENT
C SKIN FRICTION TAUP  --> T4
!
      CALL OS('X=CYZ   ', X= T4, Y= TOB, Z= MU, C=1.D0)
      CALL OS('X=+(Y,C)', X=T4, Y=T4, C=ZERO)
C V6P0 CV MODIFICATIONS
      IF(.NOT.MIXTE) THEN
        IF(.NOT.SEDCO) THEN
         IF (DEBUG > 0) WRITE(LU,*) 'SUSPENSION_EROSION'
          CALL SUSPENSION_EROSION(T4,HN,ACLADM,AVA,
     &     NPOIN,CHARR,XMVE,XMVS,GRAV,HMIN,XWC,ZERO,
     &     ZREF,AC,FLUER,CSTAEQ,QS_C,ICQ,DEBUG)
         IF (DEBUG > 0) WRITE(LU,*) 'END_SUSPENSION_EROSION'
!
C NOTE JMH : THIS SHOULD BE INCLUDED IN SUSPENSION_EROSION
!
         DO I=1,NPOIN
           FLUER%R(I)=MIN(FLUER%R(I),ELAY%R(I)*AVA(I)/DT*CSF_SABLE)
         ENDDO
!
C MUD ONLY : NEW SUBROUTINE
!
        ELSE
          CALL  SUSPENSION_EROSION_COH (T4,NPOIN,
     &       XMVE,XMVS,GRAV, VITCE, PARTHENIADES,ZERO, DEBUG,
     &      FLUER, ES, TOCE_VASE, NCOUCH_TASS, DT, MS_VASE%R,TASS)
!
          IF(.NOT.TASS) THEN
            DO I=1,NPOIN
              FLUER%R(I)=MIN(FLUER%R(I),ELAY%R(I)*CSF_VASE/DT)
            ENDDO
          ENDIF
        ENDIF
C MIXED SEDIMENT
       ELSE
C         CSF=CSF_VASE ! DEFINED IN MIXED SEDIMENT
         IF(.NOT.SEDCO) THEN
          IF(DEBUG > 0) WRITE(LU,*) 'SUSPENSION_FLUX_MIXTE'
          CALL SUSPENSION_FLUX_MIXTE(T4,HN,ACLADM,CS,
     &         NPOIN,CHARR,XMVE,XMVS,GRAV,HMIN,XWC,
     &         ZERO,KARMAN,PARTHENIADES,FLUER,
     &         FLUER_VASE,ZREF,AC,CSTAEQ,QS_C,ICQ,DEBUG,
     &         AVAIL,NSICLA,ES,TOCE_VASE,NCOUCH_TASS,DT,
     &         TOCE_MIXTE%R,MS_SABLE%R,MS_VASE%R)
          IF (DEBUG > 0) WRITE(LU,*) 'END_SUSPENSION_FLUX_MOY'
        ENDIF
        IF(SEDCO) CALL OS('X=Y     ',X=FLUER, Y=FLUER_VASE)
      ENDIF
!
C ....END OF MODIFICATION FOR MIXED SEDIMENTS
!
C     TREATS SMALL DEPTHS
!
      IF(OPTBAN.EQ.1) THEN
        DO I = 1, NPOIN
          IF(HN%R(I).LE.HMIN) THEN
            FLUDPT%R(I)=0.D0
            FLUER%R(I) =0.D0
          ENDIF
        ENDDO
      ELSEIF(OPTBAN.EQ.2) THEN
        CALL OS('X=XY    ',X=FLUER ,Y=MASKPT)
        CALL OS('X=XY    ',X=FLUDPT,Y=MASKPT)
      ENDIF
!
C     IMPLICIT SOURCE TERM FOR THE DEPOSITION
!
      CALL OS('X=-Y    ',X=T9,Y=FLUDPT)
!
C     EXPLICIT SOURCE TERM WITHOUT PUNCTUAL SOURCES
!
      CALL OS('X=Y     ',X=T11,Y=FLUER)
!
      DO I=1,NPOIN
        IF(HN%R(I).GT.HMIN) THEN
          T11%R(I)=T11%R(I)/HN%R(I)
        ELSE
          T11%R(I)=0.D0
        ENDIF
      ENDDO
!
C     JMH: 01/08/2005
C     IN DIFFIN A SPECIFIC TREATMENT IS DONE IF THE ADVECTION METHOD
C     IS THE CHARACTERISTICS: FREE OUTPUTS ARE TREATED LIKE DIRICHLET.
C     THIS SPECIFIC TREATMENT IS CANCELLED HERE BY SENDING A MODIFIED
C     VALUE FOR RESOL : RESOL_MOD (IN DIFFIN THE ONLY TEST IS:
C     IF(RESOL.EQ.1) THEN .... ELSE ....  ENDIF)
!
      RESOL_MOD=RESOL
      IF(RESOL_MOD.EQ.1) RESOL_MOD=2
      CALL DIFFIN(MASKTR,LIMDIF%I,LICBOR%I,CLT%I,U2D%R,V2D%R,
     &            MESH%XNEBOR%R,MESH%YNEBOR%R,
     &            MESH%NBOR%I,MESH%KP1BOR%I,NPTFR,
     &            KENT,KSORT,KLOG,KINC,KNEU,KDIR,KDDL,RESOL_MOD,
     &            MESH%NELBOR%I,NPOIN,MESH%NELMAX,
     &            MSK,MASKEL%R,0,
     &            .FALSE.,BID,BID,BID,CS,CBOR,MESH)
!
C     IMPOSES THE EQUILIBRIUM CONCENTRATION FOR THE INFLOW NODES  !
C     HERE CBOR FROM BOUNDARY CONDITIONS FILE OR SUBROUTINE CONLIT
C     OVERWRITTEN
!
C     T2 = RATIO BETWEEN BOTTOM CONC. AND AVERAGE CONC. MUST BE KEPT UNTIL THIS STAGE
!
      IF(IMP_INFLOW_C) THEN
!
        DO K = 1, NPTFR
          IF(CLT%I(K).EQ.KENT) THEN
            I = MESH%NBOR%I(K)
            IF(.NOT.SEDCO) THEN
CV SEE MODIFICATIONS IN SUSPENSION_EROSION
CV               CBOR%R(K) = CSTAEQ%R(I)/T2%R(I)*AVA(I)
               CBOR%R(K) = CSTAEQ%R(I)/T2%R(I)
               IF(MIXTE) CBOR%R(K) = FLUER%R(I)/T2%R(I)/XWC
            ELSE
               CBOR%R(K) = FLUER%R(I)/XWC
            ENDIF
          ENDIF
        ENDDO
!
      ENDIF
!
!
C     ADVECTION-DISPERSION STEP
!
C     CONFIGURATION TO CALL CVDFTR
!
      TETAH  = 1.D0 - TETA_SUSP
      MASSOU = 0.D0
      AGGLOT=1.D0
      YASMI2 = .TRUE.
!
C     BOUNDARY FLUXES MUST BE SPECIFIED TO CVDFTR (FINITE VOLUMES CASE)
C     AND TO SUSPENSION_BILAN
C     SISYPHE ALONE     : THEY MUST BE COMPUTED
C     WHEN COUPLING     : THEY ARE GIVEN BY THE CALLING SUBROUTINE
C                         EXCEPT AT THE 1ST ITERATION
!
      IF(CODE(1:7).NE.'TELEMAC'.OR.LT.EQ.1) THEN
        CALL VECTOR(FLBOR_SIS,'=','FLUBDF          ',IELBOR(IELMT,1),
C                        HPROP (HERE HPROP=HN, INVESTIGATE)
     &              1.D0,HN   ,HN,HN,UCONV,VCONV,VCONV,
     &              MESH,.FALSE.,MASKPT)
      ELSE
        CALL OS('X=Y     ',X=FLBOR_SIS,Y=FLBOR_TEL)
C       MUST ALSO CHANGE BOUNDARY FLUXES IF THE ADVECTION
C       FIELD IS CORRECTED (T12 MUST HAVE BEEN KEPT SINCE
C       CALL TO SUSPENSION_CONV)
        IF(CORR_CONV.AND.(.NOT.SEDCO)) THEN
          CALL OSBD('X=CXY   ',FLBOR_SIS,T12,T12,1.D0,MESH)
        ENDIF
      ENDIF
!
C     FINITE VOLUMES ADVECTION USES THE TRUE H FROM THE PREVIOUS STEP
      IF(CODE(1:7).EQ.'TELEMAC') THEN
        IF(OPTBAN.NE.0) THEN
          CALL CPSTVC(CST,T1)
C         HN_TEL IS NOT CLIPPED
          DO I=1,NPOIN
            T1%R(I)=MAX(HN_TEL%R(I),HMIN)
          ENDDO
          HOLD=>T1
        ELSE
          HOLD=>HN_TEL
        ENDIF
      ELSE
C       IN THIS CASE H AND HN ARE CONFUSED
        HOLD=>HN
      ENDIF
!
      IF (DEBUG > 0) WRITE(LU,*) 'APPEL DE CVDFTR'
      CALL CVDFTR
     & (CST, CTILD, CS, T2,
C                            H         HTILD
     &  DIFT, RESOL, .TRUE., HN, HOLD, HPROP, TETAH,
     &  UCONV,VCONV,DM1,ZCONV,SOLSYS_SIS,
C                     TEXP SMH  YASMH   TIMP
     &  DISP, DISP_C, T11, T2, .FALSE., T9,  YASMI2,AM1_S,AM2_S,
     &  ZF, CBOR, AFBOR, BFBOR, LIMDIF, MASKTR, MESH,
     &  W1, TB, T8, T12, T3, T4, T5, T6, T7, T10, TE1, TE2, TE3,
C                                                       BILMAS
     &  KDIR,KDDL,KENT,DT,ENTETS,TETA_SUSP,
     &  AGGLOT,ENTETS,.FALSE.,OPTSUP,
     &  1, LT, NIT, OPDTRA, OPTBAN, MSK, MASKEL, MASKPT, MBOR, S,
C               OPTSOU
     &  MASSOU, 1,     SLVTRA,FLBOR_SIS,V2DPAR,UNSV2D,OPTVF,FLBORTRA,
     &     S   ,  .FALSE.  )
C       FLULIM   YAFLULIM    : HERE YAFLULIM SET TO FALSE AND S
C                              VOID STRUCTURE. WHEN YAFLULIM AND FLULIM
C                              CAN BE OBTAINED FROM TELEMAC2D
C                              IT WILL SPEED-UP TIDAL FLAT VERSIONS
C                              OF ADVECTION SCHEMES 
C
      IF (DEBUG > 0) WRITE(LU,*) 'END_CVDFTR'
!
      DO I=1,NPOIN
        FLUDP%R(I)=FLUDPT%R(I)*CST%R(I)
      ENDDO
!
C     COMPUTES EVOLUTION AND UPDATES DATA
C     TASS TO BE PASSED IN ARGUMENT
!
C      IF(.NOT.MIXTE.AND..NOT.TASS) THEN
C        CALL OS('X=Y-Z   ', X=ZFCL_S, Y=FLUDP, Z=FLUER)
C        CALL OS('X=CX    ', X=ZFCL_S, C=DT/CSF)
C      ELSE
C        CALL SIS_ERODE(ZFCL_S, FLUDP, FLUER,DT,
C     *                 NPOIN,XMVS,XKV, T3,SEDCO,
C     *                 CONC_VASE,NCOUCH_TASS,
C     *                 MS_SABLE%R,MS_VASE%R)
C      ENDIF
       IF(.NOT.SEDCO) THEN
          CALL SUSPENSION_EVOL(ZFCL_S, FLUDP, FLUER,DT,
     &                 NPOIN,CSF_SABLE, XMVS,T3,MS_SABLE%R,
     &                 SEDCO,CONC_VASE,NCOUCH_TASS)
       ELSE
          CALL SUSPENSION_EVOL(ZFCL_S, FLUDP, FLUER,DT,
     &                 NPOIN,CSF_VASE,XMVS, T3,MS_VASE%R,
     &                 SEDCO,CONC_VASE,NCOUCH_TASS)
       ENDIF
!
C     WRITES OUT THE MIN/MAX VALUES TO THE LISTING
!
      IF (ENTETS) THEN
         IF (DEBUG > 0) WRITE(LU,*) 'SUSPENSION_LISTING'
         CALL SUSPENSION_LISTING
     &        (MESH,CST,ZFCL_S,UCONV,VCONV,MASKEL,
     &         IELMT,DT,MSK,T1)
         IF (DEBUG > 0) WRITE(LU,*) 'END_SUSPENSION_LISTING'
      ENDIF
!
C     MASS-BALANCE FOR THE SUSPENSION
!
      IF(BILMA) THEN
         IF (SEDCO) CSF = CSF_VASE
         IF(.NOT.SEDCO) CSF = CSF_SABLE
         IF (DEBUG > 0) WRITE(LU,*) 'SUSPENSION_BILAN'
         CALL SUSPENSION_BILAN
     &        (MESH,CST,HN,ZFCL_S,MASKEL,IELMT,ITRA,LT,NIT,
     &         DT,CSF,MASSOU,MASED0,MSK,ENTETS,MASTEN,MASTOU,
     &         MASINI,T2,T3,MASFIN,MASDEPT,MASDEP,AGGLOT,VOLU2D,
     &         NUMLIQ,NFRLIQ,NPTFR,FLBORTRA)
         IF (DEBUG > 0) WRITE(LU,*) 'END_SUSPENSION_BILAN'
      ENDIF
!
      UCONV%R=>SAVE_UCONV
      VCONV%R=>SAVE_VCONV
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
C
C#######################################################################
C
