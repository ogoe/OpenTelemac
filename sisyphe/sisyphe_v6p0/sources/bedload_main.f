C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MAIN SUBROUTINE FOR THE BEDLOAD TRANSPORT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_SISYPHE, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AC, ACLADM, AT0, AVAIL, BETA, BETA2, BIJK, BREACH, CALFA, CF, COEFPN, CONST_ALAYER, DEBUG, DEVIA, DTS, DZF_GF, EBOR, ELAY, ELAY0, ENTET, ENTETS, FD90, FDM, FRACSED_GF, FW, GRAV, HIDFAC, HIDI, HIDING, HMIN, HN, HOULE, ICF, IELMT, ISOUS, IT1, KARIM_HOLLY_YANG, KARMAN, KDDL, KDIR, KENT, KINC, KLOG, KNEU, KSORT, KSP, KSR, LCONDIS, LGRAFED, LIEBOR, LIMTEC, LIQBOR, LOADMETH, LS0, LT, MASK, MASKEL, MASKPT, MASKTR, MESH, MSK, MU, NPOIN, NPTFR, NSICLA, NSOUS, OPTBAN, PHISED, PI, Q, QBOR, QSCLXC, QSCLYC, QSCL_C, QSCL_S, QSXC, QSYC, QS_C, S, SALFA, SECCURRENT, SEDCO, SLOPEFF, SUSP, T1, T10, T11, T12, T13, T2, T3, T4, T5, T6, T7, T8, T9, THETAW, TOB, TOBW, TW, U2D, UNLADM, UNORM, UNSV2D, UW, V2D, V2DPAR, VCE, VF, XKV, XMVE, XMVS, XWC, ZERO, ZF, ZFCL_C, ZF_C
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BEDLOAD_MAIN
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> BEDLOAD_DIFFIN(), BEDLOAD_EVOL(), BEDLOAD_SOLIDISCHARGE(), DREDGESIM_INTERFACE(), OS()
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
!>      <td><center> 6.0                                       </center>
!> </td><td> 21/12/2006
!> </td><td> JMH
!> </td><td> BEDLOAD_TIMESTEP NO LONGER EXISTS
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 14/09/2004
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
!>          <tr><td>AT0
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AVAIL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BETA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BETA2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BIJK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BREACH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CALFA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>COEFPN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CONST_ALAYER
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEBUG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEVIA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DTS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DZF_GF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>EBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ELAY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ELAY0
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ENTET
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ENTETS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FD90
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FDM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FRACSED_GF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HIDFAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HIDI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HIDING
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HMIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HOULE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ICF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELMT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ISOUS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IT1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KARIM_HOLLY_YANG
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
!>          <tr><td>LCONDIS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LGRAFED
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIEBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIMTEC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIQBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LOADMETH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LS0
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASK
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
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MU
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
!>          <tr><td>OPTBAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PHISED
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Q
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSCLXC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSCLYC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSCL_C
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSCL_S
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSXC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSYC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QS_C
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>S
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SALFA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SECCURRENT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SEDCO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SLOPEFF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SUSP
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
!>          <tr><td>T13
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
!>          <tr><td>THETAW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOBW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UNLADM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UNORM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UNSV2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V2DPAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XKV
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
!>          <tr><td>ZERO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZFCL_C
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZF_C
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE BEDLOAD_MAIN !
     &  (ACLADM,KSP,KSR, V2DPAR,UNSV2D,CF,EBOR,FW,HN,LIQBOR,
     &   MASK, MASKEL, MASKPT, Q, QBOR, U2D,
     &   V2D, S,UNLADM,UW,THETAW,MU,TOB,TOBW,TW,ZF,
     &   DEBUG, HIDFAC, ICF, IELMT, ISOUS, KDDL, KDIR,
     &   KENT, KINC, KLOG, KNEU, KSORT, LOADMETH, LT,
     &   NPOIN, NPTFR, NSICLA, OPTBAN, LS0, BETA, FD90, FDM,
     &   GRAV, HIDI, HMIN, VCE, XKV, XMVE, XMVS, XWC,
     &   PI, KARMAN, ZERO, KARIM_HOLLY_YANG,MSK, SUSP, VF,
     &   ENTET, CONST_ALAYER, LCONDIS, LGRAFED, MESH,
     &   ELAY, LIEBOR, LIMTEC, MASKTR,
     &   IT1, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11,
     &   T12,T13,UNORM,AC, AT0, DTS, ELAY0, FRACSED_GF,
     &   AVAIL, BREACH, CALFA, COEFPN,
     &   DZF_GF, HIDING, QSCL_C, QSCL_S, QS_C,
     &   QSCLXC, QSXC, QSCLYC, QSYC, SALFA, ZF_C, ZFCL_C, NSOUS,
     &   ENTETS, SECCURRENT, SLOPEFF,
     &   PHISED, DEVIA, BETA2, BIJK,SEDCO,HOULE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AC             |---| 
C| ACLADM         |---| 
C| AT0            |---| 
C| AVAIL          |---| 
C| BETA           |---| 
C| BETA2          |---| 
C| BIJK           |---| 
C| BREACH         |---| 
C| CALFA          |---| 
C| CF             |---| 
C| COEFPN         |---| 
C| CONST_ALAYER   |---| 
C| DEBUG          |---| 
C| DEVIA          |---| 
C| DTS            |---| 
C| DZF_GF         |---| 
C| EBOR           |---| 
C| ELAY           |---| 
C| ELAY0          |---| 
C| ENTET          |---| 
C| ENTETS         |---| 
C| FD90           |---| 
C| FDM            |---| 
C| FRACSED_GF     |---| 
C| FW             |---| 
C| GRAV           |---| 
C| HIDFAC         |---| 
C| HIDI           |---| 
C| HIDING         |---| 
C| HMIN           |---| 
C| HN             |---| 
C| HOULE          |---| 
C| ICF            |---| 
C| IELMT          |---| 
C| ISOUS          |---| 
C| IT1            |---| 
C| KARIM_HOLLY_YAN|---| 
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
C| LCONDIS        |---| 
C| LGRAFED        |---| 
C| LIEBOR         |---| 
C| LIMTEC         |---| 
C| LIQBOR         |---| 
C| LOADMETH       |---| 
C| LS0            |---| 
C| LT             |---| 
C| MASK           |---| 
C| MASKEL         |---| 
C| MASKPT         |---| 
C| MASKTR         |---| 
C| MESH           |---| 
C| MSK            |---| 
C| MU             |---| 
C| NPOIN          |---| 
C| NPTFR          |---| 
C| NSICLA         |---| 
C| NSOUS          |---| 
C| OPTBAN         |---| 
C| PHISED         |---| 
C| PI             |---| 
C| Q             |---| 
C| QBOR           |---| 
C| QSCLXC         |---| 
C| QSCLYC         |---| 
C| QSCL_C         |---| 
C| QSCL_S         |---| 
C| QSXC           |---| 
C| QSYC           |---| 
C| QS_C           |---| 
C| S             |---| 
C| SALFA          |---| 
C| SECCURRENT     |---| 
C| SEDCO          |---| 
C| SLOPEFF        |---| 
C| SUSP           |---| 
C| T1             |---| 
C| T10            |---| 
C| T11            |---| 
C| T12            |---| 
C| T13            |---| 
C| T2             |---| 
C| T3             |---| 
C| T4             |---| 
C| T5             |---| 
C| T6             |---| 
C| T7             |---| 
C| T8             |---| 
C| T9             |---| 
C| THETAW         |---| 
C| TOB            |---| 
C| TOBW           |---| 
C| TW             |---| 
C| U2D            |---| 
C| UNLADM         |---| 
C| UNORM          |---| 
C| UNSV2D         |---| 
C| UW             |---| 
C| V2D            |---| 
C| V2DPAR         |---| 
C| VCE            |---| 
C| VF             |---| 
C| XKV            |---| 
C| XMVE           |---| 
C| XMVS           |---| 
C| XWC            |---| 
C| ZERO           |---| 
C| ZF             |---| 
C| ZFCL_C         |---| 
C| ZF_C           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_SISYPHE, EX_BEDLOAD_MAIN => BEDLOAD_MAIN
      USE DECLARATIONS_SISYPHE, ONLY : DREDGESIM
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU


      ! 2/ GLOBAL VARIABLES
      ! -------------------

      TYPE(BIEF_OBJ),   INTENT(IN)    :: ACLADM, KSR,V2DPAR,UNSV2D
      TYPE(BIEF_OBJ),   INTENT(IN)    :: CF,FW,KSP,HN,LIQBOR
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASK, MASKEL, MASKPT
      TYPE(BIEF_OBJ),   INTENT(IN)    :: Q, QBOR
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: U2D, V2D,TOB, MU,UNORM,EBOR
      TYPE(BIEF_OBJ),   INTENT(IN)    :: S,UNLADM
      TYPE(BIEF_OBJ),   INTENT(IN)    :: UW, THETAW,  TOBW, TW
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ZF
      INTEGER,          INTENT(IN)    :: DEBUG, HIDFAC, ICF
      INTEGER,          INTENT(IN)    :: IELMT, ISOUS, KDDL, KDIR, KENT
      INTEGER,          INTENT(IN)    :: KINC, KLOG, KNEU, KSORT
      INTEGER,          INTENT(IN)    :: LOADMETH, LT,NPOIN, NPTFR
      INTEGER,          INTENT(IN)    :: NSICLA, OPTBAN
      DOUBLE PRECISION, INTENT(IN)    :: LS0, BETA, FD90(NSICLA)
      DOUBLE PRECISION, INTENT(IN)    :: FDM(NSICLA),GRAV
      DOUBLE PRECISION, INTENT(IN)    :: HIDI(NSICLA),HMIN,VCE
      DOUBLE PRECISION, INTENT(IN)    :: XKV,XMVE,XMVS,XWC(NSICLA)
      DOUBLE PRECISION, INTENT(IN)    :: PI,KARMAN,ZERO
      DOUBLE PRECISION, INTENT(IN)    :: KARIM_HOLLY_YANG
      LOGICAL,          INTENT(IN)    :: MSK, SUSP, VF
      LOGICAL,          INTENT(IN)    :: ENTET, CONST_ALAYER
      LOGICAL,          INTENT(IN)    :: LCONDIS, LGRAFED,SECCURRENT
      LOGICAL,          INTENT(IN)    :: SEDCO(NSICLA),HOULE
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: ELAY
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: LIEBOR, LIMTEC, MASKTR
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: IT1,T1,T2,T3,T4,T5,T6,T7
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T8,T9,T10,T11,T12,T13
      DOUBLE PRECISION, INTENT(INOUT) :: AC(NSICLA), AT0, DTS, ELAY0
      DOUBLE PRECISION, INTENT(INOUT) :: FRACSED_GF(NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: AVAIL(NPOIN,10,NSICLA)
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: BREACH, CALFA, COEFPN
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: DZF_GF
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HIDING
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSCL_C,QSCL_S
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QS_C, QSCLXC, QSXC, QSCLYC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSYC, SALFA, ZF_C, ZFCL_C
      INTEGER,          INTENT(INOUT) :: NSOUS
      LOGICAL,          INTENT(INOUT) :: ENTETS
      DOUBLE PRECISION,   INTENT(IN)  :: BETA2, PHISED
      INTEGER, INTENT (IN)            :: SLOPEFF, DEVIA
      DOUBLE PRECISION, INTENT(IN)    :: BIJK

      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER :: I
!
!
!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!
C     INITIALISES TECHNICAL BOUNDARY CONDITIONS
!
      IF (DEBUG > 0) WRITE(LU,*) 'BEDLOAD_DIFFIN'
      CALL BEDLOAD_DIFFIN
     &        (U2D, V2D, MESH%NBOR, MESH%XNEBOR, MESH%YNEBOR,
     &         MESH%KP1BOR,
     &         MASKEL, MESH%NELBOR, NPTFR, KENT, KSORT, KLOG, KINC,
     &         KDIR, KDDL, KNEU, MSK, IT1, LIEBOR, MASKTR, LIMTEC)
      IF (DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_DIFFIN'
!
      DO I = 1, NSICLA
!
C        FOR SAND
         IF(.NOT.SEDCO(I)) THEN
           IF (DEBUG > 0) WRITE(LU,*)
     &       'BEDLOAD_SOLIDISCHARGE : ',I,'/',NSICLA
!
           CALL BEDLOAD_SOLIDISCHARGE
     &        (MESH, U2D, V2D, UNORM,HN, TW, UW, MU,TOB,CF,
     &          TOBW,FW,THETAW,AVAIL(1:NPOIN,1,I),
     &          MASKPT, MASKEL, ACLADM,
     &          UNLADM,KSP,KSR, LIQBOR, QBOR%ADR(I)%P, DEBUG, NPOIN,
     &          NPTFR, IELMT, ICF, KENT, OPTBAN, HIDFAC, GRAV,
     &          FDM(I), FD90(I), XWC(I), XMVE, XMVS, XKV, VCE, HMIN,
     &          HIDI(I),KARMAN,ZERO,PI,
     &          KARIM_HOLLY_YANG,SUSP,MSK,T1,T2,
     &          T3, T4, T5, T6, T7, T8, T9, T10, T11,T12, AC(I),
     &          HIDING,QSCL_C%ADR(I)%P,QSCL_S%ADR(I)%P,
     &          SLOPEFF,COEFPN,PHISED,CALFA,SALFA,BETA,ZF,S,
     &          DEVIA, BETA2 , SECCURRENT, BIJK,HOULE,UNSV2D)
          IF(DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_SOLIDISCHARGE'
        ELSE
C         FOR COHESIVE SEDIMENT: ZERO BEDLOAD TRANSPORT RATE
C         JMH: IS THIS USEFUL ???
          CALL OS('X=0     ',X=QSCL_C%ADR(I)%P)
          CALL OS('X=0     ',X=QSCLXC%ADR(I)%P)
          CALL OS('X=0     ',X=QSCLYC%ADR(I)%P)
        ENDIF
C
      ENDDO
!
C     COMPUTES THE EVOLUTION FOR EACH CLASS
!
      DO I = 1, NSICLA
!
        IF(.NOT.SEDCO(I)) THEN
!
          IF (DEBUG > 0) WRITE(LU,*) 'BEDLOAD_EVOL : ',I,'/',NSICLA
          CALL BEDLOAD_EVOL(HN,Q,S,ELAY,ACLADM,AVAIL(1:NPOIN,1,I),
     &                      COEFPN,CALFA,SALFA,LIMTEC,
     &                      EBOR%ADR(I)%P,MASKEL,MASK,
     &                      V2DPAR,UNSV2D,DEBUG,NPOIN,NPTFR,IELMT,
     &                      KENT,KDIR,KDDL,LOADMETH,
     &                      DTS,FDM(I),FD90(I),HMIN,LS0,GRAV,XMVS,XMVE,
     &                      VCE,VF,ENTETS,MSK,CONST_ALAYER,
     &                      LCONDIS,MESH,QSCL_C%ADR(I)%P,
     &                      T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,
     &                      T13,ELAY0,BREACH,QSCLXC%ADR(I)%P,
     &                      QSCLYC%ADR(I)%P,ZFCL_C%ADR(I)%P,SLOPEFF)
          IF(DEBUG.GT.0) WRITE(LU,*) 'END_BEDLOAD_EVOL'
C
        ELSE
C
C         NO EVOLUTION FOR COHESIVE SEDIMENT
          CALL OS('X=0     ',X=ZFCL_C%ADR(I)%P)
C
        ENDIF
C
      ENDDO
C
C     CALLS DREDGESIM
C
      IF(DREDGESIM) CALL DREDGESIM_INTERFACE(2)

      ! *********************************************** !
      ! II - EVOLUTIONS AND QS FOR EACH CLASS ARE ADDED !
      ! *********************************************** !

      ! II.1 - INITIALISES
      ! ---------------------

      CALL OS('X=0     ', X=QS_C)
      CALL OS('X=0     ', X=ZF_C)

      ! II.2 - ADDS THE CLASSES
      ! ----------------------
      DO I=1,NSICLA
         ! CORRECTS THE SOLID TRANSPORT TO NOT TAKE INTO ACCOUNT THE
         ! POROSITY COEFFICIENT (MUST BE A NON-ZERO VALUE...)
         ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        IF(.NOT.SEDCO(I)) THEN
          CALL OS('X=CX    ', X=QSCL_C%ADR(I)%P, C=1.D0/XKV)
          CALL OS('X=CX    ', X=QSCLXC%ADR(I)%P, C=1.D0/XKV)
          CALL OS('X=CX    ', X=QSCLYC%ADR(I)%P, C=1.D0/XKV)
          CALL OS('X=X+Y   ', X=QS_C, Y=QSCL_C%ADR(I)%P)
          CALL OS('X=X+Y   ', X=ZF_C, Y=ZFCL_C%ADR(I)%P)
        ENDIF
      ENDDO
!
C     TIDAL FLATS WITH MASKING     JMH ON 27/07/2006
!
      IF(OPTBAN.EQ.2) CALL OS('X=XY    ',X=ZF_C,Y=MASKPT)
!
      ! II.3 - SLOPE EFFECT FOR THE SUM OF THE QS
      ! -----------------------------------------
      ! QS : COEFPN ALREADY ADDED IN QSCL_C
      CALL OS('X=YZ    ', X=QSXC, Y=QS_C, Z=CALFA)
      CALL OS('X=YZ    ', X=QSYC, Y=QS_C, Z=SALFA)

!======================================================================!
!======================================================================!

      RETURN
      END
C
C#######################################################################
C