C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AC, ACLADM, AVA, BETA, BETA2, BIJK, CALFA, CF, COEFPN, D90, DEBUG, DEVIA, DM, FW, GRAV, HIDFAC, HIDI, HIDING, HMIN, HN, HOULE, ICF, IELMT, KARIM_HOLLY_YANG, KARMAN, KENT, KSP, KSR, LIQBOR, MASKEL, MASKPT, MESH, MSK, MU, NPOIN, NPTFR, OPTBAN, PHISED, PI, QBOR, QSC, QSS, S, SALFA, SECCURRENT, SLOPEFF, SUSP, T1, T10, T11, T12, T2, T3, T4, T5, T6, T7, T8, T9, THETAW, TOB, TOBW, TW, U2D, UNLADM, UNORM, UNSV2D, UW, V2D, VCE, XKV, XMVE, XMVS, XWC, ZERO, ZF_C
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BEDLOAD_SOLIDISCHARGE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> BEDLOAD_EFFPNT(), BEDLOAD_FORMULA(), BEDLOAD_HIDING_FACTOR(), OS()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BEDLOAD_MAIN()

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
!> </td><td> 15/09/2009
!> </td><td> J.-M. HERVOUET
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 11/03/2009
!> </td><td>
!> </td><td> MODIFICATIONS FOR PARALLEL MODE
!> </td></tr>
!>      <tr>
!>      <td><center> 5.9                                       </center>
!> </td><td> 11/03/2008
!> </td><td> J.-M. HERVOUET
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.5                                       </center>
!> </td><td> 14/09/2004
!> </td><td> F. HUVELIN
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.4                                       </center>
!> </td><td> **/10/2003
!> </td><td> C. VILLARET
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.3                                       </center>
!> </td><td> **/07/2002
!> </td><td> M. GONZALES DE LINARES
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.2                                       </center>
!> </td><td> **/12/2001
!> </td><td> B. MINH DUC
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.1                                       </center>
!> </td><td> 20/05/1995
!> </td><td> E. PELTIER; C. LENORMANT; J.-M. HERVOUET
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
!>          <tr><td>AVA
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
!>          <tr><td>CALFA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>COEFPN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>D90
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEBUG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEVIA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DM
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
!>          <tr><td>KARIM_HOLLY_YANG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KENT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KSP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KSR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIQBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKPT
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
!>          <tr><td>OPTBAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PHISED
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSS
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
!>          <tr><td>VCE
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
!>          <tr><td>ZF_C
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                     SUBROUTINE BEDLOAD_SOLIDISCHARGE
     &(MESH,U2D,V2D,UNORM,HN,TW,UW,MU,TOB,CF,TOBW,FW,THETAW,
     & AVA,MASKPT,MASKEL,ACLADM,UNLADM,KSP,KSR,LIQBOR,
     & QBOR,DEBUG,NPOIN,NPTFR,IELMT,ICF,KENT,OPTBAN,
     & HIDFAC,GRAV,DM,D90,XWC,XMVE,XMVS,XKV,VCE,HMIN,
     & HIDI,KARMAN,ZERO,PI,KARIM_HOLLY_YANG,
     & SUSP,MSK,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,
     & T11,T12,AC,HIDING,QSC,QSS,
     & SLOPEFF,COEFPN,PHISED,CALFA,SALFA,BETA,ZF_C,S,
     & DEVIA,BETA2,SECCURRENT,BIJK,HOULE,UNSV2D)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AC             |---| 
C| ACLADM         |---| 
C| AVA            |---| 
C| BETA           |---| 
C| BETA2          |---| 
C| BIJK           |---| 
C| CALFA          |---| 
C| CF             |---| 
C| COEFPN         |---| 
C| D90            |---| 
C| DEBUG          |---| 
C| DEVIA          |---| 
C| DM             |---| 
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
C| KARIM_HOLLY_YAN|---| 
C| KARMAN         |---| 
C| KENT           |---| 
C| KSP            |---| 
C| KSR            |---| 
C| LIQBOR         |---| 
C| MASKEL         |---| 
C| MASKPT         |---| 
C| MESH           |---| 
C| MSK            |---| 
C| MU             |---| 
C| NPOIN          |---| 
C| NPTFR          |---| 
C| OPTBAN         |---| 
C| PHISED         |---| 
C| PI             |---| 
C| QBOR           |---| 
C| QSC            |---| 
C| QSS            |---| 
C| S             |---| 
C| SALFA          |---| 
C| SECCURRENT     |---| 
C| SLOPEFF        |---| 
C| SUSP           |---| 
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
C| VCE            |---| 
C| XKV            |---| 
C| XMVE           |---| 
C| XMVS           |---| 
C| XWC            |---| 
C| ZERO           |---| 
C| ZF_C           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE,
     &    EX_BEDLOAD_SOLIDISCHARGE => BEDLOAD_SOLIDISCHARGE
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: U2D, V2D,  HN, TW, UW
      TYPE(BIEF_OBJ),   INTENT(IN)    :: UNORM ,MU, KSR ,KSP
      TYPE(BIEF_OBJ),   INTENT(IN)    :: TOB, CF, TOBW, FW, THETAW
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASKPT, MASKEL
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ACLADM, UNLADM, LIQBOR, QBOR
      INTEGER,          INTENT(IN)    :: DEBUG
      INTEGER,          INTENT(IN)    :: NPOIN, NPTFR, IELMT, ICF
      INTEGER,          INTENT(IN)    :: KENT, OPTBAN,HIDFAC
      DOUBLE PRECISION, INTENT(IN)    :: GRAV, DM, D90, XWC, XMVE, XMVS
      DOUBLE PRECISION, INTENT(IN)    :: XKV, VCE, HMIN
      DOUBLE PRECISION, INTENT(IN)    :: HIDI
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN, ZERO, PI
      DOUBLE PRECISION, INTENT(IN)    :: KARIM_HOLLY_YANG
      LOGICAL,          INTENT(IN)    :: SUSP, MSK,SECCURRENT,HOULE
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T1,T2,T3,T4,T5,T6
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T7,T8,T9,T10,T11,T12
      DOUBLE PRECISION, INTENT(INOUT) :: AC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: HIDING
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QSC,QSS
C
      INTEGER,          INTENT(IN)    :: SLOPEFF,DEVIA
      DOUBLE PRECISION, INTENT(IN)    :: PHISED,BETA,BETA2
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ZF_C,S,UNSV2D
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CALFA,SALFA,COEFPN
C
      DOUBLE PRECISION, INTENT(IN)    :: BIJK,AVA(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION P_DMAX
      EXTERNAL         P_DMAX
C
      INTEGER          :: I
!
!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
! ********************************************************
C 0 - COMPUTES THE PARAMETERS FOR THE SLOPE EFFECT
! ********************************************************
!
      IF (DEBUG > 0) WRITE(LU,*) 'BEDLOAD_EFFPNT'
C
C     SLOPE EFFECT
C
      IF(DEVIA.EQ.0) THEN
        CALL OS('X=Y/Z   ',CALFA, U2D, UNORM, 0.D0, 2, 1.D0, 1.D-12)
        CALL OS('X=Y/Z   ',SALFA, V2D, UNORM, 0.D0, 2, 0.D0, 1.D-12)
      ENDIF
C
      IF(SLOPEFF.EQ.0) CALL OS('X=C     ',X=COEFPN,C=1.D0)
C
      IF(SLOPEFF.NE.0.OR.DEVIA.NE.0) THEN
C
      CALL BEDLOAD_EFFPNT
     &     (MASKEL,LIQBOR,S,ZF_C,U2D,V2D,UNORM,NPOIN,NPTFR,IELMT,
     &      KENT,BETA,PI,MSK,MESH,T1,T2,T3,T4,
     &      COEFPN,CALFA,SALFA,SLOPEFF,PHISED,DEVIA,BETA2,
     &      TOB,XMVS,XMVE,DM,GRAV,UNSV2D)
C
      ENDIF
C
      IF (DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_EFFPNT'
!
      ! **************************************** !
      ! I - MASKING/EXPOSURE COEFFICIENT         !
      ! **************************************** !
      IF (DEBUG > 0) WRITE(LU,*) 'BEDLOAD_HIDING_FACTOR'
!
C     WITH HUNZIKER FORMULATION (6), THE HIDING FACTOR IS COMPUTED
C     WITH THE SOLID DISCHARGE (SEE BEDLOAD_HUNZ_MEYER.F)
!
      IF(ICF.NE.6) THEN
        CALL BEDLOAD_HIDING_FACTOR
     &     (ACLADM, HIDFAC, NPOIN, HIDI, DM, KARIM_HOLLY_YANG, HIDING)
      ENDIF
      IF (DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_HIDING_FACTOR'
!
      ! ******************************************* !
      ! II - QSC COMPUTED USING EMPIRICAL FORMULATION !
      !      T1 = DQSC/DH                           !
      ! ******************************************* !
      IF (DEBUG > 0) WRITE(LU,*) 'BEDLOAD_FORMULA'
!
      CALL BEDLOAD_FORMULA
     &  (U2D,V2D, UNORM,HN, CF, MU,TOB, TOBW, UW, TW, THETAW, FW,
     &   ACLADM, UNLADM, KSP,KSR,AVA, NPOIN, ICF, HIDFAC, XMVS, XMVE,
     &   DM, GRAV, VCE, XKV, HMIN, XWC, D90, KARMAN, ZERO,
     &   PI, SUSP, AC, HIDING, T1, T2, T3, T4, T5, T6, T7, T8, T9,
     &   T10, T11, T12, QSC, QSS, IELMT,SECCURRENT,
     &   SLOPEFF, COEFPN, BIJK, HOULE)
      IF (DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_FORMULA'
!
      ! **************************************************** !
      ! IV - BOUNDARY NODES WITH IMPOSED FLOW                !
      ! **************************************************** !
      IF (DEBUG > 0) WRITE(LU,*) 'BOUNDARY_NODES_TREATMENT'
      DO I = 1 , NPTFR
        IF(LIQBOR%I(I).EQ.KENT) QSC%R(MESH%NBOR%I(I)) = QBOR%R(I)
      ENDDO
      IF (DEBUG > 0) WRITE(LU,*) 'END_BOUNDARY_NODES_TREATMENT'
!
      ! ************************************ !
      ! V - TIDAL FLATS                      !
      ! ************************************ !
      IF(OPTBAN.EQ.2) THEN
        IF (DEBUG > 0) WRITE(LU,*) 'TIDAL_FLATS_TREATMENT'
        CALL OS('X=XY    ', X=QSC, Y=MASKPT)
        IF (DEBUG > 0) WRITE(LU,*) 'END_TIDAL_FLATS_TREATMENT'
      ENDIF
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
C
C#######################################################################
C