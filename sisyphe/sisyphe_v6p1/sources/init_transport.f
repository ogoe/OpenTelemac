C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_SISYPHE, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AC, ACLADM, AVAIL, BIJK, CALFA, CF, CHARR, CMAX, COEFPN, CORR_CONV, CS, CS0, CSTAEQ, DEBU, FD90, FDM, FW, GRAV, HIDFAC, HIDING, HMIN, HN, HOULE, ICF, ICQ, IELMT, IMP_INFLOW_C, KARMAN, KSP, KSR, MESH, MU, NPOIN, NSICLA, PARTHENIADES, PI, QS, QSCL, QSCLXS, QSCLYS, QSCL_C, QSCL_S, QSXC, QSYC, QS_C, QS_S, SALFA, SECCURRENT, SEDCO, SLOPEFF, SUSP, T1, T10, T11, T12, T14, T2, T3, T4, T5, T6, T7, T8, T9, THETAW, TOB, TOBW, TROUVE, TW, U2D, UCONV, UNLADM, UNORM, UW, V2D, VCE, VCONV, VITCD, VITCE, XKV, XMVE, XMVS, XWC, ZERO, ZREF
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_SISYPHE :<br>
!> @link DECLARATIONS_SISYPHE::NOMBLAY NOMBLAY@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AAA, AT0, I, J, USTARP
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_INIT_TRANSPORT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> BEDLOAD_FORMULA(), CONDIM_SUSP(), OS(), SUSPENSION_BIJKER(), SUSPENSION_CONV(), SUSPENSION_FREDSOE(), SUSPENSION_ROUSE()
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
!> </td><td>
!> </td><td> C. VILLARET (LNHE) 01 30 87 83 28
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                        </center>
!> </td><td> 07/12/2009
!> </td><td> JMH
!> </td><td> MODIFICATIONS FOR RESTART WITH WARNINGS WHEN A VARIABLE
!>           THAT SHOULD HAVE BEEN IN THE PREVIOUS COMPUTATION FILE
!>           IS REINITIALISED HERE
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 16/09/2009
!> </td><td> JMH
!> </td><td> AVAIL(NPOIN,10,NSICLA)
!> </td></tr>
!>      <tr>
!>      <td><center>                                        </center>
!> </td><td> 24/01/2008
!> </td><td> JMH
!> </td><td> TEST 'IF(CHARR.OR.SUSP)' ADDED AT THE END
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
!>          <tr><td>AVAIL
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
!>          <tr><td>CHARR
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>CMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>COEFPN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CORR_CONV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CS0
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CSTAEQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEBU
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>FD90
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FDM
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
!>          <tr><td>HIDING
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>HMIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>WATER DEPTH
!>    </td></tr>
!>          <tr><td>HOULE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ICF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ICQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELMT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IMP_INFLOW_C
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KSP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KSR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NSICLA
!></td><td>--></td><td>NUMBER OF SEDIMENT CLASSES
!>    </td></tr>
!>          <tr><td>PARTHENIADES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSCL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSCLXS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSCLYS
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
!>          <tr><td>QS_S
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
!>          <tr><td>T14
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
!>          <tr><td>TROUVE
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>TW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UCONV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UNLADM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UNORM
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
!>          <tr><td>VCONV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VITCD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VITCE
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
!>          <tr><td>ZREF
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE INIT_TRANSPORT
     &(TROUVE,DEBU,HIDING,NSICLA,NPOIN,
     & T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T14,
     & CHARR,QS_C,QSXC,QSYC,CALFA,SALFA,COEFPN,SLOPEFF,
     & SUSP,QS_S,QS,QSCL,QSCL_C,QSCL_S,QSCLXS,QSCLYS,
     & UNORM,U2D,V2D,HN,CF,MU,TOB,TOBW,UW,TW,THETAW,FW,HOULE,
     & AVAIL,ACLADM,UNLADM,KSP,KSR,KS,
     & ICF,HIDFAC,XMVS,XMVE,GRAV,VCE,XKV,HMIN,KARMAN,
     & ZERO,PI,AC,IMP_INFLOW_C,ZREF,ICQ,CSTAEQ,
     & CMAX,CS,CS0,UCONV,VCONV,CORR_CONV,SECCURRENT,BIJK,
     & IELMT,MESH,FDM,XWC,FD90,SEDCO,VITCE,PARTHENIADES,VITCD,
!RK
     * U3D,V3D,CODE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AC             |---| 
C| ACLADM         |---| 
C| AVAIL          |---| 
C| BIJK           |---| 
C| CALFA          |---| 
C| CF             |---| 
C| CHARR          |-->| 
C| CMAX           |---| 
C| COEFPN         |---| 
C| CORR_CONV      |---| 
C| CS             |---| 
C| CS0            |---| 
C| CSTAEQ         |---| 
C| DEBU           |-->| 
C| FD90           |---| 
C| FDM            |---| 
C| FW             |---| 
C| GRAV           |---| 
C| HIDFAC         |---| 
C| HIDING         |-->| 
C| HMIN           |---| 
C| HN             |-->| WATER DEPTH
C| HOULE          |---| 
C| ICF            |---| 
C| ICQ            |---| 
C| IELMT          |---| 
C| IMP_INFLOW_C   |---| 
C| KARMAN         |---| 
C| KSP            |---| 
C| KSR            |---|
C| KS             |---| 
C| MESH           |---| 
C| MU             |---| 
C| NPOIN          |---| 
C| NSICLA         |-->| NUMBER OF SEDIMENT CLASSES
C| PARTHENIADES   |---| 
C| PI             |---| 
C| QS             |---| 
C| QSCL           |---| 
C| QSCLXS         |---| 
C| QSCLYS         |---| 
C| QSCL_C         |---| 
C| QSCL_S         |---| 
C| QSXC           |---| 
C| QSYC           |---| 
C| QS_C           |---| 
C| QS_S           |---| 
C| SALFA          |---| 
C| SECCURRENT     |---| 
C| SEDCO          |---| 
C| SLOPEFF        |---| 
C| SUSP           |---| 
C| T1             |---| 
C| T10            |---| 
C| T11            |---| 
C| T12            |---| 
C| T14            |---| 
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
C| TROUVE         |-->| 
C| TW             |---| 
C| U2D            |---| 
C| UCONV          |---| 
C| UNLADM         |---| 
C| UNORM          |---| 
C| UW             |---| 
C| V2D            |---| 
C| VCE            |---| 
C| VCONV          |---| 
C| VITCD          |---| 
C| VITCE          |---| 
C| XKV            |---| 
C| XMVE           |---| 
C| XMVS           |---| 
C| XWC            |---| 
C| ZERO           |---| 
C| ZREF           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_SISYPHE, EX_INIT_TRANSPORT => INIT_TRANSPORT
C
      USE DECLARATIONS_SISYPHE, ONLY : NOMBLAY
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)              :: NSICLA,NPOIN,TROUVE(*),ICQ
      INTEGER, INTENT(IN)              :: ICF,HIDFAC,IELMT,SLOPEFF
      LOGICAL, INTENT(IN)              :: CHARR,DEBU,SUSP,IMP_INFLOW_C
      LOGICAL, INTENT(IN)              :: CORR_CONV,SECCURRENT,SEDCO(*)
      LOGICAL, INTENT(IN)              :: HOULE
      TYPE(BIEF_OBJ),    INTENT(IN)    :: U2D,V2D,UNORM,HN,CF
      TYPE(BIEF_OBJ),    INTENT(IN)    :: MU,TOB,TOBW,UW,TW,THETAW,FW
      TYPE(BIEF_OBJ),    INTENT(IN)    :: ACLADM,UNLADM,KSP,KSR,KS
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: HIDING
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: QS_C, QSXC, QSYC, CALFA,SALFA
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: T1,T2,T3,T4,T5,T6,T7,T8
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: T9,T10,T11,T12,T14
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: ZREF,CSTAEQ,CS,UCONV,VCONV
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: QS_S,QS,QSCL_C,QSCL_S
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: COEFPN
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: QSCLXS,QSCLYS,QSCL
      TYPE(BIEF_MESH),   INTENT(INOUT) :: MESH
      DOUBLE PRECISION,  INTENT(IN)    :: XMVS,XMVE,GRAV,VCE
      DOUBLE PRECISION,  INTENT(IN)    :: XKV,HMIN,KARMAN,ZERO,PI
      DOUBLE PRECISION,  INTENT(IN)    :: PARTHENIADES,BIJK,XWC(NSICLA)
      DOUBLE PRECISION,  INTENT(IN)    :: FD90(NSICLA),CS0(NSICLA)
      DOUBLE PRECISION,  INTENT(IN)    :: VITCE,VITCD
      DOUBLE PRECISION,  INTENT(INOUT) :: AC(NSICLA),CMAX,FDM(NSICLA)
      DOUBLE PRECISION,  INTENT(INOUT) :: AVAIL(NPOIN,10,NSICLA)
C
!RK
      TYPE(BIEF_OBJ),    INTENT(IN)    :: U3D,V3D
      CHARACTER(LEN=24), INTENT(IN)    :: CODE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,J
      DOUBLE PRECISION AT0,AAA,USTARP
C
      DOUBLE PRECISION U3DNORM     
!
!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
C --- START : INITIALISES RATE OF TRANSPORT AND SUSPENSION
!
C     FOR INITIALISATION : SLOPE EFFECT AND DEVIATION ARE CANCELLED
!RK in case of coupling with T3D, the direction should come from the
! bottom velocity
      IF(CODE(1:9).EQ.'TELEMAC3D') THEN
        DO I=1,NPOIN
          u3dnorm=SQRT(U3D%R(I)*U3D%R(I)+V3D%R(I)*V3D%R(I))
          IF(U3DNORM.GE.1.D-12) THEN
            CALFA%R(I)=U3D%R(I)/u3dnorm
            SALFA%R(I)=V3D%R(I)/u3dnorm
          ELSE
            CALFA%R(I)=1.D0
            SALFA%R(I)=0.D0
          ENDIF
        ENDDO
      ELSE
        CALL OS('X=Y/Z   ',CALFA, U2D, UNORM, 0.D0, 2, 1.D0, 1.D-12)
        CALL OS('X=Y/Z   ',SALFA, V2D, UNORM, 0.D0, 2, 0.D0, 1.D-12)
      ENDIF
C
C  appel a effpnt ?
C
      CALL OS('X=C     ',X=COEFPN,C=1.D0)
C
      IF(CHARR) THEN
C
          CALL OS('X=C     ',X=HIDING,C=1.D0)
C
          DO I = 1, NSICLA
C
            IF(SEDCO(I)) THEN
C             IF COHESIVE: NO BEDLOAD TRANSPORT
              CALL OS('X=0     ', QSCL_C%ADR(I)%P)
            ELSE
C             IF NON COHESIVE
              CALL BEDLOAD_FORMULA
     &        (U2D,V2D,UNORM,HN,CF,MU,TOB,TOBW,UW,TW,THETAW,FW,
     &        ACLADM, UNLADM,KSP,KSR,AVAIL(1:NPOIN,1,I),
     &        NPOIN,ICF,HIDFAC,XMVS,XMVE,
     &        FDM(I),GRAV,VCE,XKV,HMIN,XWC(I),FD90(I),KARMAN,ZERO,
     &        PI,SUSP,AC(I),HIDING,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,
     &        T11,T12,QSCL_C%ADR(I)%P,QSCL_S%ADR(I)%P,
     &        IELMT,SECCURRENT,SLOPEFF,COEFPN,BIJK,HOULE)
              CALL OS('X=CX    ', X=QSCL_C%ADR(I)%P,C=1.D0/XKV)
            ENDIF
C           SUM ON ALL CLASSES
            DO J=1,NPOIN
              QS_C%R(J) = QS_C%R(J) + QSCL_C%ADR(I)%P%R(J)
            ENDDO
C
          ENDDO
C
C         COMPUTES THE X AND Y COMPONENTS OF TRANSPORT
C
          CALL OS('X=YZ    ', X=QSXC, Y=QS_C, Z=CALFA)
          CALL OS('X=YZ    ', X=QSYC, Y=QS_C, Z=SALFA)
C
      ENDIF
C
C ... START : COMPUTES THE SUSPENDED TRANSPORT
C
      IF(SUSP) THEN
C
C       COMPUTES THE INITIAL CONCENTRATIONS
C
C       FOR RANK IN TROUVE SEE POINT_SISYPHE, NOMVAR_SISYPHE
C       AND ALIRE IN SISYPHE.F (IT IS THE ADDRESS OF CONCENTRATIONS)
CV errror remplacer par 22?? Error in 
       IF(.NOT.DEBU.OR.(TROUVE(22+(NOMBLAY+1)*NSICLA).EQ.0)) THEN
C
          CALL CONDIM_SUSP(CS,CS0,NSICLA,MESH%X%R,MESH%Y%R,AT0,NPOIN)
C
C INITIALISES ZREF
C START MODIFICATIONS (CV) ...
          IF(ICQ.EQ.1) THEN
                  CALL OS('X=Y     ', X=ZREF, Y=KSP)
            ELSEIF(ICQ.EQ.2) THEN
                  CALL OS('X=Y     ', X=ZREF, Y=KSR)
            ELSEIF(ICQ.EQ.3) THEN
                  CALL OS('X=Y     ', X=ZREF, Y=KS)
          ENDIF
C ...END MODIFICATIONS (CV)
C
C         OPTION: IMPOSED INFLOW CONCENTRATIONS ...
C
          IF(IMP_INFLOW_C) THEN
!
C           TAUP IN T8
            CALL OS('X=CYZ   ', X=T8, Y=TOB, Z=MU, C=1.D0)
C           USTAR (TOTAL) IN T9
            CALL OS('X=CY    ', X=T9, Y=TOB, C=1.D0/XMVE)
            CALL OS('X=SQR(Y)', X=T9, Y=T9)
!
C           START: LOOP ON THE CLASSES
!
            DO I=1,NSICLA
!
              IF(.NOT.SEDCO(I)) THEN
!
C             NON COHESIVE SED: INITIALISES CSTAEQ
!
                IF(ICQ.EQ.1) THEN
CV                  CALL OS('X=Y     ', X=ZREF, Y=KS)
                  CALL SUSPENSION_FREDSOE(ACLADM,T8,NPOIN,
     &                GRAV, XMVE, XMVS, ZERO, AC(I),  CSTAEQ )
                ELSEIF(ICQ.EQ.2) THEN
CV                  CALL OS('X=Y     ', X=ZREF, Y=KSR)
                  CALL SUSPENSION_BIJKER(T8,HN,NPOIN,CHARR,QS_C,ZREF,
     &                                   ZERO,HMIN,CSTAEQ,XMVE)
              ELSEIF(ICQ.EQ.3) THEN
                  CALL OS('X=Y     ', X=ZREF, Y=KS)
                CALL SUSPENSION_VANRIJN(ACLADM,T8,NPOIN,
     &                         GRAV,XMVE,XMVS,ZERO,AC(I),CSTAEQ,ZREF)
                ENDIF
!
C            ROUSE CONCENTRATION PROFILE IS ASSUMED BASED ON TOTAL FRICTION
C            VELOCITY
!
             CALL SUSPENSION_ROUSE(T9,HN,NPOIN,
     &                             KARMAN,HMIN,ZERO,XWC(I),ZREF,T12)
!
             DO J=1,NPOIN
               CSTAEQ%R(J)=CSTAEQ%R(J)*AVAIL(J,1,I)
             ENDDO
C            CALL OS( 'X=XY    ',X=CSTAEQ,Y=AVAI%ADR(I)%P)
             CALL OS( 'X=Y/Z   ',X=CS%ADR(I)%P,Y=CSTAEQ,Z=T12)
!
C END NON-COHESIVE
!
              ELSE
!
C               FOR COHESIVE SEDIMENT
!
C               THIS VALUE CAN BE ALSO CHANGED BY THE USER
C               IN SUBROUTINE USER_KRONE_PARTHENIADES
!
                CALL OS('X=Y     ', X=ZREF, Y=KSP)
!
                CMAX = MAX(CMAX,PARTHENIADES/XWC(I))
!
                IF(VITCE.GT.1.D-8.AND.VITCD.GT.1.D-8) THEN
                  DO J = 1, NPOIN
C FLUER
                  USTARP= SQRT(T8%R(J)/XMVE)
                  AAA= PARTHENIADES*
     &                MAX(((USTARP/VITCE)**2-1.D0),ZERO)
C FLUDPT
C                 BBB=XWC(I)*MAX((1.D0-(USTARP/VITCD)**2),ZERO)
C                 IF NO DEPOSITION, THE EQUILIBRIUM CONCENTRATION IS INFINITE!
                  CS%ADR(I)%P%R(J) = AAA/XWC(I)
!
                  ENDDO
                ELSE
                  CALL OS('X=0     ',X=CS%ADR(I)%P)
                ENDIF
!
C CV : 13/11/09
                DO J=1,NPOIN
                  CS%ADR(I)%P%R(J)=CS%ADR(I)%P%R(J)*AVAIL(J,1,I)
                ENDDO
C               CALL OS('X=XY    ',X=CS%ADR(I)%P,Y=AVAI%ADR(I)%P)
!
C END COHESIVE
!
              ENDIF
!
C END OF LOOP ON THE CLASSES
!
            ENDDO
!
C END OF OPTION: IMPOSED INFLOW CONCENTRATION
!
          ENDIF
!
C END OF ?
!
        ENDIF
C
C COMPUTES SUSPENDED TRANSPORT
C
        DO I=1,NPOIN
          UCONV%R(I) = U2D%R(I)
          VCONV%R(I) = V2D%R(I)
        ENDDO
!
        DO I=1,NSICLA
          IF(CORR_CONV.AND.(.NOT.SEDCO(I))) THEN
            CALL SUSPENSION_CONV( TOB, XMVE, CF,NPOIN,ZREF,U2D,V2D,HN,
     &                    HMIN,UCONV,VCONV,KARMAN,ZERO,XWC(I),T12,T14)
          ENDIF
C
          CALL OS('X=YZ    ',X=T11,Y=UCONV, Z=HN)
          CALL OS('X=YZ    ',X=T12,Y=VCONV, Z=HN)
C
          CALL OS('X=YZ    ',X=QSCLXS%ADR(I)%P,Y=CS%ADR(I)%P,Z=T11)
          CALL OS('X=YZ    ',X=QSCLYS%ADR(I)%P,Y=CS%ADR(I)%P,Z=T12)
C
          CALL OS('X=N(Y,Z) ',X=QSCL_S%ADR(I)%P,
     &            Y=QSCLXS%ADR(I)%P, Z=QSCLYS%ADR(I)%P)
C
          DO J=1,NPOIN
            QS_S%R(J) = QS_S%R(J) + QSCL_S%ADR(I)%P%R(J)
          ENDDO
        ENDDO
C
C     IF(SUSP) THEN
      ENDIF
C
C     COMPUTES THE TRANSPORT FOR EACH CLASS (IF NOT RESTART OR IF
C                                              DATA NOT FOUND)
C CV bug : +2
      DO I=1, NSICLA
        IF(.NOT.DEBU.OR.TROUVE(I+22+NOMBLAY*NSICLA).EQ.0) THEN
          IF(DEBU.AND.  TROUVE(I+22+NOMBLAY*NSICLA).EQ.0) THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'QSCL REINITIALISE DANS INIT_TRANSPORT'
              WRITE(LU,*) 'POUR LA CLASSE ',I
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'QSCL REINITIALISED IN INIT_TRANSPORT'
              WRITE(LU,*) 'FOR CLASS ',I
            ENDIF
          ENDIF
          IF(CHARR.AND.SUSP) THEN
            CALL OS('X=Y+Z   ', X=QSCL%ADR(I)%P,
     &              Y=QSCL_S%ADR(I)%P, Z=QSCL_C%ADR(I)%P)
          ELSEIF(CHARR) THEN
            CALL OS('X=Y     ',X=QSCL%ADR(I)%P,Y=QSCL_C%ADR(I)%P)
          ELSEIF(SUSP) THEN
            CALL OS('X=Y     ',X=QSCL%ADR(I)%P,Y=QSCL_S%ADR(I)%P)
          ENDIF
        ENDIF
      ENDDO
C
C     COMPUTES TOTAL TRANSPORT QS (IF NOT RESTART OR IF QS NOT FOUND)
C
      IF(.NOT.DEBU.OR.TROUVE(15).EQ.0) THEN
        IF(DEBU.AND.  TROUVE(15).EQ.0) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'QS REINITIALISE DANS INIT_TRANSPORT'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'QS REINITIALISED IN INIT_TRANSPORT'
          ENDIF
        ENDIF
        IF(CHARR.AND.SUSP) THEN
          CALL OS('X=Y+Z   ',X=QS,Y=QS_C,Z=QS_S)
        ELSEIF(CHARR) THEN
          CALL OS('X=Y     ',X=QS,Y=QS_C)
        ELSEIF(SUSP) THEN
          CALL OS('X=Y     ',X=QS,Y=QS_S)
        ENDIF
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
