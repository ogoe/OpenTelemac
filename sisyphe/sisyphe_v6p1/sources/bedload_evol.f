C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE EVOLUTION FOR THE BEDLOAD TRANSPORT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ACLADM, AVA, BREACH, CALFA, COEFPN, CONST_ALAYER, D90, DEBUG, DM, DTS, EBOR, ELAY, ELAY0, ENTET, GRAV, HMIN, HN, IELMT, KDDL, KDIR, KENT, LCONDIS, LIMTEC, LOADMETH, LS0, MASK, MASKEL, MESH, MSK, NPOIN, NPTFR, Q, QS, QSX, QSY, S, SALFA, SLOPEFF, T1, T10, T11, T12, T13, T2, T3, T4, T5, T6, T7, T8, T9, UNSV2D, V2DPAR, VCE, VF, XMVE, XMVS, ZFCL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> J
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BEDLOAD_EVOL
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> BEDLOAD_NERBED_VF(), BEDLOAD_SOLVS_FE(), BEDLOAD_SOLVS_VF(), OS()
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
!>          <tr><td>ACLADM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AVA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BREACH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CALFA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>COEFPN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CONST_ALAYER
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>D90
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEBUG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DTS
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
!>          <tr><td>GRAV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HMIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELMT
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
!>          <tr><td>LCONDIS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIMTEC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LOADMETH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LS0
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Q
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>S
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SALFA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SLOPEFF
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
!>          <tr><td>UNSV2D
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
!>          <tr><td>XMVE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMVS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZFCL
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE BEDLOAD_EVOL !
     &(HN,Q,S,ELAY,ACLADM, AVA,COEFPN,CALFA,SALFA,LIMTEC,EBOR,
     & MASKEL,MASK,V2DPAR,UNSV2D,DEBUG,NPOIN,NPTFR,
     & IELMT,KENT,KDIR,KDDL,LOADMETH,
     & DTS,DM,D90,HMIN,LS0,GRAV,XMVS,XMVE,VCE,
     & VF,ENTET,MSK,CONST_ALAYER,LCONDIS,MESH,
     & QS,T1, T2, T3, T4, T5, T6, T7, T8, T9,
     & T10, T11, T12, T13, ELAY0, BREACH, QSX, QSY, ZFCL,SLOPEFF)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ACLADM         |---| 
C| AVA            |---| 
C| BREACH         |---| 
C| CALFA          |---| 
C| COEFPN         |---| 
C| CONST_ALAYER   |---| 
C| D90            |---| 
C| DEBUG          |---| 
C| DM             |---| 
C| DTS            |---| 
C| EBOR           |---| 
C| ELAY           |---| 
C| ELAY0          |---| 
C| ENTET          |---| 
C| GRAV           |---| 
C| HMIN           |---| 
C| HN             |---| 
C| IELMT          |---| 
C| KDDL           |---| 
C| KDIR           |---| 
C| KENT           |---| 
C| LCONDIS        |---| 
C| LIMTEC         |---| 
C| LOADMETH       |---| 
C| LS0            |---| 
C| MASK           |---| 
C| MASKEL         |---| 
C| MESH           |---| 
C| MSK            |---| 
C| NPOIN          |---| 
C| NPTFR          |---| 
C| Q             |---| 
C| QS             |---| 
C| QSX            |---| 
C| QSY            |---| 
C| S             |---| 
C| SALFA          |---| 
C| SLOPEFF        |---| 
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
C| UNSV2D         |---| 
C| V2DPAR         |---| 
C| VCE            |---| 
C| VF             |---| 
C| XMVE           |---| 
C| XMVS           |---| 
C| ZFCL           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE, EX_BEDLOAD_EVOL => BEDLOAD_EVOL
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU


      ! 2/ GLOBAL VARIABLES
      ! -------------------

      TYPE(BIEF_OBJ),   INTENT(IN)    :: HN,Q,S,UNSV2D,ELAY,ACLADM
      TYPE(BIEF_OBJ),   INTENT(IN)    :: COEFPN,CALFA,SALFA,LIMTEC
      TYPE(BIEF_OBJ),   INTENT(IN)    :: MASKEL,MASK,V2DPAR
      INTEGER,          INTENT(IN)    :: DEBUG,SLOPEFF,NPOIN,NPTFR
      INTEGER,          INTENT(IN)    :: IELMT,KENT,KDIR,LOADMETH,KDDL
      DOUBLE PRECISION, INTENT(IN)    :: DTS,DM,D90,HMIN,LS0
      DOUBLE PRECISION, INTENT(IN)    :: GRAV,XMVS,XMVE,VCE,AVA(NPOIN)
      LOGICAL,          INTENT(IN)    :: VF,ENTET,MSK
      LOGICAL,          INTENT(IN)    :: CONST_ALAYER,LCONDIS
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QS,EBOR
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T1, T2, T3, T4, T5, T6, T7
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T8, T9, T10, T11, T12, T13
      DOUBLE PRECISION, INTENT(INOUT) :: ELAY0
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: BREACH, QSX, QSY, ZFCL


      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER          :: J


!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!

      ! **************** !
      ! I - SLOPE EFFECT !
      ! **************** !
        IF (SLOPEFF == 1) THEN
          CALL OS('X=XY    ', X=QS , Y=COEFPN)
        ENDIF
        CALL OS('X=YZ    ', X=QSX, Y=QS, Z=CALFA)
        CALL OS('X=YZ    ', X=QSY, Y=QS, Z=SALFA)

      ! ************************************* !
      ! II - TREATMENT OF NON ERODABLE BOTTOM !
      ! ************************************* !
      IF(DEBUG > 0) WRITE(LU,*) 'BEDLOAD_NERBED_VF'
      IF(VF) THEN
        CALL BEDLOAD_NERBED_VF
     &        (MESH,LIMTEC,KDDL,ELAY%R,V2DPAR%R,QSX,QSY,AVA,NPOIN,
     &         MESH%NSEG, NPTFR, DTS, QS, T1, T2, T3, BREACH)
        CALL OS('X=YZ    ', X=QSX, Y=QS, Z=CALFA)
        CALL OS('X=YZ    ', X=QSY, Y=QS, Z=SALFA)
      ENDIF
      IF(DEBUG > 0) WRITE(LU,*) 'BEDLOAD_NERBED_VF'

      ! ***************************************************** !
      ! IVA - SOLVES THE BED-EVOLUTION EQUATION : F.V.        !
      ! ***************************************************** !
      IF (VF) THEN
         IF(DEBUG > 0) WRITE(LU,*) 'BEDLOAD_SOLVS_VF'

         CALL BEDLOAD_SOLVS_VF
     &        (MESH, QSX, QSY, LIMTEC,UNSV2D, EBOR, BREACH,
     &         MESH%NSEG,NPTFR,NPOIN,KDIR,KDDL,DTS,T10,ZFCL,T11)
         IF(DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_SOLVS_VF'

      ! ****************************************************** !
      ! IVB - SOLVES THE BED-EVOLUTION EQUATION  : F.E.        !
      ! ****************************************************** !
      ELSE
         IF(DEBUG > 0) WRITE(LU,*) 'BEDLOAD_SOLVS_FE'
         DO J=1,NPOIN
           T13%R(J)=AVA(J)*ELAY%R(J)
         ENDDO
         CALL BEDLOAD_SOLVS_FE
     &        (MESH,S,EBOR,MASKEL,MASK,
     &         QSX,QSY,IELMT,NPOIN,NPTFR,KENT,KDIR,
     &         LIMTEC,DTS,MSK,ENTET,T1,T2,T3,T4,T8,
     &         ZFCL,T12,T13,MESH%GLOSEG%I,
     &         MESH%GLOSEG%DIM1,MESH%MSEG%X,
     &         MESH%MSEG%X%R(MESH%NSEG+1:2*MESH%NSEG),
     &         MESH%NSEG,UNSV2D)
         IF(DEBUG > 0) WRITE(LU,*) 'END_BEDLOAD_SOLVS_FE'
      ENDIF

      ! ************************************ !
      ! V - VARIABLE ACTIVE LAYER THICKNESS  !
      ! ************************************ !

C     IF(.NOT.CONST_ALAYER.OR.LCONDIS) THEN
C       DO J = 1,NPOIN
C         IF(.NOT.CONST_ALAYER) THEN
C           ELAY0 = MAX(ABS(ZFCL%R(J)),3.D0*ACLADM%R(J))
C         ENDIF
C         IF((QS%R(J)
C    &          (ABS(ZFCL%R(J)/(ELAY0*DTS))
C               ZFCL%R(J) = 0.D0
C         ENDIF
C       ENDDO
C     ENDIF

!======================================================================!
!======================================================================!

      RETURN
      END
C
C#######################################################################
C
