C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       NON ERODABLE METHOD FOR FINITE VOLUMES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AVA, BREACH, DT, ELAY, KSORT, LIEBOR, MESH, NPOIN, NPTFR, NSEG, QS, QSX, QSY, T1, T2, T3, V2DPAR
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
!>    </th><td> I, IEL, IEL1, IEL2, ISEGIN, K, QSP1, QSP2, QSPC, RNORM, TEMP, VNOIN1, VNOIN2, XN, YN
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BEDLOAD_NERBED_VF
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CPSTVC(), PARCOM()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BEDLOAD_EVOL()

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
!> </td><td> 31/01/2008
!> </td><td> JMH
!> </td><td> CORRECTED INITIALISATION ERROR FOR T1 AND T2
!> <br>      KSORT ADDED (WAS HARD-CODED BEFORE !!!!)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 14/09/2004
!> </td><td> F. HUVELIN
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.3                                       </center>
!> </td><td> 07/05/2002
!> </td><td> M. GONZALES DE LINARES
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AVA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BREACH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ELAY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KSORT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LIEBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NSEG
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
!>          <tr><td>T1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V2DPAR
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE BEDLOAD_NERBED_VF !
     &(MESH,LIEBOR,KSORT,ELAY,V2DPAR,QSX,QSY,AVA,NPOIN,NSEG,NPTFR,
     & DT,QS,T1,T2,T3,BREACH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AVA            |---| 
C| BREACH         |---| 
C| DT             |---| 
C| ELAY           |---| 
C| KSORT          |---| 
C| LIEBOR         |---| 
C| MESH           |---| 
C| NPOIN          |---| 
C| NPTFR          |---| 
C| NSEG           |---| 
C| QS             |---| 
C| QSX            |---| 
C| QSY            |---| 
C| T1             |---| 
C| T2             |---| 
C| T3             |---| 
C| V2DPAR         |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE, EX_BEDLOAD_NERBED_VF => BEDLOAD_NERBED_VF
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: LIEBOR
      TYPE(BIEF_OBJ),   INTENT(IN)    :: QSX, QSY
      INTEGER,          INTENT(IN)    :: NPOIN, NSEG, NPTFR,KSORT
      DOUBLE PRECISION, INTENT(IN)    :: DT
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: QS, T1, T2, T3
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: BREACH
      DOUBLE PRECISION, INTENT(IN)    :: ELAY(NPOIN),V2DPAR(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: AVA(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER          :: I, K
      INTEGER          :: IEL, IEL1, IEL2, ISEGIN
      DOUBLE PRECISION :: QSP1, QSP2, QSPC
      DOUBLE PRECISION :: XN, YN, TEMP
      DOUBLE PRECISION :: VNOIN1, VNOIN2, RNORM
!
!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
      ! ****************** !
      ! I - INITIALISES    !
      ! ****************** !
!
      ! BREACH INDICATES IF NON ERODABLE BED WILL BE REACHED
      ! DURING TIME STEP FOR THIS POINT
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
C     GIVE T1 AND T2 THE SAME STRUCTURE AS QS
!
      CALL CPSTVC(QS,T1)
      CALL CPSTVC(QS,T2)
!
      DO IEL = 1, NPOIN
        BREACH%I(IEL) = 0
        T1%R(IEL)=0.D0
        T2%R(IEL)=0.D0
      ENDDO
!
      ! ************************************************* !
      ! II - DETERMINES THE OUTGOING FLUX FOR EACH CELL   ! (_IMP_)
      ! ************************************************* !
      ! THE PRINCIPLE IS THAT QS IS CALCULATED FOR EACH SEGMENT AS
      ! HALF THE SUM OF THE QS AT THE CENTERS OF THE ELEMENTS WHICH
      ! SEGMENT FORMS THE BOUNDARY. IT IS THEN PROJECTED ON THE NORMAL
      ! TO THE SEGMENT, MULTIPLIED BY THE LENGTH OF THE SEGMENT, AND
      ! THIS FLUX TERM IS ADDED (OR SUBTRACTED) TO THE TWO ELEMENTS.
!
      DO ISEGIN = 1, NSEG
!
         IEL1 = MESH%NUBO%I(2*ISEGIN - 1)
         IEL2 = MESH%NUBO%I(2*ISEGIN    )
!
         ! II.1 - SEGMENT LENGTH (RNORM)
         ! ----------------------------------
         VNOIN1 = MESH%VNOIN%R(3*ISEGIN - 2)
         VNOIN2 = MESH%VNOIN%R(3*ISEGIN - 1)
         RNORM  = MESH%VNOIN%R(3*ISEGIN    )
!
         ! II.2 - PROJECTS QS FOR THE SEGMENT ONTO THE SEGMENT NORMAL
         ! ------------------------------------------------------------
         QSP1 = VNOIN1*QSX%R(IEL1) + VNOIN2*QSY%R(IEL1)
         QSP2 = VNOIN1*QSX%R(IEL2) + VNOIN2*QSY%R(IEL2)
         QSPC = (QSP1+QSP2)*0.5D0
!
         ! II.3 - QS SUCH AS THE OUTGOING FLUX IS MAXIMUM
         ! ----------------------------------------------
         T1%R(IEL1) = T1%R(IEL1) + RNORM*MAX(QSPC,QSP1,0.D0)
         T1%R(IEL2) = T1%R(IEL2) - RNORM*MIN(QSPC,QSP2,0.D0)
!
         IF(QSPC > 0.D0) THEN
           T2%R(IEL1) = T2%R(IEL1) + RNORM*QSP1
         ELSEIF(QSPC < 0.D0) THEN
           T2%R(IEL2) = T2%R(IEL2) - RNORM*QSP2
         ENDIF
!
      ENDDO
!
      ! ************************************** !
      ! III - LOOP ON THE BOUNDARY NODES       !
      ! ************************************** !
!
      DO K = 1, NPTFR
         IEL = MESH%NBOR%I(K)
!
         ! III.1 - FREE EVOLUTION: SEDIMENTS ARE FREE TO LEAVE
         ! ---------------------------------------------------------
         IF (LIEBOR%I(K) == KSORT) THEN
!
            ! XNEBOR (*+NPTFR) AND YNEBOR (*+NPTFR)
            ! CONTAIN THE VECTOR NORMAL TO A BOUNDARY NODE
            ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            XN   = MESH%XNEBOR%R(K+NPTFR)
            YN   = MESH%YNEBOR%R(K+NPTFR)
            TEMP = QSX%R(IEL)*XN + QSY%R(IEL)*YN

            IF (TEMP > 0.D0) THEN
               T1%R(IEL) = T1%R(IEL) + TEMP
               T2%R(IEL) = T2%R(IEL) + TEMP
            ENDIF
!
         ENDIF
!
         ! III.2 - FOR A SOLID BOUNDARY: NOTHING TO PROGRAM
         !         BECAUSE THE SEDIMENT FLUX IS ZERO HERE
         !         FOR IMPOSED EVOLUTION : SEE BEDLOAD_SOLVS_VF.F
         ! --------------------------------------------------------
      ENDDO
!
      IF(NCSIZE > 1) THEN
        CALL PARCOM(T1, 2, MESH)
        CALL PARCOM(T2, 2, MESH)
      ENDIF
!
      ! ************************************************ !
      ! IV - COMPUTES THE MAXIMUM FLUX AUTHORISED PER CELL!
      ! ************************************************ !
!
      DO I = 1, NPOIN
!
         T3%R(I)=ELAY(I)*V2DPAR(I)*AVA(I)*(1.D0-1.D-6)/DT
         IF (T3%R(I) < 0.D0) T3%R(I) = 0.D0
!
         ! IF THE OUTGOING FLUX IS TOO LARGE, QS IS CAPPED AT THE NODE
         ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
         IF(T1%R(I) > T3%R(I)) THEN
            BREACH%I(I) = 1
            IF(T2%R(I) > T3%R(I)) THEN
              QS%R(I) = QS%R(I)*T3%R(I)/T2%R(I)
            ENDIF
         ENDIF
!
      ENDDO
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
C
C#######################################################################
C