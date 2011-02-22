C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES EXNER EQUATION WITH THE FINITE VOLUME METHOD.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BREACH, DT, EBOR, FLUX, KENT, KSORT, LIEBOR, MESH, NPOIN, NPTFR, NSEG, QSX, QSY, T10, UNSV2D, ZFCL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IEL, IEL1, IEL2, ISEGIN, K, QSMOY1, QSMOY2, QSP, RNORM, VNOIN1, VNOIN2, XN, YN
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_BEDLOAD_SOLVS_VF
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), PARCOM()
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
!>      <td><center>                                           </center>
!> </td><td> 15/09/2009
!> </td><td> JMH
!> </td><td> KENT KSORT ADDED (WERE HARD-CODED BEFORE !!!)
!> </td></tr>
!>      <tr>
!>      <td><center> 5.8                                       </center>
!> </td><td> 30/10/2007
!> </td><td> J-M HERVOUET
!> </td><td> UNSV2D +DIRICL DELETED
!> </td></tr>
!>      <tr>
!>      <td><center> 5.5                                       </center>
!> </td><td> 14/09/2004
!> </td><td> F. HUVELIN
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.5                                       </center>
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
!>          <tr><td>BREACH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>EBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KENT
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
!>          <tr><td>QSX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T10
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UNSV2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZFCL
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE BEDLOAD_SOLVS_VF !
     &(MESH,QSX,QSY,LIEBOR,UNSV2D,EBOR,BREACH,NSEG,NPTFR,
     & NPOIN,KENT,KSORT,DT,T10,ZFCL,FLUX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BREACH         |---| 
C| DT             |---| 
C| EBOR           |---| 
C| FLUX           |---| 
C| KENT           |---| 
C| KSORT          |---| 
C| LIEBOR         |---| 
C| MESH           |---| 
C| NPOIN          |---| 
C| NPTFR          |---| 
C| NSEG           |---| 
C| QSX            |---| 
C| QSY            |---| 
C| T10            |---| 
C| UNSV2D         |---| 
C| ZFCL           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE, EX_BEDLOAD_SOLVS_VF => BEDLOAD_SOLVS_VF
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU


      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_MESH),  INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ),   INTENT(IN)    :: QSX, QSY
      TYPE(BIEF_OBJ),   INTENT(IN)    :: LIEBOR,UNSV2D, EBOR
      TYPE(BIEF_OBJ),   INTENT(IN)    :: BREACH
      INTEGER,          INTENT(IN)    :: NSEG,NPTFR,NPOIN,KENT,KSORT
      DOUBLE PRECISION, INTENT(IN)    :: DT
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: T10
      TYPE(BIEF_OBJ),   INTENT(INOUT)   :: ZFCL, FLUX


      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER          :: ISEGIN, K
      INTEGER          :: IEL, IEL1, IEL2
      DOUBLE PRECISION :: QSMOY1, QSMOY2
      DOUBLE PRECISION :: QSP
      DOUBLE PRECISION :: VNOIN1, VNOIN2, RNORM
      DOUBLE PRECISION :: XN, YN

!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!

      ! ***************** !
      ! I - INTIALISES !
      ! ***************** !

      CALL OS('X=0     ', X=FLUX)

      ! ************************************************* !
      ! II - DETERMINES THE OUTGOING FLUX FOR EACH CELL   !
      ! ************************************************* !

      DO ISEGIN = 1, NSEG

         IEL1 = MESH%NUBO%I(2*ISEGIN - 1)
         IEL2 = MESH%NUBO%I(2*ISEGIN    )

         ! II.1 - RNORM : SEGMENT LENGTH
         ! ----------------------------------
         VNOIN1 = MESH%VNOIN%R(3*ISEGIN - 2)
         VNOIN2 = MESH%VNOIN%R(3*ISEGIN - 1)
         RNORM  = MESH%VNOIN%R(3*ISEGIN    )

         ! II.2 - QS FOR THE SEGMENT, BROKEN UP ACCORDING TO X AND Y
         ! ---------------------------------------------
         QSMOY1 = 0.5D0*(QSX%R(IEL1) + QSX%R(IEL2))
         QSMOY2 = 0.5D0*(QSY%R(IEL1) + QSY%R(IEL2))

         ! II.3 - PROJECTS QS FOR THE SEGMENT ONTO THE SEGMENT NORMAL
         ! ------------------------------------------------------------
         QSP = VNOIN1*QSMOY1 + VNOIN2*QSMOY2

         ! II.4 - UPWIND SCHEME ON NODES WITH A "PROBLEM"
         ! ----------------------------------------------
         IF(BREACH%I(IEL1).EQ.1.AND.QSP.GT.0.D0) THEN
           QSMOY1 = QSX%R(IEL1)
           QSMOY2 = QSY%R(IEL1)
         ENDIF
         IF(BREACH%I(IEL2).EQ.1.AND.QSP.LT.0.D0) THEN
           QSMOY1 = QSX%R(IEL2)
           QSMOY2 = QSY%R(IEL2)
         ENDIF

         QSP = VNOIN1*QSMOY1 + VNOIN2*QSMOY2

         ! II.5 - INTEGRATES BY THE SEGMENT LENGTH
         ! ---------------------------------------------
         FLUX%R(IEL1) = FLUX%R(IEL1) + RNORM*QSP
         FLUX%R(IEL2) = FLUX%R(IEL2) - RNORM*QSP

      ENDDO

      ! ******************************* !
      ! III - BOUNDARIES                ! (_IMP_)
      ! ******************************* !
      DO K = 1 , NPTFR

         IEL = MESH%NBOR%I(K)

         ! III.1 - FREE EVOLUTION: SEDIMENTS ARE FREE TO LEAVE
         ! --------------------------------------------------------
         IF (LIEBOR%I(K).EQ.KSORT) THEN

            ! XNEBOR (*+NPTFR) AND YNEBOR (*+NPTFR)
            ! CONTAIN THE VECTOR NORMAL TO A BOUNDARY NODE
            ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            XN = MESH%XNEBOR%R(K+NPTFR)
            YN = MESH%YNEBOR%R(K+NPTFR)

            ! ADDS THE CONTRIBUTION OF THE FLUX ON THE BOUNDARY SEGMENT
            ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
            FLUX%R(IEL) = FLUX%R(IEL) + QSX%R(IEL)*XN + QSY%R(IEL)*YN

         ENDIF

         ! III.2 - FOR A SOLID BOUNDARY: NOTHING TO PROGRAM
         !         BECAUSE THE SEDIMENT FLUX IS ZERO HERE
         ! -------------------------------------------------------

      ENDDO

      IF(NCSIZE.GT.1) CALL PARCOM(FLUX, 2, MESH)

      ! ************************** !
      ! IV - SOLVES THE SYSTEM     !
      ! ************************** !

      ! NEGATIVE SIGN BECAUSE OUTGOING FLUX IS POSITIVE
      ! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      CALL OS('X=CYZ   ', X=ZFCL, Y=FLUX, Z=UNSV2D, C=-DT)
!
      DO K=1,NPTFR
        IF(LIEBOR%I(K).EQ.KENT) THEN
          ZFCL%R(MESH%NBOR%I(K)) = EBOR%R(K)
        ENDIF
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