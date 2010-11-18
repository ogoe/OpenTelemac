C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FLUX OF DEPOSITION AND EROSION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AC, ACLADM, AVA, CHARR, CSTAEQ, DEBUG, FLUER, GRAV, HMIN, HN, ICQ, NPOIN, QSC, TAUP, XMVE, XMVS, XWC, ZERO, ZREF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SUSPENSION_EROSION
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), SUSPENSION_BIJKER(), SUSPENSION_FREDSOE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SUSPENSION_COMPUTATION()

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
!> </td><td> 04/05/2010
!> </td><td> CV
!> </td><td> MODIFICATION FOR FREDSOE: EQUILIBRIUM CONCENTRATIONS
!>           MUST BE MULTIPLIED BY AVAI: OTHERWISE COMPUTATION OF
!>           INFLOW CONCENTRATIONS IS INCORRECT (I.E. DOES NOT
!>           TAKE AVAI INTO ACCOUNT)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 17/09/2009
!> </td><td> J-M HERVOUET + C VILLARET
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
!>          <tr><td>CF
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>CHARR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CSTAEQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DEBUG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUER
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HMIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>HAUTEUR D'EAU
!>    </td></tr>
!>          <tr><td>ICQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAUP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOB
!></td><td>--></td><td>FLUX DE DEPOT                               C
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
        SUBROUTINE SUSPENSION_EROSION  !
     &(TAUP,HN,ACLADM,AVA,NPOIN,CHARR,XMVE,XMVS,GRAV,HMIN,XWC,
     & ZERO,ZREF,AC,FLUER,CSTAEQ,QSC,ICQ,DEBUG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AC             |---| 
C| ACLADM         |---| 
C| AVA            |---| 
C| CF             |-->| 
C| CHARR          |---| 
C| CSTAEQ         |---| 
C| DEBUG          |---| 
C| FLUER          |---| 
C| GRAV           |---| 
C| HMIN           |---| 
C| HN             |-->| HAUTEUR D'EAU
C| ICQ            |---| 
C| NPOIN          |---| 
C| QSC            |---| 
C| TAUP           |---| 
C| TOB            |-->| FLUX DE DEPOT                               C
C| XMVE           |---| 
C| XMVS           |---| 
C| XWC            |---| 
C| ZERO           |---| 
C| ZREF           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE, EX_SUSPENSION_EROSION=>SUSPENSION_EROSION
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE (BIEF_OBJ),  INTENT(IN)    :: TAUP,HN,ACLADM,ZREF,QSC
      INTEGER,          INTENT(IN)    :: NPOIN,DEBUG,ICQ
      LOGICAL,          INTENT(IN)    :: CHARR
      DOUBLE PRECISION, INTENT(IN)    :: XMVE,XMVS,GRAV,HMIN,XWC,ZERO
      DOUBLE PRECISION, INTENT(IN)    :: AVA(NPOIN)
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUER,CSTAEQ
      DOUBLE PRECISION, INTENT(INOUT) :: AC

      ! 3/ LOCAL VARIABLES
      ! -------------------

      INTEGER I

!
!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!
C  COMPUTES THE NEAR BED EQUILIBRIUM CONCENTRATION --> CSTAEQ (MEAN DIAMETER)
!
      IF(ICQ.EQ.1) THEN
C
        IF(DEBUG > 0) WRITE(LU,*) 'SUSPENSION_FREDSOE'
        CALL SUSPENSION_FREDSOE(ACLADM,TAUP,NPOIN,
     &                          GRAV,XMVE,XMVS,ZERO,AC,CSTAEQ)
        IF(DEBUG > 0) WRITE(LU,*) 'END SUSPENSION_FREDSOE'
C
C       CALL OS('X=CYZ   ', X=FLUER, Y=CSTAEQ, Z=AVA, C=XWC)
C 04/05/2010
C START OF CV MODIFICATIONS ...
CV        DO I=1,NPOIN
CV          FLUER%R(I)=XWC*CSTAEQ%R(I)*AVA(I)
CV        ENDDO
          DO I=1,NPOIN
            CSTAEQ%R(I)=CSTAEQ%R(I)*AVA(I)
          ENDDO
          CALL OS('X=CY    ', X=FLUER, Y=CSTAEQ, C=XWC)
C ... END OF CV MODIFICATIONS
C
      ELSEIF(ICQ.EQ.2) THEN
C
        IF(DEBUG > 0) WRITE(LU,*) 'SUSPENSION_BIJKER'
        CALL SUSPENSION_BIJKER(TAUP,HN,NPOIN,CHARR,QSC,ZREF,
     &                         ZERO,HMIN,CSTAEQ,XMVE)
        IF(DEBUG > 0) WRITE(LU,*) 'END SUSPENSION_BIJKER'
C       NO MULTIPLICATION BY AVA BECAUSE AVA HAS ALREADY BEEN TAKEN
C       INTO ACCOUNT IN THE BEDLOAD TRANSPORT RATE
        CALL OS('X=CY    ', X=FLUER, Y=CSTAEQ, C=XWC)
C
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