C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE REFERENCE CONCENTRATION AT Z= 2*D50
!>                ACCORDING TO ZYSERMAN AND FREDSOE FORMULATION (1994).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AC, ACLADM, CSTAEQ, GRAV, NPOIN, TAUP, XMVE, XMVS, ZERO
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX, CMAX, I, TETAP
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SUSPENSION_FREDSOE
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ERODNC(), INIT_TRANSPORT(), SUSPENSION_EROSION(), SUSPENSION_FLUX_MIXTE()

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
!> </td><td> 13/06/2008
!> </td><td> JMH
!> </td><td> FORMULATION OPTIMISED WITH AUX
!> </td></tr>
!>      <tr>
!>      <td><center> 5.6                                       </center>
!> </td><td> 04/01/2005
!> </td><td> F. HUVELIN
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 5.5                                       </center>
!> </td><td> 14/04/2004
!> </td><td> C. VILLARET  01 30 87 83 28
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
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>AVA
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>CF
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>CHARR
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>CSTAEQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUER
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>HCLIP
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>HMIN
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>KSPRATIO
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>TAUP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOB
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>XMVE
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>XMVS
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>ZERO
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE SUSPENSION_FREDSOE !
     &  (ACLADM, TAUP, NPOIN, GRAV,
     &   XMVE, XMVS, ZERO, AC,  CSTAEQ)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AC             |---| 
C| ACLADM         |-->| 
C| AVA            |-->| 
C| CF             |-->| 
C| CHARR          |-->| 
C| CSTAEQ         |---| 
C| FLUER          |---| 
C| GRAV           |-->| 
C| HCLIP          |-->| 
C| HMIN           |-->| 
C| KSPRATIO       |-->| 
C| NPOIN          |-->| 
C| TAUP           |---| 
C| TOB            |-->| 
C| XMVE           |-->| 
C| XMVS           |-->| 
C| ZERO           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE,EX_SUSPENSION_FREDSOE => SUSPENSION_FREDSOE
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU


      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE(BIEF_OBJ),   INTENT(IN)    :: ACLADM, TAUP
      INTEGER,          INTENT(IN)    :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: GRAV, XMVE, XMVS
      DOUBLE PRECISION, INTENT(IN)    :: ZERO,AC
      TYPE(BIEF_OBJ),   INTENT(INOUT) :: CSTAEQ


      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER            :: I
      DOUBLE PRECISION   ::  TETAP,AUX
C
      DOUBLE PRECISION   :: CMAX
C
C     MAXIMUM CONCENTRATION CORRESPONDING TO DENSE PACKING
C
      DATA CMAX/0.6D0/
      INTRINSIC MAX

!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!

      ! ******************************** !
      !    I - CRITICAL SHIELDS PARAMETER!
      ! ******************************** !

      DO I=1,NPOIN

         ! ****************** !
         ! II - SKIN FRICTION !
         ! ****************** !

         TETAP = TAUP%R(I) / (GRAV*(XMVS-XMVE)*ACLADM%R(I))

         ! ***************** !
         ! IV - EROSION FLUX ! (_IMP_)
         ! ***************** !
         ! CONCENTRATION INCREASED BY AVA BECAUSE IT IS COMPUTED
         ! ONLY WITH ONE CLASS OF SEDIMENT (ASSUMPTION)

         IF(TETAP.GT.AC) THEN
           AUX=(TETAP-AC)**1.75D0
           CSTAEQ%R(I) = 0.331D0*AUX/(1.D0+0.72D0*AUX)
           CSTAEQ%R(I) = MIN(CSTAEQ%R(I),CMAX)
         ELSE
           CSTAEQ%R(I) = 0.D0
         ENDIF

      ENDDO

!======================================================================!
!======================================================================!

      RETURN
      END SUBROUTINE SUSPENSION_FREDSOE
C
C#######################################################################
C