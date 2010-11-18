C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PREPARES THE VARIABLES WHICH WILL BE WRITTEN TO
!>                THE RESULTS FILE OR TO THE LISTING.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AAT, LLT
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_SISYPHE :<br>
!> @link DECLARATIONS_SISYPHE::HN HN@endlink, 
!> @link DECLARATIONS_SISYPHE::KS KS@endlink, 
!> @link DECLARATIONS_SISYPHE::LEOPR LEOPR@endlink, 
!> @link DECLARATIONS_SISYPHE::LISPR LISPR@endlink, 
!> @link DECLARATIONS_SISYPHE::NPOIN NPOIN@endlink, 
!> @link DECLARATIONS_SISYPHE::PTINIG PTINIG@endlink, 
!> @link DECLARATIONS_SISYPHE::PTINIL PTINIL@endlink, 
!> @link DECLARATIONS_SISYPHE::QU QU@endlink, 
!> @link DECLARATIONS_SISYPHE::SORIMP SORIMP@endlink, 
!> @link DECLARATIONS_SISYPHE::SORLEO SORLEO@endlink, 
!> @link DECLARATIONS_SISYPHE::U2D U2D@endlink, 
!> @link DECLARATIONS_SISYPHE::V2D V2D@endlink, 
!> @link DECLARATIONS_SISYPHE::Z Z@endlink, 
!> @link DECLARATIONS_SISYPHE::ZF ZF@endlink
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, IMP, LEO, LTT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), Q()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SISYPHE(), SIS_ARRET()

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
!> </td><td> 07/12/2009
!> </td><td> JMH
!> </td><td> KS SET TO 0 IF LLT=0
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 11/09/1995
!> </td><td> E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AAT
!></td><td>--></td><td>CURRENT TIME (FOR BUILDING SOLUTIONS)
!>    </td></tr>
!>          <tr><td>LLT
!></td><td>--></td><td>LOCAL LT (MAY BE LT-1+PERCOU)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PREDES
     &(LLT,AAT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AAT            |-->| CURRENT TIME (FOR BUILDING SOLUTIONS)
C| LLT            |-->| LOCAL LT (MAY BE LT-1+PERCOU)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_SISYPHE
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)          :: LLT
      DOUBLE PRECISION, INTENT(IN) :: AAT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER LTT,I
      LOGICAL IMP,LEO
C
C-----------------------------------------------------------------------
C
C     THE OUTPUT VARIABLES ARE BUILT ONLY IF NECESSARY, HENCE THE
C     FOLLOWING TESTS, WHICH MUST BE THE SAME AS IN DESIMP (BIEF LIBRARY)
C
      IMP=.FALSE.
      LEO=.FALSE.
      LTT=(LLT/LISPR)*LISPR
      IF(LLT.EQ.LTT.AND.LLT.GE.PTINIL) IMP=.TRUE.
      LTT=(LLT/LEOPR)*LEOPR
      IF(LLT.EQ.LTT.AND.LLT.GE.PTINIG) LEO=.TRUE.
C
C     NO PRINTOUTS REUIRED: LEAVING
      IF (.NOT.(LEO.OR.IMP)) GO TO 1000
C
C=======================================================================
C     COMPUTES SECONDARY VARIABLES
C=======================================================================
C
C     FREE SURFACE: H+ZF
C
      IF((LEO.AND.SORLEO(4)).OR.(IMP.AND.SORIMP(4))) THEN
        CALL OS('X=Y+Z   ',X=Z,Y=HN,Z=ZF)
      ENDIF
C
C     DISCHARGE
C
      IF((LEO.AND.SORLEO(6)).OR.(IMP.AND.SORIMP(6))) THEN
        DO I=1,NPOIN
          Q%R(I)=HN%R(I)*SQRT(U2D%R(I)**2+V2D%R(I)**2)
        ENDDO
      ENDIF
C
C     DISCHARGE ALONG X
C
      IF((LEO.AND.SORLEO(7)).OR.(IMP.AND.SORIMP(7))) THEN
        CALL OS('X=YZ    ',X=QU,Y=U2D,Z=HN)
      ENDIF
C
C     DISCHARGE ALONG Y
C
      IF((LEO.AND.SORLEO(8)).OR.(IMP.AND.SORIMP(8))) THEN
        CALL OS('X=YZ    ',X=QU,Y=V2D,Z=HN)
      ENDIF
C
C=======================================================================
C
C     VARIABLES WHICH ARE NOT INITIALISED AT THE FIRST CALL OF PREDES
C
      IF(LLT.EQ.0) THEN
C       JMH ON 27/11/2009
        IF((LEO.AND.SORLEO(19)).OR.(IMP.AND.SORIMP(19))) THEN
          CALL OS('X=0     ',X=KS)
        ENDIF
      ENDIF
C
C=======================================================================
C
1000  CONTINUE
C
C=======================================================================
C
      RETURN
      END
C
C#######################################################################
C