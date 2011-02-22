C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       "WRAPPER" FOR DESIMP SO THAT OUTPUTS CAN BE DONE
!>                FROM WITHIN ESTEL-3D WHEN USING THE COUPLED MODEL
!>                RATHER THAN RELYING ON DESIMP (AND ITS FUNNY +
!>                RELIANCE ON LT) DIRECTLY.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> TIME
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC2D :<br>
!> @link DECLARATIONS_TELEMAC2D::MAXVAR MAXVAR@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NPOIN NPOIN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::SORIMP SORIMP@endlink, 
!> @link DECLARATIONS_TELEMAC2D::SORLEO SORLEO@endlink, 
!> @link DECLARATIONS_TELEMAC2D::T2D_FILES T2D_FILES@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TEXTE TEXTE@endlink, 
!> @link DECLARATIONS_TELEMAC2D::VARSOR VARSOR@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> HIST
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> BIEF_DESIMP(), PRERES_TELEMAC2D()
!>   </td></tr>
!>     </table>

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
!>      <td><center> 5.7                                       </center>
!> </td><td>
!> </td><td> JP RENAUD
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>TIME
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
      SUBROUTINE OUTPUT_TELEMAC2D(TIME)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| TIME           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C-----------------------------------------------------------------------
C  TIME: TIME TO PRINTOUT IN RECORD
C-----------------------------------------------------------------------
C ARGUMENTS
      DOUBLE PRECISION, INTENT(IN) :: TIME
C-----------------------------------------------------------------------
C LOCAL VARIABLES (REQUIRED BY DESIMP, NO IDEA WHY...)
      DOUBLE PRECISION :: HIST(1)
      DATA HIST /9999.D0/
C-----------------------------------------------------------------------
C     PREPARES THE RESULTS
      CALL PRERES_TELEMAC2D
C
C     OUTPUTS A STANDARD TIME STEP
      CALL BIEF_DESIMP(T2D_FILES(T2DRES)%FMT,VARSOR,
     &                 HIST,0,NPOIN,T2D_FILES(T2DRES)%LU,'STD',
     &                 TIME,1,1,1,
     &                 SORLEO,SORIMP,MAXVAR,TEXTE,0,0)
C
C-----------------------------------------------------------------------
C
      RETURN
      END SUBROUTINE OUTPUT_TELEMAC2D
C
C#######################################################################
C