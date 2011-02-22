C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES MAX DIFFERENCES BETWEEN 2 COMPUTED ARRAYS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF_DEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ERR, IERR, X1, X2
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>HOMERE_ADJ_T2D()

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
!>      <td><center> 5.1                                       </center>
!> </td><td> 02/10/00
!> </td><td> A. LEOPARDI (UNINA)
!> </td><td> UPGRADE
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 27/04/93
!> </td><td> E. BARROS
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ERR
!></td><td><--</td><td>MAXIMUM ABSOLUTE DIFFERENCE
!>    </td></tr>
!>          <tr><td>IERR
!></td><td><--</td><td>POINT WHERE THE DIFFERENCE OCCURS
!>    </td></tr>
!>          <tr><td>X1,X2
!></td><td>--></td><td>ARRAYS TO COMPARE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ERRMAX
     &(X1,X2,ERR,IERR)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ERR            |<--| MAXIMUM ABSOLUTE DIFFERENCE
C| IERR           |<--| POINT WHERE THE DIFFERENCE OCCURS
C| X1,X2          |-->| ARRAYS TO COMPARE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF_DEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER          , INTENT(OUT) :: IERR
      DOUBLE PRECISION , INTENT(OUT) :: ERR
      TYPE (BIEF_OBJ)  , INTENT(IN)  :: X1,X2
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
C
      INTRINSIC ABS
C
C---------------------------------------------------------------------
C
      IERR=1
      ERR=-1.D0
      DO I=1,X1%DIM1
C
        IF(ABS(X1%R(I)-X2%R(I)).GT.ERR) THEN
           ERR=ABS(X1%R(I)-X2%R(I))
           IERR=I
        ENDIF
C
      ENDDO
C
C---------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C