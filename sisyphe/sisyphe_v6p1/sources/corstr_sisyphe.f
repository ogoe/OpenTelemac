C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CORRECTS THE BOTTOM FRICTION COEFFICIENT
!>               (IF VARIABLE IN TIME).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
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
!>      <td><center> 5.1                                       </center>
!> </td><td> 12/11/97
!> </td><td> C. LE NORMANT (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CHESTR
!></td><td><--</td><td>BOTTOM FRICTION COEFFICIENT
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>WATER DEPTH
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NUMBER OF GRID POINTS
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td>--></td><td>ARRAY FOR USER
!>    </td></tr>
!>          <tr><td>QU , QV
!></td><td><--</td><td>FLOW RATE ALONG X AND Y
!>    </td></tr>
!>          <tr><td>TIME
!></td><td>--></td><td>TIME
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>2D COORDINATES
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>BOTTOM ELEVATION
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CORSTR_SISYPHE
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CHESTR         |<--| BOTTOM FRICTION COEFFICIENT
C| H             |-->| WATER DEPTH
C| NPOIN          |-->| NUMBER OF GRID POINTS
C| PRIVE          |-->| ARRAY FOR USER
C| QU , QV        |<--| FLOW RATE ALONG X AND Y
C| TIME           |-->| TIME
C| X,Y            |-->| 2D COORDINATES
C| ZF             |-->| BOTTOM ELEVATION
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_SISYPHE
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C-----------------------------------------------------------------------
C
C
C-----------------------------------------------------------------------
C
      RETURN
      END SUBROUTINE CORSTR_SISYPHE
C
C#######################################################################
C