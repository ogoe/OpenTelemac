C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CHECKS IF A CHARACTER STRING IS COMPRISED IN ANOTHER.
!>                INCLUS=.TRUE. MEANS 'C2 IS COMPRISED IN C1'.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C1, C2
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, LC1, LC2
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CONDIH(), HOMERE_TELEMAC2D(), HOMERE_TELEMAC3D(), LECDON_TELEMAC2D(), LECDON_TELEMAC3D(), OM(), TELEMAC2D(), TELEMAC3D()

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
!> </td><td> 17/08/94
!> </td><td> J.M. HERVOUET (LNH)   30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>C1
!></td><td>--></td><td>CHAINE CENSEE CONTENIR C2
!>    </td></tr>
!>          <tr><td>C2
!></td><td>--></td><td>CHAINE RECHERCHEE DANS C1
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        LOGICAL FUNCTION INCLUS
     &( C1 , C2 )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C1             |-->| CHAINE CENSEE CONTENIR C2
C| C2             |-->| CHAINE RECHERCHEE DANS C1
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      CHARACTER*(*) C1 , C2
C
      INTEGER I,LC1,LC2
C
      INTRINSIC LEN
C
C-----------------------------------------------------------------------
C
      INCLUS = .FALSE.
C
      LC1 = LEN(C1)
      LC2 = LEN(C2)
      IF(LC2.GT.LC1) GO TO 1000
C
      I = 0
10    I = I + 1
      IF(I.GT.LC1-LC2+1) GO TO 1000
C
      IF(C1(I:I+LC2-1).NE.C2(1:LC2)) GO TO 10
C
      INCLUS = .TRUE.
C
1000  CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C