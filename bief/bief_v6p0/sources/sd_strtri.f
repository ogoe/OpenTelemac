C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SORTS IN ASCENDING ORDER THE INTEGER ARRAY 'IS'.
!>                OUTPUT :  IS(IND(I+1) >= IS(IND(I).

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  IMPORTANT: INSPIRED FROM N3S 3.3  22/04/92  B.THOMAS

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IND, IS, N
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, I1, K
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SD_STRTRI
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SD_FABCAD()

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
!> </td><td> 20/11/06
!> </td><td> E. RAZAFINDRAKOTO (LNH) 01 30 87 74 03
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IND
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>N
!></td><td>--></td><td>LONGUEUR DE IS
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SD_STRTRI
     &(IS,N,IND)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IND            |---| 
C| IS             |---| 
C| N             |-->| LONGUEUR DE IS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_SD_STRTRI => SD_STRTRI
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: N
      INTEGER, INTENT(IN)    :: IS(N)
      INTEGER, INTENT(INOUT) :: IND(N)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,I1,K
C
C-----------------------------------------------------------------------
C
      IND(1) = 1
C
      DO 1 I = 2 , N
C
C--->    IS(1:I-1) IS SORTED
C
         I1 = I-1
         DO 5 K = I1 , 1 , -1
C
C--->       FOR L > K+1  IS(IND(L)) > IS(I)
C
            IF(IS(IND(K)).GT.IS(I)) THEN
              IND(K+1) = IND(K)
            ELSE
              GO TO 2
            ENDIF
C
5        CONTINUE
C
C--->    ASSERTION : IS(IND(K))
C
2        IND(K+1)=I
C
1     CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C