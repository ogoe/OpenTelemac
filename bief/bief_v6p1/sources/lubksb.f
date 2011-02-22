C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES THE SET OF N LINEAR EQUATIONS A \ DELTA X = B
!>                HERE A IS INPUT, NOT AS THE MATRIX A BUT AS ITS LU
!>                FACTORISATION, GIVEN BY THE SUBROUTINE LUDCMP. INDX
!>                IS INPUT AS THE PERMUTATION VECTOR RETURNED BY LUDCMP.
!>                B(1:N) IS INPUT AS THE RIGHT-HAND SIDE VECTOR B, AND
!>                RETURNS WITH THE SOLUTION VECTOR X. A, N, NP, AND
!>                INDX ARE NOT MODIFIED BY THIS SUBROUTINE AND CAN BE LEFT
!>                IN PLACE FOR SUCCESSIVE CALLS WITH DIFFERENT RIGHT-HAND
!>                SIDES B. THIS ROUTINE TAKES INTO ACCOUNT THE POSSIBILITY
!>                THAT B WILL BEGIN WITH A LOT OF 0 ELEMENTS, SO IT IS
!>                EFFICIENT FOR USE IN MATRIX INVERSION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A, B, INDX, N, NP
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, II, J, LL, XSOM
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>INVMTX()

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
!> </td><td> 28/07/2006
!> </td><td> CHUN WANG
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>B
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>INDX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>N
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NP
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                             SUBROUTINE LUBKSB
     &(A,N,NP,INDX,B)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |---| 
C| B             |---| 
C| INDX           |---| 
C| N             |---| 
C| NP             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: N,NP
      INTEGER, INTENT(IN) :: INDX(N)
      DOUBLE PRECISION, INTENT(INOUT) :: B(N)
      DOUBLE PRECISION, INTENT(IN)    :: A(NP,NP)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,II,J,LL
      DOUBLE PRECISION XSOM
C
C-----------------------------------------------------------------------
C
      II=0 ! WHEN II HAS A POSITIVE VALUE, IT WILL BECOME THE INDEX
           ! OF THE FIRST NONVANISHING ELEMENT OF B.
           ! DOES THE FORWARD SUBSTITUTION, EQUATION (2.3.6). THE ONLY
           ! NEW WRINKLE IS TO UNSCRAMBLE THE PERMUTATION AS WE GO.
C
      DO I=1,N
        LL=INDX(I)
        XSOM=B(LL)
        B(LL)=B(I)
        IF(II.NE.0) THEN
          DO J=II,I-1
           XSOM=XSOM-A(I,J)*B(J)
          ENDDO
        ELSEIF(XSOM.NE.0.) THEN
          II=I ! A NONZERO ELEMENT WAS ENCOUNTERED, SO FROM NOW ON
               ! WILL HAVE TO DO THE SUMS IN THE ABOVE LOOP
        ENDIF
        B(I)=XSOM
      ENDDO
      DO I=N,1,-1 ! DOES THE BACKSUBSTITUTION, EQUATION (2.3.7)
        XSOM=B(I)
        DO J=I+1,N
          XSOM=XSOM-A(I,J)*B(J)
        ENDDO
        B(I)=XSOM/A(I,I) ! STORES A COMPONENT OF THE SOLUTION VECTOR X
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C