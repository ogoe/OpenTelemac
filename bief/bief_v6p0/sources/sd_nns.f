C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       NUMERIC SOLUTION OF A SPARSE NONSYMMETRICAL SYSTEM
!>                OF LINEAR EQUATIONS GIVEN LDU-FACTORISATION
!>               (UNCOMPRESSED POINTER STORAGE).
!>  @code
!>       INPUT VARIABLES:   N, R,C, IL,JL,L, D, IU,JU,U, B
!>       OUTPUT VARIABLES:  Z
!>
!>       PARAMETERS USED INTERNALLY:
!> FIA   \ TMP   - HOLDS NEW RIGHT-HAND SIDE B' FOR SOLUTION OF THE
!>       \           EQUATION UX = B'.
!>       \           SIZE = N.
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  IMPORTANT : INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> B, C, D, IL, IU, JL, JU, L, N, R, TMP, U, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> BSUM, I, J, JMAX, JMIN, K
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SD_NDRV()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 18/02/08
!> </td><td> E. RAZAFINDRAKOTO (LNH) 01 30 87 74 03
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>B
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>C
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>JL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>JU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>L
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>N
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>R
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TMP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Z
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                           SUBROUTINE SD_NNS
     &(N,R,C,IL,JL,L,D,IU,JU,U,Z,B,TMP)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| B             |---| 
C| C             |---| 
C| D             |---| 
C| IL             |---| 
C| IU             |---| 
C| JL             |---| 
C| JU             |---| 
C| L             |---| 
C| N             |---| 
C| R             |---| 
C| TMP            |---| 
C| U             |---| 
C| Z             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER N,R(*),C(*),IL(*),JL(*),IU(*),JU(*)
      DOUBLE PRECISION L(*),D(*),U(*),Z(*),B(*),TMP(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,J,K,JMIN,JMAX
      DOUBLE PRECISION BSUM
C
C-----------------------------------------------------------------------
C
C  ******  SOLVES LDY = B  BY FORWARD SUBSTITUTION  ********************
C
        DO 2 K=1,N
          BSUM = B(R(K))
          JMIN = IL(K)
          JMAX = IL(K+1) - 1
          IF (JMIN.GT.JMAX)  GO TO 2
          DO 1 J=JMIN,JMAX
            BSUM = BSUM - L(J) * TMP(JL(J))
1         CONTINUE
2         TMP(K) = BSUM * D(K)
C
C  ******  SOLVES  UX = Y  BY BACK SUBSTITUTION  ***********************
C
      K = N
      DO 5 I=1,N
        BSUM = TMP(K)
        JMIN = IU(K)
        JMAX = IU(K+1) - 1
        IF (JMIN.GT.JMAX)  GO TO 4
        DO 3 J=JMIN,JMAX
          BSUM = BSUM - U(J) * TMP(JU(J))
3       CONTINUE
   4    TMP(K) = BSUM
        Z(C(K)) = BSUM
        K = K-1
5     CONTINUE
C
C------------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C