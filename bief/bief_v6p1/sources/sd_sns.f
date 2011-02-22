C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLUTION OF SPARSE SYMMETRICAL POSITIVE
!>                DEFINITE SYSTEM OF LINEAR EQUATIONS  MX = B
!>                GIVEN UT-D-U FACTORISATION OF M.
!>  @code
!>  COMPRESSED STORAGE OF SPARSE MATRICES
!>
!>    THE STRICT UPPER TRIANGULAR PORTION OF THE MATRIX U IS STORED IN
!>    (IA,JA,A) FORMAT USING THE ARRAYS IU, JU, AND U, EXCEPT THAT AN
!>    ADDITIONAL ARRAY IJU IS USED TO REDUCE THE STORAGE REQUIRED FOR JU
!>    BY ALLOWING SOME SEQUENCES OF COLUMN INDICES TO CORRESPOND TO MORE
!>    THAN ONE ROW.  FOR I < N, IJU(I) IS THE INDEX IN JU OF THE FIRST
!>    ENTRY FOR THE I-TH ROW;  IJU(N) IS THE NUMBER OF ENTRIES IN JU.
!>    THUS, THE NUMBER OF ENTRIES IN THE I-TH ROW IS  IU(I+1) - IU(I),
!>    THE NONZERO ENTRIES OF THE I-TH ROW ARE STORED CONSECUTIVELY IN
!>
!>        U(IU(I)),   U(IU(I)+1),   ..., U(IU(I+1)-1),
!>
!>    AND THE CORRESPONDING COLUMN INDICES ARE STORED CONSECUTIVELY IN
!>
!>        JU(IJU(I)), JU(IJU(I)+1), ..., JU(IJU(I)+IU(I+1)-IU(I)-1).
!>
!>    COMPRESSION IN JU OCCURS IN TWO WAYS.  FIRST, IF A ROW I WAS MERGED
!>    INTO ROW K, AND THE NUMBER OF ELEMENTS MERGED IN FROM (THE TAIL
!>    PORTION OF) ROW I IS THE SAME AS THE FINAL LENGTH OF ROW K, THEN
!>    THE KTH ROW AND THE TAIL OF ROW I ARE IDENTICAL AND IJU(K) POINTS
!>    TO THE START OF THE TAIL.  SECOND, IF SOME TAIL PORTION OF THE
!>    (K-1)ST ROW IS IDENTICAL TO THE HEAD OF THE KTH ROW, THEN IJU(K)
!>    POINTS TO THE START OF THAT TAIL PORTION.  FOR EXAMPLE, THE NONZERO
!>    STRUCTURE OF THE STRICT UPPER TRIANGULAR PART OF THE MATRIX
!>
!>             ( D 0 0 0 X X X )
!>             ( 0 D 0 X X 0 0 )
!>             ( 0 0 D 0 X X 0 )
!>         U = ( 0 0 0 D X X 0 )
!>             ( 0 0 0 0 D X X )
!>             ( 0 0 0 0 0 D X )
!>             ( 0 0 0 0 0 0 D )
!>
!>    WOULD BE STORED AS
!>
!>             \ 1  2  3  4  5  6  7  8
!>         ----+------------------------
!>          IU \ 1  4  6  8 10 12 13 13
!>          JU \ 5  6  7  4  5  6
!>         IJU \ 1  4  5  5  2  3  6           .
!>
!>    THE DIAGONAL ENTRIES OF U ARE EQUAL TO ONE AND ARE NOT STORED.
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  IMPORTANT : INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> B, D, IJU, IU, JU, N, P, TMP, U, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, J, JMAX, JMIN, K, MU, SU, TMPK
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SD_SNS
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SD_SDRV()

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
!> </td><td> 13/11/08
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
!></td><td>---</td><td>REAL ONE-DIMENSIONAL ARRAY CONTAINING THE
!>                  RIGHT-HAND SIDE B; B AND Z CAN BE THE SAME ARRAY;
!>                  DIMENSION = N
!>    </td></tr>
!>          <tr><td>D
!></td><td>---</td><td>REAL ONE-DIMENSIONAL ARRAY CONTAINING THE
!>                  RECIPROCALS OF THE DIAGONAL ENTRIES OF THE
!>                  MATRIX D;  DIMENSION = N
!>    </td></tr>
!>          <tr><td>IJU
!></td><td>--></td><td>INTEGER ONE-DIMENSIONAL ARRAY CONTAINING
!>                  POINTERS TO THE START OF EACH ROW IN JU;  DIMENSION = N
!>    </td></tr>
!>          <tr><td>IU
!></td><td>--></td><td>INTEGER ONE-DIMENSIONAL ARRAY CONTAINING
!>                  POINTERS TO DELIMIT ROWS IN U;  DIMENSION = N+1
!>    </td></tr>
!>          <tr><td>JU
!></td><td>--></td><td>INTEGER ONE-DIMENSIONAL ARRAY CONTAINING THE
!>                  COLUMN INDICES CORRESPONDING TO THE ELEMENTS
!>                  OF U;  DIMENSION = JUMAX
!>    </td></tr>
!>          <tr><td>N
!></td><td>--></td><td>ORDER OF THE MATRIX
!>    </td></tr>
!>          <tr><td>P
!></td><td>--></td><td>INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN
!>                  THE PERMUTATION OF THE ROWS AND COLUMNS OF M
!>                  CORRESPONDING TO THE MINIMUM DEGREE ORDERING;
!>                  DIMENSION = N
!>    </td></tr>
!>          <tr><td>TMP
!></td><td>---</td><td>REAL ONE-DIMENSIONAL WORK ARRAY; DIMENSION N
!>    </td></tr>
!>          <tr><td>U
!></td><td>---</td><td>REAL ONE-DIMENSIONAL ARRAY CONTAINING THE
!>                  NONZERO ENTRIES IN THE STRICT UPPER TRIANGLE
!>                  OF U, STORED BY ROWS; DIMENSION = UMAX
!>    </td></tr>
!>          <tr><td>Z
!></td><td>---</td><td>REAL ONE-DIMENSIONAL ARRAY CONTAINING THE
!>                  SOLUTION X;  Z AND B CAN BE THE SAME ARRAY;
!>                  DIMENSION = N
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                             SUBROUTINE SD_SNS
     &(N,P,D,IJU,JU,IU,U,Z,B,TMP)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| B             |---| REAL ONE-DIMENSIONAL ARRAY CONTAINING THE
C|                |   | RIGHT-HAND SIDE B; B AND Z CAN BE THE SAME ARRAY;
C|                |   | DIMENSION = N
C| D             |---| REAL ONE-DIMENSIONAL ARRAY CONTAINING THE
C|                |   | RECIPROCALS OF THE DIAGONAL ENTRIES OF THE
C|                |   | MATRIX D;  DIMENSION = N
C| IJU            |-->| INTEGER ONE-DIMENSIONAL ARRAY CONTAINING
C|                |   | POINTERS TO THE START OF EACH ROW IN JU;  DIMENSION = N
C| IU             |-->| INTEGER ONE-DIMENSIONAL ARRAY CONTAINING
C|                |   | POINTERS TO DELIMIT ROWS IN U;  DIMENSION = N+1
C| JU             |-->| INTEGER ONE-DIMENSIONAL ARRAY CONTAINING THE
C|                |   | COLUMN INDICES CORRESPONDING TO THE ELEMENTS
C|                |   | OF U;  DIMENSION = JUMAX
C| N             |-->| ORDER OF THE MATRIX
C| P             |-->| INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN
C|                |   | THE PERMUTATION OF THE ROWS AND COLUMNS OF M
C|                |   | CORRESPONDING TO THE MINIMUM DEGREE ORDERING;
C|                |   | DIMENSION = N
C| TMP            |---| REAL ONE-DIMENSIONAL WORK ARRAY; DIMENSION N
C| U             |---| REAL ONE-DIMENSIONAL ARRAY CONTAINING THE
C|                |   | NONZERO ENTRIES IN THE STRICT UPPER TRIANGLE
C|                |   | OF U, STORED BY ROWS; DIMENSION = UMAX
C| Z             |---| REAL ONE-DIMENSIONAL ARRAY CONTAINING THE
C|                |   | SOLUTION X;  Z AND B CAN BE THE SAME ARRAY;
C|                |   | DIMENSION = N
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_SD_SNS => SD_SNS
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: N
      INTEGER, INTENT(INOUT)          :: P(N),IJU(*),JU(*),IU(N+1)
      DOUBLE PRECISION, INTENT(IN)    :: B(N)
      DOUBLE PRECISION, INTENT(INOUT) :: TMP(N),Z(N),D(N),U(*)
C                                                           UMAX
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,J,K,JMIN,JMAX,MU
      DOUBLE PRECISION TMPK,SU
C
C-----------------------------------------------------------------------
C
C----SETS TMP TO PERMUTED B
C
      DO K=1,N
        TMP(K) = B(P(K))
      ENDDO
C
C----SOLVES  UT D Y = B  BY FORWARD SUBSTITUTION
C
      DO K=1,N
        TMPK = TMP(K)
        JMIN = IU(K)
        JMAX = IU(K+1) - 1
        IF(JMIN.GT.JMAX)  GO TO 3
        MU = IJU(K) - JMIN
        DO J=JMIN,JMAX
          TMP(JU(MU+J)) = TMP(JU(MU+J)) + U(J) * TMPK
        ENDDO
3       TMP(K) = TMPK * D(K)
      ENDDO
C
C----SOLVES  U X = Y  BY BACK SUBSTITUTION
C
      K = N
      DO I=1,N
        SU   = TMP(K)
        JMIN = IU(K)
        JMAX = IU(K+1) - 1
        IF(JMIN.GT.JMAX) GO TO 5
        MU = IJU(K) - JMIN
        DO J=JMIN,JMAX
          SU = SU + U(J) * TMP(JU(MU+J))
        ENDDO
5       TMP(K) = SU
        Z(P(K)) = SU
        K = K-1
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C