C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SYMMETRIC REORDERING OF SPARSE SYMMETRIC MATRIX.
!>  @code
!>    THE NONZERO ENTRIES OF THE MATRIX M ARE ASSUMED TO BE STORED
!>    SYMMETRICALLY IN (IA,JA,A) FORMAT (I.E., NOT BOTH M(I,J) AND M(J,I)
!>    ARE STORED IF I NE J).<br>
!>    SRO DOES NOT REARRANGE THE ORDER OF THE ROWS, BUT DOES MOVE
!>    NONZEROES FROM ONE ROW TO ANOTHER TO ENSURE THAT IF M(I,J) WILL BE
!>    IN THE UPPER TRIANGLE OF M WITH RESPECT TO THE NEW ORDERING, THEN
!>    M(I,J) IS STORED IN ROW I (AND THUS M(J,I) IS NOT STORED);  WHEREAS
!>    IF M(I,J) WILL BE IN THE STRICT LOWER TRIANGLE OF M, THEN M(J,I) IS
!>    STORED IN ROW J (AND THUS M(I,J) IS NOT STORED).
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  IMPORTANT: INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A, DFLAG, IA, IP, JA, N, Q, R
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AK, I, ILAST, J, JAK, JDUMMY, JMAX, JMIN, K
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SD_SRO
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SD_ODRV()

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
!>          <tr><td>A
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DFLAG
!></td><td>--></td><td>LOGICAL VARIABLE;  IF DFLAG = .TRUE., THEN
!>                  STORE NONZERO DIAGONAL ELEMENTS AT THE
!>                  BEGINNING OF THE ROW
!>    </td></tr>
!>          <tr><td>IA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>JA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>N
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Q
!></td><td>--></td><td>INTEGER ONE-DIMENSIONAL WORK ARRAY
!>    </td></tr>
!>          <tr><td>R
!></td><td>--></td><td>INTEGER ONE-DIMENSIONAL WORK ARRAY
!>                  DIMENSION = NUMBER OF
!>                  NONZERO ENTRIES IN THE UPPER TRIANGLE OF M
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                               SUBROUTINE SD_SRO
     &(N,IP,IA,JA,A,Q,R,DFLAG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |---| 
C| DFLAG          |-->| LOGICAL VARIABLE;  IF DFLAG = .TRUE., THEN
C|                |   | STORE NONZERO DIAGONAL ELEMENTS AT THE
C|                |   | BEGINNING OF THE ROW
C| IA             |---| 
C| IP             |---| 
C| JA             |---| 
C| N             |---| 
C| Q             |-->| INTEGER ONE-DIMENSIONAL WORK ARRAY
C| R             |-->| INTEGER ONE-DIMENSIONAL WORK ARRAY
C|                |   | DIMENSION = NUMBER OF
C|                |   | NONZERO ENTRIES IN THE UPPER TRIANGLE OF M
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_SD_SRO => SD_SRO
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: N
      INTEGER, INTENT(IN)             :: IP(*)
      INTEGER, INTENT(INOUT)          :: JA(*),R(*),Q(N),IA(*)
      DOUBLE PRECISION, INTENT(INOUT) :: A(*)
      LOGICAL, INTENT(IN)             :: DFLAG
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,J,K,JMIN,JMAX,ILAST,JDUMMY,JAK
      DOUBLE PRECISION AK
C
C-----------------------------------------------------------------------
C
C--PHASE 1 -- FINDS ROW IN WHICH TO STORE EACH NONZERO
C----INITIALISES COUNT OF NONZEROES TO BE STORED IN EACH ROW
C
      DO I=1,N
        Q(I) = 0
      ENDDO
C
C----FOR EACH NONZERO ELEMENT A(J)
C
      DO 3 I=1,N
        JMIN = IA(I)
        JMAX = IA(I+1) - 1
        IF(JMIN.GT.JMAX)  GO TO 3
        DO 2 J=JMIN,JMAX
C
C--------FINDS ROW (=R(J)) AND COLUMN (=JA(J)) IN WHICH TO STORE A(J)
C
          K = JA(J)
          IF (IP(K).LT.IP(I))  JA(J) = I
          IF (IP(K).GE.IP(I))  K = I
          R(J) = K
C
C--------AND INCREMENTS COUNT OF NONZEROES (=Q(R(J)) IN THAT ROW
C
          Q(K) = Q(K) + 1
2       CONTINUE
3     CONTINUE
C
C--PHASE 2 -- FINDS NEW IA AND PERMUTATION TO APPLY TO (JA,A)
C----DETERMINES POINTERS TO DELIMIT ROWS IN PERMUTED (JA,A)
C
      DO 4 I=1,N
        IA(I+1) = IA(I) + Q(I)
        Q(I) = IA(I+1)
4     CONTINUE
C
C----DETERMINES WHERE EACH (JA(J),A(J)) IS STORED IN PERMUTED (JA,A)
C----FOR EACH NONZERO ELEMENT (IN REVERSE ORDER)
C
      ILAST = 0
      JMIN = IA(1)
      JMAX = IA(N+1) - 1
      J = JMAX
      DO 6 JDUMMY=JMIN,JMAX
        I = R(J)
        IF(.NOT.DFLAG .OR. JA(J).NE.I .OR. I.EQ.ILAST)  GO TO 5
C
C------IF DFLAG, THEN PUTS DIAGONAL NONZERO AT BEGINNING OF ROW
C
        R(J) = IA(I)
        ILAST = I
        GO TO 6
C
C------PUTS (OFF-DIAGONAL) NONZERO IN LAST UNUSED LOCATION IN ROW
C
5       Q(I) = Q(I) - 1
        R(J) = Q(I)
C
        J = J-1
6     CONTINUE
C
C--PHASE 3 -- PERMUTES (JA,A) TO UPPER TRIANGULAR FORM (WRT NEW ORDERING)
C
      DO 8 J=JMIN,JMAX
7       IF (R(J).EQ.J)  GO TO 8
        K = R(J)
        R(J) = R(K)
        R(K) = K
        JAK = JA(K)
        JA(K) = JA(J)
        JA(J) = JAK
        AK = A(K)
        A(K) = A(J)
        A(J) = AK
        GO TO 7
8     CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C