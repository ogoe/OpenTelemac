C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       NUMERICAL UT-D-U FACTORISATION OF SPARSE SYMMETRICAL
!>                POSITIVE DEFINITE MATRIX.
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
!>    </th><td> A, D, FLAG, IA, IJU, IL, IP, IU, JA, JL, JU, N, P, U, UMAX
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DK, I, ILI, J, JMAX, JMIN, JUMUJ, K, MU, NEXTI, UKIDI, VJ
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SD_SNF
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
!></td><td><--</td><td>REAL ONE-DIMENSIONAL ARRAY CONTAINING THE
!>                  NONZERO ENTRIES IN (THE UPPER TRIANGLE OF) M,
!>                  STORED BY ROWS;  DIMENSION =NUMBER OF NONZERO
!>                  ENTRIES IN (THE UPPER TRIANGLE OF) M
!>    </td></tr>
!>          <tr><td>D
!></td><td>---</td><td>(D(I),I=K,N) CONTAINS THE K-TH ROW OF U (EXPANDED)
!>    </td></tr>
!>          <tr><td>FLAG
!></td><td><--</td><td>LOGICAL VARIABLE;  IF DFLAG = .TRUE., THEN
!>                  STORE NONZERO DIAGONAL ELEMENTS AT THE
!>                  BEGINNING OF THE ROW
!>    </td></tr>
!>          <tr><td>IA
!></td><td><--</td><td>INTEGER ONE-DIMENSIONAL ARRAY CONTAINING
!>                  POINTERS TO DELIMIT ROWS IN JA AND A;
!>                  DIMENSION = N+1
!>    </td></tr>
!>          <tr><td>IJU
!></td><td>--></td><td>INTEGER ONE-DIMENSIONAL ARRAY CONTAINING
!>                  POINTERS TO THE START OF EACH ROW IN JU;  DIMENSION = N
!>    </td></tr>
!>          <tr><td>IL
!></td><td>---</td><td>IL(I) POINTS TO THE FIRST NONZERO ELEMENT IN
!>                  COLUMNS K,...,N OF ROW I OF U
!>    </td></tr>
!>          <tr><td>IP
!></td><td><--</td><td>INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN
!>                  THE INVERSE OF THE PERMUTATION RETURNED IN P;
!>                  DIMENSION = N
!>    </td></tr>
!>          <tr><td>IU
!></td><td>--></td><td>INTEGER ONE-DIMENSIONAL ARRAY CONTAINING
!>                  POINTERS TO DELIMIT ROWS IN U;  DIMENSION = N+1
!>    </td></tr>
!>          <tr><td>JA
!></td><td><--</td><td>INTEGER ONE-DIMENSIONAL ARRAY CONTAINING THE
!>                  COLUMN INDICES CORRESPONDING TO THE ELEMENTS
!>                  OF A;  DIMENSION = NUMBER OF NONZERO ENTRIES
!>                  IN (THE UPPER TRIANGLE OF) M
!>    </td></tr>
!>          <tr><td>JL
!></td><td>--></td><td>INTEGER ONE-DIMENSIONAL WORK ARRAY; DIMENSION N
!>                  DIMENSION = NUMBER OF NONZERO ENTRIES IN THE
!>                  UPPER TRIANGLE OF M. JL CONTAINS LISTS OF ROWS
!>                  TO BE MERGED INTO UNELIMINATED ROWS --
!>                  I GE K => JL(I) IS THE FIRST ROW TO BE
!>                  MERGED INTO ROW I
!>                  I LT K => JL(I) IS THE ROW FOLLOWING ROW I IN
!>                  SOME LIST OF ROWS
!>                  IN EITHER CASE, JL(I) = 0 INDICATES THE
!>                  END OF A LIST
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
!></td><td><--</td><td>INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN
!>                  THE PERMUTATION OF THE ROWS AND COLUMNS OF M
!>                  CORRESPONDING TO THE MINIMUM DEGREE ORDERING;
!>                  DIMENSION = N
!>    </td></tr>
!>          <tr><td>U
!></td><td>---</td><td>REAL ONE-DIMENSIONAL ARRAY CONTAINING THE
!>                  NONZERO ENTRIES IN THE STRICT UPPER TRIANGLE
!>                  OF U, STORED BY ROWS; DIMENSION = UMAX
!>    </td></tr>
!>          <tr><td>UMAX
!></td><td>---</td><td>DECLARED DIMENSION OF THE ONE-DIMENSIONAL
!>                  ARRAY U;  UMAX MUST BE AT LEAST THE NUMBER
!>                  OF NONZERO ENTRIES IN THE STRICT UPPER TRIANGLE
!>                  OF M PLUS FILLIN (IU(N+1)-1 AFTER THE CALL TO SSF)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                            SUBROUTINE SD_SNF
     &(N,P,IP,IA,JA,A,D,IJU,JU,IU,U,UMAX,IL,JL,FLAG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |<--| REAL ONE-DIMENSIONAL ARRAY CONTAINING THE
C|                |   | NONZERO ENTRIES IN (THE UPPER TRIANGLE OF) M,
C|                |   | STORED BY ROWS;  DIMENSION =NUMBER OF NONZERO
C|                |   | ENTRIES IN (THE UPPER TRIANGLE OF) M
C| D             |---| (D(I),I=K,N) CONTAINS THE K-TH ROW OF U (EXPANDED)
C| FLAG           |<--| LOGICAL VARIABLE;  IF DFLAG = .TRUE., THEN
C|                |   | STORE NONZERO DIAGONAL ELEMENTS AT THE
C|                |   | BEGINNING OF THE ROW
C| IA             |<--| INTEGER ONE-DIMENSIONAL ARRAY CONTAINING
C|                |   | POINTERS TO DELIMIT ROWS IN JA AND A;
C|                |   | DIMENSION = N+1
C| IJU            |-->| INTEGER ONE-DIMENSIONAL ARRAY CONTAINING
C|                |   | POINTERS TO THE START OF EACH ROW IN JU;  DIMENSION = N
C| IL             |---| IL(I) POINTS TO THE FIRST NONZERO ELEMENT IN
C|                |   | COLUMNS K,...,N OF ROW I OF U
C| IP             |<--| INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN
C|                |   | THE INVERSE OF THE PERMUTATION RETURNED IN P;
C|                |   | DIMENSION = N
C| IU             |-->| INTEGER ONE-DIMENSIONAL ARRAY CONTAINING
C|                |   | POINTERS TO DELIMIT ROWS IN U;  DIMENSION = N+1
C| JA             |<--| INTEGER ONE-DIMENSIONAL ARRAY CONTAINING THE
C|                |   | COLUMN INDICES CORRESPONDING TO THE ELEMENTS
C|                |   | OF A;  DIMENSION = NUMBER OF NONZERO ENTRIES
C|                |   | IN (THE UPPER TRIANGLE OF) M
C| JL             |-->| INTEGER ONE-DIMENSIONAL WORK ARRAY; DIMENSION N
C|                |   | DIMENSION = NUMBER OF NONZERO ENTRIES IN THE
C|                |   | UPPER TRIANGLE OF M. JL CONTAINS LISTS OF ROWS
C|                |   | TO BE MERGED INTO UNELIMINATED ROWS --
C|                |   | I GE K => JL(I) IS THE FIRST ROW TO BE
C|                |   | MERGED INTO ROW I
C|                |   | I LT K => JL(I) IS THE ROW FOLLOWING ROW I IN
C|                |   | SOME LIST OF ROWS
C|                |   | IN EITHER CASE, JL(I) = 0 INDICATES THE
C|                |   | END OF A LIST
C| JU             |-->| INTEGER ONE-DIMENSIONAL ARRAY CONTAINING THE
C|                |   | COLUMN INDICES CORRESPONDING TO THE ELEMENTS
C|                |   | OF U;  DIMENSION = JUMAX
C| N             |-->| ORDER OF THE MATRIX
C| P             |<--| INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN
C|                |   | THE PERMUTATION OF THE ROWS AND COLUMNS OF M
C|                |   | CORRESPONDING TO THE MINIMUM DEGREE ORDERING;
C|                |   | DIMENSION = N
C| U             |---| REAL ONE-DIMENSIONAL ARRAY CONTAINING THE
C|                |   | NONZERO ENTRIES IN THE STRICT UPPER TRIANGLE
C|                |   | OF U, STORED BY ROWS; DIMENSION = UMAX
C| UMAX           |---| DECLARED DIMENSION OF THE ONE-DIMENSIONAL
C|                |   | ARRAY U;  UMAX MUST BE AT LEAST THE NUMBER
C|                |   | OF NONZERO ENTRIES IN THE STRICT UPPER TRIANGLE
C|                |   | OF M PLUS FILLIN (IU(N+1)-1 AFTER THE CALL TO SSF)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_SD_SNF => SD_SNF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: N,UMAX
      INTEGER, INTENT(IN) :: P(N),IP(N),IA(N+1),JA(*),IJU(N),JU(*)
      INTEGER, INTENT(IN) :: IU(N+1)
      INTEGER, INTENT(INOUT) :: IL(*),JL(*),FLAG
      DOUBLE PRECISION, INTENT(IN)    :: A(*)
      DOUBLE PRECISION, INTENT(INOUT) :: D(N),U(UMAX)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K,JMIN,JMAX,VJ,NEXTI,ILI,MU,I,J,JUMUJ
      DOUBLE PRECISION DK,UKIDI
C
C-----------------------------------------------------------------------
C
C
C----CHECKS FOR SUFFICIENT STORAGE FOR U
C
      IF(IU(N+1)-1.GT.UMAX)  GO TO 107
C
C----INITIALISES
C
      DO K=1,N
        D(K)  = 0
        JL(K) = 0
      ENDDO
C
C----FOR EACH ROW K
C
      DO 11 K=1,N
C
C------INITIALISES K-TH ROW WITH ELEMENTS NONZERO IN ROW P(K) OF M
C
        JMIN = IA(P(K))
        JMAX = IA(P(K)+1) - 1
        IF(JMIN.GT.JMAX) GO TO 5
        DO 4 J=JMIN,JMAX
          VJ = IP(JA(J))
          IF (K.LE.VJ)  D(VJ) = A(J)
4       CONTINUE
C
C------MODIFIES K-TH ROW BY ADDING IN THOSE ROWS I WITH U(I,K) NE 0
C------FOR EACH ROW I TO BE ADDED IN
C
5       DK = D(K)
        I = JL(K)
6       IF(I.EQ.0) GO TO 9
        NEXTI = JL(I)
C
C--------COMPUTES MULTIPLIER AND UPDATES DIAGONAL ELEMENT
C
        ILI = IL(I)
        UKIDI = - U(ILI) * D(I)
        DK = DK + UKIDI * U(ILI)
        U(ILI) = UKIDI
C
C--------ADDS MULTIPLE OF ROW I TO K-TH ROW ...
C
        JMIN = ILI     + 1
        JMAX = IU(I+1) - 1
        IF(JMIN.GT.JMAX)  GO TO 8
        MU = IJU(I) - IU(I)
        DO J=JMIN,JMAX
          D(JU(MU+J)) = D(JU(MU+J)) + UKIDI * U(J)
        ENDDO
C
C--------... AND ADDS I TO ROW LIST FOR NEXT NONZERO ENTRY
C
        IL(I) = JMIN
        J = JU(MU+JMIN)
        JL(I) = JL(J)
        JL(J) = I
C
8       I = NEXTI
        GO TO 6
C
C------CHECKS FOR 0 PIVOT AND SAVES DIAGONAL ELEMENT
C
9       IF(DK.EQ.0)  GO TO 108
        D(K) = 1 / DK
C
C------SAVES NONZERO ENTRIES IN K-TH ROW OF U ...
C
        JMIN = IU(K)
        JMAX = IU(K+1) - 1
        IF(JMIN.GT.JMAX)  GO TO 11
        MU = IJU(K) - JMIN
        DO J=JMIN,JMAX
          JUMUJ = JU(MU+J)
          U(J) = D(JUMUJ)
          D(JUMUJ) = 0
        ENDDO
C
C------... AND ADDS K TO ROW LIST FOR FIRST NONZERO ENTRY IN K-TH ROW
C
        IL(K) = JMIN
        I = JU(MU+JMIN)
        JL(K) = JL(I)
        JL(I) = K
11    CONTINUE
C
      FLAG = 0
      RETURN
C
C ** ERROR -- INSUFFICIENT STORAGE FOR U
C
107   FLAG = 7*N + 1
      RETURN
C
C ** ERROR -- 0 PIVOT
C
108   FLAG = 8*N + K
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C