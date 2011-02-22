C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       DRIVER FOR SPARSE MATRIX REORDERING ROUTINE.
!>  @code
!>    SDRV SOLVES SPARSE SYMMETRIC POSITIVE DEFINITE SYSTEMS OF LINEAR
!>    EQUATIONS.  THE SOLUTION PROCESS IS DIVIDED INTO THREE STAGES --<br>
!>      SSF - THE COEFFICIENT MATRIX M IS FACTORED SYMBOLICALLY TO
!>            DETERMINE WHERE FILLIN WILL OCCUR DURING THE NUMERIC
!>            FACTORIZATION.<br>
!>      SNF - M IS FACTORED NUMERICALLY INTO THE PRODUCT UT-D-U, WHERE
!>            D IS DIAGONAL AND U IS UNIT UPPER TRIANGULAR.<br>
!>      SNS - THE LINEAR SYSTEM  MX = B  IS SOLVED USING THE UT-D-U
!>            FACTORIZATION FROM SNF.<br>
!>    FOR SEVERAL SYSTEMS WITH THE SAME COEFFICIENT MATRIX, SSF AND SNF
!>    NEED BE DONE ONLY ONCE (FOR THE FIRST SYSTEM);  THEN SNS IS DONE
!>    ONCE FOR EACH ADDITIONAL RIGHT-HAND SIDE.  FOR SEVERAL SYSTEMS
!>    WHOSE COEFFICIENT MATRICES HAVE THE SAME NONZERO STRUCTURE, SSF
!>    NEED BE DONE ONLY ONCE (FOR THE FIRST SYSTEM);  THEN SNF AND SNS
!>    ARE DONE ONCE FOR EACH ADDITIONAL SYSTEM.<br><br>
!>  STORAGE OF SPARSE MATRICES<br>
!>    THE NONZERO ENTRIES OF THE MATRIX M ARE STORED ROW-BY-ROW IN THE
!>    ARRAY A.  TO IDENTIFY THE INDIVIDUAL NONZERO ENTRIES IN EACH ROW,
!>    WE NEED TO KNOW IN WHICH COLUMN EACH ENTRY LIES.  THESE COLUMN
!>    INDICES ARE STORED IN THE ARRAY JA;  I.E., IF  A(K) = M(I,J),  THEN
!>    JA(K) = J.  TO IDENTIFY THE INDIVIDUAL ROWS, WE NEED TO KNOW WHERE
!>    EACH ROW STARTS.  THESE ROW POINTERS ARE STORED IN THE ARRAY IA;
!>    I.E., IF M(I,J) IS THE FIRST NONZERO ENTRY (STORED) IN THE I-TH ROW
!>    AND  A(K) = M(I,J),  THEN  IA(I) = K.  MOREOVER, IA(N+1) POINTS TO
!>    THE FIRST LOCATION FOLLOWING THE LAST ELEMENT IN THE LAST ROW.
!>    THUS, THE NUMBER OF ENTRIES IN THE I-TH ROW IS  IA(I+1) - IA(I),
!>    THE NONZERO ENTRIES IN THE I-TH ROW ARE STORED CONSECUTIVELY IN<br>
!>            A(IA(I)),  A(IA(I)+1),  ..., A(IA(I+1)-1),<br>
!>    AND THE CORRESPONDING COLUMN INDICES ARE STORED CONSECUTIVELY IN<br>
!>            JA(IA(I)), JA(IA(I)+1), ..., JA(IA(I+1)-1).<br>
!>    SINCE THE COEFFICIENT MATRIX IS SYMMETRIC, ONLY THE NONZERO ENTRIES
!>    IN THE UPPER TRIANGLE NEED BE STORED, FOR EXAMPLE, THE MATRIX<br>
!>             ( 1  0  2  3  0 )
!>             ( 0  4  0  0  0 )
!>         M = ( 2  0  5  6  0 )
!>             ( 3  0  6  7  8 )
!>             ( 0  0  0  8  9 )<br>
!>    COULD BE STORED AS<br>
!>            \ 1  2  3  4  5  6  7  8  9 10 11 12 13
!>         ---+--------------------------------------
!>         IA \ 1  4  5  8 12 14
!>         JA \ 1  3  4  2  1  3  4  1  3  4  5  4  5
!>          A \ 1  2  3  4  2  5  6  3  6  7  8  8  9<br>
!>    OR (SYMMETRICALLY) AS<br>
!>            \ 1  2  3  4  5  6  7  8  9
!>         ---+--------------------------
!>         IA \ 1  4  5  7  9 10
!>         JA \ 1  3  4  2  3  4  4  5  5
!>          A \ 1  2  3  4  5  6  7  8  9          .<br><br>
!>  REORDERING THE ROWS AND COLUMNS OF M<br>
!>    A SYMMETRIC PERMUTATION OF THE ROWS AND COLUMNS OF THE COEFFICIENT
!>    MATRIX M (E.G., WHICH REDUCES FILLIN OR ENHANCES NUMERICAL
!>    STABILITY) MUST BE SPECIFIED.  THE SOLUTION Z IS RETURNED IN THE
!>    ORIGINAL ORDER.<br>
!>    TO SPECIFY THE TRIVIAL ORDERING (I.E., THE IDENTITY PERMUTATION),
!>    SET  P(I) = IP(I) = I,  I=1,...,N.  IN THIS CASE, P AND IP CAN BE
!>    THE SAME ARRAY.<br>
!>    IF A NONTRIVIAL ORDERING (I.E., NOT THE IDENTITY PERMUTATION) IS
!>    SPECIFIED AND M IS STORED SYMMETRICALLY (I.E., NOT BOTH M(I,J) AND
!>    M(J,I) ARE STORED FOR I NE J), THEN ODRV SHOULD BE CALLED (WITH
!>    PATH = 3 OR 5) TO SYMMETRICALLY REORDER (IA,JA,A) BEFORE CALLING
!>    SDRV.  THIS IS TO ENSURE THAT IF M(I,J) WILL BE IN THE UPPER
!>    TRIANGLE OF M WITH RESPECT TO THE NEW ORDERING, THEN M(I,J) IS
!>    STORED IN ROW I (AND THUS M(J,I) IS NOT STORED);  WHEREAS IF M(I,J)
!>    WILL BE IN THE STRICT LOWER TRIANGLE OF M, THEN M(J,I) IS STORED IN
!>    ROW J (AND THUS M(I,J) IS NOT STORED).
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
!>    </th><td> A, B, ESP, FLAG, IA, IP, ISP, JA, N, NSP, P, PATH, RSP, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> D, IJU, IL, IU, JL, JU, JUMAX, MARK, Q, RATIO, TMP, U, UMAX
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SD_SDRV
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> SD_SNF(), SD_SNS(), SD_SSF()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SD_SOLVE_1()

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
!>          <tr><td>B
!></td><td>---</td><td>REAL ONE-DIMENSIONAL ARRAY CONTAINING THE
!>                  RIGHT-HAND SIDE B; B AND Z CAN BE THE SAME ARRAY;
!>                  DIMENSION = N
!>    </td></tr>
!>          <tr><td>ESP
!></td><td>---</td><td>INTEGER VARIABLE;  IF SUFFICIENT STORAGE WAS
!>                  AVAILABLE TO PERFORM THE SYMBOLIC
!>                  FACTORIZATION (SSF), THEN ESP IS SET TO
!>                  THE AMOUNT OF EXCESS STORAGE PROVIDED
!>                  (NEGATIVE IF INSUFFICIENT STORAGE WAS
!>                  AVAILABLE TO PERFORM THE NUMERIC
!>                  FACTORIZATION (SNF))
!>    </td></tr>
!>          <tr><td>FLAG
!></td><td><--</td><td>INTEGER ERROR FLAG; VALUES AND THEIR MEANINGS:
!>                  0     NO ERRORS DETECTED
!>                  2N+K   DUPLICATE ENTRY IN A  --  ROW = K
!>                  6N+K   INSUFFICIENT STORAGE IN SSF -- ROW = K
!>                  7N+1   INSUFFICIENT STORAGE IN SNF
!>                  8N+K   ZERO PIVOT  --  ROW = K
!>                  10N+1  INSUFFICIENT STORAGE IN SDRV
!>                  11N+1  ILLEGAL PATH SPECIFICATION
!>    </td></tr>
!>          <tr><td>IA
!></td><td><--</td><td>INTEGER ONE-DIMENSIONAL ARRAY CONTAINING
!>                  POINTERS TO DELIMIT ROWS IN JA AND A;
!>                  DIMENSION = N+1
!>    </td></tr>
!>          <tr><td>IP
!></td><td><--</td><td>INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN
!>                  THE INVERSE OF THE PERMUTATION RETURNED IN P;
!>                  DIMENSION = N
!>    </td></tr>
!>          <tr><td>ISP
!></td><td><--</td><td>INTEGER ONE-DIMENSIONAL ARRAY USED FOR
!>                  WORKING STORAGE; DIMENSION = NSP
!>    </td></tr>
!>          <tr><td>JA
!></td><td><--</td><td>INTEGER ONE-DIMENSIONAL ARRAY CONTAINING THE
!>                  COLUMN INDICES CORRESPONDING TO THE ELEMENTS
!>                  OF A;  DIMENSION = NUMBER OF NONZERO ENTRIES
!>                  IN (THE UPPER TRIANGLE OF) M
!>    </td></tr>
!>          <tr><td>N
!></td><td>--></td><td>ORDER OF THE MATRIX
!>    </td></tr>
!>          <tr><td>NSP
!></td><td>--></td><td>DECLARED DIMENSION OF THE ONE-DIMENSIONAL
!>                  ARRAY ISP;  NSP MUST BE AT LEAST  3N+4K,
!>                  WHERE K IS THE NUMBER OF NONZEROES
!>                  IN THE STRICT UPPER TRIANGLE OF M
!>    </td></tr>
!>          <tr><td>P
!></td><td><--</td><td>INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN
!>                  THE PERMUTATION OF THE ROWS AND COLUMNS OF M
!>                  CORRESPONDING TO THE MINIMUM DEGREE ORDERING;
!>                  DIMENSION = N
!>    </td></tr>
!>          <tr><td>PATH
!></td><td>--></td><td>INTEGER PATH SPECIFICATION;
!>                  VALUES AND THEIR MEANINGS ARE -
!>                  1  PERFORM SSF, SNF, AND SNS
!>                  2  PERFORM SNF AND SNS (ISP/RSP IS ASSUMED
!>                  TO HAVE BEEN SET UP IN AN EARLIER CALL
!>                  TO SDRV (FOR SSF))
!>                  3  PERFORM SNS ONLY (ISP/RSP IS ASSUMED
!>                  TO HAVE BEEN SET UP IN AN EARLIER CALL
!>                  TO SDRV (FOR SSF AND SNF))
!>                  4  PERFORM SSF
!>                  5  PERFORM SSF AND SNF
!>                  6  PERFORM SNF ONLY (ISP/RSP IS ASSUMED TO
!>                  HAVE BEEN SET UP IN AN EARLIER CALL TO
!>                  SDRV (FOR SSF))
!>    </td></tr>
!>          <tr><td>RSP
!></td><td>---</td><td>REAL ONE-DIMENSIONAL ARRAY USED FOR WORKING
!>                  STORAGE;  RSP AND ISP SHOULD BE EQUIVALENCED;
!>                  DIMENSION = NSP
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
                          SUBROUTINE SD_SDRV
     &(N,P,IP,IA,JA,A,B,Z,NSP,ISP,RSP,ESP,PATH,FLAG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |<--| REAL ONE-DIMENSIONAL ARRAY CONTAINING THE
C|                |   | NONZERO ENTRIES IN (THE UPPER TRIANGLE OF) M,
C|                |   | STORED BY ROWS;  DIMENSION =NUMBER OF NONZERO
C|                |   | ENTRIES IN (THE UPPER TRIANGLE OF) M
C| B             |---| REAL ONE-DIMENSIONAL ARRAY CONTAINING THE
C|                |   | RIGHT-HAND SIDE B; B AND Z CAN BE THE SAME ARRAY;
C|                |   | DIMENSION = N
C| ESP            |---| INTEGER VARIABLE;  IF SUFFICIENT STORAGE WAS
C|                |   | AVAILABLE TO PERFORM THE SYMBOLIC
C|                |   | FACTORIZATION (SSF), THEN ESP IS SET TO
C|                |   | THE AMOUNT OF EXCESS STORAGE PROVIDED
C|                |   | (NEGATIVE IF INSUFFICIENT STORAGE WAS
C|                |   | AVAILABLE TO PERFORM THE NUMERIC
C|                |   | FACTORIZATION (SNF))
C| FLAG           |<--| INTEGER ERROR FLAG; VALUES AND THEIR MEANINGS:
C|                |   | 0     NO ERRORS DETECTED
C|                |   | 2N+K   DUPLICATE ENTRY IN A  --  ROW = K
C|                |   | 6N+K   INSUFFICIENT STORAGE IN SSF -- ROW = K
C|                |   | 7N+1   INSUFFICIENT STORAGE IN SNF
C|                |   | 8N+K   ZERO PIVOT  --  ROW = K
C|                |   | 10N+1  INSUFFICIENT STORAGE IN SDRV
C|                |   | 11N+1  ILLEGAL PATH SPECIFICATION
C| IA             |<--| INTEGER ONE-DIMENSIONAL ARRAY CONTAINING
C|                |   | POINTERS TO DELIMIT ROWS IN JA AND A;
C|                |   | DIMENSION = N+1
C| IP             |<--| INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN
C|                |   | THE INVERSE OF THE PERMUTATION RETURNED IN P;
C|                |   | DIMENSION = N
C| ISP            |<--| INTEGER ONE-DIMENSIONAL ARRAY USED FOR
C|                |   | WORKING STORAGE; DIMENSION = NSP
C| JA             |<--| INTEGER ONE-DIMENSIONAL ARRAY CONTAINING THE
C|                |   | COLUMN INDICES CORRESPONDING TO THE ELEMENTS
C|                |   | OF A;  DIMENSION = NUMBER OF NONZERO ENTRIES
C|                |   | IN (THE UPPER TRIANGLE OF) M
C| N             |-->| ORDER OF THE MATRIX
C| NSP            |-->| DECLARED DIMENSION OF THE ONE-DIMENSIONAL
C|                |   | ARRAY ISP;  NSP MUST BE AT LEAST  3N+4K,
C|                |   | WHERE K IS THE NUMBER OF NONZEROES
C|                |   | IN THE STRICT UPPER TRIANGLE OF M
C| P             |<--| INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN
C|                |   | THE PERMUTATION OF THE ROWS AND COLUMNS OF M
C|                |   | CORRESPONDING TO THE MINIMUM DEGREE ORDERING;
C|                |   | DIMENSION = N
C| PATH           |-->| INTEGER PATH SPECIFICATION;
C|                |   | VALUES AND THEIR MEANINGS ARE -
C|                |   | 1  PERFORM SSF, SNF, AND SNS
C|                |   | 2  PERFORM SNF AND SNS (ISP/RSP IS ASSUMED
C|                |   | TO HAVE BEEN SET UP IN AN EARLIER CALL
C|                |   | TO SDRV (FOR SSF))
C|                |   | 3  PERFORM SNS ONLY (ISP/RSP IS ASSUMED
C|                |   | TO HAVE BEEN SET UP IN AN EARLIER CALL
C|                |   | TO SDRV (FOR SSF AND SNF))
C|                |   | 4  PERFORM SSF
C|                |   | 5  PERFORM SSF AND SNF
C|                |   | 6  PERFORM SNF ONLY (ISP/RSP IS ASSUMED TO
C|                |   | HAVE BEEN SET UP IN AN EARLIER CALL TO
C|                |   | SDRV (FOR SSF))
C| RSP            |---| REAL ONE-DIMENSIONAL ARRAY USED FOR WORKING
C|                |   | STORAGE;  RSP AND ISP SHOULD BE EQUIVALENCED;
C|                |   | DIMENSION = NSP
C| Z             |---| REAL ONE-DIMENSIONAL ARRAY CONTAINING THE
C|                |   | SOLUTION X;  Z AND B CAN BE THE SAME ARRAY;
C|                |   | DIMENSION = N
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_SD_SDRV => SD_SDRV
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: N,NSP,PATH
      INTEGER, INTENT(INOUT) :: FLAG,P(*),IP(*),IA(*),JA(*),ISP(*),ESP
      DOUBLE PRECISION, INTENT(IN)    :: B(N)
      DOUBLE PRECISION, INTENT(INOUT) :: A(1),Z(N),RSP(NSP)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER RATIO,Q,MARK,D,U,TMP,UMAX,IJU,IU,IL,JL,JU,JUMAX
C
C-----------------------------------------------------------------------
C
      DATA RATIO/2/
C
C----VALIDATES PATH SPECIFICATION
C
      IF(PATH.LT.1.OR.6.LT.PATH) GO TO 111
C
C----ALLOCATES STORAGE AND FACTORS M SYMBOLICALLY TO DETERMINE FILL-IN
C
      IJU   = 1
      IU    = IJU     +  N
      JL    = IU      +  N+1
      JU    = JL      +  N
      Q     = (NSP+1) -  N
      MARK  = Q       -  N
      JUMAX = MARK    - JU
C
      IF((PATH-1) * (PATH-4) * (PATH-5) .NE. 0)  GO TO 1
      IF(JUMAX.LE.0) GO TO 110
      CALL SD_SSF(N,P,IP,IA,JA,ISP(IJU),ISP(JU),ISP(IU),JUMAX,
     &            ISP(Q),ISP(MARK),ISP(JL),FLAG)
      IF (FLAG.NE.0) GO TO 100
C
C----ALLOCATES STORAGE AND FACTORS M NUMERICALLY
C
1     IL   = JU      + ISP(IJU+(N-1))
      TMP  = ((IL-1)+(RATIO-1)) / RATIO  +  1
      D    = TMP     + N
      U    = D       + N
      UMAX = (NSP+1) - U
      ESP  = UMAX    - (ISP(IU+N)-1)
C
      IF ((PATH-1) * (PATH-2) * (PATH-5) * (PATH-6) .NE. 0)  GO TO 2
      IF (UMAX.LE.0)  GO TO 110
      CALL SD_SNF(N,P,IP,IA,JA,A,
     &            RSP(D),ISP(IJU),ISP(JU),ISP(IU),RSP(U),UMAX,
     &            ISP(IL),ISP(JL),FLAG)
      IF(FLAG.NE.0)  GO TO 100
C
C----SOLVES SYSTEM OF LINEAR EQUATIONS  MX = B
C
2     IF((PATH-1) * (PATH-2) * (PATH-3) .NE. 0)  GO TO 3
      IF (UMAX.LE.0)  GO TO 110
      CALL SD_SNS(N,P,RSP(D),ISP(IJU),ISP(JU),
     &            ISP(IU),RSP(U),Z,B,RSP(TMP))
C
3     RETURN
C
C ** ERROR -- ERROR DETECTED IN SSF, SNF, OR SNS
C
100   RETURN
C
C ** ERROR -- INSUFFICIENT STORAGE
C
110   FLAG = 10*N + 1
      RETURN
C
C ** ERROR -- ILLEGAL PATH SPECIFICATION
C
111   FLAG = 11*N + 1
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C