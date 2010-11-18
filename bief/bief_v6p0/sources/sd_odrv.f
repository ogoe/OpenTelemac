C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       DRIVER FOR SPARSE MATRIX REORDERING ROUTINE.
!>  @code
!>    ODRV FINDS A MINIMUM DEGREE ORDERING OF THE ROWS AND COLUMNS OF A
!>    SYMMETRIC MATRIX M STORED IN (IA,JA,A) FORMAT (SEE BELOW).  FOR THE
!>    REORDERED MATRIX, THE WORK AND STORAGE REQUIRED TO PERFORM GAUSSIAN
!>    ELIMINATION IS (USUALLY) SIGNIFICANTLY LESS.<br>
!>    IF ONLY THE NONZERO ENTRIES IN THE UPPER TRIANGLE OF M ARE BEING
!>    STORED, THEN ODRV SYMMETRICALLY REORDERS (IA,JA,A), (OPTIONALLY)
!>    WITH THE DIAGONAL ENTRIES PLACED FIRST IN EACH ROW.  THIS IS TO
!>    ENSURE THAT IF M(I,J) WILL BE IN THE UPPER TRIANGLE OF M WITH
!>    RESPECT TO THE NEW ORDERING, THEN M(I,J) IS STORED IN ROW I (AND
!>    THUS M(J,I) IS NOT STORED);  WHEREAS IF M(I,J) WILL BE IN THE
!>    STRICT LOWER TRIANGLE OF M, THEN M(J,I) IS STORED IN ROW J (AND
!>    THUS M(I,J) IS NOT STORED).<br><br>
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
!>    IN THE UPPER TRIANGLE NEED BE STORED.  FOR EXAMPLE, THE MATRIX<br>
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
!>          A \ 1  2  3  4  5  6  7  8  9
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
!>    </th><td> A, FLAG, IA, IP, ISP, JA, N, NSP, P, PATH
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DFLAG, HEAD, L, MAX, NEXT, Q, TMP, V
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SD_ODRV
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> SD_MD(), SD_SRO()
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
!>          <tr><td>FLAG
!></td><td><--</td><td>INTEGER ERROR FLAG;  VALUES AND THEIR MEANINGS ARE -
!>                  0    NO ERRORS DETECTED
!>                  9N+K  INSUFFICIENT STORAGE IN MD
!>                  10N+1  INSUFFICIENT STORAGE IN ODRV
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
!>                  1  FIND MINIMUM DEGREE ORDERING ONLY
!>                  2  FIND MINIMUM DEGREE ORDERING AND
!>                  REORDER SYMMETRICALLY
!>                  STORED MATRIX (USED WHEN ONLY THE NONZERO
!>                  ENTRIES IN THE UPPER TRIANGLE OF M ARE
!>                  BEING STORED)
!>                  3  REORDER SYMMETRICALLY STORED MATRIX AS
!>                  SPECIFIED BY INPUT PERMUTATION (USED WHEN
!>                  AN ORDERING HAS ALREADY BEEN DETERMINED
!>                  AND ONLY THE NONZERO ENTRIES IN THE
!>                  UPPER TRIANGLE OF M ARE BEING STORED)
!>                  4  SAME AS 2 BUT PUT DIAGONAL ENTRIES AT
!>                  START OF EACH ROW
!>                  5  SAME AS 3 BUT PUT DIAGONAL ENTRIES AT
!>                  START OF EACH ROW
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                         SUBROUTINE SD_ODRV
     &(N,IA,JA,A,P,IP,NSP,ISP,PATH,FLAG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |<--| REAL ONE-DIMENSIONAL ARRAY CONTAINING THE
C|                |   | NONZERO ENTRIES IN (THE UPPER TRIANGLE OF) M,
C|                |   | STORED BY ROWS;  DIMENSION =NUMBER OF NONZERO
C|                |   | ENTRIES IN (THE UPPER TRIANGLE OF) M
C| FLAG           |<--| INTEGER ERROR FLAG;  VALUES AND THEIR MEANINGS ARE -
C|                |   | 0    NO ERRORS DETECTED
C|                |   | 9N+K  INSUFFICIENT STORAGE IN MD
C|                |   | 10N+1  INSUFFICIENT STORAGE IN ODRV
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
C|                |   | 1  FIND MINIMUM DEGREE ORDERING ONLY
C|                |   | 2  FIND MINIMUM DEGREE ORDERING AND
C|                |   | REORDER SYMMETRICALLY
C|                |   | STORED MATRIX (USED WHEN ONLY THE NONZERO
C|                |   | ENTRIES IN THE UPPER TRIANGLE OF M ARE
C|                |   | BEING STORED)
C|                |   | 3  REORDER SYMMETRICALLY STORED MATRIX AS
C|                |   | SPECIFIED BY INPUT PERMUTATION (USED WHEN
C|                |   | AN ORDERING HAS ALREADY BEEN DETERMINED
C|                |   | AND ONLY THE NONZERO ENTRIES IN THE
C|                |   | UPPER TRIANGLE OF M ARE BEING STORED)
C|                |   | 4  SAME AS 2 BUT PUT DIAGONAL ENTRIES AT
C|                |   | START OF EACH ROW
C|                |   | 5  SAME AS 3 BUT PUT DIAGONAL ENTRIES AT
C|                |   | START OF EACH ROW
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_SD_ODRV => SD_ODRV
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: N,NSP,PATH
      INTEGER, INTENT(INOUT)          :: FLAG
      INTEGER, INTENT(INOUT)          :: IA(N),JA(*),P(N),IP(N),ISP(NSP)
      DOUBLE PRECISION, INTENT(INOUT) :: A(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER V,L,HEAD,TMP,Q,NEXT,MAX
      LOGICAL DFLAG
C
C-----------------------------------------------------------------------
C
C----INITIALISES ERROR FLAG AND VALIDATES PATH SPECIFICATION
C
      FLAG = 0
      IF(PATH.LT.1.OR.5.LT.PATH) GO TO 111
C
C----ALLOCATES STORAGE AND FINDS MINIMUM DEGREE ORDERING
C
      IF((PATH-1)*(PATH-2)*(PATH-4).NE.0) GO TO 1
      MAX = (NSP-N)/2
      V    = 1
      L    = V     +  MAX
      HEAD = L     +  MAX
      NEXT = HEAD  +  N
      IF(MAX.LT.N) GO TO 110
C
      CALL SD_MD(N,IA,JA,MAX,ISP(V),ISP(L),ISP(HEAD),P,IP,ISP(V),FLAG)
C
      IF(FLAG.NE.0) GO TO 100
C
C----ALLOCATES STORAGE AND SYMMETRICALLY REORDERS MATRIX
C
1     IF ((PATH-2) * (PATH-3) * (PATH-4) * (PATH-5) .NE. 0)  GO TO 2
C
      TMP = (NSP+1) -      N
      Q   = TMP     - (IA(N+1)-1)
      IF (Q.LT.1)  GO TO 110
C
      DFLAG = PATH.EQ.4 .OR. PATH.EQ.5
      CALL SD_SRO(N,IP,IA,JA,A,ISP(TMP),ISP(Q),DFLAG)
C
2     RETURN
C
C ** ERROR -- ERROR DETECTED IN MD
C
100   RETURN
C
C ** ERROR -- INSUFFICIENT STORAGE
C
110   FLAG = 10*N + 1
      RETURN
C
C ** ERROR -- ILLEGAL PATH SPECIFIED
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