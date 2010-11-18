C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MINIMUM DEGREE ALGORITHM (BASED ON ELEMENT MODEL).
!><br>            MD FINDS A MINIMUM DEGREE ORDERING OF THE ROWS AND
!>                COLUMNS OF A SYMMETRICAL MATRIX M STORED IN (IA,JA,A) FORMAT.
!>  @code
!>  PARAMETERS
!>
!>    MAX  - DECLARED DIMENSION OF THE ONE-DIMENSIONAL ARRAYS V AND L;
!>           MAX MUST BE AT LEAST  N+2K,  WHERE K IS THE NUMBER OF
!>           NONZEROES IN THE STRICT UPPER TRIANGLE OF M
!>
!>    V    - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = MAX
!>
!>    L    - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = MAX
!>
!>    HEAD - INTEGER ONE-DIMENSIONAL WORK ARRAY;  DIMENSION = N
!>
!>    LAST - INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN THE PERMUTATION
!>           OF THE ROWS AND COLUMNS OF M CORRESPONDING TO THE MINIMUM
!>           DEGREE ORDERING;  DIMENSION = N
!>
!>    NEXT - INTEGER ONE-DIMENSIONAL ARRAY USED TO RETURN THE INVERSE OF
!>           THE PERMUTATION RETURNED IN LAST;  DIMENSION = N
!>
!>    MARK - INTEGER ONE-DIMENSIONAL WORK ARRAY (MAY BE THE SAME AS V);
!>           DIMENSION = N
!>
!>    FLAG - INTEGER ERROR FLAG;  VALUES AND THEIR MEANINGS ARE -
!>             0      NO ERRORS DETECTED
!>             11N+1  INSUFFICIENT STORAGE IN MD
!>
!>
!>  DEFINITIONS OF INTERNAL PARAMETERS
!>
!>    ---------+---------------------------------------------------------
!>    V(S)     \ VALUE FIELD OF LIST ENTRY
!>    ---------+---------------------------------------------------------
!>    L(S)     \ LINK FIELD OF LIST ENTRY  (0 => END OF LIST)
!>    ---------+---------------------------------------------------------
!>    L(VI)    \ POINTER TO ELEMENT LIST OF UNELIMINATED VERTEX VI
!>    ---------+---------------------------------------------------------
!>    L(EJ)    \ POINTER TO BOUNDARY LIST OF ACTIVE ELEMENT EJ
!>    ---------+---------------------------------------------------------
!>    HEAD(D)  \ VJ => VJ HEAD OF D-LIST D
!>             \  0 => NO VERTEX IN D-LIST D
!>             \          VI UNELIMINATED VERTEX
!>             \          VI IN EK           \       VI NOT IN EK
!>    ---------+-----------------------------+---------------------------
!>    NEXT(VI) \ UNDEFINED BUT NONNEGATIVE   \ VJ => VJ NEXT IN D-LIST
!>             \                             \  0 => VI TAIL OF D-LIST
!>    ---------+-----------------------------+---------------------------
!>    LAST(VI) \ (NOT SET UNTIL MDP)         \ -D => VI HEAD OF D-LIST D
!>             \-VK => COMPUTE DEGREE        \ VJ => VJ LAST IN D-LIST
!>             \ EJ => VI PROTOTYPE OF EJ    \  0 => VI NOT IN ANY D-LIST
!>             \  0 => DO NOT COMPUTE DEGREE \
!>    ---------+-----------------------------+---------------------------
!>    MARK(VI) \ MARK(VK)                    \ NONNEGATIVE TAG < MARK(VK)
!>
!>
!>             \                   VI ELIMINATED VERTEX
!>             \      EI ACTIVE ELEMENT      \           OTHERWISE
!>    ---------+-----------------------------+---------------------------
!>    NEXT(VI) \ -J => VI WAS J-TH VERTEX    \ -J => VI WAS J-TH VERTEX
!>             \       TO BE ELIMINATED      \       TO BE ELIMINATED
!>    ---------+-----------------------------+---------------------------
!>    LAST(VI) \  M => SIZE OF EI = M        \ UNDEFINED
!>    ---------+-----------------------------+---------------------------
!>    MARK(VI) \ -M => OVERLAP COUNT OF EI   \ UNDEFINED
!>             \       WITH EK = M           \
!>             \ OTHERWISE NONNEGATIVE TAG   \
!>             \       < MARK(VK)            \
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
!>    </th><td> FLAG, HEAD, IA, JA, L, LAST, MARK, MAX, N, NEXT, V
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DMIN, EK, K, TAG, TAIL, VK
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SD_MD
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> SD_MDI(), SD_MDM(), SD_MDP(), SD_MDU()
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
!>          <tr><td>FLAG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HEAD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>JA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>L
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LAST
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MARK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>N
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NEXT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                         SUBROUTINE SD_MD
     &(N,IA,JA,MAX,V,L,HEAD,LAST,NEXT,MARK,FLAG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| FLAG           |---| 
C| HEAD           |---| 
C| IA             |---| 
C| JA             |---| 
C| L             |---| 
C| LAST           |---| 
C| MARK           |---| 
C| MAX            |---| 
C| N             |---| 
C| NEXT           |---| 
C| V             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_SD_MD => SD_MD
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: N,MAX
      INTEGER, INTENT(INOUT) :: IA(*),JA(*),V(MAX),L(MAX),HEAD(N)
      INTEGER, INTENT(INOUT) :: LAST(N),NEXT(N),MARK(N),FLAG
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER TAG,DMIN,VK,EK,TAIL,K
      EQUIVALENCE  (VK,EK)
C
C-----------------------------------------------------------------------
C
C----INITIALISES
C
      TAG = 0
      CALL SD_MDI(N,IA,JA,MAX,V,L,HEAD,LAST,NEXT,MARK,TAG,FLAG)
      IF(FLAG.NE.0)  RETURN
C
      K = 0
      DMIN = 1
C
C----WHILE  K
C
1     IF(K.GE.N) GO TO 4
C
C------SEARCHES FOR VERTEX OF MINIMUM DEGREE
C
2     IF(HEAD(DMIN).GT.0)  GO TO 3
      DMIN = DMIN + 1
      GO TO 2
C
C------REMOVES VERTEX VK OF MINIMUM DEGREE FROM DEGREE LIST
C
3     VK = HEAD(DMIN)
      HEAD(DMIN) = NEXT(VK)
      IF (HEAD(DMIN).GT.0) LAST(HEAD(DMIN)) = -DMIN
C
C------NUMBERS VERTEX VK, ADJUSTS TAG, AND TAGS VK
C
      K = K+1
      NEXT(VK) = -K
      LAST(EK) = DMIN - 1
      TAG = TAG + LAST(EK)
      MARK(VK) = TAG
C
C------FORMS ELEMENT EK FROM UNELIMINATED NEIGHBOURS OF VK
C
      CALL SD_MDM(VK,TAIL,V,L,LAST,NEXT,MARK)
C
C------PURGES INACTIVE ELEMENTS AND DOES MASS ELIMINATION
C
      CALL SD_MDP(K,EK,TAIL,V,L,HEAD,LAST,NEXT,MARK)
C
C------UPDATES DEGREES OF UNELIMINATED VERTICES IN EK
C
      CALL SD_MDU(EK,DMIN,V,L,HEAD,LAST,NEXT,MARK)
C
      GO TO 1
C
C----GENERATES INVERSE PERMUTATION FROM PERMUTATION
C
4     DO 5 K=1,N
      NEXT(K) = -NEXT(K)
5     LAST(NEXT(K)) = K
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C