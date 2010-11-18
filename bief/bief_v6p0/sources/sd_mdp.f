C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PURGES INACTIVE ELEMENTS AND DOES MASS ELIMINATION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  IMPORTANT : INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> EK, HEAD, K, L, LAST, MARK, NEXT, TAIL, V
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ES, EVI, FREE, I, ILP, ILPMAX, LI, LS, LVI, S, TAG, VI
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SD_MDP
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SD_MD()

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
!>          <tr><td>EK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HEAD
!></td><td>---</td><td>DIMENSION N
!>    </td></tr>
!>          <tr><td>K
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>L
!></td><td>---</td><td>DIMENSION MAX
!>    </td></tr>
!>          <tr><td>LAST
!></td><td>---</td><td>DIMENSION N
!>    </td></tr>
!>          <tr><td>MARK
!></td><td>---</td><td>DIMENSION N
!>    </td></tr>
!>          <tr><td>NEXT
!></td><td>---</td><td>DIMENSION N
!>    </td></tr>
!>          <tr><td>TAIL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V
!></td><td>---</td><td>DIMENSION MAX
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                           SUBROUTINE SD_MDP
     &(K,EK,TAIL,V,L,HEAD,LAST,NEXT,MARK)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| EK             |---| 
C| HEAD           |---| DIMENSION N
C| K             |-->| 
C| L             |---| DIMENSION MAX
C| LAST           |---| DIMENSION N
C| MARK           |---| DIMENSION N
C| NEXT           |---| DIMENSION N
C| TAIL           |---| 
C| V             |---| DIMENSION MAX
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_SD_MDP => SD_MDP
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: EK
      INTEGER, INTENT(INOUT) :: K,TAIL,V(*),L(*),HEAD(*)
      INTEGER, INTENT(INOUT) :: LAST(*),NEXT(*),MARK(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER TAG,FREE,LI,VI,LVI,EVI,S,LS,ES,ILP,ILPMAX,I
C
C----INITIALISES TAG
C
      TAG = MARK(EK)
C
C----FOR EACH VERTEX VI IN EK
C
      LI = EK
      ILPMAX = LAST(EK)
      IF(ILPMAX.LE.0)  GO TO 12
      DO 11 ILP=1,ILPMAX
        I = LI
        LI = L(I)
        VI = V(LI)
C
C------REMOVES VI FROM DEGREE LIST
C
        IF(LAST(VI).EQ.0) GO TO 3
        IF(LAST(VI).GT.0) GO TO 1
        HEAD(-LAST(VI)) = NEXT(VI)
        GO TO 2
1       NEXT(LAST(VI)) = NEXT(VI)
2       IF(NEXT(VI).GT.0)  LAST(NEXT(VI)) = LAST(VI)
C
C------REMOVES INACTIVE ITEMS FROM ELEMENT LIST OF VI
C
3       LS = VI
4       S = LS
        LS = L(S)
        IF(LS.EQ.0)  GO TO 6
        ES = V(LS)
        IF (MARK(ES).LT.TAG)  GO TO 5
        FREE = LS
        L(S) = L(LS)
        LS = S
5       GO TO 4
C
C------IF VI IS INTERIOR VERTEX, THEN REMOVES FROM LIST AND ELIMINATES
C
6       LVI = L(VI)
        IF(LVI.NE.0)  GO TO 7
        L(I) = L(LI)
        LI = I
C
        K = K+1
        NEXT(VI) = -K
        LAST(EK) = LAST(EK) - 1
        GO TO 11
C
C------ELSE ...
C--------CLASSIFIES VERTEX VI
C
7       IF (L(LVI).NE.0)  GO TO 9
        EVI = V(LVI)
        IF(NEXT(EVI).GE.0)  GO TO 9
        IF(MARK(EVI).LT.0)  GO TO 8
C
C----------IF VI IS PROTOTYPE VERTEX, THEN MARKS AS SUCH, INITIALISES
C----------OVERLAP COUNT FOR CORRESPONDING ELEMENT, AND MOVES VI TO END
C----------OF BOUNDARY LIST
C
        LAST(VI) = EVI
        MARK(EVI) = -1
        L(TAIL) = LI
        TAIL = LI
        L(I) = L(LI)
        LI = I
        GO TO 10
C
C----------ELSE IF VI IS DUPLICATE VERTEX, THEN MARKS AS SUCH AND ADJUSTS
C----------OVERLAP COUNT FOR CORRESPONDING ELEMENT
C
8       LAST(VI) = 0
        MARK(EVI) = MARK(EVI) - 1
        GO TO 10
C
C----------ELSE MARKS VI TO COMPUTE DEGREE
C
9       LAST(VI) = -EK
C
C--------INSERTS EK IN ELEMENT LIST OF VI
C
10      V(FREE) = EK
        L(FREE) = L(VI)
        L(VI) = FREE
11    CONTINUE
C
C----TERMINATES BOUNDARY LIST
C
12    L(TAIL) = 0
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C