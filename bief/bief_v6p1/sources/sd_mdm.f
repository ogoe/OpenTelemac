C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       FORMS ELEMENT FROM UNELIMINATED NEIGHBOURS OF VK.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  IMPORTANT : INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> L, LAST, MARK, NEXT, TAIL, V, VK
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> B, BLP, BLPMAX, ES, LB, LS, S, TAG, VB, VS
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SD_MDM
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
!>          <tr><td>L
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LAST
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MARK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NEXT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAIL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VK
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                         SUBROUTINE SD_MDM
     &(VK,TAIL,V,L,LAST,NEXT,MARK)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| L             |---| 
C| LAST           |---| 
C| MARK           |---| 
C| NEXT           |---| 
C| TAIL           |---| 
C| V             |---| 
C| VK             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_SD_MDM => SD_MDM
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: VK,LAST(*),NEXT(*),V(*)
      INTEGER, INTENT(INOUT) :: TAIL,L(*),MARK(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER TAG,S,LS,VS,ES,B,LB,VB,BLP,BLPMAX
      EQUIVALENCE (VS,ES)
C
C----INITIALISES TAG AND LIST OF UNELIMINATED NEIGHBOURS
C
      TAG = MARK(VK)
      TAIL = VK
C
C----FOR EACH VERTEX/ELEMENT VS/ES IN ELEMENT LIST OF VK
C
      LS = L(VK)
1     S = LS
      IF(S.EQ.0)  GO TO 5
      LS = L(S)
      VS = V(S)
      IF(NEXT(VS).LT.0)  GO TO 2
C
C------IF VS IS UNELIMINATED VERTEX, THEN TAGS AND APPENDS TO LIST OF
C------UNELIMINATED NEIGHBOURS
C
      MARK(VS) = TAG
      L(TAIL) = S
      TAIL = S
      GO TO 4
C
C------IF ES IS ACTIVE ELEMENT, THEN ...
C--------FOR EACH VERTEX VB IN BOUNDARY LIST OF ELEMENT ES
C
2     LB = L(ES)
      BLPMAX = LAST(ES)
      DO 3 BLP=1,BLPMAX
        B = LB
        LB = L(B)
        VB = V(B)
C
C----------IF VB IS UNTAGGED VERTEX, THEN TAGS AND APPENDS TO LIST OF
C----------UNELIMINATED NEIGHBOURS
C
        IF(MARK(VB).GE.TAG)  GO TO 3
        MARK(VB) = TAG
        L(TAIL) = B
        TAIL = B
3     CONTINUE
C
C--------MARKS ES INACTIVE
C
      MARK(ES) = TAG
C
4     GO TO 1
C
C----TERMINATES LIST OF UNELIMINATED NEIGHBOURS
C
5     L(TAIL) = 0
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C