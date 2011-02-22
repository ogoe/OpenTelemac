C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       UPDATES DEGREES OF UNELIMINATED VERTICES IN EK.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  IMPORTANT : INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DMIN, EK, HEAD, L, LAST, MARK, NEXT, V
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> B, BLP, BLPMAX, DVI, ES, EVI, I, ILP, ILPMAX, S, TAG, VB, VI, VS
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SD_MDU
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
!>          <tr><td>DMIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>EK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HEAD
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
                          SUBROUTINE SD_MDU
     &(EK,DMIN,V,L,HEAD,LAST,NEXT,MARK)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DMIN           |---| 
C| EK             |---| 
C| HEAD           |---| 
C| L             |---| 
C| LAST           |---| 
C| MARK           |---| 
C| NEXT           |---| 
C| V             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_SD_MDU => SD_MDU
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    ::  EK,V(*),L(*)
      INTEGER, INTENT(INOUT) ::  DMIN,HEAD(*),LAST(*),NEXT(*),MARK(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER TAG,VI,EVI,DVI,S,VS,ES,B,VB,ILP,ILPMAX,BLP,BLPMAX,I
      EQUIVALENCE (VS,ES)
C
C----INITIALISES TAG
C
      TAG = MARK(EK) - LAST(EK)
C
C----FOR EACH VERTEX VI IN EK
C
      I = EK
      ILPMAX = LAST(EK)
      IF(ILPMAX.LE.0)  GO TO 11
      DO 10 ILP=1,ILPMAX
        I = L(I)
        VI = V(I)
        IF (LAST(VI))  1, 10, 8
C
C------IF VI NEITHER PROTOTYPE NOR DUPLICATE VERTEX, THEN MERGES ELEMENTS
C------TO COMPUTE DEGREE
C
1       TAG = TAG + 1
        DVI = LAST(EK)
C
C--------FOR EACH VERTEX/ELEMENT VS/ES IN ELEMENT LIST OF VI
C
        S = L(VI)
2       S = L(S)
        IF(S.EQ.0) GO TO 9
        VS = V(S)
        IF(NEXT(VS).LT.0)  GO TO 3
C
C----------IF VS IS UNELIMINATED VERTEX, THEN TAGS AND ADJUSTS DEGREE
C
        MARK(VS) = TAG
        DVI = DVI + 1
        GO TO 5
C
C----------IF ES IS ACTIVE ELEMENT, THEN EXPANDS
C------------CHECK FOR OUTMATCHED VERTEX
C
3       IF(MARK(ES).LT.0)  GO TO 6
C
C------------FOR EACH VERTEX VB IN ES
C
        B = ES
        BLPMAX = LAST(ES)
        DO 4 BLP=1,BLPMAX
          B = L(B)
          VB = V(B)
C
C--------------IF VB IS UNTAGGED, THEN TAGS AND ADJUSTS DEGREE
C
          IF(MARK(VB).GE.TAG)  GO TO 4
          MARK(VB) = TAG
          DVI = DVI + 1
4       CONTINUE
C
5       GO TO 2
C
C------ELSE IF VI IS OUTMATCHED VERTEX, THEN ADJUSTS OVERLAPS BUT DOES NOT
C------COMPUTE DEGREE
C
6       LAST(VI) = 0
        MARK(ES) = MARK(ES) - 1
7       S = L(S)
        IF(S.EQ.0)  GO TO 10
        ES = V(S)
        IF(MARK(ES).LT.0)  MARK(ES) = MARK(ES) - 1
        GO TO 7
C
C------ELSE IF VI IS PROTOTYPE VERTEX, THEN CALCULATES DEGREE BY
C------INCLUSION/EXCLUSION AND RESETS OVERLAP COUNT
C
8       EVI = LAST(VI)
        DVI = LAST(EK) + LAST(EVI) + MARK(EVI)
        MARK(EVI) = 0
C
C------INSERTS VI IN APPROPRIATE DEGREE LIST
C
9       NEXT(VI)  = HEAD(DVI)
        HEAD(DVI) = VI
        LAST(VI)  = -DVI
        IF(NEXT(VI).GT.0)  LAST(NEXT(VI)) = VI
        IF(DVI.LT.DMIN)  DMIN = DVI
C
10    CONTINUE
C
11    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C