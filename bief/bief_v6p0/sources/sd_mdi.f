C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIALISES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  IMPORTANT : INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> FLAG, HEAD, IA, JA, L, LAST, MARK, MAX, N, NEXT, TAG, V
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DVI, J, JMAX, JMIN, SFS, VI, VJ
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SD_MDI
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
!>          <tr><td>TAG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>V
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                          SUBROUTINE SD_MDI
     &(N,IA,JA,MAX,V,L,HEAD,LAST,NEXT,MARK,TAG,FLAG)
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
C| TAG            |---| 
C| V             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_SD_MDI => SD_MDI
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: N,MAX,IA(*),JA(*)
      INTEGER, INTENT(INOUT) :: V(*),L(*),HEAD(*),LAST(*)
      INTEGER, INTENT(INOUT) :: NEXT(*),MARK(*),TAG,FLAG
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER SFS,VI,DVI,VJ,JMIN,JMAX,J
C
C----INITIALISES DEGREES, ELEMENT LISTS, AND DEGREE LISTS
C
      DO 1 VI=1,N
        MARK(VI) = 1
        L(VI) = 0
        HEAD(VI) = 0
1     CONTINUE
      SFS = N+1
C
C----CREATES NONZERO STRUCTURE
C----FOR EACH NONZERO ENTRY A(VI,VJ) IN STRICT UPPER TRIANGLE
C
      DO 3 VI=1,N
        JMIN = IA(VI)
        JMAX = IA(VI+1) - 1
        IF(JMIN.GT.JMAX)  GO TO 3
        DO 2 J=JMIN,JMAX
          VJ = JA(J)
          IF(VI.GE.VJ) GO TO 2
          IF(SFS.GE.MAX) GO TO 101
C
C------ENTERS VJ IN ELEMENT LIST FOR VI
C
          MARK(VI) = MARK(VI) + 1
          V(SFS) = VJ
          L(SFS) = L(VI)
          L(VI) = SFS
          SFS = SFS+1
C
C------ENTERS VI IN ELEMENT LIST FOR VJ
C
          MARK(VJ) = MARK(VJ) + 1
          V(SFS) = VI
          L(SFS) = L(VJ)
          L(VJ) = SFS
          SFS = SFS+1
2       CONTINUE
3     CONTINUE
C
C----CREATES DEGREE LISTS AND INITIALISES MARK VECTOR
C
      DO 4 VI=1,N
        DVI = MARK(VI)
        NEXT(VI) = HEAD(DVI)
        HEAD(DVI) = VI
        LAST(VI) = -DVI
        IF(NEXT(VI).GT.0)  LAST(NEXT(VI)) = VI
        MARK(VI) = TAG
4     CONTINUE
C
      RETURN
C
C ** ERROR -- INSUFFICIENT STORAGE
C
101   FLAG = 9*N + VI
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C