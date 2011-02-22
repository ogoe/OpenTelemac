C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SYMBOLIC LDU-FACTORISATION OF A NONSYMMETRICAL
!>                SPARSE MATRIX (UNCOMPRESSED POINTER STORAGE).
!>  @code
!>       INPUT VARIABLES:   N, R,IC, IA,JA, JLMAX, JUMAX.
!>       OUTPUT VARIABLES:  IL,JL, IU,JU, FLAG.
!>
!>       PARAMETERS USED INTERNALLY:
!> NIA   \ Q     - SUPPOSE M' IS THE RESULT OF REORDERING M;  IF
!>       \           PROCESSING OF THE KTH ROW OF M' (HENCE THE KTH ROWS
!>       \           OF L AND U) IS BEING DONE, THEN Q(J) IS INITIALLY
!>       \           NONZERO IF M'(K,J) IS NONZERO;  SINCE VALUES NEED
!>       \           NOT BE STORED, EACH ENTRY POINTS TO THE NEXT
!>       \           NONZERO;  FOR EXAMPLE, IF  N=9  AND THE 5TH ROW OF
!>       \           M' IS
!>       \                   0 X X 0 X 0 0 X 0,
!>       \           THEN Q WILL INITIALLY BE
!>       \                   A 3 5 A 8 A A 10 A 2        (A - ARBITRARY);
!>       \           Q(N+1) POINTS TO THE FIRST NONZERO IN THE ROW AND
!>       \           THE LAST NONZERO POINTS TO  N+1;  AS THE ALGORITHM
!>       \           PROCEEDS, OTHER ELEMENTS OF Q ARE INSERTED IN THE
!>       \           LIST BECAUSE OF FILLIN.
!>       \           SIZE = N+1.
!> NIA   \ IM    - AT EACH STEP IN THE FACTORIZATION, IM(I) IS THE LAST
!>       \           ELEMENT IN THE ITH ROW OF U WHICH NEEDS TO BE
!>       \           CONSIDERED IN COMPUTING FILLIN.
!>       \           SIZE = N.
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  IMPORTANT : INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> FLAG, IA, IC, IL, IM, IU, JA, JL, JLMAX, JU, JUMAX, N, Q, R
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, J, JLPTR, JMAX, JMIN, JUPTR, K, M, QM, VJ
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SD_NDRV()

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
!> </td><td> 18/02/08
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
!>          <tr><td>IA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>JA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>JL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>JLMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>JU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>JUMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>N
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Q
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>R
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                           SUBROUTINE SD_NSF
     &(N,R,IC,IA,JA,IL,JL,JLMAX,IU,JU,JUMAX,Q,IM,FLAG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| FLAG           |---| 
C| IA             |---| 
C| IC             |---| 
C| IL             |---| 
C| IM             |---| 
C| IU             |---| 
C| JA             |---| 
C| JL             |---| 
C| JLMAX          |---| 
C| JU             |---| 
C| JUMAX          |---| 
C| N             |---| 
C| Q             |---| 
C| R             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER N
      INTEGER R(*),IC(*),IA(*),JA(*),IL(*),JL(*)
      INTEGER IU(*),JU(*),Q(*),IM(*),FLAG,QM,VJ
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C     JLPTR - POINTS TO THE LAST POSITION USED IN  JL
C     JUPTR - POINTS TO THE LAST POSITION USED IN  JU
C
      INTEGER JLPTR,JUPTR,JLMAX,JUMAX,I,J,K,M,JMIN,JMAX
C
C-----------------------------------------------------------------------
C
C     INITIALISES POINTERS
C
      JLPTR = 0
      IL(1) = 1
      JUPTR = 0
      IU(1) = 1
C
C     FOR EACH ROW OF L AND U
C
      DO 10 K=1,N
C       SETS Q TO THE REORDERED ROW OF A
        Q(N+1) = N+1
        JMIN = IA(R(K))
        JMAX = IA(R(K)+1) - 1
        IF(JMIN.GT.JMAX) GO TO 101
        DO J=JMIN,JMAX
          VJ = IC(JA(J))
          QM = N+1
1         CONTINUE
          M = QM
          QM = Q(M)
          IF(QM.LT.VJ) GO TO 1
          IF(QM.EQ.VJ) GO TO 102
          Q(M) = VJ
          Q(VJ) = QM
        ENDDO
C
C       FOR EACH ENTRY IN THE LOWER TRIANGLE
C
        I = N+1
3       CONTINUE
        I = Q(I)
        IF(I.GE.K) GO TO 7
C       L(K,I) WILL BE NONZERO, SO ADDS IT TO JL
        JLPTR = JLPTR+1
        IF(JLPTR.GT.JLMAX) GO TO 103
        JL(JLPTR) = I
        QM = I
C       INSPECTS ITH ROW FOR FILLING, ADJUSTS IM IF POSSIBLE
        JMIN = IU(I)
        JMAX = IM(I)
        IF(JMIN.GT.JMAX)  GO TO 3
        DO 5 J=JMIN,JMAX
          VJ = JU(J)
          IF (VJ.EQ.K)  IM(I) = J
4         CONTINUE
          M = QM
          QM = Q(M)
          IF (QM.LT.VJ)  GO TO 4
          IF (QM.EQ.VJ)  GO TO 5
          Q(M) = VJ
          Q(VJ) = QM
          QM = VJ
5       CONTINUE
        GO TO 3
C
C       CHECKS FOR 0 PIVOT
C
7       CONTINUE
        IF(I.NE.K) GO TO 105
C       REMAINING ELEMENTS OF Q DEFINE STRUCTURE OF U(K, )
8       CONTINUE
        I = Q(I)
        IF(I.GT.N) GO TO 9
        JUPTR = JUPTR+1
        IF (JUPTR.GT.JUMAX)  GO TO 106
        JU(JUPTR) = I
        GO TO 8
C       GETS READY FOR NEXT ROW
9       CONTINUE
        IM(K) = JUPTR
        IL(K+1) = JLPTR+1
        IU(K+1) = JUPTR+1
10    CONTINUE
C
        FLAG = 0
        RETURN
C
C ** ERROR:  NULL ROW IN A
 101    FLAG = N + R(K)
        RETURN
C ** ERROR:  DUPLICATE ENTRY IN A
 102    FLAG = 2*N + R(K)
        RETURN
C ** ERROR:  INSUFFICIENT STORAGE FOR JL
 103    FLAG = 3*N + K
        RETURN
C ** ERROR:  0 PIVOT
 105    FLAG = 5*N + K
        RETURN
C ** ERROR:  INSUFFICIENT STORAGE FOR JU
 106    FLAG = 6*N + K
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C