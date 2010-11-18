C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       NUMERIC LDU-FACTORISATION OF SPARSE NONSYMMETRICAL
!>                MATRIX AND SOLUTION OF SYSTEM OF LINEAR EQUATIONS
!>              (UNCOMPRESSED POINTER STORAGE).
!>  @code
!>       INPUT VARIABLES:   N, R,C,IC, IA,JA,A, B, IL,JL,LMAX, IU,JU,UMAX
!>       OUTPUT VARIABLES:  Z, L,D,U, FLAG
!>
!>       PARAMETERS USED INTERNALLY:
!> FIA   \ ROW   - HOLDS INTERMEDIATE VALUES IN CALCULATION OF L, D, U.
!>       \           SIZE = N.
!> FIA   \ TMP   - HOLDS NEW RIGHT-HAND SIDE B' FOR SOLUTION OF THE
!>       \           EQUATION  UX = B'.
!>       \           SIZE = N.
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  IMPORTANT : INSPIRED FROM PACKAGE CMLIB3 - YALE UNIVERSITE-YSMP

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A, B, C, D, FLAG, IA, IC, IL, IU, JA, JL, JU, L, LMAX, N, R, ROW, TMP, U, UMAX, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> BSUM, DK, I, IMAX, IMIN, J, JMAX, JMIN, K, LI
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
!>          <tr><td>A
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>B
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>C
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>D
!></td><td>---</td><td>
!>    </td></tr>
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
!>          <tr><td>IU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>JA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>JL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>JU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>L
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>N
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>R
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ROW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TMP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Z
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                           SUBROUTINE SD_NNF
     &(N,R,C,IC,IA,JA,A,Z,B,IL,JL,L,LMAX,D,IU,JU,U,UMAX,ROW,TMP,FLAG)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |---| 
C| B             |---| 
C| C             |---| 
C| D             |---| 
C| FLAG           |---| 
C| IA             |---| 
C| IC             |---| 
C| IL             |---| 
C| IU             |---| 
C| JA             |---| 
C| JL             |---| 
C| JU             |---| 
C| L             |---| 
C| LMAX           |---| 
C| N             |---| 
C| R             |---| 
C| ROW            |---| 
C| TMP            |---| 
C| U             |---| 
C| UMAX           |---| 
C| Z             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER R(*),C(*),IC(*),IA(*),JA(*),N
      INTEGER IL(*),JL(*),LMAX,IU(*),JU(*),UMAX,FLAG
      DOUBLE PRECISION A(*),Z(*),B(*),L(*),D(*),U(*),ROW(*),TMP(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER LI,I,J,K,IMIN,IMAX,JMIN,JMAX
      DOUBLE PRECISION BSUM,DK
C
C-----------------------------------------------------------------------
C
C     CHECKS STORAGE
C
      IF(IL(N+1)-1.GT.LMAX) THEN
C       ERROR: INSUFFICIENT STORAGE FOR L
        FLAG = 4*N + 1
        RETURN
      ENDIF
      IF(IU(N+1)-1.GT.UMAX) THEN
C       ERROR: INSUFFICIENT STORAGE FOR U
        FLAG = 7*N + 1
        RETURN
      ENDIF
C
C     FOR EACH ROW
C
      DO 10 K=1,N
C
C       SETS THE INITIAL STRUCTURE OF ROW
C
        JMIN = IL(K)
        JMAX = IL(K+1) - 1
        IF(JMAX.GE.JMIN) THEN
C         IF L(K,M) .NE. 0, ROW(M)=0
          DO J=JMIN,JMAX
            ROW(JL(J)) = 0
          ENDDO
        ENDIF
        ROW(K) = 0
        JMIN = IU(K)
        JMAX = IU(K+1) - 1
        IF(JMAX.GE.JMIN) THEN
C         IF U(K,M) .NE. 0, ROW(M)=0
          DO J=JMIN,JMAX
            ROW(JU(J)) = 0
          ENDDO
        ENDIF
        JMIN = IA(R(K))
        JMAX = IA(R(K)+1) - 1
C       SETS ROW TO KTH ROW OF REORDERED A
        DO J=JMIN,JMAX
          ROW(IC(JA(J))) = A(J)
        ENDDO
C       INITIALISES BSUM
        BSUM = B(R(K))
C
C       ASSIGNS THE KTH ROW OF L AND ADJUSTS ROW, BSUM
        IMIN = IL(K)
        IMAX = IL(K+1) - 1
        IF(IMAX.GT.IMIN) THEN
          DO I=IMIN,IMAX
            LI = - ROW(JL(I))
C           IF L IS NOT REQUIRED, THEN COMMENT OUT THE FOLLOWING LINE
            L(I) = - LI
            BSUM = BSUM + LI * TMP(JL(I))
            JMIN = IU(JL(I))
            JMAX = IU(JL(I)+1) - 1
            IF(JMAX.GT.JMIN) THEN
              DO J=JMIN,JMAX
                ROW(JU(J)) = ROW(JU(J)) + LI * U(J)
              ENDDO
            ENDIF
          ENDDO
        ENDIF
C
C       ASSIGNS DIAGONAL D AND KTH ROW OF U, SETS TMP(K)
C
        IF(ROW(K).EQ.0) THEN
C         ERROR:  ZERO PIVOT
          FLAG = 8*N + K
          RETURN
        ENDIF
        DK = 1 / ROW(K)
        D(K) = DK
        TMP(K) = BSUM * DK
        JMIN = IU(K)
        JMAX = IU(K+1) - 1
        IF(JMAX.GE.JMIN) THEN
          DO J=JMIN,JMAX
            U(J) = ROW(JU(J)) * DK
          ENDDO
        ENDIF
10    CONTINUE
C
C     SOLVES  UX = TMP  BY BACK SUBSTITUTION
C
      K = N
      DO I=1,N
        BSUM = TMP(K)
        JMIN = IU(K)
        JMAX = IU(K+1) - 1
        IF(JMAX.GE.JMIN) THEN
          DO J=JMIN,JMAX
            BSUM = BSUM - U(J) * TMP(JU(J))
          ENDDO
        ENDIF
        TMP(K)  = BSUM
        Z(C(K)) = BSUM
        K = K-1
      ENDDO
C
      FLAG = 0
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C