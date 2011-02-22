C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       GIVEN A MATRIX A(1:N,1:N), WITH PHYSICAL DIMENSION NP
!>                BY NP, THIS ROUTINE REPLACES IT BY THE LU FACTORISATION
!>                OF A ROWWISE PERMUTATION OF ITSELF. A AND N ARE INPUT.
!>                A IS OUTPUT, ARRANGED AS IN EQUATION (2.3.14) ABOVE;
!>                INDX(1:N) IS AN OUTPUT VECTOR THAT RECORDS THE ROW
!>                PERMUTATION EFFECTED BY THE PARTIAL PIVOTING; D IS
!>                OUTPUT AS SIGMA1 DEPENDING ON WHETHER THE NUMBER OF
!>                ROW INTERCHANGES WAS EVEN OR ODD, RESPECTIVELY. THIS
!>                SUBROUTINE IS USED IN COMBINATION WITH LUBKSB TO SOLVE
!>                LINEAR EQUATIONS OR INVERT A MATRIX.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A, INDX, N, NP
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AAMAX, D, DUM, I, IMAX, J, K, VV, XSOM
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>INVMTX()

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
!> </td><td> 28/07/2006
!> </td><td> CHUN WANG
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
!>          <tr><td>INDX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>N
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NP
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                           SUBROUTINE LUDCMP
     &(A,N,NP,INDX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |---| 
C| INDX           |---| 
C| N             |---| 
C| NP             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: N,NP
      INTEGER, INTENT(INOUT)          :: INDX(N)
      DOUBLE PRECISION, INTENT(INOUT) :: A(NP,NP)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION D
      INTEGER I,IMAX,J,K
      DOUBLE PRECISION AAMAX,DUM,XSOM,VV(500)
C
C------------------------------------------------------------------------
C
      D=1.D0 ! NO ROW INTERCHANGES YET
C
C     LOOP OVER ROWS TO GET THE IMPLICIT SCALING INFORMATION
C
      DO I=1,N
        AAMAX=0.D0
        DO J=1,N
          IF(ABS(A(I,J)).GT.AAMAX) AAMAX=ABS(A(I,J))
        ENDDO
        IF(AAMAX.LT.1.D-20) THEN
          WRITE(LU,*) 'SINGULAR MATRIX IN LUDCMP'
          CALL PLANTE(1)
          STOP
        ENDIF
        VV(I)=1.D0/AAMAX ! SAVES THE SCALING
      ENDDO
C
C     LOOP OVER COLUMNS OF CROUT'S METHOD
C
      DO J=1,N
         DO I=1,J-1 ! EQUATION (2.3.12) EXCEPT FOR I = J
            XSOM=A(I,J)
            DO K=1,I-1
               XSOM=XSOM-A(I,K)*A(K,J)
            ENDDO
            A(I,J)=XSOM
         ENDDO
         AAMAX=0.D0 ! INITIALISES FOR THE SEARCH OF LARGEST PIVOT ELEMENT
         DO I=J,N   ! THIS IS I = J OF EQUATION (2.3.12) AND
                    ! I = J +1 : : : N OF EQUATION (2.3.13)
            XSOM=A(I,J)
            DO K=1,J-1
               XSOM=XSOM-A(I,K)*A(K,J)
            ENDDO
            A(I,J)=XSOM
            DUM=VV(I)*ABS(XSOM) ! FIGURE OF MERIT FOR THE PIVOT
            IF (DUM.GE.AAMAX) THEN ! IS IT BETTER THAN THE BEST SO FAR?
               IMAX=I
               AAMAX=DUM
            ENDIF
         ENDDO
         IF (J.NE.IMAX) THEN ! NEEDS TO INTERCHANGE ROWS?
            DO K=1,N
              DUM=A(IMAX,K)
              A(IMAX,K)=A(J,K)
              A(J,K)=DUM
            ENDDO
            D=-D !...AND CHANGES THE PARITY OF D
            VV(IMAX)=VV(J) ! ALSO INTERCHANGES THE SCALE FACTOR
         ENDIF
         INDX(J)=IMAX
         A(J,J)=MAX(A(J,J),1.D-20)
C
C  IF THE PIVOT ELEMENT IS 0 THE MATRIX IS SINGULAR (AT LEAST TO THE
C  PRECISION OF THE ALGORITHM)
C
         IF(J.NE.N) THEN ! DIVIDES BY THE PIVOT ELEMENT
           DUM=1.D0/A(J,J)
           DO I=J+1,N
             A(I,J)=A(I,J)*DUM
           ENDDO
         ENDIF
C
      ENDDO
C
C------------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C