C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ESTABLISHES THE COEFFICIENT MATRICE USED FOR SPECTRUM
!>                ANALYSIS. THE THEORY EMPLOYED HERE IS THE LEAST MEAN
!>                SQUARE ERROR METHOD.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @reference "SIMULATION DES COURANTS DE MAREE EN MANCHE ET PROCHE ATLANTIQUE",
!>                       EDF REPORT, J. M. JANIN ET. AL., PP 27-28.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AM, DT, M, NPERIAF, PERIAF
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A, AA, B, BB, C, D, I, J, PI, W
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SPECTRE()

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
!>          <tr><td>AM
!></td><td><-></td><td>(2NPERIAF*2NPERIAF) COEFFICIENT MATRIX,
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>TIME INTERVAL.
!>    </td></tr>
!>          <tr><td>M
!></td><td>--></td><td>NUMBER OF SAMPLING POINTS
!>    </td></tr>
!>          <tr><td>NPERIAF
!></td><td>--></td><td>NUMBER OF WAVES
!>    </td></tr>
!>          <tr><td>PERIAF
!></td><td>--></td><td>PERIOD OF WAVES.
!>    </td></tr>
!>          <tr><td>W
!></td><td><--</td><td>Circular frequence.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                         SUBROUTINE COEFMAT
     &(PERIAF,DT,M,AM,NPERIAF)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AM             |<->| (2NPERIAF*2NPERIAF) COEFFICIENT MATRIX,
C| DT             |-->| TIME INTERVAL.
C| M             |-->| NUMBER OF SAMPLING POINTS
C| NPERIAF        |-->| NUMBER OF WAVES
C| PERIAF         |-->| PERIOD OF WAVES.
C| W             |<--| Circular frequence.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER,          INTENT(IN   ) :: NPERIAF,M
      DOUBLE PRECISION, INTENT(IN   ) :: DT,PERIAF(NPERIAF)
      DOUBLE PRECISION, INTENT(INOUT) :: AM(2*NPERIAF,2*NPERIAF)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C                        500>NPERIAF
      DOUBLE PRECISION W(500),PI
      DOUBLE PRECISION A,B,C,D,AA,BB
      INTEGER I,J
C
      INTRINSIC COS,SIN,ACOS
C
C-----------------------------------------------------------------------
C
      PI = ACOS(-1.D0)
C
      DO I = 1, NPERIAF
        W(I)=2.D0*PI/PERIAF(I)
      ENDDO
C
      DO I = 1, NPERIAF
         DO J = 1, NPERIAF
            AA = (W(I)+W(J))*DT
            BB = (W(I)-W(J))*DT
            A = (-1.D0+COS(AA)+COS(M*AA)-COS((M+1)*AA))/(2-2*COS(AA))
            C = (SIN(AA)+SIN(M*AA)-SIN((M+1)*AA))/(2-2*COS(AA))
            IF(I.EQ.J) THEN
              B = M*1.D0
              D = 0.D0
            ELSE
              B = (-1+COS(BB)+COS(M*BB)-COS((M+1)*BB))/(2-2*COS(BB))
              D = (SIN(BB)+SIN(M*BB)-SIN((M+1)*BB))/(2-2*COS(BB))
            ENDIF
            AM(I,J) = (A+B)/(2*M)
            AM(I,NPERIAF+J) = (C-D)/(2*M)
            AM(I+NPERIAF,J) = (C+D)/(2*M)
            AM(I+NPERIAF,NPERIAF+J) = (B-A)/(2*M)
         ENDDO
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C