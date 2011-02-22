C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE VARIANCE OF THE DIRECTIONAL SPECTRUM
!>                FOR ALL THE NODES IN THE 2D MESH. IT IS COMPUTED BY
!>                INTEGRATION OVER FREQUENCIES AND DIRECTIONS AND CAN
!>                TAKE THE HIGH FREQUENCY PART OF THE SPECTRUM INTO
!>                ACCOUNT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note   THE HIGH FREQUENCY PART OF THE SPECTRUM IS ONLY CONSIDERED
!>          IF THE TAIL FACTOR (TAILF) IS STRICTLY GREATER THAN 1.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DFREQ, F, FREQ, NF, NPLAN, NPOIN2, TAILF, VARIAN
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX1, DTETAR, IP, JF, JP
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DUMP2D(), SEMIMP()

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
!>      <td><center> 1.0                                       </center>
!> </td><td> 09/02/95
!> </td><td> P. THELLIER; M. BENOIT
!> </td><td> CREATED
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DFREQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FREQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAILF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VARIAN
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE TOTNRJ
     &( VARIAN, F     , FREQ  , DFREQ , TAILF , NF    , NPLAN , NPOIN2)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DFREQ          |---| 
C| F             |---| 
C| FREQ           |---| 
C| NF             |---| 
C| NPLAN          |---| 
C| NPOIN2         |---| 
C| TAILF          |---| 
C| VARIAN         |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER          NF    , NPLAN , NPOIN2
      DOUBLE PRECISION TAILF , VARIAN(NPOIN2), FREQ(NF), DFREQ(NF)
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF)
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER          IP    , JP    , JF
      DOUBLE PRECISION AUX1  , DTETAR
C
C
      DTETAR=2.D0*3.14159265D0/FLOAT(NPLAN)
      DO 30 IP = 1,NPOIN2
        VARIAN(IP) = 0.D0
30    CONTINUE
C
C-----C-------------------------------------------------------C
C-----C  SUMS UP THE DISCRETISED PART OF THE SPECTRUM         C
C-----C-------------------------------------------------------C
      DO 20 JF = 1,NF-1
        AUX1=DFREQ(JF)*DTETAR
        DO 10 JP = 1,NPLAN
          DO 5 IP=1,NPOIN2
            VARIAN(IP) = VARIAN(IP) + F(IP,JP,JF)*AUX1
    5     CONTINUE
   10   CONTINUE
   20 CONTINUE
C
C-----C-------------------------------------------------------------C
C-----C  TAKES THE HIGH FREQUENCY PART INTO ACCOUNT (OPTIONAL)      C
C-----C-------------------------------------------------------------C
      IF (TAILF.GT.1.D0) THEN
        AUX1=DTETAR*(DFREQ(NF)+FREQ(NF)/(TAILF-1.D0))
      ELSE
        AUX1=DTETAR*DFREQ(NF)
      ENDIF
      DO 40 JP = 1,NPLAN
        DO 45 IP=1,NPOIN2
          VARIAN(IP) = VARIAN(IP) + F(IP,JP,NF)*AUX1
   45   CONTINUE
   40 CONTINUE
C
      RETURN
      END
C
C#######################################################################
C