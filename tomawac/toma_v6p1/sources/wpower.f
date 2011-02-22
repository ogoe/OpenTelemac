C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE VARIANCE OF THE DIRECTIONAL SPECTRUM FOR
!>                ALL THE NODES IN THE 2D MESH. IT IS COMPUTED BY
!>                INTEGRATION OVER FREQUENCIES AND DIRECTIONS AND CAN
!>                TAKE THE HIGH FREQUENCY PART OF THE SPECTRUM INTO
!>                ACCOUNT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  THE HIGH-FREQUENCY PART OF THE SPECTRUM IS ONLY CONSIDERED
!>         IF THE TAIL FACTOR (TAILF) IS STRICTLY GREATER THAN 1.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CG, DFREQ, F, FREQ, GRAVIT, NF, NPLAN, NPOIN2, POWER, ROEAU, TAILF
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX1, DTETAR, IP, JF, JP, ROGER
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DUMP2D()

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
!>      <td><center> 5.3                                       </center>
!> </td><td> 20/05/2003
!> </td><td> M. BENOIT
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CG(
!></td><td>--></td><td>TABLEAU DES VITESSES DE GROUPE
!>    </td></tr>
!>          <tr><td>DFREQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DFREQ(
!></td><td>--></td><td>TABLEAU DES PAS DE FREQUENCE
!>    </td></tr>
!>          <tr><td>F
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F(
!></td><td>--></td><td>SPECTRE DIRECTIONNEL DE VARIANCE
!>    </td></tr>
!>          <tr><td>FREQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FREQ(
!></td><td>--></td><td>TABLEAU DES FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>GRAVIT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE DIRECTIONS DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE SPATIAL 2D
!>    </td></tr>
!>          <tr><td>POWER
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ROEAU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAILF
!></td><td>--></td><td>FACTEUR DE QUEUE DU SPECTRE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE WPOWER
     &( POWER , F     , FREQ  , DFREQ , CG    , TAILF , NF    , NPLAN ,
     &  NPOIN2, ROEAU , GRAVIT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CG             |---| 
C| CG(            |-->| TABLEAU DES VITESSES DE GROUPE
C| DFREQ          |---| 
C| DFREQ(         |-->| TABLEAU DES PAS DE FREQUENCE
C| F             |---| 
C| F(             |-->| SPECTRE DIRECTIONNEL DE VARIANCE
C| FREQ           |---| 
C| FREQ(          |-->| TABLEAU DES FREQUENCES DE DISCRETISATION
C| GRAVIT         |---| 
C| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
C| NPLAN          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL 2D
C| POWER          |---| 
C| ROEAU          |---| 
C| TAILF          |-->| FACTEUR DE QUEUE DU SPECTRE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER          NF    , NPLAN , NPOIN2
      DOUBLE PRECISION TAILF , POWER(NPOIN2), FREQ(NF), DFREQ(NF)
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF), CG(NPOIN2,NF)
      DOUBLE PRECISION GRAVIT, ROEAU
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER          IP    , JP    , JF
      DOUBLE PRECISION AUX1  , DTETAR, ROGER
C
C
      DTETAR=2.D0*3.14159265D0/DBLE(NPLAN)
      ROGER=ROEAU*GRAVIT/1000.D0
      DO IP=1,NPOIN2
        POWER(IP)=0.D0
      ENDDO
C
C-----C-------------------------------------------------------C
C-----C  SUMS UP THE DISCRETISED PART OF THE SPECTRUM         C
C-----C-------------------------------------------------------C
      DO JF=1,NF
        AUX1=DFREQ(JF)*DTETAR
        DO JP=1,NPLAN
          DO IP=1,NPOIN2
            POWER(IP) = POWER(IP) + F(IP,JP,JF)*CG(IP,JF)*AUX1
          ENDDO
        ENDDO
      ENDDO
C
C-----C-------------------------------------------------------------C
C-----C  TAKES THE HIGH FREQUENCY PART INTO ACCOUNT (OPTIONAL)      C
C-----C-------------------------------------------------------------C
      IF (TAILF.GT.1.D0) THEN
        AUX1=DTETAR*GRAVIT/(4.D0*3.14159265D0*TAILF)
        DO JP=1,NPLAN
          DO IP=1,NPOIN2
            POWER(IP)=POWER(IP) + F(IP,JP,NF)*AUX1
          ENDDO
        ENDDO
      ENDIF
C
C-----C-------------------------------------------------------------C
C-----C  CONVERTS TO KW/M  (MULTIPLIES BY RO.G/1000)                C
C-----C-------------------------------------------------------------C
      DO IP=1,NPOIN2
        POWER(IP)=POWER(IP)*ROGER
      ENDDO
C
      RETURN
      END
C
C#######################################################################
C