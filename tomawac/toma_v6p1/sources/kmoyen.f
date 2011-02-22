C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE AVERAGE WAVE NUMBER FOR ALL THE NODES
!>                IN THE 2D MESH.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note   THE HIGH-FREQUENCY PART OF THE SPECTRUM IS ONLY CONSIDERED
!>          IF THE TAIL FACTOR (TAILF) IS STRICTLY GREATER THAN 1.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AUX1, AUX2, AUX3, DFREQ, F, FREQ, NF, NPLAN, NPOIN2, TAILF, XK, XKMOY
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX4, COEFF, CTE1, CTE2, IP, IPLAN, JF, PI, SEUIL
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SEMIMP()

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
!> </td><td> 04/04/95
!> </td><td> P. THELLIER; M. BENOIT (EDF/DER/LNH)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AUX1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AUX1(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>AUX2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AUX2(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>AUX3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AUX3(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
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
!></td><td>--></td><td>SPECTRE DIRECTIONNEL
!>    </td></tr>
!>          <tr><td>FREQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FREQ(
!></td><td>--></td><td>TABLEAU DES FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE DIRECTIONS DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE SPATIAL
!>    </td></tr>
!>          <tr><td>TAILF
!></td><td>--></td><td>FACTEUR DE QUEUE (TAILF = 4 OU 5)
!>    </td></tr>
!>          <tr><td>XK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XK(
!></td><td>--></td><td>TABLEAU DES NOMBRES D'ONDE
!>    </td></tr>
!>          <tr><td>XKMOY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XKMOY(
!></td><td><--</td><td>TABLEAU DES NOMBRES D'ONDE MOYEN
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE KMOYEN
     &( XKMOY , XK    , F     , FREQ  , DFREQ , TAILF , NF    , NPLAN ,
     &  NPOIN2, AUX1  , AUX2  , AUX3  )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AUX1           |---| 
C| AUX1(          |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| AUX2           |---| 
C| AUX2(          |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| AUX3           |---| 
C| AUX3(          |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| DFREQ          |---| 
C| DFREQ(         |-->| TABLEAU DES PAS DE FREQUENCE
C| F             |---| 
C| F(             |-->| SPECTRE DIRECTIONNEL
C| FREQ           |---| 
C| FREQ(          |-->| TABLEAU DES FREQUENCES DE DISCRETISATION
C| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
C| NPLAN          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL
C| TAILF          |-->| FACTEUR DE QUEUE (TAILF = 4 OU 5)
C| XK             |---| 
C| XK(            |-->| TABLEAU DES NOMBRES D'ONDE
C| XKMOY          |---| 
C| XKMOY(         |<--| TABLEAU DES NOMBRES D'ONDE MOYEN
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER  NF    , NPLAN , NPOIN2
      DOUBLE PRECISION TAILF
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF), XK(NPOIN2,NF)
      DOUBLE PRECISION FREQ(NF)  , DFREQ(NF) , XKMOY(NPOIN2)
      DOUBLE PRECISION AUX1(NPOIN2) , AUX2(NPOIN2) , AUX3(NPOIN2)
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER  IPLAN , JF    , IP
      DOUBLE PRECISION COEFF , PI    , SEUIL , CTE1  , CTE2  , AUX4
C
C
      PI = 3.141592654D0
      SEUIL = 1.D-20
      COEFF = SQRT(9.806D0)/(2.D0*PI)
      DO 30 IP = 1,NPOIN2
        AUX1(IP) = 0.D0
        AUX2(IP) = 0.D0
   30 CONTINUE
C
C.....SUMS UP THE CONTRIBUTIONS FOR THE DISCRETISED PART OF THE SPECTRUM
C     """"""""""""""""""""""""""""""""""""""""""""""""
      DO 20 JF = 1,NF
        AUX4=DFREQ(JF)
C
        DO 15 IP=1,NPOIN2
          AUX3(IP) = 0.D0
   15   CONTINUE
        DO 10 IPLAN = 1,NPLAN
          DO 5 IP=1,NPOIN2
            AUX3(IP) = AUX3(IP) + F(IP,IPLAN,JF)
    5     CONTINUE
   10   CONTINUE
C
        DO 25 IP = 1,NPOIN2
          AUX1(IP)=AUX1(IP)+AUX3(IP)*AUX4
          AUX2(IP)=AUX2(IP)+AUX3(IP)/SQRT(XK(IP,JF))*AUX4
   25   CONTINUE
C
   20 CONTINUE
C
C.....(OPTIONALLY) TAKES INTO ACCOUNT THE HIGH-FREQUENCY PART
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
      IF (TAILF.GT.1.D0) THEN
        CTE1=FREQ(NF)/(TAILF-1.D0)
        CTE2=COEFF/TAILF
      ELSE
        CTE1=0.D0
        CTE2=0.D0
      ENDIF
      DO 45 IP=1,NPOIN2
        AUX1(IP) = AUX1(IP) + AUX3(IP)*CTE1
        AUX2(IP) = AUX2(IP) + AUX3(IP)*CTE2
   45 CONTINUE
C
C.....COMPUTES THE AVERAGE WAVE NUMBER
C     """"""""""""""""""""""""""""""
      DO 50 IP=1,NPOIN2
        IF (AUX2(IP).LT.SEUIL) THEN
          XKMOY(IP) = 1.D0
        ELSE
          XKMOY(IP) = (AUX1(IP)/AUX2(IP))**2
        ENDIF
   50 CONTINUE
C
      RETURN
      END
C
C#######################################################################
C