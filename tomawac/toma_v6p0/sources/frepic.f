C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE PEAK FREQUENCY FOR ALL THE NODES IN THE
!>                2D MESH. THIS PEAK FREQUENCY IS DEFINED AS THE
!>                DISCRETISED FREQUENCY FOR WHICH E(F) IS GREATEST.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> E, EMAX, F, FPIC, FREQ, NF, NPLAN, NPOIN2
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IP, JF, JP
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
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>E
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>E(
!></td><td><-></td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>EMAX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>EMAX(
!></td><td><-></td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>F
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F(
!></td><td>--></td><td>SPECTRE DIRECTIONNEL DE VARIANCE
!>    </td></tr>
!>          <tr><td>FPIC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FPIC(
!></td><td><--</td><td>TABLEAU DES FREQUENCES DE PIC
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
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE SPATIAL 2D
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FREPIC
     &( FPIC  , F     , FREQ  , NF    , NPLAN , NPOIN2, EMAX  , E     )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| E             |---| 
C| E(             |<->| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| EMAX           |---| 
C| EMAX(          |<->| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| F             |---| 
C| F(             |-->| SPECTRE DIRECTIONNEL DE VARIANCE
C| FPIC           |---| 
C| FPIC(          |<--| TABLEAU DES FREQUENCES DE PIC
C| FREQ           |---| 
C| FREQ(          |-->| TABLEAU DES FREQUENCES DE DISCRETISATION
C| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
C| NPLAN          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL 2D
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER  NF    , NPLAN , NPOIN2
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF), FREQ(NF)  , FPIC(NPOIN2)
      DOUBLE PRECISION EMAX(NPOIN2),E(NPOIN2)
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER  JP    , JF    , IP
C
C
      DO 10 IP = 1,NPOIN2
        FPIC(IP) = 1.D-20
        EMAX(IP) = 0.D0
   10 CONTINUE
C
C.....LOOP OVER DISCRETISED FREQUENCIES
C     """""""""""""""""""""""""""""""""""""""""""""
      DO 20 JF = 1,NF
C
C.......INTEGRATES WRT DIRECTIONS TO GET E(F)
C       """""""""""""""""""""""""""""""""""""""""""""""""
        DO 60 IP=1,NPOIN2
          E(IP) = 0.D0
   60   CONTINUE
        DO 30 JP = 1,NPLAN
          DO 40 IP=1,NPOIN2
                 E(IP) = E(IP) + F(IP,JP,JF)
   40     CONTINUE
   30   CONTINUE
C
C.......KEEPS THE MAXIMUM VALUE FOR E(F) AND ASSOCIATED FREQUENCY
C       """""""""""""""""""""""""""""""""""""""""""""""""""""
        DO 50 IP=1,NPOIN2
          IF (E(IP).GT.EMAX(IP)) THEN
            EMAX(IP) = E(IP)
            FPIC(IP) = FREQ(JF)
          ENDIF
   50   CONTINUE
C
   20 CONTINUE
C
      RETURN
      END
C
C#######################################################################
C