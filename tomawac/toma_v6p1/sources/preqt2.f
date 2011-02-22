C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOURCE TERM RELATED TO NON-LINEAR INTERACTIONS
!>                BETWEEN FREQUENCY TRIPLETS.
!>                DEVELOPED FROM THE BOUSSINESQ EQUATIONS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> BDISPB, BDSSPB, INDI, NBD, NPLAN, TETA
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AP2, DTETA, EPS, IPL, NB1, NBPL, NBPU
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>WAC()

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
!>      <td><center> 5.0                                       </center>
!> </td><td> 11/06/98
!> </td><td> EDF/DER/LNH
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>BDISPB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BDSSPB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>COSTET(
!></td><td>--></td><td>VECTEUR DES COSINUS DES DIRECTIONS
!>    </td></tr>
!>          <tr><td>DEPTH(
!></td><td>--></td><td>TABLEAU DES PROFONDEURS (METRES)
!>    </td></tr>
!>          <tr><td>DFREQ(
!></td><td>--></td><td>TABLEAU DES PAS DE FREQUENCE
!>    </td></tr>
!>          <tr><td>F(
!></td><td>---</td><td>SPECTRE DIRECTIONNEL DE VARIANCE
!>    </td></tr>
!>          <tr><td>FREQ(
!></td><td>--></td><td>TABLEAU DES FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>GRAVIT
!></td><td>--></td><td>ACCELERATION DE LA PESANTEUR
!>    </td></tr>
!>          <tr><td>INDI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBD
!></td><td>---</td><td>
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
!>          <tr><td>RAISF
!></td><td>--></td><td>RAISON FREQUENTIELLE POUR DISCRETISATION
!>    </td></tr>
!>          <tr><td>SINTET(
!></td><td>--></td><td>VECTEUR DES   SINUS DES DIRECTIONS
!>    </td></tr>
!>          <tr><td>TETA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TETA(
!></td><td>--></td><td>VECTEUR DES DIRECTIONS DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>TSDER(
!></td><td><--</td><td>CONTRIBUTION TERME SOURCE - PARTIE DERIVEE
!>    </td></tr>
!>          <tr><td>TSTOT(
!></td><td><--</td><td>CONTRIBUTION TERME SOURCE - PARTIE TOTALE
!>    </td></tr>
!>          <tr><td>XK(
!></td><td>--></td><td>TABLEAU DES NOMBRES D'ONDE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PREQT2
     &( TETA  , NPLAN , BDISPB, BDSSPB, NBD , INDI )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| BDISPB         |---| 
C| BDSSPB         |---| 
C| COSTET(        |-->| VECTEUR DES COSINUS DES DIRECTIONS
C| DEPTH(         |-->| TABLEAU DES PROFONDEURS (METRES)
C| DFREQ(         |-->| TABLEAU DES PAS DE FREQUENCE
C| F(             |---| SPECTRE DIRECTIONNEL DE VARIANCE
C| FREQ(          |-->| TABLEAU DES FREQUENCES DE DISCRETISATION
C| GRAVIT         |-->| ACCELERATION DE LA PESANTEUR
C| INDI           |---| 
C| NBD            |---| 
C| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
C| NPLAN          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL
C| RAISF          |-->| RAISON FREQUENTIELLE POUR DISCRETISATION
C| SINTET(        |-->| VECTEUR DES   SINUS DES DIRECTIONS
C| TETA           |---| 
C| TETA(          |-->| VECTEUR DES DIRECTIONS DE DISCRETISATION
C| TSDER(         |<--| CONTRIBUTION TERME SOURCE - PARTIE DERIVEE
C| TSTOT(         |<--| CONTRIBUTION TERME SOURCE - PARTIE TOTALE
C| XK(            |-->| TABLEAU DES NOMBRES D'ONDE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER           NPLAN
      DOUBLE PRECISION  BDISPB , BDSSPB
      DOUBLE PRECISION  TETA(NPLAN)
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER           IPL
      DOUBLE PRECISION  AP2 , EPS , DTETA
C
      INTEGER           NBPL , NBPU, NBD, NB1
      INTEGER           INDI(NPLAN)
C
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""""
C
      DTETA = TETA(2)-TETA(1)
      EPS  = 1.D-5
      IF(BDSSPB.GE.BDISPB) THEN
         AP2  = (BDISPB-TETA(1))/DTETA
         NBPL = IDINT(AP2)
         AP2  = AP2 - DBLE(NBPL)
         IF(AP2.GT.EPS) THEN
            NBPL = NBPL + 2
         ELSE
            NBPL = NBPL + 1
         ENDIF
         AP2  = (BDSSPB-TETA(1))/DTETA
         NBPU = IDINT(AP2) + 1
         NBD=NBPU-NBPL+1
C         ALLOCATE(INDI(1:NBD))
         DO IPL=1,NBD
            INDI(IPL)=NBPL+IPL-1
         END DO
      ELSE
         AP2  = (BDSSPB-TETA(1))/DTETA
         NBPU = IDINT(AP2) + 1
         AP2  = (BDISPB-TETA(1))/DTETA
         NBPL = IDINT(AP2)
         AP2  = AP2 - DBLE(NBPL)
         IF(AP2.GT.EPS) THEN
            NBPL = NBPL + 2
         ELSE
            NBPL = NBPL + 1
         ENDIF
         IF(NBPL.GT.NPLAN) THEN
            NBPL = 1
            INDI(1) = 1
            NBD  = NBPU - NBPL + 1
C            ALLOCATE(INDI(1:NBD))
            DO IPL = 2,NBD
               INDI(IPL)=IPL
            END DO
         ELSE
            NB1 = NPLAN - NBPL + 1
            NBD = NB1 + NBPU
C            ALLOCATE(INDI(1:NBD))
            DO IPL = 1,NB1
               INDI(IPL)=NBPL+IPL-1
            END DO
            DO IPL = 1,NBPU
               INDI(IPL+NB1)=IPL
            END DO
         ENDIF
      ENDIF
C
      RETURN
      END
C
C#######################################################################
C