C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE PEAK FREQUENCY OF THE VARIANCE SPECTRUM
!>                USING THE SO-CALLED READ METHOD.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DENOM, DFREQ, E, EXPO, F, FREAD, FREQ, NF, NPLAN, NPOIN2, TAILF
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUXI, COEFD, COEFN, DTETAR, IP, JF, JP, SEUIL
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
!>      <td><center> 1.2                                       </center>
!> </td><td> 05/07/96
!> </td><td> M. BENOIT
!> </td><td> MODIFIED
!> </td></tr>
!>      <tr>
!>      <td><center> 1.1                                       </center>
!> </td><td> 30/01/96
!> </td><td> M. BENOIT
!> </td><td> CREATED
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DENOM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DENOM(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>DFREQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DFREQ(
!></td><td>--></td><td>TABLEAU DES PAS DE FREQUENCES
!>    </td></tr>
!>          <tr><td>E
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>E(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>EXPO
!></td><td>--></td><td>EXPOSANT DE LA METHODE DE READ
!>    </td></tr>
!>          <tr><td>F
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F(
!></td><td>--></td><td>SPECTRE DIRECTIONNEL DE VARIANCE
!>    </td></tr>
!>          <tr><td>FREAD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FREAD(
!></td><td><--</td><td>TABLEAU DES FREQUENCES DE PIC (METH. READ)
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
!>          <tr><td>TAILF
!></td><td>--></td><td>FACTEUR DE QUEUE DU SPECTRE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FPREAD
     &( FREAD , F     , FREQ  , DFREQ , NF    , NPLAN , NPOIN2, EXPO  ,
     &  TAILF , DENOM , E     )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DENOM          |---| 
C| DENOM(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| DFREQ          |---| 
C| DFREQ(         |-->| TABLEAU DES PAS DE FREQUENCES
C| E             |---| 
C| E(             |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| EXPO           |-->| EXPOSANT DE LA METHODE DE READ
C| F             |---| 
C| F(             |-->| SPECTRE DIRECTIONNEL DE VARIANCE
C| FREAD          |---| 
C| FREAD(         |<--| TABLEAU DES FREQUENCES DE PIC (METH. READ)
C| FREQ           |---| 
C| FREQ(          |-->| TABLEAU DES FREQUENCES DE DISCRETISATION
C| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
C| NPLAN          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL 2D
C| TAILF          |-->| FACTEUR DE QUEUE DU SPECTRE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER  NF    , NPLAN , NPOIN2
      DOUBLE PRECISION EXPO  , TAILF
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF), FREQ(NF), DFREQ(NF)
      DOUBLE PRECISION DENOM(NPOIN2), E(NPOIN2), FREAD(NPOIN2)
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER  JP    , JF    , IP
      DOUBLE PRECISION SEUIL , AUXI  , COEFN  , COEFD , DTETAR
C
C
      SEUIL =1.D-20
      DTETAR=2.D0*3.141592654D0/DBLE(NPLAN)
      DO 10 IP = 1,NPOIN2
        FREAD(IP)=0.D0
        DENOM(IP)=0.D0
   10 CONTINUE
C
C-----C-------------------------------------------------------C
C-----C SUMS UP THE CONTRIBUTIONS FOR THE DISCRETISED PART OF THE SPECTRUM     C
C-----C-------------------------------------------------------C
      DO 20 JF=1,NF
C
C.......INTEGRATES WRT DIRECTIONS TO GET E(F)
C       """""""""""""""""""""""""""""""""""""""""""""""""
        DO 60 IP=1,NPOIN2
          E(IP) = 0.D0
   60   CONTINUE
        DO 30 JP=1,NPLAN
          DO 40 IP=1,NPOIN2
                 E(IP) = E(IP) + F(IP,JP,JF)*DTETAR
   40     CONTINUE
   30   CONTINUE
C
C.......SUMS UP THE CONTRIBUTION OF THE FREQUENCY F
C       """""""""""""""""""""""""""""""""""""""""""
        DO 50 IP=1,NPOIN2
          IF (E(IP).GT.SEUIL) THEN
            AUXI = E(IP)**EXPO*DFREQ(JF)
            FREAD(IP) = FREAD(IP)+AUXI*FREQ(JF)
            DENOM(IP) = DENOM(IP)+AUXI
          ENDIF
   50   CONTINUE
C
   20 CONTINUE
C
C-----C-------------------------------------------------------------C
C-----C (OPTIONALLY) TAKES INTO ACCOUNT THE HIGH-FREQUENCY PART     C
C-----C-------------------------------------------------------------C
      IF (TAILF.GT.1.D0) THEN
        COEFN=FREQ(NF)**2/(TAILF*EXPO-2.D0)
        COEFD=FREQ(NF)   /(TAILF*EXPO-1.D0)
        DO 55 IP=1,NPOIN2
          AUXI=E(IP)**EXPO
          FREAD(IP) = FREAD(IP)+AUXI*COEFN
          DENOM(IP) = DENOM(IP)+AUXI*COEFD
   55   CONTINUE
      ENDIF
C
C-----C-------------------------------------------------------------C
C-----C COMPUTES THE PEAK FREQUENCY                                 C
C-----C-------------------------------------------------------------C
      DO 70 IP=1,NPOIN2
        IF (DENOM(IP).LT.1.D-90) THEN
          FREAD(IP) = SEUIL
        ELSE
          FREAD(IP) = FREAD(IP)/DENOM(IP)
        ENDIF
   70 CONTINUE
C
      RETURN
      END
C
C#######################################################################
C