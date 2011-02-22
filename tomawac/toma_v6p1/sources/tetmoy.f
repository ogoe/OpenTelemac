C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE MEAN DIRECTION OF A DIRECTIONAL SPECTRUM
!>               (BY COMPUTING THE ARC TANGENT OF THE MEAN SINE AND
!>                COSINE). THE RESULT IS IN RADIANS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> COSMOY, COSTET, DFREQ, F, FREQ, NF, NPLAN, NPOIN2, SINMOY, SINTET, TAILF, TAUXC, TAUXS, TETAM
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUXC, AUXS, COEFT, DEUPI, DFDTET, DTETAR, IP, JF, JP, SEUIL
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
!>      <td><center> 1.2                                       </center>
!> </td><td> 05/07/96
!> </td><td> M. BENOIT
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center> 1.0                                       </center>
!> </td><td> 01/02/95
!> </td><td> P. THELIIER; M. BENOIT
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>COSMOY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>COSMOY(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>COSTET
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>COSTET(
!></td><td>--></td><td>VECTEUR DES COSINUS DES DIRECTIONS
!>    </td></tr>
!>          <tr><td>DFREQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DFREQ(
!></td><td>--></td><td>VECTEUR DES PAS DE FREQUENCE
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
!></td><td>--></td><td>VECTEUR DES FREQUENCES DE DISCRETISATION
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
!>          <tr><td>SINMOY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SINMOY(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>SINTET
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SINTET(
!></td><td>--></td><td>VECTEUR DES SINUS   DES DIRECTIONS
!>    </td></tr>
!>          <tr><td>TAILF
!></td><td>--></td><td>FACTEUR DE QUEUE DU SPECTRE
!>    </td></tr>
!>          <tr><td>TAUXC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAUXC(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>TAUXS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAUXS(
!></td><td>---</td><td>TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
!>    </td></tr>
!>          <tr><td>TETAM
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE TETMOY
     &( TETAM , F     , COSTET, SINTET, NPLAN , FREQ  , DFREQ , NF    ,
     &  NPOIN2, TAILF , COSMOY, SINMOY, TAUXC , TAUXS )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| COSMOY         |---| 
C| COSMOY(        |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| COSTET         |---| 
C| COSTET(        |-->| VECTEUR DES COSINUS DES DIRECTIONS
C| DFREQ          |---| 
C| DFREQ(         |-->| VECTEUR DES PAS DE FREQUENCE
C| F             |---| 
C| F(             |-->| SPECTRE DIRECTIONNEL DE VARIANCE
C| FREQ           |---| 
C| FREQ(          |-->| VECTEUR DES FREQUENCES DE DISCRETISATION
C| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
C| NPLAN          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE SPATIAL 2D
C| SINMOY         |---| 
C| SINMOY(        |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| SINTET         |---| 
C| SINTET(        |-->| VECTEUR DES SINUS   DES DIRECTIONS
C| TAILF          |-->| FACTEUR DE QUEUE DU SPECTRE
C| TAUXC          |---| 
C| TAUXC(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| TAUXS          |---| 
C| TAUXS(         |---| TABLEAU DE TRAVAIL (DIMENSION NPOIN2)
C| TETAM          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """"""""""""""""""""
      INTEGER  NF    , NPLAN , NPOIN2
      DOUBLE PRECISION TAILF
      DOUBLE PRECISION TETAM(NPOIN2) , DFREQ(NF)    , COSMOY(NPOIN2)
      DOUBLE PRECISION COSTET(NPLAN) , SINTET(NPLAN), SINMOY(NPOIN2)
      DOUBLE PRECISION TAUXC(NPOIN2) , TAUXS(NPOIN2)
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF), FREQ(NF)
C
C.....LOCAL VARIABLES
C     """""""""""""""""
      INTEGER  IP    , JP    , JF
      DOUBLE PRECISION AUXC  , AUXS  , DEUPI , SEUIL , COEFT , DFDTET
      DOUBLE PRECISION DTETAR
C
C
      DEUPI =2.D0*3.14159265D0
      DTETAR=DEUPI/DBLE(NPLAN)
      SEUIL =1.D-10
      DO 10 IP=1,NPOIN2
        COSMOY(IP)=0.D0
        SINMOY(IP)=0.D0
   10 CONTINUE
C
C-----C-------------------------------------------------------C
C-----C  SUMS UP THE DISCRETISED PART OF THE SPECTRUM         C
C-----C-------------------------------------------------------C
      DO 30 JF=1,NF
C
        DFDTET=DFREQ(JF)*DTETAR
C
        DO 35 IP=1,NPOIN2
          TAUXC(IP)=0.D0
          TAUXS(IP)=0.D0
   35   CONTINUE
C
        DO 20 JP=1,NPLAN
          AUXC=COSTET(JP)*DFDTET
          AUXS=SINTET(JP)*DFDTET
          DO 40 IP=1,NPOIN2
            TAUXC(IP)=TAUXC(IP)+F(IP,JP,JF)*AUXC
            TAUXS(IP)=TAUXS(IP)+F(IP,JP,JF)*AUXS
   40     CONTINUE
   20   CONTINUE
C
        DO 45 IP=1,NPOIN2
          COSMOY(IP)=COSMOY(IP)+TAUXC(IP)
          SINMOY(IP)=SINMOY(IP)+TAUXS(IP)
   45   CONTINUE
C
   30 CONTINUE
C
C-----C-------------------------------------------------------------C
C-----C  TAKES THE HIGH FREQUENCY PART INTO ACCOUNT (OPTIONAL)      C
C-----C-------------------------------------------------------------C
      IF (TAILF.GT.1.D0) THEN
        COEFT=FREQ(NF)/((TAILF-1.D0)*DFREQ(NF))
        DO 55 IP=1,NPOIN2
          COSMOY(IP)=COSMOY(IP)+TAUXC(IP)*COEFT
          SINMOY(IP)=SINMOY(IP)+TAUXS(IP)*COEFT
   55   CONTINUE
      ENDIF
C
C-----C-------------------------------------------------------------C
C-----C  COMPUTES THE MEAN DIRECTION                                C
C-----C  (IN RADIANS BETWEEN 0 AND 2.PI)                            C
C-----C-------------------------------------------------------------C
      DO 60 IP=1,NPOIN2
        IF ((ABS(SINMOY(IP)).LT.SEUIL).AND.
     &      (ABS(COSMOY(IP)).LT.SEUIL)) THEN
          TETAM(IP) = 0.D0
        ELSE
          TETAM(IP)=ATAN2(SINMOY(IP),COSMOY(IP))
          IF (TETAM(IP).LT.0.D0) TETAM(IP)=TETAM(IP)+DEUPI
        ENDIF
   60 CONTINUE
C
      RETURN
      END
C
C#######################################################################
C