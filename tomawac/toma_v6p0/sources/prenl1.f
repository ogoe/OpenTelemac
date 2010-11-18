C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       PREPARES THE COMPUTATION FOR THE NON-LINEAR INTERACTION
!>                SOURCE TERM BETWEEN QUADRUPLETS USING THE DIA METHOD
!>               ("DISCRETE INTERACTION APPROXIMATION") PROPOSED BY
!>                HASSELMANN AND HASSELMANN (1985).<br>
!><br>            PROCEDURE SPECIFIC TO THE CASE WHERE THE FREQUENCIES
!>                FOLLOW A GEOMETRICAL PROGRESSION AND THE DIRECTIONS
!>                ARE EVENLY DISTRIBUTED OVER [0;2.PI].

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note   THIS SUBROUTINE IS TO BE USED IN CONJONCTION WITH THE
!>          SUBROUTINE QNLIN1, WHICH IT OPTIMISES.

!>  @reference    HASSELMANN S., HASSELMANN K. ET AL.(1985) :
!>                     "COMPUTATIONS AND PARAMETERIZATIONS OF THE NONLINEAR
!>                      ENERGY TRANSFER IN GRAVITY-WAVE SPECTRUM. PART1 :
!>                      A NEW METHOD FOR EFFICIENT COMPUTATION OF THE EXACT
!>                      NON-LINEAR TRANSFER INTEGRAL". JPO, VOL 15, PP 1369-1377.

!>  @reference    HASSELMANN S., HASSELMANN K. ET AL.(1985) :
!>                     "COMPUTATIONS AND PARAMETERIZATIONS OF THE NONLINEAR
!>                      ENERGY TRANSFER IN GRAVITY-WAVE SPECTRUM. PART2 :
!>                      PARAMETERIZATIONS OF THE NONLINEAR ENERGY TRANSFER
!>                      FOR APPLICATION IN WAVE MODELS". JPO, VOL 15, PP 1378-1391.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> COEFNL, IANGNL, NF, NPLAN, RAISF, XLAMD
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AMOIN, APLUS, BMOIN, BPLUS, DELTA1, DELTA2, DTETAD, DTMOIN, DTPLUS, FMOIN, FPLUS, JP
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ANGLES(), INTANG()
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
!>      <td><center> 1.2                                       </center>
!> </td><td> 26/06/96
!> </td><td> M. BENOIT
!> </td><td> CREATED
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>COEFNL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>COEFNL(
!></td><td><--</td><td>VECTEUR DES COEFFICIENTS DE CALCUL POUR DIA
!>    </td></tr>
!>          <tr><td>IANGNL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IANGNL(
!></td><td><--</td><td>TABLEAU DES INDICES ANGULAIRES POUR DIA
!>    </td></tr>
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE DIRECTIONS DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>RAISF
!></td><td>--></td><td>RAISON FREQUENTIELLE DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>XLAMD
!></td><td>--></td><td>COEFFICIENT LAMBDA DE LA CONFIGUARTION STD
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE PRENL1
     &( IANGNL, COEFNL, NPLAN , NF    , RAISF , XLAMD )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| COEFNL         |---| 
C| COEFNL(        |<--| VECTEUR DES COEFFICIENTS DE CALCUL POUR DIA
C| IANGNL         |---| 
C| IANGNL(        |<--| TABLEAU DES INDICES ANGULAIRES POUR DIA
C| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
C| NPLAN          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
C| RAISF          |-->| RAISON FREQUENTIELLE DE DISCRETISATION
C| XLAMD          |-->| COEFFICIENT LAMBDA DE LA CONFIGUARTION STD
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """""""""""""""""""""
      INTEGER  NPLAN , NF
      INTEGER  IANGNL(NPLAN,8)
      DOUBLE PRECISION RAISF , XLAMD
      DOUBLE PRECISION COEFNL(16)
C
C.....LOCAL VARIABLES
C     """"""""""""""""""
      INTEGER  JP
      DOUBLE PRECISION DELTA1, DELTA2, DTMOIN, DTPLUS, DTETAD
      DOUBLE PRECISION APLUS , AMOIN , BPLUS , BMOIN , FPLUS , FMOIN
C
C
C=====C---------------------------------------------------C
C  1  C COMPUTATIONS RELATED TO ANGULAR INTERPOLATION     C
C=====C---------------------------------------------------C
C
C.....1.1 DETERMINES RESONANT DIRECTIONS
C         (WITH THE CONVENTION  0
C     """""""""""""""""""""""""""""""""""""""""""""
      CALL  ANGLES( XLAMD , DTPLUS, DTMOIN)
C
C.....1.2 DETERMINES ANGULAR INDICES FOR THE 'STANDARD' CONFIGURATION
C         (CORRESPONDING TO (-DTPLUS,DTMOIN))
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""
      DELTA1=-DTPLUS
      DELTA2= DTMOIN
      DO 110 JP=1,NPLAN
        CALL INTANG( IANGNL(JP,2) , IANGNL(JP,1) , JP , NPLAN , DELTA1)
        CALL INTANG( IANGNL(JP,3) , IANGNL(JP,4) , JP , NPLAN , DELTA2)
  110 CONTINUE
C
C.....1.3 DETERMINES ANGULAR INDICES FOR THE 'IMAGE' CONFIGURATION
C         (CORRESPONDING TO (DTPLUS,-DTMOIN))
C     """""""""""""""""""""""""""""""""""""""""""""""""""""
      DELTA1= DTPLUS
      DELTA2=-DTMOIN
      DO 120 JP=1,NPLAN
        CALL INTANG( IANGNL(JP,5) , IANGNL(JP,6) , JP , NPLAN , DELTA1)
        CALL INTANG( IANGNL(JP,8) , IANGNL(JP,7) , JP , NPLAN , DELTA2)
  120 CONTINUE
C
C.....1.4 DETERMINES COEFFICIENTS OF ANGULAR INTERPOLATION
C     """""""""""""""""""""""""""""""""""""""""""
      DTETAD=360.D0/DBLE(NPLAN)
      APLUS=DTPLUS/DTETAD-DBLE(INT(DTPLUS/DTETAD))
      AMOIN=DTMOIN/DTETAD-DBLE(INT(DTMOIN/DTETAD))
C
C
C=====C---------------------------------------------------C
C  2  C COMPUTATIONS RELATED TO FREQUENCY INTERPOLATION   C
C=====C---------------------------------------------------C
      FPLUS=DLOG(1.D0+XLAMD)/DLOG(RAISF)
      FMOIN=DLOG(1.D0-XLAMD)/DLOG(RAISF)
      BPLUS=(RAISF**(FPLUS-IDINT(FPLUS)     )-1.D0)/(RAISF-1.D0)
      BMOIN=(RAISF**(FMOIN-IDINT(FMOIN)+1.D0)-1.D0)/(RAISF-1.D0)
C
C
C=====C---------------------------------------------------C
C  3 C ASSIGNS THE COEFFICIENTS FOR QNLIN1                C
C=====C---------------------------------------------------C
      COEFNL( 1)=(1.D0-APLUS) * (1.D0-BPLUS)
      COEFNL( 2)=      APLUS  * (1.D0-BPLUS)
      COEFNL( 3)=(1.D0-APLUS) *       BPLUS
      COEFNL( 4)=      APLUS  *       BPLUS
      COEFNL( 5)=(1.D0-AMOIN) * (1.D0-BMOIN)
      COEFNL( 6)=      AMOIN  * (1.D0-BMOIN)
      COEFNL( 7)=(1.D0-AMOIN) *       BMOIN
      COEFNL( 8)=      AMOIN  *       BMOIN
      COEFNL( 9)=FPLUS
      COEFNL(10)=FMOIN
      COEFNL(11)=1.D0/(1.D0+XLAMD)**4
      COEFNL(12)=1.D0/(1.D0-XLAMD)**4
      COEFNL(13)=DBLE(1)
      COEFNL(14)=DBLE(NF+IDINT(1.D0-FMOIN))
C
      RETURN
      END
C
C#######################################################################
C