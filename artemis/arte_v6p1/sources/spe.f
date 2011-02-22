C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE ENERGY DENSITY BASED ON GODA.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @reference "RANDOM SEA AND DESIGN OF MARITIME STRUCTURES",
!>                       UNIVERSITY OF TOKYO PRESS - 1985

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU<hr>
!> COEFHE : FP, GAM, DELTA
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> SIGMA
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PERALE()

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
!>  <tr>
!>    <td><center> 5.1                                    </center></td>
!>    <td> 04/06/1999                                              </td>
!>    <td> D. AELBRECHT (LNH) 01.30.87.74.12                       </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center> 2.0                                    </center></td>
!>    <td> 01/06/1993                                              </td>
!>    <td> F. LEPEINTRE (LNH) 01.30.87.78.54                       </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>F
!></td><td>--></td><td>FREQUENCE OU L'ON CALCULE LA DENSITE D'ENERGIE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        DOUBLE PRECISION FUNCTION SPE
     &(F)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F             |-->| FREQUENCE OU L'ON CALCULE LA DENSITE D'ENERGIE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      DOUBLE PRECISION F,SIGMA
C
      DOUBLE PRECISION FP,GAM,DELTA
      COMMON /COEFHE/ FP,GAM,DELTA
C
      INTRINSIC EXP
C
C-----------------------------------------------------------------------
C
      IF (F.LE.FP) THEN
        SIGMA = 0.07D0
      ELSE
        SIGMA = 0.09D0
      ENDIF
C
CC    DELTA = 0.0624D0 * FP**4 /
CC   *       ( 0.230D0 + 0.0336D0*GAM - 0.185D0 / (1.9D0+GAM) )
C
C     DELTA IS COMPUTED IN PERALE
C
      IF ( F.GE.1.D-4*FP) THEN
         SPE = DELTA/F**5 * EXP(-1.25D0*(FP/F)**4) *
     &         GAM** ( EXP( -0.5D0*( ( (F-FP)/(SIGMA*FP) ) **2 ) ) )
      ELSE
         SPE = 0.D0
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C