C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       GETS THE ANGULAR INDICES AROUND A GIVEN DIRECTION
!>                FOR THE NON-LINEAR INTERACTION TERM, USING THE DIA
!>               ("DISCRETE INTERACTION APPROXIMATION") METHOD
!>                PROPOSED BY HASSELMANN AND HASSELMANN (1985).<br>
!><br>            PROCEDURE SPECIFIC TO THE CASE WHERE THE DIRECTIONS
!>                ARE EVENLY DISTRIBUTED OVER [0;2.PI].

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note   THE DELTAD DEVIATION SHOULD BE GIVEN IN DEGREES.

!>  @note   LAVANT AND LAPRES ARE COMPRISED BETWEEN 1 AND NPLAN.

!>  @reference HASSELMANN S., HASSELMANN K. ET AL.(1985) :
!>                     "COMPUTATIONS AND PARAMETERIZATIONS OF THE NONLINEAR
!>                      ENERGY TRANSFER IN GRAVITY-WAVE SPECTRUM. PART2 :
!>                      PARAMETERIZATIONS OF THE NONLINEAR ENERGY TRANSFER
!>                      FOR APPLICATION IN WAVE MODELS". JPO, VOL 15, PP 1378-1391.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DELTAD, IPLAN, LAPRES, LAVANT, NPLAN
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DTETAD, TETA
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PRENL1()

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
!>          <tr><td>DELTAD
!></td><td>--></td><td>DEVIATION PAR RAPPORT A LA DIRECTION DEPART
!>    </td></tr>
!>          <tr><td>IPLAN
!></td><td>--></td><td>INDICE DE LA DIRECTION DE DEPART
!>    </td></tr>
!>          <tr><td>LAPRES
!></td><td><--</td><td>INDICE ANGULAIRE SUIVANT   LA DIR. ARRIVEE
!>    </td></tr>
!>          <tr><td>LAVANT
!></td><td><--</td><td>INDICE ANGULAIRE PRECEDANT LA DIR. ARRIVEE
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE DIRECTIONS DE DISCRETISATION
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE INTANG
     &( LAVANT, LAPRES, IPLAN , NPLAN , DELTAD)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DELTAD         |-->| DEVIATION PAR RAPPORT A LA DIRECTION DEPART
C| IPLAN          |-->| INDICE DE LA DIRECTION DE DEPART
C| LAPRES         |<--| INDICE ANGULAIRE SUIVANT   LA DIR. ARRIVEE
C| LAVANT         |<--| INDICE ANGULAIRE PRECEDANT LA DIR. ARRIVEE
C| NPLAN          |-->| NOMBRE DE DIRECTIONS DE DISCRETISATION
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """""""""""""""""""""
      INTEGER  LAVANT, LAPRES, NPLAN , IPLAN
      DOUBLE PRECISION DELTAD
C
C.....LOCAL VARIABLES
C     """"""""""""""""""
      DOUBLE PRECISION TETA  , DTETAD
C
C
      DTETAD=360.D0/DBLE(NPLAN)
      TETA=DBLE(IPLAN-1)*DTETAD+DELTAD
C
C.....TETA IS ADJUSTED TO BE COMPRISED BETWEEN 0 AND 360 DEG.
C     """""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  100 IF (TETA.GE.360.D0) THEN
        TETA=TETA-360.D0
        GOTO 100
      ENDIF
  110 IF (TETA.LT.0.D0) THEN
        TETA=TETA+360.D0
        GOTO 110
      ENDIF
C
C.....GETS THE ANGULAR INDICES PRECEDING AND FOLLOWING TETA
C     """"""""""""""""""""""""""""""""""""""""""""""""""""""""
      LAVANT=INT(TETA/DTETAD)+1
      LAPRES=LAVANT+1
      IF (LAPRES.GT.NPLAN) LAPRES=1
C
      RETURN
      END
C
C#######################################################################
C