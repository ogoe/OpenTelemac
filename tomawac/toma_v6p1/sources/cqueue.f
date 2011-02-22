C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ADJUSTS FREQUENCY INDICES AND COMPUTES TAIL

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note   THE SPECTRUM IS ASSUMED TO BE 0 FOR FREQUENCIES LOWER THAN
!>          THE FIRST DISCRETISED FREQUENCY.

!>  @note   BEYOND THE LAST DISCRETISED FREQUENCY THE SPECTRUM IS
!>          ASSUMED TO DECREASE FOLLOWING A FREQ**(-TAILF) LAW.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> COEF1, JBIS, JFRE, NF, RAISF, TAILF
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>QNLIN1()

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
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>COEF1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>JBIS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>JFRE
!></td><td>--></td><td>INDICE FREQUENTIEL DE LA COMPOSANTE FREQ.
!>    </td></tr>
!>          <tr><td>NF
!></td><td>--></td><td>NOMBRE DE FREQUENCES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>RAISF
!></td><td>--></td><td>RAISON FREQUENTIELLE DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>TAILF
!></td><td>--></td><td>FACTEUR DE QUEUE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CQUEUE
     &( NF    , RAISF , TAILF , JFRE  , JBIS  , COEF1 )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| COEF1          |---| 
C| JBIS           |---| 
C| JFRE           |-->| INDICE FREQUENTIEL DE LA COMPOSANTE FREQ.
C| NF             |-->| NOMBRE DE FREQUENCES DE DISCRETISATION
C| RAISF          |-->| RAISON FREQUENTIELLE DE DISCRETISATION
C| TAILF          |-->| FACTEUR DE QUEUE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C.....VARIABLES IN ARGUMENT
C     """""""""""""""""""""
      INTEGER  NF    , JFRE  , JBIS
      DOUBLE PRECISION RAISF , TAILF , COEF1
C
C
      IF (JFRE.GT.NF) THEN
        JBIS = NF
        COEF1= 1.D0/RAISF**(DBLE(JFRE-NF)*TAILF)
      ELSEIF (JFRE.LT.1) THEN
        JBIS = 1
        COEF1= 0.D0
      ELSE
        JBIS = JFRE
        COEF1= 1.D0
      ENDIF
C
      RETURN
      END
C
C#######################################################################
C