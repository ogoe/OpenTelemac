C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       WRITES HEADER LINES FOR VARIOUS AGITATION COMPUTATIONS
!>                IN THE LISTING FILE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>INTERFACE_ARTEMIS
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ALEMON, ALEMUL, BALAYE, ITITRE, LT, NBR, NBRTOT, X
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> TEXTFR, TEXTGB
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_ENTART
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ARTEMIS()

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
!>    <td> 02/06/1999                                              </td>
!>    <td> D. AELBRECHT (LNH) 01.30.87.74.12                       </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td>                                                         </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
!>    <td> LINKED TO BIEF 5.0                                      </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ALEMON
!></td><td>--></td><td>VRAI SI HOULE ALEATOIRE MONODIRECTIONNELLE
!>    </td></tr>
!>          <tr><td>ALEMUL
!></td><td>--></td><td>VRAI SI HOULE ALEATOIRE MULTIDIRECTIONNELLE
!>    </td></tr>
!>          <tr><td>BALAYE
!></td><td>--></td><td>VRAI SI BALAYAGE EN PERIODES
!>    </td></tr>
!>          <tr><td>ITITRE
!></td><td>--></td><td>TYPE DE TITRE A IMPRIMER
!>    </td></tr>
!>          <tr><td>LT
!></td><td>--></td><td>NUMERO DU CALCUL
!>    </td></tr>
!>          <tr><td>NBR
!></td><td>--></td><td>NUMERO DE LA PERIODE OU DIRECTION EN COURS
!>    </td></tr>
!>          <tr><td>NBRTOT
!></td><td>--></td><td>NOMBRES TOTAL DES PERIODES OU DIRECTIONS
!>    </td></tr>
!>          <tr><td>X
!></td><td>--></td><td>REEL A IMPRIMER
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ENTART
     &(ITITRE,X,LT,NBR,NBRTOT,ALEMON,ALEMUL,BALAYE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ALEMON         |-->| VRAI SI HOULE ALEATOIRE MONODIRECTIONNELLE
C| ALEMUL         |-->| VRAI SI HOULE ALEATOIRE MULTIDIRECTIONNELLE
C| BALAYE         |-->| VRAI SI BALAYAGE EN PERIODES
C| ITITRE         |-->| TYPE DE TITRE A IMPRIMER
C| LT             |-->| NUMERO DU CALCUL
C| NBR            |-->| NUMERO DE LA PERIODE OU DIRECTION EN COURS
C| NBRTOT         |-->| NOMBRES TOTAL DES PERIODES OU DIRECTIONS
C| X             |-->| REEL A IMPRIMER
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_ARTEMIS, EX_ENTART => ENTART
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER LT,ITITRE,NBR,NBRTOT
C
      DOUBLE PRECISION X
C
      LOGICAL ALEMON,ALEMUL,BALAYE
C
      CHARACTER*32 TEXTFR(5),TEXTGB(5)
C
C-----------------------------------------------------------------------
C
      DATA TEXTFR / 'PERIODE     ' ,
     &              ' SECONDES   ' ,
     &              'DIRECTION   ' ,
     &              ' DEGRES     ' ,
     &              '            ' /
      DATA TEXTGB / 'PERIOD      ' ,
     &              ' SECONDS    ' ,
     &              'DIRECTION   ' ,
     &              ' DEGREES    ' ,
     &              '            ' /
C
C REGULAR WAVES
C
      IF (.NOT.ALEMON .AND. .NOT.ALEMUL .AND. .NOT.BALAYE) THEN
         NBRTOT = 1
      ENDIF
C
C-----------------------------------------------------------------------
C
C   WRITES OUT THE COMPUTED WAVE PERIOD
C
C
      IF (ITITRE.EQ.1) THEN
         IF(LNG.EQ.1) WRITE(LU,100) TEXTFR(1),NBR,NBRTOT,X,TEXTFR(2)
         IF(LNG.EQ.2) WRITE(LU,100) TEXTGB(1),NBR,NBRTOT,X,TEXTGB(2)
      ENDIF
C
100   FORMAT(/,80('='),/,7X,A8,I2,'/',I2,' : ',F12.4,A10,/)
C
C
C-----------------------------------------------------------------------
C
C   WRITES OUT THE COMPUTED WAVE DIRECTION
C
C
      IF (ITITRE.EQ.2) THEN
         IF(LNG.EQ.1) WRITE(LU,110) TEXTFR(3),NBR,NBRTOT,X,TEXTFR(4)
         IF(LNG.EQ.2) WRITE(LU,110) TEXTGB(3),NBR,NBRTOT,X,TEXTGB(4)
      ENDIF
C
110   FORMAT(/,7X,A10,I2,'/',I2,' : ',F12.4,A10,/)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C