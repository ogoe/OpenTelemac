C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       DISCRETISES A DIRECTIONAL ENERGY SPECTRUM IN
!>                NDALE BANDS OF EQUAL ENERGY. THE RESULT IS A
!>                LIST OF DIRECTIONS CORRESPONDING TO EACH BAND.<br>
!>      USES THE FORMULATION GIVEN BY GODA IN ' RANDOM SEAS AND
!>      DESIGN OF MARITIME STRUCTURES' - UNIVERSITY OF TOKYO PRESS<br>
!>      G = ( COS( (TETA-TETAH))/2 ) )**EXPOS

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_ARTEMIS
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DALE, EXPOS, NDALE, NPOIN, NPRIV, PRIVE, TETAH, TETMAX, TETMIN, TRA01
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU<hr>
!> COEFHD : EXPO
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DTETA, I, K, NPAS, SUMB, SUMICI, VAR
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_DIRALE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> SPD()
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
!>          <tr><td>DALE
!></td><td><--</td><td>DIRECTIONS DE DISCRETISATION DU SPECTRE
!>    </td></tr>
!>          <tr><td>EXPOS
!></td><td>--></td><td>COEFFICIENT DANS LA FORMULE DU SPECTRE
!>    </td></tr>
!>          <tr><td>NDALE
!></td><td>--></td><td>NOMBRE DE BANDES DE DISCRETISATION
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPRIV
!></td><td><-></td><td>NOMBRE DE TABLEAUX PRIVES
!>    </td></tr>
!>          <tr><td>PRIVE
!></td><td><-></td><td>TABLEAU PRIVE DE L'UTILISATEUR
!>    </td></tr>
!>          <tr><td>TETAH
!></td><td>--></td><td>DIRECTION PRINCIPALE DE PROPAGATION
!>    </td></tr>
!>          <tr><td>TETMAX
!></td><td>--></td><td>VALEUR MAXIMUM DE L'ANGLE DE PROPAGATION
!>    </td></tr>
!>          <tr><td>TETMIN
!></td><td>--></td><td>VALEUR MINIMUM DE L'ANGLE DE PROPAGATION
!>    </td></tr>
!>          <tr><td>TRA01
!></td><td><-></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DIRALE
     &(DALE,EXPOS,TETAH,TETMIN,TETMAX,NDALE,TRA01,NPOIN,PRIVE,NPRIV)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DALE           |<--| DIRECTIONS DE DISCRETISATION DU SPECTRE
C| EXPOS          |-->| COEFFICIENT DANS LA FORMULE DU SPECTRE
C| NDALE          |-->| NOMBRE DE BANDES DE DISCRETISATION
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
C| NPRIV          |<->| NOMBRE DE TABLEAUX PRIVES
C| PRIVE          |<->| TABLEAU PRIVE DE L'UTILISATEUR
C| TETAH          |-->| DIRECTION PRINCIPALE DE PROPAGATION
C| TETMAX         |-->| VALEUR MAXIMUM DE L'ANGLE DE PROPAGATION
C| TETMIN         |-->| VALEUR MINIMUM DE L'ANGLE DE PROPAGATION
C| TRA01          |<->| TABLEAU DE TRAVAIL
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_ARTEMIS, EX_DIRALE => DIRALE
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NDALE,NPOIN,NPAS,I,K,NPRIV
C
      DOUBLE PRECISION DALE(NDALE),TRA01(NPOIN)
      DOUBLE PRECISION EXPOS,TETMIN,TETMAX,TETAH,DTETA,SUMB,VAR,SUMICI
C
      TYPE(BIEF_OBJ) :: PRIVE
C
      DOUBLE PRECISION EXPO
      COMMON /COEFHD/ EXPO
C
C      DOUBLE PRECISION SPD
C      EXTERNAL SPD
C
C-----------------------------------------------------------------------
C
C EXPOS IS IN THE COMMON STATEMENT OF FUNCTION SPD (CANNOT BE
C CALLED EXPOS BECAUSE IT IS AN ARGUMENT OF THIS SUBROUTINE)
      EXPO = EXPOS
C
C-----------------------------------------------------------------------
C
C     NUMBER OF INTEGRATION INTERVALS FOR THE TRAPEZOIDS METHOD
      NPAS = 2000*NDALE
C
C     WIDTH OF AN INTEGRATION INTERVAL
      DTETA = (TETMAX-TETMIN)/FLOAT(NPAS)
C
C     SURFACE OF THE SPECTRUM
      SUMB = (SPD(TETMIN-TETAH) + SPD(TETMAX-TETAH))/2.D0
      DO 20 I = 2,NPAS-1
         SUMB = SUMB + SPD(TETMIN-TETAH+FLOAT(I)*DTETA)
20    CONTINUE
C
C     DIVIDES THE SPECTRUM INTO 2*NDALE BANDS OF EQUAL ENERGY
      SUMB = SUMB/FLOAT(2*NDALE)
C
C     IDENTIFIES THE ANGLES EVERY (2*I-1)*SUMB (I=1,NDALE)
      SUMICI = SPD(TETMIN-TETAH)/2.D0
      I   = 1
      DO 30 K=1,NPAS
         VAR = SPD(TETMIN-TETAH+DTETA*FLOAT(K))
         SUMICI = SUMICI + VAR/2.D0
         IF (SUMICI.GE.SUMB*FLOAT(2*I-1)) THEN
            DALE(I) =  TETMIN + DTETA*(FLOAT(K)-0.5D0)
            I = I + 1
            IF (I.GT.NDALE) RETURN
         ENDIF
         SUMICI = SUMICI + VAR/2.D0
30    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C