C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INHIBITS THE DIAGONAL PRECONDITIONING IF ONE OF THE
!>                ELEMENTS OF DAM IS NEGATIVE OR ZERO.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @code
!>  --------------------------------------------------------------------
!> |  VALUE OF IPREC2   I                  MEANING
!> |    OR IPRECO       I
!>  --------------------------------------------------------------------
!> |       1            I  NOTHING.
!> |       2            I  DIAGONAL PRECONDITIONING USING THE MATRIX
!> |                    I  DIAGONAL.
!> |       3            I  DIAGONAL PRECONDITIONING USING THE CONDENSED
!> |                    I  MATRIX.
!> |       5            I  OTHER (NOT DEFINED)
!> |                    I
!> |       7            I  CROUT PRECONDITIONING BY ELEMENT
!> |                    I  (NOT CODED IN)
!>  --------------------------------------------------------------------
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DAM, IPREC2, IPRECO, NPOIN
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BERKHO()

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
!>          <tr><td>DAM
!></td><td>--></td><td>DIAGONALE DE LA MATRICE
!>    </td></tr>
!>          <tr><td>IPREC2
!></td><td><--</td><td>PRECONDITIONNEMENT UTILISE
!>    </td></tr>
!>          <tr><td>IPRECO
!></td><td>--></td><td>PRECONDITIONNEMENT DEMANDE PAR L'UTILISATEUR
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CNTPRE
     &(DAM,NPOIN,IPRECO,IPREC2)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DAM            |-->| DIAGONALE DE LA MATRICE
C| IPREC2         |<--| PRECONDITIONNEMENT UTILISE
C| IPRECO         |-->| PRECONDITIONNEMENT DEMANDE PAR L'UTILISATEUR
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      INTEGER NPOIN,IPRECO,IPREC2,I
C
      DOUBLE PRECISION DAM(NPOIN)
C
C-----------------------------------------------------------------------
C
      IF (IPRECO.EQ.0) IPRECO = 1
      IPREC2 = IPRECO
C
      IF (MOD(IPRECO,2).EQ.0.OR.MOD(IPRECO,3).EQ.0) THEN
C
         DO 10 I=1,NPOIN
            IF (DAM(I).LE.0.D0) THEN
20             CONTINUE
               IF(MOD(IPREC2,2).EQ.0) THEN
                 IPREC2 = IPREC2/2
                 GO TO 20
               ENDIF
21             CONTINUE
               IF(MOD(IPREC2,3).EQ.0) THEN
                 IPREC2 = IPREC2/3
                 GO TO 21
               ENDIF
               IF (LNG.EQ.1) WRITE(LU,100)
               IF (LNG.EQ.2) WRITE(LU,101)
100     FORMAT(1X,'CNTPRE (ARTEMIS) : PRECONDITIONNEMENT DIAGONAL NON AP
     &PLIQUE (UN ELEMENT DIAGONAL DE LA MATRICE EST NEGATIF OU NUL)')
101     FORMAT(1X,'CNTPRE (ARTEMIS) : DIAGONAL SCALING NOT APPLIED (ONE
     &COEFFICIENT OF THE MATRIX DIAGONAL IS NEGATIVE OR ZERO)')
               GOTO 30
            ENDIF
10       CONTINUE
C
      ELSEIF (MOD(IPRECO,5).EQ.0) THEN
C
         DO 40 I=1,NPOIN
            IF (ABS(DAM(I)).LE.1.D-6) THEN
50             CONTINUE
               IF(MOD(IPREC2,5).EQ.0) THEN
                 IPREC2 = IPREC2/5
                 GO TO 50
               ENDIF
               IF (LNG.EQ.1) WRITE(LU,200)
               IF (LNG.EQ.2) WRITE(LU,201)
200     FORMAT(1X,'CNTPRE (ARTEMIS) : PRECONDITIONNEMENT DIAGONAL NON AP
     &PLIQUE (UN ELEMENT DIAGONAL DE LA MATRICE EST NUL)')
201     FORMAT(1X,'CNTPRE (ARTEMIS) : DIAGONAL SCALING NOT APPLIED (ONE
     &COEFFICIENT OF THE MATRIX DIAGONAL IS ZERO)')
               GOTO 30
            ENDIF
40       CONTINUE
C
      ENDIF
C
30    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C