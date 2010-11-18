C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief      COMPUTES THE SQUARE ROOT OF THE TRANSCENDENT EQUATION
!>               IN QB, WHICH IS THE RATE OF BREAKING OR BROKEN WAVES.
!>
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> Q1, Q2, Q3
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> EPSIQB, FQ1, FQ2, FQ3, RAP
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
!>    <td> 04/06/1999                                              </td>
!>    <td> D. AELBRECHT (LNH) 01.30.87.74.12                       </td>
!>    <td>                                                         </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>Q1
!></td><td>--></td><td>EXTREMITE GAUCHE INITIALE DE LA CORDE
!>    </td></tr>
!>          <tr><td>Q2
!></td><td>--></td><td>EXTREMITE DROITE INITIALE DE LA CORDE
!>    </td></tr>
!>          <tr><td>Q3
!></td><td><--</td><td>SOLUTION APPROCHEE DE QB
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                SUBROUTINE CALCQB
     &(Q1,Q2,Q3)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| Q1             |-->| EXTREMITE GAUCHE INITIALE DE LA CORDE
C| Q2             |-->| EXTREMITE DROITE INITIALE DE LA CORDE
C| Q3             |<--| SOLUTION APPROCHEE DE QB
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      DOUBLE PRECISION Q1,Q2,Q3,FQ1,FQ2,FQ3,EPSIQB,RAP
C
      EPSIQB = 1.D-4
      RAP = Q2
C
      IF(Q2.GE.1.D0) THEN
         Q3 = 1.D0
      ELSE
         FQ3 = 1000.D0
C
C 10      FQ1 = (1.D0-Q1)+RAP*LOG(Q1)
 10      FQ1 = (1.D0-Q1)+RAP*LOG(ABS(Q1))
         FQ2 = (1.D0-Q2)+RAP*LOG(ABS(Q2))
C         FQ2 = (1.D0-Q2)+RAP*LOG(Q2)
         IF (FQ1.GE.0.D0) THEN
            Q3 = Q1
            FQ3 = EPSIQB/10.D0
         ELSE
            Q3 = Q1 - FQ1*(Q2-Q1)/(FQ2-FQ1)
            FQ3 = (1.D0-Q3)+RAP*LOG(ABS(Q3))
C            FQ3 = (1.D0-Q3)+RAP*LOG(Q3)
            IF ((FQ3*FQ1).GT.0.D0) THEN
               Q1 = Q3
            ELSE
               Q2 = Q3
            ENDIF
         ENDIF
         IF(ABS(FQ3).GE.EPSIQB) GOTO 10
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C