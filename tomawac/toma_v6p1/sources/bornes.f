C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE INTEGRATION BOUNDS FOR THE INTEGRATION
!>                OF  THE FUNCTION "FONCRO", USING GAUSS QUADRATURES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A, B, N, X0, X1, XM
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DX, EPS, EPS1, I0, I1, II, IMAX, INP, JJ, X, Y
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> FONCRO()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>QGAUSS()

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
!>      <td><center> 1.1                                       </center>
!> </td><td> 26/03/96
!> </td><td> F. BECQ (EDF/DER/LNH)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A
!></td><td>--></td><td>PARAMETRE A DE LA FONCTION A INTEGRER
!>    </td></tr>
!>          <tr><td>B
!></td><td>--></td><td>PARAMETRE B DE LA FONCTION A INTEGRER
!>    </td></tr>
!>          <tr><td>N
!></td><td>--></td><td>EXPOSANT N  DE LA FONCTION A INTEGRER
!>    </td></tr>
!>          <tr><td>X0
!></td><td><--</td><td>BORNE INFERIEURE DE L'INTERVALLE
!>    </td></tr>
!>          <tr><td>X1
!></td><td><--</td><td>BORNE SUPERIEURE DE L'INTERVALLE
!>    </td></tr>
!>          <tr><td>XM
!></td><td>--></td><td>PARAMETRE M DE LA FONCTION A INTEGRER
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE BORNES
     &( B     , N     , A     , XM    , X0    , X1    )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |-->| PARAMETRE A DE LA FONCTION A INTEGRER
C| B             |-->| PARAMETRE B DE LA FONCTION A INTEGRER
C| N             |-->| EXPOSANT N  DE LA FONCTION A INTEGRER
C| X0             |<--| BORNE INFERIEURE DE L'INTERVALLE
C| X1             |<--| BORNE SUPERIEURE DE L'INTERVALLE
C| XM             |-->| PARAMETRE M DE LA FONCTION A INTEGRER
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
C     VARIABLES IN ARGUMENT
C     """""""""""""""""""""
      INTEGER  N
      DOUBLE PRECISION B     , A     , XM    , X0    , X1
C
C     LOCAL VARIABLES
C     """"""""""""""""""
      INTEGER  I0    , I1    , II    , JJ    , IMAX  , INP
      DOUBLE PRECISION X(11) , Y(11) , EPS   , EPS1  , DX
C
C.....EXTERNAL FUNCTIONS
C     """"""""""""""""""
      DOUBLE PRECISION  FONCRO
      EXTERNAL          FONCRO
C
C
      I1  = 11
      I0  = 1
      X(I0)= 0.D0
      X(I1)= 20.D0
      Y(1) = 0.D0
      EPS1 = 0.01D0
      EPS  = 0.0001D0
      INP  = 0
C
      DO 10 II=1,20
         DX = (X(I1)-X(I0))/10.D0
         X(1) = X(I0)
         IMAX = 0
         I0   = 1
         I1   = 11
         DO JJ=2,11
            X(JJ)=X(JJ-1)+DX
            Y(JJ)=FONCRO(X(JJ),B,N,A,XM)
            IF(Y(JJ).EQ.0.D0.AND.JJ.EQ.2.AND.INP.EQ.0D0) THEN
               X(I1) = X(I1)/10.D0
               INP   = 1
               GOTO 10
            END IF
            IF(Y(JJ).LT.Y(JJ-1)) THEN
               IF(IMAX.EQ.0) THEN
                  IMAX = JJ-1
                  EPS  = EPS1*Y(IMAX)
               END IF
               IF (Y(JJ).LT.EPS) THEN
                  I1 = JJ
                  GOTO 30
               END IF
            ELSEIF(IMAX.EQ.0.AND.Y(JJ).LT.EPS.AND.JJ.NE.2) THEN
               I0 = JJ
            END IF
         END DO
   30    CONTINUE
         IF((I1-I0).LE.2) THEN
            GOTO 10
         ELSE
            GOTO 20
         END IF
   10 CONTINUE
C
   20 CONTINUE
C
      X0 = X(I0)
      X1 = X(I1)
C
      RETURN
      END
C
C#######################################################################
C