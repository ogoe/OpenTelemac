C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE INTEGRAL (0 TO INFINITY) OF THE FUNCTION
!>                GIVEN BY 'FONCRO', USING GAUSS QUADRATURES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A, B, N, XM
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A1, A2, A3, DA, DX, I, J, NFOIS, QGAUSS, SS, W, X, XB, XR, Y2
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> BORNES(), FONCRO()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>QBREK3()

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
!>          <tr><td>QGAUSS
!></td><td><--</td><td>VALEUR DE L'INTEGRALE
!>    </td></tr>
!>          <tr><td>XM
!></td><td>--></td><td>PARAMETRE M DE LA FONCTION A INTEGRER
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        FUNCTION QGAUSS
     &( B     , N     , A     , XM    )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |-->| PARAMETRE A DE LA FONCTION A INTEGRER
C| B             |-->| PARAMETRE B DE LA FONCTION A INTEGRER
C| N             |-->| EXPOSANT N  DE LA FONCTION A INTEGRER
C| QGAUSS         |<--| VALEUR DE L'INTEGRALE
C| XM             |-->| PARAMETRE M DE LA FONCTION A INTEGRER
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C     VARIABLES IN ARGUMENT
C     """""""""""""""""""""
      INTEGER  N
      DOUBLE PRECISION QGAUSS, B     , A     , XM
C
C     LOCAL VARIABLES
C     """"""""""""""""""
      INTEGER  J     , I     , NFOIS
      DOUBLE PRECISION XB    , XR    , DX    , DA    , SS    , W(5)
      DOUBLE PRECISION A1    , A2    , A3    , Y2    , X(5)
C
C     EXTERNAL FUNCTIONS
C     """""""""""""""""""
      DOUBLE PRECISION  FONCRO
      EXTERNAL          FONCRO
C
      DATA X/.1488743389D0,.4333953941D0,.6794095682D0,
     &       .8650633666D0,.9739065285D0/
      DATA W/.2955242247D0,.2692667193D0,.2190863625D0,
     &       .1494513491D0,.0666713443D0/
C
C
      NFOIS = 1
C
      CALL BORNES
     &( B     , N     , A     , XM    , A2    , A3    )
      QGAUSS = 0.D0
      DA = (A3-A2)/DBLE(NFOIS)
C
      DO I=1,NFOIS
         A1 = A2
         A2 = A2+DA
         XB = 0.5D0*(A1+A2)
         XR = 0.5D0*(A2-A1)
         SS = 0.D0
         DO J=1,5
            DX = XR*X(J)
            SS = SS + W(J)*(FONCRO(XB+DX,B,N,A,XM)
     &                     +FONCRO(XB-DX,B,N,A,XM))
         ENDDO
         Y2 = XR*SS
         QGAUSS = QGAUSS + Y2
      ENDDO
C
      RETURN
      END
C
C#######################################################################
C