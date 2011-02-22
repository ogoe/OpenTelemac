C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE BASES FUNCTIONS GRADIENTS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AIRT, DPX, DPY, NS, NT, NU, X, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AIRJI, JT, NUBO1, NUBO2, NUBO3, X1, X2, X3, Y1, Y2, Y3
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>INBIEF()

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
!>      <td><center> 5.4                                       </center>
!> </td><td>
!> </td><td> INRIA
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AIRT
!></td><td>--></td><td>AIRES DES TRIANGLES
!>    </td></tr>
!>          <tr><td>DPX,DPY
!></td><td><--</td><td>GRADIENT DES FONCTIONS DE BASE P1
!>    </td></tr>
!>          <tr><td>NS
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NT
!></td><td>--></td><td>NOMBRE DE TRIANGLES DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NU
!></td><td>--></td><td>NUMEROS DES NOEUDS PAR TRIANGLE
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES NOEUDS DU MAILLAGE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE GRADP
     &(NS,NT,NU,AIRT,X,Y,DPX,DPY)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AIRT           |-->| AIRES DES TRIANGLES
C| DPX,DPY        |<--| GRADIENT DES FONCTIONS DE BASE P1
C| NS             |-->| NOMBRE DE POINTS DU MAILLAGE
C| NT             |-->| NOMBRE DE TRIANGLES DU MAILLAGE
C| NU             |-->| NUMEROS DES NOEUDS PAR TRIANGLE
C| X,Y            |-->| COORDONNEES DES NOEUDS DU MAILLAGE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)           :: NS,NT,NU(NT,3)
      DOUBLE PRECISION, INTENT(IN)  :: X(NS),Y(NS),AIRT(NT)
      DOUBLE PRECISION, INTENT(OUT) :: DPX(3,NT),DPY(3,NT)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER JT ,NUBO1,NUBO2,NUBO3
      DOUBLE PRECISION AIRJI,X1,X2,X3,Y1,Y2,Y3
C
C-----------------------------------------------------------------------
C
      DO JT=1,NT
C
         NUBO1 = NU(JT,1)
         NUBO2 = NU(JT,2)
         NUBO3 = NU(JT,3)
C
         AIRJI = 0.5D0/AIRT(JT)
C
C        COMPUTES THE P1-GRADIENTS
C
         X1 = X(NUBO1)
         Y1 = Y(NUBO1)
         X2 = X(NUBO2)
         Y2 = Y(NUBO2)
         X3 = X(NUBO3)
         Y3 = Y(NUBO3)
C
         DPX(1,JT) = AIRJI*(Y2-Y3)
         DPX(2,JT) = AIRJI*(Y3-Y1)
         DPX(3,JT) = AIRJI*(Y1-Y2)
         DPY(1,JT) = AIRJI*(X3-X2)
         DPY(2,JT) = AIRJI*(X1-X3)
         DPY(3,JT) = AIRJI*(X2-X1)
C
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C