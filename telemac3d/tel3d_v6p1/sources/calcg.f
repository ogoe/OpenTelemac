C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CALLED WHEN MODELLING THE INFLUENCE OF
!>                TURBULENCE ON THE SETTLING VELOCITY.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> NPLAN, NPOIN2, TRAV2, TRAV3, U, UETCAR, V
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IPLAN, IPOIN2, UQUA
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>WCTURB()

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
!>      <td><center>                                           </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>      <tr>
!>      <td><center> 5.4                                       </center>
!> </td><td> 01/08/97
!> </td><td> C LE NORMANT  30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>HN
!></td><td>--></td><td>HAUTEUR D'EAU
!>    </td></tr>
!>          <tr><td>LISRUF
!></td><td>--></td><td>REGIME DE TURBULENCE POUR LE FOND
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>RUGOF
!></td><td>--></td><td>COEFFICIENT DE RUGOSITE DU FOND
!>    </td></tr>
!>          <tr><td>TRAV2,TRAV3
!></td><td><--</td><td>TABLEAUX DE TRAVAIL    NOTE JMH ?????
!>    </td></tr>
!>          <tr><td>U,V
!></td><td>--></td><td>COMPOSANTES DE LA VITESSE
!>    </td></tr>
!>          <tr><td>UETCAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Z
!></td><td>--></td><td>COTE DES NOEUDS DU MAILLAGE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE CALCG
     & (TRAV2,TRAV3,U,V,UETCAR,NPOIN2,NPLAN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| HN             |-->| HAUTEUR D'EAU
C| LISRUF         |-->| REGIME DE TURBULENCE POUR LE FOND
C| NPLAN          |-->| NOMBRE DE PLANS
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE 2D
C| RUGOF          |-->| COEFFICIENT DE RUGOSITE DU FOND
C| TRAV2,TRAV3    |<--| TABLEAUX DE TRAVAIL    NOTE JMH ?????
C| U,V            |-->| COMPOSANTES DE LA VITESSE
C| UETCAR         |---| 
C| Z             |-->| COTE DES NOEUDS DU MAILLAGE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER         , INTENT(IN)    :: NPOIN2,NPLAN
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN2),V(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: UETCAR(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: TRAV2(NPOIN2*NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: TRAV3(NPOIN2*NPLAN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IPOIN2,IPLAN
      DOUBLE PRECISION UQUA
!
!=======================================================================
!
C COMPUTES AUBORF IN TRAV1
!
      DO IPOIN2 = 1,NPOIN2
!
         UQUA = SQRT( (U(IPOIN2))**2 + (V(IPOIN2))**2 )
!
C                              AUBORF * U_B * DU/DZ
!        COMPUTES  G  =  SQRT( --------------------- ) : TRAV3
C                                     NU
!
        DO IPLAN = 1,NPLAN
         TRAV3(IPOIN2+(IPLAN-1)*NPOIN2) =
     &        SQRT(UETCAR(IPOIN2) * UQUA * 1.D06
     &        * TRAV2(IPOIN2+(IPLAN-1)*NPOIN2))
!
        ENDDO
!
      ENDDO
!
!=======================================================================
!
      RETURN
      END
C
C#######################################################################
C