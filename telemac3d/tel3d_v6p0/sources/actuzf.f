C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       UPDATES THE BOTTOM ELEVATION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> EPAI, IVIDE, NPF, NPFMAX, NPOIN2, ZF
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ECOUCH, IPF, IPOIN
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FONVAS()

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
!>      <td><center> 5.1                                       </center>
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>      <tr>
!>      <td><center> 1.2                                       </center>
!> </td><td> 05/05/93
!> </td><td>
!> </td><td> MODIFIED
!> </td></tr>
!>      <tr>
!>      <td><center> 1.2                                       </center>
!> </td><td> 13/05/92
!> </td><td> C LE NORMANT (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>EPAI
!></td><td>--></td><td>EPAISSEURS DES MAILLES DISCRETISANT LE LIT
!>    </td></tr>
!>          <tr><td>IVIDE
!></td><td>--></td><td>INDICE DES VIDES AUX POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPF
!></td><td>--></td><td>NOMBRE DE POINTS DU FOND SUR UNE VERTICALE
!>    </td></tr>
!>          <tr><td>NPFMAX
!></td><td>--></td><td>NOMBRE MAXIMUM DE PLANS HORIZONTAUX
!>                  DISCRETISANT LE FOND VASEUX
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>ZF
!></td><td><-></td><td>COTE DU FOND
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                      SUBROUTINE ACTUZF
     & ( IVIDE , EPAI , ZF , NPOIN2, NPFMAX , NPF )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| EPAI           |-->| EPAISSEURS DES MAILLES DISCRETISANT LE LIT
C| IVIDE          |-->| INDICE DES VIDES AUX POINTS DU MAILLAGE
C| NPF            |-->| NOMBRE DE POINTS DU FOND SUR UNE VERTICALE
C| NPFMAX         |-->| NOMBRE MAXIMUM DE PLANS HORIZONTAUX
C|                |   | DISCRETISANT LE FOND VASEUX
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| ZF             |<->| COTE DU FOND
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
       IMPLICIT NONE
!
!-----------------------------------------------------------------------
!
       INTEGER, INTENT(IN)             :: NPOIN2, NPFMAX
       DOUBLE PRECISION, INTENT(IN)    :: IVIDE(NPFMAX,NPOIN2)
       DOUBLE PRECISION, INTENT(IN)    :: EPAI(NPFMAX-1,NPOIN2)
       DOUBLE PRECISION, INTENT(INOUT) :: ZF(NPOIN2)
       INTEGER, INTENT(IN)             :: NPF(NPOIN2)
!
!-----------------------------------------------------------------------
!
       INTEGER IPOIN , IPF
       DOUBLE PRECISION  ECOUCH
!
!-----------------------------------------------------------------------
!
       DO IPOIN=1,NPOIN2
!
C         -----COMPUTES THE BOTTOM ELEVATION-----
!
          DO IPF=1,NPF(IPOIN)-1
            ECOUCH=(IVIDE(IPF,IPOIN)+IVIDE(IPF+1,IPOIN))/2.D0
            ZF(IPOIN)=ZF(IPOIN)+(1.D0+ECOUCH)*EPAI(IPF,IPOIN)
          END DO
!
       END DO
!
!-----------------------------------------------------------------------
!
       RETURN
       END SUBROUTINE ACTUZF

C
C#######################################################################
C