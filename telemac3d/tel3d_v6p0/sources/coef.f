C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE COEFFICIENTS RESULTING FROM THE
!>                DISCRETISATION OF THE DIFFERENTIAL EQUATION GOVERNING
!>                THE CONSOLIDATION OF MUDDY BEDS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DSIG1, DTC, EPAI, GRAV, IMAX, IVIDE, NDEB, NPFMAX, RHOS, TRA01
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A, D, I
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TASSEM()

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
!>      <td><center> 5.1                                       </center>
!> </td><td> 13/05/92
!> </td><td> C LE NORMANT (LNH)30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DSIG1
!></td><td>--></td><td>DERIVEE DE LA CONTRAINTE EFFECTIVE
!>                  AU PREMIER POINT DU MAILLAGE
!>    </td></tr>
!>          <tr><td>DTC
!></td><td>--></td><td>PAS DE TEMPS DU PHENOMENE DE CONSOLIDATION
!>    </td></tr>
!>          <tr><td>EPAI
!></td><td>--></td><td>EPAISSEURS DES MAILLES
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>ACCELERATION DE LA PESANTEUR
!>    </td></tr>
!>          <tr><td>IMAX
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>IVIDE
!></td><td>--></td><td>INDICE DES VIDES AUX POINTS DU MAILLAGE
!>                  (MAILLAGE SELON UNE VERTICALE)
!>    </td></tr>
!>          <tr><td>NDEB
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPFMAX
!></td><td>--></td><td>NOMBRE MAXIMUM DE PLANS HORIZONTAUX
!>                  DISCRETISANT LE FOND VASEUX
!>    </td></tr>
!>          <tr><td>RHOS
!></td><td>--></td><td>MASSE VOLUMIQUE DU SEDIMENT
!>    </td></tr>
!>          <tr><td>TRA01
!></td><td>--></td><td>TABLEAU DE TRAVAIL
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE COEF
     &     (IVIDE , EPAI , TRA01 ,
     &      NPFMAX, IMAX , NDEB  ,
     &      RHOS  , GRAV , DTC   , DSIG1    )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DSIG1          |-->| DERIVEE DE LA CONTRAINTE EFFECTIVE
C|                |   | AU PREMIER POINT DU MAILLAGE
C| DTC            |-->| PAS DE TEMPS DU PHENOMENE DE CONSOLIDATION
C| EPAI           |-->| EPAISSEURS DES MAILLES
C| GRAV           |-->| ACCELERATION DE LA PESANTEUR
C| IMAX           |-->| NOMBRE DE POINTS DU MAILLAGE
C| IVIDE          |-->| INDICE DES VIDES AUX POINTS DU MAILLAGE
C|                |   | (MAILLAGE SELON UNE VERTICALE)
C| NDEB           |---| 
C| NPFMAX         |-->| NOMBRE MAXIMUM DE PLANS HORIZONTAUX
C|                |   | DISCRETISANT LE FOND VASEUX
C| RHOS           |-->| MASSE VOLUMIQUE DU SEDIMENT
C| TRA01          |-->| TABLEAU DE TRAVAIL
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
       IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
       INTEGER, INTENT(IN) ::  NPFMAX , IMAX,NDEB
!
       DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPFMAX,6)
       DOUBLE PRECISION, INTENT(IN) :: EPAI(NPFMAX-1), IVIDE(NPFMAX)
       DOUBLE PRECISION, INTENT(IN) :: RHOS , GRAV ,DTC , DSIG1
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
       INTEGER I
       DOUBLE PRECISION A , D
!
!=======================================================================
!
C      -----INITIALISES THE COEFFICIENTS-----
!
        DO I=NDEB,IMAX
          TRA01(I,3)=0.D0
          TRA01(I,4)=1.D0
          TRA01(I,5)=0.D0
          TRA01(I,6)=0.D0
        END DO
!
C      -----COMPUTES THE COEFFICIENTS AT THE MESH NODES-----
!
C       ...NODE ON THE BOTTOM:
        TRA01(NDEB,5)=-1.D0
        TRA01(NDEB,6)=(RHOS-1000.D0)*GRAV*EPAI(1)/DSIG1
!
C       ...INTERIOR NODE:
        DO I=NDEB+1,IMAX-1
!
         IF (TRA01(I,1).GE.1.D-10) THEN
           A=TRA01(I,1)/(EPAI(I-1))
           D=1.D0
         ELSE
           A=TRA01(I,1)/(EPAI(I))
           D=0.D0
         ENDIF
!
         TRA01(I,3)=DTC*(-(TRA01(I,2)+TRA01(I-1,2))/((EPAI(I)+EPAI(I-1))
     &                 *EPAI(I-1))-D*A)
         TRA01(I,5)=DTC*((1.D0-D)*A-((TRA01(I,2)+TRA01(I+1,2))
     &                 /((EPAI(I)+EPAI(I-1))*EPAI(I))))
         TRA01(I,4)=1.D0-TRA01(I,3)-TRA01(I,5)
         TRA01(I,6)=IVIDE(I)
!
        END DO
!
C       ...INTERFACE NODE:
        TRA01(IMAX,6)=IVIDE(IMAX)
!
       RETURN
       END SUBROUTINE COEF

C
C#######################################################################
C