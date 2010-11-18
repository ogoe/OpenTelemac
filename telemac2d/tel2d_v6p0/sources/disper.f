C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE TENSORIAL DISPERSION COEFFICIENTS
!>                ACCORDING TO THE LONGITUDINAL AND TRANSVERSE
!>                COEFFICIENTS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CF, ELDER, H, PROPNU, U, V, VISC
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> COST, I, KL, KT, NORMV, NPOIN, NPX, SINT, USTAR
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D()

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
!>      <td><center> 5.6                                       </center>
!> </td><td> 26/05/2006
!> </td><td> C MOULIN (LNH) 30 87 83 81
!> </td><td> + MODIFS JMH
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CF
!></td><td>--></td><td>COEFFICIENT DE FROTTEMENT
!>    </td></tr>
!>          <tr><td>ELDER
!></td><td>--></td><td>COEFFICIENTS ADIMENSIONNELS DE DISPERSION
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR D'EAU
!>    </td></tr>
!>          <tr><td>PROPNU
!></td><td>--></td><td>VISCOSITE LAMINAIRE
!>    </td></tr>
!>          <tr><td>U,V
!></td><td>--></td><td>COMPOSANTES DE LA VITESSE
!>    </td></tr>
!>          <tr><td>VISC
!></td><td><--</td><td>COEFF DU TENSEUR  DE DISPERSION (DIM. NPOIN)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DISPER
     &( VISC , U , V , H , CF , ELDER , PROPNU )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CF             |-->| COEFFICIENT DE FROTTEMENT
C| ELDER          |-->| COEFFICIENTS ADIMENSIONNELS DE DISPERSION
C| H             |-->| HAUTEUR D'EAU
C| PROPNU         |-->| VISCOSITE LAMINAIRE
C| U,V            |-->| COMPOSANTES DE LA VITESSE
C| VISC           |<--| COEFF DU TENSEUR  DE DISPERSION (DIM. NPOIN)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION, INTENT(IN)  :: ELDER(2),PROPNU
      DOUBLE PRECISION, INTENT(IN)  :: H(*),CF(*),U(*),V(*)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VISC
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,NPOIN,NPX
C
      DOUBLE PRECISION KL,KT,COST,SINT,NORMV,USTAR
C
      INTRINSIC SQRT,MAX
C
C-----------------------------------------------------------------------
C COMPUTES DISPERSION COEFFICIENTS
C-----------------------------------------------------------------------
C
      NPOIN = VISC%DIM1
      NPX   = VISC%MAXDIM1
C
      DO 20 I=1,NPOIN
C
         NORMV = MAX(SQRT(U(I)**2+V(I)**2),1.D-6)
         COST = U(I)/NORMV
         SINT = V(I)/NORMV
         USTAR = SQRT( 0.5D0 * CF(I) * ( U(I)**2 + V(I)**2 ) )
         KL = ELDER(1) * USTAR * H(I)
         KT = ELDER(2) * USTAR * H(I)
         VISC%R(I      ) = PROPNU + ( KL - KT ) * COST**2    + KT
         VISC%R(I+NPX  ) = PROPNU + ( KT - KL ) * COST**2    + KL
         VISC%R(I+2*NPX) = PROPNU + ( KL - KT ) * COST*SINT
C
20    CONTINUE
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C