C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES VISCOSITY USING SMAGORINSKY'S MODEL.
!>  @code
!>                                     (1/2)
!>    NU    =   CS2 * ( 2.0 * SIJ * SIJ )  * (MESH SIZE)**2<br>
!>                         2        2            2
!>                      DU       DV     DU   DV
!>     2*SIJ*SIJ = ( 2*(--) + 2*(--) + (-- + --)
!>                      DX       DY     DY   DX
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CF, MASKEL, MESH, MSK, PROPNU, T1, T2, T3, T4, U, V, VISC
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> CS, CS2, IELMC, IELMU, N, NPOIN
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> VECTOR()
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
!> </td><td> 06/10/1997
!> </td><td> ADRIAN KLINGS (ENPC)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CF
!></td><td>--></td><td>COEFFICIENT DE FROTTEMENT POUR K-EPSILON
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS
!>                  =1. : NORMAL   =0. : ELEMENT MASQUE
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>PROPNU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T1,2,3,4
!></td><td>---</td><td>TABLEAUX DE TRAVAIL
!>    </td></tr>
!>          <tr><td>T2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U , V
!></td><td>--></td><td>COMPOSANTES DE LA VITESSE
!>    </td></tr>
!>          <tr><td>VISC
!></td><td>--></td><td>DIFFUSION TURBULENTE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SMAGOR
     &(VISC,CF,U,V,MESH,T1,T2,T3,T4,MSK,MASKEL,PROPNU)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CF             |-->| COEFFICIENT DE FROTTEMENT POUR K-EPSILON
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
C| MESH           |---| 
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| PROPNU         |---| 
C| T1,2,3,4       |---| TABLEAUX DE TRAVAIL
C| T2             |---| 
C| T3             |---| 
C| T4             |---| 
C| U , V          |-->| COMPOSANTES DE LA VITESSE
C| VISC           |-->| DIFFUSION TURBULENTE
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
      LOGICAL, INTENT(IN) :: MSK
      DOUBLE PRECISION, INTENT(IN)   :: PROPNU
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: VISC,T1,T2,T3,T4
      TYPE(BIEF_OBJ), INTENT(IN)     :: MASKEL,CF,U,V
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER N,NPOIN,IELMU,IELMC
      DOUBLE PRECISION CS,CS2
C
C-----------------------------------------------------------------------
C
      INTRINSIC SQRT
C
C-----------------------------------------------------------------------
C
      CS = 0.1D0
      CS2 = CS**2
C
C-----------------------------------------------------------------------
C
      IELMU = U%ELM
      IELMC = VISC%ELM
C
C     COMPUTES GRADIENTS (IN FACT AVERAGED GRADIENT MULTIPLIED BY
C     A SURFACE WHICH IS THE INTEGRAL OF TEST FUNCTIONS ON THE DOMAIN,
C     THIS SURFACE IS CONSIDERED TO BE (MESH SIZE)**2)
C
      CALL VECTOR(T1,'=','GRADF          X',IELMU,
     &            1.D0,U,U,U,U,U,U,MESH,MSK,MASKEL)
      CALL VECTOR(T2,'=','GRADF          Y',IELMU,
     &            1.D0,U,U,U,U,U,U,MESH,MSK,MASKEL)
      CALL VECTOR(T3,'=','GRADF          X',IELMU,
     &            1.D0,V,V,V,V,V,V,MESH,MSK,MASKEL)
      CALL VECTOR(T4,'=','GRADF          Y',IELMU,
     &            1.D0,V,V,V,V,V,V,MESH,MSK,MASKEL)
C
      NPOIN = VISC%DIM1
C
      DO N = 1,NPOIN
        VISC%R(N)=PROPNU+SQRT((2*T1%R(N)**2+2*T4%R(N)**2
     &                                       +(T2%R(N)+T3%R(N))**2))*CS2
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C