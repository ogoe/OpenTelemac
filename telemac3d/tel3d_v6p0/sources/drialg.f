C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE DAMPING FUNCTION ACCORDING TO THE
!>                RICHARDSON NUMBER FOR VISCOSITIES OBTAINED USING A
!>                MIXING LENGTH MODEL.
!>  @code
!> J.C. SOLIVA 'S ALGEBRAIC TURBULENCE MODEL IN SHORT :
!>
!>  FROM THE REYNOLDS TENSOR EQUATIONS
!>  CLOSES THE SYSTEM MODELLING MOMENTS OF ORDER > 2
!>  ASSUMES THAT TURBULENCE IS IN EQUILIBRIUM LOCALLY
!>  ---> TRANSPORT IS NEGLECTED
!>  GETS AN ALGEBRAIC SYSTEM, THE SOLUTION OF WHICH
!>  ONLY DEPENDS ON THE RICHARDSON NUMBER
!>  CAN THUS COMPUTE THE CORRELATIONS UW AND TW
!>
!> HOWEVER, J.C. SOLIVA 'S MODELLING WAS NOT RETAINED, EXCEPT FOR
!> TURBULENT DISSIPATION
!>      EPSILON = CE*(K**1.5)/L   WITH K TURBULENT KINETIC ENERGY
!>                                     L TURBULENCE MACRO-SCALE
!>      CE = 0.47 (TO GET BACK TO KARMAN CONSTANT)
!>
!>
!> THE MODELLING DESCRIBED IN P.L. VIOLLET 'S PAPER WAS CONSIDERED
!> (WE SAY SO!).
!>
!> CONSTANTS ARE FROM LAUNDER AND SPALDING.
!>
!> THE COMPUTATION DETAILS ARE IN 'A23AQ13.MITRI.LISP(MODTURBK)'
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  TAKEN BY DEFAULT IN THE COMPUTATION IF NOTHING IS SPECIFIED,
!>            IE IF 'DAMPING FUNCTION'=1 IN THE STEERING FILE.

!>  @reference "TURBULENCE MODELS FROM STATISTICAL PARAMETERS AT
!>              ONE POINT : K-EPSILON MODEL"
!>              E.D.F. BULLETIN DE LA DIRECTION DES ETUDES ET RECHERCHES -
!>              SERIE A NUCLEAIRE, HYDRAULIQUE, THERMIQUE. NO1 1987
!>              PP 35-47. (P.L. VIOLLET 'S PAPER)

!>  @reference "MODELE TRIDIMENSIONNEL D'ECOULEMENTS MESO-METEOROLOGIQUES.
!>              ETUDE DE LA CONVECTION-DIFFUSION D'UN POLLUANT PASSIF A
!>              CETTE ECHELLE"
!>             (PP 37 TO 44 AND APPENDIX 1). J.C. SOLIVA 'S THESIS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> FRT, FRV, NPOIN3, RI
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, Q, Q2, TW, UW
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>VISCLM()

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
!> </td><td> 25/11/97
!> </td><td> F LEPEINTRE (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>FRI
!></td><td><--</td><td>FONCTION D'AMORTISSEMENT
!>    </td></tr>
!>          <tr><td>FRT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FRV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>RI
!></td><td>--></td><td>NOMBRE DE RICHARDSON
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DRIALG
     & (FRV, FRT, RI, NPOIN3)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| FRI            |<--| FONCTION D'AMORTISSEMENT
C| FRT            |---| 
C| FRV            |---| 
C| NPOIN3         |-->| NOMBRE DE POINTS DU MAILLAGE 3D
C| RI             |-->| NOMBRE DE RICHARDSON
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN3
      DOUBLE PRECISION, INTENT(INOUT) :: FRV(NPOIN3), FRT(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: RI(NPOIN3)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
      DOUBLE PRECISION Q2, UW, TW, Q
!
      INTRINSIC SQRT
!
!***********************************************************************
C                            ____      ____      ____
C ADIMENSIONAL CORRELATIONS  T'W' AND  U'W' ( =  V'W' )
C (MAKES SURE THAT RI=0., FRI=1. FOR VELOCITIES)
!
      DO I=1,NPOIN3
!
C TURBULENT KINETIC ENERGY
!
        IF(RI(I).LT.1.16D0) THEN
!
      Q2=SQRT(4.507462D0*RI(I)**2+3.740367D-1*RI(I)+6.028062D-2)
     &   -2.412442D0*RI(I)+2.455211D-1
!
      Q = SQRT(Q2)
C                          ____
C ADIMENSIONAL CORRELATION T'W'
!
      TW=(Q**3*(7.421047D-1*Q**2+
     &    2.147382D-1*RI(I)))/(9.999995D-1*Q**2*RI(I)-1.033492D0*Q**2+
     &    2.893635D-1*RI(I)**2-3.280470D0*RI(I))
C                          ____
C ADIMENSIONAL CORRELATION U'W'
!
      UW=(Q**3*(4.857410D-1*Q**2+1.541820D0*RI(I)))/(9.999995D-1*Q
     & **2*RI(I)-1.033492D0*Q**2+2.893635D-1*RI(I)**2-3.280470D0*RI(I))
!
C DIVIDES BY -UW FOR RI=0. TO GET 1.
C IT IS INDEED SOUGHT THAT -UW (FOR RI=0.) BE EQUAL TO THE SQUARE OF
C THE KARMAN CONSTANT. IN VISCLM, THE MIXING LENGTH HAS ALREADY BEEN
C MULTIPLIED BY THE SQUARE OF THE KARMAN CONSTANT.
!
          FRV(I) = -UW/0.161724D0
          FRT(I) = -TW/0.161724D0
!
        ELSE
!
          FRV(I) = 0.D0
          FRT(I) = 0.D0
!
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE DRIALG
C
C#######################################################################
C