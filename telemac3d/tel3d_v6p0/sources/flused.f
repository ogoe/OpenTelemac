C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       WRITES THE FLUXES AT THE BOTTOM AND FREE SURFACE
!>                FOR THE SEDIMENT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  CHECKS MASS BALANCE AT THE BOTTOM AND FREE SURFACE.
!>         RESULTS IN A BOUNDARY CONDITION ON SEDIMENT FLUXES.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ATABOF, ATABOS, BTABOF, BTABOS, FLUER, GRADZFX, GRADZFY, GRADZSX, GRADZSY, HN, KLOG, LITABF, LITABS, NPLAN, NPOIN2, NPOIN3, PDEPOT, SEDCO, TA, TOB, TOCD, WC, X, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, NZ
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CLSEDI()

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
!>      <td><center> 5.5                                       </center>
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
!>          <tr><td>ATABO,BTABO
!></td><td><--</td><td>LOI LOG SUR TRACEURS ACTIFS: ATABO*TA + BTABO
!>    </td></tr>
!>          <tr><td>ATABOF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ATABOS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BTABOF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>BTABOS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F,  S
!></td><td>---</td><td>F : FOND     S : SURFACE
!>    </td></tr>
!>          <tr><td>FLUER
!></td><td>--></td><td>FLUX D'EROSION EN CHAQUE POINT 2D
!>    </td></tr>
!>          <tr><td>GRADZFX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRADZFY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRADZSX
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRADZSY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>HAUTEUR D'EAU
!>    </td></tr>
!>          <tr><td>KLOG
!></td><td>--></td><td>INDICATEUR DE PAROI SOLIDE
!>    </td></tr>
!>          <tr><td>LITA,BF
!></td><td><-></td><td>TYPE COND. LIMITES SUR TA         : FOND
!>    </td></tr>
!>          <tr><td>LITA,BS
!></td><td><-></td><td>TYPE COND. LIMITES SUR TA         : SURFACE
!>    </td></tr>
!>          <tr><td>LITABF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LITABS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS DISCRETISANT LA VERTICALE
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NOMBRE DE POINTS 3D
!>    </td></tr>
!>          <tr><td>PDEPOT
!></td><td><--</td><td>PROBABILITE DE DEPOT EN CHAQUE POINT 2D
!>    </td></tr>
!>          <tr><td>SEDCO
!></td><td>--></td><td>LOGIQUE POUR SEDIMENT COHESIF
!>    </td></tr>
!>          <tr><td>TA
!></td><td>--></td><td>CONCENTRATION EN SEDIMENTS
!>    </td></tr>
!>          <tr><td>TOB
!></td><td>--></td><td>CONTRAINTE DE FROTTEMENT AU FOND
!>    </td></tr>
!>          <tr><td>TOCD
!></td><td>--></td><td>CONTRAINTE CRITIQUE DE DEPOT
!>    </td></tr>
!>          <tr><td>WC
!></td><td>--></td><td>VITESSE DE CHUTE DU SEDIMENT
!>    </td></tr>
!>          <tr><td>X,Y,Z
!></td><td>--></td><td>COORDONNEES DU MAILLAGE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FLUSED
     & (ATABOF , BTABOF , ATABOS , BTABOS ,
     &  LITABF , LITABS , TA     , WC     ,
     &  X      , Y      , Z      , HN     ,
     &  GRADZFX, GRADZFY, GRADZSX, GRADZSY,
     &  TOB    , PDEPOT , FLUER  , TOCD   ,
     &  NPOIN3 , NPOIN2 , NPLAN  , KLOG   , SEDCO)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ATABO,BTABO    |<--| LOI LOG SUR TRACEURS ACTIFS: ATABO*TA + BTABO
C| ATABOF         |---| 
C| ATABOS         |---| 
C| BTABOF         |---| 
C| BTABOS         |---| 
C| F,  S          |---| F : FOND     S : SURFACE
C| FLUER          |-->| FLUX D'EROSION EN CHAQUE POINT 2D
C| GRADZFX        |---| 
C| GRADZFY        |---| 
C| GRADZSX        |---| 
C| GRADZSY        |---| 
C| HN             |-->| HAUTEUR D'EAU
C| KLOG           |-->| INDICATEUR DE PAROI SOLIDE
C| LITA,BF        |<->| TYPE COND. LIMITES SUR TA         : FOND
C| LITA,BS        |<->| TYPE COND. LIMITES SUR TA         : SURFACE
C| LITABF         |---| 
C| LITABS         |---| 
C| NPLAN          |-->| NOMBRE DE PLANS DISCRETISANT LA VERTICALE
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| NPOIN3         |-->| NOMBRE DE POINTS 3D
C| PDEPOT         |<--| PROBABILITE DE DEPOT EN CHAQUE POINT 2D
C| SEDCO          |-->| LOGIQUE POUR SEDIMENT COHESIF
C| TA             |-->| CONCENTRATION EN SEDIMENTS
C| TOB            |-->| CONTRAINTE DE FROTTEMENT AU FOND
C| TOCD           |-->| CONTRAINTE CRITIQUE DE DEPOT
C| WC             |-->| VITESSE DE CHUTE DU SEDIMENT
C| X,Y,Z          |-->| COORDONNEES DU MAILLAGE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN3, NPOIN2, NPLAN, KLOG
      LOGICAL, INTENT(IN) :: SEDCO
!
C     BOTTOM
!     ****
!
C     BY POINTS
!     ----------
!
      INTEGER, INTENT(IN) :: LITABF(NPOIN2)
!
C     BY FACES
!     ---------
      DOUBLE PRECISION, INTENT(INOUT) :: ATABOF(NPOIN2), BTABOF(NPOIN2)
!
C     FREE SURFACE
!     *******
!
C     BY POINTS
!     ----------
!
      INTEGER, INTENT(INOUT) :: LITABS(NPOIN2)
!
C     BY FACES
!     ---------
      DOUBLE PRECISION, INTENT(INOUT) :: ATABOS(NPOIN2), BTABOS(NPOIN2)
!
C     OTHER ARRAYS
!
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN3), Y(NPOIN3), Z(NPOIN3)
      DOUBLE PRECISION, INTENT(IN) :: TA(NPOIN3), WC(NPOIN3)
      DOUBLE PRECISION, INTENT(IN) :: GRADZFX(NPOIN2), GRADZFY(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: GRADZSX(NPOIN2), GRADZSY(NPOIN2)
      DOUBLE PRECISION, INTENT(IN) :: HN(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TOB(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: PDEPOT(NPOIN2), FLUER(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN) :: TOCD
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
      DOUBLE PRECISION NZ
!
      INTRINSIC MAX
!
!-----------------------------------------------------------------------
!
C     COMPUTES THE DEPOSITION PROBABILITY
!
      IF(SEDCO) THEN
!
C       COHESIVE SEDIMENT
!
        DO I=1,NPOIN2
          IF(LITABF(I).EQ.KLOG) THEN
            PDEPOT(I)=MAX(1.D0-(TOB(I)/MAX(TOCD,1.D-6)),0.D0)
          ELSE
            PDEPOT(I)=0.D0
          ENDIF
        ENDDO
!
      ELSE
!
C       NON COHESIVE SEDIMENT : PDEPOT = 1.
!
        CALL OV('X=C     ',PDEPOT,PDEPOT,PDEPOT,1.D0,NPOIN2)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
C     COMMON COMPUTATION OF THE TRACER FLUX ON THE BOTTOM
!
      DO I=1,NPOIN2
!
C       ALREADY DONE IN LIMI3D (AND ALL BOTTOM POINTS ARE KLOG IF NOT
C                               MODIFIED BY USER IN BORD3D)
C       ATABOF(I)=0.D0
C       BTABOF(I)=0.D0
!
        IF(LITABF(I).EQ.KLOG) THEN
!
C         COMPONENT ALONG Z OF THE OUTGOING NORMAL VECTOR
!
          NZ = 1.D0+GRADZFX(I)**2+GRADZFY(I)**2
          NZ = -1.D0/SQRT(NZ)
C         WC
          ATABOF(I) = - WC(I) * PDEPOT(I) * NZ
          BTABOF(I) = - FLUER(I) * NZ
!
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
C     BOUNDARY CONDITION AT THE FREE SURFACE
!
C     FLUX  = 0 (SETTLING VELOCITY FLUX + DIFFUSIVE FLUX)
!
C     ALREADY DONE IN LIMI3D !!
!
C     DO I=1,NPOIN2
C       ATABOS(I)=0.D0
C       BTABOS(I)=0.D0
C     ENDDO
!
!-----------------------------------------------------------------------
!
        RETURN
        END
C
C#######################################################################
C