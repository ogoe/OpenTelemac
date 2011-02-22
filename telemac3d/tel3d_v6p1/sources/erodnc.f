C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MODELS EROSION
!>                FOR NON-COHESIVE SEDIMENTS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AC, CF, CFDEP, CREF, DMOY, DT, FLUER, GRAV, HDEP, HN, KSPRATIO, NPOIN2, NPOIN3, RHO0, RHOS, TOB, WC
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, QS
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_ERODNC
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OS(), SUSPENSION_FREDSOE()
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
!>      <td><center> 6.0                                       </center>
!> </td><td> 12/09/2007
!> </td><td> J.-M. HERVOUET 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/06/2003
!> </td><td> CAMILLE LEQUETTE
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CF
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CFDEP
!></td><td>--></td><td>CONCENTRATION DES DEPOTS FRAIS
!>    </td></tr>
!>          <tr><td>CREF
!></td><td><-></td><td>CONCENTRATION DEQUILIBRE
!>    </td></tr>
!>          <tr><td>D50
!></td><td>--></td><td>DIAMETRE MOYEN DES GRAINS
!>    </td></tr>
!>          <tr><td>DMOY
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS HYDRAULIQUE
!>    </td></tr>
!>          <tr><td>FLUER
!></td><td><--</td><td>VALEUR DU FLUX D'EROSION
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>CONSTANTE GRAVITATIONNELLE
!>    </td></tr>
!>          <tr><td>HDEP
!></td><td><-></td><td>EPAISSEUR DE LA COUCHE DES DEPOTS FRAIS
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>HAUTEUR D'EAU A L'INSTANT N
!>    </td></tr>
!>          <tr><td>KSPRATIO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE2D
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>RHO0
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>RHOS
!></td><td>--></td><td>DENSITE DU SEDIMENT
!>    </td></tr>
!>          <tr><td>TA
!></td><td>--></td><td>CONCENTRATION DU SEDIMENT
!>    </td></tr>
!>          <tr><td>TOB
!></td><td>--></td><td>CONTRAINTE DE FROTTEMENT AU FOND
!>    </td></tr>
!>          <tr><td>WC
!></td><td>--></td><td>VITESSE DE CHUTE DU SEDIMENT
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                      SUBROUTINE ERODNC
     &(CFDEP  , WC     , HDEP     , FLUER , TOB   , DT    ,
     & NPOIN2 , NPOIN3 , KSPRATIO , AC    , RHOS  , RHO0  , HN ,
     & GRAV   , DMOY   , CREF     , CF )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AC             |---| 
C| CF             |---| 
C| CFDEP          |-->| CONCENTRATION DES DEPOTS FRAIS
C| CREF           |<->| CONCENTRATION DEQUILIBRE
C| D50            |-->| DIAMETRE MOYEN DES GRAINS
C| DMOY           |---| 
C| DT             |-->| PAS DE TEMPS HYDRAULIQUE
C| FLUER          |<--| VALEUR DU FLUX D'EROSION
C| GRAV           |-->| CONSTANTE GRAVITATIONNELLE
C| HDEP           |<->| EPAISSEUR DE LA COUCHE DES DEPOTS FRAIS
C| HN             |-->| HAUTEUR D'EAU A L'INSTANT N
C| KSPRATIO       |---| 
C| NPOIN2         |-->| NOMBRE DE POINTS DU MAILLAGE2D
C| NPOIN3         |---| 
C| RHO0           |---| 
C| RHOS           |-->| DENSITE DU SEDIMENT
C| TA             |-->| CONCENTRATION DU SEDIMENT
C| TOB            |-->| CONTRAINTE DE FROTTEMENT AU FOND
C| WC             |-->| VITESSE DE CHUTE DU SEDIMENT
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_ERODNC => ERODNC
C     TRIGGERS A PGI COMPILER ERROR
C     USE INTERFACE_SISYPHE, ONLY : SUSPENSION_FREDSOE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)             :: NPOIN2,NPOIN3
!
      DOUBLE PRECISION, INTENT(INOUT) :: HDEP(NPOIN2),FLUER(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN)    :: WC(NPOIN3)
!
      DOUBLE PRECISION, INTENT(IN)    :: DT,CFDEP,GRAV,RHOS,RHO0
      DOUBLE PRECISION, INTENT(IN)    :: KSPRATIO,AC
!
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: DMOY,TOB,CF,HN
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: CREF
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
      DOUBLE PRECISION QS
!
      INTRINSIC MIN,MAX
!
!-----------------------------------------------------------------------
!
!  ---------------------------------------------------------------------
C  ------- COMPUTES THE REFERENCE CONCENTRATION CREF (IN G/L) ----------
!  ---------------------------------------------------------------------
!
!
C     ZYSERMAN & FREDSOE (1994) (BY DEFAULT)
!
C     SUBROUTINE FROM SISYPHE LIBRARY
C MODIF V6P0
C CORRECTION FOR SKIN FRICTION (CF)
C HERE TAKES TAUP = TOB
C      CALL SUSPENSION_FREDSOE(DMOY,CF, TOB,HN,NPOIN2,KSPRATIO,
C     &                        GRAV,RHO0,RHOS,1.D-6,AC,CREF,1.D-3)
C                                            ZERO          HMIN
C                                  TAUP
      CALL SUSPENSION_FREDSOE(DMOY,TOB, NPOIN2,
     &                        GRAV,RHO0,RHOS,1.D-6,AC,CREF)
C                                            ZERO
!
C     UNITS FOR CREF G/L, NOT LIKE IN SISYPHE
!
      CALL OS('X=CX    ',X=CREF,C=RHOS)
!
!  ------------------------------------------------------------
!  -----------------     EROSION STEP    ----------------------
!  ------------------------------------------------------------
!
      DO I=1,NPOIN2
!
C       COMPUTES THE EROSION FLUX
!
        FLUER(I)=-WC(I)*CREF%R(I)
!
C       QUANTITY OF SOLID IN THE LAYER BEFORE EROSION
!
C       CFDEP IN KG/M3 ( ~ 0.65 RHOS )
        QS=CFDEP*HDEP(I)
!
C       LAYER THICKNESS AFTER EROSION
!
        HDEP(I)=MAX(0.D0,HDEP(I)-(FLUER(I)*DT/CFDEP))
!
C       LIMITS THE EROSION FLUX
!
        FLUER(I)=MIN(FLUER(I),QS/DT)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C