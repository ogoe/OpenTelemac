C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       MODELS THE MUD BED EVOLUTION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CFDEP, CFMAX, CONC, DT, DTC, EPAI, EPAI0, GIBSON, GRAV, HDEP, IVIDE, LT, NCOUCH, NPF, NPFMAX, NPOIN2, NPOIN3, PDEPOT, RHOS, TA, TASSE, TEMP, TRA01, TRA02, TRA03, TREST, WC, ZF, ZR
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, IC, IPOIN
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_FONVAS
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ACTUZF(), GESTDP(), OV(), TASSEC(), TASSEM()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC3D()

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
!> </td><td> C LE NORMANT (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CFDEP
!></td><td>--></td><td>CONCENTRATION(G/L) DE LA VASE QUI SE DEPOSE
!>    </td></tr>
!>          <tr><td>CFMAX
!></td><td>--></td><td>CONCENTRATION(G/L) DE LA VASE TASSEE
!>    </td></tr>
!>          <tr><td>CONC
!></td><td>--></td><td>MUD BED LAYER CONCENTRATION
!>                  (MULTILAYER MODEL)
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS HYDRAULIQUE
!>    </td></tr>
!>          <tr><td>DTC
!></td><td>--></td><td>PAS DE TEMPS DU PHENOMENE DE CONSOLIDATION
!>    </td></tr>
!>          <tr><td>EPAI
!></td><td>--></td><td>THICKNESS OF SOLID FRACTION OF THE BED LAYER
!>                  (EPAI=DZ/(1+IVIDE), DZ BED LAYER THICKNESS
!>    </td></tr>
!>          <tr><td>EPAI0
!></td><td>--></td><td>EPAISSEUR DE REFERENCE POUR CREER
!>                  DE NOUVELLES MAILLES
!>    </td></tr>
!>          <tr><td>GIBSON
!></td><td>--></td><td>LOGIQUE POUR MODELE DE GIBSON
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>ACCELERATION DE LA PESANTEUR
!>    </td></tr>
!>          <tr><td>HDEP
!></td><td><-></td><td>THICKNESS OF FRESH DEPOSIT(FLUID MUD LAYER)
!>    </td></tr>
!>          <tr><td>IVIDE
!></td><td><-></td><td>VOID RATIO
!>                  (GIBSON MODEL ONLY)
!>    </td></tr>
!>          <tr><td>LT
!></td><td>--></td><td>NUMERO DU PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>NCOUCH
!></td><td>--></td><td>NOMBRE DE COUCHES DISCRETISANT LE FOND VASEUX
!>                  (MODELE DE TASSEMENT MULTICOUCHES)
!>    </td></tr>
!>          <tr><td>NPF
!></td><td>--></td><td>NOMBRE DE POINTS DU FOND  SUR UNE VERTICALE
!>    </td></tr>
!>          <tr><td>NPFMAX
!></td><td>--></td><td>MAXIMUM NUMBER OF HORIZONTAL PLANES
!>                  DISCRETIZATION OF MUD BED (GIBSON MODEL)
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NUMBER OF POINTS  (2D MESH)
!>    </td></tr>
!>          <tr><td>NPOIN3
!></td><td>--></td><td>NUMBER OF POINTS  (3D MESH)
!>    </td></tr>
!>          <tr><td>PDEPOT
!></td><td><--</td><td>PROBABILITY OF DEPOSIT
!>    </td></tr>
!>          <tr><td>RHOS
!></td><td>--></td><td>MASSE VOLUMIQUE DU SEDIMENT
!>    </td></tr>
!>          <tr><td>TA
!></td><td>--></td><td>ACTIVE TRACOR
!>    </td></tr>
!>          <tr><td>TASSE
!></td><td>--></td><td>LOGIQUE POUR MODELE DE TASSEMENT MULTICOUCHES
!>    </td></tr>
!>          <tr><td>TEMP
!></td><td><-></td><td>TIME COUNTER FOR CONSOLIDATION MODEL
!>                  (MULTILAYER MODEL)
!>    </td></tr>
!>          <tr><td>TRA01,2,3
!></td><td><-></td><td>WORKING ARRAYS
!>    </td></tr>
!>          <tr><td>TRA02
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TRA03
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TREST
!></td><td>--></td><td>CONSOLIDATION TIME SCALE
!>                  (ONLY FOR MULTILAYER MODEL)
!>    </td></tr>
!>          <tr><td>WC
!></td><td>--></td><td>SETTLING VELOCITY
!>    </td></tr>
!>          <tr><td>ZF
!></td><td><-></td><td>BOTTOM ELEVATION
!>    </td></tr>
!>          <tr><td>ZR
!></td><td>--></td><td>RIGID BED LEVEL
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                      SUBROUTINE FONVAS
     &(IVIDE  , EPAI   , CONC  , TREST  , TEMP   , HDEP  ,
     & PDEPOT , ZR     , ZF    , TA     , WC     , TRA01 ,
     & TRA02  , TRA03  , NPOIN2, NPOIN3 , NPFMAX , NCOUCH,
     & NPF    , LT     , DT     , DTC   , GRAV   , RHOS  ,
     & CFMAX  , CFDEP  , EPAI0 , TASSE  , GIBSON )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CFDEP          |-->| CONCENTRATION(G/L) DE LA VASE QUI SE DEPOSE
C| CFMAX          |-->| CONCENTRATION(G/L) DE LA VASE TASSEE
C| CONC           |-->| MUD BED LAYER CONCENTRATION
C|                |   | (MULTILAYER MODEL)
C| DT             |-->| PAS DE TEMPS HYDRAULIQUE
C| DTC            |-->| PAS DE TEMPS DU PHENOMENE DE CONSOLIDATION
C| EPAI           |-->| THICKNESS OF SOLID FRACTION OF THE BED LAYER
C|                |   | (EPAI=DZ/(1+IVIDE), DZ BED LAYER THICKNESS
C| EPAI0          |-->| EPAISSEUR DE REFERENCE POUR CREER
C|                |   | DE NOUVELLES MAILLES
C| GIBSON         |-->| LOGIQUE POUR MODELE DE GIBSON
C| GRAV           |-->| ACCELERATION DE LA PESANTEUR
C| HDEP           |<->| THICKNESS OF FRESH DEPOSIT(FLUID MUD LAYER)
C| IVIDE          |<->| VOID RATIO
C|                |   | (GIBSON MODEL ONLY)
C| LT             |-->| NUMERO DU PAS DE TEMPS
C| NCOUCH         |-->| NOMBRE DE COUCHES DISCRETISANT LE FOND VASEUX
C|                |   | (MODELE DE TASSEMENT MULTICOUCHES)
C| NPF            |-->| NOMBRE DE POINTS DU FOND  SUR UNE VERTICALE
C| NPFMAX         |-->| MAXIMUM NUMBER OF HORIZONTAL PLANES
C|                |   | DISCRETIZATION OF MUD BED (GIBSON MODEL)
C| NPOIN2         |-->| NUMBER OF POINTS  (2D MESH)
C| NPOIN3         |-->| NUMBER OF POINTS  (3D MESH)
C| PDEPOT         |<--| PROBABILITY OF DEPOSIT
C| RHOS           |-->| MASSE VOLUMIQUE DU SEDIMENT
C| TA             |-->| ACTIVE TRACOR
C| TASSE          |-->| LOGIQUE POUR MODELE DE TASSEMENT MULTICOUCHES
C| TEMP           |<->| TIME COUNTER FOR CONSOLIDATION MODEL
C|                |   | (MULTILAYER MODEL)
C| TRA01,2,3      |<->| WORKING ARRAYS
C| TRA02          |---| 
C| TRA03          |---| 
C| TREST          |-->| CONSOLIDATION TIME SCALE
C|                |   | (ONLY FOR MULTILAYER MODEL)
C| WC             |-->| SETTLING VELOCITY
C| ZF             |<->| BOTTOM ELEVATION
C| ZR             |-->| RIGID BED LEVEL
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_FONVAS => FONVAS
C
      IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) ::  LT, NPOIN2, NPOIN3
      INTEGER, INTENT(IN) ::  NPFMAX, NCOUCH
!
      DOUBLE PRECISION, INTENT(INOUT) :: IVIDE(NPFMAX,NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TREST(NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: TEMP(NCOUCH,NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: HDEP(NPOIN2), PDEPOT(NPOIN2)
!
      DOUBLE PRECISION, INTENT(INOUT) :: EPAI(NPFMAX-1,NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN)    :: CONC(NCOUCH)
      DOUBLE PRECISION, INTENT(IN)    :: ZR(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: ZF(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: TA(NPOIN3), WC(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPFMAX,6)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA02(NPFMAX),TRA03(NPFMAX)
!
      DOUBLE PRECISION, INTENT(IN) :: DT, RHOS, GRAV, DTC
      DOUBLE PRECISION, INTENT(INOUT) ::  CFMAX , CFDEP, EPAI0
!
      INTEGER, INTENT(INOUT) ::  NPF(NPOIN2)
!
      LOGICAL, INTENT(IN) :: TASSE, GIBSON
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION C
      INTEGER IPOIN, IC
      INTRINSIC DMOD
!
!=======================================================================
!
C     COMPUTES THE DEPOSITED QUANTITY (IN MATERIAL COORDINATES)
!
      IF(TASSE) THEN
        DO IPOIN=1,NPOIN2
          EPAI(NCOUCH,IPOIN)=EPAI(NCOUCH,IPOIN)-
     &         WC(IPOIN)*PDEPOT(IPOIN)*TA(IPOIN)*DT/CONC(NCOUCH)
        ENDDO
      ELSE
        DO IPOIN=1,NPOIN2
          HDEP(IPOIN)=HDEP(IPOIN)-
     &                WC(IPOIN)*PDEPOT(IPOIN)*TA(IPOIN)*DT/CFDEP
        ENDDO
      ENDIF
!
C     STARTS THE COMPUTATION OF BOTTOM ELEVATION
!
      CALL OV( 'X=Y     ', ZF, ZR, ZR, C, NPOIN2)
!
      IF (GIBSON) THEN
!
C     -----CHECKS THE TIMESTEP TO TAKE CONSOLIDATION-----
C     -----              INTO ACCOUNT               -----
!
         IF (DMOD(LT*DT,DTC).LT.1.D-8) THEN
!
C     -----MANAGES THE DEPOSITED QUANTITY : CREATES-----
C     -----     NEW LAYERS NEAR THE MUDDY BED      -----
!
           CALL GESTDP( IVIDE  , EPAI   , HDEP    ,
     &                  NPOIN2 , NPFMAX , NPF     ,
     &                  EPAI0  , CFDEP  , RHOS    )
!
C     -----CONSOLIDATES THE MUDDY BED-----
C     -----    (GIBSON EQUATION)     -----
!
           CALL TASSEM( IVIDE , EPAI ,
     &                  NPOIN2, NPFMAX, NPF  ,
     &                  GRAV  , RHOS  , DTC  , CFMAX ,
     &                  TRA01 , TRA02 ,TRA03 )
!
         ENDIF
!
C     -----UPDATES THE BOTTOM ELEVATION-----
!
         CALL ACTUZF(IVIDE,EPAI,ZF,NPOIN2,NPFMAX,NPF)
!
      ELSEIF(TASSE) THEN
!
C     -----MODELS CONSOLIDATION (SIMPLE)-----
!
         IF (DMOD(LT*DT,DTC).LT.1.D-8)
     &    CALL TASSEC( CONC   , EPAI , TREST , TEMP , DTC ,
     &                 NPOIN2 , NCOUCH )
!
C     -----UPDATES THE BOTTOM ELEVATION-----
!
          DO IPOIN = 1 , NPOIN2
            DO IC = 1 , NCOUCH
              ZF(IPOIN) = ZF(IPOIN) + EPAI(IC,IPOIN)
            ENDDO
            HDEP(IPOIN)=0.D0
          ENDDO
!
      ENDIF
!
      CALL OV( 'X=Y+Z   ' , ZF, ZF, HDEP, C, NPOIN2)
!
!=======================================================================
!
      RETURN
      END SUBROUTINE FONVAS
C
C#######################################################################
C