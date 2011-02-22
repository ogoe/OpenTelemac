C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FLUX OF DEPOSITION AND EROSION
!>                ACCOUNTING FOR THE VERTICAL STRUCTURE.
!><br>            !! NEW SUBROUTINE !!

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DEBUG, DT, ES, FLUER, GRAV, MS_VASE, NCOUCH_TASS, NPOIN, PARTHENIADES, TASS, TAUP, TOCE_VASE, VITCE, XMVE, XMVS, ZERO
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX, FLUER_LOC, I, J, QER_VASE, QE_COUCHE, TEMPS, USTARP
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_SUSPENSION_EROSION_COH
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SUSPENSION_COMPUTATION()

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
!> </td><td> 31/07/2008
!> </td><td> C. VILLARET
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CF
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>DEBUG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUER
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>HAUTEUR D'EAU
!>    </td></tr>
!>          <tr><td>MS_VASE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NCOUCH_TASS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PARTHENIADES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TASS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAUP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOB
!></td><td>--></td><td>FLUX DE DEPOT                               C
!>    </td></tr>
!>          <tr><td>TOCE_VASE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VITCE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMVE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMVS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZERO
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE SUSPENSION_EROSION_COH
     &(TAUP,NPOIN,XMVE,XMVS,GRAV,VITCE,
     & PARTHENIADES,ZERO,DEBUG,
     & FLUER, ES, TOCE_VASE, NCOUCH_TASS, DT, MS_VASE,TASS)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CF             |-->| 
C| DEBUG          |---| 
C| DT             |---| 
C| ES             |---| 
C| FLUER          |---| 
C| GRAV           |---| 
C| HN             |-->| HAUTEUR D'EAU
C| MS_VASE        |---| 
C| NCOUCH_TASS    |---| 
C| NPOIN          |---| 
C| PARTHENIADES   |---| 
C| TASS           |---| 
C| TAUP           |---| 
C| TOB            |-->| FLUX DE DEPOT                               C
C| TOCE_VASE      |---| 
C| VITCE          |---| 
C| XMVE           |---| 
C| XMVS           |---| 
C| ZERO           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE, EX_SUSPENSION_EROSION_COH=>
     &                          SUSPENSION_EROSION_COH
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU

      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE (BIEF_OBJ),  INTENT(IN)    :: TAUP
      INTEGER,          INTENT(IN)    :: NPOIN,DEBUG
      DOUBLE PRECISION, INTENT(IN)    :: XMVE,XMVS,GRAV
      DOUBLE PRECISION, INTENT(IN)    :: VITCE
      DOUBLE PRECISION, INTENT(IN)    :: ZERO,PARTHENIADES
C FOR CONSOLIDATION
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_VASE(NPOIN,10)
      DOUBLE PRECISION, INTENT(IN)     :: TOCE_VASE(10), DT
      INTEGER,          INTENT(IN)    :: NCOUCH_TASS
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUER
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,10)
!
      LOGICAL, INTENT(IN) :: TASS

      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER :: I, J

      DOUBLE PRECISION :: USTARP,AUX
      DOUBLE PRECISION :: FLUER_LOC(10), QER_VASE,TEMPS, QE_COUCHE

!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!

      ! *************************************************  !
      ! IA - FORMULATION FOR COHESIVE SEDIMENTS            !
      !      (WITHOUT CONSOLIDATION: UNIFORM SEDIMENT BED) !                                   !
      ! ******************************************* *****  !

      IF(.NOT.TASS) THEN

        DO I = 1, NPOIN
          USTARP =SQRT(TAUP%R(I)/XMVE)
          IF(VITCE.GT.1.D-8) THEN
            AUX = MAX(((USTARP/VITCE)**2 - 1.D0),0.D0)
          ELSE
            AUX = 0.D0
          ENDIF
          FLUER%R(I) = PARTHENIADES*AUX
        ENDDO

      ELSE

      ! **************************************************** !
      ! IB - FORMULATION FOR COHESIVE SEDIMENTS  + CONSOLIDATION !
      !      (WITH BEDLOAD)                                  !
      ! **************************************************** !

        DO I=1,NPOIN
C
          DO J=1,NCOUCH_TASS
            IF(TAUP%R(I).GT.TOCE_VASE(J))THEN
              FLUER_LOC(J)=PARTHENIADES*
     &              ((TAUP%R(I)/TOCE_VASE(J))-1.D0)
            ELSE
              FLUER_LOC(J)=0.D0
            ENDIF
          ENDDO
          QER_VASE = 0.D0
          TEMPS= DT
C
          DO J= 1, NCOUCH_TASS
            IF(ES(I,J).GE.1.D-6) THEN
C             COMPUTES THE MASS POTENTIALLY ERODABLE IN LAYER J (KG/M2)
              QE_COUCHE = FLUER_LOC(J) *XMVS * TEMPS
              IF(QE_COUCHE.LT.MS_VASE(I,J)) THEN
                QER_VASE = QER_VASE  + QE_COUCHE
                GO TO 10
              ELSE
                QER_VASE = QER_VASE + MS_VASE(I,J)
                TEMPS= TEMPS-MS_VASE(I,J)/FLUER_LOC(J)/XMVS
                TEMPS=MAX(TEMPS,0.D0)
              ENDIF
            ENDIF
          ENDDO
C
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'ATTENTION TOUTES LES COUCHES SONT VIDES'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'BEWARE, ALL LAYERS EMPTY'
          ENDIF
          CALL PLANTE(1)
          STOP

10        CONTINUE
C         BEWARE: PARTHENIADES HAS ALREADY BEEN DEVIDED BY XMVS?
          FLUER%R(I) = QER_VASE/DT/XMVS
C
        ENDDO
      ENDIF
!
!======================================================================!
!======================================================================!
!
      RETURN
      END
C
C#######################################################################
C