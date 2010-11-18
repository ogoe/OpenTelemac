C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FLUX OF DEPOSITION AND EROSION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_SISYPHE, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AC, ACLADM, AVAIL, CHARR, CS, CSTAEQ, DEBUG, DT, ES, FLUER_SABLE, FLUER_VASE, GRAV, HMIN, HN, ICQ, KARMAN, MS_SABLE, MS_VASE, NCOUCH_TASS, NPOIN, NSICLA, PARTHENIADES, QSC, TAUP, TOCE_MIXTE, TOCE_VASE, XMVE, XMVS, XWC, ZERO, ZREF
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_SISYPHE :<br>
!> @link DECLARATIONS_SISYPHE::FDM FDM@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> FLUERSABLE, FLUERVASE, FLUER_LOC, I, J, N, QER_SABLE, QER_VASE, QE_MOY, TEMPS, TOCE_SABLE
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_FLUX_MIXTE
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> SUSPENSION_BIJKER(), SUSPENSION_FREDSOE()
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
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ACLADM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AVAIL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CHARR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CSTAEQ
!></td><td>---</td><td>
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
!>          <tr><td>FLUER_SABLE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLUER_VASE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HMIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ICQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KARMAN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MS_SABLE
!></td><td>---</td><td>
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
!>          <tr><td>NSICLA
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>PARTHENIADES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QSC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TAUP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOCE_MIXTE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>TOCE_VASE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMVE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XMVS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XWC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZERO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZREF
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
        SUBROUTINE SUSPENSION_FLUX_MIXTE!
     &  (TAUP,HN,ACLADM,CS,NPOIN,
     &   CHARR,XMVE,XMVS,GRAV,HMIN,XWC,
     &   ZERO,KARMAN,PARTHENIADES,FLUER_SABLE,FLUER_VASE,ZREF,
     &   AC,CSTAEQ,QSC,ICQ,DEBUG,AVAIL,NSICLA,ES,
     &   TOCE_VASE,NCOUCH_TASS,DT,TOCE_MIXTE,MS_SABLE,MS_VASE)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AC             |---| 
C| ACLADM         |---| 
C| AVAIL          |---| 
C| CHARR          |---| 
C| CS             |---| 
C| CSTAEQ         |---| 
C| DEBUG          |---| 
C| DT             |---| 
C| ES             |---| 
C| FLUER_SABLE    |---| 
C| FLUER_VASE     |---| 
C| GRAV           |---| 
C| HMIN           |---| 
C| HN             |---| 
C| ICQ            |---| 
C| KARMAN         |---| 
C| MS_SABLE       |---| 
C| MS_VASE        |---| 
C| NCOUCH_TASS    |---| 
C| NPOIN          |---| 
C| NSICLA         |---| 
C| PARTHENIADES   |---| 
C| QSC            |---| 
C| TAUP           |---| 
C| TOCE_MIXTE     |---| 
C| TOCE_VASE      |---| 
C| XMVE           |---| 
C| XMVS           |---| 
C| XWC            |---| 
C| ZERO           |---| 
C| ZREF           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_SISYPHE, EX_FLUX_MIXTE=>SUSPENSION_FLUX_MIXTE
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY : FDM
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU


      ! 2/ GLOBAL VARIABLES
      ! -------------------
      TYPE (BIEF_OBJ),  INTENT(IN)    :: TAUP,HN,ACLADM,CS
      INTEGER,          INTENT(IN)    :: NPOIN,DEBUG,NSICLA
      INTEGER,          INTENT(IN)    :: NCOUCH_TASS
      LOGICAL,          INTENT(IN)    :: CHARR
      DOUBLE PRECISION, INTENT(IN)    :: XMVE, XMVS, GRAV, HMIN
      DOUBLE PRECISION, INTENT(IN)    :: XWC
      DOUBLE PRECISION, INTENT(IN)    :: ZERO, KARMAN, PARTHENIADES
      TYPE (BIEF_OBJ),  INTENT(IN)    :: ZREF
      DOUBLE PRECISION, INTENT(INOUT) :: AC,AVAIL(NPOIN,10,NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,10)
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: CSTAEQ
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: FLUER_SABLE,FLUER_VASE
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_SABLE(NPOIN,10)
      DOUBLE PRECISION,  INTENT(INOUT) :: MS_VASE(NPOIN,10)
      DOUBLE PRECISION,  INTENT(INOUT) ::TOCE_MIXTE(NPOIN,10)
C
      DOUBLE PRECISION, INTENT(IN)      :: DT
C
      TYPE(BIEF_OBJ),   INTENT(IN)       ::  QSC
      INTEGER,          INTENT (IN)      :: ICQ
C
      DOUBLE PRECISION, INTENT(IN)     :: TOCE_VASE(10)
C
      ! 3/ LOCAL VARIABLES
      ! ------------------
      INTEGER I, J,N
      DOUBLE PRECISION FLUERSABLE,FLUERVASE,FLUER_LOC(10)
C
      DOUBLE PRECISION QE_MOY,TOCE_SABLE,TEMPS,QER_VASE,QER_SABLE
!
!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!
!

      ! ******************************************** !
      ! I - COMPUTES THE CRITICAL SHEAR STRESS
      !    --> TOCE_SABLE
      ! ******************************************** !
C
C COMPUTES TOCE_SABLE VIA THE SHIELDS PARAMETER
         TOCE_SABLE= AC*(XMVS-XMVE)*GRAV*FDM(1)
C
      ! **************************************** !
!        II-COMPUTES EROSION
      ! **************************************** !
C---------DOES THE EROSION COMPUTATION ONLY ONCE (SAND FOR EXAMPLE
C         BECAUSE THE COMPUTED FLUX IS A GLOBAL FLUX COMMON TO THE 2 SEDIMENTS)
C---------COMPUTES THE THEORETICAL FLUX OF EROSION FOR EACH (SEDIMENT INFINITELY AVAILABLE IN EACH LAYER)
C
C---------COMPUTES THE CRITICAL STRESS FOR EACH LAYER AS A FUNCTION OF THE PROPORTION OF MUD
      DO J=1,NCOUCH_TASS
        DO I=1,NPOIN
          IF(AVAIL(I,J,2).LE.0.3D0)THEN
            TOCE_MIXTE(I,J)=TOCE_SABLE
          ELSEIF(AVAIL(I,J,2).GE.0.5D0)THEN
             TOCE_MIXTE(I,J)=TOCE_VASE(J)
          ELSE
                 TOCE_MIXTE(I,J)= TOCE_SABLE +
     &   (AVAIL(I,J,2)-0.3D0)*(TOCE_VASE(J)-TOCE_SABLE)/(0.5D0-0.3D0)
          ENDIF
        ENDDO
      ENDDO
C CV MODIFICATIONS: INTRODUCE TOCE IN ARGUMENT
C         AC(I) = TOCE_MIXTE(I,J)/((XMVS-XMVE)*GRAV*ACLADM%R(I))
C
        IF(ICQ.EQ.1) THEN
          IF (DEBUG > 0) WRITE(LU,*) 'SUSPENSION_FREDSOE'
C
           CALL SUSPENSION_FREDSOE(ACLADM,TAUP,NPOIN,
     &         GRAV,XMVE,XMVS,ZERO,AC,CSTAEQ)
C
          IF (DEBUG > 0) WRITE(LU,*) 'END SUSPENSION_FREDSOE'
C
        ELSEIF(ICQ.EQ.2) THEN
C
          IF (DEBUG > 0) WRITE(LU,*) 'SUSPENSION_BIJKER'
C
               CALL SUSPENSION_BIJKER(TAUP,HN,NPOIN,CHARR,QSC,ZREF,
     &                                ZERO,HMIN,CSTAEQ,XMVE)
C
          IF (DEBUG > 0) WRITE(LU,*) 'END SUSPENSION_BIJKER'
C
        ENDIF
C
C      DO J=NCOUCH_TASS,1,-1
C        DO I=1,NPOIN
C           CSTAEQ_COUCHE(I,J)=CSTAEQ%R(I)
C        ENDDO
C      ENDDO
C
      DO I=1,NPOIN
C
        DO J=1,NCOUCH_TASS
C
C-----------COMPUTES FLUER_SABLE_VASE AS A FUNCTION OF THE PROPORTION OF MUD
C
          IF(AVAIL(I,J,2).LE.0.3D0)THEN
C-------------PROPORTION OF MUD < 30%, FLUXES ARE SIMILAR TO THOSE FOR SAND ONLY
            IF(TAUP%R(I).GT.TOCE_MIXTE(I,J))THEN
                 FLUER_LOC(J)=CSTAEQ%R(I)*XWC
            ELSE
               FLUER_LOC(J)=0.D0
            ENDIF
C-------------PROPORTION OF MUD > 50%, FLUXES ARE SIMILAR TO THOSE FOR MUD ONLY
          ELSEIF(AVAIL(I,J,2).GE.0.5D0)THEN
            IF(TAUP%R(I).GT.TOCE_MIXTE(I,J))THEN
               FLUER_LOC(J)=PARTHENIADES*
     &              ((TAUP%R(I)/TOCE_MIXTE(I,J))-1.D0)
            ELSE
               FLUER_LOC(J)=0.D0
            ENDIF
C-------------PROPORTION OF MUD >30% AND <50%, INTERPOLATES THE FLUXES
C             AND CRITICAL SHEAR STRESS
          ELSE
            IF(TAUP%R(I).GT.TOCE_MIXTE(I,J))THEN
               FLUERSABLE=CSTAEQ%R(I)*XWC
               FLUERVASE=PARTHENIADES*
     &             ((TAUP%R(I)/TOCE_MIXTE(I,J))-1.D0)
            ELSE
               FLUERSABLE=0.D0
               FLUERVASE=0.D0
            ENDIF
               FLUER_LOC(J)=(AVAIL(I,J,2)-0.3D0)/
     &           (0.5D0-0.3D0)*(FLUERVASE-FLUERSABLE)+FLUERSABLE
          ENDIF
        ENDDO

C
C COMPUTES THE EROSION DEPTH ZER_MOY
C AND ERODED MASSES
          QER_VASE = 0.D0
          QER_SABLE = 0.D0
C
          TEMPS= DT
C
          DO J= 1, NCOUCH_TASS
           IF(ES(I,J).GE.1.D-6) THEN
C
C COMPUTES THE MASS POTENTIALLY ERODABLE IN LAYER J (KG/M2)
C
             QE_MOY= FLUER_LOC(J) *XMVS * TEMPS
C
             IF(QE_MOY.LT.(MS_SABLE(I,J)
     &            +MS_VASE(I,J))) THEN
C
                  QER_VASE = QER_VASE
     &                  + QE_MOY*MS_VASE(I,J)/
     &                      (MS_VASE(I,J)+MS_SABLE(I,J))
                  QER_SABLE = QER_SABLE
     &                    + QE_MOY*MS_SABLE(I,J)
     &                      /(MS_VASE(I,J)+MS_SABLE(I,J))
CV
                 GO TO 10
C
              ELSE
C
                  QER_VASE = QER_VASE + MS_VASE(I,J)
                  QER_SABLE = QER_SABLE + MS_SABLE(I,J)
                 TEMPS= TEMPS -
     &             (MS_SABLE(I,J)+MS_VASE(I,J))
     &                      /FLUER_LOC(J)/XMVS
              ENDIF
          ENDIF
C
         ENDDO
          WRITE(LU,*) 'ATTENTION TOUTES LES COUCHES SONT VIDES'
C          STOP

  10    CONTINUE
C
      ! ************************************************ !
      ! II-COMPUTES THE FLUX OF EROSION FOR SAND/MUD     !
      ! ************************************************ !
C
C Q_VASE REPRESENTS THE SURFACE MASS OF MUD TO BE ERODED TO REACH ZER_MOY
C Q_SABLE REPRESENTS THE SURFACE MASS OF SAND TO BE ERODED TO REACH ZER_MOY
C
        FLUER_VASE%R(I)  = QER_VASE /(DT*XMVS)
        FLUER_SABLE%R(I) = QER_SABLE/(DT*XMVS)
C
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C