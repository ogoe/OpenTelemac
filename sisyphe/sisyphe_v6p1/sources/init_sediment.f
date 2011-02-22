C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief  

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AC, ACLADM, AVA0, AVAIL, CALAC, CALWC, CONC_VASE, ELAY, ES, FDM, FRACSED_GF, GRAV, LGRAFED, MS_SABLE, MS_VASE, NCOUCH_TASS, NPOIN, NSICLA, SEDCO, UNLADM, VCE, XMVE, XMVS, XWC, ZF, ZR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DENS, DSTAR, I, J
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_INIT_SEDIMENT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> INIT_AVAI(), INIT_MIXTE(), OS(), VITCHU_SISYPHE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>SISYPHE()

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
!> </td><td> 16/09/2009
!> </td><td> JMH
!> </td><td> AVAIL(NPOIN,10,NSICLA)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 30/12/2008
!> </td><td> C. VILLARET (LNHE) 01 30 87 83 28
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
!>          <tr><td>ACLADM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AVA0
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>AVAI
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>AVAIL
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>CALAC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>CALWC
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>CHOIX
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>CONC_VASE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ELAY
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>ES
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FDM
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>FRACSED_GF
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>GRAV
!></td><td>--></td><td>GRAVITY ACCELERATION
!>    </td></tr>
!>          <tr><td>HN
!></td><td>--></td><td>WATER DEPTH
!>    </td></tr>
!>          <tr><td>LGRAFED
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>
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
!></td><td>--></td><td>NUMBER OF POINTS
!>    </td></tr>
!>          <tr><td>NSICLA
!></td><td>--></td><td>NUMBER OF SEDIMENT CLASSES
!>    </td></tr>
!>          <tr><td>SEDCO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UNLADM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VCE
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>XMVE
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>XMVS
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>XWC
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>Z
!></td><td>--></td><td>
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>BOTTOM
!>    </td></tr>
!>          <tr><td>ZR
!></td><td>--></td><td>NON ERODABLE BED
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE INIT_SEDIMENT
     &(NSICLA,ELAY,ZF,ZR,NPOIN,AVAIL,FRACSED_GF,AVA0,
     & LGRAFED,CALWC,XMVS,XMVE,GRAV,VCE,XWC,FDM,
     & CALAC,AC,SEDCO,ES,NCOUCH_TASS,CONC_VASE,
     & MS_SABLE,MS_VASE,ACLADM,UNLADM)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AC             |---| 
C| ACLADM         |---| 
C| AVA0           |-->| 
C| AVAI           |-->| 
C| AVAIL          |-->| 
C| CALAC          |---| 
C| CALWC          |-->| 
C| CHOIX          |-->| 
C| CONC_VASE      |---| 
C| ELAY           |-->| 
C| ES             |---| 
C| FDM            |-->| 
C| FRACSED_GF     |-->| 
C| GRAV           |-->| GRAVITY ACCELERATION
C| HN             |-->| WATER DEPTH
C| LGRAFED        |-->| 
C| MESH           |-->| 
C| MS_SABLE       |---| 
C| MS_VASE        |---| 
C| NCOUCH_TASS    |---| 
C| NPOIN          |-->| NUMBER OF POINTS
C| NSICLA         |-->| NUMBER OF SEDIMENT CLASSES
C| SEDCO          |---| 
C| UNLADM         |---| 
C| VCE            |-->| 
C| XMVE           |-->| 
C| XMVS           |-->| 
C| XWC            |-->| 
C| Z             |-->| 
C| ZF             |-->| BOTTOM
C| ZR             |-->| NON ERODABLE BED
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_SISYPHE, EX_INIT_SEDIMENT => INIT_SEDIMENT
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER,           INTENT(IN)     :: NSICLA,NPOIN,NCOUCH_TASS
      TYPE(BIEF_OBJ),    INTENT(INOUT)  :: ELAY,ZF,ZR
      TYPE(BIEF_OBJ), INTENT(INOUT)     :: MS_SABLE, MS_VASE
      TYPE(BIEF_OBJ),    INTENT(INOUT)  :: ACLADM, UNLADM
      LOGICAL,           INTENT(IN)     :: LGRAFED,CALWC
      LOGICAL,           INTENT(IN)     :: CALAC
      DOUBLE PRECISION,  INTENT(IN)     :: XMVS,XMVE,GRAV,VCE
      DOUBLE PRECISION,  INTENT(INOUT)  :: AVA0(NSICLA)
      DOUBLE PRECISION,  INTENT(INOUT)  :: AVAIL(NPOIN,10,NSICLA)
      DOUBLE PRECISION,  INTENT(INOUT)  :: FRACSED_GF(NSICLA)
      DOUBLE PRECISION,  INTENT(INOUT)  :: FDM(NSICLA),XWC(NSICLA)
      DOUBLE PRECISION,  INTENT(INOUT)  :: AC(NSICLA)
C
      LOGICAL,           INTENT(IN)     :: SEDCO(NSICLA)
C
C IF SEDCO(1) OR SEDCO(2) = YES --> CONSOLIDATION MODEL
C
C
      DOUBLE PRECISION, INTENT(IN)    :: CONC_VASE(10)
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,10)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER            :: I,J
      DOUBLE PRECISION   :: DENS,DSTAR
!
!======================================================================!
!======================================================================!
C                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
C  ------ BED COMPOSITION
!
        CALL OS('X=Y-Z   ',X=ELAY,Y=ZF,Z=ZR)
!
C     ONLY ONE CLASS
C
      IF(NSICLA.EQ.1) THEN
         DO I=1,NPOIN
          AVAIL(I,1,1) = 1.D0
          ACLADM%R(I) = FDM(1)
        ENDDO
C     PURE MUD ONLY
        IF(SEDCO(1)) CALL INIT_MIXTE(XMVS,NPOIN,AVAIL,NSICLA,ES,
     &                               ELAY%R,NCOUCH_TASS,CONC_VASE,
     &                                  MS_SABLE%R,MS_VASE%R,ZF%R,
     &                                               ZR%R,AVA0)
C
      ELSE
C
C     NON-COHESIVE, MULTI-CLASSES
C
        IF(.NOT.SEDCO(2)) THEN
          CALL INIT_AVAI
C         CALL MEAN_GRAIN_SIZE
C THIS PART CAN BE INTEGRATED INTO INIT_AVAI
          DO J=1,NPOIN
            ACLADM%R(J) = 0.D0
            UNLADM%R(J) = 0.D0
            DO I=1,NSICLA
              IF(AVAIL(J,1,I).GT.0.D0) THEN
                ACLADM%R(J) = ACLADM%R(J) + FDM(I)*AVAIL(J,1,I)
                UNLADM%R(J) = UNLADM%R(J) + FDM(I)*AVAIL(J,2,I)
              ENDIF
            ENDDO
            ACLADM%R(J)=MAX(ACLADM%R(J),0.D0)
            UNLADM%R(J)=MAX(UNLADM%R(J),0.D0)
          ENDDO
        ELSE
C        MIXED (NON COHESIVE /COHESIVE)
          CALL INIT_MIXTE(XMVS,NPOIN,AVAIL,NSICLA,ES,ELAY%R,
     &                     NCOUCH_TASS,CONC_VASE,MS_SABLE%R,
     &                     MS_VASE%R,ZF%R,ZR%R,AVA0)
          DO I=1,NPOIN
            ACLADM%R(I) = FDM(1)
          ENDDO
        ENDIF
C
      ENDIF
C
      IF(LGRAFED) THEN
        DO I=1, NSICLA
          FRACSED_GF(I)=AVA0(I)
        ENDDO
      ENDIF
C
C
C ------ SETTLING VELOCITY
C
      IF(.NOT.CALWC) THEN
        DENS = (XMVS - XMVE) / XMVE
        DO I = 1, NSICLA
          CALL VITCHU_SISYPHE(XWC(I),DENS,FDM(I),GRAV,VCE)
        ENDDO
      ENDIF
C
C------ SHIELDS PARAMETER
C
      IF(.NOT.CALAC) THEN
        DENS  = (XMVS - XMVE )/ XMVE
        DO I = 1, NSICLA
          DSTAR = FDM(I)*(GRAV*DENS/VCE**2)**(1.D0/3.D0)
          IF (DSTAR <= 4.D0) THEN
            AC(I) = 0.24*DSTAR**(-1.0D0)
          ELSEIF (DSTAR <= 10.D0) THEN
            AC(I) = 0.14D0*DSTAR**(-0.64D0)
          ELSEIF (DSTAR <= 20.D0) THEN
            AC(I) = 0.04D0*DSTAR**(-0.1D0)
          ELSEIF (DSTAR <= 150.D0) THEN
            AC(I) = 0.013D0*DSTAR**(0.29D0)
          ELSE
            AC(I) = 0.055D0
          ENDIF
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C