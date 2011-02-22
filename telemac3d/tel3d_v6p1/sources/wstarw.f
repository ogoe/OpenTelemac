C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ENSURES THE TRANSFER OF THE AVERAGE PER LAYER
!>                FROM DZ/DZSTAR*WSTAR TO W
!>                BY SOLVING THE EQUATION:
!>  @code
!>                 DZ            DZ      DZ      DZ
!>           W =  ---- WSTAR + (--- + U --- + V ---)
!>               DZSTAR          DT      DX      DY
!>
!>
!>      INTEGRATED ON EACH LAYER!!!
!>
!>      BEWARE: THIS EQUATION IS WRITTEN IN THE TRANSFORMED MESH!
!>
!>      EQUATION INTEGRATED ON A LAYER, MULTIPLIED BY A 2D TEST
!>      FUNCTION, THEN INTEGRATED ON THE 2D OMEGA DOMAIN!
!>
!>      FOR EACH PLANE IP WITHIN [2,NPLAN-2], SOLVES :
!>
!>      M2D * ( W(IP+1) + W(IP) ) =  2 * M2D * DZW*(IP+1/2)
!>
!>            1
!>         + --- * M2D * (Z(N+1,IP+1)+Z(N+1,IP)-Z(N,IP+1)-Z(N,IP))
!>           DT
!>
!>                /   /Z*(IP+1)       1           DZ      DZ
!>         + 2 * /   /           ----------- ( U --- + V --- ) PSIH
!>              /OM /Z*(IP)      DZ*(IP+1/2)     DX      DY
!>
!>
!>              BEWARE: PSIH IS A 2D LINEAR BASE
!>
!>
!>
!> ALSO COMPUTES WUP STARTING FROM THE BOTTOM, AND WDOWN STARTING FROM
!> THE TOP, THEN COMPUTES THE AVERAGE OF BOTH
!>
!>
!> NOTE : ALTERNATE AND SIMPLER VERSION BY JMH :
!>
!> 1. INITIALISATION
!>
!>     CALL OS('X=Y     ',X=WW,Y=WSS)
!>
!> 2. DZ/DT TERM
!>
!>     CALL OS('X=X+CY  ',X=WW,Y=Z3   ,C= 1.D0/DT)
!>     CALL OS('X=X+CY  ',X=WW,Y=ZPROP,C=-1.D0/DT)
!>
!> 3. HORIZONTAL GRADIENTS FOR Z TERM
!>
!>     DO IPLAN=1,NPLAN
!>       CALL GRAD2D(T2_01,T2_02,ZPROP,IPLAN,SVIDE,
!>    *              UNSV2D,T2_03,IELM2H,MESH2D,MSK,MASKEL)
!>       DO I=1,NPOIN2
!>         IAD=I+(IPLAN-1)*NPOIN2
!>         WW%R(IAD)=WW%R(IAD)+U%R(IAD)*T2_01%R(I)+V%R(IAD)*T2_02%R(I)
!>       ENDDO
!>     ENDDO
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  ZPROP IS HERE USED LIKE ZN.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC, DECLARATIONS_TELEMAC3D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> SOMMEW, WDOWN, WSS, WUP, WW
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> DECLARATIONS_TELEMAC3D :<br>
!> @link DECLARATIONS_TELEMAC3D::DSSUDT DSSUDT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::DT DT@endlink, 
!> @link DECLARATIONS_TELEMAC3D::GRADZF GRADZF@endlink, 
!> @link DECLARATIONS_TELEMAC3D::GRADZS GRADZS@endlink, 
!> @link DECLARATIONS_TELEMAC3D::H H@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IELM3 IELM3@endlink, 
!> @link DECLARATIONS_TELEMAC3D::IELMH IELMH@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MASKEL MASKEL@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MAT2D MAT2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MESH2D MESH2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MESH3D MESH3D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::MSK MSK@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NELEM2 NELEM2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NELEM3 NELEM3@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPLAN NPLAN@endlink, 
!> @link DECLARATIONS_TELEMAC3D::NPOIN2 NPOIN2@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SEM2D SEM2D@endlink, 
!> @link DECLARATIONS_TELEMAC3D::SVIDE SVIDE@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T2_01 T2_01@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T2_02 T2_02@endlink, 
!> @link DECLARATIONS_TELEMAC3D::T3_01 T3_01@endlink, 
!> @link DECLARATIONS_TELEMAC3D::U U@endlink, 
!> @link DECLARATIONS_TELEMAC3D::UCONV UCONV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::V V@endlink, 
!> @link DECLARATIONS_TELEMAC3D::VCONV VCONV@endlink, 
!> @link DECLARATIONS_TELEMAC3D::Z3 Z3@endlink, 
!> @link DECLARATIONS_TELEMAC3D::ZPROP ZPROP@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I1, I2, I3, IAD, IADINF, IADSUP, IELEM2, IELEM3, IPLAN, IPLANINF, IPLANSUP, SURDT, SURNPLANMU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> CPSTVC(), OS(), OV(), PARCOM(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PRECON()

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
!>      <td><center> 5.5                                       </center>
!> </td><td> 10/12/04
!> </td><td> A. DECOENE (INRIA-LNHE)
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DT
!></td><td>--></td><td>PAS DE TEMPS
!>    </td></tr>
!>          <tr><td>IELM2
!></td><td>--></td><td>TYPE DE DISCRETISATION 2DH
!>    </td></tr>
!>          <tr><td>IELM3
!></td><td>--></td><td>TYPE DE DISCRETISATION 3D
!>    </td></tr>
!>          <tr><td>INFO
!></td><td>--></td><td>INFORMATIONS SUR LES SOLVEURS
!>    </td></tr>
!>          <tr><td>LIWBOF,L,S
!></td><td>--></td><td>TYPE DE CONDITIONS LIMITES POUR WS
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>MASQUAGE DES ELEMENTS
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>ADRESSES GLOBALES DES POINTS FRONTIERES.
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE D'ELEMENTS 2D
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS DU MAILLAGE 3D
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE DU MAILLAGE 2D
!>    </td></tr>
!>          <tr><td>SM
!></td><td><-></td><td>SECOND MEMBRE POUR LA VITESSE VERTICALE
!>    </td></tr>
!>          <tr><td>SOMMEW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SVIDE
!></td><td>--></td><td>STRUCTURE VIDE
!>    </td></tr>
!>          <tr><td>TBB
!></td><td>--></td><td>BLOC DE BLOCS DE TRAVAIL
!>    </td></tr>
!>          <tr><td>TRAV2
!></td><td><-></td><td>STRUCTURE DE TABLEAUX DE TRAVAIL 2D
!>    </td></tr>
!>          <tr><td>W1
!></td><td><-></td><td>TABLEAU DE TRAVAIL (CALCUL DES MATRICES...)
!>    </td></tr>
!>          <tr><td>WDOWN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>WSBORF,L,S
!></td><td>--></td><td>CONDITIONS AUX LIMITES DIRICHLET POUR WS
!>    </td></tr>
!>          <tr><td>WSS
!></td><td><--</td><td>MOYENNE DE DZ*WSTAR PAR COUCHE A N+1
!>    </td></tr>
!>          <tr><td>WUP
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>WW
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE WSTARW
     & (WW,WSS,WUP,WDOWN,SOMMEW)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DT             |-->| PAS DE TEMPS
C| IELM2          |-->| TYPE DE DISCRETISATION 2DH
C| IELM3          |-->| TYPE DE DISCRETISATION 3D
C| INFO           |-->| INFORMATIONS SUR LES SOLVEURS
C| LIWBOF,L,S     |-->| TYPE DE CONDITIONS LIMITES POUR WS
C| MASKEL         |-->| MASQUAGE DES ELEMENTS
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES
C| NBOR           |-->| ADRESSES GLOBALES DES POINTS FRONTIERES.
C| NELEM2         |-->| NOMBRE D'ELEMENTS 2D
C| NPLAN          |-->| NOMBRE DE PLANS DU MAILLAGE 3D
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE DU MAILLAGE 2D
C| SM             |<->| SECOND MEMBRE POUR LA VITESSE VERTICALE
C| SOMMEW         |---| 
C| SVIDE          |-->| STRUCTURE VIDE
C| TBB            |-->| BLOC DE BLOCS DE TRAVAIL
C| TRAV2          |<->| STRUCTURE DE TABLEAUX DE TRAVAIL 2D
C| W1             |<->| TABLEAU DE TRAVAIL (CALCUL DES MATRICES...)
C| WDOWN          |---| 
C| WSBORF,L,S     |-->| CONDITIONS AUX LIMITES DIRICHLET POUR WS
C| WSS            |<--| MOYENNE DE DZ*WSTAR PAR COUCHE A N+1
C| WUP            |---| 
C| WW             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ), INTENT(IN) :: WSS
      TYPE(BIEF_OBJ), INTENT(INOUT) :: WW
      DOUBLE PRECISION,INTENT(INOUT) :: WUP(*),WDOWN(*),SOMMEW(*)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IPLAN, IPLANINF, IPLANSUP
      INTEGER IAD,  IADINF, IADSUP
      INTEGER IELEM2, IELEM3, I1, I2, I3
!
      DOUBLE PRECISION :: SURDT,SURNPLANMU
!
!=======================================================================
!
C     SOLVES THE LINEAR SYSTEM
!
!=======================================================================
!
      SURDT=1.D0/DT
      SURNPLANMU=1.D0/(NPLAN-1)
!
C 1.BUILDS THE LUMPED 2D MASS MATRIX
!
      MAT2D%ADR(1)%P%TYPDIA='Q'
      MAT2D%ADR(1)%P%TYPEXT='0'
!
      CALL VECTOR
     & (MAT2D%ADR(1)%P%D, '=', 'MASBAS          ',IELMH,1.D0,
     &  SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,MESH2D,MSK,MASKEL)
!
      CALL OS('X=Y     ',X=T2_01,Y=MAT2D%ADR(1)%P%D)
!
      IF(NCSIZE.GT.1) CALL PARCOM(T2_01,2,MESH2D)
      IF(MSK) THEN
        CALL OS('X=1/Y   ',X=T2_01,Y=T2_01,
     &                IOPT=2,INFINI=0.D0,ZERO=1.D-6)
      ELSE
        CALL OS('X=1/Y   ',X=T2_01,Y=T2_01)
      ENDIF
!
!-----------------------------------------------------------------------
!
C 2. COMPUTES W AT THE BOTTOM: IMPOSES THE BOUNDARY CONDITION AT THE BOTTOM:
!
C                           DZF            DZF
C       W(PLAN1) = U(PLAN1) --- + V(PLAN1) ---
C                           DX             DY
!
C     DOES NOT CONSIDER DIRICHLET ON W FOR THE TIME BEING
!
      CALL OV ('X=YZ    ',WW%R,GRADZF%ADR(1)%P%R,U%R,0.D0,NPOIN2)
      CALL OV ('X=X+YZ  ',WW%R,GRADZF%ADR(2)%P%R,V%R,0.D0,NPOIN2)
      CALL OV ('X=Y     ',WUP,WW%R,V%R,0.D0,NPOIN2)
!
!-----------------------------------------------------------------------
!
C 3. COMPUTES W AT THE FREE SURFACE: IMPOSES THE KINEMATIC CONDITION:
!
C                  DZS            DZS            DZS
C       W(NPLAN) = --- + U(NPLAN) --- + V(NPLAN) ---
C                  DT             DX             DY
!
      IAD = NPLAN*NPOIN2  ! LAST NODE FROM NPLAN
!
      CALL OV ('X=YZ    ', WW%R(IAD-NPOIN2+1:IAD), GRADZS%ADR(1)%P%R,
     &         U%R(IAD-NPOIN2+1:IAD), 0.D0, NPOIN2)
      CALL OV ('X=X+YZ  ', WW%R(IAD-NPOIN2+1:IAD), GRADZS%ADR(2)%P%R,
     &         V%R(IAD-NPOIN2+1:IAD), 0.D0, NPOIN2)
      CALL OV ('X=X+Y   ', WW%R(IAD-NPOIN2+1:IAD), DSSUDT%R,
     &         T2_02%R, 0.D0, NPOIN2)
      CALL OV ('X=Y     ',WDOWN(IAD-NPOIN2+1:IAD),
     &                    WW%R(IAD-NPOIN2+1:IAD),V%R,0.D0,NPOIN2)
!
!=======================================================================
!
C 4. BUILDS THE NON-ASSEMBLED VECTOR PSIH * UCONV GRAD(Z)
!
      CALL VECTOR(T3_01, '=', 'VGRADF2         ',IELM3,1.D0,ZPROP,
     &            SVIDE,SVIDE,UCONV,VCONV,SVIDE,MESH3D,MSK,MASKEL)
!
C         THE NON-ASSEMBLED VECTOR IS IN MESH3D%W
!
C 5. ASSEMBLES THE VECTOR PSIH * U GRAD(Z)
C         ON EACH LAYER [IP,IP+1] IN T3_01
!
      DO IPLAN=1,NPLAN-1
        IAD = IPLAN*NPOIN2  ! LAST NODE FROM IPLAN
        CALL OV( 'X=C     ',T3_01%R(IAD-NPOIN2+1:IAD),SVIDE%R,SVIDE%R,
     &          0.D0,NPOIN2)
        DO IELEM2 = 1, NELEM2
           IELEM3 = (IPLAN-1) * NELEM2 + IELEM2
           I1=MESH3D%IKLE%I(IELEM3)
           I2=MESH3D%IKLE%I(IELEM3+NELEM3)
           I3=MESH3D%IKLE%I(IELEM3+2*NELEM3)
           T3_01%R(I1)=T3_01%R(I1)+MESH3D%W%R(IELEM3)
           T3_01%R(I2)=T3_01%R(I2)+MESH3D%W%R(IELEM3+NELEM3)
           T3_01%R(I3)=T3_01%R(I3)+MESH3D%W%R(IELEM3+2*NELEM3)
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------
!
C 6. COMPUTES [(W)PLAN_INF + (W)PLAN_SUP] ON THE INTERMEDIATE PLANES
!
C                FOR PLAN_INF = 1 TO NPLAN-1
!
C        => USES T2_02 AND SEM2D%ADR(1) FOR THE SECOND MEMBER
!
      DO IPLAN = 1,NPLAN-1
!
           IPLANINF = IPLAN
           IPLANSUP = IPLAN+1
           IADINF = IPLANINF * NPOIN2
           IADSUP = IPLANSUP * NPOIN2
!
C          1. 2*AVERAGE [DZ W*] FOR LAYER [IPLAN_INF,IPLAN_SUP]
C             T2_02 SHOULD BE HERE OF TYPE 11 (LINEAR TRIANGLE)
           CALL CPSTVC(H,T2_02)
           CALL OV('X=CY    ',T2_02%R,WSS%R(IADINF-NPOIN2+1:IADINF),
     &              WSS%R(IADINF-NPOIN2+1:IADINF),2.D0, NPOIN2)
!
C          2. (1/DT) * [Z(N+1)-Z(N)]PLAN_SUP + [Z(N+1)-Z(N)]PLAN_INF
!
C          2.1. PLAN_SUP
!
           CALL OV('X=X+CY  ', T2_02%R,
     &              Z3%R(IADSUP-NPOIN2+1:IADSUP),
     &              Z3%R(IADSUP-NPOIN2+1:IADSUP),SURDT,NPOIN2)
!
           CALL OV('X=X+CY  ', T2_02%R,
     &              ZPROP%R(IADSUP-NPOIN2+1:IADSUP),
     &              ZPROP%R(IADSUP-NPOIN2+1:IADSUP),-SURDT,NPOIN2)
!
C          2.2. PLAN_INF
!
           CALL OV('X=X+CY  ', T2_02%R,
     &              Z3%R(IADINF-NPOIN2+1:IADINF),
     &              Z3%R(IADINF-NPOIN2+1:IADINF),SURDT,NPOIN2)
!
           CALL OV('X=X+CY  ', T2_02%R,
     &              ZPROP%R(IADINF-NPOIN2+1:IADINF),
     &              ZPROP%R(IADINF-NPOIN2+1:IADINF),-SURDT,NPOIN2)
!
C          3. MULTIPLIES BY THE MASS
!
           CALL OS('X=YZ    ',X=SEM2D%ADR(1)%P,Y=MAT2D%ADR(1)%P%D,
     &                        Z=T2_02)
!
C          4. ADDS PSIH * UCONV GRAD(Z)
!
           CALL OV( 'X=X+CY  ', SEM2D%ADR(1)%P%R,
     &              T3_01%R(IADINF-NPOIN2+1:IADINF),
     &              SVIDE%R , 2.D0, NPOIN2 )
!
C          5. SOLVES THE SYSTEM
!
           IF(NCSIZE.GT.1) CALL PARCOM(SEM2D%ADR(1)%P,2,MESH2D)
!
C          SOLUTION IN (SOMMEW)PLAN_INF
!
           CALL OV('X=YZ    ',SOMMEW(IADINF-NPOIN2+1:IADINF),
     &             SEM2D%ADR(1)%P%R,T2_01%R,0.D0,NPOIN2)
!
      ENDDO
!
C    WUP COMPUTED FROM THE BOUNDARY CONDITION (BOTTOM, UP THE PLANES)
!
C     LOOP ON THE PLANES
C     NOTE FROM ASTRID: 2 TO NPLAN-1 WOULD BE ENOUGH
      DO IPLAN = 2,NPLAN
!
           IADSUP = IPLAN * NPOIN2         ! LAST NODE FROM IPLAN
           IADINF = (IPLAN-1) * NPOIN2     ! LAST NODE FROM (IPLAN-1)
!
C          [ WUP ] IPLAN = - [ WUP ] IPLAN-1 + [(W) IPLAN + (W) IPLAN-1]
C                        = - [ WUP ] IPLAN-1 + [ SOMMEW ] IPLAN-1

           CALL OV('X=Y+CZ  ', WUP(IADSUP-NPOIN2+1:IADSUP),
     &            SOMMEW(IADINF-NPOIN2+1:IADINF),
     &            WUP(IADINF-NPOIN2+1:IADINF),-1.D0, NPOIN2)
!
      ENDDO
!
C    WDOWN COMPUTED FROM THE BOUNDARY CONDITION (FREE SURFACE,
C                                                    DOWN THE PLANES)
!
C     LOOP ON THE PLANES
C     NOTE FROM ASTRID : 1 TO NPLAN - 2 WOULD BE ENOUGH
      DO IPLAN = 1,NPLAN-1
!
C          PLAN_SUP = (NPLAN-IPLAN+1)
C          PLAN_INF = (NPLAN-IPLAN)
!
           IADSUP = (NPLAN-IPLAN+1) * NPOIN2   ! LAST NODE FROM PLAN_SUP
           IADINF = (NPLAN-IPLAN) * NPOIN2     ! LAST NODE FROM PLAN_INF
!
C          [ WDOWN ] PLAN_INF  = - [ WDOWN ] PLAN_SUP + [(W) PLAN_SUP + (W) PLAN_INF]
C                        = - [ WDOWN ] PLAN_SUP  + [ SOMMEW ] PLAN_INF

           CALL OV('X=Y+CZ  ', WDOWN(IADINF-NPOIN2+1:IADINF),
     &            SOMMEW(IADINF-NPOIN2+1:IADINF),
     &             WDOWN(IADSUP-NPOIN2+1:IADSUP),-1.D0, NPOIN2)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
C     FINAL W = AVERAGE OF WUP AND WDOWN
!
      DO IPLAN=2,NPLAN-1
        IAD = IPLAN * NPOIN2  ! LAST NODE FROM IPLAN
        CALL OV('X=CY    ',WW%R(IAD-NPOIN2+1:IAD),
     &          WDOWN(IAD-NPOIN2+1:IAD),
     &          WDOWN(IAD-NPOIN2+1:IAD),(IPLAN-1.D0)*SURNPLANMU,NPOIN2)
        CALL OV('X=X+CY  ',WW%R(IAD-NPOIN2+1:IAD),
     &          WUP(IAD-NPOIN2+1:IAD),
     &          WUP(IAD-NPOIN2+1:IAD),SURNPLANMU*(NPLAN-IPLAN),NPOIN2)
      ENDDO
!
!=======================================================================
!
      RETURN
      END
C
C#######################################################################
C