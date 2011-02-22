
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE RELATIVE BALANCE OF THE MASSES OF
!>                WATER AND TRACERS DURING A TIMESTEP, AS WELL AS
!>                THE ABSOLUTE CUMULATIVE BALANCE.

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 17/06/2008
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/03/1999
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>  </table>

C
C#######################################################################
C
                        SUBROUTINE BIL3D
     *(LT,IKLBORL,IKLE2L,NPTFR,NETAG,NELEM)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| VALEUR DU TEMPS EN COURS
C| ATABO,BTABO    |<--| LOI LOG SUR TRACEURS : ATABO*TA + BTABO
C| DT             |-->| PAS DE TEMPS
C| F, L, S        |---| F : FOND   L : COTES LATERAUX  S : SURFACE
C| FLUCUM         |<->| FLUX ENTREE ET SORTIE DEPUIS LE DEBUT DU CALC.
C| FLUEXT         |<--| FLUX AVEC L'EXTERIEUR
C| FLUX           |<->| FLUX ENTRE LES 2 PAS DE TEMPS
C| IKLBOR         |-->| TABLE DE CONNECTIVITE POUR LES POINTS DE BORD
C| IKLBORL        |---| 
C| IKLE2          |-->| TABLE DE CONNECTIVITE POUR LES POINTS DU FOND
C| IKLE2L         |---| 
C| INFOGR         |-->| LOGIQUE INDIQUANT SI ON FAIT LES IMPRESSIONS
C| LT             |-->| NUMERO DU PAS DE TEMPS EN COURS
C| MASSE          |-->| MASSE AU PAS EN COURS
C| MASSEN         |-->| MASSE AU PAS PRECEDENT
C| NELEM          |---| 
C| NELEM2         |-->| NOMBRE D'ELEMENTS 2D
C| NELEM3         |-->| NOMBRE D'ELEMENTS 3D
C| NETAG          |---| 
C| NETAGE         |-->| NOMBRE D'ETAGES
C| NIT            |-->| NOMBRE TOTAL DE PAS DE TEMPS
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| NPOIN3         |-->| NOMBRE DE POINTS 3D
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE 2D
C| NPTFR3         |-->| NOMBRE DE POINTS SUR LES COTES 3D
C| NTRAC          |-->| NOMBRE DE TRACEURS ACTIFS
C| NVBIL          |-->| NOMBRE DE VARIABLES TRAITEES DANS LE BILAN
C| SEDI           |-->| INDIQUE SI IL Y A UN SEDIMENT
C| SURFAC         |-->| SURFACES DES ELEMENTS.
C| TA             |-->| TRACEURS                   AU PAS EN COURS
C| TAN3           |-->| TRACEURS                   AU PAS PRECEDENT
C| WC             |-->| VITESSE DE CHUTE DU SEDIMENT
C| X,Y,ZPROP      |-->| COORDONNEES DU MAILLAGE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_BIL3D => BIL3D
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: LT,NPTFR,NETAG,NELEM
      INTEGER, INTENT(IN) :: IKLBORL(NPTFR,NETAG,4),IKLE2L(NELEM,3)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION FLUTOT,SUR12,A1,A2,A3,A4,A5,FLURAIN
      DOUBLE PRECISION D, X_2,X_3, Y_2,Y_3, Z_1,Z_2,Z_3,Z_5,Z_6
!
      INTEGER I,L1,L2,L3,L4,N1,N2,N3,N4,IVBIL,ILIQ
      INTEGER IELEM2,IPTFR,IETAGE,ITRAC
C                            25=MAXTRA+5
      DOUBLE PRECISION FLUDI(25),FLUS1(25),FLUXTOTAL
      INTRINSIC SQRT
!
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
C
C=======================================================================
C
C     COMPUTES THE FLUXES AT THE LIQUID BOUNDARIES
C
      IF(NFRLIQ.GT.0) THEN
        DO I=1,NFRLIQ
          FLUX_BOUNDARIES(I)=0.D0
        ENDDO
        IF(NPTFR.GT.0) THEN
          DO I=1,NPTFR
            ILIQ=NUMLIQ%I(I)
            IF(ILIQ.GT.0) THEN
              FLUX_BOUNDARIES(ILIQ)=FLUX_BOUNDARIES(ILIQ)+FLBOR%R(I)
            ENDIF
          ENDDO
        ENDIF
        IF(NCSIZE.GT.1) THEN
          DO I=1,NFRLIQ
            FLUX_BOUNDARIES(I)=P_DSUM(FLUX_BOUNDARIES(I))
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
!
C   COMPUTES THE ADVECTION FLUXES ON THE VARIOUS OPEN BOUNDARIES
!
!=======================================================================
!
C     TOTAL FLUX, STARTING WITH THE LIQUID BOUNDARIES
!
      FLUXTOTAL=0.D0
      IF(NFRLIQ.GT.0) THEN
        DO I=1,NFRLIQ
          FLUXTOTAL=FLUXTOTAL+FLUX_BOUNDARIES(I)
        ENDDO
      ENDIF
!
C     RAIN AND EVAPORATION
!
      IF(RAIN) THEN
        FLURAIN=0.D0
        DO I=1,NPOIN2
          FLURAIN=FLURAIN+PLUIE%R(I)
        ENDDO
        IF(NCSIZE.GT.1) FLURAIN = P_DSUM(FLURAIN)
        FLUXTOTAL=FLUXTOTAL-FLURAIN
      ENDIF
!
      IF(NSCE.GE.1) THEN
        DO I=1,NSCE
          FLUXTOTAL = FLUXTOTAL - QSCE2(I)
        ENDDO
C       IF(NTRAC.GT.0) THEN
C         DO I=1,NSCE
C         DO IVBIL=2,1+NTRAC
C         NOW DONE IN CVDF3D AND MURD3D
C                                                      TASCE(MAXSCE,NTRAC)
C             FLUX%R(IVBIL) = FLUX%R(IVBIL) - QSCE2(I)*TASCE2(I,IVBIL-1)
C         ENDDO
C         ENDDO
C       ENDIF
      ENDIF
!
!=======================================================================
!
C  COMPUTES THE FLUXES BY DIFFUSION AND SOURCES OF TRACERS
!
!=======================================================================
!
      SUR12 = 1.D0/12.D0
!
!   ====================================
!   FLUX BY SOURCES OF TRACERS
!   ====================================
!
      IF(NTRAC.NE.0) THEN
        DO ITRAC=1,NTRAC
          FLUS1(5+ITRAC) = 0.D0
          IF(S1TA%ADR(ITRAC)%P%TYPR.NE.'0') THEN
            DO I=1,NPOIN3
!             JMH NOTE : IS NOT VOLU WITH SUPG
!                        BUT A VALUE WEIGHTED WITH VOLUN
!                        INVESTIGATE WHEN THE CASE ARISES...
              FLUS1(5+ITRAC)=FLUS1(5+ITRAC)
     *                        +S1TA%ADR(ITRAC)%P%R(I)*VOLU%R(I)*
     *                           TA%ADR(ITRAC)%P%R(I)
            ENDDO
            IF(NCSIZE.GT.1) THEN
              FLUS1(5+ITRAC) = P_DSUM(FLUS1(5+ITRAC))
            ENDIF
          ENDIF
        ENDDO
      ENDIF
!
!   ===============================
C   FLUX BY DIFFUSION OF TRACERS
!   ===============================
!
!   IN THE CASE OF A SEDIMENT, THIS FLUX INCLUDES THE SETTLING VELOCITY
!   SEE HOW ATABOF AND BTABOF ARE DONE IN FLUSED
!
      IF(NTRAC.NE.0) THEN
!
         DO ITRAC=1,NTRAC
!
            FLUDI(5+ITRAC) = 0.D0
!
!           BOTTOM AND FREE SURFACE
!
            IF(ATABOF%ADR(ITRAC)%P%TYPR.NE.'0') THEN
            DO IELEM2=1,NELEM

               L1 = IKLE2L(IELEM2,1)
               L2 = IKLE2L(IELEM2,2)
               L3 = IKLE2L(IELEM2,3)
               L4 = NETAG*NPOIN2
!
!              COMPUTES (AREA OF THE TRIANGLE)
!
               X_2 = X(L2) - X(L1)
               X_3 = X(L3) - X(L1)
               Y_2 = Y(L2) - Y(L1)
               Y_3 = Y(L3) - Y(L1)
               Z_2 = ZPROP%R(L2) - ZPROP%R(L1)
               Z_3 = ZPROP%R(L3) - ZPROP%R(L1)
               Z_5 = ZPROP%R(L2+L4) - ZPROP%R(L1+L4)
               Z_6 = ZPROP%R(L3+L4) - ZPROP%R(L1+L4)
               A1 = Y_2*Z_3 - Z_2*Y_3
               A2 = Z_2*X_3 - X_2*Z_3
               A3 = X_2*Y_3 - Y_2*X_3
!
                    FLUDI(5+ITRAC) = FLUDI(5+ITRAC)
     *                        + SQRT(A1*A1+A2*A2+A3*A3)/6.D0 *
     *            ( ATABOF%ADR(ITRAC)%P%R(L1)*TA%ADR(ITRAC)%P%R(L1)
     *             +ATABOF%ADR(ITRAC)%P%R(L2)*TA%ADR(ITRAC)%P%R(L2)
     *             +ATABOF%ADR(ITRAC)%P%R(L3)*TA%ADR(ITRAC)%P%R(L3) )
!
            ENDDO
            ENDIF
!
            IF(ATABOS%ADR(ITRAC)%P%TYPR.NE.'0') THEN
            DO IELEM2=1,NELEM

               L1 = IKLE2L(IELEM2,1)
               L2 = IKLE2L(IELEM2,2)
               L3 = IKLE2L(IELEM2,3)
               L4 = NETAG*NPOIN2
!
!         COMPUTES (AREA OF THE TRIANGLE)
!
               X_2 = X(L2) - X(L1)
               X_3 = X(L3) - X(L1)
               Y_2 = Y(L2) - Y(L1)
               Y_3 = Y(L3) - Y(L1)
               Z_2 = ZPROP%R(L2) - ZPROP%R(L1)
               Z_3 = ZPROP%R(L3) - ZPROP%R(L1)
               Z_5 = ZPROP%R(L2+L4) - ZPROP%R(L1+L4)
               Z_6 = ZPROP%R(L3+L4) - ZPROP%R(L1+L4)
               A3 = X_2*Y_3 - Y_2*X_3
               A4 = Y_2*Z_6 - Z_5*Y_3
               A5 = Z_5*X_3 - X_2*Z_6
!
                    FLUDI(5+ITRAC) = FLUDI(5+ITRAC)
     *                        + SQRT(A3*A3+A4*A4+A5*A5)/6.D0 *
     *            ( ATABOS%ADR(ITRAC)%P%R(L1)*TA%ADR(ITRAC)%P%R(L1+L4)
     *             +ATABOS%ADR(ITRAC)%P%R(L2)*TA%ADR(ITRAC)%P%R(L2+L4)
     *             +ATABOS%ADR(ITRAC)%P%R(L3)*TA%ADR(ITRAC)%P%R(L3+L4) )
!
            ENDDO
            ENDIF
!
            IF(BTABOF%ADR(ITRAC)%P%TYPR.NE.'0') THEN
            DO IELEM2=1,NELEM

               L1 = IKLE2L(IELEM2,1)
               L2 = IKLE2L(IELEM2,2)
               L3 = IKLE2L(IELEM2,3)
               L4 = NETAG*NPOIN2
!
!         COMPUTES (AREA OF THE TRIANGLE)
!
               X_2 = X(L2) - X(L1)
               X_3 = X(L3) - X(L1)
               Y_2 = Y(L2) - Y(L1)
               Y_3 = Y(L3) - Y(L1)
               Z_2 = ZPROP%R(L2) - ZPROP%R(L1)
               Z_3 = ZPROP%R(L3) - ZPROP%R(L1)
               Z_5 = ZPROP%R(L2+L4) - ZPROP%R(L1+L4)
               Z_6 = ZPROP%R(L3+L4) - ZPROP%R(L1+L4)
               A1 = Y_2*Z_3 - Z_2*Y_3
               A2 = Z_2*X_3 - X_2*Z_3
               A3 = X_2*Y_3 - Y_2*X_3
!
                    FLUDI(5+ITRAC) = FLUDI(5+ITRAC)
     *                        + SQRT(A1**2+A2**2+A3**2)/6.D0 *
     *            ( BTABOF%ADR(ITRAC)%P%R(L1)
     *             +BTABOF%ADR(ITRAC)%P%R(L2)
     *             +BTABOF%ADR(ITRAC)%P%R(L3) )
!
            ENDDO
            ENDIF
!
            IF(BTABOS%ADR(ITRAC)%P%TYPR.NE.'0') THEN
            DO IELEM2=1,NELEM

               L1 = IKLE2L(IELEM2,1)
               L2 = IKLE2L(IELEM2,2)
               L3 = IKLE2L(IELEM2,3)
               L4 = NETAG*NPOIN2
!
C         COMPUTES (AREA OF THE TRIANGLE)
!
               X_2 = X(L2) - X(L1)
               X_3 = X(L3) - X(L1)
               Y_2 = Y(L2) - Y(L1)
               Y_3 = Y(L3) - Y(L1)
               Z_2 = ZPROP%R(L2) - ZPROP%R(L1)
               Z_3 = ZPROP%R(L3) - ZPROP%R(L1)
               Z_5 = ZPROP%R(L2+L4) - ZPROP%R(L1+L4)
               Z_6 = ZPROP%R(L3+L4) - ZPROP%R(L1+L4)
               A3 = X_2*Y_3 - Y_2*X_3
               A4 = Y_2*Z_6 - Z_5*Y_3
               A5 = Z_5*X_3 - X_2*Z_6
!
                    FLUDI(5+ITRAC) = FLUDI(5+ITRAC)
     *                        + SQRT(A3*A3+A4*A4+A5*A5)/6.D0 *
     *            ( BTABOS%ADR(ITRAC)%P%R(L1)
     *            + BTABOS%ADR(ITRAC)%P%R(L2)
     *            + BTABOS%ADR(ITRAC)%P%R(L3) )
!
            ENDDO
            ENDIF
!
!        LATERAL BOUNDARIES
!
            DO IETAGE=1,NETAG
!
               IF(ATABOL%ADR(ITRAC)%P%TYPR.NE.'0') THEN
               DO IPTFR=1,NPTFR
!
                  L1 = IKLBORL(IPTFR,IETAGE,1)
                  L2 = IKLBORL(IPTFR,IETAGE,2)
                  L3 = IKLBORL(IPTFR,IETAGE,3)
                  L4 = IKLBORL(IPTFR,IETAGE,4)
                  N1 = NBOR3%I(L1)
                  N2 = NBOR3%I(L2)
                  N3 = NBOR3%I(L3)
                  N4 = NBOR3%I(L4)
!
                  FLUDI(5+ITRAC) = FLUDI(5+ITRAC) + SUR12 *
     *            ( ATABOL%ADR(ITRAC)%P%R(L1)*TA%ADR(ITRAC)%P%R(N1)
     *            + ATABOL%ADR(ITRAC)%P%R(L2)*TA%ADR(ITRAC)%P%R(N2)
     *            + ATABOL%ADR(ITRAC)%P%R(L3)*TA%ADR(ITRAC)%P%R(N3)
     *            + ATABOL%ADR(ITRAC)%P%R(L4)*TA%ADR(ITRAC)%P%R(N4) )
!
               ENDDO
               ENDIF
!
               IF(BTABOL%ADR(ITRAC)%P%TYPR.NE.'0') THEN
               DO IPTFR=1,NPTFR
!
                  L1 = IKLBORL(IPTFR,IETAGE,1)
                  L2 = IKLBORL(IPTFR,IETAGE,2)
                  L3 = IKLBORL(IPTFR,IETAGE,3)
                  L4 = IKLBORL(IPTFR,IETAGE,4)
                  N1 = NBOR3%I(L1)
                  N2 = NBOR3%I(L2)
                  N3 = NBOR3%I(L3)
                  N4 = NBOR3%I(L4)
                  D = SQRT((X(N2)-X(N1))**2 + (Y(N2)-Y(N1))**2)
                  Z_1 = ZPROP%R(N4) - ZPROP%R(N1)
                  Z_2 = ZPROP%R(N3) - ZPROP%R(N2)
                  A1 = D * (Z_1+Z_1+Z_2)
                  A2 = D * (Z_2+Z_2+Z_1)
!
                  FLUDI(5+ITRAC) = FLUDI(5+ITRAC) + SUR12 *
     *            ( BTABOL%ADR(ITRAC)%P%R(L1)*A1
     *             +BTABOL%ADR(ITRAC)%P%R(L2)*A2
     *             +BTABOL%ADR(ITRAC)%P%R(L3)*A2
     *             +BTABOL%ADR(ITRAC)%P%R(L4)*A1)
!
               ENDDO
               ENDIF
!
            ENDDO
!
            IF(NCSIZE.GT.1) FLUDI(5+ITRAC) = P_DSUM(FLUDI(5+ITRAC))
!
        ENDDO
!
      ENDIF
!
!=======================================================================
!
! COMPUTES CUMULATIVE FLUXES AND WRITES OUT
!
!=======================================================================
!
      FLUXTOTCUM = FLUXTOTCUM + FLUXTOTAL
!
      IF(INFOGR) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,601) MASSEN_WATER,MASSE_WATER,DT*FLUXTOTAL,
     *                  MASSEN_WATER-MASSE_WATER-DT*FLUXTOTAL
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,602) MASSEN_WATER,MASSE_WATER,DT*FLUXTOTAL,
     *                  MASSEN_WATER-MASSE_WATER-DT*FLUXTOTAL
        ENDIF
        IF(RAIN) THEN
          IF(LNG.EQ.1) WRITE(LU,603) DT*FLURAIN
          IF(LNG.EQ.2) WRITE(LU,604) DT*FLURAIN
        ENDIF
        IF(NFRLIQ.GT.0) THEN
          DO I=1,NFRLIQ
            IF(LNG.EQ.1) WRITE(LU,3020) I,-FLUX_BOUNDARIES(I)
            IF(LNG.EQ.2) WRITE(LU,4020) I,-FLUX_BOUNDARIES(I)
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF (NTRAC.GT.0) THEN
!
         DO IVBIL=6,5+NTRAC
!
            FLUTOT = FLUX%R(IVBIL) - FLUDI(IVBIL) + FLUS1(IVBIL)
            FLUCUM%R(IVBIL) = FLUCUM%R(IVBIL) + FLUTOT
!
            IF(INFOGR) THEN
             IF(SEDI.AND.(IVBIL.EQ.NTRAC+5)) THEN
               IF(LNG.EQ.1) WRITE(LU,611) FLUX%R(IVBIL),
     *         -FLUDI(IVBIL),MASSEN%R(IVBIL),MASSE%R(IVBIL),DT*FLUTOT,
     *                       MASSEN%R(IVBIL)-MASSE%R(IVBIL)-DT*FLUTOT
               IF(LNG.EQ.2) WRITE(LU,612) FLUX%R(IVBIL),
     *         -FLUDI(IVBIL),MASSEN%R(IVBIL),MASSE%R(IVBIL),DT*FLUTOT,
     *                       MASSEN%R(IVBIL)-MASSE%R(IVBIL)-DT*FLUTOT
             ELSE
               IF(LNG.EQ.1) THEN
                 WRITE(LU,621) IVBIL-5,FLUX%R(IVBIL),
     *         -FLUDI(IVBIL),MASSEN%R(IVBIL),MASSE%R(IVBIL),DT*FLUTOT,
     *                       MASSEN%R(IVBIL)-MASSE%R(IVBIL)-DT*FLUTOT
               ENDIF
               IF(LNG.EQ.2) THEN
                 WRITE(LU,622) IVBIL-5,FLUX%R(IVBIL),
     *         -FLUDI(IVBIL),MASSEN%R(IVBIL),MASSE%R(IVBIL),DT*FLUTOT,
     *                       MASSEN%R(IVBIL)-MASSE%R(IVBIL)-DT*FLUTOT
               ENDIF
             ENDIF
            ENDIF
!
         ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
601   FORMAT(/,'  EAU',
     *       /,'MASSE AU PAS DE TEMPS PRECEDENT               : ',G16.7,
     *       /,'MASSE AU PAS DE TEMPS EN COURS                : ',G16.7,
     *       /,'MASSE SORTIE PAR LES LIMITES PENDANT CE TEMPS : ',G16.7,
     *       /,'ERREUR SUR LA MASSE AU COURS DU PAS DE TEMPS  : ',G16.7)
!
602   FORMAT(/,'  WATER',
     *       /,'MASS AT THE PREVIOUS TIME STEP                : ',G16.7,
     *       /,'MASS AT THE PRESENT TIME STEP                 : ',G16.7,
     *       /,'MASS LEAVING THE DOMAIN DURING THIS TIME STEP : ',G16.7,
     *       /,'ERROR ON THE MASS DURING THIS TIME STEP       : ',G16.7)
!
603   FORMAT(  'BILAN PLUIE-EVAPORATION                       : ',G16.7)
604   FORMAT(  'BALANCE RAIN-EVAPORATION                      : ',G16.7)
!
611   FORMAT(/,'  SEDIMENT ',
     *       /,'FLUX CONVECTIF A TRAVERS LES BORDS            : ',G16.7,
     *       /,'FLUX DIFFUSIF + DEPOT                         : ',G16.7,
     *       /,'MASSE AU PAS DE TEMPS PRECEDENT               : ',G16.7,
     *       /,'MASSE AU PAS DE TEMPS EN COURS                : ',G16.7,
     *       /,'MASSE SORTIE PAR LES LIMITES PENDANT CE TEMPS : ',G16.7,
     *       /,'ERREUR SUR LA MASSE AU COURS DU PAS DE TEMPS  : ',G16.7)
!
612   FORMAT(/,'  SEDIMENT ',
     *       /,'ADVECTIVE FLUX THROUGH THE BOUNDARIES         : ',G16.7,
     *       /,'DIFFUSIVE FLUX + DEPOSITION                   : ',G16.7,
     *       /,'MASS AT THE PREVIOUS TIME STEP                : ',G16.7,
     *       /,'MASS AT THE PRESENT TIME STEP                 : ',G16.7,
     *       /,'MASS LEAVING THE DOMAIN DURING THIS TIME STEP : ',G16.7,
     *       /,'ERROR ON THE MASS DURING THIS TIME STEP       : ',G16.7)
!
621   FORMAT(/,'  TRACEUR  ',I2,
     *       /,'FLUX CONVECTIF A TRAVERS BORDS OU SOURCES     : ',G16.7,
     *       /,'FLUX DIFFUSIF A TRAVERS LES BORDS             : ',G16.7,
     *       /,'MASSE AU PAS DE TEMPS PRECEDENT               : ',G16.7,
     *       /,'MASSE AU PAS DE TEMPS EN COURS                : ',G16.7,
     *       /,'MASSE SORTIE (FRONTIERES OU SOURCE)           : ',G16.7,
     *       /,'ERREUR SUR LA MASSE AU COURS DU PAS DE TEMPS  : ',G16.7)
!
622   FORMAT(/,'  TRACER ',I2,
     *       /,'ADVECTIVE FLUX THROUGH BOUNDARIES OR SOURCES  : ',G16.7,
     *       /,'DIFFUSIVE FLUX THROUGH THE BOUNDARIES         : ',G16.7,
     *       /,'MASS AT THE PREVIOUS TIME STEP                : ',G16.7,
     *       /,'MASS AT THE PRESENT TIME STEP                 : ',G16.7,
     *       /,'MASS EXITING (BOUNDARIES OR SOURCE)           : ',G16.7,
     *       /,'ERROR ON THE MASS DURING THIS TIME STEP       : ',G16.7)
3020  FORMAT('FLUX FRONTIERE ',I4,' : ', G16.7 ,' M3/S',
     *          '  ( >0 : ENTRANT  <0 : SORTANT )')
4020  FORMAT('FLUX BOUNDARY ',I4,': ', G16.7 ,' M3/S',
     *          '  ( >0 : ENTERING  <0 : EXITING )')
!
!=======================================================================
!
! PRINTOUTS SPECIFIC TO THE LAST TIMESTEP
!
!=======================================================================
!
      IF (LT.EQ.NIT) THEN
!
         WRITE(LU,*)
         CALL MITTIT(11,AT,LT)
         WRITE (LU,'(A4,F16.4)') 'T = ',AT
!
         IF(LNG.EQ.1) WRITE(LU,701) MASINI_WATER,MASSE_WATER,
     *      DT*FLUXTOTCUM, MASINI_WATER-MASSE_WATER-DT*FLUXTOTCUM
         IF(LNG.EQ.2) WRITE(LU,702) MASINI_WATER,MASSE_WATER,
     *      DT*FLUXTOTCUM, MASINI_WATER-MASSE_WATER-DT*FLUXTOTCUM
!
!-----------------------------------------------------------------------
!
         IF (NTRAC.GT.0) THEN
!
            DO IVBIL=6,5+NTRAC
!
             IF(SEDI.AND.(IVBIL.EQ.NTRAC+5)) THEN
               IF(LNG.EQ.1) WRITE(LU,711)
     *               MASINI%R(IVBIL),MASSE%R(IVBIL),DT*FLUCUM%R(IVBIL),
     *               MASINI%R(IVBIL)-MASSE%R(IVBIL)-DT*FLUCUM%R(IVBIL)
               IF(LNG.EQ.2) WRITE(LU,712)
     *               MASINI%R(IVBIL),MASSE%R(IVBIL),DT*FLUCUM%R(IVBIL),
     *               MASINI%R(IVBIL)-MASSE%R(IVBIL)-DT*FLUCUM%R(IVBIL)
             ELSE
               IF(LNG.EQ.1) WRITE(LU,721) IVBIL-5,
     *               MASINI%R(IVBIL),MASSE%R(IVBIL),DT*FLUCUM%R(IVBIL),
     *               MASINI%R(IVBIL)-MASSE%R(IVBIL)-DT*FLUCUM%R(IVBIL)
               IF(LNG.EQ.2) WRITE(LU,722) IVBIL-5,
     *               MASINI%R(IVBIL),MASSE%R(IVBIL),DT*FLUCUM%R(IVBIL),
     *               MASINI%R(IVBIL)-MASSE%R(IVBIL)-DT*FLUCUM%R(IVBIL)
             ENDIF
!
            ENDDO
!
         ENDIF
!
       ENDIF
!
!-----------------------------------------------------------------------
!
701   FORMAT(//,'--- EAU ---',
     *       /,'MASSE INITIALE (DEBUT DE CE CALCUL) : ',G16.7,
     *       /,'MASSE FINALE                        : ',G16.7,
     *       /,'MASSE SORTIE DU DOMAINE (OU SOURCE) : ',G16.7,
     *       /,'MASSE PERDUE                        : ',G16.7)
!
702   FORMAT(//,'--- WATER ---',
     *       /,'INITIAL MASS                        : ',G16.7,
     *       /,'FINAL MASS                          : ',G16.7,
     *       /,'MASS LEAVING THE DOMAIN (OR SOURCE) : ',G16.7,
     *       /,'MASS LOSS                           : ',G16.7)
!
711   FORMAT(//,'--- SEDIMENT ---',
     *       /,'MASSE INITIALE (DEBUT DE CE CALCUL) : ',G16.7,
     *       /,'MASSE FINALE                        : ',G16.7,
     *       /,'MASSE SORTIE                        : ',G16.7,
     *       /,'MASSE PERDUE                        : ',G16.7)
!
712   FORMAT(//,'--- SEDIMENT ---',
     *       /,'INITIAL MASS                        : ',G16.7,
     *       /,'FINAL MASS                          : ',G16.7,
     *       /,'MASS LEAVING THE DOMAIN             : ',G16.7,
     *       /,'MASS LOSS                           : ',G16.7)
!
721   FORMAT(//,'--- TRACEUR ',I2,' ---',
     *       /,'MASSE INITIALE (DEBUT DE CE CALCUL) : ',G16.7,
     *       /,'MASSE FINALE                        : ',G16.7,
     *       /,'MASSE SORTIE (FRONTIERES OU SOURCE) : ',G16.7,
     *       /,'MASSE PERDUE                        : ',G16.7)
!
722   FORMAT(//,'--- TRACER',I2,' ---',
     *       /,'INITIAL MASS                        : ',G16.7,
     *       /,'FINAL MASS                          : ',G16.7,
     *       /,'MASS EXITING (BOUNDARIES OR SOURCE) : ',G16.7,
     *       /,'MASS LOSS                           : ',G16.7)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C
