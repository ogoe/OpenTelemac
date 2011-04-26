!                    ****************
                     SUBROUTINE BIL3D
!                    ****************
!
     &(LT,IKLBORL,IKLE2L,NPTFR,NETAG,NELEM)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE RELATIVE BALANCE OF THE MASSES OF
!+                WATER AND TRACERS DURING A TIMESTEP, AS WELL AS
!+                THE ABSOLUTE CUMULATIVE BALANCE.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
!+   FORTRAN95 VERSION
!
!history  J-M HERVOUET (LNHE)
!+        17/06/2008
!+        V5P9
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  J-M HERVOUET (LNHE)
!+        16/03/2011
!+        V6P1
!+   Mass-lumping taken into account in computation of diffusive fluxes,
!+   like in diff3d.f (messages on mass-conservation of sediment were
!+   wrong).
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IKLBORL        |-->| CONNECTIVITY TABLE OF LATERAL BOUNDARIES
!| IKLE2L         |-->| CONNECTIVITY TABLE OF 2D MESH
!| LT             |-->| CURRENT TIME STEP NUMBER
!| NELEM          |-->| NUMBER OF ELEMENTS IN THE 2D MESH
!| NETAG          |-->| NUMBER OF LAYERS (I.E. NPLAN-1)
!| NPTFR          |-->| NUMBER OF 2D BOUNDARY POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_BIL3D => BIL3D
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: LT,NPTFR,NETAG,NELEM
      INTEGER, INTENT(IN) :: IKLBORL(NPTFR,NETAG,4),IKLE2L(NELEM,3)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION FLUTOT,SUR12,A1,A2,A3,A4,A5,FLURAIN
      DOUBLE PRECISION D, X_2,X_3, Y_2,Y_3, Z_1,Z_2,Z_3,Z_5,Z_6
!
      INTEGER I,L1,L2,L3,L4,N1,N2,N3,N4,IVBIL,ILIQ
      INTEGER IELEM2,IPTFR,IETAGE,ITRAC
!                            25=MAXTRA+5
      DOUBLE PRECISION FLUDI(25),FLUS1(25),FLUXTOTAL
      INTRINSIC SQRT
!
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
!
!=======================================================================
!
!     COMPUTES THE FLUXES AT THE LIQUID BOUNDARIES
!
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
!   COMPUTES THE ADVECTION FLUXES ON THE VARIOUS OPEN BOUNDARIES
!
!=======================================================================
!
!     TOTAL FLUX, STARTING WITH THE LIQUID BOUNDARIES
!
      FLUXTOTAL=0.D0
      IF(NFRLIQ.GT.0) THEN
        DO I=1,NFRLIQ
          FLUXTOTAL=FLUXTOTAL+FLUX_BOUNDARIES(I)
        ENDDO
      ENDIF
!
!     RAIN AND EVAPORATION
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
!       IF(NTRAC.GT.0) THEN
!         DO I=1,NSCE
!         DO IVBIL=2,1+NTRAC
!         NOW DONE IN CVDF3D AND MURD3D
!                                                      TASCE(MAXSCE,NTRAC)
!             FLUX%R(IVBIL) = FLUX%R(IVBIL) - QSCE2(I)*TASCE2(I,IVBIL-1)
!         ENDDO
!         ENDDO
!       ENDIF
      ENDIF
!
!=======================================================================
!
!  COMPUTES THE FLUXES BY DIFFUSION AND SOURCES OF TRACERS
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
     &                        +S1TA%ADR(ITRAC)%P%R(I)*VOLU%R(I)*
     &                           TA%ADR(ITRAC)%P%R(I)
            ENDDO
            IF(NCSIZE.GT.1) THEN
              FLUS1(5+ITRAC) = P_DSUM(FLUS1(5+ITRAC))
            ENDIF
          ENDIF
        ENDDO
      ENDIF
!
!   ===============================
!   FLUX BY DIFFUSION OF TRACERS
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
!             WITH MASS-LUMPING LIKE IN DIFF3D
              DO I=1,NPOIN2
                FLUDI(5+ITRAC) = FLUDI(5+ITRAC)
     &          + ATABOF%ADR(ITRAC)%P%R(I)*VOLU2D%R(I)
     &                                    *TA%ADR(ITRAC)%P%R(I)
              ENDDO
            ENDIF
!
            IF(ATABOS%ADR(ITRAC)%P%TYPR.NE.'0') THEN
!             WITH MASS-LUMPING LIKE IN DIFF3D
              DO I=1,NPOIN2
                FLUDI(5+ITRAC) = FLUDI(5+ITRAC)
     &          + ATABOS%ADR(ITRAC)%P%R(I)*VOLU2D%R(I)
     &          * TA%ADR(ITRAC)%P%R(I+NETAG*NPOIN2)
              ENDDO
            ENDIF
!
            IF(BTABOF%ADR(ITRAC)%P%TYPR.NE.'0') THEN
              DO I=1,NPOIN2
                FLUDI(5+ITRAC) = FLUDI(5+ITRAC)
     &          + VOLU2D%R(I)*BTABOF%ADR(ITRAC)%P%R(I)
              ENDDO
            ENDIF
!
            IF(BTABOS%ADR(ITRAC)%P%TYPR.NE.'0') THEN
              DO I=1,NPOIN2
                FLUDI(5+ITRAC) = FLUDI(5+ITRAC)
     &          +VOLU2D%R(I)*BTABOS%ADR(ITRAC)%P%R(I)
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
     &            ( ATABOL%ADR(ITRAC)%P%R(L1)*TA%ADR(ITRAC)%P%R(N1)
     &            + ATABOL%ADR(ITRAC)%P%R(L2)*TA%ADR(ITRAC)%P%R(N2)
     &            + ATABOL%ADR(ITRAC)%P%R(L3)*TA%ADR(ITRAC)%P%R(N3)
     &            + ATABOL%ADR(ITRAC)%P%R(L4)*TA%ADR(ITRAC)%P%R(N4) )
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
     &            ( BTABOL%ADR(ITRAC)%P%R(L1)*A1
     &             +BTABOL%ADR(ITRAC)%P%R(L2)*A2
     &             +BTABOL%ADR(ITRAC)%P%R(L3)*A2
     &             +BTABOL%ADR(ITRAC)%P%R(L4)*A1)
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
     &                  MASSEN_WATER-MASSE_WATER-DT*FLUXTOTAL
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,602) MASSEN_WATER,MASSE_WATER,DT*FLUXTOTAL,
     &                  MASSEN_WATER-MASSE_WATER-DT*FLUXTOTAL
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
      IF(NTRAC.GT.0) THEN
!
        DO IVBIL=6,5+NTRAC
!
          FLUTOT = FLUX%R(IVBIL) - FLUDI(IVBIL) + FLUS1(IVBIL)
          FLUCUM%R(IVBIL) = FLUCUM%R(IVBIL) + FLUTOT
!
          IF(INFOGR) THEN
            IF(SEDI.AND.(IVBIL.EQ.NTRAC+5)) THEN
               IF(LNG.EQ.1) WRITE(LU,611) FLUX%R(IVBIL),
     &         -FLUDI(IVBIL),MASSEN%R(IVBIL),MASSE%R(IVBIL),DT*FLUTOT,
     &                       MASSEN%R(IVBIL)-MASSE%R(IVBIL)-DT*FLUTOT
               IF(LNG.EQ.2) WRITE(LU,612) FLUX%R(IVBIL),
     &         -FLUDI(IVBIL),MASSEN%R(IVBIL),MASSE%R(IVBIL),DT*FLUTOT,
     &                       MASSEN%R(IVBIL)-MASSE%R(IVBIL)-DT*FLUTOT
            ELSE
              IF(LNG.EQ.1) THEN
               WRITE(LU,621) IVBIL-5,FLUX%R(IVBIL),
     &         -FLUDI(IVBIL),MASSEN%R(IVBIL),MASSE%R(IVBIL),DT*FLUTOT,
     &                       MASSEN%R(IVBIL)-MASSE%R(IVBIL)-DT*FLUTOT
              ENDIF
              IF(LNG.EQ.2) THEN
                 WRITE(LU,622) IVBIL-5,FLUX%R(IVBIL),
     &         -FLUDI(IVBIL),MASSEN%R(IVBIL),MASSE%R(IVBIL),DT*FLUTOT,
     &                       MASSEN%R(IVBIL)-MASSE%R(IVBIL)-DT*FLUTOT
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
     &       /,'MASSE AU PAS DE TEMPS PRECEDENT               : ',G16.7,
     &       /,'MASSE AU PAS DE TEMPS EN COURS                : ',G16.7,
     &       /,'MASSE SORTIE PAR LES LIMITES PENDANT CE TEMPS : ',G16.7,
     &       /,'ERREUR SUR LA MASSE AU COURS DU PAS DE TEMPS  : ',G16.7)
!
602   FORMAT(/,'  WATER',
     &       /,'MASS AT THE PREVIOUS TIME STEP                : ',G16.7,
     &       /,'MASS AT THE PRESENT TIME STEP                 : ',G16.7,
     &       /,'MASS LEAVING THE DOMAIN DURING THIS TIME STEP : ',G16.7,
     &       /,'ERROR ON THE MASS DURING THIS TIME STEP       : ',G16.7)
!
603   FORMAT(  'BILAN PLUIE-EVAPORATION                       : ',G16.7)
604   FORMAT(  'BALANCE RAIN-EVAPORATION                      : ',G16.7)
!
611   FORMAT(/,'  SEDIMENT ',
     &       /,'FLUX CONVECTIF A TRAVERS LES BORDS            : ',G16.7,
     &       /,'FLUX DIFFUSIF + DEPOT                         : ',G16.7,
     &       /,'MASSE AU PAS DE TEMPS PRECEDENT               : ',G16.7,
     &       /,'MASSE AU PAS DE TEMPS EN COURS                : ',G16.7,
     &       /,'MASSE SORTIE PAR LES LIMITES PENDANT CE TEMPS : ',G16.7,
     &       /,'ERREUR SUR LA MASSE AU COURS DU PAS DE TEMPS  : ',G16.7)
!
612   FORMAT(/,'  SEDIMENT ',
     &       /,'ADVECTIVE FLUX THROUGH THE BOUNDARIES         : ',G16.7,
     &       /,'DIFFUSIVE FLUX + DEPOSITION                   : ',G16.7,
     &       /,'MASS AT THE PREVIOUS TIME STEP                : ',G16.7,
     &       /,'MASS AT THE PRESENT TIME STEP                 : ',G16.7,
     &       /,'MASS LEAVING THE DOMAIN DURING THIS TIME STEP : ',G16.7,
     &       /,'ERROR ON THE MASS DURING THIS TIME STEP       : ',G16.7)
!
621   FORMAT(/,'  TRACEUR  ',I2,
     &       /,'FLUX CONVECTIF A TRAVERS BORDS OU SOURCES     : ',G16.7,
     &       /,'FLUX DIFFUSIF A TRAVERS LES BORDS             : ',G16.7,
     &       /,'MASSE AU PAS DE TEMPS PRECEDENT               : ',G16.7,
     &       /,'MASSE AU PAS DE TEMPS EN COURS                : ',G16.7,
     &       /,'MASSE SORTIE (FRONTIERES OU SOURCE)           : ',G16.7,
     &       /,'ERREUR SUR LA MASSE AU COURS DU PAS DE TEMPS  : ',G16.7)
!
622   FORMAT(/,'  TRACER ',I2,
     &       /,'ADVECTIVE FLUX THROUGH BOUNDARIES OR SOURCES  : ',G16.7,
     &       /,'DIFFUSIVE FLUX THROUGH THE BOUNDARIES         : ',G16.7,
     &       /,'MASS AT THE PREVIOUS TIME STEP                : ',G16.7,
     &       /,'MASS AT THE PRESENT TIME STEP                 : ',G16.7,
     &       /,'MASS EXITING (BOUNDARIES OR SOURCE)           : ',G16.7,
     &       /,'ERROR ON THE MASS DURING THIS TIME STEP       : ',G16.7)
3020  FORMAT('FLUX FRONTIERE ',I4,' : ', G16.7 ,' M3/S',
     &          '  ( >0 : ENTRANT  <0 : SORTANT )')
4020  FORMAT('FLUX BOUNDARY ',I4,': ', G16.7 ,' M3/S',
     &          '  ( >0 : ENTERING  <0 : EXITING )')
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
     &      DT*FLUXTOTCUM, MASINI_WATER-MASSE_WATER-DT*FLUXTOTCUM
         IF(LNG.EQ.2) WRITE(LU,702) MASINI_WATER,MASSE_WATER,
     &      DT*FLUXTOTCUM, MASINI_WATER-MASSE_WATER-DT*FLUXTOTCUM
!
!-----------------------------------------------------------------------
!
         IF (NTRAC.GT.0) THEN
!
            DO IVBIL=6,5+NTRAC
!
             IF(SEDI.AND.(IVBIL.EQ.NTRAC+5)) THEN
               IF(LNG.EQ.1) WRITE(LU,711)
     &               MASINI%R(IVBIL),MASSE%R(IVBIL),DT*FLUCUM%R(IVBIL),
     &               MASINI%R(IVBIL)-MASSE%R(IVBIL)-DT*FLUCUM%R(IVBIL)
               IF(LNG.EQ.2) WRITE(LU,712)
     &               MASINI%R(IVBIL),MASSE%R(IVBIL),DT*FLUCUM%R(IVBIL),
     &               MASINI%R(IVBIL)-MASSE%R(IVBIL)-DT*FLUCUM%R(IVBIL)
             ELSE
               IF(LNG.EQ.1) WRITE(LU,721) IVBIL-5,
     &               MASINI%R(IVBIL),MASSE%R(IVBIL),DT*FLUCUM%R(IVBIL),
     &               MASINI%R(IVBIL)-MASSE%R(IVBIL)-DT*FLUCUM%R(IVBIL)
               IF(LNG.EQ.2) WRITE(LU,722) IVBIL-5,
     &               MASINI%R(IVBIL),MASSE%R(IVBIL),DT*FLUCUM%R(IVBIL),
     &               MASINI%R(IVBIL)-MASSE%R(IVBIL)-DT*FLUCUM%R(IVBIL)
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
     &       /,'MASSE INITIALE (DEBUT DE CE CALCUL) : ',G16.7,
     &       /,'MASSE FINALE                        : ',G16.7,
     &       /,'MASSE SORTIE DU DOMAINE (OU SOURCE) : ',G16.7,
     &       /,'MASSE PERDUE                        : ',G16.7)
!
702   FORMAT(//,'--- WATER ---',
     &       /,'INITIAL MASS                        : ',G16.7,
     &       /,'FINAL MASS                          : ',G16.7,
     &       /,'MASS LEAVING THE DOMAIN (OR SOURCE) : ',G16.7,
     &       /,'MASS LOSS                           : ',G16.7)
!
711   FORMAT(//,'--- SEDIMENT ---',
     &       /,'MASSE INITIALE (DEBUT DE CE CALCUL) : ',G16.7,
     &       /,'MASSE FINALE                        : ',G16.7,
     &       /,'MASSE SORTIE                        : ',G16.7,
     &       /,'MASSE PERDUE                        : ',G16.7)
!
712   FORMAT(//,'--- SEDIMENT ---',
     &       /,'INITIAL MASS                        : ',G16.7,
     &       /,'FINAL MASS                          : ',G16.7,
     &       /,'MASS LEAVING THE DOMAIN             : ',G16.7,
     &       /,'MASS LOSS                           : ',G16.7)
!
721   FORMAT(//,'--- TRACEUR ',I2,' ---',
     &       /,'MASSE INITIALE (DEBUT DE CE CALCUL) : ',G16.7,
     &       /,'MASSE FINALE                        : ',G16.7,
     &       /,'MASSE SORTIE (FRONTIERES OU SOURCE) : ',G16.7,
     &       /,'MASSE PERDUE                        : ',G16.7)
!
722   FORMAT(//,'--- TRACER',I2,' ---',
     &       /,'INITIAL MASS                        : ',G16.7,
     &       /,'FINAL MASS                          : ',G16.7,
     &       /,'MASS EXITING (BOUNDARIES OR SOURCE) : ',G16.7,
     &       /,'MASS LOSS                           : ',G16.7)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
