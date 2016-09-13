!                    ************************
                     SUBROUTINE SOURCES_SINKS
!                    ************************
!
!
!***********************************************************************
! TELEMAC3D   V7P2
!***********************************************************************
!
!brief    BUILDS THE SOURCE TERMS TO ADD IN 2D AND 3D
!+                CONTINUITY EQUATIONS.
!
!history  J-M HERVOUET (LNHE)
!+        07/04/2009
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
!+        08/08/2012
!+        V6P2
!+   Correction of SMH with rain in parallel.
!
!history  A. GINEAU, N. DURAND, N. LORRAIN, C.-T. PHAM (LNHE)
!+        09/07/2014
!+        V7P0
!+   Adding non constant rain for exchange with atmosphere module
!
!history  J-M HERVOUET (LNHE)
!+        16/09/2014
!+        V7P0
!+   Correction of SMH in parallel with sources : it must be
!+   assembled.
!
!history  A. LEROY (EDF LAB, LNHE)
!+        28/08/2015
!+        V7P1
!+   Add the option OPTSOU to treat sources as a dirac (OPTSOU=2) or
!+   not (OPTSOU=1).
!
!history  A. LEROY (EDF LAB, LNHE)
!+        25/11/2015
!+        V7P1
!+   Remove the call to INTERPMETEO: all the meteo variables are now
!+   read in meteo.f and stored in variables of waqtel
!
!history  A. LEROY & J-M HERVOUET (EDF LAB, LNHE)
!+        14/03/2016
!+        V7P2
!+   Sources of type Dirac were not properly treated in scalar mode.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        11/09/2016
!+        V7P2
!+   Section on rain slightly rearranged to avoid code duplication.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
      USE DECLARATIONS_WAQTEL
      USE EXCHANGE_WITH_ATMOSPHERE
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,I,IP,IAD
!     HEAT EXCHANGE WITH ATMOSPHERE
      DOUBLE PRECISION WW,TREEL,SAL,RO,WW2,FLUX_EVAP,FLUX_SENS,DEBEVAP
!
!-----------------------------------------------------------------------
!
!     INITIALISING SMH
!
      CALL OS ('X=0     ',X=SMH)
!
!     SOURCES AND SINKS
!
      IF(NSCE.GT.0) THEN

        IF(OPTSOU.EQ.1) THEN
!
!         HERE T3_02 LIKE VOLU, BUT CALL PARCOM (AND ZPROP INSTEAD OF Z)
          CALL VECTOR(T3_02,'=','MASBAS          ',IELM3,1.D0,SVIDE,
     &                SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &                MESH3D,.FALSE.,MASKEL)
          IF(NCSIZE.GT.1) CALL PARCOM(T3_02,2,MESH3D)
          CALL CPSTVC(T3_02,T3_03)
          DO IS=1,NSCE
            CALL OS('X=0     ',X=T3_03)
!           IN PARALLEL IF ISCE(IS)=0, THE POINT IS OUTSIDE THE SUBDOMAIN
            IF(ISCE(IS).GT.0) THEN
              I=(KSCE(IS)-1)*NPOIN2+ISCE(IS)
              T3_03%R(I)=QSCE2(IS)/MAX(1.D-8,T3_02%R(I))
            ENDIF
            CALL VECTOR(SOURCES%ADR(IS)%P,'=','MASVEC          ',
     &                  IELM3,1.D0,T3_03,SVIDE,SVIDE,SVIDE,SVIDE,SVIDE,
     &                  MESH3D,MSK,MASKEL)
          ENDDO
!         SUMS ON THE VERTICAL TO GET THE 2D SOURCES
          CALL CPSTVC(SMH,T2_01)
          CALL OS('X=0     ',X=T2_01)
          DO IS=1,NSCE
            DO IP=1,NPLAN
              DO I=1,NPOIN2
                T2_01%R(I)=T2_01%R(I)+
     &                     SOURCES%ADR(IS)%P%R(I+NPOIN2*(IP-1))
              ENDDO
            ENDDO
          ENDDO
!         SMH IS ASSEMBLED IN //
          IF(NCSIZE.GT.1) CALL PARCOM(T2_01,2,MESH2D)
          CALL OS('X=X+Y   ',X=SMH,Y=T2_01)
!
        ELSEIF(OPTSOU.EQ.2) THEN
!
          CALL OS('X=0     ',X=SOURCES%ADR(1)%P)
          IF(NCSIZE.GT.1) CALL OS('X=0     ',X=SOURCES%ADR(2)%P)
          DO IS=1,NSCE
            IF(NCSIZE.GT.1) THEN
!             IN PARALLEL IF ISCE(IS)=0, THE POINT IS OUTSIDE THE SUBDOMAIN
              IF(ISCE(IS).GT.0) THEN
                I=(KSCE(IS)-1)*NPOIN2+ISCE(IS)
                SOURCES%ADR(1)%P%R(I)=QSCE2(IS)*MESH3D%IFAC%I(I)
                SOURCES%ADR(2)%P%R(I)=QSCE2(IS)
              ENDIF
            ELSE
              I=(KSCE(IS)-1)*NPOIN2+ISCE(IS)
              SOURCES%ADR(1)%P%R(I)=QSCE2(IS)
            ENDIF
          ENDDO
!         SUMS ON THE VERTICAL TO GET THE 2D SOURCES
          CALL CPSTVC(SMH,T2_01)
          CALL OS('X=0     ',X=T2_01)
!         SMH IS ASSEMBLED IN PARALLEL, WE TAKE THE ASSEMBLED ARRAY
          IAD=1
          IF(NCSIZE.GT.1) IAD=2
          DO IP=1,NPLAN
            DO I=1,NPOIN2
              T2_01%R(I)=
     &        T2_01%R(I)+SOURCES%ADR(IAD)%P%R(I+NPOIN2*(IP-1))
            ENDDO
          ENDDO
          CALL OS('X=X+Y   ',X=SMH,Y=T2_01)
!
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     RAIN AND EVAPORATION (NEGATIVE RAIN)
!
      IF(RAIN) THEN
        IF(ATMOSEXCH.NE.1.AND.ATMOSEXCH.NE.2) THEN
!         PLUIE MUST BE NON ASSEMBLED IN PARALLEL
          CALL OS('X=CY    ',X=PLUIE,Y=VOLU2D,C=RAIN_MMPD/86400000.D0)
          IF(NCSIZE.GT.1) THEN
!           USING V2DPAR AVOIDS A CALL PARCOM OF A COPY OF PLUIE
            CALL OS('X=CY    ',X=PARAPLUIE,Y=V2DPAR,
     &                         C=RAIN_MMPD/86400000.D0)
          ENDIF
        ELSEIF(ATMOSEXCH.EQ.1.OR.ATMOSEXCH.EQ.2) THEN
          DO I=1,NPOIN2
!           TEMPERATURE AND SALINITY AT THE SURFACE
            TREEL = TA%ADR(IND_T)%P%R(NPOIN3-NPOIN2+I)
            IF (IND_S.EQ.0) THEN
              SAL = 0.D0
            ELSE
              SAL = TA%ADR(IND_S)%P%R(NPOIN3-NPOIN2+I)
            ENDIF
!
            WW = SQRT(WIND%ADR(1)%P%R(I)**2 + WIND%ADR(2)%P%R(I)**2)
!           LOG LAW FOR WIND AT 2 METERS
!            WW2 = WW * LOG(2.D0/0.0002D0)/LOG(10.D0/0.0002D0)
!           DIRECTLY WRITTEN BELOW
            WW2 = WW * LOG(1.D4)/LOG(5.D4)
!           ALTERNATIVE LAW FOR WIND AT 2 METERS
!           WW2 = 0.6D0*WW
            RO = RO0*(1.D0-(7.D0*(TREEL-4.D0)**2-750.D0*SAL)*1.D-6)
            CALL EVAPO(TREEL,TAIR%R(I),WW2,PATMOS%R(I),HREL,RO,
     &                 FLUX_EVAP,FLUX_SENS,DEBEVAP,C_ATMOS)
!           WATER FLUXES = RAIN - EVAPORATION
!           CONVERSION FROM MM/S TO M/S --> *1.D-3
            PLUIE%R(I) = VOLU2D%R(I)*(RAINFALL*1.D-3-DEBEVAP)
            IF(NCSIZE.GT.1) THEN
!             USING V2DPAR AVOIDS A CALL PARCOM OF A COPY OF PLUIE
!             CONVERSION FROM MM/S TO M/S --> *1.D-3
              PARAPLUIE%R(I) = V2DPAR%R(I)*(RAINFALL*1.D-3-DEBEVAP)
            ENDIF
          ENDDO
        ENDIF
!
!       ADDING TO SMH
!
        IF(NCSIZE.GT.1) THEN
!         SMH MUST BE ASSEMBLED IN PARALLEL
          CALL OS('X=X+Y   ',X=SMH,Y=PARAPLUIE)
        ELSE
!         PARAPLUIE%R=>PLUIE%R  ! DONE ONCE FOR ALL IN POINT_TELEMAC3D
!         BUT PARAPLUIE ALLOCATED WITH SIZE 0 CANNOT BE USED AS BIEF_OBJ
          CALL OS('X=X+Y   ',X=SMH,Y=PLUIE)
        ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     PARALLELISM, REAL VALUES REQUIRED IN SOURCES FOR MURD3D
!     BUT BEWARE IN TRIDW2, PARCOM MUST NOT BE DONE TWICE ON SOURCES
!
!     12/06/2007 : VALUES WITHOUT PARCOM STORED IN ADDRESS IS+NSCE
!                  SIZE CHANGED ACCORDINGLY IN POINT_TELEMAC3D
!
      IF(NCSIZE.GT.1) THEN
        IF(NSCE.GT.0) THEN
          IF(OPTSOU.EQ.1) THEN
            DO IS=1,NSCE
              CALL OS('X=Y     ',X=SOURCES%ADR(IS+NSCE)%P,
     &                           Y=SOURCES%ADR(IS     )%P)
              CALL PARCOM(SOURCES%ADR(IS)%P,2,MESH3D)
            ENDDO
          ENDIF
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
