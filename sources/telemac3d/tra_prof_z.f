!                    ************************************
                     DOUBLE PRECISION FUNCTION TRA_PROF_Z
!                    ************************************
!
     &( I , IPOIN2 , TIME , LT , IPLAN , ENTET , IOPT , ITRAC )
!
!***********************************************************************
! TELEMAC3D   V7P1
!***********************************************************************
!
!brief    GIVES THE VERTICAL PROFILE FOR TRACERS.
!
!history  J-M HERVOUET (LNHE)
!+        12/09/2007
!+        V5P8
!+   First version.
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
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        25/06/2015
!+        V5P8
!+   First version.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ENTET          |-->| LOGICAL, IF YES INFORMATION IS PRINTED: NOT USED
!| I              |-->| NUMBER OF THE LIQUID BOUNDARY
!| IOPT           |-->| OPTION : 0 : USER DEFINED
!|                |   | 2 : ROUSE PROFILE FOR SEDIMENT
!|                |   | 3 : MODIFIED ROUSE PROFILE (VISCOSITY)
!| IPLAN          |-->| PLAN NUMBER
!| IPOIN2         |-->| 2D GLOBAL NUMBER OF POINT CONSIDERED
!| ITRAC          |-->| TRACER NUMBER
!| LT             |-->| ITERATION NUMBER
!| TIME           |-->| TIME OF TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          , INTENT(IN) :: I,IPOIN2,IPLAN,IOPT,LT
      INTEGER          , INTENT(IN) :: ITRAC
      DOUBLE PRECISION , INTENT(IN) :: TIME
      LOGICAL          , INTENT(IN) :: ENTET
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION USTAR,HH,DELTAZ,ROUSE,ZREFE
      DOUBLE PRECISION CMEAN,B,BROUSE
      INTEGER IPOIN3
!
!-----------------------------------------------------------------------
!
      IF(IOPT.EQ.0) THEN
!
        WRITE(LU,*) 'USER DEFINE PROFILE MISSING IN TRA_PROF_Z'
        CALL PLANTE(1)
        STOP
!
      ELSEIF(IOPT.EQ.2) THEN
!
!       NOT SEDIMENT : SO FAR PROFILE = 1.D0
!
        IF(ITRAC.NE.NTRAC.OR..NOT.SEDI) THEN
          TRA_PROF_Z=1.D0
        ELSE
!
!       HERE VALID ONLY FOR SEDIMENT : ROUSE PROFILE
!         divide by cref
          IF(IPLAN.EQ.1) THEN
             TRA_PROF_Z= 1.D0
          ELSE
            IPOIN3=IPOIN2+(IPLAN-1)*NPOIN2
            USTAR=MAX(SQRT(UETCAR%R(IPOIN2)),1.D-6)
            ROUSE= WCHU0*PRANDTL/KARMAN/USTAR
!           ZREFE=KSPRATIO*DMOY%R(IPOIN2)
            ZREFE=ZREF%R(IPOIN2)
            HH=MAX( MESH3D%Z%R(IPOIN2+(NPLAN-1)*NPOIN2)
     &             -MESH3D%Z%R(IPOIN2)  , 1.D-4)
            DELTAZ=MESH3D%Z%R(IPOIN3)-MESH3D%Z%R(IPOIN2)
!           bug correction CV
            TRA_PROF_Z=(ZREFE/(HH-ZREFE)*(HH-DELTAZ)/DELTAZ)**ROUSE
          ENDIF
          TRA_PROF_Z=CREF%R(IPOIN2)*TRA_PROF_Z
!
        ENDIF
!
      ELSEIF(IOPT.EQ.3) THEN
!
!       Normalised Rouse concentration profile
!       CV 14/01/2014
!
        IF(ITRAC.NE.NTRAC.OR..NOT.SEDI) THEN
          TRA_PROF_Z=1.D0
        ELSE
!         HERE VALID ONLY FOR SEDIMENT : ROUSE PROFILE
          HH=MAX( MESH3D%Z%R(IPOIN2+(NPLAN-1)*NPOIN2)
     &           -MESH3D%Z%R(IPOIN2)  , 1.D-4)
          USTAR=MAX(SQRT(UETCAR%R(IPOIN2)),1.D-6)
          ROUSE= WCHU0*PRANDTL/KARMAN/USTAR
          ZREFE=ZREF%R(IPOIN2)
          B=ZREFE/HH
!         CMEAN : Mean value (cf Sisyphe User Manual,
!                 subroutine suspension_Rouse)
          IF(ABS(ROUSE-1.D0).LE.1.D-04)THEN
            CMEAN= -LOG(B)
          ELSE
            BROUSE=MAX(B,1.D-04)**(ROUSE-1.D0)
            CMEAN= 1.D0/(ROUSE-1.D0)*(1.D0-BROUSE)
          ENDIF
          IF(IPLAN.EQ.1) THEN
            TRA_PROF_Z= 1.D0/MAX(CMEAN,1.D-08)
          ELSE
            IPOIN3=IPOIN2+(IPLAN-1)*NPOIN2
            DELTAZ=MESH3D%Z%R(IPOIN3)-MESH3D%Z%R(IPOIN2)
            TRA_PROF_Z= (HH/DELTAZ)**ROUSE/MAX(CMEAN,1.D-08)
          ENDIF
        ENDIF
!
      ELSEIF(IOPT.EQ.4) THEN
!
!       Modified Rouse profile with eddy viscosity accounted for
!
        IF(ITRAC.NE.NTRAC.OR..NOT.SEDI) THEN
          TRA_PROF_Z=1.D0
        ELSE
!         HERE VALID ONLY FOR SEDIMENT :
!         MODIFIED ROUSE PROFILE FOR LAMINAR VISCOSITY
          IPOIN3=IPOIN2+(IPLAN-1)*NPOIN2
          USTAR=MAX(SQRT(UETCAR%R(IPOIN2)),1.D-6)
!          ROUSE=-WCHU%R(IPOIN2)/KARMAN/USTAR
          ROUSE= WCHU%R(IPOIN2)/KARMAN/USTAR
          HH=MAX( MESH3D%Z%R(IPOIN2+(NPLAN-1)*NPOIN2)
     &           -MESH3D%Z%R(IPOIN2)                  , 1.D-4)
          DELTAZ=MESH3D%Z%R(IPOIN3)-MESH3D%Z%R(IPOIN2)
          TRA_PROF_Z=((HH-DELTAZ)/(DELTAZ+DNUTAV(ITRAC)/
     &                                         KARMAN/USTAR))**ROUSE
          TRA_PROF_Z= CREF%R(IPOIN2)*TRA_PROF_Z
     &     *(DNUTAV(ITRAC)/KARMAN/USTAR/HH)**ROUSE
        ENDIF
!
      ELSE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'TRA_PROF_Z : OPTION INCONNUE POUR LE PROFIL'
          WRITE(LU,*) 'IOPT=',IOPT,' 0 ET 2 POSSIBLES SEULEMENT'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'TRA_PROF_Z: UNKNOWN OPTION FOR THE PROFILE'
          WRITE(LU,*) 'IOPT=',IOPT,' 0 AND 2 ONLY ARE POSSIBLE'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
