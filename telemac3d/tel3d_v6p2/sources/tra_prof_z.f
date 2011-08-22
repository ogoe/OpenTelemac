!                    ************************************
                     DOUBLE PRECISION FUNCTION TRA_PROF_Z
!                    ************************************
!
     &( I , IPOIN2 , TIME , LT , IPLAN , ENTET , IOPT , ITRAC )
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    GIVES THE VERTICAL PROFILE FOR TRACERS.
!
!history  J-M HERVOUET (LNHE)
!+        12/09/07
!+        V5P8
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
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
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
      DOUBLE PRECISION USTAR,Y0,HH,DENOM,AUX,KS,DELTAZ,ROUSE,ZREFE
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
!
          IF(IPLAN.EQ.1) THEN
            TRA_PROF_Z= CREF%R(IPOIN2)
          ELSE
            IPOIN3=IPOIN2+(IPLAN-1)*NPOIN2
            USTAR=MAX(SQRT(UETCAR%R(IPOIN2)),1.D-6)
            ROUSE=-WCHU%R(IPOIN2)/KARMAN/USTAR
            ZREFE=KSPRATIO*DMOY%R(IPOIN2)
            HH=MAX( MESH3D%Z%R(IPOIN2+(NPLAN-1)*NPOIN2)
     &             -MESH3D%Z%R(IPOIN2)                  , 1.D-4)
            DELTAZ=MESH3D%Z%R(IPOIN3)-MESH3D%Z%R(IPOIN2)
            TRA_PROF_Z=(ZREFE/(HH-ZREFE)*(HH-DELTAZ)/DELTAZ)**ROUSE
            TRA_PROF_Z=CREF%R(IPOIN2)*TRA_PROF_Z
          ENDIF
!
        ENDIF
!
      ELSEIF(IOPT.EQ.3) THEN
!
!       NOT SEDIMENT : SO FAR PROFILE = 1.D0
!
        IF(ITRAC.NE.NTRAC.OR..NOT.SEDI) THEN
          TRA_PROF_Z=1.D0
        ELSE
!
!       HERE VALID ONLY FOR SEDIMENT :
!       MODIFIED ROUSE PROFILE FOR LAMINAR VISCOSITY
!
          IPOIN3=IPOIN2+(IPLAN-1)*NPOIN2
          USTAR=MAX(SQRT(UETCAR%R(IPOIN2)),1.D-6)
          ROUSE=-WCHU%R(IPOIN2)/KARMAN/USTAR
          HH=MAX( MESH3D%Z%R(IPOIN2+(NPLAN-1)*NPOIN2)
     &           -MESH3D%Z%R(IPOIN2)                  , 1.D-4)
          DELTAZ=MESH3D%Z%R(IPOIN3)-MESH3D%Z%R(IPOIN2)
          TRA_PROF_Z=((HH-DELTAZ)/(DELTAZ+DNUTAV/KARMAN/USTAR))**ROUSE
          TRA_PROF_Z= CREF%R(IPOIN2)*TRA_PROF_Z
     &     *(DNUTAV/KARMAN/USTAR/HH)**ROUSE
!
!         NOTE: CREF%R(IPOIN2)*(DNUTAV/KARMAN/USTAR/HH)**ROUSE
!               IS C AT MID-DEPTH
!
        ENDIF
!
!      ELSEIF(IOPT.EQ.4) THEN
!
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
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
