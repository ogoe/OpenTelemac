!                    ************************
                     SUBROUTINE SOURCE_MOMENT
!                    ************************
!
     &(NS,G,DT,UA,H,QU,QV,YAFRICTION,CF,YACORIOL,CORIOLIS,
     & SPHERIC,COSLAT,SINLAT,LT,FU,FV,YASMO)
!
!***********************************************************************
! TELEMAC2D   V7P3
!***********************************************************************
!
!brief    COMPUTES SOURCE TERMS FOR MOMENTUM EQUATION (CASE OF FINITE
!         VOLUMES). ARE CONSIDERED:
!           - FRICTION TERM.
!           - CORIOLIS FORCE
!           - ALL OTHER TERMS ARE COMPUTED BEFORE IN PROSOU_FV
!
!history  R. ATA (EDF CHATOU LAB)
!+
!+        CREATION
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF             |-->| THE FRICTION COEFFICIENT
!| CORIOLIS       |-->| CORIOLIS COEFFICIENT
!| COSLAT         |-->| COSINUS OF LATITUDE (SPHERICAL COORDINATES)
!| DT             |-->| TIME STEP
!|  FU,FV         |-->|  SOURCE TERMS FOR MOMENTUM (ON X AND Y)
!| G              |-->| GRAVITY
!| H              |-->| WATER DEPTH AT TN
!| LT             |-->| NUMBER OF TIME STEP
!| NS             |-->| TOTAL NUMBER OF NODES
!| QU             |-->| HU AT TIME TN
!| QV             |-->| HV AT TIME TN
!| SINLAT         |-->| SINUS OF LATITUDE (SPHERICAL COORDINATES)
!| SPHERIC        |-->| IF TRUE : SPHERICAL COORDINATES
!| UA             |<->| (H,HU,HV) AT TN+1
!| YACORIOL       |-->| LOGIC: IF YES CONSIDER CORIOLIS FORCE
!| YAFRICTION     |-->| LOGIC: IF YES CONIDER FRICTION
!| YASMO          |-->| LOGIC: IF YES CONIDER REMAINING FORCES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SPECIAL
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_SOUMOM => SOURCE_MOMENT
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NS,LT
      LOGICAL, INTENT(IN)             :: SPHERIC,YACORIOL,YAFRICTION
      LOGICAL, INTENT(IN)             :: YASMO
      DOUBLE PRECISION, INTENT(IN)    :: G,DT,CORIOLIS
      DOUBLE PRECISION, INTENT(IN)    :: CF(NS)
      DOUBLE PRECISION, INTENT(IN)    :: H(NS),QU(NS),QV(NS)
      DOUBLE PRECISION, INTENT(INOUT) :: UA(3,NS)
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: COSLAT,SINLAT,FU,FV

!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS
      DOUBLE PRECISION AKAP,STRIC2,SM1,SM2,GDT,DETER
      DOUBLE PRECISION A,A2SUR4,ASUR2,FOURSUR3,PI,WROT
!
!-----------------------------------------------------------------------
!
      SM1  = 0.D0
      SM2  = 0.D0
      A    = 0.D0
      AKAP = 1.D0
!
      IF(YACORIOL)THEN
        A  = CORIOLIS*DT
        PI = 4.D0 * ATAN(1.D0)
        IF(SPHERIC)THEN
          WROT = 2.D0*PI/86164.D0
          A    = 2.D0*WROT*DT
          IF(LT.EQ.1) THEN
            IF(LNG.EQ.1) WRITE(LU,11)
            IF(LNG.EQ.2) WRITE(LU,12)
          ENDIF
11        FORMAT(1X,'SOURCE_MOMENT : EN COORDONNEES SHERIQUES, LE',/,
     &           1X,'    COEFFICIENT DE CORIOLIS EST',/,
     &           1X,'    CALCULE EN FONCTION DE LA LATITUDE.',/,
     &           1X,'    LE MOT-CLE ''COEFFICIENT DE CORIOLIS''',/,
     &           1X,'    N''EST DONC PAS PRIS EN COMPTE.')
12        FORMAT(1X,'SOURCE_MOMENT : IN SPHERICAL COORDINATES, THE ',/,
     &           1X,'    CORIOLIS  PARAMETER DEPENDS ON THE LATITUDE',/,
     &           1X,'    THE KEY WORD ''CORIOLIS COEFFICIENT''',/,
     &           1X,'    IS CONSEQUENTLY IGNORED.')

        ENDIF
      ENDIF
      A2SUR4    = 0.25D0*(A**2)
      ASUR2     = 0.5D0 *A
      GDT       = G*DT
      FOURSUR3  = 4.D0/3.D0
      DO IS =1,NS
!       INITIALIZE SM1 AND SM2
        SM1 = UA(2,IS)
        SM2 = UA(3,IS)
!
!       1-  FRICTION
!
        IF(YAFRICTION)THEN
          STRIC2 = CF(IS)**2
!         IF(H(IS).LE.1.D-12.OR.UA(1,IS).LE.1.D-12)  THEN
          IF((H(IS)   .LE.1.D-12).OR.
     &       (UA(1,IS).LE.1.D-12).OR.
     &       (CF(IS)  .LE.1.D-12)    ) THEN
            AKAP=1.D0
          ELSE
            AKAP=1.D0+ GDT*SQRT(QU(IS)**2+QV(IS)**2)/
     &               (STRIC2*H(IS)*UA(1,IS)**(FOURSUR3))
          ENDIF
        ENDIF
!
!       2- CORIOLIS
!
        IF(YACORIOL)THEN
          IF(SPHERIC)THEN
            SM1 = UA(2,IS) + A*SINLAT%R(IS)*QV(IS)
            SM2 = UA(3,IS) - A*SINLAT%R(IS)*QU(IS)
          ELSE
            SM1 = UA(2,IS) + A*QV(IS)
            SM2 = UA(3,IS) - A*QU(IS)
          ENDIF
        ENDIF
!
!       3- REMAINNING FORCES
!
!       warning: not consistant with semi implicit in time
!                to adapt if not satisfactory
        IF(YASMO)THEN
          SM1 = SM1 + 0.5D0*DT*(H(IS)+UA(1,IS))*FU%R(IS)
          SM2 = SM2 + 0.5D0*DT*(H(IS)+UA(1,IS))*FV%R(IS)
        ENDIF
!
!       DETERMINANT OF THE CRAMER SYSTEM
!
        DETER= A2SUR4+AKAP**2
!
!       FINAL VALUES OF HU AND HV
!
        UA(2,IS)= (AKAP*SM1 + ASUR2*SM2)/DETER
        UA(3,IS)= (AKAP*SM2 - ASUR2*SM1)/DETER
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
