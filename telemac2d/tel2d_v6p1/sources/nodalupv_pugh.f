!                       ************************
                        SUBROUTINE NODALUPV_PUGH
!                       ************************
!
     &(UPVM2,UPVN2,MARDAT,MARTIM)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   23/03/2011
!***********************************************************************
!
!brief    COMPUTES NODAL FACTORS PHASE FROM PUGH FORMULAE
!+
!
!history  C-T PHAM (LNHE)
!+        23/03/2011
!+        V6P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| MARDAT         |-->| DATE (YEAR, MONTH,DAY)
!| MARTIM         |-->| TIME (HOUR, MINUTE,SECOND)
!| UPVM2          |<--| U+V (ORIGIN + NODAL PHASE) FOR WAVE M2
!| UPVN2          |<--| U+V (ORIGIN + NODAL PHASE) FOR WAVE N2
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: MARDAT(3),MARTIM(3)
      DOUBLE PRECISION, INTENT(OUT) :: UPVM2,UPVN2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION PI
      DOUBLE PRECISION SLUNPUGH,HSOLPUGH,PLUNPUGH,NLUNPUGH
!      DOUBLE PRECISION PSOLPUGH
      DOUBLE PRECISION TJ
      DOUBLE PRECISION VVM2,UUM2,VVN2,UUN2
!
      INTEGER YEAR,MONTH,DAY,NDAY,HOUR,MINUTE,SECOND,I
!
!-----------------------------------------------------------------------
!
      PI = ACOS(-1.D0)
!
      YEAR  = MARDAT(1)
      MONTH = MARDAT(2)
      DAY   = MARDAT(3)
!
      HOUR   = MARTIM(1)
      MINUTE = MARTIM(2)
      SECOND = MARTIM(3)
!
!     NUMBER OF THE DAY IN YEAR YEAR
!
      NDAY = DAY
!
      DO I=MONTH-1,1,-1
        IF((I.EQ.1).OR.(I.EQ.3).OR.(I.EQ.5).OR.(I.EQ.7).OR.(I.EQ.8)
     &  .OR.(I.EQ.10)) THEN
          NDAY = NDAY + 31
        ELSEIF((I.EQ.4).OR.(I.EQ.6).OR.(I.EQ.9).OR.(I.EQ.11)) THEN
          NDAY = NDAY + 30
        ELSEIF(I.EQ.2) THEN
          IF((MOD(YEAR,4).NE.0)
     &    .OR.((MOD(YEAR,100).EQ.0).AND.(MOD(YEAR,400).NE.0))) THEN
            NDAY = NDAY + 28
          ELSE
            NDAY = NDAY + 29
          ENDIF
        ENDIF
      ENDDO
!
      TJ = DBLE(365*(YEAR-1900)+(NDAY-1)
     &         +DINT(DBLE(YEAR-1901)/4.D0))/36525.D0
!     &   +(DBLE(HOUR)+DBLE(MINUTE)/60.D0+DBLE(SECOND)/3600.D0)/876600.D0
!
      SLUNPUGH = DMOD(277.02D0+481267.89D0*TJ+0.0011D0*TJ**2,360.D0)
      HSOLPUGH = DMOD(280.19D0+ 36000.77D0*TJ+0.0003D0*TJ**2,360.D0)
      PLUNPUGH = DMOD(334.39D0+  4069.04D0*TJ-0.0103D0*TJ**2,360.D0)
      NLUNPUGH = DMOD(259.16D0-  1934.14D0*TJ+0.0021D0*TJ**2,360.D0)
!      PSOLPUGH = DMOD(281.22D0+     1.72D0*TJ+0.0005D0*TJ**2,360.D0)
!
      VVM2 = DMOD(2.D0*(HSOLPUGH-SLUNPUGH),360.D0)
      UUM2 = -2.1D0*SIN(NLUNPUGH*PI/180.D0)
      VVN2 = DMOD(-3.D0*SLUNPUGH+2.D0*HSOLPUGH+PLUNPUGH,360.D0)
      UUN2 = UUM2
!
      UPVM2 = DMOD(UUM2+VVM2,360.D0)
      UPVN2 = DMOD(UUN2+VVN2,360.D0)
!
      IF(UPVM2.LT.0.D0) UPVM2 = UPVM2 + 360.D0
      IF(UPVN2.LT.0.D0) UPVN2 = UPVN2 + 360.D0
!
!     DEGREES TO RADIANS CONVERSIONS
!
      UPVM2 = UPVM2 / 180.D0 * PI
      UPVN2 = UPVN2 / 180.D0 * PI
!
!-----------------------------------------------------------------------
!
      RETURN
      END
