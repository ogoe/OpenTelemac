!                       **********************
                        SUBROUTINE NODALF_PUGH
!                       **********************
!
     &(FFMN2,FFM4,NODALCORR,TEMPS,DEJA,MARDAT,MARTIM)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   23/03/2011
!***********************************************************************
!
!brief    COMPUTES NODAL FACTORS F FROM PUGH FORMULAE
!+
!
!history  C-T PHAM (LNHE)
!+        23/03/2011
!+        V6P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFMN2          |<--| NODAL FACTOR FOR WAVES M2 AND N2
!| FFM4           |<--| NODAL FACTOR FOR WAVE  M4
!| MARDAT         |-->| DATE (YEAR,MONTH,DAY)
!| MARTIM         |-->| TIME (HOUR,MINUTE,SECOND)
!| NODALCORR      |-->| OPTION FOR CALCULATION OF NODAL FACTOR CORRECTION
!| TEMPS          |-->| TIME
!| DEJA           |-->| LOGICAL FOR 1ST TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NODALCORR,MARDAT(3),MARTIM(3)
      DOUBLE PRECISION, INTENT(OUT) :: FFMN2,FFM4
      DOUBLE PRECISION, INTENT(IN)  :: TEMPS
      LOGICAL, INTENT(IN)           :: DEJA
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION PI,NLUNPUGH,TJ,TJMIL,NMIL
!
      INTEGER YEAR,MONTH,DAY,NDAY,HOUR,MINUTE,SECOND,I
!
      SAVE YEAR,HOUR,MINUTE,SECOND,NDAY
!
!-----------------------------------------------------------------------
!
      PI = ACOS(-1.D0)
!
      IF(.NOT.DEJA) THEN
        YEAR  = MARDAT(1)
        MONTH = MARDAT(2)
        DAY   = MARDAT(3)
!
        HOUR   = MARTIM(1)
        MINUTE = MARTIM(2)
        SECOND = MARTIM(3)
!  NUMBER OF THE DAY IN YEAR YEAR
        NDAY = DAY
!
        DO I=MONTH-1,1,-1
          IF((I.EQ.1).OR.(I.EQ.3).OR.(I.EQ.5).OR.(I.EQ.7).OR.(I.EQ.8)
     1    .OR.(I.EQ.10)) THEN
            NDAY = NDAY + 31
          ELSEIF((I.EQ.4).OR.(I.EQ.6).OR.(I.EQ.9).OR.(I.EQ.11)) THEN
            NDAY = NDAY + 30
          ELSEIF(I.EQ.2) THEN
            IF((MOD(YEAR,4).NE.0)
     1      .OR.((MOD(YEAR,100).EQ.0).AND.(MOD(YEAR,400).NE.0))) THEN
              NDAY = NDAY + 28
            ELSE
              NDAY = NDAY + 29
            ENDIF
          ENDIF
        ENDDO
      ENDIF
!
      TJ = DBLE(365*(YEAR-1900)+(NDAY-1)
     &         +DINT(DBLE(YEAR-1901)/4.D0))/36525.D0
!     &   +(DBLE(HOUR)+DBLE(MINUTE)/60.D0+DBLE(SECOND)/3600.D0)/876600.D0
!
      NLUNPUGH = DMOD(259.16D0-1934.14D0*TJ+0.0021D0*TJ**2,360.D0)
!
      IF(NODALCORR.EQ.2) THEN
        TJMIL = DBLE(365*(YEAR-1900)+(183-1)
     &              +DINT(DBLE(YEAR-1901)/4.D0))/36525.D0
        NMIL  = DMOD(259.16D0-1934.14D0*TJMIL+0.0021D0*TJMIL**2,360.D0)
        FFMN2 = 1.D0-0.037D0*COS(NMIL*PI/180.D0)
      ELSE
        FFMN2 = 1.D0-0.037D0*COS(NLUNPUGH*PI/180.D0)
      ENDIF
!
      FFM4  = FFMN2**2
!
!-----------------------------------------------------------------------
!
      RETURN
      END
