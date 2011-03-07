!                    *******************************
                     DOUBLE PRECISION FUNCTION TSLOC
!                    *******************************
!
     & (YEAR,MONTH,DAY,HOUR,MINUTE,SEC,AT)
!
!***********************************************************************
! BIEF   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE LOCAL SIDEREAL TIME IN RADIAN FOR
!+                THE GIVEN DATE IN UNIVERSAL TIME.
!code
!+     IN THIS FUNCTION, THE COMPUTATION TIME MUST BE SPLIT IN:
!+
!+     - TIME UNTIL 0:00 OF THE SAME DAY,
!+     - TIME LEFT UNTIL THE PRECISE DATE IN SECONDS.
!+
!+     THIS REQUIRES A COMBINATION BETWEEN THE STARTING DATE
!+     AND THE COMPUTING TIME AT:
!+
!+     COMPUTES THE TIME IN JULIAN CENTURY AT 0:00 OF THE SAME DAY
!+
!+     AT1 : NUMBER OF DAYS IN SECONDS TO ADD TO THE REFERENCE
!+           DATE (YEAR, MONTH, DAY) TO GET TO THE COMPUTATION
!+           DATE AT TIME (0H, 0MIN, 0SEC).
!+     ATR : NUMBER OF SECONDS BETWEEN THE DATE REPRESENTED BY AT1
!+           AND THE EXACT COMPUTATION DATE. ATR ACTUALLY CORRESPONDS
!+           TO TU EXPRESSED IN SECONDS.
!
!history  E. DAVID (LHF)
!+        02/06/08
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TEMPS
!| DAY            |-->| JOUR.
!| HOUR           |-->| HEURE EN TEMPS UNIVERSEL.
!| MINUTE         |---| 
!| MONTH          |-->| MOIS.
!| SEC            |-->| SECONDE EN TEMPS UNIVERSEL.
!| YEAR           |-->| ANNEE.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_TSLOC => TSLOC
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: MONTH,DAY,HOUR,MINUTE,SEC
      INTEGER, INTENT(INOUT)       :: YEAR
      DOUBLE PRECISION, INTENT(IN) :: AT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION T,TETA,TETA0,UT
      DOUBLE PRECISION AT1,ATR
!
      INTRINSIC ACOS,INT
!
!-----------------------------------------------------------------------
!
      ATR = AT + ( HOUR * 60.D0 + MINUTE ) * 60.D0 + SEC
      AT1 = INT ( ATR / ( 24.D0 * 3600.D0 ) ) * ( 24.D0 * 3600.D0 )
      ATR = ATR - AT1
      T = JULTIM(YEAR,MONTH,DAY,0,0,0,AT1)
!
! COMPUTES THE SIDEREAL TIME WRT GREENWICH AT 0:00 (IN HOURS)
!
      TETA0 = 6.6460656D0 + 2400.051262D0 * T + 0.00002581D0 * T**2
!
! COMPUTES THE SIDEREAL TIME WRT GREENWICH AT TU
!
      UT = ATR / 3600.D0
      TETA = TETA0 + UT*1.002737908D0
!
! COMPUTES THE LOCAL SIDEREAL TIME IN RADIANS WITHOUT TAKING LONGITUDE
! INTO ACCOUNT
!
      TSLOC = TETA * ACOS(-1.D0) / 12.D0
!
!-----------------------------------------------------------------------
!
      RETURN
      END