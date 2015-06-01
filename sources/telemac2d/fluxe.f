!                    ****************
                     SUBROUTINE FLUXE
!                    ****************
!
     &(HJ,UJ,VJ,HI,UI,VI,XN,YN,RNORM,G,FLULOC)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMUTATION OF THE LOCAL ROE FLUX.
!
!note     JMH : RNORM NOT USED
!
!history  N. GOUTAL
!+        24/11/1997
!+        V5P2
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
!| FLULOC         |<->| LOCAL FLUX AT THE INTERFACE OF IJ
!| G              |-->| GRAVITY
!| HI             |-->| WATER DEPTH OF NODE I
!| HJ             |-->| WATER DEPTH OF NODE J
!| RNORM          |---|  ???? NOT USED
!| UI             |-->| VELOCITY X-COMPONENT OF NODE I
!| UJ             |-->| VELOCITY X-COMPONENT OF NODE J
!| VI             |-->| VELOCITY Y-COMPONENT OF NODE I
!| VJ             |-->| VELOCITY Y-COMPONENT OF NODE J
!| XN             |-->| X-COMPONENT OF THE NORMAL VECTOR
!| YN             |-->| Y-COMPONENT OF THE NORMAL VECTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(INOUT) :: FLULOC(3)
      DOUBLE PRECISION, INTENT(IN) :: G,HI,HJ,UI,UJ,VI,VJ,RNORM
      DOUBLE PRECISION, INTENT(IN) :: XN,YN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION RI,RJ,CT2,UT,VT,RLAMB0
      DOUBLE PRECISION CT,PRII,PRIJ,ALPHA
      DOUBLE PRECISION RLAMBM,PS,SA,RLAMBP,TW(3)
      DOUBLE PRECISION TR(3),T(3),CI2,CI,CJ,CJ2,RLAMBI,RLAMBJ
!
!-----------------------------------------------------------------------
!
!   --->    COMPUTES THE AVERAGES OF ROE OF U,V,H,C**2 AND C
!           ---------------------------------------------
!
      IF(HI.LE.0.D0) THEN
        RI = 0.D0
      ELSE
        RI = SQRT ( HI )
      ENDIF
      IF (HJ.LE.0.D0) THEN
        RJ = 0.D0
      ELSE
        RJ = SQRT ( HJ )
      ENDIF
!
      UT = ( RI * UI + RJ * UJ ) /(RI + RJ)
      VT = ( RI * VI + RJ * VJ ) /(RI + RJ)
      IF (HI+HJ.LE.0.D0)  THEN
        CT2= 0.D0
      ELSE
      CT2 = G*(HI+HJ)/2.D0
      ENDIF
      CT = SQRT ( CT2 )
!
!   --->  TESTS THE SIGN OF THE EIGENVALUE LAMB0 =
!           ----------------------------------------------------------
!
      RLAMB0 = UT * XN + VT * YN
!
!TBTB BEGINNING: MODIFICATION OF RLAMB0 IF RLAMB0
!C     IT IS NECESSARY TO ADD FLUXES FOR THE DUPLICATED EIGENVALUES
!C     TO BE COMPLETED BY WHOEVER WISHES TO
!C
!TBTB END
!
!---------------------------------------------------------------------
      IF  ( RLAMB0 . GE .-0.000001D0 ) THEN
!     ---- END SEGMENT ---------
!
!   --->    SMALL CALCULATIONS
!
        RLAMBM = RLAMB0 - CT
!
        PRII = G*(HI**2)/2.D0
        PRIJ = G*(HJ**2)/2.D0
        ALPHA = UI * XN + VI * YN
!
!TBTB BEGINNING : MODIFICATION OF RLAMBM IF RLAMBM
!
        IF (HI.LE.0.D0) THEN
        CI2 = 0.D0
        PRII = 0.D0
        ELSE
        CI2 =  2.D0*PRII / HI
        ENDIF
        IF (HJ.LE.0.D0) THEN
        CJ2 = 0.D0
        PRIJ = 0.D0
        ELSE
        CJ2 =  2.D0*PRIJ / HJ
        ENDIF
        CI = SQRT (CI2)
        CJ = SQRT (CJ2)
        RLAMBI = ALPHA - CI
        RLAMBJ = UJ * XN + VJ * YN - CJ
!
        IF ( RLAMBI .LT. 0.D0 .AND. RLAMBJ .GT. 0.D0) THEN
          RLAMBM = MIN(0.D0,RLAMBM) - ABS(RLAMBI - RLAMBJ) / 4.D0
        ENDIF
!     END
!
!   --->    COMPUTES FLUX 1
!
        FLULOC(1) = ALPHA * HI
        FLULOC(2) = ALPHA * HI*UI
        FLULOC(3) = ALPHA * HI*VI
!
        FLULOC (2) = FLULOC(2) + PRII * XN
        FLULOC (3) = FLULOC(3) + PRII * YN
!
!   --->    TESTS THE SIGN OF LAMBDAM
!           ----------------------------
!
        IF ( RLAMBM . LT . 0.D0 ) THEN
!       - - - - - - - - - - - - - -
!
          T (1) = 1.D0
          T (2) = UT - CT * XN
          T (3) = VT - CT * YN
!
          TR(1) = HJ-HI
          TR(2) = HJ*UJ-HI*UI
          TR(3) = HJ*VJ-HI*VI
!
          TW(1) = (UT*XN + VT*YN)*CT + CT2
          TW(2) = -XN*CT
          TW(3) = -YN*CT
!
          PS = TR(1)*TW(1)+TR(2)*TW(2)+TR(3)*TW(3)
!
!   --->    COMPUTES TOTAL LOCAL FLUX
!           --------------------------
!
          SA = PS * RLAMBM / (2.D0 * CT2 )
          FLULOC(1)= FLULOC(1)+SA*T(1)
          FLULOC(2)= FLULOC(2)+SA*T(2)
          FLULOC(3)= FLULOC(3)+SA*T(3)
!
!
        ENDIF
!           -----
!
!      TESTEST
      ELSE
!      TESTEST
!
!   --->    SMALL CALCULATIONS
!           --------------
!
        RLAMBP = RLAMB0 + CT
!
!
        ALPHA = UJ * XN + VJ* YN
!
        IF (HI.LE.0.D0) THEN
          CI2 = 0.D0
        ELSE
          CI2 = G*HI
        ENDIF
        CI = SQRT (CI2)
        IF (HJ.LE.0.D0) THEN
        CJ2 = 0.D0
        PRIJ = 0.D0
        ELSE
        CJ2 = G*HJ
        PRIJ = G*(HJ**2)/2.D0
        ENDIF
        CJ = SQRT (CJ2)
        RLAMBI = UI * XN + VI * YN + CI
        RLAMBJ = ALPHA + CJ
!
        IF ( RLAMBI .LT. 0. .AND. RLAMBJ .GT. 0.) THEN
          RLAMBP = MAX(0.D0,RLAMBP) + ABS(RLAMBI - RLAMBJ) / 4.
        ENDIF
!
!   --->    COMPUTES FLUX 1
!           ----------------
!
        FLULOC(1) = ALPHA * HJ
        FLULOC(2) = ALPHA * HJ*UJ
        FLULOC(3) = ALPHA * HJ*VJ
!
        FLULOC (2) = FLULOC(2) + PRIJ * XN
        FLULOC (3) = FLULOC(3) + PRIJ * YN
!
!   --->    TESTS THE SIGN OF LAMBDAP
!           ----------------------------
!
        IF ( RLAMBP . GT . 0.D0 ) THEN
!       - - - - - - - - - - - - - -
!
          T(1) = 1.D0
          T(2) = UT + CT * XN
          T(3) = VT + CT * YN
!
          TR(1) = HJ-HI
          TR(2) = HJ*UJ-HI*UI
          TR(3) = HJ*VJ-HI*VI
!
          TW(1) = (-UT*XN - VT*YN)*CT +CT2
          TW(2) = CT*XN
          TW(3) = CT*YN
!
          PS = TR(1)*TW(1)+TR(2)*TW(2)+TR(3)*TW(3)
!
!   --->    COMPUTES TOTAL LOCAL FLUX
!           --------------------------
!
          SA = - PS * RLAMBP / (2.D0 * CT2 )
          FLULOC(1)= FLULOC(1)+SA*T(1)
          FLULOC(2)= FLULOC(2)+SA*T(2)
          FLULOC(3)= FLULOC(3)+SA*T(3)
!
        ENDIF
!           -----
      ENDIF
!
!---------------------------------------------------------------------
!
      RETURN
      END
