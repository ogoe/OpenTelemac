!                    *****************
                     SUBROUTINE DUMP2D
!                    *****************
!
     &(  XF1 , NP1 )
!
!***********************************************************************
! TOMAWAC   V6P3                                   15/06/2011
!***********************************************************************
!
!brief    WRITES OUT WAVE, WIND, CURRENT, BATHYMETRY, ...
!+                VARIABLES AT EACH NODE OF THE MESH.
!+                VARIES SPATIALLY IN 2D (BINARY SELAFIN FORMAT).
!
!warning  STSDER used as work array here.
!
!
!history  F. MARCOS
!+        01/02/95
!+        V1P0
!+   CREATED
!
!history  M. BENOIT
!+        04/07/96
!+        V1P2
!+   MODIFIED
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
!history  G.MATTAROLO (EDF - LNHE)
!+        15/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUETO (EDF R&D, LNHE)
!+        26/02/2013
!+        V6P3
!+   Use of work arrays optimised.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NP1            |-->| NPOIN2.NPLAN.NF
!| XF1            |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC
      USE INTERFACE_TOMAWAC, EX_DUMP2D => DUMP2D
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: NP1
      DOUBLE PRECISION, INTENT(IN) :: XF1(NP1)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          IP
      DOUBLE PRECISION U10   , FMIN  , FMAX
!
      FMIN=FREQ(1)
      FMAX=FREQ(NF)
!
!=====C====================================
!     C COMPUTES THE SELECTED VARIABLES
!=====C====================================
! THE ORDER IN WHICH THE VARIABLES ARE COMPUTED DOES NOT CORRESPOND TO THAT OF
! THE GRAPHICAL OUTPUT IN AN EFFORT TO LIMIT THE NUMBER OF WORKING ARRAYS.
!
!     ------------------------------- RADIATION STRESSES
!
      IF(.NOT.PROINF) THEN
        IF(SORLEO(11).OR.SORLEO(12).OR.SORLEO(13).OR.
     &     SORLEO(14).OR.SORLEO(15) ) CALL RADIAT
     &        ( FX, FY, XK, XF1, CG ,
!         SXX, SXY, SYY, XK,  CG , DEPTH,
!       WORK TABLE HERE
     &  TSDER,TRA36, TRA37, TRA38, TRA39)
      ENDIF
!
!     ------------------------------- DIRECTIONAL DIR_SPREADING
!
      IF(SORLEO(4)) THEN
        CALL DIR_SPREAD
     &( TRA31,XF1,COSTET,SINTET,NPLAN ,
     &        FREQ,DFREQ,NF,NPOIN2,TAILF)
      ENDIF
!
!     ------------------------------- MEAN DIRECTION
!
      IF(SORLEO(3)) THEN
        CALL TETMOY
     &( TRA32, XF1 , COSTET, SINTET, NPLAN , FREQ ,
     &  DFREQ  , NF  , NPOIN2  , TAILF)
        IF(TRIGO) THEN
          DO IP=1,NPOIN2
            TRA32(IP)=(PISUR2-TRA32(IP))*GRADEG
          ENDDO
        ELSE
          DO IP=1,NPOIN2
            TRA32(IP)=TRA32(IP)*GRADEG
          ENDDO
        ENDIF
      ENDIF
!
!     ------------------------------- MEAN FREQUENCY FMOY
!
      IF(SORLEO(18).OR.SORLEO(28)) THEN
        CALL FREMOY
     &( TRA33, XF1   , FREQ   , DFREQ  , TAILF , NF  ,
     &  NPLAN      , NPOIN2)
        IF(SORLEO(28)) THEN
          DO IP=1,NPOIN2
            PTMOY(IP)=1.D0/MIN(MAX(TRA33(IP),FMIN),FMAX)
          ENDDO
        ENDIF
      ENDIF
!
!     ------------------------------- MEAN FREQUENCY FM01
!
      IF(SORLEO(19).OR.SORLEO(29)) THEN
        CALL FREM01
     &( TRA34, XF1   , FREQ   , DFREQ  , TAILF , NF  ,
     &  NPLAN      , NPOIN2)
        IF (SORLEO(29)) THEN
          DO IP=1,NPOIN2
            PTM01(IP)=1.D0/MIN(MAX(TRA34(IP),FMIN),FMAX)
          ENDDO
        ENDIF
      ENDIF
!
!     ------------------------------- MEAN FREQUENCY FM02
!
      IF (SORLEO(20).OR.SORLEO(30)) THEN
        CALL FREM02
     &( TRA35, XF1   , FREQ   , DFREQ  , TAILF , NF  ,
     &  NPLAN      , NPOIN2)
        IF (SORLEO(30)) THEN
          DO IP=1,NPOIN2
            PTM02(IP)=1.D0/MIN(MAX(TRA35(IP),FMIN),FMAX)
          ENDDO
        ENDIF
      ENDIF
!
!     ------------------------------- DISCRETE PEAK FREQUENCY
!
      IF (SORLEO(21).OR.SORLEO(31)) THEN
        CALL FREPIC
     &( TRA36, XF1   , FREQ , NF   , NPLAN , NPOIN2)
        IF (SORLEO(31)) THEN
          DO IP=1,NPOIN2
            PPTPD(IP)=1.D0/MIN(MAX(TRA36(IP),FMIN),FMAX)
          ENDDO
        ENDIF
      ENDIF
!
!     ------------------------------- PEAK FREQUENCY (READ 5TH ORDER)
!
      IF (SORLEO(22).OR.SORLEO(32)) THEN
        CALL FPREAD
     &( FREA5, XF1   , FREQ, DFREQ  , NF   , NPLAN ,
     &  NPOIN2     , 5.D0  , TAILF    )
        IF (SORLEO(32)) THEN
          DO IP=1,NPOIN2
            PREA5(IP)=1.D0/MIN(MAX(FREA5(IP),FMIN),FMAX)
          ENDDO
        ENDIF
      ENDIF
!
!     ------------------------------- PEAK FREQUENCY (READ 8TH ORDER)
!
      IF (SORLEO(23).OR.SORLEO(33)) THEN
        CALL FPREAD
     &( FREA8, XF1   , FREQ , DFREQ  , NF   , NPLAN ,
     &  NPOIN2     , 8.D0  , TAILF  )
        IF (SORLEO(33)) THEN
          DO IP=1,NPOIN2
            PREA8(IP)=1.D0/MIN(MAX(FREA8(IP),FMIN),FMAX)
          ENDDO
        ENDIF
      ENDIF
!
      IF(VENT) THEN
!
!       ------------------------------- DRAG COEFFICIENT
!
        IF(SORLEO(25)) THEN
          DO IP=1,NPOIN2
            U10=UV(IP)**2+VV(IP)**2
            IF (U10.GT.1.D-6) THEN
              CDRA2(IP)=USOLD(IP)**2/U10
            ELSE
              CDRA2(IP)=0.D0
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!     ------------------------------- BOTTOM SPEED
!
      IF(.NOT.PROINF) THEN
        IF(SORLEO(16)) THEN
          CALL VITFON(VIFOND,XF1,XK,DEPTH,DFREQ,NF,
     &                NPOIN2,NPLAN)
        ENDIF
      ENDIF
!
!     ------------------------------- VARIANCE
!
      IF(SORLEO(1).OR.SORLEO(2)) THEN
        CALL TOTNRJ
     &( TRA37, XF1   , FREQ  , DFREQ , TAILF ,
     &  NF  , NPLAN , NPOIN2)
!
!     ------------------------------- SIGNIFICANT WAVE HEIGHT
!
        IF(SORLEO(2)) THEN
          DO IP=1,NPOIN2
            IF (TRA37(IP).GE.0) THEN
              TRA38(IP)=4.D0*SQRT(TRA37(IP))
            ELSE
              TRA38(IP)=0
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!     ------------------------------- POWER PER UNIT LENGTH
!
      IF(SORLEO(34)) THEN
        CALL WPOWER(POWER,XF1,DFREQ,CG,TAILF,NF,
     &              NPLAN,NPOIN2,ROEAU)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
