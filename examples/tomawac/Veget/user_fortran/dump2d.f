!                    *****************
                     SUBROUTINE DUMP2D
!                    *****************
!
     &( LT , XF1 , NP1 )
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
!| LT             |-->| NUMBER OF THE TIME STEP CURRENTLY SOLVED
!| NP1            |-->| NPOIN2.NPLAN.NF
!| XF1            |-->| VARIANCE DENSITY DIRECTIONAL SPECTRUM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TOMAWAC
      USE INTERFACE_TOMAWAC, EX_DUMP2D => DUMP2D
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)          :: LT,NP1
      DOUBLE PRECISION, INTENT(IN) :: XF1(NP1)
!VB_modif
      DOUBLE PRECISION AUX1(NPOIN2),AUX2(NPOIN2)
      DOUBLE PRECISION AUX3(NPOIN2)
!      DOUBLE PRECISION TAUX1(NPOIN2)
!VB_modif
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
     &( STRA51%R, STRA52%R, STRA53%R, STRA54%R, STRA55%R,
     &  SXK%R   , XF1     , SCG%R   , SDEPTH%R,
!       WORK TABLE HERE
     &  STSDER%R,STRA36%R, STRA37%R, STRA38%R, STRA39%R)
      ENDIF
!
!     ------------------------------- DIRECTIONAL SPREADING
!
      IF(SORLEO(4)) THEN
        CALL SPREAD
     &( STRA31%R,XF1,SCOSTE%R,SSINTE%R,NPLAN ,
     &  SFR%R,SDFR%R,NF,NPOIN2,TAILF,
     &  STRA34%R,STRA35%R,STRA36%R,STRA37%R,STRA38%R,
     &  STRA39%R)
      ENDIF
!
!     ------------------------------- MEAN DIRECTION
!
      IF(SORLEO(3)) THEN
        CALL TETMOY
     &( STRA32%R, XF1 , SCOSTE%R, SSINTE%R, NPLAN , FREQ ,
     &  SDFR%R  , NF  , NPOIN2  , TAILF, STRA36%R ,
     &  STRA37%R, STRA38%R, STRA39%R )
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
     &( STRA33%R, XF1   , SFR%R   , SDFR%R  , TAILF , NF  ,
     &  NPLAN      , NPOIN2, STRA38%R, STRA39%R)
        IF(SORLEO(28)) THEN
          DO IP=1,NPOIN2
            TRA61(IP)=1.D0/MIN(MAX(TRA33(IP),FMIN),FMAX)
          ENDDO
        ENDIF
      ENDIF
!
!     ------------------------------- MEAN FREQUENCY FM01
!
      IF(SORLEO(19).OR.SORLEO(29)) THEN
        CALL FREM01
     &( STRA34%R, XF1   , SFR%R   , SDFR%R  , TAILF , NF  ,
     &  NPLAN      , NPOIN2, STRA38%R, STRA39%R)
        IF (SORLEO(29)) THEN
          DO IP=1,NPOIN2
            TRA62(IP)=1.D0/MIN(MAX(TRA34(IP),FMIN),FMAX)
          ENDDO
        ENDIF
      ENDIF
!
!     ------------------------------- MEAN FREQUENCY FM02
!
      IF (SORLEO(20).OR.SORLEO(30)) THEN
        CALL FREM02
     &( STRA35%R, XF1   , SFR%R   , SDFR%R  , TAILF , NF  ,
     &  NPLAN      , NPOIN2, STRA38%R, STRA39%R)
        IF (SORLEO(30)) THEN
          DO IP=1,NPOIN2
            TRA63(IP)=1.D0/MIN(MAX(TRA35(IP),FMIN),FMAX)
          ENDDO
        ENDIF
      ENDIF
!
!     ------------------------------- DISCRETE PEAK FREQUENCY
!
      IF (SORLEO(21).OR.SORLEO(31)) THEN
        CALL FREPIC
     &( STRA36%R, XF1   , SFR%R , NF   , NPLAN , NPOIN2,
     &  STRA38%R, STRA39%R      )
        IF (SORLEO(31)) THEN
          DO IP=1,NPOIN2
            TRA64(IP)=1.D0/MIN(MAX(TRA36(IP),FMIN),FMAX)
          ENDDO
        ENDIF
      ENDIF
!
!     ------------------------------- PEAK FREQUENCY (READ 5TH ORDER)
!
      IF (SORLEO(22).OR.SORLEO(32)) THEN
        CALL FPREAD
     &( STRA56%R, XF1   , SFR%R, SDFR%R  , NF   , NPLAN ,
     &  NPOIN2     , 5.D0  , TAILF   , STRA38%R, STRA39%R  )
        IF (SORLEO(32)) THEN
          DO IP=1,NPOIN2
            TRA65(IP)=1.D0/MIN(MAX(TRA56(IP),FMIN),FMAX)
          ENDDO
        ENDIF
      ENDIF
!
!     ------------------------------- PEAK FREQUENCY (READ 8TH ORDER)
!
      IF (SORLEO(23).OR.SORLEO(33)) THEN
        CALL FPREAD
     &( STRA57%R, XF1   , SFR%R , SDFR%R  , NF   , NPLAN ,
     &  NPOIN2     , 8.D0  , TAILF    , STRA38%R, STRA39%R  )
        IF (SORLEO(33)) THEN
          DO IP=1,NPOIN2
            TRA66(IP)=1.D0/MIN(MAX(TRA57(IP),FMIN),FMAX)
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
              TRA58(IP)=TRA42(IP)**2/U10
            ELSE
              TRA58(IP)=0.D0
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!       ------------------------------- BOTTOM SPEED
!
      IF(.NOT.PROINF) THEN
        IF(SORLEO(16)) THEN
          CALL VITFON(STRA59%R,XF1,SXK%R,SDEPTH%R,SDFR%R,NF,
     &                NPOIN2,NPLAN,STRA39%R)
        ENDIF
      ENDIF
!
!     ------------------------------- VARIANCE
!
      IF(SORLEO(1).OR.SORLEO(2)) THEN
        CALL TOTNRJ
     &( STRA37%R , XF1   , SFR%R  , SDFR%R , TAILF ,
     &  NF  , NPLAN , NPOIN2)
!
!     ------------------------------- SIGNIFICANT WAVE HEIGHT
!
        IF(SORLEO(2)) THEN
          DO IP=1,NPOIN2
            TRA38(IP)=4.D0*SQRT(TRA37(IP))
          ENDDO
        ENDIF
      ENDIF
!
!     ------------------------------- POWER PER UNIT LENGTH
!
      IF(SORLEO(34)) THEN
        CALL WPOWER(STRA60%R,XF1,SFR%R,SDFR%R,SCG%R,TAILF,NF,
     &              NPLAN,NPOIN2,ROEAU)
      ENDIF
!     ------------------------------- KMOYEN AND QMOUT1
!
      IF(SORLEO(17)) THEN
        CALL KMOYEN(SPRIVE%R,SXK%R,SF%R,SFR%R,SDFR%R,TAILF,NF,NPLAN,
     &              NPOIN2,AUX1,AUX2,AUX3)
!        CALL QMOUT1(SPRIVE2%R,TSDER,SF%R,SXK%R,STRA37%R,SFR%R,
!     &              STRA33%R,SPRIVE1%R,PROINF,CMOUT1,CMOUT2,NF,NPLAN,
!     &              NPOIN2,TAUX1,BETA,SDEPTH%R)
      ENDIF

!
!-----------------------------------------------------------------------
!
      RETURN
      END

