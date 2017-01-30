!                         *********************
                          SUBROUTINE INIT_COMPO
!                         *********************
!
     &(NCOUCHES)
!
!***********************************************************************
! SISYPHE VERSION 6.2
!
!           Version for the FLUME CASES OF ASTRID BLOM (2003)
!              @ DELFT / DELTARES
!
!           prepared by uwe.merkel@uwe-merkel.com
!
!           Choose:  BLOMCASE = 'B1' or A1 / A2 / T5 / T10
!
!***********************************************************************
!
!     FONCTION  : DISTRIBUTION DES CLASSES
!                 % PAR COUCHE, STRATIFICATION
!     SUBROUTINE A REMPLIR PAR l'UTILISATEUR
!
!
!     FUNCTION  : INITIAL FRACTION DISTRIBUTION, STRATIFICATION,
!                 VARIATION IN SPACE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |                |    |
! |    AVAIL       |<-- | SEDIMENT FRACTION FOR EACH LAYER, CLASS AND NODE
! |                |    | AVAIL(NPOIN,10,NSICLA)
! |    ES          |<-- | THICKNESS FOR EACH LAYER AND NODE ES(NPOIN,10)
! |    NCOUCHES    |--> | NUMBER OF LAYER FOR EACH POINT
! |    NSICLA      |--> | NUMBER OF SIZE-CLASSES OF BED MATERIAL
! |                |    | (LESS THAN 10)
! |    NPOIN       |--> | NUMBER OF NODES
! |________________|____|______________________________________________
! MODE : -->(INPUT), <--(RESULT), <--> (MODIFIED INPUT)
!-----------------------------------------------------------------------
! PROGRAMME APPELANT : INIT_AVAI
! PROGRAMMES APPELES : NONE
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
      CHARACTER*2 BLOMCASE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!                                       NPOIN
      INTEGER, INTENT (INOUT)::NCOUCHES(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I, J, K, KK
!
!-----------------------------------------------------------------------
!
      DO J=1,NPOIN
!
            NCOUCHES(J) = NOMBLAY
            NLAYER%I(J) = NOMBLAY

            ES(J,1) = 0.03D0

            ES(J,2) = 0.01D0
            ES(J,3) = 0.01D0
            ES(J,4) = 0.01D0
            ES(J,5) = 0.01D0
            ES(J,6) = 0.01D0
            ES(J,7) = 0.01D0
            ES(J,8) = 0.01D0

            ES(J,9) = (ZF%R(J) -ZR%R(J)-(NOMBLAY-2)*0.01D0 - ES(J,1))
!
          BLOMCASE = 'B1'
!
          DO I = 1, NSICLA
          DO K = 1, NCOUCHES(J)
!        Nur für BLOOM B CASES
            IF (BLOMCASE.EQ.'A1') THEN
              AVAIL(J,K,I) = AVA0(I)
            ELSEIF (BLOMCASE.EQ.'A2') THEN
              AVAIL(J,K,I) = AVA0(I)
            ELSEIF (BLOMCASE.EQ.'T5') THEN
              AVAIL(J,K,I) = AVA0(I)
            ELSEIF (BLOMCASE.EQ.'T10') THEN
              AVAIL(J,K,I) = AVA0(I)
            ELSEIF ((K.EQ.1)) THEN
              AVAIL(J,K,I) = AVA0(I)
            ELSE
              AVAIL(J,K,1) = 0.998D0
              AVAIL(J,K,2) = 0.001D0
              AVAIL(J,K,3) = 0.001D0
            ENDIF

          ENDDO
        ENDDO


      ENDDO
!
!-----------------------------------------------------------------------
!  !CONVERT TO CVSM in CASE VSMTYPE = 1
!  No user edit necessary
!-----------------------------------------------------------------------
!
      IF(VSMTYPE.EQ.1) THEN
!       FOLDER FOR HIRANO PROFILE OUTPUTS
        DO KK = 1,100
        IF(CVSMOUTPUT(KK).GT.0) THEN
          CALL LAYERS_P('./VSP_',CVSMOUTPUT(KK))
        ENDIF
        ENDDO
        CALL CVSP_INIT_FROM_LAYERS
!       OUTPUT TO SELAFIN FILE
        IF (CVSM_OUT_FULL) THEN
          CALL CVSP_OUTPUT_INIT
          CALL CVSP_WRITE_PROFILE
        ENDIF
        DO KK = 1,100
        IF (CVSMOUTPUT(KK).GT.0) THEN
          CALL CVSP_P('./','V_', CVSMOUTPUT(KK))
        ENDIF
        ENDDO
      ENDIF ! END CVSM
      RETURN
      END SUBROUTINE INIT_COMPO

