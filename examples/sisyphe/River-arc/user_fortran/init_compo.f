!                         *********************
                          SUBROUTINE INIT_COMPO
!                         *********************
!
     &(NCOUCHES)
!
!***********************************************************************
! SISYPHE VERSION 5.3
!                             Matthieu GONZALES DE LINARES 2002
!
!
! COPYRIGHT EDF-BAW-IFH
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
! |    AVAIL(10,NSICLA,NPOIN)
! |    ES          |<-- |  THICKNESS FOR EACH LAYER AND NODE
! |    ES(10,NPOIN)
! |    NCOUCHES     |--> |  NUMBER OF LAYER FOR EACH POINT
! |    NSICLA      |--> |  NUMBER OF SIZE-CLASSES OF BED MATERIAL
!                           (LESS THAN 10)
! |    NPOIN       |--> |  NUMBER OF NODES
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
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!                                       NPOIN
      INTEGER, INTENT (INOUT)::NCOUCHES(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I , J
!
!-----------------------------------------------------------------------
!
      DO J=1,NPOIN
!
!       BY DEFAULT : UNIFORM BED COMPOSITION
!
          NCOUCHES(J) = 1
!
      IF (ZF%R(J)>454.5D0) THEN
!
      AVAIL(J,1,1) = 0.065
      AVAIL(J,1,2) = 0.045
      AVAIL(J,1,3) = 0.145
      AVAIL(J,1,4) = 0.285
      AVAIL(J,1,5) = 0.345
      AVAIL(J,1,6) = 0.115
!
      ELSEIF(ZF%R(J)<453.8D0) THEN
!
      AVAIL(J,1,1) = 0.065D0
      AVAIL(J,1,2) = 0.
      AVAIL(J,1,3) = 0.
      AVAIL(J,1,4) = 0.
      AVAIL(J,1,5) = 0.
      AVAIL(J,1,6) = 0.935D0
!
      ELSEIF(ZF%R(J)<454.5D0.AND.ZF%R(J)>453.8D0) THEN
!      DGRA = DMAX + (zf%R(J)-453.8D0)*(0.022D0-DMAX)/(454.5D0-453.8D0)
!
      AVAIL(J,1,1) = 0.065
      AVAIL(J,1,2) = 0.045*(ZF%R(J)-453.8D0)/(454.5D0-453.8D0)
      AVAIL(J,1,3) = 0.145*(ZF%R(J)-453.8D0)/(454.5D0-453.8D0)
      AVAIL(J,1,4) = 0.285*(ZF%R(J)-453.8D0)/(454.5D0-453.8D0)
      AVAIL(J,1,5) = 0.345*(ZF%R(J)-453.8D0)/(454.5D0-453.8D0)
      AVAIL(J,1,6) = 1.-(AVAIL(J,1,1)+AVAIL(J,1,2)+AVAIL(J,1,3)
     &                    +AVAIL(J,1,4)+AVAIL(J,1,5))
!
      ENDIF
      ENDDO
!  TO BE FILLED BY THE USER
!      NCOUCHES(J) = 10
!        ES(1,J) = 1
!      ES(2,J) = 1
!      ES(3,J) = 1
!        ES(4,J) = 1
!      ES(5,J) = 1
!      ES(6,J) = 1
!        ES(7,J) = 1
!      ES(8,J) = 1
!      ES(9,J) = 1
!         DO I = 1, NSICLA
!          DO K = 1, NCOUCHES(J)
!          AVAIL(J,K,I) = AVA0(I)
!       ENDDO
!        ENDDO
!
!
      RETURN
      END

