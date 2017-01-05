!                       *******************
                        SUBROUTINE FILL_LIM
!                       *******************
!
     & (NPTFR,NPTFRX,NTRAC,LIHBOR,LIUBOR,LIVBOR,LITBOR,
     &  HBOR,UBOR,VBOR,CHBORD,TBOR,ATBOR,BTBOR)
!
!***********************************************************************
! PROGICIEL : STBTEL  V5.2                 J-M JANIN   (LNH) 30 87 72 84
! ORIGINE   : TELEMAC
!***********************************************************************
!
!     FUNCTION  :  FILLS THE BOUNDARY CONDITIONS ARRAYS BASED ON THE
!                  COARSER MESH INFORMATION
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! | HBOR           | <->| PRESCRIBED DEPTH
! | LIHBOR         | -->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
! | LITBOR         | -->| TYPE OF BOUNDARY CONDITIONS ON TRACERS
! | LIUBOR         | -->| TYPE OF BOUNDARY CONDITIONS ON VELOCITY
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!----------------------------------------------------------------------
! APPELE PAR : STBTEL
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE

      INTEGER, INTENT(IN)    :: NPTFR,NPTFRX,NTRAC
      INTEGER,INTENT(INOUT) :: LIHBOR(NPTFRX),LIUBOR(NPTFRX)
      INTEGER,INTENT(INOUT) :: LIVBOR(NPTFRX)
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: LITBOR
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(NPTFRX,2),VBOR(NPTFRX,2)
      DOUBLE PRECISION, INTENT(INOUT) :: HBOR(NPTFRX)
      DOUBLE PRECISION, INTENT(INOUT) :: CHBORD(NPTFRX)
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: TBOR, ATBOR, BTBOR
!
      INTEGER I
!
      DO I=NPTFR,1,-1
!
        LIHBOR(2*I-1)=LIHBOR(I)
        LIUBOR(2*I-1)=LIUBOR(I)
        LIVBOR(2*I-1)=LIVBOR(I)
!
        HBOR(2*I-1)=HBOR(I)
        UBOR(2*I-1,1)=UBOR(I,1)
        VBOR(2*I-1,1)=VBOR(I,1)
        UBOR(2*I-1,2)=UBOR(I,2)
        VBOR(2*I-1,2)=VBOR(I,2)
!
        HBOR(2*I)=HBOR(I)
        UBOR(2*I,1)=UBOR(I,1)
        VBOR(2*I,1)=VBOR(I,1)
        UBOR(2*I,2)=UBOR(I,2)
        VBOR(2*I,2)=VBOR(I,2)
!
        IF(NTRAC.GT.0) THEN
          LITBOR%ADR(1)%P%I(2*I-1)=LITBOR%ADR(1)%P%I(I)
!
          TBOR%ADR(1)%P%R(2*I-1)=TBOR%ADR(1)%P%R(I)
          ATBOR%ADR(1)%P%R(2*I-1)=ATBOR%ADR(1)%P%R(I)
          BTBOR%ADR(1)%P%R(2*I-1)=BTBOR%ADR(1)%P%R(I)
!
          TBOR%ADR(1)%P%R(2*I)=TBOR%ADR(1)%P%R(I)
          ATBOR%ADR(1)%P%R(2*I)=ATBOR%ADR(1)%P%R(I)
          BTBOR%ADR(1)%P%R(2*I)=BTBOR%ADR(1)%P%R(I)
        ENDIF
!
      ENDDO
!
      DO I=1,NPTFR-1
!
        IF (LIHBOR(2*I-1).EQ.5.AND.LIHBOR(2*I+1).EQ.5) THEN
!
          IF ((LIUBOR(2*I-1).EQ.4.AND.LIUBOR(2*I+1).EQ.4).AND.
     &        (LIVBOR(2*I-1).EQ.4.AND.LIVBOR(2*I+1).EQ.4)) THEN
!
            LIHBOR(2*I)=5
            LIUBOR(2*I)=4
            LIVBOR(2*I)=4
!
            IF(NTRAC.GT.0) THEN
              LITBOR%ADR(1)%P%I(2*I)=4
            ENDIF
!
          ENDIF
!
        ELSEIF (LIHBOR(2*I-1).EQ.4.AND.LIHBOR(2*I+1).EQ.4) THEN
!
          IF ((LIUBOR(2*I-1).EQ.5.AND.LIUBOR(2*I+1).EQ.5).AND.
     &        (LIVBOR(2*I-1).EQ.5.AND.LIVBOR(2*I+1).EQ.5)) THEN
!
            LIHBOR(2*I)=4
            LIUBOR(2*I)=5
            LIVBOR(2*I)=5
!
            IF(NTRAC.GT.0) THEN
              LITBOR%ADR(1)%P%I(2*I)=4
            ENDIF
!
          ENDIF
!
        ELSE
!
          LIHBOR(2*I)=2
          LIUBOR(2*I)=2
          LIVBOR(2*I)=2
!
          IF(NTRAC.GT.0) THEN
            LITBOR%ADR(1)%P%I(2*I)=2
          ENDIF
!
        ENDIF
!
      ENDDO
!
      ! Checking for Last segment
      I=NPTFR
      IF (LIHBOR(2*I-1).EQ.5.AND.LIHBOR(1).EQ.5) THEN
!
        IF ((LIUBOR(2*I-1).EQ.4.AND.LIUBOR(1).EQ.4).AND.
     &      (LIVBOR(2*I-1).EQ.4.AND.LIVBOR(1).EQ.4)) THEN
!
          LIHBOR(2*I)=5
          LIUBOR(2*I)=4
          LIVBOR(2*I)=4
!
          IF(NTRAC.GT.0) THEN
            LITBOR%ADR(1)%P%I(2*I)=4
          ENDIF
!
        ENDIF
!
      ELSEIF (LIHBOR(2*I-1).EQ.4.AND.LIHBOR(1).EQ.4) THEN
!
        IF ((LIUBOR(2*I-1).EQ.5.AND.LIUBOR(1).EQ.5).AND.
     &      (LIVBOR(2*I-1).EQ.5.AND.LIVBOR(1).EQ.5)) THEN
!
          LIHBOR(2*I)=4
          LIUBOR(2*I)=5
          LIVBOR(2*I)=5
!
          IF(NTRAC.GT.0) THEN
            LITBOR%ADR(1)%P%I(2*I)=4
          ENDIF
!
        ENDIF
!
      ELSE
!
        LIHBOR(2*I)=2
        LIUBOR(2*I)=2
        LIVBOR(2*I)=2
!
        IF(NTRAC.GT.0) THEN
          LITBOR%ADR(1)%P%I(2*I)=2
        ENDIF
!
      ENDIF
!
      IF(NTRAC.GT.0) THEN
        LITBOR%ADR(1)%P%I(2*I)=2
      ENDIF
!
      END SUBROUTINE
