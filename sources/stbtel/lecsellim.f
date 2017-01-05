!                    *****************
                     SUBROUTINE LECSELLIM
!                    *****************
!
     &(NLIM,LIHBOR,LIUBOR,LIVBOR,HBOR,UBOR,VBOR,
     & CHBORD,NBOR,NPTFR,NPTFR2)
!
!***********************************************************************
! STBTEL
!***********************************************************************
!
!brief    READS THE BOUNDARY CONDITIONS FILE AND
!+                STORES IN ARRAYS THE DATA READ.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NLIM           |-->| LOGICAL UNIT OF BOUNDARY CONDITIONS FILE
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON U
!| LIVBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON V
!| HBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON DEPTH
!| UBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY U
!| VBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY V
!| CHBORD         |<--| FRICTION COEFFICIENT AT BOUNDARY
!| NBORD          |<--| BOUNDARY NUMBERING
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NLIM
      INTEGER, INTENT(IN)    :: NPTFR
      INTEGER, INTENT(INOUT) :: LIUBOR(NPTFR),LIVBOR(NPTFR)
      INTEGER, INTENT(INOUT) :: LIHBOR(NPTFR)
      INTEGER, INTENT(INOUT) :: NBOR(NPTFR)
      INTEGER, INTENT(OUT) :: NPTFR2
      DOUBLE PRECISION,  INTENT(INOUT) :: UBOR(NPTFR,2),VBOR(NPTFR,2)
      DOUBLE PRECISION,  INTENT(INOUT) :: HBOR(NPTFR),CHBORD(NPTFR)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER MY_NPTFR,IPTFR
      INTEGER :: IDUM, I, IERR
      DOUBLE PRECISION DDUM
!
      MY_NPTFR = 0

      REWIND(NLIM)
      DO WHILE(.TRUE.)
      ! COUNT THE NUMBER OF LINE IN THE FILE
      READ(NLIM,*,END=666) IDUM, IDUM,IDUM,
     &           DDUM, DDUM, DDUM,
     &           DDUM,IDUM,DDUM,DDUM,DDUM,
     &           IDUM,IDUM
      MY_NPTFR = MY_NPTFR + 1
      ENDDO
      ! REWIND OF THE FILE
666   CLOSE(NLIM,IOSTAT=IERR)
      CALL CHECK_CALL(IERR,'CLOSE STBND2')
      OPEN(NLIM,IOSTAT=IERR,FILE='STBND2',FORM='FORMATTED')
      CALL CHECK_CALL(IERR,'OPEN STBND2')

      REWIND(NLIM)
      DO I=1,MY_NPTFR
        READ(NLIM,*,IOSTAT=IERR)
     &         LIHBOR(I),LIUBOR(I),LIVBOR(I),
     &         HBOR(I)  ,UBOR(I,1)  ,VBOR(I,1),
     &         CHBORD(I) ,IDUM,
     &         DDUM,DDUM,DDUM,
     &         NBOR(I),IPTFR
        CALL CHECK_CALL(IERR,'READING NBND2')
        UBOR(I,2) = UBOR(I,1)
        VBOR(I,2) = VBOR(I,1)
        IF(IPTFR.NE.I) THEN
          WRITE(LU,*) 'WARNING : THERE IS AN ERROR AT LINE ',I,
     &                'OF THE BOUNDARY FILES I'
          WRITE(LU,*) 'THE LAST COLUMN NUMBER',
     &                ' SHOULD BE ',I,'AND IT IS ',IPTFR
        ENDIF
      ENDDO
      NPTFR2 = MY_NPTFR

      END SUBROUTINE LECSELLIM
