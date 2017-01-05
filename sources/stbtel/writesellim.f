!                    *****************
                     SUBROUTINE WRITESELLIM
!                    *****************
!
     &(NLIM,LIHBOR,LIUBOR,LIVBOR,HBOR,UBOR,VBOR,
     & CHBORD,NBOR,NPMAX,NPTFR)
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
      USE DECLARATIONS_SPECIAL
!
      IMPLICIT NONE
!
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NLIM
      INTEGER, INTENT(IN)    :: NPTFR
      INTEGER, INTENT(IN)    :: NPMAX
      INTEGER, INTENT(INOUT) :: LIUBOR(NPMAX),LIVBOR(NPMAX)
      INTEGER, INTENT(INOUT) :: LIHBOR(NPMAX)
      INTEGER, INTENT(INOUT) :: NBOR(NPMAX)
      DOUBLE PRECISION,  INTENT(INOUT) :: UBOR(NPMAX),VBOR(NPMAX)
      DOUBLE PRECISION,  INTENT(INOUT) :: HBOR(NPMAX),CHBORD(NPMAX)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I, IERR
!
      DO I=1,NPTFR
        WRITE(NLIM,*,IOSTAT=IERR)
     &         LIHBOR(I),LIUBOR(I),LIVBOR(I),
     &         HBOR(I)  ,UBOR(I)  ,VBOR(I),
     &         CHBORD(I) ,LIHBOR(I),
     &         0.0,0.0,0.0,
     &         NBOR(I),I
      ENDDO
!
      END SUBROUTINE
