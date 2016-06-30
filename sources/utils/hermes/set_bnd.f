!                    ******************
                     SUBROUTINE SET_BND
!                    ******************
!
     &(FFORMAT,FID,TYPE_BND_ELT,NELEBD,NDP,IKLE,LIHBOR,LIUBOR,
     & LIVBOR,HBOR,UBOR,VBOR,CHBORD,
     & LITBOR,TBOR,ATBOR,BTBOR,IERR)
!
!***********************************************************************
! HERMES   V7P0                                               01/05/2014
!***********************************************************************
!
!brief    Writes the boundary information into the mesh file
!
!history  Y AUDOUIN (LNHE)
!+        24/03/2014
!+        V7P0
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| FFORMAT        |-->| FORMAT OF THE FILE
!| FID            |-->| FILE DESCRIPTOR
!| TYPE_BND_ELT   |-->| TYPE OF THE BOUNDARY ELEMENTS
!| NELEBD         |-->| NUMBER OF BOUNDARY ELEMENTS
!| NDP            |-->| NUMBER OF POINTS PER BOUNDARY ELEMENT
!| IKLE           |-->| CONNECTIVITY ARRAY FOR THE BOUNDARY ELEMENTS
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON U
!| LIVBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON V
!| HBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON DEPTH
!| UBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY U
!| VBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY V
!| CHBORD         |<--| FRICTION COEFFICIENT AT BOUNDARY
!| LITBOR         |-->| PHYSICAL BOUNDARY CONDITIONS FOR TRACERS
!| TBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON TRACER
!| ATBOR,BTBOR    |<--| THERMAL EXCHANGE COEFFICIENTS.
!| IERR           |<--| 0 IF NO ERROR DURING THE EXECUTION
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE UTILS_SERAFIN
      USE UTILS_MED
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8), INTENT(IN)  :: FFORMAT
      INTEGER,          INTENT(IN)  :: FID
      INTEGER,          INTENT(IN)  :: TYPE_BND_ELT
      INTEGER,          INTENT(IN)  :: NELEBD
      INTEGER,          INTENT(IN)  :: NDP
      INTEGER,          INTENT(IN)  :: IKLE(NELEBD*NDP)
      INTEGER,          INTENT(IN)  :: LIUBOR(NELEBD),LIVBOR(NELEBD)
      INTEGER,          INTENT(IN)  :: LIHBOR(NELEBD),LITBOR(NELEBD)
      DOUBLE PRECISION, INTENT(IN)  :: UBOR(NELEBD),VBOR(NELEBD)
      DOUBLE PRECISION, INTENT(IN)  :: HBOR(NELEBD),CHBORD(NELEBD)
      DOUBLE PRECISION, INTENT(IN)  :: TBOR(NELEBD),ATBOR(NELEBD)
      DOUBLE PRECISION, INTENT(IN)  :: BTBOR(NELEBD)
      INTEGER,          INTENT(OUT) :: IERR
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      SELECT CASE (FFORMAT)
        CASE ('SERAFIN ','SERAFIND')
          CALL SET_BND_SRF(FID,TYPE_BND_ELT,NELEBD,NDP,IKLE,
     &                     LIHBOR,LIUBOR,
     &                     LIVBOR,HBOR,UBOR,VBOR,CHBORD,
     &                     LITBOR,TBOR,ATBOR,BTBOR,IERR)
        CASE ('MED     ')
          CALL SET_BND_MED(FID,TYPE_BND_ELT,NELEBD,NDP,IKLE,LIHBOR,
     &                     LIUBOR,LIVBOR,LITBOR,IERR)
        CASE DEFAULT
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'GET_SET_BND : MAUVAIS FORMAT : ',FFORMAT
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'GET_SET_BND: BAD FILE FORMAT: ',FFORMAT
          ENDIF
          CALL PLANTE(1)
          STOP
      END SELECT
!
!-----------------------------------------------------------------------
!
      RETURN
      END
