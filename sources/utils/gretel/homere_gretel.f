!                    ********************
                     PROGRAM HOMERE_GRETEL
!                    ********************
!
!
!***********************************************************************
! PARALLEL   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    MERGES THE RESULTS OF A PARALLEL COMPUTATION
!+                TO WRITE A SINGLE FILE IN SELAFIN FORMAT.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      INTEGER LI
      COMMON/INFO/LNG,LU
!
!-------------------------------------------------------------------------
!
      LOGICAL IS
      CHARACTER(LEN=250) :: GEO
      CHARACTER(LEN=8)   :: GEOFORMAT,RESFORMAT
      CHARACTER(LEN=250) :: RES
      INTEGER            :: NPROC, NPLAN
      
!
!-------------------------------------------------------------------------
!
      LI=5
      LU=6
      LNG=2
!
      WRITE(LU,*) 'I AM GRETEL FROM BAW HAMBURG'
      WRITE(LU,*) 'REINCARNATED BY HOLGER WEILBEER'
      WRITE(LU,*) 'ON 20TH FEBRUARY 2003'
      WRITE(LU,*)
!
      WRITE (LU, ADVANCE='NO',
     &    FMT='(/,'' GLOBAL GEOMETRY FILE: '')')
      READ(LI,*) GEO
      WRITE(LU,*) GEO
!
      WRITE (LU,ADVANCE='NO',
     &    FMT='(/,'' FORMAT OF THE GEOMERTY FILE: '')')
      READ (LI,*) GEOFORMAT
      WRITE (LU,*) GEOFORMAT
!
      WRITE(LU, ADVANCE='NO', FMT='(/,'' RESULT FILE: '')')
      READ(LI,*) RES
      WRITE(LU,*) RES
!
      WRITE (LU,ADVANCE='NO',FMT='(/,'' FORMAT OF THE RESULT FILE: '')')
      READ (LI,*) RESFORMAT
      WRITE (LU,*) RESFORMAT
!
      WRITE (LU,ADVANCE='NO',FMT='(/,'' NUMBER OF PROCESSORS: '')')
      READ (LI,*) NPROC
      WRITE (LU,*) NPROC
!
      WRITE (LU,ADVANCE='NO',FMT='(/,'' NUMBER OF PLANES: '')')
      READ (LI,*) NPLAN
      WRITE (LU,*) NPLAN
!
      INQUIRE (FILE=GEO,EXIST=IS)
      IF(.NOT.IS) THEN
        WRITE (LU,*) 'FILE DOES NOT EXIST: ', GEO
        CALL PLANTE(1)
        STOP
      ENDIF
!
      CALL GRETEL_AUTOP(GEO,GEOFORMAT,RES,RESFORMAT,NPROC,NPLAN)
!
      STOP 0
      END PROGRAM
