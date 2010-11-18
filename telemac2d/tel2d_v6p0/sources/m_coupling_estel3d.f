C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @code
!>-----------------------------------------------------------------------
!> THIS FILE IS PART OF TELEMAC-2D V5P7
!>-----------------------------------------------------------------------
!> WRITTEN BY JP RENAUD
!> SET OF STRUCTURES AND SUBROUTINES THAT LETS TELEMAC-2D AND ESTEL-3D
!> INTERACT BY READING/WRITING TWO ARRAYS WHICH LIVE IN THIS MODULE:
!>
!>       - ESTEL-3D WRITES FLUX VALUES
!>       - TELEMAC-2D READS THE FLUX VALUES
!>       - TELEMAC-2D WRITES THE DEPTH VALUES
!>       - ESTEL-3D READS THE DEPTH VALUES
!>       - ETC...
!>
!> AS THE ARRAYS ARE PRIVATE, THEY ARE ACCESSED BY PUBLIC METHODS:
!> DEPTH_FILL SAVES THE DEPTHS FROM TELEMAC-2D AND DEPTH_GET ALLOWS
!> ESTEL-3D TO RECOVER THE INFORMATION.
!>-----------------------------------------------------------------------
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Internal(s)
!>    </th><td> DEJA, DEPTH_FROM_T2D, DOINFILTRATION, FLUX_FROM_ESTEL3D, NPOIN2D
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Unknown(s)
!>    </th><td> DEPTH_FROM_T2D, DEPTH_FROM_T2D, DEPTH_FROM_T2D, DEPTH_FROM_T2D, DOINFILTRATION, DOINFILTRATION, FLUX_FROM_ESTEL3D, FLUX_FROM_ESTEL3D, FLUX_FROM_ESTEL3D, FLUX_FROM_ESTEL3D, NPOIN2D
!>   </td></tr>
!>     </table>

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>     </table>
C
C#######################################################################
C
      MODULE M_COUPLING_ESTEL3D
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE

      PRIVATE

      PUBLIC :: INFILTRATION_INIT
      PUBLIC :: INFILTRATION_FINISH
      PUBLIC :: INFILTRATION_FILL
      PUBLIC :: INFILTRATION_GET
      PUBLIC :: DEPTH_FILL
      PUBLIC :: DEPTH_GET

      INTEGER :: NPOIN2D
      LOGICAL :: DOINFILTRATION

      DOUBLE PRECISION, ALLOCATABLE :: FLUX_FROM_ESTEL3D(:)
      DOUBLE PRECISION, ALLOCATABLE :: DEPTH_FROM_T2D(:)

C-----------------------------------------------------------------------
      CONTAINS
C-----------------------------------------------------------------------

      SUBROUTINE INFILTRATION_INIT(NPOIN,ACTIVATE)
C-----------------------------------------------------------------------
C ALLOCATES THE COUPLING ARRAYS IF REQUIRED AND FILLS THEM UP WITH ZEROS
C-----------------------------------------------------------------------
      IMPLICIT NONE
C-----------------------------------------------------------------------
      INTEGER, INTENT(IN) :: NPOIN
      LOGICAL, INTENT(IN) :: ACTIVATE
C-----------------------------------------------------------------------
C
      LOGICAL DEJA
      DATA DEJA/.FALSE./
C
C-----------------------------------------------------------------------
C
      NPOIN2D = 1

      IF(ACTIVATE) THEN
        NPOIN2D        = NPOIN
        DOINFILTRATION = .TRUE.
      ENDIF

      IF(.NOT.DEJA) THEN
        ALLOCATE( FLUX_FROM_ESTEL3D( NPOIN2D ) )
        ALLOCATE( DEPTH_FROM_T2D( NPOIN2D ) )
        DEJA=.TRUE.
      ENDIF

      FLUX_FROM_ESTEL3D(:) = 0.D0
      DEPTH_FROM_T2D(:)    = 0.D0
C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE INFILTRATION_INIT

      SUBROUTINE INFILTRATION_FINISH()
C-----------------------------------------------------------------------
C DE-ALLOCATES THE COUPLING ARRAYS
C-----------------------------------------------------------------------
      IMPLICIT NONE
C-----------------------------------------------------------------------

      DEALLOCATE( FLUX_FROM_ESTEL3D )
      DEALLOCATE( DEPTH_FROM_T2D )

C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE INFILTRATION_FINISH

      SUBROUTINE INFILTRATION_FILL(ARRAY1,ARRAY2,COEFF)
C-----------------------------------------------------------------------
C FILLS THE ARRAY FLUX_FROM_ESTEL3D WITH THE VALUES FROM THE ARGUMENTS
C
C THIS SUBROUTINE IS CALLED FROM ESTEL-3D
C-----------------------------------------------------------------------
      IMPLICIT NONE
C-----------------------------------------------------------------------
      DOUBLE PRECISION, INTENT(IN) :: ARRAY1(NPOIN2D)
      DOUBLE PRECISION, INTENT(IN) :: ARRAY2(NPOIN2D)
      DOUBLE PRECISION, INTENT(IN) :: COEFF
C-----------------------------------------------------------------------
              FLUX_FROM_ESTEL3D(:) = COEFF       * ARRAY1(:)
     &                             + (1 - COEFF) * ARRAY2(:)
C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE INFILTRATION_FILL

      SUBROUTINE DEPTH_FILL(ARRAY_FROM_T2D)
C-----------------------------------------------------------------------
C FILLS THE ARRAY DEPTH_FROM_T2D WITH THE VALUES FROM THE ARGUMENT
C
C THIS SUBROUTINE IS CALLED FROM TELEMAC-2D
C-----------------------------------------------------------------------
      IMPLICIT NONE
C-----------------------------------------------------------------------
      DOUBLE PRECISION, INTENT(INOUT) :: ARRAY_FROM_T2D(NPOIN2D)
C-----------------------------------------------------------------------
      DEPTH_FROM_T2D(:) = ARRAY_FROM_T2D(:)
C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE DEPTH_FILL

      SUBROUTINE DEPTH_GET(ARRAY_FROM_ESTEL3D)
C-----------------------------------------------------------------------
C BASICALLY READS THE ARRAY DEPTH_FROM_T2D SO THAT ESTEL-3D CAN
C  USE IT FOR ITS BOUNDARY CONDITIONS
C
C THIS SUBROUTINE IS CALLED FROM ESTEL-3D
C-----------------------------------------------------------------------
      IMPLICIT NONE
C-----------------------------------------------------------------------
      DOUBLE PRECISION, INTENT(INOUT) :: ARRAY_FROM_ESTEL3D(NPOIN2D)
C-----------------------------------------------------------------------
      ARRAY_FROM_ESTEL3D(:) = DEPTH_FROM_T2D(:)
C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE DEPTH_GET

      SUBROUTINE INFILTRATION_GET(SMH,UNSV2D,YASMH)
C-----------------------------------------------------------------------
C ADDS THE INFILTRATION TERM TO THE SOURCE TERM SMH AND SWITCHES YASMH TO
C TRUE. NOTE THAT A MASS VECTOR IS REQUIRED AS ARGUMENT BECAUSE THE FLUX
C CALCULATED WITHIN ESTEL-3D IS MULTIPLIED BY A MASS VECTOR AND THE
C DIVISION IS EASIER TO DO FROM WITHIN TELEMAC-2D FOR MESH REASONS.
C
C THIS SUBROUTINE IS CALLED FROM TELEMAC-2D
C-----------------------------------------------------------------------
      IMPLICIT NONE
C-----------------------------------------------------------------------
      DOUBLE PRECISION, INTENT(INOUT) :: SMH(NPOIN2D)
      DOUBLE PRECISION, INTENT(IN)    :: UNSV2D(NPOIN2D)
      LOGICAL, INTENT(INOUT)          :: YASMH
C-----------------------------------------------------------------------
      IF(DOINFILTRATION) THEN
        YASMH  = .TRUE.
        SMH(:) = SMH(:) + FLUX_FROM_ESTEL3D(:)*UNSV2D(:)
      ENDIF
C-----------------------------------------------------------------------
      RETURN
      END SUBROUTINE INFILTRATION_GET

C-----------------------------------------------------------------------
      END MODULE M_COUPLING_ESTEL3D
C
C#######################################################################
C