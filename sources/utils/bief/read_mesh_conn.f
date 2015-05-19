!                    *************************
                     SUBROUTINE READ_MESH_CONN
!                    *************************
!
     &(FFORMAT,NFIC,NPOIN,TYP_ELEM,NELEM,NDP,TYP_BND_ELEM,NELEBD,IKLE,
     &  IPOBO)
!
!***********************************************************************
! HERMES   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    READS THE CONNECTIVITY TABLE AND NUMBERING FOR THE
!+        BOUNDARY NODES.
!
!history  J-M HERVOUET (LNH)     
!+        29/04/04
!+        V5P5
!+   First version.
!
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NFIC           |-->| LOGICAL UNIT FOR GEOMETRY FILE
!| IB             |<--| 10 INTEGERS, SEE SELAFIN FILE STANDARD
!| NDP            |<--| NUMBER OF NODES PER ELEMENT
!| NELEBD         |<--| NUMBER OF BOUNDARY ELEMENTS
!| NELEM          |<--| NUMBER OF ELEMENTS IN THE MESH
!| NPOIN          |<--| NUMBER OF POINTS IN THE MESH
!| NPTFR          |<--| NUMBER OF BOUNDARY POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, ONLY: NCSIZE
      USE INTERFACE_HERMES
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=8), INTENT(IN) :: FFORMAT
      INTEGER, INTENT(IN)    :: NFIC,NPOIN,NELEM,NDP,TYP_ELEM
      INTEGER, INTENT(IN)    :: TYP_BND_ELEM,NELEBD
      INTEGER, INTENT(INOUT) :: IKLE(NDP*NELEM)
      INTEGER, INTENT(INOUT) :: IPOBO(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IERR
!
!-----------------------------------------------------------------------
!
      CALL GET_MESH_CONNECTIVITY(FFORMAT,NFIC,TYP_ELEM,IKLE,NELEM,
     &                           NDP,IERR)
      CALL CHECK_CALL(IERR,'READ_MESH_CONN:GET_MESH_CONNECTIVITY')
!
      CALL GET_BND_IPOBO(FFORMAT,NFIC,NPOIN,NELEBD,TYP_BND_ELEM,IPOBO,
     &                   IERR)
      CALL CHECK_CALL(IERR,'READ_MESH_CONN:GET_BND_IPOBO')
!
!-----------------------------------------------------------------------
!
      RETURN
      END