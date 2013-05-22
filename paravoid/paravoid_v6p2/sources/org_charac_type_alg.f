!                    ******************************
                     SUBROUTINE ORG_CHARAC_TYPE_ALG 
!                    ****************************** 
! 
     &(ALG_CHAR)                      
!
!***********************************************************************
! PARALLEL   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    RETURNS THE MPI TYPE FOR TYPE ALG_CHAR 
!
!history  A. JOLY
!+        22/05/2013
!+        V6P3
!+   First version
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ALG_CHAR       |<-->| MPI TYPE FOR TYPE CHARAC_TYPE - ALG_CHAR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!           
      IMPLICIT NONE 
!
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
! 
      INTEGER, INTENT(INOUT) :: ALG_CHAR 
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE ORG_CHARAC_TYPE_ALG VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF ORG_CHARAC_TYPE_ALG, VOID'
!     
!----------------------------------------------------------------------
!     
      RETURN  
      END SUBROUTINE ORG_CHARAC_TYPE_ALG
