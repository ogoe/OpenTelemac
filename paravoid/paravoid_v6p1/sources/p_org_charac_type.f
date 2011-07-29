!                    ****************************
                     SUBROUTINE P_ORG_CHARAC_TYPE 
!                    **************************** 
! 
     &(NOMB,TRACE,CHARACTERISTIC)                      
!
!***********************************************************************
! PARAVOID   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MPI TYPE FOR TYPE CHARAC_TYPE - CHARACTERISTICS /
!+        USED BY TOMAWAC ONLY
!
!history  C. DENIS
!+        01/07/2011
!+        V6P1
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NOMB           |<---| NUMBER OF VARIABLES 
!| TRACE          |<---| IF .TRUE. TRACE EXECUTION
!| CHARACTERISTIC |--->| DATATYPE FOR CHARACTERISTIC 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!         
      IMPLICIT NONE 
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU 
!       
      INTEGER, INTENT(INOUT) :: NOMB 
      INTEGER, INTENT(INOUT)  :: CHARACTERISTIC 
      LOGICAL, INTENT(IN) ::TRACE 
!
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE P_ORG_CHARAC_TYPE VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF P_ORG_CHARAC_TYPE VOID VERSION'       
!     
!----------------------------------------------------------------------
!     
      RETURN  
      END 
 
 
