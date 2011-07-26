C                    ***************************
                     SUBROUTINE ORG_CHARAC_TYPE1 
C                    *************************** 
C 
     *(NOMB,TRACE,CHARACTERISTIC)                      
C 
C*********************************************************************** 
C 
C  PARAVOID VERSION 6.1   16/01/2010    C. DENIS (SINETICS) 
C 
C*********************************************************************** 
C 
C    MPI TYPE FOR TYPE CHARAC_TYPE - CHARACTERISTICS / INIT, ETC. 
C    MPI_ADDRESS POSSIBLY MOST PORTABLE THROUGH PLATFORMS  
C    HOWEVER WE APPLY MPI_TYPE_EXTENT TO ESTIMATE THE BASKET FIELD   
C   / UP TO DATE NO CHECKING OF THE MPI ERROR STATUS /  
C 
C----------------------------------------------------------------------- 
C 
C APPELE PAR : STREAMLINE  
C 
C*********************************************************************** 
C       
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU 
C 
      INTEGER, INTENT(INOUT) :: NOMB 
      INTEGER, INTENT(INOUT)  :: CHARACTERISTIC 
      LOGICAL, INTENT(IN) ::TRACE 
!
!-----------------------------------------------------------------------
!
      IF(LNG.EQ.1) WRITE(LU,*) 'APPEL DE ORG_CHARAC_TYPE1 VERSION VIDE'
      IF(LNG.EQ.2) WRITE(LU,*) 'CALL OF ORG_CHARAC_TYPE1, VOID VERSION'
!
!----------------------------------------------------------------------
!
      RETURN
      END
 
