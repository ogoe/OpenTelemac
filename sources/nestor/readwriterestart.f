      SUBROUTINE  ReadWriteRestart               !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & ( time, whatToDo )                             
                                         
!      USE m_TypeDefs_Nestor             
!      USE m_Nestor, ONLY :  F, nFields, ipid, ncsize
!     &                     , ParallelComputing
      USE m_TypeDefs_Nestor, ONLY : R8   
        
      IMPLICIT NONE                      
                                         
      REAL (KIND=R8),INTENT(IN)    :: time          !  time [s]   
      CHARACTER(8)  ,INTENT(IN)    :: whatToDo
        
      !------- local variables ---------------
      CHARACTER (128) :: fileName
        
!      dbug WRITE(6,*)'?>-------  SR ReadWriteRestart -------------'
        
      IF( whatToDo(1:4) == 'read' ) THEN  ! read one restart file
        
        !fileName = "_restart_Nestor.dat"
         fileName = "DSCFG1"
        CALL ReadActionToRestart( fileName )
        CALL ReadFieldToRestart(  fileName )
      ENDIF 
        
      IF( whatToDo(1:5) == 'write' ) THEN ! write two restart files
        
        fileName = "_restart_DigActions.dat"
        CALL WriteActionToRestart( time, fileName )
        
        fileName = "_restart_FieldData.dat"
         CALL WriteFieldToRestart(  time, fileName )
      ENDIF  
                
!      dbug WRITE(6,*)'?>-------  SR ReadWriteRestart End ---------'
      RETURN                             
!***                                              ********************************************
!***                                              ********************************************
      END SUBROUTINE ReadWriteRestart            !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************

!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
