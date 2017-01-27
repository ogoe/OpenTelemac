!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  InterFaceInitNestor            !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & (    NCSIZE, IPID, NPOIN, NSICLA  
     &    , SisStartDate, SisStartTime        ! date ,time 
     &    , SisMorpholFactor                  ! morphological factor
     &    , SisGraphicOutputPeriod
     &    , x_sis, y_sis, NodeArea_sis
     &    , maxIndex
     &  )                                
!                                        
!      USE DECLARATIONS_SISYPHE, ONLY : MESH,  T13
! 
      USE m_TypeDefs_Nestor, ONLY : R8
      
      USE INTERFACE_PARALLEL, ONLY : P_IMAX   
      
      
#ifndef  NESTOR_INTERFACES                                        
      USE m_Interfaces_Nestor, ONLY :  InitialiseNestor
#endif   NESTOR_INTERFACES                                        
      
                                       
      IMPLICIT NONE                      
!                                        
      INTEGER, INTENT(IN)        ::   NCSIZE, IPID      ! number of cpu, current cpu
      INTEGER, INTENT(IN)        ::   NPOIN, NSICLA     ! number of: points, sediment classes
      INTEGER, INTENT(IN)                               
     &       , DIMENSION (3)     ::   SisStartDate      ! year , month  , day
     &                              , SisStartTime      ! hours, minutes, seconds
      REAL (KIND=R8),INTENT(IN)  ::   SisMorpholFactor  ! morphological factor
      INTEGER, INTENT(IN)        ::   SisGraphicOutputPeriod
      
      REAL (KIND=R8), INTENT(IN)
     &        , DIMENSION (NPOIN)::   x_sis, y_sis        
     &                              , NodeArea_sis        
      INTEGER                    ::   maxIndex
                                         
#ifndef NESTOR_INTERFACES 
      !--------------------- local variables ---------------
      INTEGER   ::  i                     
      INTEGER   ::  npoin_SisGlobal
        
!                                        
!663   FORMAT(' ?>',2(/,' ?>'))            ! 3 lines like "?>         "
!      dbug WRITE(6,*)'?>-------  SR InterFaceInitNestor ---------'
!                                        
!                                        
!                                        
      !WRITE(6,*)'?>  NCSIZE = ', NCSIZE  
      !WRITE(6,*)'?>  IPID   = ', IPID    
      !WRITE(6,*)'?>  NPOIN  = ', NPOIN   
      !WRITE(6,*)'?>  NSICLA = ', NSICLA  
      !WRITE(6,'(" ?> SisStartDate", I6)')(SisStartDate(i), i = 1, 3 )
      !WRITE(6,'(" ?> SisStartTime", I6)')(SisStartTime(i), i = 1, 3 )
      
      IF( NCSIZE .GT. 1 ) THEN
        !maxIndex = MAXVAL( MESH%KNOLG%I(:) )
        npoin_SisGlobal = P_IMAX( maxIndex )
      ELSE  
        npoin_SisGlobal = NPOIN      
      ENDIF
      
!                                        
      CALL InitialiseNestor(   NCSIZE, IPID, NPOIN, NSICLA
     &                       , NodeArea_sis, x_sis,  y_sis
     &                       , SisStartDate, SisStartTime
     &                       , SisMorpholFactor, npoin_SisGlobal
     &                       , SisGraphicOutputPeriod  )          
!                                        
!      dbug WRITE(6,*)'?>-------  SR InterFaceInitNestor End -----'
!                                        
      RETURN                            
!***                                              ********************************************
!***                                              ********************************************
#endif NESTOR_INTERFACES                         !******************************************** 
      END SUBROUTINE InterFaceInitNestor         !********************************************
!**                                               ********************************************
!**                                               ********************************************
!*********************************************************************************************
!*********************************************************************************************