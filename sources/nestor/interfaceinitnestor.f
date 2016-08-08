      SUBROUTINE  InterFaceInitNestor            !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & (    NCSIZE, IPID, NPOIN, NSICLA  
     &    , SisStartDate, SisStartTime        ! date ,time 
     &    , SisMorpholFactor, SisRestart      ! morphological factor
     &    , SisGraphicOutputPeriod
     &    , x_sis, y_sis, NodeArea_sis
     &    , maxIndex
     &  )                                
!                                        
!      USE DECLARATIONS_SISYPHE, ONLY : MESH,  T13
!                                        
      IMPLICIT NONE                      
!                                        
      INTEGER, INTENT(IN)       ::   NCSIZE, IPID      ! number of cpu, current cpu
      INTEGER, INTENT(IN)       ::   NPOIN, NSICLA     ! number of: points, sediment classes
      INTEGER, INTENT(IN)       ::   SisGraphicOutputPeriod
      INTEGER, INTENT(IN)                              
     &       , DIMENSION (3)    ::   SisStartDate      ! year , month  , day
     &                             , SisStartTime      ! hours, minutes, seconds
      REAL (KIND=8) ,INTENT(IN) ::   SisMorpholFactor  ! morphological factor
      LOGICAL, INTENT(IN)       ::   SisRestart 
      
      REAL (KIND=8), INTENT(IN) 
     &       , DIMENSION (NPOIN)::  x_sis        
      REAL (KIND=8), INTENT(IN) 
     &       , DIMENSION (NPOIN)::  y_sis        
      REAL (KIND=8), INTENT(IN) 
     &       , DIMENSION (NPOIN)::  NodeArea_sis        
                                         
      INTEGER   :: i                     
!                                        
!                                        
      !  assumed-shape arrays:           
      !> Übergabe von Datenfeldern an Unterprogramme
      !> ohne Größenangaben (engl. assumed-shape arrays)
      !> Dies funktioniert nur wenn in der aufrufenden
      !> Programmeinheit der interface-Block für das
      !> Unterprogramm angeführt wird.   
      INTERFACE !------------------------------------------------------+                 
         SUBROUTINE InitialiseNestor                                   !
     &  (     NCSIZE, IPID, NPOIN, NSICLA                              !
     &      , NodeArea_sis, x_sis, y_sis                               !
     &      , SisStartDate, SisStartTime, SisMorpholFactor             !
     &      , npoin_SisGlobal, SisRestart                              !
     &      , SisGraphicOutputPeriod                 )                 !
           IMPLICIT NONE                                               !
           INTEGER       ,INTENT(IN) ::  NCSIZE, IPID, NPOIN, NSICLA   !
     &                                 , npoin_SisGlobal               !
     &                                 , SisGraphicOutputPeriod        !
           REAL (KIND=8) ,INTENT(IN) :: NodeArea_sis (:)  !  assumed-shape array
           REAL (KIND=8) ,INTENT(IN) :: x_sis     (:)     !  assumed-shape array
           REAL (KIND=8) ,INTENT(IN) :: y_sis     (:)     !  assumed-shape array
           INTEGER ,INTENT(IN)                                         !      
     &             , DIMENSION (3)   ::   SisStartDate     ! year , month  , day
     &                                  , SisStartTime     ! hours, minutes, seconds
           REAL (KIND=8) ,INTENT(IN) :: SisMorpholFactor   ! morphological factor
           LOGICAL,INTENT(IN)        :: SisRestart                     !                         
           END SUBROUTINE InitialiseNestor                            !
      END INTERFACE !--------------------------------------------------+ 
        
        
      INTEGER    P_IMAX     
      EXTERNAL   P_IMAX     
        
        
      !------- local variables ---------------
      INTEGER  ::  npoin_SisGlobal
      INTEGER  ::  maxIndex
        
!                                        
!663   FORMAT(' ?>',2(/,' ?>'))            ! 3 lines like "?>         "
      WRITE(6,*)'?>-------  SR InterFaceInitNestor ---------'
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
      CALL InitialiseNestor(  NCSIZE, IPID, NPOIN, NSICLA
!     &                       , T13%R, MESH%X%R, MESH%Y%R 
     &                       , NodeArea_sis, x_sis,  y_sis
     &                       , SisStartDate, SisStartTime
     &                       , SisMorpholFactor, npoin_SisGlobal
     &                       , SisRestart             
     &                       , SisGraphicOutputPeriod  )          
!                                        
      WRITE(6,*)'?>-------  SR InterFaceInitNestor End -----'
!                                        
      RETURN                            
!***                                              ********************************************
!***                                              ********************************************
      END SUBROUTINE InterFaceInitNestor         !********************************************
!**                                               ********************************************
!**                                               ********************************************
!*********************************************************************************************
!*********************************************************************************************
!                                        
!*********************************************************************************************
!*********************************************************************************************
!**                                               ********************************************
!**                                               ********************************************
