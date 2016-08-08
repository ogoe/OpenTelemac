      MODULE      m_TypeDefs_Nestor              !******       SUBROUTINE  ********************
!**                                               *********************************************
!**                                               *********************************************
                                         
!      ________________________________________________________________
!     |                                                                |
!     |                          DateTime                              |
      TYPE :: t_DateTime !_____________________________________________|
         INTEGER    :: year               ! The year                                     
         INTEGER    :: month              ! The month                                    
         INTEGER    :: day                ! The day of the month                         
         INTEGER    :: zone               ! Time difference with UTC in minutes          
         INTEGER    :: hour               ! The hour of the day                          
         INTEGER    :: minutes            ! The minutes of the hour                      
         INTEGER    :: seconds            ! The seconds of the minute                    
         INTEGER    :: milliseconds       ! The milliseconds of the second               
       END TYPE t_DateTime               

!      ________________________________________________________________
!     |                                                                |
!     |                          String and his length                 |
      TYPE :: t_String_Length !________________________________________|
        CHARACTER                 (128) :: s    ! string
        INTEGER                         :: i    ! length of string
      END TYPE t_String_Length                 
       
!      ________________________________________________________________
!     |                                                                |
!     |                          2D point                              |
      TYPE :: t_Point_2D !_____________________________________________|
         REAL (KIND=8) :: y              
         REAL (KIND=8) :: x              
      END TYPE t_Point_2D                
                                         
!      ________________________________________________________________
!     |                                                                |
!     |                          Polygon                               |
      TYPE :: t_Polygon !______________________________________________|
        CHARACTER                 (128) :: name = 'blabloblu'
        INTEGER                         :: nPoints  = -999
        TYPE(t_Point_2D), ALLOCATABLE    
     &                  , DIMENSION (:) :: Pt
      END TYPE t_Polygon                 
                                         
!      ________________________________________________________________
!     |                                                                |
!     |                          Leg 3D                                |
      TYPE :: t_Leg_3D !_______________________________________________|
         REAL (KIND=8) :: x1  ! x coordinate point 1
         REAL (KIND=8) :: y1  ! y coordinate point 1
         REAL (KIND=8) :: z1  ! z coordinate point 1
         REAL (KIND=8) :: x2  ! x coordinate point 2
         REAL (KIND=8) :: y2  ! y coordinate point 2
         REAL (KIND=8) :: z2  ! z coordinate point 2
         REAL (KIND=8) :: km  ! measure of river length
      END TYPE t_Leg_3D                  
                                         
!      ________________________________________________________________
!     |                                                                |
!     |                          Interface node                        |
      TYPE :: t_iFaceNode !____________________________________________|
         INTEGER  :: Index               
         INTEGER  :: nNeighbPart         
      END TYPE t_iFaceNode               
                                         
!      ________________________________________________________________
!     |                                                                |
!     |                          Field                                 |
      TYPE :: t_Field !________________________________________________|
        CHARACTER                   (128) ::   Name    = 'blabloblu'
        Integer                           ::   FieldID = -1
        INTEGER,ALLOCATABLE,DIMENSION (:) ::   Node           ! nodes inside the Polygon lokal index
        INTEGER                           ::  nNodes   = -999
!        INTEGER,ALLOCATABLE,DIMENSION (:) ::   NodeGlobal     ! nodes inside the Polygon global index
        TYPE(t_iFaceNode),ALLOCATABLE    
     &                   ,DIMENSION   (:) ::   IntFacNode
        INTEGER                           ::  nIntFacNodes = -999
        REAL (KIND=8)                     ::   Area
        REAL (KIND=8),ALLOCATABLE        
     &               ,DIMENSION   (:)     ::   NodeArea
        REAL (KIND=8),ALLOCATABLE        
     &               ,DIMENSION   (:)     ::   X
        REAL (KIND=8),ALLOCATABLE        
     &               ,DIMENSION   (:)     ::   Y
        REAL (KIND=8),ALLOCATABLE        
     &               ,DIMENSION   (:)     ::   Z
        REAL (KIND=8),ALLOCATABLE        
     &               ,DIMENSION   (:)     ::   dZ
!       REAL (KIND=8),ALLOCATABLE        
!    &               ,DIMENSION   (:)     ::   refZ
        REAL (KIND=8),POINTER            
     &               ,DIMENSION   (:)     ::   refZ => null()
        REAL (KIND=8),ALLOCATABLE        
     &               ,DIMENSION   (:)     ::   km
                                         
        !REAL (KIND=8),POINTER             ::   critZ
        REAL (KIND=8),POINTER                
     &               ,DIMENSION   (:)     ::   targZ => null()
        LOGICAL      ,ALLOCATABLE        
     &               ,DIMENSION   (:)     ::   NodeToDig 
        INTEGER                           ::  nNodeToDig = -999
                                         
      END TYPE t_Field                   
                                         
!      ________________________________________________________________
!     |                                                                |
!     |                          Action                                |
      TYPE :: t_Action !_______________________________________________|
        INTEGER         ::  ActionType     = -11
        CHARACTER (128) ::  ActionTypeStr  = 'aaaaaaaaaaa'
        CHARACTER (128) ::  FieldDig       = '000_aaaaaaaa'
        Integer         ::  FieldDigID     = -1
        CHARACTER (128) ::  ReferezLevel   = '-1aaaa'
!                                        
        REAL (KIND=8)   ::  TimeStart      = -11.1D34    !> in practice here negative values may occure 
        REAL (KIND=8)   ::  TimeEnd        = -11.1D34    !  thus to initialise a value far bejond probability is used
        REAL (KIND=8)   ::  TimeRepeat     = -11.1D0
!                                        
        REAL (KIND=8)   ::  DigVolume   =  -0.1D0
        REAL (KIND=8)   ::  DigRate     =  -0.1D0
        REAL (KIND=8)   ::  DigDepth    = -11.1D34       !> in practice here negative values may occure 
        LOGICAL         ::  DigPlanar   = .False.        !  thus to initialise a value far bejond probability is used  
        REAL (KIND=8)   ::  CritDepth   = -11.1D34       !> in practice here negative values may occure               
        REAL (KIND=8)   ::  MinVolume       =  -0.1D0    !  thus to initialise a value far bejond probability is used  
        REAL (KIND=8)   ::  MinVolumeRadius =  -0.1D0
        CHARACTER (128) ::  FieldDump   = '000_aaaaaaaa'
        INTEGER         ::  FieldDumpID = -1
        REAL (KIND=8)   ::  DumpVolume  = -0.1D0
        REAL (KIND=8)   ::  DumpRate    = -0.1D0
        LOGICAL         ::  DumpPlanar  = .False.
        REAL (KIND=8), ALLOCATABLE       
     &               , DIMENSION (:) ::  GrainClass
!        REAL (KIND=8), ALLOCATABLE       
!     &               , DIMENSION (:) ::  HeapClass !> heaps of dug material sorted by grain class 
                                         
        ! internal                       
        LOGICAL         :: FirstTimeActive = .TRUE.
                                                       
        INTEGER         :: State   = -11         !> Status of Action: 0 = not jet     active    
                                                 !                    1 = currently   active   
                                                 !                    2 = temporary inactive   
                                                 !                    9 = for ever  inactive   
        INTEGER         ::  nts     = -11        !> number of time steps that is 
                                                 !  needed till the action is finished
        INTEGER         ::  tsCount = -11        !> count time steps while action is working 
                                         
        REAL (KIND=8)   :: sumInput = -11.1D0    !> amount of sediment that was  
                                                 !  transported into the field.
                                                
        REAL (KIND=8)   :: dt_ts    = -11.1D0    !> time      per  time step    
        REAL (KIND=8)   :: dz_ts    = -11.1D0    !> evolution per  time step     
        REAL (KIND=8)   :: dz_dt    = -11.1D0    !> evolution per  time     
        REAL (KIND=8)   :: dzTot    = -11.1D0    !> total evolution  
        REAL (KIND=8), ALLOCATABLE       
     &               , DIMENSION (:) ::  dzCL_ts !> evolution per time step per class
                                         
      END TYPE t_Action !______________________________________________                                                 !
!     |                                                                |
!     |                                                                |
!     |________________________________________________________________|
                                         
                                         
!***                                              ********************************************
!***                                              ********************************************
      END MODULE   m_TypeDefs_Nestor             !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
!*********************************************************************************************
                                         
!**********************************************************************************************
!**********************************************************************************************
!**********************************************************************************************
!**                                               *********************************************
!**                                               *********************************************
