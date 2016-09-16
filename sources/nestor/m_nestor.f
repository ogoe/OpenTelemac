      MODULE      m_Nestor                       !******       SUBROUTINE  ********************
!**                                               *********************************************
!**                                               *********************************************
      USE m_TypeDefs_Nestor             
!                                        
!    __________________________________________________________________
!   /__________________________________________________________________\
!  //                                                                  \\
! <(              declaration part                                      )>
!  \\__________________________________________________________________//
!   \__________________________________________________________________/
      LOGICAL       :: ParallelComputing = .FALSE.
      LOGICAL       :: Restart           = .FALSE. 
      INTEGER       :: npoin             = 0
      INTEGER       :: npoinGlobal       = 0
      INTEGER       :: ncsize            = 0
      INTEGER       :: ipid              = 0   ! wird nur fuer Tests benoetigt
      INTEGER       :: nGrainClass       = 0
      INTEGER       :: GraphicOutputPeriod = 999999999
                                         
      REAL (KIND=R8):: MorpholFactor  = 0.0D0   ! morphological factor
                                         
      REAL (KIND=R8), PARAMETER   ::  eps = 0.0000000001D0
                                         
      CHARACTER(128)::Path = " "  
                                         
      TYPE(t_Polygon),ALLOCATABLE,DIMENSION (:) ::  Poly
      INTEGER                                   :: nPolys
      TYPE(t_Action),ALLOCATABLE,DIMENSION  (:) ::  A
      INTEGER                                   :: nActions
      TYPE(t_Field),ALLOCATABLE,DIMENSION   (:) ::  F
      INTEGER                                   :: nFields
                                         
      TYPE( t_DateTime ) :: SisStart  



!      contains
!        include  './MainNestor.f'
!        include  './Calculate_PlanarLevel.f'
!        include  './InitialiseNestor.f'
!        include  './ReadPolygons.f'
!        include  './ThreeDigitsNumeral.f'
!        include  './DateStringToSeconds.f'
!        include  './ReadDigActions.f'
!        include  './ParseSteerLine.f'
!        include  './IsActionCompletelyDefined.f'
!        include  './WriteField.f'
!        include  './WriteDigAction.f'
!        include  './inside_point_2d_d.f'
!        include  './InterFaceInitNestor.f'
!        include  './InterFaceRunNestor.f'
!        include  './Diff_Time.f'
!        include  './ErrMsgAndStop.f'
!        include  './InfoMessage.f'
!        include  './open_File.f'
!        include  './Intersection.f'
!        include  './Set_RefLevelByProfiles.f'
!        include  './Intpol_Z_parallel_Profils.f'
!        include  './Intpol_Z_angular_Profils.f'
!        include  './Dig_by_Criterion.f'
!        include  './Dump_by_Time.f'
!        include  './Dig_by_Time.f'
!        include  './Dump_by_Rate.f'
!        include  './CalcDigVolumeInRadius.f'
!        include  './WriteActionToRestart.f'
!        include  './ReadActionToRestart.f'
!        include  './WriteFieldToRestart.f'
!        include  './ReadFieldToRestart.f'
!        include  './ReadWriteRestart.f'
!        include  './my_FLUSH.f'


      
                                         
!***                                              ********************************************
!***                                              ********************************************
      END MODULE  m_Nestor                       !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************
!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
