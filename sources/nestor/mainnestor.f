!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  MainNestor                     !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & (   ts, dt_ts_sis, time, ELAY0, z_sis
     &   , AVAIL, KNOLG, dzCL_sis )
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LTL            |-->| CURRENT TIME STEP
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!      ________________________________________________________________
!     |                                                                |
!     |        we assume that the sisyphe time step is constant  !     |
!     |_______________ ________________________________________________|
      USE m_TypeDefs_InterFace
      USE m_TypeDefs_Nestor
      USE m_Nestor
      
      

#ifndef NESTOR_INTERFACES                                          
      USE m_Interfaces_Nestor, ONLY :  Dump_by_Time 
     &                               , Dig_by_Time           
     &                               , Dig_by_Criterion           
     &                               , ReadWriteRestart           
#endif NESTOR_INTERFACES 

      IMPLICIT NONE

      INTEGER      , INTENT(IN)    :: ts            !  time-step
      REAL (KIND=R8),INTENT(IN)    :: dt_ts_sis     !  time-step-duration  [ s ]
      REAL (KIND=R8),INTENT(IN)    :: time          !  time [s]
      REAL (KIND=R8),INTENT(IN)    :: ELAY0         !  activLayerThickness [ m ]
      REAL (KIND=R8),INTENT(IN)    :: z_sis(:)      !  bottom [ m+NN ]  (assumed-shape array)
      REAL (KIND=R8),INTENT(IN)    :: AVAIL(:,:,:)  !  assumed-shape array
      INTEGER       ,INTENT(IN)    :: KNOLG(:)      ! index list: Local to Global node index
      TYPE( t_PointerToArrayOfReals )
     &              ,INTENT(INOUT) :: dzCL_Sis(:)   !  bedload evolution per Class  [ m ]

#ifndef NESTOR_INTERFACES 
      !--------------------- local variables ---------------
      
      TYPE(t_String_Length) :: SRname  ! name of current Subroutine

      INTEGER              :: i, n, m, iMesh, iCL, status
      REAL (KIND=R8)       :: dt_ts     ! time-step-duration respecting MorpholFactor   [ s ]


!
663   FORMAT(' ?>',2(/,' ?>'))            ! 3 lines like "?>         "
!      dbug WRITE(6,*)'?>-------  SR MainNestor ------------------'
      SRname%s = "MainNestor"         ! subroutine name
      SRname%i =  11                   ! length of name string


      !WRITE(6,*)'?> time_sis      = ', time          ! debug test
      !WRITE(6,*)'?> npoin         = ', npoin         ! debug test
      !WRITE(6,*)'?>  z_sis(npoin) = ',z_sis(npoin)   ! debug test
      !WRITE(6,*)'?>  z_sis(1)     = ',z_sis(1)       ! debug test
      !WRITE(6,*)'?>  z_sis(638)   = ',z_sis(638)     ! debug test
      !WRITE(6,*)'?>       -5.9782670102672562'       ! debug test
      !STOP

      IF( Restart ) THEN
        CALL ReadWriteRestart( time, 'read    ' )
        Restart = .FALSE.
      ENDIF



      DO m=1, nActions
        !WRITE(6,'(" ?>   state of Action",I3," =",I3)') m, A(m)%State   !debug
!
        IF( A(m)%State == 9 ) CYCLE   !> Status of Action:  0 = not yet      active
                                      !                     1 = currently    active
                                      !                     2 = temporary  inactive
                                      !                     9 = for ever   inactive

        IF(       time >= A(m)%TimeStart )THEN
!        IF(       time >= A(m)%TimeStart
!     &      .AND. time <= A(m)%TimeEnd   )THEN



          dt_ts = dt_ts_sis / MorpholFactor
!
          SELECT CASE( A(m)%ActionType )
!            __________________________________________________________
!           |                                                          |
!           |                                                          |
            CASE(   1   )  ! _________Dig_by_time______________________|
              !WRITE(6,*)'?> CASE 1   Dig_by_time'

              n = A(m)%FieldDigID
              CALL Dig_by_Time(   A(m), F(n), dt_ts, z_sis
     &                          , dzCL_sis, AVAIL, ELAY0
     &                          , time, KNOLG, m          )

!            __________________________________________________________
!           |                                                          |
!           |                                                          |
            CASE(   2   )  ! _________Dump_by_time_____________________|
              !WRITE(6,*)'?> CASE 2   Dump_by_time'

              n = A(m)%FieldDumpID
              CALL Dump_by_Time(   A(m), F(n), dt_ts, z_sis
     &                           , dzCL_sis, ELAY0, time, m     )

!            __________________________________________________________
!           |                                                          |
!           |                                                          |
            CASE(   3   )  ! _________Dig_by_criterion_________________|
              !WRITE(6,*)'?> CASE 3   Dig_by_criterion'
              n = A(m)%FieldDigID

              CALL Dig_by_Criterion(   A(m), F(n), dt_ts, z_sis
     &                               , dzCL_sis, AVAIL, ELAY0
     &                               , time, KNOLG, m           )



!            __________________________________________________________
!           |                                                          |
            CASE DEFAULT  ! ___________________________________________|
               WRITE(6,*)'?>  CASE DEFault erreicht '


          END SELECT

        ENDIF !A(m)%TimeStart <= time <= A(m)%TimeEnd
      ENDDO !m=1, nActions

      IF( MOD(ts,GraphicOutputPeriod) == 0 )
     &  CALL ReadWriteRestart( time, 'write   ' )



!      dbug WRITE(6,*)'?>-------  SR MainNestor End --------------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif NESTOR_INTERFACES                         !******************************************** 
      END SUBROUTINE MainNestor                  !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************