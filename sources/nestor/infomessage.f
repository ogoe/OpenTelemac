!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  InfoMessage                    !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & ( A, m, time )

      USE m_TypeDefs_Nestor
      USE m_Nestor , ONLY :  nGrainClass
      
         
      
#ifndef  NESTOR_INTERFACES                                        
      USE m_Interfaces_Nestor, ONLY :  my_FLUSH 
#endif   NESTOR_INTERFACES                                        

      IMPLICIT NONE
      TYPE(t_Action),INTENT(INOUT) :: A

      INTEGER       ,INTENT(IN)    :: m         !> index of action
      REAL (KIND=R8),INTENT(IN)    :: time      !> time [ s ]
      
      
#ifndef NESTOR_INTERFACES 
      !--------------------- local variables ---------------
      INTEGER :: iCL

!669       FORMAT(' ?>',9(/,' ?> info: '))       ! write 9 pseudo empty lines like " ?> error:         "                                                                  !
668   FORMAT(5(' ?> info: ',/))             ! write 9 pseudo empty lines like " ?> error:         "                                                                  !
661   FORMAT(1(' ?> info: '))               ! write 1 pseudo empty lines like " ?> error:         "                                                                  !


670   FORMAT( 1(' ?> info:'),61('='), '+' )
672   FORMAT( 1(' ?> info:'),61(' '), '|' )
673   FORMAT( 1(' ?> info:'),12(' '),'NESTOR',43(' '),'|' )

      WRITE(6,670)
      WRITE(6,672)
      WRITE(6,673)
      WRITE(6,672)
      WRITE(6,*)'?>                 '

      SELECT CASE( A%ActionType )

      CASE(   1   )  ! _________Dig_by_time______________________|

      IF( A%State == 1 ) THEN                                           ! 1 = Action currently active)
       WRITE(6,'(" ?>        start action     : ",A)')A%ActionTypeStr
       WRITE(6,*)'?>        start time  [s]  : ',A%TimeStart
       WRITE(6,'(" ?>        action number    : ",I3)') m
       WRITE(6,'(" ?>        FieldDig         : ",A)')A%FieldDig
      ENDIF

      IF( A%State == 9 ) THEN                                            ! 9 = for ever inactive
       WRITE(6,'(" ?>        end action       : ",A)')A%ActionTypeStr
       WRITE(6,*)'?>        end time  [s]    : ',A%TimeEnd
       WRITE(6,'(" ?>        action number    : ",I3)') m
       WRITE(6,'(" ?>        FieldDig         : ",A)')A%FieldDig
       IF(A%FieldDumpID >= 0 ) THEN
         WRITE(6,'(" ?>        FieldDump        : ",A)')A%FieldDump
         WRITE(6,*)'?> relocated volume [m**3] :', A%MovedVolume
       ELSE
         WRITE(6,*)'?> removed volume   [m**3] :', A%DumpVolume
         DO iCL=1, nGrainClass
           WRITE(6,*)'?>         GrainClass      :', A%GrainClass(iCL)
         ENDDO
       ENDIF
      ENDIF !( A%State == 9 )

      CASE(   2   )  ! _________Dump_by_time_____________________|

      IF( A%State == 1 ) THEN                                           ! 1 = Action currently active)
       WRITE(6,'(" ?>        start action     : ",A)')A%ActionTypeStr
       WRITE(6,*)'?>        start time  [s]  : ',A%TimeStart
       WRITE(6,'(" ?>        action number    : ",I3)') m
       WRITE(6,'(" ?>        FieldDump        : ",A)')A%FieldDump
      ENDIF

      IF( A%State == 9 ) THEN                                            ! 9 = for ever inactive
       WRITE(6,'(" ?>        end action       : ",A)')A%ActionTypeStr
       WRITE(6,*)'?>        end time  [s]    : ',A%TimeEnd
       WRITE(6,'(" ?>        action number    :",I3)') m
       WRITE(6,'(" ?>        FieldDump        : ",A)')A%FieldDump
       WRITE(6,*)'?> preset DumpVolume [m**3]: ',A%DumpVolume
       !WRITE(6,*)'?>                           '
       !WRITE(6,*)'?> sediment that was carried '
       !WRITE(6,*)'?> through sediment transport'
       !WRITE(6,*)'?> into the field while      '
       !WRITE(6,*)'?> dumping  [m**3]         : ',A%sumInput
      ENDIF

      !> amount of sediment that was carried
      !  through sediment transport into the field.


      CASE(   3   )  ! _________Dig_by_criterion_________________|

      IF( A%State == 1 ) THEN                                           ! 1 = Action currently active)
       WRITE(6,'(" ?>        start action     : ",A)')A%ActionTypeStr
       WRITE(6,'(" ?>        FieldDig         : ",A)')A%FieldDig
       IF(A%FieldDumpID > 0)
     & WRITE(6,'(" ?>        FieldDig         : ",A)')A%FieldDump
       WRITE(6,'(" ?>        action number    : ",I3)') m
       WRITE(6,*)'?>        start time  [s]  : ',A%TimeStart

      ELSEIF ( A%State == 2 ) THEN
       WRITE(6,'(" ?>        finalise action    ")')
       WRITE(6,'(" ?>        temporarily      : ",A)')A%ActionTypeStr
       WRITE(6,'(" ?>        FieldDig         : ",A)')A%FieldDig
       IF(A%FieldDumpID > 0)
     & WRITE(6,'(" ?>        FieldDig         : ",A)')A%FieldDump
       WRITE(6,'(" ?>        action number    : ",I3)') m
       WRITE(6,*)'?>                           '
       WRITE(6,*)'?>      maitenance period    '
       WRITE(6,*)'?>        start time  [s]  : ',A%TimeStart
       WRITE(6,*)'?>              time  [s]  : ',time
       WRITE(6,*)'?>                           '
       WRITE(6,*)'?>      during the last      '
       WRITE(6,*)'?>      maitenance period    '
       WRITE(6,*)'?>      dug volume  [m**3] : ',A%MovedVolume
       IF(A%FieldDumpID > 0)
     & WRITE(6,*)'?>      dumped volume      : ',A%MovedVolume
     
      ELSEIF ( A%State == 9 ) THEN
       WRITE(6,'(" ?>        end action       : ",A)')A%ActionTypeStr
       WRITE(6,'(" ?>        FieldDig         : ",A)')A%FieldDig
       IF(A%FieldDumpID > 0)
     & WRITE(6,'(" ?>        FieldDig         : ",A)')A%FieldDump
       WRITE(6,'(" ?>        action number    : ",I3)') m
       WRITE(6,*)'?>                           '
       WRITE(6,*)'?>      maitenance period    '
       WRITE(6,*)'?>        start time  [s]  : ',A%TimeStart
       WRITE(6,*)'?>            time  [s]    : ',time
       WRITE(6,*)'?>                           '
       WRITE(6,*)'?>      during the last      '
       WRITE(6,*)'?>      maitenance period    '
       WRITE(6,*)'?>      dug volume  [m**3] : ',A%MovedVolume
       IF(A%FieldDumpID > 0)
     & WRITE(6,*)'?>      dumped volume      : ',A%MovedVolume
       WRITE(6,*)'?>                           '
       WRITE(6,*)'?>        end time  [s]    : ',A%TimeEnd

      ENDIF

      CASE DEFAULT  ! ___________________________________________|

      END SELECT
      WRITE(6,*)'?>                    '
      WRITE(6,672)
      WRITE(6,672)
      WRITE(6,672)
      WRITE(6,670)

      CALL my_FLUSH(6)                             

!      IF( ParallelComputing ) CALL P_SYNC()


!      dbug WRITE(6,*)'?>-------  SR InfoMessage End --------------'
      RETURN
!***                                              ********************************************
!***                                              ********************************************
#endif NESTOR_INTERFACES                         !******************************************** 
      END SUBROUTINE InfoMessage                 !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************


