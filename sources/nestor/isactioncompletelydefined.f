      SUBROUTINE  IsActionCompletelyDefined      !********************************************
!***                                              ********************************************
!***                                              ********************************************
     &( A, m )                              
                                         
      USE m_TypeDefs_Nestor             
      USE m_Nestor, ONLY : ipid
      IMPLICIT NONE                      
                                         
      TYPE(t_Action), INTENT(IN) :: A   ! one element of array A  
      INTEGER, INTENT(IN)        :: m   ! index to define element of array A  
      
      ! -- local --
      INTEGER                    :: sL
      CHARACTER (128)            :: str  
      TYPE(t_String_Length)      :: SRname    ! name of current Subroutine 
      
      WRITE(6,*)'?>-------  SR IsActionCompletelyDefined ----'
      SRname%s = "IsActionCompletelyDefined"     ! subroutine name  
      SRname%i =  25                             ! length of name string
      
      str = "#####"
      SELECT CASE( A%ActionType ) !=====================================        
       CASE( 1 )    !------------- Dig_by_time ------------------------   
          IF( A%TimeEnd      < -10.0D32)THEN
            str = "TimeEnd" 
            GOTO 123 
          ENDIF
          IF( A%TimeStart    < -10.0D32)THEN
            str = "TimeStart" 
            GOTO 123 
          ENDIF
          IF( A%FieldDigID   < 0       )THEN
            str = "FieldDig"               ! FieldDumpID is an internal value 
            GOTO 123 
          ENDIF
          IF( A%DigVolume    < 0.0D0   )THEN
            str = "DigVolume" 
            GOTO 123 
          ENDIF
          IF(      A%DigPlanar                        !> only if DigPlanar = true 
     &       .AND. A%ReferezLevel(1:1) == "-"         !  we need a ReferenzLevel file
     &       .AND. A%ReferezLevel(2:2) == "1" )THEN   !  the default name string = "-1aaaa"
            str = "ReferezLevel" 
            GOTO 123 
          ENDIF

          IF( A%FieldDumpID  > 0  )THEN  ! dumping is ordered
            IF( A%DumpRate < 0.0D0   )THEN
              str = "DumpRate" 
              GOTO 123 
            ENDIF
            IF(      A%DigPlanar                        !> only if DigPlanar = true 
     &         .AND. A%ReferezLevel(1:1) == "-"         !  we need a ReferenzLevel file
     &         .AND. A%ReferezLevel(2:2) == "1" )THEN   !  the default name string = "-1aaaa"
              str = "ReferezLevel" 
              GOTO 123 
            ENDIF
          ENDIF   ! dumping is ordered
          
          

       CASE( 2 )    !------------- Dump_by_time ------------------------   
          IF( A%TimeEnd      < -10.0D32)THEN
            str = "TimeEnd" 
            GOTO 123 
          ENDIF
          IF( A%TimeStart    < -10.0D32)THEN
            str = "TimeStart" 
            GOTO 123 
          ENDIF
          IF( A%FieldDumpID  < 0       )THEN
            str = "FieldDump"               ! FieldDumpID is an internal value 
            GOTO 123 
          ENDIF
          IF( A%DumpVolume   < 0.0D0   )THEN
            str = "DumpVolume" 
            GOTO 123 
          ENDIF
         !IF( A%DumpRate     < 0.0D0   )THEN
         !  str = "DumpRate" 
         !  GOTO 123 
         !ENDIF
          IF(      A%DumpPlanar                       !> only if DumpPlanar = true 
     &       .AND. A%ReferezLevel(1:1) == "-"         !  we need a ReferenzLevel file
     &       .AND. A%ReferezLevel(2:2) == "1" )THEN   !  the default name string = "-1aaaa"
            str = "ReferezLevel" 
            GOTO 123 
          ENDIF

       CASE( 3 )    !------------- Dig_by_criterion -------------------- 
          IF( A%TimeEnd      < -10.0D32)THEN
            str = "TimeEnd" 
            GOTO 123 
          ENDIF
          IF( A%TimeStart    < -10.0D32)THEN
            str = "TimeStart" 
            GOTO 123 
          ENDIF
          IF( A%TimeRepeat   <   0.00D0)THEN
            str = "TimeRepeat" 
            GOTO 123 
          ENDIF
          IF( A%FieldDigID   <   0     )THEN
            str = "FieldDig"                ! FieldDigID is an internal value 
            GOTO 123 
          ENDIF
          IF( A%DigRate      <   0.0D0 )THEN
            str = "DigRate" 
            GOTO 123 
          ENDIF
          IF( A%DigDepth     < -10000.0D0 )THEN
            str = "DigDepth" 
            GOTO 123 
          ENDIF
          IF( A%MinVolume    <   0.0D0 )THEN
            str = "MinVolume" 
            GOTO 123 
          ENDIF
          IF( A%MinVolumeRadius  <  0.0D0 )THEN
            str = "MinVolumeRadius" 
            GOTO 123 
          ENDIF
          IF(      A%ReferezLevel(1:1) == "-"         !  we need a ReferenzLevel file
     &       .AND. A%ReferezLevel(2:2) == "1" )THEN   !  the default name string = "-1aaaa"
            str = "ReferezLevel" 
            GOTO 123 
          ENDIF
          IF(       A%FieldDumpID  > 0                 !> the dug material will be dumped
     &       .AND.  A%DumpRate     < 0.0D0 )THEN
            str = "DumpRate"  
            GOTO 123 
          ENDIF
        
       CASE DEFAULT !------------- No ActionType -----------------------
          str = "ActionType" 
          GOTO 123 

      END SELECT    !===================================================
      
      
  123 CONTINUE   ! 
      IF( str(1:1) /= "#" ) THEN  ! value of str is no more "#####"  ==> error occured 
        sL  = LEN_TRIM(str)
        Call ErrMsgAndStop2("while read the Action file        ",34
     &  ,"         reason: An Action is not completely defined ",53    
     &  ,"                 missing proper values at: "//str(:sL),43+sL  
     &  ,"occured in Action: ", 19, m, SRname, ipid      )
      ENDIF
      
      WRITE(6,*)'?>-------  SR IsActionCompletelyDefined End '
      RETURN                             
!***                                              ********************************************
!***                                              ********************************************
      END SUBROUTINE IsActionCompletelyDefined   !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************

!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
