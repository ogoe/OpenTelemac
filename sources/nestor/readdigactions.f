!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  ReadDigActions                 !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & ()  

      USE m_TypeDefs_Nestor             
                                         
      USE m_Nestor, ONLY :  A, nActions, ipid, ParallelComputing
     &                     , nGrainClass, eps, Restart

         

#ifndef NESTOR_INTERFACES                 
      USE m_Interfaces_Nestor, ONLY :  open_File 
     &                               , ErrMsgAndStop
     &                               , IsActionCompletelyDefined
     &                               , ParseSteerLine
#endif NESTOR_INTERFACES     


#ifndef NESTOR_INTERFACES 
      !--------------------- local variables ---------------

      IMPLICIT NONE                      
      INTEGER         :: i,j, m, lineCount, countClasses
      INTEGER         :: stat = 0        
      INTEGER         :: status          
      CHARACTER (128) :: line           
      CHARACTER (128) :: fileName        

      REAL (KIND=R8)  :: sumGrainCL      
      REAL (KIND=R8)  :: DateStringToSeconds  ! function
      EXTERNAL        :: DateStringToSeconds  ! function
      LOGICAL         :: ThreeDigitsNumeral   ! function 
      EXTERNAL        :: ThreeDigitsNumeral   ! function 
      CHARACTER  (16) :: rCh              ! to store a real value as string
      CHARACTER (128) :: str              ! to store a string

      TYPE(t_String_Length) :: SRname     ! subroutine where the error occured 
      INTEGER,ALLOCATABLE,DIMENSION (:) :: FieldID  
      LOGICAL          :: FieldIDisDouble
      LOGICAL          :: GrCLNumErr, GrCLSumErr
      LOGICAL          :: PassedKeywordENDACTION
      LOGICAL          :: PassedKeywordACTION
      LOGICAL          :: PassedKeywordRESTART

      CHARACTER (128) :: KeyWord 
      CHARACTER (128) :: valueStr 
      INTEGER         :: Le, fu
      
!      dbug WRITE(6,*)'?>-------  SR ReadDigActions ---------------'
      SRname%s = "ReadDigActions"
      SRname%i = 14
                                         
      !fileName = "_DigActions.dat"  
       fu = 334      
       fileName = "SISMAF"              
      CALL open_File( fileName, fu, 'r' )        ! open File
      
      PassedKeywordRESTART   = .FALSE.        
      PassedKeywordACTION    = .FALSE.        
      PassedKeywordENDACTION = .TRUE.
      lineCount = 0 
      nActions  = 0                       
      DO   !  loop to read the dig action file to detect the number of Actions
        lineCount = lineCount + 1
        READ( fu, '(A)', IOSTAT = stat ) line
        IF( stat /= 0 ) EXIT
        line = ADJUSTL(line)      !      "   blabla " 
                                  !  --> "blabla    " 
        IF(line(1:1) == ''  ) CYCLE
        IF(line(1:1) == '/' ) CYCLE
        IF(line(1:7) == 'ENDFILE') EXIT
        
        IF(line(1:6) == 'ACTION' ) THEN
          IF( PassedKeywordENDACTION .EQV. .FALSE.) EXIT  ! error message is called 
                                                          ! after this do-loop
          nActions = nActions + 1
          PassedKeywordACTION    = .TRUE.        
          PassedKeywordENDACTION = .FALSE.
        ENDIF
        
        IF(line(1:9) == 'ENDACTION') THEN
          IF( PassedKeywordACTION .EQV. .FALSE.)Call ErrMsgAndStop(
     &     "while read the Action file             ",39
     &    ,"reason:  missing key word   ACTION     ",39 ," ",1
     &    ,"occured in line: ",17 , lineCount, SRname, ipid         )
        
          PassedKeywordACTION    = .FALSE. 
          PassedKeywordENDACTION = .TRUE.
        ENDIF          

      ENDDO !  loop to read the dig action file to detect the number of Actions  

      IF( PassedKeywordENDACTION .EQV. .FALSE.)THEN
        Call ErrMsgAndStop( "while read the Action file ",27
     &  ,"reason:  missing key word   ENDACTION  ",39 ," ",1
     &  ,"occured in line: ",17 , lineCount, SRname, ipid     )
      ENDIF
      
       
      ALLOCATE( A( nActions ), stat=status)
      DO m=1, nActions                   
        ALLOCATE( A(m)%GrainClass(nGrainClass), stat=status)
        A(m)%GrainClass(:) = -11.1D0  ! initialise array
        ALLOCATE( A(m)%dzCL_ts(nGrainClass), stat=status)
        A(m)%dzCL_ts(:)    = -11.1D0  ! initialise array  dz per  grainCLass   per  TimeStep
      ENDDO                              
!                                        
      REWIND fu                          
      m = 0                              
      lineCount = 0                      
      DO   ! loopt to read the file containing the dig actions
        READ( fu, '(A)', IOSTAT = stat ) line
        !WRITE(*,*) TRIM(line)           !debug
        !CALL FLUSH(6)                    !debug
        IF( stat /= 0 .OR. line(1:7) == 'ENDFILE') EXIT
        lineCount = lineCount + 1        
        line = ADJUSTL(line)                   !  "   blabla "  --> "blabla    " 
        IF( line(1:1) == ''  ) CYCLE    
        IF( line(1:1) == '/' ) CYCLE    
        IF( line(1:6) == 'ACTION' ) THEN
          m = m + 1   ! from here a new Action block beginns
          countClasses = 0 
          CYCLE                          
        ENDIF

        
                                         
        IF( line(1:9) == 'ENDACTION' ) THEN  !>  do some checks 

          
          CALL IsActionCompletelyDefined( A(m), m ) 
          
          GrCLNumErr = .FALSE.       !>  Grain Class Number Error  
          GrCLSumErr = .FALSE.       !>  Grain Class Sum of all classes Error       
          IF( A(m)%ActionType == 2 ) THEN                            !> 2: Dump_by_time
            IF( countClasses /= nGrainClass )   GrCLNumErr = .True.  !> check number of grain classes in the Action
            
            sumGrainCL = SUM( A(m)%GrainClass(:) )
            IF( ABS(sumGrainCL - 1.0D0) > eps ) GrCLSumErr = .True.  !> check sum of grain classes  it must be 1 

            !> To assure that the sum of the grain classes = 1
            !  we re-calculate the class(1) in dependence 
            !  of the other classes:   class(1) = 1 - sum( class(2:n) ) 
            A(m)%GrainClass(1)=1.D0-SUM(A(m)%GrainClass(2:nGrainClass))
        
          ENDIF   ! Dump_by_time
          
          IF( GrCLNumErr) THEN  !> check user input relating number of  grain classes 
            WRITE(rCh,'(I4)') m                !> convert integer value to string and then
            WRITE(rCh,'(16A)') adjustl(rCh)    !  convert string to left-aligned string 
            Call ErrMsgAndStop( "while read the Action file     ",31
     &      ,"         reason: Wrong number of grain classes was",50
     &      ,"                 defined in Action    "//rCh(1:4)  ,42 
     &      ,"                 It must be  ",29,nGrainClass,SRname,ipid) 
          ENDIF
          
          IF( GrCLSumErr ) THEN      
            WRITE(rCh,'(F16.8)') sumGrainCL    !> convert real value to string and then
            WRITE(rCh,'(16A)') adjustl(rCh)    !  convert string to left-aligned string 
            Call ErrMsgAndStop( "while read the Action file        ",34
     &,"reason: sum(grain classes) = "//rCh//" but it must be 1.0 !",66 
     &,"        In case dummy values are used it must be negative !",59 
     &,"occured in Action: ", 19, m, SRname, ipid      )
          ENDIF 
          
          CYCLE          
        ENDIF    !   line(1:9) == 'ENDACTION'

        CALL ParseSteerLine(line, KeyWord, valueStr)
        !   WRITE(6,*) '-------lineCount = ',lineCount                              !debug
        !   WRITE(6,*) 'KeyWord = >', KeyWord(1:LEN_TRIM(KeyWord)),'<'              !debug
        !   WRITE(6,*) 'valueStr = >', valueStr(1:LEN_TRIM(valueStr)),'<'           !debug
                                                                                    
        Le = LEN_TRIM(KeyWord)
                                        
        SELECT CASE(  KeyWord(1:Le)  )            
          CASE( "ActionType" )
            IF(valueStr(1:16) == 'Dig_by_criterion') THEN 
              A(m)%ActionType = 3
            ENDIF        
            IF(valueStr(1:12) == 'Dump_by_time') THEN 
              A(m)%ActionType = 2
            ENDIF         
            IF(valueStr(1:11) == 'Dig_by_time') THEN 
              A(m)%ActionType = 1
            ENDIF         
            IF(A(m)%ActionType == -11) THEN    !>  -11 (=default value from initialisation)
              stat = -1                        !   then throw error message
            ENDIF         
            READ(valueStr,*) A(m)%ActionTypeStr
            
          CASE( "FieldDig" )                      
            READ(valueStr,*) A(m)%FieldDig
            ! check if Field name conforms to the demand format
            IF( .NOT. ThreeDigitsNumeral(A(m)%FieldDig(1:3))) THEN     !> check if Field name conforms
              Call ErrMsgAndStop( "while read the Action file     ",31 !  to the demand format
     &        ,"         reason: FieldDig-name must have at posi- ",50  
     &        ,"                 tion 1-3 numerals like: 123_aName",50    
     &        ,"occured in line: ", 17, lineCount, SRname, ipid       )
            ENDIF                     
            READ( valueStr,'(I3)',IOSTAT=stat) A(m)%FieldDigID   ! read the first string elements as integer
            
          CASE( "ReferenceLevel" )                 !> -----------------------------------
            READ(valueStr,*) A(m)%ReferenceLevel   !   tolerate different spellings of    
          CASE( "ReferenzLevel" )                  !   the keyword "Refere.." in the   
            READ(valueStr,*) A(m)%ReferenceLevel   !   action file 
          CASE( "ReferezLevel" )                   !   
            READ(valueStr,*) A(m)%ReferenceLevel   ! ------------------------------------

          CASE( "TimeStart" )                      
            A(m)%TimeStart = DateStringToSeconds(valueStr,lineCount)
          CASE( "TimeEnd" )                      
            A(m)%TimeEnd   = DateStringToSeconds(valueStr, lineCount)
            IF( A(m)%TimeEnd <= A(m)%TimeStart ) THEN
              Call ErrMsgAndStop( "while read the Action file     ",31
     &        ,"         reason:  TimeEnd is before TimeStart     ",50
     &        ,"                                                  ",50
     &        ,"occured in line: ", 17, lineCount, SRname, ipid      )
            ENDIF                     
          CASE( "TimeRepeat" )                      
            READ(valueStr,*)  A(m)%TimeRepeat 
          CASE( "DigVolume" )                      
            READ(valueStr,*)  A(m)%DigVolume      
          CASE( "DigRate" )            
            READ(valueStr,*,IOSTAT=stat)  A(m)%DigRate
          CASE( "DigDepth" )            
            READ(valueStr,*,IOSTAT=stat)  A(m)%DigDepth       
          CASE( "DigPlanar" )            
            READ(valueStr,*,IOSTAT=stat)  A(m)%DigPlanar       
          CASE( "CritDepth" )            
            READ(valueStr,*,IOSTAT=stat)  A(m)%CritDepth      
          CASE( "MinVolume" )            
            READ(valueStr,*,IOSTAT=stat)  A(m)%MinVolume      
          CASE( "MinVolumeRadius" )                      
            READ(valueStr,*,IOSTAT=stat)  A(m)%MinVolumeRadius
          CASE( "FieldDump" )                      
            READ(valueStr,*)  A(m)%FieldDump   
            IF( .NOT. ThreeDigitsNumeral(A(m)%FieldDump(1:3))) THEN    !> check if Field name conforms 
              Call ErrMsgAndStop( "while read the Action file     ",31 !  to the demand format
     &        ,"         reason: FieldDump-name must have at posi-",50 
     &        ,"                 tion 1-3 numerals like: 123_aName",50    
     &        ,"occured in line: ", 17, lineCount, SRname, ipid      )
            ENDIF                     
            READ( valueStr,'(I3)',IOSTAT=stat) A(m)%FieldDumpID   ! read the first string elements as integer
          CASE( "DumpRate" )                      
            READ(valueStr,*,IOSTAT=stat)  A(m)%DumpRate      
          CASE( "DumpPlanar" )                  
            READ(valueStr,*,IOSTAT=stat)  A(m)%DumpPlanar    
          CASE( "DumpVolume" )                  
            READ(valueStr,*,IOSTAT=stat)  A(m)%DumpVolume    
          CASE( "GrainClass" )                      
            countClasses  =  countClasses + 1
            IF(countClasses <= nGrainClass ) THEN      !  prevent severe memory access 
              READ(valueStr,*,IOSTAT=stat) A(m)%GrainClass(countClasses)
            ENDIF  
            
          CASE( "RESTART" )                  
            READ(valueStr,*,IOSTAT=stat)  Restart
            PassedKeywordRESTART   = .TRUE.
        
          CASE DEFAULT                   
            Call ErrMsgAndStop( "while read the Action file ",27
     &      ,"reason:  unknown key word:     "//KeyWord(1:Le),31+Le 
     &      ,"         check spelling !                     ",46 
     &      ,"occured in line: ",17 , lineCount, SRname, ipid      )
        END SELECT 
        
        IF( stat /= 0 ) Call ErrMsgAndStop(
     &   "while read the Action file                    ",46
     &  ,"reason:  bad value for:        "//KeyWord(1:Le),31+Le 
     &  ,"              value is:        "//valueStr     ,31    
     &  + LEN_TRIM(valueStr) 
     &  ,"occured in line: ",17 , lineCount, SRname, ipid      )
                                         
      ENDDO   ! loop to read the file 
      
      IF( ParallelComputing ) CALL P_SYNC()
      CLOSE(fu)                          
                                         
                                                                                        
      DO m=1, nActions   ! check if grain classes are defined correctly 
        
        sumGrainCL = SUM( A(m)%GrainClass(:) )
        IF(      sumGrainCL              > 0.0D0             !> If sum of grain classes is 
     &     .AND. ABS(sumGrainCL - 1.0D0) > eps   ) THEN      !  bigger than 0.0 and not 1
          WRITE(rCh,'(F16.8)') sumGrainCL    !> convert real value to string and then
          WRITE(rCh,'(16A)') adjustl(rCh)    !  convert string to left-aligned string 
          Call ErrMsgAndStop( "while read the Action file          ",36
     &,"reason: sum(grain classes) = "//rCh//" but it must be 1.0 !",66 
     &,"        In case dummy values are used it must be negative !",59 
     &,"occured in Action: ", 19, m, SRname, ipid      )
        ENDIF                          
        
        IF( sumGrainCL < 0.0D0 ) CYCLE !> ok a negative value indicates  
                                       !  that dummy values are used.
                                       !  Thus we don't recalculate class(1) 
                                       
        !> To assure that the sum of the grain classes = 1
        !  we re-calculate the class(1) in dependence 
        !  of the other classes:   class(1) = 1 - sum( class(2:n) ) 
        A(m)%GrainClass(1) = 1.D0 - SUM(A(m)%GrainClass(2:nGrainClass))
      ENDDO 

      !----------------------------------------------------------------+      
      !>    Check if in the Action file a Field is                     |
      !     referenced more than once                                  |   
      !                                                                |
      ALLOCATE( FieldID( 2 * nActions ), stat=status)
      i = 0
      DO m=1, nActions   !> fill the array FieldID with 
        i = i + 1        !  all FieldDigIDs and FieldDumpIDs
        FieldID(i) = A(m)%FieldDigID
        i = i + 1
        FieldID(i) = A(m)%FieldDumpID
      ENDDO   
      
      FieldIDisDouble = .FALSE.                !> check if a FieldID occures  
      outerloop:DO i=1, 2*nActions - 1         !  more than once in the array     
                  IF( FieldID(i) <= 0 ) CYCLE   
                  DO j=i+1, 2*nActions   
                    IF( FieldID(j) <= 0 ) CYCLE      
                    IF( FieldID(j) == FieldID(i) ) THEN
                      FieldIDisDouble = .TRUE.
                      EXIT outerloop
                    ENDIF  
                  ENDDO                              
                ENDDO outerloop 

      IF( FieldIDisDouble ) THEN        ! throw error message     
        WRITE(str,'(I3.3)') FieldID(j)  ! example: FieldID = 1  ==> str = "001"
        Call ErrMsgAndStop( "while read the Action file             ",39
     &  ,"reason: Field "//str(:3)//"... is refenced repeatedly     ",48
     &  ,"        To identify a Field only the three numerals at the",58 
     &  ,"        begin of the Field name are decisive.             ",58 
     &  ,-1, SRname, ipid )
      ENDIF
      DEALLOCATE( FieldID )
      !                                                                |
      !----------------------------------------------------------------+


       IF( .NOT. PassedKeywordRESTART ) THEN
         Call ErrMsgAndStop( "while read the Action file",26," ",1
     &   ,"reason: missing the KeyWord     RESTART      ",45," ",1
     &   , -1, SRname, ipid )
       ENDIF
      
      !DO m=1, nActions                  
      !  CALL WriteDigAction( A(m) )        
      !ENDDO   !STOP                              
!      dbug WRITE(6,*)'?>-------  SR ReadDigActions End -----------'
      RETURN                             
!***                                              ********************************************
!***                                              ********************************************
#endif NESTOR_INTERFACES                         !******************************************** 
      END SUBROUTINE ReadDigActions              !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************