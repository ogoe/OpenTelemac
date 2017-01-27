!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
      SUBROUTINE  InitialiseNestor               !********************************************
!***                                              ********************************************
!***                                              ********************************************
!                                        
     & (  ncsize_Sis, ipid_Sis, npoin_Sis, nSiCla_Sis
     &  , NodeArea_sis, x_sis, y_sis 
     &  , SisStartDate, SisStartTime, SisMorpholFactor
     &  , npoin_SisGlobal, SisGraphicOutputPeriod      )                                
!                                        
!                                        
      USE m_TypeDefs_Nestor             
      USE m_Nestor
      USE INTERFACE_PARALLEL, ONLY : P_DSUM   
      
#ifndef  NESTOR_INTERFACES                                        
      USE m_Interfaces_Nestor, ONLY :  ReadPolygons
     &                               , inside_point_2d_d
     &                               , ReadDigActions
     &                               , ErrMsgAndStop
#endif   NESTOR_INTERFACES                                        
                            
 !                                       
      IMPLICIT NONE                      
!                                        
      INTEGER, INTENT(IN) :: ncsize_Sis, ipid_Sis, npoin_Sis 
      INTEGER, INTENT(IN) :: nSiCla_Sis
!                                        
      REAL (KIND=R8), INTENT(IN) 
     &              , DIMENSION (npoin_Sis) ::   NodeArea_sis    
     &                                         , x_sis 
     &                                         , y_sis
     
      INTEGER , INTENT(IN)               
     &        , DIMENSION (3)    ::  SisStartDate     ! year , month  , day
     &                             , SisStartTime     ! hours, minutes, seconds
      REAL (KIND=R8), INTENT(IN) ::  SisMorpholFactor ! morphological factor
                                         
      INTEGER, INTENT(IN) :: npoin_SisGlobal, SisGraphicOutputPeriod


#ifndef NESTOR_INTERFACES 
      !--------------------- local variables ---------------
      
                                         
      INTEGER  :: status                 
      INTEGER  :: i, n, m, sL                
      INTEGER  :: j              ! debug                
      INTEGER  :: nNodesInside           
      INTEGER,ALLOCATABLE,DIMENSION (:) :: iTmp
      LOGICAL, PARAMETER :: lrinc = .TRUE.  ! Gehoert der Rand zum Polygongebiet dazu? Hier Ja
      LOGICAL  :: NodeInside                ! is grid node inside a Polygon?
      LOGICAL  :: NoDigFieldLinked, NoDumpFieldLinked   ! is Field linked to an action
      
      CHARACTER      (128)  :: str     ! to store a value as string
      TYPE(t_String_Length) :: SRname  ! subroutine where the error occured 
      
!661   FORMAT('|',9(/,'|'))             ! 9 lines like "|          "
!663   FORMAT(' ?> error:',4(/,' ?> error:'))            ! 3 lines like "?>         "
!      dbug WRITE(6,*)'?>-------  SR InitialiseNestor ------------'
      SRname%s = "InitialiseNestor"   ! subroutine name  
      SRname%i =  17                   ! length of name string     
                                        
                                         
      WRITE(6,*)'?> NCSIZE  = ', NCSIZE     
      WRITE(6,*)'?> IPID    = ', IPID       
      WRITE(6,*)'?> NPOIN   = ', NPOIN_Sis  
      WRITE(6,*)'?> nSiCla  = ', nSiCla_Sis 
      WRITE(6,*)'?> MorFac  = ', SisMorpholFactor
      WRITE(6,*)'?> GraphOut= ', SisGraphicOutputPeriod
!                                        
!                                        
!                                        
      ipid          = ipid_Sis           !  copy value to the global Nestor-modul variable  
      npoin         = npoin_Sis          !  copy value to the global Nestor-modul variable  
      npoinGlobal   = npoin_SisGlobal    !  copy value to the global Nestor-modul variable        
                    
      nGrainClass   = nSiCla_Sis         !  copy value to the global Nestor-modul variable  
      MorpholFactor = SisMorpholFactor   !  copy value to the global Nestor-modul variable
                                         
      SisStart%year    = SisStartDate(1)   !  copy date values to          
      SisStart%month   = SisStartDate(2)   !> the Nestor-modul 
      SisStart%day     = SisStartDate(3)   !> DateTime structure
                                         
      SisStart%hour    = SisStartTime(1)   !  copy time values to 
      SisStart%minutes = SisStartTime(2)   !> the Nestor-modul 
      SisStart%seconds = SisStartTime(3)   !> DateTime structure
        
      !Restart             = SisRestart
      !WRITE(6,*)'?> Restart = ', Restart
      GraphicOutputPeriod = SisGraphicOutputPeriod
            
      IF( ncsize_Sis .GT. 1 ) ParallelComputing = .TRUE.
        
                                       
                                         
                                         
!                                        
      !DO i=1, 10                                               ! debug    
      ! WRITE(6,*)' NodeArea_sis  = ',NodeArea_sis(i)           ! debug
      ! WRITE(6,*)' x_sis = ',x_sis(i),' y_sis = ',y_sis(i)     ! debug
      !ENDDO                                                    ! debug
      !WRITE(6,*)' NodeArea_sis npoin  = ',NodeArea_sis(npoin)  ! debug
      !                                                         ! debug
      !WRITE(6,*)' npoin =', npoin                              ! debug
                                         
                                         
                                         
      CALL ReadPolygons()                
!                                        
      !DO i=1, nPolys                                  ! debug 
      !  WRITE(6,'("NAME:",A)')Poly(i)%name            ! debug
      !  WRITE(6,*)'numberPt = ',Poly(i)%nPoints       ! debug
      !  DO j=1, Poly(i)%nPoints                       ! debug
      !    WRITE(6,*)Poly(i)%Pt(j)%x, Poly(i)%Pt(j)%y  ! debug
      !  ENDDO                                         ! debug
      !ENDDO                                           ! debug

      
      nFields = nPolys                   
!                                        
      ALLOCATE( F(nFields), stat=status )
      ALLOCATE( iTmp(npoin), stat=status )
!                                        
!========================================================================
      DO n=1, nFields      !=============================================
                           !=============================================
      iTmp(:) = -1   !> initialise array iTmp
!                                        
      F(n)%name    = Poly(n)%name        
      READ(Poly(n)%name,'(I3)') F(n)%FieldID        !> read the (1:3) first string elements as integer
!                                        
!                                        
!      __________________________________________________________________
!     /________  find the Field-nodes    _______________________________/
      nNodesInside = 0                                             !> number of nodes inside
      DO i=1, npoin                      
        Call inside_point_2d_d ( y_sis(i)        , x_sis(i)        !> test if grid
     &                          ,Poly(n)                           !  nodes are inside
     &                          ,lrinc           , NodeInside    ) !  a polygon and
        IF(  NodeInside  ) THEN                                    !  store them in
          nNodesInside       = nNodesInside + 1                    !  array iTemp(:)
          iTmp(nNodesInside) = i         
        ENDIF                            
      ENDDO                              
                                        
      !> now that we know the required size for the array F(n)%Node(:)
      ALLOCATE( F(n)%Node( nNodesInside ), stat=status)
      F(n)%Node(:) = iTmp(1:nNodesInside)
      F(n)%nNodes  = nNodesInside         
!
      ALLOCATE( F(n)%NodeArea( nNodesInside ), stat=status)
      ALLOCATE( F(n)%X( nNodesInside )       , stat=status)
      ALLOCATE( F(n)%Y( nNodesInside )       , stat=status)
      DO i=1, nNodesInside     
        F(n)%NodeArea(i) = NodeArea_sis( F(n)%Node(i) )
        F(n)%X(i)        = x_sis(        F(n)%Node(i) )
        F(n)%Y(i)        = y_sis(        F(n)%Node(i) )
      ENDDO                             
!                                        
      F(n)%Area = 0.0D0    !> set value of field area in the Field-Structur
      DO i=1, F(n)%nNodes                           !> Here for the current partition.
        F(n)%Area = F(n)%Area + F(n)%NodeArea(i)    !  For parallel processing we do the
      ENDDO                                         !  summation over all partitions later
      IF ( ParallelComputing ) THEN      

        F(n)%Area = P_DSUM(F(n)%Area) !> this is the Field-area over all
                                      !  paritions which are "touched" by
                                      !  the Field
      ENDIF ! (ParallelComputing)        
!                                        

                               !=============================================
      ENDDO ! loop over Fields !=============================================
!============================================================================
!                                        
      CALL ReadDigActions() 
      
      DO m=1, nActions                                     !> adadaption due to the
        A(m)%TimeStart  = A(m)%TimeStart / MorpholFactor   !  compression of the timescale 
        A(m)%TimeEnd    = A(m)%TimeEnd   / MorpholFactor   !  through the morholgical
        A(m)%TimeRepeat = A(m)%TimeRepeat/ MorpholFactor   !  factor       
        A(m)%DigRate    = A(m)%DigRate   * MorpholFactor      
        A(m)%DumpRate   = A(m)%DumpRate  * MorpholFactor 
      ENDDO 
                             
      !--- link the Fields to the Action ---
      DO m=1, nActions  ! loop over Actions
        NoDigFieldLinked  = .TRUE.           
        NoDumpFieldLinked = .TRUE.           
        DO n=1, nFields 
          IF( A(m)%FieldDigID  == F(n)%FieldID ) THEN
            A(m)%FieldDigID  = n   ! A(m)%FieldDigID  is now the index to element of F(:)
            NoDigFieldLinked = .FALSE.      
            !CALL WriteField ( F(n) ) ! debug
          ENDIF 
          IF( A(m)%FieldDumpID == F(n)%FieldID ) THEN
            A(m)%FieldDumpID = n   ! A(m)%FieldDumpID is now the index to element of F(:)
            NoDumpFieldLinked = .FALSE.      
            !CALL WriteField ( F(n) ) ! debug
          ENDIF                          
        ENDDO ! end loop over Fields   

        IF(       NoDigFieldLinked 
     &     .AND. (A(m)%ActionType == 3 .OR. A(m)%ActionType == 1) )THEN! write error message and stop               
          str = TRIM(A(m)%FieldDig) 
          sL  = LEN_TRIM(str)
          Call ErrMsgAndStop( "while linking Action with Polygon  ", 35
     &    ,"reason: The Action defines the FieldDig: "//str(1:sL),41+sL 
     &    ,"        But the Polygon file contains no such Polygon ", 54 
     &    ,"occured in Action: ", 19, m, SRname, ipid      )
        ENDIF
                                                                        
        IF(    (        NoDumpFieldLinked 
     &            .AND. A(m)%ActionType  ==  2 )
     &     .OR.(        NoDumpFieldLinked 
     &            .AND. A(m)%ActionType  ==  1
     &            .AND. A(m)%FieldDumpID /= -1 )  ) THEN               ! write error message and stop               
          str = TRIM(A(m)%FieldDump) 
          sL  = LEN_TRIM(str)
          Call ErrMsgAndStop( "while linking Action with Polygon  ",  35
     &    ,"reason: The Action defines the FieldDump: "//str(1:sL),42+sL 
     &    ,"        But the Polygon file contains no such Polygon ",  54 
     &    ,"occured in Action: ", 19, m, SRname, ipid      )
        ENDIF
                                                                        
!                                                                       
                                         
      ENDDO  ! end loop over Actions       
      ! initialise the status of the Actions:    0 = not yet      active                   
      A(:)%State = 0                   !>        1 = currently    active
                                       !>        2 = temporary  inactive  
                                       !>        9 = for ever   inactive
                                         
                                         
!      dbug WRITE(6,*)'?>-------  SR InitialiseNestor END --------'
      RETURN                             
!***                                              ********************************************
!***                                              ********************************************
#endif NESTOR_INTERFACES                         !******************************************** 
      END SUBROUTINE InitialiseNestor            !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************