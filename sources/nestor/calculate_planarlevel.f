      SUBROUTINE  Calculate_PlanarLevel          !********************************************
!***                                              ********************************************
!***                                              ********************************************
     & ( Fld, TargetVolume, doWhat )

      USE m_TypeDefs_Nestor
      USE m_Nestor , ONLY : ParallelComputing, ipid

      IMPLICIT NONE

      TYPE(t_Field) ,INTENT(INOUT) :: Fld
      REAL (KIND=8) ,INTENT(IN)    :: TargetVolume   !  DumpVolume (doWhat = 1)
                                                     !> DigVolume  (doWhat = 2)
      INTEGER       ,INTENT(IN)    :: doWhat

      DOUBLE PRECISION     P_DSUM
      EXTERNAL             P_DSUM

      REAL (KIND=8)    :: Vol, offset, step, accuracy
      INTEGER          :: i, iteration, iBigger, iSmaller, iOld

      REAL (KIND=8), ALLOCATABLE, DIMENSION(:) ::   testZ

      TYPE(t_String_Length) :: SRname ! name of current Subroutine
      TYPE(t_String_Length) :: ErrMsge! error message

!663   FORMAT(' ?>',2(/,' ?>'))            ! 3 lines like "?>         "
      WRITE(6,*)'?>-------  SR Calculate_PlanarLevel --------'
      SRname%s = "Calculate_PlanarLevel"  ! subroutine name
      SRname%i =  21                      ! length of name string

      ALLOCATE( testZ( Fld%nNodes ) )
      iteration = 0
      iBigger   = 1
      iOld      = 1
      offset    = 1.0D0   !   0.5D0
      step      = 0.5D0
      accuracy  = TargetVolume * 1.0E-8_8
      IF( doWhat == 1) THEN
        DO   !  Iteration loop to find the level (testZ) below the reference level
             !> The empty volume between the level and bottom must meet with the TargetVolume.
             !> It is the volume which will be filled up through dumping material.
             !> The final result is the corresponding dz for each node of the field.
          !WRITE(6,*)'-------iteration = ',iteration       ! debug
          !WRITE(6,*)'offset = ',offset, step              ! debug
          iteration = iteration + 1
          Vol = 0.0D0
          DO i=1, Fld%nNodes  ! calculate empty volume below the level testZ
            testZ(i) = Fld%refZ(i) - offset
            IF( Fld%Z(i) <= testZ(i) ) THEN
              Fld%dZ(i) = testZ(i) - Fld%Z(i)
              Vol = Vol +  Fld%NodeArea(i) * Fld%dZ(i)
            ENDIF
          ENDDO
          IF(ParallelComputing) Vol = P_DSUM(Vol)
          !WRITE(6,*) 'Vol = ',Vol,TargetVolume             ! debug

          !------- end of iteration reached ? ----------------------------
          IF( ABS(Vol - TargetVolume) <= accuracy ) THEN !  end of iteration
            DO i=1, Fld%nNodes                           !> reached
              IF( Fld%Z(i) <= testZ(i) ) THEN
                Fld%dZ(i) = testZ(i) - Fld%Z(i)
              ELSE
                Fld%dZ(i) = 0.0D0
              ENDIF
            ENDDO
            WRITE(6,*)' num of iterations to find z-level = ',iteration
            WRITE(6,*)' volume below planar z-level       = ', Vol
            WRITE(6,*)' compare to preset dump volume     = '
     &                , TargetVolume
            EXIT   ! exit iteration loop to find dz
          ENDIF  !-------------------------------------------------------

          IF( Vol > TargetVolume )THEN
            IF( iBigger .NE. iOld )  step = step * 0.5D0  !  because the direction of
            iBigger  = 1                                  !> approximation changed we
            iOld     = iBigger                            !> reduce the iteration step
            offset   = offset  +  step
          ENDIF

          IF( Vol < TargetVolume )THEN
            IF( iSmaller .NE. iOld ) step = step * 0.5D0  !  because the direction of
            iSmaller  = -1                                !> approximation changed we
            iOld      = iSmaller                          !> reduce the iteration step
            offset    = offset  -  step
          ENDIF

          !------------loop termination-----------------------------------
          IF(iteration > 200) THEN
            ErrMsge%s( 1:36) = "max iterations reached to calculate "
            ErrMsge%s(37:67) = "planar dump level: iteration = "
            ErrMsge%i = 67
            CALL ErrMsgAndStop( ErrMsge, iteration, SRname, ipid   )
          ENDIF
          !---------------------------------------------------------------
        ENDDO  ! iteration loop to find dz
      ENDIF ! doWhat == 1

      IF( doWhat == 2) THEN     ! noch in Arbeit !!!!
        DO   !  Iteration loop to find the level (testZ) below the reference level
             !> The (gravel or sand) volume between the level and bottom line must
             !> meet with the TargetVolume.
             !> It is the volume which will be removed through digging.
             !> The final result is the corresponding dz for each node of the field.
          !WRITE(6,*)'-------iteration = ',iteration       ! debug
          !WRITE(6,*)'offset = ',offset, step              ! debug
          iteration = iteration + 1
          Vol = 0.0D0
          DO i=1, Fld%nNodes  ! calculate empty volume below the level testZ
            testZ(i) = Fld%refZ(i) - offset
            IF( Fld%Z(i) >= testZ(i) ) THEN
              Fld%dZ(i) = testZ(i) - Fld%Z(i)
              Vol = Vol +  Fld%NodeArea(i) * Fld%dZ(i)
            ENDIF
          ENDDO
          IF(ParallelComputing) Vol = P_DSUM(Vol)
          !WRITE(6,*) 'Vol = ',Vol,TargetVolume             ! debug

          !------- end of iteration reached ? ----------------------------
          IF( ABS(Vol - TargetVolume) <= accuracy ) THEN !  end of iteration
            DO i=1, Fld%nNodes                           !> reached
              IF( Fld%Z(i) >= testZ(i) ) THEN
                Fld%dZ(i) = testZ(i) - Fld%Z(i)
              ELSE
                Fld%dZ(i) = 0.0D0
              ENDIF
            ENDDO
            WRITE(6,*)' num of iterations to find z-level = ',iteration
            WRITE(6,*)' volume below planar z-level       = ', Vol
            WRITE(6,*)' compare to preset dump volume     = '
     &                , TargetVolume
            EXIT   ! exit iteration loop to find dz
          ENDIF  !-------------------------------------------------------

          IF( Vol > TargetVolume )THEN
            IF( iBigger .NE. iOld )  step = step * 0.5D0  !  because the direction of
            iBigger  = 1                                  !> approximation changed we
            iOld     = iBigger                            !> reduce the iteration step
            offset   = offset  +  step
          ENDIF

          IF( Vol < TargetVolume )THEN
            IF( iSmaller .NE. iOld ) step = step * 0.5D0  !  because the direction of
            iSmaller  = -1                                !> approximation changed we
            iOld      = iSmaller                          !> reduce the iteration step
            offset    = offset  -  step
          ENDIF

          !------------loop termination-----------------------------------
          IF(iteration > 200) THEN
            ErrMsge%s( 1:36) = "max iterations reached to calculate "
            ErrMsge%s(37:67) = "planar dump level: iteration = "
            ErrMsge%i = 67
            CALL ErrMsgAndStop( ErrMsge, iteration, SRname, ipid   )
          ENDIF
          !---------------------------------------------------------------
        ENDDO  ! iteration loop to find dz
      ENDIF ! doWhat == 1

      DEALLOCATE( testZ )


      WRITE(6,*)'?>-------  SR Calculate_PlanarLevel End ----'
!
      RETURN
!***                                              ********************************************
!***                                              ********************************************
      END SUBROUTINE Calculate_PlanarLevel       !********************************************
!***                                              ********************************************
!***                                              ********************************************
!*********************************************************************************************
!*********************************************************************************************

!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***                                              ********************************************
