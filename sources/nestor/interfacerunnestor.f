      SUBROUTINE  InterFaceRunNestor             !********************************************
!***                                              ********************************************
!***                                              ********************************************
!     & (  NPOIN, NSICLA
!     &  , LT, DTS, AT0, ELAY0, ZF, ZFCL_C, AVAIL
!     &  , KNOLG, KNOGL, sizeKNOGL, meshX, meshY )
!    & (  NPOIN, NSICLA
!    &  , LT, DTS, AT0, ELAY0, ZF, ZFCL_C, AVAIL
!    &  , KNOLG, KNOGL, sizeKNOGL )
     & (  NPOIN, NSICLA
     &  , LT, DTS, AT0, ELAY0, ZF, ZFCL_C, AVAIL
     &  , KNOLG )
!
      USE BIEF
      USE m_TypeDefs_InterFace

!
!      USE DECLARATIONS_SISYPHE, ONLY : MESH
      IMPLICIT NONE
!
      INTEGER       ,INTENT(IN)    ::   NPOIN    ! Number of points (Nodes)
     &                                , NSICLA   ! Number of SIze CLAsses (=nGrainClass)
!    &                                , sizeKNOGL! size of array KNOGL
      INTEGER       ,INTENT(IN)    :: LT         ! time-step Telemac
      REAL (KIND=8) ,INTENT(IN)    :: DTS, AT0   ! time-step-duration , time
      REAL (KIND=8) ,INTENT(IN)    :: ELAY0      ! thickness of ActivLayer
!     REAL (KIND=8) ,INTENT(IN)
!    &          ,DIMENSION (NPOIN) :: meshX, meshY ! x-, y- coordinate
!    &                               ,ZF           ! current bottom (at time AT0)
      REAL (KIND=8) ,INTENT(IN)
     &          ,DIMENSION (NPOIN) :: ZF           ! current bottom (at time AT0)
!     TYPE(BIEF_OBJ),INTENT(IN)    :: ZF         ! current bottom (at time AT0)
      TYPE(BIEF_OBJ),INTENT(INOUT) :: ZFCL_C      ! bedload evolution for each sediment class
!
      TYPE( t_PointerToArrayOfReals )
     & , ALLOCATABLE, SAVE , DIMENSION (:)      :: dzCL_sis

      REAL (KIND=8) ,INTENT(IN)
     &              ,DIMENSION (NPOIN,1,NSICLA) :: AVAIL

      INTEGER       ,INTENT(IN)
     &              ,DIMENSION (NPOIN)          :: KNOLG ! index list: Local to Global node index
!      INTEGER       ,INTENT(IN)
!     &              ,DIMENSION (sizeKNOGL)      :: KNOGL ! index list: Global to Local node index




      !---------- local variables ---------------------
      INTEGER          :: i, status

      LOGICAL , SAVE   ::  firstTime_InterFaceRunNestor = .TRUE.

!       REAL (KIND=8), POINTER, DIMENSION (:)  :: ptr2

      !  assumed-shape arrays:  Übergabe von Datenfeldern an Unterprogramme
      !> ohne Größenangaben(engl. assumed-shape arrays). Dies funktioniert
      !> nur wenn in der aufrufenden Programmeinheit der interface-Block für
      !> das Unterprogramm angeführt wird.
      INTERFACE !-----------------------------------------------------+
        SUBROUTINE MainNestor                                        !
!    &    (   ts, dts, time, ELAY0, z_sis, x_sis, y_sis               !
!    &      , AVAIL, KNOLG, KNOGL, dzCL_sis                )          !
!    &    (   ts, dts, time, ELAY0, z_sis                             !
!    &      , AVAIL, KNOLG, KNOGL, dzCL_sis                )          !
     &    (   ts, dts, time, ELAY0, z_sis                             !
     &      , AVAIL, KNOLG, dzCL_sis                       )          !
                                                                      !
          USE m_TypeDefs_InterFace                                    !
                                                                      !
          IMPLICIT NONE                                               !
                                                                      !
          INTEGER       ,INTENT(IN)    ::  ts             ! time-step
          REAL (KIND=8) ,INTENT(IN)    :: dts             ! time-step-duration  [ s ]
          REAL (KIND=8) ,INTENT(IN)    :: time            ! time [ s ]
          REAL (KIND=8) ,INTENT(IN)    :: ELAY0           ! activLayerThickness  [ m ]
          REAL (KIND=8) ,INTENT(IN)    :: z_sis (:)       ! current bottom [ m+NN ] assumed-shape array
         !REAL (KIND=8) ,INTENT(IN)    :: x_sis (:)       ! x-koord [ m ] assumed-shape array
         !REAL (KIND=8) ,INTENT(IN)    :: y_sis (:)       ! y-koord [ m ] assumed-shape array
          REAL (KIND=8) ,INTENT(IN)    :: AVAIL (:,:,:)   ! assumed-shape array
          INTEGER       ,INTENT(IN)    :: KNOLG (:)       ! index list: Local to Global node index
         !INTEGER       ,INTENT(IN)    :: KNOGL (:)       ! index list: Global to Local node index
          TYPE( t_PointerToArrayOfReals )                             !
     &                  ,INTENT(INOUT) :: dzCL_sis  (:)               !
        END SUBROUTINE MainNestor                                    !
      END INTERFACE !-------------------------------------------------+
!
!663   FORMAT(' ?>',2(/,' ?>'))            ! 3 lines like "?>         "
      WRITE(6,*)'?>-------  SR InterFaceRunNestor ----------'
      !WRITE(6,*)' LT       = ', LT
      !WRITE(6,*)' DTS      = ', DTS
      !WRITE(6,*)' AT0      = ', AT0
      !WRITE(6,*)' ZF_C(12) = ', ZF_C%R(12)
      !WRITE(*,*)'?> KNOLG ', 1, KNOLG(1)


      IF( firstTime_InterFaceRunNestor ) THEN
        WRITE(6,*)'?> firstTime_InterFaceRunNestor'  ! debug/test output
        ALLOCATE( dzCL_sis( NSICLA ), stat=status )
        DO i=1, NSICLA                             !> Set pointers to a sub-ranges of an BIEF-object.
          dzCL_sis(i)%R => ZFCL_C%ADR(i)%P%R       !  Now we have access to parts of the BIEF-object
        ENDDO                                      !  without using BIEF stuff for further calls
        firstTime_InterFaceRunNestor = .FALSE.    !  without copying data
      ENDIF

      !dzCL_sis(1)%R(100) = 0.333D0
      !rDummy =   dzCL_sis(1)%R(100)
      !WRITE(6,*)' rDummy = ', rDummy
      !rDummy =   dzCL_sis(2)%R(129)
      !WRITE(6,*)' rDummy = ', rDummy
!
!
!      CALL MainNestor(   LT, DTS, AT0, ELAY0, ZF, meshX, meshY
!     &                  , AVAIL, KNOLG, KNOGL, dzCL_sis  ) !
      CALL MainNestor(   LT, DTS, AT0, ELAY0, ZF
     &                  , AVAIL, KNOLG, dzCL_sis  ) !

!
      WRITE(6,*)'?>-------  SR InterFaceRunNestor End ------'
!
      RETURN
!**                                               ********************************************
!**                                               ********************************************
      END SUBROUTINE InterFaceRunNestor          !********************************************
!**                                               ********************************************
!**                                               ********************************************
!*********************************************************************************************
!*********************************************************************************************





!*********************************************************************************************
!*********************************************************************************************
!***                                              ********************************************
!***  *****************                           ********************************************
