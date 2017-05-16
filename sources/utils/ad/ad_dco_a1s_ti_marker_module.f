!!!    module AD_DCO_A1S_TI_MARKER_MODULE
!!!     A module that allows to store tape indicess for
!!!       later extraction of intenrmediate adjoints
!!!
!!!       Assumption :  contigouous storage
!!!   JR 2016


#if defined COMPAD    /* do that only for active compilation */

      MODULE DCO_A1S_TI_MARKER_LIST_MODULE
        USE DCO_A1S_COMMON
        IMPLICIT NONE
        TYPE DCO_A1S_TI_MARKER_LIST_ELEM
           INTEGER(DCO_A1S_TAPE_IKND)     :: frst, size
!           INTEGER                        :: tstp
        END type DCO_A1S_TI_MARKER_LIST_ELEM
        TYPE DCO_A1S_TI_MARKER_LIST_TYPE
           TYPE(DCO_A1S_TI_MARKER_LIST_ELEM), DIMENSION(:), ALLOCATABLE  :: data
        END type DCO_A1S_TI_MARKER_LIST_TYPE
      CONTAINS
        SUBROUTINE DCO_A1S_TI_MARKER_LIST_SETUP( TIM, NIT )
          TYPE(DCO_A1S_TI_MARKER_LIST_TYPE) :: TIM
          INTEGER, INTENT(IN)          :: NIT
          PRINT *,'DCO_A1S_TI_MARKER_LIST_SETUP( ', NIT,' )'
          IF ( NIT < 0 ) THEN
             PRINT *,'ERROR: DCO_A1S_TI_MARKER_LIST_SETUP :: NIT (',
     $            NIT,') < 0 !!'
             STOP
          END IF
          ALLOCATE( TIM%data(0:NIT))
          TIM%data = DCO_A1S_TI_MARKER_LIST_ELEM(0,0)
        END SUBROUTINE DCO_A1S_TI_MARKER_LIST_SETUP
        SUBROUTINE DCO_A1S_TI_MARKER_LIST_CLEAN( TIM )
          TYPE(DCO_A1S_TI_MARKER_LIST_TYPE) :: TIM
          DEALLOCATE( TIM%data )
        END SUBROUTINE DCO_A1S_TI_MARKER_LIST_CLEAN
        SUBROUTINE DCO_A1S_TI_MARKER_LIST_STORE( TIM, tstp, frst, size )
          TYPE(DCO_A1S_TI_MARKER_LIST_TYPE) :: TIM
          INTEGER,                     INTENT(IN)  :: tstp
          INTEGER(DCO_A1S_TAPE_IKND),  INTENT(IN)  :: frst, size
          TIM%data(tstp) = DCO_A1S_TI_MARKER_LIST_ELEM( frst, size )
        END SUBROUTINE DCO_A1S_TI_MARKER_LIST_STORE
        SUBROUTINE DCO_A1S_TI_MARKER_LIST_GET( TIM, tstp, frst, size )
          TYPE(DCO_A1S_TI_MARKER_LIST_TYPE),INTENT(IN) :: TIM
          INTEGER,                     INTENT(IN)  :: tstp
          INTEGER(DCO_A1S_TAPE_IKND),  INTENT(OUT)  :: frst, size
          frst  = TIM%data(tstp)%frst
          size  = TIM%data(tstp)%size
        END SUBROUTINE DCO_A1S_TI_MARKER_LIST_GET
        SUBROUTINE DCO_A1S_TI_MARKER_LIST_PRINT( TIM )
          TYPE(DCO_A1S_TI_MARKER_LIST_TYPE),INTENT(IN)  :: TIM
          INTEGER                                  :: i
          PRINT *,'TIMdata  : ',SIZE(TIM%data,1),' elements'
          DO i = 0, SIZE(TIM%data)-1
             PRINT *,'TIMdata ',i,' : ', TIM%data(i)%frst,
     $            ' -> ',TIM%data(i)%frst+TIM%data(i)%size,
     $            '  ( ',TIM%data(i)%size,' )'
          END DO
        END SUBROUTINE DCO_A1S_TI_MARKER_LIST_PRINT
      END MODULE DCO_A1S_TI_MARKER_LIST_MODULE

      MODULE DCO_A1S_TI_MARKER_MODULE
        USE DCO_A1S_TI_MARKER_LIST_MODULE
        IMPLICIT NONE
        PRIVATE
        PUBLIC :: DCO_A1S_TI_MARKER
        PUBLIC :: DCO_A1S_TI_MARKER_SETUP, DCO_A1S_TI_MARKER_CLEAN
        PUBLIC :: DCO_A1S_TI_MARKER_STORE, DCO_A1S_TI_MARKER_GET
        PUBLIC :: DCO_A1S_TI_MARKER_GET_FIRST, DCO_A1S_TI_MARKER_GET_SIZE
        PUBLIC :: DCO_A1S_TI_MARKER_PRINT
        TYPE(DCO_A1S_TI_MARKER_LIST_TYPE), DIMENSION(:), ALLOCATABLE :: DCO_A1S_TI_MARKER
      CONTAINS
        SUBROUTINE DCO_A1S_TI_MARKER_SETUP( NTIM, NIT )
          INTEGER, INTENT(IN)          :: NTIM
          INTEGER, INTENT(IN)          :: NIT
          INTEGER                      :: i
          PRINT *,'Called : DCO_A1S_TI_MARKER_SETUP(',NTIM,',',NIT,')'
          IF ( NTIM < 1 ) THEN
             PRINT *,'ERROR: DCO_A1S_TI_MARKER_SETUP :: NTIM (',
     $            NTIM,') < 1 !!'
             STOP
          END IF
          IF ( NIT < 0 ) THEN
             PRINT *,'ERROR: DCO_A1S_TI_MARKER_SETUP :: NIT (',
     $            NIT,') < 0 !!'
             STOP
          END IF
          ALLOCATE( DCO_A1S_TI_MARKER(NTIM) )
          DO i = 1, NTIM
             CALL DCO_A1S_TI_MARKER_LIST_SETUP( DCO_A1S_TI_MARKER(i), NIT )
          END DO
        END SUBROUTINE DCO_A1S_TI_MARKER_SETUP
        SUBROUTINE DCO_A1S_TI_MARKER_CLEAN( )
          INTEGER                      :: i
          PRINT *,'Called : DCO_A1S_TI_MARKER_CLEAN '
          DO i = 1, SIZE(DCO_A1S_TI_MARKER)
             CALL DCO_A1S_TI_MARKER_LIST_CLEAN( DCO_A1S_TI_MARKER(i) )
          END DO
          IF ( ALLOCATED( DCO_A1S_TI_MARKER ) ) DEALLOCATE( DCO_A1S_TI_MARKER )
        END SUBROUTINE DCO_A1S_TI_MARKER_CLEAN
        SUBROUTINE ARG_CHECK( TIM, tstp, who )
          INTEGER,                     INTENT(IN)  :: TIM, tstp
          CHARACTER(LEN=*),            INTENT(IN)  :: who
          IF ( TIM < 1 .OR. TIM > SIZE(DCO_A1S_TI_MARKER,1) ) THEN
             PRINT *,'ERROR: DCO_A1S_TI_MARKER_'//who//' :: INVALID TIM NUMBER ',
     $            TIM, ' !!'
             STOP
          END IF
          IF ( tstp < 0 .OR. tstp > SIZE(DCO_A1S_TI_MARKER(TIM)%data,1) ) THEN
             PRINT *,'ERROR: DCO_A1S_TI_MARKER_'//who//' :: INVALID TSTP NUMBER ',
     $             tstp,'                    !!'
             STOP
          END IF
        END SUBROUTINE ARG_CHECK
        SUBROUTINE DCO_A1S_TI_MARKER_STORE( TIM, tstp, frst, sze )
          INTEGER,                     INTENT(IN)  :: TIM, tstp
          TYPE(DCO_A1S_TYPE),          INTENT(IN)  :: frst
          INTEGER(DCO_A1S_TAPE_IKND),  INTENT(IN)  :: sze
!          INTEGER(DCO_A1S_TAPE_IKND),  INTENT(IN)  :: frst, sze
          PRINT *,'Called : DCO_A1S_TI_MARKER_STORE(',TIM,',',tstp,',',frst,',',sze,')'
          CALL ARG_CHECK( TIM, tstp, 'STORE' )
          CALL DCO_A1S_TI_MARKER_LIST_STORE( DCO_A1S_TI_MARKER(TIM), tstp, frst%j, sze )
        END SUBROUTINE DCO_A1S_TI_MARKER_STORE
        SUBROUTINE DCO_A1S_TI_MARKER_GET( TIM, tstp, frst, size )
          INTEGER,                     INTENT(IN)   :: TIM, tstp
          INTEGER(DCO_A1S_TAPE_IKND),  INTENT(OUT)  :: frst, size
          CALL ARG_CHECK( TIM, tstp, 'GET' )
          CALL DCO_A1S_TI_MARKER_LIST_GET( DCO_A1S_TI_MARKER(TIM), tstp, frst, size )
          PRINT *,'Called : DCO_A1S_TI_MARKER_GET(',TIM,',',tstp,',->',frst,',->',size,')'
        END SUBROUTINE DCO_A1S_TI_MARKER_GET
        FUNCTION DCO_A1S_TI_MARKER_GET_FIRST( TIM, tstp ) RESULT(res)
          INTEGER,                     INTENT(IN)   :: TIM, tstp
          INTEGER(DCO_A1S_TAPE_IKND)                :: frst, size
          INTEGER(DCO_A1S_TAPE_IKND)                :: res
          CALL ARG_CHECK( TIM, tstp, 'GET_FIRST' )
          CALL DCO_A1S_TI_MARKER_LIST_GET( DCO_A1S_TI_MARKER(TIM), tstp, frst, size )
          res = frst
          !!PRINT *,'Called : DCO_A1S_TI_MARKER_GET_FIRST(',TIM,',',tstp,') ->',res
        END FUNCTION DCO_A1S_TI_MARKER_GET_FIRST
        FUNCTION DCO_A1S_TI_MARKER_GET_SIZE( TIM, tstp ) RESULT(res)
          INTEGER,                     INTENT(IN)   :: TIM, tstp
          INTEGER(DCO_A1S_TAPE_IKND)                :: frst, size
          INTEGER(DCO_A1S_TAPE_IKND)                :: res
          CALL ARG_CHECK( TIM, tstp, 'GET_SIZE' )
          CALL DCO_A1S_TI_MARKER_LIST_GET( DCO_A1S_TI_MARKER(TIM), tstp, frst, size )
          res = size
          !!PRINT *,'Called : DCO_A1S_TI_MARKER_GET_FIRST(',TIM,',',tstp,') ->',res
        END FUNCTION DCO_A1S_TI_MARKER_GET_SIZE
        SUBROUTINE DCO_A1S_TI_MARKER_PRINT( TIM )
          INTEGER, INTENT(IN)                       :: TIM
          IF ( TIM < 1 .OR. TIM > SIZE(DCO_A1S_TI_MARKER,1) ) THEN
             PRINT *,'ERROR: DCO_A1S_TI_MARKER_PRINT :: INVALID TIM NUMBER ',
     $            TIM, ' !!'
             STOP
          END IF
          CALL DCO_A1S_TI_MARKER_LIST_PRINT( DCO_A1S_TI_MARKER(TIM) )
        END  SUBROUTINE DCO_A1S_TI_MARKER_PRINT
      END MODULE DCO_A1S_TI_MARKER_MODULE

#endif /* COMPAD    ! do that only for active compilation */
