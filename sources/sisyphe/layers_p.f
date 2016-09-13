!                    *******************
                     SUBROUTINE LAYERS_P
!                    *******************
!
     &(PATH_PRE,JG)
!
!***********************************************************************
! SISYPHE   V6P2                                   21/06/2011
!***********************************************************************
!
!BRIEF   .CSV-FILE OUTPUT OF A LAYER PROFILE IN POINT J
!
!HISTORY  UWE MERKEL
!+        2011-07-20
!+
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| JG             |<--| GLOBAL POINT NUMBER
!| PATH_PRE       |<--| WHERE TO SAVE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SISYPHE
      USE BIEF
      USE BIEF_DEF
      !
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,          INTENT(IN)    :: JG
      CHARACTER(*),        INTENT(IN )    :: PATH_PRE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=100) :: DEBUGFILE
      INTEGER  I,K,J
      DOUBLE PRECISION DEPTH, AT, MYFRA, BSUM
      INTEGER :: ID
!
!----------------------------------------------------------------
!
      AT = DT*LT/PERCOU
!
!     GLOBAL NUMBERS TO GLOBAL NUMBERS
      J = JG
!     NOTE JMH : KNOGL WILL BE SUPPRESSED IN FUTURE
      IF(NCSIZE>1) J = GLOBAL_TO_LOCAL_POINT(JG,MESH)
!
      WRITE(UNIT=DEBUGFILE, FMT='(A,I8,A,G15.8,A)')
     &      PATH_PRE,JG,'_T_',AT,'.LAY.CSV'
      DO I=1,38
        IF(DEBUGFILE(I:I)==' ') DEBUGFILE(I:I)='_'
      END DO
!
      IF(J > 0) THEN !0 IF NODE IS NOT ON THIS PARTITION
        CALL GET_FREE_ID(ID)
        OPEN(ID, FILE=DEBUGFILE , STATUS='UNKNOWN')
        REWIND ID
        WRITE(ID,*)"J K FD50(I) AT Z AVAIL(J,K,I) X Y D50 TAU H"
!
        DEPTH = ZF%R(J)
!
        !LAYER TOP
        DO K=1,NLAYER%I(J)
!
          BSUM = 0.D0
          DO I=1,NSICLA
            BSUM = FDM(I)*AVAIL(J,K,I) + BSUM
          ENDDO
!
          DO I=1,NSICLA
            WRITE (ID,'(I8,1X,I4,1X,7(G15.8,1X))')
     &      JG,(NLAYER%I(J)-K+1),FDM(I),AT,DEPTH,
     &      AVAIL(J,K,I),X(J),Y(J), BSUM, TOB%R(J), Z%R(J)
          ENDDO
            DEPTH = DEPTH - ES(J,K)

        ENDDO
!
!     RIGID BED
!
      DO I=1,NSICLA
        BSUM = FDM(I)*AVAIL(J,NLAYER%I(J),I) + BSUM
      ENDDO
!
      DO I=1,NSICLA
            MYFRA = 0.D0
            IF (I==1) MYFRA = 1.D0
            WRITE (ID,'(I8,1X,I4,1X,7(G15.8,1X))')
     &      JG,0,FDM(I),AT,DEPTH,MYFRA,X(J),Y(J),BSUM
      ENDDO
!
      CLOSE(ID)
      ENDIF
!
!----------------------------------------------------------------
!
      RETURN
      END SUBROUTINE LAYERS_P
