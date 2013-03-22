!                    ****************************************
                     SUBROUTINE CVSP_P (PATH_PRE,FILE_PRE,JG)
!                    ****************************************
!
!
!***********************************************************************
! SISYPHE   V6P3                                   14/03/2013
!***********************************************************************
!
!brief   CSV-FILE OUTPUT OF A VERTICAL SORTING PROFILE IN POINT J
!
!history UWE MERKEL
!+        2011-07-20
!+
!history  P. A. TASSI (EDF R&D, LNHE)
!+        12/03/2013
!+        V6P3
!+   Cleaning, cosmetic
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| JG             |<--| GLOBAL POINT NUMBER
!| PATH_PRE       |<--| WHERE TO SAVE
!| FILE_PRE       |<--| FILENAMETRUNK
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_SISYPHE
      USE BIEF
      USE BIEF_DEF
      USE CVSP_OUTPUTFILES
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER     , INTENT(IN)  :: JG
      CHARACTER(*), INTENT(IN) :: PATH_PRE
      CHARACTER(*), INTENT(IN) :: FILE_PRE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER*100, DEBUGFILE
      CHARACTER*5, OCSTR
      INTEGER  I, K, J, J2
      DOUBLEPRECISION AT, BSUM, LEVELBELOW
!
!----------------------------------------------------------------------- 
!
      AT = DT*LT/PERCOU
      J = JG
      OUTPUTCOUNTER = OUTPUTCOUNTER + 1
      
!     GLOBAL NUMBERS TO GLOBAL NUMBERS
      IF (NCSIZE > 1) THEN
         J = MESH%KNOGL%I(JG)
      ENDIF
      
      WRITE(UNIT=OCSTR, FMT='(I5)')
     &     OUTPUTCOUNTER
      DO I=1,5
         IF(OCSTR(I:I)==' ') OCSTR(I:I)='0'
      END DO
      
      WRITE(UNIT=DEBUGFILE, FMT='(A,A,A,A,I8,A,G15.8,A)')
     &     PATH_PRE,OCSTR,'_',FILE_PRE,
     &     JG,'_T_',AT,'.VSP.CSV'

      DO I=1,LEN_TRIM(DEBUGFILE)
         IF(DEBUGFILE(I:I)==' ') DEBUGFILE(I:I)='_'
      END DO
      
      
      IF(J > 0) THEN
         OPEN(81, FILE=DEBUGFILE, STATUS='UNKNOWN' )
         REWIND 81
       WRITE(81,*)"J K FD50(I) AT PRO_D(K_I) PRO_F(K_I) X Y D50 ALT T H"
         
       DO K=1,PRO_MAX(J)
          BSUM = 0.D0
          DO I=1,NSICLA
             BSUM = FDM(I)*PRO_F(J,PRO_MAX(J)+1-K,I) + BSUM
          ENDDO
          
          DO I=1,NSICLA
             IF (K.EQ.1) THEN
! FULL OUTPUT WITH COORDINATES ETC. ON SURFACE
                WRITE (81,'(I8,1X,I4,1X,10(G20.12,1X))')
     &               JG,PRO_MAX(J)+1-K,FDM(I),AT,
     &               PRO_D(J,PRO_MAX(J)+1-K,I),
     &               PRO_F(J,PRO_MAX(J)+1-K,I),X(J),Y(J),
     &               BSUM,ES(J,1),TOB%R(J), Z%R(J) !UHM
                  
             ELSE
! FOLLOWING SECTIONS
                WRITE (81,'(I8,1X,I4,1X,5(G20.12,1X))')
     &               JG,PRO_MAX(J)+1-K,FDM(I),AT,
     &               PRO_D(J,PRO_MAX(J)+1-K,I),
     &               PRO_F(J,PRO_MAX(J)+1-K,I)
             ENDIF
          ENDDO
       ENDDO
       
       BSUM = 0.D0
       DO I=1,NSICLA
          BSUM = FDM(I)*PRO_F(J,1,I) + BSUM
       ENDDO
       
       CLOSE(81)
       
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CVSP_P
