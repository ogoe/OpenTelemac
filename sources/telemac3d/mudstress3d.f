!                    ********************** 
                     SUBROUTINE MUDSTRESS3D 
!                    ********************** 
! 
     &(NGEO,FFORMAT,LAYTOCE,NCOUCH,MESH,LISTIN) 
! 
!*********************************************************************** 
! SISYPHE   V7P0                                   21/08/2010 
!*********************************************************************** 
! 
!brief    LOOKS IN GEOMETRY FILE FOR EROSION STRESSES (FOR MUD)  
!         FOR EACH BED LAYER AND READS THEM IF THEY ARE PRESENT.   
!         VALUES ARE READ IN AS STRESSES (N/M2) AND OUTPUTTED AS STRESSES 
!+ 
! 
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
! 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
! 
      USE BIEF 
      IMPLICIT NONE 
      INTEGER LNG,LU 
      COMMON/INFO/LNG,LU 
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
      TYPE(BIEF_MESH), INTENT(IN)   :: MESH       
      TYPE(BIEF_OBJ), INTENT(INOUT) :: LAYTOCE 
      INTEGER, INTENT(IN)           :: NGEO 
      INTEGER, INTENT(IN)           :: NCOUCH 
      LOGICAL, INTENT(IN)           :: LISTIN 
      CHARACTER(LEN=8), INTENT(IN)  :: FFORMAT 
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
      INTEGER :: ERR 
      DOUBLE PRECISION :: BID 
      REAL, ALLOCATABLE :: W(:) 
      LOGICAL :: OK 
      CHARACTER(LEN=16) :: VARNAME 
      INTEGER :: I 
! 
!----------------------------------------------------------------------- 
! 
      ALLOCATE(W(MESH%NPOIN),STAT=ERR) 
      IF(ERR.NE.0) THEN 
        IF(LNG.EQ.1) WRITE(LU,*) 'MUDSTRESS3D: MAUVAISE ALLOCATION DE W'
        IF(LNG.EQ.2) WRITE(LU,*) 'MUDSTRESS3D: WRONG ALLOCATION OF W'
        CALL PLANTE(1) 
        STOP 
      ENDIF 
! 
!----------------------------------------------------------------------- 
! 
!     ASSUMES THAT THE FILE HEADER LINES HAVE ALREADY BEEN READ 
!     WILL START READING THE RESULT RECORDS 
! 
!----------------------------------------------------------------------- 
! 
!     INITIALISE 
      OK = .FALSE. 
! 
!----------------------------------------------------------------------- 
! 
!     LOOKS FOR 1. CRITICAL SHEAR STRESS FOR EROSION PER LAYER 
!     IN THE GEOMETRY FILE: 
!     VARIABLE NAMES ARE ERO SHEAR1, ERO SHEAR2, etc....FOR EACH LAYER 
!     UP TO NCOUCH 
! 
      DO I=1,NCOUCH 
! 
!       MAKE THE NUMBERED NAME STRING 
! 
        WRITE(VARNAME,'(A9,I0)')  'ERO SHEAR',I  
!           
        CALL FIND_IN_SEL(LAYTOCE%ADR(I)%P, 
     &                   VARNAME,NGEO,FFORMAT,W,OK,TIME=BID) 
!       
        IF (OK) WRITE(LU,*)  
     &    'EROSION SHEAR (LAYER', I, ') FOUND IN GEOMETRY FILE' 
        IF (.NOT. OK) WRITE(LU,*)  
     &    'EROSION SHEAR (LAYER', I, ') NOT FOUND IN GEOMETRY FILE' 
 
!       RESET FIND FLAG: 
        OK = .FALSE. 
! 
      ENDDO 
!       
      DEALLOCATE(W) 
! 
!----------------------------------------------------------------------- 
! 
      RETURN 
      END
