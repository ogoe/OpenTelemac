!                       *****************************
                        SUBROUTINE CVSP_WRITE_PROFILE
!                       *****************************
!
!***********************************************************************
! SISYPHE   V6P3                                   14/04/2013
!***********************************************************************
!
!brief  CVSP_WRITE_PROFILE
!
!history UWE MERKEL
!+        20/07/2011
!+        V6P3
!+
!
!history PAT (PABLO TASSI)
!+        2012-08-24
!+        V6P3
!+ Add write(LU,*)
!+ Secure programming
!
!history  P. A. TASSI (EDF R&D, LNHE)
!+        12/03/2013
!+        V6P3
!+   Cleaning, cosmetic
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| -              |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE CVSP_OUTPUTFILES
      USE DECLARATIONS_SISYPHE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,K
      DOUBLE PRECISION BSUM, SUMERR, AT
!
!-----------------------------------------------------------------------
!
      WRITE(LU,*) 'CVSP_WRITE_PROFILE ON CPU ', IPID
!
!-----------------------------------------------------------------------
! CHOOSE WHAT YOU WANT TO HAVE IN VSPRES
! NUMBER OF PRINTOUT VARIABLES
!-----------------------------------------------------------------------
!
      URBLOC%N = 3 + NSICLA       ! NUMVARUR3D2RES
!      
      AT = DT*LT/PERCOU
!      
      DO I= 0, PRO_MAX_MAX-1
        DO J= 1, NPOIN         ! D50
          BSUM = 0.D0
          SUMERR = 1.D0
          DO K=1,NSICLA
            BSUM = FDM(K)*PRO_F(J,I+1,K) + BSUM
            SUMERR = SUMERR - PRO_F(J,I+1,K)
          ENDDO            
          IF ((I+1).LE.PRO_MAX(J)) THEN
            VSP_ERROR%R(J+I*NPOIN) = SUMERR
            VSP_D50%R(J+I*NPOIN) = BSUM
            VSP_D%R(J+I*NPOIN) = PRO_D(J,I+1,1)
          ELSE
            VSP_ERROR%R(J+I*NPOIN) = VSP_ERROR%R(J+(I-1)*NPOIN)
            VSP_D50%R(J+I*NPOIN)   = VSP_D50%R(J+(I-1)*NPOIN)
            VSP_D%R(J+I*NPOIN)     = VSP_D%R(J+(I-1)*NPOIN)
          ENDIF            
          DO K= 1, NSICLA
            IF ((I+1).LE.PRO_MAX(J)) THEN
              VSP_FRA(K)%R(J+I*NPOIN) = PRO_F(J,I+1,K)
            ELSE
              VSP_FRA(K)%R(J+I*NPOIN) = PRO_F(J,PRO_MAX(J),K)
            ENDIF
          ENDDO
        ENDDO
      ENDDO
!
!-----------------------------------------------------------------------    
! CHOOSE WHAT YOU WANT TO HAVE IN 2DHYD_AS_3D
! NUMBER OF PRINTOUT VARIABLES 
!-----------------------------------------------------------------------
!
      URBLOC2DHYD%N = NUMVAR2DHYD
!      
      DO J= 1, NPOIN
        UR2DHYD(1)%R(J) = ZF%R(J)
        UR2DHYD(1)%R(J+NPOIN) = Z%R(J)
!        
        UR2DHYD(2)%R(J) = U2D%R(J)
        UR2DHYD(2)%R(J+NPOIN) = UR2DHYD(2)%R(J)
!        
        UR2DHYD(3)%R(J) = V2D%R(J)
        UR2DHYD(3)%R(J+NPOIN) = UR2DHYD(3)%R(J)
!        
        UR2DHYD(4)%R(J) = 0.D0
        UR2DHYD(4)%R(J+NPOIN) = UR2DHYD(4)%R(J)
!       
        UR2DHYD(5)%R(J) = (U2D%R(J)**2.D0 + V2D%R(J)**2.D0)**0.5D0
        UR2DHYD(5)%R(J+NPOIN) = UR2DHYD(5)%R(J)
!
        UR2DHYD(6)%R(J) = TOB%R(J)
        UR2DHYD(6)%R(J+NPOIN) = 0.D0
      ENDDO
!     
!-----------------------------------------------------------------------  
! POINT TO BIEF OBJECTS THAT WILL BE PRINTED TO
! MAKE YOUR OWN DECISSION HERE   
!-----------------------------------------------------------------------
!     
      URBLOC3D%ADR(1)%P => VSP_D
      URBLOC3D%ADR(2)%P => VSP_D50
      URBLOC3D%ADR(3)%P => VSP_ERROR
      DO K = 1, NSICLA
        URBLOC3D%ADR(3+K)%P => VSP_FRA(K)
      ENDDO
!
!-----------------------------------------------------------------------  
! POINT TO BIEF OBJECTS THAT WILL BE PRINTED TO 2DHYD_AS_3D FILE
! MAKE YOUR OWN DECISSION HERE   
!-----------------------------------------------------------------------
!
      URBLOC2DHYD%ADR(1)%P => UR2DHYD(1)
      URBLOC2DHYD%ADR(2)%P => UR2DHYD(2)
      URBLOC2DHYD%ADR(3)%P => UR2DHYD(3)
      URBLOC2DHYD%ADR(4)%P => UR2DHYD(4)
      URBLOC2DHYD%ADR(5)%P => UR2DHYD(5)
      URBLOC2DHYD%ADR(6)%P => UR2DHYD(6)
!
      USERPRINTCOUNT = USERPRINTCOUNT + 1 
!
!-----------------------------------------------------------------------     
! ADD THIS TO TIME BECAUSE TECPLOT DOESN'T SUPPORT MULTIPLE TIME STAMPS IN ONE FILE.
!-----------------------------------------------------------------------
! 
      USERTIME= AT + USERPRINTCOUNT / 1.0D5
!     
!-----------------------------------------------------------------------     
! WRITE TIME STEP TO    !VSP
!-----------------------------------------------------------------------
!
      SELECT CASE (CP_FILES(3)%FMT)
      CASE ('SERAFIN ','SERAFIND')
        CALL WRITE_DATA_SERAFIN(CP_FILES(3)%LU,NUMVARUR3D2RES,
     &        USERTIME,LT,
     &        UR3D_FILES_OUTVAR,URBLOC3D,
     &        CP_FILES(3)%FMT,VSP_FRA(1)%DIM1)
!
      CASE ('MED     ')
!         
      CASE DEFAULT
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'WRITE_DATA : MAUVAIS FORMAT : ',CP_FILES(3)%FMT
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'WRITE_DATA: BAD FILE FORMAT : ',CP_FILES(3)%FMT
        ENDIF
        CALL PLANTE(1)
      END SELECT
!     
!-----------------------------------------------------------------------    
! WRITE TIME STEP TO 2DHYD_AS_3D 
!-----------------------------------------------------------------------
!
      SELECT CASE (CP_FILES(4)%FMT)
      CASE ('SERAFIN ','SERAFIND')
        CALL WRITE_DATA_SERAFIN(CP_FILES(4)%LU,NUMVAR2DHYD,
     &        USERTIME,LT,
     &        UR2DHYD_FILES_OUTVAR,URBLOC2DHYD,
     &        CP_FILES(4)%FMT,UR2DHYD(1)%DIM1)
!         
      CASE ('MED     ')
!         
      CASE DEFAULT
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'WRITE_DATA : MAUVAIS FORMAT : ',CP_FILES(4)%FMT
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'WRITE_DATA: BAD FILE FORMAT : ',CP_FILES(4)%FMT
        ENDIF
        CALL PLANTE(1)
      END SELECT
!     
!-----------------------------------------------------------------------
!
      RETURN
      END
