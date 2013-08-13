!                       ******************************
                        SUBROUTINE DREDGESIM_INTERFACE
!                       ******************************
!
     &(OPTION)
!
!***********************************************************************
! SISYPHE VERSION 6.0      02/02/2009 J-M HERVOUET (LNHE) 01 30 87 80 18
!                                                                                               
! COPYRIGHT EDF      
!***********************************************************************
!
!  FUNCTION: THIS IS THE INTERFACE TO DREDGESIM, CONTAINING ALL 
!            DEPENDENCIES TO DREDGESIM LIBRARIES 
!
!  FOR REAL INTERFACING WITH DREDGESIM, COMMENTS "!" MUST BE REMOVED
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   OPTION       | -->| 1 : INITIALISATION (CALLED IN SISYPHE)
! |                |    | 2 : CALLED EVERY TIME STEP (FROM 
! |                |    |     BEDLOAD_POSTTREATMENT)
! |                |    | 3 : END  (CALLED IN SISYPHE)
! |________________|____|______________________________________________
! MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
!-----------------------------------------------------------------------
! PROGRAMME APPELANT : SISYPHE, BEDLOAD_POSTTREATMENT
! PROGRAMMES APPELES : 
!***********************************************************************
!
      USE BIEF
      USE DECLARATIONS_SISYPHE, ONLY : DREDGESIM,DT,NPOIN,NSICLA,DZF_GF,
     &                                 ZFCL_C,AVAIL,MESH,SIS_FILES,
     &                                 SISMAF,SISGEO,ZF, MSK, MASKEL,
     &                                 MESH,T13,T14,IELMH_SIS,NELEM,
     &                                 NLAYER,E,ZR,HN,FDM,LEOPR,
     &                                 PTINIG,LT,MARDAT,MARTIM,XKV,XMVS
      USE P_SISYPHE_UI, ONLY : INIT_AND_SETUP_DS,CLEAR_SISYDREDGE
      USE P_DREDGESIM_UI, ONLY : STOP_DREDGESIM,CLEAR_DREDGESIM
      USE P_DREDGESIM_UI, ONLY : RUN_DREDGESIM
      USE M_DREDGESIM_DATA, ONLY: NODE_SEDIMENT_FRACTION

!
      IMPLICIT NONE
!
      INTEGER LNG,LU
      INTEGER IPOIN,ISICLA
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: OPTION
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      CHARACTER(LEN=250) :: DREDGEINP,SEDGEO
      DOUBLE PRECISION,ALLOCATABLE :: AVAI_GF(:,:)
      INTEGER I,J
      CHARACTER(LEN=11) EXTENS
      EXTERNAL          EXTENS
      !LEO new variable for node_Depth of sisyphe 
      DOUBLE PRECISION, DIMENSION (:), ALLOCATABLE, 
     &          SAVE,TARGET :: TDS_SIS_NODE_DEPTH
      DOUBLE PRECISION, DIMENSION (:,:), ALLOCATABLE, 
     &          SAVE,TARGET :: TDS_NODE_SEDIMENT_FRACTION
      DOUBLE PRECISION, DIMENSION (:), ALLOCATABLE, 
     &          SAVE,TARGET :: TDS_HN
      


!
!-----------------------------------------------------------------------
!

      IF(OPTION.EQ.1) THEN
!
!     INITIALISATION
!
         !LEO
         ALLOCATE(TDS_SIS_NODE_DEPTH(NPOIN))
         ALLOCATE(TDS_HN(NPOIN))
         ALLOCATE(TDS_NODE_SEDIMENT_FRACTION(NPOIN,NSICLA))

        DREDGEINP = ''
        SEDGEO = ''
        IF(NCSIZE.GT.1) THEN
!         INPUT FILE FOR DREDGESIM
          DREDGEINP = TRIM('SISMAF'//EXTENS(NCSIZE-1,IPID))
          SEDGEO = TRIM('SISGEO'//EXTENS(NCSIZE-1,IPID))
        ELSE
          DREDGEINP = 'SISMAF'
          SEDGEO = 'SISGEO'
        ENDIF
! CALCULATION OF NODE AREA IN T13
        CALL VECTOR(T13,'=','MASBAS          ',
     &    IELMH_SIS,1.D0,T14,T14,T14,T14,T14,T14,MESH,MSK,MASKEL)
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(T13,2,MESH)
        ENDIF
! NO QUASI BUBBLE SUPPORTED
        IF(IELMH_SIS.NE.11) THEN
          WRITE(*,*)'ONLY LINEAR 2D ELEMENTS ARE SUPPORTED'
          WRITE(*,*)'DREDGESIM -STOP, PLEASE START WITHOUT'
          WRITE(*,*)'DREDGESIM OR USE ANOTHER ELEMENT TYPE'
          WRITE(*,*)'ACTUAL ELEMENT TYPE OF ZF: ',IELMH_SIS
          STOP
        ENDIF

!LEODEBUG output COMPUTE MESH AREA
!      WRITE(*,*)""
!      WRITE(*,*) "LEODEBUG ELEMENT and SIZES"
!      DO i = 1,NELEM
!         WRITE(*,*), i, MESH%SURFAC%R(i)
!      END DO
!      WRITE(*,*) "LEODEBUG Node and Node Area SIZES"
!      DO i = 1,NPOIN
!         WRITE(*,*), i, T13%R(i)
!      END DO
!      WRITE(*,*)""
!
      OPEN (120, FILE = "nodearea.dat")
      DO i = 1,NPOIN
         WRITE(120,*) i, T13%R(i)
      END DO

      CLOSE(120)
!LEODEBUG output END COMUTE MESH AREA



!L.S. removed debug commented out
      CALL INIT_AND_SETUP_DS(SIS_FILES(SISMAF)%LU,DREDGEINP,
     &                        SIS_FILES(SISGEO)%LU,SEDGEO,
     &                         NCSIZE,IPID, 
     &    NELEM, NSICLA, NPOIN, NLAYER, MESH, T13, ZF, E, ZR, HN,
     &    FDM,AVAIL, LEOPR, PTINIG, DT,LT,
     &    MARDAT, MARTIM, XKV, XMVS)
  
   
!
      !#########################################
      !
      ! Run dredgesim
      !
      !#########################################
      ELSEIF(OPTION.EQ.2) THEN
!
!     CALL FROM WITHIN BEDLOAD_POSTTREATMENT
!
         !---------------- do some preprocessing -----
!       allocating avai_gf
        ALLOCATE(AVAI_GF(NPOIN,NSICLA))
!       INITIALIZATION OF THE ELEVATION TO ADD
        CALL OS('X=0     ',X=DZF_GF)

         !LEO MAKE A STUPID COPY OF TDS_SIS_NODE_DEPTH, AND WATER DEPTH HN
         DO I = 1,NPOIN
            TDS_SIS_NODE_DEPTH(I) = ZF%R(I)
            TDS_HN(I) = HN%R(I)
         END DO
       
         !LEO COPY SEDIMENT FRATIONS TO TDS_NODE_SEDIMENT_FRACTION
         DO IPOIN=1,NPOIN !GET_NOF_NODES()
            DO ISICLA=1,NSICLA !SIZE(NODE_SEDIMENT_FRACTION,2)
               TDS_NODE_SEDIMENT_FRACTION(IPOIN,ISICLA) = 
     &                         AVAIL(IPOIN,1,ISICLA)
            END DO
         END DO
   
       !-----------------RUN DREDGESIM -------------
       CALL RUN_DREDGESIM(DT,TDS_SIS_NODE_DEPTH,TDS_HN,AVAIL,NPOIN,
     &                         NSICLA,TDS_NODE_SEDIMENT_FRACTION)

      !---------------- do some postprocessing -----
       
       !LEO OLD DEFAULT 
       !DZF_GF%R = node_depth - ZF%R
       
       DZF_GF%R =  TDS_SIS_NODE_DEPTH - ZF%R
       !LEO DZF_GF%R = NODE_DEPTH - TDS_SIS_NODE_DEPTH
       
       !LEO RESET ZF%R WITH TDS_SIS_NODE_DEPTH       
       !LEO NOT LONGER NEEDED ZF%R = TDS_SIS_NODE_DEPTH


       AVAI_GF = NODE_SEDIMENT_FRACTION
!
        DO J = 1, NPOIN
          IF(DZF_GF%R(J).GT.0.D0) THEN
            DO I = 1, NSICLA
              ZFCL_C%ADR(I)%P%R(J) = ZFCL_C%ADR(I)%P%R(J) +
     &                               DZF_GF%R(J)*AVAI_GF(J,I)
            ENDDO
          ELSE
            DO I = 1, NSICLA
              ZFCL_C%ADR(I)%P%R(J) = ZFCL_C%ADR(I)%P%R(J) +
     &                               DZF_GF%R(J)*AVAIL(J,1,I)   
            ENDDO
          ENDIF
        ENDDO
        DEALLOCATE(AVAI_GF)
!
      ELSEIF(OPTION.EQ.3) THEN
!
!     CLOSING
!
      CALL STOP_DREDGESIM()
      CALL CLEAR_DREDGESIM()
      CALL CLEAR_SISYDREDGE()
      
      !LEO DEALLOCATE MY VARIABLES
      DEALLOCATE(TDS_SIS_NODE_DEPTH) 
      DEALLOCATE(TDS_NODE_SEDIMENT_FRACTION)
      DEALLOCATE(TDS_HN) 
!
      ELSE
!
!     ERROR
!
        IF(LNG.EQ.1) WRITE(LU,*) 'MAUVAISE OPTION POUR DREDGESIM'
        IF(LNG.EQ.2) WRITE(LU,*) 'BAD OPTION FOR DREDGESIM'
        CALL PLANTE(1)
        STOP        
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
