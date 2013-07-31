C                       ******************************
                        SUBROUTINE DREDGESIM_INTERFACE
C                       ******************************
C
     *(OPTION)
C
C***********************************************************************
C SISYPHE VERSION 6.0      02/02/2009 J-M HERVOUET (LNHE) 01 30 87 80 18
C                                                                                               
C COPYRIGHT EDF      
C***********************************************************************
C
C  FUNCTION: THIS IS THE INTERFACE TO DREDGESIM, CONTAINING ALL 
C            DEPENDENCIES TO DREDGESIM LIBRARIES 
C
C  FOR REAL INTERFACING WITH DREDGESIM, COMMENTS "!" MUST BE REMOVED
C
C-----------------------------------------------------------------------
C                             ARGUMENTS
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|______________________________________________
C |   OPTION       | -->| 1 : INITIALISATION (CALLED IN SISYPHE)
C |                |    | 2 : CALLED EVERY TIME STEP (FROM 
C |                |    |     BEDLOAD_POSTTREATMENT)
C |                |    | 3 : END  (CALLED IN SISYPHE)
C |________________|____|______________________________________________
C MODE : -->(DONNEE NON MODIFIEE), <--(RESULTAT), <-->(DONNEE MODIFIEE)
C-----------------------------------------------------------------------
C PROGRAMME APPELANT : SISYPHE, BEDLOAD_POSTTREATMENT
C PROGRAMMES APPELES : 
C***********************************************************************
C
      USE BIEF
!      USE DECLARATIONS_TELEMAC, ONLY : EXTENS
      USE DECLARATIONS_SISYPHE, ONLY : DREDGESIM,DT,NPOIN,NSICLA,DZF_GF,
     *                                 ZFCL_C,AVAIL,MESH,SIS_FILES,
     *                                 SISMAF,SISGEO,ZF, MSK, MASKEL,
     *                                 MESH,T13,T14,ielmh_sis,NELEM,
     *                                 NLAYER,E,ZR,HN,FDM,LEOPR,
     *                                 PTINIG,LT,MARDAT,MARTIM,XKV,XMVS
      USE p_sisyphe_ui, ONLY : init_and_setup_ds,clear_sisydredge
      USE p_dredgesim_ui, ONLY : stop_dredgesim,clear_dredgesim
      USE p_dredgesim_ui, ONLY : run_dredgesim
      USE m_dredgesim_data, ONLY: node_sediment_fraction

C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      INTEGER IPOIN,ISICLA
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: OPTION
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      CHARACTER(LEN=250) :: DREDGEINP,SEDGEO
      DOUBLE PRECISION,ALLOCATABLE :: AVAI_GF(:,:)
      INTEGER I,J
      CHARACTER(LEN=11) EXTENS
      EXTERNAL          EXTENS
      !LEO new variable for node_Depth of sisyphe 
      DOUBLE PRECISION, DIMENSION (:), ALLOCATABLE, SAVE,TARGET :: tds_sis_node_depth
      DOUBLE PRECISION, DIMENSION (:,:), ALLOCATABLE, SAVE,TARGET :: tds_node_sediment_fraction
      DOUBLE PRECISION, DIMENSION (:), ALLOCATABLE, SAVE,TARGET :: tds_hn
      


C
C-----------------------------------------------------------------------
C

      IF(OPTION.EQ.1) THEN
C
C     INITIALISATION
C
         !LEO
         ALLOCATE(tds_sis_node_depth(NPOIN))
         ALLOCATE(tds_hn(NPOIN))
         ALLOCATE(tds_node_sediment_fraction(NPOIN,NSICLA))

        DREDGEINP = ''
        sedgeo = ''
        IF(NCSIZE.GT.1) then
C         INPUT FILE FOR DREDGESIM
          DREDGEINP = trim('SISMAF'//extens(ncsize-1,ipid))
          sedgeo = trim('SISGEO'//extens(ncsize-1,ipid))
        ELSE
          dredgeinp = 'SISMAF'
          sedgeo = 'SISGEO'
        ENDIF
! calculation of node area in T13
        CALL VECTOR(T13,'=','MASBAS          ',
     &    IELMH_sis,1.D0,T14,T14,T14,T14,T14,T14,MESH,MSK,MASKEL)
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(T13,2,MESH)
        ENDIF
! no quasi bubble supported
        IF(IELMH_sis.NE.11) THEN
          WRITE(*,*)'ONLY LINEAR 2D Elements are supported'
          WRITE(*,*)'DredgeSim -STOP, please start without'
          WRITE(*,*)'DredgeSim or use another element type'
          WRITE(*,*)'Actual element type of ZF: ',IELMH_sis
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
      OPEN (120, file = "nodearea.dat")
      DO i = 1,NPOIN
         WRITE(120,*), i, T13%R(i)
      END DO

      CLOSE(120)
!LEODEBUG output END COMUTE MESH AREA



!L.S. removed debug commented out
      CALL init_and_setup_ds(SIS_FILES(SISMAF)%LU,dredgeinp,
     &                        SIS_FILES(SISGEO)%LU,sedgeo,
     &                         ncsize,ipid, 
     &    NELEM, NSICLA, NPOIN, NLAYER, MESH, T13, zf, E, ZR, HN,
     &    FDM,avail, leopr, ptinig, dt,LT,
     &    MARDAT, MARTIM, XKV, XMVS)
  
   
C
      !#########################################
      !
      ! Run dredgesim
      !
      !#########################################
      ELSEIF(OPTION.EQ.2) THEN
C
C     CALL FROM WITHIN BEDLOAD_POSTTREATMENT
C
         !---------------- do some preprocessing -----
C       allocating avai_gf
        ALLOCATE(AVAI_GF(NPOIN,NSICLA))
C       INITIALIZATION OF THE ELEVATION TO ADD
        CALL OS('X=0     ',X=DZF_GF)

         !LEO make a stupid copy of tds_sis_node_depth, and water depth HN
         DO i = 1,NPOIN
            tds_sis_node_depth(i) = ZF%R(i)
            tds_hn(i) = HN%R(i)
         END DO
       
         !LEO copy sediment frations to tds_node_sediment_fraction
         DO ipoin=1,NPOIN !get_nof_nodes()
            DO isicla=1,NSICLA !size(node_sediment_fraction,2)
               tds_node_sediment_fraction(ipoin,isicla) = AVAIL(ipoin,1,isicla)
            END DO
         END DO
   
       !-----------------run dredgesim -------------
       CALL run_dredgesim(DT,tds_sis_node_depth,tds_hn,AVAIL,NPOIN,NSICLA,tds_node_sediment_fraction)

      !---------------- do some postprocessing -----
       
       !LEO OLD DEFAULT 
       !DZF_GF%R = node_depth - ZF%R
       
       DZF_GF%R =  tds_sis_node_depth - ZF%R
       !LEO DZF_GF%R = node_depth - tds_sis_node_depth
       
       !LEO Reset ZF%R with tds_sis_node_depth       
       !LEO not longer needed ZF%R = tds_sis_node_depth


       avai_gf = node_sediment_fraction
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
        DEALLOCATE(avai_gf)
C
      ELSEIF(OPTION.EQ.3) THEN
C
C     CLOSING
C
      CALL stop_dredgesim()
      CALL clear_dredgesim()
      CALL clear_sisydredge()
      
      !LEO deallocate my variables
      DEALLOCATE(tds_sis_node_depth) 
      DEALLOCATE(tds_node_sediment_fraction)
      DEALLOCATE(tds_hn) 
C
      ELSE
C
C     ERROR
C
        IF(LNG.EQ.1) WRITE(LU,*) 'MAUVAISE OPTION POUR DREDGESIM'
        IF(LNG.EQ.2) WRITE(LU,*) 'BAD OPTION FOR DREDGESIM'
        CALL PLANTE(1)
        STOP        
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
