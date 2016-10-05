!                       ******************************
                        SUBROUTINE NESTOR_INTERFACE
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
!  FUNCTION: THIS IS THE INTERFACE TO NESTOR, CONTAINING ALL
!            DEPENDENCIES TO NESTOR LIBRARIES
!
!  FOR REAL INTERFACING WITH NESTOR, COMMENTS "!" MUST BE REMOVED
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
      USE DECLARATIONS_SISYPHE, ONLY : NESTOR,DT,NPOIN,NSICLA,DZF_GF,
     &                                 ZFCL_C,AVAIL,MESH,SIS_FILES,
     &                                 SISMAF,SISGEO,ZF, MSK, MASKEL,
     &                                 MESH,T13,T14,IELMH_SIS,NELEM,
     &                                 NLAYER,E,ZR,HN,FDM,LEOPR,
     &                                 PTINIG,LT,MARDAT,MARTIM,XKV,XMVS,
     &                                 DEBU,MOFAC,DT,NSOUS,AT0,ELAY0
!
!      USE DECLARATIONS_TELEMAC2D, ONLY : LEOPRD
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: OPTION
      DOUBLE PRECISION :: DTS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!

!-----------------------------------------------------------------------
!

      IF(OPTION.EQ.1) THEN
!
!     INITIALISATION
!

!      _____________________________________________________________________
!     /________ calculation of Node-areas for all nodes of grid ___________/
      ! calculation of Node-areas in T13      !  for parallel: here the
      ! interface-nodes                                                                                                         !> node-area is allready reduced
      CALL VECTOR(T13,'=','MASBAS          '
     &      ,IELMH_SIS,1.D0,T14,T14,T14,T14,T14,T14,MESH,MSK,MASKEL)
!RK warum ist das hier auskommentiert?!?
!         IF(NCSIZE.GT.1) CALL PARCOM(T13,2,MESH)

      CLOSE( SIS_FILES(SISMAF)%LU )
      !> Close the SimuDig steering file (e.g. _DigActions.dat )


      CALL INTERFACEINITNESTOR(
     &                             NCSIZE, IPID, NPOIN
     &                           , NSICLA
     &                           , MARDAT, MARTIM ! Sis start: date , time
     &                           , MOFAC    ! morphological factor
     &                           , LEOPR  ! period of graphical outputs
     &                        , MESH%X%R
     &                        , MESH%Y%R
     &                        , T13%R
     &                        , MAXVAL( MESH%KNOLG%I(:) )
     &                                       )


! LEOPRD aus T2D, GRAFCOUNT in sisyphe.f
! LEOPR = GRAFCOUNT in sisyphe.F
!
      WRITE(6,*)' --------  end nestor ----------'


! NO QUASI BUBBLE SUPPORTED
        IF(IELMH_SIS.NE.11) THEN
          WRITE(*,*)'ONLY LINEAR 2D ELEMENTS ARE SUPPORTED'
          WRITE(*,*)'NESTOR -STOP, PLEASE START WITHOUT'
          WRITE(*,*)'NESTOR OR USE ANOTHER ELEMENT TYPE'
          WRITE(*,*)'ACTUAL ELEMENT TYPE OF ZF: ',IELMH_SIS
          CALL PLANTE(1)
          STOP
        ENDIF

!
      !#########################################
      !
      ! Run Nestor
      !
      !#########################################
      ELSEIF(OPTION.EQ.2) THEN
!
!     CALL FROM WITHIN BEDLOAD_MAIN
!
        IF (NESTOR.EQV..TRUE.) THEN
          DTS = DT/NSOUS
          CALL INTERFACERUNNESTOR(   NPOIN      !  NUMBER OF POINTS (NODES)
     &                           , NSICLA     !  number of SIze CLAsses
     &                           , LT         !  Telemac time step
     &                           , DTS        !  duration of Sisyphe time step
     &                           , AT0        !  time
     &                           , ELAY0      !  active layer thickness [m]
     &                           , ZF%R       !  bottom [m+NN]
     &                           , ZFCL_C     !  evolution per class per time step [m]
     &                           , AVAIL(1:NPOIN,1,1:NSICLA)    !
     &                           , MESH%KNOLG%I    ! index list: Local to Global node index
     &                         )
        ENDIF

!
!
      ELSE
!
!     ERROR
!
        IF(LNG.EQ.1) WRITE(LU,*) 'MAUVAISE OPTION POUR NESTOR'
        IF(LNG.EQ.2) WRITE(LU,*) 'BAD OPTION FOR NESTOR'
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
