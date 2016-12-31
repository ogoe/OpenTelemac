!
!
!
!
!
!
!
!      include '../../_nestor88'
!      include '../../_nestor88_noDEBU.f'
!      include '../../_nestor88.f'
!      include '../_nestor87.f'
!      include '../_nestorCat.f'
!
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
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
      !> Close the Nestor steering file (e.g. _DigActions.dat )


      CALL InterFaceInitNestor(
     &                             NCSIZE, IPID, NPOIN
     &                           , NSICLA
     &                           , MARDAT, MARTIM ! Sis start: date , time
     &                           , MOFAC    ! morphological factor
!       &                           , DEBU                  !                                                                
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
         CALL InterFaceRunNestor(   NPOIN      !  number of points (Nodes)
     &                          , NSICLA     !  number of SIze CLAsses
     &                          , LT         !  Telemac time step
     &                          , DTS        !  duration of Sisyphe time step
     &                          , AT0        !  time
     &                          , ELAY0      !  active layer thickness [m]
     &                          , ZF%R       !  bottom [m+NN]
     &                          , ZFCL_C     !  evolution per class per time step [m]
     &                          , AVAIL(1:NPOIN,1,1:NSICLA)    !
     &                          , MESH%KNOLG%I    ! index list: Local to Global node index 
     &                        )
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
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!                    *********************
                     SUBROUTINE INIT_COMPO
!                    *********************
!
     &(NCOUCHES)
!
!***********************************************************************
! SISYPHE   V7P2
!***********************************************************************
!
!brief    INITIAL FRACTION DISTRIBUTION, STRATIFICATION,
!+                VARIATION IN SPACE.
!
!warning  USER SUBROUTINE; MUST BE CODED BY THE USER
!code
!+  EXAMPLE:
!+      NCOUCHES(J) = 10
!+      ES(J,1) = 1.D0
!+      ES(J,2) = 1.D0
!+      ES(J,3) = 1.D0
!+      ES(J,4) = 1.D0
!+      ES(J,5) = 1.D0
!+      ES(J,6) = 1.D0
!+      ES(J,7) = 1.D0
!+      ES(J,8) = 1.D0
!+      ES(J,9) = 1.D0
!+        DO I = 1, NSICLA
!+          DO K = 1, NCOUCHES(J)
!+          AVAIL(J,K,I) = AVA0(I)
!+          ENDDO
!+        ENDDO
!
!history  MATTHIEU GONZALES DE LINARES
!+        2002
!+        V6P0
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        2016
!+        V7P2
!+   Checking coherence of data: ZR+sediment height=ZF
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NCOUCHES       |-->| NUMBER OF LAYER FOR EACH POINT
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
!
      IMPLICIT NONE

!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!                                       NPOIN
      INTEGER, INTENT (INOUT)::NCOUCHES(*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,J,K
      DOUBLE PRECISION EPAI
!
!-----------------------------------------------------------------------
!
      DO J=1,NPOIN
      	
       NCOUCHES(J) = 3

       ES(J,1) = 0.1D0
       ES(J,2) = 0.1D0
       ES(J,3) = 9.8D0 
      
!       IF(  MESH%X%R(J) > 600.0_8 )  THEN  ! debug
!         AVA0(1) = 1.0_8                   ! debug
!         AVA0(2) = 0.0_8                   ! debug
!         AVA0(3) = 0.0_8                   ! debug
!       ELSE                                ! debug
!         AVA0(1) = 0.0_8                   ! debug
!         AVA0(2) = 0.0_8                   ! debug
!         AVA0(3) = 1.0_8                   ! debug
!       ENDIF                               ! debug

       DO I = 1, NSICLA
         DO K = 1, NCOUCHES(J)
           AVAIL(J,K,I) = AVA0(I)
         ENDDO
       ENDDO 
         
      ENDDO
      
!-----------------------------------------------------------------------
!
!     CHECKING THE CONSISTENCY OF DATA
!     THE FORMULA USED HERE ZR+SED. HEIGHT = ZF CAN BE USED TO GIVE THE
!     HEIGHT OF THE LAST LAYER.
!
      DO J=1,MESH%NPOIN
        EPAI=0.D0
        DO I=1,NCOUCHES(J)
          EPAI=EPAI+ES(J,I)
        ENDDO
        IF(ABS(ZR%R(J)+EPAI-ZF%R(J)).GT.1.D-6) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'INIT_COMPO, ERREUR :'
            WRITE(LU,*) 'ZR+EPAISSEUR=',ZR%R(J)+EPAI
            WRITE(LU,*) 'ZF=',ZF%R(J),' ZR=',ZR%R(J),' EPAISSEUR=',EPAI
            WRITE(LU,*) 'AU POINT ',J
          ELSE
            WRITE(LU,*) 'INIT_COMPO, ERROR:'
            WRITE(LU,*) 'ZR+SEDIMENT HEIGHT=',ZR%R(J)+EPAI
            WRITE(LU,*) 'ZF=',ZF%R(J),' ZR=',ZR%R(J),
     &                  ' SEDIMENT HEIGHT=',EPAI
            WRITE(LU,*) 'AT POINT ',J
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
      
      
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************
!***********************************************************************

!                    *****************
                     SUBROUTINE NOEROD
!                    *****************
!
     & (H , ZF , ZR , Z , X , Y , NPOIN , CHOIX , NLISS )
!
!***********************************************************************
! SISYPHE   V6P3                                  21/07/2011
!***********************************************************************
!
!brief    FIXES THE NON-ERODABLE BED ELEVATION ZR.
!
!note     METHODS OF TREATMENT OF NON-ERODABLE BEDS CAN LEAD TO ZF.
!note  CHOOSE TO SMOOTH THE SOLUTION WITH NLISS > 0.
!
!history  C. LENORMANT
!+
!+        V5P1
!+
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        13/07/2010
!+        V6P0
!+   Translation of French comments within the FORTRAN sources into
!+   English comments
!
!history  N.DURAND (HRW), S.E.BOURBAN (HRW)
!+        21/08/2010
!+        V6P0
!+   Creation of DOXYGEN tags for automated documentation and
!+   cross-referencing of the FORTRAN sources
!
!history  J-M HERVOUET (EDF R&D, LNHE)
!+        21/06/2013
!+        V6P3
!+   Now ZR=ZF-100.D0 by default
!+   previous versions was erronneously ZR=-100.D0
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CHOIX          |-->| SELECTED METHOD FOR THE TREATMENT OF RIGID BEDS
!| H              |-->| WATER DEPTH
!| NLISS          |<->| NUMBER OF SMOOTHINGS
!| NPOIN          |-->| NUMBER OF 2D POINTS
!| X,Y            |-->| 2D COORDINATES
!| Z              |-->| FREE SURFACE
!| ZF             |-->| BED LEVEL
!| ZR             |<--| RIGID BED LEVEL
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN):: NPOIN , CHOIX
      INTEGER, INTENT(INOUT):: NLISS
!
      DOUBLE PRECISION, INTENT(IN)::  Z(NPOIN) , ZF(NPOIN)
      DOUBLE PRECISION , INTENT(IN)::  X(NPOIN) , Y(NPOIN), H(NPOIN)
      DOUBLE PRECISION , INTENT(INOUT)::  ZR(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     INTEGER I
!
!---------------------
! RIGID BEDS POSITION
!---------------------
!
!     DEFAULT VALUE: ZR=ZF-100.D0
!
cgl   CALL OV('X=Y+C   ',ZR,ZF,ZF,-100.D0,NPOIN)
      CALL OV('X=Y+C   ',ZR,ZF,ZF, -10.D0,NPOIN)
!
!------------------
! SMOOTHING OPTION
!------------------
!
!     NLISS : NUMBER OF SMOOTHING IF  (ZF - ZR ) NEGATIVE
!             DEFAULT VALUE : NLISS = 0 (NO SMOOTHING)
!
      NLISS = 0
!
!--------------------------------------------------
! CONTROL (CAN BE ACTIVATED IF ZR USER DEFINED...)
!--------------------------------------------------
!
!     DO I=1,NPOIN
!       IF(ZR(I).GT.ZF(I)) THEN
!         WRITE(LU,*) 'POINT ',I,' NON ERODABLE BED HIGHER THAN BED'
!         CALL PLANTE(1)
!         STOP
!       ENDIF
!     ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
      

