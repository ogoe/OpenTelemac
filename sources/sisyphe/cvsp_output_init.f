!                      ***************************
                       SUBROUTINE CVSP_OUTPUT_INIT
!                      ***************************
!
!
!***********************************************************************
! SISYPHE   V6P3                                   14/03/2013
!***********************************************************************
!
!brief    CVSP_OUTPUT_INIT
!
!history  U.MERKEL
!+        30/07/2012
!+        V6P2
!+
!history  P. A. TASSI (EDF R&D, LNHE)
!+        12/03/2013
!+        V6P3
!+   Cleaning, cosmetic
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|                |<->| 
!|                |-->| 
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE
      USE CVSP_OUTPUTFILES

      IMPLICIT NONE

      CHARACTER*32 VLABEL
      CHARACTER(LEN=11) :: EXTENS
      INTEGER I, K
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER CFG(2)            ! FOR 3D MESH
      CFG(1) = OPTASS
      CFG(2) = PRODUC           ! PRODUC=1 HARD IN LECDON
!
!-----------------------------------------------------------------------     
! CHECKPOINTFILE
!-----------------------------------------------------------------------
!
      CP_FILES(3)%FMT = 'SERAFIN ' !'SERAFIN ' OR 'SERAFIND'
      CP_FILES(3)%LU = 10004    !INTEGER, INDIVIDUAL
      CP_FILES(3)%NAME = 'SORTINGPROFILE.RES' !UP TO 144 CHARACTERS
      CP_FILES(3)%TELNAME = 'VSPRES' !ONLY 6 CHARACTERS
      CP_FILES(3)%ACTION = 'WRITE'
      USRMSH_NPLAN = PRO_MAX_MAX
      
      CP_FILES(4)%FMT = 'SERAFIN ' !'SERAFIN ' OR 'SERAFIND'
      CP_FILES(4)%LU = 10005    !INTEGER, INDIVIDUAL
      CP_FILES(4)%NAME = '2DHYDROAS3D.RES' !UP TO 144 CHARACTERS
      CP_FILES(4)%TELNAME = 'VSPHYD' !ONLY 6 CHARACTERS
      CP_FILES(4)%ACTION = 'WRITE'
      USRMSH_2DHYD_NPLAN = 2
!
!-----------------------------------------------------------------------     
! ALLOCATES A 3D MESH FOR USEROUTPU: HERE VSPRES
!-----------------------------------------------------------------------
!
      CALL ALMESH(USRMSH,'USRMSH',41,SPHERI,CFG,SIS_FILES(SISGEO)%LU,
     &     EQUA,NPLAN=USRMSH_NPLAN,
     &     FILE_FORMAT=SIS_FILES(SISGEO)%FMT)
      
      DO I =1,USRMSH%NPTFR
         USRMSH%NBOR%I(I) = I
      END DO
!     
!-----------------------------------------------------------------------     
! ALLOCATES A 3D MESH FOR USEROUTPU: HERE VSPHYD
!-----------------------------------------------------------------------
!
      CALL ALMESH(USRMSH_2DHYD,'USRHYD',41,SPHERI,CFG,
     &     SIS_FILES(SISGEO)%LU,
     &     EQUA,NPLAN=USRMSH_2DHYD_NPLAN,
     &     FILE_FORMAT=SIS_FILES(SISGEO)%FMT)
      
      DO I =1,USRMSH_2DHYD%NPTFR
         USRMSH_2DHYD%NBOR%I(I) = I
      END DO
!
!-----------------------------------------------------------------------     
! SET HERE THE NAMES OF THE PRINTOUT VARIABLES FOR THE USER FILE VSPRES
!-----------------------------------------------------------------------
!
      DO I = 1,NUMVARUR3D2RES
         IF (I.LE.3+NSICLA) THEN
            UR3D_FILES_OUTVAR(I) = .TRUE.
         ELSE
            UR3D_FILES_OUTVAR(I) = .FALSE.
         ENDIF
         WRITE(UNIT=VLABEL, FMT='(A5,I2,A25)') 'FRACTION_CLASS_',I-3
         UR3D_FILES_LABELS(I) = VLABEL
      ENDDO
! EXAMPLE FOR MANUALLY OVERWRITTING LEN=32
      UR3D_FILES_LABELS(1)= 'Z [M]                           ' !'PROFILE_ELEVATION               '
      UR3D_FILES_LABELS(2)= 'PROFILE_D50 [M]                 '
      UR3D_FILES_LABELS(3)= 'PROFILE_ERROR [-]               '
!
!-----------------------------------------------------------------------     
! SET HERE THE NAMES OF THE PRINTOUT VARIABLES FOR THE USER FILE VSPHYD
!-----------------------------------------------------------------------
!
      DO I = 1,NUMVAR2DHYD
         UR2DHYD_FILES_OUTVAR(I) = .TRUE.
      ENDDO
      
      UR2DHYD_FILES_LABELS(1)= 'Z [M]                           ' !'Z [M] AND ZF [M]                '
      UR2DHYD_FILES_LABELS(2)= 'U [M/S]                         '
      UR2DHYD_FILES_LABELS(3)= 'V [M/S]                         '
      UR2DHYD_FILES_LABELS(4)= 'W [M/S]                         '
      UR2DHYD_FILES_LABELS(5)= 'SCALAR VELOCITY [M/S]           '
      UR2DHYD_FILES_LABELS(6)= 'TAU [N/M**2]                    '
!     
!-----------------------------------------------------------------------     
! ALLOCATES THE BLOCK CONNECTING A VARIABLE NAME TO ITS ARRAY  
!-----------------------------------------------------------------------
!
      CALL ALLBLO(URBLOC3D, 'URBL3D')
      CALL ALLBLO(URBLOC2DHYD, 'URB2DH')
!
!-----------------------------------------------------------------------     
! OPENS THE FILES FOR WRITING
!-----------------------------------------------------------------------
!
      DO I = 3, 4
         IF(NCSIZE.LE.1) THEN
! SCALAR
            OPEN(CP_FILES(I)%LU,FILE=CP_FILES(I)%TELNAME,
     &           ACTION=CP_FILES(I)%ACTION,FORM='UNFORMATTED')
         ELSE
! PARALLEL, FILE TYPE: SCAL
            IF(CP_FILES(I)%TYPE(1:4).EQ.'SCAL') THEN
               OPEN(CP_FILES(I)%LU,
     &              FILE=TRIM(CP_FILES(I)%TELNAME),
     &              ACTION=CP_FILES(I)%ACTION,FORM='UNFORMATTED')
! PARALLEL, OTHER FILE TYPE
            ELSE
               OPEN(CP_FILES(I)%LU,
     &              FILE=TRIM(CP_FILES(I)%TELNAME)
     &              //EXTENS(NCSIZE-1,IPID),
     &              ACTION=CP_FILES(I)%ACTION,FORM='UNFORMATTED')
            ENDIF
         ENDIF
         
      ENDDO                     !CP_FILES
!
!-----------------------------------------------------------------------     
! WRITES THE HEADER OF THE RESFILES
!-----------------------------------------------------------------------
!
      CALL CREATE_DATASET_SERAFIN(CP_FILES(4)%LU,
     &     '2D HYDRAULIC PARAMETERS IN 3D  '//
     &     '                                         ',
     &     NUMVAR2DHYD,
     &     UR2DHYD_FILES_LABELS,
     &     UR2DHYD_FILES_OUTVAR,
     &     CP_FILES(4)%FMT)

      CALL CREATE_DATASET_SERAFIN(CP_FILES(3)%LU,
     &     'USEROUTPUT3D                   '//
     &     '                                         ',
     &     NUMVARUR3D2RES,
     &     UR3D_FILES_LABELS,
     &     UR3D_FILES_OUTVAR,
     &     CP_FILES(3)%FMT)
!
!-----------------------------------------------------------------------     
! WRITES THE MESH INFORMATION IN THE OUTPUT FILE :
!-----------------------------------------------------------------------
!
      DO I = 3, 3               !UBOUND(CP_FILES)
         CALL WRITE_MESH(CP_FILES(I)%FMT, ! RESULTS FILE FORMAT
     &        CP_FILES(I)%LU,   ! LU FOR RESULTS FILE
     &        USRMSH,           ! CHARACTERISES MESH
     &        USRMSH_NPLAN,     ! NUMBER OF PLANES /NA/
     &        MARDAT,           ! START DATE
     &        MARTIM,           ! START TIME
     &        I_ORIG,J_ORIG)    ! COORDINATES OF THE ORIGIN.
      ENDDO

      DO I = 4, 4               !UBOUND(CP_FILES)
         CALL WRITE_MESH(CP_FILES(I)%FMT, ! RESULTS FILE FORMAT
     &        CP_FILES(I)%LU,   ! LU FOR RESULTS FILE
     &        USRMSH_2DHYD,     ! CHARACTERISES MESH
     &        USRMSH_2DHYD_NPLAN, ! NUMBER OF PLANES /NA/
     &        MARDAT,           ! START DATE
     &        MARTIM,           ! START TIME
     &        I_ORIG,J_ORIG)    ! COORDINATES OF THE ORIGIN.
      ENDDO
!
!-----------------------------------------------------------------------     
! INITS OUTPUT VECTORS
!-----------------------------------------------------------------------
!
! VSPRES
      DO K = 1, NSICLA
         CALL BIEF_ALLVEC(1,VSP_FRA(K),'VSPFRA',41,1,1,USRMSH)
      ENDDO
      
      CALL BIEF_ALLVEC(1,VSP_D,     'VSP__D',41,1,1,USRMSH)
      CALL BIEF_ALLVEC(1,VSP_D50,   'VSPD50',41,1,1,USRMSH)
      CALL BIEF_ALLVEC(1,VSP_ERROR, 'VSP_ER',41,1,1,USRMSH)
      
! VSPHYD
      DO K = 1, NUMVAR2DHYD
         CALL BIEF_ALLVEC(1,UR2DHYD(K),'VSPHYD',41,1,1,USRMSH_2DHYD)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
