C                       *****************
                       SUBROUTINE CVSP_OUTPUT_INIT()
C                       *****************
C
C***********************************************************************
C TELEMAC 2D VERSION 6.1          01/04/2012    U.Merkel
C***********************************************************************
C
C
C  FUNCTION  : INITIAL. USER OUTPUT IN SELFIN or SELAFIND
C  HAS TO BE EXECUTED ONLY ONCE BEFORE THE FIRST call of CVSP_WRITE_PROFILE
C
C
C
!// TOdo (!!!UHM!!!#1#): AND WHEN WILL WE CLOSE THIS FILES????
!// TOdo (!!!UHM!!!#6#): MED NOT SUPPORTED
C
C
C-----------------------------------------------------------------------
C  ARGUMENTS USED
C .________________.____.______________________________________________
C |      NOM       |MODE|                   ROLE
C |________________|____|_______________________________________________
C |________________|____|______________________________________________
C MODE : -->(doNNEE NON MODifIEE), <--(RESULTAT), <-->(doNNEE MODifIEE)
C-----------------------------------------------------------------------
C
      USE BIEF
      !USE DECLARATIONS_TELEMAC2D
      USE DECLARATIONS_SISYPHE
      USE CVSP_OUTPUTFILES

      character*32 vlabel
      CHARACTER(LEN=11) :: EXTENS

      integer i, k


       !for 3d mesh
       INTEGER CFG(2)
       CFG(1) = OPTASS
       CFG(2) = PRODUC   ! PRODUC=1 HARD IN LECdoN


C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

        !Checkpointfile

        CP_FILES(3)%FMT = 'SERAFIN '            !'SERAFIN ' or 'SERAFIND'
        CP_FILES(3)%LU = 10004                  !Integer, individual
        CP_FILES(3)%NAME = 'SortingProfile.res'    !UP to 144 characters
        CP_FILES(3)%TELNAME = 'VSPRES'          !only 6 characters
        CP_FILES(3)%ACTION = 'write'
        USRMSH_NPLAN = PRO_MAX_MAX

        CP_FILES(4)%FMT = 'SERAFIN '            !'SERAFIN ' or 'SERAFIND'
        CP_FILES(4)%LU = 10005                  !Integer, individual
        CP_FILES(4)%NAME = '2DHYDROas3D.res'    !UP to 144 characters
        CP_FILES(4)%TELNAME = 'VSPHYD'          !only 6 characters
        CP_FILES(4)%ACTION = 'write'
        USRMSH_2dHYD_NPLAN = 2


      !Allocates a 3D Mesh for Useroutpu: here VSPRES
        call ALMESH(USRMSH,'USRMSH',41,SPHERI,CFG,SIS_FILES(SISGEO)%LU,
     &            EQUA,NPLAN=USRMSH_NPLAN,
     &            FILE_FORMAT=SIS_FILES(SISGEO)%FMT)

        do I =1,USRMSH%NPTFR
            USRMSH%NBOR%I(I) = I
        END do


      !Allocates a 3D Mesh for Useroutpu: here VSPHYD
        call ALMESH(USRMSH_2dHYD,'USRHYD',41,SPHERI,CFG,
     &            SIS_FILES(SISGEO)%LU,
     &            EQUA,NPLAN=USRMSH_2dHYD_NPLAN,
     &            FILE_FORMAT=SIS_FILES(SISGEO)%FMT)

        do I =1,USRMSH_2dHYD%NPTFR
            USRMSH_2dHYD%NBOR%I(I) = I
        END do



C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------



        !SET HERE THE NAMES OF THE PRINTOUT VARIABLES for the USER File VSPRES
                do I = 1,NumVarUR3D2RES
                    if (I.le.3+NSICLA) Then
                        UR3D_FILES_OUTVAR(I) = .true.
                    else
                        UR3D_FILES_OUTVAR(I) = .false.
                    endif
            write(unit=vlabel, fmt='(A5,I2,A25)') 'Fraction_Class_',I-3
                    UR3D_FILES_LABELS(I) = vlabel
                enddo
                !Example for manually overwritting LEN=32
                UR3D_FILES_LABELS(1)= 'Z [m]                           '!'Profile_Elevation               '
                UR3D_FILES_LABELS(2)= 'Profile_D50 [m]                 '
                UR3D_FILES_LABELS(3)= 'Profile_Error [-]               '




        !SET HERE THE NAMES OF THE PRINTOUT VARIABLES for the USER File VSPHYD
                do I = 1,NumVar2dHYD
                    UR2dHYD_FILES_OUTVAR(I) = .true.
                enddo

             UR2dHYD_FILES_LABELS(1)= 'Z [m]                           '!'Z [m] and ZF [m]                '
             UR2dHYD_FILES_LABELS(2)= 'U [m/s]                         '
             UR2dHYD_FILES_LABELS(3)= 'V [m/s]                         '
             UR2dHYD_FILES_LABELS(4)= 'W [m/s]                         '
             UR2dHYD_FILES_LABELS(5)= 'Scalar Velocity [m/s]           '
             UR2dHYD_FILES_LABELS(6)= 'Tau [N/m**2]                    '



C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
    ! ALLOCATES THE BLOCK CONNECTING A VARIABLE NAME TO ITS ARRAY     !
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------



            call ALLBLO(URBLOC3D, 'URBL3D')
            call ALLBLO(URBLOC2dHYD, 'URB2DH')


C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
    !OPENS THE FILES FOR WRITING
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

        do I = 3, 4
            if(NCSIZE.LE.1) THEN
!             SCALAR
              OPEN(CP_FILES(I)%LU,FILE=CP_FILES(I)%TELNAME,
     &             ACTION=CP_FILES(I)%ACTION,FORM='UNFORMATTED')
            else
!             PARALLEL, FILE TYPE: SCAL
              if(CP_FILES(I)%TYPE(1:4).EQ.'SCAL') THEN
                OPEN(CP_FILES(I)%LU,
     &               FILE=TRIM(CP_FILES(I)%TELNAME),
     &               ACTION=CP_FILES(I)%ACTION,FORM='UNFORMATTED')
!             PARALLEL, OTHER FILE TYPE
            else
              OPEN(CP_FILES(I)%LU,
     &               FILE=TRIM(CP_FILES(I)%TELNAME)
     &               //EXTENS(NCSIZE-1,IPID),
     &               ACTION=CP_FILES(I)%ACTION,FORM='UNFORMATTED')
              endif
            endif

        enddo !CP_FILES


C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
    !WRITES THE HEADER OF THE RESFILES
C-----------------------------------------------------------------------

        call CREATE_DATASET_SERAFIN(CP_FILES(4)%LU,
     &                          '2D Hydraulic Parameters in 3D  '//
     &                    '                                         ',
     &                          NumVar2dHYD,
     &                          UR2Dhyd_FILES_LABELS,
     &                          UR2Dhyd_FILES_OUTVAR,
     &                          CP_FILES(4)%FMT)

        call CREATE_DATASET_SERAFIN(CP_FILES(3)%LU,
     &                          'UserOutput3D                   '//
     &                    '                                         ',
     &                          NumVarUR3D2RES,
     &                          UR3D_FILES_LABELS,
     &                          UR3D_FILES_OUTVAR,
     &                          CP_FILES(3)%FMT)

    !WRITES THE MESH INFORMATION IN THE OUTPUT FILE :

        do I = 3, 3 !ubound(CP_FILES)
        call WRITE_MESH(CP_FILES(I)%FMT, ! RESULTS FILE FORMAT
     &                  CP_FILES(I)%LU,  ! LU FOR RESULTS FILE
     &                  USRMSH,          ! CHARACTERISES MESH
     &                  USRMSH_NPLAN,  ! NUMBER OF PLANES /NA/
     &                  MARDAT,        ! START DATE
     &                  MARTIM,        ! START TIME
     &                  I_ORIG,J_ORIG) ! COORDINATES OF THE ORIGIN.
        enddo

        do I = 4, 4 !ubound(CP_FILES)
        call WRITE_MESH(CP_FILES(I)%FMT, ! RESULTS FILE FORMAT
     &                  CP_FILES(I)%LU,  ! LU FOR RESULTS FILE
     &                  USRMSH_2dHYD,          ! CHARACTERISES MESH
     &                  USRMSH_2dHYD_NPLAN,    ! NUMBER OF PLANES /NA/
     &                  MARDAT,        ! START DATE
     &                  MARTIM,        ! START TIME
     &                  I_ORIG,J_ORIG) ! COORDINATES OF THE ORIGIN.
        enddo


C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C     Inits output Vectors
C-----------------------------------------------------------------------

          !'VSPRES
          do k = 1, nsicla
          call BIEF_ALLVEC(1,VSP_FRA(K),'VSPFRA',41,1,1,USRMSH)
          enddo

          call BIEF_ALLVEC(1,VSP_D,     'VSP__D',41,1,1,USRMSH)
          call BIEF_ALLVEC(1,VSP_D50,   'VSPD50',41,1,1,USRMSH)
          call BIEF_ALLVEC(1,VSP_ERROR, 'VSP_ER',41,1,1,USRMSH)

          !'VSPHYD
          do k = 1, NumVar2dHYD
            call BIEF_ALLVEC(1,Ur2dHYD(k),'VSPHYD',41,1,1,USRMSH_2dHYD)
          enddo

      RETURN
      END
