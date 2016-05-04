!                    ******************
                     SUBROUTINE LECWAC1
!                    ******************
!
!
!***********************************************************************
! ARTEMIS   V7P0                                  07/2014
!***********************************************************************
!
!brief    READ A SPECTRUM IN 1 POINT AND 1 DATE FROM A TOMAWAC SPE FILE
!         THIS SPECTRUM IS APPLIED ON THE WAVE INCIDENT BOURNDARIES
!
!history  C PEYRARD (LNHE)
!+        07/2014
!+        V7P0
!+        creation
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
      USE BIEF, ONLY : FIND_VARIABLE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      CHARACTER(LEN=16),ALLOCATABLE :: VARNAME(:),VARUNIT(:)
      INTEGER      NBI1,NBCL
      INTEGER      IPLAN,IFF,ID,KK,J,IDEC,JD
      DOUBLE PRECISION :: COORD(MAXDIR*MAXFRE)
      REAL         CL1(MAXDIR,MAXFRE,1)
      REAL         XTWC(MAXDIR,MAXFRE),YTWC(MAXDIR,MAXFRE)
      REAL         STOCKD(MAXDIR)
      REAL         PI,DEGRAD,EPS
      DOUBLE PRECISION :: READTWC(MAXDIR*MAXFRE)
      PARAMETER(PI = 3.1415926535897932384626433D0 , DEGRAD=PI/180.D0)
      INTEGER :: IERR
      CHARACTER(LEN=8) :: FFORMAT

!     A AJOUTER DANS LE DICO
!      NFTWC  = 50
!      NDTWC  = 48
!      TPSTWC =30000D0
!.....CONTROLE SIZE OF TABLES FROM TOMAWAC
      IF (NDTWC.GT.MAXDIR) THEN
        WRITE(LU,*) 'TOO MANY DIRECTIONS IN TOMAWAC SPECTRUM  '
        WRITE(LU,*) 'INCREASE MAXDIR IN declarations_artemis.f'
        CALL PLANTE(1)
        STOP
      ENDIF
      IF (NFTWC.GT.MAXFRE) THEN
        WRITE(LU,*) 'TOO MANY FREQUENCIES IN TOMAWAC SPECTRUM  '
        WRITE(LU,*) 'INCREASE MAXFRE IN declarations_artemis.f'
        CALL PLANTE(1)
        STOP
      ENDIF
!-------------------------------------------
      WRITE(LU,*) '==================================================='
      WRITE(LU,*) '========== READING SPECTRUM FROM TOMAWAC =========='
!=====READING SELAPHIN FILE (.spe)
      NBI1 = ART_FILES(ARTTC1)%LU
      FFORMAT = ART_FILES(ARTTC1)%FMT
      CALL GET_MESH_COORD(FFORMAT,NBI1,1,2,NFTWC*NDTWC,COORD,IERR)
      CALL CHECK_CALL(IERR,'GET_MESH_COORD:X:NBI1')
      DO IPLAN = 1,NDTWC
        DO IFF = 1,NFTWC
          XTWC(IPLAN,IFF) = COORD((IFF-1)*NDTWC+IPLAN)
        ENDDO
      ENDDO
      CALL GET_MESH_COORD(FFORMAT,NBI1,2,2,NFTWC*NDTWC,COORD,IERR)
      CALL CHECK_CALL(IERR,'GET_MESH_COORD:Y:NBI1')
      DO IPLAN = 1,NDTWC
        DO IFF = 1,NFTWC
          YTWC(IPLAN,IFF) = COORD((IFF-1)*NDTWC+IPLAN)
        ENDDO
      ENDDO
!
!
!.....READ DATA
!
      CALL GET_DATA_NVAR(FFORMAT,NBI1,NBCL,IERR)
      CALL CHECK_CALL(IERR,'GET_DATA_NVAR')
!
      ALLOCATE(VARNAME(NBCL),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'VARNAME')
      ALLOCATE(VARUNIT(NBCL),STAT=IERR)
      CALL CHECK_ALLOCATE(IERR,'VARUNIT')
      CALL GET_DATA_VAR_LIST(FFORMAT,NBI1,NBCL,VARNAME,VARUNIT,IERR)
      CALL CHECK_CALL(IERR,'GET_DATA_VAR_LIST')
!
      ! There should be only one variable in the tomawac file
      IF(NBCL.NE.1) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'TROP DE VARIABLES DANS ',
     &               ART_FILES(ARTTC1)%NAME
        IF(LNG.EQ.2) WRITE(LU,*) 'TOO MANY VARIABLES IN ',
     &               ART_FILES(ARTTC1)%NAME
        CALL PLANTE(1)
        STOP
      ENDIF
      ! Reading data for THE variable
      CALL FIND_VARIABLE(FFORMAT,NBI1,VARNAME(1),READTWC,NFTWC*NDTWC,
     &                   IERR,TIME=TPSTWC)
      CALL CHECK_CALL(IERR,'FIND_VARIABLE')
      KK=1
!     order DATA into line,column format : CL1
      DO IFF=1,NFTWC
        DO IPLAN=1,NDTWC
          CL1(IPLAN,IFF,1)=READTWC(KK)
          KK=KK+1
        ENDDO
      ENDDO
!
!.....END READING
!==========================================



!.......SPECTRUM CONSTRUCTION
!=============================
!=====DIRECTION
      DO ID=1,NDTWC
!       COMPUTE DIRECTION FROM .spe file
        DIRTWC(NDTWC-ID+1)  = ATAN2(YTWC(ID,NFTWC),XTWC(ID,NFTWC))
!       Into degres
        DIRTWC(NDTWC-ID+1)  = DIRTWC(NDTWC-ID+1)/DEGRAD
!       Into [0,360]
        IF (DIRTWC(NDTWC-ID+1).LT.0D0) THEN
          DIRTWC(NDTWC-ID+1)  =360D0+DIRTWC(NDTWC-ID+1)
        ENDIF
      ENDDO
!     FIND THE SMALLEST DIRECTION IN [0;360] => IDEC
      IDEC=0
      EPS=1000D0
      DO ID=1,NDTWC
!       STOCK THE DIRECTION
        STOCKD(ID)=DIRTWC(ID)
        IF (ABS(DIRTWC(ID)).LT.EPS) THEN
          IDEC=ID
          EPS=ABS(DIRTWC(ID))
        ENDIF
      ENDDO
!     ORDER DIRTWC FROM 0 to 360
      DO ID=1,NDTWC
        IF (ID.GE.IDEC) THEN
          J=ID-IDEC+1
        ELSE
          J=NDTWC+ID-IDEC+1
        ENDIF
        DIRTWC(J)=STOCKD(ID)
      ENDDO
      DIRTWC(NDTWC+1)=DIRTWC(1)+360D0
!
!=====FREQUENCY
      DO IFF=1,NFTWC
        FREQTWC(IFF)  = SQRT(XTWC(1,IFF)**2+YTWC(1,IFF)**2)
      ENDDO
!=====AMPLITUDE
      DO ID=1,NDTWC
!       JD correspond to ID in the CL1 table
        JD=NDTWC-ID+1
!       J correspond to ID in the DIRTWC table
        IF (ID.GE.IDEC) THEN
          J=ID-IDEC+1
        ELSE
          J=NDTWC+ID-IDEC+1
        ENDIF
        DO IFF=1,NFTWC
!         Compute SPETWC(J,I) <=> (DIRTWC(J) ; FREQTWC(I))
          SPETWC(J,IFF)= CL1(JD,IFF,1)
          IF (J.EQ.1) THEN
            SPETWC(NDTWC+J,IFF)= SPETWC(J,IFF)
          ENDIF
        ENDDO
      ENDDO
! END BUILDING
!===========================

      WRITE(LU,*) '========END READING SPRECTRUM FROM TOMAWAC========='
      WRITE(LU,*) '                                                   '

! TEST CHAINAGE
!      DO ID=1,NDTWC+1
!        DD= DIRTWC(ID)
!        DO IFF=1,NFTWC
!          FF=FREQTWC(IFF)
!          SPETWC(ID,IFF) =SPE(FF)*SPD(DD-270D0)
!        ENDDO
!      ENDDO
!-----------------------------------------------------------------------
!
      RETURN
      END
