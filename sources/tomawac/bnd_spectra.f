!                    ******************
                     MODULE BND_SPECTRA
!                    ******************
!
!
!***********************************************************************
!     TOMAWAC   V7P3                                   23/02/2017
!***********************************************************************
!
!brief    MODULE TO IMPOSE SPECTRA ON OPEN BOUNDARIES FROM AN
!         EXTERNAL MESH FILE
!
!     history  A. JOLY (EDF - LNHE)
!     +        23/02/2017
!     +        V7P3
!     +   CREATED
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
!
      IMPLICIT NONE
!
      PRIVATE
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      PUBLIC :: IMPOSE_BND_SPECTRA
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     
      ! TIME VARIABLES
      DOUBLE PRECISION :: TV1,TV2
      INTEGER :: RECORD1,RECORD2
      DOUBLE PRECISION :: TIME1,TIME2
      !ERROR INTEGER WHEN READING THE MESH
      INTEGER :: IERR
      ! BOUNDARY SPECTRA VARIABLES
      CHARACTER(LEN=80) :: BC_TITLE ! TITLE
      INTEGER :: BC_NVAR   ! NUMBER OF VARIABLES
      INTEGER :: BC_NPOIN  ! NUMBER OF MESH NODES
      INTEGER :: BC_TYP_ELEM  ! TYPE OF ELEMENT
      INTEGER :: BC_NELEM  ! NUMBER OF ELEMENTS
      INTEGER :: BC_NDP    ! NUMBER OF ELEMENT FACES
      INTEGER :: BC_NPLAN  ! NUMBER OF PLAN
      INTEGER :: BC_NPTFR  ! NUMBER OF BOUNDARY NODES
      INTEGER :: BC_NPTIR  ! NUMBER OF INTERFACES
      INTEGER :: BC_NDIM  ! DIMENSION OF THE MESH
      CHARACTER(LEN=16), ALLOCATABLE :: BC_VARLIST(:)
      CHARACTER(LEN=16), ALLOCATABLE :: BC_UNITLIST(:)
      INTEGER :: BC_NT  ! NUMBER TIME STEPS
      DOUBLE PRECISION, ALLOCATABLE :: BC_X(:),BC_Y(:) !X AND Y COORDINATES ON SPE MESH
      DOUBLE PRECISION, ALLOCATABLE :: BC_FREQ(:),BC_TETA(:) !F AND THETA COORDINATES OF SPECTRUM
      DOUBLE PRECISION, ALLOCATABLE :: BC_VAL1(:),BC_VAL2(:) !VALUES A TIME TV1 AND TV2
      !FOR POINT OF THE SPECTRA FILE
      INTEGER, ALLOCATABLE :: BC_CORR(:) ! INDEX OF THE CLOSEST SPECTRUM TO A LIQUID BOUNDARY NODE
      ! OPTIONAL ARGUMENTS
      INTEGER NNELEBD, TYP_BND
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!***********************************************************************
      CONTAINS
!***********************************************************************

!                    *****************************
                     SUBROUTINE IMPOSE_BND_SPECTRA
!                    *****************************
!     
     &     (IMP_FILE,LT,AT,FBOR,NPTFR,NPLAN,NF)
!     
!***********************************************************************
!     TOMAWAC   V7P3                                   23/02/2017
!***********************************************************************
!
!     brief    READS SPECTRA SAVED IN A MESH FILE AND IMPOSES THEM ON
!     +        OPEN BOUNDARY POINTS
!
!     history  A. JOLY (EDF - LNHE)
!     +        23/02/2017
!     +        V7P3
!     +   CREATED
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| IMP_FILE       |-->| MESH FILE WITH THE IMPOSED SPECTRA
!| LT             |-->| NUMBER OF THE TIME STEP CURRENTLY SOLVED
!| AT             |-->| COMPUTATION TIME
!| FBOR           |<->| SPECTRAL VARIANCE DENSITY AT THE BOUNDARIES
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NF             |-->| NUMBER OF FREQUENCIES
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_HERMES
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC
      USE BIEF_DEF, ONLY : BIEF_FILE
      USE DECLARATIONS_TOMAWAC, ONLY : NPSPE,XSPE,YSPE,UNITSPE,PHASSPE,
     &                                 X,Y,NBOR,LIFBOR,F1,RAISF
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     
      TYPE(BIEF_FILE), INTENT(IN)    :: IMP_FILE
      INTEGER, INTENT(IN)            :: LT
      DOUBLE PRECISION, INTENT(IN)   :: AT
      INTEGER, INTENT(IN)            :: NPTFR,NPLAN,NF
      DOUBLE PRECISION, INTENT(INOUT):: FBOR(NPTFR,NPLAN,NF)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPTFR, ISPE, IPOIN, IPLAN, IFF
      DOUBLE PRECISION DIST_SPE,DIST
      DOUBLE PRECISION COEFT
      DOUBLE PRECISION FCL1,FCL2
      INTEGER BC_NF,BC_NPLAN
      DOUBLE PRECISION BC_F1,BC_F2,BC_RAISF
      DOUBLE PRECISION TETA_TEMP,F_TEMP
!
!-----------------------------------------------------------------------
! DURING THE FIRST TIME STEP
!-----------------------------------------------------------------------
      IF(LT.EQ.0)THEN
! 1/ FIND THE CLOSEST SPECTRUM FOR EACH LIQUID BOUNDARY NODE
        IF(ALLOCATED(BC_CORR))DEALLOCATE(BC_CORR)
        ALLOCATE(BC_CORR(NPTFR))
        DO IPTFR=1,NPTFR
          IF(LIFBOR(IPTFR).EQ.KENT)THEN
            DIST_SPE=1.D99
            DO ISPE=1,NPSPE
              DIST=SQRT((XSPE(ISPE)-X(NBOR(IPTFR)))**2+
     &                  (YSPE(ISPE)-Y(NBOR(IPTFR)))**2)
              IF(DIST.LE.DIST_SPE)THEN
                BC_CORR(IPTFR)=ISPE
                DIST_SPE=DIST
              ENDIF
            ENDDO
          ELSE
            BC_CORR(IPTFR)=0
          ENDIF
        END DO
! 2/ READ THE MESH THE SPECTRA TO BE IMPOSED ON THE BOUNDARY
        ! MESH INFO
        CALL READ_MESH_INFO(IMP_FILE%FMT,IMP_FILE%LU,
     &          BC_TITLE,BC_NVAR,
     &          BC_NPOIN,BC_TYP_ELEM,BC_NELEM,BC_NPTFR,BC_NPTIR,
     &          BC_NDP,BC_NPLAN,TYP_BND,NNELEBD)
        CALL GET_MESH_DIMENSION(IMP_FILE%FMT,IMP_FILE%LU,
     &          BC_NDIM,IERR)
        ! READ THE NAME OF THE VARIABLES
        IF(ALLOCATED(BC_VARLIST))DEALLOCATE(BC_VARLIST)
        IF(ALLOCATED(BC_UNITLIST))DEALLOCATE(BC_UNITLIST)
        ALLOCATE(BC_VARLIST(BC_NVAR))
        ALLOCATE(BC_UNITLIST(BC_NVAR))
        CALL GET_DATA_VAR_LIST(IMP_FILE%FMT,IMP_FILE%LU,
     &          BC_NVAR,BC_VARLIST,BC_UNITLIST,IERR)
! 3/ READ THE NUMBER OF TIME STEPS AND THE FIRST TIME
        ! GET NUMBER OF TIME STEPS
        CALL GET_DATA_NTIMESTEP(IMP_FILE%FMT,IMP_FILE%LU,
     &          BC_NT,IERR)
        ! READ THE FIRST TIME
        RECORD1 = 0
        RECORD2 = 1
        CALL GET_DATA_TIME(IMP_FILE%FMT,IMP_FILE%LU,
     &          RECORD1,TIME1,IERR)
        TV1=(TIME1-PHASSPE)*UNITSPE
        IF(TV1.GT.AT) THEN
          WRITE(LU,*)'*********************************************'
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)'LE PREMIER ENREGISTREMENT DU FICHIER'
            WRITE(LU,*)'  ',TV1,' EST POSTERIEUR AU TEMPS '
            WRITE(LU,*)'  DU DEBUT DU CALCUL',AT
          ELSE
            WRITE(LU,*)'THE FIRST RECORDING OF THE FILE'
            WRITE(LU,*)'  ',TV1,' IS OLDER THAN THE BEGINNING'
            WRITE(LU,*)'  OF THE COMPUTATION',AT
          ENDIF
          WRITE(LU,*)'*********************************************'
          CALL PLANTE(1)
          STOP
        ENDIF
! 4/ READ X AND Y COORDINATES
        IF(ALLOCATED(BC_X))DEALLOCATE(BC_X)
        IF(ALLOCATED(BC_Y))DEALLOCATE(BC_Y)
        IF(ALLOCATED(BC_FREQ))DEALLOCATE(BC_FREQ)
        IF(ALLOCATED(BC_TETA))DEALLOCATE(BC_TETA)
        ALLOCATE(BC_X(BC_NPOIN))
        ALLOCATE(BC_Y(BC_NPOIN))
        ALLOCATE(BC_FREQ(BC_NPOIN))
        ALLOCATE(BC_TETA(BC_NPOIN))
!
        CALL GET_MESH_COORD(IMP_FILE%FMT,IMP_FILE%LU,
     &            1,BC_NDIM,BC_NPOIN,BC_X,IERR)
        CALL GET_MESH_COORD(IMP_FILE%FMT,IMP_FILE%LU,
     &          2,BC_NDIM,BC_NPOIN,BC_Y,IERR)
! 5/ CHECK THAT THE MESH READ HAS THE SAME THE FREQUENCIES AND
!    DIRECTIONS AS IN THE SIMULATION
        BC_NF=0
        BC_NPLAN=0
        BC_F1=10.D10
        BC_F2=10.D10
        BC_RAISF=0.D0
        DO IPOIN=1,BC_NPOIN
          IF((BC_X(IPOIN).EQ.0.0).AND.(BC_Y(IPOIN).GE.0.0))THEN
            TETA_TEMP=ATAN2(BC_X(IPOIN),BC_Y(IPOIN))+ATAN(1.D0)*8.D0
            TETA_TEMP=MOD(TETA_TEMP,ATAN(1.D0)*8.D0)
            BC_NF=BC_NF+1
            IF(SIN(TETA_TEMP).NE.0.D0)THEN
              F_TEMP=BC_X(IPOIN)/SIN(BC_TETA(IPOIN))
            ELSE
              F_TEMP=BC_Y(IPOIN)/COS(BC_TETA(IPOIN))
            ENDIF
            IF(F_TEMP.LT.BC_F1)THEN
              BC_F2=BC_F1
              BC_F1=F_TEMP
            ELSEIF(F_TEMP.LT.BC_F2)THEN
              BC_F2=F_TEMP
            ENDIF
          ENDIF
        END DO
        BC_RAISF=BC_F2/BC_F1
        BC_NPLAN=BC_NPOIN/BC_NF
        IF(ABS(BC_F1-F1).GT.1.D-6) THEN
          WRITE(LU,*)'*********************************************'
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)'LA FREQUENCE MINIMALE DES SPECTRES LUS'
            WRITE(LU,*)'N''EST PAS EGALE CELLE DE LA SIMULATION.'
            WRITE(LU,*)'- FREQUENCE DE LA SIMULATION:',F1
            WRITE(LU,*)'- FREQUENCE LUE:',BC_F1
          ELSE
            WRITE(LU,*)'THE MINIMAL FREQUENCY OF THE SPECTRA READ'
            WRITE(LU,*)'IS NOT EQUAL TO THE FREQUENCY OF THE'
            WRITE(LU,*)'SIMULATION.'
            WRITE(LU,*)'- FREQUENCY OF THE SIMULATION:',F1
            WRITE(LU,*)'- FREQUENCY READ:',BC_F1
          ENDIF
          WRITE(LU,*)'*********************************************'
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(ABS(BC_RAISF-RAISF).GT.1.D-6) THEN
          WRITE(LU,*)'*********************************************'
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)'LA RAISON FREQUENTIELLE DES SPECTRES LUS'
            WRITE(LU,*)'N''EST PAS EGALE CELLE DE LA SIMULATION.'
            WRITE(LU,*)'- RAISON FREQUENTIELLE DE LA SIMULATION:',RAISF
            WRITE(LU,*)'- RAISON FREQUENTIELLE LUE:',BC_RAISF
          ELSE
            WRITE(LU,*)'THE FREQUENTIAL RATIO OF THE SPECTRA READ'
            WRITE(LU,*)'IS NOT EQUAL TO THE RATIO OF THE SIMULATION.'
            WRITE(LU,*)'- FREQUENTIAL RATIO OF THE SIMULATION:',RAISF
            WRITE(LU,*)'- FREQUENTIAL RATIO READ:',BC_RAISF
          ENDIF
          WRITE(LU,*)'*********************************************'
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(BC_NF.NE.NF) THEN
          WRITE(LU,*)'*********************************************'
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)'LE NOMBRE DE FREQUENCES DES SPECTRES LUS N''EST'
            WRITE(LU,*)'PAS EGAL AU NOMBRE DE FREQUENCE DE LA'
            WRITE(LU,*)'SIMULATION.'
            WRITE(LU,*)'- NOMBRE DE FREQUENCES DE LA SIMULATION:',NF
            WRITE(LU,*)'- NOMBRE DE FREQUENCES LUES:',BC_NF
          ELSE
            WRITE(LU,*)'THE NUMBER OF FREQUENCY OF THE SPECTRA READ'
            WRITE(LU,*)'IS NOT EQUAL TO THE NUMBER OF FREQUENCY OF THE'
            WRITE(LU,*)'SIMULATION.'
            WRITE(LU,*)'- NUMBER OF FREQUENCY OF THE SIMULATION:',NF
            WRITE(LU,*)'- NUMBER OF FREQUENCY READ:',BC_NF
          ENDIF
          WRITE(LU,*)'*********************************************'
          CALL PLANTE(1)
          STOP
        ENDIF
        IF(BC_NPLAN.NE.NPLAN) THEN
          WRITE(LU,*)'*********************************************'
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)'LE NOMBRE DE DIRECTIONS DES SPECTRES LUS N''EST'
            WRITE(LU,*)'PAS EGAL AU NOMBRE DE DIRECTIONS DE LA'
            WRITE(LU,*)'SIMULATION.'
            WRITE(LU,*)'- NOMBRE DE DIRECTIONS DE LA SIMULATION:',NPLAN
            WRITE(LU,*)'- NOMBRE DE DIRECTIONS LUES:',BC_NPLAN
          ELSE
            WRITE(LU,*)'THE NUMBER OF DIRECTIONS OF THE SPECTRA READ'
            WRITE(LU,*)'IS NOT EQUAL TO THE NUMBER OF DIRECTIONS OF THE'
            WRITE(LU,*)'SIMULATION.'
            WRITE(LU,*)'- NUMBER OF DIRECTIONS OF THE SIMULATION:',NPLAN
            WRITE(LU,*)'- NUMBER OF DIRECTIONS READ:',BC_NPLAN
          ENDIF
          WRITE(LU,*)'*********************************************'
          CALL PLANTE(1)
          STOP
        ENDIF
! 6/ CALCULATE FREQ AND DIR
        DO IPOIN=1,BC_NPOIN
          BC_TETA(IPOIN)=ATAN2(BC_X(IPOIN),BC_Y(IPOIN))+ATAN(1.D0)*8.D0
          BC_TETA(IPOIN)=MOD(BC_TETA(IPOIN),ATAN(1.D0)*8.D0)
          IF(SIN(BC_TETA(IPOIN)).NE.0.D0)THEN
            BC_FREQ(IPOIN)=BC_X(IPOIN)/SIN(BC_TETA(IPOIN))
          ELSE
            BC_FREQ(IPOIN)=BC_Y(IPOIN)/COS(BC_TETA(IPOIN))
          ENDIF
        ENDDO
! 7/ ALLOCATE THE TEMPORARY SPECTRUM AT THE TWO TIME STEPS
        IF(ALLOCATED(BC_VAL1))DEALLOCATE(BC_VAL1)
        IF(ALLOCATED(BC_VAL2))DEALLOCATE(BC_VAL2)
        ALLOCATE(BC_VAL1(BC_NPOIN))
        ALLOCATE(BC_VAL2(BC_NPOIN))
!-----------------------------------------------------------------------
      ENDIF
!-----------------------------------------------------------------------
! FOR ALL TIME STEPS
!-----------------------------------------------------------------------
! 8/ READ THE SECOND TIME STEP
      DO
        CALL GET_DATA_TIME(IMP_FILE%FMT,IMP_FILE%LU,
     &          RECORD2,TIME2,IERR)
        TV2=(TIME2-PHASSPE)*UNITSPE
        IF(TV2.LT.AT) THEN
          RECORD1 = RECORD2
          RECORD2 = RECORD2 + 1
          TV1 = TV2
          IF (RECORD2.GT.BC_NT) THEN
            WRITE(LU,*)'*****************************************'
            IF(LNG.EQ.1) THEN
              WRITE(LU,*)'LA FIN DU FICHIER DE CONDITION LIMITE'
              WRITE(LU,*)'EST ATTEINTE AU TEMPS : ',AT
            ELSE
              WRITE(LU,*)'THE END OF THE BOUNDARY CONDITION FILE'
              WRITE(LU,*)'IS REACHED AT TIME: ',AT
            ENDIF
            WRITE(LU,*)'*****************************************'
            CALL PLANTE(1)
            STOP
          ENDIF
        ELSE
          EXIT
        ENDIF
      ENDDO
!
      COEFT=(AT-TV1)/(TV2-TV1)
! 9/ IMPOSE THE SPECTRA
      DO IPTFR=1,NPTFR
        IF(LIFBOR(IPTFR).EQ.KENT)THEN
          CALL GET_DATA_VALUE(IMP_FILE%FMT,IMP_FILE%LU,
     &         RECORD1,BC_VARLIST(BC_CORR(IPTFR)),BC_VAL1,BC_NPOIN,IERR)
          CALL GET_DATA_VALUE(IMP_FILE%FMT,IMP_FILE%LU,
     &         RECORD2,BC_VARLIST(BC_CORR(IPTFR)),BC_VAL2,BC_NPOIN,IERR)
!
          DO IPLAN=1,NPLAN
            DO IFF=1,NF
              IPOIN=(IPLAN+NPLAN*(IFF-1))
              FCL1=BC_VAL1(IPOIN)
              FCL2=BC_VAL2(IPOIN)
              FBOR(IPTFR,IPLAN,IFF)=FCL1+(FCL2-FCL1)*COEFT
            ENDDO
          ENDDO
        ENDIF
      ENDDO
!
      RETURN
      END SUBROUTINE IMPOSE_BND_SPECTRA
!
      END MODULE BND_SPECTRA
