!
!
!  cvsp_init_from_layers and cvsp_output_init have been added here
!  as a fix to the Python scan problem: they are not called by the
!  standard Sisyphe, thus not compiled
!
!
!
! global variable for recirculation per class 
        MODULE RECIRCMODUL 
!              use declarations_sisyphe :: nsicla 
 
        IMPLICIT NONE 
!              DOUBLE PRECISION, SAVE  :: Q_outCLA(NSICLA) 
              DOUBLE PRECISION, SAVE  :: Q_OUTCLA(10)=0.0000001 
!
              INTEGER       :: OUTPUTCOUNTER = 0
!
        END MODULE RECIRCMODUL
!                       *****************
                        SUBROUTINE NOEROD
!                       *****************
!
     & (H , ZF , ZR , Z , X , Y , NPOIN , CHOIX , NLISS )
!
!***********************************************************************
! SISYPHE VERSION 5.1                             C. LENORMANT
!
! COPYRIGHT EDF-DTMPL-SOGREAH-LHF-GRADIENT
!***********************************************************************
!
!     FONCTION  : IMPOSE LA VALEUR DE LA COTE DU FOND NON ERODABLE  ZR
!
!
!     RQ: LES METHODES DE TRAITEMENT DES FONDS NON ERODABLES PEUVENT CONDUIRE
!     A ZF < ZR A CERTAINS PAS DE TEMPS, POUR PALLIER A CELA ON PEUT CHOISIR
!     CHOISIR DE LISSER LA SOLUTION OBTENUE i.e NLISS > 0.
!
!     FUNCTION  : IMPOSE THE RIGID BED LEVEL  ZR
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________
! |      NOM       |MODE|                   ROLE
! |________________|____|______________________________________________
! |   H            | -->| WATER DEPTH
! |   ZF           | -->| BED LEVEL
! |   ZR           |<-- | RIGID BED LEVEL
! |   Z            | -->| FREE SURFACE
! |   X,Y          | -->| 2D COORDINATES
! |   NPOIN        | -->| NUMBER OF 2D POINTS
! |   CHOIX        | -->| SELECTED METHOD FOR THE TREATMENT OF RIGID BEDS
! |   NLISS        |<-->| NUMBER OF SMOOTHINGS
! |________________|____|______________________________________________
! MODE : -->(INPUT), <--(RESULT), <-->(MODIFIED DATA)
!-----------------------------------------------------------------------
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER, INTENT(IN):: NPOIN , CHOIX
      INTEGER, INTENT(INOUT):: NLISS
!
      DOUBLE PRECISION, INTENT(IN)::  Z(NPOIN) , ZF(NPOIN)
      DOUBLE PRECISION , INTENT(IN)::  X(NPOIN) , Y(NPOIN), H(NPOIN)
      DOUBLE PRECISION , INTENT(INOUT)::  ZR(NPOIN)
!
!-----------------------------------------------------------------------
      INTEGER I
!--------------------
! RIGID BEDS POSITION
!---------------------
!
!       DEFAULT VALUE:       ZR=ZF-100
!
        CALL OV( 'X=Y+C     ',ZR,ZF,ZF,-0.3D0,NPOIN)
!
        DO I=1,NPOIN
        IF (X(I) < 100.0D0) THEN
           ! ZR(I)=ZF(I)
        END IF
        END DO
!
!------------------
! SMOOTHING OPTION
!------------------
!       NLISS : NUMBER OF SMOOTHING IF  (ZF - ZR ) NEGATIVE
!                DEFAULT VALUE : NLISS = 0 (NO SMOOTHING)
!
        NLISS = 0
!
!
      RETURN
      END SUBROUTINE NOEROD
!                         ********************* 
                          SUBROUTINE INIT_COMPO 
!                         ********************* 
! 
     &(NCOUCHES) 
! 
!*********************************************************************** 
! SISYPHE VERSION 6.2 
!
!           Version for the FLUME CASES OF ASTRID BLOM (2003)
!              @ DELFT / DELTARES 
! 
!           prepared by uwe.merkel@uwe-merkel.com
!
!           Choose:  BLOMCASE = 'B1' or A1 / A2 / T5 / T10 
! 
!*********************************************************************** 
! 
!     FONCTION  : DISTRIBUTION DES CLASSES 
!                 % PAR COUCHE, STRATIFICATION 
!     SUBROUTINE A REMPLIR PAR l'UTILISATEUR 
! 
! 
!     FUNCTION  : INITIAL FRACTION DISTRIBUTION, STRATIFICATION, 
!                 VARIATION IN SPACE 
! 
!----------------------------------------------------------------------- 
!                             ARGUMENTS 
! .________________.____.______________________________________________ 
! |      NOM       |MODE|                   ROLE 
! |________________|____|______________________________________________ 
! |                |    | 
! |    AVAIL       |<-- | SEDIMENT FRACTION FOR EACH LAYER, CLASS AND NODE 
! |                |    | AVAIL(NPOIN,10,NSICLA) 
! |    ES          |<-- | THICKNESS FOR EACH LAYER AND NODE ES(NPOIN,10) 
! |    NCOUCHES    |--> | NUMBER OF LAYER FOR EACH POINT 
! |    NSICLA      |--> | NUMBER OF SIZE-CLASSES OF BED MATERIAL 
! |                |    | (LESS THAN 10) 
! |    NPOIN       |--> | NUMBER OF NODES 
! |________________|____|______________________________________________ 
! MODE : -->(INPUT), <--(RESULT), <--> (MODIFIED INPUT) 
!----------------------------------------------------------------------- 
! PROGRAMME APPELANT : INIT_AVAI 
! PROGRAMMES APPELES : NONE 
!*********************************************************************** 
! 
      USE BIEF 
      USE DECLARATIONS_TELEMAC 
      USE DECLARATIONS_SISYPHE 
! 
      IMPLICIT NONE 
      INTEGER LNG,LU,KK
      CHARACTER*2 BLOMCASE
      COMMON/INFO/LNG,LU 
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
!                                       NPOIN 
      INTEGER, INTENT (INOUT)::NCOUCHES(*) 
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
      INTEGER I, J, K 
! 
!----------------------------------------------------------------------- 
! 
      DO J=1,NPOIN 
! 
        NCOUCHES(J) = NOMBLAY
        NLAYER%I(J) = NOMBLAY

        ES(J,1) = 0.03D0 

        ES(J,2) = 0.01D0
        ES(J,3) = 0.01D0
        ES(J,4) = 0.01D0
        ES(J,5) = 0.01D0
        ES(J,6) = 0.01D0
        ES(J,7) = 0.01D0
        ES(J,8) = 0.01D0

        ES(J,9) = (ZF%R(J) -ZR%R(J)-(NOMBLAY-2)*0.01D0 - ES(J,1))
!
        BLOMCASE = 'B1'
! 
        DO I = 1, NSICLA 
          DO K = 1, NCOUCHES(J) 
!        Nur für BLOOM B CASES 
            IF (BLOMCASE.EQ.'A1') THEN 
              AVAIL(J,K,I) = AVA0(I) 
            ELSEIF (BLOMCASE.EQ.'A2') THEN 
              AVAIL(J,K,I) = AVA0(I) 
            ELSEIF (BLOMCASE.EQ.'T5') THEN 
              AVAIL(J,K,I) = AVA0(I) 
            ELSEIF (BLOMCASE.EQ.'T10') THEN 
              AVAIL(J,K,I) = AVA0(I) 
            ELSEIF ((K.EQ.1)) THEN 
              AVAIL(J,K,I) = AVA0(I) 
            ELSE 
              AVAIL(J,K,1) = 0.998D0 
              AVAIL(J,K,2) = 0.001D0 
              AVAIL(J,K,3) = 0.001D0 
            ENDIF 
 
          ENDDO 
        ENDDO 
 
 
      ENDDO 
! 
!----------------------------------------------------------------------- 
!  !CONVERT TO CVSM in CASE VSMTYPE = 1
!  No user edit necessary
!-----------------------------------------------------------------------
!
      IF(VSMTYPE.EQ.1) THEN
!       FOLDER FOR HIRANO PROFILE OUTPUTS
        DO KK = 1,100
        IF(CVSMOUTPUT(KK).GT.0) THEN
          CALL LAYERS_P('./VSP_',CVSMOUTPUT(KK))
        ENDIF
        ENDDO
        CALL CVSP_INIT_FROM_LAYERS
!       OUTPUT TO SELAFIN FILE
        IF (CVSM_OUT_FULL) THEN
          CALL CVSP_OUTPUT_INIT
          CALL CVSP_WRITE_PROFILE
        ENDIF
        DO KK = 1,100
        IF (CVSMOUTPUT(KK).GT.0) THEN
          CALL CVSP_P('./','V_', CVSMOUTPUT(KK))
        ENDIF
        ENDDO
      ENDIF ! END CVSM 
      RETURN 
      END SUBROUTINE INIT_COMPO
!                    *********************
                     SUBROUTINE CONLIT_UWE
!                    *********************
!
     &(NBOR,AT)
!
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief    ALLOWS TO IMPOSE TIME VARYING BOUNDARY CONDITIONS
!+               (CONSTANT VALUES CAN BE DIRECTLY IMPOSED IN CONDIM
!+                INPUT FILE).
!+
!+
!+            ALLOWS TO IMPOSE A SAND TRANSPORT RATE AT SOME
!+                BOUNDARY NODES (QBOR AND LIQBOR). IT IS THEN NECESSARY
!+                TO ALSO IMPOSE LIEBOR = KSORT AT THESE NODES !
!
!history  E. PELTIER; C. LENORMANT; J.-M. HERVOUET
!+        11/09/1995
!+
!+
!
!history  C. MACHET
!+        07/06/2002
!+
!+
!
!history  CV
!+        19/06/2008
!+        V5P9
!+   TAKES INTO ACCOUNT CBOR_VASE AND CBOR_SABLE
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINT
!| AT             |-->| TEMPS (s)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SISYPHE
      USE DECLARATIONS_TELEMAC
      USE RECIRCMODUL
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN):: NBOR(NPTFR)
! CV: 12/06...
      DOUBLE PRECISION, INTENT(IN) :: AT
      DOUBLE PRECISION, EXTERNAL:: CGL
!...CV
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,K,KK,IFRLIQ,IRANK, NSIC
      DOUBLE PRECISION INFLOWWIDTH
!
!-----------------------------------------------------------------------
!


!     CALCULATING THE WITH OF THE INFLOW, OUR SEDIMENT INPUT HAS TO BE DISTRIBUTED OVER IT
        INFLOWWIDTH = 0.D0
      DO K=1,NPTFR
              KK = MESH%KP1BOR%I(K)
          IF (K.NE.KK) THEN
              IF((NUMLIQ%I(K).EQ.2).AND.(NUMLIQ%I(KK).EQ.2)) THEN ! FLUX FOR ONE CLASS
                IF(LIEBOR%I(K).EQ.4.AND.LIHBOR%I(K).EQ.4) THEN 
                INFLOWWIDTH = INFLOWWIDTH + MESH%LGSEG%R(K) 
!                print*,'Inflowwidth',k,mesh%lgseg%r(k),Inflowwidth
!     &           ,MESH%X%R(MESH%NBOR%I(K)),MESH%X%R(MESH%NBOR%I(KK)) 
                ENDIF 
              ENDIF ! FLUX FOR ONE CLASS
          ENDIF
      ENDDO
!
      DO  K=1,NPTFR
!
        I = NBOR(K)
!
!       HERE KADH (WALL WITH NO SLIP CONDITION) IS CHANGED INTO KLOG (WALL)
!
        IF(LIEBOR%I(K).EQ.KADH) THEN
          LIEBOR%I(K)= KLOG
        ENDIF
!
!       DIRICHLET CONDITIONS
!       EITHER ON EVOLUTION OR ON SOLID DISCHARGE
!
!       EXAMPLE 1: IMPOSED SOLID DISCHARGE - FREE BED EVOLUTION
!
!       QBOR%ADR(J)%P%R(K) IS THE SOLID DISCHARGE IMPOSED AT THE BOUNDARY
!       NODE K , CLASS OF SEDIMENT J, EXCLUDING VOIDS
!
!
        ! RECIRC INPUT ....
        IF (NUMLIQ%I(K).EQ.2) THEN
          LIEBOR%I(K)=KSORT
          LIQBOR%I(K)=KENT
          PRINT *, 
     &      'QBOR%ADR(NSIC)%P%R(K), Q_OUTCLA(NSIC), INFLOWWIDTH, LT'
          DO NSIC = 1,NSICLA
            QBOR%ADR(NSIC)%P%R(K)= (Q_OUTCLA(NSIC) / INFLOWWIDTH)
            PRINT *, QBOR%ADR(NSIC)%P%R(K), Q_OUTCLA(NSIC), 
     &               INFLOWWIDTH, LT
          ENDDO
        ENDIF
!
!       EXAMPLE 2: IMPOSED BED EVOLUTON
!
!       LIEBOR%I(K)=KENT
!       (LIQBOR%I(K)=KSORT IS DONE IN SISYPHE.F)
!       IF(LIEBOR%I(K).EQ.KENT) THEN
!         EBOR%ADR(1)%P%R(K)=1.D-4
!         EBOR%ADR(2)%P%R(K)=1.D-4.....
!       ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!     LICBOR : BOUNDARY CONDITION FOR SEDIMENT CONCENTRATION
!-----------------------------------------------------------------------
!
      IF(SUSP) THEN
!
        DO K=1,NPTFR
!
!         SO FAR LICBOR=LIEBOR (WITH KADH CHANGED INTO KLOG, SEE ABOVE,
!                               BUT CAN BE CHANGED)
!
          LICBOR%I(K) = LIEBOR%I(K)
!
!         ENTRANCE : IMPOSED CONCENTRATION
!         -------------------------------
!
!         NOTE JMH: KSORT MUST BE TREATED ALSO BECAUSE SUBROUTINE DIFFIN
!                   MAY CHANGE A KSORT INTO KENT, DEPENDING OF FLOW
!
          IFRLIQ=NUMLIQ%I(K)
          IF(LIEBOR%I(K).EQ.KENT.OR.LIEBOR%I(K).EQ.KSORT) THEN
            DO I=1,NSICLA
              IRANK=I+(IFRLIQ-1)*NSICLA
              CBOR%ADR(I)%P%R(K) = CBOR_CLASSE(IRANK)
            ENDDO
          ENDIF
!
! CV 12/06 READING BOUNDARY CONDITION FILE
!
          IF(LICBOR%I(K).EQ.KENT.AND.
     &               SIS_FILES(SISLIQ)%NAME(1:1).NE.' ') THEN
!
            IF(IFRLIQ.GT.0) THEN
              DO I=1,NSICLA
                CBOR%ADR(I)%P%R(K) = CGL(IFRLIQ,AT)/XMVS
              ENDDO
            ENDIF
          ENDIF
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE
!                    ********************************
                     SUBROUTINE CVSP_INIT_FROM_LAYERS
!                    ********************************
!
!
!***********************************************************************
! SISYPHE V6P3                                   14/03/2013
!***********************************************************************
!
!brief   INITS A VERTICAL SORTING PROFILE USING HIRANO LAYERS
!
!history UWE MERKEL
!+        2012-04-19
!+
!history  P. A. TASSI (EDF R&D, LNHE)
!+        12/03/2013
!+        V6P3
!+   Cleaning, cosmetic
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!|           |---|
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
!
      IMPLICIT NONE
!
      INTEGER  I, J, K, M, L, MDISC, UBS
      DOUBLE PRECISION DEPTH
!
!----------------------------------------------------------------------- 
!
      ALLOCATE(PRO_D(NPOIN,PRO_MAX_MAX,NSICLA))
      ALLOCATE(PRO_F(NPOIN,PRO_MAX_MAX,NSICLA))
      ALLOCATE(PRO_MAX(NPOIN))
!
!----------------------------------------------------------------------- 
!
      DO J=1,NPOIN
        DEPTH = 0                    ! INIT DEPTH OF THE VSP
        PRO_MAX(J) =  2* NLAYER%I(J) ! 2 SECTION POINTS PER LAYER
        L = PRO_MAX(J)
!
!-----------------------------------------------------------------------     
! WATER / BOTTOM
!-----------------------------------------------------------------------
!
        DO I=1,NSICLA
          PRO_D(J,L,I) = ZF%R(J)
          PRO_F(J,L,I) = AVAIL(J,1,I)
        ENDDO
!
!-----------------------------------------------------------------------     
! SECTIONS 
!-----------------------------------------------------------------------
!
        DO M=1,NLAYER%I(J)-1   !FOR THE UPPER 8 LAYERS
          DEPTH = DEPTH + ES(J,M)
          L = L - 1
          DO I=1,NSICLA
            PRO_D(J,L,I) = ZF%R(J) - DEPTH
            PRO_F(J,L,I) = AVAIL(J,M,I)
          ENDDO
          L = L - 1
          DO I=1,NSICLA
            PRO_D(J,L,I) = ZF%R(J) - DEPTH
            PRO_F(J,L,I) = AVAIL(J,M+1,I)
          ENDDO
        ENDDO
!
!-----------------------------------------------------------------------     
! BOTTOM / RIGID BED
!-----------------------------------------------------------------------
!         
        L = L - 1
        DO I=1,NSICLA
          PRO_D(J,L,I) = ZR%R(J)
          PRO_F(J,L,I) = AVAIL(J,NLAYER%I(J),I)
        ENDDO
!
        CALL CVSP_COMPRESS_DP(J,1.D-5)
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END SUBROUTINE CVSP_INIT_FROM_LAYERS
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
!         SCALAR
          OPEN(CP_FILES(I)%LU,FILE=CP_FILES(I)%TELNAME,
     &         ACTION=CP_FILES(I)%ACTION,FORM='UNFORMATTED')
        ELSE
!         PARALLEL, FILE TYPE: SCAL
          IF(CP_FILES(I)%TYPE(1:4).EQ.'SCAL') THEN
            OPEN(CP_FILES(I)%LU,
     &           FILE=TRIM(CP_FILES(I)%TELNAME),
     &           ACTION=CP_FILES(I)%ACTION,FORM='UNFORMATTED')
!         PARALLEL, OTHER FILE TYPE
          ELSE
            OPEN(CP_FILES(I)%LU,
     &           FILE=TRIM(CP_FILES(I)%TELNAME)
     &           //EXTENS(NCSIZE-1,IPID),
     &           ACTION=CP_FILES(I)%ACTION,FORM='UNFORMATTED')
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
     &       CP_FILES(I)%LU,   ! LU FOR RESULTS FILE
     &       USRMSH,           ! CHARACTERISES MESH
     &       USRMSH_NPLAN,     ! NUMBER OF PLANES /NA/
     &       MARDAT,           ! START DATE
     &       MARTIM,           ! START TIME
     &       I_ORIG,J_ORIG)    ! COORDINATES OF THE ORIGIN.
      ENDDO

      DO I = 4, 4               !UBOUND(CP_FILES)
        CALL WRITE_MESH(CP_FILES(I)%FMT, ! RESULTS FILE FORMAT
     &       CP_FILES(I)%LU,   ! LU FOR RESULTS FILE
     &       USRMSH_2DHYD,     ! CHARACTERISES MESH
     &       USRMSH_2DHYD_NPLAN, ! NUMBER OF PLANES /NA/
     &       MARDAT,           ! START DATE
     &       MARTIM,           ! START TIME
     &       I_ORIG,J_ORIG)    ! COORDINATES OF THE ORIGIN.
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
