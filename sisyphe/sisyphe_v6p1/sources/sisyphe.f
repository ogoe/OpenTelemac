!                    ******************
                     SUBROUTINE SISYPHE
!                    ******************
!
     &(PART,LOOPCOUNT,GRAFCOUNT,LISTCOUNT,TELNIT,
     & U_TEL,V_TEL,H_TEL,HN_TEL,ZF_SIS,UETCAR,CF_TEL,KS_TEL,
     & CONSTFLOW,NSIS_CFD,SISYPHE_CFD,CODE,PERICOU,
     & U3D,V3D,T_TEL,VISC_TEL,DT_TEL,CHARR_TEL,SUSP_TEL,
     & FLBOR_TEL,SOLSYS,DM1,UCONV_TEL,VCONV_TEL,ZCONV)
!
!***********************************************************************
! SISYPHE   V6P1                                   21/07/2011
!***********************************************************************
!
!brief
!
!history  C. LENORMANT; J.-M. HERVOUET; S. HADJI; C. MACHET; C. VILLARET; U. MERKEL; R. KOPMANN
!+        20/03/2011
!+        V6P1
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CF_TEL         |<->| QUADRATIC FRICTION COEFFICIENT FROM TELEMAC
!| CHARR_TEL      |<->| LOGICAL, BED LOAD OR NOT: Sent to TELEMAC-2D
!| CODE           |-->| NAME OF CALLING PROGRAMME (TELEMAC2D OR 3D)
!| CONSTFLOW      |<->| LOGICAL, CONSTANT FLOW DISCHARGE OR NOT (A SUPPRIMER)
!| DM1            |-->| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV)
!| DT_TEL         |-->| TIME STEP FROM TELEMAC
!| FLBOR_TEL      |-->| FLOW FLUXES AT BOUNDARIES
!| GRAFCOUNT      |-->| PERIOD OF GRAPHICAL OUTPUTS
!| HN_TEL         |-->| WATER DEPTH FROM TEL HN
!| H_TEL          |-->| WATER DEPTH FROM TEL H (N+1)
!| KS_TEL         |-->| BED ROUGHNESS SENT TO TELEMAC
!| LISTCOUNT      |-->| PERIODE DE SORTIE LISTING
!| LOOPCOUNT      |-->| NUMERO DE L'ITERATION
!| NSIS_CFD       |---| NUMBER OF ITERATIONS FOR TELEMAC (CONSTANT FLOW DISCHARGE OPTION-TO BE SUPRESSED)
!| PART           |-->| IF -1, NOT COUPLING : ON PASSE TOUTE LA
!|                |   | SUBROUTINE. SINON, INDIQUE LA PARTIE DE LA
!|                |   | SUBROUTINE DANS LAQUELLE ON PASSE
!| PERICOU        |-->| COUPLING PERIOD FOR BEDLOAD
!| SISYPHE_CFD    |<->| LOGICAL, CONSTANT FLOW DISCHARGE OR NOT
!| SOLSYS         |-->|1 OR 2. IF 2 ADVECTION FIELD IS UCONV + DM1*GRAD(ZCONV)
!| SUSP_TEL       |<->|LOGICAL, SUSPENDED LOAD OR NOT: Sent to TELEMAC-2D
!| TELNIT         |-->| NUMBER OF TELEMAC ITERATIONS
!| T_TEL          |-->| CURRENT TIME IN CALLING PROGRAMME
!| U3D,V3D        |-->| 3D VELOCITY SENT BY TELEMAC 3D
!| UCONV_TEL      |-->| ADVECTION VELOCITY FROM TELEMAC2D (X-DIRECTION)
!| UETCAR         |<->| SQUARE OF THE FRICTION VELOCITY (COUPLING WITH TEL 3D)
!| U_TEL          |-->| U VELOCITY FROM TELEMAC
!| VCONV_TEL      |-->| ADVECTION VELOCITY FROM TELEMAC2D (Y-DIRECTION)
!| VISC_TEL       |-->| VELOCITY DIFFUSIVITY (TELEMAC-2D)
!| V_TEL          |-->| V VELOCITY FROM TELEMAC
!| ZCONV          |-->| THE PIECE-WISE CONSTANT PART OF ADVECTION FIELD
!|                |   | IS DM1*GRAD(ZCONV), SEE SOLSYS.
!| ZF_SIS         |<->| BOTTOM ELEVATION SENT TO TELEMAC
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE INTERFACE_SISYPHE, EX_SISYPHE => SISYPHE
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER,           INTENT(IN)    :: PART,LOOPCOUNT,GRAFCOUNT
      INTEGER,           INTENT(IN)    :: LISTCOUNT,TELNIT,PERICOU
      CHARACTER(LEN=24), INTENT(IN)    :: CODE
      TYPE(BIEF_OBJ),    INTENT(IN)    :: U_TEL,V_TEL,H_TEL,HN_TEL
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: ZF_SIS,UETCAR,KS_TEL
      INTEGER,           INTENT(INOUT) :: NSIS_CFD
      LOGICAL,           INTENT(INOUT) :: CONSTFLOW,SISYPHE_CFD
      TYPE(BIEF_OBJ),    INTENT(IN)    :: U3D,V3D,VISC_TEL
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: CF_TEL
      DOUBLE PRECISION,  INTENT(IN)    :: T_TEL
      LOGICAL,           INTENT(INOUT) :: CHARR_TEL,SUSP_TEL
      DOUBLE PRECISION,  INTENT(IN)    :: DT_TEL
      INTEGER,           INTENT(IN)    :: SOLSYS
      TYPE(BIEF_OBJ),    INTENT(IN)    :: FLBOR_TEL,DM1,ZCONV
      TYPE(BIEF_OBJ),    INTENT(IN)    :: UCONV_TEL,VCONV_TEL
!
      INTEGER                        P_IMAX
      DOUBLE PRECISION P_DMAX,P_DMIN
      EXTERNAL         P_DMAX,P_DMIN,P_IMAX
!
      INTEGER, PARAMETER :: NHIST = 0
      INTEGER, PARAMETER :: NSOR = 100
      INTEGER            :: VALNIT,NLISS
      INTEGER            :: I,J,K,MN,MT,ISOUS,NIDT,NIT,IMA,IMI
      INTEGER            :: IMIN,IMAX,NCALCU,NUMEN,NUMENX,NUMDEB
      INTEGER            :: ALIRE(MAXVAR),ALIRV(MAXVAR),ALIRH(MAXVAR)
      INTEGER            :: ALIR0(MAXVAR)
      INTEGER            :: TROUVE(MAXVAR+10)
      DOUBLE PRECISION   :: AT0,DTS,BID,XMA,XMI
      DOUBLE PRECISION   :: XMIN,XMAX
      DOUBLE PRECISION   :: AT,VCUMU,MASS_GF
      DOUBLE PRECISION   :: HIST(1)
      LOGICAL            :: PASS,PASS_SUSP
      LOGICAL            :: RESU,ENTETS,CHGMSK,YAZR
!
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVEZF,SAVEQU,SAVEQV
      DOUBLE PRECISION, POINTER, DIMENSION(:) :: SAVEZ
!
      ! SAVES LOCAL VARIABLES
      ! --------------------------------
      SAVE VCUMU               ! FOR THE BALANCE
      SAVE MASS_GF             ! FOR GRAIN-FEEDING
      SAVE PASS, PASS_SUSP     ! IDENTIFIES 1ST TIMESTEP
      SAVE NIDT, NCALCU, NUMEN, NIT, VALNIT !
      SAVE AT0                 ! TIME
!     NUMEN0 : 1ST RECORD TO READ
      INTEGER :: NUMEN0
!
      ! VARIABLES TO READ IF COMPUTATION IS CONTINUED
      ! --------------------------------
      ! 0 : DISCARD
      ! 1 : READ  (SEE SUBROUTINE NOMVAR)
!
!   HYDRO + EVOLUTION
      DATA ALIRE /1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,400*0/
!   WAVES ONLY
      DATA ALIRH /0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,
     &            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,400*0/
!   NOTHING TO READ
      DATA ALIR0 /0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,400*0/
!
!    FOR VALIDATION, EACH VARIABLE IN THE FILE IS COMPARED
!
      DATA ALIRV /500*1/
!
!======================================================================!
!======================================================================!
!                               PROGRAM                                !
!======================================================================!
!======================================================================!
!
!------------------------------------------------------------------
!     PART 1 : INITIALISATION
!------------------------------------------------------------------
!
      IF(PART==0.OR.PART==-1) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'INITIALIZATION'
!
        WRITE(LU,*) 'PART 0 : INITIALISING SISYPHE'
!
!       INITIALISES THE CONSTANTS
!
        CALL INIT_CONSTANT(KARIM_HOLLY_YANG,KARMAN,PI)
!
!      READS THE WAVE DATA IN THE HYDRODYNAMIC FILE
!
        IF(HOULE.AND.SIS_FILES(SISCOU)%NAME(1:1).EQ.' ') THEN
          ALIRE(12)=1
          ALIRE(13)=1
          ALIRE(14)=1
        ENDIF
!
!       READS THE SEDIMENTOLOGICAL DATA IN THE CONTINUATION FILE
!
        IF(DEBU) THEN
          ALIRE(15)=1
          ALIRE(16)=1
          ALIRE(17)=1
          ALIRE(18)=1
!CV 2011 relecture des D50, KS et MU
          ALIRE(19) = 1
          ALIRE(20) = 1
          ALIRE(21) = 1
!         READS AVAI FROM THE PREVIOUS COMPUTATION FILE
          DO I=1,NSICLA*NOMBLAY
!CV 2010
            ALIRE(21+I)=1
          ENDDO
!         READS CS (CONCENTRATION) FROM THE PREVIOUS COMPUTATION FILE
          IF(SUSP) THEN
            DO I=1,NSICLA
!V 2010
             ALIRE(21+(NOMBLAY+1)*NSICLA+I)=1
            ENDDO
          ENDIF
!         READS THE LAYER THICKNESSES
          DO I=1,NOMBLAY
!V 2019
            ALIRE(27+(NOMBLAY+4)*NSICLA+I)=1
          ENDDO
        ENDIF
!
! --------  INITIALISES (SETS TO 0) THE ARRAYS
!
        CALL INIT_ZERO
!
! --------  END OF INITIALISATION
!
!       DISCRETISATION : LINEAR FOR THE TIME BEING
!
!       IELMT HARD-CODED IN LECDON
!
        IELMX  = MAX(IELMT,IELMH_SIS,IELMU_SIS)
        NELMAX = NELEM
!
!=======================================================================
!
! : 1          READS, PREPARES AND CONTROLS THE DATA
!
!=======================================================================
!
        RESU = .TRUE.
!
!       READS THE BOUNDARY CONDITIONS AND INDICES FOR THE BOUNDARY NODES
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'LECLIS'
        CALL LECLIS(LIEBOR%I,EBOR,
     &              MESH%NPTFR,MESH%NBOR%I,3,
     &              SIS_FILES(SISCLI)%LU,KENT,
     &              MESH%ISEG%I,MESH%XSEG%R,MESH%YSEG%R,MESH%NACHB%I,
     &              NUMLIQ%I,NSICLA,AFBOR%R,BFBOR%R,BOUNDARY_COLOUR%I,
     &              MESH)
        IF(DEBUG.GT.0) WRITE(LU,*) 'END_LECLIS'
!
!-----------------------------------------------------------------------
!
!       COMPLEMENTS THE DATA STRUCTURE FOR BIEF
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'INBIEF'
        CALL INBIEF(IT1%I,KLOG,IT2,IT3,IT4,LVMAC,IELMX,
     &                 0.D0,SPHERI,MESH,T1,T2,OPTASS,PRODUC,EQUA)
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'END_INBIEF'
!
!-----------------------------------------------------------------------
!
!       LOCATES THE BOUNDARIES
!
        IF(NCSIZE.GT.1) THEN
          NFRLIQ=0
          DO I=1,NPTFR
            NFRLIQ=MAX(NFRLIQ,NUMLIQ%I(I))
          ENDDO
          NFRLIQ=P_IMAX(NFRLIQ)
          WRITE(LU,*) ' '
          IF(LNG.EQ.1) WRITE(LU,*) 'FRONTIERES LIQUIDES :',NFRLIQ
          IF(LNG.EQ.2) WRITE(LU,*) 'LIQUID BOUNDARIES:',NFRLIQ
        ELSE
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE FRONT2'
          CALL FRONT2(NFRLIQ,NFRSOL,DEBLIQ,FINLIQ,DEBSOL,FINSOL,
     &                LIEBOR%I,LIEBOR%I,
     &                MESH%X%R,MESH%Y%R,MESH%NBOR%I,MESH%KP1BOR%I,
     &                IT1%I,NPOIN,NPTFR,KLOG,.TRUE.,NUMLIQ%I,MAXFRO)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE FRONT2'
        ENDIF
!
!-----------------------------------------------------------------------
!       LOOKS FOR BOTTOM AND BOTTOM FRICTION IN THE GEOMETRY FILE :
!-----------------------------------------------------------------------
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'FONSTR'
        CALL FONSTR(T1,ZF,T2,CHESTR,SIS_FILES(SISGEO)%LU,
     &              SIS_FILES(SISFON)%LU,SIS_FILES(SISFON)%NAME,
     &              MESH,SFON,.TRUE.)
        IF(DEBUG.GT.0) WRITE(LU,*) 'END_FONSTR'
!
!-----------------------------------------------------------------------
!
!       PREPARES THE RESULTS FILE (OPTIONAL)
!
!       STANDARD SELAFIN FORMAT
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'ECRGEO'
        ! CREATES DATA FILE USING A GIVEN FILE FORMAT : FORMAT_RES
        ! THE DATA ARE CREATED IN THE FILE: SISRES, AND ARE
        ! CHARACTERISED BY A TITLE AND NAME OF OUTPUT VARIABLES
        ! CONTAINED IN THE FILE.
        CALL CREATE_DATASET(SIS_FILES(SISRES)%FMT, ! RESULTS FILE FORMAT
     &                      SIS_FILES(SISRES)%LU,  ! LU FOR RESULTS FILE
     &                      TITCA,      ! TITLE
     &                      MAXVAR,     ! MAX NUMBER OF OUTPUT VARIABLES
     &                      TEXTE,      ! NAMES OF OUTPUT VARIABLES
     &                      SORLEO)     ! PRINT TO FILE OR NOT
        ! WRITES THE MESH IN THE OUTPUT FILE :
        ! IN PARALLEL, REQUIRES NCSIZE AND NPTIR.
        ! THE REST OF THE INFORMATION IS IN MESH.
        ! ALSO WRITES : START DATE/TIME AND COORDINATES OF THE
        ! ORIGIN.
        CALL WRITE_MESH(SIS_FILES(SISRES)%FMT, ! RESULTS FILE FORMAT
     &                  SIS_FILES(SISRES)%LU,  ! LU FOR RESULTS FILE
     &                  MESH,          ! CHARACTERISES MESH
     &                  1,             ! NUMBER OF PLANES /NA/
     &                  MARDAT,        ! START DATE
     &                  MARTIM,        ! START TIME
     &                  I_ORIG,J_ORIG) ! COORDINATES OF THE ORIGIN.
        IF(DEBUG.GT.0) WRITE(LU,*) 'END_ECRGEO'
!
!   --- FILLS IN MASKEL BY DEFAULT
!
        IF(MSK) CALL OS ('X=C     ', X=MASKEL, C=1.D0)
!
!       BUILDS THE MASK
!
        DO K = 1, MESH%NPTFR
          IF(LIEBOR%I(K).NE.2.AND.LIEBOR%I(MESH%KP1BOR%I(K)).NE.2) THEN
            MASK%R(K) = 1.D0
          ELSE
            MASK%R(K) = 0.D0
          ENDIF
          LIQBOR%I(K) = KSORT
        ENDDO
!
!=======================================================================
!
! : 2                  INITIALISES
!
!=======================================================================
!
        PASS      = .TRUE.
!
        PASS_SUSP = .TRUE.
        VCUMU     = 0.D0
        MASS_GF   = 0.D0
!
!
!----   DETERMINES THE NUMBER OF EVENTS (1ST LOOP)       : NCALCU
!                      NUMBER OF TIMESTEPS (2ND LOOP)    : NIDT
!                      TOTAL NUMBER OF TIMESTEPS         : NIT
!                      INITIAL TIME                      : AT0
!                      TIMESTEP                          : DT
!
!
        IF(PART.EQ.0) THEN
          AT0=T_TEL
          DT = DT_TEL
          NCALCU = 1
          NIDT   = 1
          NIT=TELNIT
        ELSE
          AT0=MAX(TPREC,0.D0)
          DT=DELT
          IF(PERMA) THEN
            NCALCU=1
            NIDT=NPAS
            NIT=NIDT
!V
            NSOUS=1
          ELSE
            NCALCU = NMAREE
! COMPUTES DT AFTER READING THE HYDRO FILE
!           NIDT =  NINT ( PMAREE / DT + 0.1D0 )
!           NIT=NIDT*NCALCU
! ELSE
            NIDT=NPAS
            NIT=NIDT*NCALCU
          ENDIF
        ENDIF
!
! UNSTEADY MODE : DT IS COMPUTED FROM THE HYDRO FILE
!                 NUMEN: TOTAL NUMBER OF RECORDS
        IF(SIS_FILES(SISHYD)%NAME(1:1).NE.' ')  THEN
            IF(DEBUG.GT.0) WRITE(LU,*) 'BIEF_SUITE'
            WRITE(LU,*) 'APPEL DE BIEF_SUITE'
!           JUST TO GET NUMEN AND DT (SEE ALIR0)
            CALL BIEF_SUITE(VARSOR,VARCL,NUMEN,SIS_FILES(SISHYD)%LU,
     &                    SIS_FILES(SISHYD)%FMT,HIST,0,NPOIN,AT,
     &                    TEXTPR,VARCLA,
     &                    0,TROUVE,ALIR0,.TRUE.,.TRUE.,MAXVAR,DT=DT)
            IF(DEBUG.GT.0) WRITE(LU,*) 'SORTIE DE BIEF_SUITE'
            WRITE(LU,*) 'LECTURE FICHIER HYDRODYNAMIQUE:'
          IF(PERMA) DT = DELT
          IF(.NOT.PERMA) THEN
             NIDT =  NINT ( PMAREE / DT + 0.1D0 )
             IF(ABS(NIDT*DT-PMAREE) > 1.D-3) THEN
               IF (LNG == 1) WRITE(LU,101) NIDT*DT
               IF (LNG == 2) WRITE(LU,102) NIDT*DT
             ENDIF
             NIT  = NCALCU * NIDT
          ENDIF
        ENDIF
!
!       VALIDATES AGAINST THE LAST TIMESTEP
!
        VALNIT = NIT
!
101     FORMAT(/,'ATTENTION : LA PERIODE DE CALCUL NE CORRESPOND PAS A',/,
     &       'UN MULTIPLE DE LA PERIODE DE SORTIE HYDRODYNAMIQUE.',/,
     &       'LE CALCUL S''EFFECTUERA DONC SUR ',G16.7,'SECONDES')
102     FORMAT(/,'CAUTION : THE PERIOD OF COMPUTATION IS NOT A MULTIPLE',
     &         /,'OF THE HYDRODYNAMIC FILE PRINTOUT PERIOD.',/,
     &       'THE LENGTH OF COMPUTATION WILL THEREFORE BE',G16.7,/,
     &       'SECONDS')
!
!  SISYPHE ONLY
!  -----------------------------------------------------------------------
!  ---- READS
!       THE PREVIOUS HYDRODYNAMIC FILE
!
!
! NUMEN : NUMBER OF RECORDS IN THE HYDRODYNAMIC FILE
! DT    : TIMESTEP OF THE HYDRODYNAMIC RECORDS
! NUMEN0: 1ST RECORD TO READ FROM HYDRODYNAMIC FILE
! TPREC : START TIME
!
!
! V5P9      NUMEN0 = INT( (TPREC - ATDEB)/DT + 1.1D0 )
!
        IF(PART.EQ.-1) THEN
          IF(.NOT.PERMA) THEN
            IF(TPREC.GE.0.D0) THEN
                NUMEN0 = INT( TPREC /DT + 1.1D0 )
            ELSE
                NUMEN0 = NUMEN-INT(PMAREE/DT+1.1D0)
            ENDIF
          ELSE
            IF(TPREC.GE.0.D0) THEN
               NUMEN0 = INT( TPREC /DT + 1.1D0 )
            ELSE
               NUMEN0 = NUMEN
            ENDIF
          ENDIF
!
          IF(SIS_FILES(SISHYD)%NAME(1:1).NE.' ')  THEN
            IF(DEBUG.GT.0) WRITE(LU,*) 'BIEF_SUITE'
            CALL BIEF_SUITE(VARSOR,VARCL,NUMEN0,SIS_FILES(SISHYD)%LU,
     &                    SIS_FILES(SISHYD)%FMT,HIST,0,NPOIN,AT,
     &                    TEXTPR,VARCLA,
     &                    0,TROUVE,ALIRE,.TRUE.,PERMA,MAXVAR)
!
!           TRACES IF WAVE DATA HAVE BEEN FOUND
!
            IF(HOULE) THEN
              IF(TROUVE(12).EQ.1) HW%TYPR='Q'
              IF(TROUVE(13).EQ.1) TW%TYPR='Q'
              IF(TROUVE(14).EQ.1) THETAW%TYPR='Q'
            ENDIF
            IF(DEBUG.GT.0) WRITE(LU,*) 'END_BIEF_SUITE'
            IF(DEBUG.GT.0) WRITE(LU,*) 'RESCUE_SISYPHE'
            CALL RESCUE_SISYPHE(QU%R,QV%R,Q%R,U2D%R,V2D%R,HN%R,Z%R,
     &                        ZF%R,HW%R,TW%R,THETAW%R,NPOIN,
     &                        TROUVE,ALIRE,PASS,ICF,.TRUE.,MAXVAR)
            IF(DEBUG.GT.0) WRITE(LU,*) 'END_RESCUE_SISYPHE'
          ENDIF
!
        ENDIF
!
!---- RESUMES SISYPHE COMPUTATION
!
        YAZR=.FALSE.
        IF(SIS_FILES(SISPRE)%NAME(1:1).NE.' ')  THEN
!
!         READS THE HYDRO AND SEDIMENTOLOGICAL VARIABLES
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'BIEF_SUITE'
          CALL BIEF_SUITE(VARSOR,VARCL,NUMENX,SIS_FILES(SISPRE)%LU,
     &                    SIS_FILES(SISPRE)%FMT,
     &                    HIST,0,NPOIN,AT0,TEXTPR,VARCLA,0,
     &                    TROUVE,ALIRE,.TRUE.,.TRUE.,MAXVAR)
          IF(TROUVE(9).EQ.1) YAZR=.TRUE.
          IF(DEBUG.GT.0) WRITE(LU,*) 'END_BIEF_SUITE'
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'RESCUE_SISYPHE'
          CALL RESCUE_SISYPHE(QU%R,QV%R,Q%R,U2D%R,V2D%R,HN%R,Z%R,ZF%R,
     &                        HW%R,TW%R,THETAW%R,NPOIN,TROUVE,ALIRE,
     &                        PASS,ICF,.TRUE.,MAXVAR)
          IF(DEBUG.GT.0) WRITE(LU,*) 'SORTIE DE BIEF_SUITE'
!
!         CHANGES THE UNITS OF CONCENTRATIONS
!
          IF(SUSP.AND.UNIT) THEN
            DO I=1,NSICLA
             IF(TROUVE(21+(NOMBLAY+1)*NSICLA+I).EQ.1) THEN
               CALL OS('X=CX    ',X=CS%ADR(I)%P,C=1.D0/XMVS)
             ENDIF
            ENDDO
          ENDIF
!
        ENDIF
!
!----   READS THE LAST RECORD : WAVE FILE
!
!       NOTE : SIS_FILES(SISCOU)%NAME SET TO ' ' IF HOULE=NO
!
        IF(SIS_FILES(SISCOU)%NAME(1:1).NE.' ')  THEN
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'SUITE_HOULE'
          WRITE(LU,*) ' LECTURE HOULE :',SIS_FILES(SISCOU)%NAME
          CALL BIEF_SUITE(VARSOR,VARCL,NUMENX,SIS_FILES(SISCOU)%LU,
     &                    SIS_FILES(SISCOU)%FMT,HIST,0,NPOIN,AT,
     &                    TEXTPR,VARCLA,0,
     &                    TROUVE,ALIRH,.TRUE.,.TRUE.,MAXVAR)
          IF(DEBUG.GT.0) WRITE(LU,*) 'END_SUITE_HOULE'
!         TRACES IF WAVE DATA HAVE BEEN FOUND
          IF(TROUVE(12).EQ.1) HW%TYPR='Q'
          IF(TROUVE(13).EQ.1) TW%TYPR='Q'
          IF(TROUVE(14).EQ.1) THETAW%TYPR='Q'
!
        ENDIF
!
        IF(CODE(1:7) == 'TELEMAC'.AND.PART==0) THEN
!V
          AT0=T_TEL
!V
          WRITE(LU,*) 'INITIALISATION EN CAS DE COUPLAGE : PART=',PART
!         INFORMATION ON SUSPENSION SENT BACK
          CHARR_TEL = CHARR
          SUSP_TEL = SUSP
!
!         OV INSTEAD OF OS IN ORDER TO AVOID PROBLEMS WITH QUASI-BUBBLE ELEMENTS
!         OPERATES ONLY ON THE (1:NPOIN) RANGE OF THE TELEMAC FIELDS
!         IT IS A HIDDEN DISCRETISATION CHANGE
!
          CALL OV( 'X=Y     ', U2D%R, U_TEL%R, U_TEL%R, 0.D0, NPOIN)
          CALL OV( 'X=Y     ', V2D%R, V_TEL%R, V_TEL%R, 0.D0, NPOIN)
          CALL OV( 'X=Y     ', HN%R , H_TEL%R, H_TEL%R, 0.D0, NPOIN)
          CALL OS( 'X=Y     ', X=ZF,Y=ZF_SIS)
!
!         CLIPS NEGATIVE DEPTHS
!
          IF(OPTBAN.GT.0) THEN
            DO I = 1,NPOIN
             IF(HN%R(I).LT.HMIN) THEN
               U2D%R(I)=0.D0
               V2D%R(I)=0.D0
               HN%R(I) = HMIN
             ENDIF
            ENDDO
          ENDIF
        ENDIF
!
!  ---- END COUPLING  -------------
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'CONDIM_SISYPHE'
        IF(.NOT.DEBU) THEN
        CALL CONDIM_SISYPHE
     &        (U2D%R,V2D%R,QU%R,QV%R,HN%R,ZF%R,Z%R,ESOMT%R,THETAW%R,
     &         Q%R,HW%R,TW%R,MESH%X%R,MESH%Y%R,NPOIN,AT0,PMAREE)
        ENDIF
        IF(DEBUG.GT.0) WRITE(LU,*) 'END_CONDIM_SISYPHE'
!
!       AT THIS LEVEL U2D,V2D,H AND ZF MUST HAVE BEEN DEFINED
!       EITHER BY BIEF_SUITE, CONDIM_SISYPHE OR CALLING PROGRAM
!
!       NOW COMPUTES FUNCTIONS OF U2D,V2D,HN AND ZF
!
!       FREE SURFACE
        CALL OS('X=Y+Z   ', X=Z, Y=ZF, Z=HN)
!
        IF(CODE(1:7).NE.'TELEMAC') THEN
!         PRODUCT H*
          CALL OS('X=YZ    ', X=QU, Y=U2D, Z=HN)
!         PRODUCT H*V
          CALL OS('X=YZ    ', X=QV, Y=V2D, Z=HN)
!         DISCHARGE
          CALL OS('X=N(Y,Z)', X=Q, Y=QU, Z=QV)
        ENDIF
!
!       CHECKS THE WAVE DATA
!
        IF(HOULE) THEN
          IF(HW%TYPR    .NE.'Q'.OR.
     &       TW%TYPR    .NE.'Q'.OR.
     &       THETAW%TYPR.NE.'Q') THEN
            WRITE(LU,*) ' '
            WRITE(LU,*) ' '
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'DONNEES DE HOULE MANQUANTES'
              IF(HW%TYPR.NE.'Q') WRITE(LU,*) 'HAUTEUR HM0'
              IF(TW%TYPR.NE.'Q') WRITE(LU,*) 'PERIODE PIC TPR5'
              IF(THETAW%TYPR.NE.'Q') WRITE(LU,*) 'DIRECTION MOY'
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'MISSING WAVE DATA'
              IF(HW%TYPR.NE.'Q') WRITE(LU,*) 'WAVE HEIGHT HM0'
              IF(TW%TYPR.NE.'Q') WRITE(LU,*) 'PEAK PERIOD TPR5'
              IF(THETAW%TYPR.NE.'Q') WRITE(LU,*) 'MEAN DIRECTION'
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
!
! END OF HYDRODYNAMIC INITIALISATION
!
!
!        COMPUTES AREAS (WITHOUT MASKING)
!
         CALL VECTOR(VOLU2D,'=','MASBAS          ',
     &               IELMH_SIS,1.D0,
     &               T1,T1,T1,T1,T1,T1,MESH,.FALSE.,MASKEL)
!        V2DPAR : LIKE VOLU2D BUT IN PARALLEL VALUES COMPLETED AT
!                 INTERFACES BETWEEN SUBDOMAINS
         CALL OS('X=Y     ',X=V2DPAR,Y=VOLU2D)
         IF(NCSIZE.GT.1) CALL PARCOM(V2DPAR,2,MESH)
!        INVERSE OF VOLUMES (DONE WITHOUT MASKING)
         CALL OS('X=1/Y   ',X=UNSV2D,Y=V2DPAR,
     &           IOPT=2,INFINI=0.D0,ZERO=1.D-12)
!
! START OF MODIFICATIONS FOR MIXED SEDIMENTS
!
!        SETTING THE NON-ERODABLE BED (IT CAN BE SET BEFORE
!                                      IF COMPUTATION CONTINUED, I.E. DEBU)
!
         IF(.NOT.DEBU.OR..NOT.YAZR) THEN
           CALL NOEROD(HN%R,ZF%R,ZR%R,Z%R,MESH%X%R,
     &                 MESH%Y%R,NPOIN,CHOIX,NLISS)
         ENDIF
!
!        INITIALISATION FOR SEDIMENT
!
         IF(DEBUG.GT.0) WRITE(LU,*) 'INIT_SEDIMENT'
         CALL INIT_SEDIMENT(NSICLA,ELAY,ZF,ZR,NPOIN,
     &                    AVAIL,FRACSED_GF,AVA0,LGRAFED,CALWC,
     &                    XMVS,XMVE,GRAV,VCE,XWC,FDM,CALAC,AC,
     &                    SEDCO, ES, NCOUCH_TASS,CONC_VASE,
     &         MS_SABLE, MS_VASE,ACLADM, UNLADM,TOCE_SABLE)
         IF(DEBUG.GT.0) WRITE(LU,*) 'END INIT_SEDIMENT'
!
!
! END OF MODIFICATIONS CV
!
!
! MEAN VELOCITY
!======================================================================
        CALL OS('X=N(Y,Z)',X=UNORM,Y=U2D,Z=V2D)
! =====================================================================
!  WAVE ORBITAL VELOCITY
! =====================================================================
        IF(HOULE) THEN
          CALL CALCUW(UW%R,HN%R,HW%R,TW%R,GRAV,NPOIN)
        ENDIF
! ======================================================================
        IF(DEBUG.GT.0) WRITE(LU,*) 'TOB_SISYPHE'
        CALL TOB_SISYPHE(TOB,TOBW,MU,KS,KSP,KSR,CF,FW,
     &                   CHESTR,UETCAR,CF_TEL,KS_TEL,CODE ,
     &                   KFROT,ICR,KSPRATIO,HOULE,
     &                   GRAV,XMVE,XMVS,VCE,KARMAN,ZERO,
     &                   HMIN,HN,ACLADM,UNORM,UW,TW,NPOIN,KSPRED,IKS)
        IF(DEBUG.GT.0) WRITE(LU,*) 'END TOB_SISYPHE'
!
!       INITIALISATION FOR TRANSPORT
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'INIT_TRANSPORT'
        CALL INIT_TRANSPORT(TROUVE,DEBU,HIDING,NSICLA,NPOIN,
     &     T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12,T14,
     &     CHARR,QS_C,QSXC,QSYC,CALFA,SALFA,COEFPN,SLOPEFF,
     &     SUSP,QS_S,QS,QSCL,QSCL_C,QSCL_S,QSCLXS,QSCLYS,
     &     UNORM,U2D,V2D,HN,CF,MU,TOB,TOBW,UW,TW,THETAW,FW,HOULE,
     &     AVAIL,ACLADM,UNLADM,KSP,KSR,KS,
     &     ICF,HIDFAC,XMVS,XMVE,GRAV,VCE,HMIN,KARMAN,
     &     ZERO,PI,AC,IMP_INFLOW_C,ZREF,ICQ,CSTAEQ,
     &     CMAX,CS,CS0,UCONV,VCONV,CORR_CONV,SECCURRENT,BIJK,
     &     IELMT,MESH,FDM,XWC,FD90,SEDCO,VITCE,PARTHENIADES,VITCD,
     &     U3D,V3D,CODE)
        IF(DEBUG.GT.0) WRITE(LU,*) 'END INIT_TRANSPORT'
!
! ---------- DEBUT IMPRESSIION INITIALISATION =================
!
        CALL ENTETE_SISYPHE(1,AT0,0)
!       PREPARES RESULTS
!
! CONCENTRATION OUTPUT IN G/L
!
        IF(UNIT) CALL OS('X=CX    ',X=CS,C=XMVS)
        CALL PREDES(0,AT0)
!
!       PRINTS OUT THE RESULTS
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'BIEF_DESIMP'
        CALL BIEF_DESIMP(SIS_FILES(SISRES)%FMT,VARSOR,
     &                   HIST,0,NPOIN,SIS_FILES(SISRES)%LU,'STD',
     &                   AT0,0,LISPR,LEOPR,SORLEO,SORIMP,MAXVAR,
     &                   TEXTE,PTINIG,PTINIL)
        IF(DEBUG.GT.0) WRITE(LU,*) 'END BIEF_DESIMP'
!
        IF(UNIT) CALL OS('X=CX    ',X=CS,C=1.D0/XMVS)
!
!===============FIN IMPRESSION CONDITIONS INITIALES =================
!
!      COUPLING
!
       IF(DREDGESIM) THEN
         CALL DREDGESIM_INTERFACE(1)
         IF(LNG.EQ.1) WRITE(LU,*) 'SISYPHE COUPLE AVEC DREDGESIM'
         IF(LNG.EQ.2) WRITE(LU,*) 'SISYPHE COUPLED WITH DREDGESIM'
       ENDIF
!
       IF(CODE(1:7).NE.'SISYPHE') THEN
         IF(LNG.EQ.1) WRITE(LU,*) 'SISYPHE COUPLE AVEC : ',CODE
         IF(LNG.EQ.2) WRITE(LU,*) 'SISYPHE COUPLED WITH: ',CODE
       ENDIF
!
!      COUPLING WITH TELEMAC-2D OR 3D
!
       IF(CODE(1:7).EQ.'TELEMAC') NCALCU = 1
!
!=======================================================================
!
!     INITIAL CONDITION FOR CONSTANT FLOW DISCHARGE
!
      IF(LCONDIS) THEN
        SISYPHE_CFD = LCONDIS
        NSIS_CFD    = NCONDIS
        CONSTFLOW   = .FALSE.
      ELSE
        SISYPHE_CFD = .FALSE.
        NSIS_CFD    = 1
        CONSTFLOW   = .FALSE.
      ENDIF
!
!=======================================================================
!
!     END OF INITIALISATIONS
         IF(DEBUG.GT.0) WRITE(LU,*) 'END_INITIALIZATION'
      ENDIF ! IF (PART==0 OR PART = -1)
!
!=======================================================================
!
      IF(PART==1.OR.PART==-1) THEN
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'TIME_LOOP'
!
!=======================================================================
!
! : 3                    /* LOOP ON TIME */
!
!=======================================================================
!
!----   STOPS THE COMPUTATION WHEN THE REQUIRED NUMBER OF ITERATIONS IS 0
!
        IF(NIT == 0) THEN
          IF (LNG == 1) WRITE(LU,200)
          IF (LNG == 2) WRITE(LU,201)
200       FORMAT(' ARRET DANS SISYPHE, NOMBRE D''ITERATIONS',/,
     &         ' DEMANDE NUL')
201       FORMAT(' STOP IN SISYPHE, NUMBER OF ITERATIONS EQ.0')
          CALL PLANTE(1)
          STOP
        ENDIF
!
!---------------------------------------------------------------------
!       STARTS THE COMPUTATIONS
!---------------------------------------------------------------------
!       LOOP ON THE NUMBER OF EVENTS
!       (IN STEADY STATE: LOOP ON THE TIMESTEPS)
!---------------------------------------------------------------------
!
        IF(CODE(1:7) == 'TELEMAC') THEN
!         VALNIT WILL BE USED FOR CALLING VALIDA
          VALNIT = (TELNIT/PERICOU)*PERICOU-PERICOU+1
!         MODIFICATION JMH + CV: TO AVOID 2 SUCCESSIVE CALLS TO VALIDA
!         WHEN BEDLOAD AND SUSPENSION
          IF(GRAFCOUNT.GT.TELNIT) VALNIT=NIT+1
!         CHARR, SUSP AND TIME STEP MONITORED BY CALLING PROGRAM
          CHARR = CHARR_TEL
          SUSP= SUSP_TEL
          AT0=T_TEL
!UM 28.02.2011 MORPHOLOGIGCAL FACTOR
          DT=MOFAC*DT
! 
        ENDIF
!
        DO 710 MN = 1, NCALCU
!
          IF(.NOT.PERMA.AND.CODE(1:7).NE.'TELEMAC') THEN
!           DETERMINES THE FIRST RECORD TO BE READ :
!           NUMDEB IS THE FIRST RECORD TO BE READ FROM THE HYDRO
!           FILE
            NUMDEB=NUMEN0
            IF(NUMDEB+NIDT > NUMEN) THEN
              IF (LNG == 1) WRITE(LU,202)
              IF (LNG == 2) WRITE(LU,203)
202           FORMAT(1X,'FICHIER HYDRODYNAMIQUE PAS ASSEZ LONG')
203           FORMAT(1X,'THE HYDRODYNAMIC FILE IS NOT LONG ENOUGH')
              CALL PLANTE(1)
            ENDIF
          ENDIF
!
!         LOOP ON THE RECORDS (IF PERMA NIDT=1)
!         ------------------------------

          DO 700 MT = 1, NIDT
!
!  ----   DETERMINES THE TIMESTEP NUMBER :
!
            LT = (MN-1)*NIDT +  MT
!
            IF(CODE(1:7) == 'TELEMAC') THEN
              DT=DT_TEL
              LT    = LOOPCOUNT
              LEOPR = GRAFCOUNT
              LISPR = LISTCOUNT
              NSOUS=1
            ENDIF
!
!  ----     PRINTOUTS TO LISTING :
!
            ENTETS = .FALSE.
            IF(LISPR*((LT-1+PERICOU)/LISPR) == (LT-1+PERICOU)) THEN
              ENTET = .TRUE.
            ELSE
              ENTET = .FALSE.
            ENDIF
!
!----      READS AND UPDATES H AND ZF
!----      IF 1ST PASS OR UNSTEADY AND NO COUPLING
!
           DTS = DT / NSOUS
!
           ISOUS = 0
!
           IF(.NOT.PERMA.OR.PASS) THEN
!
!            BEWARE : THE VALUE FOR ESOMT IS NOT READ FROM THE FILE SISHYD
!            NOTE : NAME FOR SISHYD SET TO ' ' IF COUPLING
!
             IF(SIS_FILES(SISHYD)%NAME(1:1).NE.' ')  THEN
!
!              WORK ON ZF,QU,QV,Z WILL BE IN FACT DONE ON:
!              T4,DEL_QU,DEL_QV AND DEL_Z
!              BY PLAYING WITH POINTERS
               SAVEZF=>ZF%R
               SAVEQU=>QU%R
               SAVEQV=>QV%R
               SAVEZ =>Z%R
               ZF%R  =>T4%R
               QU%R  =>DEL_QU%R
               QV%R  =>DEL_QV%R
               Z%R   =>DEL_Z%R
!
               NUMDEB=NUMDEB+1
!
               IF(ENTET) WRITE(LU,*) 'DEFINITION INITIALE DES VITESSES'
!
               CALL BIEF_SUITE(VARSOR,VARCL,NUMDEB,
     &            SIS_FILES(SISHYD)%LU,SIS_FILES(SISHYD)%FMT,
     &            HIST,0,NPOIN,BID,TEXTPR,VARCLA,0,
     &            TROUVE,ALIRE,ENTET,PERMA,MAXVAR)
!
               IF(DEBUG.GT.0) WRITE(LU,*) 'RESCUE_SISYPHE_NOTPERMA'
               CALL RESCUE_SISYPHE_NOTPERMA
     &              (QU%R,QV%R,Q%R,U2D%R,V2D%R,HN%R,Z%R,T4%R,
     &               HW%R,TW%R,THETAW%R,NPOIN,TROUVE,ALIRE,ICF,ENTET,
     &               MAXVAR)
               IF(DEBUG.GT.0) WRITE(LU,*) 'END_RESCUE_SISYPHE_NOTPERMA'
!
!              BACK TO ORIGINAL ADDRESSES
               ZF%R=>SAVEZF
               QU%R=>SAVEQU
               QV%R=>SAVEQV
               Z%R=>SAVEZ
!
!              INCREMENT OF QU, QV AND Z PER SUB-TIME-STEP
               DO I = 1,NPOIN
                 DEL_QU%R(I) = (DEL_QU%R(I)-QU%R(I))/NSOUS
                 DEL_QV%R(I) = (DEL_QV%R(I)-QV%R(I))/NSOUS
                 DEL_Z%R(I)  = (DEL_Z%R(I) -Z%R(I)) /NSOUS
               ENDDO
!
!              UPDATES UNSTEADY HYDRO
!              (TO BE MOVED TO RESCUE_SISYPHE_NOTPERMA)
!              -----------------------------------
!              CLIPS NEGATIVE DEPTHS
!              COMPUTES U2D AND V2D
!
               CALL OS('X=Y-Z   ', X=HN, Y=Z, Z=ZF)
!
               IF(OPTBAN.GT.0) THEN
                 DO I = 1,NPOIN
                   IF(HN%R(I).LT.HMIN) THEN
                     U2D%R(I)=0.D0
                     V2D%R(I)=0.D0
                     HN%R(I) = MAX(HN%R(I),HMIN)
                   ELSE
                     U2D%R(I)=QU%R(I)/HN%R(I)
                     V2D%R(I)=QV%R(I)/HN%R(I)
                   ENDIF
                 ENDDO
               ELSE
                 CALL OS('X=Y/Z   ', X=U2D, Y=QU,   Z=HN)
                 CALL OS('X=Y/Z   ', X=V2D, Y=QV,   Z=HN)
               ENDIF
!
               IF(DEBUG.GT.0) WRITE(LU,*) 'CONDIM_SISYPHE'
               CALL CONDIM_SISYPHE
     &        (U2D%R,V2D%R,QU%R,QV%R,HN%R,ZF%R,Z%R,ESOMT%R,THETAW%R,
     &         Q%R,HW%R,TW%R,MESH%X%R,MESH%Y%R,NPOIN,AT0,PMAREE)
               IF(DEBUG.GT.0) WRITE(LU,*) 'END_CONDIM_SISYPHE'
             ENDIF ! (SIS_FILES(SISHYD)%NAME(1:1) /=' ')
           ENDIF ! (NOT.PERMA.OR.PASS)
!
        IF(PASS) THEN
!         IN STEADY STATE LOGICAL FOR READING SET TO FALSE
          IF (PERMA) PASS = .FALSE.
        ELSE
!         COMPUTES THE WATER DEPTH
          CALL OS('X=Y-Z   ', X=HN, Y=Z, Z=ZF)
        ENDIF
!
!       COUPLING
!
        IF(CODE(1:7) == 'TELEMAC') THEN
!
!         OV INSTEAD OF OS IN ORDER TO AVOID PROBLEMS WITH QUASI-BUBBLE ELEMENTS
!         OPERATES ONLY ON THE (1:NPOIN) RANGE OF THE TELEMAC FIELDS
!         IT IS A HIDDEN DISCRETISATION CHANGE
!
          CALL OV( 'X=Y     ',U2D%R, U_TEL%R, U_TEL%R, 0.D0, NPOIN)
          CALL OV( 'X=Y     ',V2D%R, V_TEL%R, V_TEL%R, 0.D0, NPOIN)
          CALL OV( 'X=Y     ', HN%R, H_TEL%R, H_TEL%R, 0.D0, NPOIN)
!         ADDED BY JMH 01/07/2004 (ZF MAY BE MODIFIED BY CALLING PROGRAM)
          CALL OS('X=Y     ', X=ZF, Y=ZF_SIS)
!         CLIPS NEGATIVE DEPTHS
          IF(OPTBAN.GT.0) THEN
            DO I = 1,HN%DIM1
              IF(HN%R(I).LT.HMIN) THEN
               U2D%R(I)=0.D0
               V2D%R(I)=0.D0
               HN%R(I)=HMIN
              ENDIF
            ENDDO
          ENDIF
!         FREE SURFACE
          CALL OS('X=Y+Z   ', X=Z, Y=ZF, Z=HN)
!
        ENDIF
!
!       END OF COUPLING
! =========================================================================
! TREATMENT OF TIDAL FLATS, DEFINITION OF THE MASKS
! =====================================================================!
!
        IF(OPTBAN.EQ.2) THEN
!
! ----    BUILDS MASKING BY ELEMENTS
!
          CHGMSK = .FALSE.
          CALL OS ('X=Y     ', X=MSKTMP, Y=MASKEL)
          CALL OS ('X=C     ', X=MASKEL, C=1.D0)
          IF(CODE(1:7) == 'TELEMAC') THEN
!           MASKS ARE DERIVED FROM THE NON-CLIPPED VALUES OF H
!           PROVIDED BY TELEMAC
            CALL MASKTF(MASKEL%R,H_TEL%R,HMIN,MESH%IKLE%I,
     &                  NELEM,NPOIN)
          ELSE
            CALL MASKTF(MASKEL%R,HN%R,HMIN,MESH%IKLE%I,
     &                  NELEM,NPOIN)
          ENDIF
!
          DO I=1,NELEM
            IF(MASKEL%R(I).NE.MSKTMP%R(I)) THEN
              CHGMSK = .TRUE.
              EXIT
            ENDIF
          ENDDO
!
!        JMH 17/12/2009
!
!        ELSEIF(OPTBAN.EQ.1) THEN
!
!          CANCELS Q QU AND QV IF HN.LE.0.D0
!          CALL MASKAB(HN%R,Q%R,QU%R,QV%R,NPOIN)
!
        ENDIF
!
! ----   BUILDS THE MASK OF THE POINTS FROM THE MASK OF THE ELEMENTS
! ----   AND CHANGES IFAMAS (IFABOR WITH MASKING)
!
        IF(MSK) CALL MASKTO(MASKEL%R,MASKPT,IFAMAS%I,
     &                      MESH%IKLE%I,
     &                      MESH%IFABOR%I,MESH%ELTSEG%I,MESH%NSEG,
     &                      NELEM,NPOIN,IELMT,MESH)
!
! ------------------------------------------------------------------
!  START OF SUB-ITERATIONS IN UNSTEADY STATE
!
! ------------------------------------------------------------------
!
702      CONTINUE
!
         ISOUS = ISOUS + 1
         AT0=AT0+DTS
         IF(ENTET.AND.ISOUS.EQ.1) CALL ENTETE_SISYPHE(2,AT0,LT)
         IF(ENTET.AND.ISOUS.EQ.NSOUS) ENTETS=.TRUE.
!
!---------------------------------------------------------------------
!        FRICTION COEFFICIENT VARIABLE IN TIME
!---------------------------------------------------------------------
!
         CALL CORSTR_SISYPHE
!
! ----   READS THE BOUNDARY CONDITIONS
!
         CALL CONLIT(MESH%NBOR%I)
!
! =======================================================================
!
!        IF 'VARIABLE TIME-STEP = YES' NSOUS WILL BE COMPUTED FURTHER DOWN
!        THE CONPUTATION OF THE TIMESTEP SIS HAS BEEN MOVED BEFORE READING
!        THE HYDRO CONDITIONS
!
!  ---   MEAN DIAMETER FOR THE ACTIVE-LAYER AND UNDER-LAYER
!
         IF(.NOT.MIXTE.AND.NSICLA.GT.1) CALL MEAN_GRAIN_SIZE
!
!  ---   MEAN VELOCITY UNORM
!
         CALL OS('X=N(Y,Z)',X=UNORM,Y=U2D,Z=V2D)
!
!  ---   WAVE ORBITAL VELOCITY --> UW
!
         IF(HOULE) THEN
           CALL CALCUW(UW%R,HN%R,HW%R,TW%R,GRAV,NPOIN)
         ENDIF
!
          CALL TOB_SISYPHE
     &     (TOB,TOBW, MU, KS, KSP,KSR,CF, FW,
     &      CHESTR, UETCAR, CF_TEL,KS_TEL, CODE ,
     &      KFROT, ICR, KSPRATIO,HOULE,
     &      GRAV,XMVE,  XMVS, VCE, KARMAN,ZERO,
     &      HMIN,HN, ACLADM, UNORM,UW, TW, NPOIN,KSPRED,IKS)
!
!  END OF INITIALISATION
!
      ! ******************** !
      ! BEDLOAD COMPUTATION  !
      ! ******************** !
        IF(CHARR) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'BEDLOAD_MAIN'
          CALL BEDLOAD_MAIN
     &        (ACLADM,KSP,KSR,VOLU2D,UNSV2D,
     &         CF,EBOR,FW,HN,LIQBOR,MASK,MASKEL,
     &         MASKPT,Q,QBOR,U2D,V2D,S,UNLADM,UW,THETAW,
     &         MU,TOB,TOBW,TW,ZF,DEBUG,HIDFAC,ICF,
     &         IELMT,ISOUS,KDDL,KDIR,KENT,KINC,KLOG,KNEU,KSORT,
     &         LOADMETH,LT,NPOIN,NPTFR,NSICLA,
     &         OPTBAN,LS0,BETA,FD90,FDM,GRAV,HIDI,HMIN,
     &         VCE,CSF_SABLE,XMVE,XMVS,XWC,PI,KARMAN,ZERO,
     &         KARIM_HOLLY_YANG,MSK,SUSP,VF,ENTET,
     &         CONST_ALAYER,LCONDIS,LGRAFED,MESH,
     &         ELAY,LIEBOR,LIMTEC,MASKTR,
     &         IT1,T1,T2,T3,T4,T5,T6,T7,T8,T9,
     &         T10,T11,T12,T13,UNORM,AC,AT0,DTS,ELAY0,FRACSED_GF,
     &         AVAIL,BREACH,CALFA,COEFPN,DZF_GF,HIDING,
     &         QSCL_C,QSCL_S,QS_C,QSCLXC,QSXC,QSCLYC,
     &         QSYC,SALFA,ZF_C,ZFCL_C,NSOUS,ENTETS,
     &         SECCURRENT,SLOPEFF,PHISED,DEVIA,BETA2,BIJK,
     &         SEDCO,HOULE,U3D,V3D,CODE)
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'END_BEDLOAD_MAIN'
!
!         UPDATES THE BOTTOM
!
          CALL OS('X=X+Y   ',X=ZF,Y=ZF_C)
!
!         UPDATES THE LAYERS  --> ELAY
!
          IF(.NOT.MIXTE.AND.NSICLA.GT.1) THEN
            IF(DEBUG.GT.0) WRITE(LU,*) 'LAYER'
            CALL LAYER(ZFCL_C,NLAYER,ZR,ZF,ESTRAT,ELAY,VOLU2D,
     &                 ACLADM,NSICLA,NPOIN,ELAY0,VOLTOT,ES,
     &                 AVAIL,CONST_ALAYER,DTS,T2%R,IT1%I)
            IF(DEBUG.GT.0) WRITE(LU,*) 'END_LAYER'
          ELSE
            CALL OS('X=Y-Z   ',X=ELAY,Y=ZF,Z=ZR)
          ENDIF
! END OF BEDLOAD
        ENDIF
      ! ********************** !
      ! SUSPENSION COMPUTATION !
      ! ********************** !
        IF(SUSP) THEN
!
          IF(DEBUG.GT.0) WRITE(LU,*) 'SUSPENSION_MAIN'
          CALL SUSPENSION_MAIN
     &(SLVTRA,HN,HN_TEL,MU,TOB,FDM,KSP,KSR,KS,
     & VOLU2D,V2DPAR,UNSV2D,AFBOR,BFBOR,ZF,LICBOR,
     & IFAMAS,MASKEL,MASKPT,U2D,V2D,NSICLA,
     & NPOIN,NPTFR,IELMT,OPTDIF,RESOL,LT,NIT,OPTBAN,OPTSUP,
     & OPDTRA,KENT,KSORT,KLOG,KINC,KNEU,KDIR,KDDL,
     & DEBUG,DTS,CSF_VASE,CSF_SABLE,ZERO,GRAV,XKX,XKY,
     & KARMAN,XMVE,XMVS,VCE,HMIN,XWC,VITCD,VITCE,PARTHENIADES,
     & BILMA,MSK,CHARR,IMP_INFLOW_C,MESH,ZF_S,CS,
     & CST,CTILD,CBOR,DISP,IT1,IT2,IT3,IT4,TB,T1,T2,T3,T4,T5,T6,
     & T7,T8,T9,T10,T11,T12,W1,TE1,CLT,TE2,TE3,S,AM1_S,AM2_S,MBOR,
     & ELAY,LIMDIF,MASKTR,TETA_SUSP,AC,
     & MASED0,MASINI,MASTEN,MASTOU,ES,AVAIL,ENTETS,PASS_SUSP,
     & ZFCL_S,HPROP,FLUDPT,FLUDP,FLUER,DISP_C,KX,KY,KZ,UCONV,
     & VCONV,QSXS,QSYS,QSCLXS,QSCLYS,QSCL_S,QS_S,QS_C,
     & CSTAEQ,ICQ,MASTCP,MASFIN,MASDEPT,MASDEP,MASSOU,CORR_CONV,
     & ZREF,SEDCO,VISC_TEL,CODE,DIFT,DM1,UCONV_TEL,VCONV_TEL,
     & ZCONV,SOLSYS,FLBOR_TEL,FLBOR_SIS,FLBORTRA,NUMLIQ%I,NFRLIQ,
     & MIXTE,NCOUCH_TASS,CONC_VASE,TOCE_VASE,TOCE_SABLE,
     & FLUER_VASE,TOCE_MIXTE,MS_SABLE,MS_VASE,TASS,DIRFLU)
         IF(DEBUG.GT.0) WRITE(LU,*) 'END_SUSPENSION_MAIN'
!
!      UPDATES THE BOTTOM
!
       CALL OS('X=X+Y   ',X=ZF,Y=ZF_S)
!
!      UPDATES THE LAYERS
!      REDEFINES THE LAYER OF ERODABLE SEDIMENT
!      EXTENDED GRANULOMETRY (TO BE REPLACED WITH NOMBLAY>1
!
        IF(.NOT.MIXTE.AND.NSICLA.GT.1) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'LAYER'
          CALL LAYER(ZFCL_S,NLAYER,ZR,ZF,ESTRAT,ELAY,VOLU2D,
     &               ACLADM,NSICLA,NPOIN,ELAY0,VOLTOT,ES,
     &               AVAIL,CONST_ALAYER,DTS,T2%R,IT1%I)
          IF(DEBUG.GT.0) WRITE(LU,*) 'END_LAYER'
        ELSE
          CALL OS('X=Y-Z   ',X=ELAY,Y=ZF,Z=ZR)
        ENDIF
! END OF SUSPENSION
      ENDIF
!
! RECONSTITUTES THE BEDLOAD AND/OR SUSPENSION DATA
! -----------------------------------------------------
!
        IF( DEBUG.GT.0) WRITE(LU,*) 'QS_RESULT'
!
        CALL OS('X=0     ', X=QSX)
        CALL OS('X=0     ', X=QSY)
!
        DO I = 1, NSICLA
          CALL OS('X=Y+Z   ', X=QSCLX%ADR(I)%P, Y=QSCLXC%ADR(I)%P,
     &                                          Z=QSCLXS%ADR(I)%P)
          CALL OS('X=Y+Z   ', X=QSCLY%ADR(I)%P, Y=QSCLYC%ADR(I)%P,
     &                                          Z=QSCLYS%ADR(I)%P)
          CALL OS('X=N(Y,Z)', X=QSCL%ADR(I)%P,  Y=QSCLX%ADR(I)%P,
     &                                          Z=QSCLY%ADR(I)%P)
          CALL OS('X=X+Y   ', X=QSX, Y=QSCLX%ADR(I)%P)
          CALL OS('X=X+Y   ', X=QSY, Y=QSCLY%ADR(I)%P)
        ENDDO
        CALL OS('X=N(Y,Z)', X=QS, Y=QSX, Z=QSY)
        IF(DEBUG.GT.0) WRITE(LU,*) 'END_QS_RESULT'
!
!=======================================================================
!
!     MAXIMUM BOTTOM SLOPE : EVOL IN T1
!
      IF(SLIDE) THEN
!
        IF(ENTET) CALL ENTETE_SISYPHE(14,AT0,LT)
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE MAXSLOPE'
        CALL MAXSLOPE(PHISED,ZF%R,ZR%R,MESH%XEL%R,MESH%YEL%R,MESH%NELEM,
     &                MESH%NELMAX,NPOIN,MESH%IKLE%I,T1,UNSV2D,MESH)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE MAXSLOPE'
        CALL OS('X=X+Y   ',X=ZF,Y=T1)
!
        IF(NSICLA.EQ.1) THEN
          CALL OS('X=Y-Z   ',X=ELAY,Y=ZF,Z=ZR)
        ELSE
          WRITE(LU,*) 'SLIDE NOT IMPLEMENTED WITH GRADED SEDIMENT'
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ENDIF
!
!========================================================================
!
!     SETTLING: EVOLUTION COMPUTED IN T3
!
      IF(TASS) THEN
!
        IF(ENTET) THEN
          IF(.NOT.CHARR.AND..NOT.SUSP.AND..NOT.SLIDE) THEN
            CALL ENTETE_SISYPHE(2,AT0,LT)
          ENDIF
          CALL ENTETE_SISYPHE(15,AT0,LT)
        ENDIF
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TASSEMENT'
        CALL TASSEMENT(ZF,NPOIN,DTS,ELAY,T3,T2,LT,AVAIL,NSICLA,
     &                 ES,XMVS,XKV,TRANS_MASS,CONC_VASE,NCOUCH_TASS,
     &                 MS_SABLE%R,MS_VASE%R)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TASSEMENT'
!
!       UPDATES ZF (ELAY HAS BEEN UPDATED IN TASSEMENT)
!
        CALL OS('X=X+Y   ',X=ZF,Y=T3)
!
      ENDIF
!
!=======================================================================
! : 5        COMPUTES THE EVOLUTIONS FOR THIS CYCLE OF TIMESTEP
!            AND UPDATES AFTER THIS COMPUTATION
!=======================================================================
!
! ----  COMPUTES  THE EVOLUTIONS FOR THIS (SUB) TIMESTEP
!
      IF(CHARR) THEN
        CALL OS('X=Y     ',X=E,Y=ZF_C)
      ELSE
        CALL OS('X=0     ',X=E)
      ENDIF
      IF(SUSP)  CALL OS('X=X+Y   ',X=E,Y=ZF_S)
      IF(SLIDE) CALL OS('X=X+Y   ',X=E,Y=T1)
      IF(TASS)  CALL OS('X=X+Y   ',X=E,Y=T3)
!
      CALL OS('X=X+Y   ', X=ESOMT, Y=E)
!
!  UPDATES
!
        IF(PART.EQ.-1) THEN
!
        CALL OS('X=X-Y   ',X=HN,Y=E)
        IF(OPTBAN.GT.0) THEN
          DO I = 1,HN%DIM1
            IF(HN%R(I).LT.HMIN) THEN
              U2D%R(I)=0.D0
              V2D%R(I)=0.D0
              HN%R(I) =HMIN
            ELSE
              U2D%R(I)= QU%R(I)/HN%R(I)
              V2D%R(I)= QV%R(I)/HN%R(I)
            ENDIF
          ENDDO
        ELSE
          CALL OS('X=Y/Z   ', X=U2D, Y=QU,   Z=HN)
          CALL OS('X=Y/Z   ', X=V2D, Y=QV,   Z=HN)
        ENDIF
!
!=======================================================================
! : 6     STOPS IF EVOLUTIONS GREATER THAN EMAX = RC*(INITIAL DEPTH)
!=======================================================================
!
!       DETERMINES THE MAXIMUM EVOLUTION THRESHOLD
        DO I = 1, NPOIN
          EMAX%R(I) = RC*MAX(HN%R(I),HMIN)
        ENDDO
!
! ----  STOPS WHEN THE EVOLUTIONS ARE GREATER THAN A CERTAIN THRESHOLD
!      THIS TEST IS ONLY CALLED IN 'SISYPHE ONLY' MODE
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'ARRET'
          CALL SIS_ARRET(ESOMT,EMAX,HN,VARSOR,NPOIN,MN,
     &                   SIS_FILES(SISRES)%LU,SIS_FILES(SISRES)%FMT,
     &                   MAXVAR,AT0,RC,HIST,BINRESSIS,TEXTE,
     &                   SORLEO,SORIMP,T1,T2)
          IF(DEBUG.GT.0) WRITE(LU,*) 'END_ARRET'
        ENDIF
!
! ----     CONSTANT FLOW DISCHARGE
!
        IF(LCONDIS) THEN
          CALL CONDIS_SISYPHE(CONSTFLOW)
        ELSE
          CONSTFLOW =.FALSE.
        ENDIF
!
!=======================================================================
! : 8     MASS BALANCE
!=======================================================================
!       COMPUTES THE COMPONENTS OF SAND TRANSPORT FOR THE MASS BALANCE,
!       GRAPHIC OUTPUTS AND VALIDATION STAGE
!
        IF(BILMA.AND.CHARR) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'BILAN_SISYPHE'
          CALL BILAN_SISYPHE(E,ESOMT,QSX,QSY,
     &                       MESH,MSK,MASKEL,T1,T2,S,IELMU_SIS,VCUMU,
     &       DTS,NPTFR,ENTETS,ZFCL_C,QSCLXC,QSCLYC,NSICLA,
     &       VOLTOT,DZF_GF,MASS_GF,LGRAFED,NUMLIQ%I,NFRLIQ)
          IF(DEBUG.GT.0) WRITE(LU,*) 'END_BILAN_SISYPHE'
        ENDIF
!
!       CONTROL SECTIONS
!
        IF(NCP.GT.0) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'FLUSEC_SISYPHE'
          CALL FLUSEC_SISYPHE(U2D,V2D,HN,
     &                        QSXC,QSYC,CHARR,QSXS,QSYS,SUSP,
     &                        MESH%IKLE%I,
     &                        MESH%NELMAX,MESH%NELEM,
     &                        MESH%X%R,MESH%Y%R,
     &                        DT,NCP,CTRLSC,ENTETS,AT0,MESH%KNOGL%I)
          IF(DEBUG.GT.0) WRITE(LU,*) 'END_FLUSEC_SISYPHE'
        ENDIF
!
!-----------------------------------------------------------------------
!
        IF(.NOT.PERMA.AND.SIS_FILES(SISHYD)%NAME(1:1).NE.' ') THEN
!
!         UPDATES THE HYDRO
!
!         IF READING ON HYDRODYNAMIC FILE, INCREMENTS QU, QV AND Z
          IF(SIS_FILES(SISHYD)%NAME(1:1).NE.' ')  THEN
            CALL OS('X=X+Y   ', X=QU, Y=DEL_QU)
            CALL OS('X=X+Y   ', X=QV, Y=DEL_QV)
            CALL OS('X=X+Y   ', X=Z , Y=DEL_Z)
          ENDIF
          CALL OS('X=Y-Z   ', X=HN, Y=Z, Z=ZF)
!         CLIPS NEGATIVE DEPTHS
          IF(OPTBAN.GT.0) THEN
            DO I = 1, NPOIN
             IF(HN%R(I).LT.HMIN) THEN
               U2D%R(I)=0.D0
               V2D%R(I)=0.D0
               HN%R(I) = MAX(HN%R(I),HMIN)
             ELSE
               U2D%R(I)= QU%R(I)/HN%R(I)
               V2D%R(I)= QV%R(I)/HN%R(I)
             ENDIF
            ENDDO
          ELSE
            CALL OS('X=Y/Z   ', X=U2D, Y=QU,   Z=HN)
            CALL OS('X=Y/Z   ', X=V2D, Y=QV,   Z=HN)
          ENDIF
!
        ENDIF
!
!       END OF THE LOOP ON SUB-TIMESTEPS NSOUS
! ---------------------------------------------------------
        IF(DEBUG.GT.0) WRITE(LU,*) 'SOUS_ITERATION_NEXT'
        IF (ISOUS < NSOUS) GOTO 702
        IF(DEBUG.GT.0) WRITE(LU,*) 'END_SOUS_ITERATION'
!=======================================================================
! : 9        PRINTS OUT EXTREME VALUES
!=======================================================================
!
        IF(ENTET.AND.CHARR) THEN
          WRITE(LU,*)
          CALL MAXI(XMAX,IMAX,E%R,NPOIN)
          IF(NCSIZE.GT.1) THEN
            XMA=P_DMAX(XMAX)
            IF(XMAX.EQ.XMA) THEN
              IMA=MESH%KNOLG%I(IMAX)
            ELSE
              IMA=0
            ENDIF
            IMA=P_IMAX(IMA)
          ELSE
            IMA=IMAX
            XMA=XMAX
          ENDIF
          IF(LNG.EQ.1) WRITE(LU,371) XMA,IMA
          IF(LNG.EQ.2) WRITE(LU,372) XMA,IMA
371       FORMAT(' EVOLUTION MAXIMUM        : ',G16.7,' NOEUD : ',I6)
372       FORMAT(' MAXIMAL EVOLUTION        : ',G16.7,' NODE  : ',I6)
          CALL MINI(XMIN,IMIN,E%R,NPOIN)
          IF(NCSIZE.GT.1) THEN
            XMI=P_DMIN(XMIN)
            IF(XMIN.EQ.XMI) THEN
              IMI=MESH%KNOLG%I(IMIN)
            ELSE
              IMI=0
            ENDIF
            IMI=P_IMAX(IMI)
          ELSE
            IMI=IMIN
            XMI=XMIN
          ENDIF
          IF(LNG.EQ.1) WRITE(LU,373) XMI,IMI
          IF(LNG.EQ.2) WRITE(LU,374) XMI,IMI
373       FORMAT(' EVOLUTION MINIMUM        : ',G16.7,' NOEUD : ',I6)
374       FORMAT(' MINIMAL EVOLUTION        : ',G16.7,' NODE  : ',I6)
!
          IF(CONST_ALAYER) THEN
            IF(NSICLA.GT.1.AND.XMI.LT.-0.5D0*ELAY0) THEN
              IF(LNG.EQ.1) WRITE(LU,885)
              IF(LNG.EQ.2) WRITE(LU,886)
885           FORMAT(' EROSION SUPERIEURE A EPAISSEUR DE COUCHE !')
886           FORMAT(' EROSION GREATER THAN ONE LAYER THICKNESS !')
            ENDIF
            IF(NSICLA.GT.1.AND.XMA.GT.ELAY0) THEN
              IF(LNG.EQ.1) WRITE(LU,887)
              IF(LNG.EQ.2) WRITE(LU,888)
887           FORMAT(' DEPOT SUPERIEUR A EPAISSEUR DE COUCHE !')
888           FORMAT(' DEPOSITION MORE THAN ONE LAYER THICKNESS !')
            ENDIF
          ELSE
            DO J=1,NPOIN
              ELAY0 = 3.D0*ACLADM%R(J)
              IF(NSICLA.GT.1.AND.E%R(J).LT.-0.5D0*ELAY0) THEN
                IF(LNG.EQ.1) WRITE(LU,885)
                IF(LNG.EQ.2) WRITE(LU,886)
              ENDIF
              IF(NSICLA.GT.1.AND.E%R(J).GT.ELAY0) THEN
                IF(LNG.EQ.1) WRITE(LU,887)
                IF(LNG.EQ.2) WRITE(LU,888)
              ENDIF
            ENDDO
          ENDIF
        ENDIF
        IF(ENTET) THEN
          CALL MAXI(XMAX,IMAX,ESOMT%R,NPOIN)
          IF(NCSIZE.GT.1) THEN
            XMA=P_DMAX(XMAX)
            IF(XMAX.EQ.XMA) THEN
              IMA=MESH%KNOLG%I(IMAX)
            ELSE
              IMA=0
            ENDIF
            IMA=P_IMAX(IMA)
          ELSE
            IMA=IMAX
            XMA=XMAX
          ENDIF
          IF (LNG.EQ.1) WRITE(LU,881) XMA,IMA
          IF (LNG.EQ.2) WRITE(LU,882) XMA,IMA
881       FORMAT(' EVOLUTION MAXIMUM TOTALE : ',G16.7,' NOEUD : ',I6)
882       FORMAT(' TOTAL MAXIMAL EVOLUTION  : ',G16.7,' NODE  : ',I6)
          CALL MINI(XMIN,IMIN,ESOMT%R,NPOIN)
          IF(NCSIZE.GT.1) THEN
            XMI=P_DMIN(XMIN)
            IF(XMIN.EQ.XMI) THEN
              IMI=MESH%KNOLG%I(IMIN)
            ELSE
              IMI=0
            ENDIF
            IMI=P_IMAX(IMI)
          ELSE
            IMI=IMIN
            XMI=XMIN
          ENDIF
          IF (LNG.EQ.1) WRITE(LU,883) XMI,IMI
          IF (LNG.EQ.2) WRITE(LU,884) XMI,IMI
883       FORMAT(' EVOLUTION MINIMUM TOTALE : ',G16.7,' NOEUD : ',I6)
884       FORMAT(' TOTAL MINIMAL EVOLUTION  : ',G16.7,' NODE  : ',I6)
        ENDIF
!=======================================================================
! : 10         PRINTS OUT RESULTS AT THIS TIMESTEP
!              AND COMPARES AGAINST A REFERENCE FILE
!=======================================================================
!
!       IN COUPLING MODE, OUTPUT TIMES OF TELEMAC AND SISYPHE ARE
!       SYNCHRONISED, IT MEANS THAT WE MUST HAVE :
!       LT * DT (TIME AT THE END OF TIME STEP LT IN TELEMAC)
!       EQUAL TO:
!       (LT-1)*DT + PERICOU*DT (TIME AT THE END OF TIME STEP LT IN SISYPHE
!       HENCE THE FACT THAT LT IS REPLACED BY LT-1+PERICOU
!       DEFAULT VALUE OF PERICOU IS 1
!
        IF(UNIT) CALL OS('X=CX    ', X=CS, C= XMVS)
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PREDES'
        CALL PREDES(LT-1+PERICOU,AT0)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PREDES'
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE BIEF_DESIMP'
        CALL BIEF_DESIMP(SIS_FILES(SISRES)%FMT,VARSOR,
     &                   HIST,0,NPOIN,SIS_FILES(SISRES)%LU,
     &                   'STD',AT0,LT-1+PERICOU,LISPR,LEOPR,
     &                   SORLEO,SORIMP,MAXVAR,TEXTE,PTINIG,PTINIL)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BIEF_DESIMP'
!
        IF(UNIT) CALL OS('X=CX    ', X=CS,  C= 1.D0/XMVS)
!
!       SENDS THE NEW ZF TO TELEMAC-2D OR 3D
!
        IF(CODE(1:7) == 'TELEMAC') THEN
          CALL OV ('X=Y     ', ZF_SIS%R, ZF%R, ZF%R, 0.D0, NPOIN)
        ENDIF
!
!       THE SUBROUTINE VALIDA FROM THE LIBRARY IS STANDARD
!       IT CAN BE MODIFIED FOR EACH PARTICULAR CASE
!       BUT ITS CALL MUST BE LEFT IN THE LOOP ON TIME
!
        IF(VALID) THEN
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE BIEF_VALIDA'
          CALL BIEF_VALIDA(TB,TEXTPR,SIS_FILES(SISREF)%LU,
     &                     SIS_FILES(SISREF)%FMT,
     &                     VARSOR,TEXTE,SIS_FILES(SISRES)%LU,
     &                     SIS_FILES(SISRES)%FMT,
     &                     MAXVAR,NPOIN,LT,VALNIT,ALIRV)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BIEF_VALIDA'
        ENDIF
!
!       END OF THE LOOP ON THE RECORDS : 700
700     CONTINUE
!
!=======================================================================
!
!       END OF THE LOOP ON THE NUMBER OF EVENTS : 710
!
710     CONTINUE
!
!-----------------------------------------------------------------------
!
        IF(DEBUG.GT.0) WRITE(LU,*) 'END_TIME_LOOP'
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(DREDGESIM.AND.(LOOPCOUNT.EQ.TELNIT.AND.PART.EQ.1.
     &                                            .OR. PART.EQ.-1)) THEN
         CALL DREDGESIM_INTERFACE(3)
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
