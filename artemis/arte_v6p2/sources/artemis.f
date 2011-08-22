!                    ******************
                     SUBROUTINE ARTEMIS
!                    ******************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SOLVES THE MODIFIED BERKHOFF EQUATION.
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        21/04/1999
!+        V5P1
!+
!
!history  C. DENIS (SINETICS)
!+        21/06/2010
!+        V6P0
!+   PARALLEL VERSION
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
      USE GRACESTOP
!
!-----------------------------------------------------------------------
! DECLARES TYPES AND DIMENSIONS
!-----------------------------------------------------------------------
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
! INTEGERS
!
      INTEGER LT,NPERBA,I,J
      INTEGER NELBRD,NPFMAX,NELBRX
      INTEGER LPER,LDIR
      INTEGER ALIRE(MAXVAR)
!
! VARIABLE FOR SUBROUTINE DISMOY
!
      INTEGER LISHHO
!
! REAL SCALARS
!
      DOUBLE PRECISION RADDEG,HIST(1)
!
! VARIABLES FOR CALLS TO TELEMAC-2D SUBROUTINES
!
      INTEGER NVARCL,ISTO
      DOUBLE PRECISION LAMBD0
      LOGICAL RESU,FROVAR,PROLIN,TRAC
!
! USED FOR DUMMY ARGUMENTS
!
      INTEGER IBID
      DOUBLE PRECISION BID
!
      INTEGER  P_IMAX,P_IMIN
      DOUBLE PRECISION P_DMIN
      EXTERNAL P_IMAX,P_IMIN,P_DMIN
!
      DATA HIST /9999.D0/
!
!-----------------------------------------------------------------------
!
!  VARIABLES TO READ IF COMPUTATION IS CONTINUED :
!  0 : DISCARD    1 : READ  (SEE SUBROUTINE NOMVAR)
!
      DATA ALIRE /1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
!
!-----------------------------------------------------------------------
!
      RADDEG = 180.D0/3.141592654D0
!
!=======================================================================
!
! : 1          READS, PREPARES AND CONTROLS THE DATA
!
!=======================================================================
!
!  TYPES OF DISCRETISATION:
!
!  TRIANGLES : P1
      IELM  = 11
!  SEGMENTS  : P1 FOR THE BOUNDARY
      IELMB = 1
!
!
!  MAXIMUM SIZE (CASE OF AN ADAPTIVE GRID)
!  THESE PARAMETERS ARE USED IN BIEF CALLS
!
!     NODES
      NPMAX = NPOIN
!     ELEMENTS
      NELMAX = NELEM
!     BOUNDARY ELEMENTS
      NELBRD = NPTFR
!     BOUNDARY ELEMENTS (MAXIMUM NUMBER)
      NPFMAX = NPTFR
!     BOUNDARY NODES
      NELBRX = NPTFR
!
      IF(BALAYE) THEN
        NPERBA = INT((PERFIN-PERDEB)/PERPAS) + 1
      ENDIF
!
!=======================================================================
!
      RESU   = .TRUE.
      FROVAR = .FALSE.
      PROLIN = .FALSE.
      SPHERI = .FALSE.
      TRAC   = .FALSE.
      NVARCL = 0
!
! IN TELEMAC-2D, LIHBOR = KINC IS AUTOMATICALLY CHANGED TO KSORT
! HAS TO MODIFY THE VALUE OF KINC FOR PREDA2, TO AVOID THIS AUTOMATIC CHANGE
! IN ADDITION, IN TELEMAC-2D, LIHBOR = KADH (NOT KNOWN HERE) GENERATES
! A MESSAGE. TO AVOID IT, ISTO IS ALSO USED IN PLACE OF KADH.
!
!
      ISTO = 100
!
!-----------------------------------------------------------------------
!
! READS THE BOUNDARY CONDITIONS AND INDICES FOR THE BOUNDARY NODES.
!
      CALL LECLIM_ARTEMIS
     &(LIHBOR%I,LIUBOR%I,MESH%NPTFR,MESH%NBOR%I,STDGEO,
     & ART_FILES(ARTCLI)%LU,
     & MESH%ISEG%I,MESH%XSEG%R,MESH%YSEG%R,MESH%NACHB%I,NUMLIQ%I,
     & MESH%IFAPAR%I)
!
!-----------------------------------------------------------------------
!
! COMPLEMENTS THE DATA STRUCTURE FOR BIEF
!
      CALL INBIEF(LIHBOR%I,KLOG,IT1,IT2,IT3,LVMAC,IELM,
     &         LAMBD0,SPHERI,MESH,T1,T2,OPTASS,PRODUC,EQUA)
      IF (NCSIZE .LE. 1) THEN
         NPOIN_TOT=MESH%NPOIN
         ALLOCATE(XT(NPOIN_TOT))
         ALLOCATE(YT(NPOIN_TOT))
      ENDIF
!-----------------------------------------------------------------------
!  LOOKS FOR BOTTOM AND BOTTOM FRICTION IN THE GEOMETRY FILE :
!-----------------------------------------------------------------------
!
      CALL FONSTR(T1,ZF,T2,FW,ART_FILES(ARTGEO)%LU,ART_FILES(ARTFON)%LU,
     &            ART_FILES(ARTFON)%NAME,MESH,FFON,LISTIN)
!-----------------------------------------------------------------------
!
! PREPARES THE RESULTS FILE (OPTIONAL)
!
!     STANDARD SELAFIN FORMAT
!
        ! CREATES DATA FILE USING A GIVEN FILE FORMAT : FORMAT_RES.
        ! THE DATA ARE CREATED IN THE FILE: NRES, AND ARE
        ! CHARACTERISED BY A TITLE AND NAME OF OUTPUT VARIABLES
        ! CONTAINED IN THE FILE.
        CALL CREATE_DATASET(ART_FILES(ARTRES)%FMT, ! RESULTS FILE FORMAT
     &                      ART_FILES(ARTRES)%LU,  ! LU FOR RESULTS FILE
     &                      TITCAS,     ! TITLE
     &                      MAXVAR,     ! MAX NUMBER OF OUTPUT VARIABLES
     &                      TEXTE,      ! NAMES OF OUTPUT VARIABLES
     &                      SORLEO)     ! PRINT TO FILE OR NOT
        ! WRITES THE MESH IN THE OUTPUT FILE :
        ! IN PARALLEL, REQUIRES NCSIZE AND NPTIR.
        ! THE REST OF THE INFORMATION IS IN MESH.
        ! ALSO WRITES : START DATE/TIME AND COORDINATES OF THE
        ! ORIGIN.
        CALL WRITE_MESH(ART_FILES(ARTRES)%FMT, ! RESULTS FILE FORMAT
     &                  ART_FILES(ARTRES)%LU,  ! LU FOR RESULTS FILE
     &                  MESH,          ! CHARACTERISES MESH
     &                  1,             ! NUMBER OF PLANES /NA/
     &                  MARDAT,        ! START DATE
     &                  MARTIM,        ! START TIME
     &                  I_ORIG,J_ORIG) ! COORDINATES OF THE ORIGIN.
!
!-----------------------------------------------------------------------
!
!     INITIALISES PRIVE
!
      IF(NPRIV.GT.0) CALL OS('X=C     ',PRIVE,PRIVE,PRIVE,0.D0)
!
!=======================================================================
!
      IF(NCSIZE.GT.1) THEN
         NFRLIQ=0
         DO I=1,NPTFR
            NFRLIQ=MAX(NFRLIQ,NUMLIQ%I(I))
         ENDDO
         NFRLIQ=P_IMAX(NFRLIQ)
         WRITE(LU,*) ' '
         IF(LNG.EQ.1) WRITE(LU,*) 'NOMBRE DE FRONTIERES LIQUIDES :',
     &        NFRLIQ
         IF(LNG.EQ.2) WRITE(LU,*) 'NUMBER OF LIQUID BOUNDARIES:',NFRLIQ
      ELSE
         CALL FRONT2(NFRLIQ,NFRSOL,DEBLIQ,FINLIQ,DEBSOL,FINSOL,
     &        LIHBOR%I,LIUBOR%I,
     &        MESH%X%R,MESH%Y%R,MESH%NBOR%I,MESH%KP1BOR%I,
     &        IT1%I,NPOIN,NPTFR,KLOG,LISTIN,NUMLIQ%I,MAXFRO)
      ENDIF
! LOCATES THE BOUNDARIES
!
!=======================================================================
!
! CORRECTS THE VALUES OF THE BOTTOM (OPTIONAL)
!
! STANDARD SUBROUTINE DOES NOT DO ANYTHING
!
      CALL CORFON
!
!
!=======================================================================
!
! INITIALISES THE WAVE HEIGHT FOR RANDOM SEAS AT 0.
!
      IF (ALEMON .OR. ALEMUL) THEN
       CALL OS('X=C     ', HALE , SBID , SBID , 0.D0 )
      ENDIF
!
!
! DETERMINES THE DIFFERENT PERIODS FOR A RANDOM SEA COMPUTATION
!
      IF (ALEMON.OR.ALEMUL) THEN
         CALL PERALE(PALE%R,GAMMA,PERPIC,NPALE,T1%R,NPOIN,PRIVE,
     &               NPRIV,PMIN,PMAX)
         PER = PALE%R(1)
      ENDIF
!
!
! DETERMINES THE DIFFERENT DIRECTIONS FOR A MULTIDIRECTIONAL RANDOM
! SEA COMPUTATION
!
      IF (ALEMUL) THEN
         CALL DIRALE(DALE%R,EXPOS,TETAH,TETMIN,TETMAX,NDALE,
     &               T1%R,NPOIN,PRIVE,NPRIV)
      ENDIF
!
!
!
!=======================================================================
!
! START OF COMPUTATION
!
! LT REFERS TO THE CURRENT TIME STEP (STARTS FROM 0 SO THAT
! THE FIRST COMPUTATION ALWAYS BE RECORDED)
!
      LT = 0
!
! INITIALISES QB, T01, T02 AND TM : SET TO 0 AT THE START OF COMPUTATION
!
      CALL OS('X=C     ', QB , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', T01 , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', T02 , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', TM , SBID , SBID , 0.D0 )
!
!
! INITIALISES RADIATION STRESSES AND
! FORCINGS
!
      CALL OS('X=C     ', FX , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', FY , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', SXX , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', SXY , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', SYY , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', MCOS , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', MSIN , SBID , SBID , 0.D0 )
!
! FOR A RANDOM SEA COMPUTATION, LPER AND LDIR REFER TO THE COMPUTED
! PERIOD AND DIRECTION
!
      LPER = 1
      LDIR = 1
!
100   CONTINUE
!
      IF (BALAYE) THEN
         CALL ENTART(1,PER,LT,LPER,NPERBA,ALEMON,ALEMUL,BALAYE)
      ELSE
         CALL ENTART(1,PER,LT,LPER,NPALE,ALEMON,ALEMUL,BALAYE)
      ENDIF
!
!=======================================================================
!
! : 2                  INITIALISES
!
!=======================================================================
!
! INITIALISES PHYSICAL PARAMETERS
!
!
      CALL CONDIH
!
!=======================================================================
!
! : 3                  BOUNDARY CONDITIONS
!
!=======================================================================
!
! CALLS THE USER SUBROUTINE
!
      IF(NCSIZE.GT.1) THEN
         CALL BUILD_GLOBAL_BOUND(MESH%KNOLG%I,MESH%NPOIN,NPOIN_TOT,
     &        MESH%NPTFR,NPTFR_TOT,
     &        X,Y,K%R,C%R,CG%R,LIHBOR%I,XT,
     &        YT,KT,CTT,CGT,LIHBORT,MESH%NBOR%I,NBOR_TOT)
      ELSE
            DO I=1,NPOIN
               XT(I)=X(I)
               YT(I)=Y(I)
            ENDDO
            DO I=1,NPTFR
               NBOR_TOT(I)=MESH%NBOR%I(I)
               LIHBORT(I)=LIHBOR%I(I)
            ENDDO
      ENDIF
      CALL BORH
!
! MASKING FOR THE BOUNDARY CONDITIONS
!
! IN MULTIDIRECTIONAL RANDOM SEA, THE DIRECTIONS OF PROPAGATION
! (AT THE BOUNDARY) HAVE BEEN CALCULATED IN DALE.
!
200   IF (ALEMUL) THEN
         CALL OS('X=C     ', TETAB ,SBID,SBID, DALE%R(LDIR) )
         CALL ENTART(2,DALE%R(LDIR),LT,LDIR,NDALE,ALEMON,ALEMUL,BALAYE)
      ENDIF
!
! CALCULATES THE BOUNDARY CONDITIONS ON THE POTENTIAL FROM USER INPUT.
!
!
!      IF (LT .EQ. 0) THEN
      CALL MASQUE_ARTEMIS
!
      CALL PHBOR
!      END IF
!
!=======================================================================
!
! : 4                  SOLVES THE BERKHOFF EQUATION
!
!=======================================================================
!
      CALL BERKHO (LT)
!
!=======================================================================
!
! : 5.1        COMPUTES SPEED, FREE SURFACE ELEVATION,
!              WAVE HEIGHT AND PHASE
!
!=======================================================================
!
      CALL CALRES
!
      IF (ALEMON .OR. ALEMUL) THEN
!
!        CUMULATIVELY COMPUTES THE M1, M2, AND MT1 MOMENTUMS
!        STORED UNTIL THE LAST COMPUTATION IN T01, T02, AND TM
!
!
         CALL CALCMN
!
      ENDIF
!
!
!=======================================================================
!
! : 5.2        COMPUTES RADIATION STRESSES AND
!              DRIVING FORCES FOR REGULAR WAVES.
!
!=======================================================================
!
      IF (.NOT.ALEMON .AND. .NOT.ALEMUL) THEN
!
       IF (LISHOU) THEN
         CALL DISMOY
     &   (NPOIN,NELEM,MESH%X%R,MESH%Y%R,MESH%IKLE%I,K%R,LISHHO)
       ELSE
         LISHHO = 0
       ENDIF
!
         CALL RADIA1 (LISHHO)
!
      ELSE
         LISHHO = 0
      ENDIF
!=======================================================================
!
! : 6   CALLS A USER SUBROUTINE FOR PRINT OUTS, ANALYTICAL SOLUTIONS...
!       (STANDARD SUBROUTINE DOES NOT DO ANYTHING)
!
!=======================================================================
!
      CALL UTIMP
     &(PHIR%R,PHII%R,C%R,CG%R,K%R,MESH%X%R,MESH%Y%R,ZF%R,H%R,
     & HHO%R,U0%R,V0%R,PHAS%R,S%R,T1%R,T2%R,T3%R,T4%R,INCI%R,
     & GRAV,PER,OMEGA,MESH%IKLE%I,MESH%NBOR%I,MESH%KP1BOR%I,
     & NELEM,NELMAX,IELM,IELMB,NPTFR,NPOIN,PRIVE)
!
!=======================================================================
!
! : 7                  PRINTS OUT THE RESULTS
!
!=======================================================================
!
!
! FOR RANDOM SEAS,
! OUTPUTS ONLY AT THE PEAK PERIOD
!
      IF (.NOT.ALEMON .AND. .NOT.ALEMUL) THEN
!
!=======================================================================
!
!     CONVERTS INCI INTO DEGREES
!
!=======================================================================
!
         CALL OS('X=CX    ', INCI , SBID , SBID , RADDEG )
!
! RUBENS FILE
!
         CALL BIEF_DESIMP(ART_FILES(ARTRES)%FMT,VARSOR,
     &            HIST,0,NPOIN,ART_FILES(ARTRES)%LU,'STD',PER,0,
     &            LISPRD,LEOPRD,
     &            SORLEO,SORIMP,MAXVAR,TEXTE,0,0)
!
!=======================================================================
!
!              COMPARISON AGAINST A REFERENCE FILE
!
!=======================================================================
!
!     THE VALIDA SUBROUTINE FROM THE BIEF LIBRARY IS STANDARD.
!     IT CAN BE MODIFIED BY THE USER FOR THEIR PARTICULAR CASE.
!     BUT THE CALL TO THE SUBROUTINE MUST STAY IN THE TIME LOOP.
!
         IF(VALID) THEN
           CALL BIEF_VALIDA(TB,TEXTE,
     &                      ART_FILES(ARTREF)%LU,ART_FILES(ARTREF)%FMT,
     &                      VARSOR,TEXTE,
     &                      ART_FILES(ARTRES)%LU,ART_FILES(ARTRES)%FMT,
     &                      MAXVAR,NPOIN,LT,LT,ALIRE)
         ENDIF
!
      ENDIF
!
!=======================================================================
!
! : 8                  GOES TO NEXT PERIOD
!
!=======================================================================
!
! IF SWEEPS A RANGE OF PERIODS
!
      IF (BALAYE) THEN
         LT   = LT  + 1
         LPER = LPER + 1
         PER  = PER + PERPAS
         IF (PER.LE.PERFIN) GOTO 100
      ENDIF
!
!
!=======================================================================
!
! IF RANDOM SEAS
!
!=======================================================================
!
      IF (ALEMON .OR. ALEMUL) THEN
!
         LT  = LT  + 1
!
         IF (LT.LT.NPALE*NDALE) THEN
!
!           REACTUALISES THE ENERGY OF THE RANDOM SEA
            CALL OS('X=X+CYZ ',HALE,HHO,HHO,1.D0/DBLE(NPALE*NDALE))
!
!           GOES TO NEXT DIRECTION
            LDIR = LDIR + 1
            IF (LDIR.LE.NDALE) GOTO 200
!
!           GOES TO NEXT PERIOD
            LDIR = 1
            LPER = LPER + 1
            PER = PALE%R(LPER)
            GOTO 100
!
         ELSE
!
!           LAST COMPUTATION: DETERMINES THE MEAN PERIODS
!           (T01 AND T02), AND THE MEAN DIRECTION (INCI)
!
!
            CALL CALCTM
!
!           DETERMINES MEAN K, C AND CG
!
            CALL CALRE2
!
!           TAKES INTO ACCOUNT THE LAST WAVE HEIGHT
!           FOR RANDOM SEAS
!
            CALL OS('X=X+CYZ ',HALE,HHO,HHO,1.D0/DBLE(NPALE*NDALE))
            CALL OS('X=SQR(Y)', HALE , HALE , SBID , BID )
            CALL OS('X=CX    ',QB,SBID,SBID,1.D0/DBLE(NPALE*NDALE))
!
!=======================================================================
!
!           COMPUTES RADIATION STRESSES
!           AND DRIVING FORCES FOR RANDOM SEAS
!
!=======================================================================
!
            CALL RADIA2 (LISHHO)
!
!=======================================================================
!
!          CONVERTS INCI INTO DEGREES
!
!=======================================================================
!
            CALL OS('X=CX    ', INCI , SBID , SBID , RADDEG )
!
!=======================================================================
!
!           RUBENS FILE
!
!=======================================================================
!
            CALL BIEF_DESIMP(ART_FILES(ARTRES)%FMT,VARSOR,
     &            HIST,0,NPOIN,ART_FILES(ARTRES)%LU,'STD',PERPIC,0,
     &            LISPRD,LEOPRD,
     &            SORLEO,SORIMP,MAXVAR,TEXTE,0,0)
!
!=======================================================================
!
!              COMPARISON AGAINST A REFERENCE FILE
!
!=======================================================================
!
!
!     THE VALIDA SUBROUTINE FROM THE BIEF LIBRARY IS STANDARD.
!     IT CAN BE MODIFIED BY THE USER FOR THEIR PARTICULAR CASE.
!     BUT THE CALL TO THE SUBROUTINE MUST STAY IN THE TIME LOOP.
!
            IF(VALID) THEN
              CALL BIEF_VALIDA(TB,TEXTE,
     &                       ART_FILES(ARTREF)%LU,ART_FILES(ARTREF)%FMT,
     &                       VARSOR,TEXTE,
     &                       ART_FILES(ARTRES)%LU,ART_FILES(ARTRES)%FMT,
     &                       MAXVAR,NPOIN,LT,LT,ALIRE)
            ENDIF
!
         ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
