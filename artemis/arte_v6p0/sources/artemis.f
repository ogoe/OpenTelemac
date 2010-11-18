C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES THE MODIFIED BERKHOFF EQUATION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_ARTEMIS, DECLARATIONS_TELEMAC, GRACESTOP
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_ARTEMIS :<br>
!> @link DECLARATIONS_ARTEMIS::ALEMON ALEMON@endlink, 
!> @link DECLARATIONS_ARTEMIS::ALEMUL ALEMUL@endlink, 
!> @link DECLARATIONS_ARTEMIS::ART_FILES ART_FILES@endlink, 
!> @link DECLARATIONS_ARTEMIS::BALAYE BALAYE@endlink, 
!> @link DECLARATIONS_ARTEMIS::C C@endlink, 
!> @link DECLARATIONS_ARTEMIS::CG CG@endlink, 
!> @link DECLARATIONS_ARTEMIS::CGT CGT@endlink, 
!> @link DECLARATIONS_ARTEMIS::CTT CTT@endlink, 
!> @link DECLARATIONS_ARTEMIS::DALE DALE@endlink, 
!> @link DECLARATIONS_ARTEMIS::DEBLIQ DEBLIQ@endlink, 
!> @link DECLARATIONS_ARTEMIS::DEBSOL DEBSOL@endlink, 
!> @link DECLARATIONS_ARTEMIS::EQUA EQUA@endlink, 
!> @link DECLARATIONS_ARTEMIS::EXPOS EXPOS@endlink, 
!> @link DECLARATIONS_ARTEMIS::FFON FFON@endlink, 
!> @link DECLARATIONS_ARTEMIS::FINLIQ FINLIQ@endlink, 
!> @link DECLARATIONS_ARTEMIS::FINSOL FINSOL@endlink, 
!> @link DECLARATIONS_ARTEMIS::FW FW@endlink, 
!> @link DECLARATIONS_ARTEMIS::FX FX@endlink, 
!> @link DECLARATIONS_ARTEMIS::FY FY@endlink, 
!> @link DECLARATIONS_ARTEMIS::GAMMA GAMMA@endlink, 
!> @link DECLARATIONS_ARTEMIS::GRAV GRAV@endlink, 
!> @link DECLARATIONS_ARTEMIS::H H@endlink, 
!> @link DECLARATIONS_ARTEMIS::HALE HALE@endlink, 
!> @link DECLARATIONS_ARTEMIS::HHO HHO@endlink, 
!> @link DECLARATIONS_ARTEMIS::IELM IELM@endlink, 
!> @link DECLARATIONS_ARTEMIS::IELMB IELMB@endlink, 
!> @link DECLARATIONS_ARTEMIS::INCI INCI@endlink, 
!> @link DECLARATIONS_ARTEMIS::IT1 IT1@endlink, 
!> @link DECLARATIONS_ARTEMIS::IT2 IT2@endlink, 
!> @link DECLARATIONS_ARTEMIS::IT3 IT3@endlink, 
!> @link DECLARATIONS_ARTEMIS::I_ORIG I_ORIG@endlink, 
!> @link DECLARATIONS_ARTEMIS::J_ORIG J_ORIG@endlink, 
!> @link DECLARATIONS_ARTEMIS::K K@endlink, 
!> @link DECLARATIONS_ARTEMIS::KT KT@endlink, 
!> @link DECLARATIONS_ARTEMIS::LEOPRD LEOPRD@endlink, 
!> @link DECLARATIONS_ARTEMIS::LIHBOR LIHBOR@endlink, 
!> @link DECLARATIONS_ARTEMIS::LIHBORT LIHBORT@endlink, 
!> @link DECLARATIONS_ARTEMIS::LISHOU LISHOU@endlink, 
!> @link DECLARATIONS_ARTEMIS::LISPRD LISPRD@endlink, 
!> @link DECLARATIONS_ARTEMIS::LISTIN LISTIN@endlink, 
!> @link DECLARATIONS_ARTEMIS::LIUBOR LIUBOR@endlink, 
!> @link DECLARATIONS_ARTEMIS::LVMAC LVMAC@endlink, 
!> @link DECLARATIONS_ARTEMIS::MARDAT MARDAT@endlink, 
!> @link DECLARATIONS_ARTEMIS::MARTIM MARTIM@endlink, 
!> @link DECLARATIONS_ARTEMIS::MAXFRO MAXFRO@endlink, 
!> @link DECLARATIONS_ARTEMIS::MAXVAR MAXVAR@endlink, 
!> @link DECLARATIONS_ARTEMIS::MCOS MCOS@endlink, 
!> @link DECLARATIONS_ARTEMIS::MESH MESH@endlink, 
!> @link DECLARATIONS_ARTEMIS::MSIN MSIN@endlink, 
!> @link DECLARATIONS_ARTEMIS::NBOR_TOT NBOR_TOT@endlink, 
!> @link DECLARATIONS_ARTEMIS::NDALE NDALE@endlink, 
!> @link DECLARATIONS_ARTEMIS::NELEM NELEM@endlink, 
!> @link DECLARATIONS_ARTEMIS::NELMAX NELMAX@endlink, 
!> @link DECLARATIONS_ARTEMIS::NFRLIQ NFRLIQ@endlink, 
!> @link DECLARATIONS_ARTEMIS::NFRSOL NFRSOL@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPALE NPALE@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPMAX NPMAX@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPOIN NPOIN@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPOIN_TOT NPOIN_TOT@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPRIV NPRIV@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPTFR NPTFR@endlink, 
!> @link DECLARATIONS_ARTEMIS::NPTFR_TOT NPTFR_TOT@endlink, 
!> @link DECLARATIONS_ARTEMIS::NUMLIQ NUMLIQ@endlink, 
!> @link DECLARATIONS_ARTEMIS::OMEGA OMEGA@endlink, 
!> @link DECLARATIONS_ARTEMIS::OPTASS OPTASS@endlink, 
!> @link DECLARATIONS_ARTEMIS::PALE PALE@endlink, 
!> @link DECLARATIONS_ARTEMIS::PER PER@endlink, 
!> @link DECLARATIONS_ARTEMIS::PERDEB PERDEB@endlink, 
!> @link DECLARATIONS_ARTEMIS::PERFIN PERFIN@endlink, 
!> @link DECLARATIONS_ARTEMIS::PERPAS PERPAS@endlink, 
!> @link DECLARATIONS_ARTEMIS::PERPIC PERPIC@endlink, 
!> @link DECLARATIONS_ARTEMIS::PHAS PHAS@endlink, 
!> @link DECLARATIONS_ARTEMIS::PHII PHII@endlink, 
!> @link DECLARATIONS_ARTEMIS::PHIR PHIR@endlink, 
!> @link DECLARATIONS_ARTEMIS::PMAX PMAX@endlink, 
!> @link DECLARATIONS_ARTEMIS::PMIN PMIN@endlink, 
!> @link DECLARATIONS_ARTEMIS::PRIVE PRIVE@endlink, 
!> @link DECLARATIONS_ARTEMIS::PRODUC PRODUC@endlink, 
!> @link DECLARATIONS_ARTEMIS::QB QB@endlink, 
!> @link DECLARATIONS_ARTEMIS::S S@endlink, 
!> @link DECLARATIONS_ARTEMIS::SBID SBID@endlink, 
!> @link DECLARATIONS_ARTEMIS::SORIMP SORIMP@endlink, 
!> @link DECLARATIONS_ARTEMIS::SORLEO SORLEO@endlink, 
!> @link DECLARATIONS_ARTEMIS::SPHERI SPHERI@endlink, 
!> @link DECLARATIONS_ARTEMIS::STDGEO STDGEO@endlink, 
!> @link DECLARATIONS_ARTEMIS::SXX SXX@endlink, 
!> @link DECLARATIONS_ARTEMIS::SXY SXY@endlink, 
!> @link DECLARATIONS_ARTEMIS::SYY SYY@endlink, 
!> @link DECLARATIONS_ARTEMIS::T01 T01@endlink, 
!> @link DECLARATIONS_ARTEMIS::T02 T02@endlink, 
!> @link DECLARATIONS_ARTEMIS::T1 T1@endlink, 
!> @link DECLARATIONS_ARTEMIS::T2 T2@endlink, 
!> @link DECLARATIONS_ARTEMIS::T3 T3@endlink, 
!> @link DECLARATIONS_ARTEMIS::T4 T4@endlink, 
!> @link DECLARATIONS_ARTEMIS::TB TB@endlink, 
!> @link DECLARATIONS_ARTEMIS::TETAB TETAB@endlink, 
!> @link DECLARATIONS_ARTEMIS::TETAH TETAH@endlink, 
!> @link DECLARATIONS_ARTEMIS::TETMAX TETMAX@endlink, 
!> @link DECLARATIONS_ARTEMIS::TETMIN TETMIN@endlink, 
!> @link DECLARATIONS_ARTEMIS::TEXTE TEXTE@endlink, 
!> @link DECLARATIONS_ARTEMIS::TITCAS TITCAS@endlink, 
!> @link DECLARATIONS_ARTEMIS::TM TM@endlink, 
!> @link DECLARATIONS_ARTEMIS::U0 U0@endlink, 
!> @link DECLARATIONS_ARTEMIS::V0 V0@endlink, 
!> @link DECLARATIONS_ARTEMIS::VALID VALID@endlink, 
!> @link DECLARATIONS_ARTEMIS::VARSOR VARSOR@endlink, 
!> @link DECLARATIONS_ARTEMIS::X X@endlink, 
!> @link DECLARATIONS_ARTEMIS::XT XT@endlink, 
!> @link DECLARATIONS_ARTEMIS::Y Y@endlink, 
!> @link DECLARATIONS_ARTEMIS::YT YT@endlink, 
!> @link DECLARATIONS_ARTEMIS::ZF ZF@endlink<hr>
!> DECLARATIONS_TELEMAC :<br>
!> @link DECLARATIONS_TELEMAC::KLOG KLOG@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ALIRE, BID, FROVAR, HIST, I, IBID, ISTO, J, LAMBD0, LDIR, LISHHO, LPER, LT, NELBRD, NELBRX, NPERBA, NPFMAX, NVARCL, PROLIN, RADDEG, RESU, TRAC
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> BERKHO(), BIEF_DESIMP(), BIEF_VALIDA(), BORH(), BUILD_GLOBAL_BOUND(), CALCMN(), CALCTM(), CALRE2(), CALRES(), CONDIH(), CORFON(), CREATE_DATASET(), DIRALE(), DISMOY(), ENTART(), FONSTR(), FRONT2(), INBIEF(), LECLIM_ARTEMIS(), MASQUE_ARTEMIS(), OS(), PERALE(), PHBOR(), P_IMAX(), RADIA1(), RADIA2(), UTIMP(), WRITE_MESH()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>HOMERE_ARTEMIS()

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>  <tr>
!>    <td><center> 6.0                                    </center></td>
!>    <td> 21/06/2010                                              </td>
!>    <td> C. DENIS (SINETICS)                                     </td>
!>    <td> PARALLEL VERSION                                        </td>
!>  <tr>
!>    <td><center> 5.1                                    </center></td>
!>    <td> 21/04/1999                                              </td>
!>    <td> D. AELBRECHT (LNH) 01.30.87.74.12                       </td>
!>    <td>                                                         </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td>                                                         </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
!>    <td> LINKED TO BIEF 5.0                                      </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ARTEMIS
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
      USE GRACESTOP
C
C-----------------------------------------------------------------------
C DECLARES TYPES AND DIMENSIONS
C-----------------------------------------------------------------------
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C INTEGERS
C
      INTEGER LT,NPERBA,I,J
      INTEGER NELBRD,NPFMAX,NELBRX
      INTEGER LPER,LDIR
      INTEGER ALIRE(MAXVAR)
C
C VARIABLE FOR SUBROUTINE DISMOY
C
      INTEGER LISHHO
C
C REAL SCALARS
C
      DOUBLE PRECISION RADDEG,HIST(1)
C
C VARIABLES FOR CALLS TO TELEMAC-2D SUBROUTINES
C
      INTEGER NVARCL,ISTO
      DOUBLE PRECISION LAMBD0
      LOGICAL RESU,FROVAR,PROLIN,TRAC
C
C USED FOR DUMMY ARGUMENTS
C
      INTEGER IBID
      DOUBLE PRECISION BID


      INTEGER  P_IMAX,P_IMIN
      DOUBLE PRECISION P_DMIN
      EXTERNAL P_IMAX,P_IMIN,P_DMIN

C
      DATA HIST /9999.D0/
C
C-----------------------------------------------------------------------
C
C  VARIABLES TO READ IF COMPUTATION IS CONTINUED :
C  0 : DISCARD    1 : READ  (SEE SUBROUTINE NOMVAR)
C
      DATA ALIRE /1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
C
C-----------------------------------------------------------------------
C
      RADDEG = 180.D0/3.141592654D0
C
C=======================================================================
C
C : 1          READS, PREPARES AND CONTROLS THE DATA
C
C=======================================================================
C
C  TYPES OF DISCRETISATION:
C
C  TRIANGLES : P1
      IELM  = 11
C  SEGMENTS  : P1 FOR THE BOUNDARY
      IELMB = 1
C


C  MAXIMUM SIZE (CASE OF AN ADAPTIVE GRID)
C  THESE PARAMETERS ARE USED IN BIEF CALLS
C
C     NODES
      NPMAX = NPOIN
C     ELEMENTS
      NELMAX = NELEM
C     BOUNDARY ELEMENTS
      NELBRD = NPTFR
C     BOUNDARY ELEMENTS (MAXIMUM NUMBER)
      NPFMAX = NPTFR
C     BOUNDARY NODES
      NELBRX = NPTFR
C
      IF(BALAYE) THEN
        NPERBA = INT((PERFIN-PERDEB)/PERPAS) + 1
      ENDIF
C
C=======================================================================
C
      RESU   = .TRUE.
      FROVAR = .FALSE.
      PROLIN = .FALSE.
      SPHERI = .FALSE.
      TRAC   = .FALSE.
      NVARCL = 0
C
C IN TELEMAC-2D, LIHBOR = KINC IS AUTOMATICALLY CHANGED TO KSORT
C HAS TO MODIFY THE VALUE OF KINC FOR PREDA2, TO AVOID THIS AUTOMATIC CHANGE
C IN ADDITION, IN TELEMAC-2D, LIHBOR = KADH (NOT KNOWN HERE) GENERATES
C A MESSAGE. TO AVOID IT, ISTO IS ALSO USED IN PLACE OF KADH.
C
C
      ISTO = 100
C
C-----------------------------------------------------------------------
C
C READS THE BOUNDARY CONDITIONS AND INDICES FOR THE BOUNDARY NODES.
C
      CALL LECLIM_ARTEMIS
     &(LIHBOR%I,LIUBOR%I,MESH%NPTFR,MESH%NBOR%I,STDGEO,
     & ART_FILES(ARTCLI)%LU,
     & MESH%ISEG%I,MESH%XSEG%R,MESH%YSEG%R,MESH%NACHB%I,NUMLIQ%I,
     & MESH%IFAPAR%I)

C
C-----------------------------------------------------------------------
C
C COMPLEMENTS THE DATA STRUCTURE FOR BIEF
C
      CALL INBIEF(LIHBOR%I,KLOG,IT1,IT2,IT3,LVMAC,IELM,
     &         LAMBD0,SPHERI,MESH,T1,T2,OPTASS,PRODUC,EQUA)

      IF (NCSIZE .LE. 1) THEN
         NPOIN_TOT=MESH%NPOIN
         ALLOCATE(XT(NPOIN_TOT))
         ALLOCATE(YT(NPOIN_TOT))
      ENDIF
C-----------------------------------------------------------------------
C  LOOKS FOR BOTTOM AND BOTTOM FRICTION IN THE GEOMETRY FILE :
C-----------------------------------------------------------------------
C
      CALL FONSTR(T1,ZF,T2,FW,ART_FILES(ARTGEO)%LU,ART_FILES(ARTFON)%LU,
     &            ART_FILES(ARTFON)%NAME,MESH,FFON,LISTIN)

C-----------------------------------------------------------------------
C
C PREPARES THE RESULTS FILE (OPTIONAL)
C
C     STANDARD SELAFIN FORMAT
C
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


C-----------------------------------------------------------------------
C
C     INITIALISES PRIVE
C
      IF(NPRIV.GT.0) CALL OS('X=C     ',PRIVE,PRIVE,PRIVE,0.D0)
C
C=======================================================================
C
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
C LOCATES THE BOUNDARIES
C

C=======================================================================
C
C CORRECTS THE VALUES OF THE BOTTOM (OPTIONAL)
C
C STANDARD SUBROUTINE DOES NOT DO ANYTHING
C
      CALL CORFON


C
C=======================================================================
C
C INITIALISES THE WAVE HEIGHT FOR RANDOM SEAS AT 0.
C
      IF (ALEMON .OR. ALEMUL) THEN
       CALL OS('X=C     ', HALE , SBID , SBID , 0.D0 )
      ENDIF


C
C DETERMINES THE DIFFERENT PERIODS FOR A RANDOM SEA COMPUTATION
C
      IF (ALEMON.OR.ALEMUL) THEN
         CALL PERALE(PALE%R,GAMMA,PERPIC,NPALE,T1%R,NPOIN,PRIVE,
     &               NPRIV,PMIN,PMAX)
         PER = PALE%R(1)
      ENDIF




C DETERMINES THE DIFFERENT DIRECTIONS FOR A MULTIDIRECTIONAL RANDOM
C SEA COMPUTATION
C
      IF (ALEMUL) THEN
         CALL DIRALE(DALE%R,EXPOS,TETAH,TETMIN,TETMAX,NDALE,
     &               T1%R,NPOIN,PRIVE,NPRIV)
      ENDIF




C
C=======================================================================
C
C START OF COMPUTATION
C
C LT REFERS TO THE CURRENT TIME STEP (STARTS FROM 0 SO THAT
C THE FIRST COMPUTATION ALWAYS BE RECORDED)
C
      LT = 0
C
C INITIALISES QB, T01, T02 AND TM : SET TO 0 AT THE START OF COMPUTATION
C
      CALL OS('X=C     ', QB , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', T01 , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', T02 , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', TM , SBID , SBID , 0.D0 )
C
C
C INITIALISES RADIATION STRESSES AND
C FORCINGS
C
      CALL OS('X=C     ', FX , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', FY , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', SXX , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', SXY , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', SYY , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', MCOS , SBID , SBID , 0.D0 )
      CALL OS('X=C     ', MSIN , SBID , SBID , 0.D0 )
C
C FOR A RANDOM SEA COMPUTATION, LPER AND LDIR REFER TO THE COMPUTED
C PERIOD AND DIRECTION
C
      LPER = 1
      LDIR = 1
C
100   CONTINUE
C
      IF (BALAYE) THEN
         CALL ENTART(1,PER,LT,LPER,NPERBA,ALEMON,ALEMUL,BALAYE)
      ELSE
         CALL ENTART(1,PER,LT,LPER,NPALE,ALEMON,ALEMUL,BALAYE)
      ENDIF
C
C=======================================================================
C
C : 2                  INITIALISES
C
C=======================================================================
C
C INITIALISES PHYSICAL PARAMETERS
C


      CALL CONDIH
C
C=======================================================================
C
C : 3                  BOUNDARY CONDITIONS
C
C=======================================================================
C
C CALLS THE USER SUBROUTINE
C


!
      IF (NCSIZE .GT. 1) THEN
         CALL BUILD_GLOBAL_BOUND(MESH%KNOLG%I,MESH%NPOIN,NPOIN_TOT,
     &        MESH%NPTFR,NPTFR_TOT,
     &        X,Y,K%R,C%R,CG%R,LIHBOR%I,XT,
     &        YT,KT,CTT,CGT,LIHBORT,MESH%NBOR%I,NBOR_TOT)
         ELSE

            DO I=1,NPOIN
               XT(I)=X(I)
               YT(I)=Y(I)
            END DO
            DO I=1,NPTFR
               NBOR_TOT(I)=MESH%NBOR%I(I)
               LIHBORT(I)=LIHBOR%I(I)
            END DO
      END IF

      CALL BORH
C
C MASKING FOR THE BOUNDARY CONDITIONS
C

C IN MULTIDIRECTIONAL RANDOM SEA, THE DIRECTIONS OF PROPAGATION
C (AT THE BOUNDARY) HAVE BEEN CALCULATED IN DALE.
C
200   IF (ALEMUL) THEN
         CALL OS('X=C     ', TETAB ,SBID,SBID, DALE%R(LDIR) )
         CALL ENTART(2,DALE%R(LDIR),LT,LDIR,NDALE,ALEMON,ALEMUL,BALAYE)
      ENDIF
C
C CALCULATES THE BOUNDARY CONDITIONS ON THE POTENTIAL FROM USER INPUT.
C
C
C      IF (LT .EQ. 0) THEN

      CALL MASQUE_ARTEMIS


      CALL PHBOR
C      END IF
C
C=======================================================================
C
C : 4                  SOLVES THE BERKHOFF EQUATION
C
C=======================================================================
C
      CALL BERKHO (LT)
C
C=======================================================================
C
C : 5.1        COMPUTES SPEED, FREE SURFACE ELEVATION,
C              WAVE HEIGHT AND PHASE
C
C=======================================================================
C
      CALL CALRES
C
      IF (ALEMON .OR. ALEMUL) THEN
C
C        CUMULATIVELY COMPUTES THE M1, M2, AND MT1 MOMENTUMS
C        STORED UNTIL THE LAST COMPUTATION IN T01, T02, AND TM
C
C
         CALL CALCMN
C
      ENDIF
C
C
C=======================================================================
C
C : 5.2        COMPUTES RADIATION STRESSES AND
C              DRIVING FORCES FOR REGULAR WAVES.
C
C=======================================================================
C
      IF (.NOT.ALEMON .AND. .NOT.ALEMUL) THEN
C
       IF (LISHOU) THEN
         CALL DISMOY
     &   (NPOIN,NELEM,MESH%X%R,MESH%Y%R,MESH%IKLE%I,K%R,LISHHO)
       ELSE
         LISHHO = 0
       ENDIF
C
         CALL RADIA1 (LISHHO)
C
      ELSE
         LISHHO = 0
      ENDIF

C=======================================================================
C
C : 6   CALLS A USER SUBROUTINE FOR PRINT OUTS, ANALYTICAL SOLUTIONS...
C       (STANDARD SUBROUTINE DOES NOT DO ANYTHING)
C
C=======================================================================
C
      CALL UTIMP
     &(PHIR%R,PHII%R,C%R,CG%R,K%R,MESH%X%R,MESH%Y%R,ZF%R,H%R,
     & HHO%R,U0%R,V0%R,PHAS%R,S%R,T1%R,T2%R,T3%R,T4%R,INCI%R,
     & GRAV,PER,OMEGA,MESH%IKLE%I,MESH%NBOR%I,MESH%KP1BOR%I,
     & NELEM,NELMAX,IELM,IELMB,NPTFR,NPOIN,PRIVE)
C
C=======================================================================
C
C : 7                  PRINTS OUT THE RESULTS
C
C=======================================================================
C
C
C FOR RANDOM SEAS,
C OUTPUTS ONLY AT THE PEAK PERIOD
C
      IF (.NOT.ALEMON .AND. .NOT.ALEMUL) THEN
C
C=======================================================================
C
C     CONVERTS INCI INTO DEGREES
C
C=======================================================================
C
         CALL OS('X=CX    ', INCI , SBID , SBID , RADDEG )
C
C RUBENS FILE
C
         CALL BIEF_DESIMP(ART_FILES(ARTRES)%FMT,VARSOR,
     &            HIST,0,NPOIN,ART_FILES(ARTRES)%LU,'STD',PER,0,
     &            LISPRD,LEOPRD,
     &            SORLEO,SORIMP,MAXVAR,TEXTE,0,0)
C
C=======================================================================
C
C              COMPARISON AGAINST A REFERENCE FILE
C
C=======================================================================
C
C     THE VALIDA SUBROUTINE FROM THE BIEF LIBRARY IS STANDARD.
C     IT CAN BE MODIFIED BY THE USER FOR THEIR PARTICULAR CASE.
C     BUT THE CALL TO THE SUBROUTINE MUST STAY IN THE TIME LOOP.
C
         IF(VALID) THEN
           CALL BIEF_VALIDA(TB,TEXTE,
     &                      ART_FILES(ARTREF)%LU,ART_FILES(ARTREF)%FMT,
     &                      VARSOR,TEXTE,
     &                      ART_FILES(ARTRES)%LU,ART_FILES(ARTRES)%FMT,
     &                      MAXVAR,NPOIN,LT,LT,ALIRE)
         ENDIF
C
      ENDIF
C
C=======================================================================
C
C : 8                  GOES TO NEXT PERIOD
C
C=======================================================================
C
C IF SWEEPS A RANGE OF PERIODS
C
      IF (BALAYE) THEN
         LT   = LT  + 1
         LPER = LPER + 1
         PER  = PER + PERPAS

         IF (PER.LE.PERFIN) GOTO 100
      ENDIF
C
C
C=======================================================================
C
C IF RANDOM SEAS
C
C=======================================================================
C
      IF (ALEMON .OR. ALEMUL) THEN
C
         LT  = LT  + 1
C
         IF (LT.LT.NPALE*NDALE) THEN
C
C           REACTUALISES THE ENERGY OF THE RANDOM SEA
            CALL OS('X=X+CYZ ',HALE,HHO,HHO,1.D0/DBLE(NPALE*NDALE))
C
C           GOES TO NEXT DIRECTION
            LDIR = LDIR + 1
            IF (LDIR.LE.NDALE) GOTO 200
C
C           GOES TO NEXT PERIOD
            LDIR = 1
            LPER = LPER + 1
            PER = PALE%R(LPER)
            GOTO 100
C
         ELSE
C
C           LAST COMPUTATION: DETERMINES THE MEAN PERIODS
C           (T01 AND T02), AND THE MEAN DIRECTION (INCI)
C
C
            CALL CALCTM
C
C           DETERMINES MEAN K, C AND CG
C
            CALL CALRE2
C
C           TAKES INTO ACCOUNT THE LAST WAVE HEIGHT
C           FOR RANDOM SEAS
C
            CALL OS('X=X+CYZ ',HALE,HHO,HHO,1.D0/DBLE(NPALE*NDALE))
            CALL OS('X=SQR(Y)', HALE , HALE , SBID , BID )
            CALL OS('X=CX    ',QB,SBID,SBID,1.D0/DBLE(NPALE*NDALE))
C
C=======================================================================
C
C           COMPUTES RADIATION STRESSES
C           AND DRIVING FORCES FOR RANDOM SEAS
C
C=======================================================================
C
            CALL RADIA2 (LISHHO)
C
C=======================================================================
C
C          CONVERTS INCI INTO DEGREES
C
C=======================================================================
C
            CALL OS('X=CX    ', INCI , SBID , SBID , RADDEG )
C
C=======================================================================
C
C           RUBENS FILE
C
C=======================================================================
C
            CALL BIEF_DESIMP(ART_FILES(ARTRES)%FMT,VARSOR,
     &            HIST,0,NPOIN,ART_FILES(ARTRES)%LU,'STD',PERPIC,0,
     &            LISPRD,LEOPRD,
     &            SORLEO,SORIMP,MAXVAR,TEXTE,0,0)
C
C=======================================================================
C
C              COMPARISON AGAINST A REFERENCE FILE
C
C=======================================================================
C


C     THE VALIDA SUBROUTINE FROM THE BIEF LIBRARY IS STANDARD.
C     IT CAN BE MODIFIED BY THE USER FOR THEIR PARTICULAR CASE.
C     BUT THE CALL TO THE SUBROUTINE MUST STAY IN THE TIME LOOP.
C

            IF(VALID) THEN
              CALL BIEF_VALIDA(TB,TEXTE,
     &                       ART_FILES(ARTREF)%LU,ART_FILES(ARTREF)%FMT,
     &                       VARSOR,TEXTE,
     &                       ART_FILES(ARTRES)%LU,ART_FILES(ARTRES)%FMT,
     &                       MAXVAR,NPOIN,LT,LT,ALIRE)
            ENDIF
C
         ENDIF
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C