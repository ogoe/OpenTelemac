
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SOLVES THE SAINT-VENANT EQUATIONS FOR U,V,H.
!><br>            ADJO = .TRUE.  : DIRECT MODE
!><br>            ADJO = .FALSE. : ADJOINT MODE

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
!>      <tr>
!>      <td><center> 6.0                                       </center>
!> </td><td> 25/11/2009
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td> VERSION WITH MULTIPLE TRACERS
!> <br>      DOUBLE CALL TO SISYPHE (BEDLOAD + SUSPENSION)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 22/07/2009
!> </td><td>
!> </td><td> 3 NEW ARGUMENTS IN PROPAG
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 20/07/2009
!> </td><td>
!> </td><td> 1 OUT OF 3 CALLS TO TEL4DEL REMOVED (THANKS TO A
!>           MODIFICATION OF PROPAG: COMPUTATION OF UDEL AND VDEL
!>           IF(SOLSYS.EQ.1)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 09/07/2009
!> </td><td>
!> </td><td> ARGUMENT NPTFR2 ADDED TO LECLIM
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 02/04/2009
!> </td><td>
!> </td><td> NEW FILE STRUCTURE T2D_FILES AND MED FORMAT
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 19/02/2009
!> </td><td>
!> </td><td> H CLIPPED IN CASE OF COMPUTATION CONTINUED
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 16/02/2009
!> </td><td>
!> </td><td> CALL TO POSITIVE_DEPTHS
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 09/02/2009
!> </td><td>
!> </td><td> IF H CLIPPED, USES HMIN INSTEAD OF 0.D0
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 21/10/2008
!> </td><td>
!> </td><td> CALL TO MODIFIED MASKTO (PARALLEL VERSION OF MASKTO)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 25/09/2008
!> </td><td>
!> </td><td> CALL TO MODIFIED TEL4DEL (FLUXES SENT THRU MESH%W%R)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 02/09/2008
!> </td><td>
!> </td><td> CALL TO MODIFIED TEL4DEL (ADDED VELOCITY AND DIFFUSION)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 20/08/2008
!> </td><td>
!> </td><td> LIST_PTS MODIFIED IN PARALLEL
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 13/08/2008
!> </td><td>
!> </td><td> CHANGED CALL AND CALL CONDITIONS TO CHARAC
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 29/07/2008
!> </td><td>
!> </td><td> ADDED CALL TO FLUSEC BEFORE THE 1ST CALL PRERES
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 27/06/2008
!> </td><td>
!> </td><td> ARGUMENTS OF PROPIN_TELEMAC2D : MESH ADDED TO THE END
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 25/06/2008
!> </td><td>
!> </td><td> DIFFIN2 RENAMED DIFFIN + ARGUMENT MESH
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 16/06/2008
!> </td><td>
!> </td><td> SECOND CALL TO PROPIN FOLLOWING CALL TO BORD
!>          (USER CHANGES THE CONDITIONS IN BORD)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 06/06/2008
!> </td><td>
!> </td><td> OPTIONAL ARGUMENT BOUNDARY_COLOUR ADDED TO LECLIM
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 20/05/2008
!> </td><td>
!> </td><td> FLUXES DUE TO SMOOTHING OF NEGATIVE VALUES (NEW FILTER_H) PASSED TO TEL4DEL
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 14/05/2008
!> </td><td>
!> </td><td> HN INITIALISED BEFORE CALL TO SISYPHE
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 05/05/2008
!> </td><td>
!> </td><td> USIS AND VSIS IN CALL TO SISYPHE
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 07/04/2008
!> </td><td>
!> </td><td> SOURCES VARY IN TIME
!> </td></tr>
!>  </table>

C
C#######################################################################
C
                        SUBROUTINE TELEMAC2D
     &(PASS,ATDEP,NITER,CODE,DTDEP,NEWTIME,DOPRINT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ATDEP          |-->| STARTING TIME WHEN CALLED FOR COUPLING
C| CODE           |-->| CALLING PROGRAM (IF COUPLING)
C| DOPRINT        |---| 
C| DTDEP          |---| 
C| NEWTIME        |---| 
C| NITER          |-->| NUMBER OF ITERATIONS WHEN CALLED FOR COUPLING
C| PASS           |-->| -1 : ALL STEPS
C|                |   | 0 : ONLY INITIALISATION
C|                |   | 1 : ONLY TIME-STEPS STEPS
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D, EX_TELEMAC2D => TELEMAC2D
      USE INTERFACE_SISYPHE, ONLY: SISYPHE
      USE INTERFACE_TOMAWAC, ONLY: WAC
      USE GRACESTOP
      USE FRICTION_DEF
C     MODULE SPECIFIC TO COUPLING WITH ESTEL-3D
      USE M_COUPLING_ESTEL3D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER,          INTENT(IN) :: PASS,NITER
      DOUBLE PRECISION, INTENT(IN) :: ATDEP
      CHARACTER(LEN=*), INTENT(IN) :: CODE
C     TIME STEP TO USE WHEN COUPLING WITH ESTEL-3D
      DOUBLE PRECISION, INTENT(IN), OPTIONAL :: DTDEP
C     ARE WE STARTING A NEW TIME STEP OR JUST ITERATING?
      LOGICAL,          INTENT(IN), OPTIONAL :: NEWTIME
C     DO WE WANT TELEMAC2D TO OUTPUT IN THE LISTING OR NOT?
      LOGICAL,          INTENT(IN), OPTIONAL :: DOPRINT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C INTEGERS
C
      INTEGER IELM,I,IELMX,ISOUSI,IBID,STOP2,LEOPRD_CHARR
      INTEGER ALIRE(MAXVAR),TROUVE(MAXVAR+10)
C
C REAL SCALARS
C
      DOUBLE PRECISION KMIN,KMAX,KARMAN,FLUSOR,FLUENT,HIST(1),AT0
      DOUBLE PRECISION C,MASSES,RELAXS,CFLMAX,TETAHC,DTCAS,RELAX
      DOUBLE PRECISION EMAX,EMIN,SCHMIT,ESTAR,SIGMAE,SIGMAK,C2,C1,CMU
C
C FOR TRACERS
C
C     MASSOU: MASS CREATED BY SOURCE TERM DURING THE TIME STEP
C     MASTR0: INITIAL MASS
C     MASTR2: CURRENT MASS
C     MASTEN: MASS ENTERED THROUGH BOUNDARIES
C     MASTOU: TOTAL MASS CREATED BY SOURCE TERM
      DOUBLE PRECISION MASSOU(MAXTRA),MASTR0(MAXTRA),MASTR2(MAXTRA)
      DOUBLE PRECISION MASTEN(MAXTRA),MASTOU(MAXTRA)
C
C LOGICALS
C
      LOGICAL AKEP,INFOGS,INFOGT,ARRET1,ARRET2,YASMH,ARRET3,CORBOT
      LOGICAL CHARR,SUSP,SUSP1,NON,INIFLOW,YAFLODEL,YAFLULIM
      LOGICAL YASMI(MAXTRA)
C
      CHARACTER(LEN=24), PARAMETER :: CODE1='TELEMAC2D               '
      CHARACTER(LEN=16) :: FORMUL
C
C-----------------------------------------------------------------------
C
      INTEGER IOPTAN,IMAX,ITRAC,NPTFR2
C
C-----------------------------------------------------------------------
C
C ADDED FOR KINETIC SCHEMES
C
      DOUBLE PRECISION FLUTSOR(MAXTRA),FLUTENT(MAXTRA),DTN
      DOUBLE PRECISION FLUSORTN,FLUENTN,TMAX,DTT
      INTEGER LTT
C
C-----------------------------------------------------------------------
C
C     FOR SISYPHE : GRAIN FEEDING AND CONSTANT FLOW DISCHARGE
      INTEGER :: ISIS_CFD, NSIS_CFD
      LOGICAL :: SISYPHE_CFD, CONSTFLOW_SIS
C     FRICTION DATA
      INTEGER :: KFROT_TP
C
      INTEGER  P_IMAX,P_IMIN
      DOUBLE PRECISION P_DMIN
      EXTERNAL P_IMAX,P_IMIN,P_DMIN
C
C-----------------------------------------------------------------------
C
      INTRINSIC MAX
C
C-----------------------------------------------------------------------
C
      DATA HIST /9999.D0/
C
C-----------------------------------------------------------------------
C
C  VARIABLES TO READ IN THE EVENT OF A CONTINUATION:
C  0 : DISCARD    1 : READ  (SEE SS-PG NOMVAR)
C
C                                 0: OLD PLACE FOR THE TRACER
      DATA ALIRE /1,1,0,1,1,1,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
     &            0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0/
C
C-----------------------------------------------------------------------
C
C     ADVECTION FIELD USED FOR SISYPHE CALL
C
      TYPE(BIEF_OBJ), POINTER :: USIS,VSIS
C
C-----------------------------------------------------------------------
C
      SAVE
C
C-----------------------------------------------------------------------
C
      NON=.FALSE.
      CHARR=.FALSE.
      SUSP=.FALSE.
C
C-----------------------------------------------------------------------
C
C     FOR INITIALISATION OF FLODEL (ARRAY FLOW) IN DELWAQ
C
      IF(OPTBAN.EQ.1.OR.OPTBAN.EQ.3) THEN
        INIFLOW=.FALSE.
      ELSE
        INIFLOW=.TRUE.
      ENDIF
C
C     FOR COMPUTING EXTRA FLOWS DUE TO TIDAL FLATS TREATMENT
C
      IF(INCLUS(COUPLING,'DELWAQ')) THEN
        YAFLODEL=.TRUE.
      ELSE
        YAFLODEL=.FALSE.
      ENDIF
C
C     FOR TAKING INTO ACCOUNT FLUX LIMITATION OF ARRAY FLULIM IN ADVECTION
C     SCHEMES (SO FAR ONLY FOR TRACERS IN CASE SOLSYS=2 AND OPT_HNEG=2).
C
      YAFLULIM=.FALSE.
C
C     FOR READING TRACERS IN SELAFIN FILES
C
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
C         SEE POINT_TELEMAC2D
          ALIRE(31+ITRAC) = 1
        ENDDO
      ENDIF
C
C     FOR AVOIDING READING K, EPSILON AND DIFFUSION WHEN NOT RELEVANT
C
      IF(ITURB.NE.3) ALIRE(10) = 0
      IF(ITURB.NE.3) ALIRE(11) = 0
      IF(ITURB.EQ.1) ALIRE(12) = 0
C
C-----------------------------------------------------------------------
C
C     USE DOPRINT TO LIMIT TELEMAC-2D OUTPUTS IN THE LISTING
C
      IF(PRESENT(DOPRINT)) THEN
        LISTIN =  DOPRINT
        ENTET  =  DOPRINT
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(PASS.EQ.0) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'INITIALISATION DE TELEMAC2D POUR ',CODE
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'INITIALISING TELEMAC2D FOR ',CODE
        ENDIF
      ELSEIF(PASS.EQ.1) THEN
        GO TO 700
      ELSEIF(PASS.NE.-1) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'MAUVAIS ARGUMENT PASS : ',PASS
        IF(LNG.EQ.2) WRITE(LU,*) 'WRONG ARGUMENT PASS: ',PASS
        CALL PLANTE(1)
        STOP
      ENDIF
C
C=======================================================================
C
C : 1          READS, PREPARES AND CONTROLS THE DATA
C
C=======================================================================
C
C  TYPES OF DISCRETISATION: P1 TRIANGLES FOR NOW
C
      IELM=IELM1
C     THE MOST COMPLEX ELEMENT
      IELMX = MAX(IELMH,IELMU,IELMT,IELMK,IELME)
C
C-----------------------------------------------------------------------
C
C READS THE BOUNDARY CONDITIONS AND INDICES OF THE BOUNDARY POINTS
C
      IF(IELMX.EQ.13) THEN
        NPTFR2=2*NPTFR
      ELSE
        NPTFR2=NPTFR
      ENDIF
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE LECLIM'
      CALL LECLIM (LIHBOR%I   , LIUBOR%I , LIVBOR%I , LITBOR%ADR(1)%P%I,
     &             HBOR%R     , UBOR%R   , VBOR%R   , TBOR%ADR(1)%P%R ,
     &             CHBORD%R    , ATBOR%ADR(1)%P%R   , BTBOR%ADR(1)%P%R ,
     &             MESH%NPTFR , 3        ,NTRAC.GT.0,
     &             T2D_FILES(T2DCLI)%LU,
     &             KENT       , KENTU    , KSORT ,  KADH , KLOG , KINC,
     &             NUMLIQ%I   ,MESH,BOUNDARY_COLOUR%I,NPTFR2)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE LECLIM'
C
C DUPLICATES THE BOUNDARY CONDITIONS FOR THE TRACERS
C
      IF(NTRAC.GE.2) THEN
        DO ITRAC=2,NTRAC
          DO I=1,NPTFR
            LITBOR%ADR(ITRAC)%P%I(I)=LITBOR%ADR(1)%P%I(I)
              TBOR%ADR(ITRAC)%P%R(I)=  TBOR%ADR(1)%P%R(I)
             ATBOR%ADR(ITRAC)%P%R(I)= ATBOR%ADR(1)%P%R(I)
             BTBOR%ADR(ITRAC)%P%R(I)= BTBOR%ADR(1)%P%R(I)
          ENDDO
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C  COMPLEMENT OF THE DATA STRUCTURE FOR BIEF
C-----------------------------------------------------------------------
C
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE INBIEF'
      CALL INBIEF(LIHBOR%I,KLOG,IT1,IT2,IT3,LVMAC,IELMX,
     &            LAMBD0,SPHERI,MESH,T1,T2,OPTASS,PRODUC,EQUA)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE INBIEF'
C
      IF(IELMX.EQ.13) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE COMPLIM'
        CALL COMPLIM( LIUBOR%I , LIVBOR%I , LITBOR%ADR(1)%P%I,
     &                UBOR%R   , VBOR%R   , TBOR%ADR(1)%P%R ,
     &                CHBORD%R , ATBOR%ADR(1)%P%R , BTBOR%ADR(1)%P%R ,
     &                MESH%NBOR%I,MESH%NPTFR , MESH%NPOIN, NTRAC.GT.0,
     &                KENT , KENTU , KSORT ,KADH , KLOG , KINC,
     &                IELMU,IELMU,IELMT,MESH)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE COMPLIM'
      ENDIF
C
C-----------------------------------------------------------------------
C  DEFINITION OF ZONES BY THE USER
C-----------------------------------------------------------------------
C
      IF(DEFZON) CALL DEF_ZONES
C
C-----------------------------------------------------------------------
C  CHANGES FROM GLOBAL TO LOCAL IN LIST OF POINTS IN PARALLEL
C-----------------------------------------------------------------------
C
      IF(NPTS.GT.0.AND.NCSIZE.GT.0) THEN
        DO I=1,NPTS
          LIST_PTS(I)=MESH%KNOGL%I(LIST_PTS(I))
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C  LOOKS FOR VARIABLES BOTTOM AND BOTTOM FRICTION IN THE GEOMETRY FILE:
C-----------------------------------------------------------------------
C
      IF(     .NOT.INCLU2(ESTIME,'FROTTEMENT')
     &   .AND..NOT.INCLU2(ESTIME,'FRICTION'  )  ) THEN
C       NO PARAMETER ESTIMATION
        CALL FONSTR(T1,ZF,T2,CHESTR,T2D_FILES(T2DGEO)%LU,
     &              T2D_FILES(T2DFON)%LU,T2D_FILES(T2DFON)%NAME,
     &              MESH,FFON,LISTIN)
        CORBOT=.TRUE.
      ELSEIF(NITERA.EQ.1.AND..NOT.ADJO) THEN
C       WITH PARAMETER ESTIMATION (HENCE NITERA DEFINED),
C       FONSTR CALLED ONCE TO GET
C       THE BOTTOM TOPOGRAPHY AND THE INITIAL FRICTION (CALL TO STRCHE)
        CALL FONSTR(T1,ZF,T2,CHESTR,T2D_FILES(T2DGEO)%LU,
     &              T2D_FILES(T2DFON)%LU,T2D_FILES(T2DFON)%NAME,
     &              MESH,FFON,LISTIN)
C       IF OPTID=0, VALUES OF SETSTR ARE GIVEN BY FILE, MUST NOT BE ERASED
        IF(OPTID.NE.0) CALL INITSTR(CHESTR,SETSTR,ZONE%I,NZONE,NPOIN,T1)
        CALL ASSIGNSTR(CHESTR,SETSTR,ZONE%I,NZONE,NPOIN)
        CORBOT=.TRUE.
      ELSE
C       IN PARAMETER ESTIMATION, FROM NITERA=2 ON, BOTTOM IS NOT READ
C       AGAIN, SO NO CALL TO CORFON
        CORBOT=.FALSE.
      ENDIF
C
C     INITIALISES FRICTION COEFFICIENT BY ZONE
C
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE FRICTION_CHOICE'
      CALL FRICTION_CHOICE(0,KARMAN)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE FRICTION_CHOICE'
C
C-----------------------------------------------------------------------
C
C PREPARES THE RESULTS FILE (OPTIONAL)
C
C      STANDARD SELAFIN
C
      IF(ADJO) THEN
C
        IF(T2D_FILES(T2DRBI)%NAME.NE.' '.AND.
     &     INCLU2(ESTIME,'DEBUG')) THEN
         CALL ECRGEO(MESH%X%R,MESH%Y%R,MESH%NPOIN,MESH%NBOR%I,
     &               T2D_FILES(T2DRBI)%LU,IBID,TEXTE,VARCLA,NVARCL,
     &               TITCAS,SORLEOA,MAXVAR,MESH%IKLE%I,
     &               MESH%NELEM,MESH%NPTFR,3,MARDAT,MARTIM,
     &               NCSIZE,NPTIR,MESH%KNOLG%I,I3=I_ORIG,I4=J_ORIG)
        ENDIF
C
      ELSE
C
C       CALL ECRGEO(MESH%X%R,MESH%Y%R,MESH%NPOIN,MESH%NBOR%I,
C    *            NRES,IBID,TEXTE,VARCLA,NVARCL,
C    *            TITCAS,SORLEO,MAXVAR,MESH%IKLE%I,
C    *            MESH%NELEM,MESH%NPTFR,3,MARDAT,MARTIM,
C    *            NCSIZE,NPTIR,MESH%KNOLG%I,I3=I_ORIG,I4=J_ORIG)
        ! CREATES THE DATA FILE USING A GIVEN FILE FORMAT:
        ! FORMAT_RES.
        ! THE DATA ARE CREATED IN THE FILE: NRES, AND ARE
        ! CHARACTERISED BY A TITLE AND NAME OF OUTPUT VARIABLES.
        CALL CREATE_DATASET(T2D_FILES(T2DRES)%FMT, ! RESULTS FILE FORMAT
     &                      T2D_FILES(T2DRES)%LU,  ! LU FOR RESULTS FILE
     &                      TITCAS,     ! TITLE
     &                      MAXVAR,     ! MAX NUMBER OF OUTPUT VARIABLES
     &                      TEXTE,      ! NAMES OF OUTPUT VARIABLES
     &                      SORLEO)     ! PRINT TO FILE OR NOT
        ! WRITES THE MESH IN THE OUTPUT FILE :
        ! IN PARALLEL, REQUIRES NCSIZE AND NPTIR.
        ! THE REST OF THE INFORMATION IS IN MESH.
        ! ALSO WRITES : START DATE/TIME AND COORDINATES OF THE
        ! ORIGIN.
        CALL WRITE_MESH(T2D_FILES(T2DRES)%FMT, ! RESULTS FILE FORMAT
     &                  T2D_FILES(T2DRES)%LU,  ! LU FOR RESULTS FILE
     &                  MESH,          ! CHARACTERISES MESH
     &                  1,             ! NUMBER OF PLANES /NA/
     &                  MARDAT,        ! START DATE
     &                  MARTIM,        ! START TIME
     &                  I_ORIG,J_ORIG) ! COORDINATES OF THE ORIGIN.
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C     INITIALISES PRIVE
C
      IF(NPRIV.GT.0) CALL OS('X=0     ',X=PRIVE)
C
C  ENLARGES COSLAT AND SINLAT TO GIVE THEM THE DIMENSION OF U AND V
C  SAME THING FOR FRICTION
C
      IF(IELMU.NE.IELM1) THEN
        IF(SPHERI) CALL CHGDIS(MESH%COSLAT,IELM1,IELMU,MESH)
        IF(SPHERI) CALL CHGDIS(MESH%SINLAT,IELM1,IELMU,MESH)
        CALL CHGDIS(CHESTR,IELM1,IELMU,MESH)
      ENDIF
C
C=======================================================================
C
C  LOCATES THE BOUNDARIES
C
      IF(NCSIZE.GT.1) THEN
       NFRLIQ=0
       DO I=1,NPTFR
         NFRLIQ=MAX(NFRLIQ,NUMLIQ%I(I))
       ENDDO
       NFRLIQ=P_IMAX(NFRLIQ)
       WRITE(LU,*) ' '
       IF(LNG.EQ.1) WRITE(LU,*) 'NOMBRE DE FRONTIERES LIQUIDES :',NFRLIQ
       IF(LNG.EQ.2) WRITE(LU,*) 'NUMBER OF LIQUID BOUNDARIES:',NFRLIQ
      ELSE
       IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE FRONT2'
       CALL FRONT2(NFRLIQ,NFRSOL,DEBLIQ,FINLIQ,DEBSOL,FINSOL,
     &             LIHBOR%I,LIUBOR%I,
     &             MESH%X%R,MESH%Y%R,MESH%NBOR%I,MESH%KP1BOR%I,
     &             IT1%I,NPOIN,NPTFR,KLOG,LISTIN,NUMLIQ%I,MAXFRO)
       IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE FRONT2'
      ENDIF
C
C=======================================================================
C
C  READS THE FILE WITH STAGE-DISCHARGE CURVES
C
      IF(T2D_FILES(T2DMAB)%NAME(1:1).NE.' ') THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE READ_FIC_CURVES'
        CALL READ_FIC_CURVES(T2D_FILES(T2DMAB)%LU,NFRLIQ,
     &                       STA_DIS_CURVES,PTS_CURVES)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE READ_FIC_CURVES'
      ENDIF
C
C=======================================================================
C
C CORRECTS THE NORMALS TO THE BOUNDARY NODES TO HAVE NORMALS TO
C ADJACENT LIQUID SEGMENT IN THE CASE OF A TRANSITION FROM LIQUID TO SOLID
C
      IF(EQUA(1:15).NE.'SAINT-VENANT VF') THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CORNOR'
        CALL CORNOR(MESH%XNEBOR%R,MESH%YNEBOR%R,
     &              MESH%XSGBOR%R,MESH%YSGBOR%R,
     &              MESH%KP1BOR%I,NPTFR,KLOG,LIHBOR%I,
     &              T1,T2,MESH)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CORNOR'
      ENDIF
C
C=======================================================================
C
C FILLS IN MASKEL BY DEFAULT
C (ALL THE ELEMENTS ARE TO BE CONSIDERED)
C
      IF(MSK) CALL OS ( 'X=C     ' , MASKEL , S , S , 1.D0 )
C
C     USER CHOOSES TO HIDE SOME OF THE ELEMENTS
C     THIS SUBROUTINE IS ALSO CALLED AT EVERY TIME STEP
      IF(MSKUSE) THEN
        CALL MASKOB (MASKEL%R,MESH%X%R,MESH%Y%R,
     &               IKLE%I,NELEM,NELMAX,NPOIN,0.D0,0)
      ENDIF
C
C-----------------------------------------------------------------------
C  INTEGRAL OF TEST FUNCTIONS (ONCE FOR ALL AND WITHOUT MASKING)
C-----------------------------------------------------------------------
C
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE MASBAS2D'
      CALL MASBAS2D(VOLU2D,V2DPAR,UNSV2D,IELM1,MESH,.FALSE.,
     &              MASKEL,T2,T2)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE MASBAS2D'
C
C=======================================================================
C
C CORRECTS THE BOTTOM WITH USER-SUBROUTINE CORFON
C ZF IS TREATED AS LINEAR IN CORFON
C IF(CORBOT) : SEE CALL FONSTR ABOVE, IN PARAMETER ESTIMATION,
C ZF IS READ ONLY AT THE FIRST RUN
C
      IF(CORBOT) THEN
        IF(IELMH.NE.IELM1) CALL CHGDIS(ZF,IELMH,IELM1,MESH)
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CORFON'
        CALL CORFON
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CORFON'
        IF(IELMH.NE.IELM1) CALL CHGDIS(ZF,IELM1,IELMH,MESH)
      ENDIF
C
C=======================================================================
C
C IS POSSIBLE TO REDEFINE THE CHARACTERISTICS OF THE SOURCES
C
C STANDARD SUBROUTINE DOES NOT DO ANYTHING
C
      CALL SOURCE_TELEMAC2D
C
C=======================================================================
C
C CAREFULLY ANALYSES TOPOGRAPHY
C
      IF(OPTBAN.EQ.2) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TOPOGR'
        CALL TOPOGR(ZF%R,T1%R,ZFE%R,IKLE%I,MESH%IFABOR%I,
     &              MESH%NBOR%I,MESH%NELBOR%I,MESH%NULONE%I,
     &              IT1%I,IT2%I,IT3%I,
     &              NELEM,NPTFR,NPOIN,MXPTVS)
       IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TOPOGR'
      ENDIF
C
C=======================================================================
C
C : 2                  INITIALISES
C
C=======================================================================
C
C K-EPSILON AND OTHER CONSTANTS (KARMAN IS USED EVEN WITHOUT K-E)
C
      CALL COSAKE(KARMAN,CMU,C1,C2,SIGMAK,SIGMAE,
     &            ESTAR,SCHMIT,KMIN,KMAX,EMIN,EMAX)
C
      IF(ITURB.EQ.3) THEN
C       WILL HAVE TO INITIALISE K AND EPSILON
        AKEP = .TRUE.
      ELSE
C       SHOULD NOT INITIALISE K AND EPSILON
        AKEP = .FALSE.
      ENDIF
C
C INITIALISES PHYSICAL PARAMETERS
C
C     CONDIN IS CALLED EVEN IN THE EVENT OF A CONTINUATION, SO THAT THE DEFINITION
C     OF C0 DOES NOT CHANGE IN THE EVENT OF A CONTINUATION (CASE OF INCIDENT WAVES)
C
      IF(ADJO) THEN
        CALL CONDIN_ADJ(ALIRE,T2D_FILES(T2DRES)%LU,TROUVE)
      ELSE
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CONDIN'
        CALL CONDIN
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CONDIN'
      ENDIF
C
C     CORRECTS USER ERRORS IF H HAS BEEN USED
C     HERE THE NUMBER OF POINTS IS FORCED TO NPOIN.
      CALL CLIP(H,0.D0,.TRUE.,1.D6,.FALSE.,-NPOIN)
C
C     STORES THE INITIAL CELERITY (FOR THE INCIDENT WAVE)
C
      CALL CELERITE
C
C COMPUTES REFERENCE HEIGHT FOR BOUSSINESQ EQUATIONS
C
      IF(EQUA(1:10).EQ.'BOUSSINESQ') THEN
        CALL HREF
      ENDIF
C     ADDED ON 27/05/2002 (WAS NOT INITIALISED BEFORE)
      AT0=0.D0
C
      IF(.NOT.DEBU.AND..NOT.ADJO) THEN
C
C       BEWARE : BIEF_SUITE WILL TAKE THE BOTTOM IN THE FILE
C                IF IT IS THERE.
C
C       FRICTION COEFFICIENT ALSO READ IN CASE IT HAS BEEN DONE
C       BY THE USER INTERFACE (JMH 27/11/2006)
        ALIRE(19)=1
        CALL BIEF_SUITE(VARSOR,VARCL,IBID,
     &                  T2D_FILES(T2DPRE)%LU,
     &                  T2D_FILES(T2DPRE)%FMT,
     &                  HIST,0,NPOIN,AT,TEXTPR,VARCLA,
     &                  NVARCL,TROUVE,ALIRE,LISTIN,.TRUE.,MAXVAR)
        ALIRE(19)=0
        IF(RAZTIM) THEN
          AT=0.D0
          IF(LNG.EQ.1) WRITE(LU,*) 'TEMPS ECOULE REMIS A ZERO'
          IF(LNG.EQ.2) WRITE(LU,*) 'ELAPSED TIME RESET TO ZERO'
        ENDIF
        AT0=AT
        CALL RESCUE(U%R,V%R,H%R,FV%R,ZF%R,T,TRAC0,NTRAC,
     &              ITURB,NPOIN,AKEP,TROUVE)
C       CASE WHERE POSITIVE DEPTHS ARE NECESSARY
        IF(OPTBAN.EQ.1.AND.OPT_HNEG.EQ.2) THEN
          CALL CLIP(H,0.D0,.TRUE.,1.D6,.FALSE.,-NPOIN)
        ENDIF
      ENDIF
C
      TMAX=DUREE+AT0
C
C-----------------------------------------------------------------------
C
C  INITIALISES PARAMETERS SPECIFIC TO FINITE VOLUMES
C
C-----------------------------------------------------------------------
C
      IF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
C
        CALL OS( 'X=YZ    ' , QU , U , H , C )
        CALL OS( 'X=YZ    ' , QV , V , H , C )
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      LT=0
      LTT=0
C
C=======================================================================
C EXTENDS THE VARIABLES WHICH ARE NOT LINEAR P1
C=======================================================================
C
      IF(NTRAC.GT.0.AND.IELMT.NE.IELM1) THEN
        DO ITRAC=1,NTRAC
          CALL CHGDIS( T%ADR(ITRAC)%P ,IELM1 , IELMT , MESH )
        ENDDO
      ENDIF
      IF(IELMH.NE.IELM1) THEN
        CALL CHGDIS( H  , IELM1 , IELMH , MESH )
        CALL CHGDIS( ZF , IELM1 , IELMH , MESH )
      ENDIF
      IF(IELMU.NE.IELM1) THEN
        CALL CHGDIS( U , IELM1 , IELMU , MESH )
        CALL CHGDIS( V , IELM1 , IELMU , MESH )
      ENDIF
C
C=======================================================================
C INITIAL CONDITIONS NOT IN CONTINUATION FILE NOR IN CONDIN
C=======================================================================
C
C  CLIPPING (CONDITIONAL) OF H
C
      IF(CLIPH) CALL CLIP( H , HMIN , .TRUE. , 1.D6 , .FALSE. , 0 )
C
C-----------------------------------------------------------------------
C INITIAL WEATHER CONDITIONS
C
      IF (VENT.OR.ATMOS) THEN
        CALL METEO(PATMOS%R,WINDX%R,WINDY%R,
     &             FUAIR,FVAIR,MESH%X%R,MESH%Y%R,AT,LT,NPOIN,VENT,ATMOS,
     &             H%R,T1%R,GRAV,ROEAU,NORD,PRIVE)
      ENDIF
C
C-----------------------------------------------------------------------
C
C READS THE GEOMETRY OF SINGULARITIES
C
      IF(NWEIRS.GT.0) THEN
       CALL LECSNG(NWEIRS,NWRMAX,NPSING,NUMDIG%I,
     &             ZDIG%R,PHIDIG%R,IOPTAN,NPSMAX,NPOIN,
     &             T2D_FILES(T2DFO1)%LU)
      ENDIF
      IF(NSIPH.GT.0) THEN
       CALL LECSIP(RELAXS,NSIPH,ENTSIP,SORSIP,SECSCE,
     &             ALTSCE,CSSCE,CESCE,DELSCE,
     &             ANGSCE,LSCE,MAXSCE,T2D_FILES(T2DFO1)%LU)
      ENDIF
C
C-----------------------------------------------------------------------
C
C INITIAL CONDITIONS FOR K-EPSILON MODEL AND DIFFUSION
C
C   K-EPSILON
C
C     IF AKEP = .FALSE. K AND EPSILON COME FROM SUITE OR CONDIN
      IF(AKEP) THEN
C
        CALL FRICTION_CHOICE(1, KARMAN)
        IF(FRICTB) THEN
           KFROT_TP = 0
           IF(KFROT.EQ.NZONES) KFROT_TP = 1 ! NEED A NON ZERO VALUE
        ELSE
           KFROT_TP = KFROT
        ENDIF
!
        CALL AKEPIN(AK%R,EP%R,U%R,V%R,H%R,NPOIN,KFROT_TP,CMU,C2,
     &              ESTAR,SCHMIT,KMIN,EMIN,CF%R)
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C     PREPARES BOUNDARY CONDITIONS FOR WEIRS.
C
      IF(NWEIRS.GT.0) THEN
C
        CALL CLSING(NWEIRS,NPSING,NPSMAX,NUMDIG%I,
     &              MESH%X%R,MESH%Y%R,ZF%R,CHESTR%R,NKFROT%I,
     &              KARMAN,ZDIG%R,PHIDIG%R,MESH%NBOR%I,
     &              H%R,T,NTRAC,IOPTAN,T1%R,UBOR%R,VBOR%R,TBOR,
     &              LIHBOR%I,LIUBOR%I,LIVBOR%I,LITBOR,GRAV)
C
      ENDIF
C
C-----------------------------------------------------------------------
C
C     TYPES OF CONDITIONS FOR TRACER:
C
      IF(NTRAC.GT.0) THEN
        IF(NWEIRS.GT.0) CALL CLTRAC(NWEIRS,NPSING,NPSMAX,NUMDIG%I,
     &                 ZF%R,ZDIG%R,H%R,T,MESH%NBOR%I,LITBOR,TBOR,NTRAC)
        DO ITRAC=1,NTRAC
        CALL DIFFIN(MASKTR,LIMTRA%I,LITBOR%ADR(ITRAC)%P%I,
     &              IT1%I,U%R,V%R,MESH%XNEBOR%R,MESH%YNEBOR%R,
     &              MESH%NBOR%I,MESH%KP1BOR%I,NPTFR,
     &              KENT,KSORT,KLOG,KINC,KNEU,KDIR,KDDL,
     &              ICONVF(3),MESH%NELBOR%I,NPOIN,NELMAX,MSK,MASKEL%R,
     &              NFRLIQ,THOMFR,DEBLIQ,FINLIQ,FRTYPE,
     &              TN%ADR(ITRAC)%P,TBOR%ADR(ITRAC)%P,MESH)
        ENDDO
      ENDIF
C
C     TYPES OF CONDITIONS FOR PROPAGATION:
C
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PROPIN'
      CALL PROPIN_TELEMAC2D
     &            (LIMPRO%I,LIMPRO%DIM1,MASK,LIUBOR%I,LIVBOR%I,
     &             LIHBOR%I,MESH%KP1BOR%I,MESH%NBOR%I,NPTFR,
     &             KENT,KENTU,KSORT,KADH,KLOG,KINC,
     &             KNEU,KDIR,KDDL,KOND,CLH%I,CLU%I,CLV%I,
     &             U%ELM,U%R,V%R,GRAV,H%R,LT,NPOIN,
     &             MESH%NELBOR%I,NELMAX,MSK,MASKEL%R,
     &             NFRLIQ,THOMFR,DEBLIQ,FINLIQ,FRTYPE,
     &             MESH%XNEBOR%R,MESH%YNEBOR%R,ENTET,MESH)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PROPIN'
C
C     PROPIN WILL BE CALLED IN THE TIME LOOP AFTER EACH CALL
C     TO BORD
C
C-----------------------------------------------------------------------
C
C     FRICTION COEFFICIENT:
C
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE FRICTION_CHOICE'
      CALL FRICTION_CHOICE(1,KARMAN)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE FRICTION_CHOICE'
C
C  DIFFUSION OF SPEED (CALLED HERE TO INITIALISE VISC IN CASE
C                           IT IS ONE OF THE OUTPUT VARIABLES)
      IF(ITURB.EQ.1) THEN
C
        CALL OS('X=C     ', X=VISC , C=PROPNU )
C
      ELSEIF(ITURB.EQ.2) THEN
C
        CALL DISPER( VISC , U%R , V%R , H%R , CF%R , ELDER , PROPNU )
C
      ELSEIF(ITURB.EQ.3) THEN
C
        CALL VISTUR(VISC,AK,EP,NPOIN,CMU,PROPNU)
C
      ELSEIF(ITURB.EQ.4) THEN
C
        CALL SMAGOR(VISC,CF,U,V,MESH,T1,T2,T3,T4,MSK,MASKEL,PROPNU)
C
      ELSE
        IF(LISTIN) THEN
          IF(LNG.EQ.1) WRITE(LU,15) ITURB
          IF(LNG.EQ.2) WRITE(LU,16) ITURB
        ENDIF
15      FORMAT(1X,'ITURB=',1I6,'MODELE DE TURBULENCE NON PREVU')
16      FORMAT(1X,'ITURB=',1I6,'UNKNOWN TURBULENCE MODEL')
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C  DROGUE(S)
C
      IF(NFLOT.NE.0) CALL FLOT(XFLOT%R,YFLOT%R,NFLOT,NITFLO,
     &                         FLOPRD,MESH%X%R,MESH%Y%R,
     &                         NPOIN,DEBFLO%I,FINFLO%I,NIT)
C
C-----------------------------------------------------------------------
C  LAGRANGIAN DRIFT(S)
C
      IF(NLAG.NE.0) CALL LAGRAN(NLAG,DEBLAG%I,FINLAG%I)
C
C-----------------------------------------------------------------------
C  LOCATION OF THE OUTLETS
C
      IF(NREJET.NE.0.OR.NREJTR.NE.0) THEN
        CALL PROXIM(ISCE,XSCE,YSCE,
     &              MESH%X%R,MESH%Y%R,
     &              NREJET,NPOIN,
     &              MESH%IKLE%I,NELEM,NELMAX)
      ENDIF
C
C=======================================================================
C END OF INITIAL CONDITIONS
C=======================================================================
C
C INITIALISES ADVECTION
C FTILD COMPRISES UTILD,VTILD,HTILD,(TTILD),(AKTILD AND EPTILD)
C
      CALL OS( 'X=0     ' , X=FTILD )
C
C***********************************************************************
C
C LISTING AND OUTPUT FOR THE INITIAL CONDITIONS.
C
      IF(LISTIN) CALL ENTETE(1,AT,LT)
C
C     OUTINI IS KEY-WORD "OUTPUT OF INITIAL CONDITONS"
C     IT HAS PRIORITY OVER FIRST TIME-STEP FOR GRAPHIC PRINTOUTS.
C
C     NOTE THAT OUTPUTS ARE DONE WITHIN ESTEL3D IN COUPLED MODE)
C
      IF(OUTINI .AND. (.NOT.ADJO)
     &          .AND. (CODE(1:7).NE.'ESTEL3D') ) THEN
C
C CONTROL SECTIONS (0. IN PLACE OF DT)
C
        IF(NCP.NE.0.AND.(ENTET.OR.CUMFLO)) THEN
          CALL FLUSEC_TELEMAC2D(U,V,H,MESH%IKLE%I,MESH%XEL%R,MESH%YEL%R,
     &                          MESH%NELMAX,MESH%NELEM,
     &                          MESH%X%R,MESH%Y%R,
     &                          0.D0,NCP,CTRLSC,ENTET,AT,MESH%KNOGL%I,
     &                          MSKSEC,BM1,BM2,T1,H,MESH,S,CV1,
     &                          MESH%IFABOR%I,COMFLU,CUMFLO)
        ENDIF
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PRERES_TELEMAC2D'
        CALL PRERES_TELEMAC2D
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PRERES_TELEMAC2D'
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE DESIMP'
        CALL BIEF_DESIMP(T2D_FILES(T2DRES)%FMT,VARSOR,
     &                  HIST,0,NPOIN,T2D_FILES(T2DRES)%LU,'STD',AT,LT,
     &                  LISPRD,LEOPRD,
     &                  SORLEO,SORIMP,MAXVAR,TEXTE,0,     0)
C                                                  PTINIG,PTINIL
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE DESIMP'
C
      ENDIF
C
C=======================================================================
C
C     COUPLING WITH DELWAQ
C
      IF(INCLUS(COUPLING,'DELWAQ')) THEN
C
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TEL4DEL'
C
C     T3 : MODIFIED DEPTH TO TAKE INTO ACCOUNT MASS-LUMPING
C          IN THE CONTINUITY EQUATION
      IF(ABS(1.D0-AGGLOC).GT.1.D-8) THEN
        CALL VECTOR(T3 ,'=','MASVEC          ',IELMH,
     &              1.D0-AGGLOC,H ,S,S,S,S,S,MESH,MSK,MASKEL)
        IF(NCSIZE.GT.1) CALL PARCOM(T3,2,MESH)
        CALL OS('X=XY    ',X=T3 ,Y=UNSV2D)
        CALL OS('X=X+CY  ',X=T3 ,Y=H ,C=AGGLOC)
      ELSE
        CALL OS('X=Y     ',X=T3 ,Y=H )
      ENDIF
      CALL TEL4DEL(MESH%NPOIN,
     &        MESH%NPOIN,MESH%NELEM,MESH%NSEG,MESH%IKLE%I,MESH%ELTSEG%I,
     &        MESH%GLOSEG%I,MESH%ORISEG%I,MESH%GLOSEG%DIM1,
     &        MESH%X%R,MESH%Y%R,MESH%NPTFR,LIHBOR%I,
     &        MESH%NBOR%I,1,AT,DT,LT,NIT,T3%R,H%R,T3%R,U%R,V%R,
     &        T%ADR(MAX(IND_S,1))%P%R,
     &        T%ADR(MAX(IND_T,1))%P%R,VISC%R,TITCAS,
     &        T2D_FILES(T2DGEO)%NAME,T2D_FILES(T2DCLI)%NAME,WAQPRD,
     & T2DDL1,T2D_FILES(T2DDL1)%NAME,T2DDL2,T2D_FILES(T2DDL2)%NAME,
     & T2DDL3,T2D_FILES(T2DDL3)%NAME,T2DDL5,T2D_FILES(T2DDL5)%NAME,
     & T2DDL6,T2D_FILES(T2DDL6)%NAME,T2DDL7,T2D_FILES(T2DDL7)%NAME,
     & T2DL11,T2D_FILES(T2DL11)%NAME,T2DDL4,T2D_FILES(T2DDL4)%NAME,
     & T2DDL8,T2D_FILES(T2DDL8)%NAME,T2DDL9,T2D_FILES(T2DDL9)%NAME,
     & T2DL10,T2D_FILES(T2DL10)%NAME,INFOGR,NELEM,SALI_DEL,TEMP_DEL,
     & VELO_DEL,DIFF_DEL,MARDAT,MARTIM,FLODEL%R,INIFLOW,MESH%W%R,
     & .FALSE.,FLULIM%R,V2DPAR%R,MESH%KNOLG%I,MESH,MESH)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TEL4DEL'
C
      ENDIF
C
C=======================================================================
C
C     OPTIONAL USER OUTPUT (COURTESY JACEK JANKOWSKI, BAW)
      CALL UTIMP_TELEMAC2D(LT,AT,PTINIG,LEOPRD,PTINIL,LISPRD)
C
C=======================================================================
C
C  INITIALISES THE ADVECTION AND PROPAGATION FIELDS
C
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE HPROPA'
      CALL HPROPA(HPROP,H,H,PROLIN,HAULIN,TETAC,NSOUSI)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE HPROPA APPEL DE CHPCON'
      CALL CHPCON(UCONV,VCONV,U,V,U,V,TETAU)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CHPCON'
      IF(SOLSYS.EQ.2) THEN
C       INITIALISES UDEL AND VDEL ONLY AT FIRST CALL TO SISYPHE
        CALL OS('X=Y     ',X=UDEL,Y=UCONV)
        CALL OS('X=Y     ',X=VDEL,Y=VCONV)
        USIS=>UDEL
        VSIS=>VDEL
      ELSE
        USIS=>UCONV
        VSIS=>VCONV
      ENDIF
C
C=======================================================================
C
C     TETAHC: SEMI-IMPLICITATION OF H IN THE CONTINUITY EQUATION
C             IS ALSO USED FOR FLUXES IN THE MASS BALANCE
      TETAHC = TETAC
      IF(ICONVF(2).EQ.5) TETAHC = 0.D0
C
C     FIRST COMPUTATION OF POROSITY
C
      IF(OPTBAN.EQ.3) THEN
        CALL POROS(TE5,ZF,H,MESH)
        IF(MSK) CALL OS('X=XY    ',X=TE5,Y=MASKEL)
      ENDIF
C
C FIRST COMPUTATIONS FOR BALANCE
C
      IF(BILMAS) THEN
C
        MASSES = 0.D0
        FLUSOR = 0.D0
        FLUENT = 0.D0
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE BILAN'
        CALL BILAN(MESH,H,T1,MASK,AT,0.D0,LT,NIT,LISTIN,
     &             MASSES,MSK,MASKEL,EQUA,TE5,OPTBAN,
     &             MESH%NPTFR,FLBOR,
     &             FLUX_BOUNDARIES,NUMLIQ%I,NFRLIQ)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BILAN'
C
        IF(NTRAC.GT.0) THEN
C
          IF(EQUA(1:15).NE.'SAINT-VENANT VF') THEN
            IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE BILANT'
            DO ITRAC=1,NTRAC
            MASSOU(ITRAC) = 0.D0
            CALL BILANT(H,T2,T3,DT,LT,NIT,LISTIN,
     &                  T%ADR(ITRAC)%P,
     &                  AGGLOT,MASSOU(ITRAC),MASTR0(ITRAC),
     &                  MASTR2(ITRAC),MASTEN(ITRAC),
     &                  MASTOU(ITRAC),MSK,MASKEL,MESH,FLBOR,
     &                  NUMLIQ%I,NFRLIQ,NPTFR,NAMETRAC(ITRAC),
     &                  FLBORTRA)
            ENDDO
            IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BILANT'
C
          ELSE
            FLUTSOR = 0.D0
            FLUTENT = 0.D0
            IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE BILANT1'
            DO ITRAC=1,NTRAC
            CALL BILANT1(H,UCONV,VCONV,HPROP,T2,T3,T4,T5,T6,
     &                   DT,LT,NIT,ENTET,MASKTR,
     &                   T%ADR(1)%P,TN%ADR(1)%P,TETAT,
     &                   MASSOU(ITRAC),MSK,MASKEL,MESH,
     &                   FLUTSOR(ITRAC),FLUTENT(ITRAC),EQUA,LTT,ITRAC)
            ENDDO
            IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BILANT1'
          ENDIF
C
        ENDIF
C
      ENDIF
C
C=======================================================================
C
      IF(NIT.EQ.0) THEN
        IF(LISTIN) THEN
          IF(LNG.EQ.1) WRITE(LU,9)
          IF(LNG.EQ.2) WRITE(LU,10)
        ENDIF
9      FORMAT(1X,'ARRET DANS TELEMAC, NOMBRE D''ITERATIONS DEMANDE NUL')
10     FORMAT(1X,'STOP IN TELEMAC, NUMBER OF TIME STEP ASKED EQUALS 0')
       STOP
      ENDIF
C
C=======================================================================
C
C     COUPLING 
C
      IF(COUPLING.NE.' ') THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'TELEMAC2D COUPLE AVEC : ',COUPLING
        IF(LNG.EQ.2) WRITE(LU,*) 'TELEMAC2D COUPLED WITH: ',COUPLING
      ENDIF
C
C     INITIALISES CONSTANT FLOW DISCHARGE (SEE SISYPHE)
C     ------------------------------------------------------------------
C
      SISYPHE_CFD   = .FALSE.
      CONSTFLOW_SIS = .FALSE.
      NSIS_CFD      = 1
C
      IF(INCLUS(COUPLING,'SISYPHE')) THEN
C
         IF(INCLUS(COUPLING,'FILE-SISYPHE')) THEN
C
           WRITE (LU,*) 'TELEMAC-2D: FILE-COUPLING HAS NOW BEEN'
           WRITE (LU,*) '            SUPPRESSED'
           WRITE (LU,*) '            USE INTER-SISYPHE OR SISYPHE'
           WRITE (LU,*) '            INSTEAD OF FILE-SISYPHE'
           CALL PLANTE(1)
           STOP
C
         ELSEIF(INCLUS(COUPLING,'SISYPHE')) THEN
C
           IF(LNG.EQ.1) THEN
             WRITE (LU,*) 'TELEMAC-2D : COUPLAGE INTERNE AVEC SISYPHE'
           ENDIF
           IF(LNG.EQ.2) THEN
             WRITE (LU,*) 'TELEMAC-2D: INTERNAL COUPLING WITH SISYPHE'
           ENDIF
           CALL CONFIG_CODE(2)
           IF(DEBUG.GT.0) WRITE(LU,*) 'PREMIER APPEL DE SISYPHE'
           CALL SISYPHE(0,LT,LEOPRD,LISPRD,NIT,U,V,H,H,ZF,CF,CF,CHESTR,
     &                  CONSTFLOW_SIS,NSIS_CFD,SISYPHE_CFD,CODE1,PERCOU,
     &                  U,V,AT,VISC,DT,CHARR,SUSP,
C                                      CHARR,SUSP : RETURNED BY SISYPHE
C                                                   BUT THEN GIVEN TO IT
     &                  FLBOR,SOLSYS,DM1,USIS,VSIS,ZCONV)
           IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SISYPHE'
           CALL CONFIG_CODE(1)
C          AVOIDS TWO OUTPUTS WHEN SISYPHE IS CALLED TWICE
           IF(SUSP.AND.CHARR.AND.PERCOU.NE.1) THEN
             LEOPRD_CHARR=NIT+PERCOU
           ELSE
             LEOPRD_CHARR=LEOPRD
           ENDIF
C
         ENDIF
C
       ENDIF
C
      IF(INCLUS(COUPLING,'TOMAWAC')) THEN
C
        IF(LNG.EQ.1) THEN
          WRITE (LU,*) 'TELEMAC-2D : COUPLAGE INTERNE AVEC TOMAWAC'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE (LU,*) 'TELEMAC-2D: INTERNAL COUPLING WITH TOMAWAC'
        ENDIF
        CALL CONFIG_CODE(3)
        IF(DEBUG.GT.0) WRITE(LU,*) 'PREMIER APPEL DE TOMAWAC'
!       CALL WAC(0,U,V,H,FXWAVE,FYWAVE,WINDX,WINDY,CODE1,AT,DT,NIT,
!                PERCOU_WAC)
        CALL WAC(0,U,V,H,FXWAVE,FYWAVE,T1   ,T2   ,CODE1,AT,DT,NIT,
     *           PERCOU_WAC)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TOMAWAC'
        CALL CONFIG_CODE(1)
C
       ENDIF
C
C=======================================================================
C INITIALISES INFILTRATION STRUCTURES FOR COUPLING WITH ESTEL3D
C
      CALL INFILTRATION_INIT(NPOIN,(CODE(1:7).EQ.'ESTEL3D'))
C
C     SAVES THE DEPTH CALCULATED BY TELEMAC2D FOR ESTEL3D
C
      IF(CODE(1:7).EQ.'ESTEL3D') CALL DEPTH_FILL(H%R)
C
C=======================================================================
C
C : 3                    /* TIME LOOP */
C
C=======================================================================
C
C     STORES DT FOR CASE WITH VARIABLE TIME-STEP
C
      DTCAS = DT
C
C     CALLED BY ANOTHER PROGRAM, ONLY INITIALISATION REQUIRED
      IF(PASS.EQ.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'FIN D''INITIALISATION DE TELEMAC2D'
        IF(LNG.EQ.2) WRITE(LU,*) 'TELEMAC2D INITIALISED'
        RETURN
      ENDIF
C
700   CONTINUE
C
      IF(PASS.EQ.1) THEN
        IF(CODE(1:7).EQ.'ESTEL3D') THEN
          AT=ATDEP
          NIT=NITER
C --- JP RENAUD START ---
C         USE THE TIME STEP SPECIFIED BY ESTEL-3D
          IF(PRESENT(DTDEP)) THEN
            DT = DTDEP
            DTCAS = DTDEP
          ! TO DO: CHECK WHAT HAPPENS WITH ADAPTIVE TIME STEP
          ENDIF
C --- JP RENAUD END ---
        ELSE
          CALL PLANTE(1)
          STOP 'UNKNOWN CALLING PROGRAM'
        ENDIF
      ENDIF
C
      LT = LT + 1
C
      IF(DTVARI.AND.EQUA(1:15).NE.'SAINT-VENANT VF') THEN
C       COURANT NUMBER FOR PSI SCHEME IN P1
        CALL CFLPSI(T1,U,V,DT,IELM,MESH,MSK,MASKEL)
        CALL MAXI(CFLMAX,IMAX,T1%R,NPOIN)
C       LIMITS VARIATIONS IN THE RANGE (1/2, 2)
        DT = DT * MAX(MIN(CFLWTD/MAX(CFLMAX,1.D-6),2.D0),0.5D0)
C       LIMITS DT TO THAT OF THE STEERING FILE
        DT=MIN(DT,DTCAS)
        IF(NCSIZE.GT.1) DT=P_DMIN(DT)
        IF(ENTET) THEN
          IF (LNG.EQ.1) WRITE(LU,78) CFLMAX,DT
          IF (LNG.EQ.2) WRITE(LU,79) CFLMAX,DT
78        FORMAT(1X,'    NOMBRE DE COURANT MAXIMUM :',G16.7,/,1X,
     &              '    PAS DE TEMPS              :',G16.7)
79        FORMAT(1X,'    MAXIMUM COURANT NUMBER: ',G16.7,/,1X,
     &              '    TIME-STEP                 :',G16.7)
        ENDIF
      ENDIF
C
C=======================================================================
C
C     COUPLING WITH SISYPHE
C
      IF(INCLUS(COUPLING,'SISYPHE')) THEN
C
        CALL CONFIG_CODE(2)
C       HN NOT DEFINED HERE AT FIRST ITERATION
        IF(LT.EQ.1) CALL OS('X=Y     ',X=HN,Y=H)
C
        SUSP1=SUSP.AND.PERCOU.EQ.1
        IF(SUSP1.OR.(CHARR.AND.(PERCOU*((LT-1)/PERCOU).EQ.LT-1))) THEN
C
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SISYPHE, CHARRIAGE'
          CALL SISYPHE(1,LT,LEOPRD_CHARR,LISPRD,NIT,U,V,H,HN,ZF,
     &                 CF,CF,CHESTR,CONSTFLOW_SIS,NSIS_CFD,SISYPHE_CFD,
     &                 CODE1,PERCOU,U,V,AT,VISC,DT*PERCOU,CHARR,SUSP1,
     &                 FLBOR,SOLSYS,DM1,USIS,VSIS,ZCONV)
          IF(DEBUG.GT.0) WRITE(LU,*) 'FIN APPEL SISYPHE, CHARRIAGE'
C
        ENDIF
C
        IF(SUSP.AND.PERCOU.NE.1) THEN
C
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SISYPHE, SUSPENSION'
          CALL SISYPHE(1,LT,LEOPRD,LISPRD,NIT,U,V,H,HN,ZF,
     &                 CF,CF,CHESTR,CONSTFLOW_SIS,NSIS_CFD,SISYPHE_CFD,
     &                 CODE1,1,U,V,AT,VISC,
     &                 DT,NON,SUSP,
     &                 FLBOR,SOLSYS,DM1,USIS,VSIS,ZCONV)
          IF(DEBUG.GT.0) WRITE(LU,*) 'FIN APPEL DE SISYPHE, SUSPENSION'
C
        ENDIF
C
        CALL CONFIG_CODE(1)
C
      ENDIF
C
C=======================================================================
C
C     COUPLING WITH TOMAWAC
C
      IF(INCLUS(COUPLING,'TOMAWAC').AND.
     *   PERCOU_WAC*((LT-1)/PERCOU_WAC).EQ.LT-1) THEN
C
        CALL CONFIG_CODE(3)
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TOMAWAC'
!       CALL WAC(1,U,V,H,FXWAVE,FYWAVE,WINDX,WINDY,CODE1,AT,
!    *           DT*PERCOU_WAC,NIT,PERCOU_WAC)
        CALL WAC(1,U,V,H,FXWAVE,FYWAVE,T1   ,T2   ,CODE1,AT,
     *           DT*PERCOU_WAC,NIT,PERCOU_WAC)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TOMAWAC'
        CALL CONFIG_CODE(1)
C
       ENDIF
C
C=======================================================================
C
      IF(ADJO) THEN
        AT = AT - DT
      ELSE
C       DT IS NOT YET KNOWN IN FINITE VOLUMES
        IF(EQUA(1:15).NE.'SAINT-VENANT VF') AT = AT + DT
      ENDIF
C
      IF(DTVARI) THEN
        IF(AT.GT.DUREE+AT0) THEN
C         LAST TIME STEP
          NIT = LT
        ELSE
C         DUMMY VALUE GREATER THAN LT
          NIT = LT + 10
        ENDIF
      ENDIF
C
      IF((LISPRD*(LT/LISPRD).EQ.LT.AND.LT.GE.PTINIL).OR.LT.EQ.NIT) THEN
        ENTET=LISTIN
      ELSE
        ENTET=.FALSE.
      ENDIF

C --- JP RENAUD START ---
C CONSTRAINS TELEMAC-2D OUTPUT IN THE LISTING
      IF (PRESENT(DOPRINT)) ENTET = ENTET .AND. DOPRINT
C --- JP RENAUD END ---

      IF(ENTET) CALL ENTETE(2,AT,LT)
C
C=======================================================================
C
C BACKUP OF UN, VN, HN, TN, AKN AND EPN (THEY ARE IN FN)
C
C --- JP RENAUD START ---
C THIS IS NOT DONE WHEN ITERATING FOR THE COUPLING WITH ESTEL-3D
       IF(CODE(1:7).EQ.'ESTEL3D'.AND.PRESENT(NEWTIME)) THEN
        IF(NEWTIME) CALL OS('X=Y     ',X=FN,Y=F)
       ELSE
         CALL OS('X=Y     ',X=FN,Y=F)
       ENDIF
C      CALL OS( 'X=Y     ' , FN , F , F , C )
C --- JP RENAUD END ---
C
C=======================================================================
C
C NEW COUPLING WITH SISYPHE FOR CONSTANT FLOW DISCHARGE
C
      IF(SISYPHE_CFD.AND.CONSTFLOW_SIS) GOTO 999
C
      DO 888 ISIS_CFD=1,NSIS_CFD
C
C=======================================================================
C
C  MASKING OF THE WETTING/DRYING ELEMENTS
C
      IF(MSK) CALL OS( 'X=C     ' , MASKEL , S , S , 1.D0 )
      IF (OPTBAN.EQ.2) THEN
        CALL MASKBD(MASKEL%R,ZFE%R,ZF%R,H%R,
     &              HMIN,MESH%IKLE%I,MESH%IFABOR%I,IT1%I,NELEM,NPOIN)
      ENDIF
C
C  MASKING SPECIFIED BY USER
C
      IF(MSKUSE) THEN
      CALL MASKOB (MASKEL%R,MESH%X%R,MESH%Y%R,
     &             MESH%IKLE%I,NELEM,NELMAX,NPOIN,AT,LT)
      ENDIF
C
C CREATES THE MASK OF THE POINTS FROM THE MASK OF THE ELEMENTS
C AND CHANGES OF IFAMAS (IFABOR WITH MASKING)
C
      IF(MSK) THEN
        CALL MASKTO(MASKEL%R,MASKPT,IFAMAS%I,MESH%IKLE%I,
     &              MESH%IFABOR%I,MESH%ELTSEG%I,MESH%NSEG,
     &              NELEM,NPOIN,IELMT,MESH)
        IF(IELMX.NE.IELM1) CALL CHGDIS(MASKPT,IELM1,IELMX,MESH)
      ENDIF
C
C-----------------------------------------------------------------------
C  COMPUTATION OF THE INTEGRAL OF THE BASES
C-----------------------------------------------------------------------
C
C     IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE MASBAS2D'
C     IF(MSK) THEN
C       CALL MASBAS2D(VOLU2D,V2DPAR,UNSV2D,IELM1,MESH,MSK,MASKEL,T2,T2)
C     ENDIF
C     IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE MASBAS2D'
C
C-----------------------------------------------------------------------
C
C UPDATES POROSITY : NEW VALUE IN TE5
C                    OLD - NEW IN TE4
C
      IF(OPTBAN.EQ.3) THEN
C
         CALL OS('X=Y     ',TE4,TE5,TE5,0.D0)
         CALL POROS(TE5,ZF,HN,MESH)
         IF(MSK) CALL OS('X=XY    ',X=TE5,Y=MASKEL)
C        TEST OF UNDER-RELAXATION
         RELAX = 0.05D0
         CALL OS('X=CX    ',X=TE5,C=RELAX)
         CALL OS('X=X+CY  ',X=TE5,Y=TE4,C=1.D0-RELAX)
C        TE4 = OLD POROS - NEW POROS
         CALL OS('X=X-Y   ',X=TE4,Y=TE5)
C
      ENDIF
C
C=======================================================================
C
C NEW ADVECTION AND PROPAGATION FIELDS
C NOTE THAT U = UN, V = VN AND H = HN AT THIS STAGE
C
      IF(CONV) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CHPCON'
        CALL CHPCON(UCONV,VCONV,U,V,UN,VN,TETAU)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CHPCON'
      ENDIF
C
C     COMPUTATION OF THE NEW PROPAGATION TERM
C
      IF(PROPA) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE HPROPA'
        CALL HPROPA(HPROP ,HN,H,PROLIN,HAULIN,TETAHC,NSOUSI)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE HPROPA'
      ENDIF
C
C=======================================================================
C
C PREPARES BOUNDARY CONDITIONS FOR WEIRS.
C
      IF(NWEIRS.GT.0) THEN
C
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CLSING'
        CALL CLSING(NWEIRS,NPSING,NPSMAX,NUMDIG%I,
     &              MESH%X%R,MESH%Y%R,ZF%R,CHESTR%R,NKFROT%I,
     &              KARMAN,ZDIG%R,PHIDIG%R,MESH%NBOR%I,
     &              H%R,T,NTRAC,IOPTAN,T1%R,UBOR%R,VBOR%R,TBOR,
     &              LIHBOR%I,LIUBOR%I,LIVBOR%I,LITBOR,GRAV)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CLSING'
C
      ENDIF
C
C IT IS ASSUMED THAT THE TYPES OF BOUNDARY CONDITIONS DO NOT CHANGE
C DURING THE SUB-ITERATIONS. IF NOT IT IS NECESSARY TO MOVE THE CALLS
C TO KEPSIN, DIFFIN, PROPIN
C
C TYPES OF CONDITIONS FOR THE K-EPSILON MODEL
C
      IF(ITURB.EQ.3) CALL KEPSIN(LIMKEP%I,LIUBOR%I,NPTFR,
     &                           KENT,KENTU,KSORT,KADH,KLOG,
     &                           KINC,KNEU,KDIR)
C
C TYPES OF CONDITIONS FOR THE DIFFUSION OF THE TRACER:
C
      IF(NTRAC.GT.0) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE DIFFIN'
        DO ITRAC=1,NTRAC
        CALL DIFFIN(MASKTR,LIMTRA%I,LITBOR%ADR(ITRAC)%P%I,
     &              IT1%I,U%R,V%R,MESH%XNEBOR%R,MESH%YNEBOR%R,
     &              MESH%NBOR%I,MESH%KP1BOR%I,NPTFR,
     &              KENT,KSORT,KLOG,KINC,KNEU,KDIR,KDDL,
     &              ICONVF(3),MESH%NELBOR%I,NPOIN,NELMAX,MSK,MASKEL%R,
     &              NFRLIQ,THOMFR,DEBLIQ,FINLIQ,FRTYPE,
     &              TN%ADR(ITRAC)%P,TBOR%ADR(ITRAC)%P,MESH)
        ENDDO
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE DIFFIN'
      ENDIF
C
C TYPES OF CONDITIONS FOR THE PROPAGATION:
C REQUIRED FOR THOMFR ?? (OTHERWISE DONE AFTER BORD !)
C
C
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PROPIN'
      CALL PROPIN_TELEMAC2D
     &            (LIMPRO%I,LIMPRO%DIM1,MASK,LIUBOR%I,LIVBOR%I,
     &             LIHBOR%I,MESH%KP1BOR%I,MESH%NBOR%I,NPTFR,
     &             KENT,KENTU,KSORT,KADH,KLOG,KINC,
     &             KNEU,KDIR,KDDL,KOND,CLH%I,CLU%I,CLV%I,
     &             U%ELM,U%R,V%R,GRAV,H%R,LT,NPOIN,
     &             MESH%NELBOR%I,NELMAX,MSK,MASKEL%R,
     &             NFRLIQ,THOMFR,DEBLIQ,FINLIQ,FRTYPE,
     &             MESH%XNEBOR%R,MESH%YNEBOR%R,.FALSE.,MESH)
C    *             MESH%XNEBOR%R,MESH%YNEBOR%R, ENTET ,MESH)
C       WARNINGS WILL BE GIVEN AT THE SECOND CALL AFTER BORD
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PROPIN'
C
C=======================================================================
C                 COMPUTES THE FRICTION COEFFICIENTS
C                         VARIABLE IN TIME
C=======================================================================
C CORSTR DOES NOT DO ANYTHING UNLESS MODIFIED BY THE USER.
C
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CORSTR'
      CALL CORSTR
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CORSTR'
C
      IF(IELMU.EQ.12.OR.IELMU.EQ.13) CALL CHGDIS(CHESTR,11,IELMU,MESH)
C
      CALL FRICTION_CHOICE(1,KARMAN)
C
C=======================================================================
C                 COMPUTES VISCOSITY COEFFICIENTS
C=======================================================================
C
C  COMPUTES DYNAMIC VISCOSITY VISC
C
      IF(ITURB.EQ.1) THEN
C
        CALL OS( 'X=C     ' , VISC , VISC , VISC , PROPNU )
C
      ELSEIF(ITURB.EQ.2) THEN
C
        CALL DISPER( VISC , U%R , V%R , H%R , CF%R , ELDER , PROPNU )
C
      ELSEIF(ITURB.EQ.3) THEN
C
        CALL VISTUR(VISC,AK,EP,NPOIN,CMU,PROPNU)
C
      ELSEIF(ITURB.EQ.4) THEN
C
        CALL SMAGOR(VISC,CF,U,V,MESH,T1,T2,T3,T4,MSK,MASKEL,PROPNU)
C
      ELSE
C
        IF(LISTIN) THEN
          IF(LNG.EQ.1) WRITE(LU,15) ITURB
          IF(LNG.EQ.2) WRITE(LU,16) ITURB
        ENDIF
        CALL PLANTE(1)
        STOP
C
      ENDIF
C
C  COEFFICIENT FOR THERMAL DIFFUSION (PRANDTL = 1 FOR NOW)
C  AND THE SAME FOR ALL THE TRACERS
C
      IF(NTRAC.GT.0.AND.DIFT) THEN
        DO ITRAC=1,NTRAC
          CALL OS( 'X=Y     ' , X=VISCT%ADR(ITRAC)%P , Y=VISC )
          CALL OS( 'X=X+C   ' , X=VISCT%ADR(ITRAC)%P , C=DIFNU-PROPNU )
        ENDDO
      ENDIF
C
C  IT IS POSSIBLE TO CORRECT THE VISCOSITY COEFFICIENTS.
C
      CALL CORVIS
!
!=======================================================================
C  SOURCES : COMPUTATION OF INPUTS WHEN VARYING IN TIME
C            IF NO VARIATION IN TIME DSCE2=DSCE AND TSCE2=TSCE
!=======================================================================
!
      IF(NREJET.GT.0) THEN
        DO I=1,NREJET
          DSCE2(I)=DEBSCE(AT,I,DSCE)
        ENDDO
        IF(NTRAC.GT.0) THEN
          DO I=1,NREJET
            DO ITRAC=1,NTRAC
              TSCE2(I,ITRAC)=TRSCE(AT,I,ITRAC)
            ENDDO
          ENDDO
        ENDIF
      ENDIF
C
C=======================================================================
C BOUNDARY CONDITIONS
C=======================================================================
C
      IF(THOMFR) THEN
C
      CALL CPSTVC(H,T9)
      CALL PREBOR(HBOR%R,UBOR%R,VBOR%R,TBOR,U%R,V%R,H%R,
     &            T9%R,T,MESH%NBOR%I,MESH%KP1BOR%I,
     &            NPOIN,NPTFR,NTRAC,DEBLIQ,FINLIQ,NFRLIQ)
C
      ENDIF
C
C CALLS THE USER-SUBROUTINE DETERMINING THE BOUNDARY CONDITIONS.
C
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE BORD'
      CALL BORD(HBOR%R,UBOR%R,VBOR%R,TBOR,
     &          U,V,H,ZF%R,MESH%NBOR%I,W1,T8,
     &          LIHBOR%I,LIUBOR%I,LITBOR,
     &          MESH%XNEBOR%R,MESH%YNEBOR%R,NPOIN,NPTFR,
     &          NPTFR2,AT,
     &          NDEBIT,NCOTE,NVITES,NTRAC,NTRACE,NFRLIQ,NUMLIQ%I,
     &          KENT,KENTU,PROVEL,MASK,MESH,EQUA,T2D_FILES(T2DIMP)%NAME)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BORD'
C
C COMPUTES LIMPRO, CLU,CLV, CLH AND MASK
C
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PROPIN'
      CALL PROPIN_TELEMAC2D
     &            (LIMPRO%I,LIMPRO%DIM1,MASK,LIUBOR%I,LIVBOR%I,
     &             LIHBOR%I,MESH%KP1BOR%I,MESH%NBOR%I,NPTFR,
     &             KENT,KENTU,KSORT,KADH,KLOG,KINC,
     &             KNEU,KDIR,KDDL,KOND,CLH%I,CLU%I,CLV%I,
     &             U%ELM,U%R,V%R,GRAV,H%R,LT,NPOIN,
     &             MESH%NELBOR%I,NELMAX,MSK,MASKEL%R,
     &             NFRLIQ,THOMFR,DEBLIQ,FINLIQ,FRTYPE,
     &             MESH%XNEBOR%R,MESH%YNEBOR%R,ENTET,MESH)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PROPIN'
C
C PREPARING THE FRICTION ON THE LATERAL BOUNDARIES
C
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE WALL_FRICTION'
      CALL WALL_FRICTION(UETUTA%R,AUBOR%R,CFBOR%R,
     &                   MESH%DISBOR%R,UN%R,VN%R,LIMPRO%I,
     &                   MESH%NBOR%I,NPTFR,KARMAN,PROPNU,
     &                   LISRUG,KNEU,KDIR,KENT,KENTU,KADH,KLOG,
     &                   IELMU,MESH%KP1BOR%I)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE WALL_FRICTION'
C
C K-EPSILON BOUNDARY CONDITIONS: KBOR,EBOR AND AUBOR
C
      IF(ITURB.EQ.3) THEN
        CALL KEPSCL(KBOR%R,EBOR%R,AUBOR%R,CF%R,CFBOR%R,
     &              MESH%DISBOR%R,
     &              UN%R,VN%R,HN%R,LIMKEP%I,LIUBOR%I,LIMPRO%I,
     &              MESH%NBOR%I,NPTFR,KARMAN,CMU,C2,ESTAR,
     &              SCHMIT,LISRUG,PROPNU,KMIN,EMIN,KNEU,KDIR,
     &              KENT,KENTU,KADH,KLOG,UETUTA%R)
      ENDIF
C
C CALLS THE SYSTEM OF RESOLUTION FOR BOUNDARIES BY THE CHARACTERISTICS
C METHOD (THOMPSON)
C
      IF(THOMFR) THEN
C
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE THOMPS'
      CALL THOMPS(HBOR%R,UBOR%R,VBOR%R,TBOR,U,V,T9,
     &            T,ZF,MESH%X%R,MESH%Y%R,MESH%NBOR%I,
     &            FRTYPE,T1,T2,T3,T4,T6,FU,FV,
     &            LIHBOR%I,LIUBOR%I,LIVBOR%I,LITBOR,IT1%I,
     &            T8,W1,IT2%I,
     &            CV2%R,CV3%R,TE1%R,TE2%R,HTILD%R,UTILD%R,VTILD%R,
     &            TTILD,TE3%R,
     &            MESH%SURDET%R,MESH%IKLE%I,CF,SMH,
     &            MESH%IFABOR%I,MESH%NULONE%I,NELEM,MESH,
     &            MESH%KP1BOR%I,MESH%XNEBOR%R,MESH%YNEBOR%R,
     &            NPOIN,NPTFR,
     &            LT,NIT,AT,DT,GRAV,DEBLIQ,FINLIQ,NTRAC,
     &            NFRLIQ,KSORT,MESH%LV,
     &            MSK,MASKEL,MASKPT,MESH%NELBOR%I,
     &            NELMAX,IELM,NORD,FAIR,WINDX,WINDY,VENT,HWIND,
     &            CORIOL,FCOR,SPHERI,OPTPRO,MAREE,MARDAT,MARTIM,
     &            PHI0,OPTSOU,ISCE,DSCE2,USCE,VSCE,T5%R,COUROU,NPTH,
     &            VARCL,NVARCL,VARCLA,NUMLIQ%I,BM1%X%R,UNSV2D,HFROT,
     &            FXWAVE,FYWAVE)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE THOMPS'
C
      ENDIF
C
C     CHECKS HBOR BECAUSE THE USER CAN MODIFY BORD AND MAKE A MISTAKE
      CALL CLIP(HBOR,0.D0,.TRUE.,1.D6,.FALSE.,0)
C
C=======================================================================
C
C LOOP OVER THE SUB-ITERATIONS WHERE ADVECTION AND PROPAGATION ARE UPDATED
C
C=======================================================================
C
      DO 701 ISOUSI = 1 , NSOUSI
      IF(DEBUG.GT.0) WRITE(LU,*) 'BOUCLE 701 ISOUSI=',ISOUSI
C
C=======================================================================
C
C : 4                     ADVECTION
C
C=======================================================================
C
      IF(CONV.AND.FTILD%N.GT.0) THEN
C
        IF(ENTET) CALL ENTETE(3,AT,LT)
C
        IF(SPHERI) THEN
          CALL OS('X=Y/Z   ',UCONV,UCONV,MESH%COSLAT,C)
          CALL OS('X=Y/Z   ',VCONV,VCONV,MESH%COSLAT,C)
        ENDIF
C
C       CALLS CHARAC
C
        CALL CHARAC( FNCAR , FTILD  , FTILD%N  , UCONV , VCONV,S,S,
     &               DT    , IFAMAS , IELM     , NPOIN , 1 , 1,
     &               MSK   , MASKEL , BM1%X    , BM1%D , TB   ,
     &               IT1%I , IT2%I  , IT3%I    , IT4%I , MESH ,
     &               MESH%NELEM,MESH%NELMAX,MESH%IKLE,MESH%SURDET)
C
        IF(SPHERI) THEN
          CALL OS('X=XY    ',UCONV,MESH%COSLAT,S,C)
          CALL OS('X=XY    ',VCONV,MESH%COSLAT,S,C)
        ENDIF
C
        IF(IELM1.NE.IELMH.AND.CONVV(2)) THEN
          CALL CHGDIS(HTILD,IELM1,IELMH,MESH)
        ENDIF
        IF(IELM1.NE.IELMU.AND.CONVV(1)) THEN
C         QUASI-BUBBLE POINTS OBTAINED BY INTERPOLATION
          IF(IELMU.EQ.12) THEN
            CALL CHGDIS(UTILD,IELM1,IELMU,MESH)
            CALL CHGDIS(VTILD,IELM1,IELMU,MESH)
          ENDIF
        ENDIF
        IF(NTRAC.GT.0.AND.IELM1.NE.IELMT.AND.CONVV(3)) THEN
          DO ITRAC=1,NTRAC
            CALL CHGDIS(TTILD%ADR(ITRAC)%P,IELM1,IELMT,MESH)
          ENDDO
        ENDIF
C
      ENDIF
C
C MANAGEMENT OF THE ARRAYS.
C
      CALL GESTIO(UN   ,VN   ,HN   ,TN   ,AKN   ,EPN   ,
     &            UTILD,VTILD,HTILD,TTILD,AKTILD,EPTILD,
     &            NTRAC.GT.0,PROPA,CONVV,ITURB,3)
C
C=======================================================================
C                       END OF ADVECTION
C=======================================================================
C=======================================================================
C
C : 6                DIFFUSION - PROPAGATION
C
C=======================================================================
C
      IF(PROPA) THEN
      IF(ENTET) CALL ENTETE(6,AT,LT)
C     INFORMATION ON THE METHOD OF RESOLUTION IS GIVEN ONLY
C     IF LISTING IS REQUESTED
      INFOGS=.FALSE.
      IF(INFOGR.AND.ENTET) INFOGS=.TRUE.
C
C  WEATHER CONDITIONS.
C
      IF(VENT.OR.ATMOS) THEN
        CALL METEO(PATMOS%R,WINDX%R,WINDY%R,
     &             FUAIR,FVAIR,MESH%X%R,MESH%Y%R,AT,LT,NPOIN,VENT,ATMOS,
     &             H%R,T1%R,GRAV,ROEAU,NORD,PRIVE)
      ENDIF
C
C  COMPUTES THE DENSITY WHEN IT IS VARIABLE
C
      IF(ROVAR) THEN
C       BEWARE, SALINITY MUST BE HERE THE FIRST TRACER
        CALL VALRO(RO,T,ROEAU)
      ENDIF
C
C  SOURCE TERMS DUE TO NOZZLES AND SIPHONS.
C
      IF(NSIPH.GT.0) THEN
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE SIPHON'
      CALL SIPHON(RELAXS,NSIPH,ENTSIP,SORSIP,GRAV,
     &            H%R,ZF%R,ISCE,DSCE,SECSCE,ALTSCE,CSSCE,CESCE,
     &            DELSCE,ANGSCE,LSCE,
     &            NTRAC,T,TSCE,USCE,VSCE,U%R,V%R,ENTET,MAXSCE)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE SIPHON'
      ENDIF
C
C  SOURCE TERMS FOR PROPAGATION.
C
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PROSOU'
      CALL PROSOU(FU,FV,SMH,UN,VN,HN,GRAV,NORD,
     &            FAIR,WINDX,WINDY,VENT,HWIND,
     &            CORIOL,FCOR,SPHERI,YASMH,
     &            MESH%COSLAT,MESH%SINLAT,AT,LT,
     &            NREJET,NREJEU,DSCE2,ISCE,T1,MESH,MSK,MASKEL,
     &            MAREE,MARDAT,MARTIM,PHI0,OPTSOU,COUROU,NPTH,
     &            VARCL,NVARCL,VARCLA,UNSV2D,FXWAVE,FYWAVE)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PROSOU'
C
C  PROPAGATION.
C
      IF(EQUA(1:15).EQ.'SAINT-VENANT EF'.OR.
     &   EQUA(1:10).EQ.'BOUSSINESQ') THEN
C
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PROPAG'
      CALL PROPAG
     &(U,V,H,UCONV,VCONV,CONVV,H0,C0,COTOND,PATMOS,ATMOS,
     & HPROP,UN,VN,HN,UTILD,VTILD,HTILD,DH,DU,DV,DHN,VISC,VISC_S,
     & FU,FV,
     & SMH,MESH,ZF,AM1,AM2,AM3,BM1,BM2,CM1,CM2,TM1,A23,A32,MBOR,
     & CV1,CV2,CV3,W1,UBOR,VBOR,AUBOR,HBOR,DIRBOR,
     & TE1,TE2,TE3,TE4,TE5,T1,T2,T3,T4,T5,T6,T7,T8,
     & LIMPRO,MASK,GRAV,ROEAU,CF,DIFVIT,IORDRH,IORDRU,LT,AT,DT,
     & TETAC,TETAHC,TETAU,TETAD,
     & AGGLOC,AGGLOU,KDIR,INFOGS,KFROT,ICONVF,
     & PRIVE,ISOUSI,BILMAS,MASSES,YASMH,OPTBAN,CORCON,
     & OPTSUP,MSK,MASKEL,MASKPT,RO,ROVAR,
     & MAT,RHS,UNK,TB,S,TB,PRECCU,SOLSYS,CFLMAX,OPDVIT,
C                       TB HERE TO REPLACE BD SUPPRESSED, NOT USED
     & OPTSOU,NFRLIQ,SLVPRO,EQUA,VERTIC,ADJO,ZFLATS,TETAZCOMP,
     & UDEL,VDEL,DM1,ZCONV,COUPLING,FLBOR,BM1S,BM2S,CV1S,
     & VOLU2D,V2DPAR,UNSV2D,NUMDIG%I,NWEIRS,NPSING,HFROT,
     & FLULIM,YAFLULIM)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PROPAG'
C
      IF(ADJO) THEN
C
       IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE PROPAG_ADJ'
       CALL PROPAG_ADJ
     &(UCONV,VCONV,CONVV,H0,C0,COTOND,PATMOS,ATMOS,
     & HPROP,UN,VN,HN,UTILD,VTILD,HTILD,DH,DU,DV,DHN,VISC,VISC_S,
     & FU,FV,SMH,MESH,ZF,AM1,AM2,AM3,BM1,BM2,CM1,CM2,TM1,A23,A32,
     & MBOR,CV1,CV2,CV3,W1,UBOR,VBOR,AUBOR,HBOR,DIRBOR,
     & TE1,TE2,TE3,TE4,TE5,T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,
     & LIMPRO,MASK,GRAV,ROEAU,CF,DIFVIT,IORDRH,IORDRU,LT,AT,DT,
     & TETAC,TETAHC,TETAU,TETAD,
     & AGGLOC,AGGLOU,KDIR,INFOGS,KFROT,ICONVF,
     & PRIVE,ISOUSI,BILMAS,MASSES,YASMH,OPTBAN,CORCON,
     & OPTSUP,MSK,MASKEL,MASKPT,RO,ROVAR,
     & MAT,RHS,UNK,TB,S,TB,PRECCU,SOLSYS,CFLMAX,OPDVIT,
     & OPTSOU,NFRLIQ,SLVPRO,EQUA,VERTIC,
     & ADJO,UD,VD,HD,U,V,H,UU,VV,HH,UIT1,VIT1,HIT1,PP,QQ,RR,
     & TAM1,TAM2,TAM3,TBM1,TBM2,TCM1,TCM2,MATADJ,UNKADJ,
     & ALPHA1,ALPHA2,ALPHA3,ADJDIR,ESTIME,OPTCOST,NIT,NVARRES,
     & VARSOR,T2D_FILES(T2DRES)%LU,T2D_FILES(T2DREF)%LU,
     & ALIRE,TROUVE,MAXVAR,VARCL,VARCLA,TEXTE,
     & TEXREF,TEXRES,W,OUTINI,CHESTR,KARMAN,NDEF,ITURB,LISRUG,
     & LINDNER,SB,DP,SP,CHBORD,CFBOR,HFROT,UNSV2D)
       IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE PROPAG_ADJ'
C
      ENDIF
C
      ELSEIF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
C
C      VOLFIN MAY CHANGE DT
C
C      CM1%D%R : HT
C
       IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE VOLFIN'
       CALL VOLFIN(W1%R,AT,DT,LT,NIT,NELEM,NPTFR,
     &      TB,ZF%R,CHESTR%R,NPOIN,HN%R,H%R,U%R,V%R,QU%R,QV%R,
     &      GRAV,ENTET,S,MSK,MASKEL,MESH,LIMPRO%I,
     &      MESH%NBOR%I,KDIR,KNEU,KDDL,HBOR%R,UBOR%R,VBOR%R,
     &      MASSES,FLUENT,FLUSOR,CFLWTD,DTVARI,KFROT,
     &      NREJET,ISCE,TSCE2,MAXSCE,MAXTRA,YASMH,SMH%R,
     &      NTRAC,T%ADR(1)%P%DIM1,T,HT,TN,
     &      LIMTRA%DIM1,LIMTRA%I,
     &      TBOR,MASSOU,FLUTENT,FLUTSOR,MESH%DTHAUT%R,
     &      MESH%DPX%R,MESH%DPY%R,CM1%X%R,CM2%X%R,
     &      MESH%CMI%R,MESH%JMI%I,TE1%R,TE2%R,
     &      DIFVIT,ITURB,PROPNU,DIFT,DIFNU,
     &      BM1%X%R,BM2%X%R,OPTVF,
     &      HSTOK%R,HCSTOK%R,LOGFR%I,DSZ%R,FLUXT,FLUHBOR,
     &      FLBOR,DTN,FLUSORTN,FLUENTN,
     &      LTT,FLUXTEMP,FLUHBTEMP,HC%R,SMTR,MESH%AIRST%R,
     &      TMAX,DTT)
       IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE VOLFIN'
C
       AT = AT + DT
       IF(AT.GE.TMAX) THEN
         NIT = LT
         IF(LISTIN) CALL ENTETE(1,AT,LT)
       ENDIF
C
      ELSE
C
        IF(LNG.EQ.1) WRITE(LU,*) 'EQUATIONS INCONNUES : ',EQUA
        IF(LNG.EQ.2) WRITE(LU,*) 'UNKNOWN EQUATIONS: ',EQUA
        CALL PLANTE(1)
        STOP
C
      ENDIF
C
C IF NO PROPAGATION :
C
      ELSE
C
C MANAGEMENT OF THE ARRAYS .
C
         CALL GESTIO(U    ,V    ,H    ,T,AK  ,EP ,
     &               UTILD,VTILD,HTILD,T,AK  ,EP ,
     &               NTRAC.GT.0,PROPA,CONVV,ITURB ,6)
C
C        SMH USED BY THE TRACER
C        TO SIMULATE SUBIEF TAKING OFF PROPAGATION
C        AND ADVECTION, PROSOU IS NOT CALLED AND DISCRETE
C        SOURCES ARE NOT TAKEN INTO ACCOUNT.
C        STRICTLY 'CALL PROSOU' SHOULD BE HERE.
         IF(NTRAC.GT.0) CALL OS('X=0     ',X=SMH)
C
      ENDIF
C
C     TREATMENT OF NEGATIVE DEPTHS
C
      CALL CORRECTION_DEPTH_2D(MESH%GLOSEG%I,MESH%GLOSEG%DIM1,
     &                         YAFLODEL,YASMH,YAFLULIM)
C
C=======================================================================
C                          END OF PROPAGATION
C=======================================================================
C
C  COMPUTES THE NEW ADVECTION FIELDS IF THERE REMAIN
C  SUB-ITERATIONS.
C
C  THE TEST ON ISOUSI IS MADE ONLY FOR HPROP AND NOT FOR UCONV
C  FOR REASONS OF TRACER MASS CONSERVATION (IT IS NECESSARY TO KEEP
C  THE SAME HPROP FOR THE TRACER AS THAT FOR H AND U)
C
      IF(ISOUSI.NE.NSOUSI) THEN
C      COMPUTES THE NEW PROPAGATION FIELD IF PROPAGATION
       IF(PROPA) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE HPROPA'
        CALL HPROPA(HPROP ,HN,H,PROLIN,HAULIN,TETAHC,NSOUSI)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE HPROPA'
       ENDIF
      ENDIF
C
C     COMPUTES THE NEW ADVECTION FIELD (IF ADVECTION)
      IF(CONV) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CHPCON'
        CALL CHPCON(UCONV,VCONV,U,V,UN,VN,TETAU)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CHPCON'
      ENDIF
C
C=======================================================================
C END OF THE LOOP OF THE SUB-ITERATIONS
C
701   CONTINUE
C
C=======================================================================
C
C : 5                 DIFFUSION OF THE TRACER
C
C=======================================================================
C
      IF(NTRAC.GT.0.AND.EQUA(1:15).NE.'SAINT-VENANT VF') THEN
C
      IF(ENTET) CALL ENTETE(5,AT,LT)
C
      DO ITRAC=1,NTRAC
C
C  BOUNDARY CONDITIONS FOR THE DIFFUSION OF THE TRACER.
C
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE DIFFCL POUR ITRAC=',ITRAC
      CALL DIFFCL(LITBOR%ADR(ITRAC)%P%I,
     &            TTILD%ADR(ITRAC)%P%R,TBOR%ADR(ITRAC)%P%R,
     &            MESH%NBOR%I,ICONVF(3),NPOIN,NPTFR)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE DIFFCL'
C
      ENDDO
C
C  SOURCE TERMS FOR DIFFUSION - SOURCE TERMS OF THE TRACER
C
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE DIFSOU'
      CALL DIFSOU(TEXP,TIMP,YASMI,TSCEXP,HPROP,TN,TETAT,NREJTR,
     &            ISCE,DSCE2,TSCE2,MAXSCE,MAXTRA,AT,DT,MASSOU,NTRAC,
     &            MESH%FAC%R)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE DIFSOU'
C
      DO ITRAC=1,NTRAC
C
C  CALLS THE STANDARD DIFFUSER. (CV1 IS THE SECOND MEMBER)
C
      INFOGT=INFOGR.AND.ENTET
C     HTILD: WORKING ARRAY WHERE HPROP IS RE-COMPUTED
C             (SAME ARRAY STRUCTURE)
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE CVDFTR SOLSYS=',SOLSYS
      IF(SOLSYS.EQ.1) THEN
      CALL CVDFTR(T%ADR(ITRAC)%P,TTILD%ADR(ITRAC)%P,TN%ADR(ITRAC)%P,
     &            TSCEXP%ADR(ITRAC)%P,
     &            DIFT,ICONVF(3),CONVV(3),H,HN,HTILD,TETAHC,
     &            UCONV,VCONV,DM1,ZCONV,SOLSYS,
     &            VISCT%ADR(ITRAC)%P,VISC_S,TEXP%ADR(ITRAC)%P,SMH,YASMH,
     &            TIMP%ADR(ITRAC)%P,YASMI(ITRAC),AM1,AM2,ZF,
     &            TBOR%ADR(ITRAC)%P,ATBOR%ADR(ITRAC)%P,
     &            BTBOR%ADR(ITRAC)%P,LIMTRA,MASKTR,MESH,W1,TB,
     &            T1,T2,T3,T4,T5,T6,T7,T10,TE1,TE2,TE3,
     &            KDIR,KDDL,KENT,
     &            DT,ENTET,TETAT,AGGLOT,INFOGT,BILMAS,OPTSUP(3),
     &            ISOUSI,LT,NIT,OPDTRA,OPTBAN,
     &            MSK,MASKEL,MASKPT,MBOR,S,MASSOU(ITRAC),
     &            OPTSOU,SLVTRA,FLBOR,V2DPAR,UNSV2D,2,FLBORTRA,
     &            FLULIM,YAFLULIM)
C
      ELSE
      CALL CVDFTR(T%ADR(ITRAC)%P,TTILD%ADR(ITRAC)%P,TN%ADR(ITRAC)%P,
     &            TSCEXP%ADR(ITRAC)%P,
     &            DIFT,ICONVF(3),CONVV(3),H,HN,HTILD,TETAHC,
     &            UDEL,VDEL,DM1,ZCONV,SOLSYS,
     &            VISCT%ADR(ITRAC)%P,VISC_S,TEXP%ADR(ITRAC)%P,SMH,YASMH,
     &            TIMP%ADR(ITRAC)%P,YASMI(ITRAC),AM1,AM2,ZF,
     &            TBOR%ADR(ITRAC)%P,ATBOR%ADR(ITRAC)%P,
     &            BTBOR%ADR(ITRAC)%P,LIMTRA,MASKTR,MESH,W1,TB,
     &            T1,T2,T3,T4,T5,T6,T7,T10,TE1,TE2,TE3,
     &            KDIR,KDDL,KENT,
     &            DT,ENTET,TETAT,AGGLOT,INFOGT,BILMAS,OPTSUP(3),
     &            ISOUSI,LT,NIT,OPDTRA,OPTBAN,
     &            MSK,MASKEL,MASKPT,MBOR,S,MASSOU(ITRAC),
     &            OPTSOU,SLVTRA,FLBOR,V2DPAR,UNSV2D,2,FLBORTRA,
     &            FLULIM,YAFLULIM)      
      ENDIF
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE CVDFTR'
C
      IF(BILMAS) THEN
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE BILANT'
      CALL BILANT(H,T2,T3,DT,LT,NIT,ENTET,
     &            T%ADR(ITRAC)%P,AGGLOT,MASSOU(ITRAC),MASTR0(ITRAC),
     &            MASTR2(ITRAC),MASTEN(ITRAC),
     &            MASTOU(ITRAC),MSK,MASKEL,MESH,FLBOR,NUMLIQ%I,
     &            NFRLIQ,NPTFR,NAMETRAC(ITRAC),FLBORTRA)
      ENDIF
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BILANT'
C
      ENDDO
C
      ENDIF
C
C=======================================================================
C                    END OF DIFFUSION OF THE TRACER
C=======================================================================
C
C
C
C
C=======================================================================
C           DIFFUSION AND SOURCE TERMS FOR K-EPSILON MODEL
C=======================================================================
C
      IF(ITURB.EQ.3.AND..NOT.ADJO) THEN
C
        IF (ENTET) CALL ENTETE(4,AT,LT)
C
C BEWARE THE MATRIX STRUCTURE (SYMMETRICAL OR NOT)
C WHEN CONSIDERING THE COUPLED SYSTEM K-E
C
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE KEPSIL'
        CALL KEPSIL(AK,EP,AKTILD,EPTILD,AKN,EPN,VISC,CF,U,V,H,
     &              UCONV,VCONV,KBOR,EBOR,LIMKEP%I,IELMK,IELME,
     &              CV1,CV2,TM1,BM1,BM2,CM2,TE1,TE2,NPTFR,DT,
     &              MESH,T1,T2,T3,TB,
     &              CMU,C1,C2,SIGMAK,SIGMAE,ESTAR,SCHMIT,
     &              KMIN,KMAX,EMIN,EMAX,INFOKE.AND.ENTET,
     &              KDIR,MSK,MASKEL,MASKPT,S,SLVK,SLVEP,
     &              ICONVF(4),OPTSUP(4))
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE KEPSIL'
C
      ENDIF
C
C=======================================================================
C  1)                 CHECKS MASS BALANCE
C=======================================================================
C
C CONTROL SECTIONS
C
      IF(NCP.NE.0.AND.(ENTET.OR.CUMFLO)) THEN
        CALL FLUSEC_TELEMAC2D(U,V,H,MESH%IKLE%I,MESH%XEL%R,MESH%YEL%R,
     &                        MESH%NELMAX,MESH%NELEM,
     &                        MESH%X%R,MESH%Y%R,DT,NCP,
     &                        CTRLSC,ENTET,AT,MESH%KNOGL%I,
     &                        MSKSEC,BM1,BM2,T1,HPROP,MESH,S,CV1,
     &                        MESH%IFABOR%I,COMFLU,CUMFLO)
      ENDIF
C
C MASS BALANCE
C
      IF(BILMAS) THEN
C
        CALL BILAN(MESH,H,T1,MASK,AT,DT,LT,NIT,ENTET,
     &             MASSES,MSK,MASKEL,EQUA,TE5,OPTBAN,
     &             MESH%NPTFR,FLBOR,
     &             FLUX_BOUNDARIES,NUMLIQ%I,NFRLIQ)
C
C       ADDED FOR THE KINETIC SCHEMES (TO BE CHECKED)
C
        IF(NTRAC.GT.0) THEN
          IF(EQUA(1:15).EQ.'SAINT-VENANT VF') THEN
C
            DO ITRAC=1,NTRAC
            CALL BILANT1(HSTOK,UCONV,VCONV,HPROP,T2,T3,T4,T5,T6,
     &                   DT,LT,NIT,ENTET,MASKTR,
     &                   T%ADR(1)%P,TN%ADR(1)%P,TETAT,
     &                   MASSOU(ITRAC),MSK,MASKEL,MESH,
     &                   FLUTSOR(ITRAC),FLUTENT(ITRAC),EQUA,LTT,ITRAC)
            ENDDO
C
          ENDIF
        ENDIF
C
      ENDIF
C
C=======================================================================
C                           DROGUE(S)
C=======================================================================
C
      IF(NFLOT.NE.0) THEN
C
        IF(ENTET) CALL ENTETE(12,AT,LT)
C
        IF(SPHERI) THEN
          CALL OS('X=Y/Z   ',UCONV,UCONV,MESH%COSLAT,C)
          CALL OS('X=Y/Z   ',VCONV,VCONV,MESH%COSLAT,C)
        ENDIF
C
        CALL DERIVE(UCONV%R,VCONV%R,DT,
     &              MESH%X%R,MESH%Y%R,MESH%IKLE%I,MESH%IFABOR%I,
     &              LT,IELM,3,NPOIN,
     &              NELEM,NELMAX,MESH%SURDET%R,XFLOT%R,YFLOT%R,
     &              SHPFLO%R,DEBFLO%I,FINFLO%I,ELTFLO%I,
     &              NFLOT,NITFLO,FLOPRD,T8%R)
C
        IF(SPHERI) THEN
          CALL OS('X=XY    ',UCONV,MESH%COSLAT,S,C)
          CALL OS('X=XY    ',VCONV,MESH%COSLAT,S,C)
        ENDIF
C
      ENDIF
C
C=======================================================================
C       OIL SPILL MODEL (UNDER DEVELOPMENT IN MYGRHYCAR PROJECT)
C=======================================================================
C
      IF(SPILL_MODEL) THEN
C
        CALL OIL_SPILL
C
      ENDIF
C
C=======================================================================
C                        LAGRANGIAN DRIFT(S)
C=======================================================================
C
      IF(NLAG.NE.0) THEN
C
        IF (ENTET) CALL ENTETE(13,AT,LT)
C
        CALL DERLAG(UCONV%R,VCONV%R,DT,MESH%X%R,MESH%Y%R,
     &              MESH%IKLE%I,MESH%IFABOR%I,LT,IELM,3,NPOIN,
     &              NELEM,NELMAX,MESH%SURDET%R,
     &              XLAG%R,YLAG%R,T1%R,T2%R,IT1%I,SHPLAG%R,
     &              DEBLAG%I,FINLAG%I,ELTLAG%I,NLAG,
     &              T7%R,T8%R,MESH%NBOR%I,MESH%NELBOR%I,
     &              MESH%NULONE%I,NPTFR,MSK,MASKEL%R,MASKPT%R,T8%R)
C
      ENDIF
C
C=======================================================================
C                     CREDIBILITY CHECKS
C                   LOOKS FOR A STEADY STATE
C=======================================================================
C
      ARRET1=.FALSE.
      IF(VERLIM) THEN
        CALL ISITOK(H%R,H%DIM1,U%R,U%DIM1,V%R,V%DIM1,NTRAC,
     &              T,T%ADR(1)%P%DIM1,
     &              MESH%X%R,MESH%Y%R,BORNES,ARRET1)
C       CORRECTION SUGGESTED BY NOEMIE DURAND (CHC-NRC) 04/01/2006
        IF(NCSIZE.GT.1) THEN
          STOP2=0
          IF(ARRET1) STOP2=1
          STOP2=P_IMAX(STOP2)
          IF(STOP2.EQ.1) ARRET1=.TRUE.
        ENDIF
      ENDIF
      ARRET2=.FALSE.
      IF(STOPER) THEN
        CALL STEADY(H%R,HN%R,H%DIM1,U%R,UN%R,U%DIM1,V%R,VN%R,
     &              V%DIM1,NTRAC,T,TN,T%ADR(1)%P%DIM1,
     &              CRIPER,ARRET2)
C       CORRECTION BY NOEMIE DURAND (CHC-NRC) 04/01/2006
        IF(NCSIZE.GT.1) THEN
          STOP2=0
          IF(ARRET2) STOP2=1
          STOP2=P_IMIN(STOP2)
          ARRET2=.NOT.(STOP2.EQ.0)
        ENDIF
      ENDIF
      IF(ARRET1.OR.ARRET2) THEN
        LEOPRD=1
        LISPRD=1
      ENDIF
C
      ARRET3=.FALSE.
      CALL TRAPSIG()
      IF(BREAKER) ARRET3=.TRUE.
C
      IF(ARRET1.OR.ARRET2.OR.ARRET3) THEN
        LEOPRD=1
        LISPRD=1
      ENDIF
C
C FH-BMD
C=============================================
C     FOR NEW COUPLING
888   CONTINUE
      IF (SISYPHE_CFD) CONSTFLOW_SIS = .TRUE.
999   CONTINUE
C=============================================
C FH-BMD

C
C=======================================================================
C                      WRITES OUT THE RESULTS
C=======================================================================
C
      IF(ADJO) THEN
C
        IF(T2D_FILES(T2DRBI)%NAME.NE.' '.AND.
     &     INCLU2(ESTIME,'DEBUG')) THEN
          CALL BIEF_DESIMP('SERAFIN ',VARSORA,
     &                     HIST,0,NPOIN,T2D_FILES(T2DRBI)%LU,
     &                     'STD',-AT,LT,LISPRD,1,
     &                     SORLEOA,SORIMPA,MAXVAR,TEXTE,PTINIG,PTINIL)
        ENDIF
C
      ELSE
C
        IF(CODE(1:7).EQ.'ESTEL3D') THEN
C
C         SAVES THE DEPTH FOR ESTEL3D
          CALL DEPTH_FILL(H%R)
C
C (NOTE THAT OUTPUTS ARE DONE WITHIN ESTEL3D IN COUPLED MODE)
C
        ELSE
          CALL PRERES_TELEMAC2D
          CALL BIEF_DESIMP(T2D_FILES(T2DRES)%FMT,VARSOR,
     &            HIST,0,NPOIN,T2D_FILES(T2DRES)%LU,'STD',AT,LT,
     &            LISPRD,LEOPRD,
     &            SORLEO,SORIMP,MAXVAR,TEXTE,PTINIG,PTINIL)
        ENDIF
C
C
        IF(INCLUS(COUPLING,'DELWAQ')) THEN
C
C         T3 : MODIFIED DEPTH TO TAKE INTO ACCOUNT MASS-LUMPING
C              IN THE CONTINUITY EQUATION
          IF(ABS(1.D0-AGGLOC).GT.1.D-8) THEN
            CALL VECTOR(T3,'=','MASVEC          ',IELMH,
     &                  1.D0-AGGLOC,H ,S,S,S,S,S,MESH,MSK,MASKEL)
            IF(NCSIZE.GT.1) CALL PARCOM(T3,2,MESH)
            CALL OS('X=XY    ',X=T3 ,Y=UNSV2D)
            CALL OS('X=X+CY  ',X=T3 ,Y=H ,C=AGGLOC)
          ELSE
            CALL OS('X=Y     ',X=T3 ,Y=H )
          ENDIF
C         FOR COMPUTATION OF THE FLUXES (CALL VECTOR BELOW)
          FORMUL='HUGRADP         '
          IF(SOLSYS.EQ.2) FORMUL(8:8)='2'
C
          CALL VECTOR(T4,'=',FORMUL,11,-1.D0,
     &                HPROP,DM1,ZCONV,UDEL,VDEL,VDEL,MESH,MSK,MASKEL)
          IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE TEL4DEL'
          CALL TEL4DEL(MESH%NPOIN,
     &    MESH%NPOIN,MESH%NELEM,MESH%NSEG,MESH%IKLE%I,MESH%ELTSEG%I,
     &    MESH%GLOSEG%I,MESH%ORISEG%I,MESH%GLOSEG%DIM1,
     &    MESH%X%R,MESH%Y%R,MESH%NPTFR,LIHBOR%I,MESH%NBOR%I,1,
     &    AT,DT,LT,NIT,T3%R,HPROP%R,T3%R,UDEL%R,VDEL%R,
     &    T%ADR(MAX(IND_S,1))%P%R,T%ADR(MAX(IND_T,1))%P%R,
     &    VISC%R,TITCAS,T2D_FILES(T2DGEO)%NAME,
     &    T2D_FILES(T2DCLI)%NAME,WAQPRD,
     &    T2DDL1,T2D_FILES(T2DDL1)%NAME,T2DDL2,T2D_FILES(T2DDL2)%NAME,
     &    T2DDL3,T2D_FILES(T2DDL3)%NAME,T2DDL5,T2D_FILES(T2DDL5)%NAME,
     &    T2DDL6,T2D_FILES(T2DDL6)%NAME,T2DDL7,T2D_FILES(T2DDL7)%NAME,
     &    T2DL11,T2D_FILES(T2DL11)%NAME,T2DDL4,T2D_FILES(T2DDL4)%NAME,
     &    T2DDL8,T2D_FILES(T2DDL8)%NAME,T2DDL9,T2D_FILES(T2DDL9)%NAME,
     &    T2DL10,T2D_FILES(T2DL10)%NAME,ENTET,NELEM,SALI_DEL,
     &    TEMP_DEL,VELO_DEL,DIFF_DEL,MARDAT,MARTIM,FLODEL%R,
     &    INIFLOW,MESH%W%R,.FALSE.,FLULIM%R,V2DPAR%R,MESH%KNOLG%I,
     &    MESH,MESH)
          IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE TEL4DEL'
C
        ENDIF
C
      ENDIF  !(ADJO)
C
C     OPTIONAL USER OUTPUT (COURTESY JACEK JANKOWSKI, BAW)
      IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE UTIMP_TELEMAC2D'
      CALL UTIMP_TELEMAC2D(LT,AT,PTINIG,LEOPRD,PTINIL,LISPRD)
      IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE UTIMP_TELEMAC2D'
C
C=======================================================================
C              COMPARISON AGAINST A REFERENCE FILE
C=======================================================================
C
C     THE VALIDA SUBROUTINE FROM THE BIEF LIBRARY IS STANDARD.
C     IT CAN BE MODIFIED BY THE USER FOR THEIR PARTICULAR CASE.
C     BUT THE CALL TO THE SUBROUTINE MUST STAY IN THE TIME LOOP.
C
      IF(VALID) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'APPEL DE BIEF_VALIDA'
        CALL BIEF_VALIDA(TB,TEXTPR,
     &                   T2D_FILES(T2DREF)%LU,T2D_FILES(T2DREF)%FMT,
     &                   VARSOR,TEXTE,
     &                   T2D_FILES(T2DRES)%LU,T2D_FILES(T2DRES)%FMT,
     &                   MAXVAR,NPOIN,LT,NIT,ALIRE)
        IF(DEBUG.GT.0) WRITE(LU,*) 'RETOUR DE BIEF_VALIDA'
      ENDIF
C
C=======================================================================
C
C  NEAT (PROGRAMMED) STOP OF THE MODEL:
C
      IF(ARRET1) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*)
          WRITE(LU,*) 'VALEURS LIMITES DEPASSEES, ARRET DE TELEMAC-2D'
          WRITE(LU,*)
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*)
          WRITE(LU,*) 'LIMIT VALUES TRESPASSED, TELEMAC-2D IS STOPPED'
          WRITE(LU,*)
        ENDIF
        RETURN
      ENDIF
      IF(ARRET2) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*)
          WRITE(LU,*) 'ETAT PERMANENT ATTEINT, ARRET DE TELEMAC-2D'
          WRITE(LU,*)
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*)
          WRITE(LU,*) 'STEADY STATE REACHED, TELEMAC-2D IS STOPPED'
          WRITE(LU,*)
        ENDIF
        RETURN
      ENDIF
      IF(ARRET3) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*)
          CALL ENTETE(1,AT,LT)
          WRITE(LU,*) 'TELEMAC-2D ARRETE PAR L''UTILISATEUR'
          WRITE(LU,*) 'AVEC SIGNAL ',SIGUSR1
          WRITE(LU,*)
        ENDIF
        IF(LNG.EQ.2) THEN
          CALL ENTETE(1,AT,LT)
          WRITE(LU,*)
          WRITE(LU,*) 'TELEMAC-2D CHECKPOINTED BY THE USER'
          WRITE(LU,*) 'USING SIGNAL ',SIGUSR1
          WRITE(LU,*)
        ENDIF
        RETURN
      ENDIF
C
C     NOW ADVECTION SCHEME WILL BE CHANGED AND FLULIM
C     WILL NOT CORRESPOND TO IT.
C
      YAFLULIM=.FALSE.
C
C
C 700: TIME LOOP
C
      IF(LT.LT.NIT) GO TO 700
C
C=======================================================================
C
C :                 /* END OF THE LOOP IN TIME */
C
C=======================================================================
C
      IF(LNG.EQ.1.AND.LISTIN) WRITE(LU,18)
      IF(LNG.EQ.2.AND.LISTIN) WRITE(LU,19)
18    FORMAT(/,1X,'FIN DE LA BOUCLE EN TEMPS',////)
19    FORMAT(/,1X,'END OF TIME LOOP',////)
C
C-----------------------------------------------------------------------
C
      IF (NFLOT.NE.0) CALL SORFLO
     &   (XFLOT%R,YFLOT%R,IKLFLO%I,DEBFLO%I,FINFLO%I,
     &    NFLOT,NITFLO,FLOPRD,T2D_FILES(T2DRBI)%LU,TITCAS,
     &    'STD',T2D_FILES(T2DRBI)%NAME,
     &    NIT,MAXVAR,MARDAT,MARTIM,MESH,I_ORIG,J_ORIG)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
