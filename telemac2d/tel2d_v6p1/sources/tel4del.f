C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COUPLES LNH-TELEMAC-3D TO DELFT-WAQ ONLINE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  JMH 12/06/2009 : THE COMPUTATION OF VERTICAL FLUXES IS EXACTLY
!>                       WHAT IS DONE IN TRIDW2. WE HAVE:
!>                       VERTICAL FLOW HERE = -WSS/UNSV2D IN TRIDW2.
!>                       CALLING TRIDW2 BEFORE AND USING WSS
!>                       WOULD PROBABLY SIMPLIFY A LOT HERE.

!>  @note  JMH : FLUXES OF SEGMENTS ARE POSITIVE IF THEY GO FROM
!>               POINT 1 TO POINT 2. THE VERTICAL SEGMENTS HERE
!>               ARE ORIENTED FROM TOP TO BOTTOM, SO A POSITIVE
!>               VALUE REPRESENTS A FLUX GOING DOWNWARD.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AAT, DDT, DIFF_DEL, ELTSEG, FLOW, FLULIM, GLOSEG, HNEW, HPROP, IKLE, INFOGR, INIFLOW, KNOLG, LIHBOR, LLT, MARDAT, MARTIM, MAXSEG, MESH2D, MESH3D, NBOR, NCOB, NCOU, NELEM, NELEM2, NINI, NMAB, NMAF, NNIT, NOLAY, NOMCOB, NOMCOU, NOMGEO, NOMINI, NOMLIM, NOMMAB, NOMMAF, NOMSAL, NOMSOU, NOMTEM, NOMVEB, NOMVEL, NOMVIS, NPOIN, NPOIN2, NPTFR, NSAL, NSEG, NSOU, NSTEPA, NTEM, NVEB, NVEL, NVIS, ORISEG, SALI, SALI_DEL, TEMP, TEMP_DEL, TITRE, U, V, V2DPAR, VELO_DEL, VISC, W, X, Y, YAFLULIM, ZNEW
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A1, A2, AREAWQ, ATOLD, DIST, DX, DY, ERR, ERRTOT, F, I, I2D, I3D, IBOR, IELEM, IFRM1, IFROM, IFROM1, INODE, IOPT1, ISEG, ISEGL, ISTEPA, ITO, ITOLD, ITOP1, ITOPL1, ITSTEP, ITSTOP, ITSTRT, J, K, LENGTH, MASINI, MBND, ND1, ND2, ND3, NLAY, NODENRS, NOQ, NOQ1, NOQ3, NOQF, NPTFR2, SSUM, STATIO, SUMAREA, SUMFLOW, SUMSALI, SUMTEMP, SUMVELO, SUMVISC, SURFACC, TRICK, VELO, VOL2, VOLOLD, VOLUME, W2D, X2, X3, Y2, Y3
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_TEL4DEL
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> FDNRST(), FLUX3DLIM(), FLUX_EF_VF(), FLUX_EF_VF_3D(), WRIHYD()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D(), TELEMAC3D()

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
!> </td><td> LEO POSTMA (DELFT HYDRAULICS); CHARLES MOULINEC (LNHE)
!> </td><td> WRITTEN IN FORTRAN 95 (CM)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 03/09/2009
!> </td><td>
!> </td><td> CALLS FLUX_EF_VF TO COMPUTE THE FLUXES
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 15/06/2009
!> </td><td> CHI-TUAN PHAM
!> </td><td> V2DPAR AND KNOLG ADDED, ADAPTED FOR PARALLELISM
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 05/04/2009
!> </td><td> LEO POSTMA
!> </td><td> BOUNDARY CELLS IN MODEL GRID  , LOOK FOR 'LP  05/04/2009'
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 27/03/2009
!> </td><td> JMH
!> </td><td> EXCHANGE AREAS, BUG CORRECTED,  LOOK FOR 'JMH 27/03/2009'
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 25/09/2008
!> </td><td>
!> </td><td> FLUXES NOW RECEIVED IN ARRAY W(NELEM,*) AND COMPUTED BEFORE BY TELEMAC-2D OR 3D
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 24/09/2008
!> </td><td> JMH
!> </td><td> F AND G VARIABLE IN TIME AND SPACE FOR GENERALISED SIGMA TRANSFORMATION
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 20/05/2008
!> </td><td>
!> </td><td> FLOW IS NOW AN ARGUMENT AND IS INITIALISED ONLY IF INIFLOW
!> <br>      IT MAY CONTAIN FLUXES DUE TO TIDAL FLATS TREATMENT
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 20/12/2007
!> </td><td> JMH
!> </td><td> RESFIL CHANGED TO NOMGEO, CLIFIL TO NOMLIM
!> <br>      FIRST DIMENSION OF STOSEG (MAXSEG) ADDED
!> <br>      MARDAT AND MARTIM ADDED
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 11/09/2007
!> </td><td> JMH
!> </td><td> SALINITY AND TEMPERATURE ADDED
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 18/05/2007
!> </td><td>
!> </td><td> LAST VERTICAL FLOWS LOOP
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 22/02/2007
!> </td><td>
!> </td><td> MODIFIED DURING LEO'S VISIT IN LNHE
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 14/11/2005
!> </td><td> LEO POSTMA
!> </td><td> MODIFIED, MAKING IT ONLINE
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 07/03/2005
!> </td><td> LEO POSTMA
!> </td><td> MODIFIED
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> **/11/2004
!> </td><td>
!> </td><td> ORIGINAL VERSION
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AAT,DDT
!></td><td>--></td><td>CURRENT TIME, TIME STEP
!>    </td></tr>
!>          <tr><td>DIFF_DEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ELTSEG
!></td><td>--></td><td>SEGMENTS COMPOSING AN ELEMENT
!>    </td></tr>
!>          <tr><td>FLOW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FLULIM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>GLOSEG
!></td><td>--></td><td>GLOBAL NUMBERS OF POINTS OF A SEGMENT
!>    </td></tr>
!>          <tr><td>HNEW
!></td><td>--></td><td>DEPTH AT NEW TIME (2D) ELEVATION Z (3D)
!>    </td></tr>
!>          <tr><td>HPROP
!></td><td>--></td><td>DEPTH IN THE DIV(HU) TERM
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>CONNECTIVITY TABLE
!>    </td></tr>
!>          <tr><td>INFOGR
!></td><td>--></td><td>IF YES, INFORMATION PRINTED ON LISTING
!>    </td></tr>
!>          <tr><td>INIFLOW
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>KNOLG
!></td><td>--></td><td>GLOBAL NUMBERS OF LOCAL POINTS IN PARALLEL
!>    </td></tr>
!>          <tr><td>LIHBOR
!></td><td>--></td><td>TYPE OF 2D BOUNDARIES FOR DEPTH
!>    </td></tr>
!>          <tr><td>LLT,NNIT
!></td><td>--></td><td>ITERATION NUMBER,NUMBER OF ITERATIONS
!>    </td></tr>
!>          <tr><td>MARDAT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MARTIM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MAXSEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH3D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>GLOBAL NUMBERS OF BOUNDARY NODES
!>    </td></tr>
!>          <tr><td>NCOB,NOMCOB
!></td><td>--></td><td>DELWAQ STEERING FILE CANAL AND FILE
!>    </td></tr>
!>          <tr><td>NCOU,NOMCOU
!></td><td>--></td><td>FLUX CANAL AND FILE
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NUMBER OF ELEMENTS IN THE MESH
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NUMBER OF ELEMENTS IN 2D
!>    </td></tr>
!>          <tr><td>NINI,NOMINI
!></td><td>--></td><td>HORIZONTAL SURFACE CANAL AND FILE
!>    </td></tr>
!>          <tr><td>NMAB,NOMMAB
!></td><td>--></td><td>AREA CANAL AND FILE
!>    </td></tr>
!>          <tr><td>NMAF,NOMMAF
!></td><td>--></td><td>NODE DISTANCE CANAL AND FILE
!>    </td></tr>
!>          <tr><td>NOLAY
!></td><td>--></td><td>NUMBER OF PLANES
!>    </td></tr>
!>          <tr><td>NOMGEO
!></td><td>--></td><td>RESULT FILE OF THE SIMULATION
!>    </td></tr>
!>          <tr><td>NOMLIM
!></td><td>--></td><td>BOUNDARY FILE OF THE SIMULATION
!>    </td></tr>
!>          <tr><td>NOMVEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NOMVIS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NUMBER OF 3D POINTS IN THE MESH
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NUMBER OF 2D POINTS IN THE MESH
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NUMBER OF 3D BOUNDARY POINTS
!>    </td></tr>
!>          <tr><td>NSAL,NOMSAL
!></td><td>--></td><td>SALINITY FOR DELWAQ, CANAL AND FILE
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>--></td><td>NUMBER OF 2D SEGMENTS IN THE MESH
!>    </td></tr>
!>          <tr><td>NSOU,NOMSOU
!></td><td>--></td><td>VOLUME CANAL AND FILE
!>    </td></tr>
!>          <tr><td>NSTEPA
!></td><td>--></td><td>NUMBER OF TIME-STEPS FOR TIME AGGREGATION
!>    </td></tr>
!>          <tr><td>NTEM,NOMTEM
!></td><td>--></td><td>TEMPERATURE FOR DELWAQ, CANAL AND FILE
!>    </td></tr>
!>          <tr><td>NVEB,NOMVEB
!></td><td>--></td><td>NODE EXCHANGE CANAL AND FILE
!>    </td></tr>
!>          <tr><td>NVEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NVIS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ORISEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SALI,TEMP
!></td><td>--></td><td>SALINITY, TEMPERATURE (IF SALI_DEL, IF TEMP_DEL)
!>    </td></tr>
!>          <tr><td>SALI_DEL
!></td><td>--></td><td>IF YES, THERE IS SALINITY
!>    </td></tr>
!>          <tr><td>TEMP_DEL
!></td><td>--></td><td>IF YES, THERE IS TEMPERATURE
!>    </td></tr>
!>          <tr><td>TITRE
!></td><td>--></td><td>TITLE OF STUDY
!>    </td></tr>
!>          <tr><td>U,V
!></td><td>--></td><td>COMPONENTS OF HORIZONTAL VELOCITY
!>    </td></tr>
!>          <tr><td>V2DPAR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VELO_DEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VISC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>W
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDINATES OF HORIZONTAL MESH
!>    </td></tr>
!>          <tr><td>YAFLULIM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>ZNEW
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE TEL4DEL
     &(NPOIN,NPOIN2,NELEM,NSEG,IKLE,ELTSEG,GLOSEG,ORISEG,MAXSEG,
     & X,Y,NPTFR,LIHBOR,NBOR,NOLAY,AAT,DDT,LLT,NNIT,HNEW,HPROP,ZNEW,
     & U,V,SALI,TEMP,VISC,TITRE,NOMGEO,NOMLIM,NSTEPA,
     & NSOU,NOMSOU,NMAB,NOMMAB,NCOU,NOMCOU,NINI,NOMINI,NVEB,NOMVEB,
     & NMAF,NOMMAF,NCOB,NOMCOB,NSAL,NOMSAL,NTEM,NOMTEM,NVEL,NOMVEL,
     & NVIS,NOMVIS,INFOGR,NELEM2,SALI_DEL,TEMP_DEL,VELO_DEL,DIFF_DEL,
     & MARDAT,MARTIM,FLOW,INIFLOW,W,YAFLULIM,FLULIM,V2DPAR,KNOLG,
     & MESH2D,MESH3D)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AAT,DDT        |-->| CURRENT TIME, TIME STEP
C| DIFF_DEL       |---| 
C| ELTSEG         |-->| SEGMENTS COMPOSING AN ELEMENT
C| FLOW           |---| 
C| FLULIM         |---| 
C| GLOSEG         |-->| GLOBAL NUMBERS OF POINTS OF A SEGMENT
C| HNEW           |-->| DEPTH AT NEW TIME (2D) ELEVATION Z (3D)
C| HPROP          |-->| DEPTH IN THE DIV(HU) TERM
C| IKLE           |-->| CONNECTIVITY TABLE
C| INFOGR         |-->| IF YES, INFORMATION PRINTED ON LISTING
C| INIFLOW        |---| 
C| KNOLG          |-->| GLOBAL NUMBERS OF LOCAL POINTS IN PARALLEL
C| LIHBOR         |-->| TYPE OF 2D BOUNDARIES FOR DEPTH
C| LLT,NNIT       |-->| ITERATION NUMBER,NUMBER OF ITERATIONS
C| MARDAT         |---| 
C| MARTIM         |---| 
C| MAXSEG         |---| 
C| MESH2D         |---| 
C| MESH3D         |---| 
C| NBOR           |-->| GLOBAL NUMBERS OF BOUNDARY NODES
C| NCOB,NOMCOB    |-->| DELWAQ STEERING FILE CANAL AND FILE
C| NCOU,NOMCOU    |-->| FLUX CANAL AND FILE
C| NELEM          |-->| NUMBER OF ELEMENTS IN THE MESH
C| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D
C| NINI,NOMINI    |-->| HORIZONTAL SURFACE CANAL AND FILE
C| NMAB,NOMMAB    |-->| AREA CANAL AND FILE
C| NMAF,NOMMAF    |-->| NODE DISTANCE CANAL AND FILE
C| NOLAY          |-->| NUMBER OF PLANES
C| NOMGEO         |-->| RESULT FILE OF THE SIMULATION
C| NOMLIM         |-->| BOUNDARY FILE OF THE SIMULATION
C| NOMVEL         |---| 
C| NOMVIS         |---| 
C| NPOIN          |-->| NUMBER OF 3D POINTS IN THE MESH
C| NPOIN2         |-->| NUMBER OF 2D POINTS IN THE MESH
C| NPTFR          |-->| NUMBER OF 3D BOUNDARY POINTS
C| NSAL,NOMSAL    |-->| SALINITY FOR DELWAQ, CANAL AND FILE
C| NSEG           |-->| NUMBER OF 2D SEGMENTS IN THE MESH
C| NSOU,NOMSOU    |-->| VOLUME CANAL AND FILE
C| NSTEPA         |-->| NUMBER OF TIME-STEPS FOR TIME AGGREGATION
C| NTEM,NOMTEM    |-->| TEMPERATURE FOR DELWAQ, CANAL AND FILE
C| NVEB,NOMVEB    |-->| NODE EXCHANGE CANAL AND FILE
C| NVEL           |---| 
C| NVIS           |---| 
C| ORISEG         |---| 
C| SALI,TEMP      |-->| SALINITY, TEMPERATURE (IF SALI_DEL, IF TEMP_DEL)
C| SALI_DEL       |-->| IF YES, THERE IS SALINITY
C| TEMP_DEL       |-->| IF YES, THERE IS TEMPERATURE
C| TITRE          |-->| TITLE OF STUDY
C| U,V            |-->| COMPONENTS OF HORIZONTAL VELOCITY
C| V2DPAR         |---| 
C| VELO_DEL       |---| 
C| VISC           |---| 
C| W             |---| 
C| X,Y            |-->| COORDINATES OF HORIZONTAL MESH
C| YAFLULIM       |---| 
C| ZNEW           |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE INTERFACE_TELEMAC2D, EX_TEL4DEL => TEL4DEL
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)            :: NPOIN,NPOIN2,NELEM,NSEG,NPTFR
      INTEGER, INTENT(IN)            :: NOLAY,LLT,NNIT,NELEM2
      INTEGER, INTENT(IN)            :: MAXSEG,MARDAT(3),MARTIM(3)
      INTEGER, INTENT(INOUT)         :: NSTEPA
      INTEGER, INTENT(IN)            :: IKLE(NELEM2,3),LIHBOR(*)
      INTEGER, INTENT(IN)            :: ELTSEG(NELEM2,3)
      INTEGER, INTENT(IN)            :: ORISEG(NELEM2,3)
      INTEGER, INTENT(IN)            :: GLOSEG(MAXSEG,2)
      INTEGER, INTENT(IN)            :: NBOR(*),KNOLG(NPOIN2)
      INTEGER, INTENT(IN)            :: NSOU,NMAB,NCOU,NINI,NVEL,NVIS
      INTEGER, INTENT(IN)            :: NVEB,NMAF,NCOB,NSAL,NTEM
      DOUBLE PRECISION  , INTENT(IN) :: X(NPOIN2),Y(NPOIN2),ZNEW(NPOIN)
      DOUBLE PRECISION  , INTENT(IN) :: HPROP(NPOIN2),HNEW(NPOIN2)
      DOUBLE PRECISION  , INTENT(IN) :: AAT,DDT,V2DPAR(NPOIN2)
      DOUBLE PRECISION  , INTENT(IN) :: U(NPOIN),V(NPOIN),FLULIM(NSEG)
      DOUBLE PRECISION  , INTENT(IN) :: SALI(NPOIN),TEMP(NPOIN)
      DOUBLE PRECISION  , INTENT(IN) :: VISC(NPOIN)
C                                               NSEG EN 2D, NOQ EN 3D
      DOUBLE PRECISION  , INTENT(INOUT) :: FLOW(*),W(NELEM,*)
      CHARACTER(LEN=72) , INTENT(IN) :: TITRE
      CHARACTER(LEN=144), INTENT(IN) :: NOMSOU,NOMMAB,NOMCOU,NOMINI
      CHARACTER(LEN=144), INTENT(IN) :: NOMVEB,NOMMAF,NOMCOB,NOMSAL
      CHARACTER(LEN=144), INTENT(IN) :: NOMTEM,NOMGEO,NOMLIM,NOMVEL
      CHARACTER(LEN=144), INTENT(IN) :: NOMVIS
      LOGICAL           , INTENT(IN) :: INFOGR,SALI_DEL,TEMP_DEL,INIFLOW
      LOGICAL           , INTENT(IN) :: VELO_DEL,DIFF_DEL,YAFLULIM
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH2D,MESH3D
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ERR,IOPT1
      INTEGER ISTEPA            ! I  ITERATION NUMBER FOR TIME AGGREGATION
C
      INTEGER ITSTRT            ! O  STARTTIME
      INTEGER ITSTOP            ! O  STOPTIME
      INTEGER ITSTEP            ! O  TIMESTEPSIZE
C
      LOGICAL TRICK             ! I  IF TRUE, TRICK FOR DDT
C
C     LOCAL ALLOCATABLE ARRAYS:                DESCRIPTION
C
      INTEGER, ALLOCATABLE :: NODENRS(:)  ! WAQ ARRAY FOR OPEN BOUNDARIES
      INTEGER, ALLOCATABLE :: IFROM1 (:)  ! FROM MINUS 1 NODE-NRS OF AN EXCHANGE  LP 05/04/2009
      INTEGER, ALLOCATABLE :: ITOPL1 (:)  ! TO PLUS 1 NODE-NRS OF AN EXCHANGE     LP 05/04/2009
      DOUBLE PRECISION , ALLOCATABLE :: VOLOLD (:)  ! WAQ 3D VOLUMES PREV. TIME LEVEL
      DOUBLE PRECISION , ALLOCATABLE :: VOLUME (:)  ! WAQ 3D VOLUMES
      DOUBLE PRECISION , ALLOCATABLE :: VOL2   (:)  ! WAQ 2D TOTAL VOLUME WORK ARRAY
      DOUBLE PRECISION , ALLOCATABLE :: AREAWQ (:)  ! WAQ EXCHANGE SURFACES OF LINKS
      DOUBLE PRECISION , ALLOCATABLE :: VELO   (:)  ! WAQ 3D VELOCITIES AT NODES
      DOUBLE PRECISION , ALLOCATABLE :: LENGTH(:,:) ! WAQ 3D DISTANCES OF NODES
      DOUBLE PRECISION , ALLOCATABLE :: DIST  (:,:) ! LENGTH OF GRAVITY LINE FROM POINT I
      DOUBLE PRECISION , ALLOCATABLE :: F(:,:)      ! ARRAY TO STORE FRACTION OF DEPTH PER LAYER
      DOUBLE PRECISION , ALLOCATABLE :: SUMAREA(:)  ! FOR TIME INTEGRATION
      DOUBLE PRECISION , ALLOCATABLE :: SUMFLOW(:)  ! FOR TIME INTEGRATION
      DOUBLE PRECISION , ALLOCATABLE :: SUMSALI(:)  ! FOR TIME INTEGRATION
      DOUBLE PRECISION , ALLOCATABLE :: SUMTEMP(:)  ! FOR TIME INTEGRATION
      DOUBLE PRECISION , ALLOCATABLE :: SUMVISC(:)  ! FOR TIME INTEGRATION
      DOUBLE PRECISION , ALLOCATABLE :: SUMVELO(:)  ! FOR TIME INTEGRATION
      DOUBLE PRECISION , ALLOCATABLE :: W2D(:,:)    ! FOR FLUXES IN 3D
C
C     LOCAL SINGLE VARIABLES OR ARRAYS WITH FIXED LENGTH
C
      INTEGER NPTFR2                    !  NUMBER OF 2D BOUNDARY POINTS
      INTEGER STATIO                    !  IF 1, THEN STATIONARY OUTPUT
      INTEGER NLAY                      !  NUMBER OF LAYERS AND LOOP COUNTER OF IT
      INTEGER ISEG, ISEGL               !  LOOP COUNTER TELEMAC SEGMENTS, VALUE AT A LAYER
      INTEGER NOQ1 , NOQ3 , NOQ , NOQF  !  NUMBER OF FLOWS IN 3 DIRECTIONS + TOTAL   LP 05/04/2009
      INTEGER IFROM, ITO , IFRM1, ITOP1 !  FROM AND TO NODE-NRS OF AN EXCHANGE
      INTEGER IELEM                     !  LOOP COUNTER TELEMAC ELEMENTS
      INTEGER INODE                     !  LOOP COUNTER TELEMAC NODES
      INTEGER IBOR, MBND                !  LOOP AND SEQUENTIAL COUNTER OPEN BOUNDARIES
      DOUBLE PRECISION  DX, DY          !  DISTANCES IN X AND Y BETWEEN NODES
      DOUBLE PRECISION  ATOLD           !  TIME IN THE FILE, ITS OLD VALUE
      INTEGER I,J,I2D,I3D ,K            !  GENERAL, MULTI USE LOOP COUNTER
      INTEGER ND1 , ND2 , ND3           !  2D NODE NUMBER HELP VARIABLES WITHIN LOOPS
      DOUBLE PRECISION A1 , A2          !  PROJECTIONS OF THE ELEMENT VELOCITY
      DOUBLE PRECISION SSUM,MASINI,ERRTOT
      DOUBLE PRECISION X2, Y2, X3, Y3
      DOUBLE PRECISION SURFACC
C
      INTEGER ITOLD
C
      INTRINSIC MAX,REAL,ABS
C
      SAVE
C
C-----------------------------------------------------------------------
C
      IF(DDT.LT.1.D0) THEN
        TRICK=.TRUE.
      ELSE
        TRICK=.FALSE.
      ENDIF
C
C     FIRST CALL: INITIALISATIONS AND MEMORY ALLOCATION
C
      IF(LLT.EQ.0) THEN
C
         NPTFR2 = NPTFR/NOLAY
C        THE WRITING OF EXCHANGE POINTERS IS MOVED TO HERE   LP 05/04/2009
C
C        DERIVES THE OPEN BOUNDARY NODES AND NUMBERS THEM NEGATIVELY
C
C        NODENRS : IF NOT OPEN BOUNDARY = NODE NUMBER
C                  IF     OPEN BOUNDARY = - (OPEN BOUNDARY NODE NUMBER)
C
         ALLOCATE( NODENRS(NPOIN2) ,STAT=ERR)
         DO INODE = 1, NPOIN2
           NODENRS(INODE) = INODE
         ENDDO
         MBND = 0
         DO IBOR = 1, NPTFR2
            IF ( LIHBOR(IBOR) .NE. 2 ) THEN
               MBND = MBND + 1
               NODENRS(NBOR(IBOR)) = -MBND
            ENDIF
         ENDDO
C        BECAUSE MBND EXCHANGES ARE ADDED                              LP 05/04/2009
C
C        DERIVES THE HORIZONTAL PART (NOQ1) OF THE FROM-TO EXCHANGE TABLE
C            FOR COMPUTATIONAL ELEMENTS. THE ELEMENT NUMBERS AT THE LAYERS
C            DIFFER NPOIN2 IN IN COMPUTATIONAL ELEMENT NUMBER AND MBND IN
C            BOUNDARY NUMBER (MBND ITSELF IS POSITIVE)
C        COMPUTES HORIZONTAL 'FROM' AND 'TO' HALF DISTANCES ON THE FLY
C            THEY ARE THE SAME FOR ALL LAYERS, SO DO ONLY FOR LAYER 1
C                                                                    ! LP 05/04/2009
         NOQ1 =  NOLAY   *(NSEG + MBND)  ! NUMBER OF FLOWS IN FIRST DIRECTION
         NOQ3 = (NOLAY-1)*NPOIN2         ! NUMBER OF VERTICAL FLOW TERMS
         NOQ  = NOQ1 + NOQ3              ! TOTAL NUMBER OF FLOW TERMS FOR WAQ
         NOQF = NOLAY*NSEG + NOQ3        ! OLD NOQ FOR FLOW ARRAY    ! LP 05/04/2009
C
C        ALLOCATES ALL THE ARRAYS USED IN THE SUBROUTINE
C
         ALLOCATE( DIST(3,NELEM2)  ,STAT=ERR)
         ALLOCATE( LENGTH (2,NSEG+MBND) ,STAT=ERR)                   ! LP 05/04/2009
         ALLOCATE( W2D(NELEM2,3)   ,STAT=ERR)
         ALLOCATE( IFROM1(MAXSEG)  ,STAT=ERR)                        ! LP 05/04/2009
         ALLOCATE( ITOPL1(MAXSEG)  ,STAT=ERR)                        ! LP 05/04/2009
C
C        ARRAYS FOR ALL TRANSPORT AND OTHER ADVECTION DIFFUSION TERMS
C
         ALLOCATE( F(NPOIN2,NOLAY)      ,STAT=ERR)
         ALLOCATE( VELO(NPOIN)   ,STAT=ERR)
         ALLOCATE( VOL2(NPOIN2)  ,STAT=ERR)
         ALLOCATE( VOLUME(NPOIN) ,STAT=ERR)
         ALLOCATE( AREAWQ(NOQ)   ,STAT=ERR)
         ALLOCATE( VOLOLD(NPOIN) ,STAT=ERR)
         ALLOCATE( SUMAREA(NOQ)  ,STAT=ERR)
         SUMAREA = 0.D0
         ALLOCATE( SUMFLOW(NOQ)  ,STAT=ERR)
         SUMFLOW = 0.D0
         IF(SALI_DEL) THEN
           ALLOCATE( SUMSALI(NPOIN),STAT=ERR)
           SUMSALI = 0.D0
         ENDIF
         IF(TEMP_DEL) THEN
           ALLOCATE( SUMTEMP(NPOIN),STAT=ERR)
           SUMTEMP = 0.D0
         ENDIF
         IF(VELO_DEL) THEN
           ALLOCATE( SUMVELO(NPOIN),STAT=ERR)
           SUMVELO = 0.D0
         ENDIF
         IF(DIFF_DEL) THEN
           ALLOCATE( SUMVISC(NPOIN),STAT=ERR)
           SUMVISC = 0.D0
         ENDIF
C
      ENDIF
C
      IF(LLT.EQ.0) THEN
C
         STATIO = 0
C
C        COMPUTES THE AREA ASSOCIATED WITH EACH NODE
C        AS 1/3 OF EVERY NEIGHBOURING TRIANGLE
C
C        COMPUTES 1/3 OF THE HEIGHT OF THE TRIANGLES FROM NODE I
C        HEIGHT = 2*SURFACE/(LENGTH OF THE SEGMENT)
C
         DO IELEM=1,NELEM2
           ND1 = IKLE(IELEM,1)
           ND2 = IKLE(IELEM,2)
           ND3 = IKLE(IELEM,3)
           X2=X(ND2)-X(ND1)
           X3=X(ND3)-X(ND1)
           Y2=Y(ND2)-Y(ND1)
           Y3=Y(ND3)-Y(ND1)
           SURFACC=0.5D0*(X2*Y3-X3*Y2)
           DO I = 1 , 3
             ISEG = ELTSEG(IELEM,I)
             A1   = X(GLOSEG(ISEG,1))-X(GLOSEG(ISEG,2))
             A2   = Y(GLOSEG(ISEG,1))-Y(GLOSEG(ISEG,2))
             DIST(I,IELEM)=2.D0*SURFACC/SQRT(A1**2+A2**2)/3.D0
           ENDDO
         ENDDO
C
C        CHECKS THE INITIAL MASS
C
C        MASINI=0.D0
C        DO I=1,NPOIN2
C          MASINI=MASINI+V2DPAR(I)*HNEW(I)
C        ENDDO
C        WRITE(LU,*) 'INITIAL MASS=',MASINI
C
         IF(NCSIZE.GT.0) THEN
           WRITE(NINI) 'AREA2D '
           WRITE(NINI) NOLAY
         ENDIF
C
         WRITE(NINI) NPOIN2,0,NPOIN2,NPOIN2,NPOIN2,0
         WRITE(NINI) (REAL(V2DPAR(I)),I=1,NPOIN2)
C
         IF(NCSIZE.GT.0) THEN
           WRITE(NVEB) 'IFRMTO '
           WRITE(NVEB) NOLAY
           WRITE(NMAF) 'LENGTH '
           WRITE(NMAF) NOLAY
         ENDIF
C
C        THE WRITING OF EXCHANGE POINTERS IS CHANGED       *START*     LP 05/04/2009
         DO NLAY  = 1, NOLAY
            DO ISEG  = 1, NSEG
               IFROM = GLOSEG(ISEG,1)
               ITO   = GLOSEG(ISEG,2)
               IF ( NLAY .EQ. 1 ) THEN
                  CALL FDNRST(IFROM,ITO,X,Y,NODENRS,NPOIN2,
     &                                  IFROM1(ISEG),ITOPL1(ISEG))
                  DX = X(GLOSEG(ISEG,1)) - X(GLOSEG(ISEG,2))
                  DY = Y(GLOSEG(ISEG,1)) - Y(GLOSEG(ISEG,2))
                  LENGTH(1,ISEG) = SQRT(DX**2+DY**2)*0.5D0
                  LENGTH(2,ISEG) = LENGTH(1,ISEG)
                  IF ( IFROM1(ISEG) .LT. 0 .AND.              !  *START*  LP 24/04/2009
     &                 IFROM1(ISEG) .NE. NODENRS(IFROM) ) THEN
                     DO I = 1,NPOIN2
                        IF ( NODENRS(I) .EQ. IFROM1(ISEG) ) THEN
                           IFROM1(ISEG) = I
                           EXIT
                        ENDIF
                     ENDDO
                  ENDIF
                  IF ( ITOPL1(ISEG) .LT. 0 .AND.
     &                 ITOPL1(ISEG) .NE. NODENRS(ITO  ) ) THEN
                     DO I = 1,NPOIN2
                        IF ( NODENRS(I) .EQ. ITOPL1(ISEG) ) THEN
                           ITOPL1(ISEG) = I
                           EXIT
                        ENDIF
                     ENDDO
                  ENDIF                                       !  **END**  LP 24/04/2009
               ENDIF
               IFRM1 = IFROM1(ISEG)
               ITOP1 = ITOPL1(ISEG)
               IFROM = IFROM + (NLAY-1)*NPOIN2
               IF ( IFRM1 .GT. 0 ) THEN
                  IFRM1 = IFRM1 + (NLAY-1)*NPOIN2
               ELSE
                  IFRM1 = IFRM1 - (NLAY-1)*MBND                      ! LP 24/04/2009
               ENDIF
               ITO   = ITO   + (NLAY-1)*NPOIN2
               IF ( ITOP1 .GT. 0 ) THEN
                  ITOP1 = ITOP1 + (NLAY-1)*NPOIN2
               ELSE
                  ITOP1 = ITOP1 - (NLAY-1)*MBND                      ! LP 24/04/2009
               ENDIF
               WRITE ( NVEB ) IFROM,ITO,IFRM1,ITOP1
            ENDDO
            DO IBOR = 1, NPTFR2                                      ! LP 05/04/2009
               IF ( LIHBOR(IBOR) .NE. 2 ) THEN                       ! OPEN BOUNDARY
                  IFROM = NODENRS(NBOR(IBOR))                        ! EXCHANGES ADDED
                  ITO   = NBOR(IBOR)
                  IF ( NLAY .EQ. 1 ) THEN
C                    11/06/2009 CHI-TUAN PHAM, CORRECTED BUG
C                    ISEG WAS USED INSTEAD OF NSEG
                     LENGTH(1,NSEG-IFROM) = 10.D0                   ! DUMMY LENGTH
                     LENGTH(2,NSEG-IFROM) = 10.D0
                  ENDIF
                  IFRM1 = IFROM
                  ITOP1 = ITO
                  IFROM = IFROM - (NLAY-1)*MBND
                  IFRM1 = IFRM1 - (NLAY-1)*MBND
                  ITO   = ITO   + (NLAY-1)*NPOIN2
                  ITOP1 = ITOP1 + (NLAY-1)*NPOIN2
                  WRITE ( NVEB ) IFROM,ITO,IFRM1,ITOP1
               ENDIF
            ENDDO
C        THE WRITING OF EXCHANGE POINTERS IS CHANGED       **END**     LP 05/04/2009
         ENDDO
C
C        WRITES ALL HORIZONTAL 'FROM' 'TO' HALF LENGTHS
C
         WRITE(NMAF) 0
         DO NLAY = 1, NOLAY
           WRITE(NMAF) ((REAL(LENGTH(I,J)),I=1,2),J=1,NSEG+MBND)     ! LP 05/04/2009
         ENDDO
C
C        DERIVES THE FROM-TO EXCHANGE TABLE FOR COMPUTATIONAL ELEMENTS
C        VERTICALLY FOR ALL LAYERS. THE LAYERS DIFFER NPOIN2 IN
C        COMPUTATIONAL ELEMENT NUMBER. BOUNDARY NODES HAVE NO VERTICAL FLOW
C        WRITES 1.0 FOR THE VERTICAL 'FROM' AND 'TO' HALFDISTANCES
C        THEY ARE UPDATED BY WAQ TO BECOME VOLUME/AREA/2.0 DURING
C        SIMULATION TIME, SINCE VERTICAL DISTANCES CHANGE WITH VOLUME.
C
         DO NLAY = 1, NOLAY-1
            DO INODE = 1, NPOIN2
C        THE WRITING OF EXCHANGE POINTERS IS CHANGED       *START*     LP 05/04/2009
               IFROM = INODE
               IFRM1 = IFROM +  MAX(NLAY-2,   0   )*NPOIN2
               ITOP1 = IFROM +  MIN(NLAY+1,NOLAY-1)*NPOIN2
               IFROM = IFROM + (    NLAY-1        )*NPOIN2
               ITO   = IFROM +                      NPOIN2
               WRITE ( NVEB ) IFROM,ITO,IFRM1,ITOP1
C        THE WRITING OF EXCHANGE POINTERS IS CHANGED       **END**     LP 05/04/2009
            ENDDO
            WRITE(NMAF) (1.0, I=1,NPOIN2*2) ! VERTICAL LENGTHS AT A DUMMY 1.0
         ENDDO                  ! WAQ COMPUTES THEM ON THE FLY FROM VOLUMES
C
C        FILLS IN THE HORIZONTAL AREA IN THE LAST DIRECTION EXCHANGE AREA
C
         DO NLAY = 1 , NOLAY-1
            AREAWQ( (NOLAY*(NSEG+MBND))+(NLAY-1)*NPOIN2+1 :             !  LP 05/04/2009
     &              (NOLAY*(NSEG+MBND))+ NLAY   *NPOIN2     ) = V2DPAR  !  LP 05/04/2009
         ENDDO
C
         IF(TRICK) THEN
C           WRITE ( 4 , * )
!    *           "TIME STEP: ",DDT," SMALLER THAN ONE SECOND !"
C           WRITE ( 4 , * )
!    *           "SYSTEM WILL COMPUTE IN TIMESTEPS AND M3/TIMESTEP"
            ITSTRT = 0
            ITSTEP = 1
         ELSE
            ITSTRT = INT(AAT)
         ENDIF
C
         ISTEPA=0
C
         IF(NCSIZE.GT.0) THEN
C
           WRITE(NSOU) NPOIN ! VOLUMES
           WRITE(NSOU) NOLAY
           WRITE(NSOU)(KNOLG(I),I=1,NPOIN2)
C
           WRITE(NMAB) 'SUMAREA'
           WRITE(NMAB) NPOIN
           WRITE(NMAB) NSEG
           WRITE(NMAB) MBND
           WRITE(NMAB) NOQ
           WRITE(NMAB) NOLAY
           WRITE(NMAB) NPTFR2
           WRITE(NMAB)(KNOLG(I),I=1,NPOIN2)
           WRITE(NMAB)((GLOSEG(I,J),J=1,2),I=1,NSEG)
           WRITE(NMAB)(NODENRS(I),I=1,NPOIN2)
           WRITE(NMAB)(NBOR(I),I=1,NPTFR2)
           WRITE(NMAB)(LIHBOR(I),I=1,NPTFR2)
C
           WRITE(NCOU) 'SUMFLOW'
           WRITE(NCOU) NPOIN
           WRITE(NCOU) NSEG
           WRITE(NCOU) MBND
           WRITE(NCOU) NOQ
           WRITE(NCOU) NOLAY
           WRITE(NCOU) NPTFR2
           WRITE(NCOU)(KNOLG(I),I=1,NPOIN2)
           WRITE(NCOU)((GLOSEG(I,J),J=1,2),I=1,NSEG)
           WRITE(NCOU)(NODENRS(I),I=1,NPOIN2)
           WRITE(NCOU)(NBOR(I),I=1,NPTFR2)
           WRITE(NCOU)(LIHBOR(I),I=1,NPTFR2)
C
           IF(SALI_DEL) THEN
             WRITE(NSAL) NPOIN
             WRITE(NSAL) NOLAY
             WRITE(NSAL)(KNOLG(I),I=1,NPOIN2)
           ENDIF
           IF(TEMP_DEL) THEN
             WRITE(NTEM) NPOIN
             WRITE(NTEM) NOLAY
             WRITE(NTEM)(KNOLG(I),I=1,NPOIN2)
           ENDIF
           IF(VELO_DEL) THEN
             WRITE(NVEL) NPOIN
             WRITE(NVEL) NOLAY
             WRITE(NVEL)(KNOLG(I),I=1,NPOIN2)
           ENDIF
           IF(DIFF_DEL) THEN
             WRITE(NVIS) NPOIN
             WRITE(NVIS) NOLAY
             WRITE(NVIS)(KNOLG(I),I=1,NPOIN2)
           ENDIF
C
         ENDIF
C
      ENDIF   ! FOR LLT = 0
C
C-----------------------------------------------------------------------
C
C        TIME STEP
C
C-----------------------------------------------------------------------
C
C
C        GETS THE DEPTH MULTIPLICATION FACTOR FOR AN ACTIVE POINT
C                   TO USE FOR THE WHOLE AREA (SIGMA COORDINATES)
C                   F IN TELEMAC ORDER (FIRST LAYER = BOTTOM)
C                   F IS THE LAYER THICKNESS AROUND THE PLANES
C
       IF(NOLAY.EQ.1) THEN !  2D
         DO I=1,NPOIN2
           F(I,1) = 1.D0
         ENDDO
       ELSE !  3D
         DO ND1=1,NPOIN2
           DO NLAY = 1 , NOLAY !  DETERMINES F WITH THIS NODE
             ND2 = ND1 + (NLAY-1)*NPOIN2
             IF(NLAY.EQ.1) THEN
               IF(HNEW(ND1).GT.1.D-4) THEN
                 F(ND1,NLAY)=(ZNEW(ND2+NPOIN2)-ZNEW(ND2))
     &                       /(2.D0*HNEW(ND1))
               ELSE
                 F(ND1,NLAY)=0.5D0/(NOLAY-1.D0)
               ENDIF
             ELSEIF(NLAY.EQ.NOLAY) THEN
               IF(HNEW(ND1).GT.1.D-4) THEN
                 F(ND1,NLAY)=(ZNEW(ND2)-ZNEW(ND2-NPOIN2))
     &                        /(2.D0*HNEW(ND1))
               ELSE
                 F(ND1,NLAY)=0.5D0/(NOLAY-1.D0)
               ENDIF
             ELSE
               IF(HNEW(ND1).GT.1.D-4) THEN
                 F(ND1,NLAY)=(ZNEW(ND2+NPOIN2)-ZNEW(ND2-NPOIN2))
     &                        /(2.D0*HNEW(ND1))
               ELSE
                 F(ND1,NLAY)=1.D0/(NOLAY-1.D0)
               ENDIF
             ENDIF
           ENDDO
         ENDDO
       ENDIF
C
C
C
      IF (.NOT.TRICK) ITSTEP = INT(DDT)
      IF (.NOT.TRICK .AND. DDT < 1.D0 )
     &     STOP "DT CHANGED FROM BIGGER THAN 1 TO SMALLER THAN 1"
      IF (       TRICK .AND. DDT > 1.D0 )
     &     STOP "DT CHANGED FROM SMALLER THAN 1 TO BIGGER THAN 1"
      IF ( .NOT. TRICK .AND. ( FLOAT(INT(DDT)) .NE. DDT ) )
     &     STOP "DT IN FRACTIONS OF SECONDS IS NOT SUPPORTED"
      ATOLD = AAT
      IF(LLT.NE.0) VOLOLD = VOLUME   ! SAVES VOLUME OF PREV. TIME LEVEL
      IF ( NOLAY .EQ. 1 ) THEN
C
C        THIS IS THE 2D VOLUME AT START OF TIME STEP
C
         DO INODE = 1, NPOIN2
           VOLUME(INODE) = HNEW(INODE)*V2DPAR(INODE)
         ENDDO
C
      ELSE
C
C        NOTE THAT WAQ NUMBERS 3D FROM TOP TO BOTTOM !!!!
C        NLAY IS THE TELEMAC CURRENT PLANE NUMBER
C        ND1  IS THE WAQ VOLUMES COUNTER
C
         ND1 = 1
         DO NLAY = NOLAY , 1 , -1 ! REVERSED ORDER FOR WAQ
           DO INODE = 1, NPOIN2
             VOLUME(ND1)=HNEW(INODE)*F(INODE,NLAY)*V2DPAR(INODE)
             ND1  =  ND1  + 1
           ENDDO
         ENDDO
      ENDIF
C
      IF ( MOD(ISTEPA,NSTEPA) .EQ. 0 ) THEN
         IF(STATIO.NE.1) THEN
            IF(TRICK) THEN
              ITOLD = LLT                ! <=== CHANGED BY LEO
            ELSE
              ITOLD = INT(ATOLD)         ! <=== CHANGED BY LEO
            ENDIF
         ENDIF
      ENDIF
      IF(MOD(ISTEPA,NSTEPA).EQ.0) THEN
        IF(STATIO.NE.1) THEN
          WRITE(NSOU) ITOLD, (REAL(VOLUME(I)),I=1,NPOIN)
        ENDIF
      ENDIF
      IF(LLT.EQ.0) THEN
        ISTEPA=ISTEPA+1
        RETURN                 ! INITIALISATION DONE
      ENDIF
C
C     HORIZONTAL VELOCITIES IN THE NODES (FOR MORPHOLOGY, REAERATION, ETC.)
C
      ND1 = 1                   ! WAQ NUMBERING IS TOP TO BOTTOM
      DO NLAY  = 1, NOLAY
         ND2 = (NOLAY-NLAY)*NPOIN2 + 1 ! TELEMAC NUMBERING IS BOTTOM TO TOP
         DO INODE = 1, NPOIN2
            VELO(ND1) = SQRT(U(ND2)**2+V(ND2)**2)
            ND1 = ND1 + 1
            ND2 = ND2 + 1
         ENDDO
      ENDDO
C
C        ZEROES THE FLOWS AND THE EXCHANGE AREAS
C        NB: LAST DIRECTION OF EXCHANGE AREAS REMAINS AT HORSURF
C
      IF(INIFLOW) THEN
        DO I=1,NOQF
          FLOW(I) = 0.D0
        ENDDO
      ELSE
C       3D PART SET TO ZERO (MAYBE NOT NECESSARY FOR VERTICAL FLOWS)
C       THE PREVIOUS CODE WAS IN ERROR, SO I HOPE IT WAS NOT NECESSARY   LP 05/04/2009
        IF(NOQ.GT.NSEG) THEN
          DO I=NOLAY*NSEG+1,NOQF                                      !  LP 05/04/2009
            FLOW(I) = 0.D0
          ENDDO
        ENDIF
      ENDIF
C
C     MAKES EXCHANGE AREAS
C
      AREAWQ(1:NOLAY*(NSEG+MBND)) = 0.D0                              !  LP 05/04/2009
      DO IELEM=1,NELEM2
        DO NLAY = 1 , NOLAY
        DO I = 1,3
          ISEG  = ELTSEG(IELEM,I)
          ISEGL = ISEG + (NOLAY-NLAY)*(NSEG+MBND) ! WAQ ORDER         !  LP 05/04/2009
C         JMH 27/03/2009 : NOT SURE, I DID NOT UNDERSTAND THE FORMULA
C                          USED IN VERSION 5.8
C         IT WAS :
C         AREAWQ(ISEGL) = AREAWQ(ISEGL) + DIST(I,IELEM)*H*F(NLAY)
C         WITH H=(HOLD(ND1)+HOLD(ND2)+HOLD(ND3))/3. AVERAGE DEPTH
C         OF A TRIANGLE
C*LEO        DIST IS THE DISTANCE OF THE GRAVITY POINT TO THIS SEGMENT
C                 SEE LINE 322 IN THIS CODE. THIS DISTANCE TIMES THE
C                 AVERAGE THICKNESS OF THE SLICE (H*F(NLAY)) IS THE
C                 AREA AS USED IN THE DIFFUSION TERM D*A/DL IN M3/S
C                 A SIMILAR CONTRIBUTION COMES FROM THE OTHER ELEMENT
C                 SHARING THIS SEGMENT, THAT IS WHY CONTRIBUTIONS ARE
C                 ADDED. IF YOU WANT TO DO SOMETHING SIMILAR THEN YOU COULD
C                 DO SOMETHING LIKE:
C         IF ( I .EQ. 1 ) H = ( HOLD(GLOSEG(ISEG,1)*F(GLOSEG(ISEG,1),NLAY) +
C    *                          HOLD(GLOSEG(ISEG,2)*F(GLOSEG(ISEG,2),NLAY) +
C    *                          HOLD(GLOSEG(ISEG,3)*F(GLOSEG(ISEG,3),NLAY) ) / 3
C         AREAWQ(ISEGL) = AREAWQ(ISEGL) + DIST(I,IELEM)*H
C         YOUR FORMULA IS SLIGHTLY DIFFERENT, BUT IT IS NOT SO CRITICAL.
C         HORIZONTAL DIFFUSION IS GENERALLY NOT A SENSITIVE PROCESS IN 3D.
C*LEO     PLEASE REMOVE THESE LINES AFTER READING (JMH: READ BUT NOT REMOVED)
          AREAWQ(ISEGL) = AREAWQ(ISEGL) +
     &    DIST(I,IELEM)*(HPROP(GLOSEG(ISEG,1))+HPROP(GLOSEG(ISEG,2)))*
     &        ( F(GLOSEG(ISEG,1),NLAY)+F(GLOSEG(ISEG,2),NLAY) )*0.25D0
        ENDDO
        ENDDO
      ENDDO
      DO I=1,NOQ                                                       !  LP 05/04/2009
         IF ( AREAWQ(I) .LT. 1.0D-20 ) AREAWQ(I) = 10.D0  ! BOUNDARIES    LP 05/04/2009
      ENDDO                                                            !  LP 05/04/2009
C
C     NOW THE MOST IMPORTANT THING, THE TRANSPORT ITSELF
C
C     THE FINITE ELEMENT FLUXES PER NODE ARE IN W, THEY ARE
C     GIVEN BY TELEMAC-2D OR 3D
C
C--------------------------------------------------------------------
C DIFFERENT OPTIONS TO COMPUTE THE FLUXES (TAKEN FROM CVTRVF IN BIEF)
C--------------------------------------------------------------------
C
      IOPT1 = 2
      IF(NOLAY.EQ.1) THEN
!
C       2D CASE
        CALL FLUX_EF_VF(FLOW,W,NSEG,NELEM2,ELTSEG,ORISEG,IKLE,
     &                  INIFLOW,IOPT1)
!
      ELSE
!
C       3D CASE
        CALL FLUX_EF_VF_3D(FLOW,W2D,W,NSEG,MESH3D%NSEG,NELEM2,
     &                     MESH3D%NELEM,MESH2D,MESH3D,INIFLOW,
     &                     IOPT1,2)
C                                2: HORIZONTAL FLUXES FROM TOP TO BOTTOM
C       FLUX LIMITATION (FLULIM IS 2D, SO NUMBERING FROM TOP TO BOTTOM
C                        MAKES NO PROBLEM)
        IF(YAFLULIM) CALL FLUX3DLIM(FLOW,FLULIM,NOLAY,NSEG)
!
      ENDIF
C
C     WRITES THE EXCHANGE AREAS (LAST DIRECTION REMAINS AT HORSURF)
C
      SUMAREA = SUMAREA + AREAWQ
      IF(MOD(ISTEPA,NSTEPA).EQ.0) THEN
        SUMAREA=SUMAREA/NSTEPA
        IF(STATIO.NE.1) THEN
          IF(TRICK) THEN
            WRITE(NMAB) ITOLD-NSTEPA,(REAL(SUMAREA(I)),I=1,NOQ)
          ELSE
            WRITE(NMAB) INT(AAT-NSTEPA*DDT),(REAL(SUMAREA(I)),I=1,NOQ)
          ENDIF
        ENDIF
        SUMAREA=0.D0
      ENDIF
C
C     WRITES THE SALINITY (AND INVERTS THE PLANES FOR DELWAQ)
C
      IF(SALI_DEL) THEN
        SUMSALI=SUMSALI+SALI
        IF(MOD(ISTEPA,NSTEPA).EQ.0) THEN
          SUMSALI=SUMSALI/NSTEPA
          IF(STATIO.NE.1) THEN
            IF(TRICK) THEN
              WRITE(NSAL) ITOLD-NSTEPA,
     &((REAL(SUMSALI(I+(NOLAY-J)*NPOIN2)),I=1,NPOIN2),J=1,NOLAY)
            ELSE
              WRITE(NSAL) INT(AAT-NSTEPA*DDT),
     &((REAL(SUMSALI(I+(NOLAY-J)*NPOIN2)),I=1,NPOIN2),J=1,NOLAY)
            ENDIF
          ENDIF
          SUMSALI=0.D0
        ENDIF
      ENDIF
C
C     WRITES THE TEMPERATURE (AND INVERTS THE PLANES FOR DELWAQ)
C
      IF(TEMP_DEL) THEN
        SUMTEMP=SUMTEMP+TEMP
        IF(MOD(ISTEPA,NSTEPA).EQ.0) THEN
          SUMTEMP=SUMTEMP/NSTEPA
          IF(STATIO.NE.1) THEN
            IF(TRICK) THEN
              WRITE(NTEM) ITOLD-NSTEPA,
     &((REAL(SUMTEMP(I+(NOLAY-J)*NPOIN2)),I=1,NPOIN2),J=1,NOLAY)
            ELSE
              WRITE(NTEM) INT(AAT-NSTEPA*DDT),
     &((REAL(SUMTEMP(I+(NOLAY-J)*NPOIN2)),I=1,NPOIN2),J=1,NOLAY)
            ENDIF
          ENDIF
          SUMTEMP=0.D0
        ENDIF
      ENDIF
C
C     WRITES THE DIFFUSION (AND INVERTS THE PLANES FOR DELWAQ)
C
      IF(DIFF_DEL) THEN
        SUMVISC=SUMVISC+VISC
        IF(MOD(ISTEPA,NSTEPA).EQ.0) THEN
          SUMVISC=SUMVISC/NSTEPA
          IF(STATIO.NE.1) THEN
            IF(TRICK) THEN
              WRITE(NVIS) ITOLD-NSTEPA,
     &((REAL(SUMVISC(I+(NOLAY-J)*NPOIN2)),I=1,NPOIN2),J=1,NOLAY)
            ELSE
              WRITE(NVIS) INT(AAT-NSTEPA*DDT),
     &((REAL(SUMVISC(I+(NOLAY-J)*NPOIN2)),I=1,NPOIN2),J=1,NOLAY)
            ENDIF
          ENDIF
          SUMVISC=0.D0
        ENDIF
      ENDIF
C
C     WRITES THE VELOCITY (AND NOT !!!!!!    INVERTS THE PLANES FOR DELWAQ)
C                         AS IT HAS BEEN DONE WITH DELWAQ NUMBERING
C
      IF(VELO_DEL) THEN
        SUMVELO=SUMVELO+VELO
        IF(MOD(ISTEPA,NSTEPA).EQ.0) THEN
          SUMVELO=SUMVELO/NSTEPA
          IF(STATIO.NE.1) THEN
            IF(TRICK) THEN
              WRITE(NVEL) ITOLD-NSTEPA,(REAL(SUMVELO(I)),I=1,NPOIN)
            ELSE
              WRITE(NVEL) INT(AAT-NSTEPA*DDT),
     &                    (REAL(SUMVELO(I)),I=1,NPOIN)
            ENDIF
          ENDIF
          SUMVELO=0.D0
        ENDIF
      ENDIF
C
C     MAKES THE VERTICAL FLOWS HERE
C     FIRST THE NEW VOLUMES WITHOUT VERTICAL FLOWS
C
      DO NLAY = 1 , NOLAY       ! NOW IN WAQ ORDER
         DO ISEG = 1 , NSEG
            IFROM = GLOSEG(ISEG,1)                            !        LP 24/04/2009
            ITO   = GLOSEG(ISEG,2)                            !        LP 24/04/2009
            ISEGL = ISEG + (NLAY-1)*NSEG
C           IF ( IFROM .GT. 0 ) THEN                          !        LP 24/04/2009
               IFROM = IFROM + (NLAY-1)*NPOIN2
               VOLOLD(IFROM) = VOLOLD(IFROM) - FLOW(ISEGL) * DDT
C           ENDIF                                             !        LP 24/04/2009
C           IF(ITO.GT.0) THEN                                 !        LP 24/04/2009
               ITO = ITO + (NLAY-1)*NPOIN2
               VOLOLD(ITO) = VOLOLD(ITO) + FLOW(ISEGL) * DDT
C           ENDIF                                             !        LP 24/04/2009
         ENDDO
      ENDDO
C
C     THEN THE TOTAL NEW VOLUMES: VOL2 IS THE SUM OF ALL
C     3D VOLUMES OF A VERTICAL
C
      DO INODE = 1 , NPOIN2
        VOL2(INODE)=0.D0
        DO NLAY=1,NOLAY
          ND1 = INODE + (NLAY-1)*NPOIN2
          VOL2(INODE) = VOL2(INODE) + VOLOLD(ND1)
        ENDDO
      ENDDO
C
C     DETERMINES THE OPEN BOUNDARY FLOWS                    *START*     LP 05/04/2009
C
      DO IBOR = 1, NPTFR2                              ! PROBABLY THE OPEN BOUNDARY FLOW
        IF(LIHBOR(IBOR).NE.2) THEN                     ! IS AVAILABLE SOMEWHERE IN
          A1 = 0.D0                                    ! TELEMAC FOR FLOW BOUNDARIES
          INODE = NBOR(IBOR)                           ! IT IS HOWEVER UNLIKELY THAT IT
          DO NLAY=1,NOLAY                              ! IS AVAILABLE FOR WATER LEVEL
            ND1 = INODE + (NLAY-1)*NPOIN2              ! BOUNDARIES AND THIS PROCEDURE
            A1 = A1 + VOLUME(ND1)                      ! SHOULD WORK EQUALLY WELL FOR BOTH
          ENDDO                                        ! A1   IS CORRECT DEPTH INTEGRATED VOLUME
          A2 = ( A1 - VOL2(INODE) ) / DDT              ! VOL2 IS PREVIOUS VOLUME + FLOWS*DT
          VOL2(INODE) = A1                             ! A2   IS DIFFERENCE IN M3/S THAT
          DO NLAY=1,NOLAY                              !      COMES FROM THE OPEN BOUNDARY
            ND1 = INODE + (NLAY-1)*NPOIN2
            ND2 = (NLAY-1)*(NSEG+MBND)+NSEG-NODENRS(INODE)
            SUMFLOW(ND2)=SUMFLOW(ND2)+A2*F(INODE,NOLAY-NLAY+1)
            VOLOLD (ND1)=VOLOLD (ND1)+A2*F(INODE,NOLAY-NLAY+1)*DDT
          ENDDO
        ENDIF
      ENDDO
C     DETERMINES THE OPEN BOUNDARY FLOWS                    **END**     LP 05/04/2009
C
C     NOW MAKES THE VERTICAL FLOWS (STORED AFTER THE HORIZONTAL FLOWS)
C
      ND2 = NOLAY*NSEG
      DO INODE = 1 , NPOIN2
        IF(NOLAY.GT.1) THEN
C       FROM BOTTOM TO LAYER BELOW THE FREE SURFACE
        DO NLAY=1,NOLAY-1
          ND1 = INODE + (NLAY-1)*NPOIN2
          FLOW(ND1+ND2)=(VOLOLD(ND1)-VOL2(INODE)*
     &                   F(INODE,NOLAY-NLAY+1))/DDT
          VOLOLD(ND1       )= VOLOLD(ND1       ) - FLOW(ND1+ND2)*DDT
          VOLOLD(ND1+NPOIN2)= VOLOLD(ND1+NPOIN2) + FLOW(ND1+ND2)*DDT
        ENDDO
        ENDIF
      ENDDO
C
C     TESTS MASS ERRORS ON INTERNAL NODES
C
      IF(INFOGR) THEN
        SSUM  =0.D0
        ERRTOT=0.D0
        DO I2D=1,NPOIN2
          IF(NODENRS(I2D).GT.0) THEN
            DO NLAY=1,NOLAY
              I3D=I2D+NPOIN2*(NLAY-1)
              SSUM=MAX(SSUM,ABS(VOLOLD(I3D)-VOLUME(I3D)))
              ERRTOT=ERRTOT+VOLOLD(I3D)-VOLUME(I3D)
            ENDDO
          ENDIF
        ENDDO
        WRITE(LU,*) ' '
        IF(LNG.EQ.1) THEN
         WRITE(LU,*) 'TEL4DEL : ERREUR DE MASSE MAXIMUM:',SSUM
         WRITE(LU,*) '          SOMME SUR LES POINTS INTERIEURS:',ERRTOT
        ELSE
         WRITE(LU,*) 'TEL4DEL: MAXIMUM MASS ERROR:',SSUM
         WRITE(LU,*) '         SUM OF ERRORS ON INTERNAL POINTS:',ERRTOT
        ENDIF
        WRITE(LU,*) ' '
      ENDIF
C
C     WRITES THE FLOWS (MULTIPLICATION WITH DT IS TRICK TO MODEL)
C
CCCCCCAGREG
      I3D = 1                                                 !  *START*  LP 05/04/2009
      DO I=1,NOLAY
        I2D = (I-1)*(NSEG+MBND)
        DO J=1,NSEG
          SUMFLOW(I2D+J) = SUMFLOW(I2D+J) + FLOW(I3D)
          I3D = I3D + 1
        ENDDO
      ENDDO
      DO I=1,NOLAY-1                                          !  *START*  LP 24/04/2009
        I2D = NOLAY*(NSEG+MBND) + (I-1)*NPOIN2
        DO J=1,NPOIN2
          SUMFLOW(I2D+J) = SUMFLOW(I2D+J) + FLOW(I3D)
          I3D = I3D + 1
        ENDDO
      ENDDO                                                   !  **END**  LP 24/04/2009
C     DO I=1,NOQ
C       SUMFLOW(I) = SUMFLOW(I) + FLOW(I)
C     ENDDO                                                      **END**  LP 05/04/2009
      IF(MOD(ISTEPA,NSTEPA).EQ.0) THEN
        SUMFLOW = SUMFLOW / NSTEPA
        IF(STATIO.NE.1) THEN
          IF(TRICK) THEN
            SUMFLOW = SUMFLOW*DDT
            WRITE(NCOU) ITOLD-NSTEPA , (REAL(SUMFLOW(I)),I=1,NOQ)
          ELSE
            WRITE(NCOU) INT(AAT-NSTEPA*DDT), (REAL(SUMFLOW(I)),I=1,NOQ)
          ENDIF
        ENDIF
        SUMFLOW = 0.D0
      ENDIF
CCCCCCAGREG
C
C        STATIONARY DATABASE IF REQUIRED
C
 20   CONTINUE
C
      IF ( LLT .LT. NNIT ) THEN
         ISTEPA = ISTEPA + 1
         RETURN                 !  FINALISATION
      ENDIF
      IF ( STATIO .EQ. 1 ) THEN
         IF ( TRICK ) THEN
            WRITE(NSOU) ITOLD , (REAL(VOLUME(I)),I=1,NPOIN)
            WRITE(NMAB) ITOLD , (REAL(SUMAREA(I)),I=1,NOQ)
            WRITE(NCOU) ITOLD , (REAL(SUMFLOW(I)),I=1,NOQ)
         ELSE
            WRITE(NSOU) INT(ATOLD), (REAL(VOLUME(I)),I=1,NPOIN)
            WRITE(NMAB) INT(ATOLD), (REAL(SUMAREA(I)),I=1,NOQ)
            WRITE(NCOU) INT(ATOLD), (REAL(SUMFLOW(I)),I=1,NOQ)
         ENDIF
         DO NLAY  = 1, NOLAY
            DO ISEG  = 1, NSEG
               ISEGL  = ISEG + (NOLAY-NLAY)*NSEG ! WAQ ORDER
               IFROM = NODENRS(GLOSEG(ISEG,1))
               IF ( IFROM .GT. 0 ) THEN
                  IFROM = IFROM + (NLAY-1)*NPOIN2
                  VOLUME(IFROM) = VOLUME(IFROM) - FLOW(ISEGL)*DDT
               ENDIF
               ITO   = NODENRS(GLOSEG(ISEG,2))
               IF ( ITO   .GT. 0 ) THEN
                  ITO   = ITO   + (NLAY-1)*NPOIN2
                  VOLUME(ITO  ) = VOLUME(ITO  ) + FLOW(ISEGL)*DDT
               ENDIF
            ENDDO
         ENDDO
         ND2 = NOLAY*NSEG
         DO INODE = 1 , NPOIN2
            DO ND1 = INODE , NPOIN-NPOIN2 , NPOIN2
               VOLUME(ND1)       = VOLUME(ND1)       - FLOW(ND1+ND2)*DDT
               VOLUME(ND1+NPOIN2)= VOLUME(ND1+NPOIN2)+ FLOW(ND1+ND2)*DDT
            ENDDO
         ENDDO
         IF(TRICK) THEN
           WRITE(NSOU) ITOLD,(REAL(VOLUME(I)),I=1,NPOIN)
         ELSE
           WRITE(NSOU) INT(AAT-NSTEPA*DDT),(REAL(VOLUME(I)),I=1,NPOIN)
         ENDIF
      ENDIF
C
C     DUMMY RECORDS AT THE END. THEY ARE NOT USED BY WAQ BUT SHOULD BE THERE
C
      SUMAREA = 0.D0
      SUMFLOW = 0.D0
      IF(TRICK) THEN
        K=ITOLD
      ELSE
        K=NINT(AAT)
      ENDIF
      WRITE ( NMAB ) K, (REAL(SUMAREA(I)),I=1,NOQ)
      WRITE ( NCOU ) K, (REAL(SUMFLOW(I)),I=1,NOQ)
      IF(SALI_DEL) THEN
        WRITE ( NSAL ) K,
     &        ((REAL(SUMSALI(I+(NOLAY-J)*NPOIN2)),I=1,NPOIN2),J=1,NOLAY)
      ENDIF
      IF(TEMP_DEL) THEN
        WRITE ( NTEM ) K,
     &        ((REAL(SUMTEMP(I+(NOLAY-J)*NPOIN2)),I=1,NPOIN2),J=1,NOLAY)
      ENDIF
      IF(DIFF_DEL) THEN
        WRITE ( NVIS ) K,
     &        ((REAL(SUMVISC(I+(NOLAY-J)*NPOIN2)),I=1,NPOIN2),J=1,NOLAY)
      ENDIF
      IF(VELO_DEL) THEN
        WRITE ( NVEL ) K, (REAL(SUMVELO(I)),I=1,NPOIN)
      ENDIF
      ITSTOP = K
      IF(.NOT.TRICK) NSTEPA = INT(NSTEPA*DDT)   ! FOR WRIHYD ONLY
C
C     WRITES A COMMAND FILE FOR DELWAQ
C
      IF(NCSIZE.GT.0) THEN
        WRITE(NCOB,'(I6)')NOLAY
        WRITE(NCOB,'(I3)')LEN_TRIM(TITRE)
        WRITE(NCOB,'(A)')TITRE(1:LEN_TRIM(TITRE))
        WRITE(NCOB,'(I4)')MARDAT(1)
        WRITE(NCOB,'(I2)')MARDAT(2)
        WRITE(NCOB,'(I2)')MARDAT(3)
        WRITE(NCOB,'(I2)')MARTIM(1)
        WRITE(NCOB,'(I2)')MARTIM(2)
        WRITE(NCOB,'(I2)')MARTIM(3)
        WRITE(NCOB,'(I14)')ITSTRT
        WRITE(NCOB,'(I14)')ITSTOP
        WRITE(NCOB,'(I14)')NSTEPA
        WRITE(NCOB,'(I6)')NOLAY
        WRITE(NCOB,'(I3)')LEN_TRIM(NOMGEO)
        WRITE(NCOB,'(A)')NOMGEO(1:LEN_TRIM(NOMGEO))
        WRITE(NCOB,'(I3)')LEN_TRIM(NOMLIM)
        WRITE(NCOB,'(A)')NOMLIM(1:LEN_TRIM(NOMLIM))
        WRITE(NCOB,'(I3)')LEN_TRIM(NOMSOU)
        WRITE(NCOB,'(A)')NOMSOU(1:LEN_TRIM(NOMSOU))
        WRITE(NCOB,'(I3)')LEN_TRIM(NOMMAB)
        WRITE(NCOB,'(A)')NOMMAB(1:LEN_TRIM(NOMMAB))
        WRITE(NCOB,'(I3)')LEN_TRIM(NOMCOU)
        WRITE(NCOB,'(A)')NOMCOU(1:LEN_TRIM(NOMCOU))
        WRITE(NCOB,'(I3)')LEN_TRIM(NOMVEB)
        WRITE(NCOB,'(A)')NOMVEB(1:LEN_TRIM(NOMVEB))
        WRITE(NCOB,'(I3)')LEN_TRIM(NOMMAF)
        WRITE(NCOB,'(A)')NOMMAF(1:LEN_TRIM(NOMMAF))
        WRITE(NCOB,'(L1)')SALI_DEL
        IF(SALI_DEL) THEN
          WRITE(NCOB,'(I3)')LEN_TRIM(NOMSAL)
          WRITE(NCOB,'(A)')NOMSAL(1:LEN_TRIM(NOMSAL))
        ENDIF
        WRITE(NCOB,'(L1)')TEMP_DEL
        IF(TEMP_DEL) THEN
          WRITE(NCOB,'(I3)')LEN_TRIM(NOMTEM)
          WRITE(NCOB,'(A)')NOMTEM(1:LEN_TRIM(NOMTEM))
        ENDIF
        WRITE(NCOB,'(L1)')DIFF_DEL
        IF(DIFF_DEL) THEN
          WRITE(NCOB,'(I3)')LEN_TRIM(NOMVIS)
          WRITE(NCOB,'(A)')NOMVIS(1:LEN_TRIM(NOMVIS))
        ENDIF
        WRITE(NCOB,'(L1)')VELO_DEL
        IF(VELO_DEL) THEN
          WRITE(NCOB,'(I3)')LEN_TRIM(NOMVEL)
          WRITE(NCOB,'(A)')NOMVEL(1:LEN_TRIM(NOMVEL))
        ENDIF
          WRITE(NCOB,'(I3)')LEN_TRIM(NOMINI)
          WRITE(NCOB,'(A)')NOMINI(1:LEN_TRIM(NOMINI))
        DO I=1,NOLAY
          WRITE(NCOB,'(F10.4)')F(1,I)
        ENDDO
      ENDIF
C
      CALL WRIHYD( TITRE  , ITSTRT , ITSTOP , ITSTEP , NPOIN2, MBND   ,
     &             NSEG   , NOLAY  , NOMGEO , NOMLIM ,
     &             F      , NSTEPA ,
     &             NOMSOU , NOMMAB , NOMCOU , NOMINI , NOMVEB,
     &             NOMMAF , NOMSAL , NOMTEM , NOMVEL , NOMVIS, NCOB   ,
     &             SALI_DEL,TEMP_DEL,VELO_DEL,DIFF_DEL,MARDAT, MARTIM)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C