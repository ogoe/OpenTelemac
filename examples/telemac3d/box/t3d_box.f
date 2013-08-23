!                    *********************
                     SUBROUTINE T3D_CORFON
!                    *********************
!
     &(SZF, ST1, ST2, ZF, T1, T2, X, Y, PRIVE, NPOIN2,
     & LISFON, MSK, MASKEL, MATR2D, MESH2D, S)
!
!***********************************************************************
! TELEMAC3D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    MODIFIES THE BOTTOM TOPOGRAPHY.
!+
!+            STANDARD ACTION: SMOOTHES THE BOTTOM ELEVATION.
!+
!+           (KEYWORD:  'NUMBER OF BOTTOM SMOOTHINGS')
!
!note     EQUIVALENT TO CORFON (BIEF LIBRARY), EXCEPT THAT THIS
!+         SUBROUTINE DISTINGUISHES DATA FROM STRUCTURES.
!
!history  J.M. JANIN  (LNH)
!+        25/11/97
!+        V5P1
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
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
!history  J-M HERVOUET (LNHE)
!+        29/09/2011
!+        V6P2
!+   Name changed into T3D_CORFON to avoid duplication with Telemac-2D
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| LISFON         |-->| NUMBER OF SMOOTHINGS REQUIRED
!| MASKEL         |-->| MASK OF ELEMENTS
!| MATR2D         |<->| WORK MATRIX IN 2DH
!| MESH2D         |<->| 2D MESH
!| MSK            |-->| IF YES, THERE ARE MASKED ELEMENTS
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| PRIVE          |<->| BLOCK OF PRIVATE ARRAYS FOR USER
!| S              |-->| VOID STRUCTURE
!| ST1            |<->| STRUCTURE OF T1
!| ST2            |<->| STRUCTURE OF T2
!| SZF            |<->| STRUCTURE OF ZF
!| T1             |<->| WORK ARRAY
!| T2             |<->| WORK ARRAY
!| X              |-->| MESH COORDINATE
!| Y              |-->| MESH COORDINATE
!| ZF             |<->| ELEVATION OF BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN2, LISFON
      LOGICAL, INTENT(IN) :: MSK
      TYPE (BIEF_OBJ), INTENT(INOUT) :: SZF, ST1, ST2
      DOUBLE PRECISION, DIMENSION(NPOIN2), INTENT(INOUT) :: ZF, T1, T2
      DOUBLE PRECISION, DIMENSION(NPOIN2), INTENT(IN) :: X,Y
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: PRIVE
      TYPE (BIEF_OBJ),  INTENT(IN)    :: MASKEL
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: MATR2D
      TYPE (BIEF_MESH), INTENT(INOUT) :: MESH2D
      TYPE (BIEF_OBJ),  INTENT(IN)    :: S
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,I
      LOGICAL MAS
      double precision z1, z2, x1      
!
!***********************************************************************
!
!     SMOOTHES THE BOTTOM ELEVATION
!
      IF(LISFON.GT.0) THEN
!
         MAS = .TRUE.
!
         CALL FILTER(SZF,MAS,ST1,ST2,MATR2D,'MATMAS          ',
     &               1.D0,S,S,S,S,S,S,MESH2D,MSK,MASKEL,LISFON)
      ENDIF
      
!     Slope from -15m at x = 0m to -5m and x = 4000m

      do i = 1,npoin2
        zf(i) = 0.0025*x(i) - 15.0
      end do

      
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                    *********************
                     SUBROUTINE MURD3D_POS
!                    *********************
!
     &(FC,FN,VOLU,SVOLU,VOLUN,SVOLUN,VOLU2,SVOLU2,RMASS,
     & TRA01,TRA02,TRA03,STRA01,STRA02,STRA03,MESH2,MESH3,
     & NELEM3,NPOIN3,DT,SCHCF,MSK,MASKEL,INFOR,CALFLU,FLUX,FLUEXT,
     & S0F,NSCE,SOURCES,FSCE,RAIN,PLUIE,TRAIN,NPOIN2,
     & OPTBAN,FLODEL,FLOPAR,GLOSEG,DIMGLO,NSEG,NPLAN,
     & T5,FLUX_REMOVED,SAVED_VOLU2,SAVED_F,OPTION,IELM3)
!
!***********************************************************************
! TELEMAC3D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    ADVECTION OF A VARIABLE WITH AN UPWIND FINITE
!+                VOLUME SCHEME.
!+
!+           (THE ADVECTION IS DONE EDGE BY EDGE, WHICH ENABLES
!+                LOCAL DEPTHS EQUAL TO ZERO).
!
!warning  HERE FLUXES IN FLODEL ARE FROM POINT 2 TO POINT 1.
!+
!+        SEE FLUX3D (HORIZONTAL FLUXES BASED ON FLUINT)
!+            AND PRECON (VERTICAL FLUXES BASED ON WSCONV)
!
!history  J-M HERVOUET (LNHE)
!+        19/04/2010
!+        V6P0
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
!history  J-M HERVOUET (LNHE)
!+        28/10/2011
!+        V6P2
!+   Updated for element 51 (prisms cut into tetrahedra). Better memory
!+   allocation of INDIC.
!
!history  J-M HERVOUET (LNHE)
!+        23/04/2012
!+        V6P2
!+   Values of tracers in rain taken into account.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CALFLU         |-->| INDICATE IF FLUX IS CALCULATED FOR BALANCE
!| DIMGLO         |-->| FIRST DIMENSION OF ARRAY GLOSEG
!| DT             |-->| TIME STEP
!| FC             |<->| VARIABLE AFTER CONVECTION
!| FLODEL         |-->| FLUXES BY SEGMENT
!| FLOPAR         |-->| FLUXES BY SEGMENT, ASSEMBLED IN PARALLEL
!| FLUEXT         |-->| OUTPUT FLUX BY NODE
!| FLUX_REMOVED   |<->| TOTAL FLUX REMOVED OF EACH POINT
!| FLUX           |<->| GLOBAL FLUXES TO BE CHANGED
!| FN             |-->| VARIABLE AT TIME N
!| FSCE           |-->| DIRICHLET BOUNDARY CONDITIONS OF F
!| GLOSEG         |-->| FIRST AND SECOND POINT OF SEGMENTS
!| IELM3          |-->| TYPE OF ELEMENT (41: PRISM, ETC.)
!| INFOR          |-->| INFORMATIONS FOR SOLVERS
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH2          |<->| 2D MESH
!| MESH3          |<->| 3D MESH
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NELEM3         |-->| NUMBER OF ELEMENTS IN 3D
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH OF PRISMS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| NSCE           |-->| NUMBER OF GIVEN POINTS FOR SOURCES
!| NSEG           |-->| NUMBER OF SEGMENTS
!| OPTBAN         |-->| option2 FOR TIDAL FLATS, IF 1, FREE SURFACE
!|                |   | MODIFIED AND PIECE-WISE LINEAR
!| PLUIE          |-->| RAIN IN M/S MULTIPLIED BY VOLU2D
!| RAIN           |-->| IF YES, THERE IS RAIN OR EVAPORATION
!| RMASS          |<->| REMAINING MASSES
!| S0F            |-->| EXPLICIT SOURCE TERM
!| SAVED_F        |<->| TRACER SAVED
!| SAVED_VOLU2    |<->| VOLUME VOLU2 SAVED
!| SCHCF          |-->| ADVECTION SCHEME FOR F
!| SOURCES        |-->| SOURCES
!| STRA01         |<->| STRUCTURE OF TRA01
!| STRA02         |<->| STRUCTURE OF TRA02
!| STRA03         |<->| STRUCTURE OF TRA03
!| SVOLU          |-->| STRUCTURE OF VOLU
!| SVOLU2         |<->| STRUCTURE OF VOLU2
!| SVOLUN         |-->| STRUCTURE OF VOLUN
!| T5             |<->| WORK ARRAY
!| TRA01          |<->| WORK ARRAY OF DIMENSION NPOIN3 EQUIVALENT TO
!|                |   | VOLU2 FOR CURRENT FINAL TIME
!| TRA02          |<->| WORK ARRAY OF DIMENSION NPOIN3
!| TRA03          |<->| WORK ARRAY OF DIMENSION NPOIN3
!| TRAIN          |-->| VALUE OF TRACER IN RAIN
!| VOLU           |-->| CONTROL VOLUME AT TIME N+1
!| VOLU2          |<->| LIKE VOLU, BUT ASSEMBLED IN PARALLEL
!| VOLUN          |-->| CONTROL VOLUME AT TIME N
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      use declarations_telemac3d, only : nptfr3,mesh2d,wsconv,nbor3,
     $                                   uconv,vconv,volu2d
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: SCHCF,NELEM3,NPOIN3,NPOIN2
      INTEGER, INTENT(IN)             :: NSCE,OPTBAN,NSEG,NPLAN,DIMGLO
      INTEGER, INTENT(IN)             :: GLOSEG(DIMGLO,2),OPTION,IELM3
      integer option2
!
      DOUBLE PRECISION, INTENT(INOUT) :: FC(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: FN(NPOIN3),PLUIE(NPOIN2)
!
      DOUBLE PRECISION, INTENT(IN)    :: MASKEL(NELEM3)
      DOUBLE PRECISION, INTENT(IN), TARGET    :: FLUEXT(NPOIN3)
!
      DOUBLE PRECISION, INTENT(IN)    :: VOLUN(NPOIN3), VOLU(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: VOLU2(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPOIN3),FLUX
      DOUBLE PRECISION, INTENT(INOUT) :: TRA02(NPOIN3)
      DOUBLE PRECISION, INTENT(INOUT), TARGET :: TRA03(NPOIN3)
      DOUBLE PRECISION, INTENT(IN)    :: DT,TRAIN,FSCE(NSCE)
!
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SVOLU2
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: FLUX_REMOVED
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: SAVED_VOLU2,SAVED_F,T5
      TYPE(BIEF_OBJ), INTENT(IN)      :: SOURCES,S0F,SVOLU,SVOLUN
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: STRA01,STRA02,STRA03
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH2,MESH3
!
!     DIMENSION OF FLODEL AND FLOPAR=NSEG2D*NPLAN+NPOIN2*NETAGE
      DOUBLE PRECISION, INTENT(IN)    :: FLODEL(*),FLOPAR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: RMASS(*)
!                                        SIZE IN MEMORY = 30*NELEM
!                                        THIS IS ENOUGH
!
      LOGICAL, INTENT(IN)             :: MSK,INFOR,CALFLU,RAIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN,NITER,IS,IIS,I,NSEGH,NSEGV,OPT,IR
      INTEGER I1,I2,IPLAN,ISEG3D,I2D,I3D,IPTFR,SIZEINDIC
      INTEGER REMAIN_SEG,NEWREMAIN,REMAIN_TOT,REMAIN_SEG_INIT
!
!-----------------------------------------------------------------------
!
      DOUBLE PRECISION P_DSUM,P_DMIN,P_DMAX
      EXTERNAL         P_DSUM,P_DMIN,P_DMAX
      INTEGER  P_ISUM
      EXTERNAL P_ISUM
!
      DOUBLE PRECISION RINIT,C,C2,NEWVOL,RFLUX,RFLUX_OLD
      DOUBLE PRECISION VOLSEG1,VOLSEG2
!
      DOUBLE PRECISION EPS
      DATA EPS /1.D-6/
      DOUBLE PRECISION ALLOW
      DATA ALLOW /1.D-5/
      DOUBLE PRECISION REDUC
      DATA REDUC /1.D-9/
      DOUBLE PRECISION EPS_VOLUME
      DATA EPS_VOLUME /1.D-8/
      INTEGER NITMAX
      DATA NITMAX /100/
!
      LOGICAL TESTING
      DATA    TESTING/.FALSE./
!
!-----------------------------------------------------------------------
!
!     INDIC WILL BE A LIST OF SEGMENTS WITH NON ZERO FLUXES
!
      LOGICAL DEJA
      DATA DEJA/.FALSE./
      INTEGER, ALLOCATABLE :: INDIC(:)
!
!-----------------------------------------------------------------------
!
      integer iseg, iseg3d0, nsegtot
      integer k, ilist, nlist, maxnumel, nj, errall
      integer ielem0
      integer n1, n2, n01, n02, iflux
      integer nflodel, ip, iws, iopt
      integer ielem
      integer j, i3

      integer, dimension(:), allocatable :: numel
      integer, dimension(:,:), allocatable :: nodeelt
      integer, dimension(:,:), allocatable :: segelt2

      double precision xseg1, yseg1, fseg1, useg1, vseg1
      double precision xseg2, yseg2, fseg2, useg2, vseg2
      double precision dxseg, dyseg, dfseg, dsseg
      double precision useg, vseg, ucomp
      double precision dxds, dyds, dfds
      double precision xseg1m, yseg1m, xseg2p, yseg2p, x0, y0
      double precision x1, x2, x3, y1, y2, y3
      double precision det1, det2, det3, f2, f3
      double precision det0, dfdx0, dfdy0, dfds0
      double precision flux2, weight
      double precision ux1, fLx, u1, uL, uLh, sign
      double precision newvol1, newvol2

      double precision zseg1, zseg2, dzseg, wseg1, wseg2, wseg
      double precision zseg01, zseg02, fseg01, fseg02
      double precision dfseg0, dsseg0

      double precision total1, total2, transfer, ratio
      
      double precision sum, total(npoin3)

      logical first

      double precision mm
      external mm

      data first /.true./
!
!-----------------------------------------------------------------------
!
      SAVE

      option2 = 1
!
      if (first) then
        first = .false.

        weight = 1.5d0

*       Construct array of node neighbours

        allocate(numel(npoin2), stat=errall)
        if (errall.ne.0) then
          write(*,*) 'Error allocating'
        endif

        do i = 1,npoin2
          numel(i) = 0
        end do

        do i = 1,mesh2d%nelem
          do j = 1,3
            nj = mesh2d%ikle%i(i+mesh2d%nelem*(j-1))
            numel(nj) = numel(nj) + 1
          end do
        end do

        maxnumel = 1
        do i = 1,npoin2
          if (numel(i).gt.maxnumel) then
            maxnumel = numel(i)
          endif
        end do

        allocate(nodeelt(npoin2,maxnumel), stat=errall)
        if (errall.ne.0) then
          write(*,*) 'Error allocating'
        endif

        do i = 1,npoin2
          numel(i) = 0
          do j = 1,maxnumel
            nodeelt(i,j) = 0
          end do
        end do

        do i = 1,mesh2d%nelem
          do j = 1,3
            nj = mesh2d%ikle%i(i+mesh2d%nelem*(j-1))
            numel(nj) = numel(nj) + 1
            nodeelt(nj,numel(nj)) = i
          end do
        end do

*       Construct an array of elements at ends of segments

        allocate(segelt2(nseg,2), stat=errall)
        if (errall.ne.0) then
          write(*,*) 'Error allocating'
        endif

        DO I = 1,NSEG

          xseg1 = mesh2d%x%r(gloseg(i,1))
          yseg1 = mesh2d%y%r(gloseg(i,1))

          xseg2 = mesh2d%x%r(gloseg(i,2))
          yseg2 = mesh2d%y%r(gloseg(i,2))

          dxseg = xseg2-xseg1
          dyseg = yseg2-yseg1
          dsseg = sqrt(dxseg**2+dyseg**2)

          dxds = dxseg/dsseg
          dyds = dyseg/dsseg

          xseg1m = xseg1 - 0.1d0 * dxseg
          yseg1m = yseg1 - 0.1d0 * dyseg
          xseg2p = xseg2 + 0.1d0 * dxseg
          yseg2p = yseg2 + 0.1d0 * dyseg

          do k = 1,2

          if (k.eq.1) then
            x0 = xseg1m
            y0 = yseg1m
          elseif (k.eq.2) then
            x0 = xseg2p
            y0 = yseg2p
          endif

          n1 = gloseg(i,k)
          nlist = numel(n1)
          ielem0 = 0

          do ilist = 1,nlist
          
            ielem = nodeelt(n1,ilist)
            
            i1 = mesh2d%ikle%i(ielem)
            i2 = mesh2d%ikle%i(ielem+mesh2d%nelem)
            i3 = mesh2d%ikle%i(ielem+mesh2d%nelem*2)

            x1 = mesh2d%x%r(i1)
            x2 = mesh2d%x%r(i2)
            y1 = mesh2d%y%r(i1)
            y2 = mesh2d%y%r(i2)
            x3 = mesh2d%x%r(i3)
            y3 = mesh2d%y%r(i3)

            det1 = (x2-x1)*(y0-y1) - (y2-y1)*(x0-x1)
            det2 = (x3-x2)*(y0-y2) - (y3-y2)*(x0-x2)
            det3 = (x1-x3)*(y0-y3) - (y1-y3)*(x0-x3)
 
            if (det1.ge.0.d0.and.det2.ge.0.d0.and.det3.ge.0.d0) then
              ielem0 = ielem
            endif

          end do

          segelt2(i,k) = ielem0

          end do

        END DO

        deallocate(numel)
        deallocate(nodeelt)

      endif
!
      CALL CPSTVC(SVOLU2,STRA01)
      CALL CPSTVC(SVOLU2,STRA02)
      CALL CPSTVC(SVOLU2,STRA03)
!
!     TRA03 WILL BE THE SHARED ASSEMBLED FLUEXT IN PARALLEL
!
      IF(NCSIZE.GT.1) THEN
        CALL OV('X=Y     ',TRA03,FLUEXT,FLUEXT,0.D0,NPOIN3)
!       TRA03 WILL BE THE ASSEMBLED AND SHARED FLUEXT
        CALL PARCOM(STRA03,2,MESH3)
!       SHARES AFTER SUMMING (AS WILL BEEN DONE WITH VOLUMES)
        DO IPTFR=1,NPTIR
          I2D=MESH2%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
          DO IPLAN=1,NPLAN
            I3D=I2D+(IPLAN-1)*NPOIN2
            TRA03(I3D)=TRA03(I3D)*MESH3%FAC%R(I3D)
          ENDDO
        ENDDO
      ENDIF
!
      NSEGH=NSEG*NPLAN
      NSEGV=NPOIN2*(NPLAN-1)
      IF(SCHCF.EQ.ADV_LPO_TF) THEN
!       HORIZONTAL AND VERTICAL SEGMENTS
        REMAIN_SEG=NSEGH+NSEGV
        OPT=1
      ELSEIF(SCHCF.EQ.ADV_NSC_TF) THEN
!       ALL SEGMENTS
        IF(IELM3.EQ.41) THEN
          REMAIN_SEG=NSEGH+NSEGV+2*NSEG*(NPLAN-1)
        ELSEIF(IELM3.EQ.51) THEN
          REMAIN_SEG=NSEGH+NSEGV+NSEG*(NPLAN-1)
        ELSE
          WRITE(LU,*) 'UNKNOWN ELEMENT IN MURD3D_POS:',IELM3
          CALL PLANTE(1)
          STOP
        ENDIF
        OPT=2
      ELSE
        WRITE(LU,*) 'UNKNOWN SCHEME IN MURD3D_POS:',SCHCF
        CALL PLANTE(1)
        STOP
      ENDIF
!
      REMAIN_SEG_INIT=REMAIN_SEG
!
      IF(.NOT.DEJA) THEN  
        ALLOCATE(INDIC(REMAIN_SEG)) 
        SIZEINDIC=REMAIN_SEG   
        DEJA=.TRUE.
      ELSE
        IF(REMAIN_SEG.GT.SIZEINDIC) THEN
!         LARGER SIZE OF INDIC REQUIRED (CASE OF SEVERAL CALLS WITH
!         DIFFERENT SCHEMES THAT IMPLY DIFFERENT NUMBERS OF SEGMENTS
!         LIKE SCHEMES LPO_TF AND NSC_TF IN PRISMS
          DEALLOCATE(INDIC) 
          ALLOCATE(INDIC(REMAIN_SEG)) 
          SIZEINDIC=REMAIN_SEG 
        ENDIF
      ENDIF
!
      IF(option2.EQ.2) CALL CPSTVC(SVOLU2,FLUX_REMOVED)
!
!***********************************************************************
!
!     COPIES FLUXES FROM FLODEL TO ARRAY RMASS (REMAINING MASSES
!     TO BE TRANSFERRED AND THEIR ADDRESS IN INDIC)
!
      IF(option2.EQ.1) THEN
        DO I=1,REMAIN_SEG
          INDIC(I)=I
          RMASS(I)=DT*FLOPAR(I)
        ENDDO
      ELSEIF(option2.EQ.2) THEN
        DO I=1,REMAIN_SEG
          INDIC(I)=I
          RMASS(I)=-DT*FLOPAR(I)
        ENDDO
      ENDIF
!
!     SHARES ASSEMBLED FLUXES ON INTERFACE SEGMENTS BY:
!     DIVIDING BY 2 ON INTERFACE HORIZONTAL AND CROSSED SEGMENTS
!     MULTIPLYING BY FAC ON VERTICAL FLUXES
!     THIS WILL GIVE THE SAME UPWINDING INFORMATION
!
      IF(NCSIZE.GT.1) THEN
        CALL SHARE_3D_FLUXES(RMASS,1.D0,NPLAN,MESH2,MESH3,OPT)
      ENDIF
!
!     REMAINING FLUXES (SPLIT INTO POSITIVE AND NEGATIVE
!                       SO THAT THEY SUM CORRECTLY IN PARALLEL
!                       ABSOLUTE VALUES WOULD NOT SUM CORRECTLY)
!
      RFLUX_OLD=0.D0
      DO I=1,REMAIN_SEG
        RFLUX_OLD=RFLUX_OLD+ABS(RMASS(I))
      ENDDO
      IF(NCSIZE.GT.1) RFLUX_OLD=P_DSUM(RFLUX_OLD)
      RINIT=RFLUX_OLD
      IF(TESTING) WRITE(LU,*) 'SOMME INITIALE DES ABS(FLUX)=',RINIT/DT
!
!     INITIAL VALUE OF TRACER = FN
!
      CALL OV ('X=Y     ',FC,FN,FN,C,NPOIN3)
!
!     VOLU2 WILL BE THE VOLUME CHANGING PROGRESSIVELY FROM VOLUN TO VOLU
!
      CALL OS ('X=Y     ',X=SVOLU2,Y=SVOLUN) 
!
!     TAKES INTO ACCOUNT ENTERING EXTERNAL FLUXES
!     THIS IS DONE WITHOUT CHANGING THE TRACER
!    (BUT THE MASS OF TRACER CHANGES)
!
      IF(NCSIZE.GT.1) THEN
        DO I=1,NPOIN3
!         FLUEXT SHARED IN PARALLEL (HENCE SAME SIGN)
          VOLU2(I)=VOLU2(I)-MIN(TRA03(I),0.D0)*DT
        ENDDO
      ELSE
        DO I=1,NPOIN3
!         FLUEXT SHARED IN PARALLEL (HENCE SAME SIGN)
          VOLU2(I)=VOLU2(I)-MIN(FLUEXT(I),0.D0)*DT
        ENDDO
      ENDIF
!
      IF(CALFLU) THEN
        IF(NCSIZE.GT.1) THEN
          DO IPOIN = 1,NPOIN3
            FLUX = FLUX + DT*FC(IPOIN)*MIN(TRA03(IPOIN),0.D0)
          ENDDO
        ELSE
          DO IPOIN = 1,NPOIN3
            FLUX = FLUX + DT*FC(IPOIN)*MIN(FLUEXT(IPOIN),0.D0)
          ENDDO
        ENDIF
!       ENTERING SOURCES
        IF(NSCE.GT.0) THEN
          DO IS=1,NSCE
            IIS=IS
!           HERE IN PARALLEL SOURCES WITHOUT PARCOM
!           ARE STORED AT ADRESSES IS+NSCE (SEE SOURCES_SINKS.F)
            IF(NCSIZE.GT.1) IIS=IIS+NSCE
            DO IPOIN=1,NPOIN3
              IF(SOURCES%ADR(IS)%P%R(IPOIN).GT.0.D0) THEN
                FLUX=FLUX-DT*FSCE(IS)*SOURCES%ADR(IIS)%P%R(IPOIN)
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(NCSIZE.GT.1) CALL PARCOM(SVOLU2,2,MESH3)
!
!     ENTERING SOURCES (WITH FC = FSCE)
!     IT WILL ALWAYS GIVE POSITIVE VOLUMES
      IF(NSCE.GT.0) THEN
        DO IS=1,NSCE
          DO IPOIN=1,NPOIN3
!           HERE VERSION OF SOURCES ASSEMBLED IN PARALLEL
            IF(SOURCES%ADR(IS)%P%R(IPOIN).GT.0.D0) THEN
              VOLU2(IPOIN)=VOLU2(IPOIN)+DT*SOURCES%ADR(IS)%P%R(IPOIN)
              FC(IPOIN)=FC(IPOIN)+DT*(FSCE(IS)-FC(IPOIN))
     &                       *SOURCES%ADR(IS)%P%R(IPOIN)/VOLU2(IPOIN)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
!     RAIN-EVAPORATION (HERE ONLY RAIN, NOT EVAPORATION, HENCE 
!                       VALUE OF TRACER IN RAIN TAKEN INTO ACCOUNT)
!
      IF(RAIN) THEN
        DO IPOIN=1,NPOIN2
          IF(PLUIE(IPOIN).GT.0.D0) THEN
            IS=NPOIN3-NPOIN2+IPOIN
!           ASSEMBLED FORM OF PLUIE NEEDED HERE
            VOLU2(IS)=VOLU2(IS)+DT*PLUIE(IPOIN)
!           DILUTION EFFECT FOR ALL TRACERS 
            FC(IS)=FC(IS)+DT*(TRAIN-FC(IS))*PLUIE(IPOIN)/VOLU2(IS)
          ENDIF
        ENDDO
      ENDIF
!
      NITER = 0
!
777   CONTINUE
!
      NITER = NITER + 1
!
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     FOR DISTRIBUTING THE VOLUMES BETWEEN SEGMENTS
!
      IF(option2.EQ.2) THEN
!
!       FLUX_REMOVED (T6)    : TOTAL FLUX REMOVED OF EACH POINT
!       SAVED_VOLU2 (T8)     : VOLUME VOLU2 SAVED
!       SAVED_F (T9)         : TRACER SAVED
!
        IF(NITER.EQ.1) THEN
          DO I=1,NPOIN3
            FLUX_REMOVED%R(I)=0.D0
            SAVED_VOLU2%R(I)=VOLU2(I)
            SAVED_F%R(I)=FC(I)
            T5%R(I)=FC(I)*VOLU2(I)
          ENDDO
          IF(NCSIZE.GT.1) THEN
!           SHARES AFTER SUMMING (AS HAS BEEN DONE WITH FLUXES)
            DO IPTFR=1,NPTIR
              I2D=MESH2%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
              DO IPLAN=1,NPLAN
                I3D=I2D+(IPLAN-1)*NPOIN2
                VOLU2(I3D)=VOLU2(I3D)*MESH3%FAC%R(I3D)
                T5%R(I3D) = T5%R(I3D)*MESH3%FAC%R(I3D)
              ENDDO
            ENDDO
          ENDIF
        ELSE
!         NOT ALL THE POINTS NEED TO BE INITIALISED NOW
          DO IR=1,REMAIN_SEG
            I=INDIC(IR)
            I1=GLOSEG(I,1)
            I2=GLOSEG(I,2)
            FLUX_REMOVED%R(I1)=0.D0
            FLUX_REMOVED%R(I2)=0.D0
!           SAVING THE DEPTH AND TRACER
            SAVED_VOLU2%R(I1)=VOLU2(I1)
            SAVED_VOLU2%R(I2)=VOLU2(I2)
            SAVED_F%R(I1)=FC(I1)
            SAVED_F%R(I2)=FC(I2)
            T5%R(I1)=FC(I1)*VOLU2(I1)
            T5%R(I2)=FC(I2)*VOLU2(I2)
          ENDDO
!         CANCELLING INTERFACE POINTS (SOME MAY BE ISOLATED IN A SUBDOMAIN
!         AT THE TIP OF AN ACTIVE SEGMENT WHICH IS ELSEWHERE)
          IF(NCSIZE.GT.1) THEN
            DO IPTFR=1,NPTIR
              I2D=MESH3%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
              DO IPLAN=1,NPLAN
                I3D=(IPLAN-1)*NPOIN2+I2D
                FLUX_REMOVED%R(I3D)=0.D0
!               SAVING THE VOLUME AND TRACER
                SAVED_VOLU2%R(I3D)=VOLU2(I3D)
                SAVED_F%R(I3D)=FC(I3D)
                VOLU2(I3D)=VOLU2(I3D)*MESH3%FAC%R(I3D)
                T5%R(I3D) = T5%R(I3D)*MESH3%FAC%R(I3D)
              ENDDO
            ENDDO
          ENDIF
        ENDIF
        DO I=1,REMAIN_SEG
          ISEG3D=INDIC(I)
          I1=GLOSEG(ISEG3D,1)
          I2=GLOSEG(ISEG3D,2)
!         POSITIVE FLUXES FROM 1 TO 2 !!!
          IF(RMASS(ISEG3D).GT.EPS_VOLUME) THEN
            FLUX_REMOVED%R(I1)=FLUX_REMOVED%R(I1)+RMASS(ISEG3D)
            VOLU2(I1)=0.D0
            T5%R(I1)=0.D0
          ELSEIF(RMASS(ISEG3D).LT.-EPS_VOLUME) THEN
            FLUX_REMOVED%R(I2)=FLUX_REMOVED%R(I2)-RMASS(ISEG3D)
            VOLU2(I2)=0.D0
            T5%R(I2)=0.D0
          ENDIF
        ENDDO
!
        IF(NCSIZE.GT.1) CALL PARCOM(FLUX_REMOVED,2,MESH3)
!
!       FOR ISOLATED POINTS CONNECTED TO AN ACTIVE SEGMENT
!       THAT IS IN ANOTHER SUBDOMAIN
        IF(NCSIZE.GT.1) THEN
          DO IPTFR=1,NPTIR
            I2D=MESH3%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
            DO IPLAN=1,NPLAN
              I3D=(IPLAN-1)*NPOIN2+I2D
              IF(FLUX_REMOVED%R(I3D).GT.EPS_VOLUME) THEN
!               ALL VOLUME SHARED
                VOLU2(I3D)=0.D0
                T5%R(I3D)=0.D0
              ENDIF
            ENDDO
          ENDDO
        ENDIF
!
      ELSEIF(option2.EQ.1) THEN
!
        IF(NCSIZE.GT.1) THEN
!         SHARES AFTER SUMMING (AS HAS BEEN DONE WITH FLUXES)
          DO IPTFR=1,NPTIR
            I2D=MESH2%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
            DO IPLAN=1,NPLAN
              I3D=I2D+(IPLAN-1)*NPOIN2
              VOLU2(I3D)=VOLU2(I3D)*MESH3%FAC%R(I3D)
            ENDDO
          ENDDO
        ENDIF
!
      ENDIF
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
!     FROM HERE RMASS IS THE REMAINING MASSES TO BE PASSED BETWEEN POINTS
!
!     COMPUTES THE NEW VOLUME WITH FV SCHEME + DB
!
!     HORIZONTAL AND VERTICAL FLUXES TREATED TOGETHER
!
      RFLUX=0.D0
      NEWREMAIN=0
!
      IF(option2.EQ.1) THEN
!
      DO I=1,REMAIN_SEG
        ISEG3D=INDIC(I)
        IF(RMASS(ISEG3D).GT.EPS_VOLUME) THEN
            iflux = 1
            iseg3d0 = iseg3d + npoin2
            k  = 2
            n1 = gloseg(iseg3d,2)
            n2 = gloseg(iseg3d,1)
            sign = -1.d0
        ELSEIF(RMASS(ISEG3D).LT.-EPS_VOLUME) THEN
            iflux = 1
            iseg3d0 = iseg3d - npoin2
            k  = 1
            n1 = gloseg(iseg3d,1)
            n2 = gloseg(iseg3d,2)
            sign = 1.d0
        ENDIF

          if (iflux.eq.1) then

            if (iseg3d.le.nsegh) then

!           HORIZONTAL SEGMENT
            
            iplan = (iseg3d-1)/nseg + 1
            iseg = iseg3d - (iplan-1)*nseg

            xseg1 = mesh3%x%r(n1)
            yseg1 = mesh3%y%r(n1)
            fseg1 = fc(n1)
            useg1 = uconv%r(n1)
            vseg1 = vconv%r(n1)

            xseg2 = mesh3%x%r(n2)
            yseg2 = mesh3%y%r(n2)
            fseg2 = fc(n2)
            useg2 = uconv%r(n2)
            vseg2 = vconv%r(n2)

            dxseg = xseg2-xseg1
            dyseg = yseg2-yseg1
            dfseg = fseg2-fseg1
            dsseg = sqrt(dxseg**2+dyseg**2)

            useg = 0.5d0*(useg1+useg2)
            vseg = 0.5d0*(vseg1+vseg2)
            ucomp = (useg*dxseg + vseg*dyseg)/dsseg

            dxds = dxseg/dsseg
            dyds = dyseg/dsseg
            dfds = dfseg/dsseg

            ielem0 = segelt2(iseg,k)

            if (ielem0.ne.0) then

              x2 = mesh2d%xel%r(ielem0+mesh2d%nelem)
              x3 = mesh2d%xel%r(ielem0+mesh2d%nelem*2)
              y2 = mesh2d%yel%r(ielem0+mesh2d%nelem)
              y3 = mesh2d%yel%r(ielem0+mesh2d%nelem*2)

              i1 = mesh2d%ikle%i(ielem0)
              i2 = mesh2d%ikle%i(ielem0+mesh2d%nelem)
              i3 = mesh2d%ikle%i(ielem0+mesh2d%nelem*2)

              i1 = (iplan-1)*npoin2 + i1
              i2 = (iplan-1)*npoin2 + i2
              i3 = (iplan-1)*npoin2 + i3

              f2 = fc(i2)-fc(i1)
              f3 = fc(i3)-fc(i1)

              det0 = x2*y3-x3*y2

              dfdx0 = (f2*y3-f3*y2)/det0
              dfdy0 = (x2*f3-x3*f2)/det0

              dfds0 = dfdx0 * dxds + dfdy0 * dyds

            else

              dfds0 = 0.d0

            endif

            else

!           VERTICAL SEGMENT

            zseg1 = mesh3%z%r(n1)
            fseg1 = fc(n1)

            zseg2 = mesh3%z%r(n2)
            fseg2 = fc(n2)

            dzseg = zseg2-zseg1
            dfseg = fseg2-fseg1
            dsseg = abs(dzseg)

            wseg = wsconv%r(iseg3d-nsegh)

            if (k.eq.1) then
              ucomp = wseg
            else
              ucomp = -wseg
            endif

            if (dsseg.gt.0.d0) then
              dfds = dfseg/dsseg
            else
              dfds = 0.d0
            endif

            if ((iseg3d0.gt.nsegh).and.
     $          (iseg3d0.le.nsegh+nsegv)) then

              if (k.eq.1) then
                n01 = gloseg(iseg3d0,1)
                n02 = gloseg(iseg3d0,2)
              else
                n01 = gloseg(iseg3d0,2)
                n02 = gloseg(iseg3d0,1)
              endif

              zseg01 = mesh3%z%r(n01)
              fseg01 = fc(n01)
              zseg02 = mesh3%z%r(n02)
              fseg02 = fc(n02)
              dsseg0 = abs(zseg02-zseg01)
              dfseg0 = fseg02-fseg01
              if (dsseg0.gt.0.d0) then
                dfds0 = dfseg0/dsseg0
              else
                dfds0 = 0.d0
              endif
            else
              dfds0 = 0.d0
            endif

            endif

            ux1 = mm(weight,dfds0,dfds)

            if (abs(ux1).lt.1e-10) then
              ux1 = 0.d0
            endif

            fLx = ucomp * ux1

            u1  = fc(n1)
            uL  = u1 + dsseg * ux1 * 0.5d0
            uLh = uL

            if (uL.gt.u1) then
              if ((uLh.lt.u1).or.(uLh.gt.uL)) then
                uLh = u1
              endif
            elseif (uL.lt.u1) then
              if ((uLh.lt.uL).or.(uLh.gt.u1)) then
                uLh = u1
              endif
            else
              uLh = u1
            endif

          endif

        IF(RMASS(ISEG3D).GT.EPS_VOLUME) THEN
!         FLUX FROM 2 TO 1 !!! (SEE REMARKS AND HOW RMASS INITIALISED)
          I1=GLOSEG(ISEG3D,1)
          I2=GLOSEG(ISEG3D,2)
          IF(RMASS(ISEG3D).GT.VOLU2(I2)) THEN
            RMASS(ISEG3D)=RMASS(ISEG3D)-VOLU2(I2)
            newvol2=0.d0
            NEWVOL1=VOLU2(I1)+VOLU2(I2)
            IF(NEWVOL1.GT.0.D0) THEN

              fc(i1)=fc(i1)*(1.d0-volu2(i2)/newvol1)
     $              +u1*volu2(i2)/newvol1

              VOLU2(I2)=newvol2
              VOLU2(I1)=NEWVOL1
            ENDIF
            RFLUX=RFLUX+RMASS(ISEG3D)
            NEWREMAIN=NEWREMAIN+1
            INDIC(NEWREMAIN)=ISEG3D
          ELSE
            total2 = fc(i2)*volu2(i2)
            transfer = uLh * rmass(iseg3d)

            newvol2=volu2(i2)-rmass(iseg3d)

            if (newvol2.gt.1d-14) then
              ratio = volu2(i2)/newvol2
            else
              ratio = 1.d7
            endif
            if (ratio.gt.2.d0) then
              transfer = u1 * rmass(iseg3d)
            endif

            NEWVOL1=VOLU2(I1)+RMASS(ISEG3D)
            IF(NEWVOL1.GT.0.D0) THEN

              if (ratio.lt.1.d6) then
                fc(i2)=fc(i2)*(1.d0+rmass(iseg3d)/newvol2)
     $                -transfer/newvol2
              endif

              fc(i1)=fc(i1)*(1.d0-rmass(iseg3d)/newvol1)
     $              +transfer/newvol1

            ENDIF
            volu2(i2)=newvol2
            VOLU2(I1)=NEWVOL1
          ENDIF
        ELSEIF(RMASS(ISEG3D).LT.-EPS_VOLUME) THEN
!         FLUX FROM 1 TO 2 !!! (SEE REMARKS AND HOW RMASS INITIALISED)
          I1=GLOSEG(ISEG3D,1)
          I2=GLOSEG(ISEG3D,2)
          IF(-RMASS(ISEG3D).GT.VOLU2(I1)) THEN
            RMASS(ISEG3D)=RMASS(ISEG3D)+VOLU2(I1)
            newvol1=0.d0
            NEWVOL2=VOLU2(I2)+VOLU2(I1)
            IF(NEWVOL2.GT.0.D0) THEN

              fc(i2)=fc(i2)*(1.d0-volu2(i1)/newvol2)
     $              +u1*volu2(i1)/newvol2

              volu2(i1)=newvol1
              VOLU2(I2)=NEWVOL2
            ENDIF
            RFLUX=RFLUX-RMASS(ISEG3D)
            NEWREMAIN=NEWREMAIN+1
            INDIC(NEWREMAIN)=ISEG3D
          ELSE
            total1 = fc(i1)*volu2(i1)
            transfer = -uLh * rmass(iseg3d)

            newvol1=volu2(i1)+rmass(iseg3d)

            if (newvol1.gt.1e-14) then
              ratio = volu2(i1)/newvol1
            else
              ratio = 1.d7
            endif
            if (ratio.gt.2.d0) then
              transfer = -u1 * rmass(iseg3d)
            endif

            NEWVOL2=VOLU2(I2)-RMASS(ISEG3D)
            IF(NEWVOL2.GT.0.D0) THEN

              if (ratio.lt.1.d6) then
                fc(i1)=fc(i1)*(1.d0-rmass(iseg3d)/newvol1)
     $                -transfer/newvol1
              endif

              fc(i2)=fc(i2)*(1.d0+rmass(iseg3d)/newvol2)
     $              +transfer/newvol2

            ENDIF
            volu2(i1)=newvol1
            VOLU2(I2)=NEWVOL2
          ENDIF
        ENDIF
      ENDDO
!
      ELSEIF(option2.EQ.2) THEN
!
      DO IR=1,REMAIN_SEG
        I=INDIC(IR)
        IF(RMASS(I).GT.EPS_VOLUME) THEN
          I1=GLOSEG(I,1)
!         FLUX FROM 1 TO 2 !!! (SEE REMARKS AND HOW RMASS INITIALISED)
          IF(SAVED_VOLU2%R(I1).GT.0.D0) THEN
            I2=GLOSEG(I,2)
!           SHARING ON DEMAND: RMASS(I)/FLUX_REMOVED%R(I1) IS A PERCENTAGE
            VOLSEG1=SAVED_VOLU2%R(I1)*RMASS(I)/FLUX_REMOVED%R(I1)
!           END OF SHARING ON DEMAND
            IF(RMASS(I).GE.VOLSEG1) THEN
!             ALL VOLSEG1 WILL BE TRANSFERED TO POINT2
!             VOLSEG1 > 0, HENCE VOLU2(I2) ALSO
              RMASS(I) =RMASS(I) -VOLSEG1
              VOLU2(I2)=VOLU2(I2)+VOLSEG1
!             GROUPING H*F
              T5%R(I2)=T5%R(I2)+VOLSEG1*SAVED_F%R(I1)
!             THIS MAY BE DONE SEVERAL TIMES FOR THE SAME POINT
!             BUT THE LAST ONE WILL BE THE GOOD ONE
              FC(I2)=T5%R(I2)/VOLU2(I2)
              RFLUX=RFLUX+RMASS(I)
              NEWREMAIN=NEWREMAIN+1
              INDIC(NEWREMAIN)=I
            ELSE
              VOLSEG1=VOLSEG1-RMASS(I)
!             GATHERING VOLUMES (HERE VOLU2(I2) WILL REMAIN POSITIVE)
              VOLU2(I2)=VOLU2(I2)+RMASS(I)
              VOLU2(I1)=VOLU2(I1)+VOLSEG1
              T5%R(I1)=T5%R(I1)+VOLSEG1*SAVED_F%R(I1)
              T5%R(I2)=T5%R(I2)+RMASS(I)*SAVED_F%R(I1)
              FC(I1)=T5%R(I1)/VOLU2(I1)
              FC(I2)=T5%R(I2)/VOLU2(I2)
            ENDIF
          ELSE
            RFLUX=RFLUX+RMASS(I)
            NEWREMAIN=NEWREMAIN+1
            INDIC(NEWREMAIN)=I
          ENDIF
        ELSEIF(RMASS(I).LT.-EPS_VOLUME) THEN
          I2=GLOSEG(I,2)
!         FLUX FROM 2 TO 1 !!! (SEE REMARKS AND HOW RMASS INITIALISED)
          IF(SAVED_VOLU2%R(I2).GT.0.D0) THEN
            I1=GLOSEG(I,1)
!           SHARING ON DEMAND
            VOLSEG2=-SAVED_VOLU2%R(I2)*RMASS(I)/FLUX_REMOVED%R(I2)
!           END OF SHARING ON DEMAND
            IF(-RMASS(I).GE.VOLSEG2) THEN
!             ALL VOLSEG2 WILL BE TRANSFERED TO POINT 1
!             VOLSEG2 > 0, HENCE VOLU2(I1) ALSO
              VOLU2(I1)=VOLU2(I1)+VOLSEG2
              RMASS(I) =RMASS(I) +VOLSEG2
              T5%R(I1)=T5%R(I1)+VOLSEG2*SAVED_F%R(I2)
              FC(I1)=T5%R(I1)/VOLU2(I1)
              RFLUX=RFLUX-RMASS(I)
              NEWREMAIN=NEWREMAIN+1
              INDIC(NEWREMAIN)=I
            ELSE
              VOLSEG2=VOLSEG2+RMASS(I)
!             GATHERING VOLUMES (HERE VOLU2(I1) WILL REMAIN POSITIVE)
              VOLU2(I1)=VOLU2(I1)-RMASS(I)
              VOLU2(I2)=VOLU2(I2)+VOLSEG2
              T5%R(I1)=T5%R(I1)-RMASS(I)*SAVED_F%R(I2)
              T5%R(I2)=T5%R(I2)+VOLSEG2*SAVED_F%R(I2)
              FC(I1)=T5%R(I1)/VOLU2(I1)
              FC(I2)=T5%R(I2)/VOLU2(I2)
            ENDIF
          ELSE
            RFLUX=RFLUX-RMASS(I)
            NEWREMAIN=NEWREMAIN+1
            INDIC(NEWREMAIN)=I
          ENDIF
        ENDIF
      ENDDO
!
!     ELSE
!       UNKNOWN option2
      ENDIF
!
      REMAIN_SEG=NEWREMAIN
!
!     MERGES VOLUMES AND FC AT INTERFACE POINTS
!
      IF(NCSIZE.GT.1) THEN
        DO IPTFR=1,NPTIR
          I2D=MESH2%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
          DO IPLAN=1,NPLAN
            I3D=I2D+(IPLAN-1)*NPOIN2
!           ARRAY WITH VOLUME*FC AT INTERFACE POINTS
            TRA01(I3D)=VOLU2(I3D)*FC(I3D)
          ENDDO
        ENDDO
!       SUMS VOLUME*FC AT INTERFACE POINTS
        CALL PARCOM(STRA01,2,MESH3)
!       SUMS THE NEW POSITIVE PARTIAL VOLUMES OF INTERFACE POINTS
        CALL PARCOM(SVOLU2,2,MESH3)
        DO IPTFR=1,NPTIR
          I2D=MESH2%NACHB%I(NBMAXNSHARE*(IPTFR-1)+1)
          DO IPLAN=1,NPLAN
            I3D=I2D+(IPLAN-1)*NPOIN2
!           ARRAY WITH VOLUME*F AT INTERFACE POINTS
            IF(VOLU2(I3D).GT.0.D0) THEN
              FC(I3D)=TRA01(I3D)/VOLU2(I3D)
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
      REMAIN_TOT=REMAIN_SEG
      IF(NCSIZE.GT.1) THEN
        RFLUX=P_DSUM(RFLUX)
!       WILL NOT SUM CORRECTLY IN PARALLEL, BUT ONLY TEST IF .EQ.0
        REMAIN_TOT=P_ISUM(REMAIN_TOT)
      ENDIF
!
!     4 POSSIBLE REASONS FOR STOPPING:
!
!     1) THERE IS NO REMAINING FLUX
!     2) REMAINING FLUXES DO NOT CHANGE (POSITIVE AND NEGATIVE)
!        WITH SOME ABSOLUTE ALLOWANCE OR REDUCTION COEFFICIENT
!     3) ALL SEGMENTS HAVE BEEN TREATED
!     4) MAXIMUM NUMBER OF ITERATIONS IS REACHED
!
      IF( RFLUX.NE.0.D0                                   .AND.
     &    ABS(RFLUX-RFLUX_OLD).GT.MIN(RINIT*REDUC,ALLOW)  .AND.
     &               REMAIN_TOT.NE.0                      .AND.
     &               NITER.LT.NITMAX                   ) THEN
        RFLUX_OLD=RFLUX
        GO TO 777
      ENDIF  
!
!     TAKES INTO ACCOUNT EXITING EXTERNAL FLUXES
!     THIS IS DONE WITHOUT CHANGING THE TRACER
!    (BUT THE MASS OF TRACER CHANGES)
!
      IF(CALFLU) THEN
!       EXITING FLUXES
        IF(NCSIZE.GT.1) THEN
          DO IPOIN = 1,NPOIN3
            FLUX = FLUX + DT*FC(IPOIN)*MAX(TRA03(IPOIN),0.D0)
          ENDDO
        ELSE
          DO IPOIN = 1,NPOIN3
            FLUX = FLUX + DT*FC(IPOIN)*MAX(FLUEXT(IPOIN),0.D0)
          ENDDO
        ENDIF
!       EXITING SOURCES
        IF(NSCE.GT.0) THEN
          DO IS=1,NSCE
            IIS=IS
!           HERE IN PARALLEL SOURCES WITHOUT PARCOM
!           ARE STORED AT ADRESSES IS+NSCE (SEE SOURCES_SINKS.F)
            IF(NCSIZE.GT.1) IIS=IIS+NSCE
            DO IPOIN=1,NPOIN3
              IF(SOURCES%ADR(IS)%P%R(IPOIN).LT.0.D0) THEN
                FLUX=FLUX
     &              -DT*FC(IPOIN)*SOURCES%ADR(IIS)%P%R(IPOIN)
              ENDIF
            ENDDO
          ENDDO
        ENDIF
      ENDIF
!
!     RAIN-EVAPORATION (HERE ONLY EVAPORATION, NOT RAIN, HENCE
!                       VALUE OF TRACER IN RAIN NOT TAKEN INTO ACCOUNT
!                       AND ASSUMED TO BE 0)
!
      IF(RAIN) THEN
        DO IPOIN=1,NPOIN2
          IF(PLUIE(IPOIN).LT.0.D0) THEN
            IS=NPOIN3-NPOIN2+IPOIN
            VOLU2(IS)=VOLU2(IS)+DT*PLUIE(IPOIN)
!           DIVISION BY 0 NOT CHECKED (BUT COULD BE A PROBLEM AS PLUIE<0)
!           CONCENTRATION EFFECT FOR ALL TRACERS (WRONG FOR TEMPERATURE ??)
            FC(IS)=FC(IS)-DT*FC(IS)*PLUIE(IPOIN)/VOLU2(IS)
          ENDIF
        ENDDO
      ENDIF
!
      IF(TESTING) THEN
!       EXITING SOURCES (WITH FC UNCHANGED)
        IF(NSCE.GT.0) THEN
          DO IS=1,NSCE
            DO IPOIN=1,NPOIN3
!             HERE VERSION OF SOURCES ASSEMBLED IN PARALLEL
              IF(SOURCES%ADR(IS)%P%R(IPOIN).LT.0.D0) THEN
                VOLU2(IPOIN)=VOLU2(IPOIN)+
     &                      DT*SOURCES%ADR(IS)%P%R(IPOIN)
              ENDIF
            ENDDO
          ENDDO
        ENDIF
!       EXITING FLUXES
        IF(NCSIZE.GT.1) THEN
          DO I=1,NPOIN3
            TRA02(I)=MAX(TRA03(I),0.D0)
!           SHARED FLUEXT IN TRA03 ERASED NOW (NOT USED AFTER)
!           NOW VOLU IS COPIED INTO TRA03 FOR PARCOM
            TRA03(I)=VOLU(I)
          ENDDO
        ELSE
          DO I=1,NPOIN3
            TRA02(I)=MAX(FLUEXT(I),0.D0)
            TRA03(I)=VOLU(I)
          ENDDO
        ENDIF
        IF(NCSIZE.GT.1) THEN
          CALL PARCOM(STRA02,2,MESH3)
          CALL PARCOM(STRA03,2,MESH3)
        ENDIF
!!!!!
!!!!!   IMPORTANT WARNING: THIS SHOULD BE DONE EVEN WITHOUT TESTING
!!!!!   IF WE WANT THE CORRECT VOLU2, BUT VOLU2 HERE IS NOT USED
!!!!!   AFTER.
!!!!!
        DO I=1,NPOIN3
          VOLU2(I)=VOLU2(I)-TRA02(I)*DT
          IF(VOLU2(I).LT.0.D0) THEN
            WRITE(LU,*) 'POINT ',I,' VOLU2=',VOLU2(I)
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
!!!!!
!!!!!   END OF IMPORTANT NOTE
!!!!!
!       CHECKS EQUALITY OF ASSEMBLED VOLUMES
        C=0.D0
        IF(NCSIZE.GT.1) THEN
          DO I=1,NPOIN3
            C=C+ABS(VOLU2(I)-TRA03(I))*MESH3%FAC%R(I)
          ENDDO
          C=P_DSUM(C)
        ELSE
          DO I=1,NPOIN3
!                            VOLU
            C=C+ABS(VOLU2(I)-TRA03(I))
          ENDDO
        ENDIF
        WRITE(LU,*) 'ERROR ON VOLUMES = ',C
      ENDIF
!
!-----------------------------------------------------------------------
!
!     EXPLICIT SOURCE TERMS
!
      IF(S0F%TYPR.NE.'0') THEN
        DO IPOIN=1,NPOIN3
          IF(VOLU2(IPOIN).GT.EPS) THEN
            FC(IPOIN)=FC(IPOIN)+DT*S0F%R(IPOIN)/VOLU2(IPOIN)
          ENDIF
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      IF(INFOR) THEN
        IF(LNG.EQ.1) WRITE(LU,101) SCHCF,NITER
        IF(LNG.EQ.2) WRITE(LU,102) SCHCF,NITER
      ENDIF
!
101   FORMAT(1X,'MURD3D_POS option2: ',1I4,'  ',1I4,' ITERATIONS')
102   FORMAT(1X,'MURD3D_POS option2: ',1I4,'  ',1I4,' ITERATIONS')
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C
      INTEGER FUNCTION USIGN( w )
      IMPLICIT NONE
      DOUBLE PRECISION :: w
         USIGN = 1; IF( w.LT.0.D0 ) USIGN = -1
      END FUNCTION

      DOUBLE PRECISION FUNCTION mm( w, a,b )
      IMPLICIT NONE
      INTEGER :: USIGN; DOUBLE PRECISION :: w,a,b
      INTRINSIC DABS,DMIN1; EXTERNAL USIGN
      mm = w * 0.5D0 * ( USIGN(a)+USIGN(b) )*DMIN1( DABS(a),DABS(b) )
      mm = 0.5D0 * ( USIGN(a+b)+USIGN(mm) )*
     &           DMIN1( 0.5D0*DABS(a+b),DABS(mm) )
      END FUNCTION
C
C#######################################################################
C
