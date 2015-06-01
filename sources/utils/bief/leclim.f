!                    *****************
                     SUBROUTINE LECLIM
!                    *****************
!
     &(LIHBOR,LIUBOR,LIVBOR,LITBOR,HBOR,UBOR,VBOR,TBOR,
     & CHBORD,ATBOR,BTBOR,NPTFR,CODE,TRAC,FFORMAT,NGEO,
     & KENT,KENTU,KSORT,KADH,KLOG,KINC,NUMLIQ,MESH,BOUNDARY_COLOUR,
     & NPTFR2)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    READS THE BOUNDARY CONDITIONS FILE AND
!+                STORES IN ARRAYS THE DATA READ.
!
!history  J-M HERVOUET (LNHE)
!+        09/07/2009
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
!history Y AUDOUIN (LNHE)
!+       25/05/2015
!+       V7P0
!+       Modification to comply with the hermes module
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ATBOR,BTBOR    |<--| THERMAL EXCHANGE COEFFICIENTS.
!| BOUNDARY_COLOUR|<--| COLOUR OF BOUNDARY POINT (DEFAULT: ITS RANK)
!| CHBORD         |<--| FRICTION COEFFICIENT AT BOUNDARY
!| HBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON DEPTH
!| KADH           |-->| CONVENTION FOR NO SLIP BOUNDARY CONDITION
!| KENT           |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VALUE
!| KENTU          |-->| CONVENTION FOR LIQUID INPUT WITH PRESCRIBED VELOCITY
!| KINC           |-->| CONVENTION FOR INCIDENT WAVE BOUNDARY CONDITION
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| KSORT          |-->| CONVENTION FOR FREE OUTPUT
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LITBOR         |-->| PHYSICAL BOUNDARY CONDITIONS FOR TRACERS
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON U
!| LIVBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON V
!| MESH           |-->| MESH STRUCTURE
!| NGEO           |-->| LOGICAL UNIT OF BOUNDARY CONDITIONS FILE
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NPTFR2         |-->| NUMBER OF QUADRATIC BOUNDARY POINTS
!| NUMLIQ         |-->| LIQUID BOUNDARY NUMBER OF BOUNDARY POINTS
!| STDGEO         |-->| STANDARD OF GEOMETRY FILE.
!| TBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON TRACER
!| TRAC           |-->| IF YES, THERE ARE TRACERS
!| UBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY U
!| VBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY V
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_LECLIM => LECLIM
      USE DECLARATIONS_SPECIAL
      USE INTERFACE_HERMES
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)    :: NGEO,KENT,KSORT,KADH,KLOG,KINC,KENTU
      INTEGER, INTENT(IN)    :: NPTFR
      LOGICAL, INTENT(IN)    :: TRAC
      INTEGER, INTENT(INOUT) :: NUMLIQ(*)
      INTEGER, INTENT(INOUT) :: LIUBOR(NPTFR),LIVBOR(NPTFR)
      INTEGER, INTENT(INOUT) :: LIHBOR(NPTFR),LITBOR(*)
      DOUBLE PRECISION,  INTENT(INOUT) :: UBOR(*),VBOR(*)
      DOUBLE PRECISION,  INTENT(INOUT) :: HBOR(NPTFR),CHBORD(NPTFR)
      DOUBLE PRECISION,  INTENT(INOUT) :: TBOR(NPTFR),ATBOR(NPTFR)
      DOUBLE PRECISION,  INTENT(INOUT) :: BTBOR(NPTFR)
      TYPE(BIEF_MESH),   INTENT(INOUT) :: MESH
      CHARACTER(LEN=3),  INTENT(IN) :: CODE
      CHARACTER(LEN=8),  INTENT(IN) :: FFORMAT
      INTEGER, OPTIONAL, INTENT(INOUT) :: BOUNDARY_COLOUR(NPTFR)
      INTEGER, OPTIONAL, INTENT(IN)    :: NPTFR2
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NPTFR_BND, I, VAL, IERR, K
      INTEGER, ALLOCATABLE :: BOUNDARY_COLOUR2(:)
      CHARACTER(LEN=144) :: NAMEPAR
      INTEGER :: DIMUBOR, TYPE_BND_ELEM, NDP_BND, NELEBD
      INTEGER, ALLOCATABLE :: IKLE_BND(:)
!
      CHARACTER(LEN=11) :: EXTENS
      EXTERNAL EXTENS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE_BND_ELEM = MESH%TYPELMBND
      DIMUBOR = NPTFR
      IF(PRESENT(NPTFR2)) DIMUBOR = NPTFR2
      CALL GET_BND_NPOIN(FFORMAT,NGEO,TYPE_BND_ELEM,NPTFR_BND,IERR)
      CALL CHECK_CALL(IERR,'LECLIM:GET_BND_NPOIN')
      ! Test difference between nptfr and nptfr_bnd
      IF(NPTFR.NE.NPTFR_BND) THEN
        IF(LNG.EQ.1) WRITE(LU,23) NPTFR_BND,NPTFR
        IF(LNG.EQ.2) WRITE(LU,24) NPTFR_BND,NPTFR
23      FORMAT(1X,'LECLIM : ERREUR DANS LE FICHIER DES',
     &       /,1X,'         CONDITIONS AUX LIMITES, ',1I5,' LIGNES',
     &       /,1X,'         AU LIEU DE ',I5)
24      FORMAT(1X,'LECLIM: ERROR IN THE BOUNDARY CONDITIONS FILE,',
     &       /,9X,1I5,' LINES INSTEAD OF ',I5,' REQUESTED')
        CALL PLANTE(1)
        STOP
      ENDIF
      !
      CALL GET_BND_NELEM(FFORMAT,NGEO,TYPE_BND_ELEM,NELEBD,IERR)
      CALL CHECK_CALL(IERR,'LECLIM:GET_BND_NPOIN')

      CALL GET_NODES_PER_ELEMENT(TYPE_BND_ELEM,NDP_BND)
      ! Get nbor
      ! Only reading nbor from mesh in serial (because of renumbering)
      ! In parallel it will be read by read_partel_info
      IF(NCSIZE.LE.1) THEN
        CALL GET_BND_NUMBERING(FFORMAT,NGEO,TYPE_BND_ELEM,NPTFR_BND,
     &                    MESH%NBOR%I,IERR)
      ELSE
        NAMEPAR = CODE//'PAR'//EXTENS(NCSIZE-1,IPID)
        ALLOCATE(BOUNDARY_COLOUR2(NPTFR),STAT=IERR)
        CALL CHECK_ALLOCATE(IERR,'LECLIM:BOUNDARY_COLOUR2')
!
        CALL READ_PARTEL_INFO(NAMEPAR,NPTFR,NUMLIQ,BOUNDARY_COLOUR2,
     &                        MESH)
!
        IF(PRESENT(BOUNDARY_COLOUR)) THEN
          DO I=1,NPTFR
            BOUNDARY_COLOUR(I) = BOUNDARY_COLOUR2(I)
          ENDDO
        ENDIF
        DEALLOCATE(BOUNDARY_COLOUR2)
      ENDIF
      ! Get value of each boundary conditions
      CALL GET_BND_VALUE(FFORMAT,NGEO,TYPE_BND_ELEM,NELEBD,
     &                   LIHBOR,NPTFR_BND,MESH%NBOR%I,IERR)
      CALL CHECK_CALL(IERR,'LECLIM:GET_BND_VALUE')
      DO I=1,NPTFR_BND
        VAL = LIHBOR(I)
        LIHBOR(I) = VAL/1000
        LIUBOR(I) = (VAL - LIHBOR(I)*1000)/100
        LIVBOR(I) = (VAL - LIHBOR(I)*1000 - LIUBOR(I)*100)/10
        HBOR(I) = 0.D0
        UBOR(I) = 0.D0
        VBOR(I) = 0.D0
        CHBORD(I) = 0.D0
        IF(TRAC) THEN
          LITBOR(I) =  VAL - LIHBOR(I)*1000 - LIUBOR(I)*100
     &                     - LIVBOR(I)*10
          TBOR(I) = 0.D0
          ATBOR(I) = 0.D0
          BTBOR(I) = 0.D0
        ENDIF
        IF(NCSIZE.LE.1) THEN
          IF(PRESENT(BOUNDARY_COLOUR)) THEN
            BOUNDARY_COLOUR(I) = I
          ENDIF
        ENDIF
      ENDDO
!
      DO K=1,NPTFR
!
!       ADHERENCE FOR H CHANGED AT THE WALL
!
        IF(LIHBOR(K).EQ.KADH) THEN
          LIHBOR(K)=KLOG
          IF(LNG.EQ.1) WRITE(LU,51) K
          IF(LNG.EQ.2) WRITE(LU,52) K
51        FORMAT(1X,'LECLIM : ADHERENCE SUR LA HAUTEUR AU POINT ',1I6,
     &              '         CHANGEE EN CONDITION DE PAROI')
52        FORMAT(1X,'LECLIM : ADHERENCE SUR LA HAUTEUR AU POINT ',1I6,
     &              '         CHANGEE EN CONDITION DE PAROI')
        ENDIF
!
!       INCIDENT WAVE FOR H TREATED LIKE A FREE EXIT
!
        IF(LIHBOR(K).EQ.KINC) THEN
          LIHBOR(K)=KSORT
        ENDIF
!
!       CANCELS DIRICHLET VALUES WHEN THE POINT IS NOT DIRICHLET
!       FOR POINTS WITH ADHERENCE, NEEDS UBOR OR VBOR =0
!
        IF(LIUBOR(K).NE.KENT.AND.LIUBOR(K).NE.KENTU) UBOR(K)=0.D0
        IF(LIVBOR(K).NE.KENT.AND.LIVBOR(K).NE.KENTU) VBOR(K)=0.D0
!
!       BACKS UP UBOR AND VBOR ON THEIR SECOND DIMENSION
!
        UBOR(K+DIMUBOR) = UBOR(K)
        VBOR(K+DIMUBOR) = VBOR(K)
!
      ENDDO ! K
!
!-----------------------------------------------------------------------
!
      RETURN
      END
