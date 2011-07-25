!                    *****************
                     SUBROUTINE CLHUVT
!                    *****************
!
     &(NWEIRS,NPSING,NPSMAX,NUMDIG,ZDIG,X,Y,ZF,IOPTAN,UNORM,CHESTR,
     & NKFROT,KARMAN,T,NTRAC,H,UBOR,VBOR,TBOR,NBOR,
     & LIHBOR,LIUBOR,LIVBOR,LITBOR)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    DEFINES THE DEPTHS, VELOCITIES, ... TO BE IMPOSED
!+                AT THE NODES, FROM THE DEPTHS AND AVERAGE FLOWS
!+                ON THE SEGMENTS CONSTITUTING THE SINGULARITY.
!
!history  V. GUINOT (LHF)
!+        19/04/1996
!+
!+
!
!history  J.-M. HERVOUET (LNH)
!+        23/11/2005
!+        V5P6
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
!| CHESTR         |-->| FRICTION COEFFICIENT.
!| H              |-->| WATER DEPTH.
!| IOPTAN         |-->| OPTION FOR TANGENTIAL VELOCITIES.
!| KARMAN         |-->| VON KARMAN CONSTANT.
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| LIUBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON VELOCITY
!| LITBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON TRACERS
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NKFROT         |-->| FRICTION LAW, PER POINT
!| NPSING         |-->| NUMBER OF POINTS FOR EVERY SINGULARITY.
!| NPSMAX         |-->| MAXIMUM NUMBER OF POINTS FOR ONE SIDE OF A
!|                |   | SINGULARITY.
!| NTRAC          |-->| NUMBER OF TRACERS
!| NUMDIG         |-->| NUMDIG(K,I,NP) : BOUNDARY NUMBER OF POINT NP
!|                |   | OF SIDE K OF WEIR I.
!| NWEIRS         |-->| NUMBER OF SINGULARITIES
!| T              |-->| BLOCK OF TRACERS.
!| UBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY U
!| VBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON VELOCITY V
!| TBOR           |<--| PRESCRIBED BOUNDARY CONDITION ON TRACER
!| UNORM          |-->| NORM OF VELOCITY 
!| X              |-->| ABSCISSAE OF NODES
!| Y              |-->| ORDINATES OF NODES
!| ZDIG           |-->| ELEVATIONS OF POINTS OF WEIRS 
!| ZF             |-->| BOTTOM TOPOGRAPHY
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
      INTEGER, INTENT(IN)    :: NWEIRS,NPSMAX,IOPTAN
      INTEGER, INTENT(IN)    :: NPSING(NWEIRS),NUMDIG(2,NWEIRS,NPSMAX)
      INTEGER, INTENT(IN)             :: NBOR(*),NKFROT(*)
      INTEGER, INTENT(INOUT)          :: LIUBOR(*),LIHBOR(*),LIVBOR(*)
      INTEGER, INTENT(IN)             :: NTRAC
      DOUBLE PRECISION, INTENT(IN)    :: ZDIG(NWEIRS,NPSMAX)
      DOUBLE PRECISION, INTENT(IN)    :: X(*),Y(*),ZF(*),CHESTR(*),H(*)
      DOUBLE PRECISION, INTENT(IN)    :: UNORM(*)
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(*),VBOR(*)
      DOUBLE PRECISION, INTENT(IN)    :: KARMAN
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: TBOR,LITBOR
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: T
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,I1,I2,K,N,N0,N1,N2,ITRAC
!
      DOUBLE PRECISION DL,NX,NY,PENTE,CZ,HH,TX,TY,UTAN,XX
!
      INTRINSIC ABS,SQRT,SIGN
!
!-----------------------------------------------------------------------
!
!     LOOPS ON THE HYDRAULIC STRUCTURES
!
      DO 10 N=1,NWEIRS
!
!       LOOPS ON THE CRESTS OF THE HYDRAULIC STRUCTURES
!
        DO 20 K=1,2
!
!         LOOPS ON THE NODES OF EACH CREST
!
          DO 30 I=1,NPSING(N)
!
          I1=NUMDIG(K,N,I)
          N1=NBOR(I1)
!
          IF(I.EQ.1) THEN
            N0=N1
            N2=NBOR(NUMDIG(K,N,I+1))
            XX=0.D0
          ELSEIF(I.LT.NPSING(N)) THEN
            N0=NBOR(NUMDIG(K,N,I-1))
            N2=NBOR(NUMDIG(K,N,I+1))
            XX=1.D0
          ELSE
            N0=NBOR(NUMDIG(K,N,I-1))
            N2=N1
            XX=0.D0
          ENDIF
!
!         CALCULATES THE NORMAL VECTOR, OUTGOING CREST 1, ENTERING CREST 2
!
          TX=X(N2)-X(N0)
          TY=Y(N2)-Y(N0)
          DL=SQRT(TX*TX+TY*TY)
          TX=TX/DL
          TY=TY/DL
          NX=-TY
          NY=TX
!
!         CALCULATES THE TANGENTIAL VELOCITY
!
          IF (IOPTAN.EQ.0) THEN
!
             UTAN=0.D0
!
          ELSEIF(IOPTAN.EQ.1) THEN
!
!            ONE TAKES THE HEIGHT ON THE CREST (TO BE DISCUSSED)
!            HH = H (N1)
             HH = H(N1)+ZF(N1)-ZDIG(N,I)
!            LINE ADDED ON 23/11/2005 BY JMH (HH MAY BE NEGATIVE)
             HH=MAX(HH,0.D0)
             PENTE=(H(N0)-H(N2)+ZF(N0)-ZF(N2))/DL
!
             IF (NKFROT(N1).EQ.2) THEN
                UTAN = CHESTR(N1)*SQRT(HH*ABS(PENTE))*SIGN(1.D0,PENTE)
             ELSEIF (NKFROT(N1).EQ.3) THEN
                UTAN = CHESTR(N1)*HH**(2.D0/3.D0)*SQRT(ABS(PENTE))
     &                                           *SIGN(1.D0,PENTE)
             ELSEIF (NKFROT(N1).EQ.4) THEN
                UTAN = HH**(2.D0/3.D0)*SQRT(ABS(PENTE))
     &                                *SIGN(1.D0,PENTE)/CHESTR(N1)
             ELSEIF (NKFROT(N1).EQ.5) THEN
                HH   = MAX(HH,1.D-9)
              CZ = MAX(1.D-9,LOG(11.D0*HH/MAX(CHESTR(N1),1.D-9))/KARMAN)
                UTAN = CZ*SQRT(HH*ABS(PENTE))*SIGN(1.D0,PENTE)
             ELSE
                IF (LNG.EQ.1) THEN
                   WRITE(LU,*)'CLHUVT : OPTION INCONNUE :',NKFROT(N1)
                   WRITE(LU,*)'         POUR LA LOI DE FROTTEMENT'
                ELSEIF(LNG.EQ.2) THEN
                   WRITE(LU,*)'CLHUVT : UNKNOWN OPTION:',NKFROT(N1)
                   WRITE(LU,*)'         FOR THE FRICTION LAW'
                ENDIF
                CALL PLANTE(1)
                STOP
             ENDIF
!            TO GET ZERO TANGENTIAL VELOCITIES IN THE CORNERS
             UTAN = XX*UTAN
          ELSE
             IF (LNG.EQ.1) THEN
                WRITE(LU,*)'CLHUVT : OPTION INCONNUE :',IOPTAN
                WRITE(LU,*)'         POUR LES VITESSES TANGENTIELLES'
             ELSEIF(LNG.EQ.2) THEN
                WRITE(LU,*)'CLHUVT : UNKNOWN OPTION:',IOPTAN
                WRITE(LU,*)'         FOR THE TANGENTIAL VELOCITY'
             ENDIF
             CALL PLANTE(1)
             STOP
          ENDIF
!
!         ONE CALCULATES VELOCITY COMPONENTS U AND V
!         IN THE ORDINARY COORDINATE SYSTEM (X,Y).
!
          UBOR(I1)=UTAN*TX+UNORM(I1)*NX
          VBOR(I1)=UTAN*TY+UNORM(I1)*NY
!
30    CONTINUE
20    CONTINUE
!
!
!-----------------------------------------------------------------------
!
!  TYPES OF CONDITIONS FOR THE DEPTH AND THE VELOCITY:
!
      DO 40 I=1,NPSING(N)
!
        I1=NUMDIG(1,N,I)
        I2=NUMDIG(2,N,I)
        LIHBOR(I1)=4
        LIUBOR(I1)=6
        LIVBOR(I1)=6
        LIHBOR(I2)=4
        LIUBOR(I2)=6
        LIVBOR(I2)=6
!
!       CORRECTION: SOLID WALL TYPE IF NORMAL VELOCITY IS ZERO
!
        IF(ABS(UNORM(I1)).LT.1.D-10) THEN
          LIHBOR(I1)=2
          LIUBOR(I1)=2
          LIVBOR(I1)=2
          LIHBOR(I2)=2
          LIUBOR(I2)=2
          LIVBOR(I2)=2
          IF(NTRAC.GT.0) THEN
            DO ITRAC=1,NTRAC
              LITBOR%ADR(ITRAC)%P%I(I1)=2
              LITBOR%ADR(ITRAC)%P%I(I2)=2
            ENDDO
          ENDIF
        ENDIF
!
40    CONTINUE
!
!-----------------------------------------------------------------------
!
!  TYPES OF CONDITIONS FOR THE TRACER AND VALUES TO BE IMPOSED
!
      IF(NTRAC.GT.0) THEN
!
        DO ITRAC=1,NTRAC
        DO I=1,NPSING(N)
!
          I1=NUMDIG(1,N,I)
          I2=NUMDIG(2,N,I)
!
          IF(UNORM(I1).LT.-1.D-8) THEN
!           OUTGOING SPEED IN 1, ENTERING IN 2
            LITBOR%ADR(ITRAC)%P%I(I1)=4
            LITBOR%ADR(ITRAC)%P%I(I2)=5
            TBOR%ADR(ITRAC)%P%R(I2)=T%ADR(ITRAC)%P%R(NBOR(I1))
          ELSEIF(UNORM(I1).GT.1.D-8) THEN
!           OUTGOING SPEED IN 2, ENTERING IN 1
            LITBOR%ADR(ITRAC)%P%I(I1)=5
            TBOR%ADR(ITRAC)%P%R(I1)=T%ADR(ITRAC)%P%R(NBOR(I2))
            LITBOR%ADR(ITRAC)%P%I(I2)=4
          ELSE
!           ZERO VELOCITY
            LITBOR%ADR(ITRAC)%P%I(I1)=2
            LITBOR%ADR(ITRAC)%P%I(I2)=2
          ENDIF
!
        ENDDO
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
! END OF THE LOOP ON THE CREST
!
10    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
