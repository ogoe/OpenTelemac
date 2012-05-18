!                    *****************
                     SUBROUTINE DIFSOU
!                    *****************
!
     &(TEXP,TIMP,YASMI,TSCEXP,HPROP,TN,TETAT,NREJTR,ISCE,DSCE,TSCE,
     & MAXSCE,MAXTRA,AT,DT,MASSOU,NTRAC,FAC,NSIPH,ENTSIP,SORSIP,
     & DSIP,TSIP)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   21/08/2010
!***********************************************************************
!
!brief    PREPARES THE SOURCES TERMS IN THE DIFFUSION EQUATION
!+                FOR THE TRACER.
!
!warning  BEWARE OF NECESSARY COMPATIBILITIES FOR HPROP, WHICH
!+            SHOULD REMAIN UNCHANGED UNTIL THE COMPUTATION OF THE
!+            TRACER MASS IN CVDFTR
!
!history  J-M HERVOUET (LNHE); C MOULIN (LNH)
!+        23/02/2009
!+        V6P0
!+
!
!history  J-M HERVOUET (LNHE)
!+        01/10/2009
!+
!+   MODIFIED TEST ON ICONVF(3)
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
!history  C.COULET (ARTELIA)
!+        11/05/2012
!+        V6P2
!+   Modification for culvert management
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| AT             |-->| TIME IN SECONDS
!| DSCE           |-->| DISCHARGE OF POINT SOURCES
!| DSIP           |<--| DISCHARGE OF CULVERT.
!| DT             |-->| TIME STEP
!| ENTSIP         |-->| INDICES OF ENTRY OF PIPE IN GLOBAL NUMBERING
!| FAC            |-->| IN PARALLEL :
!|                |   | 1/(NUMBER OF SUB-DOMAINS OF THE POINT)
!| HPROP          |-->| PROPAGATION DEPTH
!| ISCE           |-->| NEAREST POINTS OF DISCHARGES
!| MASSOU         |<--| MASS OF TRACER ADDED BY SOURCE TERM
!| MAXSCE         |-->| MAXIMUM NUMBER OF SOURCES
!| MAXTRA         |-->| MAXIMUM NUMBER OF TRACERS
!| NREJTR         |-->| NUMBER OF POINT SOURCES AS GIVEN BY TRACERS KEYWORDS
!| NSIPH          |-->| NUMBER OF CULVERTS
!| NTRAC          |-->| NUMBER OF TRACERS
!| SORSIP         |-->| INDICES OF PIPES EXITS IN GLOBAL NUMBERING
!| TETAT          |-->| COEFFICIENT OF IMPLICITATION FOR TRACERS.
!| TEXP           |-->| EXPLICIT SOURCE TERM.
!| TIMP           |-->| IMPLICIT SOURCE TERM.
!| TN             |-->| TRACERS AT TIME N
!| TSCE           |-->| PRESCRIBED VALUES OF TRACERS AT POINT SOURCES
!| TSCEXP         |<--| EXPLICIT SOURCE TERM OF POINT SOURCES
!|                |   | IN TRACER EQUATION, EQUAL TO:
!|                |   | TSCE - ( 1 - TETAT ) TN
!| TSIP           |-->| VALUES OF TRACERS AT CULVERT EXTREMITY
!| YASMI          |<--| IF YES, THERE ARE IMPLICIT SOURCE TERMS
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
      INTEGER          , INTENT(IN)    :: ISCE(*),NREJTR,NTRAC,NSIPH
      INTEGER          , INTENT(IN)    :: ENTSIP(NSIPH),SORSIP(NSIPH)
      INTEGER          , INTENT(IN)    :: MAXSCE,MAXTRA
      LOGICAL          , INTENT(INOUT) :: YASMI(*)
      DOUBLE PRECISION , INTENT(IN)    :: AT,DT,TETAT,DSCE(*)
      DOUBLE PRECISION , INTENT(IN)    :: DSIP(NSIPH)
      DOUBLE PRECISION , INTENT(IN)    :: TSCE(MAXSCE,MAXTRA),FAC(*)
      DOUBLE PRECISION , INTENT(INOUT) :: MASSOU(*)
      TYPE(BIEF_OBJ)   , INTENT(IN)    :: TN,HPROP,TSIP
      TYPE(BIEF_OBJ)   , INTENT(INOUT) :: TSCEXP,TEXP,TIMP
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,IR,ITRAC
!
      DOUBLE PRECISION DEBIT,TRASCE,RAIN_MPS
!
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
!
!-----------------------------------------------------------------------
!
!     EXPLICIT SOURCE TERMS
!
      DO ITRAC=1,NTRAC
        CALL OS('X=0     ',X=TSCEXP%ADR(ITRAC)%P)
        CALL OS('X=0     ',X=TEXP%ADR(ITRAC)%P)
        MASSOU(ITRAC) = 0.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
!     IMPLICIT SOURCE TERMS (HERE SET TO ZERO)
!
      DO ITRAC=1,NTRAC
!       CALL OS('X=0     ',X=TIMP%ADR(ITRAC)%P)
!       EQUIVALENT A
        YASMI(ITRAC)=.FALSE.
      ENDDO
!
!                                   N+1
!     EXAMPLE WHERE WE ADD -0.0001 T      IN THE RIGHT HAND-SIDE
!     OF THE TRACER EQUATION THAT BEGINS WITH DT/DT=...
!     (T12=SMI WILL BE DIVIDED BY HPROP IN CVDFTR, THE EQUATION IS:
!     DT/DT=...+SMI*T(N+1)/H
!
!     HERE THIS IS DONE FOR TRACER 3 ONLY IN A RECTANGULAR ZONE
!
!     CALL OS('X=0     ',X=TIMP%ADR(3)%P)
!     DO I=1,HPROP%DIM1
!       IF(X(I).GE.263277.D0.AND.X(I).LE.265037.D0) THEN
!       IF(Y(I).GE.379007.D0.AND.Y(I).LE.380326.D0) THEN
!         TIMP%ADR(3)%P%R(I)=-0.00001D0*HPROP%R(I)
!       ENDIF
!       ENDIF
!     ENDDO
!     YASMI(3)=.TRUE.
!
!-----------------------------------------------------------------------
!
!  TAKES THE SOURCES OF TRACER INTO ACCOUNT
!
!-----------------------------------------------------------------------
!
      DO ITRAC=1,NTRAC
!
        IF(NREJTR.GT.0) THEN
!       
          DO I = 1 , NREJTR
!         
            IR = ISCE(I)
!           TEST IR.GT.0 FOR THE PARALLELISM
            IF(IR.GT.0) THEN
              DEBIT=DSCE(I)
              IF(DEBIT.GT.0.D0) THEN
                TRASCE = TSCE(I,ITRAC)
              ELSE
!               THE VALUE AT THE SOURCE IS TN IF THE FLOW IS OUTGOING
                TRASCE = TN%ADR(ITRAC)%P%R(IR)
              ENDIF
!             SOURCE TERM ADDED TO THE MASS OF TRACER
              IF(NCSIZE.GT.1) THEN
!               FAC TO AVOID COUNTING THE POINT SEVERAL TIMES
!               (SEE CALL TO P_DSUM BELOW)
                MASSOU(ITRAC)=MASSOU(ITRAC)+DT*DEBIT*TRASCE*FAC(IR)
              ELSE
                MASSOU(ITRAC)=MASSOU(ITRAC)+DT*DEBIT*TRASCE
              ENDIF
              TRASCE = TRASCE - (1.D0 - TETAT) * TN%ADR(ITRAC)%P%R(IR)
              TSCEXP%ADR(ITRAC)%P%R(IR)=TSCEXP%ADR(ITRAC)%P%R(IR)+TRASCE
!         
!             THE IMPLICIT PART OF THE TERM - T * SCE
!             IS DEALT WITH IN CVDFTR.
!         
            ENDIF
!         
          ENDDO
!       
          IF(NCSIZE.GT.1) MASSOU(ITRAC)=P_DSUM(MASSOU(ITRAC))
!       
        ENDIF
!
        IF(NSIPH.GT.0) THEN
          DO I = 1 , NSIPH
!
            IR = ENTSIP(I)
            IF(IR.GT.0) THEN
              IF(NCSIZE.GT.1) THEN
!               FAC TO AVOID COUNTING THE POINT SEVERAL TIMES
!               (SEE CALL TO P_DSUM BELOW)
                MASSOU(ITRAC)=MASSOU(ITRAC)-DT*DSIP(I)*
     &                        TSIP%ADR(ITRAC)%P%R(I)*FAC(IR)
              ELSE
                MASSOU(ITRAC)=MASSOU(ITRAC)-DT*DSIP(I)*
     &                        TSIP%ADR(ITRAC)%P%R(I)
              ENDIF
              TSCEXP%ADR(ITRAC)%P%R(IR)=TSCEXP%ADR(ITRAC)%P%R(IR) +
     &           TSIP%ADR(ITRAC)%P%R(I) -
     &           (1.D0 - TETAT) * TN%ADR(ITRAC)%P%R(IR)
            ENDIF
!
            IR = SORSIP(I)
            IF(IR.GT.0) THEN
              IF(NCSIZE.GT.1) THEN
!               FAC TO AVOID COUNTING THE POINT SEVERAL TIMES
!               (SEE CALL TO P_DSUM BELOW)
                MASSOU(ITRAC)=MASSOU(ITRAC)+DT*DSIP(I)*
     &                        TSIP%ADR(ITRAC)%P%R(NSIPH+I)*FAC(IR)
              ELSE
                MASSOU(ITRAC)=MASSOU(ITRAC)+DT*DSIP(I)*
     &                        TSIP%ADR(ITRAC)%P%R(NSIPH+I)
              ENDIF
              TSCEXP%ADR(ITRAC)%P%R(IR)=TSCEXP%ADR(ITRAC)%P%R(IR) +
     &           TSIP%ADR(ITRAC)%P%R(NSIPH+I) -
     &           (1.D0 - TETAT) * TN%ADR(ITRAC)%P%R(IR)
            ENDIF
          ENDDO
          IF(NCSIZE.GT.1) MASSOU(ITRAC)=P_DSUM(MASSOU(ITRAC))
        ENDIF
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
