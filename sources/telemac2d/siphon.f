!                    *****************
                     SUBROUTINE SIPHON
!                    *****************
!
     &(RELAXS,NSIPH,ENTSIP,SORSIP,GRAV,
     & H,ZF,DSIP,SECSIP,ALTSIP,CSSIP,CESIP,DELSIP,ANGSIP,LSIP,
     & NTRAC,T,TSIP,USIP,VSIP,U,V,ENTET)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   21/08/2010
!***********************************************************************
!
!brief    TREATS SIPHONS.
!
!history  V. GUINOT (LHF)
!+        19/04/1996
!+
!+
!
!history  E. DAVID & C. COULET (SOGREAH)
!+        **/03/2000
!+   Limitation of Q when depth less than 2 cm
!+
!
!history  J-M HERVOUET (LNHE)
!+        16/02/2009
!+        V5P9
!+   CORRECTED 03/2000 CORRECTION (IN PARALLEL MODE)
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
!history  J-M HERVOUET (LNHE) & P. LANG (INGEROP)
!+        14/10/2011
!+        V6P2
!+   Correction of DSCE in view of available water.
!
!history  C.COULET (ARTELIA)
!+        30/03/2012
!+        V6P2
!+   Modification for culvert management
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ALTSIP         |-->| ELEVATIONS OF PIPES
!| ANGSIP         |-->| ANGLE OF PIPES WITH AXIS OX.
!| CESIP          |-->| HEAD LOSS COEFFICIENT WHEN WORKING AS AN INFLOW
!| CSSIP          |-->| HEAD LOSS COEFFICIENT WHEN WORKING AS AN OUTFLOW
!| DELSIP         |-->| ANGLE OF PIPES WITH BOTTOM
!| DSIP           |-->| DISCHARGE OF CULVERT.
!| ENTET          |-->| IF YES, PRINTING INFORMATION ON LISTING
!| ENTSIP         |-->| INDICES OF ENTRY OF PIPE IN GLOBAL NUMBERING
!| GRAV           |-->| GRAVITY
!| H              |-->| DEPTH
!| LSIP           |-->| LINEAR HEAD LOSS OF PIPE
!| NSIPH          |-->| NUMBER OF CULVERTS
!| NTRAC          |-->| NUMBER OF TRACERS
!| RELAXS         |-->| RELAXATION COEFFICIENT
!| SECSIP         |-->| CROSS SECTION OF CULVERTS (NUMBERED AS SOURCES)
!| SORSIP         |-->| INDICES OF PIPES EXITS IN GLOBAL NUMBERING
!| T              |-->| BLOCK OF TRACERS
!| TSIP           |<->| VALUES OF TRACERS AT CULVERT EXTREMITY
!| U              |-->| X-COMPONENT OF VELOCITY
!| USIP           |<->| VELOCITY U AT CULVERT EXTREMITY
!| V              |-->| Y-COMPONENT OF VELOCITY
!| VSIP           |<->| VELOCITY V AT CULVERT EXTREMITY
!| ZF             |-->| ELEVATION OF BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY : V2DPAR,DT
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NSIPH,NTRAC
      INTEGER, INTENT(IN)             :: ENTSIP(NSIPH),SORSIP(NSIPH)
      LOGICAL, INTENT(IN)             :: ENTET
      DOUBLE PRECISION, INTENT(IN)    :: RELAXS,GRAV
      DOUBLE PRECISION, INTENT(INOUT) :: USIP(NSIPH,2),VSIP(NSIPH,2)
      DOUBLE PRECISION, INTENT(INOUT) :: DSIP(NSIPH)
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: TSIP
      DOUBLE PRECISION, INTENT(IN)    :: ANGSIP(NSIPH,2),LSIP(NSIPH)
      DOUBLE PRECISION, INTENT(IN)    :: CESIP(NSIPH,2),CSSIP(NSIPH,2)
      DOUBLE PRECISION, INTENT(IN)    :: DELSIP(NSIPH,2)
      DOUBLE PRECISION, INTENT(IN)    :: SECSIP(NSIPH),ALTSIP(NSIPH,2)
      DOUBLE PRECISION, INTENT(IN)    :: H(*),ZF(*),U(*),V(*)
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: T
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N,I1,I2,ITRAC
!
      DOUBLE PRECISION SEC,L
      DOUBLE PRECISION D1,D2,S1,S2,CE1,CE2,CS1,CS2,Q,QMAX1,QMAX2
!
      INTRINSIC SQRT,COS,SIN
!
      DOUBLE PRECISION P_DMAX
      EXTERNAL         P_DMAX
!
!-----------------------------------------------------------------------
!
! LOOP OVER THE SIPHONS
!
      DO N=1,NSIPH
!
!     IDENTIFIES ENTRY / EXIT NODES
!
!     NUMBER OF THE POINTS
      I1=ENTSIP(N)
      I2=SORSIP(N)
!
!     LOADS, TAKEN AS FREE SURFACE ELEVATION
!
      IF(I1.GT.0) THEN
        S1=H(I1)+ZF(I1)
        QMAX1=0.9D0*MAX(H(I1),0.D0)*V2DPAR%R(I1)/DT
      ELSE
        S1=-1.D10
        QMAX1=-1.D10
      ENDIF
      IF(I2.GT.0) THEN
        S2=H(I2)+ZF(I2)
        QMAX2=0.9D0*MAX(H(I2),0.D0)*V2DPAR%R(I2)/DT
      ELSE
        S2=-1.D10
        QMAX2=-1.D10
      ENDIF
!     CASE WHERE ONE OF THE ENDS IS NOT IN THE SUB-DOMAIN
      IF(NCSIZE.GT.1) THEN
        S1=P_DMAX(S1)
        S2=P_DMAX(S2)
        QMAX1=P_DMAX(QMAX1)
        QMAX2=P_DMAX(QMAX2)
      ENDIF
!
!     COEFFICIENTS FOR COMPUTATION OF PRESSURE LOSS
!
      D1=DELSIP(N,1)
      D2=DELSIP(N,2)
      CE1=CESIP(N,1)
      CE2=CESIP(N,2)
      CS1=CSSIP(N,1)
      CS2=CSSIP(N,2)
      SEC=SECSIP(N)
      L  =LSIP(N)
!
!     COMPUTES THE FLOW ACCORDING TO DELTAH
!     IF THE LINEAR PRESSURE LOSS IS NEGLIGIBLE, COULD HAVE DIFFERENT
!     ENTRY / EXIT SECTIONS
!
      IF(S1.GE.S2) THEN
        IF(S1.GT.ALTSIP(N,1).AND.S1.GT.ALTSIP(N,2)) THEN
          Q = SEC * SQRT( 2.D0*GRAV*(S1-S2)/(CE1+L+CS2) )
        ELSE
          Q=0.D0
        ENDIF
      ELSE
        IF(S2.GT.ALTSIP(N,1).AND.S2.GT.ALTSIP(N,2)) THEN
          Q = - SEC * SQRT( 2.D0*GRAV*(S2-S1)/(CS1+L+CE2) )
        ELSE
          Q=0.D0
        ENDIF
      ENDIF
!
!     NOTHING HAPPENS IF THE LOADS AT THE 2 ENDS ARE LOWER THAN
!     THE ELEVATION OF THE NOZZLES
!
      IF(S1.LT.ALTSIP(N,1).AND.S2.LT.ALTSIP(N,2)) Q=0.D0
!
!     FILLS OUT DSIP(N) USING RELAXATION
!
      DSIP(N)= RELAXS * Q + (1.D0-RELAXS) * DSIP(N)
!
!     LIMITATION WITH AVAILABLE WATER
!
      IF(DSIP(N).GT.0.D0) THEN
        DSIP(N)=MIN(QMAX1,DSIP(N))
      ELSE
        DSIP(N)=MAX(-QMAX2,DSIP(N))
      ENDIF
!
      IF(ENTET) THEN
        WRITE(LU,*) ' '
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'SIPHON ',N,' DEBIT DE ',DSIP(N),' M3/S'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'CULVERT ',N,' DISCHARGE OF ',DSIP(N),' M3/S'
        ENDIF
        WRITE(LU,*) ' '
      ENDIF
!
!  TREATS THE VELOCITIES AT THE SOURCES
!  SAME APPROACH FOR VELOCITY AND TRACER
!
      IF(DSIP(N).GT.0.D0) THEN
        USIP(N,2) = ( COS(D2)*DSIP(N)/SECSIP(N) ) * COS(ANGSIP(N,2))
        VSIP(N,2) = ( COS(D2)*DSIP(N)/SECSIP(N) ) * SIN(ANGSIP(N,2))
        IF(I1.GT.0) THEN
          USIP(N,1) = U(I1)
          VSIP(N,1) = V(I1)
        ELSE
          USIP(N,1) = 0.D0
          VSIP(N,1) = 0.D0
        ENDIF
      ELSE
        USIP(N,1) = ( COS(D1)*DSIP(N)/SECSIP(N) ) * COS(ANGSIP(N,1))
        VSIP(N,1) = ( COS(D1)*DSIP(N)/SECSIP(N) ) * SIN(ANGSIP(N,1))
        IF(I2.GT.0) THEN
          USIP(N,2) = U(I2)
          VSIP(N,2) = V(I2)
        ELSE
          USIP(N,2) = 0.D0
          VSIP(N,2) = 0.D0
        ENDIF
      ENDIF
      IF(NCSIZE.GT.1) THEN
        USIP(N,1)=P_DMAX(USIP(N,1))
        VSIP(N,1)=P_DMAX(VSIP(N,1))
        USIP(N,2)=P_DMAX(USIP(N,2))
        VSIP(N,2)=P_DMAX(VSIP(N,2))
      ENDIF
!
!  TREATS THE TRACER :
!  NOTA : NSIPH + N <==> N,2
!                 N <==> N,1
!
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          IF(DSIP(N).GT.0.D0) THEN ! I1 --> I2
            IF(I1.GT.0) THEN
              TSIP%ADR(ITRAC)%P%R(NSIPH+N)=T%ADR(ITRAC)%P%R(I1)
              TSIP%ADR(ITRAC)%P%R(N)      =T%ADR(ITRAC)%P%R(I1)
            ELSE
              TSIP%ADR(ITRAC)%P%R(NSIPH+N)=-1.D10
              TSIP%ADR(ITRAC)%P%R(N)      =-1.D10
            ENDIF
          ELSEIF(DSIP(N).LT.0.D0) THEN ! I2 --> I1
            IF(I2.GT.0) THEN
              TSIP%ADR(ITRAC)%P%R(N)      =T%ADR(ITRAC)%P%R(I2)
              TSIP%ADR(ITRAC)%P%R(NSIPH+N)=T%ADR(ITRAC)%P%R(I2)
            ELSE
              TSIP%ADR(ITRAC)%P%R(N)      =-1.D10
              TSIP%ADR(ITRAC)%P%R(NSIPH+N)=-1.D10
            ENDIF
!         THE CASE DSIP=0 IS NOT TREATED - LET THE TRACER FREE?
          ENDIF
          IF(NCSIZE.GT.1) THEN
            TSIP%ADR(ITRAC)%P%R(NSIPH+N)=
     &        P_DMAX(TSIP%ADR(ITRAC)%P%R(NSIPH+N))
            TSIP%ADR(ITRAC)%P%R(N)      =
     &        P_DMAX(TSIP%ADR(ITRAC)%P%R(N))
          ENDIF
        ENDDO
      ENDIF
!
!  END OF THE LOOP OVER THE SIPHONS
!
      ENDDO ! N
!
!-----------------------------------------------------------------------
!
      RETURN
      END
