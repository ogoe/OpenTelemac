!                    *****************
                     SUBROUTINE SIPHON
!                    *****************
!
     &(RELAXS,NSIPH,ENTSIP,SORSIP,GRAV,
     & H,ZF,ISCE,DSCE,SECSCE,ALTSCE,CSSCE,CESCE,DELSCE,ANGSCE,LSCE,
     & NTRAC,T,TSCE,USCE,VSCE,U,V,ENTET,MAXSCE)
!
!***********************************************************************
! TELEMAC2D   V6P2                                   21/08/2010
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
!+   Correction of DSCE if requested water is not available.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| ALTSCE         |-->| ELEVATIONS OF PIPES
!| ANGSCE         |-->| ANGLE OF PIPES WITH AXIS OX.
!| CESCE          |<--| HEAD LOSS COEFFICIENT WHEN WORKING AS AN INFLOW
!| CSSCE          |<--| HEAD LOSS COEFFICIENT WHEN WORKING AS AN OUTFLOW
!| DELSCE         |<--| ANGLE OF PIPES WITH VERTICAL
!| DSCE           |<--| DISCHARGE OF SOURCES.
!| ENTET          |-->| IF YES, PRINTING INFORMATION ON LISTING
!| ENTSIP         |<--| INDICES OF ENTRY OF PIPE IN POINT SOURCES NUMBERING
!| GRAV           |-->| GRAVITY
!| H              |-->| DEPTH
!| ISCE           |-->| GLOBAL NUMBER OF POINT SOURCES.
!| LSCE           |<--| LINEAR HEAD LOSS OF PIPE
!| MAXSCE         |-->| MAXIMUM NUMBER OF SOURCES
!| NSIPH          |-->| NUMBER OF CULVERTS
!| NTRAC          |-->| NUMBER OF TRACERS
!| RELAXS         |-->| RELAXATION COEFFICIENT
!| SECSCE         |-->| CROSS SECTION OF CULVERTS (NUMBERED AS SOURCES)
!| SORSIP         |-->| INDICES OF PIPES EXITS IN SOURCES NUMBERING
!| T              |-->| BLOCK OF TRACERS
!| TSCE           |-->| VALUES OF TRACERS AT SOURCES
!| U              |<->| X-COMPONENT OF VELOCITY
!| USCE           |-->| VELOCITY U OF THE SOURCES
!| V              |<->| Y-COMPONENT OF VELOCITY
!| VSCE           |-->| VELOCITY V OF THE SOURCES
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
      INTEGER, INTENT(IN)             :: NSIPH,NTRAC,MAXSCE
      INTEGER, INTENT(IN)             :: ENTSIP(*),SORSIP(*),ISCE(*)
      LOGICAL, INTENT(IN)             :: ENTET
      DOUBLE PRECISION, INTENT(IN)    :: RELAXS,GRAV
      DOUBLE PRECISION, INTENT(INOUT) :: USCE(*),VSCE(*),DSCE(*)
      DOUBLE PRECISION, INTENT(INOUT) :: TSCE(MAXSCE,NTRAC)
      DOUBLE PRECISION, INTENT(IN)    :: ANGSCE(*),LSCE(*),CESCE(*)
      DOUBLE PRECISION, INTENT(IN)    :: CSSCE(*),DELSCE(*)
      DOUBLE PRECISION, INTENT(IN)    :: SECSCE(*),ALTSCE(*)
      DOUBLE PRECISION, INTENT(IN)    :: H(*),ZF(*),U(*),V(*)
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: T
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER N,I1,I2,IR1,IR2,ITRAC
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
      DO 10 N=1,NSIPH
!
!     IDENTIFIES ENTRY / EXIT NODES
!
!     NUMBER OF THE CORRESPONDING SOURCES
      I1=ENTSIP(N)
      I2=SORSIP(N)
!     NUMBER OF THE POINTS FOR THESE SOURCES
      IR1=ISCE(I1)
      IR2=ISCE(I2)
!
!     LOADS, TAKEN AS FREE SURFACE ELEVATION
!
      IF(IR1.GT.0) THEN
        S1=H(IR1)+ZF(IR1)
        QMAX1=0.9D0*H(IR1)*V2DPAR%R(IR1)/DT
      ELSE
        S1=-1.D10
        QMAX1=-1.D10
      ENDIF
      IF(IR2.GT.0) THEN
        S2=H(IR2)+ZF(IR2)
        QMAX2=0.9D0*H(IR2)*V2DPAR%R(IR2)/DT
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
      D1=DELSCE(I1)
      D2=DELSCE(I2)
      CE1=CESCE(I1)
      CE2=CESCE(I2)
      CS1=CSSCE(I1)
      CS2=CSSCE(I2)
      SEC=SECSCE(I1)
      L  =LSCE(I1)
!
!     COMPUTES THE FLOW ACCORDING TO DELTAH
!     IF THE LINEAR PRESSURE LOSS IS NEGLIGIBLE, COULD HAVE DIFFERENT
!     ENTRY / EXIT SECTIONS
!
      IF(S1.GE.S2) THEN
        IF(S1.GT.ALTSCE(I1).AND.S1.GT.ALTSCE(I2)) THEN
          Q = SEC * SQRT( 2.D0*GRAV*(S1-S2)/(CE1+L+CS2) )
        ELSE
          Q=0.D0
        ENDIF
      ELSE
        IF(S2.GT.ALTSCE(I1).AND.S2.GT.ALTSCE(I2)) THEN
          Q = - SEC * SQRT( 2.D0*GRAV*(S2-S1)/(CS1+L+CE2) )
        ELSE
          Q=0.D0
        ENDIF
      ENDIF
!
!     NOTHING HAPPENS IF THE LOADS AT THE 2 ENDS ARE LOWER THAN
!     THE ELEVATION OF THE NOZZLES
!
      IF(S1.LT.ALTSCE(I1).AND.S2.LT.ALTSCE(I2)) Q=0.D0
!
!     FILLS OUT DSCE(I2) USING RELAXATION
!
      DSCE(I2)= RELAXS * Q + (1.D0-RELAXS) * DSCE(I2)
!
!     LIMITATION WITH AVAILABLE WATER
!
      IF(DSCE(I2).GT.0.D0) THEN
        DSCE(I2)=MIN(QMAX1,DSCE(I2))
      ELSE
        DSCE(I2)=MAX(-QMAX2,DSCE(I2))
      ENDIF
!
!     NOW POINT I1
!
      DSCE(I1)=-DSCE(I2)
!
      IF(ENTET) THEN
        WRITE(LU,*) ' '
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'SIPHON ',N,' DEBIT DE ',DSCE(I2),' M3/S'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'CULVERT ',N,' DISCHARGE OF ',DSCE(I2),' M3/S'
        ENDIF
        WRITE(LU,*) ' '
      ENDIF
!
!  TREATS THE VELOCITIES AT THE SOURCES
!  SAME APPROACH FOR VELOCITY AND TRACER
!
      IF(DSCE(I1).GT.0.D0) THEN
        USCE(I1) = ( COS(D1)*DSCE(I1)/SECSCE(I1) ) * COS(ANGSCE(I1))
        VSCE(I1) = ( COS(D1)*DSCE(I1)/SECSCE(I1) ) * SIN(ANGSCE(I1))
      ELSE
        IF(IR1.GT.0) THEN
          USCE(I1) = U(IR1)
          VSCE(I1) = V(IR1)
        ENDIF
      ENDIF
      IF(DSCE(I2).GT.0.D0) THEN
        USCE(I2) = ( COS(D2)*DSCE(I2)/SECSCE(I2) ) * COS(ANGSCE(I2))
        VSCE(I2) = ( COS(D2)*DSCE(I2)/SECSCE(I2) ) * SIN(ANGSCE(I2))
      ELSE
        IF(IR2.GT.0) THEN
          USCE(I2) = U(IR2)
          VSCE(I2) = V(IR2)
        ENDIF
      ENDIF
!
!  TREATS THE TRACER :
!
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          IF(DSCE(I1).GT.0.D0) THEN
            IF(IR2.GT.0) THEN
              TSCE(I1,ITRAC)=T%ADR(ITRAC)%P%R(IR2)
            ELSE
              TSCE(I1,ITRAC)=-1.D10
            ENDIF
            IF(NCSIZE.GT.1) TSCE(I1,ITRAC)=P_DMAX(TSCE(I1,ITRAC))
          ENDIF
          IF(DSCE(I2).GT.0.D0) THEN
            IF(IR1.GT.0) THEN
              TSCE(I2,ITRAC)=T%ADR(ITRAC)%P%R(IR1)
            ELSE
              TSCE(I2,ITRAC)=-1.D10
            ENDIF
            IF(NCSIZE.GT.1) TSCE(I2,ITRAC)=P_DMAX(TSCE(I2,ITRAC))
          ENDIF
        ENDDO
      ENDIF
!
!  END OF THE LOOP OVER THE SIPHONS
!
10    CONTINUE
!
!-----------------------------------------------------------------------
!
      RETURN
      END
