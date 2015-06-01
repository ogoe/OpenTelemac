!                    ****************
                     SUBROUTINE PROPA
!                    ****************
!
     &(F,B,SHP,SHZ,SHF,ELT,ETA,FRE,IKLE_EXT,NPOIN3,NPOIN2,
     & NPLAN,NF,COURAN,TRA01,TRA02,ITR01,T3_01,T3_02,ISUB,MESH3D)
!
!***********************************************************************
! TOMAWAC   V7P0
!***********************************************************************
!
!brief    ADVECTION STEP.
!+                INTERPOLATES AT THE FOOT OF THE CHARACTERISTICS.
!
!history  F. MARCOS (LNH)
!+        05/12/95
!+        V1P0
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
!history  G.MATTAROLO (EDF - LNHE)
!+        23/06/2011
!+        V6P1
!+   Translation of French names of the variables in argument
!
!history  J-M HERVOUET (EDF LAB, LNHE)
!+        25/11/2014
!+        V7P0
!+   Bug corrected: size of array WSHZ for POST_INTERP was not correct
!+   in the call, as TRA01(1,2), TRA01(1,4) was needed.
!+   Moreover the real size of TRA01 is (NPOIN3,6), not (NPOIN3,8), see
!+   point_tomawac.f. Intent completed.
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| B              |-->| JACOBIAN TO TRANSFORM N(KX,KY) INTO F(FR,TETA)
!| COURAN         |-->| LOGICAL INDICATING IF THERE IS A CURRENT
!| ELT            |-->| NUMBERS OF THE ELEMENTS 2D OF THE
!|                |   | POINTS TO BE ADVECTED
!| ETA            |-->| NUMBERS OF THE LAYERS OF THE
!|                |   | POINTS TO BE ADVECTED
!| F              |<->| WAVE ACTION DENSITY OR VARIANCE DENSITY
!|                |   | DIRECTIONAL SPECTRUM
!| FRE            |-->| NUMBER OF THE FREQUENCIES OF THE
!|                |   | POINTS TO BE ADVECTED
!| IKLE_EXT       |-->| TRANSITION BETWEEN LOCAL AND GLOBAL NUMBERING
!|                |   | OF THE 2D MESH (IN AN EXTENDED FORM)
!| NF             |-->| NUMBER OF FREQUENCIES
!| NPLAN          |-->| NUMBER OF DIRECTIONS
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D MESH
!| NPOIN3         |-->| NPOIN2*NPLAN
!| SHF            |-->| BARYCENTRIC COORDINATES ALONG F OF THE
!|                |   | NODES IN THEIR ASSOCIATED FREQUENCIES "FRE"
!| SHP            |-->| BARYCENTRIC COORDINATES OF THE NODES IN
!|                |   | THEIR ASSOCIATED 2D ELEMENT "ELT"
!| SHZ            |-->| BARYCENTRIC COORDINATES ALONG TETA OF THE
!|                |   | NODES IN THEIR ASSOCIATED LAYER "ETA"
!| TRA01          |<->| WORK TABLE
!| TRA02          |<->| WORK TABLE
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TOMAWAC, EX_PROPA => PROPA
      USE STREAMLINE, ONLY : POST_INTERP
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPOIN3,NPOIN2,NPLAN,NF
!
      DOUBLE PRECISION, INTENT(INOUT) :: F(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION, INTENT(IN)    :: B(NPOIN2,NF)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPOIN3,6)
      INTEGER, INTENT(INOUT) :: ELT(NPOIN3,NF),ETA(NPOIN3,NF)
      INTEGER, INTENT(INOUT) :: FRE(NPOIN3,NF)
      INTEGER, INTENT(IN)    :: ISUB(NPOIN3,NF)
      INTEGER, INTENT(INOUT) :: ITR01(NPOIN3*3)
      LOGICAL, INTENT(IN)            :: COURAN
!                                       TRA02(NPOIN2,NPLAN,NF)
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: TRA02
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: SHP,SHZ,SHF,T3_01,T3_02
      TYPE(BIEF_OBJ), INTENT(IN)     :: IKLE_EXT
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH3D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IFF,I,I3,IPLAN
!
      INTRINSIC MAX
!
!----------------------------------------------------------------------
!
!     WITH CURRENT ALL FUNCTION MUST BE BUILT BEFORE INTERPOLATION
!     AS ALL FREQUENCIES WILL BE USED TOGETHER
!
      IF(COURAN) THEN
        DO IFF=1,NF
          DO IPLAN=1,NPLAN
            DO I=1,NPOIN2
              I3=I+(IPLAN-1)*NPOIN2+(IFF-1)*NPOIN3
              TRA02%R(I3)=F(I,IPLAN,IFF)*B(I,IFF)
            ENDDO
          ENDDO
        ENDDO
      ENDIF
!
!     NOW FREQUENCY PER FREQUENCY
!
      DO IFF=1,NF
!
!       COPY OF F*B INTO T3_01
!
        IF(.NOT.COURAN) THEN
          DO IPLAN=1,NPLAN
            DO I=1,NPOIN2
              I3=I+(IPLAN-1)*NPOIN2
              TRA02%R(I3)=F(I,IPLAN,IFF)*B(I,IFF)
            ENDDO
          ENDDO
        ENDIF
!
        CALL POST_INTERP(TRA02,T3_02,SHP%ADR(IFF)%P%R,
     &                   SHZ%ADR(IFF)%P%R,SHF%ADR(IFF)%P%R,
     &                   IKLE_EXT%I,IKLE_EXT%DIM1,1,
     &                   NPOIN2,ELT(1,IFF),ETA(1,IFF),FRE(1,IFF),
     &                   ISUB(1,IFF),3,NPLAN,41,41,NPOIN3,
     &                   NPOIN2,TRA01,TRA01(1,4),
     &                   T3_01%R,ITR01(1:NPOIN3),
     &                   ITR01(NPOIN3+1:2*NPOIN3),
     &                   ITR01(2*NPOIN3+1:3*NPOIN3),
     &                   NPOIN3,
     &                   .TRUE.,
!                      PERIODICITY
     &                   COURAN)
!                          4D
!
        IF(NCSIZE.GT.1) CALL PARCOM(T3_02,1,MESH3D)
!
!       FINAL COMPUTATION OF F
!
        DO IPLAN=1,NPLAN
          DO I=1,NPOIN2
            I3=I+(IPLAN-1)*NPOIN2
!           MAX(..,0.D0) SEEMS NECESSARY, WHERE F BECOMES < 0 ??
            F(I,IPLAN,IFF)=MAX(T3_02%R(I3)/B(I,IFF),0.D0)
          ENDDO
        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END

