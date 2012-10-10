!                    ****************
                     SUBROUTINE PROPA
!                    ****************
!
     &(F,B,SHP,SHZ,SHF,ELT,ETA,FRE,IKLE2,IKLE_EXT,
     & ETAP1,NPOIN3,NPOIN2,NELEM2,
     & NPLAN,NF,COURAN,TRA01,TRA02,ITR01,T3_01,T3_02,ISUB,MESH3D)
!
!***********************************************************************
! TOMAWAC   V6P3                                   23/06/2011
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| B              |-->| JACOBIAN TO TRANSFORM N(KX,KY) INTO F(FR,TETA)
!| COURAN         |-->| LOGICAL INDICATING IF THERE IS A CURRENT
!| ELT            |-->| NUMBERS OF THE ELEMENTS 2D OF THE
!|                |   | POINTS TO BE ADVECTED
!| ETA            |-->| NUMBERS OF THE LAYERS OF THE
!|                |   | POINTS TO BE ADVECTED
!| ETAP1          |<->| HIGHER LAYERS TABLE
!| F              |<->| WAVE ACTION DENSITY OR VARIANCE DENSITY
!|                |   | DIRECTIONAL SPECTRUM
!| FRE            |-->| NUMBER OF THE FREQUENCIES OF THE
!|                |   | POINTS TO BE ADVECTED
!| IKLE2          |-->| TRANSITION BETWEEN LOCAL AND GLOBAL NUMBERING
!|                |   | OF THE 2D MESH
!| NELEM2         |-->| NUMBER OF ELEMENTS IN 2D MESH
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
      USE TOMAWAC_MPI
      USE STREAMLINE, ONLY : POST_INTERP
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NPOIN3,NPOIN2,NELEM2,NPLAN,NF
!
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION, INTENT(IN) :: B(NPOIN2,NF)
      DOUBLE PRECISION TRA01(NPOIN3,8),TRA02(NPOIN2,NPLAN,NF)
      INTEGER, INTENT(INOUT) :: ELT(NPOIN3,NF),ETA(NPOIN3,NF)
      INTEGER, INTENT(IN) :: FRE(NPOIN3,NF),ISUB(NPOIN3,NF)
      INTEGER, INTENT(INOUT) :: ITR01(NPOIN3*3)
      INTEGER IKLE2(NELEM2,3),ETAP1(NPLAN)
      LOGICAL, INTENT(IN)            :: COURAN
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: SHP,SHZ,SHF,T3_01,T3_02
      TYPE(BIEF_OBJ), INTENT(IN)     :: IKLE_EXT
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH3D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IFF,I,ISTAT,I3,IPLAN
      DOUBLE PRECISION BID
!
!----------------------------------------------------------------------
!
      IF(.NOT.COURAN) THEN
!
        DO IFF=1,NF
!
!           JMH: A QUOI SERT CE IFREQ ??? (QUI VAUT NF POUR FINIR)
!           IFREQ = IFF
!           CALL INTERP_TOMAWAC(F(1,IFF),B(1,IFF),
!    &                          SHP%ADR(IFF)%P%R,
!    &                          SHZ%ADR(IFF)%P%R,ELT(1,IFF),
!    &                          ETA(1,IFF),IKLE2,
!    &                          ETAP1,NPOIN2,NELEM2,NPLAN,TRA01)
!
!         COPY OF F*B INTO T3_01
!
          DO IPLAN=1,NPLAN
            DO I=1,NPOIN2
              I3=I+(IPLAN-1)*NPOIN2
              T3_01%R(I3)=F(I,IPLAN,IFF)*B(I,IFF) 
            ENDDO
          ENDDO
!
          CALL POST_INTERP(T3_01,T3_02,SHP%ADR(IFF)%P%R,
     &                     SHZ%ADR(IFF)%P%R,IKLE_EXT%I,
     &                     IKLE_EXT%DIM1,1,
     &                     NPOIN2,ELT(1,IFF),ETA(1,IFF),
     &                     ISUB(1,IFF),3,NPLAN,41,41,NPOIN3,
     &                     NPOIN2,TRA01,TRA02,ITR01(1:NPOIN3),
     &                     ITR01(NPOIN3+1:2*NPOIN3),NPOIN3,.TRUE.)
!                                                        PERIODICITY                                                         
          IF(NCSIZE.GT.1) CALL PARCOM(T3_02,1,MESH3D)                    
!
!         FINAL COMPUTATION OF F
!
          DO IPLAN=1,NPLAN
            DO I=1,NPOIN2
              I3=I+(IPLAN-1)*NPOIN2
!             JMH: WHY ???
              T3_02%R(I3)=MAX(0.D0,T3_02%R(I3))
              F(I,IPLAN,IFF)=T3_02%R(I3)/B(I,IFF) 
            ENDDO
          ENDDO
!
        ENDDO
!
      ELSE
!
        CALL INTER4D(F,B,SHP,SHZ,SHF,ELT,ETA,
     &             FRE,IKLE2,ETAP1,NPOIN2,NPOIN3,NELEM2,NPLAN,NF,TRA02)
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
