!                    ****************
                     SUBROUTINE PROPA
!                    ****************
!
     &   (F,B,SHP1,SHP2,SHP3,SHZ,SHF,ELT,ETA,FRE,IKLE2,ETAP1,
     &    NPOIN3,NPOIN2,NELEM2,NPLAN,NF,COURAN,TRA01,TRA02)
!
!***********************************************************************
! TOMAWAC   V6P1                                   23/06/2011
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
!| SHP1,SHP2,SHP3 |-->| BARYCENTRIC COORDINATES OF THE NODES IN
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
!
      IMPLICIT NONE
!
      INTEGER NPOIN3,NPOIN2,NELEM2,NPLAN,NF
!
      DOUBLE PRECISION F(NPOIN2,NPLAN,NF)
      DOUBLE PRECISION SHP1(NPOIN3,NF) , SHP2(NPOIN3,NF)
      DOUBLE PRECISION SHP3(NPOIN3,NF) , SHZ(NPOIN3,NF)
      DOUBLE PRECISION SHF(NPOIN3,NF)
      DOUBLE PRECISION B(NPOIN2,NF)
      DOUBLE PRECISION TRA01(NPOIN3,8),TRA02(NPOIN2,NPLAN,NF)
      INTEGER ELT(NPOIN3,NF),ETA(NPOIN3,NF),FRE(NPOIN3,NF)
      INTEGER IKLE2(NELEM2,3),ETAP1(NPLAN)
      LOGICAL COURAN
      REAL WW(1)
!
      DOUBLE PRECISION X(1)
      INTEGER IFF,I,ISTAT,LU, IB(1)
      CHARACTER*3 CAR
!
!----------------------------------------------------------------------
!
      LU=6
!
        IF (.NOT.COURAN) THEN
!
         DO 300 IFF=1,NF
!
            IFREQ = IFF
            CALL INTERP_TOMAWAC
     &        (F(1,1,IFF),B(1,IFF),SHP1(1,IFF),SHP2(1,IFF),
     &             SHP3(1,IFF),SHZ(1,IFF),ELT(1,IFF),ETA(1,IFF),IKLE2,
     &         ETAP1,NPOIN2,NELEM2,NPLAN,TRA01)
!
300      CONTINUE
!
        ELSE
!
            CALL INTER4D
     &       (F,B,SHP1,SHP2,SHP3,SHZ,SHF,ELT,ETA,
     &        FRE,IKLE2,ETAP1,NPOIN2,NELEM2,NPLAN,NF,TRA02)
!
        ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
