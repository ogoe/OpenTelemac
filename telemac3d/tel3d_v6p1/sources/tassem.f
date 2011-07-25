!                    *****************
                     SUBROUTINE TASSEM
!                    *****************
!
     & ( IVIDE , EPAI ,
     &   NPOIN2, NPFMAX, NPF  ,
     &   GRAV  , RHOS  , DTC  , CFMAX ,
     &   TRA01 , SIGMA , CHARGE)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MODELS CONSOLIDATION OF THE MUDDDY BED.
!
!note     THE EXPERIMENTAL RELATIONSHIPS K(E) AND SIGMA(E)
!+         ARE BASED ON THE WORK OF BEEN AND SILLS.
!+         THE USER CAN CHOOSE DIFFERENT RELATIONSHIPS.
!
!reference  BEEN AND SILLS. GEOTECHNIQUE, 31, N4. 1981.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  C LE NORMANT (LNH)
!+        13/05/92
!+        V5P1
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
!| CFMAX          |-->| CONCENTRATION OF CONSOLIDATED MUD (G/L)
!| CHARGE         |<->| WEIGHT SUPPORTED BY SOLID SKELETON
!| DTC            |-->| TIME STEP OF CONSOLIDATION  MODEL
!| EPAI           |-->| THICKNESS OF SOLID FRACTION OF THE BED LAYER
!|                |   | (EPAI=DZ/(1+IVIDE), DZ BED LAYER THICKNESS)
!| GRAV           |-->| ACCELERATION OF GRAVITY
!| IVIDE          |<->| VOID RATIO
!| NPF            |-->| NUMBER OF POINTS WITHIN THE BED
!| NPFMAX         |-->| MAXIMUM NUMBER OF HORIZONTAL PLANES
!|                |   | WITHIN THE BED
!| NPOIN2         |-->| NUMBER OF POINTS OF 2D MESH
!| RHOS           |-->| SEDIMENT DENSITY
!| SIGMA          |<->| EFFECTIVE STRESS
!| TRA01          |<->| WORK ARRAY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
       USE INTERFACE_TELEMAC3D, EX_TASSEM => TASSEM
       IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
       INTEGER, INTENT(IN)             :: NPOIN2,NPFMAX
       INTEGER, INTENT(IN)             :: NPF(NPOIN2)
       DOUBLE PRECISION, INTENT(IN)    :: EPAI(NPFMAX-1,NPOIN2)
       DOUBLE PRECISION, INTENT(INOUT) :: IVIDE(NPFMAX,NPOIN2)
       DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPFMAX,6)
       DOUBLE PRECISION, INTENT(INOUT) :: SIGMA(NPFMAX)
       DOUBLE PRECISION, INTENT(INOUT) :: CHARGE(NPFMAX)
       DOUBLE PRECISION, INTENT(IN)    :: RHOS,GRAV,DTC,CFMAX
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
       DOUBLE PRECISION DSIGMA,DSIG1,PE,DPE
       INTEGER  IPOIN,IPF,NDEB
!
       INTRINSIC DLOG
!
!=======================================================================
!
!   ----- LOOP ON 2D MESH POINTS-----
!
       DO IPOIN=1,NPOIN2
         IF (NPF(IPOIN).GE.5) THEN
!
           CHARGE(NPF(IPOIN))=0.D0
!
!   ----- COMPUTES EQUATION COEFFICIENTS -----
!
          DO IPF=1,NPF(IPOIN)
!
!   ----- THE FOLLOWING LINES CAN BE MODIFIED BY USER -----
!
!     BED PERMEABILITY: LOG K=0.24*E-7.86
!                       PE=K/(1+IVIDE)) :
            PE=10.D0**(0.24D0*IVIDE(IPF,IPOIN)-7.86D0)/
     &                 (1.D0+IVIDE(IPF,IPOIN))
!
!     DERIVATIVE OF PE RELATIVE TO IVIDE :
!
            DPE=PE*(0.24D0*DLOG(10.D0)-1.D0/(1.D0+IVIDE(IPF,IPOIN)))
!
!     EFFECTIVE STRESS: SIGMA=900/(E-2.5)
!
            SIGMA(IPF)=900.D0/(IVIDE(IPF,IPOIN)-2.5D0)
!
!     DERIVATIVE OF SIGMA RELATIVE TO IVIDE :
!
            DSIGMA=-900.D0/((IVIDE(IPF,IPOIN)-2.5D0)**2)
!
!   **** END OF USER'S MODIFICATIONS
!
!
            IF (IPF.EQ.1) DSIG1=DSIGMA
            TRA01(IPF,1)=((RHOS/1000.D0)-1.D0)*DPE
            TRA01(IPF,2)=-1.D0/(1000.D0*GRAV)*DSIGMA*PE
!
          END DO
!
!   ------ NON LINEAR CHARGE INCREASE -----
!
           DO IPF=NPF(IPOIN)-1,1,-1
!
!     WEIGHT SUPPORTED BY SOLID SKELETON
!
           CHARGE(IPF)=(2650.D0-1000.D0)*9.81D0
     &     * EPAI(IPF,IPOIN)+CHARGE(IPF+1)
           END DO
!
           NDEB=1
           DO IPF=2,NPF(IPOIN)
           IF ((SIGMA(IPF)-CHARGE(IPF)).GT.1.D-3) THEN
           NDEB=IPF
           ELSE
!
! ARGH, OUT OF A LOOP
!
          GOTO 50
            ENDIF
           END DO
50         CONTINUE
!
          IF (NDEB.LT.(NPF(IPOIN)-1)) THEN
!
!   ------COMPUTES THE BISSEL COEFFICIENTS -----
!
          CALL COEF(IVIDE(1,IPOIN), EPAI(1,IPOIN), TRA01,
     &              NPFMAX        , NPF(IPOIN)   , NDEB ,
     &              RHOS          , GRAV         , DTC  , DSIG1)
!
!   ----- SOLVES GIBSON EQUATION -----
!
          CALL BISSEL(IVIDE(1,IPOIN), TRA01, NPFMAX, NPF(IPOIN),NDEB)
!
          ENDIF
!
         ENDIF
       END DO
!
      RETURN
      END SUBROUTINE TASSEM
