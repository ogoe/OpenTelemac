!                    *****************
                     SUBROUTINE MASSED
!                    *****************
!
     &(MASBED,EPAI,CONC,HDEP,TRA02,NPOIN2,  
     & NPFMAX,NCOUCH,NPF,TASSE,GIBSON,RHOS,VOLU2D,
     & CFDEP,EPAICO,EPAINCO,MIXTE)
!
!***********************************************************************
! TELEMAC3D   V7P0                                   21/08/2010
!***********************************************************************
!
!brief    PERFORMS INITIAL RELATIVE MASS BALANCE FOR
!+                THE SEDIMENT.
!
!history  C.LE NORMANT(LNH)
!+        26/08/92
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
!+        17/03/2011
!+        V6P1
!+   Rewritten (formula changed, parallelism,...)
!
!history  C. VILLARET & T. BENSON & D. KELLY (HR-WALLINGFORD)
!+        27/02/2014
!+        V7P0
!+   New developments in sediment merged on 25/02/2014.
!
!history  G. ANTOINE & M. JODEAU & J.M. HERVOUET (EDF - LNHE)
!+        13/10/2014
!+        V7P0
!+   New developments in sediment for mixed sediment transport
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CFDEP          |-->| CONCENTRATION OF MUD DEPOSIT (G/L)
!| CONC           |-->| CONCENTRATION OF MUD BED LAYER
!|                |   | (MULTILAYER MODEL)
!| EPAI           |-->| THICKNESS OF SOLID BED LAYER
!|                |   | (EPAI=DZ/(1+IVIDE), DZ TOTAL BED THICKNESS)
!| EPAICO         |-->| THICKNESS OF COHESIVE SUB-LAYER
!| EPAINCO        |-->| THICKNESS OF NON-COHESIVE SUB-LAYER
!| GIBSON         |-->| LOGICAL FOR GIBSON MODEL
!| HDEP           |-->| THICKNESS OF FRESH DEPOSIT (FLUID MUD LAYER)
!| MASBED         |<->| MASS OF SEDIMENT BED
!| MIXTE          |-->| LOGICAL, MIXED SEDIMENTS OR NOT
!| NCOUCH         |-->| NUMBER OF LAYERS WITHIN THE BED
!|                |   | (GIBSON MODEL)
!| NPF            |-->| NUMBER OF POINTS OF THE BOTTOM ON ONE VERTICAL
!| NPFMAX         |-->| MAXIMUM NUMBER OF HORIZONTAL PLANES THAT
!|                |   | DISCRETISE MUD BED
!| NPOIN2         |-->| NUMBER OF POINTS IN 2D
!| RHOS           |-->| DENSITY OF SEDIMENT
!| TASSE          |-->| MULTILAYER SETTLING MODEL LOGICAL
!| TRA02          |<->| WORK ARRAY
!| VOLU2D         |-->| INTEGRAL OF TEST FUNCTIONS IN 2D
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_MASSED => MASSED
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPFMAX,NPOIN2,NCOUCH
      DOUBLE PRECISION, INTENT(IN)    :: CFDEP
      INTEGER, INTENT(IN)             :: NPF(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: MASBED
      DOUBLE PRECISION, INTENT(IN)    :: EPAI(NPOIN2,NCOUCH)
      DOUBLE PRECISION, INTENT(IN)    :: EPAICO(NPOIN2), EPAINCO(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: VOLU2D(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: HDEP(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: CONC(NPOIN2,NCOUCH)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA02(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: RHOS
      LOGICAL, INTENT(IN)             :: TASSE, GIBSON, MIXTE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN,IPF
      DOUBLE PRECISION MASSE6
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
!   
!=======================================================================
!
! MASS OF MUDDY DEPOSITS ON THE RIGID BED (MASSE6)
! 
!=======================================================================
!
      IF(MIXTE) THEN

        DO IPOIN=1,NPOIN2
          TRA02(IPOIN)=0.D0
          TRA02(IPOIN)=TRA02(IPOIN) + CONC(IPOIN,1)*EPAICO(IPOIN)
     &      + CFDEP*EPAINCO(IPOIN)
        ENDDO

      ELSE

        DO IPOIN=1,NPOIN2
          TRA02(IPOIN)=0.D0
          DO IPF=1,NCOUCH
            TRA02(IPOIN)=TRA02(IPOIN)+CONC(IPOIN,IPF)*EPAI(IPOIN,IPF)
          ENDDO
        ENDDO
!
      ENDIF

      MASSE6=0.D0
      DO IPOIN=1,NPOIN2
        MASSE6=MASSE6+VOLU2D(IPOIN)*TRA02(IPOIN)
      ENDDO
      IF(NCSIZE.GT.1) MASSE6=P_DSUM(MASSE6)
      MASBED = MASSE6      
!
!-----------------------------------------------------------------------
!
      RETURN
      END

