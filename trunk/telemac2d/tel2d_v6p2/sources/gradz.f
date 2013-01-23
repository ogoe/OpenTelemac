!                    ****************
                     SUBROUTINE GRADZ
!                    ****************
!
     &(NS,NT,NSEG,NU,NUBO,X,Y,AIRT,AIRS,CMI,JV,
     & ZF,DPX,DPY,DSZ,BETA,AIRST,DXIZ,DYIZ,DSP,DSM,CORR)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SAINT VENANT-KINETIC.
!+
!+            COMPUTES THE Z VARIATIONS (2ND ORDER).
!
!history  INRIA
!+
!+        V5P4
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
!| AIRS           |-->| CELL'S AREAS
!| AIRST          |-->| AREAS OF SUBTRIANGLES WITHIN THE CELLS
!| AIRT           |-->| TRIANGLES' AREAS
!| BETA           |-->|  EXTRAPOLATION COEFFICIENT
!| CMI            |-->| COORDINATES OF THE INTERFACE MIDDLE POINT
!| DPX,DPY        |-->| GRADIENT OF P1 BASE FUNCTIONS
!|                |   | PER TRIANGLE
!| DSM            |<->| EXTRAPOLATED GRADIENTS
!| CORR           |<->| CORRECTION TO HAVE CONSERVATION
!| DSZ            |<--| VARIATION OF Z FOR ORDRE 2
!| DXIZ,DYIZ,DSP  |<->| WORKING TABLES 
!| JV             |-->| NUMBER OF TRIANGLE IN WHICH IS LOCATED 
!|                |   | THE INTERFACE MIDDLE POINT
!| NS             |-->| TOTAL NUMER OF NODES IN THE MESH
!| NSEG           |-->| TOTAL NUMER OF SEGMENTS IN THE MESH
!| NT             |-->| TOTAL NUMBER OF ELEMENTS IN THE MESH
!| NU             |-->| NUMBERING OF NODES IN THE TRIANGLE
!| NUBO           |-->| NUMBERS OF THE TWO NODES FORMING ONE EDGE (SEGMENT)
!| X,Y            |-->| COORDINATES IF THE NODES
!| ZF             |-->| BATHYMETRY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NS,NT,NSEG
      INTEGER, INTENT(IN)             :: NU(NT,3),NUBO(2,NSEG),JV(*)
      DOUBLE PRECISION, INTENT(INOUT) :: DSZ(2,*)
      DOUBLE PRECISION, INTENT(IN)    :: X(NS),Y(NS),AIRT(NT),AIRS(NS)
      DOUBLE PRECISION, INTENT(INOUT) :: DXIZ(NS),DYIZ(NS)
      DOUBLE PRECISION, INTENT(INOUT) :: DSP(NS),DSM(NS),CORR(NS),BETA
      DOUBLE PRECISION, INTENT(IN)    :: DPX(3,NT),DPY(3,NT)
      DOUBLE PRECISION, INTENT(IN)    :: CMI(2,*),AIRST(2,*),ZF(NS)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,I1,I2,I3,JT,J,NSG,NUBO1,NUBO2,ILIM
      DOUBLE PRECISION AIRJ,DXTZ,DYTZ,AIX,AIY,AJX,AJY
      DOUBLE PRECISION ZF1,ZF2,GRADI,GRADJ,GRIJ,GRJI,AMDS,DSH
!
      DOUBLE PRECISION EXLIM
      EXTERNAL         EXLIM
!
!-----------------------------------------------------------------------
!
      DO IS=1,NS
        DXIZ(IS) = 0.D0
        DYIZ(IS) = 0.D0
      ENDDO
!
      DO JT=1,NT
!
         I1 = NU(JT,1)
         I2 = NU(JT,2)
         I3 = NU(JT,3)
!
         AIRJ =   AIRT(JT)
         DXTZ =ZF(I1)*DPX(1,JT) +ZF(I2)*DPX(2,JT) + ZF(I3)*DPX(3,JT)
         DYTZ =ZF(I1)*DPY(1,JT) +ZF(I2)*DPY(2,JT) + ZF(I3)*DPY(3,JT)
!
         DXIZ(I1) = DXIZ(I1) + AIRJ*DXTZ
         DXIZ(I2) = DXIZ(I2) + AIRJ*DXTZ
         DXIZ(I3) = DXIZ(I3) + AIRJ*DXTZ
!
         DYIZ(I1) = DYIZ(I1) + AIRJ*DYTZ
         DYIZ(I2) = DYIZ(I2) + AIRJ*DYTZ
         DYIZ(I3) = DYIZ(I3) + AIRJ*DYTZ
      ENDDO
!
      DO IS=1,NS
         DXIZ(IS) = DXIZ(IS)/(3.D0*AIRS(IS))
         DYIZ(IS) = DYIZ(IS)/(3.D0*AIRS(IS))
         DSP(IS)  = 0.D0
         DSM(IS)  = 0.D0
      ENDDO
!
!    REBUILDS BY INTERFACE
!
      DO NSG=1,NSEG
!
         J         = JV(NSG)
!
         NUBO1     = NUBO(1,NSG)
         NUBO2     = NUBO(2,NSG)
!
         ZF1   =    ZF(NUBO1)
         ZF2   =    ZF(NUBO2)
!
         AIX       = CMI(1,NSG)-X(NUBO1)
         AIY       = CMI(2,NSG)-Y(NUBO1)
         AJX       = CMI(1,NSG)-X(NUBO2)
         AJY       = CMI(2,NSG)-Y(NUBO2)
!
!        NODE GRADIENTS
!
         GRADI  = AIX*DXIZ(NUBO1) + AIY*DYIZ(NUBO1)
!
         GRADJ  = AJX*DXIZ(NUBO2) + AJY*DYIZ(NUBO2)
!
!
         I1 = NU(J,1)
         I2 = NU(J,2)
         I3 = NU(J,3)
!
!        GRADIENT BY TRIANGLE
!
         DXTZ =ZF(I1)*DPX(1,J) +ZF(I2)*DPX(2,J) + ZF(I3)*DPX(3,J)
         DYTZ =ZF(I1)*DPY(1,J) +ZF(I2)*DPY(2,J) + ZF(I3)*DPY(3,J)
!
         GRIJ  = AIX*DXTZ + AIY*DYTZ
!
         GRJI  = AJX*DXTZ + AJY*DYTZ
!
!    EXTRAPOLATES AND CAPS
!
       ILIM=1
       BETA=1.D0
         DSZ(1,NSG)  =  EXLIM(ILIM,BETA,GRADI,GRIJ )
         DSZ(2,NSG)  =  EXLIM (ILIM,BETA,GRADJ,GRJI )
!
         IF(DSZ(1,NSG).GE.0.D0) THEN
         DSP(NUBO1) = DSP(NUBO1) + AIRST(1,NSG)*DSZ(1,NSG)
         ELSE
         DSM(NUBO1) = DSM(NUBO1) - AIRST(1,NSG)*DSZ(1,NSG)
         ENDIF
         IF(DSZ(2,NSG).GE.0.) THEN
         DSP(NUBO2) = DSP(NUBO2) + AIRST(2,NSG)*DSZ(2,NSG)
         ELSE
         DSM(NUBO2) = DSM(NUBO2) - AIRST(2,NSG)*DSZ(2,NSG)
         ENDIF
!
       ENDDO
!
!  COMPUTES THE CORRECTIONS NECESSARY TO HAVE CONSERVATION
!
      DO IS=1,NS
       CORR(IS) =  DSM(IS) - DSP(IS)
       AMDS =MAX(DSP(IS),DSM(IS))
        IF(AMDS.GT.0.D0) THEN
        CORR(IS) = CORR(IS)/AMDS
        ENDIF
      ENDDO
!
      DO NSG=1,NSEG
!
         NUBO1 = NUBO(1,NSG)
         NUBO2 = NUBO(2,NSG)
!
         DSH =  DSZ(1,NSG)
         DSZ(1,NSG) =   DSH +
     & MIN(0.D0,CORR(NUBO1))*MAX(0.D0,DSH)+
     & MAX(0.D0,CORR(NUBO1))*MAX(0.D0,-DSH)
!
         DSH     =  DSZ(2,NSG)
         DSZ(2,NSG) =   DSH +
     & MIN(0.D0,CORR(NUBO2))*MAX(0.D0,DSH)+
     & MAX(0.D0,CORR(NUBO2))*MAX(0.D0,-DSH)
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
