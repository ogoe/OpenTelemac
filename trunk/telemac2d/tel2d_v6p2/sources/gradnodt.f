!                    *******************
                     SUBROUTINE GRADNODT
!                    *******************
!
     &(NS,NT,NU,AIRT,AIRS,H,T,DPX,DPY,DJX,DJY,
     & DX,DY,DIFT,CVIST,CE,DTT)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE GRADIENTS BY TRIANGLES AND NODE
!+                AND THE DIFFUSION TERM FOR TRACER.
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
!| AIRT           |-->| TRIANGLES' AREAS
!| CE             |<--| DIFFUSION TERM
!| CVIST          |-->| COEFFICIENT OF TRACER DIFFUSION
!| DIFT           |-->| LOGICAL TO SAY IF THERE IS TRACER DIFFUSION OR NO
!| DJX,DJY        |<--| GRADIENTS PER TRIANGLE
!| DTT            |-->| TRACER TIME STEP
!| DX,DY          |<--| GRADIENTS AT NODES
!| H              |-->| WATER DEPTH
!| NS             |-->| TOTAL NUMER OF NODES IN THE MESH
!| NT             |-->| TOTAL NUMBER OF ELEMENTS IN THE MESH
!| NU             |-->| NUMBERING OF NODES IN THE TRIANGLE
!| T              |-->| TRACERS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NS,NT
      INTEGER, INTENT(IN)             :: NU(NT,3)
      LOGICAL, INTENT(IN)             :: DIFT
      DOUBLE PRECISION, INTENT(IN)    :: DPX(3,NT),DPY(3,NT)
      DOUBLE PRECISION, INTENT(IN)    :: AIRT(NT),AIRS(NS),H(NS),T(NS)
      DOUBLE PRECISION, INTENT(INOUT) :: DJX(NT),DJY(NT),DX(NS),DY(NS)
      DOUBLE PRECISION, INTENT(INOUT) :: CE(NS)
      DOUBLE PRECISION, INTENT(IN)    :: DTT,CVIST
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,JT,NUBO1,NUBO2,NUBO3
      DOUBLE PRECISION AIRJ,UA1,UA2,UA3,AIS,HTT,AUX
!
!-----------------------------------------------------------------------
!
!     INITIALISES THE HERMITIAN NODAL GRADIENTS
!
      DO IS=1,NS
        DX(IS)  = 0.D0
        DY(IS)  = 0.D0
      ENDDO
!
!     LOOP ON GLOBAL LIST OF TRIANGLES
!
      DO JT=1,NT
!
         NUBO1 = NU(JT,1)
         NUBO2 = NU(JT,2)
         NUBO3 = NU(JT,3)
!
         AIRJ=   AIRT(JT)
         HTT = H(NUBO1)+H(NUBO2)+H(NUBO3)
         AUX =  CVIST*DTT*AIRJ*HTT/3.
!
!        COMPUTES THE P1-GRADIENTS
!
           UA1=T(NUBO1)
           UA2=T(NUBO2)
           UA3=T(NUBO3)
!
!  GRADIENTS BY TRIANGLES
!
            DJX(JT)      = UA1*DPX(1,JT) +
     &               UA2*DPX(2,JT) + UA3*DPX(3,JT)
            DJY(JT)      = UA1*DPY(1,JT) +
     &               UA2*DPY(2,JT) + UA3*DPY(3,JT)
!
!  GRADIENTS BY NODES
!
         DX(NUBO1)    = DX(NUBO1) + AIRJ*DJX(JT)
         DX(NUBO2)    = DX(NUBO2) + AIRJ*DJX(JT)
         DX(NUBO3)    = DX(NUBO3) + AIRJ*DJX(JT)
!
         DY(NUBO1)    = DY(NUBO1) + AIRJ*DJY(JT)
         DY(NUBO2)    = DY(NUBO2) + AIRJ*DJY(JT)
         DY(NUBO3)    = DY(NUBO3) + AIRJ*DJY(JT)
!
!   DIFFUSION TERM
!
       IF(DIFT.AND.CVIST.NE.0.) THEN
         CE(NUBO1)       = CE(NUBO1) -AUX*
     &  (DJX(JT)*DPX(1,JT)+DJY(JT)*DPY(1,JT))
         CE(NUBO2)       = CE(NUBO2) -AUX*
     &  (DJX(JT)*DPX(2,JT)+DJY(JT)*DPY(2,JT))
         CE(NUBO3)       = CE(NUBO3) -AUX*
     &  (DJX(JT)*DPX(3,JT)+DJY(JT)*DPY(3,JT))
!
        ENDIF
!
       ENDDO
!
!     COMPLETES THE COMPUTATION OF THE NODAL GRADIENTS
!
      DO IS=1,NS
         AIS = 1.D0/(3.D0*AIRS(IS))
         DX(IS)       = DX(IS)*AIS
         DY(IS)       = DY(IS)*AIS
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
