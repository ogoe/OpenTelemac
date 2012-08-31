!                    ******************
                     SUBROUTINE GRADNOD
!                    ******************
!
     &(NS,NT,NU,AIRT,AIRS,UA,DPX,DPY,DJX,DJY,DX,DY,IVIS,CVIS,CE,ZF)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES THE GRADIENTS BY TRIANGLES AND NODE
!+                AND THE DIFFUSION TERMS.
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
!| CVIS           |-->| COEFFICIENT OF DIFFUSION
!| DJX,DJY        |<--| GRADIENTS PER TRIANGLE
!| IVIS           |-->| OPTION FOR DIFFUSION OF VELOCITY
!| DX,DY          |<--| GRADIENTS AT NODES
!| NS             |-->| TOTAL NUMER OF NODES IN THE MESH
!| NT             |-->| TOTAL NUMBER OF ELEMENTS IN THE MESH
!| NU             |-->| NUMBERING OF NODES IN THE TRIANGLE
!| UA             |-->| UA(1,IS) = H,  UA(2,IS)=U  ,UA(3,IS)=V
!| T              |-->| TRACERS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NS,NT,IVIS
      INTEGER, INTENT(IN)             :: NU(NT,3)
      DOUBLE PRECISION, INTENT(IN)    :: AIRT(NT),AIRS(NS),CVIS
      DOUBLE PRECISION, INTENT(INOUT) :: DJX(3,NT),DJY(3,NT)
      DOUBLE PRECISION, INTENT(INOUT) :: DX(3,NS),DY(3,NS)
      DOUBLE PRECISION, INTENT(INOUT) :: CE(NS,3)
      DOUBLE PRECISION, INTENT(IN)    :: UA(3,NS),ZF(NS)
      DOUBLE PRECISION, INTENT(IN)    :: DPX(3,NT),DPY(3,NT)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,JT,NUBO1,NUBO2,NUBO3,IVAR
      DOUBLE PRECISION AIRJ,UA1,UA2,UA3,AIS,HTT,AUX
!
!-----------------------------------------------------------------------
!
!     INITIALISES THE HERMITIAN NODAL GRADIENTS
!
      DO IS=1,NS
        DO IVAR=1,3
          DX(IVAR,IS) = 0.D0
          DY(IVAR,IS) = 0.D0
        ENDDO
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
         HTT = UA(1,NUBO1)+UA(1,NUBO2)+UA(1,NUBO3)
         AUX = CVIS*AIRJ*HTT/3.D0
!
!        COMPUTES THE P1-GRADIENTS
!
!
!   COMPUTES THE H+Z GRADIENT
!
       IVAR=1
           UA1=UA(IVAR,NUBO1) + ZF(NUBO1)
           UA2=UA(IVAR,NUBO2) + ZF(NUBO2)
           UA3=UA(IVAR,NUBO3) + ZF(NUBO3)
!
            DJX(IVAR,JT)      = UA1*DPX(1,JT) +
     &               UA2*DPX(2,JT) + UA3*DPX(3,JT)
            DJY(IVAR,JT)      = UA1*DPY(1,JT) +
     &               UA2*DPY(2,JT) + UA3*DPY(3,JT)
!
         DX(IVAR,NUBO1)    = DX(IVAR,NUBO1) + AIRJ*DJX(IVAR,JT)
         DX(IVAR,NUBO2)    = DX(IVAR,NUBO2) + AIRJ*DJX(IVAR,JT)
         DX(IVAR,NUBO3)    = DX(IVAR,NUBO3) + AIRJ*DJX(IVAR,JT)
!
         DY(IVAR,NUBO1)    = DY(IVAR,NUBO1) + AIRJ*DJY(IVAR,JT)
         DY(IVAR,NUBO2)    = DY(IVAR,NUBO2) + AIRJ*DJY(IVAR,JT)
         DY(IVAR,NUBO3)    = DY(IVAR,NUBO3) + AIRJ*DJY(IVAR,JT)
!
!    COMPUTES THE VELOCITY GRADIENTS
!
         DO IVAR=2,3
!
           UA1=UA(IVAR,NUBO1)
           UA2=UA(IVAR,NUBO2)
           UA3=UA(IVAR,NUBO3)
!
            DJX(IVAR,JT)      = UA1*DPX(1,JT) +
     &               UA2*DPX(2,JT) + UA3*DPX(3,JT)
            DJY(IVAR,JT)      = UA1*DPY(1,JT) +
     &               UA2*DPY(2,JT) + UA3*DPY(3,JT)
!
         DX(IVAR,NUBO1)    = DX(IVAR,NUBO1) + AIRJ*DJX(IVAR,JT)
         DX(IVAR,NUBO2)    = DX(IVAR,NUBO2) + AIRJ*DJX(IVAR,JT)
         DX(IVAR,NUBO3)    = DX(IVAR,NUBO3) + AIRJ*DJX(IVAR,JT)
!
         DY(IVAR,NUBO1)    = DY(IVAR,NUBO1) + AIRJ*DJY(IVAR,JT)
         DY(IVAR,NUBO2)    = DY(IVAR,NUBO2) + AIRJ*DJY(IVAR,JT)
         DY(IVAR,NUBO3)    = DY(IVAR,NUBO3) + AIRJ*DJY(IVAR,JT)
         ENDDO
!
!  COMPUTES THE VELOCITY DIFFUSION TERMS
!
      IF(IVIS.EQ.0.OR.CVIS.EQ.0.) GOTO 10
         CE(NUBO1,2)       = CE(NUBO1,2) -AUX*
     &  (DJX(2,JT)*DPX(1,JT)+DJY(2,JT)*DPY(1,JT))
         CE(NUBO2,2)       = CE(NUBO2,2) -AUX*
     &  (DJX(2,JT)*DPX(2,JT)+DJY(2,JT)*DPY(2,JT))
         CE(NUBO3,2)       = CE(NUBO3,2) -AUX*
     &  (DJX(2,JT)*DPX(3,JT)+DJY(2,JT)*DPY(3,JT))
!
         CE(NUBO1,3)       = CE(NUBO1,3) -AUX*
     &  (DJX(3,JT)*DPX(1,JT)+DJY(3,JT)*DPY(1,JT))
         CE(NUBO2,3)       = CE(NUBO2,3) -AUX*
     &  (DJX(3,JT)*DPX(2,JT)+DJY(3,JT)*DPY(2,JT))
         CE(NUBO3,3)       = CE(NUBO3,3) -AUX*
     &  (DJX(3,JT)*DPX(3,JT)+DJY(3,JT)*DPY(3,JT))
 10       CONTINUE
         ENDDO
!
!     COMPLETES THE COMPUTATION OF THE NODAL GRADIENTS
!
      DO IS=1,NS
!
        AIS = 1.D0/(3.D0*AIRS(IS))
!
        DO IVAR=1,3
          DX(IVAR,IS) = DX(IVAR,IS)*AIS
          DY(IVAR,IS) = DY(IVAR,IS)*AIS
        ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
