!                    ******************
                     SUBROUTINE GRADNOD
!                    ******************
!
     &(NS,NT,NU,AIRT,AIRS,UA,DPX,DPY,DJX,DJY,DX,DY,IVIS,CVIS,CE,ZF,
     & MESH)
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
!history  R. ATA (EDF R&D-LNHE)
!+        13/04/2013
!+        V6P3
!+   Optimization and parallel implementation
!+   More explicit english comments
!
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
      USE BIEF
      USE BIEF_DEF, ONLY: NCSIZE
      USE INTERFACE_TELEMAC2D, EX_GRADNOD => GRADNOD
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
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IS,JT,NUBO1,NUBO2,NUBO3,IVAR
      DOUBLE PRECISION AIRJ,UA1,UA2,UA3,AIS,HTT,AUX,TIERS,TEMPOR
!
      TIERS = 1.0D0/3.0D0
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
!
!       COMPUTES THE P1-GRADIENTS
!
!   COMPUTES THE H+Z GRADIENT
!
        IVAR=1
        UA1=UA(IVAR,NUBO1) + ZF(NUBO1)
        UA2=UA(IVAR,NUBO2) + ZF(NUBO2)
        UA3=UA(IVAR,NUBO3) + ZF(NUBO3)
!
        DJX(IVAR,JT) = UA1*DPX(1,JT)+UA2*DPX(2,JT)+UA3*DPX(3,JT) ! GRAD_X(H+Z)|_Tk
        DJY(IVAR,JT) = UA1*DPY(1,JT)+UA2*DPY(2,JT)+UA3*DPY(3,JT) ! GRAD_Y(H+Z)|_Tk
!
        TEMPOR = AIRJ*DJX(IVAR,JT)
        DX(IVAR,NUBO1) = DX(IVAR,NUBO1) + TEMPOR ! SUM( |C_k|*GRAD_X(H+Z)|_Tk )
        DX(IVAR,NUBO2) = DX(IVAR,NUBO2) + TEMPOR ! SAME AS FOR NUBO1
        DX(IVAR,NUBO3) = DX(IVAR,NUBO3) + TEMPOR ! SAME AS FOR NUBO1
!
        TEMPOR = AIRJ*DJY(IVAR,JT)
        DY(IVAR,NUBO1) = DY(IVAR,NUBO1) + TEMPOR ! SUM( |C_k|*GRAD_Y(H+Z)|_Tk )
        DY(IVAR,NUBO2) = DY(IVAR,NUBO2) + TEMPOR ! SAME AS FOR NUBO1
        DY(IVAR,NUBO3) = DY(IVAR,NUBO3) + TEMPOR ! SAME AS FOR NUBO2
!
!    COMPUTES THE VELOCITY GRADIENTS
!
        DO IVAR=2,3
!
          UA1=UA(IVAR,NUBO1) ! U OR V
          UA2=UA(IVAR,NUBO2)
          UA3=UA(IVAR,NUBO3)
!
          DJX(IVAR,JT) = UA1*DPX(1,JT)+UA2*DPX(2,JT)+UA3*DPX(3,JT)! GRAD_X(U)|_Tk
          DJY(IVAR,JT) = UA1*DPY(1,JT)+UA2*DPY(2,JT)+UA3*DPY(3,JT)! GRAD_Y(U)|_Tk
!
          TEMPOR = AIRJ*DJX(IVAR,JT)
          DX(IVAR,NUBO1) = DX(IVAR,NUBO1) + TEMPOR ! SUM( |C_k|*GRAD_Y(U)|_Tk )
          DX(IVAR,NUBO2) = DX(IVAR,NUBO2) + TEMPOR ! SAME AS FOR NUBO1
          DX(IVAR,NUBO3) = DX(IVAR,NUBO3) + TEMPOR ! SAME AS FOR NUBO1
!
          TEMPOR = AIRJ*DJY(IVAR,JT)
          DY(IVAR,NUBO1) = DY(IVAR,NUBO1) + TEMPOR ! SUM( |C_k|*GRAD_Y(U)|_Tk )
          DY(IVAR,NUBO2) = DY(IVAR,NUBO2) + TEMPOR ! SAME AS FOR NUBO1
          DY(IVAR,NUBO3) = DY(IVAR,NUBO3) + TEMPOR ! SAME AS FOR NUBO1
        ENDDO
      ENDDO ! RA: SEPARATION OF LOOPS TO EXECUTE PARCOMS

!     FOR PARALLELILSM
      IF(NCSIZE.GT.1)THEN                 ! NPON,NPLAN,ICOM,IAN
        CALL PARCOM2(DX(1,:),DX(2,:),DX(3,:),NS,1,2,3,MESH )
        CALL PARCOM2(DY(1,:),DY(2,:),DY(3,:),NS,1,2,3,MESH )
      ENDIF
!
      IF(IVIS.EQ.0.OR.CVIS.EQ.0.) GOTO 10 ! IF THERE IS NO VISCOSITY OR NO VELOCITY DIFFUSISION
!
!     INITIALISES CE (WAS NOT DONE BEFORE)
      CALL OV( 'X=0     ' ,CE(:,1)   ,CE(:,1)  ,CE(:,1),0.D0 ,NS)
      CALL OV( 'X=0     ' ,CE(:,2)   ,CE(:,2)  ,CE(:,2),0.D0 ,NS)
      CALL OV( 'X=0     ' ,CE(:,3)   ,CE(:,3)  ,CE(:,3),0.D0 ,NS)
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
!  COMPUTES THE VELOCITY DIFFUSION TERMS
!
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
      ENDDO
!     FOR PARALLELILSM
      IF(NCSIZE.GT.1)THEN                 ! NPON,NPLAN,ICOM,IAN
        CALL PARCOM2(CE(:,1),CE(:,2),CE(:,3),NS,1,2,3,MESH )
      ENDIF
!
10       CONTINUE
!
!     COMPLETES THE COMPUTATION OF THE NODAL GRADIENTS
!
      DO IS=1,NS
!
        AIS = TIERS/AIRS(IS)
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
