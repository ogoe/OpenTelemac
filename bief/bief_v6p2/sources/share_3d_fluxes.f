!                    **************************
                     SUBROUTINE SHARE_3D_FLUXES
!                    **************************
!
     &(FLUX,XMUL,NPLAN,MESH2,MESH3,OPT)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SHARES ASSEMBLED FLUXES BETWEEN SUB-DOMAINS.
!+
!+            THIS IS IN SOME SENSE THE CONTRARY OF PARCOM2_SEG, BUT
!+                THE FLUXES ON THE HORIZONTAL SEGMENTS WILL BE DIVIDED
!+                BY XMUL*2 AND THE FLUXES OF VERTICAL INTERFACE SEGMENTS
!+                WILL BE DIVIDED BY XMUL*MESH%FAC%R.
!
!warning  MUST NOT BE CALLED WHEN NCSIZE = 0
!
!history  J-M HERVOUET (LNHE)
!+        14/04/2010
!+        V6P0
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
!| FLUX           |<->| FLUXES TO BE SHARED
!| MESH2          |-->| 2D MESH
!| MESH3          |-->| 3D MESH
!| NPLAN          |-->| NUMBER OF PLANES
!| OPT            |-->| 1 : HORIZONTAL AND VERTICAL SEGMENTS ONLY
!|                |   | 2 : ALL SEGMENTS
!| XMUL           |-->| MULTIPLICATING FACTOR
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN) :: NPLAN,OPT
!
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH2,MESH3
!
      DOUBLE PRECISION, INTENT(INOUT) :: FLUX(*)
      DOUBLE PRECISION, INTENT(INOUT) :: XMUL
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPLAN,NSEG,NSEGH,NSEGV,NPOIN2,I,I3D,I2D,IAD
!
!-----------------------------------------------------------------------
!
      NSEG=MESH2%NSEG
      NPOIN2=MESH2%NPOIN
      NSEGH=NSEG*NPLAN
      NSEGV=NPOIN2*(NPLAN-1)
!
!     HORIZONTAL FLUXES
!
      DO IPLAN=1,NPLAN
        CALL MULT_INTERFACE_SEG(FLUX(1+(IPLAN-1)*NSEG:IPLAN*NSEG),
     &                          MESH2%NH_COM_SEG%I,
     &                          MESH2%NH_COM_SEG%DIM1,
     &                          MESH2%NB_NEIGHB_SEG,
     &                          MESH2%NB_NEIGHB_PT_SEG%I,
     &                          0.5D0*XMUL,NSEG)
      ENDDO
!
!     VERTICAL FLUXES (SAME NUMBERING AS POINTS, SO FAC%R(I))
!
      IAD=1
      DO I=1,NPTIR
!       I2D=NACHB(1,I) WITH NACHB OF SIZE NACHB(NBMAXNSHARE,NPTIR)
!       IAD IS (I-1)*NBMAXNSHARE+1
        I2D=MESH2%NACHB%I(IAD)
        DO IPLAN=1,NPLAN-1
          I3D=(IPLAN-1)*NPOIN2+I2D
          FLUX(NSEGH+I3D)=FLUX(NSEGH+I3D)*MESH3%FAC%R(I3D)*XMUL
        ENDDO
        IAD=IAD+NBMAXNSHARE
      ENDDO
!
!     ALTERNATIVE: SIMPLER BUT LESS EFFICIENT FOR VERTICAL FLUXES:
!
!     DO I3D=1,NSEGV
!       FLUX(NSEGH+I3D)=FLUX(NSEGH+I3D)*MESH3%FAC%R(I3D)*XMUL
!     ENDDO
!
!     CROSSED FLUXES (SEE STOSEG41 FOR STORAGE). THERE ARE 2*NESG
!     PER LAYER AND NPLAN-1 LAYER. HERE ORISEG=1 AND ORISEG=2 SEGMENTS
!     ARE MULTIPLIED BY THE SAME NUMBER, SO GIVEN HOW THE NUMBERING
!     IS BUILT IT IS AS IF WE HAVE 2*NPLAN-2 LAYERS OF HORIZONTAL SEGMENTS
!
      IF(OPT.EQ.2) THEN
        DO IPLAN=1,2*(NPLAN-1)
          CALL MULT_INTERFACE_SEG(FLUX(1+NSEGH+NSEGV+(IPLAN-1)*NSEG:
     &                                   NSEGH+NSEGV+ IPLAN   *NSEG),
     &                            MESH2%NH_COM_SEG%I,
     &                            MESH2%NH_COM_SEG%DIM1,
     &                            MESH2%NB_NEIGHB_SEG,
     &                            MESH2%NB_NEIGHB_PT_SEG%I,
     &                            0.5D0*XMUL,NSEG)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
