!                    *****************
                     SUBROUTINE TASSEC
!                    *****************
!
     &( CONC   , EPAI , TREST , TEMP , DTC ,
     &  NPOIN2 , NCOUCH )
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    MULTI-LAYER MODEL FOR CONSOLIDATION OF THE MUDDY BED.
!code
!+      DETERMINES IF THE QUANTITY OF MUD IN A GIVEN LAYER HAS
!+      BEEN THERE FOR A LONG ENOUGH TIME (I.E TEMP>TREST) TO
!+      BE MOVED TO THE (MORE CONSOLIDATED) NEXT LAYER DOWN
!+      IF THAT IS THE CASE, THE THICKNESS OF THE LATTER (EPAI)
!+      INCREASES.
!+      STARTS WITH THE LAYER BEFORE LAST (THE LAST ONE HAVING
!+      AN INFINITE RESIDENCE TIME), AND ENDS WITH THE LAYER
!+      CLOSEST TO THE BED SURFACE.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  C LE NORMANT (LNH)
!+        04/05/92
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
!| CONC           |-->| CONCENTRATION OF MUD BED LAYERS
!| DTC            |-->| TIME STEP FOR CONSOLIDATION PHENOMENON
!| EPAI           |<->| THICKNESS OF MESH ELEMENTS DISCRETISING THE BED
!| NCOUCH         |-->| NUMBER OF LAYERS WITHIN THE MUD BED
!| NCOUCH         |-->| NOMBRE DE COUCHES DISCRETISANT LE FOND VASEUX
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| TEMP           |<->| TIME COUNTER FOR CONSOLIDATION MODEL
!|                |   | (MULTILAYER MODEL)
!| TREST          |-->| CONSOLIDATION TIME SCALE
!|                |   | (ONLY FOR MULTILAYER MODEL)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)             :: NPOIN2, NCOUCH
      DOUBLE PRECISION, INTENT(INOUT) :: EPAI(NCOUCH,NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: TEMP(NCOUCH,NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: CONC(NCOUCH), TREST(NCOUCH)
      DOUBLE PRECISION, INTENT(IN)    :: DTC
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IPOIN, IC
!
!======================================================================
!
       DO IPOIN=1,NPOIN2
         DO IC=2,NCOUCH
           IF (EPAI(IC,IPOIN).GT.0.D0)
     &       TEMP(IC,IPOIN) = TEMP(IC,IPOIN) +DTC
           IF (TEMP(IC,IPOIN).GE.TREST(IC)) THEN
             EPAI(IC-1,IPOIN) = EPAI(IC-1,IPOIN) +
     &                EPAI(IC,IPOIN)*CONC(IC)/CONC(IC-1)
             EPAI(IC,IPOIN)=0.D0
             TEMP(IC,IPOIN)=0.D0
          ENDIF
         END DO
       END DO
!
       RETURN
       END SUBROUTINE TASSEC
