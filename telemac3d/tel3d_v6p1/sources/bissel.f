!                    *****************
                     SUBROUTINE BISSEL
!                    *****************
!
     &(IVIDE,TRA01,NPFMAX,IMAX,NDEB)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SOLVES A SYSTEM OF IMAX EQUATIONS WITH UNKNOWN E
!+                AT TIME N+1.
!+
!+            METHOD KNOWN AS DOUBLE SWEEPING METHOD.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  NOEMIE DURAND AND SEBASTIEN BOURBAN; C LE NORMANT (LNH)
!+        18/07/06
!+        V5P7
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
!| IMAX           |-->| NUMBER OF POINTS AT THE BOTTOM MESH
!| IVIDE          |<->| INDEX OF EMPTY SPACES AT MESH POINTS
!| NDEB           |-->| INDEX LIMITING THE RANGE OF THE RESOLUTION
!| NPFMAX         |-->| MAXIMUM NUMBER OF HORIZONTAL PLANES THAT
!|                |   | DISCRETISE MUDDY BOTTOM
!| TRA01          |<->| WORKING ARRAY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER, INTENT(IN) :: NPFMAX, IMAX
!
      DOUBLE PRECISION, INTENT(INOUT) :: IVIDE(NPFMAX)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPFMAX,6)
!
      INTEGER K,NDEB
      DOUBLE PRECISION EPS
!
!======================================================================
!
!#####> NOE-CHANGES
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!  THE FLOATING POINT EXCEPTIONS ARE NOW CHECKED BEFORE BEING USED
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!#####
      EPS=1.D-8
      DO K=1+NDEB,IMAX-1
        IF( ABS(TRA01(K-1,4)).LT.EPS ) THEN
          WRITE(LU,*) 'FLOATING EXCEPTION IN BISSEL (CALLED BY TASSEM)'
          CALL PLANTE(1)
          STOP
        ENDIF
        TRA01(K,4) = TRA01(K,4)-(TRA01(K,3)*TRA01(K-1,5))/TRA01(K-1,4)
        TRA01(K,6) = TRA01(K,6)-(TRA01(K,3)*TRA01(K-1,6))/TRA01(K-1,4)
      ENDDO
!
      IF(ABS(TRA01(IMAX,3)*TRA01(IMAX-1,5)
     &         -TRA01(IMAX-1,4)*TRA01(IMAX,4)).LT.EPS ) THEN
        WRITE(LU,*) 'DIVISION BY ZERO IN BISSEL (CALLED BY TASSEM)'
        CALL PLANTE(1)
        STOP
      ENDIF
      IVIDE(IMAX)=
     &  (TRA01(IMAX,6)*TRA01(IMAX-1,4)-TRA01(IMAX,3)*TRA01(IMAX-1,6))/
     &  (TRA01(IMAX,4)*TRA01(IMAX-1,4)-TRA01(IMAX,3)*TRA01(IMAX-1,5))
!
!#####> SEB-CHANGES
! SLIGHT MISTAKE IN THE LOOP INDEXING
!       DO I=1,IMAX-NDEB+1
!          K=IMAX+1-I
!          IVIDE(K)=0.D0
!          IVIDE(K)=(TRA01(K,6)-TRA01(K,5)*IVIDE(K+1))/TRA01(K,4)
       DO K = IMAX-1,NDEB,-1
         IF( ABS(TRA01(K,4)).LT.EPS ) THEN
           WRITE(LU,*) 'DIVISION BY ZERO IN BISSEL (CALLED BY TASSEM)'
           CALL PLANTE(1)
           STOP
         ENDIF
         IVIDE(K) = ( TRA01(K,6) - TRA01(K,5)*IVIDE(K+1) )/TRA01(K,4)
       ENDDO
!#####
!
!-----------------------------------------------------------------------
!
      RETURN
      END
