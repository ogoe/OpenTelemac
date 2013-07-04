!                    ***********************
                     SUBROUTINE CHECK_DIGITS
!                    ***********************
!
     &(F,T1,MESH)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    IN PARALLEL MODE, CHECKS THAT PROCESSORS SHARING AN
!+                INTERFACE POINT HAVE EXACTLY THE SAME VALUE FOR ARRAY F.
!
!history  J-M HERVOUET (LNHE)
!+        02/06/08
!+        V5P9
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
!| F              |-->| BIEF_OBJ STRUCTURE TO BE CHECKED
!| MESH           |-->| MESH STRUCTURE
!| T1             |<->| WORK BIEF STRUCTURE, SIMILAR TO F
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
      TYPE(BIEF_OBJ),  INTENT(IN   ) :: F
      TYPE(BIEF_OBJ),  INTENT(INOUT) :: T1
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I,ISTOP
!
      INTEGER  P_IMAX
      EXTERNAL P_IMAX
!
!-----------------------------------------------------------------------
!
      CALL OS('X=Y     ',X=T1,Y=F)
      CALL PARCOM(T1,3,MESH)
      ISTOP=0
      DO I=1,T1%DIM1
        IF(T1%R(I).NE.F%R(I)) THEN
          IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'CHECK_DIGITS : DIFFERENCE DANS ',F%NAME
          WRITE(LU,*) '               AU POINT LOCAL ',I
          WRITE(LU,*) '               =  POINT GLOBAL ',MESH%KNOLG%I(I)
          WRITE(LU,*) '               VALEUR ',F%R(I)
          WRITE(LU,*) '               MINIMUM ',T1%R(I)
          WRITE(LU,*) '            DIFFERENCE ',F%R(I)-T1%R(I)
          ENDIF
          IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'CHECK_DIGITS : DIFFERENCE IN ',F%NAME
          WRITE(LU,*) '               AT LOCAL POINT ',I
          WRITE(LU,*) '               =  GLOBAL POINT ',MESH%KNOLG%I(I)
          WRITE(LU,*) '               VALUE ',F%R(I)
          WRITE(LU,*) '               MINIMUM ',T1%R(I)
          WRITE(LU,*) '            DIFFERENCE ',F%R(I)-T1%R(I)
          ENDIF
          ISTOP=I
        ENDIF
      ENDDO
!
      IF(NCSIZE.GT.1) ISTOP=P_IMAX(ISTOP)
      IF(ISTOP.GT.0) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'CHECK_DIGITS : ERREUR SUR VECTEUR ',F%NAME
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'CHECK_DIGITS : ERROR ON VECTOR ',F%NAME
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
