!                    ********************************
                     DOUBLE PRECISION FUNCTION P_DOTS
!                    ********************************
!
     &( X , Y , MESH )
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SAME AS DOTS BUT TAKING PARALLELISM INTO ACCOUNT.
!+
!+            SCALAR PRODUCT OF TWO OBJECTS, WHICH CAN BE:
!+
!+            TWO VECTORS STRUCTURES, OR
!+
!+            TWO VECTOR BLOCKS STRUCTURES OF IDENTICAL NUMBER AND
!+                CHARACTERISTICS.
!
!warning  IF THE VECTORS HAVE A SECOND DIMENSION, IT IS IGNORED
!+            FOR THE TIME BEING
!
!history  J-M HERVOUET (LNH)
!+        24/04/97
!+        V5P1
!+   AFTER REINHARD HINKELMANN (HANNOVER UNI.)
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
!| MESH           |-->| MESH STRUCTURE
!| X              |-->| BIEF_OBJ STRUCTURE (MAY BE A BLOCK)
!| Y              |-->| BIEF_OBJ STRUCTURE (MAY BE A BLOCK)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF, EX_P_DOTS => P_DOTS
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      TYPE(BIEF_MESH), INTENT(IN) :: MESH
      TYPE(BIEF_OBJ), INTENT(IN)  :: X,Y
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      INTEGER NPX,IBL,TYPX
!
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
!
!-----------------------------------------------------------------------
!
      TYPX = X%TYPE
!
!-----------------------------------------------------------------------
!
!  CASE WHERE THE STRUCTURES ARE BLOCKS
!
      IF(TYPX.EQ.4) THEN
!
       P_DOTS = 0.D0
!
       IF(NCSIZE.LE.1.OR.NPTIR.EQ.0) THEN
         DO IBL = 1 , X%N
           P_DOTS=P_DOTS+DOT(X%ADR(IBL)%P%DIM1,X%ADR(IBL)%P%R,
     &                                         Y%ADR(IBL)%P%R)
         ENDDO
       ELSE
         DO IBL = 1 , X%N
           P_DOTS=P_DOTS+P_DOT(X%ADR(IBL)%P%DIM1,X%ADR(IBL)%P%R,
     &                                           Y%ADR(IBL)%P%R,
     &                                           MESH%FAC%R)
         ENDDO
       ENDIF
!
!-----------------------------------------------------------------------
!
!  CASE WHERE THE STRUCTURES ARE NOT BLOCKS
!  (ASSUMES THAT Y HAS THE SAME TYPE AS X)
!
      ELSEIF(TYPX.EQ.2) THEN
!
        NPX = X%DIM1
!
        IF(Y%DIM1.NE.NPX) THEN
          IF (LNG.EQ.1) WRITE(LU,50) X%NAME,X%TYPE
          IF (LNG.EQ.1) WRITE(LU,51) Y%NAME,Y%TYPE
          IF (LNG.EQ.1) WRITE(LU,52) X%DIM1,Y%DIM1
          IF (LNG.EQ.2) WRITE(LU,60) X%NAME,X%TYPE
          IF (LNG.EQ.2) WRITE(LU,61) Y%NAME,Y%TYPE
          IF (LNG.EQ.2) WRITE(LU,62) X%DIM1,Y%DIM1
52        FORMAT(1X,'TAILLES DIFFERENTES : ',1I6,' ET ',1I6)
62        FORMAT(1X,'DIFFERENT SIZES: ',1I6,' AND ',1I6)
          CALL PLANTE(1)
          STOP
        ENDIF
!
        IF(NCSIZE.LE.1.OR.NPTIR.EQ.0) THEN
          P_DOTS=DOT(NPX,X%R,Y%R)
        ELSE
          P_DOTS=P_DOT(NPX,X%R,Y%R,MESH%FAC%R)
        ENDIF
!
!-----------------------------------------------------------------------
!
!  ERROR
!
      ELSE
!
         IF (LNG.EQ.1) WRITE(LU,50) X%NAME,X%TYPE
         IF (LNG.EQ.1) WRITE(LU,51) Y%NAME,Y%TYPE
         IF (LNG.EQ.1) WRITE(LU,53)
         IF (LNG.EQ.2) WRITE(LU,60) X%NAME,X%TYPE
         IF (LNG.EQ.2) WRITE(LU,61) Y%NAME,Y%TYPE
         IF (LNG.EQ.2) WRITE(LU,63)
50       FORMAT(1X,'P_DOTS (BIEF) : NOM DE X : ',A6,'  TYPE : ',1I6)
51       FORMAT(1X,'                NOM DE Y : ',A6,'  TYPE : ',1I6)
53       FORMAT(1X,'                CAS NON PREVU')
60       FORMAT(1X,'P_DOTS (BIEF) : NAME OF X : ',A6,'  TYPE : ',1I6)
61       FORMAT(1X,'                NAME OF Y : ',A6,'  TYPE : ',1I6)
63       FORMAT(1X,'                NOT IMPLEMENTED')
         CALL PLANTE(1)
         STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
! FINAL SUM ON ALL THE SUB-DOMAINS
!
      IF(NCSIZE.GT.1) P_DOTS = P_DSUM(P_DOTS)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
