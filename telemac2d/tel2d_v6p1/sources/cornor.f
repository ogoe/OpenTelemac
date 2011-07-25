!                    *****************
                     SUBROUTINE CORNOR
!                    *****************
!
     &(XNEBOR,YNEBOR,XSGBOR,YSGBOR,KP1BOR,NPTFR,KLOG,
     & LIHBOR,T1,T2,MESH)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    CORRECTS THE NORMALS TO THE NODES IN ACCORDANCE WITH
!+                THE BOUNDARY CONDITIONS TO HAVE NORMALS TO ADJACENT
!+                LIQUID SEGMENTS IN THE CASE OF A TRANSITION BETWEEN
!+                LIQUID AND SOLID.
!
!history  J-M HERVOUET (LNHE)
!+        19/09/2008
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
!| KLOG           |-->| CONVENTION FOR SOLID BOUNDARY
!| KP1BOR         |-->| GIVES THE NEXT BOUNDARY POINT IN A CONTOUR
!| LIHBOR         |-->| TYPE OF BOUNDARY CONDITIONS ON DEPTH
!| MESH           |-->| MESH STRUCTURE
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| T1             |<->| WORK BIEF_OBJ STRUCTURE
!| T2             |<->| WORK BIEF_OBJ STRUCTURE
!| XNEBOR         |<--| X-COMPONENT OF NORMAL AT NODES
!| XSGBOR         |-->| X-COMPONENT OF NORMAL TO SEGMENTS
!| YNEBOR         |<--| Y-COMPONENT OF NORMAL AT NODES
!| YSGBOR         |-->| Y-COMPONENT OF NORMAL TO SEGMENTS
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
      INTEGER, INTENT(IN)             :: NPTFR,KLOG
      INTEGER, INTENT(IN)             :: LIHBOR(NPTFR)  ,KP1BOR(NPTFR,2)
      DOUBLE PRECISION, INTENT(IN)    :: XSGBOR(NPTFR,4),YSGBOR(NPTFR,4)
      DOUBLE PRECISION, INTENT(INOUT) :: XNEBOR(NPTFR,2),YNEBOR(NPTFR,2)
!
      TYPE(BIEF_OBJ), INTENT(INOUT)   :: T1,T2
      TYPE(BIEF_MESH), INTENT(INOUT)  :: MESH
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER K,KP1,KM1
      DOUBLE PRECISION XNORM
!
      INTRINSIC SQRT
!
!-----------------------------------------------------------------------
!
      IF(NCSIZE.LE.1) THEN
!
!     IN SCALAR MODE
!
      DO K = 1 , NPTFR
!
!     LOOP OVER THE BOUNDARY POINTS
!
!     IF THE NODE IS BETWEEN A LIQUID SEGMENT AND A SOLID SEGMENT
!     ONLY CONSIDERS THE NORMAL TO THE ADJACENT LIQUID SEGMENT.
!
      KP1 = KP1BOR(K,1)
      KM1 = KP1BOR(K,2)
!
      IF( LIHBOR(KM1).EQ.KLOG.AND.
     &    LIHBOR(K  ).NE.KLOG.AND.
     &    LIHBOR(KP1).NE.KLOG      ) THEN
!
        XNEBOR(K,1) = XSGBOR(K,1)
        YNEBOR(K,1) = YSGBOR(K,1)
!
      ELSEIF( LIHBOR(KM1).NE.KLOG.AND.
     &        LIHBOR(K  ).NE.KLOG.AND.
     &        LIHBOR(KP1).EQ.KLOG      ) THEN
!
        XNEBOR(K,1) = XSGBOR(KM1,1)
        YNEBOR(K,1) = YSGBOR(KM1,1)
!
      ENDIF
!
      ENDDO
!
      ELSE
!
!     IN PARALLEL MODE
!
!     CONSIDERS ONLY THE NORMALS TO LIQUID BOUNDARIES HERE
!     THE ONLY ONES USED
!
!     COPIES THEN CANCELS T1 AND T2 FOR SOLID BOUNDARIES
!
      IF(NPTFR.GT.0) THEN
        DO K=1,NPTFR
!         NON NORMALISED VERSION OF XSGBOR AND YSGBOR
          T1%R(K)=XSGBOR(K,3)
          T2%R(K)=YSGBOR(K,3)
          XNEBOR(K,1)=0.D0
          YNEBOR(K,1)=0.D0
          KP1 = KP1BOR(K,1)
!         IF KP1 NOT IN DOMAIN: KP1=K, IT WORKS
          IF(LIHBOR(K).EQ.KLOG.OR.LIHBOR(KP1).EQ.KLOG) THEN
            T1%R(K)=0.D0
            T2%R(K)=0.D0
          ENDIF
        ENDDO
      ENDIF
!
!     START OF COMPUTATION OF XNEBOR AND YNEBOR FOR THE LIQUID BOUNDARIES
!
      IF(NPTFR.GT.0) THEN
        DO K=1,NPTFR
          KP1 = KP1BOR(K,1)
!         IF SEGMENT IN DOMAIN
          IF(K.NE.KP1) THEN
            XNEBOR(K  ,1)=XNEBOR(K  ,1)+T1%R(K)
            YNEBOR(K  ,1)=YNEBOR(K  ,1)+T2%R(K)
            XNEBOR(KP1,1)=XNEBOR(KP1,1)+T1%R(K)
            YNEBOR(KP1,1)=YNEBOR(KP1,1)+T2%R(K)
          ENDIF
        ENDDO
      ENDIF
!
!     ASSEMBLY IN PARALLEL
!
      CALL PARCOM_BORD(XNEBOR(1:NPTFR,1),2,MESH)
      CALL PARCOM_BORD(YNEBOR(1:NPTFR,1),2,MESH)
!
!     RENORMALISATION
!
      IF(NPTFR.GT.0) THEN
        DO K=1,NPTFR
          XNORM=SQRT(XNEBOR(K,1)**2+YNEBOR(K,1)**2)
          IF(XNORM.GT.1.D-10) THEN
            XNEBOR(K,1)=XNEBOR(K,1)/XNORM
            YNEBOR(K,1)=YNEBOR(K,1)/XNORM
          ELSE
!           POINT BETWEEN TWO SOLID SEGMENTS
!           TAKES THE COMPUTATION DONE IN NORMAB
            XNORM=SQRT(XNEBOR(K,2)**2+YNEBOR(K,2)**2)
            XNEBOR(K,1)=XNEBOR(K,2)/XNORM
            YNEBOR(K,1)=YNEBOR(K,2)/XNORM
          ENDIF
        ENDDO
      ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
