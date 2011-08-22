!                    *****************
                     SUBROUTINE DERLAG
!                    *****************
!
     &( U , V , DT , X , Y , IKLE , IFABOR , LT , IELM , NDP , NPOIN ,
     &  NELEM , NELMAX , SURDET , XLAG , YLAG , DX , DY ,
     &  NSP , SHPLAG , DEBLAG , FINLAG , ELTLAG , NLAG , RESUX , RESUY ,
     &  NBOR , NELBOR , NULONE , NPTFR , MSK,MASKEL,MASKPT,T8)
!
!***********************************************************************
! BIEF   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    - SETS THE BARYCENTRIC COORDINATES IN THE MESH,
!+                  AT THE START OF COMPUTATION FOR EACH DRIFTING FLOAT.
!+                  HERE WE COMPUTE THE LAGRANGIAN DRIFT.
!+
!+            - COMPUTES THE SUCCESSIVE POSITIONS OF THIS FLOAT
!+                 (SUBSEQUENT TIMESTEPS).
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
!| DEBLAG         |-->| TIME STEP FOR STARTING THE COMPUTATION
!| DT             |-->| TIME STEP
!| DX             |<->| WORK ARRAY
!| DY             |<->| WORK ARRAY
!| ELTLAG         |<->| ELEMENT NUMBERS OF FLOATS
!| FINLAG         |-->| TIME STEP FOR ENDING THE COMPUTATION
!| IELM           |-->| TYPE OF ELEMENT IN THE MESH
!| IFABOR         |-->| ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!|                |   | IF NEGATIVE OR ZERO, THE EDGE IS A LIQUID
!|                |   | BOUNDARY
!| IKLE           |-->| CONNECTIVITY TABLE.
!| LT             |-->| TIME STEP NUMBER.
!| MASKEL         |-->| MASKING OF ELEMENTS.
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MASKPT         |-->| MASKING PER POINT.
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NBOR           |-->| GLOBAL NUMBERS OF BOUNDARY POINTS
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| NELBOR         |-->| FOR THE KTH BOUNDARY EDGE, GIVES THE CORRESPONDING
!|                |   | ELEMENT.
!| NELEM          |-->| NUMBER OF ELEMENTS.
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS.
!| NLAG           |-->| NOMBER OF FLOATS.
!| NPOIN          |-->| NUMBER OF POINTS
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NSP            |-->| NUMBER OF SUB-STEPS IN THE RUNGE-KUTTA METHOD
!| NULONE         |-->| GOES WITH ARRAY NELBOR. NELBOR GIVES THE 
!|                |   | ADJACENT ELEMENT, NULONE GIVES THE LOCAL
!|                |   | NUMBER OF THE FIRST NODE OF THE BOUNDARY EDGE
!|                |   | I.E. 1, 2 OR 3 FOR TRIANGLES.
!| RESUX          |<--| ARRAY WITH SUCCESSIVE ABSCISSAE OF FLOATS
!| RESUY          |<--| ARRAY WITH SUCCESSIVE ORDINATES OF FLOATS
!| SHPLAG         |<->| BARYCENTRIC COORDINATES OF FLOATS
!|                |   | IN THEIR ELEMENTS.
!| SURDET         |-->| GEOMETRIC COEFFICIENT USED IN THE ISOPARAMETRIC
!|                |   | TRANSFORMATION.
!| T8             |-->| BLOCK OF WORK BIEF_OBJ STRUCTURES.
!| U              |-->| X-COMPONENT OF VELOCITY
!| V              |-->| Y-COMPONENT OF VELOCITY
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH
!| XLAG           |<->| INSTANTANEOUS X POSITIONS OF FLOATS
!| Y              |-->| ORDINATES OF POINTS IN THE MESH
!| YLAG           |<->| INSTANTANEOUS Y POSITIONS OF FLOATS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF   !, EX_DERLAG => DERLAG
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NPOIN,LT,IELM,NDP,NELEM,NLAG
      INTEGER         , INTENT(IN)    :: NPTFR,NELMAX
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN),V(NPOIN),DT
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      INTEGER         , INTENT(IN)    :: IKLE(NELMAX,NDP)
      INTEGER         , INTENT(IN)    :: IFABOR(NELMAX,NDP)
      DOUBLE PRECISION, INTENT(IN)    :: SURDET(NELEM)
      DOUBLE PRECISION, INTENT(INOUT) :: XLAG(NPOIN,NLAG)
      DOUBLE PRECISION, INTENT(INOUT) :: YLAG(NPOIN,NLAG)
      INTEGER         , INTENT(INOUT) :: DEBLAG(NLAG),FINLAG(NLAG)
      INTEGER         , INTENT(INOUT) :: ELTLAG(NPOIN,NLAG)
      DOUBLE PRECISION, INTENT(INOUT) :: T8(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: DX(NPOIN),DY(NPOIN)
      INTEGER         , INTENT(INOUT) :: NSP(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: RESUX(NPOIN),RESUY(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: SHPLAG(NDP,NPOIN,NLAG)
      INTEGER         , INTENT(IN)    :: NBOR(NPTFR),NELBOR(NPTFR)
      INTEGER         , INTENT(IN)    :: NULONE(NPTFR)
      LOGICAL         , INTENT(IN)    :: MSK
      DOUBLE PRECISION, INTENT(IN)    :: MASKEL(NELMAX),MASKPT(NPOIN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ILAG,JLAG,LTT,NRK,IPOIN
!
      DOUBLE PRECISION Z(1),C
!
!-----------------------------------------------------------------------
!
      DO 10 ILAG=1,NLAG
!
        IF(LT.EQ.DEBLAG(ILAG)) THEN
!
!-----------------------------------------------------------------------
!
!   - SETS THE BARYCENTRIC COORDINATES IN THE MESH , AT THE START
!     OF COMPUTATION FOR EACH FLOAT
!
!-----------------------------------------------------------------------
!
          IF(IELM.EQ.11) THEN
!
!  P1 TRIANGLES
!  ============
!
!      FILLS THE SHP AND ELT (OPTIMISED)
!
            CALL GTSH11(SHPLAG(1,1,ILAG),ELTLAG(1,ILAG),IKLE,NPOIN,
     &                  NELEM,NELMAX,MSK,MASKEL)
!
          ELSE
!
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) IELM,' : ELEMENT NON PREVU DANS DERLAG'
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) IELM,': ELEMENT NOT IMPLEMENTED IN DERLAG'
            ENDIF
            CALL PLANTE(1)
            STOP
!
          ENDIF
!
          CALL OV( 'X=Y     ' , XLAG(1,ILAG) , X , Z , C , NPOIN )
          CALL OV( 'X=Y     ' , YLAG(1,ILAG) , Y , Z , C , NPOIN )
!
        ELSEIF(LT.GT.DEBLAG(ILAG).AND.LT.LE.FINLAG(ILAG)) THEN
!
!-----------------------------------------------------------------------
!
!   - COMPUTES THE SUCCESSIVE POSITIONS OF THIS FLOAT
!     (SUBSEQUENT TIMESTEPS)
!
!-----------------------------------------------------------------------
!
! NUMBER OF RUNGE-KUTTA SUB-STEPS, BY CROSSED ELEMENT
! ======================================================
!
          NRK     =  3
!
!  P1 TRIANGLES
!  ============
!
          CALL CHAR11( U , V , DT , NRK , X , Y , IKLE , IFABOR ,
     &                 XLAG(1,ILAG) , YLAG(1,ILAG) , DX , DY ,
     &                 SHPLAG(1,1,ILAG) , ELTLAG(1,ILAG) , NSP ,
     &                 NPOIN , NPOIN , NELEM , NELMAX , SURDET , 1 ,T8)
!
        ENDIF
!
!-----------------------------------------------------------------------
!
!   - CANCELS THE FLOATS LEAVING THE DOMAIN
!
!-----------------------------------------------------------------------
!
        IF(LT.EQ.FINLAG(ILAG)) THEN
          DO IPOIN=1,NPOIN
            IF(ELTLAG(IPOIN,ILAG).LE.0) THEN
              XLAG(IPOIN,ILAG) = X(IPOIN)
              YLAG(IPOIN,ILAG) = Y(IPOIN)
            ENDIF
          ENDDO
        ENDIF
!
10    CONTINUE
!
!-----------------------------------------------------------------------
!
!   - STORAGE FOR RESULTS OUTPUT OF THE LAST COMPUTED FLOAT
!
!-----------------------------------------------------------------------
!
      CALL OV( 'X=C     ' , RESUX , Y , Z , 0.D0 , NPOIN )
      CALL OV( 'X=C     ' , RESUY , Y , Z , 0.D0 , NPOIN )
      LTT=0
      JLAG=1
      DO ILAG=1,NLAG
        IF(FINLAG(ILAG).GT.LTT.AND.FINLAG(ILAG).LE.LT) THEN
          LTT=FINLAG(ILAG)
          JLAG=ILAG
        ENDIF
      ENDDO
      IF(LTT.NE.0) THEN
        CALL OV( 'X=Y-Z   ' , RESUX , XLAG(1,JLAG) , X , C , NPOIN )
        CALL OV( 'X=Y-Z   ' , RESUY , YLAG(1,JLAG) , Y , C , NPOIN )
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
