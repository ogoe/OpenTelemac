!                    *****************
                     SUBROUTINE CARAFR
!                    *****************
!
     &(U,V,H,T,UCONV,VCONV,X,Y,SHP,
     & SURDET,DT,IKLE , IFABOR , ELT ,
     & NBOR , NELBOR , NULONE , IELM , NELEM , NELMAX ,
     & NPOIN , NDP , NPTFR ,
     & MSK , MASKEL , MASKPT , NPT , LISPFR, NTRAC ,
     & HBTIL , UBTIL , VBTIL , TBTIL , ZBTIL , ZF, T5  )
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    SOLVES THE ADVECTION EQUATIONS BY THE METHOD OF
!+                CHARACTERISTICS, FOR A NUMBER OF FUNCTIONS AND ON AN
!+                ENSEMBLE OF FIXED BOUNDARY POINTS: LISPFR(NPT).
!
!history  E. DAVID (LHF)
!+        05/09/2008
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
!| DT             |-->| TIME STEP
!| ELT            |<->| ELEMENT NUMBERS AT THE FOOT OF CHARACTERISTICS
!| H              |-->| WATER DEPTH
!| HBTIL          |<--| RESULT OF ADVECTION OF H
!| IELM           |-->| TYPE OF ELEMENT
!|                |   | 11 : LINEAR TRIANGLE
!|                |   | 41 : TELEMAC-3D PRISM
!| IFABOR         |-->| ELEMENTS BEHIND THE EDGES OF A TRIANGLE
!|                |   | IF NEGATIVE OR ZERO, THE EDGE IS A LIQUID
!|                |   | BOUNDARY
!| IKLE           |-->| CONNECTIVITY TABLE
!| LISPFR         |-->| LIST OF POINTS TO BE DEALT WITH
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MASKPT         |-->| MASKING PER POINT.
!|                |   | =1. : NORMAL   =0. : MASKED
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| NELBOR         |-->| FOR THE KTH BOUNDARY EDGE, GIVES THE CORRESPONDING
!|                |   | ELEMENT.
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NELMAX         |-->| MAXIMUM NUMBER OF ELEMENTS
!| NPOIN          |-->| NUMBER OF POINTS
!| NPT            |-->| NUMBER OF POINTS TO BE DEALT WITH
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS
!| NTRAC          |-->| NUMBER OF TRACERS
!| NULONE         |-->| GOES WITH ARRAY NELBOR. NELBOR GIVES THE 
!|                |   | ADJACENT ELEMENT, NULONE GIVES THE LOCAL
!|                |   | NUMBER OF THE FIRST NODE OF THE BOUNDARY EDGE
!|                |   | I.E. 1, 2 OR 3 FOR TRIANGLES.
!| SHP            |<->| BARYCENTRIC COORDINATES AT THE FOOT OF 
!|                |   | CHARACTERISTICS
!| SURDET         |-->| 1/DETERMINANT(2D ELEMENTS)
!| T              |-->| BLOCK OF TRACERS
!| T5             |<->| WORK ARRAY
!| TBTIL          |<--| RESULT OF ADVECTION OF T (A BLOCK)
!| U              |-->| X-COMPONENT OF VELOCITY AT TIME N.
!| UBTIL          |<--| RESULT OF ADVECTION OF U.
!| UCONV          |-->| X-COMPONENT OF ADVECTION FIELD.
!| V              |-->| Y-COMPONENT OF VELOCITY AT TIME N.
!| VBTIL          |<--| RESULT OF ADVECTION OF V.
!| VCONV          |-->| Y-COMPONENT OF ADVECTION FIELD.
!| X              |-->| ABSCISSAE OF POINTS IN THE MESH.
!| Y              |-->| ORDINATES OF POINTS IN THE MESH.
!| ZBTIL          |<--| BOTTOM TOPOGRAPHY AFTER ADVECTION
!| ZF             |-->| BOTTOM TOPOGRAPHY
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
      INTEGER, INTENT(IN) :: NELEM,NELMAX,NPOIN,NDP,NPTFR,IELM,NPT,NTRAC
      INTEGER, INTENT(IN) :: LISPFR(NPTFR)
      INTEGER, INTENT(IN) :: IKLE(NELMAX,NDP),IFABOR(NELMAX,*)
      INTEGER, INTENT(IN) :: NBOR(NPTFR),NELBOR(NPTFR),NULONE(NPTFR)
      INTEGER, INTENT(INOUT)          :: ELT(NPOIN)
      LOGICAL, INTENT(IN)             :: MSK
      DOUBLE PRECISION, INTENT(INOUT) :: HBTIL(NPTFR),UBTIL(NPTFR)
      DOUBLE PRECISION, INTENT(INOUT) :: VBTIL(NPTFR),T5(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: ZBTIL(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: U(NPOIN),V(NPOIN),H(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: UCONV(NPOIN),VCONV(NPOIN)
      DOUBLE PRECISION, INTENT(INOUT) :: SHP(NDP,NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: ZF(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: MASKEL(NELMAX),MASKPT(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: SURDET(NELEM)
      DOUBLE PRECISION, INTENT(IN)    :: DT
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: TBTIL
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: T
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER NSP(1),IFR,IPT,NRK,ITRAC
!
      DOUBLE PRECISION XCONV(1),YCONV(1),DX(1),DY(1)
!
!-----------------------------------------------------------------------
!
! NUMBER OF SUB TIME STEPS OF RUNGE-KUTTA BY PROCESSED ELEMENT
!
      NRK = 3
!
      IF(IELM.EQ.11) THEN
!
!    P1 TRIANGLES
!    ============
!
!      CALLS THE SUBROUTINE UPWINDING THE CURVES OF CHARATERISTICS
!
        DO IFR=1,NPT
          IPT=NBOR(LISPFR(IFR))
          XCONV(1) = X(IPT)
          YCONV(1) = Y(IPT)
          CALL CHAR11(UCONV,VCONV,DT,NRK , X , Y , IKLE , IFABOR ,
     &                XCONV,YCONV,DX,DY , SHP(1,IPT) ,
     &                ELT(IPT) , NSP , 1 , NPOIN , NELEM , NELMAX ,
     &                SURDET , -1 ,T5)
        ENDDO
!
!-----------------------------------------------------------------------
!
      ELSE
!
        IF(LNG.EQ.1) WRITE(LU,11) IELM
        IF(LNG.EQ.2) WRITE(LU,12) IELM
11      FORMAT(1X,'CARAFR : TYPE D''ELEMENT INCONNU : ',I6)
12      FORMAT(1X,'CARAFR : UNKNOWN TYPE OF ELEMENT : ',I6)
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!  INTERPOLATION AT THE FOOT OF THE CHARACTERISTICS
!
      DO IFR=1,NPT
        IPT=NBOR(LISPFR(IFR))
        HBTIL(LISPFR(IFR)) =
     &         H(IKLE(ELT(IPT),1)) * SHP(1,IPT)
     &       + H(IKLE(ELT(IPT),2)) * SHP(2,IPT)
     &       + H(IKLE(ELT(IPT),3)) * SHP(3,IPT)
        UBTIL(LISPFR(IFR)) =
     &         U(IKLE(ELT(IPT),1)) * SHP(1,IPT)
     &       + U(IKLE(ELT(IPT),2)) * SHP(2,IPT)
     &       + U(IKLE(ELT(IPT),3)) * SHP(3,IPT)
        VBTIL(LISPFR(IFR)) =
     &         V(IKLE(ELT(IPT),1)) * SHP(1,IPT)
     &       + V(IKLE(ELT(IPT),2)) * SHP(2,IPT)
     &       + V(IKLE(ELT(IPT),3)) * SHP(3,IPT)
        ZBTIL(LISPFR(IFR)) =
     &         ZF(IKLE(ELT(IPT),1)) * SHP(1,IPT)
     &       + ZF(IKLE(ELT(IPT),2)) * SHP(2,IPT)
     &       + ZF(IKLE(ELT(IPT),3)) * SHP(3,IPT)
        IF(NTRAC.GT.0) THEN
          DO ITRAC=1,NTRAC
              TBTIL%ADR(ITRAC)%P%R(LISPFR(IFR)) =
     &        T%ADR(ITRAC)%P%R(IKLE(ELT(IPT),1)) * SHP(1,IPT)
     &      + T%ADR(ITRAC)%P%R(IKLE(ELT(IPT),2)) * SHP(2,IPT)
     &      + T%ADR(ITRAC)%P%R(IKLE(ELT(IPT),3)) * SHP(3,IPT)
          ENDDO
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
