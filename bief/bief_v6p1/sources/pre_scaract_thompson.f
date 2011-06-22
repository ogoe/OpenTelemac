!                *******************************
                 SUBROUTINE PRE_SCARACT_THOMPSON
!                *******************************
!
     *(NPTFR,NPT,NDP,NELEM,NBOR,LISPFR,
     * X,Y,ITRAV2,SHP,XCONV,YCONV,SHPT,ELT)
!     
!***********************************************************************
! BIEF   V6P1                                   25/03/2011
!***********************************************************************
!
!brief    PREPARES ADVECTION BY THE METHOD OF CHARACTERISTICS IN THE
!+                CASE THOMPSON BOUNDARY CONDITIONS
!
!history  CHRISTOPHE DENIS (SINETICS)
!+        25/03/2011
!+        V6P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| NPTFR          |-->| NUMBER OF BOUNDARY POINTS IN THE MESH 
!| NPT            |-->| NUMBER OF POINTS TO BE TREATED
!| NDP            |-->| NUMBER OF POINTS PER ELEMENT
!| NELEM          |-->| NUMBER OF ELEMENTS
!| NBOR           |-->| GLOBAL NUMBER OF BOUNDARY POINTS
!| LISPFR         |-->| LIST OF BOUNDARY POINTS NUMBERS OF POINTS TO BE
!|                |   | TREATED
!| X              |-->| ABSCISSAE OF NODES IN THE MESH
!| Y              |-->| ORDINATES OF NODES IN THE MESH
!| ITRAV2         |-->| ORDINATES OF NODES IN THE MESH
!| SHP            |<--| HORIZONTAL BARYCENTRIC COORDINATES OF POINTS IN
!|                |   | THE MESH
!| XCONV          |<--| ABSCISSAE OF POINTS TO BE ADVECTED
!| YCONV          |<--| ORDINATES OF POINTS TO BE ADVECTED
!| SHPT           |<--| HORIZONTAL BARYCENTRIC COORDINATES OF POINTS TO
!|                |   | BE ADVECTED
!| ELT            |<--| STARTING ELEMENT OF POINT TO BE ADVECTED
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
      INTEGER, INTENT(IN)          :: NPTFR,NPT,NDP,NELEM           
      INTEGER, INTENT(IN)          :: NBOR(*),LISPFR(*),ITRAV2(*)
      INTEGER, INTENT(OUT)         :: ELT(*)
      DOUBLE PRECISION, INTENT(OUT):: XCONV(*),YCONV(*),SHPT(NDP,NPTFR)
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),SHP(NDP,*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IFR,IPT
!  
!-----------------------------------------------------------------------   
!
      DO IFR=1,NPT
        IPT=NBOR(LISPFR(IFR))
        XCONV(IFR) = X(IPT)
        YCONV(IFR) = Y(IPT)         
        ELT(IFR)   = ITRAV2(IPT)
        SHPT(1,IFR)= SHP(1,IPT)
        SHPT(2,IFR)= SHP(2,IPT)
        SHPT(3,IFR)= SHP(3,IPT)
      ENDDO
!  
!-----------------------------------------------------------------------   
!
      RETURN         
      END 
