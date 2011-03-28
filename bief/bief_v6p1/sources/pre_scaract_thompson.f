!                *******************************
                 SUBROUTINE PRE_SCARACT_THOMPSON
!                *******************************
!
     *(NPOIN,NPTFR,NPT,NDP,NELEM,NELMAX,IKLE,MSK,MASKEL,
     * NBOR,LISPFR,X,Y,ITRAV2,SHP,XCONV,YCONV,SHPT,ELT)
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
!|                |---| 
!|                |---|
!|                |---|
!|                |---|
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
      INTEGER, INTENT(IN)          :: NPOIN
      INTEGER, INTENT(IN)          :: NPTFR
      INTEGER, INTENT(IN)          :: NPT
      INTEGER, INTENT(IN)          :: NDP
      INTEGER, INTENT(IN)          :: NELEM
      INTEGER, INTENT(IN)          :: NELMAX
      INTEGER, INTENT(IN)          :: IKLE(NELEM,*)                  
      LOGICAL, INTENT(IN)          :: MSK
      TYPE(BIEF_OBJ), INTENT(IN)   :: MASKEL 
      INTEGER                      :: NBOR(*)
      INTEGER                      :: LISPFR(*)
      DOUBLE PRECISION, INTENT(OUT):: XCONV(*)
      DOUBLE PRECISION, INTENT(OUT):: YCONV(*)
      DOUBLE PRECISION, INTENT(OUT)         :: SHPT(NDP,NPTFR)
      INTEGER, INTENT(OUT)         :: ELT(*)
      DOUBLE PRECISION, INTENT(IN) :: X(*)
      DOUBLE PRECISION, INTENT(IN) :: Y(*)
      INTEGER, INTENT(IN)          :: ITRAV2(*)
      DOUBLE PRECISION, INTENT(IN) :: SHP(NDP,*)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER :: IFR,IPT
!  
!-----------------------------------------------------------------------   
!
      DO IFR=1,NPT
        IPT=NBOR(LISPFR(IFR))
        XCONV(IFR) = X(IPT)
        YCONV(IFR) = Y(IPT)         
        ELT(IFR)=ITRAV2(IPT)
        SHPT(1,IFR)=SHP(1,IPT)
        SHPT(2,IFR)=SHP(2,IPT)
        SHPT(3,IFR)=SHP(3,IPT)
      ENDDO
!  
!-----------------------------------------------------------------------   
!
      RETURN         
      END 
     
