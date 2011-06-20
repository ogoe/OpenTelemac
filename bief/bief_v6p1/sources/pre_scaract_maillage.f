!                *******************************
                 SUBROUTINE PRE_SCARACT_MAILLAGE
!                *******************************             
                        
     & ( U , UTILD , UCONV , VCONV , WCONV , X , Y , ZSTAR ,
     &   XCONV , YCONV , ZCONV, Z , SHP , SHZ , SURDET ,
     &   DT , IKLE , IFABOR , ELT , ETA , ITRAV1, ITRAV2, IELM ,
     &   IELMU , NELEM , NELMAX , NOMB , NPOIN , NPOIN2 , NDP , NPLAN , 
     &   LV , MSK , MASKEL , MESH , FAC , TEST , STEST , INITLOC,QUAD )    
!     
!***********************************************************************
! BIEF   V6P1                                   25/03/2011
!***********************************************************************
!
!brief    PREPARES ADVECTION BY THE METHOD OF CHARACTERISTICS IN THE
!+                CASE OF POINTS LINKED TO A MESH
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
      USE BIEF    !, EX_PRE_SCARACT_MAILLAGE => PRE_SCARACT_MAILLAGE
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     
      INTEGER, INTENT(IN)             :: NELEM,NELMAX,NPOIN,NPOIN2
      INTEGER, INTENT(IN)             :: NOMB,NDP,NPLAN,IELM,IELMU,LV
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: U
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: UTILD
      DOUBLE PRECISION, INTENT(IN)    :: UCONV(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: VCONV(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: WCONV(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: X(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: Y(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: Z(NPOIN2,NPLAN),ZSTAR(NPLAN)
      DOUBLE PRECISION, INTENT(OUT)   :: ZCONV(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(OUT)   :: SHP(NDP,NPOIN),SHZ(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: MASKEL(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: SURDET(NELEM)
      DOUBLE PRECISION, INTENT(IN)    :: DT
      INTEGER, INTENT(IN)             :: IKLE(NELMAX,NDP)
      INTEGER, INTENT(IN)             :: IFABOR(NELMAX,*)
      INTEGER, INTENT(OUT)            :: ELT(NPOIN),ETA(NPOIN)
      INTEGER, INTENT(IN)             :: ITRAV1(NPOIN),ITRAV2(NPOIN)
      LOGICAL, INTENT(IN)             :: MSK,INITLOC
      DOUBLE PRECISION, INTENT(IN)    :: TEST(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: FAC(NPOIN)
      TYPE(BIEF_MESH) , INTENT(INOUT) :: MESH
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: STEST,XCONV,YCONV
      LOGICAL, INTENT(OUT) :: QUAD
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!     
      DOUBLE PRECISION                :: C
      INTEGER                         :: NPOINT,NPOINT2,I
!       
!-----------------------------------------------------------------------
!     
      QUAD=.FALSE.
      CALL OV( 'X=Y     ' , XCONV%R , X , Z , C , NPOIN )
      CALL OV( 'X=Y     ' , YCONV%R , Y , Z , C , NPOIN )
!
      IF(IELM==11) THEN
!     
!       IS THERE ANY QUADRATIC ELEMENT ?
!     
        IF(U%TYPE.EQ.4) THEN
          CALL CPSTVC(U%ADR(1)%P,STEST)      
          DO I=1,U%N
            IF(U%ADR(I)%P%ELM.EQ.13) THEN
              QUAD = .TRUE.
!             VERY IMPORTANT FOR: CALL PARCOM(STEST,2,MESH)
              CALL CPSTVC(U%ADR(I)%P,STEST)
            ENDIF
          ENDDO
        ELSEIF(U%TYPE.EQ.2) THEN
          CALL CPSTVC(U,STEST)      
          IF(U%ELM.EQ.13) QUAD = .TRUE.
        ELSE
          IF(LNG.EQ.1) WRITE(LU,17) U%TYPE,UTILD%TYPE
          IF(LNG.EQ.2) WRITE(LU,18) U%TYPE,UTILD%TYPE
          CALL PLANTE(1)
          STOP
        ENDIF
!     
!-----------------------------------------------------------------------
!     
        IF(.NOT.QUAD) THEN
!     
!         P1 TRIANGLES 
!         ============
!
          NPOINT=NPOIN
          NPOINT2=NPOIN
!     
!         FILLING SHP AND ELT
!
          IF(INITLOC) THEN  
            CALL GTSH11(SHP,ELT,IKLE,NPOINT,NELEM,NELMAX,MSK,MASKEL)
          ENDIF
! 		  
        ELSEIF(QUAD) THEN
!     
          CALL CHGDIS(XCONV,IELM,13,MESH)
          CALL CHGDIS(YCONV,IELM,13,MESH) 
!     
!         QUADRATIC TRIANGLES FOR ONE OF THE ADVECTED VARIABLES
!         =====================================================
!     
!         FILLING SHP AND ELT
!     
          NPOINT  = NPOIN+MESH%NSEG 
          NPOINT2 = NPOIN+MESH%NSEG 
!     
!         NOT IMPLEMENTED : QUADRATIC TRACER AND LINEAR VELOCITY
!     
          IF(IELMU.NE.13) THEN
            IF(LNG.EQ.1) WRITE(LU,21) 
            IF(LNG.EQ.2) WRITE(LU,22) 
            CALL PLANTE(1)
            STOP
          ENDIF
!      
          IF(INITLOC) THEN  		
            CALL GTSH13(SHP,ELT,IKLE,NPOINT2,NELEM,NELMAX,MSK,MASKEL)
          ENDIF
!
        ENDIF
!
      ELSEIF (IELM==41) THEN
!     
!       PRISMES DE TELEMAC-3D
!       =====================
!
        NPOINT  = NPOIN
        NPOINT2 = NPOIN2
!
        DO I=1,NPLAN
          CALL OV('X=C     ',ZCONV(1,I),Y,Z,ZSTAR(I),NPOIN2)
        ENDDO
!     
!       REMPLISSAGE DES SHP ET DES ELT OPTIMISE
!     
        IF(INITLOC) THEN  
          CALL GTSH41(WCONV,SHP,SHZ,ELT,ETA,IKLE,NPOINT2,
     &                NELEM,NPLAN,MSK,MASKEL)  
        ENDIF
!       
!-----------------------------------------------------------------------
!
      ELSE 
!
        IF(LNG.EQ.1) WRITE(LU,11) IELM
        IF(LNG.EQ.2) WRITE(LU,12) IELM
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
11    FORMAT(1X,'PRE_SCARACT_MAILLAGE::  TYPE D''ELEMENT INCONNU : ',I6)
12    FORMAT(1X,'PRE_SCARACT_MAILLAGE:: UNKNOWN TYPE OF ELEMENT : ',I6)
13    FORMAT(1X,'PRE_SCARACT_MAILLAGE::',
     &          ' (PARALLELE) REMONTEE INCOMPLETE POUR : ',I6)
14    FORMAT(1X,'PRE_SCARACT_MAILLAGE::',
     &          ' (PARALLEL) INCOMPLETE PATH LINE FOR : ',I6)
15    FORMAT(1X,'PRE_SCARACT_MAILLAGE::',
     &          ' MAUVAIS BLOC DES VARIABLES : ',2I6)
16    FORMAT(1X,'PRE_SCARACT_MAILLAGE::',
     &          ' WRONG BLOCK OF VARIABLES : ',2I6)
17    FORMAT(1X,'PRE_SCARACT_MAILLAGE:: TYPE D''OBJET INCONNU : ',2I6)
18    FORMAT(1X,'PRE_SCARACT_MAILLAGE:: UNKNOWN TYPE OF OBJECT : ',2I6)
19    FORMAT(1X,'SCARACT : PARALLELISME NON PREVU EN QUADRATIQUE')
20    FORMAT(1X,'SCARACT: PARALLELISM NOT TREATED WITH QUADRATIC')
21    FORMAT(1X,'SCARACT : VITESSES LINEAIRES ET TRACEUR QUADRATIQUE')
22    FORMAT(1X,'SCARACT: LINEAR VELOCITY AND QUADRATIC TRACER')
!
!-----------------------------------------------------------------------  
! 
      RETURN    
      END
