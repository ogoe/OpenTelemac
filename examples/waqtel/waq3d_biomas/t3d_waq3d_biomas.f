!                    *****************
                     SUBROUTINE CONDIN
!                    *****************
!
!
!***********************************************************************
! TELEMAC2D   V6P0                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES THE PHYSICAL PARAMETERS H, U, V ETC.
!
!history  J-M HERVOUET (LNHE)
!+        30/08/2007
!+        V6P0
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
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER I
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ITRAC
!
!-----------------------------------------------------------------------
!
!   INITIALISES THE TIME
!
      AT = 0.D0
!
!-----------------------------------------------------------------------
!
!   INITIALISES THE VELOCITIES: ZERO VELOCITIES
!
      CALL OS( 'X=0     ' , X=U )
      CALL OS( 'X=0     ' , X=V )
!
!-----------------------------------------------------------------------
!
!   INITIALISES THE WATER DEPTH H
!
      IF(CDTINI(1:10).EQ.'COTE NULLE'.OR.
     &   CDTINI(1:14).EQ.'ZERO ELEVATION') THEN
        CALL OS( 'X=0     ' , X=H )
        CALL OS( 'X=X-Y   ' , X=H , Y=ZF )
      ELSEIF(CDTINI(1:14).EQ.'COTE CONSTANTE'.OR.
     &       CDTINI(1:18).EQ.'CONSTANT ELEVATION') THEN
        CALL OS( 'X=C     ' , H , H  , H , COTINI )
        CALL OS( 'X=X-Y   ' , H , ZF , H , 0.D0   )
      ELSEIF(CDTINI(1:13).EQ.'HAUTEUR NULLE'.OR.
     &       CDTINI(1:10).EQ.'ZERO DEPTH') THEN
        CALL OS( 'X=C     ' , H , H  , H , 0.D0  )
      ELSEIF(CDTINI(1:17).EQ.'HAUTEUR CONSTANTE'.OR.
     &       CDTINI(1:14).EQ.'CONSTANT DEPTH') THEN
        CALL OS( 'X=C     ' , H , H  , H , HAUTIN )
      ELSEIF(CDTINI(1:13).EQ.'PARTICULIERES'.OR.
     &       CDTINI(1:10).EQ.'PARTICULAR'.OR.
     &       CDTINI(1:07).EQ.'SPECIAL') THEN
!  TO BE MODIFIED BY USER
!  MODIFIED BY RA
!  FOR PRECSCRIBING A CONSTANT INITIAL DEPTH AND VELOCITY
        CALL OS( 'X=C     ' , H , H  , H , 2.D0 )
        CALL OS( 'X=0     ' , X=U )
        CALL OS( 'X=0     ' , X=V )
!  END OF CODE TO BE MODIFIED BY USER
      ELSE
        IF(LNG.EQ.1) THEN
        WRITE(LU,*) 'CONDIN : CONDITION INITIALE NON PREVUE : ',CDTINI
        ENDIF
        IF(LNG.EQ.2) THEN
        WRITE(LU,*) 'CONDIN: INITIAL CONDITION UNKNOWN: ',CDTINI
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!   INITIALISES THE TRACERS
!
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
            CALL OS( 'X=C     ' ,X=T%ADR(ITRAC)%P,C=TRAC0(ITRAC))
        ENDDO
      ENDIF


!
!-----------------------------------------------------------------------
!
! INITIALISES THE VISCOSITY
!
      CALL OS( 'X=C     ' , X=VISC , C=PROPNU )
!
!-----------------------------------------------------------------------
!
      RETURN
      END
!                   **********************
                     SUBROUTINE NUTEFF
!                    **********************
!
     &(LNUT,TRR,NPOIN,IPO4,INO3,KP,KN)
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    COMPUTES LNUT: EFFECTS OF PHOSPHORIOUS AND NITROGENIOUS 
!           NUTRIMENTS ON ALGAE GROWTH 
!                              
!
!history  R. ATA (LNHE)
!+        02/09/2015
!+        V7P1
!+
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| INO3           |-->| INDEX OF NO3 IN TRR
!| IPO4           |-->| INDEX OF PO4 IN TRR
!| KN             |-->| CONSTANT OF SEMI-SATURATION WITH PHOSPHATE 
!| KP             |-->| CONSTANT OF HALF-SATURATION WITH NITROGEN
!| LNUT           |<--| NUTRIMENTS EFFECT ON ALGAE GROWTH
!| NPOIN          |-->| TOTAL NUMBER OF MESH NODES 
!| TRR            |-->| TRACER (CAN BE PHY: PHYTOPLAKTONIC BIOMASS)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_WAQTEL, EX_NUTEFF => NUTEFF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER         , INTENT(IN)    :: NPOIN,IPO4,INO3
      DOUBLE PRECISION, INTENT(IN)    :: KN,KP
      DOUBLE PRECISION, INTENT(INOUT) :: LNUT(NPOIN)
      TYPE(BIEF_OBJ)  , INTENT(IN)    :: TRR
!     LOCAL VARIABLES 
      INTEGER                    :: KK
      INTRINSIC MIN
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!     
!
      DO KK=1,NPOIN
!     for this example lnut is considered constant      
!        LNUT(KK)= MIN(TRR%ADR(IPO4)%P%R(KK)/(KP+TRR%ADR(IPO4)%P%R(KK)),
!     &                TRR%ADR(INO3)%P%R(KK)/(KN+TRR%ADR(INO3)%P%R(KK)))
          LNUT(KK)=1.D0
      ENDDO
!
      RETURN
      END
!
!-----------------------------------------------------------------------
!


