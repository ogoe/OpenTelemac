!                    ************************
                     SUBROUTINE COST_FUNCTION
!                    ************************
!
     &(JCOUT,OPTION,MODE)
!
!***********************************************************************
! TELEMAC2D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    PARTIALLY COMPUTES (ONE TIMESTEP) THE COST FUNCTION.
!
!history  E. BARROS
!+        25/06/1993
!+
!+
!
!history  A. LEOPARDI (UNINA)
!+        02/10/2000
!+        V5P2
!+   UPGRADE
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
!| JCOUT          |<->| COST FUNCTION
!| MODE           |-->| FCT: COST FUNCTION
!|                |   | GRD: GRADIENT COST FUNCTION
!|                |   | RHS: RIGHT HAND SIDE
!| OPTION         |-->| 1: COST FUNCTION COMPUTED WITH DEPTH
!|                |   | 2: COST FUNCTION COMPUTED WITH CELERITY
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D, EX_COST_FUNCTION => COST_FUNCTION
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION , INTENT(INOUT) :: JCOUT
      INTEGER , INTENT(IN)             :: OPTION
      CHARACTER(LEN=3) , INTENT(IN)    :: MODE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
!
      INTEGER I,J
      DOUBLE PRECISION C
!
!=======================================================================
!
      IF(MODE.EQ.'FCT') THEN
!
!       HERE U,V AND H ARE GIVEN BY A CALL TO LITENR
!
        IF(OPTION.EQ.1) THEN
!
          DO I=1,NPOIN
!
            JCOUT = JCOUT + ALPHA1%R(I) * (H%R(I)-HD%R(I))**2
     &                    + ALPHA2%R(I) * (U%R(I)-UD%R(I))**2
     &                    + ALPHA3%R(I) * (V%R(I)-VD%R(I))**2
!
          ENDDO
!
        ELSEIF(OPTION.EQ.2) THEN
!
          DO I=1,NPOIN
!
            JCOUT = JCOUT + ALPHA1%R(I) * GRAV *
     &              (SQRT(MAX(H%R(I),0.D0))-SQRT(MAX(HD%R(I),0.D0)))**2
     &                    + ALPHA2%R(I) * (U%R(I)-UD%R(I))**2
     &                    + ALPHA3%R(I) * (V%R(I)-VD%R(I))**2
!
          ENDDO
!
        ELSE
!
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'COST_FUNCTION : OPTION NON PREVUE : ',MODE
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'COST_FUNCTION: UNEXPECTED OPTION : ',MODE
          ENDIF
          CALL PLANTE(1)
          STOP
!
!       TEST ON OPTION
        ENDIF
!
!=======================================================================
!
      ELSEIF(MODE.EQ.'GRD') THEN
!
      IF(    INCLU2(ESTIME,'FROTTEMENT')
     &   .OR.INCLU2(ESTIME,'FRICTION'  )  ) THEN
!
!     IDENTIFICATION OF FRICTION
!
      IF (KFROT.EQ.3.OR.KFROT.EQ.2) THEN
!
      CALL SLOPES(TE3,ZF,MESH)
      CALL VECTOR(T1,'=','MASBAS          ',U%ELM,1.D0,
     &            T3,T3,T3,T3,T3,T3,MESH,.TRUE.,TE3)
!
      CALL FRICTI(T3,T4,T2,T2,UN,VN,HN,CF,MESH,T2,T5,VERTIC,UNSV2D,MSK,
     &            MASKEL,HFROT)
!
      CALL OS( 'X=XY    ' , T3 , T1 , T1 , C )
      CALL OS( 'X=XY    ' , T4 , T1 , T1 , C )
!
      CALL OS( 'X=CXYZ  ' , T3 , QQ , UU , -2.D0 )
      CALL OS( 'X=CXYZ  ' , T4 , RR , VV , -2.D0 )
!
      CALL OS( 'X=X+Y   ' , T3 , T4 , T4 , C )
      CALL OS( 'X=Y/Z   ' , T3 , T3 , CHESTR , C )
!
      ELSE
!
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'COST_FUNCTION : FROTTEMENT NON TRAITE : ',KFROT
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'COST_FUNCTION: UNEXPECTED FRICTION LAW: ',KFROT
        ENDIF
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
      ELSE
!
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'COST_FUNCTION : PARAMETRE NON PREVU :'
          WRITE(LU,*) ESTIME
          WRITE(LU,*) 'VERIFIER LE MOT CLEF : ESTIMATION DE PARAMETRES'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'COST_FUNCTION: UNEXPECTED PARAMETER :'
          WRITE(LU,*) ESTIME
          WRITE(LU,*) 'CHECK THE KEY-WORD: PARAMETER ESTIMATION'
        ENDIF
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
! COMPUTATION OF A GRADIENT FOR EVERY ZONE, AFTER BUILDING
! A GRADIENT VECTOR FOR EVERY POINT (IN T3)
!
      IF(NZONE.GT.0) THEN
!       IF ONE IDENTIFIES ONLY ONE PARAMETER S FOR A SET OF NODES
!       GRADIENT DJ/DS IS THE SUM OF THE GRADIENTS OF EACH
!       NODE OF THE SET
        DO J=1,NZONE
          DO I=1,NPOIN
            IF(ZONE%I(I).EQ.J) GRADJ%R(J)=GRADJ%R(J)+T3%R(I)
          ENDDO
        ENDDO
      ELSE
!       NOTE JMH: HERE IT IS SUPPOSED THAT NPARAM = NPOIN
        CALL OS('X=X+Y   ',GRADJ,T3,T3,0.D0)
      ENDIF
!
!=======================================================================
!
      ELSEIF(MODE.EQ.'RHS') THEN
!
!           IT    IT    IT
!  TERMS 2 W   ( X   - M   ) OR EQUIVALENT DEPENDING ON THE OPTION
!           IP    IP    IP
!
      IF(OPTION.EQ.1) THEN
!
        CALL OS( 'X=Y-Z   ', CV1 , HH , HD , C )
        CALL OS( 'X=Y-Z   ', CV2 , UU , UD , C )
        CALL OS( 'X=Y-Z   ', CV3 , VV , VD , C )
!
        CALL OS( 'X=CXY   ', CV1 , ALPHA1 , ALPHA1 , 2.D0 )
        CALL OS( 'X=CXY   ', CV2 , ALPHA2 , ALPHA2 , 2.D0 )
        CALL OS( 'X=CXY   ', CV3 , ALPHA3 , ALPHA3 , 2.D0 )
!
      ELSEIF(OPTION.EQ.2) THEN
!
!       HERE COST FUNCTION COMPUTED WITH CELERITY INSTEAD OF DEPTH
        CALL OS( 'X=SQR(Y)', T1  , HH , T1 , C   )
        CALL OS( 'X=SQR(Y)', T2  , HD , T2 , C   )
        CALL OS( 'X=Y-Z   ', T3  , T1 , T2 , C )
        CALL OS( 'X=Y/Z   ', CV1 , T3 , T1 , C )
        CALL OS( 'X=Y-Z   ', CV2 , UU , UD , C )
        CALL OS( 'X=Y-Z   ', CV3 , VV , VD , C )
!
        CALL OS( 'X=CXY   ', CV1 , ALPHA1 , ALPHA1 , GRAV )
        CALL OS( 'X=CXY   ', CV2 , ALPHA2 , ALPHA2 , 2.D0 )
        CALL OS( 'X=CXY   ', CV3 , ALPHA3 , ALPHA3 , 2.D0 )
!
      ENDIF
!
!=======================================================================
!
      ELSE
!
!=======================================================================
!
        IF(LNG.EQ.1) WRITE(LU,*) 'COST_FUNCTION : MODE NON PREVU : ',
     &                           MODE
        IF(LNG.EQ.2) WRITE(LU,*) 'COST_FUNCTION: UNEXPECTED MODE : ',
     &                           MODE
        CALL PLANTE(1)
        STOP
!
!=======================================================================
!
!     TEST ON MODE
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
