!                    *****************
                     SUBROUTINE TRIDW2
!                    *****************
!
     &(WSS)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES AN AVERAGED VALUE OF H * WSTAR IN A WAY
!+                COMPATIBLE WITH THE PSI SCHEME.
!code
!+      IN WSS (WHICH IS WSCONV) WE FIRST PUT (LOOP 1) :
!+
!+      STARTING FROM THE FIRST PLANE (BOTTOM) :
!+
!+      (H WSTAR) LEVEL 3/2 - (H WSTAR) LEVEL 1/2     (THE LATTER=0)
!+      (H WSTAR) LEVEL 5/2 - (H WSTAR) LEVEL 3/2
!+      ......
!+
!+      (H WSTAR) LEVEL NPLAN - 1/2   -  (H WSTAR) LEVEL NPLAN - 3/2
!+
!+      THE FOLLOWING IS NOT SOLVED
!+      (WOULD ONLY GIVE (H WSTAR) LEVEL NPLAN + 1/2 = 0
!+
!+      (H WSTAR) LEVEL NPLAN + 1/2   -  (H WSTAR) LEVEL NPLAN - 1/2
!+
!+
!+      THEN BY SUCCESSIVE SUMS WE GET IN WSS (LOOP 2):
!+
!+      (H WSTAR) LEVEL 3/2
!+      (H WSTAR) LEVEL 5/2
!+      ......
!+
!+      (H WSTAR) LEVEL NPLAN - 1/2
!
!reference  PHD OF J-M HERVOUET, EQUATION 5.58
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history
!+        25/10/2004
!+
!+   DONE WITH MASS-LUMPING, SEE ALSO MT14PP FOR COMPATIBILITY
!
!history  JMH
!+        14/11/2008
!+
!+   CALLS TO OV REPLACED BY LOOPS
!
!history  J-M HERVOUET  (LNHE)
!+        04/12/09
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
!| WSS            |<->| VERTICAL VELOCITY COMPONENT WSTAR
!|                |---| IN TRANSFORMED MESH AT TIME N+1
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      TYPE(BIEF_OBJ), INTENT(INOUT) :: WSS
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IETAGE, I,IAD1,IAD2,IAD3,IS
!
      DOUBLE PRECISION :: SURDT
!
!***********************************************************************
!
!   SOLVES THE LINEAR SYSTEM
!
!=======================================================================
!
      SURDT=1.D0/DT
!
!-----------------------------------------------------------------------
!
!     LOOP 1
!
!     WSCONV OF LAST LEVEL WILL BE 0 (CHECKED,IT WORKS !)
!     BECAUSE SUM ON THE VERTICAL=2D CONTINUITY EQUATION
!     HENCE LAST LEVEL NOT SOLVED, SO LOOP UP TO NETAGE
!     A CONSEQUENCE IS THAT RAIN AND EVAPORATION IS NOT SEEN HERE
!
      IAD1=0
      IAD3=0
      DO IETAGE = 1,NETAGE
!
         DO I=1,NPOIN2
           IAD1=IAD1+1
           SEM2D%ADR(1)%P%R(I) = FLUINT%R(IAD1)-FLUEXT%R(IAD1)
     &                          +SURDT*(VOLUN%R(IAD1)-VOLU%R(IAD1))
         ENDDO
!
!        PARALLELISM
!
         IF(NCSIZE.GT.1) CALL PARCOM(SEM2D%ADR(1)%P,2,MESH2D)
!
!        WITH SOURCES (DONE AFTER CALL PARCOM BECAUSE
!        CALL PARCOM ON SOURCES IS ALREADY DONE IN SOURCES_SINKS)
!
         IF(NSCE.GT.0) THEN
!          WITH SOURCES
           DO IS=1,NSCE
           DO I=1,NPOIN2
             IAD2=(IETAGE-1)*NPOIN2+I
             SEM2D%ADR(1)%P%R(I)
     &      =SEM2D%ADR(1)%P%R(I)+SOURCES%ADR(IS)%P%R(IAD2)
           ENDDO
           ENDDO
         ENDIF
!
!        SOLVES THE SYSTEM (JUST A DIVISION BY DIAGONAL)
!
         DO I=1,NPOIN2
           IAD3=IAD3+1
           WSS%R(IAD3)=SEM2D%ADR(1)%P%R(I)*UNSV2D%R(I)
         ENDDO
!
      ENDDO
!
!-----------------------------------------------------------------------
!
!     LOOP 2
!
      IF(NETAGE.GT.1) THEN
        IAD1=0
        IAD2=NPOIN2
        DO IETAGE = 2,NETAGE
          DO I=1,NPOIN2
            IAD1=IAD1+1
            IAD2=IAD2+1
            WSS%R(IAD2)=WSS%R(IAD2)+WSS%R(IAD1)
          ENDDO
        ENDDO
      ENDIF
!
!  LAST LEVEL : WSCONV = 0 (JMH : NOT USEFUL, BECAUSE INITIALISED AT 0,
!                           AT THE BEGINNING OF TELEMAC3D.F
!                           HOWEVER WSCONV IS MODIFIED AFTER BECAUSE WSTAR
!                           IS COPIED INTO IT, BUT IN FACT SET TO 0. ALSO)
!
!     CALL OV ('X=C     ',WSS%R((NPLAN-1)*NPOIN2+1:NPLAN*NPOIN2),
!    &                    WSS%R((NPLAN-1)*NPOIN2+1:NPLAN*NPOIN2),
!    &                    WSS%R((NPLAN-1)*NPOIN2+1:NPLAN*NPOIN2),
!    &                    0.D0, NPOIN2 )
!
!=======================================================================
!
! DIRICHLET CONDITIONS ON LATERAL BOUNDARIES
!     WSS = WSBORL
!
!=======================================================================
! FORTRAN77:
!
! THE WAY IN WHICH THE EQUATIONS ARE TREATED DOES NOT ALLOW TAKING INTO
! ACCOUNT SUCH CONDITIONS (OR MASS ERRORS)
!
!     DO 60 IPTFR = 1,NPTFR
!        IPOIN2 = NBOR(IPTFR)
!        DO 70 IETAGE = 1,NETAGE
!           C = 0.D0
!           IF (LIWBOL(IPTFR,IETAGE).EQ.KENT.OR.
!    *          LIWBOL(IPTFR,IETAGE).EQ.KADH)
!    *          C = 0.5D0*(WSBORL(IPTFR,IETAGE  )-WS(IPOIN2,IETAGE))
!           IF (LIWBOL(IPTFR,IETAGE).EQ.KENT.OR.
!    *          LIWBOL(IPTFR,IETAGE).EQ.KADH)
!    *          C = 0.5D0*(WSBORL(IPTFR,IETAGE+1)-WS(IPOIN2,IETAGE)) + C
!           WS(IPOIN2,IETAGE) = WS(IPOIN2,IETAGE) + C
!70      CONTINUE
!60   CONTINUE
!
!     PRINT*,'WSS=',DOTS(WSS,WSS)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
