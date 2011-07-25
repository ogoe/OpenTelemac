!                    *****************
                     SUBROUTINE DRSURR
!                    *****************
!
     & (DELTAR, TA, BETAC,T0AC,RHO,RHO0,RHOS,DENLAW,SEDI,NTRAC,
     &  IND_T,IND_S)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    COMPUTES DELTAR = (RHO-RHO0)/RHO0.
!
!note     DENLAW =0,1,2,3 (NOTHING, DEPENDS ON TEMP., SALINITY, OR BOTH).
!
!warning  ASSUMES THAT RHO0 IS GIVEN AT T=15degC AND S=0 MG/L
!+            AND THAT BETA(S)=-1E-3 (L/MG) AT T=15degC
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
!+
!
!history  JMH   AG - LNHE
!+        **/11/2000
!+        V5P2
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
!| BETAC          |-->| -(1/RHO)*(DRHO/DT) FOR TRACERS WHEN CONSTANT
!| DELTAR         |<->| (RHO-RHO0)/RHO0
!| DENLAW         |-->| CHOICE OF DENSITY LAW (SEE ABOVE)
!| IND_S          |-->| INDEX FOR SALINITY
!| IND_T          |-->| INDEX FOR TEMPERATURE
!| NTRAC          |-->| NUMBER OF ACTIVE TRACERS
!| RHO            |<->| WATER DENSITY
!| RHO0           |-->| WATER DENSITY AT REFERENCE CONCENTRATION
!| RHOS           |-->| SEDIMENT DENSITY
!| SEDI           |-->| IF YES, THERE IS SEDIMENT
!| T0AC           |-->| REFERENCE CONCENTRATION OF TRACERS
!| TA             |-->| TRACERS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NTRAC, DENLAW,IND_T,IND_S
      DOUBLE PRECISION, INTENT(IN)  :: RHO0,RHOS
      DOUBLE PRECISION, INTENT(IN)  :: BETAC(NTRAC), T0AC(NTRAC)
      TYPE(BIEF_OBJ), INTENT(INOUT) :: DELTAR
      TYPE(BIEF_OBJ), INTENT(IN)    :: TA
      TYPE(BIEF_OBJ), INTENT(INOUT) :: RHO
      LOGICAL, INTENT(IN)           :: SEDI
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ITRAC,NTRACM1
!
!***********************************************************************
!
      IF(DENLAW.GE.1.AND.DENLAW.LE.3) THEN
!
        IF(DENLAW.EQ.1) THEN
!
!        LAW ACCORDING TO TEMPERATURE
!        RHO = RHO0(1-(7(T-T0)**2)*1.E-6)
!                                               -3
!        WITH T0=4degC   AND   RHO0=999.972 KG.M
!
!        NOTE: ONLY THE GRADIENT OF DELTAR APPEARS IN EQUATIONS
!
         CALL OS( 'X=Y+C   ',X=RHO,Y=TA%ADR(IND_T)%P,C=-4.D0)
         CALL OS( 'X=XY    ',X=RHO,Y=RHO)
         CALL OS( 'X=CX    ',X=RHO,C=7.D-6 )
         CALL OS( 'X=X+C   ',X=RHO,C=-1.D0 )
         CALL OS( 'X=CX    ',X=RHO,C=-999.972D0)
!
        ELSEIF(DENLAW.EQ.2) THEN
!
!        LAW ACCORDING TO SALINITY S
!        RHO = RHO0(1+750S*1.E-6)
!
!                              -3
!        WITH RHO0=999.972 KG.M
!
         CALL OS( 'X=CY    ',X=RHO,Y=TA%ADR(IND_S)%P,C=750.D-6)
         CALL OS( 'X=X+C   ',X=RHO,C=1.D0)
         CALL OS( 'X=CX    ',X=RHO,C=999.972D0)
!
        ELSEIF(DENLAW.EQ.3) THEN
!
!        LAW ACCORDING TO BOTH TEMPERATURE AND SALINITY
!        RHO = RHO0(1-(7(T-T0)**2-750S)*1.E-6)
!                                               -3
!        WITH T0=4degC   AND   RHO0=999.972 KG.M
!
         CALL OS( 'X=Y+C   ',X=RHO,Y=TA%ADR(IND_T)%P,C=-4.D0  )
         CALL OS( 'X=XY    ',X=RHO,Y=RHO)
         CALL OS( 'X=CX    ',X=RHO,C=7.D-6 )
         CALL OS( 'X=X+CY  ',X=RHO,Y=TA%ADR(IND_S)%P,C=-750.D-6)
         CALL OS( 'X=X+C   ',X=RHO,C=-1.D0)
         CALL OS( 'X=CX    ',X=RHO,C=-999.972D0 )
!
        ENDIF
!
!       COMPUTES DRHO/DRO  = (RHO - RHO0)/RHO0
!       THE VALUE OF RHO0 GIVEN BY THE USER IS TAKEN HERE, IT TAKES INTO
!       ACCOUNT AN AVERAGE TEMPERATURE OR SALINITY IN THE DOMAIN, FOR A
!       BETTER BOUSSINESQ APPROXIMATION
!
        CALL OS( 'X=Y+C   ', X=DELTAR , Y=RHO , C=-RHO0 )
        CALL OS( 'X=CX    ', X=DELTAR , C=1.D0/RHO0 )
!
      ELSEIF(DENLAW.EQ.4) THEN
!
!       COMPUTES DELTAR WITH COEFFICIENTS BETAC GIVEN BY THE USER
!
!       BEWARE : BETA = - (1/RHO0)*(RHO-RHO0)/(TA-T0AC)
!                HENCE - SIGN IN SECOND CALL TO OS
!                BECAUSE DELTAR = (RHO-RHO0)/RHO0
!
!       SEDIMENT (TRACER NUMBER NTRAC) IS NATURALLY INCLUDED
!       IN THIS LOOP
!
        CALL OS( 'X=0     ' , X=DELTAR )
!
        IF(SEDI) THEN
          NTRACM1=NTRAC-1
        ELSE
          NTRACM1=NTRAC
        ENDIF
!
        IF(NTRACM1.GT.0) THEN
          DO ITRAC = 1,NTRACM1
            CALL OS('X=X+CY  ',DELTAR,TA%ADR(ITRAC)%P,
     &                              TA%ADR(ITRAC)%P, -BETAC(ITRAC) )
            CALL OS('X=X+C   ',DELTAR,DELTAR,DELTAR,
     &                              T0AC(ITRAC)*BETAC(ITRAC))
          ENDDO
        ENDIF
!
      ELSEIF(DENLAW.EQ.0) THEN
!
        CALL OS('X=0     ',X=DELTAR)
!
      ELSE
!
        IF(LNG.EQ.1) WRITE(LU,*) 'LOI DE DENSITE INCONNUE DANS DRSURR :'
        IF(LNG.EQ.2) WRITE(LU,*) 'WRONG DENSITY LAW IN DRSURR'
        CALL PLANTE(1)
        STOP
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     EFFECT OF SEDIMENT IS ALWAYS ADDED
!
!     SEDIMENT (SEDIMENT MUST BE THE LAST TRACER, HENCE NUMBER NTRAC)
!     ADDS UP THE SEDIMENT EFFECT
!
      IF(SEDI) THEN
!
        CALL OS('X=X+CY  ',X=DELTAR,Y=TA%ADR(NTRAC)%P,
     &                       C=(RHOS-RHO0)/(RHO0*RHOS))
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
