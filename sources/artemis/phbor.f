!                    ****************
                     SUBROUTINE PHBOR
!                    ****************
!
!
!***********************************************************************
! ARTEMIS   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    TRANSLATES THE BOUNDARY CONDITIONS SPECIFIED
!+                BY THE USER,
!+                I.E. COMPUTES THE COEFFICIENTS
!+                APHIR, APHII, ... FOR EACH BOUNDARY SEGMENT.
!
!history  J-M HERVOUET (LNH)
!+
!+
!+   LINKED TO BIEF 5.0
!
!history  D. AELBRECHT (LNH)
!+        21/08/2000
!+        V5P1
!+
!
!history  C. DENIS (SINETICS)
!+        18/03/2010
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
!history  C.PEYRARD (LNHE)
!+        01/06/2012
!+        V6P0
!+   KSORT   : end of application to neighbours
!+   KINC    : end of applicatin to the I+1 node
!+   SEGMENT : If a segment links a solid node to a liquid node, 
!+             this segment is regarded as solid. 
!+   BUG correction         : Full KINC boundaries taken into account
!+   NEW boundary condition : incident potential can be given by the user 
!+   Parallel correction    : - End of HBT,CGT,CTT,KT,XT,YT tables
!+                              only   HB%R,CG%R, etc... are used.
!+                            - No use of NCSIZE variable. 
!history  C.PEYRARD (LNHE)
!+        18/03/2014
!+        V7P0
!+   KSORT   : application to neighbours (best for automatic angles)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_ARTEMIS
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
      INTEGER I,IG,IG0,IGP1
!
! 
      DOUBLE PRECISION GRE, GIM
      DOUBLE PRECISION DDXGRE,DDYGRE,DDXGIM,DDYGIM
!       

      DOUBLE PRECISION PI,DEGRAD,PHASEOI,X0,Y0
      DOUBLE PRECISION AUXI1,AUXIC,AUXIS,RADDEG
!     DOUBLE PRECSION AUXI2
      INTRINSIC COS,SIN
!
!-----------------------------------------------------------------------
!
      PARAMETER( PI = 3.1415926535897932384626433D0 , DEGRAD=PI/180.D0 )
      PARAMETER( RADDEG = 180.D0 / PI )
!
!-----------------------------------------------------------------------
!
! INITIALISES LIDIR TO KSORT (A DIFFERENT VALUE FROM KENT)
! IN ORDER NOT TO TAKE NODES IMPOSED IN PRIDIH INTO ACCOUNT,
! WHEN IT HAS NOT BEEN REQUESTED.
!
      DO I=1,NPTFR
         LIDIR%I(I) = KSORT
!        BEWARE: IT IS ASSUMED HERE THAT NPTFRX=NPTFR
         LIDIR%I(I+NPTFR) = KSORT
         IF (LIHBOR%I(I).EQ.KENT) THEN
           LIHBOR%I(I) = KINC
         ENDIF
         APHI1B%R(I) = 0.D0
         BPHI1B%R(I) = 0.D0
         CPHI1B%R(I) = 0.D0
         DPHI1B%R(I) = 0.D0
         APHI2B%R(I) = 0.D0
         BPHI2B%R(I) = 0.D0
         CPHI2B%R(I) = 0.D0
         DPHI2B%R(I) = 0.D0
         APHI3B%R(I) = 0.D0
         BPHI3B%R(I) = 0.D0
         CPHI3B%R(I) = 0.D0
         DPHI3B%R(I) = 0.D0
         APHI4B%R(I) = 0.D0
         BPHI4B%R(I) = 0.D0
         CPHI4B%R(I) = 0.D0
         DPHI4B%R(I) = 0.D0
         CGRX1B%R(I) = 0.D0
         CGRY1B%R(I) = 0.D0
         DGRX1B%R(I) = 0.D0
         DGRY1B%R(I) = 0.D0

      ENDDO
!
!-----------------------------------------------------------------------
!     INITIALISATION OF PHASE VARIABLES FOR AUTOMATIC CALCULATION
      X0      = X_PHREF
      Y0      = Y_PHREF
      PHASEOI = 0D0
!
      DO I=1,NPTFR
!
!        ********************************
!        GLOBAL NUMBER OF THE BOUNDARY NODE I
!        ********************************
!
         IG   = MESH%NBOR%I(I)
!
!        ******************************************
!        GLOBAL NUMBER OF THE BOUNDARY NODE PRECEDING I
!        ******************************************
!
         IG0  = MESH%NBOR%I(MESH%KP1BOR%I(I+NPTFR))
!
!        ****************************************
!        GLOBAL NUMBER OF THE BOUNDARY NODE FOLLOWING I
!        ****************************************
!
         IGP1 = MESH%NBOR%I(MESH%KP1BOR%I(I))

!        -------------------------------------------------
!              COEFFICIENTS FOR A INCIDENT WAVE
!        -------------------------------------------------
!
         IF (LIHBOR%I(I).EQ.KINC) THEN
!          WRITE(LU,*) 'ENTREE TETAP, I =',TETAP%R(I),I
          AUXIC      = COS(TETAB%R(I)*DEGRAD)
          AUXIS      = SIN(TETAB%R(I)*DEGRAD)
          AUXI1      = GRAV/OMEGA * HB%R(I)/2.D0 

!---------- AUTOMATIC PHASE CALCULATION (TETAB HAVE TO BE UNIFORM ON THE BOUNDARY)
          IF (LPHASEAUTO) THEN
           PHASEOI = KPHREF*AUXIC*(X(IG)-X0)+KPHREF*AUXIS*(Y(IG)-Y0)
          ENDIF
!---------- INCIDENT WAVE --> INCIDENT POTENTIAL  (REAL, IMAGINAR)
          GRE= AUXI1*SIN( ALFAP%R(I)*DEGRAD + PHASEOI)
          GIM=-AUXI1*COS( ALFAP%R(I)*DEGRAD + PHASEOI)
! --------- INCIDENT WAVE --> GRADIENTS           (REAL, IMAGINAR)
          DDXGRE= AUXI1*COS(ALFAP%R(I)*DEGRAD+PHASEOI)*AUXIC*K%R(IG)
          DDYGRE= AUXI1*COS(ALFAP%R(I)*DEGRAD+PHASEOI)*AUXIS*K%R(IG)
          DDXGIM= AUXI1*SIN(ALFAP%R(I)*DEGRAD+PHASEOI)*AUXIC*K%R(IG)
          DDYGIM= AUXI1*SIN(ALFAP%R(I)*DEGRAD+PHASEOI)*AUXIS*K%R(IG)

! --------- COEFFICIENTS
! -- MATRIX AM AND BM COEFFICIENTS
          APHI1B%R(I) = - K%R(IG) * C%R(IG) * CG%R(IG)
     &                 * COS(TETAP%R(I)*DEGRAD)
          BPHI1B%R(I) = 0.D0

! -- SECOND MEMBER CV1 AND CV2 COEFFICIENTS
! ----  i * K * Gamma (multiplied by "- cos THETAP" in BERKHO)
          CPHI1B%R(I)  = ( -GIM*K%R(IG) ) *C%R(IG)*CG%R(IG)
          DPHI1B%R(I)  = (  GRE*K%R(IG) ) *C%R(IG)*CG%R(IG)

! ---- GRAD(Gamma) will be used in BERKHO...
          CGRX1B%R(I)=   ( DDXGRE ) *C%R(IG)*CG%R(IG)
          CGRY1B%R(I)=   ( DDYGRE ) *C%R(IG)*CG%R(IG)
          DGRX1B%R(I)=   ( DDXGIM ) *C%R(IG)*CG%R(IG) 
          DGRY1B%R(I)=   ( DDYGIM ) *C%R(IG)*CG%R(IG)
         ENDIF

!
!        -------------------------------------------------
!             COEFFICIENTS FOR AN INCIDENT POTENTIAL
!        -------------------------------------------------
!
         IF (LIHBOR%I(I).EQ.KPOT) THEN
!          WRITE(LU,*) 'ENTREE TETAP, I =',TETAP%R(I),I
!------------ POTENTIAL (REAL, IMAGINAR)            
          GRE=PRB%R(I)
          GIM=PIB%R(I)
! ----------- GRADIENTS (REAL, IMAGINAR)
          DDXGRE= DDXPRB%R(I)
          DDYGRE= DDYPRB%R(I)
          DDXGIM= DDXPIB%R(I)
          DDYGIM= DDYPIB%R(I)
! ----------- COEFFICIENTS
! -- MATRIX AM AND BM COEFFICIENTS
          APHI1B%R(I) = - K%R(IG) * C%R(IG) * CG%R(IG)
     &                 * COS(TETAP%R(I)*DEGRAD)
          BPHI1B%R(I) = 0.D0
! -- SECOND MEMBER CV1 AND CV2 COEFFICIENTS
! ----  i * K * Gamma (multiplied by "- cos THETAP" in BERKHO)
          CPHI1B%R(I)  = ( -GIM*K%R(IG) ) *C%R(IG)*CG%R(IG)
          DPHI1B%R(I)  = (  GRE*K%R(IG) ) *C%R(IG)*CG%R(IG)
! ---- GRAD(Gamma) will be used in BERKHO...
          CGRX1B%R(I)=   ( DDXGRE ) *C%R(IG)*CG%R(IG)
          CGRY1B%R(I)=   ( DDYGRE ) *C%R(IG)*CG%R(IG)
          DGRX1B%R(I)=   ( DDXGIM ) *C%R(IG)*CG%R(IG) 
          DGRY1B%R(I)=   ( DDYGIM ) *C%R(IG)*CG%R(IG)
         ENDIF
!        -------------------------------------------------
!        COEFFICIENTS FOR A FREE EXIT BOUNDARY SEGMENT
!        -------------------------------------------------
!
         IF (LIHBOR%I(I).EQ.KSORT) THEN
!          WRITE(LU,*) 'SORTIE TETAP, I =',TETAP%R(I),I
            APHI2B%R(I)  = - K%R(IG) * C%R(IG) * CG%R(IG)
     &                   * COS(TETAP%R(I)*DEGRAD)
!
            BPHI2B%R(I)  = 0.D0
!
            CPHI2B%R(I)  = 0.D0
!
            DPHI2B%R(I)  = 0.D0
!
        ELSEIF (LIHBOR%I(MESH%KP1BOR%I(I)).EQ.KSORT) THEN
            APHI2B%R(I)  = - K%R(IG) * C%R(IG) * CG%R(IG)
     &                   * COS(TETAP%R(MESH%KP1BOR%I(I))*DEGRAD)

            BPHI2B%R(I)  = 0.D0

            CPHI2B%R(I)  = 0.D0

            DPHI2B%R(I)  = 0.D0
!
        ELSEIF (LIHBOR%I(MESH%KP1BOR%I(I+NPTFR)).EQ.KSORT) THEN
            APHI2B%R(I)  = - K%R(IG) * C%R(IG) * CG%R(IG)
     &                  * COS(TETAP%R(MESH%KP1BOR%I(I+NPTFR))*DEGRAD)

            BPHI2B%R(I)  = 0.D0

            CPHI2B%R(I)  = 0.D0

            DPHI2B%R(I)  = 0.D0
!
         ELSE
            APHI2B%R(I)  = 0.D0
!
            BPHI2B%R(I)  = 0.D0
!
            CPHI2B%R(I)  = 0.D0
!
            DPHI2B%R(I)  = 0.D0
!
         ENDIF
!
!        -------------------------------------------
!        COEFFICIENTS FOR A SOLID BOUNDARY SEGMENT
!        -------------------------------------------
!
         IF (LIHBOR%I(I).EQ.KLOG) THEN
!          WRITE(LU,*) 'TETAP, I =',TETAP%R(I),I,X(IG),Y(IG)
          AUXI1 = K%R(IG) * C%R(IG) * CG%R(IG) *
     &      COS(TETAP%R(I)*DEGRAD) /
     &      ( 1.D0 + RP%R(I)*RP%R(I) +
     &        2.D0*RP%R(I)*COS(ALFAP%R(I)*DEGRAD) )
!
          APHI3B%R(I) = - (1.D0 - RP%R(I) * RP%R(I) ) * AUXI1
!
          BPHI3B%R(I) = 2.D0*RP%R(I)*SIN(ALFAP%R(I)*DEGRAD) * AUXI1
!
          CPHI3B%R(I)  = 0.D0
!
          DPHI3B%R(I)  = 0.D0
!
         ELSEIF (LIHBOR%I(MESH%KP1BOR%I(I)).EQ.KLOG) THEN
          AUXI1 = K%R(IG) * C%R(IG) * CG%R(IG) *
     &      COS(TETAP%R(MESH%KP1BOR%I(I))*DEGRAD) /
     &      (1.D0 + RP%R(MESH%KP1BOR%I(I))*RP%R(MESH%KP1BOR%I(I))
     &      +2.D0 * RP%R(MESH%KP1BOR%I(I))*
     &       COS(ALFAP%R(MESH%KP1BOR%I(I))*DEGRAD))
!
          APHI3B%R(I) = - (1.D0-RP%R(MESH%KP1BOR%I(I))*
     &      RP%R(MESH%KP1BOR%I(I))) * AUXI1
!
          BPHI3B%R(I) = 2.D0*RP%R(MESH%KP1BOR%I(I))
     &                * SIN(ALFAP%R(MESH%KP1BOR%I(I))*DEGRAD) * AUXI1
!
          CPHI3B%R(I)  = 0.D0
!
          DPHI3B%R(I)  = 0.D0
!
         ELSEIF (LIHBOR%I(MESH%KP1BOR%I(I+NPTFR)).EQ.KLOG) THEN
          AUXI1 = K%R(IG) * C%R(IG) * CG%R(IG) *
     &     COS(TETAP%R(MESH%KP1BOR%I(I+NPTFR))*DEGRAD) /
     &     (1.D0 + RP%R(MESH%KP1BOR%I(I+NPTFR))
     &      *RP%R(MESH%KP1BOR%I(I+NPTFR))
     &      +2.D0 * RP%R(MESH%KP1BOR%I(I+NPTFR))*
     &       COS(ALFAP%R(MESH%KP1BOR%I(I+NPTFR))*DEGRAD))
!
          APHI3B%R(I) = - (1.D0-RP%R(MESH%KP1BOR%I(I+NPTFR))*
     &      RP%R(MESH%KP1BOR%I(I+NPTFR))) * AUXI1
!
          BPHI3B%R(I) = 2.D0*RP%R(MESH%KP1BOR%I(I+NPTFR))
     &      * SIN(ALFAP%R(MESH%KP1BOR%I(I+NPTFR))*DEGRAD) * AUXI1
!
          CPHI3B%R(I)  = 0.D0
!
          DPHI3B%R(I)  = 0.D0
!
         ELSE
          APHI3B%R(I)  = 0.D0
!
          BPHI3B%R(I)  = 0.D0
!
          CPHI3B%R(I)  = 0.D0
!
          DPHI3B%R(I)  = 0.D0
!
         ENDIF
!
!        -------------------------------------------------
!        COEFFICIENTS FOR AN IMPOSED WAVE BOUNDARY SEGMENT
!        -------------------------------------------------
!DA      -----------------------------------
!DA      KEPT FOR MEMORY !
!DA      -----------------------------------
!DA
!DA         IF (LIHBOR(I).EQ.KENT) THEN
!DA         AUXIC      = COS(TETAB(I)*DEGRAD)
!DA         AUXIS      = SIN(TETAB(I)*DEGRAD)
!DA         AUXI1      = GRAV/OMEGA * HB(I)/2.D0 * C(IG) * CG(IG) *
!DA     *                K(IG) * ( AUXIC *XSGBOR(I) +
!DA     *                          AUXIS *YSGBOR(I) )
!DA         AUXI2      = K(IG) * ( X(IG)*AUXIC +
!DA     *                          Y(IG)*AUXIS )
!DA
!DA         APHI4B(I)  = 0.D0
!DA
!DA         BPHI4B(I)  = 0.D0
!DA
!DA         CPHI4B(I)  = AUXI1 * COS( AUXI2 )
!DA
!DA         DPHI4B(I)  = AUXI1 * SIN( AUXI2 )
!DA
!DA       VALUES IMPOSED AT THE NODES OF A KENT SEGMENT
!DA         LIDIR(I)         = KENT
!DA
!DA         AUXI1 = GRAV/OMEGA * HB(I)/2.D0
!DA         AUXI2 = K(IG) * (X(IG)*AUXIC +
!DA     *                    Y(IG)*AUXIS )
!DA
!DA            PHIRB(I) =   AUXI1 * SIN( AUXI2 )
!DA            PHIIB(I) = - AUXI1 * COS( AUXI2 )
!DA         ENDIF
!
!
      ENDDO
!-----------------------------------------------------------------------
!
!
      RETURN
      END SUBROUTINE

