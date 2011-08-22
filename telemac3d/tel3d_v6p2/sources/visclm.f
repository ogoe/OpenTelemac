!                    *****************
                     SUBROUTINE VISCLM
!                    *****************
!
     & (VISCVI,VISCTA,RI,U,V,DELTAR,X,Y,Z,HN,TRAV1,TRAV2,TRAV3,
     &  TRAV4,TRAV5,TRAV6,TRAV7,SVIDE,MESH3D,IELM3,GRAV,
     &  NPLAN,NPOIN3,NPOIN2,NTRAC,MSK,MASKEL,TA,MIXING,
     &  DAMPING,IND_T,DNUVIV,DNUTAV,KARMAN,PRANDTL,UETCAR,KFROT,
     &  RUGOF,ZF)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    INITIALISES VISCOSITIES.
!
!history  C. VILLARET, P. TASSI, J.-M. HERVOUET
!+        02/02/2011
!+
!+
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/99
!+
!+   FORTRAN95 VERSION
!
!history  AG (LNHE)
!+        **/01/01
!+        V5P8
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
!| DAMPING        |-->| NUMBER FOR CHOICE OF DAMPING FUNCION
!| DELTAR         |-->| (RHO-RHO0)/RHO0
!| DNUTAV         |-->| COEFFICIENT FOR VERTICAL DIFFUSION OF TRACER
!| DNUVIV         |-->| COEFFICIENT FOR VERTICAL DIFFUSION OF VELOCITIES
!| GRAV           |-->| GRAVITY ACCELERATION
!| HN             |-->| WATER DEPTH
!| IELM3          |-->| TYPE OF 3D DISCRETISATION
!| IND_T          |-->| INDEX FOR TEMPERATURE
!| KARMAN         |-->| KARMAN CONSTANT
!| KFROT          |-->| LAW OF BOTTOM FRICTION
!| MASKEL         |-->| MASKING OF ELEMENTS
!|                |   | =1. : NORMAL   =0. : MASKED ELEMENT
!| MESH3D         |---| 3D MESH
!| MIXING         |-->| MIXING LENGTH MODEL
!| MSK            |-->| IF YES, THERE IS MASKED ELEMENTS.
!| NPLAN          |-->| NUMBER OF PLANES IN THE 3D MESH
!| NPOIN2         |-->| NUMBER OF 2D POINTS
!| NPOIN3         |-->| NUMBER OF 3D POINTS
!| NTRAC          |-->| NUMBER OF ACTIVE TRACERS
!| PRANDTL        |-->| PRANDTL NUMBER
!| RI             |<->| RICHARDSON NUMBER
!| RUGOF          |-->| FRICTION COEFFICIENT
!| SVIDE          |<->| VOID STRUCTURE
!| TA             |-->| ACTIVE TRACERS CONCENTRATION
!| TRAV1          |<->| WORK ARRAY
!| TRAV2          |<->| WORK ARRAY
!| TRAV3          |<->| WORK ARRAY
!| TRAV4          |<->| WORK ARRAY
!| TRAV5          |<->| WORK ARRAY
!| TRAV6          |<->| WORK ARRAY
!| TRAV7          |<->| WORK ARRAY
!| U              |-->| HORIZONTAL COMPONENT OF VELOCITY
!| UETCAR         |-->| USTAR**2 FOR BOTTOM
!| V              |-->| HORIZONTAL COMPONENT OF VELOCITY
!| VISCTA         |<->| DYNAMIC VISCOSITY COEFFICIENTS FOR TRACERS
!| VISCVI         |<->| DYNAMIC VISCOSITY COEFFICIENTS FOR VELOCITIES
!| X              |-->| MESH COORDINATES
!| Y              |-->| MESH COORDINATES
!| Z              |-->| MESH COORDINATES
!| ZF             |-->| ELEVATION OF BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE INTERFACE_TELEMAC3D, EX_VISCLM => VISCLM
      USE DECLARATIONS_TELEMAC
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)            :: NPOIN3, NPOIN2,NPLAN,KFROT
      INTEGER, INTENT(IN)            :: NTRAC,DAMPING
      INTEGER, INTENT(IN)            :: IELM3, MIXING,IND_T
      DOUBLE PRECISION, INTENT(IN)   :: GRAV,DNUVIV,DNUTAV,KARMAN
      DOUBLE PRECISION, INTENT(IN)   :: PRANDTL
      LOGICAL, INTENT(IN)            :: MSK
      TYPE (BIEF_OBJ), INTENT(INOUT) :: VISCVI, VISCTA
      TYPE (BIEF_OBJ), INTENT(IN)    :: TA,UETCAR,RUGOF,ZF
      TYPE (BIEF_OBJ), INTENT(INOUT) :: RI
      TYPE (BIEF_OBJ), INTENT(IN)    :: U, V, DELTAR, X, Y, Z, HN
      TYPE (BIEF_OBJ), INTENT(INOUT) :: TRAV1, TRAV2, TRAV3, TRAV4
      TYPE (BIEF_OBJ), INTENT(INOUT) :: TRAV5, TRAV6, TRAV7
      TYPE (BIEF_OBJ), INTENT(IN)    :: MASKEL
      TYPE (BIEF_OBJ), INTENT(INOUT) :: SVIDE
      TYPE (BIEF_MESH)               :: MESH3D
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER ITRAC,I,IPLAN,I3D
      DOUBLE PRECISION SDELTAZ,ZMIL,HH,SDLOGZ
!
!***********************************************************************
!
!     DISCRETISATION DES VISCOSITES SUR LA VERTICALE
!     ON LES DECLARE P1 TRIANGLES AVEC DEUXIEME DIMENSION NPLAN-1
!     POUR AVOIR UNE DISCRETISATION P0 SUR LA VERTICALE ET LINEAIRE EN PLAN
!
!     CECI EST FAIT EN UTILISANT LA COMPOSANTE DIMDISC MISE A 4111
!     POUR UNE UTILISATION DANS MT02PP
!
      VISCVI%ADR(3)%P%DIMDISC=4111
      IF(NTRAC.GT.0) THEN
        DO ITRAC=1,NTRAC
          VISCTA%ADR(ITRAC)%P%ADR(3)%P%DIMDISC=4111
        ENDDO
      ENDIF
!
      CALL CPSTVC(DELTAR,TRAV1)
      CALL CPSTVC(DELTAR,TRAV2)
      CALL CPSTVC(DELTAR,TRAV3)
      CALL CPSTVC(DELTAR,TRAV4)
      CALL CPSTVC(DELTAR,TRAV5)
!
!     COMPUTING DISTANCE TO BOTTOM OF MIDDLE OF EVERY LAYER (TRAV7)
!
      IF(KFROT.NE.5) THEN
        DO IPLAN= 1, NPLAN-1
          DO I = 1, NPOIN2
            I3D=I+NPOIN2*(IPLAN-1)
            TRAV7%R(I3D)=(Z%R(I3D+NPOIN2)+Z%R(I3D))*0.5D0-ZF%R(I)
            TRAV7%R(I3D)=MAX(TRAV7%R(I3D),1.D-8)
          ENDDO
        ENDDO
      ELSE
!       THIS OPTION WORKS ONLY WITH NIKURADSE LAW, HENCE RUGOF
!       IS HERE THE GRAIN SIZE
        DO IPLAN= 1, NPLAN-1
          DO I = 1, NPOIN2
            I3D=I+NPOIN2*(IPLAN-1)
            ZMIL=(Z%R(I3D+NPOIN2)+Z%R(I3D))*0.5D0
            TRAV7%R(I3D)=ZMIL+RUGOF%R(I)/30.D0-ZF%R(I)
            TRAV7%R(I3D)=MAX(TRAV7%R(I3D),1.D-8)
          ENDDO
        ENDDO
      ENDIF
!     UPPER PLANE USELESS... BUT COMPUTATION DONE IN LONGML
!                            AND SHOULD BE REMOVED
      DO I=1,NPOIN2
        I3D=I+NPOIN2*(NPLAN-1)
        TRAV7%R(I3D)=Z%R(I3D)-ZF%R(I)
        TRAV7%R(I3D)=MAX(TRAV7%R(I3D),1.D-8)
      ENDDO
!
      IF(KFROT.NE.5) THEN
!       LINEAR DERIVATIVE
        DO I=1,NPOIN3-NPOIN2
          SDELTAZ=1.D0/MAX(Z%R(I+NPOIN2)-Z%R(I),1.D-4)
          TRAV1%R(I)=(     U%R(I+NPOIN2)-     U%R(I))*SDELTAZ
          TRAV2%R(I)=(     V%R(I+NPOIN2)-     V%R(I))*SDELTAZ
          TRAV3%R(I)=(DELTAR%R(I+NPOIN2)-DELTAR%R(I))*SDELTAZ
        ENDDO
      ELSE
!       LOGARITHMIC DERIVATIVE (A SPLENDID IDEA BY CATHERINE)
! 	DU/DZ =DU/DLOGZ/Z
        DO IPLAN=1,NPLAN-1
        DO I=1,NPOIN2
          I3D=I+NPOIN2*(IPLAN-1)
!         THIS OPTION WORKS ONLY WITH NIKURADSE LAW, HENCE RUGOF
!         IS HERE THE GRAIN SIZE
          SDLOGZ=1.D0/(LOG(Z%R(I3D+NPOIN2)-ZF%R(I)+RUGOF%R(I)/30.D0)
     &                -LOG(Z%R(I3D       )-ZF%R(I)+RUGOF%R(I)/30.D0))
          TRAV1%R(I3D)=(U%R(I3D+NPOIN2)-U%R(I3D))
     &                *SDLOGZ/TRAV7%R(I3D)
          TRAV2%R(I3D)=(V%R(I3D+NPOIN2)-V%R(I3D))
     &                *SDLOGZ/TRAV7%R(I3D)
!         HUM, NOT FOR DELTAR... BACK TO LINEAR DERIVATIVE
          SDELTAZ=1.D0/MAX(Z%R(I3D+NPOIN2)-Z%R(I3D),1.D-4)
          TRAV3%R(I3D)=(DELTAR%R(I3D+NPOIN2)-DELTAR%R(I3D))
     &                *SDELTAZ
        ENDDO
        ENDDO
      ENDIF
!
!     SURFACE VALUE SET TO 0, IT IS ACTUALLY NOT USED
!     EXCEPT IN LONGML OR LONGMB (FOR NOTHING)
!
      DO I=NPOIN3-NPOIN2+1,NPOIN3
        TRAV1%R(I)=0.D0
        TRAV2%R(I)=0.D0
        TRAV3%R(I)=0.D0
        TRAV4%R(I)=0.D0
        RI%R(I)=0.D0
      ENDDO
!
!-----------------------------------------------------------------------
!
!     COMPUTES THE RICHARDSON NUMBER
!
      DO I=1,NPOIN3-NPOIN2
        TRAV4%R(I)=TRAV1%R(I)**2+TRAV2%R(I)**2
        RI%R(I)=-GRAV*TRAV3%R(I)/MAX(TRAV4%R(I),1.D-10)
        TRAV1%R(I)=SQRT(TRAV4%R(I))
      ENDDO
!
!-----------------------------------------------------------------------
!
!     SQUARE OF THE MIXING LENGTH : IN TRAV2
!
      IF(MIXING.EQ.1.OR.MIXING.EQ.3.OR.MIXING.EQ.5.OR.MIXING.EQ.6) THEN
!
        DO I = 1,NPOIN2
          DO IPLAN= 1, NPLAN
!           BACK TO ABSOLUTE VALUES (BUT Z0/30 KEPT)
            I3D=I+NPOIN2*(IPLAN-1)
            TRAV7%R(I3D)=TRAV7%R(I3D)+ZF%R(I)
          ENDDO
        ENDDO
!
        IF(KFROT.NE.5) THEN
          CALL LONGML(TRAV2%R,TRAV7%R,HN%R,NPOIN3,NPOIN2,
     &                NPLAN,MIXING,KARMAN,ZF%R)
        ELSE
!         SHIFTING DEPTH
          CALL OS('X=Y+CZ  ',X=TRAV6,Y=HN,Z=RUGOF,C=1.D0/30.D0)
          CALL LONGML(TRAV2%R,TRAV7%R,TRAV6%R,NPOIN3,NPOIN2,
     &                NPLAN,MIXING,KARMAN,ZF%R)
        ENDIF
!
      ELSEIF(MIXING.EQ.4) THEN
!
        IF(IND_T.GT.0) THEN
          CALL LONGMB(TRAV2%R,Z%R,HN%R,NPOIN3,NPOIN2,NPLAN,
     &                U%R,V%R,X%R,Y%R,TRAV5%R,TRAV6%R,
     &                TRAV7%R,NTRAC,TA%ADR(IND_T)%P%R,KARMAN,ZF%R)
        ELSE
          IF(LNG.EQ.1) THEN
            WRITE(LU,*) 'TEMPERATURE NECESSAIRE POUR LE MODELE DE JET'
            WRITE(LU,*) 'RENSEIGNER LE MOT-CLE : NOMS DES TRACEURS'
            WRITE(LU,*) 'POUR DIRE LEQUEL EST LA TEMPERATURE'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*) 'TEMPERATURE IS NECESSARY FOR THE JET MODEL'
            WRITE(LU,*) 'USE THE KEY-WORD: NAMES OF TRACERS'
            WRITE(LU,*) 'TO SAY WHICH TRACER IS THE TEMPERATURE'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
!
      ELSE
        IF(LNG.EQ.1) WRITE(LU,*) 'MODELE INCONNU DANS VISCLM : ',MIXING
        IF(LNG.EQ.2) WRITE(LU,*) 'UNKNOWN MODEL IN VISCLM : ',MIXING
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
!     STANDARD VISCOSITY WITHOUT DAMPING FUNCTION : TRAV4
!
      CALL OS('X=YZ    ',X=TRAV4,Y=TRAV1,Z=TRAV2)
!
!-----------------------------------------------------------------------
!
! DAMPING FUNCTION FOR
! THE VERTICAL TURBULENT VISCOSITY OF THE VELOCITIES : TRAV5
!
      IF(NTRAC.GT.0) THEN
!
      IF(DAMPING.EQ.1) THEN
!       USER DEFINED
        CALL DRIUTI(TRAV5%R,RI%R,1,0,NPOIN3)
      ELSEIF(DAMPING.EQ.2) THEN
!       MODELE ALGEBRIQUE
        CALL DRIALG(TRAV5%R,TRAV3%R,RI%R,NPOIN3)
      ELSEIF(DAMPING.EQ.3) THEN
!       MUNK ET ANDERSON
        CALL DRICV(TRAV5%R,TRAV3%R,RI%R,NPOIN3)
      ELSEIF(DAMPING.NE.0) THEN
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'FCT. D''AMORTISSEMENT INCONNUE DANS VISCLM : ',
     &                DAMPING
        ENDIF
        IF(LNG.EQ.2) WRITE(LU,*) 'UNKNOWN DUMPING FUNCTION IN VISCLM: ',
     &                DAMPING
        CALL PLANTE(1)
        STOP
      ENDIF
!
      ENDIF
!
!-----------------------------------------------------------------------
!
!     COMPUTES VISCVI(1,3): VERTICAL VISCOSITY OF THE VELOCITIES
!
!-----------------------------------------------------------------------
!
      IF(DAMPING.EQ.0.OR.NTRAC.EQ.0) THEN
!       SANS FONCTION D'AMORTISSEMENT
        CALL OS('X=Y     ',X=VISCVI%ADR(3)%P,Y=TRAV4)
      ELSE
!       AVEC FONCTION D'AMORTISSEMENT
        CALL OS('X=YZ    ',X=VISCVI%ADR(3)%P,Y=TRAV5,Z=TRAV4)
      ENDIF
!
!     ADDING THE MOLECULAR VISCOSITY
!
      CALL OS('X=X+C   ',X=VISCVI%ADR(3)%P,C=DNUVIV)
!
!-----------------------------------------------------------------------
!
!     COMPUTES VISCTA(1,3,ITRAC): VERTICAL VISCOSITY OF THE TRACERS
!
!-----------------------------------------------------------------------
!
      IF(NTRAC.GT.0) THEN
!
        DO ITRAC=1,NTRAC
!
          IF(DAMPING.EQ.1) THEN
!           USER DEFINED
            CALL DRIUTI(TRAV3%R,RI%R,2,ITRAC,NPOIN3)
          ELSEIF(DAMPING.EQ.2) THEN
!           VIOLLET (SEE CALL DRIALG ABOVE)
!
          ELSEIF(DAMPING.EQ.3) THEN
!           CV (SEE CALL DRICV ABOVE)
!
          ENDIF
!
          IF(DAMPING.EQ.0) THEN
            CALL OS('X=Y     ',X=VISCTA%ADR(ITRAC)%P%ADR(3)%P,Y=TRAV4)
          ELSE
            CALL OS('X=YZ    ',X=VISCTA%ADR(ITRAC)%P%ADR(3)%P,
     &                         Y=TRAV3,Z=TRAV4)
          ENDIF
!
!         DIVIDING BY THE PRANDTL NUMBER
!
          IF(ABS(PRANDTL-1.D0).GT.1.D-4) THEN
            CALL OS('X=CX    ',VISCTA%ADR(ITRAC)%P%ADR(3)%P,
     &                         C=1.D0/PRANDTL)
          ENDIF
!
!         ADDING THE MOLECULAR VISCOSITY
!
          CALL OS('X=X+C   ',X=VISCTA%ADR(ITRAC)%P%ADR(3)%P,C=DNUTAV)
!
        ENDDO
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
