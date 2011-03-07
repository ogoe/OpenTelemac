!                    *****************
                     SUBROUTINE TRIDW3
!                    *****************
!
     &(WSS,FLUVER,SUMFLU,ERROR,PRESSURE,COR_INT,COR_VER,LT)
!
!***********************************************************************
! TELEMAC3D
!***********************************************************************
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| WSS            |<--| COMPOSANTE WSTAR DE LA VITESSE A N+1
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
      TYPE(BIEF_OBJ), INTENT(INOUT) :: WSS,FLUVER,SUMFLU,ERROR,PRESSURE
      TYPE(BIEF_OBJ), INTENT(INOUT) :: COR_INT,COR_VER
      INTEGER, INTENT(IN)           :: LT
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER IETAGE, I,IAD1,IAD2,IAD3,IS,IPLAN,IPOIN2
      DOUBLE PRECISION :: SURDT,AUX,TETA
      CHARACTER(LEN=16) FORMUL
      LOGICAL TESTING
      DATA TESTING/.FALSE./
!
!=======================================================================
!
      SURDT=1.D0/DT
      CALL CPSTVC(U,SUMFLU)
      CALL CPSTVC(U,FLUVER)
      CALL CPSTVC(U,UCONV)
!
      DO I=1,NPOIN3
!       INITIALISING PRESSURE
        PRESSURE%R(I)=0.D0
      ENDDO
!
      CALL PREDIV(PRESSURE,UCONV,VCONV,WCONV,
     &            INFOGR,.FALSE.,3,.TRUE.,.FALSE.,.FALSE.)
      CALL CPSTVC(UCONV,FLUVER)
      CALL OS('X=C     ', X=FLUVER,C=1.D0)
!     FLUINT PART OF THE PRESSURE EQUATION RIGHT HAND SIDE
      FORMUL = 'VECDIF *     HOR'
      IF(SIGMAG.OR.OPTBAN.EQ.1) FORMUL(7:7)='2'
      CALL VECTOR(COR_INT,'=',FORMUL,IELM3,1.D0,
     &            FLUVER,FLUVER,FLUVER,
     &            PRESSURE,SVIDE,SVIDE,
     &            MESH3D,MSK,MASKEL)
!     ON RETURN MESH3D%W%R IS THE EBE MATRIX VECTOR PRODUCT
!     COR_INT IS DONE BUT WILL BE REDONE BELOW
!     EBE TERMS AT FREE SURFACE = -(ALL TERMS ON THE SAME VERTICAL)
      CALL COMPLETE_EBE_FLUINT(MESH3D%W%R,NELEM2,NPLAN)
      IF(N_ADV(ADV_NSC).GT.0.OR.N_ADV(ADV_PSI   ).GT.0
     &                      .OR.N_ADV(ADV_NSC_TF).GT.0) THEN
!       RESULT SUBTRACTED TO MTRA1
        DO I=1,6*MESH3D%NELEM
          MTRA1%X%R(I)=MTRA1%X%R(I)-MESH3D%W%R(I)
        ENDDO
      ENDIF
!     RE-ASSEMBLING AFTER COMPLETION AT THE FREE SURFACE
      CALL ASSVEC(COR_INT%R,MESH3D%IKLE%I,
     &            NPOIN3,NELEM3,NELEM3,IELM3,
     &            MESH3D%W%R,.TRUE.,1,MSK,MASKEL%R,
     &            BIEF_NBPEL(IELM3,MESH3D))
!     VERTICAL PART OF RIGHT-HAND SIDE
!     FORMUL = 'VECDIF *     VER'
!     IF(SIGMAG.OR.OPTBAN.EQ.1) FORMUL(7:7)='2'
!     CALL VECTOR(COR_VER,'=',FORMUL,IELM3,1.D0,
!    *            FLUVER,FLUVER,FLUVER,PRESSURE,SVIDE,SVIDE,
!    *            MESH3D,MSK,MASKEL)
!     OTHER POSSIBILITY: IT IS RHS-FLUINT PART
!     RHS_PRESSURE IS CALLED HERE JUST TO GET THE INITIAL
!     RHS OF THE PRESSURE EQUATION (DESTROYED BY SOLVER)
!     COULD BE OPTIMISED SOMEHOW
      CALL RHS_PRESSURE(SEM3D,UCONV,VCONV,WCONV,IELM3,DM1,
     &                  ZCONV,SVIDE,MESH3D,MSK,MASKEL,FLUEXT,
     &                  NSCE,RAIN,PLUIE,SOURCES,GRADZF,VOLU2D,
     &                  DSSUDT,NPOIN2,NPOIN3,NPLAN)
      CALL OS('X=Y-Z   ',X=COR_VER,Y=SEM3D,Z=COR_INT)
!
      IF(TESTING) THEN
!       TESTING SUMS OF FLUVER ON VERTICAL
        DO I=1,NPOIN2
          T2_01%R(I)=0.D0
        ENDDO
        DO IPLAN=1,NPLAN
          DO I=1,NPOIN2
            T2_01%R(I)=T2_01%R(I)+COR_VER%R((IPLAN-1)*NPOIN2+I)
          ENDDO
        ENDDO
        IF(NCSIZE.GT.1) CALL PARCOM(T2_01,2,MESH2D)
        WRITE(LU,*) 'COR_VER ON VERTICAL=',P_DOTS(T2_01,T2_01,MESH2D)
      ENDIF
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
      CALL FLUVER_2(FLUVER,UCONV,VCONV,WCONV,GRADZF,VOLU2D,DSSUDT,
     &              NPLAN,NPOIN2)
!
      DO I=1,NPOIN3
        FLUVER%R(I)=FLUVER%R(I)+SURDT*(VOLU%R(I)-VOLUN%R(I))
      ENDDO
!
      IF(TESTING) THEN
        DO I=1,NPOIN2
          T2_01%R(I)=0.D0
        ENDDO
        DO IPLAN=1,NPLAN
          DO I=1,NPOIN2
            T2_01%R(I)=T2_01%R(I)+FLUVER%R((IPLAN-1)*NPOIN2+I)
          ENDDO
        ENDDO
        IF(NCSIZE.GT.1) CALL PARCOM(T2_01,2,MESH2D)
        WRITE(LU,*) 'FLUVER ON VERTICAL=',P_DOTS(T2_01,T2_01,MESH2D)
!       CHECKING SUM OF ERRORS ON THE VERTICAL
!       IN PARALLELISM : WORKING HERE ON NON-ASSEMBLED VALUES
        DO I=1,NPOIN3
          SUMFLU%R(I)=FLUEXT%R(I)+SURDT*(VOLU%R(I)-VOLUN%R(I))
        ENDDO
!       WITH SOURCES (EVAPORATION IS LACKING HERE !!!!!, MAYBE IT CANNOT BE DISCARDED
!       LIKE WITH PREVIOUS TRIDW2)
        IF(NSCE.GT.0) THEN
!         WITH SOURCES
          DO IS=1,NSCE
            DO I=1,NPOIN3
!             NON ASSEMBLED SOURCES
              SUMFLU%R(I)=SUMFLU%R(I)-SOURCES%ADR(IS+NSCE)%P%R(I)
            ENDDO
          ENDDO
        ENDIF
        IF(RAIN) THEN
          DO IPOIN2=1,NPOIN2
            I=NPOIN3-NPOIN2+IPOIN2
            SUMFLU%R(I)=SUMFLU%R(I)-PLUIE%R(IPOIN2)
          ENDDO
        ENDIF
        DO I=1,NPOIN3
          ERROR%R(I)=SUMFLU%R(I)-FLUVER%R(I)-FLUINT%R(I)
        ENDDO
        DO I=1,NPOIN2
          T2_01%R(I)=0.D0
        ENDDO
        DO IPLAN=1,NPLAN
          DO I=1,NPOIN2
            T2_01%R(I)=T2_01%R(I)+ERROR%R((IPLAN-1)*NPOIN2+I)
          ENDDO
        ENDDO
        IF(NCSIZE.GT.1) CALL PARCOM(T2_01,2,MESH2D)
        WRITE(LU,*) 'SUM OF ERRORS ON VERTICAL=',
     &               P_DOTS(T2_01,T2_01,MESH2D)
        IF(NCSIZE.GT.1) CALL PARCOM(ERROR,2,MESH3D)
        WRITE(LU,*) 'ERROR INITIALE=',P_DOTS(ERROR,ERROR,MESH3D)
      ENDIF
!
!     CORRECTING WITH PRESSURE GRADIENT FLUXES
!
      DO I=1,NPOIN3
        FLUINT%R(I)=FLUINT%R(I)-COR_INT%R(I)
        FLUVER%R(I)=FLUVER%R(I)-COR_VER%R(I)
      ENDDO
!
      IF(TESTING) THEN
        DO I=1,NPOIN3
          ERROR%R(I)=SUMFLU%R(I)-FLUVER%R(I)-FLUINT%R(I)
        ENDDO
        DO I=1,NPOIN2
          T2_01%R(I)=0.D0
        ENDDO
        DO IPLAN=1,NPLAN
          DO I=1,NPOIN2
            T2_01%R(I)=T2_01%R(I)+ERROR%R((IPLAN-1)*NPOIN2+I)
          ENDDO
        ENDDO
        IF(NCSIZE.GT.1) CALL PARCOM(T2_01,2,MESH2D)
        WRITE(LU,*) 'NOUVELLE SOMME ERROR VERTICALE=',
     &               P_DOTS(T2_01,T2_01,MESH2D)
        IF(NCSIZE.GT.1) CALL PARCOM(ERROR,2,MESH3D)
        WRITE(LU,*) 'ERROR=',P_DOTS(ERROR,ERROR,MESH3D)
        IF(NCSIZE.GT.1) CALL PARCOM(COR_INT,2,MESH3D)
        IF(NCSIZE.GT.1) CALL PARCOM(COR_VER,2,MESH3D)
        WRITE(LU,*) 'CORRECTION H=',P_DOTS(COR_INT,COR_INT,MESH3D)
        WRITE(LU,*) 'CORRECTION V=',P_DOTS(COR_VER,COR_VER,MESH3D)
      ENDIF
!
!     POSSIBLE FINAL CORRECTION LIKE TRIDW2
!     TO CORRECT TRUNCATION AND SOLVER ERRORS
!
!     DO I=1,NPOIN3
!       FLUVER%R(I)=SUMFLU%R(I)-FLUINT%R(I)
!     ENDDO
!
!-----------------------------------------------------------------------
!
!     PARALLELISM: NOW ASSEMBLING FLUVER
!
      IF(NCSIZE.GT.1) CALL PARCOM(FLUVER,2,MESH3D)
!
!     FINDING WSS FOR ADVECTION
!
      IAD3=0
!     DO IETAGE = 1,NETAGE
      DO IETAGE = 1,NETAGE+1
        DO I=1,NPOIN2
          IAD3=IAD3+1
          WSS%R(IAD3)=-FLUVER%R(IAD3)*UNSV2D%R(I)
        ENDDO
      ENDDO
!
      IF(NETAGE.GT.1) THEN
        IAD1=0
        IAD2=NPOIN2
!       DO IETAGE = 2,NETAGE
        DO IETAGE = 2,NETAGE+1
          DO I=1,NPOIN2
            IAD1=IAD1+1
            IAD2=IAD2+1
            WSS%R(IAD2)=WSS%R(IAD2)+WSS%R(IAD1)
          ENDDO
        ENDDO
      ENDIF
!
!     CHECKING WSS OF LAST PLANE
!
      IF(TESTING) THEN
        DO I=1,NPOIN2
          T2_01%R(I)=0.D0
        ENDDO
        DO I=1,NPOIN2
          T2_01%R(I)=T2_01%R(I)+WSS%R((NPLAN-1)*NPOIN2+I)
        ENDDO
        WRITE(LU,*) 'WSCONV FREE SURFACE=',
     &               P_DOTS(T2_01,T2_01,MESH2D)
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
! CONDITIONS DE DIRICHLET SUR LES PAROIS LATERALES
!     WSS = WSBORL
!
!=======================================================================
! FORTRAN77:
!
! LA FACON DONT ON TRAITE LES EQUATIONS N'AUTORISE PAS A PRENDRE EN
! COMPTE DE TELLES CONDITIONS SOUS PEINE D'ERREUR DE MASSE
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