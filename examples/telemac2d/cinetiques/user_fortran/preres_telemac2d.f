!                    ***************************
                     SUBROUTINE PRERES_TELEMAC2D
!                    ***************************
     &    (IMP,LEO)
!
!
!***********************************************************************
! TELEMAC2D   V7P1
!***********************************************************************
!
!brief    PREPARES THE VARIABLES WHICH WILL BE WRITTEN TO
!+                THE RESULTS FILE OR TO THE LISTING.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_SPECIAL
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D
!
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL ,INTENT(INOUT)::IMP,LEO
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      LOGICAL DEJA1,DEJA2,DEJA3
!
      INTEGER LTT,N,IMAX,I
!
      DOUBLE PRECISION HHH,XMAX,NF,PI,AMP,PHA,CC
      DOUBLE PRECISION, PARAMETER:: EPSS=1.E-10
      DOUBLE PRECISION GPRDTIME,LPRDTIME,RESTE
!
      INTRINSIC MAX,SQRT
      DATA DEJA1/.FALSE./
      DATA DEJA2/.FALSE./
      DATA DEJA3/.FALSE./
      SAVE DEJA1,DEJA2,DEJA3,NF
!
!-----------------------------------------------------------------------
!
!     THE OUTPUT VARIABLES ARE BUILT ONLY IF NECESSARY, HENCE THE
!     FOLLOWING TESTS, WHICH MUST BE THE SAME AS IN BIEF_DESIMP (BIEF LIBRARY)
!
!     THIS WILL TRIGGER THE OUTPUT OF LAST TIMESTEP
!     BUT NOT WITH PARAMETER ESTIMATION (LISPRD WOULD STAY AT 1
!     FOR FURTHER COMPUTATIONS)
      IF(LT.EQ.NIT.AND.ESTIME(1:1).EQ.' ') THEN
        LISPRD=1
        LEOPRD=1
      ENDIF
!
      IMP=.FALSE.
      LEO=.FALSE.
!     Always write the intial conditions
      IF(LT.EQ.0) THEN
        IMP=.TRUE.
        LEO=.TRUE.
        COMPLEO=0
      ELSE
        IF(EQUA(1:15).NE.'SAINT-VENANT VF') THEN
!         FEM
          LTT=(LT/LISPRD)*LISPRD
          IF(LT.EQ.LTT.AND.LT.GE.PTINIL) IMP=.TRUE.
          LTT=(LT/LEOPRD)*LEOPRD
          IF(LT.EQ.LTT.AND.LT.GE.PTINIG) LEO=.TRUE.
!         FOR GRAPHICAL OUTPUTS
          IF(LEO)COMPLEO=COMPLEO+1
        ELSE
!         FVM
          GPRDTIME=LEOPRD*DTINI
          LPRDTIME=LISPRD*DTINI
          IF(GPRDTIME.LT.EPSS.OR.LPRDTIME.LT.EPSS)THEN
            CALL PLANTE(1)
            STOP
          ENDIF
          IF(LT.GE.PTINIG)THEN
!           GRAPHIC OUTPUT
            LTT=CEILING(AT/GPRDTIME)
            RESTE=(LTT*GPRDTIME-AT)/GPRDTIME
            IF(RESTE.LT.EPSS.OR.ABS(RESTE-1.D0).LT.EPSS.OR.
!                                   CASE WHERE RESTE=1
     &        LT.EQ.NIT)THEN
              LEO=.TRUE.
              COMPLEO=COMPLEO+1
            ENDIF

          ENDIF
          IF(LT.GT.PTINIL)THEN
!           LISTING OUTPUT
            LTT=CEILING(AT/LPRDTIME)
            RESTE=(LTT*LPRDTIME-AT)/LPRDTIME
            IF(RESTE.LT.EPSS.OR.ABS(RESTE-1.D0).LT.EPSS.OR.
!                                   CASE WHERE RESTE=1
     &        LT.EQ.NIT)THEN
              IMP=.TRUE.
            ENDIF
          ENDIF
        ENDIF
      ENDIF
!
!
!-----------------------------------------------------------------------
!
! 1)  PART WHICH MUST BE DONE EVEN IF THERE IS NO OUTPUT FOR THIS STEP
!     BUT ONLY AFTER FIRST TIME STEP FOR GRAPHIC PRINTOUTS
!
!-----------------------------------------------------------------------
!
      IF(NPERIAF.GT.0.AND.LT.EQ.0) THEN
!       FOR OUTPUT OF INITIAL CONDITIONS
        CALL OS('X=C     ',AMPL,AMPL,AMPL,0.D0)
        CALL OS('X=C     ',PHAS,PHAS,PHAS,0.D0)
      ENDIF
!
      IF(LT.GE.PTINIG) THEN
!
!=======================================================================
! CALCUL DE LA COTE MAXIMUM ET TEMPS ASSOCIE
!=======================================================================
!
      IF(SORLEO(27).OR.SORIMP(27)) THEN
        IF(.NOT.DEJA1) THEN
          CALL OS('X=Y     ',MAXZ ,ZF,ZF,0.D0)
          CALL OS('X=C     ',TMAXZ,ZF,ZF,AT  )
          DEJA1=.TRUE.
        ELSE
          DO N=1,NPOIN
            XMAX=H%R(N)+ZF%R(N)
!           DRY LAND EXCLUDED (TO AVOID RANDOM TIMES)
            IF(XMAX.GT.MAXZ%R(N).AND.H%R(N).GT.0.01D0) THEN
              MAXZ%R(N)=XMAX
              IF(SORLEO(28).OR.SORIMP(28)) TMAXZ%R(N)=AT
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
! CALCUL DE LA VITESSE MAXIMUM ET TEMPS ASSOCIE
!=======================================================================
!
      IF(SORLEO(29).OR.SORIMP(29)) THEN
        IF(.NOT.DEJA2) THEN
          CALL OS('X=C     ',MAXV ,MAXV ,MAXV ,0.D0)
          CALL OS('X=C     ',TMAXV,TMAXV,TMAXV,  AT)
          DEJA2=.TRUE.
        ELSE
          DO N=1,NPOIN
            XMAX=SQRT(U%R(N)**2+V%R(N)**2)
!           DRY LAND EXCLUDED (TO AVOID RANDOM TIMES)
            IF(XMAX.GT.MAXV%R(N).AND.H%R(N).GT.0.01D0) THEN
              MAXV%R(N)=XMAX
              IF(SORLEO(30).OR.SORIMP(30)) TMAXV%R(N)=AT
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!=======================================================================
! IMPRESSIONS POUR LES POINTS REMARQUABLES
!=======================================================================
!
      IF(LT.EQ.NIT.AND.NPTS.GT.0) THEN
        DO I=27,30
!         CAUTION : HERE SORLEO IS USED INSTEAD OF SORIMP
          IF(SORLEO(I)) THEN
            WRITE(LU,*) ' '
            WRITE(LU,*) ' '
            WRITE(LU,*) ' '
            WRITE(LU,*) TEXTE(I)(1:16)
            WRITE(LU,*) ' '
            DO N=1,NPTS
              WRITE(LU,*) NAME_PTS(N),' : ',
     &                                    VARSOR%ADR(I)%P%R(LIST_PTS(N))
            ENDDO
          ENDIF
        ENDDO
      ENDIF
!
!=======================================================================
! ANALYSES DE FOURIER DE LA COTE
!=======================================================================
!
!     NF : NOMBRE DE POINTS DE LA SERIE TEMPORELLE
!     ON CALCULE D'ABORD : SOMME (SIGNAL * EXP(-I OMEGA AT))
!     EN METTANT LA PARTIE REELLE DANS AMPL ET L'IMAGINAIRE DANS PHAS
      IF(NPERIAF.GT.0) THEN
!
        PI=ACOS(-1.D0)
        IF(.NOT.DEJA3) THEN
          DO I=1,NPERIAF
            DO N=1,NPOIN
             AMPL%ADR(I)%P%R(N)= (H%R(N)+ZF%R(N))*COS(2*PI*AT/PERIAF(I))
             PHAS%ADR(I)%P%R(N)=-(H%R(N)+ZF%R(N))*SIN(2*PI*AT/PERIAF(I))
            ENDDO
          ENDDO
          DEJA3=.TRUE.
          NF=1.D0
        ELSE
          DO I=1,NPERIAF
            DO N=1,NPOIN
             AMPL%ADR(I)%P%R(N)=AMPL%ADR(I)%P%R(N)
     &                          +(H%R(N)+ZF%R(N))*COS(2*PI*AT/PERIAF(I))
             PHAS%ADR(I)%P%R(N)=PHAS%ADR(I)%P%R(N)
     &                          -(H%R(N)+ZF%R(N))*SIN(2*PI*AT/PERIAF(I))
            ENDDO
          ENDDO
          NF=NF+1.D0
        ENDIF
!
!       PASSAGE FINAL A AMPLITUDE ET PHASE
!       (APRES DIVISION PAR NF, ON A AMPL = AMPLITUDE * COS(PHASE)
!                                 ET PHAS = AMPLITUDE * SIN(PHASE)
        IF(LT.EQ.NIT) THEN
          DO I=1,NPERIAF
            DO N=1,NPOIN
             AMPL%ADR(I)%P%R(N)=AMPL%ADR(I)%P%R(N)/NF
             PHAS%ADR(I)%P%R(N)=PHAS%ADR(I)%P%R(N)/NF
             AMP=SQRT(AMPL%ADR(I)%P%R(N)**2+PHAS%ADR(I)%P%R(N)**2)
             PHA=ATAN2(PHAS%ADR(I)%P%R(N),AMPL%ADR(I)%P%R(N))
!            PHASE ENTRE 0 ET 360 DEGRES
             PHA=PHA*180.D0/PI+180.D0
             AMPL%ADR(I)%P%R(N)=AMP
             PHAS%ADR(I)%P%R(N)=PHA
            ENDDO
            IF(NPTS.GT.0) THEN
              WRITE(LU,*) ' '
              WRITE(LU,*) ' '
              WRITE(LU,*) ' '
              IF(LNG.EQ.1) WRITE(LU,*) 'ANALYSE DE LA PERIODE ',
     &                                  PERIAF(I),' S :'
              IF(LNG.EQ.2) WRITE(LU,*) 'ANALYSIS OF PERIOD ',
     &                                  PERIAF(I),' S :'
              WRITE(LU,*) ' '
              DO N=1,NPTS
                WRITE(LU,*) 'AMPLITUDE ',NAME_PTS(N),' : ',
     &                                   AMPL%ADR(I)%P%R(LIST_PTS(N))
                WRITE(LU,*) 'PHASE     ',NAME_PTS(N),' : ',
     &                                   PHAS%ADR(I)%P%R(LIST_PTS(N))
                WRITE(LU,*) ' '
              ENDDO
            ENDIF
          ENDDO
!       ENDIF DE : IF(LT.EQ.NIT)
        ENDIF
!
!     ENDIF DE : IF(NPERIAF.GT.0) THEN
      ENDIF
!
!-----------------------------------------------------------------------
!
!     ENDIF DE : IF(LT.GE.PTINIG) THEN
      ENDIF
!
!-----------------------------------------------------------------------
!
! 2)  PART WHICH MUST BE DONE ONLY IF THERE IS AN OUTPUT FOR THIS STEP
!
!-----------------------------------------------------------------------
!
!     PAS D'IMPRESSION, PAS DE SORTIE SUR FICHIER, ON RESSORT
      IF(.NOT.(LEO.OR.IMP)) GO TO 1000
!
!
!=======================================================================
! CALCUL DE LA CELERITE (MISE DANS FU, VOIR LE BLOC VARSOR)
!=======================================================================
!
      IF((LEO.AND.SORLEO(3)).OR.(IMP.AND.SORIMP(3))) THEN
        DO N=1,NPOIN
          FU%R(N) = SQRT ( GRAV * MAX(H%R(N),0.D0) )
        ENDDO
      ENDIF
!
!=======================================================================
! CALCUL DE LA SURFACE LIBRE (= H + ZF, MISE DANS FV)
!=======================================================================
!
      IF((LEO.AND.SORLEO(5)).OR.(IMP.AND.SORIMP(5))) THEN
        CALL OS( 'X=Y+Z   ' , FV , H , ZF , 0.D0 )
      ENDIF
!
!=======================================================================
! CALCUL DU NOMBRE DE FROUDE
!=======================================================================
!
      IF((LEO.AND.SORLEO(7)).OR.(IMP.AND.SORIMP(7))) THEN
        DO N=1,NPOIN
          HHH = MAX( H%R(N) , 1.D-8 )
          T2%R(N) = SQRT (( U%R(N)**2 + V%R(N)**2 ) / ( HHH*GRAV ))
        ENDDO
      ENDIF
!
!=======================================================================
! CALCUL DU DEBIT SCALAIRE
!=======================================================================
!
      IF((LEO.AND.SORLEO(8)).OR.(IMP.AND.SORIMP(8))) THEN
        DO N=1,NPOIN
          T3%R(N) = SQRT (U%R(N)**2 + V%R(N)**2) * H%R(N)
        ENDDO
      ENDIF
!
!=======================================================================
! CALCUL DU DEBIT VECTORIEL , COMPOSANTE SUIVANT X
!=======================================================================
!
      IF((LEO.AND.SORLEO(13)).OR.(IMP.AND.SORIMP(13))) THEN
        CALL CPSTVC(H,T4)
        DO N=1,NPOIN
          T4%R(N)=H%R(N)*U%R(N)
        ENDDO
      ENDIF
!
!=======================================================================
! CALCUL DU DEBIT VECTORIEL , COMPOSANTE SUIVANT Y
!=======================================================================
!
      IF((LEO.AND.SORLEO(14)).OR.(IMP.AND.SORIMP(14))) THEN
        CALL CPSTVC(H,T5)
        DO N=1,NPOIN
          T5%R(N)=H%R(N)*V%R(N)
        ENDDO
      ENDIF
!
!=======================================================================
! CALCUL DE LA VITESSE SCALAIRE
!=======================================================================
!
      IF((LEO.AND.SORLEO(15)).OR.(IMP.AND.SORIMP(15))) THEN
        CALL OS( 'X=N(Y,Z)' , T6 , U , V , 0.D0 )
      ENDIF
!
!=======================================================================
! CALCUL DU NOMBRE DE COURANT
!=======================================================================
!
      IF((LEO.AND.SORLEO(22)).OR.(IMP.AND.SORIMP(22))) THEN
!                             IELM
        CALL CFLPSI(T9,U,V,DT,11,MESH,MSK,MASKEL)
        CALL MAXI(XMAX,IMAX,T9%R,NPOIN)
        IF (LNG.EQ.1) WRITE(LU,78) XMAX
        IF (LNG.EQ.2) WRITE(LU,79) XMAX
78      FORMAT(1X,'PRERES : NOMBRE DE COURANT MAXIMUM :',G16.7)
79      FORMAT(1X,'PRERES: MAXIMUM COURANT NUMBER: ',G16.7)
      ENDIF
!
!=======================================================================
! CALCUL DE LA HAUTEUR EXACTE
!=======================================================================
!
      CC=SQRT(4.D0*9.81D0)
      IF((LEO.AND.SORLEO(23)).OR.(IMP.AND.SORIMP(23))) THEN
        DO N=1,NPOIN
          HHH = MAX(0.D0,CC-(X(N)-10.5D0)/2.D0/MAX(AT,DT))
          HHH = 4.D0*HHH**2/9.D0/9.81D0
          PRIVE%ADR(1)%P%R(N) = MIN(4.D0,HHH)
        ENDDO
      ENDIF
!
!=======================================================================
! CALCUL DE LA VITESSE EXACTE
!=======================================================================
!
      IF((LEO.AND.SORLEO(24)).OR.(IMP.AND.SORIMP(24))) THEN
        DO N=1,NPOIN
          PRIVE%ADR(2)%P%R(N) = 2.D0*(CC+X(N)/MAX(AT,DT))/3.D0
        ENDDO
      ENDIF
!
!=======================================================================
!
1000  CONTINUE
      RETURN
      END

