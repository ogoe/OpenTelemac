!                       ************************
                        SUBROUTINE CALCUL_Q_WEIR
!                       ************************
!
     &    (NWEIRS,X,Y,ZF,HN,CHESTR,NKFROT,KARMAN,IOPTAN,NTRAC,T)
!
!***********************************************************************
! TELEMAC2D   V6P3                                   22/03/2013
!***********************************************************************
!
!brief    COMPUTE THE DISCHARGE ON THE WEIRS WHEN THE TYPE IS EQUAL 2
!+
!
!+
!history  C.COULET / A.REBAI / E.DAVID (ARTELIA)
!+        22/03/2013
!+        V6P3
!+   Creation
!
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!| CHESTR         |-->| FRICTION COEFFICIENT
!| HN             |-->| DEPTH AT TIME T(N)
!| IOPTAN         |-->| OPTION FOR TANGENTIAL VELOCITIES.
!| KARMAN         |-->| VON KARMAN CONSTANT.
!| NKFROT         |-->| FRICTION LAW, PER POINT
!| NTRAC          |-->| NUMBER OF TRACERS
!| NWEIRS         |-->| NUMBER OF WEIRS
!| T              |-->| BLOCK OF TRACERS
!| X              |-->| ABSCISSAE OF NODES
!| Y              |-->| ORDINATES OF NODES
!| ZF             |-->| BOTTOM
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC2D, ONLY: NPSING,NDGA1,NDGA2,NDGB1,NDGB2,
     &                                  QWA,QWB,QP0,ZDIG,WDIG,
     &                                  UWEIRA,UWEIRB,VWEIRA,VWEIRB,
     &                                  TWEIRA,TWEIRB,MAXNPS
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER, INTENT(IN)           :: NWEIRS,IOPTAN,NTRAC
      INTEGER, INTENT(IN)           :: NKFROT(*)
      DOUBLE PRECISION, INTENT(IN)  :: X(*),Y(*),ZF(*),HN(*),CHESTR(*)
      DOUBLE PRECISION, INTENT(IN)  :: KARMAN
      TYPE(BIEF_OBJ)  , INTENT(IN)  :: T
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      INTEGER          I, N, ITRAC, INDIC
      INTEGER          IA1, IB1, IA2, IB2
      INTEGER          IC1, ID1, IC2, ID2
!
      DOUBLE PRECISION PHI, GRAV, RELAX
      DOUBLE PRECISION YS1, YS2
      DOUBLE PRECISION SL_A1, SL_B1, SL_A2, SL_B2
      DOUBLE PRECISION SL_C1, SL_D1, SL_C2, SL_D2
      DOUBLE PRECISION ZF_A1, ZF_B1, ZF_A2, ZF_B2
      DOUBLE PRECISION ZF_C1, ZF_D1, ZF_C2, ZF_D2
      DOUBLE PRECISION H_A1, H_B1, H_A2, H_B2
      DOUBLE PRECISION H_C1, H_D1, H_C2, H_D2
      DOUBLE PRECISION SLA, SLB
      DOUBLE PRECISION ZFA, ZFB
      DOUBLE PRECISION HSA, HSB
      DOUBLE PRECISION HA, HB, HMIN
      DOUBLE PRECISION QELEM
      DOUBLE PRECISION TXA,TYA,DLA,NXA,NYA
      DOUBLE PRECISION TXB,TYB,DLB,NXB,NYB
      DOUBLE PRECISION UTANA, UTANB, UTANC, UTAND
      DOUBLE PRECISION PENTA,PENTB
      DOUBLE PRECISION DENOM
!
      DOUBLE PRECISION XP_A1,XP_B1,XP_A2,XP_B2
      DOUBLE PRECISION XP_C1,XP_D1,XP_C2,XP_D2
      DOUBLE PRECISION YP_A1,YP_B1,YP_A2,YP_B2
      DOUBLE PRECISION YP_C1,YP_D1,YP_C2,YP_D2
!
      DOUBLE PRECISION TRAC_A1(NTRAC),TRAC_B1(NTRAC)
      DOUBLE PRECISION TRAC_A2(NTRAC),TRAC_B2(NTRAC)
      DOUBLE PRECISION TRAC_C1(NTRAC),TRAC_D1(NTRAC)
      DOUBLE PRECISION TRAC_C2(NTRAC),TRAC_D2(NTRAC)
      DOUBLE PRECISION TRAC_A(NTRAC) ,TRAC_B(NTRAC)
      DOUBLE PRECISION TRAC_C(NTRAC) ,TRAC_D(NTRAC)
!
      DOUBLE PRECISION P_DMAX,P_DMIN
      EXTERNAL         P_DMAX,P_DMIN
!
      INTRINSIC MAX,SQRT,ABS
!
!-----------------------------------------------------------------------
!
!
      HMIN = 1.D-3
      PHI = 0.4
      GRAV = 9.81D0
      RELAX = 0.5D0 ! eventuellement à rendre paramétrable dans le fichier des seuils
!
      DO N = 1, NWEIRS
        CALL OS('X=0     ',X=QWA%ADR(N)%P)
        CALL OS('X=0     ',X=QWB%ADR(N)%P)
        CALL OS('X=0     ',X=UWEIRA%ADR(N)%P)
        CALL OS('X=0     ',X=UWEIRB%ADR(N)%P)
        CALL OS('X=0     ',X=VWEIRA%ADR(N)%P)
        CALL OS('X=0     ',X=VWEIRB%ADR(N)%P)
      ENDDO
!
      DO ITRAC = 1, NTRAC
        CALL OS('X=0     ',X=TWEIRA%ADR(ITRAC)%P)
        CALL OS('X=0     ',X=TWEIRB%ADR(ITRAC)%P)
      ENDDO
!
      DO N = 1, NWEIRS
        DO I = 1 ,NPSING%I(N)-1   !LOOP ON ELEMENTARY PEACE OF WEIRS
!         FIND THE NODES OF MESH AROUND THE NODE COMPOSING THE WEIR
          IA1 = NDGA1%ADR(N)%P%I(I)
          IB1 = NDGB1%ADR(N)%P%I(I)
          IA2 = NDGA2%ADR(N)%P%I(I)
          IB2 = NDGB2%ADR(N)%P%I(I)
          IC1 = NDGA1%ADR(N)%P%I(I+1)
          ID1 = NDGB1%ADR(N)%P%I(I+1)
          IC2 = NDGA2%ADR(N)%P%I(I+1)
          ID2 = NDGB2%ADR(N)%P%I(I+1)
!         FIND THE VALUE OF BOTTOM, WATER LEVEL, TRACER
!           AND ALSO POSITION OF NODES
          CALL COLLECT_VALUES(X,Y,ZF,HN,NTRAC,T,
     &       IA1,XP_A1,YP_A1,ZF_A1,H_A1,SL_A1,TRAC_A1)
          CALL COLLECT_VALUES(X,Y,ZF,HN,NTRAC,T,
     &       IB1,XP_B1,YP_B1,ZF_B1,H_B1,SL_B1,TRAC_B1)
          CALL COLLECT_VALUES(X,Y,ZF,HN,NTRAC,T,
     &       IA2,XP_A2,YP_A2,ZF_A2,H_A2,SL_A2,TRAC_A2)
          CALL COLLECT_VALUES(X,Y,ZF,HN,NTRAC,T,
     &       IB2,XP_B2,YP_B2,ZF_B2,H_B2,SL_B2,TRAC_B2)
          CALL COLLECT_VALUES(X,Y,ZF,HN,NTRAC,T,
     &       IC1,XP_C1,YP_C1,ZF_C1,H_C1,SL_C1,TRAC_C1)
          CALL COLLECT_VALUES(X,Y,ZF,HN,NTRAC,T,
     &       ID1,XP_D1,YP_D1,ZF_D1,H_D1,SL_D1,TRAC_D1)
          CALL COLLECT_VALUES(X,Y,ZF,HN,NTRAC,T,
     &       IC2,XP_C2,YP_C2,ZF_C2,H_C2,SL_C2,TRAC_C2)
          CALL COLLECT_VALUES(X,Y,ZF,HN,NTRAC,T,
     &       ID2,XP_D2,YP_D2,ZF_D2,H_D2,SL_D2,TRAC_D2)
!
!         COMPUTE MEAN VALUE
!
          SLA = 0.25D0 * (SL_A1 +SL_A2 +SL_C1 +SL_C2 )
          SLB = 0.25D0 * (SL_B1 +SL_B2 +SL_D1 +SL_D2 )
          ZFA = 0.25D0 * (ZF_A1 +ZF_A2 +ZF_C1 +ZF_C2 )
          ZFB = 0.25D0 * (ZF_B1 +ZF_B2 +ZF_D1 +ZF_D2 )
          HA  = 0.25D0 * ( H_A1 + H_A2 + H_C1 + H_C2 )
          HB  = 0.25D0 * ( H_B1 + H_B2 + H_D1 + H_D2 )
          DO ITRAC = 1, NTRAC
            TRAC_A(ITRAC) = 0.5D0 * (TRAC_A1(ITRAC)+TRAC_A2(ITRAC))
            TRAC_B(ITRAC) = 0.5D0 * (TRAC_B1(ITRAC)+TRAC_B2(ITRAC))
            TRAC_C(ITRAC) = 0.5D0 * (TRAC_C1(ITRAC)+TRAC_C2(ITRAC))
            TRAC_D(ITRAC) = 0.5D0 * (TRAC_D1(ITRAC)+TRAC_D2(ITRAC))
          ENDDO
!
!         CALCULATES THE NORMAL VECTOR, OUTGOING SIDE A, ENTERING SIDE B
!
          TXA=XP_C2-XP_A1
          TYA=YP_C2-YP_A1
          DLA=SQRT(TXA**2+TYA**2)
          TXA=TXA/DLA
          TYA=TYA/DLA
          NXA=-TYA
          NYA=TXA
!
          TXB=XP_D2-XP_B1
          TYB=YP_D2-YP_B1
          DLB=SQRT(TXB**2+TYB**2)
          TXB=TXB/DLB
          TYB=TYB/DLB
          NXB=-TYB
          NYB=TXB
!
!         COMPUTATION OF THE DISCHARGE
!
!         ADDING A SECURITY ON THE LEVEL OF THE WEIR
          YS1 = MAX(ZDIG%ADR(N)%P%R(I)  ,ZFA+0.01D0,ZFB+0.01D0)
          YS2 = MAX(ZDIG%ADR(N)%P%R(I+1),ZFA+0.01D0,ZFB+0.01D0)
!
          QELEM = 0.D0
!         UPSTREAM IS ON SIDE A
          IF (SLA.GT.SLB) THEN
            IF (HA.GT.0.01D0) THEN
              CALL LOI_W_INC(SLA,SLB,YS1,YS2,WDIG%ADR(N)%P%R(I),
     &                       PHI,QELEM,GRAV)
            ELSE
              QELEM=0.D0
            ENDIF
!         UPSTREAM IS ON SIDE B
          ELSE
            IF (HB.GT.0.01D0) THEN
              CALL LOI_W_INC(SLB,SLA,YS1,YS2,WDIG%ADR(N)%P%R(I),
     &                       PHI,-QELEM,GRAV)
            ELSE
              QELEM=0.D0
            ENDIF
          ENDIF
!
          QELEM = (QELEM*(1D0-RELAX)+QP0%ADR(N)%P%R(I)*RELAX)
          QP0%ADR(N)%P%R(I)=QELEM
!
!       DISTRIBUTE THE COMPUTED DISCHARGE OF EACH ELEMENTS OF WEIRS ON NODES
!       QELEM > 0 means the flow is from side A to side B
!
          QWA%ADR(N)%P%R(I)   = QWA%ADR(N)%P%R(I  ) - 0.5D0 * QELEM
          QWA%ADR(N)%P%R(I+1) = QWA%ADR(N)%P%R(I+1) - 0.5D0 * QELEM
          QWB%ADR(N)%P%R(I)   = QWB%ADR(N)%P%R(I  ) + 0.5D0 * QELEM
          QWB%ADR(N)%P%R(I+1) = QWB%ADR(N)%P%R(I+1) + 0.5D0 * QELEM
!
! COMPUTATION OF TRACERS ACCORDING THE COMPUTED DISCHARGE
!
          IF(NTRAC.GT.0) THEN
            INDIC = (N-1)*MAXNPS + I
            DO ITRAC=1,NTRAC
              IF(QELEM.GT.0.D0) THEN ! A --> B
                IF(IA1.GT.0) THEN
                  TWEIRA%ADR(ITRAC)%P%R(INDIC  ) = TRAC_A(ITRAC) *
     &               0.5D0 * QELEM + TWEIRA%ADR(ITRAC)%P%R(INDIC  )
                  TWEIRA%ADR(ITRAC)%P%R(INDIC+1) = TRAC_C(ITRAC) *
     &               0.5D0 * QELEM + TWEIRA%ADR(ITRAC)%P%R(INDIC+1)
                  TWEIRB%ADR(ITRAC)%P%R(INDIC  ) = TRAC_A(ITRAC) *
     &               0.5D0 * QELEM + TWEIRB%ADR(ITRAC)%P%R(INDIC  )
                  TWEIRB%ADR(ITRAC)%P%R(INDIC+1) = TRAC_C(ITRAC) *
     &               0.5D0 * QELEM + TWEIRB%ADR(ITRAC)%P%R(INDIC+1)
                ELSE
                  TWEIRA%ADR(ITRAC)%P%R(INDIC  ) = 0.D0 +
     &               TWEIRA%ADR(ITRAC)%P%R(INDIC  )
                  TWEIRA%ADR(ITRAC)%P%R(INDIC+1) = 0.D0 +
     &               TWEIRA%ADR(ITRAC)%P%R(INDIC+1)
                  TWEIRB%ADR(ITRAC)%P%R(INDIC  ) = 0.D0 +
     &               TWEIRB%ADR(ITRAC)%P%R(INDIC  )
                  TWEIRB%ADR(ITRAC)%P%R(INDIC+1) = 0.D0 +
     &               TWEIRB%ADR(ITRAC)%P%R(INDIC+1)
                ENDIF
              ELSEIF(QELEM.LT.0.D0) THEN ! B --> A
                IF(IB1.GT.0) THEN
                  TWEIRA%ADR(ITRAC)%P%R(INDIC  ) = -TRAC_B(ITRAC) *
     &               0.5D0 * QELEM + TWEIRA%ADR(ITRAC)%P%R(INDIC  )
                  TWEIRA%ADR(ITRAC)%P%R(INDIC+1) = -TRAC_D(ITRAC) *
     &               0.5D0 * QELEM + TWEIRA%ADR(ITRAC)%P%R(INDIC+1)
                  TWEIRB%ADR(ITRAC)%P%R(INDIC  ) = -TRAC_B(ITRAC) *
     &               0.5D0 * QELEM + TWEIRB%ADR(ITRAC)%P%R(INDIC  )
                  TWEIRB%ADR(ITRAC)%P%R(INDIC+1) = -TRAC_D(ITRAC) *
     &               0.5D0 * QELEM + TWEIRB%ADR(ITRAC)%P%R(INDIC+1)
                ELSE
                  TWEIRA%ADR(ITRAC)%P%R(INDIC  ) = 0.D0 +
     &               TWEIRA%ADR(ITRAC)%P%R(INDIC  )
                  TWEIRA%ADR(ITRAC)%P%R(INDIC+1) = 0.D0 +
     &               TWEIRA%ADR(ITRAC)%P%R(INDIC+1)
                  TWEIRB%ADR(ITRAC)%P%R(INDIC  ) = 0.D0 +
     &               TWEIRB%ADR(ITRAC)%P%R(INDIC  )
                  TWEIRB%ADR(ITRAC)%P%R(INDIC+1) = 0.D0 +
     &               TWEIRB%ADR(ITRAC)%P%R(INDIC+1)
                ENDIF
              ELSE ! No Flow
                  TWEIRA%ADR(ITRAC)%P%R(INDIC  ) = 0.D0 +
     &               TWEIRA%ADR(ITRAC)%P%R(INDIC  )
                  TWEIRA%ADR(ITRAC)%P%R(INDIC+1) = 0.D0 +
     &               TWEIRA%ADR(ITRAC)%P%R(INDIC+1)
                  TWEIRB%ADR(ITRAC)%P%R(INDIC  ) = 0.D0 +
     &               TWEIRB%ADR(ITRAC)%P%R(INDIC  )
                  TWEIRB%ADR(ITRAC)%P%R(INDIC+1) = 0.D0 +
     &               TWEIRB%ADR(ITRAC)%P%R(INDIC+1)
              ENDIF
              IF(NCSIZE.GT.1) THEN
                TWEIRA%ADR(ITRAC)%P%R(INDIC)=
     &            P_DMAX(MAX( TWEIRA%ADR(ITRAC)%P%R(INDIC)  ,0.D0))
     &           -P_DMIN(MAX(-TWEIRA%ADR(ITRAC)%P%R(INDIC)  ,0.D0))
                TWEIRA%ADR(ITRAC)%P%R(INDIC+1)=
     &            P_DMAX(MAX( TWEIRA%ADR(ITRAC)%P%R(INDIC+1),0.D0))
     &           -P_DMIN(MAX(-TWEIRA%ADR(ITRAC)%P%R(INDIC+1),0.D0))
                TWEIRB%ADR(ITRAC)%P%R(INDIC)=
     &            P_DMAX(MAX( TWEIRB%ADR(ITRAC)%P%R(INDIC)  ,0.D0))
     &           -P_DMIN(MAX(-TWEIRB%ADR(ITRAC)%P%R(INDIC)  ,0.D0))
                TWEIRB%ADR(ITRAC)%P%R(INDIC+1)=
     &            P_DMAX(MAX( TWEIRB%ADR(ITRAC)%P%R(INDIC+1),0.D0))
     &           -P_DMIN(MAX(-TWEIRB%ADR(ITRAC)%P%R(INDIC+1),0.D0))
              ENDIF
            ENDDO
          ENDIF
!
! COMPUTATION OF VELOCITIES
!
!
!         CALCULATES THE TANGENTIAL VELOCITY
!
          IF(IOPTAN.EQ.0) THEN
            UTANA = 0.D0
            UTANB = 0.D0
          ELSEIF(IOPTAN.EQ.1) THEN
            HSA = MAX(SLA - 0.5D0 * (YS1 + YS2), 0.D0)
            HSB = MAX(SLB - 0.5D0 * (YS1 + YS2), 0.D0)
            PENTA = (SL_C2 - SL_A1) / DLA
            PENTB = (SL_D2 - SL_B1) / DLB
            CALL CALCUL_TANG_W2(IA1,NKFROT,CHESTR,HSA,PENTA,KARMAN,
     &         UTANA)
            CALL CALCUL_TANG_W2(IC2,NKFROT,CHESTR,HSA,PENTA,KARMAN,
     &         UTANC)
            CALL CALCUL_TANG_W2(IB1,NKFROT,CHESTR,HSB,PENTB,KARMAN,
     &         UTANB)
            CALL CALCUL_TANG_W2(ID2,NKFROT,CHESTR,HSB,PENTB,KARMAN,
     &         UTAND)
          ELSE
            IF (LNG.EQ.1) THEN
              WRITE(LU,*)'CLHUVT : OPTION INCONNUE :',IOPTAN
              WRITE(LU,*)'         POUR LES VITESSES TANGENTIELLES'
            ELSEIF(LNG.EQ.2) THEN
              WRITE(LU,*)'CLHUVT : UNKNOWN OPTION:',IOPTAN
              WRITE(LU,*)'         FOR THE TANGENTIAL VELOCITY'
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
!
!         ONE CALCULATES VELOCITY COMPONENTS U AND V
!         IN THE ORDINARY COORDINATE SYSTEM (X,Y).
!
          IF(ABS(QELEM).GT.0.D0) THEN
            UWEIRA%ADR(N)%P%R(I)   = UTANA * TXA + QELEM * NXA / HA +
     &         UWEIRA%ADR(N)%P%R(I)
            VWEIRA%ADR(N)%P%R(I)   = UTANA * TYA + QELEM * NYA / HA +
     &         VWEIRA%ADR(N)%P%R(I)
            UWEIRA%ADR(N)%P%R(I+1) = UTANC * TXA + QELEM * NXA / HA +
     &         UWEIRA%ADR(N)%P%R(I+1)
            VWEIRA%ADR(N)%P%R(I+1) = UTANC * TYA + QELEM * NYA / HA +
     &         VWEIRA%ADR(N)%P%R(I+1)
            UWEIRB%ADR(N)%P%R(I)   = UTANB * TXB + QELEM * NXB / HB +
     &         UWEIRB%ADR(N)%P%R(I)
            VWEIRB%ADR(N)%P%R(I)   = UTANB * TYB + QELEM * NYB / HB +
     &         VWEIRB%ADR(N)%P%R(I)
            UWEIRB%ADR(N)%P%R(I+1) = UTAND * TXB + QELEM * NXB / HB +
     &         UWEIRB%ADR(N)%P%R(I+1)
            VWEIRB%ADR(N)%P%R(I+1) = UTAND * TYB + QELEM * NYB / HB +
     &         VWEIRB%ADR(N)%P%R(I+1)
          ELSE
            UWEIRA%ADR(N)%P%R(I)   = 0.D0 + UWEIRA%ADR(N)%P%R(I)
            VWEIRA%ADR(N)%P%R(I)   = 0.D0 + VWEIRA%ADR(N)%P%R(I)
            UWEIRA%ADR(N)%P%R(I+1) = 0.D0 + UWEIRA%ADR(N)%P%R(I+1)
            VWEIRA%ADR(N)%P%R(I+1) = 0.D0 + VWEIRA%ADR(N)%P%R(I+1)
            UWEIRB%ADR(N)%P%R(I)   = 0.D0 + UWEIRB%ADR(N)%P%R(I)
            VWEIRB%ADR(N)%P%R(I)   = 0.D0 + VWEIRB%ADR(N)%P%R(I)
            UWEIRB%ADR(N)%P%R(I+1) = 0.D0 + UWEIRB%ADR(N)%P%R(I+1)
            VWEIRB%ADR(N)%P%R(I+1) = 0.D0 + VWEIRB%ADR(N)%P%R(I+1)
          ENDIF
        ENDDO
      ENDDO
!
! THE DISCHARGE IS NOW COMPUTED ON ALL NODES OF WEIRS
! SO WE COULD COMPUTE AN AVERAGE VALUE OF CONCENTRATION OF TRACERS
!
      IF(NTRAC.GT.0) THEN
        DO N = 1, NWEIRS
          DO I = 1 ,NPSING%I(N)
            INDIC = (N-1)*MAXNPS + I
            QELEM = ABS(QWA%ADR(N)%P%R(I))
            IF(QELEM.GT.0.D0) THEN
              DENOM = 1.D0 / QELEM
              DO ITRAC = 1, NTRAC
                TWEIRA%ADR(ITRAC)%P%R(INDIC) = DENOM *
     &             TWEIRA%ADR(ITRAC)%P%R(INDIC)
                IF(NCSIZE.GT.1) THEN
                  TWEIRA%ADR(ITRAC)%P%R(INDIC)=
     &              P_DMAX(MAX( TWEIRA%ADR(ITRAC)%P%R(INDIC),0.D0))
     &             -P_DMIN(MAX(-TWEIRA%ADR(ITRAC)%P%R(INDIC),0.D0))
                ENDIF
              ENDDO
            ENDIF
            QELEM = ABS(QWB%ADR(N)%P%R(I))
            IF(QELEM.GT.0.D0) THEN
              DENOM = 1.D0 / QELEM
              DO ITRAC = 1, NTRAC
                TWEIRB%ADR(ITRAC)%P%R(INDIC) = DENOM *
     &             TWEIRB%ADR(ITRAC)%P%R(INDIC)
                IF(NCSIZE.GT.1) THEN
                  TWEIRB%ADR(ITRAC)%P%R(INDIC)=
     &              P_DMAX(MAX( TWEIRB%ADR(ITRAC)%P%R(INDIC),0.D0))
     &             -P_DMIN(MAX(-TWEIRB%ADR(ITRAC)%P%R(INDIC),0.D0))
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ENDDO
      ENDIF
!
! END OF COMPUTATION OF TRACERS
!
      RETURN
      END
