!                    *****************
                     SUBROUTINE CALCOT
!                    *****************
!
     &(ZZ,HH)
!
!***********************************************************************
! TELEMAC3D   V6P1                                   21/08/2010
!***********************************************************************
!
!brief    BUILDS THE ARRAY OF THE ELEVATIONS OF THE MESH.
!
!history  JACEK A. JANKOWSKI PINXIT
!+        **/03/1999
!+
!+   FORTRAN95 VERSION
!
!history  J-M HERVOUET (LNHE)     ; F LEPEINTRE (LNH)    ; J-M JANIN (LNH)
!+        11/03/2010
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
!| HH             |-->| WATER DEPTH
!| ZZ             |<->| ELEVATION OF MESH POINTS
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_TELEMAC3D
      USE INTERFACE_TELEMAC3D, EX_CALCOT => CALCOT
!
      USE DECLARATIONS_SPECIAL
      IMPLICIT NONE
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION, INTENT(IN)    :: HH(NPOIN2)
      DOUBLE PRECISION, INTENT(INOUT) :: ZZ(NPOIN2,NPLAN)
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      DOUBLE PRECISION RPLS,RPLI,ZFP,ZSP,DISBOT,DISSUR
      DOUBLE PRECISION DISMIN_BOT,DISMIN_SUR,MIN_DZ
      INTEGER IPOIN,IPLAN,I1,I2,ITRAC
!
!***********************************************************************
!
!     HARDCODED
!
      DISMIN_SUR = 0.2D0
      DISMIN_BOT = 0.2D0
      MIN_DZ     = 0.D0
!
!     1) IN ALL CASES: FREE SURFACE = BOTTOM+DEPTH
!
      IF(OPTBAN.EQ.1.AND.OPT_HNEG.NE.2) THEN
        DO IPOIN = 1,NPOIN2
          ZZ(IPOIN,NPLAN) = ZZ(IPOIN,1) + MAX(HH(IPOIN),0.D0)
        ENDDO
      ELSE
        DO IPOIN = 1,NPOIN2
          ZZ(IPOIN,NPLAN) = ZZ(IPOIN,1) + HH(IPOIN)
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
!     HERE IMPLEMENTATION BY USER
!
      IF(TRANSF.EQ.0) THEN
!
        IF(LNG.EQ.1) WRITE(LU,81)
        IF(LNG.EQ.2) WRITE(LU,82)
81      FORMAT('CALCOT: TRANSFORMATION A PROGRAMMER PAR L''UTILISATEUR')
82      FORMAT('CALCOT: TRANSFORMATION TO BE PROGRAMMED BY USER')
        CALL PLANTE(1)
        STOP
!
!-----------------------------------------------------------------------
!
!     ADAPTIVE MESH REFINEMENT (BY CHRIS CAWTHORN)
!
      ELSEIF(TRANSF.EQ.5.AND.AT.GT.1.D-4) THEN
!
!       ITRAC: CHOICE OF TRACER FOR ADAPTIVE MESH
        ITRAC=1
        CALL AMR_PLAN(ZZ,TA%ADR(ITRAC)%P%R,'A',NPOIN2,NPLAN,
     &                MESH2D%NSEG,MESH2D%GLOSEG%I,MESH2D%GLOSEG%DIM1,
     &                T3_01%R,T3_02%R,T3_03%R,T3_04%R,T3_05%R,T3_06,
     &                T3_06%R,IT1%I,T2_01,T2_01%R,T2_02%R,MESH2D,MESH3D)
!
!-----------------------------------------------------------------------
!
!     NOW ALL OTHER CASES: SEQUENCES OF SIGMA TRANSFORMATIONS
!                          AND PLANES WITH PRESCRIBED ELEVATION
!
      ELSEIF(NPLAN.GT.2) THEN
!
!-----------------------------------------------------------------------
!
!       2) SETS THE PLANES WITH PRESCRIBED ELEVATION
!
        DO IPLAN=2,NPLAN-1
          IF(TRANSF_PLANE%I(IPLAN).EQ.3) THEN
!           IF NOT POSSIBLE BECAUSE OF FREE SURFACE OR BOTTOM, A SECURITY
!           DISTANCE, DISMIN, IS USED. ALL PLANES THAT WOULD CROSS E.G.
!           THE BOTTOM AVOID IT AT A DISTANCE DISMIN*RPLI, SEE RPLI BELOW
            RPLS = DBLE(NPLAN-IPLAN) / DBLE(NPLAN)
            RPLI = DBLE(IPLAN-    1) / DBLE(NPLAN)
            DO IPOIN = 1,NPOIN2
              ZFP = ZZ(IPOIN,1)
              ZSP = ZZ(IPOIN,NPLAN)
              DISBOT = MIN(ZSP-ZFP,DISMIN_BOT)
              DISSUR = MIN(ZSP-ZFP,DISMIN_SUR)
              ZZ(IPOIN,IPLAN)=MIN(                    ZSP-DISSUR*RPLS,
     &                            MAX(ZPLANE%R(IPLAN),ZFP+DISBOT*RPLI))
            ENDDO
          ENDIF
        ENDDO
!
!       3) SETS THE PLANES WITH SIGMA TRANSFORMATION
!
        I1=2
        DO WHILE(I1.NE.NPLAN)
          IF(TRANSF_PLANE%I(I1).EQ.3) THEN
            I1=I1+1
          ELSE
!           LOOKS FOR SEQUENCES OF SIGMA TRANSFORMATION PLANES
            I2=I1
            DO WHILE(TRANSF_PLANE%I(I2+1).NE.3.AND.I2+1.NE.NPLAN)
              I2=I2+1
            ENDDO
!           SIGMA TRANSFORMATION FOR PLANES I1 TO I2
!           BETWEEN ALREADY TREATED PLANES I1-1 AND I2+1
            DO IPLAN=I1,I2
              IF(TRANSF_PLANE%I(IPLAN).EQ.1) THEN
                ZSTAR%R(IPLAN)=FLOAT(IPLAN-I1+1)/FLOAT(I2-I1+2)
!             ELSE
!               ZSTAR%R(IPLAN) HAS BEEN GIVEN BY USER IN CONDIM
              ENDIF
              DO IPOIN = 1,NPOIN2
                ZZ(IPOIN,IPLAN) = ZZ(IPOIN,I1-1)
     &                          + ZSTAR%R(IPLAN)*(  ZZ(IPOIN,I2+1)
     &                                             -ZZ(IPOIN,I1-1) )
              ENDDO
            ENDDO
            I1=I2+1
          ENDIF
        ENDDO
!
!       4) CHECKS
!
        IF(NPLAN.GT.2) THEN
          DO IPLAN=2,NPLAN-1
            DO IPOIN = 1,NPOIN2
              IF(ZZ(IPOIN,IPLAN).LT.ZZ(IPOIN,IPLAN-1)) THEN
                IF(LNG.EQ.1) THEN
                  WRITE(LU,*) 'CALCOT : LES PLANS ',IPLAN-1,' ET ',IPLAN
                  WRITE(LU,*) '         SE CROISENT AU POINT ',IPOIN
                  WRITE(LU,*) '         COTE BASSE : ',ZZ(IPOIN,IPLAN-1)
                  WRITE(LU,*) '         COTE HAUTE : ',ZZ(IPOIN,IPLAN)
                  WRITE(LU,*) '         DIFFERENCE : ',ZZ(IPOIN,IPLAN)-
     &                                                 ZZ(IPOIN,IPLAN-1)
                  WRITE(LU,*) '         HAUTEUR    : ',HH(IPOIN)
                ENDIF
                 IF(LNG.EQ.2) THEN
                  WRITE(LU,*) 'CALCOT: PLANES ',IPLAN-1,' AND ',IPLAN
                  WRITE(LU,*) '        INTERCROSS AT POINT ',IPOIN
                  WRITE(LU,*) '        LOWER POINT : ',ZZ(IPOIN,IPLAN-1)
                  WRITE(LU,*) '        HIGHER POINT: ',ZZ(IPOIN,IPLAN)
                  WRITE(LU,*) '        DIFFERENCE  : ',ZZ(IPOIN,IPLAN)-
     &                                                 ZZ(IPOIN,IPLAN-1)
                  WRITE(LU,*) '        DEPTH       : ',HH(IPOIN)
                ENDIF
                CALL PLANTE(1)
                STOP
              ENDIF
            ENDDO
          ENDDO
        ENDIF
!
!       5) A POINT THAT IS TOO CLOSE TO THE LOWER ONE ON A VERTICAL
!          IS PUT ON THE LOWER, I.E. A MINIMUM HEIGHT IS PRESCRIBED
!          IN ELEMENTS, OTHERS ARE FRANKLY SMASHED. THIS IS NOT DONE
!          FOR FREE SURFACE.
!
        IF(NPLAN.GT.2.AND.MIN_DZ.GT.0.D0) THEN
          DO IPLAN=2,NPLAN-1
            DO IPOIN = 1,NPOIN2
              IF(ZZ(IPOIN,IPLAN).LT.ZZ(IPOIN,IPLAN-1)+MIN_DZ) THEN
                ZZ(IPOIN,IPLAN)=ZZ(IPOIN,IPLAN-1)
              ENDIF
            ENDDO
          ENDDO
        ENDIF
!
!-----------------------------------------------------------------------
!
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
