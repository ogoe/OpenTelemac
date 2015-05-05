!                       ****************** 
                        SUBROUTINE BREACH 
!                       ****************** 
! 
! 
!*********************************************************************** 
! TELEMAC2D   V6P2                                   03/08/2012 
!*********************************************************************** 
! 
!brief    MODIFICATION OF THE BOTTOM TOPOGRAPHY FOR BREACHES 
! 
! 
!history  P. CHASSE (CETMEF) / C. COULET (ARTELIA) 
!+        03/08/2012 
!+        V6P2 
!+        Creation 
!
!history  Y.B. TADESSE (TUHH, INSTITUTE OF RIVER AND COASTAL ENGINEERING)
!+        14/02/2014
!+        V6P3R2
!+   Addition of later breach growth option      
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
      LOGICAL, SAVE :: DEJALU=.FALSE. 
! 
      INTEGER I, J, K, N, M,END1, END2,CURNBR
      INTEGER ISTAT,VECZ
      INTEGER TEMPND(NPOIN)
      DOUBLE PRECISION ZC, ZW, ZB 
      DOUBLE PRECISION AT1, AT2, AT3
!      
      DOUBLE PRECISION X1, X2, Y1, Y2, DX, DY,DS1,DS2,END1X,END1Y
      DOUBLE PRECISION U1, U2, V1, V2, DELS,CURDIS,DIS,END2X,END2Y
      DOUBLE PRECISION, DIMENSION (:), ALLOCATABLE :: XL, YL, XP, YP
! 
      INTEGER          P_ISUM,P_IMAX,P_IMIN 
      DOUBLE PRECISION P_DMAX,P_DMIN,P_DSUM 
      EXTERNAL         P_ISUM,P_DMAX,P_DMIN,P_DSUM,P_IMAX,P_IMIN
! 
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ 
! 
      IF (.NOT.DEJALU) THEN 
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING LECBREACH' 
        CALL LECBREACH(T2D_FILES(T2DBRC)%LU) 
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM LECBREACH' 
        DEJALU=.TRUE. 
        IF(LNG.EQ.1) WRITE (LU,*) 'LECTURE DONNEES BRECHE = OK'
        IF(LNG.EQ.2) WRITE (LU,*) 'READING BREACH DATA = OK'  
! 
        DO I = 1, NBRECH 
          ZCRBR%R(I) = -HUGE(100.D0) 
          DO J = 1, NBNDBR%I(I) 
            K = INDBR%ADR(I)%P%I(J) 
            IF(ZF%R(K).GT.ZCRBR%R(I)) THEN 
              ZCRBR%R(I) = ZF%R(K) 
            ENDIF 
          ENDDO 
          IF(NCSIZE.GT.1) THEN 
            ZCRBR%R(I) = P_DMAX(ZCRBR%R(I)) 
          ENDIF 
        ENDDO 
      ENDIF 
! 
      DO I = 1, NBRECH 
        IF (OPTERO%I(I).EQ.1) THEN
          IF((OPTNBR%I(I).EQ.2).AND.(TDECBR%R(I).LT.0.D0)) THEN 
            ZC = 0.D0 
            N = 0 
            DO J = 1, NBNDBR%I(I) 
              K = INDBR%ADR(I)%P%I(J) 
              IF(H%R(K).GT.0.D0) THEN 
                ZW = ZF%R(K)+H%R(K) 
                ZC = ZC + ZW 
                N = N + 1 
              ENDIF 
            ENDDO 
            IF(NCSIZE.GT.1) THEN 
              N = P_ISUM(N) 
              ZC = P_DSUM(ZC) 
            ENDIF 
            IF(N.GT.1) ZC = ZC/N 
            IF(ZC.GT.ZDECBR%R(I)) THEN 
              IF(LNG.EQ.1) WRITE(LU,10) I, AT 
              IF(LNG.EQ.2) WRITE(LU,20) I, AT 
              TDECBR%R(I) = AT 
            ENDIF 
          ENDIF 
          IF((OPTNBR%I(I).EQ.3).AND.(TDECBR%R(I).LT.0.D0)) THEN 
            IF(NUMPSD%I(I).GT.0) THEN 
              ZW = ZF%R(NUMPSD%I(I)) + H%R(NUMPSD%I(I)) 
            ELSE 
              ZW = 0.D0 
            ENDIF 
!           CASE WHERE ONE OF THE ENDS IS NOT IN THE SUB-DOMAIN 
            IF(NCSIZE.GT.1) THEN 
              ZW = P_DMAX(ZW)+P_DMIN(ZW) 
            ENDIF 
            IF(ZW.GT.ZDECBR%R(I)) THEN 
              IF(LNG.EQ.1) WRITE(LU,10) I, AT 
              IF(LNG.EQ.2) WRITE(LU,20) I, AT 
              TDECBR%R(I) = AT 
            ENDIF 
          ENDIF 
          AT1 = TDECBR%R(I) 
          AT2 = AT1 + DURBR%R(I) 
          IF(AT1.GT.0.D0) THEN 
            IF(AT.GT.AT1) THEN 
              IF(AT.GT.AT2) THEN 
                ZB = ZFINBR%R(I) 
              ELSE 
                ZB = ZCRBR%R(I)+(ZFINBR%R(I)-ZCRBR%R(I))/(AT2-AT1) 
     &             *(AT-AT1) 
              ENDIF 
              DO J = 1, NBNDBR%I(I) 
                K = INDBR%ADR(I)%P%I(J) 
                ZF%R(K)=MIN(ZF%R(K), ZB) 
              ENDDO 
            ENDIF 
          ENDIF 
        ELSEIF (OPTERO%I(I).EQ.2) THEN
!       FIND NUMBER POINTS IN THE CURRENT BREACH REGION
          CURDIS=0.D0
          END1X=0.D0
          END1Y=0.D0
          END2X=0.D0
          END2Y=0.D0
          END1=0
          END2=0
          FIRSTEND: DO M=1,NPONBR%I(I)-1
            CURDIS=CURDIS+PONDSB%ADR(I)%P%R(M)
            IF (CURDIS .GT. ((FINBRW%R(I)-CURBRW%R(I))/2.D0)) THEN
              END1=M+1
              DS1= CURDIS - (FINBRW%R(I)-CURBRW%R(I))/2.D0
              DX = DKAXCR%ADR(I)%P%R(M+1) - DKAXCR%ADR(I)%P%R(M)
              DY = DKAYCR%ADR(I)%P%R(M+1) - DKAYCR%ADR(I)%P%R(M)
              U1 = DX/PONDSB%ADR(I)%P%R(M)
              U2 = DY/PONDSB%ADR(I)%P%R(M)
              IF(CURBRW%R(I).LT.FINBRW%R(I)) THEN
                END1X = DKAXCR%ADR(I)%P%R(M+1) - U1*DS1
                END1Y = DKAYCR%ADR(I)%P%R(M+1) - U2*DS1
              ELSEIF(CURBRW%R(I).EQ.FINBRW%R(I)) THEN
                END1X = DKAXCR%ADR(I)%P%R(1)- U1*
     &          (PONDSB%ADR(I)%P%R(1)/10.D0)
                END1Y = DKAYCR%ADR(I)%P%R(1)- U2*
     &          (PONDSB%ADR(I)%P%R(1)/10.D0)                
              ELSE
                IF(LNG.EQ.1) WRITE(LU,300)
                IF(LNG.EQ.2) WRITE(LU,400)
                STOP
              ENDIF
              DIS=DS1
              SECONDEND: DO J=M+1,NPONBR%I(I)-1
                DIS=DIS+PONDSB%ADR(I)%P%R(J)
                IF (DIS .GE. CURBRW%R(I)) THEN
                  END2=J
                  DS2=DIS - CURBRW%R(I)
                  DX = DKAXCR%ADR(I)%P%R(J+1) - DKAXCR%ADR(I)%P%R(J)
                  DY = DKAYCR%ADR(I)%P%R(J+1) - DKAYCR%ADR(I)%P%R(J)
                  U1 = DX/PONDSB%ADR(I)%P%R(J)
                  U2 = DY/PONDSB%ADR(I)%P%R(J)
                  IF(CURBRW%R(I).LT.FINBRW%R(I)) THEN
                    END2X = DKAXCR%ADR(I)%P%R(J+1) - U1*DS2
                    END2Y = DKAYCR%ADR(I)%P%R(J+1) - U2*DS2
                  ELSEIF (CURBRW%R(I).EQ.FINBRW%R(I)) THEN
                    END2X = DKAXCR%ADR(I)%P%R(NPONBR%I(I))+ U1*
     &              (PONDSB%ADR(I)%P%R(NPONBR%I(I)-1)/10.D0)
                    END2Y = DKAYCR%ADR(I)%P%R(NPONBR%I(I))+ U2*
     &              (PONDSB%ADR(I)%P%R(NPONBR%I(I)-1)/10.D0)
                  ELSE
                    IF(LNG.EQ.1) WRITE(LU,300) I
                    IF(LNG.EQ.2) WRITE(LU,400) I
                    STOP
                  ENDIF
                 EXIT SECONDEND
                ENDIF
              END DO SECONDEND
!                           
              EXIT FIRSTEND
            ENDIF
          ENDDO FIRSTEND
300       FORMAT(1X, 'BRECHE: BRECHE LARGEUR NE PEUT ETRE SUPERIEURE ',
     &           'A LARGEUR FINALE BRECHE: ', 1I6)
400       FORMAT(1X, 'BREACH: BREACH WIDTH CANNOT BE GREATER ',
     &           'THAN FINAL WIDTH FOR BREACH: ', 1I6)
!         ALLOCATION OF LOCAL VARIABLE FOR BREACH DEFINITION
          ISTAT = 0
          VECZ=END2-END1+3
          ALLOCATE(XL(VECZ), STAT=ISTAT)
          IF(ISTAT.NE.0) THEN
            IF(LNG.EQ.1) WRITE(LU,100) ISTAT
            IF(LNG.EQ.2) WRITE(LU,200) ISTAT
            STOP
          ENDIF
          ALLOCATE(YL(VECZ), STAT=ISTAT)
          IF(ISTAT.NE.0) THEN
            IF(LNG.EQ.1) WRITE(LU,100) ISTAT
            IF(LNG.EQ.2) WRITE(LU,200) ISTAT
            STOP
          ENDIF
!
100       FORMAT(1X,'ERREUR A L''ALLOCATION DU VECTEUR : ',
     &           'CODE D''ERREUR : ',1I6)
200       FORMAT(1X,'ERROR DURING ALLOCATION OF VECTOR: ',
     &           'ERROR CODE: ',1I6)
!
          XL(1)=END1X
          YL(1)=END1Y
          XL(VECZ)=END2X
          YL(VECZ)=END2Y
          DO J=2,VECZ-1
            XL(J)=DKAXCR%ADR(I)%P%R(END1+J-2)
            YL(J)=DKAYCR%ADR(I)%P%R(END1+J-2)
          ENDDO
!         SEARCH MESH POINTS INSIDE THE BREACH DOMAIN
          ISTAT = 0
          ALLOCATE(XP(2*VECZ), STAT=ISTAT)
          IF(ISTAT.NE.0) THEN
            IF(LNG.EQ.1) WRITE(LU,100) ISTAT
            IF(LNG.EQ.2) WRITE(LU,200) ISTAT
            STOP
          ENDIF
          ALLOCATE(YP(2*VECZ), STAT=ISTAT)
          IF(ISTAT.NE.0) THEN
            IF(LNG.EQ.1) WRITE(LU,100) ISTAT
            IF(LNG.EQ.2) WRITE(LU,200) ISTAT
            STOP
          ENDIF
!
          X1 = XL(1)
          Y1 = YL(1)
          X2 = XL(2)
          Y2 = YL(2)
          DX = X2 - X1
          DY = Y2 - Y1
          DELS=DSQRT(DX*DX+DY*DY)
          IF(DELS.GE.0.D0) THEN
            U1 = DX/DELS
            U2 = DY/DELS
          ELSE
            IF(LNG.EQ.1)
     &        WRITE(LU,*) 'PROBLEME DANS LA DEFINITION DE LA BRECHE :',I
            IF(LNG.EQ.2)
     &        WRITE(LU,*) 'PROBLEM IN DEFINITION OF BREACH :',I
              CALL PLANTE(1)
          ENDIF
          V1 = -U2
          V2 = U1
          XP(1)      = X1 + V1*POLWDT%R(I)/2.D0
          YP(1)      = Y1 + V2*POLWDT%R(I)/2.D0
          XP(2*VECZ) = X1 - V1*POLWDT%R(I)/2.D0
          YP(2*VECZ) = Y1 - V2*POLWDT%R(I)/2.D0
!
          DO M = 2,VECZ
            X2 = XL(M)
            Y2 = YL(M)
            DX = X2 - X1
            DY = Y2 - Y1
            DELS=DSQRT(DX*DX+DY*DY)
            IF(DELS.GE.0.D0) THEN
              U1 = DX/DELS
              U2 = DY/DELS             
            ELSE
            IF(LNG.EQ.1)
     &       WRITE(LU,*) 'PROBLEME DANS LA DEFINITION DE LA BRECHE :',I
            IF(LNG.EQ.2)
     &        WRITE(LU,*) 'PROBLEM IN DEFINITION OF BREACH :',I
            CALL PLANTE(1)
          ENDIF
          V1 = -U2
          V2 = U1
          XP(M)         = X2 + V1*POLWDT%R(I)/2.D0
          YP(M)         = Y2 + V2*POLWDT%R(I)/2.D0
          XP(2*VECZ-M+1) = X2 - V1*POLWDT%R(I)/2.D0
          YP(2*VECZ-M+1) = Y2 - V2*POLWDT%R(I)/2.D0
          X1=X2
          Y1=Y2
        ENDDO
        CURNBR = 0
        DO M = 1, NPOIN
          IF(INPOLY(MESH%X%R(M), MESH%Y%R(M), XP, YP, 2*VECZ)) THEN
            CURNBR = CURNBR +1
            TEMPND(CURNBR) = M
          ENDIF
        ENDDO
!
        IF((OPTNBR%I(I).EQ.2).AND.(TDECBR%R(I).LT.0.D0)) THEN 
          ZC = 0.D0 
          N = 0 
          DO J = 1, CURNBR 
            K = TEMPND(J) 
            IF(H%R(K).GT.0.D0) THEN 
              ZW = ZF%R(K)+H%R(K) 
              ZC = ZC + ZW 
              N = N + 1 
            ENDIF 
          ENDDO 
          IF(NCSIZE.GT.1) THEN 
            N = P_ISUM(N) 
            ZC = P_DSUM(ZC) 
          ENDIF 
          IF(N.GT.1) ZC = ZC/N 
          IF(ZC.GT.ZDECBR%R(I)) THEN 
            IF(LNG.EQ.1) WRITE(LU,10) I, AT 
            IF(LNG.EQ.2) WRITE(LU,20) I, AT 
            TDECBR%R(I) = AT
          ENDIF 
          ENDIF 
          IF((OPTNBR%I(I).EQ.3).AND.(TDECBR%R(I).LT.0.D0)) THEN 
            IF(NUMPSD%I(I).GT.0) THEN 
              ZW = ZF%R(NUMPSD%I(I)) + H%R(NUMPSD%I(I)) 
            ELSE 
              ZW = 0.D0 
            ENDIF 
!           CASE WHERE ONE OF THE ENDS IS NOT IN THE SUB-DOMAIN 
            IF(NCSIZE.GT.1) THEN 
              ZW = P_DMAX(ZW)+P_DMIN(ZW) 
            ENDIF 
            IF(ZW.GT.ZDECBR%R(I)) THEN 
              IF(LNG.EQ.1) WRITE(LU,10) I, AT 
              IF(LNG.EQ.2) WRITE(LU,20) I, AT 
              TDECBR%R(I) = AT
            ENDIF 
          ENDIF 
!
!         DURATION FOR THE VERTICAL EROSION IS TAKEN AS A TENTH
!         OF THE TOTAL BREACH DURATION
          AT1 = TDECBR%R(I) 
          AT2 = AT1 + DURBR%R(I)
          AT3= AT1 + DURBR%R(I)/10.D0
          IF(AT1.GT.0.D0) THEN 
            IF(AT.GT.AT1) THEN
              IF(AT.GT.AT3) THEN
                ZB = ZFINBR%R(I) 
              ELSE 
                ZB = ZCRBR%R(I)+(ZFINBR%R(I)-ZCRBR%R(I))/(AT3-AT1) 
     &          *(AT-AT1)
              ENDIF
              IF(AT.GT.AT2) THEN            
                CURBRW%R(I)=FINBRW%R(I) 
              ELSE 
                CURBRW%R(I)=INIBRW%R(I)+(FINBRW%R(I)-INIBRW%R(I))
     &           /(AT2-AT1)*(AT-AT1)
              ENDIF 
              DO J = 1, CURNBR 
                K = TEMPND(J) 
                ZF%R(K)=MIN(ZF%R(K), ZB) 
              ENDDO 
            ENDIF 
          ENDIF 
          DEALLOCATE(XL)
          DEALLOCATE(YL)
          DEALLOCATE(XP)
          DEALLOCATE(YP)
        ENDIF
      ENDDO 
! 
!----------------------------------------------------------------------- 
!     MESSAGES 
10    FORMAT(1X,'CREATION DE LA BRECHE : ',I4,/,1X, 
     &          'AU TEMPS : ',G16.7) 
20    FORMAT(1X,'CREATION OF BREACH : ',I4,/,1X, 
     &          'AT TIME : ',G16.7) 
!----------------------------------------------------------------------- 
! 
      RETURN 
      END                   
  
