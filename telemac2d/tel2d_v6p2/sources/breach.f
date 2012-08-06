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
      INTEGER I, J, K, N
      DOUBLE PRECISION Zc, Zw, Zb
      DOUBLE PRECISION AT1, AT2
!
      INTEGER          P_ISUM
      DOUBLE PRECISION P_DMAX,P_DMIN,P_DSUM
      EXTERNAL         P_ISUM,P_DMAX,P_DMIN,P_DSUM
!
!+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
!
      IF (.NOT.DEJALU) THEN
        IF(DEBUG.GT.0) WRITE(LU,*) 'CALLING LECBREACH'
        CALL LECBREACH(T2D_FILES(T2DBRC)%LU)
        IF(DEBUG.GT.0) WRITE(LU,*) 'BACK FROM LECBREACH'
        DEJALU=.TRUE.
        WRITE (LU,*) 'LECTURE DONNEES BRECHE = OK'
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
         IF((OPTNBR%I(I).EQ.2).AND.(TDECBR%R(I).LT.0.D0)) THEN
           Zc = 0.D0
           N = 0
           DO J = 1, NBNDBR%I(I)
              K = INDBR%ADR(I)%P%I(J)
              IF(H%R(K).GT.0.D0) THEN
                Zw = ZF%R(K)+H%R(K)
                Zc = Zc + Zw
                N = N + 1
              ENDIF
           ENDDO
           IF(NCSIZE.GT.1) THEN
             N = P_ISUM(N)
             Zc = P_DSUM(Zc)
           ENDIF
           IF(N.GT.1) Zc = Zc/N
           IF(Zc.GT.ZDECBR%R(I)) THEN
             IF(LNG.EQ.1) WRITE(LU,10) I, AT
             IF(LNG.EQ.2) WRITE(LU,20) I, AT
             TDECBR%R(I) = AT
           ENDIF
         ENDIF
         IF((OPTNBR%I(I).EQ.3).AND.(TDECBR%R(I).LT.0.D0)) THEN
           IF(NUMPSD%I(I).GT.0) THEN
             Zw = ZF%R(NUMPSD%I(I)) + H%R(NUMPSD%I(I))
           ELSE
             Zw = 0.D0
           ENDIF
!          CASE WHERE ONE OF THE ENDS IS NOT IN THE SUB-DOMAIN
           IF(NCSIZE.GT.1) THEN
             Zw = P_DMAX(Zw)+P_DMIN(Zw)
           ENDIF
           IF(Zw.GT.ZDECBR%R(I)) THEN
             IF(LNG.EQ.1) WRITE(LU,10) I, AT
             IF(LNG.EQ.2) WRITE(LU,20) I, AT
             TDECBR%R(I) = AT
           ENDIF
         ENDIF
!
         AT1 = TDECBR%R(I)
         AT2 = AT1 + DURBR%R(I)
         IF(AT1.GT.0.D0) THEN
           IF(AT.GT.AT1) THEN
             IF(AT.GT.AT2) THEN
               Zb = ZFINBR%R(I)
             ELSE
               Zb = ZCRBR%R(I)+(ZFINBR%R(I)-ZCRBR%R(I))/(AT2-AT1)
     &              *(AT-AT1)
             ENDIF
             DO J = 1, NBNDBR%I(I)
                K = INDBR%ADR(I)%P%I(J)
                ZF%R(K)=MIN(ZF%R(K), Zb)
             ENDDO
           ENDIF
         ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!     MESSAGES
10      FORMAT(1X,'CREATION DE LA BRECHE : ',I4,/,1X,
     &            'AU TEMPS : ',G16.7)
20      FORMAT(1X,'CREATION OF BREACH : ',I4,/,1X,
     &            'AT TIME : ',G16.7)
!-----------------------------------------------------------------------
!
      RETURN
      END                  
 
