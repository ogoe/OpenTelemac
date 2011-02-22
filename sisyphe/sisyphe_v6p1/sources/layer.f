C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES AVAIL FOR EACH CLASS AND EACH LAYER;
!>                NEW STRATUM THICKNESS ESTRAT.
!><br>            ACTIVE LAYER IS LAYER 1, IT IS KEPT AT A PRESCRIBED
!>                HEIGHT OF ELAY0.
!><br>            STRATUM IS LAYER 2 OF HEIGHT ESTRAT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Development history
!>   <br><table>
!> <tr><th> Release </th><th> Date </th><th> Author </th><th> Notes </th></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 21/08/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Creation of DOXYGEN tags for automated documentation and cross-referencing of the FORTRAN sources
!>   </td></tr>
!>  <tr><td><center> 6.0                                       </center>
!>    </td><td> 13/07/2010
!>    </td><td> N.DURAND (HRW), S.E.BOURBAN (HRW)
!>    </td><td> Translation of French comments within the FORTRAN sources into English comments
!>   </td></tr>
!>      <tr>
!>      <td><center> 6.0                                       </center>
!> </td><td> 10/05/2010
!> </td><td> JMH
!> </td><td> CASE WITH DEPOSITION REWRITTEN, TESTS CHANGED.
!> <br>      OTHER PARTS REWRITTEN AND/OR OPTIMISED.
!> <br>      CLEAN STOP IN PARALLEL IF PROBLEM.
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 16/09/2009
!> </td><td> JMH
!> </td><td> AVAIL(NPOIN,10,NSICLA)
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 2002
!> </td><td> MATTHIEU GONZALES DE LINARES
!> </td><td>
!> </td></tr>
!>  </table>

C
C#######################################################################
C
                        SUBROUTINE LAYER
     &(ZFCL_W,NLAYER,ZR,ZF,ESTRAT,ELAY,MASBAS,ACLADM,NSICLA,NPOIN,
     & ELAY0,VOLTOT,ES,AVAIL,CONST_ALAYER,DTS,ESTRATNEW,NLAYNEW)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ACLADM         |---| 
C| AVAIL          |<--| SEDIMENT FRACTION FOR EACH LAYER, CLASS, POINT
C| CONST_ALAYER   |---| 
C| DTS            |---| 
C| ELAY           |<--| ACTIVE LAYER THICKNESS FOR EACH POINT
C| ELAY0          |---| 
C| ES             |---| 
C| ESTRAT         |<--| ACTIVE STRATUM THICKNESS FOR EACH POINT
C| ESTRATNEW      |---| 
C| MASBAS         |---| 
C| NLAYER         |<--| NUMBER OF LAYER FOR EACH POINT
C| NLAYNEW        |---| 
C| NPOIN          |---| 
C| NSICLA         |---| 
C| VOLTOT         |---| 
C| ZF             |---| 
C| ZFCL_W         |-->| EVOLUTION FOR EACH SEDIMENT CLASS
C| ZR             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE (BIEF_OBJ),  INTENT(IN)    :: ZFCL_W,ZR,ZF
      TYPE (BIEF_OBJ),  INTENT(IN)    :: MASBAS,ACLADM
      INTEGER,          INTENT(IN)    :: NSICLA,NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: DTS
      LOGICAL,          INTENT(IN)    :: CONST_ALAYER
      TYPE (BIEF_OBJ),  INTENT(INOUT) :: NLAYER,ESTRAT,ELAY
      DOUBLE PRECISION, INTENT(INOUT) :: ELAY0
      DOUBLE PRECISION, INTENT(INOUT) :: ES(NPOIN,10)
      DOUBLE PRECISION, INTENT(INOUT) :: AVAIL(NPOIN,10,NSICLA)
      DOUBLE PRECISION, INTENT(INOUT) :: VOLTOT(10),ESTRATNEW(NPOIN)
      INTEGER         , INTENT(INOUT) :: NLAYNEW(NPOIN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
      INTEGER  P_ISUM
      EXTERNAL P_ISUM
C
C-----------------------------------------------------------------------
C
      INTEGER I,J,K,ARRET,ARRET2
      DOUBLE PRECISION EVOL,HEIGH,TEST1,TEST2,AEVOL,AUX
C
C-----------------------------------------------------------------------
C
C     TO CHECK FRACTIONS IN THE RANGE [-ZERO,1+ZERO]
C
      DOUBLE PRECISION ZERO
      DATA             ZERO/1.D-10/
C
C-----------------------------------------------------------------------
C
      ARRET=0
C
C-----------------------------------------------------------------------
C
      DO J=1,NPOIN
C
        IF(.NOT.CONST_ALAYER) ELAY0 = 3.D0 * ACLADM%R(J)
C
        NLAYNEW(J) = NLAYER%I(J)
C
C QUESTION JMH, EVOLUTION HAS BEEN COMPUTED BEFORE IN ARRAY E, WHY NOT
C               EVOL=E(J) ?????
C               ELAY(J) = ES(J,1) WHY IS IT AN EXTRA ARRAY ??
C
C
        EVOL  = 0.D0
        HEIGH = ZF%R(J)-ZR%R(J)
C       HERE ELAY.NE.HEIGH BECAUSE ELAY IS THE ACTIVE LAYER THICKNESS
        DO I=1,NSICLA
          EVOL = EVOL + ZFCL_W%ADR(I)%P%R(J)
        ENDDO
C
        IF(NLAYER%I(J).GT.1) THEN
C
          IF(EVOL.GE.0.D0) THEN
C
C           DEPOSITION
C
C           NEW HEIGHT OF LAYER 2 (IT RECEIVES EVOL TO KEEP LAYER 1 CONSTANT)
            ESTRATNEW(J) = ESTRAT%R(J) + EVOL
C
            DO I=1,NSICLA
C             JMH 28/04/2010. THE OLD IMPLEMENTATION CONSISTED OF FIRST
C             GIVING EVOL TO LAYER 2, WITH OLD AVAIL, THEN OF RECEIVING
C             THE DEPOSITION, BUT IF A CLASS DISAPPEARS IN LAYER 1,
C             IT IS NOT POSSIBLE TO GIVE IT FIRST TO LAYER 2, SO NEW
C             FRACTIONS MUST BE COMPUTED BEFORE GIVING EVOL TO LAYER 2
C             THEN I SEE NO DIFFERENCE BETWEEN EVOLELAY0
C             SO BOTH ARE TREATED BELOW, UNLIKE RELEASE 5.9.
C
C             1) LAYER 1 RECEIVES ZFCL_W OF CLASS I, WE COMPUTE THE
C                PROVISIONAL NEW AVAIL(J,1,I) IN AUX
              AUX=(AVAIL(J,1,I)*ELAY0+ZFCL_W%ADR(I)%P%R(J))/
     &                        (ELAY0+EVOL)
C
C             2) LAYER 2 RECEIVES AUX*EVOL OF CLASS I (AUX MAY BE 0 HERE)
              AVAIL(J,2,I)=(AUX*EVOL+AVAIL(J,2,I)*ESTRAT%R(J))/
     &                               ESTRATNEW(J)
C
C             3) SEEN FROM LAYER 1, AUX*EVOL OF CLASS I HAS BEEN GIVEN
C                AND THE NEW LAYER THICKNESS IS ELAY0, HENCE THE NEW AVAIL
              AVAIL(J,1,I)=( AVAIL(J,1,I)*ELAY0-AUX*EVOL
     &                          +ZFCL_W%ADR(I)%P%R(J) )/ELAY0
!
C NOTE CV: CAN BE REPLACED BY
C              AVAIL(J,1,I)= AUX
!
C             OLD (AND WRONG) FORMULATION (IN IT -AVAIL*EVOL SHOULD BE -AUX*EVOL)
C             AVAIL(J,1,I)=( AVAIL(J,1,I)*(ELAY0-EVOL)
C    &                       +ZFCL_W%ADR(I)%P%R(J)     )/ELAY0
!
C             IF(AVAIL(J,1,I).GT.1.D0+ZERO.OR.
C    *           AVAIL(J,1,I).LT.-ZERO) THEN
C               WRITE(LU,*) 'ERROR IN LAYER CASE 1'
C               STOP
C             ENDIF
            ENDDO
C           NEW HEIGHT OF LAYER 1
            ELAY%R(J) = ELAY0
C
          ELSEIF(EVOL.GT.-ELAY0) THEN
C CV: I DON'T AGREE WITH THE COMMENT BELOW
C     WE'RE IN THE CASE : -ELAY0<EVOL<0 HENCE ELAY0>-EVOL>0
C     THICKNESS OF THE FIRST LAYER IS THEREFORE SUFFICIENT
C
C           EROSION GREATER THAN LAYER 1, WE HAVE TO DESTROY A STRATUM
C
            IF(-EVOL.GT.ESTRAT%R(J)) THEN
!
C  CV: USUALLY, LAYER 2 IS VERY BROAD AND TWO LAYERS ARE IN GENERAL SUFFICIENT
C      HERE LAYER 2 IS DESTROYED
!
C             USUAL CASE (NOTE JMH : WHY NOT .GE.2 ?
!
              IF(NLAYER%I(J).GT.2) THEN
!
                DO I=1,NSICLA
                  AVAIL(J,1,I) = (  AVAIL(J,1,I)*ELAY0
     &                            + ZFCL_W%ADR(I)%P%R(J)
     &                            + AVAIL(J,2,I)*ESTRAT%R(J)
     &                            - AVAIL(J,3,I)*(EVOL+ESTRAT%R(J))
     &                           )/ ELAY0
C                 IF(AVAIL(J,1,I).GT.1.D0+ZERO.OR.
C    &               AVAIL(J,1,I).LT.-ZERO) THEN
C                   WRITE(LU,*) 'ERROR IN LAYER CASE 2'
C                   STOP
C                 ENDIF
                  AVAIL(J,2,I) = AVAIL(J,3,I)
                  DO K=3,MIN(9,NLAYER%I(J))
                    AVAIL(J,K,I) = AVAIL(J,K+1,I)
                  ENDDO
                ENDDO
                ELAY%R(J) = ELAY0
                NLAYNEW(J) = NLAYER%I(J) - 1
                ESTRATNEW(J) = ESTRAT%R(J) + EVOL + ES(J,3)
                DO K=3,MIN(9,NLAYER%I(J))
                  ES(J,K) = ES(J,K+1)
                ENDDO
!
C             ONLY ONE LAYER LEFT (NOTE JMH : 1 OR 2 ?)
!
              ELSE
                DO I=1,NSICLA
                  IF(HEIGH.GT.0.D0) THEN
                    AVAIL(J,1,I) = (  AVAIL(J,1,I)*ELAY0
     &                              + ZFCL_W%ADR(I)%P%R(J)
     &                              + ESTRAT%R(J)*AVAIL(J,2,I)
     &                              )/(ELAY%R(J)+EVOL+ESTRAT%R(J))
C                   IF(AVAIL(J,1,I).GT.1.D0+ZERO.OR.
C    &                 AVAIL(J,1,I).LT.-ZERO) THEN
C                     WRITE(LU,*) 'ERROR IN LAYER CASE 3'
C                     STOP
C                   ENDIF
                  ELSE
                    AVAIL(J,1,I) = 0.D0
                  ENDIF
                  AVAIL(J,2,I) = 0.D0
                ENDDO
                NLAYNEW(J) = NLAYER%I(J) - 1
                ELAY%R(J) = HEIGH
                ESTRATNEW(J) = 0.D0
              ENDIF
!
C           ONLY LAYER 1 ERODED
!
            ELSE
              DO I=1,NSICLA
                AVAIL(J,1,I) = (  AVAIL(J,1,I) * ELAY0
     &                          + ZFCL_W%ADR(I)%P%R(J)
     &                          - EVOL*AVAIL(J,2,I)    )/ELAY0
C               IF(AVAIL(J,1,I).GT.1.D0+ZERO.OR.
C    &             AVAIL(J,1,I).LT.-ZERO) THEN
C                 WRITE(LU,*) 'ERROR IN LAYER CASE 4'
C                 STOP
C               ENDIF
              ENDDO
              ELAY%R(J) = ELAY0
              ESTRATNEW(J) = ESTRAT%R(J) + EVOL
            ENDIF
!
          ELSE
C
C           STOPS IF EROSION IS GREATER THAN
C           THICKNESS OF THE ACTIVE LAYER!
C
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'EROSION TROP FORTE AU NOEUD J=',J
              WRITE(LU,*) 'DIMINUER DT OU AUGMENTER ELAY0'
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) 'TOO MUCH EROSION AT POINT J=',J
              WRITE(LU,*) 'DECREASE DT OR INCREASE ELAY0'
            ENDIF
            WRITE(LU,*) 'EVOL=', EVOL, 'ELAY0=',ELAY0
            CALL PLANTE(1)
            STOP
C
C         END OF TESTS ON EVOL
!
          ENDIF
!
C THERE WAS ONLY ONE LAYER
! ------------------------
!
        ELSE
!
C         IT IS NOW BIG ENOUGH TO MAKE TWO LAYERS
!
          IF(HEIGH.GT.ELAY0) THEN
            NLAYNEW(J) = 2
            ESTRATNEW(J) = HEIGH - ELAY0
            ELAY%R(J) = ELAY0
            DO I=1,NSICLA
              AVAIL(J,2,I) = AVAIL(J,1,I)
              AVAIL(J,1,I) = (AVAIL(J,1,I) * (ELAY0-EVOL)
     &                     + ZFCL_W%ADR(I)%P%R(J) )/ELAY0
C             IF(AVAIL(J,1,I).GT.1.D0+ZERO.OR.
C    &           AVAIL(J,1,I).LT.-ZERO) THEN
C                WRITE(LU,*) 'ERROR IN LAYER CASE 5'
C                STOP
C             ENDIF
            ENDDO
!
! IF THERE REMAINS ONLY ONE LAYER
! -------------------------------
!
          ELSE
C           NOTE JMH: THE TRICKIEST PART...
C           THE PROBLEM OF 0/0 CREATED BY THE CHOICE OF AVAIL
C           AS MAIN VARIABLE...
            IF(ELAY%R(J)+EVOL.GT.1.D-15) THEN
              DO I=1,NSICLA
C               AUX=AVAIL(J,1,I)
                AVAIL(J,1,I) = (AVAIL(J,1,I)*ELAY%R(J)+
     &                          ZFCL_W%ADR(I)%P%R(J))
     &                          / (ELAY%R(J)+EVOL)
C               IF(AVAIL(J,1,I).GT.1.D0+ZERO.OR.
C    &            AVAIL(J,1,I).LT.-ZERO) THEN
C                 WRITE(LU,*) 'ERROR IN LAYER CASE 6'
C                 WRITE(LU,*) 'INITIAL AVAIL=',AUX
C                 WRITE(LU,*) 'J=',J,' CLASS ',I
C                 WRITE(LU,*) 'EVOL=',EVOL,' ELAY=',ELAY%R(J)
C                 WRITE(LU,*) 'ZFCL=',ZFCL_W%ADR(I)%P%R(J)
C                 WRITE(LU,*) 'DENOMINATOR=',ELAY%R(J)+EVOL
C                 WRITE(LU,*) 'NUMERATOR=',AUX*ELAY%R(J)+
C    &                                     ZFCL_W%ADR(I)%P%R(J)
C               ENDIF
                AVAIL(J,2,I) = 0.D0
              ENDDO
              IF(ELAY%R(J)+EVOL.LT.1.D-7) THEN
C               PLAYING WITH ZEROES, RISK OF SUM NOT EQUAL TO 1
C               ONLY BECAUSE OF TRUNCATION ERRORS, WE NORMALISE
                TEST1=0.D0
                DO I=1,NSICLA
                  AVAIL(J,1,I)=MAX(0.D0,MIN(1.D0,AVAIL(J,1,I)))
                  TEST1=TEST1+AVAIL(J,1,I)
                ENDDO
                IF((TEST1-1.D0)**2.GT.ZERO) THEN
                  DO I=1,NSICLA
                    AVAIL(J,1,I)=AVAIL(J,1,I)/MAX(TEST1,1.D-21)
                  ENDDO
                ENDIF
              ENDIF
            ELSE
              DO I=1,NSICLA
                AVAIL(J,1,I) = 0.D0
                AVAIL(J,2,I) = 0.D0
              ENDDO
            ENDIF
            ELAY%R(J) = HEIGH
            ESTRATNEW(J) = 0.D0
            NLAYNEW(J) = 1
          ENDIF
        ENDIF
!
      NLAYER%I(J) = NLAYNEW(J)
      ESTRAT%R(J) = ESTRATNEW(J)
      ES(J,1) = ELAY%R(J)
      IF(NLAYER%I(J).GT.1) ES(J,2) = ESTRAT%R(J)
!
      TEST1 = 0.D0
      TEST2 = 0.D0
!
      DO I=1,NSICLA
        DO K=1,NLAYER%I(J)
C         CHECKS THAT AVAIL IS IN THE RANGE (-ZERO,1+ZERO)
          IF(AVAIL(J,K,I).GT.1.D0+ZERO.OR.AVAIL(J,K,I).LT.-ZERO) THEN
            WRITE(LU,*) 'ERROR ON FRACTIONS'
            WRITE(LU,*) 'LAYER ',K,' CLASS ',I,' POINT ',J
            IF(AVAIL(J,K,I).LT.0.D0) THEN
              WRITE(LU,*) 'AVAIL=' ,AVAIL(J,K,I)
            ELSE
              WRITE(LU,*) 'AVAIL-1=' ,AVAIL(J,K,I)-1.D0
            ENDIF
            WRITE(LU,*) 'ZFCL=',ZFCL_W%ADR(I)%P%R(J)
            WRITE(LU,*) 'EVOL=',EVOL,' ELAY=',ELAY%R(J)
            ARRET=1
          ELSE
C           ONCE CHECKED THAT WE HAVE ONLY TRUNCATION ERRORS, CLIPS
            AVAIL(J,1,I)=MAX(AVAIL(J,1,I),0.D0)
            AVAIL(J,1,I)=MIN(AVAIL(J,1,I),1.D0)
          ENDIF
        ENDDO
        TEST1 = TEST1 + AVAIL(J,1,I)
        TEST2 = TEST2 + AVAIL(J,2,I)
      ENDDO
!
C     CHECKS THAT SUM OF AVAIL IS 1 FOR FIRST 2 LAYERS
!
      IF(TEST1.GT.ZERO.AND.(TEST1-1.D0)**2>ZERO) THEN
        WRITE(LU,*) ' PROBLEM IN LAYER: J,TEST1',J,TEST1
        WRITE(LU,*) ' IN LAYER 1 SUM OF FRACTIONS NOT 1'
        ARRET=1
      ENDIF
      IF(TEST2.GT.ZERO.AND.(TEST2-1.D0)**2>ZERO) THEN
        WRITE(LU,*) ' PROBLEM IN LAYER: J,TEST2',J,TEST2
        WRITE(LU,*) ' IN LAYER 2 SUM OF FRACTIONS IS NOT 1'
        ARRET=1
      ENDIF
!
C     END OF LOOP ON ALL POINTS
!
      ENDDO
C
C     COMPUTES THE TOTAL VOLUME OF SEDIMENTS IN THE DOMAIN
C
      DO I = 1, NSICLA
        VOLTOT(I) = 0.D0
      ENDDO
      DO I=1,NSICLA
        DO J=1,NPOIN
          DO K=1,NLAYER%I(J)
            VOLTOT(I) = VOLTOT(I) + ES(J,K)*AVAIL(J,K,I)*MASBAS%R(J)
          ENDDO
        ENDDO
      ENDDO
!
      IF(NCSIZE.GT.1) THEN
        DO I=1,NSICLA
          VOLTOT(I) = P_DSUM(VOLTOT(I))
        ENDDO
      ENDIF
!
!-----------------------------------------------------------------------
!
C     CLEAN STOP FOR ALL PROCESSORS IF PROBLEM
!
      ARRET2=ARRET
      IF(NCSIZE.GT.1) ARRET2=P_ISUM(ARRET)
      IF(ARRET2.GT.0) THEN
        IF(LNG.EQ.1) WRITE(LU,*) 'ARRET APRES ERREUR DANS LAYER'
        IF(LNG.EQ.2) WRITE(LU,*) 'STOP AFTER AN ERROR IN LAYER'
        IF(ARRET.EQ.0) THEN
          IF(LNG.EQ.1) WRITE(LU,*) 'DANS ',ARRET2,' PROCESSEUR(S)'
          IF(LNG.EQ.2) WRITE(LU,*) 'IN ',ARRET2,' PROCESSOR(S)'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C
