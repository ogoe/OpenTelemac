C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INITIAL FRACTION DISTRIBUTION AND LAYER THICKNESS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note   PHILOSOPHY: INIT_COMPO DEFINES THE STRATIFICATION CORRESPONDING
!>                TO THE BOTTOM INITIAL COMPOSITION; NCOUCHES CORRESPONDS
!>                TO THE NUMBER OF REAL INITIAL LAYERS.
!>  @note         INIT_AVAI CORRECTS AND SUPPLEMENTS THIS STRATIFICATION
!>                IF PROBLEMS, AND ADDS THE ACTIVE LAYER; NLAYER CORRESPONDS
!>                TO THE NUMBER OF LAYERS USED IN THE COMPUTATION.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_SISYPHE, DECLARATIONS_TELEMAC, INTERFACE_SISYPHE
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_SISYPHE :<br>
!> @link DECLARATIONS_SISYPHE::AVAIL AVAIL@endlink, 
!> @link DECLARATIONS_SISYPHE::DEBU DEBU@endlink, 
!> @link DECLARATIONS_SISYPHE::ELAY ELAY@endlink, 
!> @link DECLARATIONS_SISYPHE::ELAY0 ELAY0@endlink, 
!> @link DECLARATIONS_SISYPHE::ES ES@endlink, 
!> @link DECLARATIONS_SISYPHE::ESTRAT ESTRAT@endlink, 
!> @link DECLARATIONS_SISYPHE::IT1 IT1@endlink, 
!> @link DECLARATIONS_SISYPHE::NLAYER NLAYER@endlink, 
!> @link DECLARATIONS_SISYPHE::NOMBLAY NOMBLAY@endlink, 
!> @link DECLARATIONS_SISYPHE::NPOIN NPOIN@endlink, 
!> @link DECLARATIONS_SISYPHE::NSICLA NSICLA@endlink, 
!> @link DECLARATIONS_SISYPHE::VOLTOT VOLTOT@endlink, 
!> @link DECLARATIONS_SISYPHE::VOLU2D VOLU2D@endlink, 
!> @link DECLARATIONS_SISYPHE::ZF ZF@endlink, 
!> @link DECLARATIONS_SISYPHE::ZR ZR@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> HAUTSED, I, J, K, NMAXI, TEST1
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_INIT_AVAI
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> INIT_COMPO(), PLANTE(), P_DSUM()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>INIT_SEDIMENT()

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
!>      <td><center>                                           </center>
!> </td><td> 2002/2003
!> </td><td> MATTHIEU GONZALES DE LINARES
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 2002
!> </td><td> BUI MINH DUC
!> </td><td> INITIAL FRACTION DISTRIBUTION FOR NON-UNIFORM BED MATERIALS
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AVAIL
!></td><td><--</td><td>SEDIMENT FRACTION FOR EACH LAYER, CLASS, NODE
!>    </td></tr>
!>          <tr><td>ELAY
!></td><td><--</td><td>ACTIVE LAYER THICKNESS FOR EACH POINT
!>    </td></tr>
!>          <tr><td>ELAY0
!></td><td>--></td><td>WANTED ACTIVE LAYER THICKNESS
!>    </td></tr>
!>          <tr><td>ES
!></td><td><--</td><td>THICKNESS FOR EACH LAYER AND NODE
!>    </td></tr>
!>          <tr><td>ESTRAT
!></td><td><--</td><td>ACTIVE STRATUM THICKNESS FOR EACH POINT
!>    </td></tr>
!>          <tr><td>NLAYER
!></td><td><--</td><td>NUMBER OF LAYER FOR EACH POINT
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>BOTTOM
!>    </td></tr>
!>          <tr><td>ZR
!></td><td>--></td><td>RIGID BOTTOM
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                          SUBROUTINE INIT_AVAI
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AVAIL          |<--| SEDIMENT FRACTION FOR EACH LAYER, CLASS, NODE
C| ELAY           |<--| ACTIVE LAYER THICKNESS FOR EACH POINT
C| ELAY0          |-->| WANTED ACTIVE LAYER THICKNESS
C| ES             |<--| THICKNESS FOR EACH LAYER AND NODE
C| ESTRAT         |<--| ACTIVE STRATUM THICKNESS FOR EACH POINT
C| NLAYER         |<--| NUMBER OF LAYER FOR EACH POINT
C| ZF             |-->| BOTTOM
C| ZR             |-->| RIGID BOTTOM
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC
      USE DECLARATIONS_SISYPHE
      USE INTERFACE_SISYPHE,EX_INIT_AVAI=> INIT_AVAI
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
C
      INTEGER I,J,K,NMAXI
C
      DOUBLE PRECISION HAUTSED,TEST1
C
C-----------------------------------------------------------------------
C
      NMAXI = 0
C
C     THE INITIAL NUMBER OF LAYERS, THEIR THICKNESS AND THEIR COMPOSITION
C     ARE SET BY THE USER
C
C     NOTE: WHEN COMPUTATION CONTINUED INIT_COMPO MUST NOT
C           CHANGE ES AND AVAIL
C
      IF(DEBU) THEN
C       TENTATIVE VALUE, THIS IS TO CHECK
!       ADDED BY JMH 30/06/2010
        DO J=1,NPOIN
          I=1
          DO K=2,NOMBLAY
            IF(ES(J,K).GT.0.D0) I = I + 1
          ENDDO
          NLAYER%I(J)=I
!         CHECKING ALL LAYERS AND CORRECTING AVAIL
!         DUE TO POSSIBLE SHIFT OF SINGLE PRECISION STORAGE
          DO K=1,NLAYER%I(J)
            TEST1=0.D0
            DO I=1,NSICLA
              TEST1=TEST1+AVAIL(J,K,I)
            ENDDO
            IF(TEST1.GT.1.D-10.AND.(TEST1-1.D0)**2.GT.1.D-10) THEN
              DO I=1,NSICLA
                AVAIL(J,K,I)=AVAIL(J,K,I)/TEST1
              ENDDO
            ENDIF
          ENDDO
        ENDDO
      ELSE
        CALL INIT_COMPO(IT1%I)
C
        DO 45 J=1,NPOIN
C
C       10 IS THE MAXIMUM NUMBER OF LAYERS ALLOWED
        NLAYER%I(J) = IT1%I(J)
        IF(NLAYER%I(J).GT.10) THEN
          IF(LNG.EQ.1) WRITE(LU,1800)
          IF(LNG.EQ.2) WRITE(LU,1815)
          CALL PLANTE(1)
          STOP
        ENDIF
C
C       THE HEIGHT OF SEDIMENT (SUM OF ES) MUST NOT BE MORE THAN ZF-ZR
C       IF SO, THE HEIGHT OF THE LAST LAYER IS REDUCED
C       IF THERE ARE LAYERS UNDER ZR, THEY ARE NOT TAKEN INTO ACCOUNT
        HAUTSED = 0.D0
        DO K=1,IT1%I(J)
        IF(HAUTSED + ES(J,K) .GE. ZF%R(J) - ZR%R(J)) THEN
          ES(J,K) = ZF%R(J) - ZR%R(J) -  HAUTSED
          NLAYER%I(J) = K
          HAUTSED = HAUTSED + ES(J,K)
          GOTO 144
        ENDIF
        HAUTSED = HAUTSED + ES(J,K)
        ENDDO
C
144     CONTINUE
C
C       THE THICKNESS OF THE LAST LAYER IS ENLARGED SO THAT
C       THE HEIGHT OF SEDIMENT (SUM OF ES) IS EQUAL TO ZF-ZR
        IF(HAUTSED.LT.ZF%R(J)-ZR%R(J)) THEN
          ES(J,NLAYER%I(J))=ES(J,NLAYER%I(J))+ZF%R(J)-ZR%R(J)-HAUTSED
        ENDIF
C
        IF(NLAYER%I(J).GT.1) THEN
C         IT IS ASSUMED THAT ELAY0 IS SMALLER THAN THE FIRST STRATUM
C         NEED TO ADD THE CASE WHERE ELAY0 IS LARGER
          IF(ELAY0.GT.ES(J,1)) THEN
            IF(LNG.EQ.1) THEN
              WRITE(LU,*) 'COUCHE ACTIVE TROP GROSSE/STRATIFICATION'
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,*) ' ACTIVE LAYER TOO BIG/STRATIFICATION'
            ENDIF
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDIF
C       THE FIRST STRATUM IS SEPARATED BETWEEN ACTIVE LAYER + ACTIVE STRATUM
        IF(ELAY0.LT.ES(J,1)) THEN
          NLAYER%I(J) = NLAYER%I(J) + 1
          IF(NLAYER%I(J).GT.10) THEN
            IF(LNG.EQ.1) WRITE(LU,1800)
            IF(LNG.EQ.2) WRITE(LU,1815)
            CALL PLANTE(1)
            STOP
          ENDIF
C         INDICES FOR ES AND AVAIL NEED TO BE OFFSET
          IF(NLAYER%I(J).GT.2) THEN
            DO K=NLAYER%I(J),3,-1
              ES(J,K) = ES(J,K-1)
            ENDDO
          ENDIF
          ES(J,2) = ES(J,1) - ELAY0
          ES(J,1) = ELAY0
          DO I=1,NSICLA
            DO K=NLAYER%I(J),2,-1
              AVAIL(J,K,I) = AVAIL(J,K-1,I)
            ENDDO
          ENDDO
        ENDIF
C
45      CONTINUE
C
      ENDIF
C
      NMAXI=0
      DO J=1,NPOIN
        ELAY%R(J) = ES(J,1)
        IF(NLAYER%I(J).GT.1) THEN
          ESTRAT%R(J) = ES(J,2)
        ENDIF
C       TO RE-EXAMINE :
C       UNUSED AVAIL ARE FILLED WITH ZEROS, IS IT BETTER !!!???
        IF(NLAYER%I(J).LT.10) THEN
          DO I = 1, NSICLA
            DO K = NLAYER%I(J)+1,10
              AVAIL(J,K,I) = 0.D0
            ENDDO
          ENDDO
        ENDIF
        IF(NLAYER%I(J).GT.NMAXI) NMAXI = NLAYER%I(J)
      ENDDO
C
C     COMPUTES THE TOTAL VOLUME OF SEDIMENTS IN EACH CLASS
C
      DO I=1,NSICLA
        VOLTOT(I)=0.D0
        DO J=1,NPOIN
          DO K=1,NLAYER%I(J)
            VOLTOT(I) = VOLTOT(I) + ES(J,K)*AVAIL(J,K,I)*VOLU2D%R(J)
          ENDDO
        ENDDO
      ENDDO
C
      IF(NCSIZE.GT.1) THEN
        DO I=1,NSICLA
          VOLTOT(I) = P_DSUM(VOLTOT(I))
        ENDDO
      ENDIF
C
      WRITE(LU,*) 'MAXIMUM INITIAL NUMBER OF LAYERS :',NMAXI
      DO I=1,NSICLA
        IF(LNG.EQ.1) THEN
          WRITE(LU,*)'VOLUME TOTAL DE LA CLASSE ',I ,' :',VOLTOT(I)
        ENDIF
        IF(LNG.EQ.1) THEN
          WRITE(LU,*)'TOTAL VOLUME OF CLASS ',I ,' :',VOLTOT(I)
        ENDIF
      ENDDO
C
C-----------------------------------------------------------------------
C
1800  FORMAT(1X,'IL Y A PLUS DE 10 COUCHES DANS LA STRATIFICATION')
1815  FORMAT(1X,'THERE ARE MORE THAN 10 LAYERS IN THE STRATIFICATION')
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C