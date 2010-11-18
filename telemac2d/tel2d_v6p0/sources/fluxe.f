C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INTEGRATES IN TIME.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note     JMH : RNORM, A AND A2 NOT USED

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> A1, A2, FLULOC, G, HI, HJ, RNORM, UI, UJ, VI, VJ, XN, YN
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ALPHA, CI, CI2, CJ, CJ2, CT, CT2, PRII, PRIJ, PROD, PS, RI, RJ, RLAMB0, RLAMBI, RLAMBJ, RLAMBM, RLAMBP, SA, T, TR, TW, UT, VT
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>FLUROE()

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
!>      <td><center> 5.2                                       </center>
!> </td><td> 24/11/1997
!> </td><td> N. GOUTAL
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>A2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>AIRS
!></td><td>--></td><td>TABLEAU DES AIRES DES CELLULES
!>    </td></tr>
!>          <tr><td>FLULOC
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>G
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>H
!></td><td><-></td><td>ENTHALPIE
!>    </td></tr>
!>          <tr><td>HI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>HJ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>RNORM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UJ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VJ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>XN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YN
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FLUXE
     &(HJ,UJ,VJ,HI,UI,VI,XN,YN,RNORM,A1,A2,G,FLULOC)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A1             |---| 
C| A2             |---| 
C| AIRS           |-->| TABLEAU DES AIRES DES CELLULES
C| FLULOC         |---| 
C| G             |---| 
C| H             |<->| ENTHALPIE
C| HI             |---| 
C| HJ             |---| 
C| RNORM          |---| 
C| UI             |---| 
C| UJ             |---| 
C| VI             |---| 
C| VJ             |---| 
C| XN             |---| 
C| YN             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
         IMPLICIT NONE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
         DOUBLE PRECISION, INTENT(INOUT) :: FLULOC(3)
         DOUBLE PRECISION, INTENT(IN) :: G,HI,HJ,UI,UJ,VI,VJ,RNORM
         DOUBLE PRECISION, INTENT(IN) :: XN,YN,A1,A2
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
         DOUBLE PRECISION RI,RJ,CT2,UT,VT,RLAMB0
         DOUBLE PRECISION CT,PRII,PRIJ,ALPHA
         DOUBLE PRECISION RLAMBM,PS,SA,RLAMBP,PROD,TW(3)
         DOUBLE PRECISION TR(3),T(3),CI2,CI,CJ,CJ2,RLAMBI,RLAMBJ
C
C-----------------------------------------------------------------------
C
C   --->    COMPUTES THE AVERAGES OF ROE OF U,V,H,C**2 AND C
C           ---------------------------------------------
C
         IF(HI.LE.0.D0) THEN
           RI = 0.D0
         ELSE
           RI = SQRT ( HI )
         ENDIF
         IF (HJ.LE.0.D0) THEN
           RJ = 0.D0
         ELSE
           RJ = SQRT ( HJ )
         ENDIF
C
         UT = ( RI * UI + RJ * UJ ) /(RI + RJ)
         VT = ( RI * VI + RJ * VJ ) /(RI + RJ)
         IF ( (HI + HJ) .LE.0.D0)  THEN
           CT2= 0.D0
         ELSE
         CT2 = G*(HI+HJ)/2.D0
         ENDIF
         CT = SQRT ( CT2 )
C
C   --->  TESTS THE SIGN OF THE EIGENVALUE LAMB0 =
C           ----------------------------------------------------------
C
         RLAMB0 = UT * XN + VT * YN
C
CTBTB BEGINNING: MODIFICATION OF RLAMB0 IF RLAMB0
CC     IT IS NECESSARY TO ADD FLUXES FOR THE DUPLICATED EIGENVALUES
CC     TO BE COMPLETED BY WHOEVER WISHES TO
CC
CTBTB END
C
C---------------------------------------------------------------------
         IF  ( RLAMB0 . GE .-0.000001D0 ) THEN
C        ---- END SEGMENT ---------
C
C   --->    SMALL CALCULATIONS
C
            RLAMBM = RLAMB0 - CT
C
            PRII = G*(HI**2)/2.D0
            PRIJ = G*(HJ**2)/2.D0
            ALPHA = UI * XN + VI * YN
C
CTBTB BEGINNING : MODIFICATION OF RLAMBM IF RLAMBM
C
           IF (HI.LE.0.D0) THEN
           CI2 = 0.D0
           PRII = 0.D0
           ELSE
           CI2 =  2.D0*PRII / HI
           ENDIF
           IF (HJ.LE.0.D0) THEN
           CJ2 = 0.D0
           PRIJ = 0.D0
           ELSE
           CJ2 =  2.D0*PRIJ / HJ
           ENDIF
           CI = SQRT (CI2)
           CJ = SQRT (CJ2)
           RLAMBI = ALPHA - CI
           RLAMBJ = UJ * XN + VJ * YN - CJ
           PROD = RLAMBI * RLAMBJ
C
CTBTB : MODIFICATION ONLY IN THE RELAXATION :
           IF ( RLAMBI .LT. 0.D0 .AND. RLAMBJ .GT. 0.D0
CC   *                         .AND. ABS(RLAMBM) .LT. D1
     &                                                   ) THEN
C
C     : MODIFICATION IN THE RELAXATION OR THE SHOCK:
C          IF( PROD. LT. 0.D0 .AND. ABS(RLAMBM).LT.D1 ) THEN
C
            RLAMBM = MIN(0.D0,RLAMBM) - ABS(RLAMBI - RLAMBJ) / 4.D0
           ENDIF
C     END
C
C   --->    COMPUTES FLUX 1
C
            FLULOC(1) = ALPHA * HI
            FLULOC(2) = ALPHA * HI*UI
            FLULOC(3) = ALPHA * HI*VI
C
            FLULOC (2) = FLULOC(2) + PRII * XN
            FLULOC (3) = FLULOC(3) + PRII * YN
C
C   --->    TESTS THE SIGN OF LAMBDAM
C           ----------------------------
C
            IF ( RLAMBM . LT . 0.D0 ) THEN
C           - - - - - - - - - - - - - -
C
               T (1) = 1.D0
               T (2) = UT - CT * XN
               T (3) = VT - CT * YN
C
               TR(1) = HJ-HI
               TR(2) = HJ*UJ-HI*UI
               TR(3) = HJ*VJ-HI*VI
C
               TW(1) = (UT*XN + VT*YN)*CT + CT2
               TW(2) = -XN*CT
               TW(3) = -YN*CT
C
                 PS = TR(1)*TW(1)+TR(2)*TW(2)+TR(3)*TW(3)
C
C   --->    COMPUTES TOTAL LOCAL FLUX
C           --------------------------
C
               SA = PS * RLAMBM / (2.D0 * CT2 )
        FLULOC(1)= FLULOC(1)+SA*T(1)
        FLULOC(2)= FLULOC(2)+SA*T(2)
        FLULOC(3)= FLULOC(3)+SA*T(3)
C
C
            ENDIF
C           -----
C
C      TESTEST
         ELSE
C      TESTEST
C
C   --->    SMALL CALCULATIONS
C           --------------
C
            RLAMBP = RLAMB0 + CT
C
C
            ALPHA = UJ * XN + VJ* YN
C
CTBTB BEGINNING: MODIFICATION OF RLAMBP IF RLAMBP
C
            IF (HI.LE.0.D0) THEN
              CI2 = 0.D0
            ELSE
              CI2 = G*HI
            ENDIF
            CI = SQRT (CI2)
            IF (HJ.LE.0.D0) THEN
            CJ2 = 0.D0
            PRIJ = 0.D0
            ELSE
            CJ2 = G*HJ
            PRIJ = G*(HJ**2)/2.D0
            ENDIF
            CJ = SQRT (CJ2)
            RLAMBI = UI * XN + VI * YN + CI
            RLAMBJ = ALPHA + CJ
            PROD = RLAMBI * RLAMBJ
C
CTBTB: MODIFICATION ONLY IN THE RELAXATION:
            IF ( RLAMBI .LT. 0. .AND. RLAMBJ .GT. 0.
CC   *                          .AND. ABS(RLAMBP) .LT. D1
     &                                                    ) THEN
C
CTBTB: MODIFICATION IN THE RELAXATION OR IN THE SHOCK:
C           IF ( PROD . LT . 0. .AND. ABS(RLAMBP).LT.D1 ) THEN
C
               RLAMBP = MAX(0.D0,RLAMBP) + ABS(RLAMBI - RLAMBJ) / 4.
            ENDIF
CTBTB END
C
C   --->    COMPUTES FLUX 1
C           ----------------
C
            FLULOC(1) = ALPHA * HJ
            FLULOC(2) = ALPHA * HJ*UJ
            FLULOC(3) = ALPHA * HJ*VJ
C
            FLULOC (2) = FLULOC(2) + PRIJ * XN
            FLULOC (3) = FLULOC(3) + PRIJ * YN
C
C   --->    TESTS THE SIGN OF LAMBDAP
C           ----------------------------
C
            IF ( RLAMBP . GT . 0.D0 ) THEN
C           - - - - - - - - - - - - - -
C
               T(1) = 1.D0
               T(2) = UT + CT * XN
               T(3) = VT + CT * YN
C
               TR(1) = HJ-HI
               TR(2) = HJ*UJ-HI*UI
               TR(3) = HJ*VJ-HI*VI
C
               TW(1) = (-UT*XN - VT*YN)*CT +CT2
               TW(2) = CT*XN
               TW(3) = CT*YN
C
               PS = TR(1)*TW(1)+TR(2)*TW(2)+TR(3)*TW(3)
C
C   --->    COMPUTES TOTAL LOCAL FLUX
C           --------------------------
C
               SA = - PS * RLAMBP / (2.D0 * CT2 )
        FLULOC(1)= FLULOC(1)+SA*T(1)
        FLULOC(2)= FLULOC(2)+SA*T(2)
        FLULOC(3)= FLULOC(3)+SA*T(3)
C
            ENDIF
C           -----
        ENDIF
C
C
        RETURN
        END
C
C#######################################################################
C