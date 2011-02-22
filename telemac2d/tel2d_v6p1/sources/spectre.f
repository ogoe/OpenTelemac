C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       HARMONIC ANALYSIS OF TIDE WAVES USING LEAST-SQUARE
!>                FITTING METHOD.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @reference "SIMULATION DES COURANTS DE MAREE EN MANCHE ET PROCHE ATLANTIQUE",
!>                       EDF REPORT, J. M. JANIN ET. AL., PP 40-41.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF, DECLARATIONS_TELEMAC2D, INTERFACE_TELEMAC2D
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink<hr>
!> DECLARATIONS_TELEMAC2D :<br>
!> @link DECLARATIONS_TELEMAC2D::AMPL AMPL@endlink, 
!> @link DECLARATIONS_TELEMAC2D::AT AT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::DT DT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::H H@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LIST_PTS LIST_PTS@endlink, 
!> @link DECLARATIONS_TELEMAC2D::LT LT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NAME_PTS NAME_PTS@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NIT NIT@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NPERIAF NPERIAF@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NPOIN NPOIN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::NPTS NPTS@endlink, 
!> @link DECLARATIONS_TELEMAC2D::PERIAF PERIAF@endlink, 
!> @link DECLARATIONS_TELEMAC2D::PHAS PHAS@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TAFBGN TAFBGN@endlink, 
!> @link DECLARATIONS_TELEMAC2D::TAFEND TAFEND@endlink, 
!> @link DECLARATIONS_TELEMAC2D::ZF ZF@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A, AFBGN, AFEND, AM, BM, CFX, CFY, DEJA, HA, I, J, K, M, N, PI
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> COEFMAT(), INVMTX(), PLANTE(), P_DMAX(), P_DMIN()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PRERES_TELEMAC2D()

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
!> </td><td> 26/10/1994
!> </td><td> J-M HERVOUET LNH 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>AFBGN
!></td><td>--></td><td>TIME STEP AT WHICH HARMONIC ANALYSIS BEGINS
!>    </td></tr>
!>          <tr><td>AFEND
!></td><td>--></td><td>TIME STEP AT WHICH HARMONIC ANALYSIS ENDS
!>    </td></tr>
!>          <tr><td>AM
!></td><td><--</td><td>coefficient matrix in least square method
!>    </td></tr>
!>          <tr><td>AT
!></td><td>--></td><td>CURRENT TIME
!>    </td></tr>
!>          <tr><td>BM
!></td><td><--</td><td>inverse matrix of AM
!>    </td></tr>
!>          <tr><td>DT
!></td><td>--></td><td>TIME INTERVAL
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>WATER DEPTH
!>    </td></tr>
!>          <tr><td>LT
!></td><td>--></td><td>ITERATION TIME STEP
!>    </td></tr>
!>          <tr><td>M
!></td><td>--></td><td>SAMPLING POINTS
!>    </td></tr>
!>          <tr><td>NPERIAF
!></td><td>--></td><td>NUMBER OF WAVES
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NUMBER OF POINTS IN THE MESH.
!>    </td></tr>
!>          <tr><td>PERIAF
!></td><td>--></td><td>PERIODS OF WAVES
!>    </td></tr>
!>          <tr><td>ZF
!></td><td>--></td><td>BATHYMETRY
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                          SUBROUTINE SPECTRE
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AFBGN          |-->| TIME STEP AT WHICH HARMONIC ANALYSIS BEGINS
C| AFEND          |-->| TIME STEP AT WHICH HARMONIC ANALYSIS ENDS
C| AM             |<--| coefficient matrix in least square method
C| AT             |-->| CURRENT TIME
C| BM             |<--| inverse matrix of AM
C| DT             |-->| TIME INTERVAL
C| H             |-->| WATER DEPTH
C| LT             |-->| ITERATION TIME STEP
C| M             |-->| SAMPLING POINTS
C| NPERIAF        |-->| NUMBER OF WAVES
C| NPOIN          |-->| NUMBER OF POINTS IN THE MESH.
C| PERIAF         |-->| PERIODS OF WAVES
C| ZF             |-->| BATHYMETRY
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      USE DECLARATIONS_TELEMAC2D
      USE INTERFACE_TELEMAC2D
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
      DOUBLE PRECISION PI,CFX,CFY,A
C
      DOUBLE PRECISION, ALLOCATABLE :: AM(:,:),BM(:,:),HA(:)
C
      INTEGER M,I,J,K,N,AFBGN,AFEND
      SAVE AFBGN,AFEND,AM,BM,HA
C
      LOGICAL DEJA
      DATA DEJA/.FALSE./
C
      INTRINSIC COS,SIN,ACOS,DMOD,DATAN2
C
      DOUBLE PRECISION P_DMIN,P_DMAX
      EXTERNAL         P_DMIN,P_DMAX
C
C-----------------------------------------------------------------------
C
      IF(.NOT.DEJA) THEN
C
        AFBGN=INT((TAFBGN-AT)/DT +1.D-6)
        AFEND=INT((TAFEND-AT)/DT +1.D-6)
        IF(AFEND.LE.AFBGN) THEN
          IF(LNG.EQ.1) THEN
            WRITE(LU,*)
            WRITE(LU,*) 'BORNES EN TEMPS POUR L''ANALYSE DE FOURIER'
            WRITE(LU,*) 'VERIFIER OU RENSEIGNER CE MOT-CLE'
          ENDIF
          IF(LNG.EQ.2) THEN
            WRITE(LU,*)
            WRITE(LU,*) 'TIME RANGE FOR FOURIER ANALYSIS'
            WRITE(LU,*) 'CHECK OR USE THIS KEY-WORD'
          ENDIF
          CALL PLANTE(1)
          STOP
        ENDIF
        ALLOCATE(AM(2*NPERIAF,2*NPERIAF))
        ALLOCATE(BM(2*NPERIAF,2*NPERIAF))
        ALLOCATE(HA(2*NPERIAF))
        DEJA=.TRUE.
C       INITIALISES TO ZERO AT THE FIRST CALL (THINK ABOUT DESIMP)
        DO I = 1,NPERIAF
          DO J = 1,NPOIN
            AMPL%ADR(I)%P%R(J) = 0.D0
            PHAS%ADR(I)%P%R(J) = 0.D0
          ENDDO
        ENDDO
      ENDIF
C
C-----------------------------------------------------------------------
C
      PI = ACOS(-1.D0)
      M = AFEND-AFBGN+1
C
      IF(LT.GE.AFBGN.AND.LT.LE.AFEND) THEN
         DO I=1,NPERIAF
            A=2.D0*PI*DMOD(AT/PERIAF(I),1.D0)
            CFX=COS(A)/M
            CFY=SIN(A)/M
            DO J=1,NPOIN
         AMPL%ADR(I)%P%R(J)=AMPL%ADR(I)%P%R(J)+(H%R(J)+ZF%R(J))*CFX
         PHAS%ADR(I)%P%R(J)=PHAS%ADR(I)%P%R(J)+(H%R(J)+ZF%R(J))*CFY
            ENDDO
         ENDDO
      ENDIF
C
C ESTABLISHES THE COEFFICIENT MATRICES AND INVERSES IT. MULTIPLIED BY
C RIGHT HAND SIDES, WE GET THE UNKOWNS OF LINEAR EQNS. NOW, AMPL
C CONTAINS THE AMPLITUDE OF THE SIN HARMONIC COMPONENTS OF SURFACE
C ELEVATION; PHAS CONTAINS THE AMPLITUDE OF THE COS HARMONIC
C COMPONENTS OF SURFACE ELEVATION.
C
      IF(LT.EQ.AFEND) THEN
         CALL COEFMAT(PERIAF,DT,M,AM,NPERIAF)
         N=2*NPERIAF
         CALL INVMTX(AM,BM,N)
         DO J=1,NPOIN
            DO I=1,NPERIAF
               HA(I) = AMPL%ADR(I)%P%R(J)
               HA(I+NPERIAF) = PHAS%ADR(I)%P%R(J)
            ENDDO
            DO I = 1,NPERIAF
               AMPL%ADR(I)%P%R(J) = 0
               PHAS%ADR(I)%P%R(J) = 0
               DO K=1,2*NPERIAF
         AMPL%ADR(I)%P%R(J) = AMPL%ADR(I)%P%R(J)+BM(I,K)*HA(K)
         PHAS%ADR(I)%P%R(J) = PHAS%ADR(I)%P%R(J)+BM(I+NPERIAF,K)*HA(K)
               ENDDO
            ENDDO
         ENDDO
C
         DO J = 1, NPOIN
            DO I = 1,NPERIAF
               CFX = AMPL%ADR(I)%P%R(J)
               CFY = PHAS%ADR(I)%P%R(J)
               AMPL%ADR(I)%P%R(J) = SQRT(CFX**2+CFY**2)
               PHAS%ADR(I)%P%R(J) = 180.D0*DATAN2(CFY,CFX)/PI
               IF (PHAS%ADR(I)%P%R(J).LT.0.D0)
     &	           PHAS%ADR(I)%P%R(J)=PHAS%ADR(I)%P%R(J)+360.D0
            ENDDO
         ENDDO
      ENDIF
C
C OUTPUTS THE AMPLITUDES AND PHASES AT EACH POINT OF INTEREST
C
      IF(LT.EQ.NIT) THEN
         DO I = 1, NPERIAF
            IF(NPTS.GT.0) THEN
               WRITE(LU,*) ' '
               WRITE(LU,*) ' '
               WRITE(LU,*) ' '
               IF(LNG.EQ.1) WRITE(LU,*) 'ANALYSE DE LA PERIODE ',
     &	                                 PERIAF(I), ' S :'
               IF(LNG.EQ.2) WRITE(LU,*) 'ANALYSE OF PERIOD ',
     &	                                 PERIAF(I), ' S :'
               WRITE(LU,*) ' '
               WRITE(LU,90) 'NOM DE POINT', 'AMPLITUDE', 'PHASE'
               WRITE(LU,*) ' '
               DO J = 1, NPTS
C                 IN PARALLEL POINT DOES NOT ALWAYS EXIST, MAYBE ELSEWHERE
                  IF(NCSIZE.GT.0) THEN
                    WRITE(LU,100) NAME_PTS(J),
     &                            P_DMIN(AMPL%ADR(I)%P%R(LIST_PTS(J)))
     &                           +P_DMAX(AMPL%ADR(I)%P%R(LIST_PTS(J))),
     &	                          P_DMIN(PHAS%ADR(I)%P%R(LIST_PTS(J)))
     &                           +P_DMAX(PHAS%ADR(I)%P%R(LIST_PTS(J)))
                  ELSE
                    WRITE(LU,100) NAME_PTS(J),
     &                            AMPL%ADR(I)%P%R(LIST_PTS(J)),
     &	                          PHAS%ADR(I)%P%R(LIST_PTS(J))
                  ENDIF
               ENDDO
            ENDIF
         ENDDO
      ENDIF
C
 90   FORMAT(1X, A15, A16  , A11  )
 100  FORMAT(1X, A15, F16.3, F11.2)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C