C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE AVERAGE OF A 3D VARIABLE ON THE VERTICAL.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F1, F2, FINT1, FINT2, IPLAN1, IPLAN2, NFONC, NPLAN, NPOIN2, OPTBAN, TRA01, TRA02, TRA03, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> AUX, I, IP, IPOIN2
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> OV(), OVD(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>PRERES_TELEMAC3D(), TELEMAC3D(), WAVE_EQUATION()

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
!> </td><td> **/03/99
!> </td><td> JACEK A. JANKOWSKI PINXIT
!> </td><td> FORTRAN95 VERSION
!> </td></tr>
!>      <tr>
!>      <td><center> 5.3                                       </center>
!> </td><td> 25/11/97
!> </td><td> F LEPEINTRE (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>F1,F2
!></td><td>--></td><td>VARIABLES A MOYENNER
!>    </td></tr>
!>          <tr><td>FINT1,FINT2
!></td><td><--</td><td>VARIABLES MOYENNEES
!>    </td></tr>
!>          <tr><td>IPLAN1,IPLAN2
!></td><td>--></td><td>PLANS ENTRE LESQUELS ON PROCEDE A
!>                  L'INTEGRATION
!>    </td></tr>
!>          <tr><td>NFONC
!></td><td>--></td><td>NOMBRE DE VARIABLES
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS HORIZONTAUX
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS PAR PLANS HORIZONTAUX
!>    </td></tr>
!>          <tr><td>OPTBAN
!></td><td>--></td><td>OPTION DE TRAITEMENT DES BANCS DECOUVRANTS
!>    </td></tr>
!>          <tr><td>TRA01,TRA02
!></td><td>--></td><td>TABLEAUX DE TRAVAIL
!>    </td></tr>
!>          <tr><td>TRA03
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Z
!></td><td>--></td><td>COTES DES NOEUDS DU MAILLAGE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VERMOY
     & (FINT1,FINT2,F1,F2,NFONC,Z,TRA01,TRA02,TRA03,
     &  IPLAN1,IPLAN2,NPOIN2,NPLAN,OPTBAN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F1,F2          |-->| VARIABLES A MOYENNER
C| FINT1,FINT2    |<--| VARIABLES MOYENNEES
C| IPLAN1,IPLAN2  |-->| PLANS ENTRE LESQUELS ON PROCEDE A
C|                |   | L'INTEGRATION
C| NFONC          |-->| NOMBRE DE VARIABLES
C| NPLAN          |-->| NOMBRE DE PLANS HORIZONTAUX
C| NPOIN2         |-->| NOMBRE DE POINTS PAR PLANS HORIZONTAUX
C| OPTBAN         |-->| OPTION DE TRAITEMENT DES BANCS DECOUVRANTS
C| TRA01,TRA02    |-->| TABLEAUX DE TRAVAIL
C| TRA03          |---| 
C| Z             |-->| COTES DES NOEUDS DU MAILLAGE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPOIN2,NPLAN,NFONC,IPLAN1,IPLAN2,OPTBAN
      DOUBLE PRECISION, INTENT(INOUT) :: FINT1(NPOIN2),FINT2(NPOIN2)
      DOUBLE PRECISION, INTENT(IN)    :: F1(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: F2(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(IN)    :: Z(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA01(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA02(NPOIN2,NPLAN)
      DOUBLE PRECISION, INTENT(INOUT) :: TRA03(NPOIN2,NPLAN)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER IPOIN2,IP,I
      DOUBLE PRECISION AUX
!
!***********************************************************************
!
C CHECKS THE PLANE NUMBERS IPLAN1 AND IPLAN2
!
      IF( IPLAN2.LE.IPLAN1 .OR.
     &    IPLAN1.LT.1      .OR. IPLAN1.GT.NPLAN .OR.
     &    IPLAN2.LT.1      .OR. IPLAN2.GT.NPLAN     ) THEN
         IF (LNG.EQ.1) WRITE(LU,11) IPLAN1,IPLAN2
         IF (LNG.EQ.2) WRITE(LU,12) IPLAN1,IPLAN2
         CALL PLANTE(1)
         STOP
      ENDIF
!
C CHECKS NFONC
!
      IF(NFONC.NE.1.AND.NFONC.NE.2) THEN
        IF (LNG.EQ.1) WRITE(LU,13) NFONC
        IF (LNG.EQ.2) WRITE(LU,14) NFONC
        CALL PLANTE(1)
        STOP
      ENDIF
!
C CHECKS OPTBAN
!
      IF(OPTBAN.NE.0.AND.OPTBAN.NE.1.AND.OPTBAN.NE.2) THEN
        IF (LNG.EQ.1) WRITE(LU,15) OPTBAN
        IF (LNG.EQ.2) WRITE(LU,16) OPTBAN
        CALL PLANTE(1)
        STOP
      ENDIF
!
!-----------------------------------------------------------------------
!
C COMPUTES FINT = SUM OF 2*F*DZ FROM IPLAN1 TO IPLAN2
C THE INTEGRAL IS COMPUTED BY THE TRAPEZOID RULE
!
      IPOIN2 = NPOIN2*(IPLAN2-IPLAN1)
!
      IF(NFONC.GE.1) THEN
      CALL OV('X=Y+Z   ',TRA01,F1(1,IPLAN1+1),F1(1,IPLAN1),0.D0,IPOIN2)
      ENDIF
      IF(NFONC.EQ.2) THEN
      CALL OV('X=Y+Z   ',TRA02,F2(1,IPLAN1+1),F2(1,IPLAN1),0.D0,IPOIN2)
      ENDIF
      CALL OV('X=Y-Z   ',TRA03,Z(1,IPLAN1+1),Z(1,IPLAN1),0.D0,IPOIN2)
!
      IF(NFONC.GE.1) CALL OV('X=YZ    ',FINT1,TRA01,TRA03,0.D0,NPOIN2)
      IF(NFONC.EQ.2) CALL OV('X=YZ    ',FINT2,TRA02,TRA03,0.D0,NPOIN2)
!
      IF(IPLAN2-IPLAN1.GE.2) THEN
        IF(NFONC.GE.1) THEN
        DO IP = 2,IPLAN2-IPLAN1
          CALL OV('X=X+YZ  ',FINT1,TRA01(1,IP),TRA03(1,IP),0.D0,NPOIN2)
        END DO
        ENDIF
        IF(NFONC.EQ.2) THEN
        DO IP = 2,IPLAN2-IPLAN1
          CALL OV('X=X+YZ  ',FINT2,TRA02(1,IP),TRA03(1,IP),0.D0,NPOIN2)
        END DO
        ENDIF
      ENDIF
!
C     COMPUTES THE ELEVATION
!
      CALL OV('X=Y-Z   ',TRA03,Z(1,IPLAN2),Z(1,IPLAN1),0.D0,NPOIN2)
!
      IF(OPTBAN.EQ.0.OR.OPTBAN.EQ.2) THEN
!
C       DIVIDES BY 2 H (OR 0 IF H=0)
!
        IF(NFONC.GE.1) THEN
          CALL OVD('X=CY/Z  ',FINT1,FINT1,TRA03,0.5D0,NPOIN2,
     &                        2,0.D0,1.D-8)
        ENDIF
        IF(NFONC.EQ.2) THEN
          CALL OVD('X=CY/Z  ',FINT2,FINT2,TRA03,0.5D0,NPOIN2,
     &                        2,0.D0,1.D-8)
        ENDIF
!
      ELSEIF(OPTBAN.EQ.1) THEN
!
        AUX=1.D0/FLOAT(IPLAN2-IPLAN1+1)
!
C       DIVIDES BY 2 H OR ARITHMETIC MEAN IF THERE IS NO WATER
!
        IF(NFONC.GE.1) THEN
          DO I=1,NPOIN2
            IF(TRA03(I,1).GT.1.D-4) THEN
              FINT1(I)=FINT1(I)*0.5D0/TRA03(I,1)
            ELSE
              FINT1(I)=0.D0
              DO IP=IPLAN1,IPLAN2
                FINT1(I)=FINT1(I)+F1(I,IP)
              ENDDO
              FINT1(I)=FINT1(I)*AUX
            ENDIF
          ENDDO
        ENDIF
        IF(NFONC.EQ.2) THEN
          DO I=1,NPOIN2
            IF(TRA03(I,1).GT.1.D-4) THEN
              FINT2(I)=FINT2(I)*0.5D0/TRA03(I,1)
            ELSE
              FINT2(I)=0.D0
              DO IP=IPLAN1,IPLAN2
                FINT2(I)=FINT2(I)+F2(I,IP)
              ENDDO
              FINT2(I)=FINT2(I)*AUX
            ENDIF
          ENDDO
        ENDIF
      ENDIF
!
!-----------------------------------------------------------------------
!
11    FORMAT('VERMOY: ERREUR SUR LES ARGUMENTS IPLAN1 ET IPLAN2',2I3)
12    FORMAT('VERMOY: ERROR ON THE DATA IPLAN1 AND IPLAN2',2I3)
13    FORMAT('VERMOY: ERREUR SUR L'' ARGUMENT NFONC',1I6)
14    FORMAT('VERMOY: ERROR ON ARGUMENT NFONC',1I6)
15    FORMAT('VERMOY: ERREUR SUR L'' ARGUMENT OPTBAN : ',1I6)
16    FORMAT('VERMOY: ERROR ON ARGUMENT OPTBAN: ',1I6)
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C