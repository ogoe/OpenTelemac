C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       OPERATIONS ON VECTORS INCLUDING DIVISIONS
!>                DIVISION BY 0 CAN BE TESTED.
!><br>            IN THE EVENT OF A DIVIDE CHECK, CAN EITHER STOP THE
!>                PROGRAM OR SET THE RESULT OF THE OPERATION TO
!>                A VALUE: D.
!>  @code
!>   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!>   PERFORMED ON VECTORS X,Y AND Z AND CONSTANT C.
!>
!>   THE RESULT IS VECTOR X.
!>
!>   OP = 'X=1/Y   '     :  COPIES INVERSE OF Y IN X
!>   OP = 'X=Y/Z   '     :  DIVIDES Y BY Z
!>   OP = 'X=CY/Z  '     :  DIVIDES C.Y BY Z
!>   OP = 'X=CXY/Z '     :  DIVIDES C.X.Y BY Z
!>   OP = 'X=X+CY/Z'     :  ADDS C.Y/Z TO X
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  DIVIDE OPERATIONS INTERNALLY TAKE CARE OF DIVISIONS BY 0.
!>            SUCCESSFUL EXIT OF OVD IS THEREFORE NOT A PROOF THAT Y
!>            OR Z NEVER ARE 0

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C, D, EPS, IOPT, NPOIN, OP, X, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>OS(), OVD_2(), TRISOU(), VERMOY()

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
!> </td><td> 26/11/93
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18; F  LEPEINTRE (LNH) 30 87 78 54
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>C
!></td><td>--></td><td>CONSTANTE DONNEE
!>    </td></tr>
!>          <tr><td>D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>EPS
!></td><td>--></td><td>CRITERE DE DIVISION PAR ZERO
!>    </td></tr>
!>          <tr><td>IOPT
!></td><td>--></td><td>OPTION : 1 ON NE FAIT PAS DE TEST
!>                  2 LES TERMES INFINIS SONT REMPLACES
!>                  PAR LA CONSTANTE D.
!>                  3 ARRET EN CAS DE DIVISION PAR ZERO
!>                  4 LES TERMES INFINIS SONT TRONQUES
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>DIMENSION DES VECTEURS
!>    </td></tr>
!>          <tr><td>OP
!></td><td>--></td><td>CHAINE DE CARACTERES INDIQUANT L'OPERATION
!>                  A EFFECTUER.
!>    </td></tr>
!>          <tr><td>X
!></td><td><--</td><td>VECTEUR RESULTAT
!>    </td></tr>
!>          <tr><td>Y
!></td><td>--></td><td>VECTEUR OPERANDE
!>    </td></tr>
!>          <tr><td>Z
!></td><td>--></td><td>VECTEUR OPERANDE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE OVD
     & ( OP , X , Y , Z , C , NPOIN , IOPT , D , EPS )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |-->| CONSTANTE DONNEE
C| D             |---| 
C| EPS            |-->| CRITERE DE DIVISION PAR ZERO
C| IOPT           |-->| OPTION : 1 ON NE FAIT PAS DE TEST
C|                |   | 2 LES TERMES INFINIS SONT REMPLACES
C|                |   | PAR LA CONSTANTE D.
C|                |   | 3 ARRET EN CAS DE DIVISION PAR ZERO
C|                |   | 4 LES TERMES INFINIS SONT TRONQUES
C| NPOIN          |-->| DIMENSION DES VECTEURS
C| OP             |-->| CHAINE DE CARACTERES INDIQUANT L'OPERATION
C|                |   | A EFFECTUER.
C| X             |<--| VECTEUR RESULTAT
C| Y             |-->| VECTEUR OPERANDE
C| Z             |-->| VECTEUR OPERANDE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER,          INTENT(IN)    :: NPOIN,IOPT
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN)
      DOUBLE PRECISION, INTENT(IN)    :: Y(NPOIN),Z(NPOIN),C,D,EPS
      CHARACTER(LEN=8), INTENT(IN)    :: OP
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
C
C-----------------------------------------------------------------------
C
      IF(OP(1:8).EQ.'X=1/Y   ') THEN
C
        IF(IOPT.EQ.1) THEN
C
        DO 30 I=1,NPOIN
            X(I) = 1.D0/Y(I)
30      CONTINUE
C
        ELSEIF(IOPT.EQ.2) THEN
C
        DO 31 I=1,NPOIN
C
          IF (ABS(Y(I)).GT.EPS) THEN
            X(I) = 1.D0/Y(I)
          ELSE
            X(I) = D
          ENDIF
C
31      CONTINUE
C
        ELSEIF(IOPT.EQ.3) THEN
C
        DO 32 I=1,NPOIN
C
          IF (ABS(Y(I)).GT.EPS) THEN
            X(I) = 1.D0/Y(I)
          ELSE
            IF(LNG.EQ.1) WRITE(LU,1000) I,OP,EPS
            IF(LNG.EQ.2) WRITE(LU,2000) I,OP,EPS
            CALL PLANTE(1)
            STOP
          ENDIF
C
32      CONTINUE
C
        ELSEIF(IOPT.EQ.4) THEN
C
        DO 33 I=1,NPOIN
C
          IF (ABS(Y(I)).GT.EPS) THEN
            X(I) = 1.D0/Y(I)
          ELSEIF (Y(I).GE.0.D0) THEN
            X(I) =  1.D0/EPS
          ELSE
            X(I) = -1.D0/EPS
          ENDIF
C
33      CONTINUE
C
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=Y/Z   ') THEN
C
        IF(IOPT.EQ.1) THEN
C
        DO 40 I=1,NPOIN
            X(I) = Y(I) / Z(I)
40      CONTINUE
C
        ELSEIF(IOPT.EQ.2) THEN
C
        DO 41 I=1,NPOIN
C
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = Y(I) / Z(I)
          ELSE
            X(I) = D
          ENDIF
C
41      CONTINUE
C
        ELSEIF(IOPT.EQ.3) THEN
C
        DO 42 I=1,NPOIN
C
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = Y(I) / Z(I)
          ELSE
            IF(LNG.EQ.1) WRITE(LU,1000) I,OP,EPS
            IF(LNG.EQ.2) WRITE(LU,2000) I,OP,EPS
            CALL PLANTE(1)
            STOP
          ENDIF
C
42      CONTINUE
C>>>>
        ELSEIF(IOPT.EQ.4) THEN
C
        DO 43 I=1,NPOIN
C
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = Y(I) / Z(I)
          ELSEIF (ABS(Y(I)).LT.EPS) THEN
            X(I) = D
          ELSEIF (Z(I).GE.0.D0) THEN
            X(I) =  Y(I) / EPS
          ELSE
            X(I) = -Y(I) / EPS
          ENDIF
C
43      CONTINUE
C<<<<
C
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=CY/Z  ') THEN
C
        IF(IOPT.EQ.1) THEN
C
        DO 50 I=1,NPOIN
            X(I) = C*Y(I) / Z(I)
50      CONTINUE
C
        ELSEIF(IOPT.EQ.2) THEN
C
        DO 51 I=1,NPOIN
C
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = C*Y(I) / Z(I)
          ELSE
            X(I) = D
          ENDIF
C
51      CONTINUE
C
        ELSEIF(IOPT.EQ.3) THEN
C
        DO 52 I=1,NPOIN
C
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = C*Y(I) / Z(I)
          ELSE
            IF(LNG.EQ.1) WRITE(LU,1000) I,OP,EPS
            IF(LNG.EQ.2) WRITE(LU,2000) I,OP,EPS
            CALL PLANTE(1)
            STOP
          ENDIF
C
52      CONTINUE
C>>>>
        ELSEIF(IOPT.EQ.4) THEN
C
        DO 53 I=1,NPOIN
C
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = C*Y(I) / Z(I)
          ELSEIF (ABS(C*Y(I)).LT.EPS) THEN
            X(I) = D
          ELSEIF (Z(I).GE.0.D0) THEN
            X(I) =  C*Y(I) / EPS
          ELSE
            X(I) = -C*Y(I) / EPS
          ENDIF
C
53      CONTINUE
C<<<<
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=CXY/Z ') THEN
C
        IF(IOPT.EQ.1) THEN
C
        DO 60 I=1,NPOIN
            X(I) = C*X(I)*Y(I) / Z(I)
60      CONTINUE
C
        ELSEIF(IOPT.EQ.2) THEN
C
        DO 61 I=1,NPOIN
C
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = C*X(I)*Y(I) / Z(I)
          ELSE
            X(I) = D
          ENDIF
C
61      CONTINUE
C
        ELSEIF(IOPT.EQ.3) THEN
C
        DO 62 I=1,NPOIN
C
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = C*X(I)*Y(I) / Z(I)
          ELSE
            IF(LNG.EQ.1) WRITE(LU,1000) I,OP,EPS
            IF(LNG.EQ.2) WRITE(LU,2000) I,OP,EPS
            CALL PLANTE(1)
            STOP
          ENDIF
C
62      CONTINUE
C>>>>
        ELSEIF(IOPT.EQ.4) THEN
C
        DO 63 I=1,NPOIN
C
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = C*X(I)*Y(I) / Z(I)
          ELSEIF (ABS(C*X(I)*Y(I)).LT.EPS) THEN
            X(I) = D
          ELSEIF (Z(I).GE.0.D0) THEN
            X(I) =  C*Y(I) / EPS
          ELSE
            X(I) = -C*Y(I) / EPS
          ENDIF
C
63      CONTINUE
C<<<<
C
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSEIF(OP(1:8).EQ.'X=X+CY/Z') THEN
C
        IF(IOPT.EQ.1) THEN
C
        DO 70 I=1,NPOIN
            X(I) = X(I) + C * Y(I) / Z(I)
70      CONTINUE
C
        ELSEIF(IOPT.EQ.2) THEN
C
        DO 71 I=1,NPOIN
C
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = X(I) + C * Y(I) / Z(I)
          ELSE
            X(I) = D
          ENDIF
C
71      CONTINUE
C
        ELSEIF(IOPT.EQ.3) THEN
C
        DO 72 I=1,NPOIN
C
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = X(I) + C * Y(I) / Z(I)
          ELSE
            IF(LNG.EQ.1) WRITE(LU,1000) I,OP,EPS
            IF(LNG.EQ.2) WRITE(LU,2000) I,OP,EPS
            CALL PLANTE(1)
            STOP
          ENDIF
C
72      CONTINUE
C>>>>
        ELSEIF(IOPT.EQ.4) THEN
C
        DO 73 I=1,NPOIN
C
          IF (ABS(Z(I)).GT.EPS) THEN
            X(I) = X(I) + C*Y(I) / Z(I)
          ELSEIF (ABS(C*Y(I)).LT.EPS) THEN
            X(I) = D
          ELSEIF (Z(I).GE.0.D0) THEN
            X(I) = X(I) + C*Y(I) / EPS
          ELSE
            X(I) = X(I) - C*Y(I) / EPS
          ENDIF
C
73      CONTINUE
C<<<<
C
        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSE
C
         IF (LNG.EQ.1) WRITE(LU,3000) OP
         IF (LNG.EQ.2) WRITE(LU,4000) OP
         CALL PLANTE(1)
         STOP
C
      ENDIF
C
C-----------------------------------------------------------------------
C
1000     FORMAT(1X,'OVD (BIEF) : DIVISION PAR ZERO AU POINT ',1I6,
     &             ' LORS DE L''OPERATION ',A8,/,1X,
     &             'LE CRITERE EST ',G16.7)
2000     FORMAT(1X,'OVD (BIEF) : DIVIDE BY ZERO AT POINT ',1I6,
     &             ' FOR OPERATION ',A8,/,1X,
     &             'THE CRITERION IS ',G16.7)
3000     FORMAT(1X,'OVD (BIEF) : OPERATION INCONNUE : ',A8)
4000     FORMAT(1X,'OVD (BIEF) : UNKNOWN OPERATION: ',A8)
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C