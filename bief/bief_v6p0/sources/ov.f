C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       OPERATIONS ON VECTORS.
!>  @code
!>   OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES THE OPERATION TO BE
!>   PERFORMED ON VECTORS X,Y AND Z AND CONSTANT C.<br>
!>   THE RESULT IS VECTOR X.<br>
!>   OP = 'X=C     '     :  SETS X TO C
!>   OP = 'X=Y     '     :  COPIES Y IN X
!>   OP = 'X=+Y    '     :  IDEM
!>   OP = 'X=-Y    '     :  COPIES -Y IN X
!>   OP = 'X=1/Y   '     :  COPIES INVERSE OF Y IN X
!>   OP = 'X=Y+Z   '     :  ADDS Y AND Z
!>   OP = 'X=Y-Z   '     :  SUBTRACTS Z TO Y
!>   OP = 'X=YZ    '     :  Y.Z
!>   OP = 'X=-YZ   '     :  -Y.Z
!>   OP = 'X=XY    '     :  X.Y
!>   OP = 'X=X+YZ  '     :  ADDS Y.Z TO X
!>   OP = 'X=X-YZ  '     :  SUBTRACTS Y.Z FROM X
!>   OP = 'X=CXY   '     :  C.X.Y
!>   OP = 'X=CYZ   '     :  C.Y.Z
!>   OP = 'X=CXYZ  '     :  C.X.Y.Z
!>   OP = 'X=X+CYZ '     :  ADDS C.Y.Z TO X
!>   OP = 'X=Y/Z   '     :  DIVIDES Y BY Z
!>   OP = 'X=CY/Z  '     :  DIVIDES C.Y BY Z
!>   OP = 'X=CXY/Z '     :  DIVIDES C.X.Y BY Z
!>   OP = 'X=X+CY/Z'     :  ADDS C.Y/Z TO X
!>   OP = 'X=X+Y   '     :  ADDS Y TO X
!>   OP = 'X=X-Y   '     :  SUBTRACTS Y FROM X
!>   OP = 'X=CX    '     :  MULTIPLIES X BY C
!>   OP = 'X=CY    '     :  MULTIPLIES Y BY C
!>   OP = 'X=Y+CZ  '     :  ADDS C.Z TO Y
!>   OP = 'X=X+CY  '     :  ADDS C.Y TO X
!>   OP = 'X=SQR(Y)'     :  SQUARE ROOT OF Y
!>   OP = 'X=ABS(Y)'     :  ABSOLUTE VALUE OF Y
!>   OP = 'X=N(Y,Z)'     :  NORM OF THE VECTOR WITH COMPONENTS Y AND Z
!>   OP = 'X=Y+C   '     :  ADDS C TO Y
!>   OP = 'X=X+C   '     :  ADDS C TO X
!>   OP = 'X=Y**C  '     :  Y TO THE POWER C
!>   OP = 'X=COS(Y)'     :  COSINE OF Y
!>   OP = 'X=SIN(Y)'     :  SINE OF Y
!>   OP = 'X=ATN(Y)'     :  ARC TANGENT OF Y
!>   OP = 'X=A(Y,Z)'     :  ATAN2(Y,Z)
!>   OP = 'X=+(Y,C)'     :  MAXIMUM OF Y AND C
!>   OP = 'X=-(Y,C)'     :  MINIMUM OF Y AND C
!>   OP = 'X=+(Y,Z)'     :  MAXIMUM OF Y AND Z
!>   OP = 'X=-(Y,Z)'     :  MINIMUM OF Y AND Z
!>   OP = 'X=YIFZ<C'     :  COPIES Y IN X IF Z < C , FOR EACH POINT
!>   OP = 'X=C(Y-Z)'     :  MULTIPLIES C BY (Y-Z)
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  DIVIDE OPERATIONS INTERNALLY TAKE CARE OF DIVISIONS BY 0.
!>            SUCCESSFUL EXIT OF OV IS THEREFORE NOT A PROOF THAT Y OR Z
!>            NEVER ARE 0. FOR SUCH OPERATIONS, PREFERABLY USE OVD

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C, NPOIN, OP, X, Y, Z
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
!><br>ALLVEC(), ASMVEC(), ASSVEC(), CARACT(), CLIP(), CONDIM(), CONDIS(), CONDIW(), CORFON(), CORMAR(), COUPE(), CVTRVF(), DECV11(), DECV21(), DERLAG(), DESCEN(), DESSEG(), DIFF3D(), DIRI01(), DIRI04(), DIRI09(), DLDU11(), DLDU21(), DLDU41(), DLDUSEG(), DRIUTI(), DWNUP1(), FILTER_H(), FLUSED(), FONVAS(), FWSPEC(), GETTRIEBE(), GETTRISEG(), HOMERE_ADJ_T2D(), INBIEF(), INITAB(), INIVEN(), LECDOI(), LECHAM(), LECSUI(), LIMI3D(), LITENR(), MASK3D(), MASSED(), METEO(), METGRA(), MURD3D(), MURD3D_POS(), MV0202(), MV0303(), MV0304(), MV0306(), MV0403(), MV0404(), MV0603(), MV0606(), MVSEG(), MW0303(), NEWSTR(), NOEROD(), NOUDON(), NOUMAR(), NUWAVE_P0(), OM0101(), OM1111(), OM1112(), OM1113(), OM1211(), OM1311(), OM2121(), OM4111(), OM4141(), OM5111(), OMSEG(), OMSEGBOR(), OS(), OV_2(), PHSTAT(), PORO11(), PREBOR(), PRECON(), PREDIV(), PREVEREBE(), PREVERSEG(), PROPAG(), RADIAT(), REMONT(), REMSEG(), RESCUE(), RESCUE_SISYPHE(), RESCUE_SISYPHE_NOTPERMA(), SCOPE(), SED3D(), SISYPHE(), SPEINI(), STREAMLINE(), STREAMLINE_TOMAWAC(), STRESS(), SUISED(), TELEMAC3D(), TNOMER(), TOB_SISYPHE(), TRANSF(), TRISOU(), VECLE3(), VECLE4(), VECLE6(), VERMOY(), WAC(), WAVE_EQUATION(), WSTARW()

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
!> </td><td> 17/03/2010
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18; F  LEPEINTRE (LNH) 30 87 78 54
!> </td><td> ORIGINAL IDEA IN ULYSSE. THANK YOU D. LAURENCE
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
                        SUBROUTINE OV
     & ( OP , X , Y , Z , C , NPOIN )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |-->| CONSTANTE DONNEE
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
      INTEGER, INTENT(IN)             :: NPOIN
      DOUBLE PRECISION, INTENT(IN)    :: Y(NPOIN),Z(NPOIN),C
      DOUBLE PRECISION, INTENT(INOUT) :: X(NPOIN)
      CHARACTER(LEN=8), INTENT(IN)    :: OP
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I
C
      INTRINSIC SQRT,ABS,COS,SIN,ATAN,MAX,MIN
C
C-----------------------------------------------------------------------
C
      SELECT CASE(OP(3:8))
C
C-----------------------------------------------------------------------
C
        CASE('C     ')
C
        DO I=1,NPOIN
          X(I) = C
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('0     ')
C
        DO I=1,NPOIN
          X(I) = 0.D0
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('Y     ')
C
        DO I=1,NPOIN
          X(I) = Y(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('+Y    ')
C
        DO I=1,NPOIN
          X(I) = Y(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('-Y    ')
C
        DO I=1,NPOIN
          X(I) = - Y(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('1/Y   ')
C
        DO I=1,NPOIN
          X(I) = 1.D0/Y(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('Y+Z   ')
C
        DO I=1,NPOIN
          X(I) = Y(I) + Z(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('Y-Z   ')
C
        DO I=1,NPOIN
          X(I) = Y(I) - Z(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('YZ    ')
C
        DO I=1,NPOIN
          X(I) = Y(I) * Z(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('-YZ   ')
C
        DO I=1,NPOIN
          X(I) = - Y(I) * Z(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('XY    ')
C
        DO I=1,NPOIN
          X(I) = X(I) * Y(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('X+YZ  ')
C
        DO I=1,NPOIN
          X(I) = X(I) + Y(I) * Z(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('X-YZ  ')
C
        DO I=1,NPOIN
          X(I) = X(I) - Y(I) * Z(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('CXY   ')
C
         DO I=1,NPOIN
           X(I) = C * X(I) * Y(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('CYZ   ')
C
        DO I=1,NPOIN
          X(I) = C * Y(I) * Z(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('CXYZ  ')
C
        DO I=1,NPOIN
          X(I) = C * X(I) * Y(I) * Z(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('X+CYZ ')
C
        DO I=1,NPOIN
          X(I) = X(I) + C * Y(I) * Z(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('Y/Z   ')
C
        DO I=1,NPOIN
          X(I) = Y(I) / Z(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('CY/Z  ')
C
        DO I=1,NPOIN
          X(I) = C*Y(I) / Z(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('CXY/Z ')
C
        DO I=1,NPOIN
          X(I) = C*X(I)*Y(I) / Z(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('X+CY/Z')
C
        DO I=1,NPOIN
          X(I) = X(I) + C * Y(I) / Z(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('X+Y   ')
C
        DO I=1,NPOIN
          X(I) = X(I) + Y(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('X-Y   ')
C
        DO I=1,NPOIN
          X(I) = X(I) - Y(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('CX    ')
C
        DO I=1,NPOIN
          X(I) = C * X(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('CY    ')
C
        DO I=1,NPOIN
          X(I) = C * Y(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('Y+CZ  ')
C
        DO I=1,NPOIN
          X(I) = Y(I) + C * Z(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('X+CY  ')
C
        DO I=1,NPOIN
          X(I) = X(I) + C * Y(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('SQR(Y)')
C
        DO I=1,NPOIN
          X(I) = SQRT(Y(I))
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('ABS(Y)')
C
        DO I=1,NPOIN
          X(I) = ABS(Y(I))
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('N(Y,Z)')
C
        DO I=1,NPOIN
          X(I) = SQRT( Y(I)**2 + Z(I)**2 )
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('Y+C   ')
C
        DO I=1,NPOIN
          X(I) = Y(I) + C
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('X+C   ')
C
        DO I=1,NPOIN
          X(I) = X(I) + C
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('Y**C  ')
C
        DO I=1,NPOIN
          IF(Y(I).GE.0.D0) THEN
            X(I) = Y(I)**C
          ELSE
            IF (LNG.EQ.1) WRITE(LU,100)
            IF (LNG.EQ.2) WRITE(LU,101)
100         FORMAT(1X,'OV (BIEF) : Y**C INTERDIT SI Y < 0')
101         FORMAT(1X,'OV (BIEF): Y**C FORBIDDEN IF Y < 0')
            CALL PLANTE(1)
            STOP
          ENDIF
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('COS(Y)')
C
        DO I=1,NPOIN
          X(I) = COS(Y(I))
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('SIN(Y)')
C
        DO I=1,NPOIN
          X(I) = SIN(Y(I))
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('ATN(Y)')
C
        DO I=1,NPOIN
          X(I) = ATAN(Y(I))
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('A(Y,Z)')
C
        DO I=1,NPOIN
          X(I) = ATAN2(Y(I),Z(I))
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('+(Y,C)')
C
        DO I=1,NPOIN
          X(I) = MAX(Y(I),C)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('-(Y,C)')
C
        DO I=1,NPOIN
          X(I) = MIN(Y(I),C)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('+(Y,Z)')
C
        DO I=1,NPOIN
          X(I) = MAX(Y(I),Z(I))
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('-(Y,Z)')
C
        DO I=1,NPOIN
          X(I) = MIN(Y(I),Z(I))
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('YIFZ<C')
C
        DO I=1,NPOIN
          IF ( Z(I).LT.C ) X(I) = Y(I)
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE('C(Y-Z)')
C
        DO I=1,NPOIN
          X(I) = C*(Y(I)-Z(I))
        ENDDO
C
C-----------------------------------------------------------------------
C
        CASE DEFAULT
C
          IF (LNG.EQ.1) WRITE(LU,1000) OP
          IF (LNG.EQ.2) WRITE(LU,1001) OP
1000      FORMAT(1X,'OV (BIEF) : OPERATION INCONNUE: ',A8)
1001      FORMAT(1X,'OV (BIEF) : UNKNOWN OPERATION: ',A8)
          CALL PLANTE(1)
          STOP
C
C-----------------------------------------------------------------------
C
      END SELECT
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C