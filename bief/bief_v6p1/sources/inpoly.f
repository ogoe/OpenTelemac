C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       INDICATES IF A POINT WITH COORDINATES X AND Y IS
!>                IN A POLYGON WITH GIVEN VERTICES.
!>  @code
!>    PRINCIPLE: TAKES HALF A LINE STARTING FROM THE POINT AND COUNTS THE
!>               NUMBER OF TIMES IT INTERSECTS WITH THE POLYGON
!>
!>    ALSO WORKS IF THE POLYGON IS NOT CONVEX
!>
!>    INTERSECTIONS ARE IDENTIFIED USING THE LINES PARAMETRIC EQUATIONS :
!>
!>
!>    X + A * MU = XDEP + (XARR-XDEP) * LAMBDA
!>    Y + B * MU = YDEP + (YARR-YDEP) * LAMBDA
!>
!>    THE HALF-LINE IS CHARACTERISED BY THE CHOICE OF A AND B, AND THE
!>    SIGN OF MU. THERE IS INTERSECTION IF MU > 0 AND 0 < LAMBDA < 1
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE POLYGON VERTICES MUST BE DISTINCT (NO DUPLICATE NODES)

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> NSOM, X, XSOM, Y, YSOM
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> A, ANGLE, B, DET, EPS, LAMBDA, MU, N, NSECT, XARR, XDEP, YARR, YDEP
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>DRAGFO(), FILP10(), FILP11(), FILP12()

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
!> </td><td> 27/07/99
!> </td><td> JEAN-PHILIPPE RENAUD (CSN BRISTOL)
!> </td><td> CORRECTION FOR A SPECIAL CASE
!> </td></tr>
!>      <tr>
!>      <td><center> 5.2                                       </center>
!> </td><td> 18/06/96
!> </td><td> J.-M. HERVOUET (LNH) 30 87 83 81
!> </td><td>
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td>
!> </td><td> E. DAVID (LHF)
!> </td><td> ORIGINAL IDEA AND CODE
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>NSOM
!></td><td>---</td><td>: -->: NOMBRE DE SOMMETS
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>---</td><td>:: COORDONNEES DU POINT.
!>    </td></tr>
!>          <tr><td>XSOM
!></td><td>---</td><td>: -->: TABLEAU DES ABSCISSES DES SOMMETS DU POLYGONE
!>    </td></tr>
!>          <tr><td>YSOM
!></td><td>---</td><td>: -->: TABLEAU DES ORDONNEES DES SOMMETS DU POLYGONE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        LOGICAL FUNCTION INPOLY
     &( X , Y , XSOM , YSOM , NSOM )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| NSOM           |---| : -->: NOMBRE DE SOMMETS
C| X,Y            |---| :: COORDONNEES DU POINT.
C| XSOM           |---| : -->: TABLEAU DES ABSCISSES DES SOMMETS DU POLYGONE
C| YSOM           |---| : -->: TABLEAU DES ORDONNEES DES SOMMETS DU POLYGONE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NSOM
      DOUBLE PRECISION, INTENT(IN) :: X , Y
      DOUBLE PRECISION, INTENT(IN) :: XSOM(NSOM) , YSOM(NSOM)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER N,NSECT
C
      DOUBLE PRECISION A,B,ANGLE,XDEP,YDEP,XARR,YARR,DET,MU,LAMBDA,EPS
C
      INTRINSIC COS,SIN,ABS,MOD
C
C-----------------------------------------------------------------------
C
      EPS = 1.D-9
      ANGLE = -1.D0
C
C CHOOSES A AND B SUCH AS TO AVOID SPECIAL CASES
C
1000  CONTINUE
      ANGLE = ANGLE + 1.D0
      IF(ANGLE.GT.360.D0) THEN
C       SPECIAL CASE OF A POINT ON THE CONTOUR
        INPOLY=.TRUE.
        RETURN
      ENDIF
      A = COS(ANGLE*3.141592653D0/180.D0)
      B = SIN(ANGLE*3.141592653D0/180.D0)
      NSECT=0
C
C LOOP ON ALL THE SEGMENTS OF THE POLYGON
C
      DO 10 N=1,NSOM
C
C     DEP : 1ST POINT OF THE SEGMENT    ARR : 2ND POINT
C
      XDEP=XSOM(N)
      YDEP=YSOM(N)
      IF(N.LT.NSOM) THEN
        XARR=XSOM(N+1)
        YARR=YSOM(N+1)
      ELSE
        XARR=XSOM(1)
        YARR=YSOM(1)
      ENDIF
C
C     CASE WHERE TWO SUCCESSIVE POINTS ARE DUPLICATES
C
      IF(ABS(XDEP-XARR)+ABS(YDEP-YARR).LT.EPS) THEN
        WRITE(LU,*) ' '
        WRITE(LU,*) ' '
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'INPOLY : POINTS CONFONDUS DANS LE POLYGONE'
          WRITE(LU,*) 'AU POINT DE COORDONNEES : ',XDEP,'  ET  ',YDEP
          WRITE(LU,*) 'DE NUMERO ',N
          IF(N.EQ.NSOM) THEN
            WRITE(LU,*) 'LE DERNIER POINT NE DOIT PAS ETRE EGAL AU'
            WRITE(LU,*) 'PREMIER (POUR UN TRIANGLE, PAR EXEMPLE,'
            WRITE(LU,*) 'IL FAUT DONNER TROIS POINTS ET NON QUATRE)'
          ENDIF
          WRITE(LU,*) 'INPOLY EST PROBABLEMENT CALLED BY FILPOL'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'INPOLY: SUPERIMPOSED POINTS IN THE POLYGON'
          WRITE(LU,*) 'AT POINT: ',XDEP,'  AND  ',YDEP,' WITH NUMBER ',N
          IF(N.EQ.NSOM) THEN
            WRITE(LU,*) 'THE LAST POINT MUST NOT BE EQUAL TO THE FIRST'
            WRITE(LU,*) 'FOR EXAMPLE, GIVE 3 POINTS FOR A TRIANGLE'
          ENDIF
          WRITE(LU,*) 'INPOLY IS PROBABLY CALLED BY FILPOL'
        ENDIF
        WRITE(LU,*) ' '
        WRITE(LU,*) ' '
        STOP
      ENDIF
C
C     CASE WHERE THE POINT IS A VERTEX
C     (THE GENERAL ALGORITHM WOULD DEFINE IT AS EXTERNAL WITH 2 INTERSECTIONS)
C
      IF(ABS(X-XDEP).LE.EPS.AND.ABS(Y-YDEP).LE.EPS) THEN
        NSECT=1
        GO TO 2000
      ENDIF
C
C     DETERMINANT OF THE KRAMER SYSTEM
C
      DET = A*(YDEP-YARR)-B*(XDEP-XARR)
      IF(ABS(DET).LT.EPS) GO TO 1000
C
      MU     = ( (XDEP-X)*(YDEP-YARR)-(YDEP-Y)*(XDEP-XARR) ) / DET
      LAMBDA = (    A    *(YDEP-Y   )-    B   *(XDEP-X   ) ) / DET
C
C-------------------------------------------------------
C JP RENAUD (CSN BRISTOL) CORRECTION TO AVOID THAT THE INTERSECTION
C POINT BE ONE OF THE VRTICES
C
C IF THE INTERSECTION POINT IS A VERTEX, INCREASES THE ANGLE
C OTHERWISE THE POINT WOULD BE COUNTED TWICE INSTEAD OF JUST ONCE
C
      IF ((ABS(X+A*MU-XDEP).LE.EPS.AND.ABS(Y+B*MU-YDEP).LE.EPS)
     &    .OR.
     &    (ABS(X+A*MU-XARR).LE.EPS.AND.ABS(Y+B*MU-YARR).LE.EPS))
     &    GOTO 1000
C
C END OF JP RENAUD CORRECTION
C-------------------------------------------------------
C
      IF(MU.GE.-EPS.AND.LAMBDA.GE.-EPS.AND.LAMBDA.LE.1.D0+EPS) THEN
        NSECT=NSECT+1
      ENDIF
C
10    CONTINUE
C
2000  CONTINUE
C
      INPOLY=(MOD(NSECT,2).EQ.1)
C
C-----------------------------------------------------------------------
C
      RETURN
      END

C
C#######################################################################
C