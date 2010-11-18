C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       OPTIMISES THE TIME STEP.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CFLWTD, DT, DTVARI, EPS, G, H, LISTIN, NPOIN, NSEG, NUBO, QU, QV, VNOIN, X, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DIJ, HI, HJ, HMAX, IEL1, IEL2, ISEGIN, UI, UJ, UN, UNI, UNJ, VI, VJ, W1, XNC
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>RESOLU()

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
!> </td><td> 18/03/1998
!> </td><td> N.GOUTAL
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CFLWTD
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>DTVARI
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>EPS
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>G
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>H
!></td><td>--></td><td>HAUTEUR AU TEMPS N
!>    </td></tr>
!>          <tr><td>LISTIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NSEG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NUBO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QU
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>QV
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>UI,VI
!></td><td>--></td><td>COMPOSANTES DE LA VITESSE DE LA
!>                  CELLULE I
!>    </td></tr>
!>          <tr><td>VNOIN
!></td><td>--></td><td>NORMALE DU SEGMENT INTERNE
!>                  (2 PREMIERES COMPOSANTES) ET
!>                  LONGUEUR DE CE SEGMENT (3IEME COMPOSANTE)
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DES POINTS DU MAILLAGE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VFCFL
     &(NUBO,VNOIN,NSEG,NPOIN,X,Y,G,H,EPS,QU,QV,DT,CFLWTD,DTVARI,LISTIN)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CFLWTD         |---| 
C| DT             |---| 
C| DTVARI         |---| 
C| EPS            |---| 
C| G             |---| 
C| H             |-->| HAUTEUR AU TEMPS N
C| LISTIN         |---| 
C| NPOIN          |---| 
C| NSEG           |---| 
C| NUBO           |---| 
C| QU             |---| 
C| QV             |---| 
C| UI,VI          |-->| COMPOSANTES DE LA VITESSE DE LA
C|                |   | CELLULE I
C| VNOIN          |-->| NORMALE DU SEGMENT INTERNE
C|                |   | (2 PREMIERES COMPOSANTES) ET
C|                |   | LONGUEUR DE CE SEGMENT (3IEME COMPOSANTE)
C| X,Y            |-->| COORDONNEES DES POINTS DU MAILLAGE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NSEG,NPOIN
      INTEGER, INTENT(IN) :: NUBO(2,*)
      LOGICAL, INTENT(IN) :: DTVARI,LISTIN
      DOUBLE PRECISION, INTENT(IN) :: X(NPOIN),Y(NPOIN),H(NPOIN)
      DOUBLE PRECISION, INTENT(IN) :: QU(NPOIN),QV(NPOIN),VNOIN(3,*)
      DOUBLE PRECISION, INTENT(IN) :: EPS,G,CFLWTD
      DOUBLE PRECISION, INTENT(INOUT) :: DT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ISEGIN,IEL1,IEL2
C
      DOUBLE PRECISION W1,DIJ,UI,VI,UN,XNC,UNI,UNJ,UJ,VJ,HI,HJ,HMAX
C
      INTRINSIC SQRT,MAX,ABS
C
C-----------------------------------------------------------------------
C
C------
C 1. INITIALISATION
C------
C
      XNC = 0.D0
C
C------
C 2. LOOP OVER THE INTERIOR SEGMENTS
C------
C

      DO 100 ISEGIN = 1 , NSEG
C
         IEL1 = NUBO(1,ISEGIN)
         IEL2 = NUBO(2,ISEGIN)
C
C
C   --->    COMPUTES DIJ
C           -------------
C
       DIJ = SQRT((X(IEL1)-X(IEL2))**2+(Y(IEL1)-Y(IEL2))**2)
C
C
C   --->    COMPUTES THE COMPONENTS FOR 'INTERIOR SPEED'
C           --------------------------------------------
C
       HI = H(IEL1)
       IF (H(IEL1).GE.EPS) THEN
          UI = QU(IEL1)/H(IEL1)
          VI = QV(IEL1)/H(IEL1)
       ELSE
          UI = 0.D0
          VI = 0.D0
       ENDIF
C
C
       IF (H(IEL2).GE.EPS) THEN
          HJ = H(IEL2)
          UJ = QU(IEL2)/H(IEL2)
          VJ = QV(IEL2)/H(IEL2)
       ELSE
          HJ = 0.D0
          UJ = 0.D0
          VJ = 0.D0
       ENDIF
C
C
C   --->    PROJECTION OF THE VELOCITY ON THE NORMAL
C           --------------------------------------------
C
          UNI = ABS(UI*VNOIN(1,ISEGIN)+VI*VNOIN(2,ISEGIN))
          UNJ = ABS(UJ*VNOIN(1,ISEGIN)+VJ*VNOIN(2,ISEGIN))
          UN = (UNI+UNJ)/2.D0
          HMAX = MAX(HJ,HI)
C
C   --->    COMPUTES THE COURANT NUMBER
C           ---------------------------
C
          W1 = (UN+SQRT(G*HMAX))/DIJ
C
          IF (W1.GT.XNC) XNC = W1
C
100    CONTINUE
C
       IF(DTVARI) THEN
         DT = CFLWTD/XNC
         IF(LISTIN.AND.LNG.EQ.1) WRITE(LU,*) 'PAS DE TEMPS : ',DT
         IF(LISTIN.AND.LNG.EQ.2) WRITE(LU,*) 'TIME-STEP: ',DT
       ENDIF
C
       RETURN
       END
C
C#######################################################################
C