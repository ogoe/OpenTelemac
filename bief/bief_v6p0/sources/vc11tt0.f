C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES THE FOLLOWING VECTOR IN FINITE ELEMENTS:
!>  @code
!>   (EXAMPLE OF THE X COMPONENT, WHICH CORRESPONDS TO ICOORD=1)<br>
!>                       /            DF
!>    VEC(I)  =  XMUL   /  ( G  P  *( --  )) D(OMEGA)
!>                     /OMEGA    I    DX<br><br>
!>    P   IS A LINEAR BASE
!>     I<br>
!>    F IS A VECTOR OF TYPE P1
!>    W IS A VECTOR OF TYPE P0
!>    G IS ALSO A VECTOR OF TYPE P0
!>    IT IS THEREFORE NOT REQUIRED TO ASSEMBLE THE RESULT VECTOR
!>    THE RESULT IS DIRECTLY IN W
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  THE JACOBIAN MUST BE POSITIVE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, G, ICOORD, IKLE1, IKLE2, IKLE3, IKLE4, NELEM, NPOIN, SF, SG, W, X, XMUL, Y, Z
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DET, F1, F2, F2MF1, F3, F3MF1, F4, F4MF1, I1, I2, I3, I4, IELEM, IELMF, IELMG, PX, PY, X1, X2, X3, X4, XSUR24, Y1, Y2, Y3, Y4
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> PX, PY
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>VECTOS()

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
!>      <td><center> 5.5                                       </center>
!> </td><td> **/06/04
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>F,G,H
!></td><td>--></td><td>FONCTIONS INTERVENANT DANS LA FORMULE.
!>    </td></tr>
!>          <tr><td>ICOORD
!></td><td>--></td><td>COORDONNEE SUIVANT LAQUELLE ON DERIVE.
!>    </td></tr>
!>          <tr><td>IKLE1,
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE3
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IKLE4
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DANS LE MAILLAGE
!>    </td></tr>
!>          <tr><td>SF,SG,SH
!></td><td>--></td><td>STRUCTURES DES FONCTIONS F,G ET H
!>    </td></tr>
!>          <tr><td>SU,SV,SW
!></td><td>--></td><td>STRUCTURES DES FONCTIONS U,V ET W
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACE DES ELEMENTS.
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTES D'UN VECTEUR
!>                  INTERVENANT DANS LA FORMULE.
!>    </td></tr>
!>          <tr><td>W
!></td><td><--</td><td>VECTEUR RESULTAT
!>    </td></tr>
!>          <tr><td>X, Y, Z
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>COEFFICIENT MULTIPLICATEUR.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VC11TT0
     &( XMUL,SF,SG,F,G,X,Y,Z,IKLE1,IKLE2,IKLE3,IKLE4,NELEM,NPOIN,
     &  W,ICOORD)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LA FORMULE.
C| ICOORD         |-->| COORDONNEE SUIVANT LAQUELLE ON DERIVE.
C| IKLE1,         |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE.
C| IKLE2          |---| 
C| IKLE3          |---| 
C| IKLE4          |---| 
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE.
C| NPOIN          |-->| NOMBRE DE POINTS DANS LE MAILLAGE
C| SF,SG,SH       |-->| STRUCTURES DES FONCTIONS F,G ET H
C| SU,SV,SW       |-->| STRUCTURES DES FONCTIONS U,V ET W
C| SURFAC         |-->| SURFACE DES ELEMENTS.
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR
C|                |   | INTERVENANT DANS LA FORMULE.
C| W             |<--| VECTEUR RESULTAT
C| X, Y, Z        |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELEM          ! NUMBER OF ELEMENTS
      INTEGER, INTENT(IN) :: NPOIN          ! NUMBER OF POINTS
      INTEGER, INTENT(IN) :: ICOORD         ! DIRECTION OF GRAD :
                                            ! 1 - X
                                            ! 2 - Y
                                            ! 3 - Z
C
      INTEGER, INTENT(IN) :: IKLE1(NELEM)  ! NODE NUMBER 1 OF ELEMENTS
      INTEGER, INTENT(IN) :: IKLE2(NELEM)  ! NODE NUMBER 2 OF ELEMENTS
      INTEGER, INTENT(IN) :: IKLE3(NELEM)  ! NODE NUMBER 3 OF ELEMENTS
      INTEGER, INTENT(IN) :: IKLE4(NELEM)  ! NODE NUMBER 4 OF ELEMENTS
C
      DOUBLE PRECISION, DIMENSION(NPOIN), TARGET, INTENT(IN) :: X,Y,Z
      DOUBLE PRECISION, INTENT(IN)         :: XMUL        ! CONSTANT FACTOR
      DOUBLE PRECISION, INTENT(OUT)        :: W(NELEM)   ! RESULT
C
C     STRUCTURES OF F, G, H, U, V, W AND REAL DATA
C
      TYPE(BIEF_OBJ)  , INTENT(IN)   :: SF,SG !
      DOUBLE PRECISION, INTENT(IN)   :: G(*)  ! VECTOR TO MULTIPLY BY
      DOUBLE PRECISION, INTENT(IN)   :: F(*)  ! VECTOR WE COMPUTE THE
                                              ! GRAD OF
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C LOCAL VARIABLES
C
      INTEGER          :: IELEM,IELMF,IELMG  ! ELEMENT TYPES
      DOUBLE PRECISION :: F1,F2,F3,F4        ! THE 4 VALUES OF F AT THE
                                             ! NODES OF AN ELEMENT
      DOUBLE PRECISION :: X2,X3,X4,Y2,Y3,Y4  ! DELTA_X, DELTA_Y
      DOUBLE PRECISION :: X1, Y1             ! COORD OF THE FIRST NODE
      INTEGER          :: I1,I2,I3,I4        ! THE NUMBERS OF THE NODES
                                             ! OF AN ELEMENT
C
      DOUBLE PRECISION, DIMENSION(:), POINTER :: PX, PY ! POINTER TO
                                                        ! THE COORD
      DOUBLE PRECISION          :: XSUR24
      DOUBLE PRECISION          :: F2MF1,F3MF1,F4MF1
      DOUBLE PRECISION          :: DET               ! JACOBIAN
C
C-----------------------------------------------------------------------
C INITIALISES
C
      XSUR24 = XMUL/24.D0
C
      IELMF = SF%ELM
      IELMG = SG%ELM
C
C-----------------------------------------------------------------------
C TEST ON THE COMPONENT TO DIFFERENTIATE :
C 1 FOR X, 2 FOR Y, 3 FOR Z. OTHER VALUES ARE NOT ALLOWED.
C THE POINTER POINTS TO THE ARRAYS OF THE COORDINATES THAT
C WILL BE USED.

      SELECT CASE (ICOORD )

       CASE ( 1 )

         PX => Y
         PY => Z

       CASE ( 2 )

         PX => Z
         PY => X

       CASE ( 3 )

         PX => X
         PY => Y

       CASE DEFAULT

         IF (LNG.EQ.1) WRITE(LU,202) ICOORD
         IF (LNG.EQ.2) WRITE(LU,203) ICOORD
 202     FORMAT(1X,'VC11TT0 (BIEF) : COMPOSANTE IMPOSSIBLE ',
     &        1I6,' VERIFIER ICOORD')
 203     FORMAT(1X,'VC11TT0 (BIEF) : IMPOSSIBLE COMPONENT ',
     &        1I6,' CHECK ICOORD')
         CALL PLANTE(1)

      END SELECT


      IF(IELMF.EQ.31.AND.IELMG.EQ.30) THEN


C LOOP ON THE ELEMENTS
      DO  IELEM = 1 , NELEM

C GETS THE ID OF THE FOUR NODES OF THE ELEMENT

         I1 = IKLE1(IELEM)
         I2 = IKLE2(IELEM)
         I3 = IKLE3(IELEM)
         I4 = IKLE4(IELEM)

C GETS THE FOUR NODAL VALUES OF THE VECTOR TO DIFFERENTIATE

         F1 = F(I1)
         F2 = F(I2)
         F3 = F(I3)
         F4 = F(I4)

C DIFFERENCES OF THE NODAL VALUES OF F

         F2MF1 = F2-F1
         F3MF1 = F3-F1
         F4MF1 = F4-F1

!
C  REAL COORDINATES OF THE POINTS OF THE ELEMENT (ORIGIN IN 1)
!
         X1  =  PX(I1)
         X2  =  PX(I2) - X1
         X3  =  PX(I3) - X1
         X4  =  PX(I4) - X1
         Y1  =  PY(I1)
         Y2  =  PY(I2) - Y1
         Y3  =  PY(I3) - Y1
         Y4  =  PY(I4) - Y1

         DET =  (X3*Y4-X4*Y3)*F2MF1 + (Y2*X4-X2*Y4)*F3MF1+
     &           (X2*Y3-Y2*X3)*F4MF1

C RESULT

         W(IELEM) = DET* G(IELEM) * XSUR24

      ENDDO


C
C=======================================================================
C     ERROR ON THE ELEMENT TYPES


      ELSE
C-----------------------------------------------------------------------
C
         IF (LNG.EQ.1) WRITE(LU,1100) IELMF,SF%NAME
         IF (LNG.EQ.1) WRITE(LU,1200) IELMG,SG%NAME
         IF (LNG.EQ.1) WRITE(LU,1300)
         IF (LNG.EQ.2) WRITE(LU,1101) IELMF,SF%NAME
         IF (LNG.EQ.2) WRITE(LU,1201) IELMG,SG%NAME
         IF (LNG.EQ.2) WRITE(LU,1301)
         CALL PLANTE(1)
         STOP
 1100    FORMAT(1X,'VC11TT0 (BIEF) :',/,
     &          1X,'DISCRETISATION DE F : ',1I6,
     &          1X,'NOM REEL : ',A6)
 1200    FORMAT(1X,'DISCRETISATION DE G : ',1I6,
     &          1X,'NOM REEL : ',A6)
 1300    FORMAT(1X,'CAS NON PREVU')
 1101    FORMAT(1X,'VC11TT0 (BIEF) :',/,
     &          1X,'DISCRETIZATION OF F:',1I6,
     &          1X,'REAL NAME: ',A6)
 1201    FORMAT(1X,'DISCRETIZATION OF G:',1I6,
     &          1X,'REAL NAME: ',A6)
 1301    FORMAT(1X,'CASE NOT IMPLEMENTED')

C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END SUBROUTINE VC11TT0
C
C#######################################################################
C