C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       ALLOCATES MEMORY FOR A VECTOR STRUCTURE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DIM2, IELM, NAT, NOM, STATUT, VEC
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ERR, I, IMAX, XMAX
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_ALLVEC
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> NBMPTS(), NBPTS(), OV()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ALLMAT(), ALLVEC_IN_BLOCK(), ALMESH(), INBIEF(), PARINI(), POINT_ADJ_T2D(), POINT_ARTEMIS(), POINT_SISYPHE(), POINT_TELEMAC2D(), POINT_TELEMAC3D(), POINT_TOMAWAC(), PROSOU(), TRISOU()

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
!>      <td><center> 5.6                                       </center>
!> </td><td> 09/01/06
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DIM2
!></td><td>--></td><td>DEUXIEMME DIMENSION DU VECTEUR
!>    </td></tr>
!>          <tr><td>IELM
!></td><td>--></td><td>TYPE D'ELEMENT DU VECTEUR, OU DIMENSION
!>                  (SUIVANT LE STATUT, VOIR PLUS BAS)
!>    </td></tr>
!>          <tr><td>NAT
!></td><td><--</td><td>1: VECTEUR REEL   2:VECTEUR ENTIER
!>    </td></tr>
!>          <tr><td>NOM
!></td><td>--></td><td>NOM FORTRAN DU TABLEAU
!>    </td></tr>
!>          <tr><td>STATUT
!></td><td>--></td><td>STATUT DU VECTEUR :
!>                  0 : VECTEUR LIBRE, IELM EST ALORS SA DIMENSION
!>                  1 : VECTEUR DEFINI SUR LE MAILLAGE
!>                  IELM EST ALORS LE TYPE D'ELEMENT
!>                  CHANGEMENT DE DISCRETISATION INTERDIT
!>                  2 : COMME 1 MAIS CHANGEMENTS AUTORISES
!>    </td></tr>
!>          <tr><td>VEC
!></td><td><--</td><td>VECTEUR A ALLOUER
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ALLVEC
     &( NAT , VEC , NOM , IELM , DIM2 , STATUT )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DIM2           |-->| DEUXIEMME DIMENSION DU VECTEUR
C| IELM           |-->| TYPE D'ELEMENT DU VECTEUR, OU DIMENSION
C|                |   | (SUIVANT LE STATUT, VOIR PLUS BAS)
C| NAT            |<--| 1: VECTEUR REEL   2:VECTEUR ENTIER
C| NOM            |-->| NOM FORTRAN DU TABLEAU
C| STATUT         |-->| STATUT DU VECTEUR :
C|                |   | 0 : VECTEUR LIBRE, IELM EST ALORS SA DIMENSION
C|                |   | 1 : VECTEUR DEFINI SUR LE MAILLAGE
C|                |   | IELM EST ALORS LE TYPE D'ELEMENT
C|                |   | CHANGEMENT DE DISCRETISATION INTERDIT
C|                |   | 2 : COMME 1 MAIS CHANGEMENTS AUTORISES
C| VEC            |<--| VECTEUR A ALLOUER
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_ALLVEC => ALLVEC
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ)  , INTENT(INOUT) :: VEC
      INTEGER         , INTENT(IN)    :: NAT,IELM,DIM2,STATUT
      CHARACTER(LEN=6), INTENT(IN)    :: NOM
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ERR
      INTRINSIC MAX
C
      INTEGER IMAX,I
      DOUBLE PRECISION XMAX
C
C-----------------------------------------------------------------------
C  HEADER COMMON TO ALL OBJECTS
C-----------------------------------------------------------------------
C
C     KEY OF THE OBJECT - TO CHECK MEMORY CRASHES
C
      VEC%KEY = 123456
C
C     TYPE OF THE OBJECT (HERE VECTOR)
C
      VEC%TYPE = 2
C
C     NAME OF THE OBJECT
C
      VEC%NAME = NOM
C
C-----------------------------------------------------------------------
C  PART SPECIFIC TO VECTORS
C-----------------------------------------------------------------------
C
C     NATURE
C
      VEC%NAT = NAT
C
C     MAXIMUM SIZE PER DIMENSION
C
      IF(STATUT.EQ.1.OR.STATUT.EQ.2) THEN
        VEC%MAXDIM1 = NBMPTS(IELM)
      ELSE
        VEC%MAXDIM1 = IELM
      ENDIF
C
C     VEC%MAXDIM1 MUST BE AT LEAST 1
C     TO AVOID BOUND CHECKING ERRORS ON SOME COMPILERS
C
      VEC%MAXDIM1=MAX(VEC%MAXDIM1,1)
C
C     DISCRETISES
C
      IF(STATUT.EQ.1.OR.STATUT.EQ.2) THEN
        VEC%ELM = IELM
      ELSE
        VEC%ELM = -1000
      ENDIF
C
C     FIRST DIMENSION OF VECTOR
C
      IF(STATUT.EQ.1.OR.STATUT.EQ.2) THEN
        VEC%DIM1 = NBPTS(IELM)
      ELSE
        VEC%DIM1 = IELM
      ENDIF
C
C     SECOND DIMENSION OF VECTOR (VEC%DIM2 MAY BE CHANGED)
C
      VEC%DIM2    = DIM2
      VEC%MAXDIM2 = DIM2
C
C     CASE OF DISCONTINUITY BETWEEN ELEMENTS
C     (SEE CORRSL, VC13AA, VC13BB)
C
      VEC%DIMDISC = 0
C
C     STATUS
C
      VEC%STATUS = STATUT
C
C     INFORMATION ON CONTENT
C
      VEC%TYPR = '?'
      VEC%TYPI = '?'
C
C     DYNAMICALLY ALLOCATES MEMORY (REAL OR INTEGER, DEPENDING OF NAT)
C
      IF(NAT.EQ.1) THEN
C
        ALLOCATE(VEC%R(VEC%MAXDIM1*VEC%DIM2),STAT=ERR)
C       JAJ NULLIFY THE INTEGER PART
        NULLIFY(VEC%I)
C
C       FILLS ARRAY WITH BIG NUMBERS
C       TO RAISE QUESTIONS IF NOT INITIALISED
C
        XMAX = HUGE(100.D0)
        CALL OV('X=C     ',VEC%R,VEC%R,VEC%R,XMAX,
     &          VEC%MAXDIM1*VEC%DIM2)
C
      ELSEIF(NAT.EQ.2) THEN
C
        ALLOCATE(VEC%I(VEC%MAXDIM1*VEC%DIM2),STAT=ERR)
C       JAJ NULLIFY THE REAL PART
        NULLIFY(VEC%R)
C
C       FILLS ARRAY WITH BIG NUMBERS
C       TO RAISE QUESTIONS IF NOT INITIALISED
C
        IMAX = HUGE(100)
          DO I=1,VEC%MAXDIM1*VEC%DIM2
            VEC%I(I) = IMAX
          ENDDO
C
      ELSE
        STOP 'UNKNOWN NAT IN ALLVEC'
      ENDIF
C
C-----------------------------------------------------------------------
C
      IF(ERR.EQ.0) THEN
C       IF(LNG.EQ.1) WRITE(LU,*) 'VECTEUR : ',NOM,' ALLOUE'
C       IF(LNG.EQ.2) WRITE(LU,*) 'VECTOR: ',NOM,' ALLOCATED'
      ELSE
        IF(LNG.EQ.1) WRITE(LU,10) NOM,ERR
        IF(LNG.EQ.2) WRITE(LU,20) NOM,ERR
10      FORMAT(1X,'ERREUR A L''ALLOCATION DU VECTEUR : ',A6,/,1X,
     &            'CODE D''ERREUR : ',1I6)
20      FORMAT(1X,'ERROR DURING ALLOCATION OF VECTOR: ',A6,/,1X,
     &            'ERROR CODE: ',1I6)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C