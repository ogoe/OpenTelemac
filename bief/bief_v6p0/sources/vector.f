C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES VECTORS.
!><br>            THE VECTOR IS IDENTIFIED BY THE FORMULATION IN
!>                THE CHARACTER STRING 'FORMUL'.
!><br>            'OP' IS = OR +.
!>  @code
!>  MEANING OF IELM1
!>
!>  TYPE OF ELEMENT      NUMBER OF POINTS
!>
!>   0 : P0 SEGMENT             1
!>   1 : P1 SEGMENT             2
!>
!>  10 : P0 TRIANGLE            1
!>  11 : P1 TRIANGLE            3
!>  12 : QUASI-BUBBLE TRIANGLE  4
!>
!>  20 : Q0 QUADRILATERAL       1
!>  21 : Q1 QUADRILATERAL       4
!>
!>  40 : TELEMAC-3D P0 PRISMS   1
!>  41 : TELEMAC-3D P1 PRISMS   6
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, FORMUL, G, H, IELM1, MASKEL, MESH, MSK, OP, U, V, VEC, W, XMUL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELM0, LEGO, NPT
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_VECTOR
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> DIMENS(), NBPTS(), PLANTE(), VECTOS()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BEDLOAD_EFFPNT(), BEDLOAD_SECCURRENT(), BEDLOAD_SOLVS_FE(), BERKHO(), BILAN(), BILANT(), BILANT1(), BILAN_SISYPHE(), CALCUE(), CALRES(), CFLPSI(), CORMAR(), COST_FUNCTION(), CVDF3D(), CVDFTR(), CVTRVF(), CVTRVF_POS(), DEBIMP(), DEBIMP3D(), DIFF3D(), DRAGFO(), FILTER_H(), FLUX3D(), FRICTI(), FSGRAD(), GRAD2D(), INBIEF(), INITAB(), KEPSIL(), MASBAS2D(), MASS3D(), MESH_PROP(), MESURES(), PREDIV(), PROPAG(), PROPAG_ADJ(), PROSOU(), RADIA1(), RADIA2(), RADIAT(), SISYPHE(), SMAGO(), SMAGO3D(), SMAGOR(), SOUKEP(), SOUKOM(), SOURCES_SINKS(), STRESS(), SUSPENSION_BILAN(), SUSPENSION_COMPUTATION(), SUSPENSION_MAIN(), TELEMAC2D(), TELEMAC3D(), TRISOU(), VELRES(), VGFPSI(), VOLFIN(), WAVE_EQUATION(), WCTURB(), WSTARW()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 25/06/2008
!> </td><td> JM HERVOUET (LNHE) 01 30 87 80 18
!> </td><td> NO CALL TO VECTOS IF THE NUMBER OF ELEMENTS IS 0
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 22/09/05
!> </td><td> REGINA NEBAUER
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>F,G,H
!></td><td>--></td><td>FONCTIONS INTERVENANT DANS LA FORMULE
!>    </td></tr>
!>          <tr><td>FORMUL
!></td><td>--></td><td>FORMULE DECRIVANT LE VECTEUR
!>    </td></tr>
!>          <tr><td>IELM1
!></td><td>--></td><td>TYPE D'ELEMENT DU VECTEUR.
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS
!>                  =1. : NORMAL   =0. : ELEMENT MASQUE
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>STRUCTURE DE MAILLAGE : BLOC DES ENTIERS.
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>OP
!></td><td>--></td><td>'=' : ON FAIT VEC= LE VECTEUR
!>                  '+' : ON FAIT VEC=VEC+ LE VECTEUR
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTES D'UN VECTEUR U DANS LA FORMULE
!>    </td></tr>
!>          <tr><td>VEC
!></td><td><-></td><td>VECTEUR A REMPLIR OU MODIFIER
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>COEFFICIENT MULTIPLICATEUR DU RESULTAT
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VECTOR
     &(VEC,OP,FORMUL,IELM1,XMUL,F,G,H,U,V,W,MESH,MSK,MASKEL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LA FORMULE
C| FORMUL         |-->| FORMULE DECRIVANT LE VECTEUR
C| IELM1          |-->| TYPE D'ELEMENT DU VECTEUR.
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
C| MESH           |-->| STRUCTURE DE MAILLAGE : BLOC DES ENTIERS.
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| OP             |-->| '=' : ON FAIT VEC= LE VECTEUR
C|                |   | '+' : ON FAIT VEC=VEC+ LE VECTEUR
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR U DANS LA FORMULE
C| VEC            |<->| VECTEUR A REMPLIR OU MODIFIER
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR DU RESULTAT
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_VECTOR => VECTOR
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      TYPE(BIEF_OBJ),    INTENT(INOUT) :: VEC
      DOUBLE PRECISION,  INTENT(IN)    :: XMUL
      INTEGER,           INTENT(IN)    :: IELM1
      LOGICAL,           INTENT(IN)    :: MSK
      CHARACTER(LEN=16), INTENT(IN)    :: FORMUL
      CHARACTER(LEN=1),  INTENT(IN)    :: OP
      TYPE(BIEF_OBJ),    INTENT(IN)    :: F,G,H,U,V,W,MASKEL
      TYPE(BIEF_MESH),   INTENT(INOUT) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER  :: NPT   ! NUMBER OF POINTS PER ELEMENT
      LOGICAL  :: LEGO  ! ASSEMBLY OR NOT
      INTEGER  :: IELM0 ! P0 DISCRETISATION
C
C-----------------------------------------------------------------------
C  POSSIBLE CHANGE OF DISCRETISATION
C-----------------------------------------------------------------------
C IF A VECTOR HAS BEEN ALLOCATED WITH STATUS 1, I.E ITS DISCRETISATION
C CANNOT CHANGE, IT IS NECESSARY TO TEST THE COHERENCE BETWEEN THE
C DISCRETISATION OF THE VECTOR AND THAT PROPOSED IN ARGUMENT.
C MIGHT HAVE SURPRISES OTHERWISE !!!!
C
      IF(VEC%STATUS.EQ.1.AND.VEC%ELM.NE.IELM1) THEN
        IF(LNG.EQ.1) WRITE(LU,1001) VEC%NAME,VEC%ELM,IELM1
        IF(LNG.EQ.2) WRITE(LU,1002) VEC%NAME,VEC%ELM,IELM1
1001    FORMAT(1X,'VECTOR : CHANGEMENT DE DISCRETISATION IMPOSSIBLE',
     &  ' POUR VECTEUR ',A6,' : ',1I6,' <=> ',1I6)
1002    FORMAT(1X,'VECTOR: CHANGING DISCRETIZATION FORBIDDEN',
     &  ' FOR THE VECTOR ',A6,' : ',1I6,' <=> ',1I6)
        CALL PLANTE(1)
        STOP
      ELSEIF(VEC%STATUS.EQ.2.OR.VEC%STATUS.EQ.1) THEN
        NPT = NBPTS(IELM1)
        VEC%ELM = IELM1
        VEC%DIM1= NPT
      ELSE
        IF(LNG.EQ.1) THEN
          WRITE(LU,*) 'LE VECTEUR ',VEC%NAME,' A UN STATUT EGAL A',
     &                VEC%STATUS,' IL NE PEUT ETRE UTILISE DANS VECTOR'
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,*) 'VECTOR ',VEC%NAME,' HAS A STATUS ',VEC%STATUS,
     &                ' IT CANNOT BE USED IN SUBROUTINE VECTOR'
        ENDIF
        CALL PLANTE(1)
        STOP
      ENDIF
C
C ASSEMBLY: NOT PERFORMED FOR VECTORS
C RESULT OF P0 DISCRETISATION
C LEGO IS SET TO TRUE IF THE RESULTANT VECTOR DISCRETISATION
C IS P1, FALSE OTHERWISE.
C
      IELM0 = 10*(IELM1/10)
      LEGO  = IELM0 .NE. IELM1
C
C MODIFICATION: IN THE FUTURE VARIABLE LEGO IS PASSED IN ARGUMENT
C
C-----------------------------------------------------------------------
C  CALLS THE SUBROUTINE THAT SHUNTS AND ASSEMBLES
C-----------------------------------------------------------------------
C
      IF(DIMENS(IELM1).EQ.MESH%DIM) THEN
C       NORMAL VECTOR: CALL WITH SURFAC, IKLE, NELEM, NELMAX
C                                XEL, YEL, ZEL
        CALL VECTOS(VEC%R,OP,FORMUL,XMUL,
     &              F%R,G%R,H%R,U%R,V%R,W%R,
     &              F,G,H,U,V,W,MESH%W%R,LEGO,
     &              MESH%XEL%R  , MESH%YEL%R  , MESH%ZEL%R  ,
     &              MESH%SURFAC%R,MESH%IKLE%I,MESH%NBOR%I,
     &              MESH%XSGBOR%R, MESH%YSGBOR%R, MESH%ZSGBOR%R,
     &              NPT,MESH%NELEM,MESH%NELMAX,
     &              IELM1,MESH%LV,MSK,MASKEL%R,MESH)
      ELSE
C       BOUNDARY VECTOR: CALL WITH LGSEG, IKLBOR, NELEB, NELEBX
C                                  X, Y, Z
        IF(MESH%NELEB.GT.0) THEN
          CALL VECTOS(VEC%R,OP,FORMUL,XMUL,
     &                F%R,G%R,H%R,U%R,V%R,W%R,
     &                F,G,H,U,V,W,MESH%W%R,LEGO,
     &                MESH%X%R,MESH%Y%R,MESH%Z%R  ,
     &                MESH%LGSEG%R,MESH%IKLBOR%I,MESH%NBOR%I,
     &                MESH%XSGBOR%R,MESH%YSGBOR%R,MESH%ZSGBOR%R,
     &                NPT,MESH%NELEB,MESH%NELEBX,
     &                IELM1,MESH%LV,MSK,MASKEL%R,MESH)
        ENDIF
C
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C