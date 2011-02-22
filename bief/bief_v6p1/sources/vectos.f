C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       COMPUTES VECTORS.
!><br>            THE VECTOR IS IDENTIFIED BY THE FORMULATION IN
!>                THE CHARACTER STRING 'FORMUL'.
!>  @code
!>  MEANING OF IELM1
!>
!>  TYPE OF ELEMENT      NUMBER OF POINTS
!>
!>  10 : P0 TRIANGLE            1
!>  11 : P1 TRIANGLE            3
!>  12 : QUASI-BUBBLE TRIANGLE  4
!>  13 : P2 TRIANGLE            6
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
!>    </th><td> F, FORMUL, G, H, IELM1, IKLE, LEGO, LV, MASKEL, MESH, MSK, NBOR, NELEM, NELMAX, NPT, OP, SF, SG, SH, SU, SURFAC, SV, SW, T, U, V, VEC, W, XEL, XMUL, XNOR, YEL, YNOR, ZEL, ZNOR
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> ICOORD, INIT, SPECAD
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ASSVEC(), PLANTE(), VC00AA(), VC00BB(), VC00CC(), VC00FF(), VC00FT(), VC00PP(), VC00PP2(), VC00TT(), VC01AA(), VC01BB(), VC01FF(), VC01FT(), VC01FT2(), VC01OO(), VC01PP(), VC01TT(), VC01TT0(), VC03AA(), VC03BB(), VC04AA(), VC04PP(), VC04TT(), VC05AA(), VC05FF(), VC05FT(), VC05OO(), VC08AA(), VC08BB(), VC08CC(), VC08PP(), VC08TT(), VC09AA(), VC10OO(), VC11AA(), VC11AA2(), VC11BB(), VC11PP(), VC11TT(), VC11TT0(), VC13AA(), VC13BB(), VC13CC(), VC13PP(), VC13PP2(), VC13TT(), VC14AA(), VC15AA(), VC16AA(), VC18PP(), VC19AA()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BEDLOAD_SOLVS_FE(), CORRECTION_DEPTH_2D(), CORRECTION_DEPTH_3D(), VECTOR()

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
!> </td><td> 16/07/07
!> </td><td> JM HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
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
!></td><td>--></td><td>TYPE D'ELEMENT
!>    </td></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
!>    </td></tr>
!>          <tr><td>LEGO
!></td><td>--></td><td>LOGIQUE : POUR ASSEMBLER LE VECTEUR
!>    </td></tr>
!>          <tr><td>LV
!></td><td>--></td><td>LONGUEUR DU VECTEUR POUR LA VECTORISATION
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS
!>                  =1. : NORMAL   =0. : ELEMENT MASQUE
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMEROTATION GLOBALE DES POINTS DE BORD
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NELMAX
!></td><td>--></td><td>NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
!>                  (CAS DU MAILLAGE ADAPTATIF)
!>    </td></tr>
!>          <tr><td>NPT
!></td><td>--></td><td>NOMBRE DE POINTS DU VECTEUR.
!>    </td></tr>
!>          <tr><td>OP
!></td><td>--></td><td>OPERATION A EFFECTUER.01TT0
!>    </td></tr>
!>          <tr><td>SF,SG,SH
!></td><td>--></td><td>STRUCTURES DE F,G ET H
!>    </td></tr>
!>          <tr><td>SU,SV,SW
!></td><td>--></td><td>STRUCTURES DE U,V ET W
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACE DES ELEMENTS.
!>    </td></tr>
!>          <tr><td>T
!></td><td>--></td><td>TABLEAU DE TRAVAIL QUI
!>                  CONTIENDRA LE VECTEUR NON ASSEMBLE
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTES D'UN VECTEUR U DANS LA FORMULE
!>    </td></tr>
!>          <tr><td>VEC
!></td><td><-></td><td>VECTEUR A REMPLIR OU MODIFIER
!>    </td></tr>
!>          <tr><td>XEL,YEL,ZEL
!></td><td>--></td><td>COORDONNEES DES POINTS DANS L'ELEMENT
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>COEFFICIENT MULTIPLICATEUR DU RESULTAT
!>    </td></tr>
!>          <tr><td>XNOR,YNOR,ZNOR
!></td><td>--></td><td>COMPOSANTES DES NORMALES.
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE VECTOS
     &(VEC,OP,FORMUL,
     & XMUL,F,G,H,U,V,W,SF,SG,SH,SU,SV,SW,
     & T,LEGO,
     & XEL,YEL,ZEL, SURFAC,IKLE,NBOR,
     & XNOR,YNOR,ZNOR,NPT,NELEM,NELMAX,IELM1,LV,MSK,MASKEL,MESH)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LA FORMULE
C| FORMUL         |-->| FORMULE DECRIVANT LE VECTEUR
C| IELM1          |-->| TYPE D'ELEMENT
C| IKLE           |-->| PASSAGE DE LA NUMEROTATION LOCALE A GLOBALE
C| LEGO           |-->| LOGIQUE : POUR ASSEMBLER LE VECTEUR
C| LV             |-->| LONGUEUR DU VECTEUR POUR LA VECTORISATION
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
C| MESH           |---| 
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| NBOR           |-->| NUMEROTATION GLOBALE DES POINTS DE BORD
C| NELEM          |-->| NOMBRE D'ELEMENTS DU MAILLAGE
C| NELMAX         |-->| NOMBRE MAXIMUM D'ELEMENTS DU MAILLAGE
C|                |   | (CAS DU MAILLAGE ADAPTATIF)
C| NPT            |-->| NOMBRE DE POINTS DU VECTEUR.
C| OP             |-->| OPERATION A EFFECTUER.01TT0
C| SF,SG,SH       |-->| STRUCTURES DE F,G ET H
C| SU,SV,SW       |-->| STRUCTURES DE U,V ET W
C| SURFAC         |-->| SURFACE DES ELEMENTS.
C| T              |-->| TABLEAU DE TRAVAIL QUI
C|                |   | CONTIENDRA LE VECTEUR NON ASSEMBLE
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR U DANS LA FORMULE
C| VEC            |<->| VECTEUR A REMPLIR OU MODIFIER
C| XEL,YEL,ZEL    |-->| COORDONNEES DES POINTS DANS L'ELEMENT
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR DU RESULTAT
C| XNOR,YNOR,ZNOR |-->| COMPOSANTES DES NORMALES.
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF !, EX_VECTOS => VECTOS
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NELMAX,NPT,NELEM,IELM1,LV
      INTEGER, INTENT(IN) :: IKLE(NELMAX,*),NBOR(*)
C
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELMAX)
      DOUBLE PRECISION, INTENT(IN)    :: XEL(*),YEL(*),ZEL(*)
      DOUBLE PRECISION, INTENT(IN)    :: XNOR(*),YNOR(*),ZNOR(*)
      DOUBLE PRECISION, INTENT(INOUT) :: T(NELMAX,*),VEC(*)
      DOUBLE PRECISION, INTENT(IN)    :: XMUL,MASKEL(NELMAX)
C
C     STRUCTURES OF FUNCTIONS F, G, H, U, V, W AND REAL DATA
C
      DOUBLE PRECISION, INTENT(IN) :: F(*),G(*),H(*),U(*),V(*),W(*)
C
      TYPE(BIEF_OBJ), INTENT(IN) :: SF,SG,SH,SU,SV,SW
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
C
      LOGICAL, INTENT(IN) :: MSK,LEGO
C
      CHARACTER(LEN=16), INTENT(IN) :: FORMUL
      CHARACTER(LEN=1), INTENT(IN) ::  OP
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER ICOORD
      LOGICAL INIT,SPECAD
C
C-----------------------------------------------------------------------
C
C     CHECKS THE TYPE OF VECTOR
C
C=======================================================================
C     MASS MATRIX VECTOR X VECTOR F EQUALS 1
C=======================================================================
C
      IF(FORMUL(1:16).EQ.'MASBAS          '.OR.
     &   FORMUL(1:16).EQ.'MASBAS2         '     ) THEN
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 TRIANGLE
C
        IF(IELM1.EQ.11) THEN
C
             CALL VC00AA(XMUL,SURFAC,NELEM,NELMAX,T(1,1),T(1,2),T(1,3))
C
C-----------------------------------------------------------------------
C
C       ELEMENT QUASI-BUBBLE TRIANGLE
C
        ELSEIF(IELM1.EQ.12) THEN
C
             CALL VC00BB(XMUL,SURFAC,NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3),T(1,4))
C
C
C-----------------------------------------------------------------------
C
C       ELEMENT P2 TRIANGLE
C
        ELSEIF(IELM1.EQ.13) THEN
C
             CALL VC00CC(XMUL,SURFAC,NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3),T(1,4),T(1,5),T(1,6))
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 SEGMENT
C
C       ELSEIF(IELM1.EQ.1) THEN
C
C            CALL VC00OO(XMUL,SURFAC,NELEM,NELMAX,T(1,1),T(1,2))
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 PRISM
C
        ELSEIF(IELM1.EQ.41) THEN
C
             IF(FORMUL(7:7).EQ.'2') THEN
C              COMPATIBLE WITH MASS-LUMPING IN 2D
               CALL VC00PP2(XMUL,ZEL,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3),T(1,4),T(1,5),T(1,6))
             ELSE
C              NORMAL COMPUTATION OF INTEGRAL OF BASIS FUNCTIONS
               CALL VC00PP(XMUL,ZEL,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3),T(1,4),T(1,5),T(1,6))
             ENDIF
C
C-----------------------------------------------------------------------
C
C       ELEMENT TETRAHEDRON T1
C
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
C
             CALL VC00TT(XMUL,XEL,YEL,ZEL,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4))
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 TRIANGLE (SIDE FACES OF A MESH OF PRISMS
C                            SPLIT IN TETRAHEDRONS)
C
        ELSEIF(IELM1.EQ.61) THEN
C
             CALL VC00FT(XMUL,XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),NBOR,
     &                   NELEM,NELMAX,T(1,1),T(1,2),T(1,3))
C
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 QUADRILATERAL
C
        ELSEIF(IELM1.EQ.71) THEN
C
C              FOR VERTICAL RECTANGULAR SIDES OF THE PRISMS
               CALL VC00FF(XMUL,XEL,YEL,ZEL,
     &                     IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),NBOR,
     &                     NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4))
C
C-----------------------------------------------------------------------
C       OTHER
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ELEMENT TYPE
C-----------------------------------------------------------------------
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
C
        ENDIF
C
C=======================================================================
C     MASS MATRIX VECTOR X VECTOR F
C=======================================================================
C
      ELSEIF(FORMUL(1:6).EQ.'MASVEC') THEN
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 TRIANGLE
C
        IF(IELM1.EQ.11) THEN
C
             CALL VC01AA(XMUL,SF,F,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                   NELEM,NELMAX,T(1,1),T(1,2),T(1,3) )
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 TRIANGLE (SIDE FACES OF A MESH OF PRISMS
C                            SPLIT IN TETRAHEDRONS)
C
        ELSEIF(IELM1.EQ.61.OR.IELM1.EQ.81) THEN
C
           IF(FORMUL(7:7).NE.'2') THEN
C
              CALL VC01FT(XMUL,SF,F,XEL,YEL,ZEL,
     &             IKLE(1,1),IKLE(1,2),IKLE(1,3),NBOR,
     &             NELEM,NELMAX,T(1,1),T(1,2),T(1,3))
           ELSE
C
              CALL VC01FT2(XMUL,SF,F,SG,G,XEL,YEL,ZEL,
     &             IKLE(1,1),IKLE(1,2),IKLE(1,3),NBOR,
     &             NELEM,NELMAX,T(1,1),T(1,2),T(1,3))
           ENDIF
C
C-----------------------------------------------------------------------
C
C       ELEMENT QUASI-BUBBLE TRIANGLE
C
        ELSEIF(IELM1.EQ.12) THEN
C
             CALL VC01BB(XMUL,SF,F,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4) )
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 SEGMENT
C
        ELSEIF(IELM1.EQ.1) THEN
C
             CALL VC01OO(XMUL,SF,F,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),NBOR,NELEM,NELMAX,
     &                   T(1,1),T(1,2)  )
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 QUADRILATERAL
C
        ELSEIF(IELM1.EQ.71) THEN
C
C              FOR VERTICAL RECTANGULAR SIDES OF THE PRISMS
               CALL VC01FF(XMUL,SF,F,XEL,YEL,ZEL,
     &                     IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),NBOR,
     &                     NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4))
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 PRISM
C
        ELSEIF(IELM1.EQ.41) THEN
C
             CALL VC01PP(XMUL,SF,F,ZEL,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3),T(1,4),T(1,5),T(1,6))
C
C-----------------------------------------------------------------------
C
C       ELEMENT T1 TETRAHEDRON
C
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
C
             CALL VC01TT(XMUL,SF,F,XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4))

C
C-----------------------------------------------------------------------
C
C       ELEMENT T0 TETRAHEDRON
C
        ELSEIF(IELM1.EQ.30) THEN
C
             CALL VC01TT0(XMUL,SF,F,XEL,YEL,ZEL,
     &                   IKLE(:,1),IKLE(:,2),IKLE(:,3),IKLE(:,4),
     &                   NELEM,NELMAX,VEC)


C
C-----------------------------------------------------------------------
C       OTHER
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ELEMENT TYPE
C-----------------------------------------------------------------------
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
C
        ENDIF
C
C=======================================================================
C                 -    ---->   --->
C     VECTOR:     F  . GRAD(U).GRAD(PSI)
C
C     F DIFFUSION TENSOR WITH DIAGONAL COMPONENTS F, G, H
C
C     EQUIVALENT OF MATDIF * U
C
C=======================================================================
C
      ELSEIF(FORMUL(1:6).EQ.'VECDIF') THEN
C
C       ELEMENT SEGMENT P1
C
        IF(IELM1.EQ.41.AND.FORMUL(8:8).EQ.'*') THEN
C
          CALL VC02PP_STAR(XMUL,SF,SG,SH,SU,F,G,H,U,XEL,YEL,ZEL,SURFAC,
     *                     IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     *                     IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),
     *                     T(1,2),T(1,3),T(1,4),T(1,5),T(1,6),FORMUL) 
C
C-----------------------------------------------------------------------
C       ERROR ON THE ELEMENT TYPE
C-----------------------------------------------------------------------
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
C
        ENDIF
C
C=======================================================================
C     VECTOR K GRAD(PSI) U.GRAD(F)
C=======================================================================
C
      ELSEIF(FORMUL(1:16).EQ.'SUPG            ') THEN
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 TRIANGLE
C
        IF(IELM1.EQ.11) THEN
C
             CALL VC03AA(XMUL,SF,SG,SH,SU,SV,F,G,H,U,V,
     &                   XEL,YEL,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3) )
C
C-----------------------------------------------------------------------
C
C       ELEMENT QUASI-BUBBLE TRIANGLE
C
        ELSEIF(IELM1.EQ.12) THEN
C
             CALL VC03BB(XMUL,SF,SG,SH,SU,SV,F,G,H,U,V,
     &                   XEL,YEL,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4) )
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 SEGMENT
C
C       ELSEIF(IELM1.EQ.1) THEN
C
C            CALL VC03OO(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
C    *                   XEL,YEL,ZEL,SURFAC,
C    *                   IKLE(1,1),IKLE(1,2),NELEM,NELMAX,
C    *                   T(1,1),T(1,2)  )
C
C-----------------------------------------------------------------------
C       OTHER
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ELEMENT TYPE
C-----------------------------------------------------------------------
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
C
        ENDIF
C
C=======================================================================
C     VECTOR U GRAD(PSI)
C=======================================================================
C
      ELSEIF(FORMUL(1:6).EQ.'VGRADP') THEN
C
        SPECAD = .FALSE.
        IF(FORMUL(8:8).EQ.'2') SPECAD = .TRUE.
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 TRIANGLE
C
        IF(IELM1.EQ.11) THEN
C
             CALL VC04AA(XMUL,SU,SV,U,V,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                   NELEM,NELMAX,T(1,1),T(1,2),T(1,3),SPECAD)
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 PRISM
C
        ELSEIF(IELM1.EQ.41) THEN
C
             CALL VC04PP(XMUL,SU,SV,SW,U,V,W,SF,SG,SH,F,G,H,XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3),T(1,4),T(1,5),T(1,6),SPECAD,FORMUL,
     &                   MESH%NPOIN/BIEF_NBPTS(11,MESH))
C
C-----------------------------------------------------------------------
C
C       ELEMENT T1 TETRAHEDRON
C
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
C
             CALL VC04TT(XMUL,SU,SV,SW,U,V,W,XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3),T(1,4),FORMUL)
C
C-----------------------------------------------------------------------
C       OTHER
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ELEMENT TYPE
C-----------------------------------------------------------------------
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
C
        ENDIF
C
C=======================================================================
C     VECTOR U.N    (N VECTOR NORMAL TO THE ELEMENT)
C=======================================================================
C
      ELSEIF(FORMUL(1:16).EQ.'FLUBOR          ') THEN
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 TRIANGLE (BOTTOM OR SURFACE OF A 3D MESH ?)
C
        IF(IELM1.EQ.11) THEN
C
             CALL VC05AA(XMUL,SW,W,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3) )
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 TRIANGLE FOR VERTICAL SIDES OF THE PRISMS SPLIT
C       IN TETRAHEDRONS
C
        ELSEIF(IELM1.EQ.61) THEN
C
             CALL VC05FT(XMUL,SU,SV,U,V,XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),NBOR,
     &                   NELEM,NELMAX,T(1,1),T(1,2),T(1,3))
C
C       ELEMENT P1 QUADRILATERAL FOR VERTICAL SIDES OF THE PRISMS
C
        ELSEIF(IELM1.EQ.71) THEN
C
             CALL VC05FF(XMUL,SU,SV,U,V,XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),NBOR,
     &                   NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4))
C
C       ELEMENT LINEAR SEGMENT
C
        ELSEIF(IELM1.EQ.1) THEN
C
             CALL VC05OO(XMUL,SU,SV,U,V,XNOR,YNOR,SURFAC,
     &                   IKLE,NBOR,NELEM,NELMAX,T(1,1),T(1,2))
C
C-----------------------------------------------------------------------
C       OTHER
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ELEMENT TYPE
C-----------------------------------------------------------------------
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
C
        ENDIF
C
C=======================================================================
C     VECTOR U GRAD(F)
C=======================================================================
C
      ELSEIF(FORMUL(1:13).EQ.'VGRADF       ') THEN
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 TRIANGLE
C
        IF(IELM1.EQ.11) THEN
C
             CALL VC08AA(XMUL,SF,SU,SV,F,U,V,XEL,YEL,IKLE,
     &                   NELEM,NELMAX,T(1,1),T(1,2),T(1,3) , FORMUL )
C
C-----------------------------------------------------------------------
C
C       ELEMENT P2 TRIANGLE
C
        ELSEIF(IELM1.EQ.13) THEN
C
             CALL VC08CC(XMUL,SF,SU,SV,F,U,V,XEL,YEL,
     &            IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &            IKLE(1,5),IKLE(1,6),NELEM,NELMAX,
     &            T(1,1),T(1,2),T(1,3),T(1,4),T(1,5),T(1,6),FORMUL)

C
C-----------------------------------------------------------------------
C
C       ELEMENT QUASI-BUBBLE TRIANGLE
C
        ELSEIF(IELM1.EQ.12) THEN
C
             CALL VC08BB(XMUL,SF,SU,SV,F,U,V,XEL,YEL,
     &            IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &            NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4),FORMUL)
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 SEGMENT
C
C       ELSEIF(IELM1.EQ.1) THEN
C
C            CALL VC08OO(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
C    *                   XEL,YEL,ZEL,SURFAC,
C    *                   IKLE(1,1),IKLE(1,2),NELEM,NELMAX,
C    *                   T(1,1),T(1,2)  )
C
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 PRISM
C
        ELSEIF(IELM1.EQ.41) THEN
C
             CALL VC08PP(XMUL,SF,SU,SV,SW,F,U,V,W,XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3),T(1,4),T(1,5),T(1,6))
C
C-----------------------------------------------------------------------
C
C       ELEMENT T1 TETRAHEDRON
C
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
C
             CALL VC08TT(XMUL,SF,SU,SV,SW,F,U,V,W,XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3),T(1,4))
C
C-----------------------------------------------------------------------
C       OTHER
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ELEMENT TYPE
C-----------------------------------------------------------------------
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
C
        ENDIF
C
C A.D. MODIFICATION 25/11/04
!
C=======================================================================
C     VECTOR U GRAD(F) 2
C=======================================================================
C
      ELSEIF(FORMUL(1:13).EQ.'VGRADF2      ') THEN
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 TRIANGLE
C
C-----------------------------------------------------------------------
C
C       ELEMENT QUASI-BUBBLE TRIANGLE
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 SEGMENT
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 PRISM
C
        IF(IELM1.EQ.41) THEN
C
             CALL VC18PP(XMUL,SF,SU,SV,F,U,V,XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3))
C
C-----------------------------------------------------------------------
C
C       ELEMENT T1 TETRAHEDRON
C
C-----------------------------------------------------------------------
C       OTHER
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ELEMENT TYPE
C-----------------------------------------------------------------------
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
C
        ENDIF
C
C  A.D. END OF MODIFICATION 25/11/04
C=======================================================================
C     VECTOR Q GRAD(F)
C=======================================================================
C
      ELSEIF(FORMUL(1:16).EQ.'QGRADF          ') THEN
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 TRIANGLE
C
        IF(IELM1.EQ.11) THEN
C
             CALL VC09AA(XMUL,SF,SG,SU,SV,F,G,U,V,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3) )
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 SEGMENT
C
C       ELSEIF(IELM1.EQ.1) THEN
C
C            CALL VC09OO(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
C    *                   XEL,YEL,ZEL,SURFAC,
C    *                   IKLE(1,1),IKLE(1,2),NELEM,NELMAX,
C    *                   T(1,1),T(1,2)  )
C
C-----------------------------------------------------------------------
C       OTHER
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ELEMENT TYPE
C-----------------------------------------------------------------------
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
C
        ENDIF
C
C=======================================================================
C     VECTOR F U.N    (N VECTOR NORMAL TO THE ELEMENT)
C=======================================================================
C
      ELSEIF(FORMUL(1:16).EQ.'FLUBDF          ') THEN
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 TRIANGLE
C
C       IF(IELM1.EQ.11) THEN
C
C            CALL VC10AA(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
C    *                   XEL,YEL,ZEL,SURFAC,
C    *                   IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
C    *                   T(1,1)   ,T(1,2)   ,T(1,3))
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 SEGMENT
C
        IF(IELM1.EQ.1) THEN
C
             CALL VC10OO(XMUL,SF,SU,SV,F,U,V,XNOR,YNOR,SURFAC,
     &                   IKLE,NBOR,NELEM,NELMAX,T(1,1),T(1,2)  )
C
C-----------------------------------------------------------------------
C       OTHER
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ELEMENT TYPE
C-----------------------------------------------------------------------
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
C
        ENDIF
C
C=======================================================================
C     VECTOR G GRADIENT(F)
C=======================================================================
C
      ELSEIF(FORMUL(1:15).EQ.'GGRADF         ') THEN
C
C     CHARACTER 16 GIVES THE SELECTED COORDINATE
C
      IF(FORMUL(16:16).EQ.'X') THEN
        ICOORD=1
      ELSEIF(FORMUL(16:16).EQ.'Y') THEN
        ICOORD=2
      ELSEIF(FORMUL(16:16).EQ.'Z') THEN
        ICOORD=3
      ENDIF
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 TRIANGLE
C
        IF(IELM1.EQ.11) THEN
C
C CHECKS IF G IS DISC P1
C
         IF (SG%DIM1.EQ.NELEM.AND.SG%DIM2.EQ.3.AND.
     &       SG%DIMDISC.EQ.11) THEN
C
            CALL VC11AA2(XMUL,SF,SG,SH,F,G,H,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3) , ICOORD )
C
C CLASSICAL CASE: G IS P1
C
         ELSE
C
             CALL VC11AA(XMUL,SF,SG,F,G,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3) , ICOORD )
         ENDIF
C
C-----------------------------------------------------------------------
C
        ELSEIF(IELM1.EQ.12) THEN
C
             CALL VC11BB(XMUL,SF,SG,F,G,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3),T(1,4) , ICOORD )
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 SEGMENT
C
C       ELSEIF(IELM1.EQ.1) THEN
C
C            CALL VC11OO(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
C    *                   XEL,YEL,ZEL,XNOR,YNOR,ZNOR,SURFAC,
C    *                   IKLE(1,1),IKLE(1,2),NBOR,NELEM,NELMAX,
C    *                   T(1,1),T(1,2)  )
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 PRISM
C
        ELSEIF(IELM1.EQ.41) THEN
C
             CALL VC11PP(XMUL,SF,SG,F,G,
     &                   XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3),T(1,4),T(1,5),T(1,6),ICOORD)
C
C-----------------------------------------------------------------------
C
C       ELEMENT T1 TETRAHEDRON
C
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
C
             CALL VC11TT(XMUL,SF,SG,F,G,
     &                   XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3),T(1,4),ICOORD)
C
        ELSEIF(IELM1.EQ.30) THEN
C
C COMMENT JP RENAUD 21/09/2005
C BEWARE: THIS CALL CREATES A P0 VECTOR. THEREFORE
C THERE IS NO NEED TO "ASSEMBLE" IT AFTERWARDS. NOTE
C THAT VEC ITSELF (AND NOT T) IS GIVEN TO VC11TT0 AND
C THAT LEGO IS ALSO SET TO .FALSE. TO AVOID
C CALLING ASSVEC AT THE END OF THE SUBROUTINE.
C
             CALL VC11TT0(XMUL,SF,SG,F,G,
     &                   XEL,YEL,ZEL,
     &                   IKLE(1:NELEM,1),IKLE(1:NELEM,2),
     &                   IKLE(1:NELEM,3),IKLE(1:NELEM,4),
     &                   NELEM,MESH%NPOIN,
     &                   VEC,ICOORD)
C
C
C-----------------------------------------------------------------------
C       OTHER
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ELEMENT TYPE
C-----------------------------------------------------------------------
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
C
        ENDIF
C
C=======================================================================
C     VECTOR GRADIENT(F)
C=======================================================================
C
      ELSEIF(FORMUL(1:5).EQ.'GRADF') THEN
C
C     CHARACTER 16 GIVES THE SELECTED COORDINATE
C
      IF(FORMUL(16:16).EQ.'X') THEN
        ICOORD=1
      ELSEIF(FORMUL(16:16).EQ.'Y') THEN
        ICOORD=2
      ELSEIF(FORMUL(16:16).EQ.'Z') THEN
        ICOORD=3
      ENDIF
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 TRIANGLE
C
        IF(IELM1.EQ.11) THEN
C
             CALL VC13AA(XMUL,SF,F,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   NELEM,NELMAX,T(1,1),T(1,2),T(1,3),ICOORD)
C
C-----------------------------------------------------------------------
C
C       ELEMENT QUASI-BUBBLE TRIANGLE
C
        ELSEIF(IELM1.EQ.12) THEN
C
             CALL VC13BB(XMUL,SF,F,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),NELEM,
     &                   NELMAX,T(1,1),T(1,2),T(1,3),T(1,4),ICOORD)
C
C-----------------------------------------------------------------------
C
C       ELEMENT P2 TRIANGLE
C
        ELSEIF(IELM1.EQ.13) THEN
C
            CALL VC13CC(XMUL,SF,F,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                   IKLE(1,4),IKLE(1,5),IKLE(1,6),
     &                   NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3),
     &                   T(1,4),T(1,5),T(1,6),ICOORD)
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 SEGMENT
C
C       ELSEIF(IELM1.EQ.1) THEN
C
C            CALL VC13OO(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
C    *                   XEL,YEL,ZEL,XNOR,YNOR,ZNOR,SURFAC,
C    *                   IKLE(1,1),IKLE(1,2),NBOR,NELEM,NELMAX,
C    *                   T(1,1),T(1,2)  )
C
C
C
C-----------------------------------------------------------------------
C
C       ELEMENT P2 SEGMENT
C
C       ELSEIF(IELM1.EQ.2) THEN
C
C            CALL VC13OC(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
C    *                   XEL,YEL,ZEL,XNOR,YNOR,ZNOR,SURFAC,
C    *                   IKLE(1,1),IKLE(1,2),NBOR,NELEM,NELMAX,
C    *                   T(1,1),T(1,2),T(1,3)  )
C
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 PRISM
C
        ELSEIF(IELM1.EQ.41) THEN
C
             IF(FORMUL(1:15).EQ.'GRADF(X,Y)     ') THEN
C            SIMPLIFIED FORMULATION FOR EFFICIENCY AND ACCURACY
             CALL VC13PP2(XMUL,SF,F,XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3),T(1,4),T(1,5),T(1,6),ICOORD)
             ELSEIF( FORMUL(8:15).EQ.'        ') THEN
C                    FORMUL(6:7) IS LEFT FOR OPTIONS
             CALL VC13PP(XMUL,SF,F,XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   IKLE(1,5),IKLE(1,6),NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3),T(1,4),T(1,5),T(1,6),ICOORD,FORMUL)
C
             ELSE
               IF(LNG.EQ.1) WRITE(LU,1000) FORMUL
               IF(LNG.EQ.2) WRITE(LU,1001) FORMUL
               CALL PLANTE(1)
               STOP
             ENDIF
C
C-----------------------------------------------------------------------
C
C       ELEMENT T1 TETRAHEDRON
C
        ELSEIF(IELM1.EQ.31.OR.IELM1.EQ.51) THEN
C
             IF(FORMUL(1:15).EQ.'GRADF          '.OR.
     &          FORMUL(1:15).EQ.'GRADF2         '  ) THEN
             CALL VC13TT(XMUL,SF,F,XEL,YEL,ZEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   NELEM,NELMAX,T(1,1),T(1,2),
     &                   T(1,3),T(1,4),ICOORD,FORMUL)
C
             ELSE
               IF(LNG.EQ.1) WRITE(LU,1000) FORMUL
               IF(LNG.EQ.2) WRITE(LU,1001) FORMUL
               CALL PLANTE(1)
               STOP
             ENDIF
C
C-----------------------------------------------------------------------
C       OTHER
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ELEMENT TYPE
C-----------------------------------------------------------------------
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
C
        ENDIF
C
C=======================================================================
C     TURBULENT PRODUCTION VECTOR
C=======================================================================
C
      ELSEIF(FORMUL(1:16).EQ.'PRODF           ') THEN
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 TRIANGLE
C
        IF(IELM1.EQ.11) THEN
C
             CALL VC14AA(XMUL,SU,SV,U,V,XEL,YEL,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3))
C
C-----------------------------------------------------------------------
C
C       ELEMENT QUASI-BUBBLE TRIANGLE
C
C       ELSEIF(IELM1.EQ.12) THEN
C
C            CALL VC14BB(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
C    *                   XEL,YEL,ZEL,SURFAC,
C    *                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),NELEM,
C    *                   NELMAX,T(1,1),T(1,2),T(1,3),T(1,4))
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 SEGMENT
C
C       ELSEIF(IELM1.EQ.1) THEN
C
C            CALL VC14OO(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
C    *                   XEL,YEL,ZEL,XNOR,YNOR,ZNOR,SURFAC,
C    *                   IKLE(1,1),IKLE(1,2),NBOR,NELEM,NELMAX,
C    *                   T(1,1),T(1,2))
C
C-----------------------------------------------------------------------
C       OTHER
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ELEMENT TYPE
C-----------------------------------------------------------------------
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
C
        ENDIF
C
C=======================================================================
C     VECTOR DIV(HU)
C=======================================================================
C
      ELSEIF(FORMUL(1:16).EQ.'DIVQ            ') THEN
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 TRIANGLE
C
        IF(IELM1.EQ.11) THEN
C
             CALL VC15AA(XMUL,SF,SU,SV,F,U,V,XEL,YEL,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
     &                   NELEM,NELMAX,T(1,1),T(1,2),T(1,3))
C
C-----------------------------------------------------------------------
C
C       ELEMENT QUASI-BUBBLE TRIANGLE
C
C       ELSEIF(IELM1.EQ.12) THEN
C
C            CALL VC15BB(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
C    *            XEL,YEL,ZEL,SURFAC,
C    *            IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
C    *            NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4),FORMUL)
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 SEGMENT
C
C       ELSEIF(IELM1.EQ.1) THEN
C
C            CALL VC15OO(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
C    *                   XEL,YEL,ZEL,SURFAC,
C    *                   IKLE(1,1),IKLE(1,2),NELEM,NELMAX,
C    *                   T(1,1),T(1,2)  )
C
C-----------------------------------------------------------------------
C       OTHER
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ELEMENT TYPE
C-----------------------------------------------------------------------
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
C
        ENDIF
C
C=======================================================================
C     VECTOR K.GRAD(PSI) DIV(U)
C=======================================================================
C
      ELSEIF(FORMUL(1:16).EQ.'SUPGDIVU        ') THEN
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 TRIANGLE
C
        IF(IELM1.EQ.11) THEN
C
             CALL VC16AA(XMUL,SF,SG,SU,SV,F,G,U,V,XEL,YEL,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),
     &                   NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3) )
C
C-----------------------------------------------------------------------
C
C       ELEMENT QUASI-BUBBLE TRIANGLE
C
C       ELSEIF(IELM1.EQ.12) THEN
C
C            CALL VC16BB(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
C    *            XEL,YEL,ZEL,SURFAC,
C    *            IKLE(1,1),IKLE(1,2),IKLE(1,3),IKLE(1,4),
C    *            NELEM,NELMAX,T(1,1),T(1,2),T(1,3),T(1,4),FORMUL)
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 SEGMENT
C
C       ELSEIF(IELM1.EQ.1) THEN
C
C            CALL VC16OO(XMUL,SF,SG,SH,SU,SV,SW,F,G,H,U,V,W,
C    *                   XEL,YEL,ZEL,SURFAC,
C    *                   IKLE(1,1),IKLE(1,2),NELEM,NELMAX,
C    *                   T(1,1),T(1,2)  )
C
C
C-----------------------------------------------------------------------
C       ERROR ON THE ELEMENT TYPE
C-----------------------------------------------------------------------
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
C
        ENDIF
C
C
C=======================================================================
C     VECTOR H U.GRAD(PSI)
C=======================================================================
C
      ELSEIF(FORMUL(1:7).EQ.'HUGRADP') THEN
C
C-----------------------------------------------------------------------
C
C       ELEMENT P1 TRIANGLE
C
        IF(IELM1.EQ.11) THEN
C
             CALL VC19AA(XMUL,SF,SG,SH,SU,SV,F,G,H,U,V,
     &                   XEL,YEL,SURFAC,
     &                   IKLE(1,1),IKLE(1,2),IKLE(1,3),NELEM,NELMAX,
     &                   T(1,1),T(1,2),T(1,3),FORMUL)
C
C-----------------------------------------------------------------------
C       OTHER
C-----------------------------------------------------------------------
C
C       ELSEIF
C
C-----------------------------------------------------------------------
C       ERROR ON THE ELEMENT TYPE
C-----------------------------------------------------------------------
C
        ELSE
C
          IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
          IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
          IF (LNG.EQ.1) WRITE(LU,2000) IELM1
          IF (LNG.EQ.2) WRITE(LU,2001) IELM1
          CALL PLANTE(1)
          STOP
C
        ENDIF
C
C=======================================================================
C     OTHER VECTOR
C=======================================================================
C
C
C=======================================================================
C     ERROR: TYPE OF VECTOR NOT CODED UP
C=======================================================================
C
      ELSE
        IF (LNG.EQ.1) WRITE(LU,1000) FORMUL
        IF (LNG.EQ.2) WRITE(LU,1001) FORMUL
        CALL PLANTE(1)
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
C  POSSIBLE ASSEMBLY OF THE VECTOR
C
      IF(LEGO) THEN
C
        IF(OP(1:1).EQ.'=') THEN
          INIT = .TRUE.
        ELSEIF(OP(1:1).EQ.'+') THEN
          INIT = .FALSE.
        ELSE
          IF (LNG.EQ.1) WRITE(LU,3000) OP
          IF (LNG.EQ.2) WRITE(LU,3001) OP
3000      FORMAT(1X,'VECTOS (BIEF) : OP NON PREVU :',A1)
3001      FORMAT(1X,'VECTOS (BIEF) : OP NOT RECOGNISED:',A1)
          CALL PLANTE(1)
          STOP
        ENDIF
C
        CALL ASSVEC(VEC, IKLE, NPT ,NELEM,NELMAX,IELM1,
     &              T,INIT,LV,MSK,MASKEL,BIEF_NBPEL(IELM1,MESH))
C
      ENDIF
C
C-----------------------------------------------------------------------
C
1000  FORMAT(1X,'VECTOS (BIEF) : VECTEUR NON PREVU : ',A16)
1001  FORMAT(1X,'VECTOS (BIEF) : VECTOR NOT IMPLEMENTED:',A16)
2000  FORMAT(1X,'                POUR IELM1 = ',1I6)
2001  FORMAT(1X,'                FOR IELM1 = ',1I6)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C
