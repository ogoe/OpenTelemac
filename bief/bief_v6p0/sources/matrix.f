C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       OPERATIONS BETWEEN MATRICES.
!><br>            THE MATRIX IS IDENTIFIED BY THE FORMULATION IN
!>                CHARACTER STRING FORMUL.
!>  @code
!>     OP IS A STRING OF 8 CHARACTERS, WHICH INDICATES HOW M IS
!>     MODIFIED. THE SYNTAX IS THE SAME AS THAT OF OM, FOR EXAMPLE.
!>
!>     OP='M=N     '
!>     OP='M=TN    '
!>     OP='M=M+N   '
!>     OP='M=M+TN  '
!>
!>     ALL THE OPERATIONS IN OM WHICH HAVE N ON THE RIGHT OF THE
!>     = SIGN ARE VALID.
!>  @endcode
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @code
!>-----------------------------------------------------------------------
!>  MEANING OF IELM AND IELM2
!>
!>  TYPE OF ELEMENT      NUMBER OF POINTS
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
!>
!>-----------------------------------------------------------------------
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> F, FORMUL, G, H, IELM1, IELM2, M, MASKEL, MESH, MSK, OP, U, V, W, XMUL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, IKLE, LEGO, NELEM, NELMAX, NPT, SS, SURFAC, XX, YY, ZZ
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_MATRIX, IKLE, SURFAC, XX, YY, ZZ
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> ASSEX3(), ASSVEC(), DIM1_EXT(), DIM2_EXT(), DIMENS(), MATRIY(), NBPTS(), OM(), OS(), PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BERKHO(), CVDFTR(), DIFF3D(), FILTER(), FLUSEC(), FLUSEC_TELEMAC2D(), KEPSIL(), MATBOU(), MESH_PROP(), PRECON(), PREDIV(), PROPAG(), PROPAG_ADJ(), ROTNE0(), WAVE_EQUATION()

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
!> </td><td> 14/10/2009
!> </td><td> JMH
!> </td><td> ARGUMENTS ADDED TO ASSEX3
!> </td></tr>
!>      <tr>
!>      <td><center>                                           </center>
!> </td><td> 25/06/2008
!> </td><td> JM HERVOUET (LNHE) 01 30 87 80 18
!> </td><td> DOES NOT CALL MATRIY IF NUMBER OF ELEMENTS IS 0
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
!></td><td>--></td><td>FORMULE DECRIVANT LA MATRICE
!>    </td></tr>
!>          <tr><td>IELM1
!></td><td>--></td><td>TYPE D'ELEMENT POUR LES LIGNES
!>    </td></tr>
!>          <tr><td>IELM2
!></td><td>--></td><td>TYPE D'ELEMENT POUR LES COLONNES
!>    </td></tr>
!>          <tr><td>M
!></td><td><-></td><td>MATRICE A REMPLIR OU MODIFIER
!>    </td></tr>
!>          <tr><td>MASKEL
!></td><td>--></td><td>TABLEAU DE MASQUAGE DES ELEMENTS
!>                  =1. : NORMAL   =0. : ELEMENT MASQUE
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>--></td><td>BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>MSK
!></td><td>--></td><td>SI OUI, PRESENCE D'ELEMENTS MASQUES.
!>    </td></tr>
!>          <tr><td>OP
!></td><td>--></td><td>VOIR PLUS HAUT.
!>    </td></tr>
!>          <tr><td>U,V,W
!></td><td>--></td><td>COMPOSANTES D'UN VECTEUR U DANS LA FORMULE
!>    </td></tr>
!>          <tr><td>XMUL
!></td><td>--></td><td>COEFFICIENT MULTIPLICATEUR DU RESULTAT
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE MATRIX
     &(M,OP,FORMUL,IELM1,IELM2,XMUL,F,G,H,U,V,W,MESH,MSK,MASKEL)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| F,G,H          |-->| FONCTIONS INTERVENANT DANS LA FORMULE
C| FORMUL         |-->| FORMULE DECRIVANT LA MATRICE
C| IELM1          |-->| TYPE D'ELEMENT POUR LES LIGNES
C| IELM2          |-->| TYPE D'ELEMENT POUR LES COLONNES
C| M             |<->| MATRICE A REMPLIR OU MODIFIER
C| MASKEL         |-->| TABLEAU DE MASQUAGE DES ELEMENTS
C|                |   | =1. : NORMAL   =0. : ELEMENT MASQUE
C| MESH           |-->| BLOC DES TABLEAUX D'ENTIERS DU MAILLAGE
C| MSK            |-->| SI OUI, PRESENCE D'ELEMENTS MASQUES.
C| OP             |-->| VOIR PLUS HAUT.
C| U,V,W          |-->| COMPOSANTES D'UN VECTEUR U DANS LA FORMULE
C| XMUL           |-->| COEFFICIENT MULTIPLICATEUR DU RESULTAT
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_MATRIX => MATRIX
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)            :: IELM1,IELM2
C
      DOUBLE PRECISION, INTENT(IN)   :: XMUL
C
      LOGICAL, INTENT(IN)            :: MSK
C
      CHARACTER(LEN=16), INTENT(IN)  :: FORMUL
      CHARACTER(LEN=8), INTENT(IN)   :: OP
C
      TYPE(BIEF_OBJ), INTENT(IN)     :: F,G,H,U,V,W,MASKEL
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: M
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER NELMAX,NELEM,NPT,SS
      INTEGER, DIMENSION(:), POINTER :: IKLE
      DOUBLE PRECISION, DIMENSION(:), POINTER :: SURFAC,XX,YY,ZZ
      DOUBLE PRECISION C
      LOGICAL LEGO
C
C-----------------------------------------------------------------------
C
C     STORES 1 FOR THE WROKING ARRAY
C     CAN BE MODIFIED BY ASSEXT THEREAFTER
C
      MESH%M%STO = 1
C
C-----------------------------------------------------------------------
C  CALLS THE SHUNTING AND ASSEMBLY SUBROUTINE : MATRIY
C-----------------------------------------------------------------------
C
C     LEGO CAN BE MODIFIED BY MATRIY
      LEGO = .TRUE.
C
      IF(DIMENS(IELM1).EQ.MESH%DIM) THEN
C       NORMAL MATRIX
        NELEM  = MESH%NELEM
        NELMAX = MESH%NELMAX
        IKLE   =>MESH%IKLE%I
        SURFAC =>MESH%SURFAC%R
        XX=>MESH%XEL%R
        YY=>MESH%YEL%R
        ZZ=>MESH%ZEL%R
      ELSE
C       BOUNDARY MATRIX
        NELEM  = MESH%NELEB
        NELMAX = MESH%NELEBX
        IKLE   =>MESH%IKLBOR%I
        SURFAC =>MESH%LGSEG%R
        XX=>MESH%X%R
        YY=>MESH%Y%R
        ZZ=>MESH%Z%R
      ENDIF
C
C     MATRIY FILLS THE DIAGONAL AND EXTRA DIAGONAL TERMS
C
C     REFLECTS CHOICE OF PRE-ASSEMBLY STORAGE
      IF(M%STO.EQ.1.OR.M%STO.EQ.3) THEN
        SS = 1
      ELSEIF(M%STO.EQ.2) THEN
        SS = 2
      ELSE
        WRITE(LU,*) 'UNKNOWN STORAGE IN MATRIX'
        CALL PLANTE(1)
        STOP
      ENDIF
C
      IF(NELEM.GT.0) THEN
        CALL MATRIY(FORMUL,MESH%M%X%R,
     &              MESH%M%TYPDIA,MESH%M%TYPEXT,
     &              XMUL,F,G,H,U,V,W,
     &              F%R,G%R,H%R,U%R,V%R,W%R,
     &              MESH%W%R,LEGO,XX,YY,ZZ,
     &              SURFAC,IKLE,MESH%NBOR%I ,
     &              NELEM,NELMAX,IELM1,IELM2,SS)
      ENDIF
C
C  UPDATES THE INFORMATION OF THE MATRIX
C
      NPT = NBPTS(MIN(IELM1,IELM2))
C
      IF(NPT.GT.MESH%M%D%MAXDIM1) THEN
        IF (LNG.EQ.1) WRITE(LU,500) MESH%M%NAME
        IF (LNG.EQ.2) WRITE(LU,501) MESH%M%NAME
        IF (LNG.EQ.1) WRITE(LU,2000) IELM1
        IF (LNG.EQ.2) WRITE(LU,2001) IELM1
        IF (LNG.EQ.1) WRITE(LU,3000) IELM2
        IF (LNG.EQ.2) WRITE(LU,3001) IELM2
        CALL PLANTE(1)
        STOP
      ENDIF
C
      MESH%M%D%ELM  = MIN(IELM1,IELM2)
      MESH%M%D%DIM1 = NPT
      MESH%M%ELMLIN = IELM1
      MESH%M%ELMCOL = IELM2
C
C  ASSEMBLES THE DIAGONAL (POSSIBLY)
C
      IF(LEGO.AND.MESH%M%TYPDIA.EQ.'Q') THEN
C
            CALL ASSVEC(MESH%M%D%R,
     &                  IKLE,NPT,NELEM,NELMAX,MIN(IELM1,IELM2),
     &                  MESH%W%R,LEGO,MESH%LV,MSK,MASKEL%R)
C
      ENDIF
C
C  MASKS EXTRA-DIAGONAL TERMS (POSSIBLY)
C
C     NOTE: FOR STORAGE 2, EXTRA-DIAGONAL TERMS ARE NOT WHERE
C           THEY SHOULD BE BUT DOES NOT AFFECT THE MULTIPLICATION
C           BY MASKEL
C
      IF(MSK) CALL OM( 'M=MSK(M)',MESH%M,MESH%M,MASKEL,C,MESH)
C
C  ASSEMBLES EXTRA-DIAGONAL TERMS (POSSIBLY)
C
      IF(M%STO.EQ.3) THEN
C       COPIES THE DIAGONAL
        CALL OS('X=Y     ',MESH%MSEG%D,MESH%M%D,MESH%M%D,0.D0)
        MESH%MSEG%TYPDIA(1:1)='Q'
C       ASSEMBLES EXTRA-DIAGONAL TERMS
        IF(MESH%M%TYPEXT.EQ.'Q'.OR.MESH%M%TYPEXT.EQ.'S') THEN
C       CASE OF MATRICES WITH INVERTED STORAGE OF OFF-DIAGONAL TERMS
C       SO FAR ONLY MAMURD. SEE INVERSION OF DIM1_EXT AND DIM2_EXT
C       AND 2 INSTEAD OF 1 FOR STOXMT
          IF(FORMUL(1:6).EQ.'MAMURD') THEN
            CALL ASSEX3(MESH%MSEG%X%R,MESH%M%STO,MESH%M%NAME,
     &                  MESH%M%ELMLIN,MESH%M%ELMCOL,
     &                  MESH%M%TYPEXT,MESH%M%X%R,
     &                  DIM2_EXT(IELM1,IELM2,MESH%M%STO,MESH%M%TYPEXT),
     &                  DIM1_EXT(IELM1,IELM2,MESH%M%STO,MESH%M%TYPEXT),
     &                  2,
     &                  MESH,MESH%NELMAX,MESH%ELTSEG%I,MESH%ORISEG%I)
          ELSE
            CALL ASSEX3(MESH%MSEG%X%R,MESH%M%STO,MESH%M%NAME,
     &                  MESH%M%ELMLIN,MESH%M%ELMCOL,
     &                  MESH%M%TYPEXT,MESH%M%X%R,
     &                  DIM1_EXT(IELM1,IELM2,MESH%M%STO,MESH%M%TYPEXT),
     &                  DIM2_EXT(IELM1,IELM2,MESH%M%STO,MESH%M%TYPEXT),
     &                  1,
     &                  MESH,MESH%NELMAX,MESH%ELTSEG%I,MESH%ORISEG%I)
          ENDIF
        ENDIF
        MESH%MSEG%TYPEXT=MESH%M%TYPEXT
        MESH%MSEG%ELMLIN = IELM1
        MESH%MSEG%ELMCOL = IELM2
        MESH%MSEG%D%ELM  = MIN(IELM1,IELM2)
        MESH%MSEG%D%DIM1 = NPT
        MESH%MSEG%X%DIM1 = DIM1_EXT(IELM1,IELM2,M%STO,MESH%MSEG%TYPEXT)
        MESH%MSEG%X%DIM2 = DIM2_EXT(IELM1,IELM2,M%STO,MESH%MSEG%TYPEXT)
      ENDIF
C
C     DIMENSIONS OF THE ARRAY WITH EXTRADIAGONAL TERMS
C     BEWARE M%STO (NOT MESH%M%STO BECAUSE IT EQUALS 1)
C                   SEE BEGINNING OF SUBROUTINE
C
      MESH%M%X%DIM1 = DIM1_EXT(IELM1,IELM2,M%STO,MESH%M%TYPEXT)
      MESH%M%X%DIM2 = DIM2_EXT(IELM1,IELM2,M%STO,MESH%M%TYPEXT)
C
C-----------------------------------------------------------------------
C  UPDATES M AFTER WORK ON MESH%M IS COMPLETE
C-----------------------------------------------------------------------
C
      IF(M%STO.EQ.1) THEN
        CALL OM( OP , M , MESH%M , F , C , MESH )
      ELSEIF(M%STO.EQ.3) THEN
        CALL OM( OP , M , MESH%MSEG , F , C , MESH )
      ELSE
        WRITE(LU,*) 'MATRIX, STOCKAGE INCONNU : ',M%STO
        STOP
      ENDIF
C
C-----------------------------------------------------------------------
C
500   FORMAT(1X,'MATRIX (BIEF) : MATRICE ',A6,' TROP PETITE')
501   FORMAT(1X,'MATRIX (BIEF) : MATRIX ',A6,' TOO SMALL')
2000  FORMAT(1X,'                POUR IELM1 = ',1I6)
2001  FORMAT(1X,'                FOR IELM1 = ',1I6)
3000  FORMAT(1X,'                ET IELM2 = ',1I6)
3001  FORMAT(1X,'                AND IELM2 = ',1I6)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C