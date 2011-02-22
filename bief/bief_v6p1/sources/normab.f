C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       1) COMPUTES THE COMPONENTS OF THE OUTGOING NORMAL VECTOR
!><br>               - FOR THE BOUNDARY POINTS      (XNEBOR,YNEBOR)
!><br>               - FOR THE BOUNDARY SEGMENTS    (XSGBOR,YSGBOR)
!><br>            2) DISTANCE TO THE BOUNDARY OF THE FIRST ELEMENT POINTS
!><br>            3) LENGTH OF THE BOUNDARY SEGMENTS
!><br>            4) DISTANCE TO THE BOUNDARY OF THE FIRST INTERNAL POINTS
!>  @code
!>  BEWARE:  XSGBOR AND YSGBOR DIMENSION IS (NPTFR,4):
!>
!>           (K,1) : NORMALISED    , SEGMENT FOLLOWING K
!>           (K,2) : NORMALISED    , SEGMENT PRECEDING K
!>           (K,3) : NOT NORMALISED, SEGMENT FOLLOWING K
!>           (K,4) : NOT NORMALISED, SEGMENT PRECEDING K
!>
!>           XSGBOR(K,1) AND YSGBOR(K,1) ARE THE COMPONENTS
!>           FOR THE SEGMENT FOLLOWING POINT K.
!>
!>           XSGBOR(K,2) AND YSGBOR(K,2) ARE THE COMPONENTS
!>           FOR THE SEGMENT PRECEDING POINT K.
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DISBOR, KP1BOR, LGSEG, MESH, NBOR, NELBOR, NELEM, NPTFR, SURFAC, T1, X, XNEBOR, XSGBOR, Y, YNEBOR, YSGBOR
!>   </td></tr>
!>     <tr><th> Use(s)
!>    </th><td>
!> BIEF_DEF :<br>
!> @link BIEF_DEF::NCSIZE NCSIZE@endlink
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, IELEM, K1, K2, N1, N2, X1, X12, X2, XNORM, Y1, Y12, Y2, Z
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_NORMAB
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PARCOM_BORD()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>INBIEF()

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
!> </td><td> 26/06/2008
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>DISBOR
!></td><td><--</td><td>DISTANCE DU BORD AU PREMIER POINT INTERIEUR.
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td>--></td><td>NUMERO DE L'EXTREMITE DES SEGMENTS DE BORD
!>    </td></tr>
!>          <tr><td>LGSEG
!></td><td><--</td><td>LONGUEUR DES SEGMENTS DE BORD
!>    </td></tr>
!>          <tr><td>MESH
!></td><td><-></td><td>STRUCTURE DE MAILLAGE
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>ADRESSES DES POINTS FRONTIERES.
!>    </td></tr>
!>          <tr><td>NELBOR
!></td><td>--></td><td>NUMERO DE L'ELEMENT DE BORD
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>--></td><td>NOMBRE D'ELEMENTS
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERES
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td>--></td><td>SURFACE DES TRIANGLES
!>    </td></tr>
!>          <tr><td>T1
!></td><td><-></td><td>STRUCTURE BIEF_OBJ POUR TABLEAU DE TRAVAIL
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES X,Y DONNEES PAR POINT.
!>    </td></tr>
!>          <tr><td>XNEBOR
!></td><td><--</td><td>COMPOSANTE SUIVANT X DE LA NORMALE AU POINT.
!>    </td></tr>
!>          <tr><td>XSGBOR
!></td><td><--</td><td>COMPOSANTE SUIVANT X DE LA NORMALE DU SEGMENT
!>    </td></tr>
!>          <tr><td>YNEBOR
!></td><td><--</td><td>COMPOSANTE SUIVANT Y DE LA NORMALE AU POINT.
!>    </td></tr>
!>          <tr><td>YSGBOR
!></td><td><--</td><td>COMPOSANTE SUIVANT Y DE LA NORMALE DU SEGMENT
!>                  (SEGMENT COMPRIS ENTRE LES POINTS K ET K+1)
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE NORMAB
     &(XNEBOR,YNEBOR,XSGBOR,YSGBOR,DISBOR,SURFAC,NELEM,
     & NBOR,KP1BOR,NELBOR,LGSEG,NPTFR,X,Y,MESH,T1)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| DISBOR         |<--| DISTANCE DU BORD AU PREMIER POINT INTERIEUR.
C| KP1BOR         |-->| NUMERO DE L'EXTREMITE DES SEGMENTS DE BORD
C| LGSEG          |<--| LONGUEUR DES SEGMENTS DE BORD
C| MESH           |<->| STRUCTURE DE MAILLAGE
C| NBOR           |-->| ADRESSES DES POINTS FRONTIERES.
C| NELBOR         |-->| NUMERO DE L'ELEMENT DE BORD
C| NELEM          |-->| NOMBRE D'ELEMENTS
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERES
C| SURFAC         |-->| SURFACE DES TRIANGLES
C| T1             |<->| STRUCTURE BIEF_OBJ POUR TABLEAU DE TRAVAIL
C| X,Y            |-->| COORDONNEES X,Y DONNEES PAR POINT.
C| XNEBOR         |<--| COMPOSANTE SUIVANT X DE LA NORMALE AU POINT.
C| XSGBOR         |<--| COMPOSANTE SUIVANT X DE LA NORMALE DU SEGMENT
C| YNEBOR         |<--| COMPOSANTE SUIVANT Y DE LA NORMALE AU POINT.
C| YSGBOR         |<--| COMPOSANTE SUIVANT Y DE LA NORMALE DU SEGMENT
C|                |   | (SEGMENT COMPRIS ENTRE LES POINTS K ET K+1)
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_NORMAB => NORMAB
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPTFR,NELEM
      INTEGER, INTENT(IN) :: NBOR(NPTFR),KP1BOR(NPTFR),NELBOR(NPTFR)
C
      DOUBLE PRECISION, INTENT(INOUT) :: XNEBOR(NPTFR,2),YNEBOR(NPTFR,2)
      DOUBLE PRECISION, INTENT(INOUT) :: XSGBOR(NPTFR,4),YSGBOR(NPTFR,4)
      DOUBLE PRECISION, INTENT(INOUT) :: DISBOR(NPTFR),LGSEG(NPTFR)
      DOUBLE PRECISION, INTENT(IN)    :: SURFAC(NELEM),X(*),Y(*)
C
      TYPE(BIEF_OBJ), INTENT(INOUT)  :: T1
      TYPE(BIEF_MESH), INTENT(INOUT) :: MESH
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K1,K2,N1,N2,I,IELEM
      DOUBLE PRECISION X12,Y12,XNORM,X1,X2,Y1,Y2,Z(1)
C
      INTRINSIC SQRT
C
C-----------------------------------------------------------------------
C
C     COMPUTES THE NORMAL VECTORS AND SEGMENT LENGTHS
C
C     0) INITIALISES LGSEG, XSGBOR AND YSGBOR TO 0
C
      IF(NPTFR.GT.0) THEN
C
        DO K1=1,NPTFR
          LGSEG(K1)    = 0.D0
          XSGBOR(K1,1) = 0.D0
          YSGBOR(K1,1) = 0.D0
          XSGBOR(K1,2) = 0.D0
          YSGBOR(K1,2) = 0.D0
        ENDDO
C
C       1) NORMALS BY SEGMENT AND LENGTH OF THE BOUNDARY SEGMENT
C          COMMON VERSION FOR SCALAR/PARALLEL MODES
C
        DO K1=1,NPTFR
C
          K2=KP1BOR(K1)
          IF(K2.NE.K1) THEN
            N1=NBOR(K1)
            N2=NBOR(K2)
            X1 = X(N1)
            Y1 = Y(N1)
            X2 = X(N2)
            Y2 = Y(N2)
            X12 = X2 - X1
            Y12 = Y2 - Y1
C           LENGTH OF THE BOUNDARY SEGMENT
            LGSEG(K1) = SQRT( X12**2 + Y12**2 )
C           NORMAL TO THE SEGMENT FOLLOWING K1:
            XSGBOR(K1,1) =  Y12
            YSGBOR(K1,1) = -X12
C           NORMAL TO THE SEGMENT PRECEDING THE ONE FOLLOWING K1:
            XSGBOR(K2,2) =  Y12
            YSGBOR(K2,2) = -X12
          ENDIF
C
        ENDDO
C
      ENDIF
C
C     2) COMPLEMENT IN PARALLEL MODE, WITH PARCOM OPTION 1
C        (VALUE OF GREATER ABSOLUTE VALUE)
C
      IF(NCSIZE.GT.1) THEN
        IF(NPTFR.GT.0) THEN
          CALL PARCOM_BORD(LGSEG            ,1,MESH)
          CALL PARCOM_BORD(XSGBOR(1:NPTFR,1),1,MESH)
          CALL PARCOM_BORD(XSGBOR(1:NPTFR,2),1,MESH)
          CALL PARCOM_BORD(YSGBOR(1:NPTFR,1),1,MESH)
          CALL PARCOM_BORD(YSGBOR(1:NPTFR,2),1,MESH)
        ELSE
          CALL PARCOM_BORD(Z,1,MESH)
          CALL PARCOM_BORD(Z,1,MESH)
          CALL PARCOM_BORD(Z,1,MESH)
          CALL PARCOM_BORD(Z,1,MESH)
          CALL PARCOM_BORD(Z,1,MESH)
        ENDIF
      ENDIF
C
C     3) NORMALS BY NODES, APPROXIMATE DISTANCE FROM THE BOUNDARY
C        THE VECTORS ARE THEN NORMALISED
C
      IF(NPTFR.GT.0) THEN
C
      DO K1=1,NPTFR
C
C       NORMAL AT THE POINT: AVERAGE OF 2 NOT NORMALISED NORMALS
C       ASSOCIATED WITH THE 2 ADJACENT SEGMENTS
C
C       NOT NORMALISED VERSION XNEBOR(*,2) AND YNEBOR(*,2)
        XNEBOR(K1,2)=(XSGBOR(K1,1)+XSGBOR(K1,2))*0.5D0
        YNEBOR(K1,2)=(YSGBOR(K1,1)+YSGBOR(K1,2))*0.5D0
C
C       NOT NORMALISED VERSION XSGBOR(*,3) AND XSGBOR(*,4)
C                              YSGBOR(*,3) AND YSGBOR(*,4)
        XSGBOR(K1,3)=XSGBOR(K1,1)
        XSGBOR(K1,4)=XSGBOR(K1,2)
        YSGBOR(K1,3)=YSGBOR(K1,1)
        YSGBOR(K1,4)=YSGBOR(K1,2)
C
C       NORMALISED VERSION XNEBOR(*,1) AND YNEBOR(*,1)
        XNORM=SQRT(XNEBOR(K1,2)**2+YNEBOR(K1,2)**2)
        XNEBOR(K1,1)=XNEBOR(K1,2)/XNORM
        YNEBOR(K1,1)=YNEBOR(K1,2)/XNORM
C
C       NORMALISED VERSION OF XSGBOR AND YSGBOR FOR FOLLOWING SEGMENT
        XNORM=SQRT(XSGBOR(K1,1)**2+YSGBOR(K1,1)**2)
        XSGBOR(K1,1)=XSGBOR(K1,1)/XNORM
        YSGBOR(K1,1)=YSGBOR(K1,1)/XNORM
C
C       NORMALISED VERSION OF XSGBOR AND YSGBOR FOR PRECEDING SEGMENT
        XNORM=SQRT(XSGBOR(K1,2)**2+YSGBOR(K1,2)**2)
        XSGBOR(K1,2)=XSGBOR(K1,2)/XNORM
        YSGBOR(K1,2)=YSGBOR(K1,2)/XNORM
C
C       THIS CAN BE APPROXIMATION OF THE MESH SIZE AT THE BOUNDARY
C       AND IS USED FOR LOG LAW AT THE BOUNDARIES
        IELEM=NELBOR(K1)
        IF(IELEM.GT.0) THEN
          DISBOR(K1) = 2.D0*SURFAC(NELBOR(K1))/LGSEG(K1)
        ELSE
          DISBOR(K1) = 0.D0
        ENDIF
C
      ENDDO
C
      ENDIF
C
C     DISBOR IS POSITIVE, CAN TAKE THE MAX
      IF(NCSIZE.GT.1) THEN
        CALL PARCOM_BORD(DISBOR,3,MESH)
      ENDIF
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C