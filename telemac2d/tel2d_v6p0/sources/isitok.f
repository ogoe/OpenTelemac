C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CHECKS THAT THE PHYSICAL PARAMETERS ARE CREDIBLE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @warning  ARRET IS NOT INITIALISED

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> ARRET, BORNES, H, NPH, NPT, NPU, NPV, NTRAC, T, U, V, X, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, ITRAC
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>TELEMAC2D()

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
!>      <td><center> 5.8                                       </center>
!> </td><td> 05/09/2007
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>ARRET
!></td><td><--</td><td>LOGIQUE MIS A TRUE SI BORNES SONT DEPASSEES
!>    </td></tr>
!>          <tr><td>BORNES
!></td><td>--></td><td>VALEURS LIMITES DES TABLEAUX H,U,V,T
!>                  DANS L'ORDRE SUIVANT : HMIN,HMAX,UMIN,UMAX,...
!>    </td></tr>
!>          <tr><td>H,NPH
!></td><td>--></td><td>HAUTEUR ET NOMBRE DE POINTS DE HAUTEUR.
!>    </td></tr>
!>          <tr><td>NTRAC
!></td><td>--></td><td>NOMBRE DE TRACEURS.
!>    </td></tr>
!>          <tr><td>T,NPT
!></td><td>--></td><td>TRACEUR ET NOMBRE DE POINTS DE TRACEUR.
!>    </td></tr>
!>          <tr><td>U,NPU
!></td><td>--></td><td>VITESSE U ET NOMBRE DE POINTS DE VITESSE U.
!>    </td></tr>
!>          <tr><td>V,NPV
!></td><td>--></td><td>VITESSE V ET NOMBRE DE POINTS DE VITESSE V.
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ISITOK
     &(H,NPH,U,NPU,V,NPV,NTRAC,T,NPT,X,Y,BORNES,ARRET)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| ARRET          |<--| LOGIQUE MIS A TRUE SI BORNES SONT DEPASSEES
C| BORNES         |-->| VALEURS LIMITES DES TABLEAUX H,U,V,T
C|                |   | DANS L'ORDRE SUIVANT : HMIN,HMAX,UMIN,UMAX,...
C| H,NPH          |-->| HAUTEUR ET NOMBRE DE POINTS DE HAUTEUR.
C| NTRAC          |-->| NOMBRE DE TRACEURS.
C| T,NPT          |-->| TRACEUR ET NOMBRE DE POINTS DE TRACEUR.
C| U,NPU          |-->| VITESSE U ET NOMBRE DE POINTS DE VITESSE U.
C| V,NPV          |-->| VITESSE V ET NOMBRE DE POINTS DE VITESSE V.
C| X,Y            |-->| COORDONNEES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)          :: NPH,NPU,NPV,NPT,NTRAC
      LOGICAL, INTENT(INOUT)       :: ARRET
      DOUBLE PRECISION, INTENT(IN) :: H(NPH),U(NPU),V(NPV)
      DOUBLE PRECISION, INTENT(IN) :: X(*),Y(*),BORNES(8)
      TYPE(BIEF_OBJ)  , INTENT(IN) :: T
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,ITRAC
C
C-----------------------------------------------------------------------
C
C  CHECKS THE DEPTH
C
      DO 10 I = 1 , NPH
        IF(H(I).LT.BORNES(1)) THEN
          ARRET = .TRUE.
          IF(LNG.EQ.1) THEN
           WRITE(LU,100) 'INFERIEURE','H',I,X(I),Y(I),'H',H(I),BORNES(1)
          ENDIF
          IF(LNG.EQ.2) THEN
           WRITE(LU,101) 'LOWER','H',I,X(I),Y(I),'H',H(I),BORNES(1)
          ENDIF
        ENDIF
        IF(H(I).GT.BORNES(2)) THEN
          ARRET = .TRUE.
          IF(LNG.EQ.1) THEN
           WRITE(LU,100) 'SUPERIEURE','H',I,X(I),Y(I),'H',H(I),BORNES(2)
          ENDIF
          IF(LNG.EQ.2) THEN
           WRITE(LU,101) 'UPPER','H',I,X(I),Y(I),'H',H(I),BORNES(2)
          ENDIF
        ENDIF
10    CONTINUE
C
C-----------------------------------------------------------------------
C
C  CHECKS THE VELOCITY U
C
      DO 20 I = 1 , NPU
        IF(U(I).LT.BORNES(3)) THEN
          ARRET = .TRUE.
          IF(LNG.EQ.1) THEN
           WRITE(LU,100) 'INFERIEURE','U',I,X(I),Y(I),'U',U(I),BORNES(3)
          ENDIF
          IF(LNG.EQ.2) THEN
           WRITE(LU,101) 'LOWER','U',I,X(I),Y(I),'U',U(I),BORNES(3)
          ENDIF
        ENDIF
        IF(U(I).GT.BORNES(4)) THEN
          ARRET = .TRUE.
          IF(LNG.EQ.1) THEN
           WRITE(LU,100) 'SUPERIEURE','U',I,X(I),Y(I),'U',U(I),BORNES(4)
          ENDIF
          IF(LNG.EQ.2) THEN
           WRITE(LU,101) 'UPPER','U',I,X(I),Y(I),'U',U(I),BORNES(4)
          ENDIF
        ENDIF
20    CONTINUE
C
C-----------------------------------------------------------------------
C
C  CHECKS THE VELOCITY V
C
      DO 30 I = 1 , NPV
        IF(V(I).LT.BORNES(5)) THEN
          ARRET = .TRUE.
          IF(LNG.EQ.1) THEN
           WRITE(LU,100) 'INFERIEURE','V',I,X(I),Y(I),'V',V(I),BORNES(5)
          ENDIF
          IF(LNG.EQ.2) THEN
           WRITE(LU,101) 'LOWER','V',I,X(I),Y(I),'V',V(I),BORNES(5)
          ENDIF
        ENDIF
        IF(V(I).GT.BORNES(6)) THEN
          ARRET = .TRUE.
          IF(LNG.EQ.1) THEN
           WRITE(LU,100) 'SUPERIEURE','V',I,X(I),Y(I),'V',V(I),BORNES(6)
          ENDIF
          IF(LNG.EQ.2) THEN
           WRITE(LU,101) 'UPPER','V',I,X(I),Y(I),'V',V(I),BORNES(6)
          ENDIF
        ENDIF
30    CONTINUE
C
C-----------------------------------------------------------------------
C
C  CHECKS THE TRACER
C
      IF(NTRAC.GT.0) THEN
C
      DO ITRAC=1,NTRAC
C
      DO I = 1 , NPT
        IF(T%ADR(ITRAC)%P%R(I).LT.BORNES(7)) THEN
          ARRET = .TRUE.
          IF(LNG.EQ.1) THEN
           WRITE(LU,100) 'INFERIEURE','T',I,X(I),Y(I),
     &                   'T',T%ADR(ITRAC)%P%R(I),BORNES(7)
          ENDIF
          IF(LNG.EQ.2) THEN
           WRITE(LU,101) 'LOWER','T',I,X(I),Y(I),
     &                   'T',T%ADR(ITRAC)%P%R(I),BORNES(7)
          ENDIF
        ENDIF
        IF(T%ADR(ITRAC)%P%R(I).GT.BORNES(8)) THEN
          ARRET = .TRUE.
          IF(LNG.EQ.1) THEN
           WRITE(LU,100) 'SUPERIEURE','T',I,X(I),Y(I),
     &                   'T',T%ADR(ITRAC)%P%R(I),BORNES(8)
          ENDIF
          IF(LNG.EQ.2) THEN
           WRITE(LU,101) 'UPPER','T',I,X(I),Y(I),
     &                   'T',T%ADR(ITRAC)%P%R(I),BORNES(8)
          ENDIF
        ENDIF
      ENDDO
C
      ENDDO
C
      ENDIF
C
C-----------------------------------------------------------------------
C
100   FORMAT(/,1X,'LIMITE ',A10,' SUR ',A1,' ATTEINTE AU POINT ',I6,/,
     &         1X,'DE COORDONNEES ',G16.7,' ET ',G16.7,/,1X,
     &         A1,' VAUT : ',G16.7,' ET LA LIMITE EST :',G16.7)
101   FORMAT(/,1X,A5,' LIMIT ON ',A1,' REACHED AT POINT ',I6,/,1X,
     &         'WITH COORDINATES',G16.7,' AND ',G16.7,/,1X,
     &         'THE VALUE OF ',A1,' IS ',G16.7,' THE LIMIT IS: ',G16.7)
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C