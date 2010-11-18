C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SETS VARIABLES SORLEO AND SORIMP.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> CHAINE, MNEMO, NBRE, SORLEO
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> C, I, I1, I2, J, L, LONG, MOT, NMOT, OK
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>LECDON_ARTEMIS(), LECDON_SISYPHE(), LECDON_TELEMAC2D(), LECDON_TELEMAC3D(), LECDON_TOMAWAC()

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
!> </td><td> 03/11/2009
!> </td><td> J-M HERVOUET (LNHE)
!> </td><td> JOKER '*' ALLOWED IN NAMES
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>CHAINE
!></td><td><-></td><td>CHAINE DES VARIABLES DE SORTIE
!>    </td></tr>
!>          <tr><td>MNEMO
!></td><td><-></td><td>MNEMONIQUE DES VARIABLES
!>    </td></tr>
!>          <tr><td>NBRE
!></td><td><-></td><td>NOMBRE MAXIMAL DE VARIABLES A IMPRIMER
!>    </td></tr>
!>          <tr><td>SORLEO
!></td><td><-></td><td>TABLEAU DE LOGIQUES POUR IMPRESSION
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE SORTIE
     &( CHAINE , MNEMO , NBRE , SORLEO )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| CHAINE         |<->| CHAINE DES VARIABLES DE SORTIE
C| MNEMO          |<->| MNEMONIQUE DES VARIABLES
C| NBRE           |<->| NOMBRE MAXIMAL DE VARIABLES A IMPRIMER
C| SORLEO         |<->| TABLEAU DE LOGIQUES POUR IMPRESSION
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NBRE
C
      CHARACTER*(*), INTENT(INOUT) :: CHAINE
      CHARACTER(LEN=8), INTENT(IN) :: MNEMO(NBRE)
C
      LOGICAL, INTENT(INOUT) :: SORLEO(NBRE)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
C INTERNAL VARIABLES:
C
      CHARACTER C(2)
      CHARACTER(LEN=8) MOT(100)
      INTEGER I,J,LONG,I1,I2,NMOT,L
      LOGICAL OK
C
      INTRINSIC LEN
C
C-----------------------------------------------------------------------
C
C  RECOGNISED SEPARATORS IN 'CHAINE'
C
      C(1) = ','
      C(2) = ';'
      LONG = LEN(CHAINE)
      IF (LONG.EQ.0) THEN
        IF(LNG.EQ.1) WRITE(LU,1002)
        IF(LNG.EQ.2) WRITE(LU,1003)
1002    FORMAT(1X,'SORTIE (BIEF) : CHAINE VIDE')
1003    FORMAT(1X,'SORTIE (BIEF): EMPTY STRING')
        CALL PLANTE(1)
        STOP
      ENDIF
C
      DO I=1,LONG
        DO J=1,2
          IF(CHAINE(I:I).EQ.C(J)) CHAINE(I:I) = ' '
        ENDDO
      ENDDO
C
C 'CHAINE' NOW IS MADE UP OF WORDS SEPARATED BY WHITE SPACES
C
      I1 = 0
      NMOT=0
C
 10   CONTINUE
      IF (I1.GE.LONG) GOTO 30
      I1=I1+1
      IF (CHAINE(I1:I1).EQ.' ') GOTO 10
C
      I2=0
C
 20   CONTINUE
      I2=I2+1
      IF (CHAINE(I1+I2:I1+I2).NE.' ') GOTO 20
C
      NMOT=NMOT+1
      IF (I2.GT.8) THEN
        IF(LNG.EQ.1) WRITE(LU,1004) CHAINE
        IF(LNG.EQ.2) WRITE(LU,1005) CHAINE
1004    FORMAT(1X,'SORTIE (BIEF) : PLUS DE 8 CARACTERES PAR MOT',/,1X,
     &            '                 DANS LA CHAINE :',A)
1005    FORMAT(1X,'SORTIE (BIEF): MORE THAN 8 LETTERS PER WORD',/,1X,
     &            '                 IN THE CHAIN: ',A)
        CALL PLANTE(1)
        STOP
      ENDIF
      MOT(NMOT)=CHAINE(I1:I1+I2)
      I1=I1+I2
      GOTO 10
C
30    CONTINUE
C
C     COMPARES 'MOT' AND 'MNEMO'
C
      DO I=1,NBRE
        DO J=1,NMOT
          OK=.TRUE.
          DO L=1,8
C           A JOKER '*' IS ALLOWED
            IF(MOT(J)(L:L).NE.MNEMO(I)(L:L).AND.MOT(J)(L:L).NE.'*') THEN
              OK=.FALSE.
              EXIT
            ENDIF
          ENDDO
          SORLEO(I)=OK
          IF(SORLEO(I)) EXIT
        ENDDO
      ENDDO
C
C-----------------------------------------------------------------------
C
      RETURN
      END
C
C#######################################################################
C