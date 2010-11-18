C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       WRITES OUT VALUES ACCORDING TO VARIOUS STANDARDS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  FORMER SUBROUTINE ECRIT;
!>         NAME CHANGED BECAUSE THIS NAME EXISTS IN THE CALCIUM LIBRARY

!>  @warning  SUBROUTINES ECRIBM AND ECRI3E ARE DEPENDENT ON THE
!>            MACHINE USED.
!>            FOR EXAMPLE ECRIBM APPEARS IN THE GENERAL IMA LIBRARY
!>           (EDF). ON WORKSTATION, ECRIBM IS AN EMPTY SUBROUTINE.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> C, CANAL, I, ISTAT, NVAL, STD, TYPE, X
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> J
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CREATE_DATASET_SERAFIN(), DESSED(), ECRGEO(), SFLO3D(), SOR3D(), SORFLO(), WRITE_DATA_SERAFIN(), WRITE_MESH_SERAFIN()

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
!>      <td><center> 5.1                                       </center>
!> </td><td> 17/08/94
!> </td><td> J-M HERVOUET (LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>C
!></td><td>--></td><td>CHAINE DE CARACTERES A ECRIRE
!>    </td></tr>
!>          <tr><td>CANAL
!></td><td>--></td><td>UNITE LOGIQUE POUR L'ECRITURE
!>    </td></tr>
!>          <tr><td>I
!></td><td>--></td><td>TABLEAU A ECRIRE S'IL EST ENTIER
!>    </td></tr>
!>          <tr><td>ISTAT
!></td><td><--</td><td>ENTIER EN CAS D'ERREUR
!>    </td></tr>
!>          <tr><td>NVAL
!></td><td>--></td><td>NOMBRE DE VALEURS DANS LE TABLEAU
!>                  OU NOMBRE DE CARACTERES DE LA CHAINE
!>    </td></tr>
!>          <tr><td>STD
!></td><td>--></td><td>STANDARD D'ECRITURE : STD , IBM OU I3E
!>    </td></tr>
!>          <tr><td>TYPE
!></td><td>--></td><td>TYPE DES DONNEES A ECRIRE :
!>                  'I' , 'CH' , 'R4' , 'R8'
!>    </td></tr>
!>          <tr><td>X
!></td><td>--></td><td>TABLEAU A ECRIRE S'IL EST REEL
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ECRI2
     &(X , I , C , NVAL , TYPE , CANAL , STD , ISTAT)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| C             |-->| CHAINE DE CARACTERES A ECRIRE
C| CANAL          |-->| UNITE LOGIQUE POUR L'ECRITURE
C| I             |-->| TABLEAU A ECRIRE S'IL EST ENTIER
C| ISTAT          |<--| ENTIER EN CAS D'ERREUR
C| NVAL           |-->| NOMBRE DE VALEURS DANS LE TABLEAU
C|                |   | OU NOMBRE DE CARACTERES DE LA CHAINE
C| STD            |-->| STANDARD D'ECRITURE : STD , IBM OU I3E
C| TYPE           |-->| TYPE DES DONNEES A ECRIRE :
C|                |   | 'I' , 'CH' , 'R4' , 'R8'
C| X             |-->| TABLEAU A ECRIRE S'IL EST REEL
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NVAL,CANAL
      DOUBLE PRECISION, INTENT(IN) :: X(NVAL)
      INTEGER, INTENT(IN) :: I(NVAL)
      CHARACTER*(*), INTENT(IN) :: TYPE,STD,C
      INTEGER, INTENT(OUT) :: ISTAT
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER J
C
      INTRINSIC REAL
C
C-----------------------------------------------------------------------
C
      ISTAT = 0
C
C-----------------------------------------------------------------------
C
      IF(STD(1:3).EQ.'STD') THEN
C
         IF (TYPE(1:2).EQ.'R4') THEN
            WRITE(CANAL)(REAL(X(J)),J=1,NVAL)
         ELSEIF (TYPE(1:2).EQ.'R8') THEN
            WRITE(CANAL)(X(J),J=1,NVAL)
         ELSEIF (TYPE(1:1).EQ.'I') THEN
            WRITE(CANAL)(I(J),J=1,NVAL)
         ELSEIF (TYPE(1:2).EQ.'CH') THEN
            WRITE(CANAL) C(1:NVAL)
         ELSE
            IF(LNG.EQ.1) THEN
              WRITE(LU,20) TYPE
20            FORMAT(1X,'ECRI2 : TYPE INCONNU :',A2)
            ENDIF
            IF(LNG.EQ.2) THEN
              WRITE(LU,21) TYPE
21            FORMAT(1X,'ECRI2 : UNKNOWN TYPE:',A2)
            ENDIF
            CALL PLANTE(0)
            STOP
         ENDIF
C
C-----------------------------------------------------------------------
C
C     ELSEIF(STD(1:3).EQ.'IBM') THEN
C
C BEWARE : CRAY DOUBLE PRECISION IS NOT ENVISAGED HERE
C
C        IF (TYPE(1:2).EQ.'R4') THEN
C           CALL ECRIBM( X , NVAL , TYPE , CANAL )
C        ELSEIF (TYPE(1:2).EQ.'R8') THEN
C           CALL ECRIBM( X , NVAL , TYPE , CANAL )
C        ELSEIF (TYPE(1:1).EQ.'I') THEN
C           CALL ECRIBM( I , NVAL , TYPE , CANAL )
C        ELSEIF (TYPE(1:2).EQ.'CH') THEN
C           THIS COPY APPEAR TO AVOID A BUG IN ECRIBM
C           CHAINE(1:NVAL) = C(1:NVAL)
C           CALL ECRIBM( CHAINE , NVAL , TYPE , CANAL )
C        ELSE
C           IF(LNG.EQ.1) WRITE(LU,20) TYPE
C           IF(LNG.EQ.2) WRITE(LU,21) TYPE
C           CALL PLANTE(0)
C           STOP
C        ENDIF
C
C-----------------------------------------------------------------------
C
C     ELSEIF(STD(1:3).EQ.'I3E') THEN
C
C BEWARE : CRAY DOUBLE PRECISION IS NOT ENVISAGED HERE
C
C        IF (TYPE(1:2).EQ.'R4') THEN
C           CALL ECRI3E( X , NVAL , 'F' , CANAL , ISTAT )
C        ELSEIF (TYPE(1:2).EQ.'R8') THEN
C           CALL ECRI3E( X , NVAL , 'F' , CANAL , ISTAT )
C        ELSEIF (TYPE(1:1).EQ.'I') THEN
C           CALL ECRI3E( I , NVAL , 'I' , CANAL , ISTAT )
C        ELSEIF (TYPE(1:2).EQ.'CH') THEN
C           CALL ECRI3E( C(1:NVAL) , NVAL , 'C' , CANAL , ISTAT )
C        ELSE
C           IF(LNG.EQ.1) WRITE(LU,20) TYPE
C           IF(LNG.EQ.2) WRITE(LU,21) TYPE
C           CALL PLANTE(0)
C           STOP
C        ENDIF
C
C-----------------------------------------------------------------------
C
      ELSE
C
        IF(LNG.EQ.1) THEN
          WRITE(LU,10) STD
10        FORMAT(1X,'ECRI2 : STANDARD D''ECRITURE INCONNU :',A8)
        ENDIF
        IF(LNG.EQ.2) THEN
          WRITE(LU,11) STD
11        FORMAT(1X,'ECRI2 : UNKNOWN STANDARD:',A8)
        ENDIF
        STOP
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