C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       READS THE CURRENTS / WINDS IN A USER-DEFINED FILE FORMAT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note - THE DATA READ WILL BE STORED IN ARRAYS XRELV, YRELV, UR
!>        AND VR.
!><br>    - INTERPOLATION ON THE COMPUTATION MESH IS AUTOMATICALLY DONE
!>        AT THE END OF THE SUBROUTINE.
!><br>    - REMEMBER TO SUPPLY NP FOR THE INTERPOLATION AND TO COMMENT
!>        OUT THE WRITE STATEMENT.

!>  @warning  USER SUBROUTINE; MUST BE CODED BY THE USER TO READ IN THE CURRENT/WIND FILE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> NCOU, NP, NPMAX, NPOIN, TRA01, UR, VR, X, XRELV, Y, YRELV
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
!>   </td></tr>
!>     </table>

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
!>      <td><center> 1.0                                       </center>
!> </td><td> 30/08/95
!> </td><td> F.MARCOS (LNH) 30 87 72 66
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>NCOU
!></td><td>--></td><td>NUMERO D'UNITE LOGIQUE DU FICHIER DES COURA.
!>    </td></tr>
!>          <tr><td>NP
!></td><td><--</td><td>NOMBRE DE POINTS RELEVES
!>    </td></tr>
!>          <tr><td>NPMAX
!></td><td>--></td><td>NOMBRE DE POINTS RELEVES MAXIMUM
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>TRA01
!></td><td><-></td><td>TABLEAU DE TRAVAIL DE DIMENSION NPMAX
!>    </td></tr>
!>          <tr><td>UR,VR
!></td><td><-></td><td>TABLEAU DES COURANTS OU VENTS RELEVES
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DU MAILLAGE
!>    </td></tr>
!>          <tr><td>XRELV
!></td><td><-></td><td>TABLEAU DES ABSCISSES DES POINTS RELEVES
!>    </td></tr>
!>          <tr><td>YRELV
!></td><td><-></td><td>TABLEAU DES ORDONNEES DES POINTS RELEVES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE LECUTI
     &(X,Y,NPOIN,NCOU,XRELV,YRELV,UR,VR,TRA01,NP,NPMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| NCOU           |-->| NUMERO D'UNITE LOGIQUE DU FICHIER DES COURA.
C| NP             |<--| NOMBRE DE POINTS RELEVES
C| NPMAX          |-->| NOMBRE DE POINTS RELEVES MAXIMUM
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
C| TRA01          |<->| TABLEAU DE TRAVAIL DE DIMENSION NPMAX
C| UR,VR          |<->| TABLEAU DES COURANTS OU VENTS RELEVES
C| X,Y            |-->| COORDONNEES DU MAILLAGE
C| XRELV          |<->| TABLEAU DES ABSCISSES DES POINTS RELEVES
C| YRELV          |<->| TABLEAU DES ORDONNEES DES POINTS RELEVES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
      INTEGER NPMAX,NP
C
      INTEGER NCOU,NPOIN
C
      DOUBLE PRECISION X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION XRELV(NPMAX),YRELV(NPMAX),UR(NPMAX),VR(NPMAX)
      DOUBLE PRECISION TRA01(NPMAX)
C
C-----------------------------------------------------------------------
C
      WRITE(LU,*)'***********************************************'
      IF (LNG.EQ.1) THEN
        WRITE(LU,*)'  VOUS FAITES APPEL A LA PROCEDURE LECUTI      '
        WRITE(LU,*)'  (FORMAT DU FICHIER DES COURANTS OU VENTS = 4)'
        WRITE(LU,*)'     MAIS VOUS NE L''AVEZ PAS MODIFIEE         '
      ELSE
        WRITE(LU,*)'     YOU CALL SUBROUTINE LECUTI                '
        WRITE(LU,*)'  (FORMAT OF CURRENT OF WIND FILE = 4)         '
        WRITE(LU,*)'     BUT YOU DID NOT CHANGE IT                 '
      ENDIF
      WRITE(LU,*)'***********************************************'
      CALL PLANTE(0)

      RETURN
      END
C
C#######################################################################
C