C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       READS IN THE CURRENTS USING A USER-DEFINED FORMAT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note DURING THE FIRST PASS THE USER MUST IDENTIFY THE TIMES TC1 AND TC2
!>        WHICH SURROUND THE FIRST TIME STEP. NEXT, USING THE ARRAYS
!>        XRELC,YRELC,UR,VR OR DIRECTLY FROM THE CURRENT FILE, THE USER
!>        MAY HAVE TO INTERPOLATE THE CURRENTS READ FROM THE FILE INTO THE
!>        ARRAYS UC1,VC1 UC2,VC2.
!><br>    INTERPOLATION SUBROUTINE FASP :
!><br>    CALL FASP(X,Y,UC1,NPOIN,XRELC,YRELC,UR,NP,NBOR,MESH%KP1BOR%I,NPTFR,0.D0)
!><br>    CALL FASP(X,Y,VC1,NPOIN,XRELC,YRELC,VR,NP,NBOR,MESH%KP1BOR%I,NPTFR,0.D0)
!><br>    THE CODE WILL INTERPOLATE THE CURRENT AUTOMATICALLY BETWEEN THESE
!>        2 TIME STEPS.
!><br>    THE OTHER PASSES OCCUR WHEN A NEW RECORD IS REQUIRED (AT>TC2).
!>        IN THIS CASE TC2,UC2,VC2 ONLY ARE TO BE COMPUTED.

!>  @warning  USER SUBROUTINE; MUST BE CODED BY THE USER TO READ IN THE CURRENT FILE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>DECLARATIONS_TOMAWAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, BINCOU, DDC, NBOR, NCOU, NP, NPMAX, NPOIN, NPTFR, TC1, TC2, UC1, UC2, UR, VC1, VC2, VR, X, XRELC, Y, YRELC
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

!>  @par Called by
!><br>LECDOI(), LECDON(), NOUDON()

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
!>          <tr><td>AT
!></td><td>--></td><td>TEMPS
!>    </td></tr>
!>          <tr><td>BINCOU
!></td><td>--></td><td>BINAIRE DU FICHIER DES COURANTS
!>    </td></tr>
!>          <tr><td>DDC
!></td><td>--></td><td>DATE DE DEBUT DU CALCUL
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMERO GLOBAUX DES POINTS DE BORD
!>    </td></tr>
!>          <tr><td>NCOU
!></td><td>--></td><td>NO D'UNITE LOGIQUE DU FICHIER DES COURANTS
!>    </td></tr>
!>          <tr><td>NP
!></td><td>--></td><td>NOMBRE DE POINTS RELEVES
!>    </td></tr>
!>          <tr><td>NPMAX
!></td><td>--></td><td>NOMBRE DE POINTS RELEVES MAXIMUM
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>--></td><td>NOMBRE DE POINTS DU MAILLAGE
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS DE BORD
!>    </td></tr>
!>          <tr><td>TC1
!></td><td>--></td><td>DATE CORRESPONDANT AU COURANT (UC1,VC1)
!>    </td></tr>
!>          <tr><td>TC2
!></td><td>--></td><td>DATE CORRESPONDANT AU COURANT (UC2,VC2)
!>    </td></tr>
!>          <tr><td>UC1,VC1
!></td><td><-></td><td>TABLEAU DES COURANTS RELEVES AU TEMPS 1
!>    </td></tr>
!>          <tr><td>UC2,VC2
!></td><td><-></td><td>TABLEAU DES COURANTS RELEVES AU TEMPS 2
!>    </td></tr>
!>          <tr><td>UR,VR
!></td><td><-></td><td>TABLEAU DES COURANTS RELEVES
!>    </td></tr>
!>          <tr><td>X,Y
!></td><td>--></td><td>COORDONNEES DU MAILLAGE
!>    </td></tr>
!>          <tr><td>XRELC
!></td><td><-></td><td>TABLEAU DES ABSCISSES DES POINTS RELEVES
!>    </td></tr>
!>          <tr><td>YRELC
!></td><td><-></td><td>TABLEAU DES ORDONNEES DES POINTS RELEVES
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE COUUTI
     &(X,Y,NPOIN,NCOU, BINCOU,NBOR,NPTFR,AT,DDC,TC1,TC2,
     & NP,XRELC,YRELC,UR,VR,UC1,VC1,UC2,VC2,NPMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TEMPS
C| BINCOU         |-->| BINAIRE DU FICHIER DES COURANTS
C| DDC            |-->| DATE DE DEBUT DU CALCUL
C| NBOR           |-->| NUMERO GLOBAUX DES POINTS DE BORD
C| NCOU           |-->| NO D'UNITE LOGIQUE DU FICHIER DES COURANTS
C| NP             |-->| NOMBRE DE POINTS RELEVES
C| NPMAX          |-->| NOMBRE DE POINTS RELEVES MAXIMUM
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
C| NPTFR          |-->| NOMBRE DE POINTS DE BORD
C| TC1            |-->| DATE CORRESPONDANT AU COURANT (UC1,VC1)
C| TC2            |-->| DATE CORRESPONDANT AU COURANT (UC2,VC2)
C| UC1,VC1        |<->| TABLEAU DES COURANTS RELEVES AU TEMPS 1
C| UC2,VC2        |<->| TABLEAU DES COURANTS RELEVES AU TEMPS 2
C| UR,VR          |<->| TABLEAU DES COURANTS RELEVES
C| X,Y            |-->| COORDONNEES DU MAILLAGE
C| XRELC          |<->| TABLEAU DES ABSCISSES DES POINTS RELEVES
C| YRELC          |<->| TABLEAU DES ORDONNEES DES POINTS RELEVES
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_TOMAWAC, EX_COUUTI => COUUTI
C
      IMPLICIT NONE
C
      INTEGER LNG,LU
      COMMON/INFO/ LNG,LU
C
      INTEGER NCOU,NPOIN,NPMAX,NP,NPTFR,NBOR(NPTFR,2)
C
      DOUBLE PRECISION X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION XRELC(NPMAX),YRELC(NPMAX), UR(NPMAX),VR(NPMAX)
      DOUBLE PRECISION UC1(NPMAX),VC1(NPMAX),UC2(NPMAX),VC2(NPMAX)
      DOUBLE PRECISION AT,DDC,TC1,TC2
C
      CHARACTER*3 BINCOU
C-----------------------------------------------------------------------
C
      IF(LNG.EQ.1) THEN
         WRITE(LU,*) '*********************************************'
         WRITE(LU,*) '  VOUS FAITES APPEL A LA PROCEDURE COUUTI    '
         WRITE(LU,*) '    (FORMAT DU FICHIER DES COURANTS = 3)     '
         WRITE(LU,*) '     MAIS VOUS NE L''AVEZ PAS MODIFIEE       '
         WRITE(LU,*) '*********************************************'
      ELSE
         WRITE(LU,*) '*********************************************'
         WRITE(LU,*) '       YOU CALL THE SUBROUTINE COUUTI        '
         WRITE(LU,*) '        (CURRENTS FILE FORMAT = 3)           '
         WRITE(LU,*) '       BUT YOU DID NOT MODIFIED IT           '
         WRITE(LU,*) '*********************************************'
      ENDIF
      CALL PLANTE(0)

      RETURN
      END
C
C#######################################################################
C
