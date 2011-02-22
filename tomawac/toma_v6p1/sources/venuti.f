C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       READS THE WINDS FROM A USED-DEFINED FILE FORMAT.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note DURING THE FIRST PASS THE USER MUST IDENTIFY THE TIMES TV1 AND TV2
!>        WHICH SURROUND THE FIRST TIME STEP. NEXT, USING THE ARRAYS
!>        XRELV,YRELV,UR,VR OR DIRECTLY FROM THE WIND FILE, THE USER
!>        MAY HAVE TO INTERPOLATE THE TIDES READ FROM THE FILE INTO THE
!>        ARRAYS U1,V1 U2,V2.
!><br>    INTERPOLATION SUBROUTINE FASP :
!><br>    CALL FASP(X,Y,U1,NPOIN,XRELV,YRELV,UR,NP,NBOR,MESH%KP1BOR%I,NPTFR,0.D0)
!><br>    CALL FASP(X,Y,V1,NPOIN,XRELV,YRELV,VR,NP,NBOR,MESH%KP1BOR%I,NPTFR,0.D0)
!><br>    THE CODE WILL INTERPOLATE THE WIND AUTOMATICALLY BETWEEN THESE
!>        2 TIME STEPS.
!><br>    THE OTHER PASSES OCCUR WHEN A NEW RECORD IS REQUIRED (AT>TV2).
!>        IN THIS CASE TV2,U2,V2 ONLY ARE TO BE COMPUTED.

!>  @warning  USER SUBROUTINE; MUST BE CODED BY THE USER TO READ IN THE WIND FILE

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>DECLARATIONS_TOMAWAC
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> AT, BINVEN, DDC, NBOR, NP, NPMAX, NPOIN, NPTFR, NVEN, TV1, TV2, U1, U2, UR, V1, V2, VR, X, XRELV, Y, YRELV
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
!><br>INIVEN(), LECDOI(), LECDON(), NOUDON()

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
!>          <tr><td>BINVEN
!></td><td>--></td><td>BINAIRE DU FICHIER DES VENTS
!>    </td></tr>
!>          <tr><td>DDC
!></td><td>--></td><td>DATE DE DEBUT DU CALCUL
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>NUMERO GLOBAUX DES POINTS DE BORD
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
!>          <tr><td>NVEN
!></td><td>--></td><td>NUMERO D'UNITE LOGIQUE DU FICHIER DES VENTS
!>    </td></tr>
!>          <tr><td>TV1
!></td><td>--></td><td>DATE CORRESPONDANT AU CHAMP DE VENT U1,V1
!>    </td></tr>
!>          <tr><td>TV2
!></td><td>--></td><td>DATE CORRESPONDANT AU CHAMP DE VENT U2,V2
!>    </td></tr>
!>          <tr><td>U1,V1
!></td><td><-></td><td>TABLEAU DES VENTS RELEVES AU TEMPS 1
!>    </td></tr>
!>          <tr><td>U2,V2
!></td><td><-></td><td>TABLEAU DES VENTS RELEVES AU TEMPS 2
!>    </td></tr>
!>          <tr><td>UR,VR
!></td><td><-></td><td>TABLEAU DES VENTS RELEVES
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
                        SUBROUTINE VENUTI
     &(X,Y,NPOIN,NVEN, BINVEN,NBOR,NPTFR,AT,DDC,TV1,TV2,
     & NP,XRELV,YRELV,UR,VR,U1,V1,U2,V2,NPMAX)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| AT             |-->| TEMPS
C| BINVEN         |-->| BINAIRE DU FICHIER DES VENTS
C| DDC            |-->| DATE DE DEBUT DU CALCUL
C| NBOR           |-->| NUMERO GLOBAUX DES POINTS DE BORD
C| NP             |-->| NOMBRE DE POINTS RELEVES
C| NPMAX          |-->| NOMBRE DE POINTS RELEVES MAXIMUM
C| NPOIN          |-->| NOMBRE DE POINTS DU MAILLAGE
C| NPTFR          |-->| NOMBRE DE POINTS DE BORD
C| NVEN           |-->| NUMERO D'UNITE LOGIQUE DU FICHIER DES VENTS
C| TV1            |-->| DATE CORRESPONDANT AU CHAMP DE VENT U1,V1
C| TV2            |-->| DATE CORRESPONDANT AU CHAMP DE VENT U2,V2
C| U1,V1          |<->| TABLEAU DES VENTS RELEVES AU TEMPS 1
C| U2,V2          |<->| TABLEAU DES VENTS RELEVES AU TEMPS 2
C| UR,VR          |<->| TABLEAU DES VENTS RELEVES
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
      INTEGER NVEN,NPOIN,NPMAX,NP,NPTFR,NBOR(NPTFR,2)
C
      DOUBLE PRECISION X(NPOIN),Y(NPOIN)
      DOUBLE PRECISION XRELV(NPMAX),YRELV(NPMAX), UR(NPMAX),VR(NPMAX)
      DOUBLE PRECISION U1(NPMAX),V1(NPMAX),U2(NPMAX),V2(NPMAX)
      DOUBLE PRECISION AT,DDC,TV1,TV2
C
      CHARACTER*3 BINVEN
C-----------------------------------------------------------------------
C
      WRITE(LU,*) '*********************************************'
      WRITE(LU,*) '  VOUS FAITES APPEL A LA PROCEDURE VENUTI    '
      WRITE(LU,*) '    (FORMAT DU FICHIER DES VENTS = 4)        '
      WRITE(LU,*) '     MAIS VOUS NE L''AVEZ PAS MODIFIEE       '
      WRITE(LU,*) '*********************************************'
      CALL PLANTE(0)

      RETURN
      END
C
C#######################################################################
C
