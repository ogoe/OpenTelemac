C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CALCULATES THE NUMBER OF SMOOTHINGS ON THE WAVE
!>                HEIGHT (LISHHO), A PRIORI NECESSARY TO FILTER OUT
!>                THE PARASITIC OSCILLATIONS (REGULAR WAVES).
!>                ESTIMATED FROM THE AVERAGE DISTANCE BETWEEN
!>                NODES AND THE AVERAGE NUMBER OF NODES IN HALF A
!>                WAVELENGTH.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>INTERFACE_ARTEMIS
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLE, K, LISHHO, NELEM, NPOIN, X, Y
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> D1, D2, DMOY, IELEM, ISOM, NG, PI, SOM, SOMD
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_DISMOY
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>ARTEMIS()

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
!>  <tr>
!>    <td><center> 5.1                                    </center></td>
!>    <td> 04/06/1999                                              </td>
!>    <td> D. AELBRECHT (LNH) 01.30.87.74.12; P. THELLIER (LNH)    </td>
!>    <td> THANK YOU PAUL                                          </td>
!>  <tr>
!>    <td><center>                                        </center></td>
!>    <td>                                                         </td>
!>    <td> J-M HERVOUET (LNH) 01.30.87.80.18                       </td>
!>    <td> LINKED TO BIEF 5.0                                      </td>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IKLE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>K
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>LISHHO
!></td><td><--</td><td>NOMBRE DE LISSAGES SUR HHO
!>    </td></tr>
!>          <tr><td>NELEM
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>X
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Y
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DISMOY
     &(NPOIN,NELEM,X,Y,IKLE,K,LISHHO)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLE           |---| 
C| K             |---| 
C| LISHHO         |<--| NOMBRE DE LISSAGES SUR HHO
C| NELEM          |---| 
C| NPOIN          |---| 
C| X             |---| 
C| Y             |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE INTERFACE_ARTEMIS, EX_DISMOY => DISMOY
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C
C VARIABLES FOR THE COMPUTATION OF AVERAGE DISTANCES
C
      INTEGER NG, ISOM, IELEM
      DOUBLE PRECISION SOM, D1, D2, DMOY, PI, SOMD
C
C
      INTEGER NPOIN,NELEM,LISHHO
      INTEGER IKLE(NELEM,*)
C
      DOUBLE PRECISION X(NPOIN),Y(NPOIN),K(NPOIN)
C
C-----------------------------------------------------------------------
C
      PI = 3.1415926535897932384626433D0
C
C     COMPUTES THE AVERAGE DISTANCE BETWEEN A NODE AND ITS NEIGHBOURS
C     -----------------------------------------------------------
C
      SOMD = 0.D0
      DO 300 NG=1,NPOIN
         ISOM = 0
         SOM = 0.D0
C
         DO 250 IELEM=1,NELEM
C
            IF (IKLE(IELEM,1).EQ.NG) THEN
C           --                       ---
C
C            PT 1 IS COMMON, COMPUTES THE DISTANCE TO PTS 2 AND 3
             D1 = SQRT ( (X(NG)-X(IKLE(IELEM,2))) **2.D0 +
     &                   (Y(NG)-Y(IKLE(IELEM,2))) **2.D0 )
             D2 = SQRT ( (X(NG)-X(IKLE(IELEM,3))) **2.D0 +
     &                   (Y(NG)-Y(IKLE(IELEM,3))) **2.D0 )
             SOM = SOM + D1 + D2
             ISOM = ISOM + 2
C
             ELSEIF (IKLE(IELEM,2).EQ.NG) THEN
C            ------                       ----
C
C            PT 2 IS COMMON, COMPUTES THE DISTANCE TO PTS 1 AND 3
             D1 = SQRT ( (X(NG)-X(IKLE(IELEM,1))) **2.D0 +
     &                   (Y(NG)-Y(IKLE(IELEM,1))) **2.D0 )
             D2 = SQRT ( (X(NG)-X(IKLE(IELEM,3))) **2.D0 +
     &                   (Y(NG)-Y(IKLE(IELEM,3))) **2.D0 )
             SOM = SOM + D1 + D2
             ISOM = ISOM + 2
C
             ELSEIF (IKLE(IELEM,3).EQ.NG) THEN
C            ------                       ----
C
C            PT 3 IS COMMON, COMPUTES THE DISTANCE TO PTS 1 AND 2
             D1 = SQRT ( (X(NG)-X(IKLE(IELEM,1))) **2.D0 +
     &                   (Y(NG)-Y(IKLE(IELEM,1))) **2.D0 )
             D2 = SQRT ( (X(NG)-X(IKLE(IELEM,2))) **2.D0 +
     &                   (Y(NG)-Y(IKLE(IELEM,2))) **2.D0 )
             SOM = SOM + D1 + D2
             ISOM = ISOM + 2
C
            ENDIF
C           -----
C
 250     CONTINUE
C
         DMOY = SOM / FLOAT(ISOM)
         SOMD = SOMD + (PI / (K(NG)*DMOY))
C
 300  CONTINUE
C
C     ESTIMATES THE NUMBER OF SMOOTHINGS FROM THE AVERAGE DISTANCE
C     -------------------------------------------------------------------
C
      LISHHO = INT((SOMD/FLOAT(NPOIN))) * 10
C
      RETURN
      END
C
C#######################################################################
C