C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SELECTS THE COMPUTATION NODES CLOSEST
!>                TO THE REQUESTED OUTPUT POINTS.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLE, ISLEO, NELEM2, NLEO, NOLEO, NPOIN2, SURDET, X, XLEO, Y, YLEO
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> ECRSPE_MPI : SPE_SEND
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> DIST, DIST2, I, IELEM, ILEO, N1G, N2G, N3G, NOELEM, SHP1, SHP2, SHP3
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>WAC()

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
!> </td><td> 01/02/95
!> </td><td> F. MARCOS (LNH) 30 87 72 66
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IKLE
!></td><td>--></td><td>CONNECTIVITE NOEUD ELEMENTS
!>    </td></tr>
!>          <tr><td>ISLEO
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NCSIZE
!></td><td>--></td><td>NOMBRE D'ELEMENTS 2D
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE D'ELEMENTS 2D
!>    </td></tr>
!>          <tr><td>NLEO
!></td><td>--></td><td>NOMBRE DE POINTS DE SORTIE
!>    </td></tr>
!>          <tr><td>NOLEO
!></td><td><-></td><td>TABLEAU DES NUMERO DES POINTS CHOISIS
!>    </td></tr>
!>          <tr><td>NOPID
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>SURDET
!></td><td>--></td><td>1/SUPERFICIE ELEMENTS
!>    </td></tr>
!>          <tr><td>X
!></td><td>--></td><td>ABSCISSES DES POINTS
!>    </td></tr>
!>          <tr><td>XLEO
!></td><td>--></td><td>TABLEAU DES ABSCISSES DES POINTS DE SORTIE
!>    </td></tr>
!>          <tr><td>Y
!></td><td>--></td><td>ORDONNEES DES POINTS
!>    </td></tr>
!>          <tr><td>YLEO
!></td><td>--></td><td>TABLEAU DES ORDONNEES DES POINTS DE SORTIE
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
      SUBROUTINE PRELEO_MPI
     &(XLEO,YLEO,NLEO,X,Y,IKLE,SURDET,NPOIN2,NELEM2,NOLEO,ISLEO)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLE           |-->| CONNECTIVITE NOEUD ELEMENTS
C| ISLEO          |---| 
C| NCSIZE         |-->| NOMBRE D'ELEMENTS 2D
C| NELEM2         |-->| NOMBRE D'ELEMENTS 2D
C| NLEO           |-->| NOMBRE DE POINTS DE SORTIE
C| NOLEO          |<->| TABLEAU DES NUMERO DES POINTS CHOISIS
C| NOPID          |---| 
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| SURDET         |-->| 1/SUPERFICIE ELEMENTS
C| X             |-->| ABSCISSES DES POINTS
C| XLEO           |-->| TABLEAU DES ABSCISSES DES POINTS DE SORTIE
C| Y             |-->| ORDONNEES DES POINTS
C| YLEO           |-->| TABLEAU DES ORDONNEES DES POINTS DE SORTIE
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
      IMPLICIT NONE

      COMMON/ECRSPE_MPI/SPE_SEND
      INTEGER SPE_SEND
C
      INTEGER I,ILEO,NLEO,NPOIN2,NELEM2,IELEM,NOELEM
C
      DOUBLE PRECISION X(NPOIN2)  , Y(NPOIN2)
      DOUBLE PRECISION XLEO(NLEO)  , YLEO(NLEO)
      DOUBLE PRECISION SURDET(NELEM2)
      DOUBLE PRECISION DIST,DIST2,SHP1,SHP2,SHP3
C
      INTEGER NOLEO(NLEO),N1G,N2G,N3G
      INTEGER IKLE(NELEM2,3)
      LOGICAL ISLEO(NLEO)
C
C-----------------------------------------------------------------------
C
C       DO 10 ILEO=1,NLEO
C         DIST=1.D99
C         DO 20 I=1,NPOIN2
C          DIST2=(XLEO(ILEO)-X(I))**2+(YLEO(ILEO)-Y(I))**2
C          IF (DIST2.LT.DIST) THEN
C              DIST=DIST2
C              NOLEO(ILEO)=I
C          ENDIF
C 20      CONTINUE
C 10    CONTINUE
       SPE_SEND = 0
       ISLEO = .FALSE.
       NOLEO = 1
       DO ILEO = 1,NLEO
          NOLEO(ILEO) = 1
          ISLEO(ILEO) = .FALSE.
          NOELEM = 0
          DO 20 IELEM = 1,NELEM2
             N1G=IKLE(IELEM,1)
             N2G=IKLE(IELEM,2)
             N3G=IKLE(IELEM,3)

               SHP1 = ((X(N3G)-X(N2G))*(YLEO(ILEO)-Y(N2G))
     &               -(Y(N3G)-Y(N2G))*(XLEO(ILEO)-X(N2G)))*SURDET(IELEM)
               SHP2 = ((X(N1G)-X(N3G))*(YLEO(ILEO)-Y(N3G))
     &               -(Y(N1G)-Y(N3G))*(XLEO(ILEO)-X(N3G)))*SURDET(IELEM)
               SHP3 = ((X(N2G)-X(N1G))*(YLEO(ILEO)-Y(N1G))
     &               -(Y(N2G)-Y(N1G))*(XLEO(ILEO)-X(N1G)))*SURDET(IELEM)

             IF ((SHP1.GE.0.D0).AND.(SHP2.GE.0.D0)
     &                                        .AND.(SHP3.GE.0.D0)) THEN
               ISLEO(ILEO) = .TRUE.
               NOELEM = IELEM
               IF (SHP2>SHP1) THEN
                  NOLEO(ILEO) = N2G
                  IF (SHP3>SHP2) NOLEO(ILEO) = N3G
               ELSE
                  NOLEO(ILEO) = N1G
                  IF (SHP3>SHP1) NOLEO(ILEO) = N3G
               ENDIF
                   SPE_SEND=SPE_SEND+1
               GOTO 30
              ENDIF
20         CONTINUE
30       CONTINUE
       ENDDO

C
C-----------------------------------------------------------------------
C
      RETURN
      END


C
C#######################################################################
C