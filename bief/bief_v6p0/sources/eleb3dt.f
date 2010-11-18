C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       CASE OF PRISMS SPLIT IN TETRAHEDRONS.
!>                BUILDS THE 3D MESH.
!><br>            INPUT: 3D MESH ARRAYS FILLED BY A PRELIMINARY CALL
!>                       TO ELEBD.
!><br>            OUTPUT: ARRAYS COMPLETE IN 3D.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> IKLBOR, IKLE3, KP1BOR, NBOR, NELBOR, NELEM2, NELMAX2, NETAGE, NPLAN, NPOIN2, NPTFR, NULONE
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> IELEM, IETAGE, IG, IG1, IG2, IG3, IG4, IL, IL1, IL2, IL3, IL4, IPOIN, IPTFR, K, L, M, N, NUM1, NUM2, NUM3, OK, T
!>   </td></tr>
!>     <tr><th> Alias(es)
!>    </th><td> EX_ELEB3DT
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> PLANTE()
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
!>      <td><center> 5.3                                       </center>
!> </td><td> 23/08/99
!> </td><td> J-M HERVOUET(LNH) 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>IKLBOR
!></td><td><--</td><td>TABLE DE CONNECTIVITE ELEMENTS DE BORD
!>    </td></tr>
!>          <tr><td>IKLE2
!></td><td>--></td><td>CORRESPONDANCE LOCALE - GLOGALE EN 2D
!>    </td></tr>
!>          <tr><td>IKLE3
!></td><td><--</td><td>CORRESPONDANCE LOCALE - GLOBALE EN 3D
!>    </td></tr>
!>          <tr><td>KP1BOR
!></td><td>--></td><td>PT FRONT. SUIVANT LE PT FRONT. CONSIDERE
!>    </td></tr>
!>          <tr><td>NBOR
!></td><td>--></td><td>CORRESPONDANCE ENTRE LA NUMEROTATION DE BORD
!>                  ET LA NUMEROTATION GLOBALE (2D)
!>    </td></tr>
!>          <tr><td>NBOR3
!></td><td><--</td><td>CORRESPONDANCE ENTRE LA NUMEROTATION DE BORD
!>                  ET LA NUMEROTATION GLOBALE (3D)
!>    </td></tr>
!>          <tr><td>NELBO3
!></td><td><--</td><td>ASSOCIE A CHAQUE FACE DE BORD L'ELEMENT 3D
!>                  AUQUEL ELLE APPARTIENT
!>    </td></tr>
!>          <tr><td>NELBOR
!></td><td>--></td><td>NUMERO GLOBAUX DES ELEMENTS DE BORD
!>    </td></tr>
!>          <tr><td>NELEM2
!></td><td>--></td><td>NOMBRE D'ELEMENTS EN 2D
!>    </td></tr>
!>          <tr><td>NELMAX2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NETAGE
!></td><td>--></td><td>NPLAN - 1
!>    </td></tr>
!>          <tr><td>NPLAN
!></td><td>--></td><td>NOMBRE DE PLANS HORIZONTAUX
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>--></td><td>NOMBRE DE POINTS 2D
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS DE BORD 2D
!>    </td></tr>
!>          <tr><td>NULONE
!></td><td><--</td><td>ASSOCIE LA NUMEROTATION LOCALE DE BORD A LA
!>                  NUMEROTATION LOCALE 3D
!>    </td></tr>
!>          <tr><td>SURFAC
!></td><td><-></td><td>SURFACE DES TRIANGLES ETENDUE EN 3D
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE ELEB3DT
     &(IKLE3 , NBOR   , KP1BOR, NELBOR, IKLBOR, NULONE,
     & NELEM2, NELMAX2, NPOIN2, NPLAN , NETAGE, NPTFR  )
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| IKLBOR         |<--| TABLE DE CONNECTIVITE ELEMENTS DE BORD
C| IKLE2          |-->| CORRESPONDANCE LOCALE - GLOGALE EN 2D
C| IKLE3          |<--| CORRESPONDANCE LOCALE - GLOBALE EN 3D
C| KP1BOR         |-->| PT FRONT. SUIVANT LE PT FRONT. CONSIDERE
C| NBOR           |-->| CORRESPONDANCE ENTRE LA NUMEROTATION DE BORD
C|                |   | ET LA NUMEROTATION GLOBALE (2D)
C| NBOR3          |<--| CORRESPONDANCE ENTRE LA NUMEROTATION DE BORD
C|                |   | ET LA NUMEROTATION GLOBALE (3D)
C| NELBO3         |<--| ASSOCIE A CHAQUE FACE DE BORD L'ELEMENT 3D
C|                |   | AUQUEL ELLE APPARTIENT
C| NELBOR         |-->| NUMERO GLOBAUX DES ELEMENTS DE BORD
C| NELEM2         |-->| NOMBRE D'ELEMENTS EN 2D
C| NELMAX2        |---| 
C| NETAGE         |-->| NPLAN - 1
C| NPLAN          |-->| NOMBRE DE PLANS HORIZONTAUX
C| NPOIN2         |-->| NOMBRE DE POINTS 2D
C| NPTFR          |-->| NOMBRE DE POINTS DE BORD 2D
C| NULONE         |<--| ASSOCIE LA NUMEROTATION LOCALE DE BORD A LA
C|                |   | NUMEROTATION LOCALE 3D
C| SURFAC         |<->| SURFACE DES TRIANGLES ETENDUE EN 3D
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF, EX_ELEB3DT => ELEB3DT
!
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN)    :: NELEM2,NPOIN2,NPLAN,NETAGE,NPTFR,NELMAX2
      INTEGER, INTENT(INOUT) :: IKLE3(NELEM2,3,NETAGE,4)
      INTEGER, INTENT(INOUT) :: IKLBOR(NPTFR,2,NETAGE,3)
      INTEGER, INTENT(INOUT) :: NULONE(NPTFR,2,NETAGE,3)
      INTEGER, INTENT(INOUT) :: NELBOR(NPTFR,2,NETAGE),NBOR(NPTFR*NPLAN)
      INTEGER, INTENT(INOUT) :: KP1BOR(NPTFR)
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      LOGICAL OK(2)
!
      INTEGER IELEM,IPOIN,T(3)
      INTEGER IETAGE,IPTFR,IL1,IL2,IL3,IL4,IG(2,2,3),IL(2,2,3)
      INTEGER IG1,IG2,IG3,IG4
      INTEGER NUM1(12),NUM2(12),NUM3(12),K,L,M,N
!
      DATA NUM1 / 1 , 2 , 4 , 1 , 3 , 2 , 2 , 3 , 4 , 3 , 1 , 4 /
      DATA NUM2 / 2 , 4 , 1 , 3 , 2 , 1 , 3 , 4 , 2 , 1 , 4 , 3 /
      DATA NUM3 / 4 , 1 , 2 , 2 , 1 , 3 , 4 , 2 , 3 , 4 , 3 , 1 /
!
!***********************************************************************
!
C CONNECTIVITY TABLES FOR BOUNDARY FACES --> IKLBOR , NBOR3 ,
C CORRESPONDENCE BETWEEN LOCAL BOUNDARY NUMBERS AND 3D LOCAL NUMBERS --> NULONE
C
C COMPUTES NELBO3
!
C LATERAL BOUNDARIES :
C FOR EACH RECTANGULAR FACE SPLIT IN TWO TRIANGLES
C THE LOWER TRIANGLE IS NUMBER 1, THE HIGHER IS NUMBER 2
!
      DO IPTFR = 1,NPTFR
!
C        NUMBER OF THE TRIANGLE TOUCHING THE BOUNDARY IN 2D
         IELEM = NELBOR(IPTFR,1,1)
!
C        GLOBAL NUMBER OF BOTTOM-LEFT POINT OF THE RECTANGULAR FACE
C        IN THE FIRST LAYER (THE PATTERN REPEATS ITSELF THEREAFTER)
         IPOIN = NBOR(IPTFR)
!
         DO IETAGE = 1,NETAGE
!
C           3D BOUNDARY NUMBERING OF THE 4 POINTS OF THE RECTANGULAR FACE
!
            IL1 =       IPTFR   + (IETAGE-1)*NPTFR
            IL2 = KP1BOR(IPTFR) + (IETAGE-1)*NPTFR
            IL3 = IL2 + NPTFR
            IL4 = IL1 + NPTFR
!
C           3D GLOBAL NUMBERING OF THE 4 POINTS OF THE RECTANGULAR FACE
!
            IG1 =        NBOR(IPTFR)  + (IETAGE-1)*NPOIN2
            IG2 = NBOR(KP1BOR(IPTFR)) + (IETAGE-1)*NPOIN2
            IG3 = IG2 + NPOIN2
            IG4 = IG1 + NPOIN2
!
C NUMBERS OF THE 3 TETRAHEDRONS POSSIBLY TOUCHING THE FACE
!
            T(1) = (IETAGE-1)*3*NELEM2+IELEM
            T(2) = T(1) + NELEM2
            T(3) = T(2) + NELEM2
!
C LOOKS FOR THE LOWER TRIANGLE (CAN BE 1-2-4 OR 1-2-3)
!
C           2 POSSIBLE FORMS OF THE LOWER TRIANGLE (GLOBAL AND BOUNDARY)
            IG(1,1,1)=IG1
            IG(1,1,2)=IG2
            IG(1,1,3)=IG4
            IG(1,2,1)=IG1
            IG(1,2,2)=IG2
            IG(1,2,3)=IG3
            IL(1,1,1)=IL1
            IL(1,1,2)=IL2
            IL(1,1,3)=IL4
            IL(1,2,1)=IL1
            IL(1,2,2)=IL2
            IL(1,2,3)=IL3
C           2 POSSIBLE FORMS OF THE HIGHER TRIANGLE (GLOBAL AND BOUNDARY)
            IG(2,1,1)=IG1
            IG(2,1,2)=IG3
            IG(2,1,3)=IG4
            IG(2,2,1)=IG2
            IG(2,2,2)=IG3
            IG(2,2,3)=IG4
            IL(2,1,1)=IL1
            IL(2,1,2)=IL3
            IL(2,1,3)=IL4
            IL(2,2,1)=IL2
            IL(2,2,2)=IL3
            IL(2,2,3)=IL4
!
            OK(1)=.FALSE.
            OK(2)=.FALSE.
!
C           K=1 LOWER TRIANGLE   K=2 HIGHER TRIANGLE
            DO K=1,2
C           2 POSSIBLE SPLITS
            DO L=1,2
C           12 WAYS FOR A TETRAHEDRON OF PRESENTING ITS FACES
            DO M=1,12
C           3 POSSIBLE TETRAHEDRONS
            DO N=1,3
              IF(IG(K,L,1).EQ.IKLE3(IELEM,N,IETAGE,NUM1(M)).AND.
     &           IG(K,L,2).EQ.IKLE3(IELEM,N,IETAGE,NUM2(M)).AND.
     &           IG(K,L,3).EQ.IKLE3(IELEM,N,IETAGE,NUM3(M))) THEN
!
                  IKLBOR(IPTFR,K,IETAGE,1) = IL(K,L,1)
                  IKLBOR(IPTFR,K,IETAGE,2) = IL(K,L,2)
                  IKLBOR(IPTFR,K,IETAGE,3) = IL(K,L,3)
                  NELBOR(IPTFR,K,IETAGE)   = T(N)
                  NULONE(IPTFR,K,IETAGE,1) = NUM1(M)
                  NULONE(IPTFR,K,IETAGE,2) = NUM2(M)
                  NULONE(IPTFR,K,IETAGE,3) = NUM3(M)
!
                  OK(K) = .TRUE.
!
              ENDIF
            ENDDO
            ENDDO
            ENDDO
            ENDDO
            IF(.NOT.OK(1).OR..NOT.OK(2)) THEN
            WRITE(LU,*) 'PB IN ELEB3DT IELEM=',IELEM,' IPTFR=',IPTFR
            CALL PLANTE(1)
            STOP
            ENDIF
!
C           GLOBAL NUMBERS OF THE LATERAL BOUNDARY POINTS
C           ALL THE PLANES EXCEPT SURFACE
            NBOR(IPTFR +(IETAGE-1)*NPTFR)=IPOIN+(IETAGE-1)*NPOIN2
!
         ENDDO
      ENDDO
!
C GLOBAL NUMBERS OF THE LATERAL BOUNDARY POINTS: ON THE SURFACE
!
      DO IPTFR = 1,NPTFR
         NBOR(IPTFR+(NPLAN-1)*NPTFR)=NBOR(IPTFR)+(NPLAN-1)*NPOIN2
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C