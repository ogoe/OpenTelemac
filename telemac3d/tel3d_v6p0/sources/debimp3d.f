C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       IMPOSES FLUX BOUNDARY CONDITIONS,
!>                WITH AN ASSUMPTION OF AFFINITY WITH THE
!>                VELOCITY PROFILES AT THE ENTRANCE.
!>  @code
!>      PRINCIPLE: 1) COMPUTES THE FLOW ON THE BOUNDARY COMPRISED
!>                    BETWEEN THE BOUNDARY NODES NDEB AND NFIN.
!>
!>                 2) CORRECTS THE IMPOSED VELOCITY BETWEEN NDEB AND
!>                    NFIN SO THAT THE DISCHARGE EQUALS THE DESIRED
!>                    DISCHARGE. THIS CORRECTION IS A SIMPLE RULE OF 3
!>                    AND THUS DOES NOT MODIFY THE VELOCITY PROFILE
!>                    PREVIOUSLY PROVIDED OR COMPUTED.
!>
!>      THE RESULT IS PUT IN THE ARRAYS UBOR AND VBOR TO BE
!>      IMPOSED TO U AND V DURING THE VARIOUS STAGES OF COMPUTATION.
!>
!>      IF THE DISCHARGE THROUGH A BOUNDARY IS 0, CANNOT USE THE
!>      RULE OF 3, UBOR AND VBOR ARE THEN REPLACED BY U AND V
!>      OBTAINED AT THE TIME STEP N.
!>
!>      THE CASE WHERE THE SEGMENT (NDEB,NFIN) PRESENTS A NUMBERING
!>      DISCONTINUITY (CASE WHERE THIS SEGMENT CONTAINS THE NODE
!>      NUMBER 1) IS TREATED.
!>  @endcode

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @note  JMH : T3_01, SIGMAG NOT USED.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> FORMUL, H, IELM2V, IFRLIQ, MASK, MASKBR, MESH, NETAGE, NPOIN2, NPTFR, NUMLIQ, Q, SIGMAG, SVIDE, T3_01, T3_02, T3_03, U, UBOR, V, VBOR, WBOR, ZPROP
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
!>    </th><td> I3D, IETAGE, IPTFR, K, Q1
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> BIEF_SUM(), OS(), PLANTE(), P_DSUM(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>BORD3D()

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
!>      <td><center> 5.5                                       </center>
!> </td><td> **/07/02
!> </td><td> AG
!> </td><td> 3D VERSION OF THE 2D DEBIMP
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>FORMUL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IELM2V
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>IFRLIQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASK
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MASKBR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>MESH
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NDEB,NFIN
!></td><td>--></td><td>NUMEROS DES POINTS DE BORD ENTRE LESQUELS ON
!>                  VEUT IMPOSER LE DEBIT
!>    </td></tr>
!>          <tr><td>NETAGE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPOIN2
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>NPTFR
!></td><td>--></td><td>NOMBRE DE POINTS FRONTIERE.
!>    </td></tr>
!>          <tr><td>NUMLIQ
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>Q
!></td><td>--></td><td>VALEUR DU DEBIT IMPOSE
!>    </td></tr>
!>          <tr><td>SIGMAG
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>SVIDE
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T3_01
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T3_02
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>T3_03
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>U , V , H
!></td><td>--></td><td>VALEURS DE U,V,H AU TEMPS N
!>    </td></tr>
!>          <tr><td>UBOR,VBOR
!></td><td><--</td><td>VALEURS DE U ET V DIRICHLET A L'ENTREE
!>    </td></tr>
!>          <tr><td>WBOR
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>WORK1,WORK2
!></td><td>--></td><td>TABLEAUX DE TRAVAIL.
!>    </td></tr>
!>          <tr><td>ZPROP
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE DEBIMP3D
     &(Q,UBOR,VBOR,WBOR,U,V,H,NUMLIQ,IFRLIQ,
     & T3_01,T3_02,T3_03,
     & NPTFR,NETAGE,MASK,MESH,FORMUL,NPOIN2,
     & IELM2V,SIGMAG,SVIDE,MASKBR,ZPROP)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| FORMUL         |---| 
C| IELM2V         |---| 
C| IFRLIQ         |---| 
C| MASK           |---| 
C| MASKBR         |---| 
C| MESH           |---| 
C| NDEB,NFIN      |-->| NUMEROS DES POINTS DE BORD ENTRE LESQUELS ON
C|                |   | VEUT IMPOSER LE DEBIT
C| NETAGE         |---| 
C| NPOIN2         |---| 
C| NPTFR          |-->| NOMBRE DE POINTS FRONTIERE.
C| NUMLIQ         |---| 
C| Q             |-->| VALEUR DU DEBIT IMPOSE
C| SIGMAG         |---| 
C| SVIDE          |---| 
C| T3_01          |---| 
C| T3_02          |---| 
C| T3_03          |---| 
C| U , V , H      |-->| VALEURS DE U,V,H AU TEMPS N
C| UBOR,VBOR      |<--| VALEURS DE U ET V DIRICHLET A L'ENTREE
C| WBOR           |---| 
C| WORK1,WORK2    |-->| TABLEAUX DE TRAVAIL.
C| ZPROP          |---| 
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C
      USE BIEF
C
      IMPLICIT NONE
      INTEGER LNG,LU
      COMMON/INFO/LNG,LU
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER, INTENT(IN) :: NPTFR,NETAGE,NPOIN2,IFRLIQ
      INTEGER, INTENT(IN) :: NUMLIQ(NPTFR)
C
      DOUBLE PRECISION, INTENT(INOUT) :: UBOR(*),VBOR(*),WBOR(*)
      DOUBLE PRECISION, INTENT(IN) :: MASK(*)
      DOUBLE PRECISION, INTENT(IN) :: Q
C
      CHARACTER(LEN=16) FORMUL
C
      INTEGER, INTENT(IN) :: IELM2V
      LOGICAL, INTENT(IN) :: SIGMAG
      TYPE(BIEF_MESH) :: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT) :: MASKBR
      TYPE(BIEF_OBJ) , INTENT(INOUT):: H,U,V
      TYPE(BIEF_OBJ) , INTENT(INOUT):: T3_01,T3_02,T3_03
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: ZPROP
      TYPE(BIEF_OBJ) , INTENT(INOUT) :: SVIDE
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER K,IETAGE,IPTFR,I3D
      DOUBLE PRECISION Q1
C
      INTRINSIC ABS
      DOUBLE PRECISION P_DSUM
      EXTERNAL         P_DSUM
C
C=======================================================================
C     COMPUTES THE FLUX
C=======================================================================
C
C  IN THE FOLLOWING LOOP RESTRICTS THE MASK OF DIRICHLETS SEGMENTS
C  TO THOSE OF THE LIQUID BOUNDARY NUMBER IFRLIQ. AS NUMLIQ IS
C  DEFINED AT NODES, POSSIBLY INTRODUCES AN ERROR FOR THE SEGMENT
C  FOLLOWING THE LAST BOUNDARY NODE. IN FACT THIS SEGMENT WILL BE
C  SOLID AND WILL ALREADY HAVE A MASK AT 0.
C
      CALL OS('X=0     ',X=MASKBR)
C
C  CHECKS IF THERE'S A DIRICHLET ON EITHER U OR V
C
      IF(MASKBR%ELM.EQ.70) THEN
C
C         QUADRILATERAL ON THE LATERAL BOUNDARIES
C
          DO K = 1,NPTFR
            IF(NUMLIQ(K).EQ.IFRLIQ) THEN
              DO IETAGE = 1,NETAGE
                MASKBR%R((IETAGE-1)*NPTFR+K)=MASK(K)
              ENDDO
            ENDIF
          ENDDO
C
      ELSEIF(MASKBR%ELM.EQ.60) THEN
C
C         TRIANGLES ON THE LATERAL BOUNDARIES
C
          DO K = 1,NPTFR
            IF(NUMLIQ(K).EQ.IFRLIQ) THEN
              DO IETAGE = 1,NETAGE
                MASKBR%R((IETAGE-1)*2*NPTFR+K      )=MASK(K)
                MASKBR%R((IETAGE-1)*2*NPTFR+K+NPTFR)=MASK(K)
              ENDDO
            ENDIF
          ENDDO
C
      ELSE
        WRITE(LU,*) 'UNKNOWN ELEMENT FOR MASKBR IN DEBIMP3D'
        CALL PLANTE(1)
        STOP
      ENDIF
C
      FORMUL = 'FLUBOR          '
C     IF (SIGMAG) FORMUL(7:7) = '2'
      CALL VECTOR(T3_02,'=',FORMUL,IELM2V,1.D0,SVIDE,SVIDE,SVIDE,
     &            U,V,SVIDE,MESH,.TRUE.,MASKBR)
C
      Q1 = - BIEF_SUM(T3_02)
C
      IF(NCSIZE.GT.1) Q1=P_DSUM(Q1)
C
C  ZERO FLOW: WARNING MESSAGE
C
      IF(ABS(Q1).LT.1.D-10) THEN
        IF(ABS(Q).GT.1.D-10) THEN
          IF(LNG.EQ.1) WRITE(LU,30) IFRLIQ
          IF(LNG.EQ.2) WRITE(LU,31) IFRLIQ
30        FORMAT(1X,'DEBIMP3D : PROBLEME SUR LA FRONTIERE ',1I6,/,1X,
     &     '         DONNER UN PROFIL DE VITESSES        ',/,1X,
     &     '         DANS LE FICHIER DES CONDITIONS AUX LIMITES',/,1X,
     &     '         OU VERIFIER LES HAUTEURS D''EAU')
31        FORMAT(1X,'DEBIMP3D : PROBLEM ON BOUNDARY NUMBER ',1I6,/,1X,
     &     '         GIVE A VELOCITY PROFILE  ',/,1X,
     &     '         IN THE BOUNDARY CONDITIONS FILE',/,1X,
     &     '         OR CHECK THE WATER DEPTHS')
          CALL PLANTE(1)
          STOP
        ELSE
          Q1 = 1.D0
        ENDIF
      ENDIF
!
!=======================================================================
C   NORMALISES UBOR VBOR WBOR
!=======================================================================
!
      DO IPTFR=1,NPTFR
        IF(NUMLIQ(IPTFR).EQ.IFRLIQ) THEN
          DO IETAGE =1, NETAGE+1
            I3D=(IETAGE-1)*NPTFR+IPTFR
            UBOR(I3D) = UBOR(I3D) * Q / Q1
            VBOR(I3D) = VBOR(I3D) * Q / Q1
C           SEE BORD3D
C           WBOR(I3D) = 0.D0
          ENDDO
        ENDIF
      ENDDO
!
!-----------------------------------------------------------------------
!
      RETURN
      END
C
C#######################################################################
C