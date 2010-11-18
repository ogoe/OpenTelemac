C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @brief       SMOOTHES NEGATIVE DEPTHS AND COMPUTES CORRESPONDING
!>                FLUXES IN THE EQUATION OF CONTINUITY.

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Use(s)
!><br>BIEF
!>  @par Variable(s)
!>  <br><table>
!>     <tr><th> Argument(s)
!>    </th><td> DT, FLODEL, MASKEL, MESH, MSK, N, T1, UNSV2D, VEC, W1, YAFLODEL
!>   </td></tr>
!>     <tr><th> Common(s)
!>    </th><td>
!> INFO : LNG, LU
!>   </td></tr>
!>     <tr><th> Internal(s)
!>    </th><td> I, NELEM
!>   </td></tr>
!>     </table>

!>  @par Call(s)
!>  <br><table>
!>     <tr><th> Known(s)
!>    </th><td> FLUX_EF_VF(), OS(), OV(), PARCOM(), SMOOTHING_FLUX(), VECTOR()
!>   </td></tr>
!>     </table>

!>  @par Called by
!><br>CORRECTION_DEPTH_2D()

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
!>      <td><center> 5.9                                       </center>
!> </td><td> 20/05/2008
!> </td><td> J-M HERVOUET (LNHE) 01 30 87 80 18
!> </td><td>
!> </td></tr>
!>  </table>

C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!>  @par Details of primary variable(s)
!>  <br><table>
!>
!>     <tr><th>Name(s)</th><th>(in-out)</th><th>Description</th></tr>
!>          <tr><td>A
!></td><td><-></td><td>MATRICE (DONNEE OU CONSTRUITE SUIVANT BLDMAT)
!>    </td></tr>
!>          <tr><td>BLDMAT
!></td><td>--></td><td>LOGIQUE : ON CONSTRUIT LA MATRICE OU PAS.
!>    </td></tr>
!>          <tr><td>DT
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>F,G,H,U,V,W
!></td><td>--></td><td>FONCTIONS INTERVENANT DANS LA MATRICE
!>    </td></tr>
!>          <tr><td>FLODEL
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>FORMUL
!></td><td>--></td><td>FORMULE DECRIVANT LA MATRICE
!>                  (MEMES CONVENTIONS QUE DANS MATRIX)
!>    </td></tr>
!>          <tr><td>MESH,
!></td><td>--></td><td>BLOCS DU MAILLAGE.
!>    </td></tr>
!>          <tr><td>MSK,MASKEL
!></td><td>--></td><td>LOGIQUE ET TABLEAU POUR LE MASQUAGE
!>    </td></tr>
!>          <tr><td>N
!></td><td>--></td><td>NOMBRE DE FOIS OU ON FAIT L'OPERATION.
!>    </td></tr>
!>          <tr><td>T1
!></td><td>--></td><td>TABLEAU DE TRAVAIL.
!>    </td></tr>
!>          <tr><td>T2
!></td><td><-></td><td>TABLEAU DE TRAVAIL. MATRICE A MASS-LUMPEE
!>                  EN SORTIE (VOIR AUSSI XMUL)
!>    </td></tr>
!>          <tr><td>UNSV2D
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>VEC
!></td><td><-></td><td>VECTEUR A FILTRER
!>    </td></tr>
!>          <tr><td>W1
!></td><td>---</td><td>
!>    </td></tr>
!>          <tr><td>YAFLODEL
!></td><td>---</td><td>
!>    </td></tr>
!>     </table>
C
C#######################################################################
C
                        SUBROUTINE FILTER_H
     &(VEC,T1,MESH,MSK,MASKEL,N,FLODEL,YAFLODEL,DT,W1,UNSV2D)
C
C~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
C| A             |<->| MATRICE (DONNEE OU CONSTRUITE SUIVANT BLDMAT)
C| BLDMAT         |-->| LOGIQUE : ON CONSTRUIT LA MATRICE OU PAS.
C| DT             |---| 
C| F,G,H,U,V,W    |-->| FONCTIONS INTERVENANT DANS LA MATRICE
C| FLODEL         |---| 
C| FORMUL         |-->| FORMULE DECRIVANT LA MATRICE
C|                |   | (MEMES CONVENTIONS QUE DANS MATRIX)
C| MESH,          |-->| BLOCS DU MAILLAGE.
C| MSK,MASKEL     |-->| LOGIQUE ET TABLEAU POUR LE MASQUAGE
C| N             |-->| NOMBRE DE FOIS OU ON FAIT L'OPERATION.
C| T1             |-->| TABLEAU DE TRAVAIL.
C| T2             |<->| TABLEAU DE TRAVAIL. MATRICE A MASS-LUMPEE
C|                |   | EN SORTIE (VOIR AUSSI XMUL)
C| UNSV2D         |---| 
C| VEC            |<->| VECTEUR A FILTRER
C| W1             |---| 
C| YAFLODEL       |---| 
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
      INTEGER, INTENT(IN)           :: N
      DOUBLE PRECISION, INTENT(IN)  :: DT
      LOGICAL, INTENT(IN)           :: MSK,YAFLODEL
      TYPE(BIEF_MESH), INTENT(INOUT):: MESH
      TYPE(BIEF_OBJ), INTENT(INOUT) :: VEC,T1,FLODEL,W1
      TYPE(BIEF_OBJ), INTENT(IN)    :: MASKEL,UNSV2D
C
C+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
C
      INTEGER I,NELEM
C
C-----------------------------------------------------------------------
C
      IF(YAFLODEL) CALL OV('X=C     ',W1%R,W1%R,W1%R,0.D0,3*MESH%NELMAX)
C
      DO 10 I=1,N
C
C-----------------------------------------------------------------------
C
C     COMPUTES FLUXES DUE TO SMOOTHING (SEE RELEASE NOTES 5.9)
C
      IF(YAFLODEL) THEN
        NELEM=MESH%NELEM
        CALL SMOOTHING_FLUX(-1.D0/DT,VEC,VEC%R,MESH%SURFAC%R,
     &                      MESH%IKLE%I(      1  :  NELEM),
     &                      MESH%IKLE%I(NELEM+1  :2*NELEM),
     &                      MESH%IKLE%I(2*NELEM+1:3*NELEM),
     &                      NELEM,MESH%NELMAX,W1%R)
      ENDIF
C
C-----------------------------------------------------------------------
C
C     COMPUTES THE MATRIX/VETOR PRODUCT OF MASS X VEC
C
      CALL VECTOR(T1 ,'=','MASVEC          ',VEC%ELM,
     &            1.D0,VEC,VEC,VEC,VEC,VEC,VEC,MESH,MSK,MASKEL)
      IF(NCSIZE.GT.1) CALL PARCOM(T1,2,MESH)
C
C-----------------------------------------------------------------------
C
C     DIVIDES BY THE MASS OF THE BASES: F = M * F / (ASSEMBLED M)
C
      CALL OS('X=YZ    ',X=VEC,Y=T1,Z=UNSV2D)
C
C-----------------------------------------------------------------------
C
10    CONTINUE
C
C     TAKES FLUXES DUE TO THE SMOOTHING OF NEGATIVE DEPTHS INTO ACCOUNT
C
      IF(YAFLODEL) THEN
C
        CALL FLUX_EF_VF(FLODEL%R,W1%R,MESH%NSEG,MESH%NELEM,
     &                  MESH%ELTSEG%I,MESH%ORISEG%I,
     &                  MESH%IKLE%I,.TRUE.,0)
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