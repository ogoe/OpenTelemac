! -------------------- FUNCTION FCTE2.f      
  
      FUNCTION FCTE2(XX)
!***********************************************************************
! ARTEMIS   V6P1                                   31/05/2011
!***********************************************************************
!
!brief    EVALUATE FUNCTION E2(KH) FOR SECOND  
!+        ORDER BOTTOM EFFECTS (CURVATURE)

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!   IN:   XX         = K*H
!
!                                   (TH(X)-X)* CH(X)
!   OUT :  E2(KH)/K0 * 1/2H  =  ------------------------     (IN PENTECO, WE WRITE 2*H*E2(KH)*LAPLACIAN(H)
!                                   X ( SH(X) + X )**2
!          given X=2KH
!         
!      
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!history  C.PEYRARD 
!+        31/05/2011
!+        V6P1
!
       DOUBLE PRECISION  XX , SHHX , THHX , FCTE2
       INTRINSIC         SINH, COSH

       XX      = 2.*XX
       
       IF ( XX.LT.(0.001) ) THEN
         FCTE2=-1./12.
        ELSE
         SHHX    = SINH(XX)
         CHHX    = COSH(XX)
         THHX    = SHHX/CHHX
	 
         FCTE2=(THHX-XX)*CHHX/(XX*(SHHX+XX)**2.) 
       ENDIF
      END
