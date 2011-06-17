! -------------------- FUNCTION FCTE1.f      
  
      FUNCTION FCTE1(XX)

!***********************************************************************
! ARTEMIS   V6P1                                   31/05/2011
!***********************************************************************
!
!brief    EVALUATE FUNCTION E1(KH) FOR SECOND  
!+        ORDER BOTTOM EFFECTS (GRADIENT)

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!         IN:   XX         = K*H
!
!                           ( X**4 + 4 X**3 SH(X) - 9 SH(X)SH(2X) + 3 X (X+2SH(X))*(CH(X)**2-2CH(X)+3) )
!         OUT :  E1(KH) = -----------------------------------------------------------------------------
!                                                     3 ( X+SH(X) )**4  
!                given X=2KH
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
!history  C.PEYRARD 
!+        31/05/2011
!+        V6P1

       DOUBLE PRECISION   XX , SHHX , SHH2X , CHHX , FCTE1
       INTRINSIC          SINH, COSH


       XX=2.*XX
       
       IF ( XX.LT.(0.001) )THEN
         FCTE1=-1./6.
        ELSE
         SHHX  = SINH(XX)
         SHH2X = SINH(2.*XX)
         CHHX  = COSH(XX)
      
         FCTE1=3.*( (CHHX-1.)**2. + 2. )*(XX + 2.*SHHX)*XX 
         FCTE1=FCTE1 +  SHHX*(XX**4./SHHX +4.*XX**3. -9.*SHH2X)
         FCTE1=FCTE1/(3.*(XX+SHHX)**4.)
       ENDIF
      END
